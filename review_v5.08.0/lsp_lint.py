# -*- coding: utf-8 -*-
"""
lsp_lint.py - targeted static analyzer for AutoLISP (written for Sbugo_addlay_insert.lsp).

Checks:
  1. paren/string balance, top-level form inventory
  2. duplicate defuns, nested defuns that leak into the global namespace, *error* not local
  3. variables assigned (setq / out-params) that are not declared local (global leaks)
  4. variables read but never assigned anywhere (typos), declared-but-unused locals/params
  5. DCL key cross-check (get_tile/set_tile/mode_tile/action_tile/start_list vs "key =" in embedded DCL)
  6. paper-size dispatch tables (rotation symmetry, sizes vs INI, duplicates between the two tables)
  7. smells: non-ASCII symbols outside strings, stray backslash atoms, osnap exposure of (command ...),
     no-op initget, hard-coded paths, duplicated code blocks
Usage: python lsp_lint.py <file.lsp> [-o report.txt]
  The embedded DCL is extracted from the source automatically (override with --dcl file.dcl).
  Built-in function names are read from atoms_builtin.txt next to this script (override with --atoms);
  it lists the core AutoLISP built-ins (symbols of type SUBR from (atoms-family 1) in AutoCAD 2026); vla-/vlax-/vl- names are recognised by prefix.
Known limits (possible false positives):
  * symbols created only at run time by (set ..)/(eval ..) are not seen
  * quoted data lists are not analysed, except '(lambda ..) which is treated as code
  * "dead locals" can still list a local function that is only passed by name, e.g. (mapcar 'f ..)
"""
import re, sys, os, argparse, hashlib
from collections import defaultdict, Counter

# ------------------------------------------------------------------ tokenizer / parser
class Node:
    __slots__ = ("kind", "val", "line", "kids", "quote")
    def __init__(self, kind, val=None, line=0):
        self.kind, self.val, self.line, self.kids, self.quote = kind, val, line, [], False
    def head(self):
        if self.kind == "list" and self.kids and self.kids[0].kind == "atom":
            return self.kids[0].val.upper()
        return None
    def __repr__(self):
        if self.kind == "list":
            return "(" + " ".join(map(repr, self.kids)) + ")"
        if self.kind == "str":
            return '"%s"' % self.val
        return str(self.val)

DELIMS = set(" \t\r\n()\"';")

def tokenize(src):
    toks, i, n, line = [], 0, len(src), 1
    problems = []
    while i < n:
        c = src[i]
        if c == "\n":
            line += 1; i += 1
        elif c in " \t\r":
            i += 1
        elif c == ";":
            if i + 1 < n and src[i + 1] == "|":
                j = src.find("|;", i + 2)
                if j < 0:
                    problems.append((line, "unterminated ;| block comment")); j = n - 2
                line += src.count("\n", i, j + 2); i = j + 2
            else:
                j = src.find("\n", i)
                i = n if j < 0 else j
        elif c == "(":
            toks.append(("(", None, line)); i += 1
        elif c == ")":
            toks.append((")", None, line)); i += 1
        elif c == "'":
            toks.append(("'", None, line)); i += 1
        elif c == '"':
            start_line = line; j = i + 1; buf = []
            while j < n:
                ch = src[j]
                if ch == "\\" and j + 1 < n:
                    buf.append(src[j:j + 2]);
                    if src[j + 1] == "\n": line += 1
                    j += 2; continue
                if ch == '"':
                    break
                if ch == "\n": line += 1
                buf.append(ch); j += 1
            else:
                problems.append((start_line, "unterminated string"))
            toks.append(("str", "".join(buf), start_line)); i = j + 1
        else:
            j = i
            while j < n and src[j] not in DELIMS: j += 1
            toks.append(("atom", src[i:j], line)); i = j
    return toks, problems

def parse(toks):
    forms, stack, problems = [], [], []
    pend = [0]            # pending quote marks per nesting level (a quote applies to the next COMPLETE datum of its level)
    def add(node):
        while pend[-1]:
            q = Node("list", None, node.line); q.quote = True
            qa = Node("atom", "QUOTE", node.line); q.kids = [qa, node]; node = q; pend[-1] -= 1
        (stack[-1].kids if stack else forms).append(node)
    for kind, val, line in toks:
        if kind == "(":
            stack.append(Node("list", None, line)); pend.append(0)
        elif kind == ")":
            if not stack:
                problems.append((line, "extra ')' at top level")); continue
            node = stack.pop(); pend.pop(); add(node)
        elif kind == "'":
            pend[-1] += 1
        elif kind == "str":
            add(Node("str", val, line))
        else:
            add(Node("atom", val, line))
    for node in stack:
        problems.append((node.line, "unclosed '(' opened here"))
    return forms, problems

def parse_src(src):
    toks, p1 = tokenize(src)
    forms, p2 = parse(toks)
    return forms, p1 + p2

# ------------------------------------------------------------------ helpers
NUM = re.compile(r"^[+-]?(\d+\.?\d*|\.\d+)([eE][+-]?\d+)?$")
SPECIAL = {"T", "NIL", "PI", "/"}
def is_var_atom(a):
    v = a.val
    return not (NUM.match(v) or v.upper() in SPECIAL or v.startswith(":") or v == ".")

def split_params(plist):
    params, locs, seen_slash = [], [], False
    for k in plist.kids:
        if k.kind != "atom": continue
        if k.val == "/": seen_slash = True; continue
        (locs if seen_slash else params).append(k.val.upper())
    return params, locs

class FuncInfo:
    def __init__(self, name, params, locs, node, parent=None):
        self.name, self.params, self.locs, self.node, self.parent = name, params, locs, node, parent
        self.assigned = defaultdict(list)    # sym -> [line] (setq/out-param)
        self.reads = defaultdict(list)
        self.calls = defaultdict(list)
        self.nested = []
    @property
    def path(self):
        return (self.parent.path + " > " if self.parent else "") + self.name

OUT_METHODS = {"GETPAPERSIZE", "GETBOUNDINGBOX", "GETCUSTOMSCALE"}

class Analyzer:
    def __init__(self, forms, callback_forms=None):
        self.forms = forms
        self.funcs = []                # all FuncInfo (top-level + nested)
        self.toplevel = []             # top-level defuns
        self.global_setq = []          # (sym, line) at top level
        self.defined_anywhere = set()  # every symbol ever bound/assigned/declared
        self.read_anywhere = defaultdict(list)
        self.keys_used = defaultdict(list)  # dialog key -> [(line, fn)]
        self.callbacks = []            # (line, code-string)
        self.nested_defuns = []        # (parent, name, line, is_local_in_parent)
        self.initget_lines = []
        self.command_calls = []        # (line, fn)
        self.cyr_symbols = []
        self.stray_atoms = []
        self.all_calls = defaultdict(list)
        self.visit_top()

    # --- scopes
    def visit_top(self):
        for f in self.forms:
            if f.kind == "list" and f.head() == "DEFUN":
                self.visit_defun(f, None)
            else:
                self.walk(f, None, None)

    def visit_defun(self, node, parent):
        k = node.kids
        name = k[1].val.upper() if len(k) > 1 and k[1].kind == "atom" else "?"
        plist = k[2] if len(k) > 2 and k[2].kind == "list" else Node("list")
        params, locs = split_params(plist)
        fi = FuncInfo(name, params, locs, node, parent)
        fi.line = node.line
        self.funcs.append(fi)
        (parent.nested if parent else self.toplevel).append(fi)
        self.defined_anywhere.add(name)
        self.defined_anywhere.update(params); self.defined_anywhere.update(locs)
        if parent is not None:
            self.nested_defuns.append((parent, fi, node.line, name in parent.params or name in parent.locs))
        scope = set(params) | set(locs)
        chain = (parent_scope(parent) if parent else set())
        fi.scope = scope | chain
        for body in k[3:]:
            self.walk(body, fi, fi.scope)
        return fi

    def declare(self, fi, sym, line):
        self.defined_anywhere.add(sym)

    # --- expression walker
    def walk(self, n, fi, scope):
        if n.kind == "atom":
            if is_var_atom(n):
                s = n.val.upper()
                self.read_anywhere[s].append(n.line)
                if fi: fi.reads[s].append(n.line)
                self.check_atom(n)
            return
        if n.kind == "str":
            return
        if not n.kids:
            return
        h = n.head()
        kids = n.kids
        if n.quote or h == "QUOTE":
            inner = kids[1] if len(kids) > 1 else None
            if inner is not None and inner.kind == "list" and inner.head() == "LAMBDA":
                self.walk_lambda(inner, fi, scope)   # '(lambda ..) is code (mapcar, vl-remove-if, ...)
            return                         # anything else quoted is data, not code
        if h == "FUNCTION":
            for x in kids[1:]:
                if x.kind == "list" and x.head() == "LAMBDA":
                    self.walk_lambda(x, fi, scope)
                elif x.kind == "atom":
                    pass
            return
        if h == "LAMBDA":
            self.walk_lambda(n, fi, scope); return
        if h == "DEFUN":
            if fi is None:
                self.visit_defun(n, None)
            else:
                self.visit_defun(n, fi)
            return
        if h == "SETQ":
            i = 1
            while i < len(kids):
                tgt = kids[i]
                if tgt.kind == "atom":
                    s = tgt.val.upper(); self.defined_anywhere.add(s)
                    if fi: fi.assigned[s].append(tgt.line)
                    else: self.global_setq.append((s, tgt.line))
                    self.check_atom(tgt)
                if i + 1 < len(kids): self.walk(kids[i + 1], fi, scope)
                i += 2
            return
        if h in ("FOREACH", "VLAX-FOR"):
            if len(kids) > 1 and kids[1].kind == "atom":
                self.defined_anywhere.add(kids[1].val.upper())
                self.check_atom(kids[1])
                if fi: fi.assigned["__loopvar__:" + kids[1].val.upper()].append(kids[1].line)
            for x in kids[2:]: self.walk(x, fi, scope)
            return
        if h == "COND":
            for cl in kids[1:]:
                if cl.kind == "list":
                    for x in cl.kids: self.walk(x, fi, scope)
                else:
                    self.walk(cl, fi, scope)
            return
        # generic call
        if h:
            self.all_calls[h].append(n.line)
            if fi: fi.calls[h].append(n.line)
            if h in ("COMMAND", "COMMAND-S", "VL-CMDF"):
                self.command_calls.append((n.line, fi.name if fi else "<top>", n))
            if h == "INITGET":
                self.initget_lines.append(n.line)
            if h in ("GET_TILE", "SET_TILE", "MODE_TILE", "ACTION_TILE", "START_LIST", "START_IMAGE",
                     "DIMX_TILE", "DIMY_TILE") and len(kids) > 1 and kids[1].kind == "str":
                self.keys_used[kids[1].val].append((kids[1].line, fi.name if fi else "<top>"))
            if h == "ACTION_TILE" and len(kids) > 2 and kids[2].kind == "str":
                self.callbacks.append((kids[2].line, kids[2].val, fi))
            # out-parameters: 'sym after 'GetPaperSize / 'GetBoundingBox ...
            self.mark_out_params(n, fi)
        else:
            # head is not an atom => e.g. ((lambda...) args) or a data-ish list; evaluate everything
            pass
        start = 1 if h else 0
        for x in kids[start:]:
            self.walk(x, fi, scope)

    def mark_out_params(self, n, fi):
        # look at this list and at (list ...) children: quoted atoms following a quoted OUT method name
        def scan(seq):
            armed = False
            for x in seq:
                if x.kind == "list" and x.quote and len(x.kids) == 2 and x.kids[1].kind == "atom":
                    nm = x.kids[1].val.upper()
                    if nm in OUT_METHODS: armed = True; continue
                    if armed:
                        self.defined_anywhere.add(nm)
                        if fi: fi.assigned[nm].append(x.kids[1].line)
                elif x.kind == "list" and x.head() == "LIST":
                    scan(x.kids[1:])
        scan(n.kids[1:])
        # (vla-getboundingbox obj 'a 'b)
        if n.head() == "VLA-GETBOUNDINGBOX":
            for x in n.kids[2:]:
                if x.kind == "list" and x.quote and x.kids[1].kind == "atom":
                    nm = x.kids[1].val.upper(); self.defined_anywhere.add(nm)
                    if fi: fi.assigned[nm].append(x.kids[1].line)

    def walk_lambda(self, n, fi, scope):
        kids = n.kids
        plist = kids[1] if len(kids) > 1 and kids[1].kind == "list" else Node("list")
        params, locs = split_params(plist)
        self.defined_anywhere.update(params); self.defined_anywhere.update(locs)
        inner = set(scope or ()) | set(params) | set(locs)
        # record lambda locals under the enclosing function for 'unused' accounting
        if fi:
            fi.assigned.setdefault("__lambda_locals__", []).extend(params + locs)
        for x in kids[2:]:
            self.walk_in_lambda(x, fi, inner, set(params) | set(locs))

    def walk_in_lambda(self, n, fi, scope, lam_locals):
        # same as walk but setq targets that are lambda locals are not "assigned in fi"
        saved = None
        if fi is not None:
            saved = {k: list(v) for k, v in fi.assigned.items()}
        self.walk(n, fi, scope)
        if fi is not None:
            for sym in lam_locals:
                if sym in fi.assigned and (sym not in saved or len(fi.assigned[sym]) != len(saved[sym])):
                    fi.assigned[sym] = saved.get(sym, [])
                    if not fi.assigned[sym]: del fi.assigned[sym]

    def check_atom(self, a):
        v = a.val
        if any(ord(ch) > 127 for ch in v):
            self.cyr_symbols.append((a.line, v))
        if "\\" in v:
            self.stray_atoms.append((a.line, v))

def parent_scope(fi):
    return fi.scope if fi is not None else set()

# ------------------------------------------------------------------ report
def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("file"); ap.add_argument("--atoms"); ap.add_argument("--dcl"); ap.add_argument("-o")
    ap.add_argument("--enc", default="cp1251")
    a = ap.parse_args()
    raw = open(a.file, "rb").read()
    src = raw.decode(a.enc).replace("\r\n", "\n")
    out = []
    P = out.append
    def H(t): P("\n" + "=" * 100 + "\n" + t + "\n" + "=" * 100)

    forms, problems = parse_src(src)
    lines = src.split("\n")
    H("1. STRUCTURE")
    P("file: %s   bytes=%d lines=%d" % (a.file, len(raw), len(lines)))
    P("CRLF lines: %d / LF-only: %d" % (raw.count(b"\r\n"), raw.count(b"\n") - raw.count(b"\r\n")))
    P("top-level forms: %d  (defun: %d)" % (len(forms), sum(1 for f in forms if f.kind == "list" and f.head() == "DEFUN")))
    P("paren/string problems: %s" % (problems if problems else "none"))
    toplevel_non_defun = [(f.line, repr(f)[:90]) for f in forms if not (f.kind == "list" and f.head() == "DEFUN")]
    P("top-level non-defun forms executed at load time (%d):" % len(toplevel_non_defun))
    for l, r in toplevel_non_defun: P("   line %5d: %s" % (l, r))
    an = Analyzer(forms)

    # action_tile callback strings are code too: parse them BEFORE the typo checks
    cb_keys = defaultdict(list); cb_msgs = []
    for line, code, fi in an.callbacks:
        try:
            sub_forms, probs = parse_src(code.replace('\\"', '"').replace("\\\\", "\\"))
        except Exception as e:
            cb_msgs.append("callback at line %d did not parse: %s" % (line, e)); continue
        if probs: cb_msgs.append("callback at line %d has paren problems: %s -> %s" % (line, probs, code[:80]))
        sub = Analyzer(sub_forms)
        for k, lst in sub.keys_used.items(): cb_keys[k].append(line)
        for sym in sub.defined_anywhere: an.defined_anywhere.add(sym)
        for sym, ls in sub.read_anywhere.items():
            an.read_anywhere[sym].extend([line] * len(ls))
        for h, ls in sub.all_calls.items(): an.all_calls[h].extend([line] * len(ls))

    # builtin names
    builtin = set()
    atoms_path = a.atoms or os.path.join(os.path.dirname(os.path.abspath(__file__)), "atoms_builtin.txt")
    if os.path.exists(atoms_path):
        for ln in open(atoms_path, "rb").read().decode("cp1251", "replace").splitlines():
            ln = ln.strip()
            if ln: builtin.add(ln.upper())
    else:
        out.append("note: atoms_builtin.txt not found - built-in function names are only approximated")
    def is_builtin(sym):
        s = sym.upper()
        return (s in builtin or s.startswith(("VLA-", "VLAX-", "VLR-", "VL-", "VLISP-", "AC")) or s in GUI_ONLY)
    GUI_ONLY = set("""LAYOUTLIST LOAD_DIALOG NEW_DIALOG START_DIALOG DONE_DIALOG UNLOAD_DIALOG SET_TILE GET_TILE MODE_TILE
    ACTION_TILE START_LIST ADD_LIST END_LIST START_IMAGE END_IMAGE FILL_IMAGE SLIDE_IMAGE DIMX_TILE DIMY_TILE ALERT GETFILED
    COMMAND-S HANDENT TBLSEARCH DICTSEARCH NAMEDOBJDICT ENTSEL SSGET SSADD SSNAME SSLENGTH ENTGET ENTMAKE ENTLAST ENTNEXT
    TRANS GETVAR SETVAR FINDFILE WCMATCH $VALUE $REASON ITOA ATOI ATOF RTOS DISTANCE ANGLE POLAR READ-LINE WRITE-LINE OPEN CLOSE
    MAPCAR APPLY FOREACH REPEAT WHILE PRINC PRIN1 TERPRI EXIT LENGTH NTH CAR CDR CAAR CADR CDDR CADAR CDDDR CDDDDR CADDR
    ASSOC MEMBER REVERSE APPEND LIST CONS SUBST EQUAL MINUSP ZEROP ABS FIX COS SIN BOUNDP TYPE READ EVAL STRCAT STRCASE
    SUBSTR STRLEN ENTMAKEX""".split())

    H("2. FUNCTION INVENTORY / SCOPE HYGIENE")
    names = Counter(f.name for f in an.funcs if f.parent is None)
    P("top-level defuns (%d):" % len(an.toplevel))
    for f in an.toplevel:
        P("   line %5d  %-48s params=%d locals=%d nested=%d" % (f.line, f.name, len(f.params), len(f.locs), len(f.nested)))
    dups = [n for n, c in names.items() if c > 1]
    P("duplicate top-level names: %s" % (dups or "none"))
    P("\nnested defuns:")
    for parent, fi, line, is_local in an.nested_defuns:
        flag = "ok (declared local)" if is_local else "LEAKS to global namespace when parent runs"
        if fi.name == "*ERROR*" and not is_local: flag = "*ERROR* NOT LOCAL -> overrides global handler"
        P("   line %5d  %-40s inside %-40s %s" % (line, fi.name, parent.name, flag))
    # nested defun that redefines a top-level function
    top_names = {f.name for f in an.toplevel}
    for parent, fi, line, is_local in an.nested_defuns:
        if fi.name in top_names and not is_local:
            P("   !! nested defun at line %d redefines top-level function %s (defined at line %d)" %
              (line, fi.name, next(t.line for t in an.toplevel if t.name == fi.name)))

    H("3. GLOBAL LEAKS: variables assigned (setq / out-params) without being local or a parameter")
    leaks = defaultdict(list)
    for fi in an.funcs:
        scope = fi.scope
        for sym, ls in fi.assigned.items():
            if sym.startswith("__"): continue
            if sym in scope: continue
            leaks[sym].append((fi.name, ls[0], len(ls)))
    intentional = []
    for sym in sorted(leaks):
        where = ", ".join("%s@%d" % (f, l) for f, l, c in leaks[sym])
        P("   %-42s %s" % (sym, where[:150]))
    P("total distinct leaked symbols: %d" % len(leaks))
    P("\ntop-level (load time) setq: %s" % an.global_setq)

    H("4. TYPOS: symbols read but never assigned/declared anywhere and not built-in")
    suspicious = []
    for sym, ls in sorted(an.read_anywhere.items()):
        if sym in an.defined_anywhere or is_builtin(sym): continue
        suspicious.append((sym, ls[0], len(ls)))
    for s, l, c in suspicious: P("   %-40s first line %5d  (%d reads)" % (s, l, c))
    if not suspicious: P("   none")
    P("   (Cyrillic words and a lone backslash here are the pseudo-comments after '\\\\' - see section 6; "
      "'T' with Cyrillic letter is line 1668)")

    P("\nDeclared local but never assigned AND never read in the function (dead locals):")
    for fi in an.funcs:
        dead = []
        lam = set(fi.assigned.get("__lambda_locals__", []))
        for s in fi.locs:
            if s == "*ERROR*": continue
            used = (s in fi.assigned) or (s in fi.reads) or (s in lam) or ("__loopvar__:" + s in fi.assigned) or (s in an.all_calls)
            # used inside nested functions?
            if not used:
                for nf in all_nested(fi):
                    if s in nf.reads or s in nf.assigned: used = True; break
            if not used: dead.append(s)
        if dead: P("   %-45s %s" % (fi.name, ", ".join(dead)))
    P("\nParameters never read (unused parameters):")
    for fi in an.funcs:
        dead = [s for s in fi.params if s not in fi.reads and s not in fi.assigned
                and not any((s in nf.reads or s in nf.assigned) for nf in all_nested(fi))]
        if dead: P("   %-45s %s" % (fi.name, ", ".join(dead)))
    P("\nFunctions defined but never called (excluding commands c:*):")
    called = set(an.all_calls)
    # also function names referenced via quote/function/strings in callbacks
    cb_text = " ".join(c[1].upper() for c in an.callbacks)
    for f in an.funcs:
        if f.name.startswith("C:"): continue
        if f.name == "*ERROR*": continue
        if f.name not in called and f.name not in an.read_anywhere and f.name not in cb_text and not re.search(r"'" + re.escape(f.name) + r"\b", src, re.I):
            P("   line %5d %s" % (f.line, f.name))

    H("5. DCL KEY CROSS-CHECK")
    if a.dcl:
        dcl_text = open(a.dcl, "rb").read().decode("cp1251", "replace")
        P("DCL taken from file %s" % a.dcl)
    else:
        dcl_text = extract_dcl(src)
        P("DCL extracted from the (foreach item '(...)) string list inside the source")
    dcl_keys = set(re.findall(r'key\s*=\s*"([^"]+)"', dcl_text))
    P("DCL: %d bytes (cp1251, CRLF), %d keys, braces: open=%d close=%d" % (len(dcl_text.encode("cp1251", "replace")), len(dcl_keys),
                                                                           dcl_text.count("{"), dcl_text.count("}")))
    for m in cb_msgs: P("   " + m)
    used_keys = set(an.keys_used) | set(cb_keys)
    special = {"accept", "cancel", "help", "info"}
    P("keys used in code but missing in DCL:")
    for k in sorted(used_keys - dcl_keys - special):
        P("   %-45s lines %s" % (k, [x[0] for x in an.keys_used.get(k, [])] + cb_keys.get(k, [])))
    P("DCL keys never referenced from code:")
    for k in sorted(dcl_keys - used_keys):
        P("   %s" % k)
    # simple DCL lint
    P("\nDCL attribute lint:")
    ENUM = {"alignment": {"left", "right", "centered", "top", "bottom"},
            "children_alignment": {"left", "right", "centered", "top", "bottom"}}
    for ln_no, ln in enumerate(dcl_text.split("\r\n") if "\r\n" in dcl_text else dcl_text.split("\n"), 1):
        for attr, vals in ENUM.items():
            for m in re.finditer(r"\b%s\s*=\s*([A-Za-z_]+)\s*;" % attr, ln):
                if m.group(1) not in vals: P("   dcl line %d: %s = %s   <-- not a valid value" % (ln_no, attr, m.group(1)))
        for m in re.finditer(r"\bvalue\s*=\s*(-?\d+)\s*;", ln):
            P("   dcl line %d: unquoted numeric value '%s' (value is a string attribute)" % (ln_no, m.group(1)))

    H("6. SMELLS")
    P("non-ASCII characters inside symbol names (look-alike letters): %s" % (an.cyr_symbols or "none"))
    P("stray backslash atoms outside strings (probably meant to be comments): ")
    for l, v in an.stray_atoms: P("   line %d: %r -> %s" % (l, v, lines[l - 1].strip()[:110]))
    # atoms on lines right after a closing paren and a backslash
    P("\nstandalone '\\\\' tokens in code lines:")
    for i, ln in enumerate(lines, 1):
        code = strip_comment_and_strings(ln)
        if re.search(r"(^|\s)\\\\?(\s|$)", code) and code.strip():
            P("   line %d: %s" % (i, ln.strip()[:120]))
    P("\ninitget calls (line numbers): %s" % an.initget_lines)
    P("   -> initget is only meaningful right before getXXX/entsel; verifying neighbours:")
    for l in an.initget_lines:
        window = " ".join(lines[l:l + 6]).lower()
        if not re.search(r"\((getpoint|getint|getreal|getstring|getkword|getdist|entsel|nentsel|getangle|getcorner|getorient)", window):
            P("   line %5d: initget with no get* call within the next 6 lines (no-op)" % l)
    P("\n(command ...) calls: %d total" % len(an.command_calls))
    P("   calls that pass literal coordinates / point variables (osnap-sensitive) without binding OSMODE:")
    osm_funcs = {f.name for f in an.funcs if "OSMODE" in " ".join(repr(f.node).upper().split())}
    for line, fn, node in an.command_calls:
        txt = repr(node)
        if re.search(r"\b(point1|point2|sbugoPDSimpleBlock\w+|sbugoPDointNextInsert|list)\b", txt, re.I) or re.search(r"\(\s*(list|polar)\b", txt, re.I):
            flag = "osmode handled in this function" if fn in osm_funcs else "NO OSMODE handling"
            P("   line %5d in %-34s %s :: %s" % (line, fn, flag, txt[:80]))
    P("\nhard-coded absolute paths:")
    for i, ln in enumerate(lines, 1):
        if re.search(r'[A-Za-z]:[\\/]{1,2}[\w\\/_ ]+', strip_comment(ln)) and '"' in ln:
            m = re.search(r'"[^"]*[A-Za-z]:[\\/]{1,2}[^"]*"', ln)
            if m: P("   line %5d: %s" % (i, m.group(0)[:100]))
    P("\nsystem variables modified with setvar (potential state leaks):")
    sv = defaultdict(list)
    for m in re.finditer(r"\(setvar\s+(?:'|\")([A-Za-z0-9_]+)", src, re.I):
        sv[m.group(1).upper()].append(src.count("\n", 0, m.start()) + 1)
    for k, v in sorted(sv.items()): P("   %-14s lines %s" % (k, v))

    H("7. DUPLICATED CODE")
    norm = []
    for i, ln in enumerate(lines, 1):
        t = strip_comment(ln).strip()
        t = re.sub(r"\s+", " ", t)
        norm.append((i, t))
    W = 8
    seen = {}; dup_lines = set()
    sig = [t for _, t in norm]
    for i in range(len(sig) - W):
        win = sig[i:i + W]
        if sum(1 for x in win if len(x) > 3) < W - 1: continue
        h = hashlib.md5("\n".join(win).encode("utf8", "replace")).hexdigest()
        if h in seen and abs(seen[h] - i) > W:
            for k in range(W): dup_lines.add(i + k); dup_lines.add(seen[h] + k)
        else:
            seen.setdefault(h, i)
    code_lines = sum(1 for _, t in norm if len(t) > 3)
    P("lines participating in duplicated %d-line windows: %d of %d non-trivial lines (%.1f%%)" %
      (W, len(dup_lines), code_lines, 100.0 * len(dup_lines) / max(1, code_lines)))
    rng = []
    for i in sorted(dup_lines):
        if rng and i == rng[-1][1] + 1: rng[-1][1] = i
        else: rng.append([i, i])
    P("duplicated ranges (1-based lines): " + ", ".join("%d-%d" % (s + 1, e + 1) for s, e in rng if e - s >= 6))

    # ----- paper-size tables
    H("8. PAPER-SIZE DISPATCH TABLES")
    tables = {}
    for fi in an.funcs:
        if fi.name in ("SBUGO-SET-LAYOUT-TYPE-PRINTER", "SBUGO-LOCAL-SELECT"):
            rows = []
            for cl in find_clauses(fi.node):
                r = parse_clause(cl)
                if r: rows.append(r)
            tables[fi.name] = rows
            P("%s: %d size clauses" % (fi.name, len(rows)))
    # also nested funcs
    for fi in an.funcs:
        if fi.name == "SBUGO-LOCAL-SELECT" and "SBUGO-LOCAL-SELECT" not in tables:
            pass
    ini = parse_ini_strings(src)
    P("INI records parsed from embedded strings: %d" % len(ini))
    ini_by_pos = {r["pos"]: r for r in ini}
    t1 = tables.get("SBUGO-SET-LAYOUT-TYPE-PRINTER", [])
    t2 = tables.get("SBUGO-LOCAL-SELECT", [])
    # rotation symmetry
    P("\n8a. rotation symmetry in Sbugo-set-layout-type-printer (landscape H<W => rot as in INI, portrait => swapped):")
    bad = 0
    for r in t1:
        H_, W_, pos, rot, line = r["H"], r["W"], r["pos"], r["rot"], r["line"]
        want = "same" if H_ < W_ else "swapped"
        if rot != want:
            bad += 1
            P("   line %5d  pos %s  H=%g W=%g -> rotation clause is '%s' but should be '%s'   <-- BUG" % (line, pos, H_, W_, rot, want))
    if not bad: P("   all clauses symmetric")
    # pairs
    P("\n8b. every position must have both orientations exactly once:")
    cnt = Counter()
    for r in t1: cnt[(r["pos"], "L" if r["H"] < r["W"] else "P")] += 1
    for pos in sorted({r["pos"] for r in t1}):
        if cnt[(pos, "L")] != 1 or cnt[(pos, "P")] != 1:
            P("   pos %s: landscape=%d portrait=%d" % (pos, cnt[(pos, "L")], cnt[(pos, "P")]))
    P("   positions present: %d" % len({r["pos"] for r in t1}))
    P("\n8c. sizes in clauses vs INI sizes for the same position (INI field 4/5):")
    for r in t1:
        rec = ini_by_pos.get(r["pos"])
        if not rec: P("   pos %s has no INI record" % r["pos"]); continue
        a_, b_ = sorted((r["H"], r["W"])); c_, d_ = sorted((rec["s1"], rec["s2"]))
        if (a_, b_) != (c_, d_):
            P("   pos %s (%s): clause %g x %g but INI says %g x %g   (line %d)" % (r["pos"], rec["name"], a_, b_, c_, d_, r["line"]))
    P("\n8d. two tables identical (H, W, pos)?")
    s1 = sorted((r["H"], r["W"], r["pos"]) for r in t1); s2 = sorted((r["H"], r["W"], r["pos"]) for r in t2)
    P("   table1=%d rows, table2=%d rows, identical=%s" % (len(s1), len(s2), s1 == s2))
    if s1 != s2:
        P("   only in set-layout-type-printer: %s" % sorted(set(s1) - set(s2)))
        P("   only in local-select: %s" % sorted(set(s2) - set(s1)))
    P("\n8e. ambiguity: pairs of different sizes whose +-1.1 mm windows overlap in both dimensions:")
    amb = 0
    for i in range(len(t1)):
        for j in range(i + 1, len(t1)):
            x, y = t1[i], t1[j]
            if x["pos"] != y["pos"] and abs(x["H"] - y["H"]) <= 2.2 and abs(x["W"] - y["W"]) <= 2.2:
                amb += 1; P("   %s vs %s (lines %d / %d)" % (x["pos"], y["pos"], x["line"], y["line"]))
    if not amb: P("   none")
    P("\n8f. INI record sanity:")
    for r in ini:
        probs = []
        if min(r["s1"], r["s2"]) == 0: probs.append("zero size")
        if r["name"] == "A4" and (r["s1"], r["s2"]) != (210, 297): probs.append("A4 should be 210x297, INI has %g x %g" % (r["s1"], r["s2"]))
        m = re.search(r"\(([\d.]+) x ([\d.]+)", r["media"])
        if m and r["type"] == "PDF":
            mw, mh = float(m.group(1)), float(m.group(2))
            if sorted((round(mw), round(mh))) != sorted((r["s1"], r["s2"])): probs.append("media name says %gx%g" % (mw, mh))
        if probs: P("   record %s (%s %s): %s" % (r["pos"], r["type"], r["name"], "; ".join(probs)))
    pdf = {r["name"]: r for r in ini if r["type"] == "PDF"}; plt = {r["name"]: r for r in ini if r["type"] == "PLOT"}
    P("   PDF names=%d PLOT names=%d; same set=%s" % (len(pdf), len(plt), set(pdf) == set(plt)))
    for n_, r in plt.items():
        if "PDF" in r["plotter"].upper(): P("   PLOT row %s (%s) uses the PDF driver %s" % (r["pos"], n_, r["plotter"]))

    text = "\n".join(out)
    if a.o:
        open(a.o, "wb").write(text.encode("utf-8"))
    sys.stdout.buffer.write((text + "\n").encode("utf-8", "replace"))

def extract_dcl(src):
    """Return the DCL text embedded as a list of strings: (foreach item '("..." "...") (write-line item h))."""
    lines = src.split("\n")
    # the file has two such lists: the INI text (line ~32) and the DCL text (line ~1786) - take the one with dialogs
    for start in [i for i, l in enumerate(lines) if "(foreach item" in l and i + 1 < len(lines)
                  and lines[i + 1].lstrip().startswith("'(\"")]:
        items = []
        for l in lines[start + 1:]:
            m = re.match(r'^\s*(?:\'\()?"(.*)"\s*\)?\s*$', l)
            if not m:
                break
            s, out, i = m.group(1), [], 0
            while i < len(s):
                if s[i] == "\\" and i + 1 < len(s):
                    out.append({"\\": "\\", "\"": "\"", "n": "\n", "t": "\t", "r": "\r"}.get(s[i + 1], "\\" + s[i + 1])); i += 2
                else:
                    out.append(s[i]); i += 1
            items.append("".join(out))
            if l.strip() == '"")':
                break
        text = "\r\n".join(items) + "\r\n"
        if ": dialog" in text:
            return text
    return ""

def all_nested(fi):
    for n in fi.nested:
        yield n
        yield from all_nested(n)

def strip_comment(ln):
    # remove ; comment not inside a string
    out, instr, esc = [], False, False
    for ch in ln:
        if instr:
            out.append(ch)
            if esc: esc = False
            elif ch == "\\": esc = True
            elif ch == '"': instr = False
        else:
            if ch == ";": break
            out.append(ch)
            if ch == '"': instr = True
    return "".join(out)

def strip_comment_and_strings(ln):
    s = strip_comment(ln)
    return re.sub(r'"(\\.|[^"\\])*"', '""', s)

# --- dispatch-table helpers
def find_clauses(node):
    res = []
    def rec(n):
        if n.kind != "list": return
        if n.head() == "COND":
            for cl in n.kids[1:]:
                if cl.kind == "list" and cl.kids and cl.kids[0].kind == "list" and cl.kids[0].head() == "AND":
                    res.append(cl)
        for k in n.kids: rec(k)
    rec(node)
    return res

def parse_clause(cl):
    test = cl.kids[0]
    H = W = None
    has_tempdata = False
    for t in test.kids[1:]:
        if t.kind == "list" and t.head() == "EQUAL" and len(t.kids) >= 3:
            v = t.kids[1].val.upper() if t.kids[1].kind == "atom" else ""
            try: num = float(t.kids[2].val)
            except Exception: continue
            if v == "VIEWPORTHIGHT": H = num
            elif v == "VIEWPORTWIDTH": W = num
    if H is None or W is None: return None
    text = repr(cl)
    pm = re.search(r'SBUGOPDCONFIGSSETINGLAYOUT "(\d+)"', text, re.I)
    if not pm: return None
    rot = None
    m = re.search(r'\(IF \(= SBUGOPDANGLEOFROTATION "ac0degrees"\) (\w+) (\w+)\)', text, re.I)
    if m:
        rot = "same" if (m.group(1).lower() == "ac0degrees") else "swapped"
    return {"H": H, "W": W, "pos": pm.group(1), "rot": rot, "line": cl.line}

def parse_ini_strings(src):
    recs = []
    for m in re.finditer(r'"(\d{2,3})\\n(PDF|PLOT)\\n([^\\"]+)\\n(\d+)\\n(\d+)\\n([^\\"]+)\\n([^\\"]+)\\n(ac\d+degrees)"', src):
        recs.append({"pos": m.group(1), "type": m.group(2), "name": m.group(3), "s1": float(m.group(4)),
                     "s2": float(m.group(5)), "plotter": m.group(6), "media": m.group(7), "rot": m.group(8),
                     "line": src.count("\n", 0, m.start()) + 1})
    return recs

if __name__ == "__main__":
    main()
