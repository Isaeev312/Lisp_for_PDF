;;; ============================================================================
;;; Sbugo_addlay_insert_tests.lsp   (ASCII only - safe for any code page)
;;; Pure-LISP regression tests for Sbugo_addlay_insert.lsp v5.08.0.
;;; No dialogs, no layouts, no viewports are touched, so it runs both in a normal
;;; AutoCAD session and in accoreconsole.
;;;
;;; Usage:   (load "Sbugo_addlay_insert_tests.lsp")   then   (sbt:run)
;;; Output:  command line  +  <dir>/sbugo_tests_result.txt
;;;          PASS  - expectation met          FAIL - regression / unexpected
;;;          BUG   - a defect of the PLUGIN, found by running plugin code; turns FIXED once repaired
;;;          INFO  - a measured AutoLISP fact or a snapshot of the source that the plugin relies on.
;;;                  Never changes after a fix of the plugin; kept as documentation/evidence.
;;;          NOTE  - observation, not judged as a defect
;;;          SKIP  - test needs the headless environment (it would touch your real INI in TEMP)
;;; The A3x4 rotation asymmetry (line 1129) is a STATIC finding: re-check it with
;;; lsp_lint.py (section 8a) after editing Sbugo-set-layout-type-printer.
;;; Set  (setq sbt:dir "C:/some/dir/")  BEFORE loading to choose the output folder (default: TEMP).
;;; ============================================================================
(vl-load-com)
(if (not (and (boundp 'sbt:dir) sbt:dir))
  (setq sbt:dir (strcat (vl-string-right-trim "\\" (vl-string-right-trim "/" (getenv "TEMP"))) "/")))
(setq sbt:n-pass 0 sbt:n-fail 0 sbt:n-bug 0 sbt:n-skip 0 sbt:lines nil)

(defun sbt:log (s) (setq sbt:lines (cons s sbt:lines)) (princ (strcat "\n" s)) (princ))
(defun sbt:assert (name ok)
  (if ok
    (progn (setq sbt:n-pass (1+ sbt:n-pass)) (sbt:log (strcat "PASS  " name)))
    (progn (setq sbt:n-fail (1+ sbt:n-fail)) (sbt:log (strcat "FAIL  " name))))
  (princ))
(defun sbt:bug (name ok note)
  (if ok
    (sbt:log (strcat "FIXED " name))
    (progn (setq sbt:n-bug (1+ sbt:n-bug)) (sbt:log (strcat "BUG   " name "  ::  " note))))
  (princ))
(defun sbt:skip (name why) (setq sbt:n-skip (1+ sbt:n-skip)) (sbt:log (strcat "SKIP  " name "  ::  " why)) (princ))
(defun sbt:info (name measured note) (sbt:log (strcat "INFO  " name "  ->  " measured "  ::  " note)) (princ))
(defun sbt:note (name note) (sbt:log (strcat "NOTE  " name "  ::  " note)) (princ))
(defun sbt:try (name fn / r)
  (setq r (vl-catch-all-apply fn nil))
  (if (vl-catch-all-error-p r)
    (progn (setq sbt:n-fail (1+ sbt:n-fail))
           (sbt:log (strcat "FAIL  " name "  ::  exception: " (vl-catch-all-error-message r)))
           nil)
    r))
(defun sbt:headless-p () (null (vlax-get-acad-object)))
(defun sbt:count-lines (path / f n)
  (setq n 0 f (open path "r"))
  (if f (progn (while (read-line f) (setq n (1+ n))) (close f)))
  n)
(defun sbt:range (a b / r) (while (<= a b) (setq r (cons a r) a (1+ a))) (reverse r))
(defun sbt:key (i) (if (< i 10) (strcat "0" (itoa i)) (itoa i)))
;; remember the built-in ALERT so that a stub used by the tests can always be undone
(if (not (boundp 'sbt:alert-orig)) (setq sbt:alert-orig alert))

;;; ---------------------------------------------------------------------------
;;; Reference implementations of PROPOSED fixes (see the review). They are tested here
;;; so that they can be dropped into the plugin with confidence.
;;; ---------------------------------------------------------------------------
;; data-driven replacement of the two 54-clause COND tables.
;; tbl = list of (pos s1 s2), s1 < s2.  Returns (pos landscape-p) or nil.
(defun sbugo:find-format (h w tbl / hit)
  (foreach r tbl
    (if (null hit)
      (cond ((and (equal h (cadr r) 1.1) (equal w (caddr r) 1.1)) (setq hit (list (car r) T)))
            ((and (equal h (caddr r) 1.1) (equal w (cadr r) 1.1)) (setq hit (list (car r) nil))))))
  hit)
;; rotation rule: landscape => as in INI, portrait => swapped (symmetrical for ALL formats)
(defun sbugo:rotation (ini-rot landscape-p)
  (if (eq (= ini-rot "ac0degrees") (not (not landscape-p))) 0 1))
;; symmetrical rounding instead of FIX (truncation)
(defun sbugo:iround (x) (fix (+ x (if (minusp x) -0.5 0.5))))
;; case-insensitive membership (layout names / printer names are case-insensitive in AutoCAD)
(defun sbugo:member-ci (s lst) (member (strcase s) (mapcar 'strcase lst)))

;;; ---------------------------------------------------------------------------
;;; Test data generated from the plugin source (Sbugo-set-layout-type-printer COND, lines 1102-1155)
;;; each row: (ViewportHight ViewportWidth position rotation-clause)  rotation-clause: 0 = as INI, 1 = swapped
;;; ---------------------------------------------------------------------------
(setq sbt:cond-table
  '(
    (210.0 297.0 "01" 0)   ; line 1102
    (297.0 210.0 "01" 1)   ; line 1103
    (297.0 420.0 "02" 0)   ; line 1104
    (420.0 297.0 "02" 1)   ; line 1105
    (420.0 594.0 "03" 0)   ; line 1106
    (594.0 420.0 "03" 1)   ; line 1107
    (594.0 841.0 "04" 0)   ; line 1108
    (841.0 594.0 "04" 1)   ; line 1109
    (841.0 1189.0 "05" 0)   ; line 1110
    (1189.0 841.0 "05" 1)   ; line 1111
    (297.0 630.0 "06" 0)   ; line 1112
    (630.0 297.0 "06" 1)   ; line 1113
    (297.0 841.0 "07" 0)   ; line 1114
    (841.0 297.0 "07" 1)   ; line 1115
    (297.0 1051.0 "08" 0)   ; line 1116
    (1051.0 297.0 "08" 1)   ; line 1117
    (297.0 1261.0 "09" 0)   ; line 1118
    (1261.0 297.0 "09" 1)   ; line 1119
    (297.0 1471.0 "10" 0)   ; line 1120
    (1471.0 297.0 "10" 1)   ; line 1121
    (297.0 1682.0 "11" 0)   ; line 1122
    (1682.0 297.0 "11" 1)   ; line 1123
    (297.0 1892.0 "12" 0)   ; line 1124
    (1892.0 297.0 "12" 1)   ; line 1125
    (420.0 891.0 "13" 0)   ; line 1126
    (891.0 420.0 "13" 1)   ; line 1127
    (420.0 1189.0 "14" 0)   ; line 1128
    (1189.0 420.0 "14" 0)   ; line 1129
    (420.0 1486.0 "15" 0)   ; line 1130
    (1486.0 420.0 "15" 1)   ; line 1131
    (420.0 1783.0 "16" 0)   ; line 1132
    (1783.0 420.0 "16" 1)   ; line 1133
    (420.0 2080.0 "17" 0)   ; line 1134
    (2080.0 420.0 "17" 1)   ; line 1135
    (594.0 1261.0 "18" 0)   ; line 1136
    (1261.0 594.0 "18" 1)   ; line 1137
    (594.0 1682.0 "19" 0)   ; line 1138
    (1682.0 594.0 "19" 1)   ; line 1139
    (594.0 2102.0 "20" 0)   ; line 1140
    (2102.0 594.0 "20" 1)   ; line 1141
    (841.0 1783.0 "21" 0)   ; line 1142
    (1783.0 841.0 "21" 1)   ; line 1143
    (841.0 2378.0 "22" 0)   ; line 1144
    (2378.0 841.0 "22" 1)   ; line 1145
    (1189.0 1682.0 "23" 0)   ; line 1146
    (1682.0 1189.0 "23" 1)   ; line 1147
    (1189.0 2523.0 "24" 0)   ; line 1148
    (2523.0 1189.0 "24" 1)   ; line 1149
    (594.0 1050.0 "25" 0)   ; line 1150
    (1050.0 594.0 "25" 1)   ; line 1151
    (841.0 2972.0 "26" 0)   ; line 1152
    (2972.0 841.0 "26" 1)   ; line 1153
    (841.0 3567.0 "27" 0)   ; line 1154
    (3567.0 841.0 "27" 1)   ; line 1155
  ))

;;; ---------------------------------------------------------------------------
;;; test groups
;;; ---------------------------------------------------------------------------
(defun sbt:t-load ()
  (foreach s '(SBUGO-ADDLAY SBUGOPDLOADCONFIGSBUGOADDLAYINSERTINI SBUGOPDNEWCONFIGSBUGOADDLAYINSERTINI
               SBUGOPDCONFIGSSETINGLAYOUT MINELEMENT MAXELEMENT SBUGOPD-SPECIAL-CHARACTERS-NAME-NEW-BLOCK
               SBUGO-SET-LAYOUT-TYPE-PRINTER SBUGO-SET-SELECT-TYPE-PRINTER SBUGO-ADDLAY-RETURNS-LAYER-STATUS
               SBUGOPDMXM SBUGOPDMXV SBUGOPDVXS SBUGOPDTRP SBUGOPDPCS2WCS SBUGOPDVPOINSERT C:SBADDLAY)
    (sbt:assert (strcat "defined: " (vl-symbol-name s)) (boundp s)))
  (sbt:assert "global flag G_SbugoPDButtonSplitSheetIntoSeveralSheet is T after load"
              (eq G_SbugoPDButtonSplitSheetIntoSeveralSheet T))
  ;; the nested defun at lines 551/592 must not be what the global really is at load time
  (sbt:assert "Sbugo-addlay-Error-read exists after load" (boundp 'SBUGO-ADDLAY-ERROR-READ)))

(defun sbt:t-minmax ()
  (sbt:assert "MinElement ints" (= (MinElement '(3 1 2 1)) 1))
  (sbt:assert "MaxElement ints" (= (MaxElement '(3 1 2 1)) 3))
  (sbt:assert "MinElement reals/neg" (equal (MinElement '(0.5 -2.25 7.0)) -2.25 1e-9))
  (sbt:assert "MaxElement reals/neg" (equal (MaxElement '(0.5 -2.25 7.0)) 7.0 1e-9))
  (sbt:assert "MinElement single" (= (MinElement '(42)) 42))
  (sbt:assert "MinElement of empty list is nil (caller must guard)" (null (MinElement nil))))

(defun sbt:t-special-chars ( / old r)
  (setq sbt:alerts nil)
  (defun alert (s) (setq sbt:alerts (cons s sbt:alerts)) nil)
  (setq old (getvar "CMDECHO"))
  (setvar "CMDECHO" 1)
  (sbt:assert "special: nil -> empty string" (= (SbugoPD-special-characters-name-new-block nil) ""))
  (sbt:assert "special: clean text unchanged" (= (SbugoPD-special-characters-name-new-block "Sheet_") "Sheet_"))
  (sbt:assert "special: no alert for clean text" (null sbt:alerts))
  (sbt:assert "special: left spaces trimmed" (= (SbugoPD-special-characters-name-new-block "   abc") "abc"))
  (sbt:assert "special: slash/backslash removed" (= (SbugoPD-special-characters-name-new-block "A/B\\C") "ABC"))
  (sbt:assert "special: alert shown for illegal char" (= (length sbt:alerts) 1))
  (setq sbt:alerts nil)
  (setq r (SbugoPD-special-characters-name-new-block "<>/\\?\":;*|,=`x"))
  (sbt:assert "special: all 13 illegal chars removed, 'x' kept" (= r "x"))
  (sbt:assert "special: ONE alert even with many illegal chars" (= (length sbt:alerts) 1))
  (sbt:assert "special: repeated illegal chars removed (A//B///C)" (= (SbugoPD-special-characters-name-new-block "A//B///C") "ABC"))
  (sbt:assert "special: CMDECHO restored" (= (getvar "CMDECHO") 1))
  (sbt:note "special: trailing spaces are kept (line 2942 trims on the left only)"
            (strcat "result=[" (SbugoPD-special-characters-name-new-block "abc  ") "]"))
  (setvar "CMDECHO" old)
  (setq alert sbt:alert-orig)
  (sbt:assert "alert restored to the built-in" (= (type alert) 'SUBR)))

(defun sbt:t-matrix ( / m)
  (setq m '((1.0 2.0) (3.0 4.0)))
  (sbt:assert "trp" (equal (sbugoPDtrp m) '((1.0 3.0) (2.0 4.0)) 1e-9))
  (sbt:assert "mxv" (equal (sbugoPDmxv m '(1.0 1.0)) '(3.0 7.0) 1e-9))
  (sbt:assert "mxm with identity" (equal (sbugoPDmxm m '((1.0 0.0) (0.0 1.0))) m 1e-9))
  (sbt:assert "mxm 2x2" (equal (sbugoPDmxm m m) '((7.0 10.0) (15.0 22.0)) 1e-9))
  (sbt:assert "vxs" (equal (sbugoPDvxs '(1.0 -2.0 3.0) 2.0) '(2.0 -4.0 6.0) 1e-9)))

(defun sbt:t-ini ( / ini f hdr lst bad pos rec)
  (setq ini (strcat sbt:dir "Sbugo_addlay_insert.ini"))
  (if (not (sbt:headless-p))
    (sbt:skip "INI generator/reader round trip"
              "GUI session: the generator always writes to YOUR TEMP\\Sbugo_addlay_insert.ini - run this group headless")
    (progn
      ;; stubs: redirect the plugin's TEMP lookup to sbt:dir (headless only, process ends afterwards)
      (defun vlax-get-acad-object () 'ACADOBJ)
      (defun vla-get-preferences (a) 'PREFS)
      (defun vla-get-files (p) 'FILES)
      (defun vla-get-tempfilepath (f) (vl-string-translate "/" "\\" sbt:dir))
      (if (findfile ini) (vl-file-delete ini))
      (setq G_SbugoPDDefaultSelectIniFile ini)
      (sbt:try "INI generator runs" (function (lambda () (SBUGOPDNEWCONFIGSBUGOADDLAYINSERTINI))))
      (sbt:assert "INI file created" (findfile ini))
      (sbt:assert "INI line count = 55 header + 54*9 + 1 trailing empty = 542" (= (sbt:count-lines ini) 542))
      (sbt:try "INI loader runs" (function (lambda () (SBUGOPDLOADCONFIGSBUGOADDLAYINSERTINI))))
      (sbt:assert "loader produced 54*8 = 432 elements" (= (length sbugoPDListOfSheetSettings) 432))
      (sbt:assert "loader: no nil elements" (not (member nil sbugoPDListOfSheetSettings)))
      ;; every position key must occur exactly once, otherwise MEMBER-based lookup would hit a wrong record
      (setq bad nil)
      (foreach k (append (mapcar 'sbt:key (sbt:range 1 27)) (mapcar '(lambda (i) (itoa (+ 100 i))) (sbt:range 1 27)))
        (if (/= 1 (length (vl-remove-if-not '(lambda (x) (= x k)) sbugoPDListOfSheetSettings))) (setq bad (cons k bad))))
      (sbt:assert "position keys 01-27 / 101-127 are unique in the settings list" (null bad))
      ;; PDF rows
      (setq SbugoPDTypephysicalPrinter 0 bad nil)
      (foreach i (sbt:range 1 27)
        (SBUGOPDCONFIGSSETINGLAYOUT (sbt:key i))
        (if (not (and sbugoPDPlotNameFile sbugoPDFormToLayout sbugoPDAngleOfRotation
                      (= sbugoPDPlotNameFile "DWG To PDF_all_formats.pc3")
                      (wcmatch sbugoPDFormToLayout "UserDefinedMetric*")
                      (member sbugoPDAngleOfRotation '("ac0degrees" "ac90degrees"))))
          (setq bad (cons i bad))))
      (sbt:assert "PDF rows 01-27: plotter/media/rotation resolved" (null bad))
      ;; PLOT rows
      (setq SbugoPDTypephysicalPrinter 1 bad nil)
      (foreach i (sbt:range 1 27)
        (SBUGOPDCONFIGSSETINGLAYOUT (sbt:key i))
        (if (not (and sbugoPDPlotNameFile (wcmatch (strcase sbugoPDPlotNameFile) "*.PC3") sbugoPDFormToLayout
                      (member sbugoPDAngleOfRotation '("ac0degrees" "ac90degrees"))))
          (setq bad (cons i bad))))
      (sbt:assert "PLOT rows 101-127: plotter/media/rotation resolved" (null bad))
      (setq SbugoPDTypephysicalPrinter 0)
      ;; A4 sizes stored in INI (fields 4 and 5 of record 01)
      (setq rec (member "01" sbugoPDListOfSheetSettings))
      (sbt:bug "INI record 01 (A4) says 210 x 297"
               (and (= (nth 3 rec) "210") (= (nth 4 rec) "297"))
               (strcat "INI says " (nth 3 rec) " x " (nth 4 rec) " (typo 291 in lines 88 and 142)"))
      ;; robustness: truncated INI must not raise exceptions inside the loader
      (setq f (open (strcat sbt:dir "trunc.ini") "w"))
      (repeat 60 (write-line "x" f))
      (close f)
      (setq G_SbugoPDDefaultSelectIniFile (strcat sbt:dir "trunc.ini"))
      (sbt:try "loader survives a truncated INI (no exception)" (function (lambda () (SBUGOPDLOADCONFIGSBUGOADDLAYINSERTINI))))
      (sbt:bug "loader detects a truncated/corrupted INI (should warn, not return a list of nils)"
               (not (member nil sbugoPDListOfSheetSettings))
               "reads blindly 55+54*9 lines; a truncated or hand-edited INI silently yields nil elements")
      (setq G_SbugoPDDefaultSelectIniFile ini)
      (SBUGOPDLOADCONFIGSBUGOADDLAYINSERTINI))))

(defun sbt:t-dispatch ( / ini-tbl row ok bad h w r)
  ;; table built from the INI records (A4 corrected to 297, i.e. what the INI SHOULD say)
  (setq ini-tbl '(("01" 210.0 297.0) ("02" 297.0 420.0) ("03" 420.0 594.0) ("04" 594.0 841.0) ("05" 841.0 1189.0)
                  ("06" 297.0 630.0) ("07" 297.0 841.0) ("08" 297.0 1051.0) ("09" 297.0 1261.0) ("10" 297.0 1471.0)
                  ("11" 297.0 1682.0) ("12" 297.0 1892.0) ("13" 420.0 891.0) ("14" 420.0 1189.0) ("15" 420.0 1486.0)
                  ("16" 420.0 1783.0) ("17" 420.0 2080.0) ("18" 594.0 1261.0) ("19" 594.0 1682.0) ("20" 594.0 2102.0)
                  ("21" 841.0 1783.0) ("22" 841.0 2378.0) ("23" 1189.0 1682.0) ("24" 1189.0 2523.0) ("25" 594.0 1050.0)
                  ("26" 841.0 2972.0) ("27" 841.0 3567.0)))
  (sbt:assert "cond-table generated from source has 54 rows" (= (length sbt:cond-table) 54))
  (setq bad nil)
  (foreach row sbt:cond-table
    (setq h (nth 0 row) w (nth 1 row) r (sbugo:find-format h w ini-tbl))
    (if (not (and r (= (car r) (nth 2 row)))) (setq bad (cons row bad))))
  (sbt:assert "sbugo:find-format reproduces position of ALL 54 clauses of the old COND" (null bad))
  ;; rotation: the old COND is asymmetric for exactly one clause; the data-driven rule is symmetric
  (setq bad nil)
  (foreach row sbt:cond-table
    (setq h (nth 0 row) w (nth 1 row) r (sbugo:find-format h w ini-tbl))
    (if r
      (progn
        (if (/= (nth 3 row) (if (cadr r) 0 1)) (setq bad (cons (list (nth 2 row) h w) bad))))))
  (sbt:info "SNAPSHOT of the old COND (v5.08.0, extracted when this file was generated): rotation symmetry"
            (if bad (strcat "ASYMMETRIC (pos H W): " (vl-prin1-to-string bad)) "symmetric")
            "static finding - portrait A3x4 gets the landscape rotation (line 1129); re-check with lsp_lint.py section 8a")
  (sbt:assert "new rule: portrait swaps rotation (ac0 -> 1)" (= (sbugo:rotation "ac0degrees" nil) 1))
  (sbt:assert "new rule: landscape keeps rotation (ac0 -> 0)" (= (sbugo:rotation "ac0degrees" T) 0))
  (sbt:assert "new rule: INI ac90 + landscape -> 1" (= (sbugo:rotation "ac90degrees" T) 1))
  (sbt:assert "new rule: INI ac90 + portrait -> 0" (= (sbugo:rotation "ac90degrees" nil) 0))
  ;; tolerance behaviour
  (sbt:assert "find-format: +1.0 mm jitter accepted" (equal (car (sbugo:find-format 421.0 298.0 ini-tbl)) "02"))
  (sbt:assert "find-format: 1.3 mm off rejected" (null (sbugo:find-format 421.3 297.0 ini-tbl)))
  (sbt:assert "find-format: custom format added to the table only (A5 148x210)"
              (equal (car (sbugo:find-format 148.0 210.0 (cons '("A5" 148.0 210.0) ini-tbl))) "A5"))
  ;; FIX truncation makes the +-1.1 tolerance asymmetric - show it
  (sbt:info "FIX + (equal x 297 1.1): is 295.9 accepted / is 298.9 accepted"
            (strcat (if (equal (fix 295.9) 297 1.1) "T" "nil") " / " (if (equal (fix 298.9) 297 1.1) "T" "nil"))
            "plugin lines 853-854 use FIX: effective window is [296,299) instead of [295.9,298.1] (asymmetric)")
  (sbt:assert "sbugo:iround is symmetric" (and (= (sbugo:iround 296.4) 296) (= (sbugo:iround 296.6) 297) (= (sbugo:iround 296.9999999) 297)))
  (sbt:assert "sbugo:iround: 295.9->296, 298.9->299"
              (and (= (sbugo:iround 295.9) 296) (= (sbugo:iround 298.9) 299))))

(defun sbt:t-language ( / pts a b)
  ;; facts the plugin relies on, measured in THIS AutoCAD
  (sbt:info "(wcmatch \"ACAD.CTB\" \"*.ctb\")"
            (if (wcmatch "ACAD.CTB" "*.ctb") "T (case-insensitive)" "nil (case-SENSITIVE)")
            "plugin lines 364, 2046, 2049: an upper-case style name would be silently replaced by '' (no plot style)")
  (sbt:assert "workaround: strcase before wcmatch works" (wcmatch (strcase "ACAD.CTB") "*.CTB"))
  (sbt:info "scale names built by (strcat \"1:\" (rtos s 2 0)) for s = 0.5 / 2.5 / 0.4"
            (strcat (strcat "1:" (rtos 0.5 2 0)) " / " (strcat "1:" (rtos 2.5 2 0)) " / " (strcat "1:" (rtos 0.4 2 0)))
            "plugin lines 782-792: fractional scales get wrong or invalid names (expected 2:1 / 1:2.5 / 2.5:1)")
  ;; vl-sort on frames
  (setq a (list '(0.0 0.0 0.0) '(10.0 10.0 0.0)) b (list '(0.0 20.0 0.0) '(10.0 30.0 0.0)))
  (sbt:assert "vl-sort keeps distinct frames that share an X coordinate"
              (= (length (vl-sort (list a b) (function (lambda (p q) (< (caar p) (caar q)))))) 2))
  (sbt:info "(length (vl-sort (list a a b) <by X>)) for two IDENTICAL boxes a and one box b"
            (itoa (length (vl-sort (list a a b) (function (lambda (p q) (< (caar p) (caar q)))))))
            "plugin lines 498-499: if 2 (not 3), a duplicated frame object shortens Points and (nth j Points) becomes nil for the last sheet")
  (sbt:assert "vl-position returns the FIRST of identical boxes (so the 2nd frame is processed as the 1st)"
              (= (vl-position a (list a a b)) 0))
  (sbt:assert "foreach restores the loop variable (so 'item' is not a leak)"
              (progn (setq sbt:v 'orig) (foreach sbt:v '(1 2) nil) (eq sbt:v 'orig)))
  (defun sbt:outer ( / ) (defun sbt:inner (x) x) 1)
  (sbt:outer)
  (sbt:assert "nested defun becomes a global function after the outer one ran" (boundp 'sbt:inner))
  (sbt:assert "comparison with nil does not raise (> nil 0)" (not (vl-catch-all-error-p (vl-catch-all-apply '> '(nil 0))))))

;;; ---------------------------------------------------------------------------
(defun sbt:run ( / f s)
  (setq sbt:n-pass 0 sbt:n-fail 0 sbt:n-bug 0 sbt:n-skip 0 sbt:lines nil)
  (if (not (boundp 'SBUGO-ADDLAY))
    (sbt:log "NOTE: plugin not loaded - (load \"Sbugo_addlay_insert.lsp\") first"))
  (foreach g '(sbt:t-load sbt:t-minmax sbt:t-special-chars sbt:t-matrix sbt:t-ini sbt:t-dispatch sbt:t-language)
    (sbt:log (strcat "---- " (vl-symbol-name g)))
    (sbt:try (vl-symbol-name g) (eval (list 'function g))))
  (setq alert sbt:alert-orig)   ; never leave a stubbed ALERT behind, even after an exception
  (setq s (strcat "SUMMARY  pass=" (itoa sbt:n-pass) "  fail=" (itoa sbt:n-fail) "  known-bugs-present=" (itoa sbt:n-bug) "  skipped=" (itoa sbt:n-skip)))
  (sbt:log s)
  (setq f (open (strcat sbt:dir "sbugo_tests_result.txt") "w"))
  (if f (progn (foreach l (reverse sbt:lines) (write-line l f)) (close f)))
  (princ))
(princ "\nSbugo tests loaded. Run:  (sbt:run)")
(princ)
