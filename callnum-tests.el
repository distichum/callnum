;;; callnum-tests.el --- Tests for callnum.el -*- lexical-binding: t; -*-

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; ERT test suite for callnum.el.
;;
;; The ordered sample files `lc-sample.txt' and `sudoc-sample.txt' are
;; the ground-truth oracle: each is split into groups of call numbers
;; that are already in correct shelf order.  At load time we parse those
;; files ONCE into `callnum-test-lc-groups' and `callnum-test-sudoc-groups'
;; (we never reparse during the test run).  The core property tested is:
;;
;;   pad each entry -> the resulting sort keys are non-decreasing in the
;;   file's order, and entries marked "(same as above/below)" produce
;;   EQUAL keys.
;;
;; Annotations understood in the sample files:
;;   "(same as above)"            -> key must equal the previous entry
;;   "(same as below)"            -> key must equal the next entry
;;   "(not programming around this)" -> entry skipped (known out of scope)
;;   ";; ..."                      -> trailing comment, stripped
;;   "(... free text ...)"        -> other notes, stripped, entry kept
;;
;; Run non-interactively with:
;;   emacs -Q --batch -L . -l callnum-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'subr-x)
(require 'cl-lib)

;; Locate and load callnum.el relative to this file.
(defconst callnum-test--dir
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "Directory containing this test file and the sample data files.")

(add-to-list 'load-path callnum-test--dir)
(require 'callnum)


;;; Sample-file parser

(defun callnum-test--parse-line (raw)
  "Parse one RAW line into a plist (:callnum STRING :tag TAG), or nil.

TAG is one of `normal', `equal-above', `equal-below', `skip'.  Returns
nil for blank lines.  Leading whitespace in the call number is preserved
because in the SuDoc samples it is a deliberate normalization test."
  (let* ((body (replace-regexp-in-string " *;;.*\\'" "" raw)) ; drop ;; comment
         (annot ""))
    ;; Pull a trailing parenthesised annotation, if present.
    (when (string-match " *(\\([^()]*\\))[ \t]*\\'" body)
      (setq annot (match-string 1 body)
            body  (substring body 0 (match-beginning 0))))
    (setq body (replace-regexp-in-string "[ \t]+\\'" "" body)) ; drop trailing ws
    (if (string-blank-p body)
        nil
      (list :callnum body
            :tag (cond
                  ((string-match-p "not programming" annot) 'skip)
                  ((string-match-p "same as above"   annot) 'equal-above)
                  ((string-match-p "same as below"   annot) 'equal-below)
                  (t 'normal))))))

(defun callnum-test--parse-file (file)
  "Parse FILE into a list of (GROUP-NAME . ENTRIES).

ENTRIES is an ordered list of plists from `callnum-test--parse-line'.
Lines before the first `- group header -' (e.g. the title) are ignored."
  (let ((groups nil) (cur nil) (cur-entries nil) (started nil))
    (dolist (raw (split-string
                  (with-temp-buffer (insert-file-contents file) (buffer-string))
                  "\n"))
      (let ((trimmed (string-trim raw)))
        (cond
         ;; Group header:  - Some Name -
         ((string-match "\\`-+ *\\(.*?\\) *-+\\'" trimmed)
          (when cur (push (cons cur (nreverse cur-entries)) groups))
          (setq cur (match-string 1 trimmed) cur-entries nil started t))
         ;; Skip the title / anything before the first group, and blanks.
         ((or (not started) (string-empty-p trimmed)) nil)
         ;; Content line inside a group.
         (t (let ((entry (callnum-test--parse-line raw)))
              (when entry (push entry cur-entries)))))))
    (when cur (push (cons cur (nreverse cur-entries)) groups))
    (nreverse groups)))

;; Parse the data files ONCE, at load time.
(defvar callnum-test-lc-groups
  (callnum-test--parse-file (expand-file-name "lc-sample.txt" callnum-test--dir))
  "Ordered LC sample groups, parsed once from lc-sample.txt.")

(defvar callnum-test-sudoc-groups
  (callnum-test--parse-file (expand-file-name "sudoc-sample.txt" callnum-test--dir))
  "Ordered SuDoc sample groups, parsed once from sudoc-sample.txt.")

(defvar callnum-test-dewey-groups
  (callnum-test--parse-file (expand-file-name "dewey-sample.txt" callnum-test--dir))
  "Ordered Dewey sample groups, parsed once from dewey-sample.txt.")


;;; Sort-key functions (mirror the cl-flet bodies of the interactive commands)

(defun callnum-test-key-lc (cn)
  "LC sort key for CN, as produced by `callnum-lc-make-region-sortable'."
  (callnum-pad-concat (callnum-lc-all-parts cn) t))

(defun callnum-test-key-lc2 (cn)
  "LC sort key for CN, mirroring `callnum-lc-make-region-sortable'.
Uses the digit-aware specification with the malformed-input fallback."
  (callnum-lc-sort-key cn))

(defun callnum-test-key-lc-cutter (cn)
  "Sort key for a cutter-only fixture CN.
Cutter-only strings have no classification, so sort them under a constant
dummy class; relative order then reflects the cutter/spec portion only."
  (callnum-test-key-lc2 (concat "AA1 ." cn)))

(defun callnum-test-key-sudoc-clean (cn)
  "SuDoc sort key for CN, as produced by the -clean region command.
Upcased to match `callnum-act-on-region-by-line', which the interactive
commands route through."
  (let ((c (callnum-sudoc-correct-space
            (callnum-sudoc-eliminate-punctuation cn))))
    (upcase
     (callnum-pad-concat
      (callnum-named-alist
       (callnum-regex-result-list c callnum-sudoc-rx)
       callnum-sudoc-alist)))))

(defun callnum-test-key-dewey (cn)
  "Dewey sort key for CN, as produced by `callnum-dewey-make-region-sortable'.
Upcased to match `callnum-act-on-region-by-line', which the interactive
commands route through."
  (upcase
   (callnum-pad-concat
    (callnum-named-alist
     (callnum-regex-result-list cn callnum-dewey-rx)
     callnum-dewey-alist))))

(defun callnum-test--key-fn (scheme group-name)
  "Return the key function for SCHEME (`lc', `sudoc' or `dewey') and GROUP-NAME."
  (cond
   ((eq scheme 'sudoc) #'callnum-test-key-sudoc-clean)
   ((eq scheme 'dewey) #'callnum-test-key-dewey)
   ((string-match-p "Cutter number part only" group-name)
    #'callnum-test-key-lc-cutter)
   (t #'callnum-test-key-lc2)))           ; mirror the wired-in command


;;; Order-checking

(defun callnum-test--order-violations (entries key-fn)
  "Return a list of human-readable ordering violations for ENTRIES.

Keys (from KEY-FN) must be non-decreasing in list order.  Entries tagged
`equal-above', and the successors of entries tagged `equal-below', must
have keys EQUAL to that neighbor.  `skip' entries are ignored."
  (let ((prev nil) (violations nil))
    (dolist (e entries)
      (unless (eq (plist-get e :tag) 'skip)
        (let* ((cn   (plist-get e :callnum))
               (key  (funcall key-fn cn))
               (tag  (plist-get e :tag)))
          (when prev
            (let ((pcn (plist-get prev :callnum))
                  (pkey (plist-get prev :key))
                  (ptag (plist-get prev :tag)))
              (cond
               ((or (eq tag 'equal-above) (eq ptag 'equal-below))
                (unless (string= pkey key)
                  (push (format "EXPECTED EQUAL but keys differ:\n    %S => %S\n    %S => %S"
                                pcn pkey cn key)
                        violations)))
               ((string-lessp key pkey)
                (push (format "OUT OF ORDER (later sorts before earlier):\n    %S => %S\n    %S => %S"
                              pcn pkey cn key)
                      violations)))))
          (setq prev (list :callnum cn :tag tag :key key)))))
    (nreverse violations)))

(defun callnum-test--run-group (scheme groups group-name)
  "Return ordering violations for GROUP-NAME within GROUPS under SCHEME."
  (let ((grp (assoc group-name groups)))
    (unless grp
      (error "No group named %S; have %S" group-name (mapcar #'car groups)))
    (callnum-test--order-violations (cdr grp)
                                    (callnum-test--key-fn scheme group-name))))


;;; Order tests, generated once per group in each sample file

(defun callnum-test--slug (s)
  "Make a test-name fragment from group name S."
  (let ((x (downcase s)))
    (setq x (replace-regexp-in-string "[^a-z0-9]+" "-" x))
    (string-trim x "-+" "-+")))

(dolist (spec (list (cons 'lc    callnum-test-lc-groups)
                    (cons 'sudoc callnum-test-sudoc-groups)
                    (cons 'dewey callnum-test-dewey-groups)))
  (let ((scheme (car spec)) (groups (cdr spec)))
    (dolist (grp groups)
      (let* ((gname (car grp))
             (test-name (intern (format "callnum-test/%s/order/%s"
                                        scheme (callnum-test--slug gname)))))
        (eval
         `(ert-deftest ,test-name ()
            ,(format "Entries of group %S in the %s sample file must be in sorted order."
                     gname scheme)
            (let ((violations (callnum-test--run-group ',scheme
                                                       (cond
                                                        ((eq ',scheme 'lc) callnum-test-lc-groups)
                                                        ((eq ',scheme 'dewey) callnum-test-dewey-groups)
                                                        (t callnum-test-sudoc-groups))
                                                       ,gname)))
              (should (null violations))))
         t)))))


;;; Focused correctness and robustness tests

(ert-deftest callnum-test/sample-vars-distinct ()
  "The SuDoc and LC example constants are distinct and well-formed."
  (should (string-match-p ":" (car callnum-sudoc-examples)))      ; SuDoc has a colon
  (should (boundp 'callnum-lc-examples))
  (should (string-match-p "\\." (car callnum-lc-examples))))      ; LC sample

(ert-deftest callnum-test/normalize-no-period ()
  "Period-less LC numbers normalize correctly, independent of global match-data."
  ;; Set global match data first, to confirm parsing does not depend on it.
  (string-match "XYZ" "XYZ")
  (let ((key (callnum-test-key-lc "Z696 U5 H 1995")))
    (should (string-prefix-p "Z" key))     ; class recovered, not garbage
    (should (string-match-p "696" key))    ; caption integer present
    (should (string-match-p "U5" key)))    ; cutter present
  (should (equal (cadr (assoc "class" (callnum-lc-normalize-callnum "Z696 U5 H 1995")))
                 "Z")))

(ert-deftest callnum-test/decimal-cutter-order ()
  "Cutter numbers sort as decimal fractions: .A349 < .A35 < .A4 < .A45 < .A5."
  (let* ((nums '("QA76 .A5" "QA76 .A4" "QA76 .A45" "QA76 .A349" "QA76 .A35"))
         (want '("QA76 .A349" "QA76 .A35" "QA76 .A4" "QA76 .A45" "QA76 .A5"))
         (got (mapcar #'car
                      (sort (mapcar (lambda (cn) (cons cn (callnum-test-key-lc cn))) nums)
                            (lambda (a b) (string-lessp (cdr a) (cdr b)))))))
    (should (equal got want))))

(ert-deftest callnum-test/all-parts-pads-spec-digits ()
  "all-parts zero-pads spec digit groups so vol.3 sorts before vol.10."
  (let ((k3  (callnum-test-key-lc2 "M3 .V48 1983 Ser.I vol.3"))
        (k10 (callnum-test-key-lc2 "M3 .V48 1983 Ser.I vol.10")))
    (should (string-match-p "vol0003" k3))
    (should (string-match-p "vol0010" k10))
    (should (string-lessp k3 k10))))

(ert-deftest callnum-test/all-parts-no-crash-on-samples ()
  "all-parts + pad-concat must not error across all LC examples."
  (dolist (cn callnum-lc-examples)
    (should (stringp (callnum-test-key-lc2 cn)))))

(ert-deftest callnum-test/dewey-rx-alist-aligned ()
  "Dewey regex capture-group count must match the Dewey alist length."
  (should (= (regexp-opt-depth callnum-dewey-rx)
             (length callnum-dewey-alist))))

(ert-deftest callnum-test/dewey-spec-slot ()
  "A trailing Dewey specification lands in the `specification' slot."
  (let ((parts (callnum-named-alist
                (callnum-regex-result-list "535.6 L661c vol.2" callnum-dewey-rx)
                callnum-dewey-alist)))
    (should (equal (cadr (assoc "specification" parts)) "vol.2"))
    (should (null (cadr (assoc "cutter-three" parts))))))

(ert-deftest callnum-test/get-callnum-blank-line ()
  "`callnum-get-callnum-from-line' must not error on a blank line."
  (with-temp-buffer
    (insert "\n")
    (goto-char (point-min))
    (should (equal (callnum-get-callnum-from-line 1) ""))))

(ert-deftest callnum-test/find-invalid-flags-garbage ()
  "Unrecognized strings are flagged; real call numbers are not."
  (cl-flet ((verdict (cn)
              (if (cadr (assoc "class" (callnum-lc-all-parts cn))) 'valid 'invalid)))
    (should (eq (verdict "QA76.73 .L37 1999") 'valid))
    (should (eq (verdict "Z696 U5 H 1995") 'valid))
    (should (eq (verdict "!!!garbage!!!") 'invalid))))

;;; End-to-end tests for the `*-sort-region' commands

;; Unlike the order tests above, which check the key functions in
;; isolation, these drive the interactive `sort-subr'-based commands
;; through a real buffer.  They assert two properties at once: the lines
;; end up in the right order, AND the buffer text is only reordered --
;; no padded key is inserted (the whole point of the sort-region
;; commands versus `*-make-region-sortable').

(defun callnum-test--sort-buffer (command input field-num)
  "Insert INPUT lines, run COMMAND on the whole buffer, return result lines.

COMMAND is one of the `callnum-*-sort-region' commands.  FIELD-NUM is
passed as its field argument.  Region is inactive, so the command sorts
the whole buffer."
  (with-temp-buffer
    (dolist (line input) (insert line "\n"))
    (funcall command field-num (point-min) (point-max))
    (split-string (buffer-string) "\n" t)))

(ert-deftest callnum-test/sort-region/lc-end-to-end ()
  "`callnum-lc-sort-region' reorders lines by LC order without inserting keys."
  (should (equal (callnum-test--sort-buffer
                  #'callnum-lc-sort-region
                  '("QA76.5 .A1,gamma" "QA9 .C3,alpha" "QA76 .B2,beta")
                  1)
                 '("QA9 .C3,alpha" "QA76 .B2,beta" "QA76.5 .A1,gamma"))))

(ert-deftest callnum-test/sort-region/sudoc-end-to-end ()
  "`callnum-sudoc-sort-region' reorders lines by SuDoc order, text intact.

The first line carries a leading space, which makes the `bol'-anchored
SuDoc regex fail to match: without cleaning its key is empty and it would
sort to the very top.  Asserting it lands LAST proves the command routes
through `callnum-sudoc-sort-key-clean'.  (The detailed ordering of the
normalization variants is the sample file's job; see the `A 1.2:D 56'
group in sudoc-sample.txt.)"
  (should (equal (callnum-test--sort-buffer
                  #'callnum-sudoc-sort-region
                  '(" A 93.73:89,x" "A 13.2:T 73/4,y" "A 93.73:76,z")
                  1)
                 '("A 13.2:T 73/4,y" "A 93.73:76,z" " A 93.73:89,x"))))

(ert-deftest callnum-test/sort-region/dewey-end-to-end ()
  "`callnum-dewey-sort-region' reorders lines by Dewey order, text intact."
  (should (equal (callnum-test--sort-buffer
                  #'callnum-dewey-sort-region
                  '("535.6 L661c,p" "398.24 An22u 2010,q" "439.1 Se81c,r")
                  1)
                 '("398.24 An22u 2010,q" "439.1 Se81c,r" "535.6 L661c,p"))))

(ert-deftest callnum-test/sort-region/non-destructive ()
  "Sorting only permutes the input lines; it inserts no padded key text."
  (let* ((input '("QA76.5 .A1,gamma" "QA9 .C3,alpha" "QA76 .B2,beta"))
         (output (callnum-test--sort-buffer #'callnum-lc-sort-region input 1)))
    ;; Same multiset of lines, just reordered -- nothing added or altered.
    (should (equal (sort (copy-sequence output) #'string<)
                   (sort (copy-sequence input) #'string<)))
    ;; No padding-character artifacts (zero-pad, `!' / `+' markers) leaked in.
    (should-not (cl-some (lambda (l) (string-match-p "[!+]\\|00" l)) output))))

(ert-deftest callnum-test/sort-region/field-num ()
  "The call number is read from FIELD-NUM, not always field one."
  (should (equal (callnum-test--sort-buffer
                  #'callnum-lc-sort-region
                  '("gamma,QA76.5 .A1" "alpha,QA9 .C3" "beta,QA76 .B2")
                  2)
                 '("alpha,QA9 .C3" "beta,QA76 .B2" "gamma,QA76.5 .A1"))))

(ert-deftest callnum-test/sort-region/reverse ()
  "`callnum-sort-region-by-key' with REVERSE non-nil sorts descending."
  (with-temp-buffer
    (insert "QA76.5 .A1,gamma\nQA9 .C3,alpha\nQA76 .B2,beta\n")
    (callnum-sort-region-by-key #'callnum-lc-sort-key t 1 (point-min) (point-max))
    (should (equal (split-string (buffer-string) "\n" t)
                   '("QA76.5 .A1,gamma" "QA76 .B2,beta" "QA9 .C3,alpha")))))

(provide 'callnum-tests)
;;; callnum-tests.el ends here
