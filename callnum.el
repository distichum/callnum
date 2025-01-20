;;; callnum.el --- Library call number helper functions -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Joshua Lambert

;; Author: Joshua Lambert <jlambert@missouristate.edu>
;; Maintainer: Joshua Lambert <jlambert@missouristate.edu>
;; Created: 20 Dec 2024
;; Version: 0.2
;; Keywords: tools, convenience, sorting
;; URL: https://github.com/distichum/callnum
;; Package-Requires: ((emacs "25.2"))

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 'callnum.el' provides functions to create zero padded, sortable,
;; call numbers (shelf marks) for library classification systems. It
;; also contains a few call number normalization and correction
;; functions. This relates to book libraries, not code libraries.

;; Superintendent of Documents (SuDoc) Call Numbers:
;; S 1.71:34
;; D 101.2:R 57
;; Y 4.F 76/1:AN 2/4
;; A13.92:H88/AMDT./V.1

;; to

;; S00010071!0034,S 1.71:34
;; D01010002!R0057,D 101.2:R 57
;; Y0004F00760001!AN00020004,Y 4.F 76/1:AN 2/4
;; A130092!H88AMDTV0001,A13.92:H88/AMDT./V.1

;; Library of Congress Call (LC) Numbers:
;; E505.5 102nd.F57 1999
;; HB3717 1929.E37 2015
;; D769 .A533 vol.6,pt.1 vol.1
;; LC1044 .O38x Cat.6

;; E0000505.50000000102ND!F570000000001999,E505.5 102nd.F57 1999
;; HB003717*1929!E370000000002015,HB3717 1929.E37 2015
;; D0000769!A53300000000,D769 .A533 vol.6,pt.1 vol.1
;; LC001044!O38X00000000,LC1044 .O38x Cat.6

;; Emacs' SORT-LINES function will then correctly sort the call
;; numbers. callnum.el does not provide features for deleting that
;; padded string. There are many other Emacs functions that can do
;; that such as replace-regexp "^.*, *" -> "" or CSV mode's
;; (csv-kill-fields).

;;; Change Log:


;;; Code:
;;; Prerequisites
(require 'subr-x)
(require 'cl-lib)
(if (version< emacs-version "28.1")
    (require 'seq-25)
  (require 'seq))


;;; User variables
(defgroup callnum nil
  "Create sortable padded callnumber strings."
  :group 'matching
  :tag "callnum")

(defcustom callnum-separator ","
  "The separator used to identify the call number field.
A CSV file will use a comma. Change to tab or something else as
needed."
  :type 'string
  :group 'callnum
  :package-version '(callnum . "0.2"))

(defcustom callnum-field-num 1
  "The field number in which to find the call number.
This variable assumes the file is delimited with some specific
character or string of characters as defined in
CALLNUM-SEPARATOR. The first field on a line is field 1.

This is used as the default but you can generally use a prefix
argument to specify field number on the fly. See documentation
for CALLNUM-SUDOC-MAKE-REGION-SORTABLE and
CALLNUM-LC-MAKE-REGION-SORTABLE functions."
  :type 'integer
  :group 'callnum
  :package-version '(callnum . "0.2"))


;;; Helper functions
(defun callnum-regex-result-list (string regex)
  "Return the list of all regex matches from a string.

The result is a list which contains the string from each of the
match groups. If there are no explicit match groups specified,
then the whole matching string is returned. STRING is any string
to which a regex will be applied. REGEX is the applied regular
expression."
  (when string
    (let ((execute-regex (string-match regex string)) ;; Stores match data.
	  (n-matches (1- (/ (length (match-data)) 2))))
      (if execute-regex
	  (cdr (mapcar (lambda (i) (match-string i string)) ;; Retrieves match data.
		       (number-sequence 0 n-matches)))))))

(defun callnum-named-alist (callnum-part-list part-alist)
  "Create a named association list of call number parts.

The CALLNUM-PART-LIST is a list of call number parts from
CALLNUM-REGEX-RESULT-LIST. PART-ALIST should be one of the
defined variables named CALLNUM-*-ALIST where the asterisk is the
name of a library classification."
  (let* ((part-list callnum-part-list)
	 (pad-alist part-alist)
	 (named-alist nil))
    (while pad-alist
      (setq named-alist
	    (append named-alist
		    (list (list (car (car pad-alist))
				(pop part-list)
				(cadr (pop pad-alist)))))))
    (append named-alist (list (list "left-overs" (pop part-list))))))

(defun callnum-string-pad (str len char direction)
  "Pad a string with a specific character.

STR is the string to pad. LEN is the length of the final created
string, including padding. CHAR is the padding character.
DIRECTION determines whether to pad left or right. If DIRECTION
is t, pad left. If nil, pad right. When providing arguments for
CHAR, it must be preceeded by a '?' unless you know the Emacs
chararcter number and use that instead. If LEN is less than STR,
then it will automatically be changed to the length of STR."
  (let ((len2 (if (< len (length str))
		  (length str)
		len)))
    (if direction
	(store-substring (make-string len2 char) (- len2 (length str)) str)
      (store-substring (make-string len2 char) 0 str))))

(defun callnum-pad-concat (callnum-alist)
  "Pad the call number parts of a named alist.

CALLNUM-ALIST is the alist which comes from CALLNUM-NAMED-ALIST.
The result of this function is a padded string for one call
number."
  (let* ((call-alist callnum-alist)
	 (new-str nil)
	 (part nil)
	 (pad-spec nil))
    (while (and call-alist
		(not (string-equal (caar call-alist)
				   "specification")))
      (setq part (pop call-alist))
      (setq pad-spec (caddr part))
      (if (cadr part)
	  (setq new-str (concat new-str
				(callnum-string-pad (cadr part)
						    (car pad-spec)
						    (cadr pad-spec)
						    (caddr pad-spec))))))
    (setq new-str (concat new-str (cadar call-alist)))
    new-str))

(defun callnum--field-bounds (&optional field-num)
  "Find the start and end position of a field in a buffer.
This assumes the text is a csv or csv-like buffer. CSV-mode not
required.

FIELD-NUM is the field number."
  (catch 'no-sep
    (save-excursion
      (let ((start-end nil))
	(let* ((field-num2 (if (and field-num (> field-num 1))
			       field-num
			     1))
	       ;; Find the field-num2 minus one instance of the
	       ;; separator.
	       (before (if (> field-num2 1)
			   (search-forward callnum-separator
					   (line-end-position)
					   t
					   (- field-num2 1))
			 (line-beginning-position)))
 	       ;; Point has already moved to after the first
 	       ;; separater. Find one more.
 	       (after (search-forward callnum-separator
				      (line-end-position) t 1)))
	  ;; Find the field-num2 instance of the separator.
	  ;; (after (- (search-forward callnum-separator
	  ;; 			       (line-end-position) t 1)
	  ;; 	       1)))
	  (cond ((and before after)
		 (setq start-end (list before after)))
		((and (not before) after)
		 ;; This can happen if someone specifies a field
		 ;; larger than the number of separators.
		 (if (eq field-num2 1)
		     (setq start-end (list (line-beginning-position) after))
		   (throw 'no-sep
			  (prin1
			   (format "Incorrect separator(s) found on line %d."
				   (line-number-at-pos))))))
		((and before (not after))
		 (setq start-end (list before (line-end-position))))
		((and (not before) (not after))
		 (setq start-end (list (line-beginning-position)
				       (line-end-position))))))
	start-end))))

(defun callnum-toolong-error (&optional checker)
  "Message the user when separators are inconsistent.
This is only used internally in the ‘callnum--field-bounds’
function. CHECKER checks to see if something is wrong."
  (if checker checker
    (throw 'no-sep
	   (prin1 (format "Incorrect separator(s) found on line %d."
			  (line-number-at-pos))))))

(defun callnum-get-callnum-from-line (&optional field-num)
  "Return call number from a line of text in a csv file.
FIELD-NUM is the field number in which to find the call number."
  (interactive "*p")
  (let* ((call-bounds (callnum--field-bounds field-num))
	 (return-string (buffer-substring-no-properties (car call-bounds)
							(cadr call-bounds))))
    (if (string-equal callnum-separator
		      (substring return-string -1))
	(replace-regexp-in-string "\"" "" (substring return-string 0 -1))
      (replace-regexp-in-string "\"" "" return-string))))

(defun callnum-act-on-region-by-line (function-to-use &optional field-num beg end)
  "Perform a function on every line of the region.
FUNCTION-TO-USE should return a string. BEG and END will
typically be passed from another interactive function. FIELD-NUM
is the field number in which to find the call number."
  (let ((beg2 (if (region-active-p)
		  beg
		(line-beginning-position)))
	(end2 (if (region-active-p)
		  end
		(line-end-position))))
    (save-excursion
      (save-restriction
	(narrow-to-region beg2 end2)
	(goto-char (point-min))
	(while (not (eobp))
	  (goto-char (line-beginning-position))
          (insert (upcase (funcall function-to-use
				   (callnum-get-callnum-from-line field-num))))
	  (insert callnum-separator)
          (forward-line))))))

;; Benchmarking tests.
;; (benchmark-run (dotimes (i 10000)
;; 		 (callnum-sudoc-pad-concat
;; 		  (callnum-sudoc-divide sudoc-sample-callnum))))


;;; SuDoc functions
;; Terminology related to the United States Superintendent of
;; Documents comes from the following FDLP document.
;; https://www.fdlp.gov/cataloging-and-classification/classification-guidelines/class-stems

(defvar callnum-sudoc-alist
  (list
   (list "agency" (list 4 ?0 nil))
   (list "office" (list 4 ?0 nil))
   (list "cong-comm" (list 4 ?0 nil)) ;; Congressional Committee
   (list "sub-office" (list 4 ?0 t)) ;; Subordinate office
   (list "main-series" (list 4 ?0 t))
   (list "related-series" (list 4 ?0 t))
   (list "stem-other" (list 4 ?0 t))
   (list "stem-other2" (list 4 ?0 t))
   (list "delimiter" (list 0 ?! t))
   (list "suffix-part1" (list 8 ?0 t))
   (list "suffix-part2" (list 8 ?0 nil))
   (list "suffix-part3" (list 8 ?0 t))
   (list "suffix-part4" (list 8 ?0 nil))
   (list "suffix-part5" (list 8 ?0 t))
   (list "suffix-part6" (list 8 ?0 nil))
   (list "suffix-part7" (list 8 ?0 t))
   (list "suffix-part8" (list 8 ?0 t))
   (list "suffix-part9" (list 8 ?0 t)))
  "Specifies name, length, pad character and direction.
  Each item in the alist has a name in the car and a list of
  details in the cdr. List item one is an integer that directs up
  to N many characters to pad. DIRECTION determines whether to
  pad left or right. If DIRECTION is t, pad left. If nil, pad
  right. When providing arguments for CHAR, it must be preceeded
  by a '?' unless you know the Emacs chararcter number and use
  that instead. If LEN is less than STR, then it will
  automatically be changed to the length of STR.")

(defvar callnum-sudoc-rx
  (rx bol
      (group (or (** 1 4 alpha) (seq "9" digit)))
      (? (? (any blank "-./")) (group (** 1 4 digit)))
      (? (** 0 2 (any blank "-./")) (group (** 1 4 alpha)))
      (? (** 0 2 (any blank "-./")) (group (** 1 4 digit)))
      (? (** 0 2 (any blank "-./")) (group (** 1 4 digit)))
      (? (** 0 2 (any blank "-./")) (group (** 1 4 alpha)))
      (? (** 0 2 (any blank "-./")) (group (** 1 4 digit)))
      (? (** 0 2 (any blank "-./")) (group (** 1 4 digit)))
      (seq (? blank) (group ":"))
      (? (** 0 2 (any blank "-./")) (group (+ (any "(" alpha))))
      (? (** 0 2 (any blank "-./")) (group (+ (any "(" digit))))
      (? (** 0 2 (any blank "-./")) (group (+ (any "(" alpha))))
      (? (** 0 2 (any blank "-./")) (group (+ (any "(" digit))))
      (? (** 0 2 (any blank "-./")) (group (+ (any "(" alpha))))
      (? (** 0 2 (any blank "-./")) (group (+ (any "(" digit))))
      (? (** 0 2 (any blank "-./")) (group (+ (any "(" alpha))))
      (? (** 0 2 (any blank "-./")) (group (+ (any "(" digit))))
      (group (? ")")))
  "Regex that matches SuDoc parts.")

(defconst callnum-sudoc-examples
  (list "A 13.2:T 73/4" "A 93.2:N 95/3" "A 93.73:76" "A 93.73:89" "A 93.73/2:62" "C 13.58:7564" "C 13.58:7611" "HE 20.4002:AD 9/2" "HE 20.4002:AD 9/5" "HE 20.4002:F 94" "L 36.202:F 15/2" "L 36.202:F 15/2/980" "L 36.202:F 15/3" "Y 1.1/7:109-118" "Y 1.1/7:109-131" "Y 1.1/7:110-6" "Y 1.1/7:110-20" "Y 4.EC 7:C 73/7" "Y 4.EC 7:C 73/10" "Y 4.EC 7:S.HRG.110-646" "Y 4.EC 7:SA 9/2" "Y 4.EC 7:SCH 6" "Y 4.EC 7:SE 2")
  "A list of sample SuDoc call numbers.")

(defun callnum-sudoc-correct-space (callnum)
  "Corrects some incorrectly spaced call numbers.
A determination for incorrect comes from the FDLP SuDoc
classification guidelines. This function corrects two cases.
First, there must be a space when the call number string switches
from [:alpha:] to [:digit:] or [:digit:] to [:alpha:]. Second,
there should not be a space after the colon.

CALLNUM is a string representing a call number."
  (let* ((new-callstr (substring callnum 0 1)))
    (dotimes (x (1- (length callnum)))
      ;; Add spaces when an alpha string changes to digits or when
      ;; digits turn to alphas.
      (when (or (and (string-match "[[:alpha:]]" (substring callnum x (1+ x)))
		     (string-match "[[:digit:]]" (substring callnum (+ x 1) (+ x 2))))
		(and (string-match "[[:digit:]]" (substring callnum x (1+ x)))
		     (string-match "[[:alpha:]]" (substring callnum (+ x 1) (+ x 2)))))
	(setq new-callstr (concat new-callstr " ")))
      ;; Don't concat a space after the colon if there is one.
      (when (not (string-match ": " (substring callnum x (+ x 2))))
	(setq new-callstr (concat new-callstr (string (aref callnum (1+ x)))))))
    new-callstr))

(defun callnum-sudoc-eleminate-punctuation (callnum)
  "Replace extra spaces and punctuation."
  (let* ((callnum
	  (replace-regexp-in-string (rx (or (seq (? space) (any "-/.") (? space))
					    (seq blank blank (* blank))))
				    " "
				    callnum))
	 (callnum
	  (replace-regexp-in-string (rx string-start (* space))
				    ""
				    callnum)))
    callnum))

(defun callnum-sudoc-make-region-sortable (&optional field-num beg end)
  "Add a padded call number to each line in the region.

FIELD-NUM is the field number. A numeric prefix argument
specifies in which field the call numbers are located. With no
prefix argument, it assumes field one contains the call number.
Interactively, BEG and END are the region.

This function does not account for quoted CSV files, therefore
make sure to place the call number field before any field with a
comma. For example, if you have a CSV file with two columns, one
being the call number field and another being the title field,
place the call number field to the left of the title field.  The
function should work then. You can alternatively change the user
variable CALLNUM-SEPARATOR to a character that is not in any of
your fields, assuming that is in fact the separator in your file."
  (interactive "*p\nr")
  (cl-flet ((pad-callnum (callnum)
	      (callnum-pad-concat
	       (callnum-named-alist
		(callnum-regex-result-list callnum callnum-sudoc-rx)
		callnum-sudoc-alist))))
    (callnum-act-on-region-by-line #'pad-callnum field-num beg end)))

(defun callnum-sudoc-make-region-sortable-clean (&optional field-num beg end)
  "Add a padded call number to each line in the region.

FIELD-NUM is the field number. A numeric prefix argument
specifies in which field the call numbers are located. With no
prefix argument, it assumes field one contains the call number.
Interactively, BEG and END are the region.

This function does not account for quoted CSV files, therefore
make sure to place the call number field before any field with a
comma. For example, if you have a CSV file with two columns, one
being the call number field and another being the title field,
place the call number field to the left of the title field.  The
function should work then. You can alternatively change the user
variable CALLNUM-SEPARATOR to a character that is not in any of
your fields, assuming that is in fact the separator in your file."
  (interactive "*p\nr")
  (cl-flet ((pad-callnum (callnum)
	      (let ((callnum-cleaned
		     (callnum-sudoc-correct-space
		      (callnum-sudoc-eleminate-punctuation callnum))))
		(callnum-pad-concat
		 (callnum-named-alist
		  (callnum-regex-result-list callnum-cleaned callnum-sudoc-rx)
		  callnum-sudoc-alist)))))
    (callnum-act-on-region-by-line #'pad-callnum field-num beg end)))


;;; LC functions

(defconst callnum-sudoc-examples
  (list "BR60 .P25 t. 1 fasc. 2x" "BX8608 .U87x reel 1 MOR-5" "BX8608 .U87x reel 10 MOR-150" "BX8608 .U87x reel 2 MOR-22A" "DD3 .M8 reel 11b" "DD3 .M8 reel 12-" "DD3 .M8 reel 15b-17" "DD3 .M8 reel 7c/8a" "DS154.9 .D38 E9 1989 v.1" "DS154.9 .D38 E9 1990 vol.2" "DS154.9 .D38 E9 1991 Vol.10" "E169.1 .A5 no. 1" "E169.1 .A5 no. 10" "E169.1 .A5 no. 100" "E187 .J4x E2361-2363" "E187 .J4x ES1150-1151" "G3301.C5 2005 .G4" "GV854.9 .C7  B67x 1988" "JK526 1928 .S5 1981" "KBB 1x .H3" "M3 .V48 1983 Ser.V vol.1" "M3 .V48 1983 Ser.I vol.10" "M3 .V48 1983 Ser.I vol.19 crit. comm" "M3 .V48 1983 Ser.I vol.3" "PN2020 .S65 Reel 10, no. 41" "PR1901 .A3 1st ser. no. 2, etc." "PR1901 .A3 1st ser. no. 21" "PR2750 .C5 no. 23/24" "PR2888 .L5 no.2" "PR2888 .L6 ser. 6 no. 14" "PR3560 1665a" "PR8633 .S4 no. 50,53-54,56-57,63" "Z4 .A88 1991" "Z52.5 .I24 H87 1986 c.1" "Z52.5 .M52 C63x 1989 C. 2" "Z115.5 .E85 A94 1997" "Z286.L58 R67 1991b" "Z674 .C4 14th 1970" "Z675.U5 A585 1965-66" "Z694.15.A56 A53 1988 Supp.1993" "Z696.U5 H 1986" "Z3507 .A45 n.s. no. 3" "Z7553.M3 S3 1962" "Z8069.2.R88 Index" "Z8301.2 .B69 Suppl." "Z8396.3 .H45 Suppl. 1975" "ZA5055.U6 M67 A47 1992" "ZZZ4 .P5172x 1958" "M652 .B8 op.36 P3 parts 1900" "M1001 .H4 M.97 P7x 1900" "M1029 .R6 K.III 36 1960" "M452 .H42 H. III 75-80 2003" "M119 .H36 HWV430 1965" "M1389.M543 T7 1994 bk.1" "M2 .E68 Bd.97-98 1997" "M23.S9 W55x (1903-4) 1974" "M2125 .H974 (1982) 1985 Suppl." "M3 .S3927 1991 Ser.4 Werkgr.3 Bd.2" "M1010 .H4 H.XVIII.11 .E8 1931" "M1030 .H39 H.VIIe:1 1950" "M254 .T45 TWV41:f1 1960" "M270.R4 T43 TWV41 1988" "M317.B23 S.1039 S3 2002" "Z696 U5 H Hj 1981" "Z696 U5 H 1995" "Z696 U5 HM HX 1980" "JK1059 1st .D6 vol. 1" "BM499.5 .E4 1984 vol.23A" "HD5724 .C72 1964aa" "JK501 .C43 vol.2 no.1" "D769 .A533 vol. 11,pt.2" "LC1044 .O38x Cat.G" "BS1192 .O87 deel20" "HT166 .H373x D75-10" "BX830 787 .W34" "LB1570 .C89x unit 10-12" "M3 .M896 Ser.10 Wkg.28 Abt.1 Bd2" "M3 .S36 Ab.3 Rh.A Bd.8 t.1" "M3 .H46 R.1 Bd.16" "D769 .A533 vol.,pt. 1" "D769 .A533 vol.6,pt.1 vol.1" "AS36.M82 vol.6 no.3-vol.7 no.2" "AP30 .S76 vol.33 no.3" "BX830.1962 .R94 1999" "M38 .M9 K.439b .R7 1900" "M117 .S24 op.28 1913 no.3" "M231 .B23 op. 6, 1936" "M322 .B117 W.161 no.2 1948" "M1507 .S63 bari vol.1")
  "A list of unusual SuDoc call numbers.
These call numbers give an idea of the diversity of strings that
callnum.el must handle.")

(defconst callnum-sudoc-sort-sample
  (list "R222.2 2013 102nd.A349 2004" "RA2.2 2013 102nd.A349 2004" "RA30.2 2013 102nd.A349 2004" "RM202.2 2013 102nd.A349 2004" "RM203.21 2013 102nd.A349 2004" "RM203.3 2013 102nd.A349 2004" "RM222.2 102nd.A349 2004" "RM222.2 2013 102nd.A349 2004" "RM222.2 2014 102nd.A349 2004" "RM222.2 2015 2nd.A349 2004" "RM222.2 2015 102nd.A349 2004" "RM222.2 2015 102nd.A.349 2004" "RM222.2 2015 102nd. A349 2004" "RM222.2 2015 102nd.A349 2004" "RM222.2 2015 102nd.B349 2004" "RM222.2 2015 102nd.B9 2004" "RM222.2 2015 102nd.B9 2005" "RM222.2 2015 102nd.C9 2004" "RM222.2 2015 102nd.C9a 2004" "RM222.2 2015 102nd.C9b 2004" "RM222.2 2015 102nd.D23 R328945987" "RM222.2 2015 102nd.D23 R4" "RM222.2 2015 102nd.D23 R4Lo" "RM222.2 2015 102nd.D23 R4x" "RM222.2 2015 102nd.D23 R5 2013" "RM222.2 2015 102nd.D23 2021 R5 2013" "RM222.2 2015 102nd.D23 2022 R5 2013" "RM222.2 2015 102nd.D23 2022 R.5 2013" "RM222.2 2015 102nd.D23 2022 R5 2014" "RM222.2 2015 102nd.D23x 2022 R5 2014" "RM222.2 2015 102nd.D24 2022 R5 2014" "RM222.2 2015 102nd.D24 2022 R5 2014 A5" "RM222.2 2015 102nd.D24 2022 R5 2014 A5 1912" "RM222.2 2015 102nd.D24 2022 R5 2014 A5 1912 vol.1" "RM222.2 2015 102nd.D24 2022 R5 2014 A5 1912 v2" "RM222.2 2015 102nd.D24 2022 R5 2014 A5 1915 bk1" "RM222.2 2015 102nd.D24 2022 R5 2014 A5 1915 bk.2" "RM222.2 2015 102nd.D24 2022 R5 2014 A5 1915 vol.6 no.3-vol.7 no.2" "RM222.2 2015 102nd.Z1 1958" "RM222.2 2015 102nd.Z1 1958-63" "RM222.2123456 2015 102nd.Z1 1958-63")
  "A sample of contrived SuDoc numbers to check a sorting algorithm.")

(defvar callnum-lc-alist
  (list
   (list "class" (list 3 ?0 nil))
   (list "caption-integer" (list 4 ?0 t))
   (list "caption-decimal" (list 8 ?0 nil)) ;; decimal included in capture group
   (list "caption-date" (list 4 ?* t)) ;; adds an asterisk for sorting
   (list "caption-ordinal" (list 4 ?0 t))
   (list "caption-ord-indicator" (list 2 ?0 t))
   (list "cutter-one" (list 12 ?0 nil))
   (list "cutter-one-date" (list 4 ?0 nil))
   (list "cutter-two" (list 12 ?0 nil))
   (list "cutter-two-date" (list 4 ?0 nil))
   (list "cutter-three" (list 12 ?0 nil))
   (list "cutter-three-date" (list 4 ?0 nil))
   (list "spec-string-one" (list 0 ?0 nil))
   (list "spec-digits-one" (list 4 ?0 t))
   (list "spec-string-two" (list 0 ?0 nil))
   (list "spec-digits-two" (list 4 ?0 t))
   ;; (list "spec-leftovers" (list 0 ?0 nil))
   )
  "Defines the padding requirements for call number parts.

Change this varible if the padding amounts do not meet your
needs. In (list 4 ?0 t), the ‘4’ specifies total padding. The
‘?0’ is the padding character. The question mark is required in
front of any character but is not a padding character. The t
specifies to pad left. nil pads right. See the function
LC-STRING-PAD for details.")


(defvar callnum-lc-rx
  (rx bol
      (group (** 1 3 alpha)) (? blank)	   ;; class
      (group (** 0 4 digit))		   ;; caption integer
      (group "." (** 0 8 digit)) (? blank) ;; caption decimal
      (group (? (= 4 digit))) (? blank)	   ;; caption date
      (group (** 1 4 digit)) (? blank)	   ;; caption ordinal
      (group (** 1 2 alpha)) (* blank) ;; caption ordinal indicator
      (group ".") (? blank) ;; dividing period
      ;; Cutter section. There may be up to three cutter number and date pairs.
      (group (? (? alpha (** 1 10 digit)) (** 0 2 alpha))) (** 0 3 blank)
      (group (? (= 4 digit))) (** 0 3 blank)
      (group (? (? alpha (** 1 10 digit)) (** 0 2 alpha))) (** 0 3 blank)
      (group (? (= 4 digit))) (** 0 3 blank)
      (group (? (? alpha (** 1 10 digit)) (** 0 2 alpha))) (** 0 3 blank)
      (group (? (= 4 digit))) (** 0 3 blank)
      ;; Specification
      (group (? (+ alpha) (? punct) (+ digit))) (** 0 3 blank)
      (group (? (+ alpha) (? punct) (+ digit)))
      (group (* not-newline)))
  "Regex that matches the parts of an LC call number.

TODO: It does not currently match classifications without
cutters. It also does not correctly match LC call numbers that
have a cutter string but do not have a period separating the
classification string from the cutter string. In such cases, it
may capture the first part of the cutter.

If you have such call numbers, run function CALLNUM-LC-NORMALIZE-CALLNUM
to find them and add a separator.")
(defvar callnum-lc-class-normalize-regex
  "^\\(\\([[:alpha:]]\\{1,3\\}\\)\\([0-9]\\{1,4\\}\\)?\\(\\.[0-9]\\{1,10\\}\\)?[ ]?\\([0-9]\\{4\\}\\)?[ ]?\\([0-9]\\{1,5\\}[ ]?[[:alpha:]]\\{0,5\\}\\)?\\)\\(.*\\)"
  "Recognize the classification string if there is no period.

This is similar but not the same as LC-CLASS-REGEX. This regex is
only used if the first one finds no class parts.")

(defun callnum-lc-normalize-callnum (callnum)
  "Normalize a call number.
Adds a period in call numbers that do not have one, assuming it
recognizes it as a call number. CALLNUM is the call number."
  (let* (;; FIXME: delete the following if function works fine.
	 ;; (store-data (callnum-lc-regex-result-list
	 ;; 	      callnum callnum-lc-class-normalize-regex))
	 (new-callnum (mapconcat 'identity (list (match-string 1 callnum) "."
						 (match-string 7 callnum))
				 "")))
    (callnum-lc-named-alist (callnum-lc-regex-result-list new-callnum
							  callnum-lc-class-regex)
			    callnum-lc-class-alist)))

(defun callnum-lc-find-invalid (&optional field-num beg end)
  "Find invalid classification strings in region or on line.

If a classification string is not recognized by callnum.el, then
this function will find it. The user can then remove it from the
list or correct the call number. If this function finds some call
numbers invalid that are valid, let the author know.

FIELD-NUM is the field number. A numeric prefix argument
specifies in which field the call numbers are located. With no
prefix argument, it assumes field one contains the call number.
Interactively, BEG and END are the region."
  (interactive "*p\nr")
  (cl-flet ((bad-callnum (callnum)
	      (callnum-lc-pad-concat
	       (callnum-lc-all-parts callnum))))
    (callnum-act-on-region-by-line #'bad-callnum field-num beg end)))


;;; Dewey functions
(defconst callnum-dewey-examples
  (list "535.6 L661c" "398.24 An22u 2010" "439.1 Se81c" "398.21 G8827kE 2012" "523.43 B4173t  " "373.18 C189b" "421.1 B2749e" "752 H3678c" "948.022 T767v" "956.7 H278i" "599.668 J2548r" "926.415 C42064c" "636.9676 So126e" "523.7 M6191w  " "363.7382 W7345o" "927.41 On29w" "578.4 R928an" "928.1 K928k 2023" "398.208996 M1751w" "621.8 Sch79r" "362.42 H36c" "811.6 B2646g" "363.179 B648m" "929.56 Z42w" "395.5 D673ca" "398.209667 C4516t" "741.5 M132u 1994" "362.734 R7468i" "928.61 AL547a" "811.54 M79m" "927.8892 M35a" "398.2 Sh557t" "593.6 C684o" "811.008 Ex87" "927.89 Sa232c" "940.5318 Sh42i 2023" "811.6 R7273m" "938 T416g" "513.212 W7266s" "927.69 P84t")
  "A list of unusual SuDoc call numbers.
These call numbers give an idea of the diversity of strings that
callnum.el must handle.")

(defvar callnum-dewey-alist
  (list
   (list "dewey-integer" (list 3 ?0 t))
   (list "decimal-separator" (list 1 ?! nil))
   (list "dewey-decimal" (list 10 ?0 nil))
   ;; This does nothing but leave the space for readability.
   (list "visual-separator" (list 0 ?! nil))
   (list "cutter-one" (list 12 ?0 nil)) ;; 12 spaces required for some MSU cutters.
   (list "cutter-one-date" (list 4 ?0 nil))
   (list "cutter-two" (list 12 ?0 nil))
   (list "cutter-two-date" (list 4 ?0 nil))
   (list "cutter-three" (list 12 ?0 nil))
   (list "cutter-three-date" (list 4 ?0 nil))
   (list "specification" (list 10 ?0 nil))) ;; This probably needs to exceed 10.
  "Defines the padding requirements for call number parts.

Change this varible if the padding amounts do not meet your
needs. In (list 3 ?0 t), the ‘3’ specifies total padding. The
‘?0’ is the padding character. The question mark is required in
front of any character but is not a padding character. The t
specifies to pad left. nil pads right.

The padding is typically zero and will sort before any other
alphanumeric character, assuming sort ASCII order. The number of
named lists here must correspond to the number of capture groups
in CALLNUM-DEWEY-RX.")

(defvar callnum-dewey-rx
  (rx bol
      ;; Dewey classification
      (= 1
	 (group (= 3 digit))
	 (group (? "."))
	 (group (** 0 10 digit)))
      (? (group space)) ;; Space saved for visual separator of padded string.
      ;; Cutter1
      (? (group (** 1 3 alpha)
		(** 1 5 digit)
		(** 0 4 alpha))
	 (? space
	    (group (= 4 digit)
		   (** 0 2 alpha))))
      ;; Cutter2
      (? space ;; Space required if there is another cutter.
	 (group (** 1 3 alpha)
		(** 1 5 digit)
		(** 0 4 alpha))
	 (? space
	    (group (= 4 digit)
		   (** 0 2 alpha))))
      ;; Specification; the other stuff
      (? space
	 (group (* not-newline))))
  "Regex that matches Dewey parts.")

(defun callnum-dewey-make-region-sortable (&optional field-num beg end)
  "Add a padded LC call number to each line in the region.

FIELD-NUM is the field number. A numeric prefix argument
specifies in which field the call numbers are located. With no
prefix argument, it assumes field one contains the call number.
Interactively, BEG and END are the region.

This function does not account for quoted CSV files, therefore
make sure to place the call number field before any field with a
comma. For example, if you have a CSV file with two columns, one
being the call number field and another being the title field,
place the call number field to the left of the title field. The
function should work then. You can alternatively change the user
variable CALLNUM-SEPARATOR to a character that is not in any of
your fields, assuming that is in fact the separator in your file."
  (interactive "*p\nr")
  (cl-flet ((pad-callnum (callnum)
	      (callnum-pad-concat
	       (callnum-named-alist
		(callnum-regex-result-list callnum callnum-dewey-rx)
		callnum-dewey-alist))))
    (callnum-act-on-region-by-line #'pad-callnum field-num beg end)))


;;; Provide
(provide 'callnum)
;;; callnum.el ends here
