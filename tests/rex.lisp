(cl:defpackage :recursive-regex.rex-test
    (:nicknames :rex-test)
  (:use :cl :cl-user :iterate :anaphora :recex :lisp-unit :rec-regex-test)
  (:shadowing-import-from
   :recex :unescaped-quote-pos :handle-quoted-rules :read-rex-file-to-dispatchers)
  (:export  ))

(in-package :rex-test)
(cl-interpol:enable-interpol-syntax)

(deftest rex-unescaped-quote-pos
  (assert-false (unescaped-quote-pos "a tough find" 0))
  (assert-eql 8 (unescaped-quote-pos "a tough \"find" 0))
  (assert-false (unescaped-quote-pos "a tough \\\"find" 0))
  (assert-eql 10 (unescaped-quote-pos "a tough \\\\\"find" 0))
  (assert-eql 13 (unescaped-quote-pos "a tough \"find\"" 9)))

(deftest test-handle-quoted-rules
  (assert-equal
      `(:sequence :start-anchor "test")
      (handle-quoted-rules "\"test\""))
  (assert-equal
      `(:SEQUENCE :START-ANCHOR (:GREEDY-REPETITION 0 NIL :EVERYTHING) "test"
        (:CHAR-CLASS (:RANGE #\A #\Z)))
      (handle-quoted-rules ".*\"test\"[A-Z]")))

(defparameter +csv-rex+
  (asdf:system-relative-pathname :recursive-regex "tests/csv.rex"))
(defparameter +sexp-rex+
  (asdf:system-relative-pathname :recursive-regex "tests/sexp.rex"))

(defun sexp-dispatchers-from-rex ()
  (let ((recex::*dispatchers*))
    (clear-dispatchers)
    (add-body-matcher "body")
    (add-matched-pair-matcher "parens" #\( #\))
    (add-matched-pair-matcher "string" #\" #\" #\\ )
    (add-matched-pair-matcher "symbol-bars" #\| #\| #\\ )
    (read-rex-file-to-dispatchers +sexp-rex+)
    recex::*dispatchers*))

(defparameter +rex-sexp-parser-dispatchers+
  (sexp-dispatchers-from-rex))

(defun sexp-parser (string &optional (regex #?r"^(?<sexp>)$"))
  (let ((recex::*dispatchers* +rex-sexp-parser-dispatchers+))
    (regex-recursive-groups regex string)))

(deftest sexp.atom
  (let* ((res (sexp-parser "a-symbol"))
        (name (full-match (find-node res :name))))
    (assert-equal "a-symbol" name)))

(deftest sexp.quoted-atom
  (let* ((res (sexp-parser "'a-symbol"))
         (prefix (full-match (find-node res :prefix)))
         (name (full-match (find-node res :name))))
    (assert-equal "'" prefix)
    (assert-equal "a-symbol" name)))

(deftest sexp.sharp-quoted-atom
  (let* ((res (sexp-parser "#'a-symbol"))
         (prefixes (find-nodes res :prefix))
         (name (full-match (find-node res :name))))
    (assert-equal 2 (length prefixes))
    (assert-equal "#" (full-match (first prefixes)))
    (assert-equal "'" (full-match (second prefixes)))
    (assert-equal "a-symbol" name)))

(deftest sexp.list-of-atoms
  (let* ((res (sexp-parser "(atom1 atom2 atom3)"))
         (atoms (mapcar #'full-match (find-nodes res :name))))
    (assert-equal '("atom1" "atom2" "atom3")  atoms)))

(deftest sexp.list-of-quoted-atoms
  (let* ((res (sexp-parser "('atom1 #'atom2 'atom3)"))
         (prefixes (mapcar #'full-match (find-nodes res :prefix)))
         (atoms (mapcar #'full-match (find-nodes res :name))))
    (assert-equal '("'" "#" "'" "'")  prefixes)
    (assert-equal '("atom1" "atom2" "atom3")  atoms)))

(deftest sexp.list-of-lists-of-atoms
  (let* ((res (sexp-parser "((atom1 atom2 atom3) (atom4 atom5 atom6) (atom7 atom8 atom9) )"))
         (sexp-lists (find-nodes res :sexp-lists))
         (atoms (mapcar #'full-match (find-nodes res :name))))
    (assert-equal '("atom1" "atom2" "atom3" "atom4" "atom5"
                    "atom6" "atom7" "atom8" "atom9")  atoms)
    ))

(deftest sexp.listed-atom
  (let* ((res (sexp-parser "(a-symbol)"))
         (parens (find-node res :matched-parens))
         (body-match (full-match (first (kids parens)))))
    (assert-equal body-match "a-symbol")))

(deftest sexp.listed-quoted-atom
  (let* ((res (sexp-parser "('a-symbol)"))
         (parens (find-node res :matched-parens))
         (body-match (full-match (first (kids parens))))
         (name (full-match (find-node parens :name))))
    (assert-equal body-match "'a-symbol")
    (assert-equal name "a-symbol")))

(deftest sexp.quoted-listed-atom
  (let* ((res (recex:treeify-regex-results (sexp-parser "'(a-symbol)"))))
    (assert-equal
        `(:SEXP "'(a-symbol)" (:PREFIX "'")
          (:MATCHED-PARENS "(a-symbol)" (:NAME "a-symbol")))
        res)
    ))