(cl:defpackage :recursive-regex.sexp-parser
    (:nicknames :recex.sexp)
  (:use :cl :cl-user :iterate :anaphora :recex :lisp-unit :rec-regex-test)
  (:export  ))

(in-package :recex.sexp)

(cl-interpol:enable-interpol-syntax)

(defun sexp-dispatchers ()
  (let ((recex::*dispatchers*))
    (clear-dispatchers)
    (add-body-matcher "body")
    (add-matched-pair-matcher "parens" #\( #\))
    (add-matched-pair-matcher "string" #\" #\" #\\ )
    (add-matched-pair-matcher "symbol-bars" #\| #\| #\\ )
    (add-named-regex-matcher "prefix" #?r"(?:'|`|#)")
    (add-named-regex-matcher
     "name" #?r"(?i)(?:\d|\w|_|-|\+|=|\*|&|\^|%|\$|@|!)+")
    (add-named-regex-matcher
     ;; TODO: It would be cooler if this worked irrespective of order
     "atom" #?r"(?:(?<string>)|(?<symbol-bars>)|(?<name>))")
    (add-named-regex-matcher
     "sexp-list" #?r"\s*(?:(?<sexp>)\s+)*(?<sexp>)\s*")

    (add-named-regex-matcher
     "less-sexp" #?r"(?<parens>(?<sexp-list>)?)|(?<atom>)")

    (add-named-regex-matcher
     "sexp" #?r"(?<prefix>)(?<sexp>)|(?<less-sexp>)")
    recex::*dispatchers*))

(defparameter +sexp-dispatchers+
  (sexp-dispatchers))

(defmacro with-sexp-dispatchers (() &body body)
  `(let ((recex::*dispatchers* +sexp-dispatchers+))
     ,@body))

(defun make-sexp-parser(string &optional (regex #?r"^(?<sexp>)$"))
  (with-sexp-dispatchers ()
    (recex::create-recursive-scanner regex string)))

(defun sexp-parser (string &optional (regex #?r"^(?<sexp>)$"))
  (with-sexp-dispatchers ()
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
         (atoms (mapcar #'full-match (find-nodes res :atom))))
    (assert-equal '("atom1" "atom2" "atom3")  atoms)))

(deftest sexp.list-of-quoted-atoms
  (let* ((res (sexp-parser "('atom1 #'atom2 'atom3)"))
         (prefixes (mapcar #'full-match (find-nodes res :prefix)))
         (atoms (mapcar #'full-match (find-nodes res :atom))))
    (assert-equal '("'" "#" "'" "'")  prefixes)
    (assert-equal '("atom1" "atom2" "atom3")  atoms)))

(deftest sexp.list-of-lists-of-atoms
  (let* ((res (sexp-parser "((atom1 atom2 atom3) (atom4 atom5 atom6) (atom7 atom8 atom9) )"))
         (sexp-lists (find-nodes res :sexp-lists))
         (atoms (mapcar #'full-match (find-nodes res :atom))))
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
  (let* ((res (sexp-parser "'(a-symbol)"))
         (first-sexp (first (kids res)))
         (prefix-match (full-match (find-node res :prefix)))
         (parens (find-node res :matched-parens))
         (body-match (full-match (first (kids parens)))))
    (assert-eql 2 (length (kids first-sexp)))
    (assert-eql :prefix (name (first (kids first-sexp))))
    (assert-eql :sexp (name (second (kids first-sexp))))
    (assert-equal prefix-match "'")
    (assert-equal body-match "a-symbol")))