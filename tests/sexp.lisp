(cl:defpackage :recursive-regex.sexp-parser
    (:nicknames :recex.sexp)
  (:use :cl :cl-user :iterate :anaphora :recex :lisp-unit :rec-regex-test)
  (:export  ))

(in-package :recex.sexp)

(cl-interpol:enable-interpol-syntax)

(defparameter +sexp-dispatchers+
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

(defmacro with-sexp-dispatchers (() &body body)
  `(let ((recex::*dispatchers* +sexp-dispatchers+))
     ,@body))

(defun make-sexp-parser(string &optional (regex #?r"^(?<sexp>)$"))
  (with-sexp-dispatchers ()
    (recex::create-recursive-scanner regex string)))

(defun sexp-parser (string &optional (regex #?r"^(?<sexp>)$"))
  (with-sexp-dispatchers ()
    (regex-recursive-groups regex string)))


(lisp-unit:define-test test-basics.atom
  (let* ((res (sexp-parser "a-symbol"))
        (name (full-match (find-node res :name))))
    (assert-equal "a-symbol" name)))

(lisp-unit:define-test test-basics.quoted-atom
  (let* ((res (sexp-parser "'a-symbol"))
         (prefix (full-match (find-node res :prefix)))
         (name (full-match (find-node res :name))))
    (assert-equal "'" prefix)
    (assert-equal "a-symbol" name)))

(lisp-unit:define-test test-basics.listed-atom
  (let* ((res (sexp-parser "(a-symbol)"))
         (parens (find-node res :matched-parens))
         (body-match (full-match (first (kids parens)))))
    (assert-equal body-match "a-symbol")))

(lisp-unit:define-test test-basics.listed-quoted-atom
  (let* ((res (sexp-parser "('a-symbol)"))
         (parens (find-node res :matched-parens))
         (body-match (full-match (first (kids parens))))
         (name (full-match (find-node parens :name))))
    (assert-equal body-match "'a-symbol")
    (assert-equal name "a-symbol")))

(lisp-unit:define-test test-basics.quoted-listed-atom
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