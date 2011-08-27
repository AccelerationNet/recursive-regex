(cl:defpackage :rec-regex-test
  (:use :cl :cl-user :iterate :anaphora :rec-regex :lisp-unit)
  (:export))

(in-package :rec-regex-test)
(cl-interpol:enable-interpol-syntax)
(declaim (optimize (debug 3)))

(defparameter *parens-test-phrase*
  "some times I like to \"function (calling all coppers (), another param (), test)\" just to see what happens")

(defparameter *test-csv* #?|this,is,a,"test
of
multiline", data
row2,of,the,"test
of
multiline", data|)

(define-test parens
  (let ((res (regex-recursive-groups
	      #?r"function\s*(?<parens>)"
	      *parens-test-phrase*)))
    (assert-true res)))

(define-test parens-with-body
  (let ((res (regex-recursive-groups
	      #?r"function\s*(?<parens>(([^,]+,)*[^,]+))"
	      *parens-test-phrase*)))
    (assert-true res)))

(define-test parens-no-match
  (let ((res (regex-recursive-groups
	      #?r"function\s*(?<parens>not-matching-at-all)"
	      *parens-test-phrase*)))
    (assert-false res)))

(define-test parens-comma-list
  (let ((res (regex-recursive-groups
           #?r"function\s*(?<parens>(?<comma-list>))"
	   *parens-test-phrase* )))
    (assert-true res)))

(define-test double-quotes
  (let ((res (regex-recursive-groups
	      #?r"(?<dbl-quotes>)"
	      "this string has a \"quo\\\"ted\" sub phrase" )))
    (assert-true res)))

(define-test csv-file
  (let ((res (regex-recursive-groups
	      #?r"(?<csv-file>)"
	      *test-csv*)))
    (assert-true res)))

