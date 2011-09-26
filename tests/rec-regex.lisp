(cl:defpackage :rec-regex-test
  (:use :cl :cl-user :iterate :anaphora :rec-regex :lisp-unit)
  (:export :find-node :find-nodes))

(in-package :rec-regex-test)
(cl-interpol:enable-interpol-syntax)

(defparameter *parens-test-phrase*
  "some times I like to \"function (calling all coppers (), another param (), test)\" just to see what happens")

(defparameter *test-csv* #?|this,is,a,"test
of
multiline", data
row2,of,the,"test
of
multiline", data|)

(defun find-node (tree name)
  (if (eql (name tree) name)
      tree
      (iter (for k in (kids tree))
	    (awhen (find-node k name)
	      (return it)))))

(defun find-nodes (tree &key name)
  (iter
    (when (and (first-iteration-p)
	       (eql (name tree) name)
	       (collect tree)))
    (for k in (kids tree))
    (appending (find-nodes k :name name))))

(define-test parens
  (let ((res (regex-recursive-groups
	      #?r"function\s*(?<parens>)"
	      *parens-test-phrase*)))
    (assert-true res)))

(define-test parens-with-body
  (let* ((res (regex-recursive-groups
	      #?r"function\s*(?<parens>(([^,]+,)*[^,]+))"
	      *parens-test-phrase*)))
    (assert-true res)))

(define-test parens-no-match
  (let* ((res (regex-recursive-groups
	      #?r"function\s*(?<parens>not-matching-at-all)"
	      *parens-test-phrase*)))
    (assert-false res)))

(define-test parens-comma-list
  (let* ((res (regex-recursive-groups
           #?r"function\s*(?<parens>(?<comma-list>))"
	   *parens-test-phrase* ))
	(commas (find-node res :comma-list)))
    (assert-eql 3 (length (kids commas))
		"backtracking should remove a match")
    ;;Backtracking will remove a match of test from the
    ;; ((body),)* phrase and replace it with the final (body)
    (assert-true res)))

(define-test double-quotes
  (let* ((res (regex-recursive-groups
	       #?r"(?<double-quotes>)"
	       "this string has a \"quo\\\"ted\" sub phrase" ))
	 (quote-body (full-match (first (kids res)))))
    (assert-equal "\"quo\\\"ted\"" quote-body)
    (assert-true res)))

(define-test double-quotes-escaped-escape
  (let* ((res (regex-recursive-groups
	      #?r"(?<double-quotes>)"
	      "this string has a \"quo\\\\\"ted\" sub phrase" ))
	(quote-body (full-match (first (kids res)))))
    (assert-true (string-equal "\"quo\\\\\"" quote-body)
		  quote-body "shouldn't count escaped escapes")
    (assert-true res)))

(define-test csv-file
  (let* ((res (regex-recursive-groups
	       #?r"(?<csv-file>)"
	       *test-csv*))
	 (rows (find-nodes res :name :csv-row))
	 (commas (find-node (first rows) :comma-list))
	 (row1 (iter (for k in (kids commas))
		     (collect (full-match k)))))
    
    (assert-eql 3 (length rows) "extra empty row...")
    (assert-equalp '("this" "is" "a" "\"test
of
multiline\"" "data") row1)
    (assert-true res)))

