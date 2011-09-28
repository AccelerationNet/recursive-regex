(cl:defpackage :recursive-regex-test
  (:nicknames :recex-test :rec-regex-test)
  (:use :cl :cl-user :iterate :anaphora :rec-regex :lisp-unit)
  (:export :find-node :find-nodes :deftest))

(in-package :rec-regex-test)
(cl-interpol:enable-interpol-syntax)

(defun log-time (&optional (time (get-universal-time)) stream)
  "returns a date as ${mon}/${d}/${y} ${h}:${min}:{s}, defaults to get-universal-time"
  (multiple-value-bind ( s min h  )
      (decode-universal-time time)
    (format stream "~2,'0d:~2,'0d:~2,'0d "  h min s)))

(defmacro deftest (name &body body)
  `(define-test ,name
    (format T "~%Starting :~A " ',name) (log-time (get-universal-time) t)
    ,@body
    (format T "~%Finished :~A " ',name) (log-time (get-universal-time) t)))

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

(defun find-nodes (tree name)
  (iter
    (when (and (first-iteration-p)
	       (eql (name tree) name)
	       (collect tree)))
    (for k in (kids tree))
    (appending (find-nodes k name))))

(deftest parens
  (let ((res (regex-recursive-groups
	      #?r"function\s*(?<parens>)"
	      *parens-test-phrase*)))
    (assert-true res)))

(deftest parens-with-body
  (let* ((res (regex-recursive-groups
	      #?r"function\s*(?<parens>(([^,]+,)*[^,]+))"
	      *parens-test-phrase*)))
    (assert-true res)))

(deftest parens-no-match
  (let* ((res (regex-recursive-groups
	      #?r"function\s*(?<parens>not-matching-at-all)"
	      *parens-test-phrase*)))
    (assert-false res)))

(deftest parens-comma-list
  (let* ((res (regex-recursive-groups
           #?r"function\s*(?<parens>(?<comma-list>))"
	   *parens-test-phrase* ))
	(commas (find-node res :comma-list)))
    (assert-eql 3 (length (kids commas))
		"backtracking should remove a match")
    ;;Backtracking will remove a match of test from the
    ;; ((body),)* phrase and replace it with the final (body)
    (assert-true res)))

(deftest double-quotes
  (let* ((res (regex-recursive-groups
	       #?r"(?<double-quotes>)"
	       "this string has a \"quo\\\"ted\" sub phrase" ))
	 (quote-body (full-match (find-node res :matched-double-quotes))))
    (assert-equal "\"quo\\\"ted\"" quote-body)
    (assert-true res)))

(deftest backtracking-match-removal
  (let* ((res (regex-recursive-groups
	       #?r"(?<double-quotes>)((?<single-quotes>)test)?"
	       "\"some-double-quotes\"''not-matching" )))
    (assert-false (find-nodes res :MATCHED-SINGLE-QUOTES))))

(deftest double-quotes-escaped-escape
  (let* ((res (regex-recursive-groups
               #?r"(?<double-quotes>)"
               "this string has a \"quo\\\\\"ted\" sub phrase" ))
         (quote-body (full-match (find-node res :matched-double-quotes))))
    (assert-true (string-equal "\"quo\\\\\"" quote-body)
     quote-body "shouldn't count escaped escapes")
    (assert-true res)))

(deftest csv-file
  (let* ((res (regex-recursive-groups
	       #?r"(?<csv-file>)"
	       *test-csv*))
	 (commas (find-nodes res :comma-list))
	 (row1 (iter (for k in (kids (first commas)))
		     (collect (full-match k)))))
    
    (assert-eql 2 (length commas) "two rows of results")
    (assert-equalp '("this" "is" "a" "\"test
of
multiline\"" "data") row1)
    (assert-true res)))

