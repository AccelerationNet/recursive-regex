(cl:defpackage :rec-regex
  (:use :cl :cl-user :iterate :anaphora)
  (:export :result-node :start :end :full-match :groups :kids
	   :treeify-regex-results :create-recursive-scanner
	   :regex-recursive-groups))

(in-package :rec-regex)
(cl-interpol:enable-interpol-syntax)
(declaim (optimize (debug 3)))

;; TODO: propogate current scanner options to body scanners

(defvar *groups* nil)

(defun make-displaced-array (array &optional (start 0) (end (length array)))
  (make-array (- end start)
	      :element-type (array-element-type array)
	      :displaced-to array
	      :displaced-index-offset start))

(define-condition inner-match ()
  ((data :accessor data :initarg :data :initform nil)))

(defun inner-match (data)
  (restart-case (signal (make-condition 'inner-match :data data))
    (continue (&optional c) c)))


(defclass result-node () 
  ((name :accessor name :initarg :name :initform nil)
   (start :accessor start :initarg :start :initform nil)
   (end :accessor end :initarg :end :initform nil)
   (full-match :accessor full-match :initarg :full-match :initform nil)
   (groups :accessor groups :initarg :groups :initform nil)
   (kids :accessor kids :initarg :kids :initform nil)))

(defun result-node (name start end full-match &optional groups kids)
  (make-instance 'result-node :name name :start start :end end
                              :full-match full-match :groups groups :kids kids))

(defmethod print-object ((o result-node) s)
  "Print the database object, and a couple of the most common identity slots."
  (print-unreadable-object (o s :type t :identity t)
    (format s "~A s:~D e:~D kids:~a ~S "
            (name o)
            (start o) (end o) (length (kids o))
            (full-match o))
    ))

(defun %collect-groups-to-tree (name scanner target
				&optional (start 0) (end (length target))
				&aux children)
  (let* ( ;; TODO: Bind these to the appropriate length
	 (cl-ppcre::*reg-starts* #(nil nil nil nil nil nil))
	 (cl-ppcre::*reg-ends* #(nil nil nil nil nil nil))
	 (results
	  (multiple-value-list
	      (handler-bind
		  ((inner-match (lambda (c) (push (data c) children) (continue c))))
		(cl-ppcre:scan scanner target :start start :end end))))
	 (success? (first results)))
    (when success?
      (iter
	(with (s e group-starts group-ends) = results)
	(with match = (make-displaced-array target s e))
	(for start in-vector group-starts)
	(for end in-vector group-ends)
	(when (and start end)
	  (collect (make-displaced-array target start end) into groups))
	(finally
	 (let ((n (result-node
		   name s e match groups
		   (%current-results-without-backtracked-nodes
		    children))))
           (inner-match n))))
      results)))

(defun %current-results-without-backtracked-nodes (kids )
  ;; handle backtracking (remove groups that were backtracked passed)
  (iter
    (for n in kids)
    (for (start end) = (list (start n) (end n)))
    (for last-start previous start)
    (when (or (null last-start)
	      (>= last-start end))
      (collect n at start into res))
    (finally (return res))))

(defun treeify-regex-results (tree)
  (labels ((help (tree)
	     (when tree
	       (iter (for n in (kids tree))
		     (collect (treeify-regex-results n) into ns)
		     (finally
		      (return (list* (name tree) (full-match tree) ns)))))))
    (help tree)))

(defun regex-recursive-groups (regex target
			       &optional (dispatchers *dispatchers*))
  (let ((*dispatchers* dispatchers)
        (scanner (create-recursive-scanner regex dispatchers))
        res)
    (handler-bind ((inner-match (lambda (c) (setf res (data c)) (continue c))))
    (%collect-groups-to-tree :root scanner target))
    (values res (treeify-regex-results res))))

(defvar *body-regex* nil )
(defvar *uncompiled-br*  nil)

(defun devoid (regex) (if (eql :void regex) nil regex))

(defun make-matched-pair-matcher (name open-char close-char
				  &optional (escape nil))
  "Will create a regex filter that can match arbitrary pairs of
   matched characters such as (start (other () some) end)"
  (lambda (body-regex &aux (br (devoid body-regex)))
    (setf body-regex (awhen (devoid body-regex)
                       (create-recursive-scanner
			`(:NAMED-REGISTER
			  "body"
			  ;; dispatch to body to capture it
			  (:SEQUENCE :START-ANCHOR ,br :END-ANCHOR)))))
    (lambda (pos)
      (let ((*body-regex* nil) 
	      ;; because we compiled it into a default body above
            (*uncompiled-br* `(:NAMED-REGISTER
			       "body"
			       ;; dispatch to body to capture it
			       (:SEQUENCE :START-ANCHOR ,br :END-ANCHOR)))
            (name (symbol-munger:english->keyword #?"matched ${name}"))
            fail
            (start pos)
            (cnt 0))
        (iter
          ;; went past the string without matching
          (when (>= pos (length cl-ppcre::*string*)) (return fail))

          (for c = (char cl-ppcre::*string* pos))
          (for c-1 previous c)
	  (for c-2 previous c-1)
	  (for not-escaped? =
	       (or (null escape) ;; dont have an escape
		   (not (eql escape c-1))
		   (and (eql escape c-1)
			(eql escape c-2))))

          ;; we dont match the open char so fail
          (when (and (first-iteration-p)
                     (not (eql c open-char)))
            (return fail))

          (cond
            ((and not-escaped? (eql c open-char)
                  ;; if the open and close are the same they cannot be nested
                  (not (and (plusp cnt) (eql open-char close-char))))
             (incf cnt))
            ((and not-escaped? (eql c close-char))
             (decf cnt)
             (when (zerop cnt) ;; found our last matching char
               (let* ((match-end (+ 1 pos))
                     (match (make-displaced-array
			     cl-ppcre::*string* start match-end)))
                 (acond
		   ;; dont have a body to match so, just push our match
                   ((null body-regex)
		    (let ((n (result-node name start pos match)))
		      (inner-match n))
                    (return match-end))
		   ;; we have a body to match, and it matched so set
		   ;; the information on our node and assume kids are
		   ;; in place
                   ((handler-bind
			((inner-match
			  (lambda (c &aux (me (data c)))
			    (setf (start me) start
				  (end me) match-end
				  (full-match me) match)
			    )))
		      (%collect-groups-to-tree
		       name body-regex cl-ppcre::*string* (+ 1 start) pos))
                    (return match-end))
		   ;; our body didnt match so we must fail
                   (T fail))))))
          (incf pos)
        )))))

(defun make-named-regex-matcher (name named-regex)
  "Handles matching by delegating to another named regular expression"
  (lambda (body-regex &aux (br (devoid body-regex)))
    (setf body-regex (awhen (devoid body-regex)
                       (create-recursive-scanner it)))
    (setf named-regex (create-recursive-scanner named-regex))
    (setf name (symbol-munger:english->keyword name))
    (lambda (pos)
      (let ((*body-regex* body-regex)
            (*uncompiled-br* br))
        (let* ((results (%collect-groups-to-tree
                         name
                         named-regex cl-ppcre::*string*
                         pos cl-ppcre::*end-pos*)))
          (when results
            (second results)))))))

(defun make-body-matcher ()
  "Handles matching the body of a named regular expression"
  (lambda (body-regex &aux (br (devoid body-regex)))
    ;; handle default body regex (if one is not provided)
    (setf body-regex (awhen (devoid body-regex)
                       (create-recursive-scanner it)))
    (lambda (pos)
      (awhen (or *body-regex* body-regex)
        (let* ((*uncompiled-br* (or *uncompiled-br* br))
               (results
		(%collect-groups-to-tree
		 :body it cl-ppcre::*string* pos cl-ppcre::*end-pos*))
               (end (second results)))
          end)))))

(defun default-dispatch-table ()
  "Creates a default dispatch table with a parens dispatcher that can match
   pairs of parentheses"
  `(("body" . ,(make-body-matcher))
    ("parens" . ,(make-matched-pair-matcher "parens" #\( #\) ))
    ("brackets" . ,(make-matched-pair-matcher "brackets" #\{ #\} ))
    ("braces" . ,(make-matched-pair-matcher "braces" #\[ #\] ))
    ("angles" . ,(make-matched-pair-matcher "angles" #\< #\> ))
    ("double-quotes" . ,(make-matched-pair-matcher "double-quotes" #\" #\" #\\ ))
    ("single-quotes" . ,(make-matched-pair-matcher "single-quotes" #\' #\' #\\ ))
    ("comma-list" . ,(make-named-regex-matcher "comma-list"
                      #?r"[\t ]*(?:(?<body>[^,]*)[\t ]*,)*[\t ]*(?<body>[^,]*)[\t ]*"))
    ("csv-row"  . ,(make-named-regex-matcher "csv-row"
                      #?r"(?<comma-list>((?<double-quotes>)|[^\n,]*))(?:\n|$)"))
    ("csv-file"  . ,(make-named-regex-matcher "csv-file"
                      #?r"(?<csv-row>)*"))))

(defparameter *dispatchers* (default-dispatch-table))

(defun create-recursive-scanner
    (regex &optional (function-table *dispatchers*)
           &aux (cl-ppcre:*allow-named-registers* T))
  "Allows named registers to refer to functions that should be in
   the place of the named register"
  (typecase regex
    (function regex)
    (string
     (create-recursive-scanner (cl-ppcre:parse-string regex)
      function-table))
    (list regex
     (let* ((p-tree regex))
       (labels ((dispatcher? (name)
                  "Return the name of the dispatcher from the table if
                applicable"
                  (cdr (assoc name function-table :test #'string-equal)))
                (mutate-tree (tree)
                  "Changes the scanner parse tree to include any filter
                functions specified in the table"
                  (typecase tree
                    (null nil)
                    (atom tree)
                    (list
                     (aif (and (eql :named-register (first tree))
                               (dispatcher? (second tree)))
                          `(:named-register ,(second tree)
                            (:filter ,(funcall it (third tree))))
                          (iter (for item in tree)
                            (collect (mutate-tree item))))))))
         ;; mutate the regex to contain our matcher functions
         ;; then compile it
         (cl-ppcre:create-scanner (mutate-tree p-tree)))))
    ))

