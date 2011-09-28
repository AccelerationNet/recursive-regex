(cl:defpackage :recursive-regex
    (:nicknames :recex :rec-regex)
  (:use :cl :cl-user :iterate :anaphora)
  (:export :result-node :start :end :full-match :groups :kids :name
	   :treeify-regex-results :create-recursive-scanner
	   :regex-recursive-groups
	   :add-body-matcher
	   :add-named-regex-matcher
	   :add-matched-pair-matcher
	   :clear-dispatchers ))

(in-package :rec-regex)
(cl-interpol:enable-interpol-syntax)

;; TODO: propogate current scanner options to body scanners
(defparameter *dispatchers* nil)
(defvar *body-regex* nil )
(defvar *uncompiled-br*  nil)
(defvar *trace-parse* nil)
(defvar *trace-depth* 0)
(defvar *minimize-results* T)

(defmacro tracer (&rest args)
  ;; theoretically a touch faster than just using %tracer
  `(when *trace-parse* (%tracer ,@args)))

(defun %tracer ( label name pos match-end &key data level )
  (when (and *trace-parse*
             (or (null level)
                 (and (numberp *trace-parse*)
                      (numberp level)
                      (>= *trace-parse* level))))
    (format *trace-output* "~&~vT~A ~A (~A-~A) ~S ~{~s~^ ~}"
            *trace-depth*
            label name pos match-end
            (when (and pos match-end)
              (subseq cl-ppcre::*string* pos match-end))
            (alexandria:ensure-list data))))

(defmacro def-traced-matcher-lambda ((pos-name name &rest rest-tracing) &body body)
  (alexandria:with-unique-names (res)
    `(lambda (,pos-name)
      (let ((*trace-depth* *trace-depth*)
            (*print-pretty* *trace-parse*))
        (incf *trace-depth* 2)
        (tracer "Before" ',name ,pos-name nil :data (list ,@rest-tracing) :level 1)
        (let* ((,res (multiple-value-list (progn ,@body))))
          (tracer "After" ',name ,pos-name (first ,res) :data (list ,@rest-tracing))
          (apply #'values ,res))))))

(define-condition inner-match ()
  ((data :accessor data :initarg :data :initform nil)))

(defun inner-match (data)
  (restart-case (signal (make-condition 'inner-match :data data))
    (continue-matching (&optional c) c)))

(defun continue-matching (&optional c)
  (invoke-restart 'continue-matching c))

(defun make-displaced-array (array &optional (start 0) (end (length array)))
  (make-array (- end start)
	      :element-type (array-element-type array)
	      :displaced-to array
	      :displaced-index-offset start))

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
  (print-unreadable-object (o s :type t :identity t)
    (format s "~A s:~D e:~D kids:~a ~S "
            (name o)
            (start o) (end o) (length (kids o))
            (full-match o))
    ))

(defmacro with-child-pusher ((place) &body body)
  "pushes child-matches into the place and continues-matching
   discards results that have been backtracked passed"
  `(handler-bind
      ((inner-match
	(lambda (c)
	  (let ((new-kid (data c)))
	    ;; handle backtracking
	    ;; (remove matches that end after our start)
	    (iter (for n = (first ,place))
		  (while (and n (< (start new-kid) (end n))))
		  (pop ,place))
	    (push new-kid ,place)
	    (continue-matching c)))))
    ,@body))

(defun %collect-groups-to-tree (name scanner target
				&optional (start 0) (end (length target))
				&aux (children (list)))
  (let* ( ;; TODO: Bind these to the appropriate length
	 (cl-ppcre::*reg-starts* #(nil nil nil nil nil nil))
	 (cl-ppcre::*reg-ends* #(nil nil nil nil nil nil))
	 (results
	  (multiple-value-list
	    (with-child-pusher (children)
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
         ;; remove backtracked nodes, if any of the children
         ;; start their match after our full match they, must
         ;; have been backtracked passed
         (setf children
               (iter
                 (with end-of-match = (second results))
                 (for k in (nreverse children))
                 (unless (>= (start k) end-of-match)
                   (collect k))))
         (cond ((and *minimize-results* (= 1 (length children)))
                (inner-match (first children)))
               (T (let ((n (result-node name s e match groups children)))
                    (inner-match n))))))
      results)))

(defun devoid (regex) (if (eql :void regex) nil regex))

(defun convert-to-full-match (regex )
  (flet ((wrap (it)
           (setf it (devoid it))
           (when it
             `(:SEQUENCE :START-ANCHOR ,it
               ;;:END-ANCHOR
               ))))
    (typecase regex
      (function regex) ;; presumably already did what was required :/
      (string
       (let* ((cl-ppcre:*allow-named-registers* T)
              (res (cl-ppcre:parse-string regex)))
         (wrap res)))
      (T (wrap regex)))))

(defun make-matched-pair-matcher (name open-char close-char
				  &optional (escape nil))
  "Will create a regex filter that can match arbitrary pairs of
   matched characters such as (start (other () some) end)"
  (lambda (body-regex &aux (br ))
    (tracer "in matched pair creator" :matched nil nil :data (list body-regex) :level 2)
    (setf br (convert-to-full-match body-regex))
    (tracer "in matched pair creator AFTER" :matched nil nil :data (list br) :level 2)
    (setf body-regex (when br
                       (create-recursive-scanner
			`(:NAMED-REGISTER "body" ,br))))
    (def-traced-matcher-lambda (pos name)
      (tracer "in matched pair" :matched pos nil :level 2)
      (let ((*body-regex* nil)
            ;; because we compiled it into a default body above
            (*uncompiled-br* `(:NAMED-REGISTER "body" ,br))
            (name (symbol-munger:english->keyword #?"matched ${name}"))
            fail
            (start pos)
            (cnt 0) (escape-cnt 0))
        (iter
          (tracer "looping" :matched pos nil :data (list c) :level 2)
          (when (>= pos (length cl-ppcre::*string*)) (return fail))
          (for c = (char cl-ppcre::*string* pos))
          (for c-1 previous c)
	  (if (and c-1 (eql c-1 escape))
	      (incf escape-cnt)
	      (setf escape-cnt 0))
	  (for not-escaped? =
           (or (null escape)      ;; dont have an escape-char
               (evenp escape-cnt) ;; zero escapes, escaped-escape, etc
               ))
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
                                   (full-match me) match))))
		      (%collect-groups-to-tree
		       name body-regex cl-ppcre::*string* (+ 1 start) pos))
                    (return match-end))
		   ;; our body didnt match so we must fail
                   (T fail))))))
          (incf pos)
          )))))

(defun make-named-regex-matcher (name named-regex)
  "Handles matching by delegating to another named regular expression"
  (lambda (body-regex
      &aux (br (convert-to-full-match body-regex))
      (nr (convert-to-full-match named-regex)))
    (setf body-regex (when br (create-recursive-scanner br)))
    (setf named-regex (create-recursive-scanner nr))
    (setf name (symbol-munger:english->keyword name))
    (def-traced-matcher-lambda (pos name (or nr br))
      (let ((*body-regex* body-regex)
            (*uncompiled-br* br))
        (let* ((results (%collect-groups-to-tree
                         name
                         named-regex cl-ppcre::*string*
                         pos cl-ppcre::*end-pos*)))
          (when results
            (second results)))))))

(defun make-body-matcher ( &optional (name :body))
  "Handles matching the body of a named regular expression"
  (lambda (default-body-regex
      &aux (br (convert-to-full-match default-body-regex)))
    ;; handle default body regex (if one is not provided)
    (setf default-body-regex (create-recursive-scanner br))
    (def-traced-matcher-lambda (pos name)
      (awhen (or *body-regex* default-body-regex)
        (let* ((*uncompiled-br* (or *uncompiled-br* br))
               (results
                 (%collect-groups-to-tree
                  name it cl-ppcre::*string* pos cl-ppcre::*end-pos*))
               (end (second results)))
          end)))))

(defun clear-dispatchers ()
  "removes all the dispatchers"
  (setf *dispatchers* nil))

(defun add-body-matcher (name)
  "Add a new body matcher that dispatches on name"
  (push (cons name (make-body-matcher)) *dispatchers*))

(defun add-named-regex-matcher (name regex)
  "Add a new dispatcher on name to child regex"
  (push (cons name (make-named-regex-matcher name regex))
	*dispatchers*))

(defun add-matched-pair-matcher (name open close &optional escape)
  "Add a matched pair matcher dispatched on name"
  (push
   (cons name (make-matched-pair-matcher name open close escape))
   *dispatchers*))

(defun make-default-dispatch-table ()
  "Creates a default dispatch table with a parens dispatcher that can match
   pairs of parentheses"
  (clear-dispatchers)
  (add-body-matcher "body")
  (add-matched-pair-matcher "parens" #\( #\))
  (add-matched-pair-matcher "brackets" #\{ #\} )
  (add-matched-pair-matcher "braces" #\[ #\] )
  (add-matched-pair-matcher "angles" #\< #\> )
  (add-matched-pair-matcher "double-quotes" #\" #\" #\\ )
  (add-matched-pair-matcher "single-quotes" #\' #\' #\\ )
  (add-named-regex-matcher
   "comma-list" #?r"[\t ]*(?:(?<body>[^,]*)[\t ]*,)*[\t ]*(?<body>[^,]*)[\t ]*")
  (add-named-regex-matcher
   "csv-row" #?r"(?<comma-list>((?<double-quotes>)|[^\n,]*))(?:\n|$)")
  (add-named-regex-matcher "csv-file" #?r"(?<csv-row>)*"))

(make-default-dispatch-table)

(defun dispatch-fn (name &optional (function-table *dispatchers*))
  (cdr (assoc name function-table :test #'string-equal)))

(defun %make-dispatcher (name body-regex function-table)
  "Whenever we meet a named group, change it to a named dispatcher
   if we find it in the list we use that matcher, otherwise we use
   a body matcher."
  (def-traced-matcher-lambda (pos "dispatcher" name)
    (let* ((dispatch (dispatch-fn name function-table))
           ;; pull the default body matcher
           (fn (or dispatch
                   (dispatch-fn "body" function-table)
                   (make-body-matcher))))
      (tracer "dispatching" name pos nil :data (list name :dispatcher dispatch) :level 2)
      ;; (break "Dispatch: ~A ~A ~A ~A" pos name body-regex fn)
      (handler-bind ((inner-match
                       (lambda (c &aux (n (data c)))
                         (when (and *minimize-results* (eql :body (name n)))
                           (setf (name n) (symbol-munger:english->keyword name)))
                         )))
        (funcall (funcall fn body-regex) pos)))))

(defun create-recursive-scanner
    (regex &optional (function-table *dispatchers*)
     &aux (cl-ppcre:*allow-named-registers* T)
     (*dispatchers* function-table))
  "Allows named registers to refer to functions that should be in
   the place of the named register"
  (typecase regex
    (function regex)
    (string
     (create-recursive-scanner (cl-ppcre:parse-string regex)
      function-table))
    (list regex
     (let* ((p-tree regex))
       (labels ((mutate-tree (tree)
                  "Changes the scanner parse tree to include any filter
                functions specified in the table"
                  (typecase tree
                    (null nil)
                    (atom tree)
                    (list
		       ;; all named-registers should call to the
		       ;; dispatcher for resolution (by default)
		       ;; just matching the regex in the named group
		       (if (eql :named-register (first tree))
			  `(:named-register ,(second tree)
			    (:filter
			     ,(%make-dispatcher
			       (second tree) (third tree)
			       function-table)))
			   (iter (for item in tree)
				 (collect (mutate-tree item))))))))
         ;; mutate the regex to contain our matcher functions
         ;; then compile it
         (cl-ppcre:create-scanner (mutate-tree p-tree)))))))

(defun treeify-regex-results (tree)
  "Make a lisp tree of the results
   of the matches from the clos tree"
  (when tree
    (iter
      (for n in (kids tree))
      (collect (treeify-regex-results n) into ns)
      (finally
       (return (list* (name tree) (full-match tree) ns))))))

(defun regex-recursive-groups (regex target
			       &optional (dispatchers *dispatchers*)
			       &aux (*dispatchers* dispatchers) res)
  "run a recursive regular expression and gather all the results for
   each of them into a tree"
  (let ((scanner (create-recursive-scanner regex dispatchers)))
    (with-child-pusher (res)
      (%collect-groups-to-tree :root scanner target))
    (values (first res) (treeify-regex-results (first res)))))


