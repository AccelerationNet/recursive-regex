(cl:defpackage :recursive-regex
    (:nicknames :recex :rec-regex)
  (:use :cl :cl-user :iterate :anaphora)
  (:export :result-node :start :end :full-match :groups :kids :name
	   :treeify-regex-results :create-recursive-scanner
	   :regex-recursive-groups
	   :add-body-matcher
	   :add-named-regex-matcher
	   :add-matched-pair-matcher
	   :clear-dispatchers
           :read-rex-file-to-dispatchers
           :make-default-dispatch-table
           :*dispatchers*
           :*case-insensitive*))

(in-package :rec-regex)
(cl-interpol:enable-interpol-syntax)

;; TODO: propogate current scanner options to body scanners
(defparameter *dispatchers* nil)
(defvar *body-regex* nil )
(defvar *uncompiled-br*  nil)
(defparameter *trace-parse* nil)
(defvar *trace-depth* 0)
(defvar *minimize-results* T)
(defvar *case-insensitive* nil)

(defmacro tracer (&rest args)
  ;; theoretically a touch faster than just using %tracer
  `(when *trace-parse* (%tracer ,@args)))

(defun %tracer ( label name pos match-end &key data level next )
  (when (and *trace-parse*
             (or (null level)
                 (and (numberp *trace-parse*) (numberp level)
                      (>= *trace-parse* level))))
    (format *trace-output* "~&~vT~A ~A (~A-~A) ~S ~{~s~^ ~} ~A"
            *trace-depth*
            label name pos match-end
            (when (and pos match-end)
              (subseq cl-ppcre::*string* pos match-end))
            (alexandria:ensure-list data)
            next)))

(defmacro trace-log (msg &rest args)
  `(when *trace-parse* (%trace-log ,msg ,@args)))

(defun %trace-log (msg &rest args)
  (when *trace-parse*
    (format *trace-output* "~&~vT~?"
            *trace-depth*
            msg args)))

(defmacro def-traced-matcher-lambda ((pos-name next-name name &rest rest-tracing) &body body)
  (alexandria:with-unique-names (res)
    `(lambda (,pos-name ,next-name)
      (let ((*trace-depth* *trace-depth*)
            (*print-pretty* *trace-parse*))
        (incf *trace-depth* 2)
        (tracer "Before" ,name ,pos-name nil :data (list ,@rest-tracing)
                                             :next ,next-name)
        (let* ((,res (multiple-value-list (progn ,@body))))
          (tracer "After" ,name ,pos-name (first ,res) :data (list ,@rest-tracing)
                                                       :next ,next-name)
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

(defun result-node (name start end &optional (target cl-ppcre::*string*) groups kids )
  (make-instance
   'result-node :name name :start start :end end
   :full-match (make-displaced-array target start end)
   :groups groups :kids kids))

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
                (trace-log "backtracker throwing out ~A" n)
                (pop ,place))
              (push new-kid ,place)
              (continue-matching c)))))
     ,@body))

(defmethod create-psudo-matcher-aux (regex-tree next-fn)
  (multiple-value-bind (regex reg-num starts-with reg-names)
      (cl-ppcre::convert regex-tree)
    (declare (ignore reg-num starts-with reg-names))
    (let ((regex (cl-ppcre::gather-strings (cl-ppcre::flatten regex)))
          (cl-ppcre::*rep-num* 0)
          (cl-ppcre::*zero-length-num* 0))
      (cl-ppcre::compute-min-rest regex 0)
      ;; set the OFFSET slots of the STR objects
      (cl-ppcre::compute-offsets regex 0)
      (cl-ppcre::create-matcher-aux regex next-fn))))

(defun %next-scanner-fn (name regex pos next-fn
                              &aux (children (list)))
  ;;

  (let* ((results
           (with-child-pusher (children)
             (multiple-value-list
              (funcall
               (create-recursive-scanner regex :next-fn next-fn) cl-ppcre::*string* pos cl-ppcre::*end-pos*))))
         (start (first results))
         (end (second results)))
    ;(break "~A~%~A ~A~%~A~%~A" regex start end children next-fn)
    ;; (format T "~% COL ~A ~A ~A ~A" name start end results)
    (when (and start end)
      (let ((node (result-node name start end cl-ppcre::*string* nil children)))
        (inner-match node)
        (apply #'values results)))))

(defun devoid (regex) (if (eql :void regex) nil regex))

(defun %convert (regex )
  "Ensures we are always working with the regex we need (eg void->nil)"
  (flet ((wrap (it)
           (setf it (devoid it))
           (when it
             ;; `(:sequence :start-anchor ,it)
             it
             )))
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
    (setf br (%convert body-regex))
    (tracer "in matched pair creator AFTER" :matched nil nil :data (list br) :level 2)
    (setf body-regex (when br
			`(:NAMED-REGISTER "body" ,br)))
    (def-traced-matcher-lambda (pos next-fn name)
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
               (let* ((match-end (+ 1 pos)))
                 (acond
		   ;; dont have a body to match so, just push our match
                   ((null body-regex)
		    (let ((n (result-node name start match-end)))
		      (inner-match n))
                    (return match-end))
		   ;; we have a body to match, and it matched so set
		   ;; the information on our node and assume kids are
		   ;; in place
                   ((%next-scanner-fn name body-regex (+ 1 start) next-fn)
                    (return match-end))
		   ;; our body didnt match so we must fail
                   (T fail))))))
          (incf pos)
          )))))

(defun make-named-regex-matcher (name named-regex)
  "Handles matching by delegating to another named regular expression"
  (lambda (body-regex &aux (br (%convert body-regex))
      (nr (%convert named-regex)))

    ;; (setf body-regex (when br (create-recursive-scanner br )))
    ;;(setf named-regex (create-recursive-scanner nr ))
    (setf name (symbol-munger:english->keyword name))
    (def-traced-matcher-lambda (pos next-fn name (or nr br))
      (let ((*body-regex* body-regex)
            (*uncompiled-br* br))
        (%next-scanner-fn name nr pos next-fn)))))

(defun make-body-matcher ( &optional (name :body))
  "Handles matching the body of a named regular expression"
  (lambda (default-body-regex &aux (br (%convert default-body-regex)))
    ;; handle default body regex (if one is not provided)
    (def-traced-matcher-lambda (pos next-fn name)
      (awhen (or *body-regex* br)
        (let* ((*uncompiled-br* (or *uncompiled-br* br)))
          (%next-scanner-fn name it pos next-fn))))))

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
  (def-traced-matcher-lambda (pos next-fn "dispatcher" name)
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
        (funcall (funcall fn body-regex) pos next-fn)))))

(defun create-recursive-scanner
    (regex &key (function-table *dispatchers*) next-fn
     &aux (cl-ppcre:*allow-named-registers* T)
     (*dispatchers* function-table))
  "Allows named registers to refer to functions that should be in
   the place of the named register"
  (typecase regex
    (function regex)
    (string
     (create-recursive-scanner (cl-ppcre:parse-string regex)
      :function-table function-table
      ))
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
         (cl-ppcre:create-scanner
          (mutate-tree p-tree)
          :case-insensitive-mode *case-insensitive*
          :next-fn next-fn))))))

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
			       &key (dispatchers *dispatchers*)
                               tree-results?
			       &aux (*dispatchers* dispatchers) res)
  "run a recursive regular expression and gather all the results for
   each of them into a tree"
  (let ((scanner (create-recursive-scanner regex  :function-table dispatchers)))
    (with-child-pusher (res) (cl-ppcre:scan scanner target))
    (if tree-results?
        (treeify-regex-results (first res))
        (first res))))


