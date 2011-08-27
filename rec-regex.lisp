(in-package :adwutils)
(cl-interpol:enable-interpol-syntax)
(declaim (optimize (debug 3)))

;; TODO: group binds in body expressions
;; TODO: propogate current scanner options to body scanners

(defvar *groups* nil)

(defclass result-node ()
  #.(slot-defs '(name start end full-match groups kids)))

(defun result-node (name start end full-match &optional groups kids)
  (make-instance 'result-node :name name :start start :end end
                              :full-match full-match :groups groups :kids kids))

(defmethod print-object ((o result-node) (s stream))
  "Print the database object, and a couple of the most common identity slots."
  (print-unreadable-object (o s :type t :identity t)
    (format s "~A s:~D e:~D kids:~a ~S "
            (name o)
            (start o) (end o) (length (kids o))
            (full-match o))
    ))

(defun %collect-groups-to-tree (name scanner target &optional (start 0) (end (length target))
                                     &aux (existing-nodes *groups*) rtn)
  ;; TODO: Bind these to the appropriate length
  (let* ((*groups* nil)
         (CL-PPCRE::*REG-STARTS* #(nil nil nil nil nil nil))
         (CL-PPCRE::*REG-ENDS* #(nil nil nil nil nil nil))
         (results (setf
                   rtn
                   (multiple-value-list
                    (cl-ppcre:scan scanner target :start start :end end))))
         (s (first results))
         (e (second results))
         (match (when (first results) ;; mvl gives me '(nil)
                  (make-displaced-array target s e)))
         (group-starts (third results))
         (group-ends (fourth results)))
    (iter
      (for start in-vector group-starts)
      (for end in-vector group-ends)
      (when (and start end)
        (collect (make-displaced-array target start end) into groups))
      (finally
       (push (result-node
              name s e match groups
              (%current-results-without-backtracked-nodes))
             existing-nodes))))
  (setf *groups* existing-nodes)
  rtn)

(defun %current-results-without-backtracked-nodes ()
  (let* ((kids *groups*))
    ;; handle backtracking (remove groups that were backtracked passed)
    (iter
      (for n in kids)
      (for (start end) = (list (start n) (end n)))
      (for last-start previous start)
      (when (or (null last-start)
                (>= last-start end))
        (collect n at start into res))
      (finally (return res)))
    ))

(defun treeify-regex-results (tree)
  (labels ((help (tree)
             (iter (for n in (kids tree))
               (collect (treeify-regex-results n) into ns)
               (finally
                (return (list* (name tree) (full-match tree) ns))))))
    (help tree)))

(defun regex-recursive-groups (regex filters target)
  (let ((*groups* nil)
        (scanner (create-scanner-with-filters regex filters))
        res)
    (%collect-groups-to-tree :root scanner target)
    (setf res (pop *groups*))
    (print (treeify-regex-results res))
    res))

(defvar *body-regex* nil )
(defvar *uncompiled-br*  nil)

(defun devoid (regex) (if (eql :void regex) nil regex))

(defun make-matched-pair-matcher (name open-char close-char &optional (escape nil))
  "Will create a regex filter that can match arbitrary pairs of matched characters
   such as (start (other () some) end)"
  (lambda (body-regex &aux (br (devoid body-regex)))
    (setf body-regex (awhen (devoid body-regex)
                       (create-scanner-with-filters
                        `(:SEQUENCE :START-ANCHOR ,it :END-ANCHOR))))
    (lambda (pos)
      (let ((*body-regex* body-regex)
            (*uncompiled-br* br)
            (name (symbol-munger:english->keyword #?"matched ${name}"))
            fail
            (start pos)
            (cnt 0))
        (iter
          ;; went past the string without matching
          (when (>= pos (length cl-ppcre::*string*)) (return fail))

          (for c = (char cl-ppcre::*string* pos))
          (for not-escaped? = (or (null escape) (not (eql escape last-c))))
          (for last-c previous c)

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
                     (match (make-displaced-array cl-ppcre::*string* start match-end)))
                 (cond
                   ((null body-regex)
                    (push (result-node name start pos match) *groups*)
                    (return match-end))
                   ((%collect-groups-to-tree
                     name
                     body-regex cl-ppcre::*string* (+ 1 start) pos)
                    (let ((me (first *groups*)))
                      (setf (start me) start (end me) match-end (full-match me) match))
                    (return match-end))
                   (T fail))))))
          (incf pos)
        )))))

(defun make-named-regex-matcher (name named-regex)
  "Handles matching by delegating to another named regular expression"
  (lambda (body-regex &aux (br (devoid body-regex)))
    (setf body-regex (awhen (devoid body-regex)
                       (create-scanner-with-filters it)))
    (setf named-regex (create-scanner-with-filters named-regex))
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
                       (create-scanner-with-filters it)))
    (lambda (pos)
      (when-bind body (or *body-regex* body-regex)
        (let* ((*uncompiled-br* (or *uncompiled-br* br))
               (results
                 (%collect-groups-to-tree
                  :body
                  body cl-ppcre::*string* pos cl-ppcre::*end-pos*))
               (end (second results)))
          end)))))

(defun default-dispatch-table ()
  "Creates a default dispatch table with a parens dispatcher that can match
   pairs of parentheses"
  `(("body" . ,(make-body-matcher))
    ("parens" . ,(make-matched-pair-matcher "parens" #\( #\) ))
    ("dbl-quotes" . ,(make-matched-pair-matcher "dbl-quotes" #\" #\" #\\ ))
    ("single-quotes" . ,(make-matched-pair-matcher "single-quotes" #\' #\' #\\ ))
    ("comma-list" . ,(make-named-regex-matcher "comma-list"
                      #?r"[\t ]*(?:(?<body>[^,]*)[\t ]*,)*[\t ]*(?<body>[^,]*)[\t ]*"))
    ("csv-row"  . ,(make-named-regex-matcher "csv-row"
                      #?r"(?<comma-list>((?<dbl-quotes>)|[^\n,]*))(?:\n|$)"))
    ("csv-file"  . ,(make-named-regex-matcher "csv-file"
                      #?r"(?<csv-row>)*"))))

(defvar *dispatchers* nil)
(setf *dispatchers* (default-dispatch-table))

(defun create-scanner-with-filters
    (regex &optional (function-table *dispatchers*)
           &aux (cl-ppcre:*allow-named-registers* T))
  "Allows named registers to refer to functions that should be in
   the place of the named register"
  (typecase regex
    (function regex)
    (string
     (create-scanner-with-filters (cl-ppcre:parse-string regex)
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
                          `(:named-register (second tree)
                            (:filter ,(funcall it (third tree))))
                          (iter (for item in tree)
                            (collect (mutate-tree item))))))))
         ;; mutate the regex to contain our matcher functions
         ;; then compile it
         (cl-ppcre:create-scanner (mutate-tree p-tree)))))
    ))

(defparameter *example-function-phrase*
  "some times I like to \"function (calling all coppers (), another param (), test)\" just to see what happens")
(defparameter *test-csv* #?|this,is,a,"test
of
multiline", data
row2,of,the,"test
of
multiline", data|)

(defun run-examples ( &aux (table *dispatchers*))
  "Just runs some examples expected results:

   ((\"function (calling all coppers (), another param (), test)\"
     #(\"(calling all coppers (), another param (), test)\"))
    (\"function (calling all coppers (), another param (), test)\"
     #(\"(calling all coppers (), another param (), test)\"))
    (NIL))
  "
  (iter
    (for c upfrom 1)
    (for i in
         (list
          ;(regex-recursive-groups #?r"function\s*(?<parens>)")
          ;(regex-recursive-groups #?r"function\s*(?<parens>(([^,]+,)*[^,]+))")
          ;(regex-recursive-groups #?r"function\s*(?<parens>not-matching-at-all)")
          (regex-recursive-groups
           #?r"function\s*(?<parens>(?<comma-list>))" table *example-function-phrase*)
          ;(regex-recursive-groups #?r"(?<dbl-quotes>)" table "this string has a \"quo\\\"ted\" sub phrase")
          ;(regex-recursive-groups #?r"(?<csv-row>)" table *test-csv*)
          ;(regex-recursive-groups #?r"^(?<csv-row>)*$" table *test-csv*)
          (regex-recursive-groups #?r"(?<csv-file>)" table *test-csv*)
          ))
    (print c)
    (print i)))

