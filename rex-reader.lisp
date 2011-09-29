(in-package :recex)
(cl-interpol:enable-interpol-syntax)

;;;; This file reads the rex file format modeled vaguely on (f)lex
;;;; starts with %option rows
;;;; defines rewrite names on the left and expansions on the right
;;;; separated by \s*=>\s*
;;;;
;;;; named matchers are separated from declared names by %%
;;;;
;;;; see example sexp parser in test/sexp.rex

(defparameter +option+ #?r"%option\s")
(defparameter +option-case-insensitive+ #?r"%option\scase-insensitive")
(defparameter +end-of-defs+ #?r"^%%")
(defparameter +production-split+ #?r"\s*=>\s*")


(defun trimmed-readline (stream &optional (eof-error-p t) eof-value recursive-p)
  "read a line and trim it, if it is empty return nil instead of empty string"
  (let ((l (read-line stream eof-error-p eof-value recursive-p)))
    (typecase l
      (string
       (setf l (cl-ppcre:regex-replace-all #?r"(^[ \r\n\t\f]*|[ \r\n\t\f]*$)" l ""))
       (when (plusp (length l)) l))
      (t l))))

(defun replace-all (string part replacement &key (test #'char=) stream)
  "Returns a new string in which all the occurences of the part 
is replaced with replacement. [FROM http://cl-cookbook.sourceforge.net/strings.html#manip]"
  (let ((out (or stream (make-string-output-stream))))
    (loop with part-length = (length part)
	  for old-pos = 0 then (+ pos part-length)
	  for pos = (search part string
			    :start2 old-pos
			    :test test)
	  do (write-string string out
		   :start old-pos
		   :end (or pos (length string)))
	  when pos do (write-string replacement out)
	    while pos)
    (unless stream
      (get-output-stream-string out))))

(defun unescaped-quote-pos (s start)
  "find the index of an unescaped quote"
  (flet ((num-escapes (idx)
           (iter
             (for n upfrom 1)
             (for i = (- idx n))
             (while (and (> i 0) (char-equal #\\ (char s i))))
             (finally (return (- n 1))))))
    (when (< start (length s))
      (iter (for idx = (position #\" s :test #'char-equal :start start))
        (unless idx (finish))
        (if (evenp (num-escapes idx))
            (return idx)
            (setf start (+ 1 idx)))))))

(defun handle-quoted-rules (regex &aux idx idx2 (start 0))
  "quotes in rex productions should be direct string matches (not regex)"
  (macrolet ((collect-parsed (regex idx1 &optional idx2)
               (alexandria:with-unique-names (subr)
                 `(let ((,subr (subseq ,regex ,idx1 ,idx2)))
                   (when (plusp (length ,subr))
                     (collect (cl-ppcre:parse-string ,subr)))))))
    `(:sequence :START-ANCHOR
      ,@(iter
          (setf idx (unescaped-quote-pos regex start))
          (when idx (setf idx2 (unescaped-quote-pos regex (+ 1 idx))))
          (cond
            ((and idx idx2) ;; found a quoted subseq
             (collect-parsed regex start idx)
             ;; dont include the open quote
             (collect (subseq regex (+ 1 idx) idx2))
             (setf start (+ 1 idx2)));; dont include the end quote
            (T ;; handle the remainder of the regex
             (collect-parsed regex start)
             (finish)))))))

(defun option? (line) (cl-ppcre:scan +option+ line))
(defun option-ci? (line) (cl-ppcre:scan +option-case-insensitive+ line))
(defun end-of-defs? (line) (cl-ppcre:scan +end-of-defs+ line))

(defun replace-expansions (defs new-regex)
  "If we encounter {name} style definitions in the right hand side
   replace them with their text (regex fragment) value"
  (iter (for (k v) in-hashtable defs)
	(setf new-regex
	      (replace-all
	       new-regex (format nil "{~A}" k) v )))
  new-regex)


(defun process-rex-def (name regex defs)
  (when (gethash name defs) (error "~A already defined" name))
  (setf regex (replace-expansions defs regex))
  ;; check that we created a valid regex
  (handler-case (cl-ppcre:parse-string regex)
    (error () (error "Error creating def: ~A~%   couldnt parse: ~s" name regex)))
  (setf (gethash name defs) regex))

(defun read-rex-file-to-dispatchers
    (file &aux ci?
          (defs (make-hash-table :test 'equalp))
          done-with-defs?
          (cl-ppcre:*allow-named-registers* t))
  "reads a set of definitions in from a rex file, calls
   add-named-regex-matcher for each production
      name => regex
  "
  (iter (for line in-file file using #'trimmed-readline)
    (unless line (next-iteration)) ;; got an empty line
    ;; look for the options ci mark
    (when (option-ci? line)
      (setf ci? T)
      (next-iteration))
    ;; look for the end of defs mark
    (when (end-of-defs? line)
      (setf done-with-defs? T)
      (next-iteration))

    (for (name regex) = (cl-ppcre:split +production-split+ line :limit 2))
    (cond
      (done-with-defs? ;; making name-regex-matchers now
       (let ((regex (handle-quoted-rules (replace-expansions defs regex))))
         (add-named-regex-matcher name regex)))
      ;;new def
      (t (process-rex-def name regex defs)))
    ))