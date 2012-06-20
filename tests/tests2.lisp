(in-package :rec-regex-test)
(cl-interpol:enable-interpol-syntax)


(defun do-log (msg &rest args)
  (when t (format T "~%~?" msg args)))
(defun cl-ppcre::do-log (msg &rest args)
  (when nil (format T "~%~?" msg args)))

(defun test-create ()
  (let ((*case-insensitive* t)
        (*dispatchers* nil)
        (rec-regex::*trace-parse* t)
        )
    (add-body-matcher :body)
    (add-named-regex-matcher
     "PA" #?r"(Pennsylvania|Penn|PA)")
    (regex-recursive-groups #?r"(?<PA>)" "- Penn -" )
    ))

(defun test-it-2 ()
  (let ((*case-insensitive* t)
        (*dispatchers* *dispatchers*)
        ;;(rec-regex::*trace-parse* t)
        )
    (flet ((names (inp)
             (let* ((n (regex-recursive-groups #?r"(?<state>)" inp ))
                    (kids (when n (kids n))))
               ;;(do-log "~%~S~%" (treeify-regex-results n))
               (values
                (cond
                  ((adwutils::only-one? kids)
                   (name (first kids)))
                  (kids (mapcar #'name kids))
                  (n (name n)))
                kids))))
      (add-named-regex-matcher
       "PA" #?r"(Pennsylvania|Penn|PA)")
      (add-named-regex-matcher
       "FL" #?r"(FL|Flor|Florida)")
      (add-named-regex-matcher
       ;; It would be nice to include the terminators in the state rule,
       ;; but that fails
       "state" #?r"(?:\s|^)(?:(?<FL>)|(?<PA>))(?:\s|$)")
      (macrolet ((l (exp s)
                   `(multiple-value-bind (res kids)
                     (names ,s)
                     (if (eql ,exp res)
                         (do-log "PASS ~A ~A ~A ~A" ,exp res ,s kids)
                         (do-log "FAIL ~A ~A ~A ~A" ,exp res ,s kids)))))
        (l :fl "florida")
        (l nil "asdflorida")
        (l nil "floridaaaa")
        (l nil "asdffloridaaaa")
        (l :fl "asd florida aaa")
        (l :fl "FLOR")
        (l :fl "fl")
        (l :pa "PA")
        (l :pa "PENN")
        (l :pa "pennSylvania")
        (l :fl "Union Park FL")
        (l :pa "Floridationville PA penntuckey")
        ))))

(defun test-it ()
  (let ((scanner (cl-ppcre:create-scanner
                  #?r"(?:\s|^)(?:(Pennsylvania|Penn|PA)|(FL|Flor|Florida))(?:\s|$)"
                  :case-insensitive-mode t)))
    (flet ((matches (it) (cl-ppcre:scan-to-strings scanner it)))
      (list
        (matches "florida")
        (matches "FLOR")
        (matches "asdflorida")
        (matches "fl")
        (matches "PA")
        (matches "PENN")
        (matches "pennSylvania")
        (matches "Union Park FL")
       (matches "Floridationville PA penntuckey"))
      )))

#|

|#



(progn
  cl-ppcre::(progn
              (defmethod create-matcher-aux ((filter filter) next-fn)
                (declare #.*standard-optimize-settings*)
                (let ((fn (fn filter)))
                  (lambda (start-pos)
                    (do-log "Filter: ~A ~A" start-pos next-fn)
                    (prog1 (funcall fn start-pos next-fn)
                      (do-log "Done Filter: ~A ~A" start-pos next-fn)))))

              (defmethod create-matcher-aux ((register register) next-fn)
                (declare #.*standard-optimize-settings*)
                ;; the position of this REGISTER within the whole regex; we start to
                ;; count at 0
                (let ((num (num register)))
                  (declare (fixnum num))
                  ;; STORE-END-OF-REG is a thin wrapper around NEXT-FN which will
                  ;; update the corresponding values of *REGS-START* and *REGS-END*
                  ;; after the inner matcher has succeeded
                  (flet ((store-end-of-reg (start-pos)
                           (declare (fixnum start-pos)
                                    (function next-fn))
                           (setf (svref *reg-starts* num) (svref *regs-maybe-start* num)
                                 (svref *reg-ends* num) start-pos)
                           (funcall next-fn start-pos)))
                    ;; the inner matcher is a closure corresponding to the regex
                    ;; wrapped by this REGISTER
                    (let ((inner-matcher (create-matcher-aux (regex register)
                                                             #'store-end-of-reg)))
                      (declare (function inner-matcher))
                      ;; here comes the actual closure for REGISTER
                      (lambda (start-pos)
                        (declare (fixnum start-pos))
                        ;; remember the old values of *REGS-START* and friends in
                        ;; case we cannot match
                        (let ((old-*reg-starts* (svref *reg-starts* num))
                              (old-*regs-maybe-start* (svref *regs-maybe-start* num))
                              (old-*reg-ends* (svref *reg-ends* num)))
                          ;; we cannot use *REGS-START* here because Perl allows
                          ;; regular expressions like /(a|\1x)*/
                          (setf (svref *regs-maybe-start* num) start-pos)
                          (let ((next-pos (funcall inner-matcher start-pos)))
                            (unless next-pos
                              ;; restore old values on failure
                              (setf (svref *reg-starts* num) old-*reg-starts*
                                    (svref *regs-maybe-start* num) old-*regs-maybe-start*
                                    (svref *reg-ends* num) old-*reg-ends*))
                            next-pos)))))))

              (defmethod create-matcher-aux ((alternation alternation) next-fn)
                (declare #.*standard-optimize-settings*)
                ;; first create closures for all alternations of ALTERNATION
                (let ((all-matchers (mapcar #'(lambda (choice)
                                                (create-matcher-aux choice next-fn))
                                            (choices alternation))))
                  ;; now create a closure which checks if one of the closures
                  ;; created above can succeed
                  (lambda (start-pos)
                    (declare (fixnum start-pos))
                    (loop for matcher in all-matchers
                          for choice in (choices alternation)
                          for res = (funcall (the function matcher) start-pos)
                          do (do-log "ALT ~A ~A: ~A ~A"
                               (list start-pos *start-pos* *real-start-pos*)
                               alternation
                               (typecase choice
                                 (str (str choice))
                                 (char-class (test-function choice))
                                 (anchor (if (cl-ppcre::startp choice)
                                             " ^ "
                                             " $ " ))
                                 (register
                                  #?"Reg:${(name choice)}")
                                 (t choice))
                               res )
                          thereis res))))
              (defmethod create-matcher-aux ((anchor anchor) next-fn)
                (declare #.*standard-optimize-settings*)
                (declare (function next-fn))
                (let ((startp (startp anchor))
                      (multi-line-p (multi-line-p anchor)))
                  (cond ((no-newline-p anchor)
                         ;; this must be an end-anchor and it must be modeless, so
                         ;; we just have to check whether START-POS equals
                         ;; *END-POS*
                         (lambda (start-pos)
                           (declare (fixnum start-pos))
                           (do-log "ANC nn: ~A ~A"
                                   (list start-pos (or *real-start-pos* *start-pos*))
                                   (= start-pos (or *real-start-pos* *start-pos*)))
                           (and (= start-pos *end-pos*)
                                (funcall next-fn start-pos))))
                        ((and startp multi-line-p)
                         ;; a start-anchor in multi-line-mode: check if we're at
                         ;; *START-POS* or if the last character was #\Newline
                         (lambda (start-pos)
                           (declare (fixnum start-pos))
                           (do-log "ANC sm: ~A ~A"
                                   (list start-pos (or *real-start-pos* *start-pos*))
                                   (= start-pos (or *real-start-pos* *start-pos*)))
                           (let ((*start-pos* (or *real-start-pos* *start-pos*)))
                             (and (or (= start-pos *start-pos*)
                                      (and (<= start-pos *end-pos*)
                                           (> start-pos *start-pos*)
                                           (char= #\Newline
                                                  (schar *string* (1- start-pos)))))
                                  (funcall next-fn start-pos)))))
                        (startp
                         ;; a start-anchor which is not in multi-line-mode, so just
                         ;; check whether we're at *START-POS*
                         (lambda (start-pos)
                           (declare (fixnum start-pos))
                           (do-log "ANC s: ~A ~A"
                                   (list start-pos *real-start-pos* *start-pos*)
                                   (= start-pos (or *real-start-pos* *start-pos*)))
                           (and (= start-pos (or *real-start-pos* *start-pos*))
                                (funcall next-fn start-pos))))
                        (multi-line-p
                         ;; an end-anchor in multi-line-mode: check if we're at
                         ;; *END-POS* or if the character we're looking at is
                         ;; #\Newline
                         (lambda (start-pos)
                           (declare (fixnum start-pos))
                           (do-log "ANC ml: ~A ~A"
                                   (list start-pos (or *real-start-pos* *start-pos*))
                                   (= start-pos (or *real-start-pos* *start-pos*)))
                           (and (or (= start-pos *end-pos*)
                                    (and (< start-pos *end-pos*)
                                         (char= #\Newline
                                                (schar *string* start-pos))))
                                (funcall next-fn start-pos))))
                        (t
                         ;; an end-anchor which is not in multi-line-mode, so just
                         ;; check if we're at *END-POS* or if we're looking at
                         ;; #\Newline and there's nothing behind it
                         (lambda (start-pos)
                           (declare (fixnum start-pos))
                           (do-log "ANC end: ~A ~A"
                                   (list start-pos (or *real-start-pos* *start-pos*))
                                   (= start-pos (or *real-start-pos* *start-pos*)))
                           (and (or (= start-pos *end-pos*)
                                    (and (= start-pos (1- *end-pos*))
                                         (char= #\Newline
                                                (schar *string* start-pos))))
                                (funcall next-fn start-pos)))))))
              ))