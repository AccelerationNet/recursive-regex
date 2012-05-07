(in-package :rec-regex-test)
(cl-interpol:enable-interpol-syntax)


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
               (cond (kids (mapcar #'name kids))
                     (n (list (name n)))))))
      (add-named-regex-matcher
       "PA" #?r"(Pennsylvania|Penn|PA)")
      (add-named-regex-matcher
       "FL" #?r"(FL|Flor|Florida)")
      (add-named-regex-matcher
       ;; It would be nice to include the terminators in the state rule,
       ;; but that fails
       "state" #?r"(?:\s|^)(?:(?<FL>)|(?<PA>))(?:\s|$)")

      (list
       ;;(names "florida")
       (names "asdflorida")
       ;; (names "FLOR")
       ;; (names "fl")
       ;; (names "PA")
       ;; (names "PENN")
       ;; (names "pennSylvania")

       ;; (names "Union Park FL")
       ;; (names "Floridationville PA penntuckey")

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
ALT 0 #<ALTERNATION {10068423D3}>: #<FUNCTION CL-PPCRE::WHITESPACEP> NIL
ALT 0 #<ALTERNATION {1006842793}>: Pennsylvania NIL
ALT 0 #<ALTERNATION {1006842793}>: Penn NIL
ALT 0 #<ALTERNATION {1006842793}>: PA NIL
ALT 0 #<ALTERNATION {1006842BE3}>: #<REGISTER {10068427D3}> NIL
ALT 2 #<ALTERNATION {1006842CE3}>: #<FUNCTION CL-PPCRE::WHITESPACEP> NIL
ALT 2 #<ALTERNATION {1006842CE3}>: #<ANCHOR {1006842C93}> NIL
ALT 0 #<ALTERNATION {1006842B53}>: FL NIL
ALT 4 #<ALTERNATION {1006842CE3}>: #<FUNCTION CL-PPCRE::WHITESPACEP> NIL
ALT 4 #<ALTERNATION {1006842CE3}>: #<ANCHOR {1006842C93}> NIL
ALT 0 #<ALTERNATION {1006842B53}>: Flor NIL
ALT 7 #<ALTERNATION {1006842CE3}>: #<FUNCTION CL-PPCRE::WHITESPACEP> NIL
ALT 7 #<ALTERNATION {1006842CE3}>: #<ANCHOR {1006842C93}> NIL
ALT 0 #<ALTERNATION {1006842B53}>: Florida NIL
ALT 0 #<ALTERNATION {1006842BE3}>: #<REGISTER {1006842B93}> NIL
ALT 0 #<ALTERNATION {10068423D3}>: #<ANCHOR {1006842383}> NIL
ALT 1 #<ALTERNATION {10068423D3}>: #<FUNCTION CL-PPCRE::WHITESPACEP> NIL
ALT 1 #<ALTERNATION {10068423D3}>: #<ANCHOR {1006842383}> NIL
ALT 2 #<ALTERNATION {10068423D3}>: #<FUNCTION CL-PPCRE::WHITESPACEP> NIL
ALT 2 #<ALTERNATION {10068423D3}>: #<ANCHOR {1006842383}> NIL
ALT 3 #<ALTERNATION {10068423D3}>: #<FUNCTION CL-PPCRE::WHITESPACEP> NIL
ALT 3 #<ALTERNATION {10068423D3}>: #<ANCHOR {1006842383}> NIL
ALT 4 #<ALTERNATION {10068423D3}>: #<FUNCTION CL-PPCRE::WHITESPACEP> NIL
ALT 4 #<ALTERNATION {10068423D3}>: #<ANCHOR {1006842383}> NIL
ALT 5 #<ALTERNATION {10068423D3}>: #<FUNCTION CL-PPCRE::WHITESPACEP> NIL
ALT 5 #<ALTERNATION {10068423D3}>: #<ANCHOR {1006842383}> NIL
ALT 6 #<ALTERNATION {10068423D3}>: #<FUNCTION CL-PPCRE::WHITESPACEP> NIL
ALT 6 #<ALTERNATION {10068423D3}>: #<ANCHOR {1006842383}> NIL
ALT 7 #<ALTERNATION {10068423D3}>: #<FUNCTION CL-PPCRE::WHITESPACEP> NIL
ALT 7 #<ALTERNATION {10068423D3}>: #<ANCHOR {1006842383}> NIL
ALT 8 #<ALTERNATION {10068423D3}>: #<FUNCTION CL-PPCRE::WHITESPACEP> NIL
ALT 8 #<ALTERNATION {10068423D3}>: #<ANCHOR {1006842383}> NIL
ALT 9 #<ALTERNATION {10068423D3}>: #<FUNCTION CL-PPCRE::WHITESPACEP> NIL
ALT 9 #<ALTERNATION {10068423D3}>: #<ANCHOR {1006842383}> NIL
ALT 10 #<ALTERNATION {10068423D3}>: #<FUNCTION CL-PPCRE::WHITESPACEP> NIL
ALT 10 #<ALTERNATION {10068423D3}>: #<ANCHOR {1006842383}> NIL
ALT 11 #<ALTERNATION {10068423D3}>: #<FUNCTION CL-PPCRE::WHITESPACEP> NIL
ALT 11 #<ALTERNATION {10068423D3}>: #<ANCHOR {1006842383}> NIL
ALT 12 #<ALTERNATION {10068423D3}>: #<FUNCTION CL-PPCRE::WHITESPACEP> NIL
ALT 12 #<ALTERNATION {10068423D3}>: #<ANCHOR {1006842383}> NIL
ALT 13 #<ALTERNATION {10068423D3}>: #<FUNCTION CL-PPCRE::WHITESPACEP> NIL
ALT 13 #<ALTERNATION {10068423D3}>: #<ANCHOR {1006842383}> NIL
ALT 14 #<ALTERNATION {10068423D3}>: #<FUNCTION CL-PPCRE::WHITESPACEP> NIL
ALT 14 #<ALTERNATION {10068423D3}>: #<ANCHOR {1006842383}> NIL
ALT 15 #<ALTERNATION {10068423D3}>: #<FUNCTION CL-PPCRE::WHITESPACEP> NIL
ALT 15 #<ALTERNATION {10068423D3}>: #<ANCHOR {1006842383}> NIL
ALT 17 #<ALTERNATION {1006842793}>: Pennsylvania NIL
ALT 17 #<ALTERNATION {1006842793}>: Penn NIL
ALT 19 #<ALTERNATION {1006842CE3}>: #<FUNCTION CL-PPCRE::WHITESPACEP> 20
ALT 17 #<ALTERNATION {1006842793}>: PA 20
ALT 17 #<ALTERNATION {1006842BE3}>: #<REGISTER {10068427D3}> 20
ALT 16 #<ALTERNATION {10068423D3}>: #<FUNCTION CL-PPCRE::WHITESPACEP> 20


|#

(progn
  cl-ppcre::(progn
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
                          do (format T "~%ALT ~A ~A: ~A ~A"
                                     (list start-pos *start-pos* *real-start-pos*)
                                     alternation
                                     (typecase choice
                                       (str (str choice))
                                       (char-class (test-function choice))
                                       (register
                                        #?"Reg:${(name choice)}")
                                       (t choice)) res )
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
                           (format T "~%ANC nn: ~A ~A"
                                   (list start-pos (or *real-start-pos* *start-pos*))
                                   (= start-pos (or *real-start-pos* *start-pos*)))
                           (and (= start-pos *end-pos*)
                                (funcall next-fn start-pos))))
                        ((and startp multi-line-p)
                         ;; a start-anchor in multi-line-mode: check if we're at
                         ;; *START-POS* or if the last character was #\Newline
                         (lambda (start-pos)
                           (declare (fixnum start-pos))
                           (format T "~%ANC sm: ~A ~A"
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
                           (format T "~%ANC s: ~A ~A"
                                   (list start-pos (or *real-start-pos* *start-pos*))
                                   (= start-pos (or *real-start-pos* *start-pos*)))
                           (and (= start-pos (or *real-start-pos* *start-pos*))
                                (funcall next-fn start-pos))))
                        (multi-line-p
                         ;; an end-anchor in multi-line-mode: check if we're at
                         ;; *END-POS* or if the character we're looking at is
                         ;; #\Newline
                         (lambda (start-pos)
                           (declare (fixnum start-pos))
                           (format T "~%ANC ml: ~A ~A"
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
                           (format T "~%ANC end: ~A ~A"
                                   (list start-pos (or *real-start-pos* *start-pos*))
                                   (= start-pos (or *real-start-pos* *start-pos*)))
                           (and (or (= start-pos *end-pos*)
                                    (and (= start-pos (1- *end-pos*))
                                         (char= #\Newline
                                                (schar *string* start-pos))))
                                (funcall next-fn start-pos)))))))
              ))