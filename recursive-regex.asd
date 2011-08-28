;; -*- lisp -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :recursive-regex.system)
    (defpackage :recursive-regex.system
	(:use :common-lisp :asdf))))

(in-package recursive-regex.system)

(defsystem :recursive-regex
  :description "Recursive regular expression parsing engine"
  :licence "BSD"
  :version "0.1"
  :components ((:file "rec-regex"))
  :depends-on (:iterate :anaphora :cl-interpol :cl-ppcre
			:alexandria :symbol-munger))

(defsystem :recursive-regex-test
  :description "Tests for a recursive regular expressions"
  :licence "BSD"
  :version "0.1"
  :components ((:module :tests
			:serial t
			:components ((:file "rec-regex"))))
  :depends-on (:recursive-regex :lisp-unit))

(defmethod asdf:perform ((o asdf:test-op) (c (eql (find-system :recursive-regex))))
  (asdf:oos 'asdf:load-op :recursive-regex-test)
  (let ((*package* (find-package :recursive-regex-test)))
    (eval (read-from-string "(run-tests)"))))

;; Copyright (c) 2011 Russ Tyndall , Acceleration.net http://www.acceleration.net
;; Copyright (c) 2002-2006, Edward Marco Baringer
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;  - Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;;
;;  - Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;;  - Neither the name of Edward Marco Baringer, nor BESE, nor the names
;;    of its contributors may be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
