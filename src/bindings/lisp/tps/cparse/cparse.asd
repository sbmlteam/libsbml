;;; -*- Lisp -*-
;;; 
;;; Copyright (c) 2001 Timothy Moore
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;; 3. The name of the author may not be used to endorse or promote products
;;;    derived from this software without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
;;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
;;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.

;;;
;;; CPARSE - library for parsing C header files.

(in-package :cl-user)


(when (find-package "UFFI")
  (pushnew :uffi *features*))


(asdf:defsystem :cparse
    :components
    ((:file cparse-package)
     (:file ctype :depends-on (cparse-package))
     (:file cparse :depends-on (cparse-package ctype))
     #+:allegro (:file acl-ffi :depends-on (cparse))
     #+:uffi (:file uffi :depends-on (cparse))
     #+CMU (:file "cmu-alien-package")
     #+CMU (:file "cmu-alien" :depends-on ("cparse-package"
					   "cmu-alien-package"
					   "cparse"))
     (:file asdf :depends-on (cparse-package))
     )
    )


;;; CMUCL 18c and earlier didn't hash strings with fill pointers correctly.
#+:cmu
(let ((string-with-fill (make-array 32
				    :element-type 'base-char
				    :adjustable t
				    :fill-pointer 7)))
  (setf (subseq string-with-fill 0) "typedef")
  (unless (= (sxhash string-with-fill) (sxhash "typedef"))
    (pushnew :hash-fill-bug *features*)))

;(asdf:operate 'asdf:load-op :cparse)
