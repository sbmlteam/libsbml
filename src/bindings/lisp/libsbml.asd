;;; -*- Lisp -*-
#|
\file    libsbml.asd
\brief
\author  Martin Ginkel <mginkel@mpi-mageburg.mpg.de>

$Id$
$Source$

Copyright 2004 Max-Planck-Institute Magdeburg

This is free software; you can redistribute it and/or modify it
under the terms of the GNU Lesser General Public License as published
by the Free Software Foundation; either version 2.1 of the License, or
any later version.

You should have received a copy of the GNU Lesser General Public License
along with this library; if not, write to the Free Software Foundation,
Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.

THIS SOFTWARE IS PROVIDED BY THE AUTHORS AND CONTRIBUTORS ``AS IS'' AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
SUCH DAMAGE.
|#


(cond ((ignore-errors (logical-pathname-translations "libsbml"))
       (load (logical-pathname "libsbml:libsbml-config.lisp")))
      (*load-truename*
       (load (make-pathname :name "libsbml-config" :type "lisp" 
			    :defaults *load-truename*)))
      (t
       ;; assume that we are in the correct directory
       (load "libsbml-config.lisp")))
(require :asdf (logical-pathname "libsbml:utilities;asdf.lisp"))
(defpackage :libsbml-system (:use :asdf :cl))
(in-package :libsbml-system)
;(load (logical-pathname "libsbml:uffi;uffi-config.lisp"))
(pushnew (logical-pathname "libsbml:") asdf:*central-registry* :test #'equal)
;(find-system :uffi)
(operate 'load-op :uffi)
;(load (logical-pathname "libsbml:cparse;cparse-config.lisp"))
;(pushnew (logical-pathname "libsbml:cparse;" ) asdf:*central-registry* :test #'equal)
;(asdf:find-system :cparse)
(asdf:operate 'asdf:load-op :cparse)

(defpackage :libsbml-system (:use :asdf :cl )
	    (:import-from :cparse :c-header-file :c-preproc-header-file)
	    )
; (defmethod perform ((operation operation) (c c-source-file))
;   nil)
; (defmethod operation-done-p ((o operation) (c c-source-file))
;   t)

(defsystem :libsbml
    :depends-on (uffi cparse)
    :components	
    (;(:external-system uffi)
     ;(:external-system cparse)
     (:file package)
     #+:build
     (:file boolean-functions :depends-on (package))
     #+:build
     (:c-header-file 
      "sbmllisp"
      :depends-on (package boolean-functions)
      :preprocessor cl-user::*cpp*
      :preproc-options (format nil " -I~a/../.. "
			       (namestring (truename (logical-pathname "libsbml:")))))
     #+:build
     (:c-preproc-header-file 
      "sbmllisp-pre"
      :depends-on ("sbmllisp")
      :lisp-file "libsbmlc"
      :parser-options (:boolean-functions 
		       "LIBSBML::*BOOLEAN-FUNCTIONS*"
		       :drop-symbols ("ASTNode_copyFromToken" 
				      ;; not implemented, but in Header
				      "StringBuffer_appendNumber" 
				      ;; c-stdarg not possible through ffi
				      )
		       :foreign-lib "libsbml:;libsbml.so"
		       :package "LIBSBMLC")
      )
     #+:build
     (:file libsbmlc :depends-on ("sbmllisp"))
     #-:build
     (:file libsbmlc :depends-on (package))
     ;(:file libsbml :depends-on (libsbmlc))
     (:file utilities :depends-on (libsbmlc))
     (:file sbml :depends-on (utilities))
     (:file sbml-reader :depends-on (libsbmlc sbml utilities))
     (:file memtest :depends-on (libsbmlc))
     ))

#|
(require :asdf "path:to;asdf.lisp")
(load "libsbml:libsbml.asd")
(asdf:operate 'asdf:load-op :libsbml)
|#
