#|
Description : partial translation from C-Headers to UFFI 
Author(s)   : Martin Ginkel <mginkel@mpi-mageburg.mpg.de>
Revision    : $Id$
Source      : $Source$

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


(in-package :cparse)

(defgeneric foreign-lib (self)
  )

(defclass uffi-cparser ()
  ((infile :accessor infile :initarg :infile)
   (outfile :accessor outfile :initarg :outfile)
   (symbols :accessor symbols :initform (make-hash-table))
   (types :accessor types :initform (make-hash-table))
   (foreign-lib :accessor foreign-lib :initarg :foreign-lib :initform nil)
   (drop-symbols :accessor drop-symbols
		 :initarg :drop-symbols
		 :initform nil)
   (boolean-functions :accessor boolean-functions 
		      :initarg :boolean-functions   
		      :initform nil
		      :documentation "List of boolean functions, that should
  have argument and return conversion. Entries can be strings \"function-name\" or
lists (\"function-name\" [:return] \"argname\"+)"
		      )
   (package :accessor package :initarg :package :initform *package*)
   ))

(defmethod normalize-boolean-functions ((self uffi-cparser))
  (with-slots ((b boolean-functions)) self
    (when (stringp b)
      (setf b (eval (read-from-string b))))
    (setf b
      (mapcar #'(lambda (f)
		  (if (stringp f)
		      (list f :return)
		    f))
	      b)))
  )



(defmethod initialize-instance ((self uffi-cparser) &key foreign-lib infile outfile &allow-other-keys)
  (call-next-method)
  (let ((infile (when (slot-boundp self 'infile)
		  (infile self)))
	(outfile (when (slot-boundp self 'outfile)
		   (outfile self)))
	)
    (when (or (stringp infile)
	      (pathnamep infile))
      (setf (infile self) (open infile 
				:direction :input
				:if-does-not-exist :error)))
    (when (or (stringp outfile)
	      (pathnamep outfile))
      (setf (outfile self) (open  outfile 
				  :direction :output
				  :if-does-not-exist :create
				  :if-exists :new-version)))
    (cond ((packagep (package self)) t)
	  ((find-package (package self))
	   (setf (package self) (find-package (package self))))
	  (t (setf (package self) (eval `(defpackage ,(package self) 
					   (:use )))))
	  )
    (normalize-boolean-functions self)
    (write-header self)
    )
  )

(defmethod write-header ((self uffi-cparser))
  (format (outfile self) ";; -*- model:common-lisp package: ~a -*-
(cl::in-package :cl-user)

;(cl:load \"uffi:;uffi-config.lisp\")
;(cl:load \"uffi:uffi.asd\")
(asdf:operate 'asdf:load-op :uffi)

(unless (cl:find-package ~s)
  (cl::defpackage ~s (:use )))
(cl::in-package ~s)

(uffi:def-function (\"free\" uffi::c-free)
    ((o :pointer-void))
  :returning :void)

(cl:defmacro uffi::lboolm (x)
  `(cl:if (cl:> (cl:the cl:fixnum ,x) 0) cl:t cl:nil))
(cl:defun uffi::lbool (x)
  (uffi::lboolm x))

(cl:defmacro uffi::cboolm (x)
  `(cl:if ,x 1 0))
(cl:defun uffi::cbool (x)
  (uffi::cboolm x))

" (package-name (package self))
(package-name (package self))
(package-name (package self))
(package-name (package self)))
(when (foreign-lib self)
  (format (outfile self) "(uffi:load-foreign-library (cl:translate-logical-pathname ~s))~%~%" (foreign-lib self))
  ))

(defmethod register-type ((self uffi-cparser) type name)
  (let ((old-name (gethash type (types self))))
    (unless old-name
      (setf (gethash type (types self)) name))))

(defmethod make-lisp-name ((self uffi-cparser)(name string) &key local)
  ;;TODO: collision-checking
  (let ((lname (intern (string-upcase (uplow-to-dash name))(package self))))
    (cond (local nil)
	  ((gethash lname (symbols self))
	   (warn "Symbol ~a already found for ~a, new c-name ~a!" lname 
		 (gethash lname (symbols self)) 
		 name )
	   )
	  (t
	   (unless (find name (drop-symbols self) :test #'string-equal)
	     (setf (gethash lname (symbols self)) name))))
    lname
    )
  )



(defmethod define-enum ((self uffi-cparser) name type)
  (let* ((lisp-name (make-lisp-name self name))
;(prefix (concatenate 'string (symbol-name lisp-name) "#"))
	 (*print-case* :downcase)
	 )
    (declare (special *print-case*))
    (when (find name (drop-symbols self) :test #'string-equal)
      (return-from define-enum nil))
    (format (outfile self) "~&;; ~a ~a~%" (print-type self type) name)
    (register-type self type name)
    (print `(uffi:def-foreign-type ,lisp-name :int) (outfile self))
    (mapcar #'(lambda (enum)
		(print `(cl:defconstant ,(make-lisp-name
					  self 
					  (format nil "+~a+" (symbol-name (car enum))))
			    ,(cdr enum))
		       (outfile self)
		       )
		)
	    (enumerators type)))
  )

(defmethod map-type ((self uffi-cparser) type)
  (typecase type
    (pointer-type (typecase (to type)
		    (cparse-void           :pointer-void)
		    (struct-type           :pointer-void)
		    (func-type             :pointer-void)
		    (cparse-char
		     (if (find '|const| (qualifiers (to type)))
			 :const-cstring
		       :cstring))
		    (cparse-signed-char    :char)
		    (cparse-void           :pointer-void)
		    (t (error "Unhandled pointer ~a" (to type)))))
    (enum-type             :int)
    (cparse-short          :short)
    (cparse-unsigned-short :unsigned-short)
    (cparse-int            :int)
    (cparse-unsigned-int   :unsigned-int)
    (cparse-char           :char)
    (cparse-unsigned-char  :unsigned-char)
    (cparse-signed-char    :char)
    (cparse-long           :long)
    (cparse-unsigned-long  :unsigned-long)
    (cparse-cfloat         :float)
    (cparse-double         :double)
;(int                 `(:int fixnum))

    (cparse-void         :void)
    (t (error "Unhandled type ~s" type))
    )
  )

(defmethod create-function-args ((self uffi-cparser) (args list))
  (mapcar #'(lambda (arg)
	      (let* ((type (car arg))
		     (name (symbol-name (second arg)))
		     (lisp-name (make-lisp-name self name :local t)))
		(setf type (map-type self type))
		`(,lisp-name ,type)
		)
	      )
	  args)
  )

(defmethod print-type ((self uffi-cparser) type &optional types)
  (cond ((gethash type (types self))
	 (gethash type (types self))
	 )
	((find type types)
	 (format nil "~a" type)) ;;recursive case
	(t
	 (typecase type
    (pointer-type (typecase (to type)
		    (cparse-void "void*")
		    (struct-type (format nil "~a*" (print-type self (to type)
							       (cons type types))))
		    (func-type   "function*")
		    (cparse-char "char*")
		    (pointer-type (format nil "~a*" (print-type self (to type)
								(cons type types))))
		    (t (error "Unhandled pointer ~a" (to type)))))
    (enum-type (format nil "enum{~{~a~^, ~}}" (mapcar #'car (enumerators type))))
    (struct-type (format nil "struct{~{~a~^, ~}}" 
			 (mapcar #'(lambda (memb) 
				     (format nil "~a ~a"
					     (print-type self 
							 (first memb) (cons type
									    types))
					     (symbol-name (second memb)))
				     ) (members type))
			 ) )
    (union-type (format nil "union{~{~a~^,~}}" 
			 (mapcar #'(lambda (memb) 
				     (format nil "~a ~a"
					     (print-type self 
							 (first memb) (cons type
									    types))
					     (symbol-name (second memb)))
				     ) (members type))
			 ) )
    (cparse-short          "short")
    (cparse-unsigned-short "unsigned short")
    (cparse-int            "int")
    (cparse-unsigned-int   "unsigned int")
    (cparse-char           "char")
    (cparse-signed-char    "char")
    (cparse-unsigned-char  "unsigned char")
    (cparse-long           "long")
    (cparse-unsigned-long  "unsigned long")
    (cparse-cfloat         "float")
    (cparse-double         "double")
;(int                 `(:int fixnum))

    (cparse-void           "void")
    (t (error "Unhandled type ~s" type))
    ))
  ))

(defmethod print-signature ((self uffi-cparser) name func)
  (let (
	(args (mapcar #'(lambda (arg)
			  (format nil "~a ~a" 
				  (print-type self 
					      (first arg))
				  (symbol-name (second arg))))
			  
			  (args func))))
    ;(break)
    (format nil "~a ~a(~{~a~^, ~})" (print-type self (return-type func)) 
						    name args)
  ))


(defmethod get-arg-converters ((self uffi-cparser) args)
  (let ((free-forms))
    (values (mapcar #'(lambda (a) 
			(case (first (last a))
			  ((:const-cstring :cstring) 
			   (push 
			    `(uffi:free-cstring ,(first a)) free-forms)
			   `(setq ,(first a) (uffi:convert-to-cstring
					      ,(first a))))
			  (:bool `(setq ,(first a) (uffi::cboolm ,(first a)))))
			) args)
	    free-forms))
  )

(defmethod get-return-converter ((self uffi-cparser) return)
  (case return
    (:const-cstring 'uffi:convert-from-cstring)
    (:cstring       'uffi:convert-from-foreign-string)
    (:bool          'uffi::lboolm))
  )

(defmethod define-function ((self uffi-cparser) name func )
  (when (find name (drop-symbols self) :test #'string-equal)
    (return-from define-function nil))
  (handler-case 
  (let* ((lisp-name (make-lisp-name self name))
	 (return (list (map-type self (return-type func))))
	 (args (create-function-args self (args func)))
	 (booldef (find name (boolean-functions self) :key #'car :test #'string-equal))
	 (*print-case* :downcase)
	 retconv
	 (ffname lisp-name)
	 argnames
	 resname
	 )
    
    (declare (special *print-case*))
    
    (when booldef
      (setf booldef (cdr booldef))
      (when (and (find :return booldef)
		 (find (car return) '(:int :short :unsigned-int :unsigned-short)))
	(setf booldef (remove :return booldef))
	(setf return (append return (list :bool))))
      (setf args (mapcar #'(lambda (fa a)
			     (if (and (find (symbol-name (second fa))
					    booldef :test #'string-equal)
				      (find (second a) '(:int :short :unsigned-int :unsigned-short)))
				 (append a (list :bool))
			       a)
			     ) (args func) args))
      )
    (setf retconv (get-return-converter self (first (last return))))
    (multiple-value-bind (argconvs freeforms) (get-arg-converters self args)
      (format (outfile self) "~&;; Function ~a" (print-signature self name func))
      (when (not (some #'identity argconvs))
	(setf argconvs nil))
      (when (or retconv argconvs)
	(setf ffname (intern (symbol-name (gensym (symbol-name ffname))) (symbol-package ffname)))
	)
      (print `(uffi:def-function (,name ,ffname) ,(mapcar #'(lambda (a) 
							      (substitute
							       :cstring
							       :const-cstring 
							       (subseq a 0 2)))
							   args)
		:returning ,(case (car return)
			      (:const-cstring :cstring)
			      (:cstring  :pointer-void)
			      (t (car return)))
		)
	     (outfile self)
	     )
      (when (or retconv argconvs)
	(print `(cl:defun ,lisp-name ,(setf argnames (mapcar #'first args))
		  ,@(remove nil argconvs)
		  (let (,@(when (eq (car return) :cstring)
			    (setf resname (intern (symbol-name (gensym "RES")) (package self)))
			    (push `(when ,resname
				     (uffi::c-free ,resname)
				     ;(uffi:free-foreign-object ,resname)
				     ) freeforms)
			    `(,resname)))
		    (unwind-protect 
			,(cond (resname 
				`(progn
				   (setq ,resname (,ffname ,@argnames))
				   (,retconv ,resname)))
			       (retconv
				`(,retconv (,ffname ,@argnames))
				)
			       (t
				`(,ffname ,@argnames)
				))
		      nil;; To have at least one unwind-form
		      ,@(remove nil freeforms))))
	       (outfile self))
	)
      (format (outfile self) "~&~%")
    )
    )
  (serious-condition (e) 
    (warn "~&Error defining function ~a~%" name) 
    (apply #'format (list *error-output*
			  (or 
			   ;; cmu has no method for normal errors
			   (ignore-errors (simple-condition-format-control e))
			   "~a")
			  (or
			   (ignore-errors (simple-condition-format-arguments e))
			   e
			   )
			   ))
    )
  )
  )

(defmethod parse-cexpression ((self uffi-cparser) exp (s scope) (l lookahead-stream))
;(break "parse-cexpression ~a ~a ~a" (car exp) s l)
#+:test(test:debug-write "parse-cexpression ~a ~a ~a~%" (car exp) s l)
  (cond ((typep (car (car exp)) 'func-type) 
	 (define-function self (symbol-name (second (car exp))) (car (car exp))))
	((and (typep (caar exp) 'typedef-type)
	      (typep (defined-type (caar exp)) 'enum-type))
	 (define-enum self (symbol-name (second (car exp))) (defined-type (caar exp)))
	 
	 )
	((typep (caar exp) 'typedef-type)
	 (format (outfile self) "~&;;~a ~a~%" (print-type self 
						      (defined-type (caar exp)))
		 (symbol-name (second (car exp))))
	 (register-type self (defined-type (caar exp)) (symbol-name (second (car exp))))
	 )
	(t (break))
	
	)

  )

(defmethod parse ((self uffi-cparser))
  (unwind-protect
      (let ((*package* (package self)))
	(cparse-stream (infile self) 
		       :stmt-fun #'(lambda (exp &optional ex1 ex2) 
				     ;(break "ex ~a"exp)
;(test:debug-write "|~a ~a~%|" ex1 ex2)
				     (parse-cexpression self exp ex1 ex2))))
    (let ((syms nil))
      (maphash #'(lambda (k v) (push (symbol-name k) syms)) (symbols self))
      ;(sort syms #'string<)
      (format (outfile self)
	      "

(cl:defpackage ~s
  ~t~t(:use )
  ~t~t(:export 
       \"CBOOL\"
       \"LBOOL\"
       "
(package-name (package self)))
(dolist (s syms)
  (format (outfile self) "~t~t~s~%" s)
  )
(format (outfile self) "))")
)
(close (infile self))
(close (outfile self)))


)


;(define-c-function SBASE-INIT "SBase_init" ((SB :INT) (TC :INT)) :void)



#|
(setf p (make-instance 'uffi-cparser :infile "promot:libsbml;SBMLConvert.hl" 
		       :outfile "promot:libsbml;SBMLConvert.lisp"
		       :package "LIBSBML"
		       :boolean-functions '("SBase_isSetMetaId")
		       ))
 (parse p)
(load "promot:src;sbml;libsbml;SBMLConvert.lisp")

(setf p (make-instance 'uffi-cparser :infile "~/Projects/Promot/libsbml/SBMLConvert.hl" 
		       :outfile "~/Projects/Promot/SBMLConvert.lisp"
		       :boolean-functions '("SBase_isSetMetaId")
 :package "LIBSBML"))
 (parse p)
(load "promot:src;sbml;libsbml;SBMLConvert.lisp")

|#
