(in-package :cparse)

(defclass allegro-cparser ()
  ((infile :accessor infile :initarg :infile)
   (outfile :accessor outfile :initarg :outfile)
   (symbols :accessor symbols :initform (make-hash-table))
   (types :accessor types :initform (make-hash-table))
   (boolean-functions :accessor boolean-functions 
		      :initarg :boolean-functions   
		      :initform nil
		      :documentation "List of boolean functions, that should
  have argument and return conversion. Entries can be strings \"function-name\" or
lists (\"function-name\" [:return] \"argname\"+)"
		      )
   (package :accessor package :initarg :package :initform *package*)
   ))

(defmethod normalize-boolean-functions ((self allegro-cparser))
  (setf (boolean-functions self)
    (mapcar #'(lambda (f)
		(if (stringp f)
		    (list f :return)
		  f))
	    (boolean-functions self)))
  )

(defmethod initialize-instance ((self allegro-cparser) &key infile outfile &allow-other-keys)
  (call-next-method)
  (let ((infile (when (slot-boundp self 'infile)
		  (infile self)))
	(outfile (when (slot-boundp self 'outfile)
		   (outfile self))))
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
    (format (outfile self) ";; -*- model:common-lisp package: ~a -*-
(cl::defpackage ~s (:use ))
(cl::in-package ~s)

(cl:defmacro lboolm (x)
  `(cl:if (cl:> (cl:the cl:fixnum ,x) 0) cl:t cl:nil))
(cl:defun lbool (x)
  (lboolm x))

(cl:defmacro cboolm (x)
  `(cl:if ,x 1 0))
(cl:defun cbool (x)
  (cboolm x))

" (package-name (package self))
(package-name (package self))
(package-name (package self)))
    ))



(defmethod register-type ((self allegro-cparser) type name)
  (let ((old-name (gethash type (types self))))
    (unless old-name
      (setf (gethash type (types self)) name))))

(defmethod make-lisp-name ((self allegro-cparser)(name string) &key local)
  ;;TODO: collision-checking
  (let ((lname (intern (string-upcase (uplow-to-dash name))(package self))))
    (cond (local nil)
	  ((gethash lname (symbols self))
	   (warn "Symbol ~a already found for ~a, new c-name ~a!" lname 
		 (gethash lname (symbols self)) 
		 name )
	   )
	  (t (setf (gethash lname (symbols self)) name)))
    lname
    )
  )



(defmethod define-enum ((self allegro-cparser) name type)
  (let* ((lisp-name (make-lisp-name self name))
;(prefix (concatenate 'string (symbol-name lisp-name) "#"))
	 )
    (format (outfile self) "~&;; ~a ~a~%" (print-type self type) name)
    (register-type self type name)
    (print `(ff:def-foreign-type ,lisp-name :int) (outfile self))
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

(defmethod map-type ((self allegro-cparser) type)
  (typecase type
    (pointer-type (typecase (to type)
		    (cparse-void :int)
		    (struct-type :int)
		    (func-type   :int)
		    (cparse-char :string)
		    (t (break "Unhandled pointer ~a" (to type)))))
    (enum-type :int)
    (cparse-short          :short)
    (cparse-unsigned-short :unsigned-short)
    (cparse-int            :int)
    (cparse-unsigned-int   :unsigned-int)
    (cparse-char           :char)
    (cparse-unsigned-char  :unsigned-char)
    (cparse-long           :long)
    (cparse-unsigned-long  :unsigned-long)
    (cparse-cfloat         :float)
    (cparse-double         :double)
;(int                 `(:int fixnum))

    (cparse-void         :void)
    (t (break "Unhandled type ~s" type))
    )
  )


(defmethod create-function-args ((self allegro-cparser) (args list))
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

(defmethod print-type ((self allegro-cparser) type &optional types)
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
		    (t (break "Unhandled pointer ~a" (to type)))))
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
    (cparse-unsigned-char  "unsigned char")
    (cparse-long           "long")
    (cparse-unsigned-long  "unsigned long")
    (cparse-cfloat         "float")
    (cparse-double         "double")
;(int                 `(:int fixnum))

    (cparse-void           "void")
    (t (break "Unhandled type ~s" type))
    ))
  ))

(defmethod print-signature ((self allegro-cparser) name func)
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

(defmethod define-function ((self allegro-cparser) name func )
  (let ((lisp-name (make-lisp-name self name))
	(return (map-type self (return-type func)))
	(args (create-function-args self (args func)))
	(booldef (find name (boolean-functions self) :key #'name))
	)
    (format (outfile self) "~&;; Function ~a" (print-signature self name func))
    (print `(define-c-function ,lisp-name ,name ,args ,return)
	   (outfile self)
	   )
    (format (outfile self) "~&~%")
    )
  )

(defmethod parse-cexpression ((self allegro-cparser) exp (s scope) (l lookahead-stream))
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

(defmethod parse ((self allegro-cparser))
  (unwind-protect
      (let ((*package* (package self)))
	(cparse-stream (infile self) 
		       :stmt-fun #'(lambda (exp &optional ex1 ex2) 
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

(defmacro define-c-function (name cname args return)
  (let ((ffname name)
	(string-args nil)
	argnames
	)
    (setf args (mapcar #'(lambda (arg) 
			   (case (second arg)
			     (:string 
			      (setf string-args t)
			      `(,(first arg) (* :char) cl::string))
			     (t arg))
			   ) args))
    (when (eq return :string)
      (setf ffname (intern (gensym (symbol-name name)) (symbol-package name))))
    #+(and :allegro (not :uffi))    
    `(progn 
       (ff:def-foreign-call (,ffname ,cname) 
	   ,args 
	 :convention :c
	 ,@(when string-args
	     `(:strings-convert t))
	 ,@(case return 
	     (:void nil)
	     (:string `(:returning :int))
	     (t `(:returning ,return))))
       ,@(when (eq return :string)
	   `((defun ,name ,(setf argnames (mapcar #'first args))
	       (let ((res (,ffname ,@argnames)))
		 (when (> res 0)
		   (excl:native-to-string res))))))
       )
    
    
    ))
;(define-c-function SBASE-INIT "SBase_init" ((SB :INT) (TC :INT)) :void)




;(setf p (make-instance 'allegro-cparser :infile "promot:src;sbml;libsbml;SBMLConvert.hl" 
;		       :outfile "promot:src;sbml;libsbml;SBMLConvert.lisp"
; :package "LIBSBML"))
; (parse p)
; (load "promot:src;sbml;libsbml;SBMLConvert.lisp")
