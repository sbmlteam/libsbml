;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: UFFI -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          strings.lisp
;;;; Purpose:       UFFI source to handle strings, cstring and foreigns
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Feb 2002
;;;;
;;;; $Id$
;;;;
;;;; This file, part of UFFI, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; UFFI users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package :uffi)


(defvar +null-cstring-pointer+
    #+(or cmu sbcl scl) nil
    #+allegro 0
    #+lispworks (fli:make-pointer :address 0 :type '(:unsigned :char))
    #+mcl (ccl:%null-ptr)
)

(defmacro convert-from-cstring (obj)
  "Converts a string from a c-call. Same as convert-from-foreign-string, except
that LW/CMU automatically converts strings from c-calls."
  #+(or cmu sbcl lispworks scl) obj
  #+allegro 
  (let ((stored (gensym)))
    `(let ((,stored ,obj))
       (if (zerop ,stored)
	   nil
	 (values (excl:native-to-string ,stored)))))
  #+mcl 
  (let ((stored (gensym)))
    `(let ((,stored ,obj))
       (if (ccl:%null-ptr-p ,stored)
	   nil
	 (values (ccl:%get-cstring ,stored)))))
  )

(defmacro convert-to-cstring (obj)
  #+(or cmu sbcl scl lispworks) obj
  #+allegro
  `(if (null ,obj)
    0
    (values (excl:string-to-native ,obj)))
  #+mcl
  `(if (null ,obj)
    +null-cstring-pointer+
    (let ((ptr (new-ptr (1+ (length ,obj)))))
      (ccl::%put-cstring ptr ,obj)
      ptr))
  )

(defmacro free-cstring (obj)
  #+(or cmu sbcl scl lispworks) (declare (ignore obj))
  #+allegro
  `(unless (zerop ,obj)
     (ff:free-fobject ,obj))
  #+mcl
  `(unless (ccl:%null-ptr-p ,obj)
     (dispose-ptr ,obj))
  )

(defmacro with-cstring ((cstring lisp-string) &body body)
  #+(or cmu sbcl scl lispworks)
  `(let ((,cstring ,lisp-string)) ,@body) 
  #+allegro
  (let ((acl-native (gensym)))
    `(excl:with-native-string (,acl-native ,lisp-string)
       (let ((,cstring (if ,lisp-string ,acl-native 0)))
	 ,@body)))
  #+mcl
  `(if (stringp ,lisp-string)
     (ccl:with-cstrs ((,cstring ,lisp-string))
       ,@body)
     (let ((,cstring +null-cstring-pointer+))
       ,@body))
  )

(defmacro with-cstrings (bindings &rest body)
  (if bindings
      `(with-cstring ,(car bindings)
	(with-cstrings ,(cdr bindings)
	  ,@body))
      `(progn ,@body)))

;;; Foreign string functions

(defmacro convert-to-foreign-string (obj)
  #+lispworks
  `(if (null ,obj)
    +null-cstring-pointer+
    (fli:convert-to-foreign-string ,obj :external-format '(:latin-1 :eol-style :lf)))
  #+allegro
  `(if (null ,obj)
       0
     (values (excl:string-to-native ,obj)))
  #+(or cmu scl)
  (let ((size (gensym))
	(storage (gensym))
	(i (gensym)))
    `(etypecase ,obj
      (null 
       (alien:sap-alien (system:int-sap 0) (* (alien:unsigned 8))))
      (string
       (let* ((,size (length ,obj))
	      (,storage (alien:make-alien (alien:unsigned 8) (1+ ,size))))
	 (setq ,storage (alien:cast ,storage (* (alien:unsigned 8))))
	 (locally
	     (declare (optimize (speed 3) (safety 0)))
	   (dotimes (,i ,size)
	     (declare (fixnum ,i))
	     (setf (alien:deref ,storage ,i) (char-code (char ,obj ,i))))
	   (setf (alien:deref ,storage ,size) 0))
	 ,storage))))
  #+sbcl
  (let ((size (gensym))
	(storage (gensym))
	(i (gensym)))
    `(etypecase ,obj
      (null 
       (sb-alien:sap-alien (sb-sys:int-sap 0) (* (sb-alien:unsigned 8))))
      (string
       (let* ((,size (length ,obj))
	      (,storage (sb-alien:make-alien (sb-alien:unsigned 8) (1+ ,size))))
	 (setq ,storage (sb-alien:cast ,storage (* (sb-alien:unsigned 8))))
	 (locally
	     (declare (optimize (speed 3) (safety 0)))
	   (dotimes (,i ,size)
	     (declare (fixnum ,i))
	     (setf (sb-alien:deref ,storage ,i) (char-code (char ,obj ,i))))
	   (setf (sb-alien:deref ,storage ,size) 0))
	 ,storage))))
  #+mcl
  `(if (null ,obj)
       +null-cstring-pointer+
     (let ((ptr (new-ptr (1+ (length ,obj)))))
       (ccl::%put-cstring ptr ,obj)
       ptr))
  )


;; Either length or null-terminated-p must be non-nil
(defmacro convert-from-foreign-string (obj &key
					   length
					   (locale :default)
					   (null-terminated-p t))
  #+allegro
  `(if (zerop ,obj)
       nil
     (if (eq ,locale :none)
	 (fast-native-to-string ,obj ,length)
       (values
	(excl:native-to-string
	 ,obj 
	 ,@(when length (list :length length))
	 :truncate (not ,null-terminated-p)))))
  #+lispworks
  `(if (fli:null-pointer-p ,obj)
       nil
     (if (eq ,locale :none)
	 (fast-native-to-string ,obj ,length)
       (fli:convert-from-foreign-string 
	,obj
	,@(when length (list :length length))
	:null-terminated-p ,null-terminated-p
	:external-format '(:latin-1 :eol-style :lf))))
  #+(or cmu scl)
  `(if (null-pointer-p ,obj)
    nil
    (cmucl-naturalize-cstring (alien:alien-sap ,obj)
     :length ,length
     :null-terminated-p ,null-terminated-p))
  #+sbcl
  `(if (null-pointer-p ,obj)
    nil
    (sbcl-naturalize-cstring (sb-alien:alien-sap ,obj)
     :length ,length
     :null-terminated-p ,null-terminated-p))
  #+mcl
  (declare (ignore null-terminated-p))
  #+mcl
  `(if (ccl:%null-ptr-p ,obj)
     nil
    #+(and mcl (not openmcl)) (ccl:%get-cstring ,obj 0 ,@(if length (list length) nil))
    #+openmcl ,@(if length
		    `((ccl:%str-from-ptr ,obj ,length))
		    `((ccl:%get-cstring ,obj))))
  )


(defmacro allocate-foreign-string (size &key (unsigned t))
  #+ignore
  (let ((array-def (gensym)))
    `(let ((,array-def (list 'alien:array 'c-call:char ,size)))
       (eval `(alien:cast (alien:make-alien ,,array-def) 
			  ,(if ,unsigned 
			       '(* (alien:unsigned 8))
			     '(* (alien:signed 8)))))))

  #+(or cmu scl)
  `(alien:make-alien ,(if unsigned 
			     '(alien:unsigned 8)
			     '(alien:signed 8))
    ,size)

  #+sbcl
  `(sb-alien:make-alien ,(if unsigned 
			     '(sb-alien:unsigned 8)
			     '(sb-alien:signed 8))
    ,size)

  #+lispworks
  `(fli:allocate-foreign-object :type 
				,(if unsigned 
				     ''(:unsigned :char) 
				   :char)
				:nelems ,size)
  #+allegro
  (declare (ignore unsigned))
  #+allegro
  `(ff:allocate-fobject :char :c ,size)
  #+mcl
  (declare (ignore unsigned))
  #+mcl
  `(new-ptr ,size)
  )

(defun foreign-string-length (foreign-string)
  #+allegro `(ff:foreign-strlen ,foreign-string)
  #-allegro
  `(loop with size = 0
    until (char= (deref-array ,foreign-string '(:array :unsigned-char) size) #\Null)
    do (incf size)
    finally return size))


(defmacro with-foreign-string ((foreign-string lisp-string) &body body)
  (let ((result (gensym)))
    `(let* ((,foreign-string (convert-to-foreign-string ,lisp-string))
	    (,result (progn ,@body)))
      (declare (dynamic-extent ,foreign-string))
      (free-foreign-object ,foreign-string)
      ,result)))


;; Modified from CMUCL's source to handle non-null terminated strings
#+cmu
(defun cmucl-naturalize-cstring (sap &key length (null-terminated-p t))
  (declare (type system:system-area-pointer sap))
  (locally
      (declare (optimize (speed 3) (safety 0)))
    (let ((null-terminated-length
	   (when null-terminated-p
	     (loop
		 for offset of-type fixnum upfrom 0
		 until (zerop (system:sap-ref-8 sap offset))
		 finally (return offset)))))
      (if length
	  (if (and null-terminated-length
		   (> (the fixnum length) (the fixnum null-terminated-length)))
	      (setq length null-terminated-length))
	(setq length null-terminated-length)))
    (let ((result (make-string length)))
      (kernel:copy-from-system-area sap 0
				    result (* vm:vector-data-offset
					      vm:word-bits)
				    (* length vm:byte-bits))
      result)))

#+scl
;; kernel:copy-from-system-area doesn't work like it does on CMUCL or SBCL,
;; so have to iteratively copy from sap
(defun cmucl-naturalize-cstring (sap &key length (null-terminated-p t))
  (declare (type system:system-area-pointer sap))
  (locally
      (declare (optimize (speed 3) (safety 0)))
    (let ((null-terminated-length
	   (when null-terminated-p
	     (loop
		 for offset of-type fixnum upfrom 0
		 until (zerop (system:sap-ref-8 sap offset))
		 finally (return offset)))))
      (if length
	  (if (and null-terminated-length
		   (> (the fixnum length) (the fixnum null-terminated-length)))
	      (setq length null-terminated-length))
	(setq length null-terminated-length)))
    (let ((result (make-string length)))
      (dotimes (i length)
	(declare (type fixnum i))
	(setf (char result i) (code-char (system:sap-ref-8 sap i))))
      result)))

#+sbcl
(defun sbcl-naturalize-cstring (sap &key length (null-terminated-p t))
  (declare (type sb-sys:system-area-pointer sap))
  (locally
      (declare (optimize (speed 3) (safety 0)))
    (let ((null-terminated-length
	   (when null-terminated-p
	     (loop
		 for offset of-type fixnum upfrom 0
		 until (zerop (sb-sys:sap-ref-8 sap offset))
		 finally (return offset)))))
      (if length
	  (if (and null-terminated-length
		   (> (the fixnum length) (the fixnum null-terminated-length)))
	      (setq length null-terminated-length))
	(setq length null-terminated-length)))
    (let ((result (make-string length)))
      (sb-kernel:copy-from-system-area sap 0
				    result (* sb-vm:vector-data-offset
					      sb-vm:n-word-bits)
				    (* length sb-vm:n-byte-bits))
      result)))

(eval-when (:compile-toplevel :load-toplevel :execute)
   (def-function "strlen"
     ((str (* :unsigned-char)))
     :returning :unsigned-int))

(def-type char-ptr-def (* :unsigned-char))

#+(or lispworks (and allegro (not ics)))
(defun fast-native-to-string (s len)
  (declare (optimize (speed 3) (space 0) (safety 0) (compilation-speed 0))
	   (type char-ptr-def s))
  (let* ((len (or len (strlen s)))
         (str (make-string len)))
    (declare (fixnum len)
	     (type (simple-array (signed-byte 8) (*)) str))
    (dotimes (i len str)
      (setf (aref str i) 
	(uffi:deref-array s '(:array :char) i)))))

#+(and allegro ics)
(defun fast-native-to-string (s len)
  (declare (optimize (speed 3) (space 0) (safety 0) (compilation-speed 0))
	   (type char-ptr-def s))
  (let* ((len (or len (strlen s)))
         (str (make-string len)))
      (dotimes (i len str)
        (setf (schar str i) (code-char (uffi:deref-array s '(:array :unsigned-byte) i))))))
