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

;;; Numeric types in C.  This file implements the representation of and
;;; arithmetic on types, but the types themselves can be used in
;;; classes that represent parse trees, variables and such.

(in-package "CPARSE")

(defclass c-super ()
  ())

#+PCL
(defmethod print-object ((obj c-super) stream)
  (let ((slots (mapcan #'(lambda (slot-def)
			   (let ((name (pcl:slot-definition-name slot-def)))
			     (if (slot-boundp obj name)
				 (list name (slot-value obj name))
				 nil)))
		       (pcl:class-slots (class-of obj)))))
    (print-unreadable-object  (obj stream :type t)
      (format stream "~<~@{~W ~@_~W~^ ~_~}~:>" slots))))

(defclass void (c-super)
  ())

(defclass c-const (c-super)
  ((value :accessor value :initarg :value)))

(defmacro defnumtype (cname super &body body)
  "Define class CNAME with superclasses SUPER and CNAME-CONST with superclasses
(,@SUPER c-const)"
  (let ((const-name (intern (concatenate 'string
					 (symbol-name cname) "-CONST"))))
    `(progn
       (defclass ,cname ,super ,@body)
       (defclass ,const-name (,cname c-const) ,@body))))

(defclass unsigned ()
  ())

(defclass cinteger-super ()
  ())

(defnumtype int (cinteger-super)
  ())

(defnumtype unsigned-int (unsigned int)
  ())

(defnumtype long (cinteger-super)
  ())

(defnumtype unsigned-long (unsigned long)
  ())

(defnumtype long-long (cinteger-super)
  ())

(defnumtype unsigned-long-long (unsigned long-long)
  ())

(defnumtype short (cinteger-super)
  ())

(defnumtype unsigned-short (unsigned short)
  ())

(defclass cfloat-super ()
  ())

(defnumtype cfloat (cfloat-super)
  ())

(defnumtype double (cfloat-super)
  ())

(defnumtype char ()
  ())

(defnumtype signed-char ()
  ())

(defnumtype unsigned-char (unsigned char)
  ())

;;; Is a type unsigned?  For characters this is defined by the
;;; implementation, so put it into a generic function.

(defgeneric unsignedp (comp-imp type))

(defmethod unsignedp (comp-imp (type t))
  nil)

(defmethod unsignedp (comp-imp (type unsigned))
  t)

(defgeneric max-val (comp-imp type))

(defmethod max-val (comp-imp (type cinteger-super))
  (1- (expt 2 (1- (type-width comp-imp type)))))

(defmethod min-val (comp-imp (type cinteger-super))
  (- (expt 2 (1- (type-width comp-imp type)))))

(defmethod max-val (comp-imp (type unsigned))
  (1- (expt 2 (type-width comp-imp type))))

(defmethod min-val (comp-imp (type unsigned))
  0)

;;; Class representing C compiler implementation characteristics.
;;; This includes whether or not char is signed and the widths of each type.

(defclass compiler-impl (c-super)
  ())

(defgeneric type-width (comp-imp type))

(defmethod type-width ((comp-imp compiler-impl) (type char))
  8)

(defmethod type-width ((comp-imp compiler-impl) (type unsigned-char))
  8)

(defun truncate-with-sign (val width)
  "Utility function to truncate a Lisp integer VAL of width WIDTH.  The sign
bit is extended."
  (let ((truncated (ldb (byte width 0) val)))
    (if (logbitp (1- width) val)
	(logior (ash -1 32) truncated)
	truncated)))



(defclass impl-32bit (compiler-impl)
  ())

(defmethod unsignedp ((comp-imp impl-32bit) (type char))
  t)

(defmethod unsignedp ((comp-imp impl-32bit) (type signed-char))
  nil)

(defmethod type-width ((comp-imp impl-32bit) (type short))
  16)

(defmethod type-width ((comp-imp impl-32bit) (type int))
  32)

(defmethod type-width ((comp-imp impl-32bit) (type long))
  32)

(defmethod type-width ((comp-imp impl-32bit) (type long-long))
  64)

(defmethod type-width ((comp-imp impl-32bit) (type cfloat))
  32)

(defmethod type-width ((comp-imp impl-32bit) (type double))
  64)

;;; XXX Hack to avoid dealing with pointer types at this level
(defgeneric pointer-width (comp-imp))

(defmethod pointer-width ((comp-imp impl-32bit))
  32)

(defgeneric pointer-alignment (comp-imp))

(defmethod pointer-alignment ((comp-imp impl-32bit))
  32)

(defgeneric type-alignment (comp-imp type))

(defmethod type-alignment ((comp-imp impl-32bit) (type t))
  (type-width comp-imp type))

(defmethod type-alignment ((comp-imp impl-32bit) (type long-long))
  32)

(defvar *target-compiler* (make-instance 'impl-32bit))

;;; Type promotion
;;; Avoid writing two versions of every method

(defconstant +type-precedence+
  '(int-const unsigned-int-const long-const unsigned-long-const
    long-long-const unsigned-long-long-const cfloat-const double-const))

(defun convert-operands (cimpl op1 op2)
  (let* ((real-op1 (promote cimpl op1))
	 (real-op2 (promote cimpl op2))
	 (op1-prec (position (type-of real-op1) +type-precedence+))
	 (op2-prec (position (type-of real-op2) +type-precedence+)))
    (cond ((eql op1-prec op2-prec)
	   (values real-op1 real-op2))
	  ((> op1-prec op2-prec)
	   (convert-operand-type cimpl real-op1 real-op2))
	  (t (multiple-value-bind (new-op2 new-op1)
		 (convert-operand-type cimpl real-op2 real-op1)
	       (values new-op1 new-op2))))))

(defgeneric promote (cimpl cval))

(defmethod promote ((cimpl impl-32bit) (cval t))
  cval)

(defmethod promote ((cimpl impl-32bit) (cval char-const))
  (make-instance 'int-const :value (value cval)))

(defmethod promote ((cimpl impl-32bit) (cval unsigned-char-const))
  (make-instance 'int-const :value (value cval)))

(defmethod promote ((cimpl impl-32bit) (cval short-const))
  (make-instance 'int-const :value (value cval)))

(defmethod promote ((cimpl impl-32bit) (cval unsigned-short-const))
  (make-instance 'int-const :value (value cval)))

;;; Convert op2 to the type of op1
(defgeneric convert-operand-type (cimpl op1 op2))

(defmethod convert-operand-type ((cimpl impl-32bit) (op1 double-const) op2)
  (values op1
	  (make-instance 'double-const
			 :value (float (value op2) (value op1)))))

(defmethod convert-operand-type ((cimpl impl-32bit) (op1 cfloat-const) op2)
  (values op1
	  (make-instance 'cfloat-const
			 :value (float (value op2) (value op1)))))

(defmethod convert-operand-type ((cimpl impl-32bit)
				 (op1 unsigned-long-const) op2)
  (values op1
	  (make-instance 'unsigned-long-const
			 :value (ldb (byte 32 0) (value op2)))))

;;; Long can't accommodate all unsigned values, so both operands get converted
;;; to unsigned long.
(defun convert-long-to-unsigned (cimpl op1 op2 promote-class)
  (values (make-instance promote-class
			 :value (ldb (byte (type-width cimpl op1) 0)
				     (value op1)))
	  (make-instance promote-class
			 :value (ldb (byte (type-width cimpl op2) 0)
				     (value op2)))))

(defmethod convert-operand-type ((cimpl impl-32bit)
				 (op1 long-const) (op2 unsigned))
  (convert-long-to-unsigned cimpl op1 op2 'unsigned-long-const))

(defmethod convert-operand-type ((cimpl impl-32bit) (op1 long-const) op2)
  (values op1 (make-instance 'long-const :value (value op2))))

(defmethod convert-operand-type ((cimpl impl-32bit)
				 (op1 long-long-const) (op2 unsigned))
  (convert-long-to-unsigned cimpl op1 op2 'unsigned-long-long-const))

(defmethod convert-operand-type ((cimpl impl-32bit)
				 (op1 unsigned-int-const) op2)
  (values op1
	  (make-instance 'unsigned-int-const
			 :value (ldb (byte 32 0) (value op2)))))

;;; Sanity check: if the first operand is an int, the second better be too.
(defmethod convert-operand-type ((cimpl impl-32bit)
				 (op1 int-const) (op2 int-const))
  (values op1 op2))

;;; Make types compatible before doing arithmetic

(defun c+ (cimpl op1 op2)
  (multiple-value-bind (op1-converted op2-converted)
      (convert-operands cimpl op1 op2)
    (c+-internal cimpl op1-converted op2-converted)))

(defgeneric c+-internal (cimpl op1 op2))

(defmethod c+-internal ((cimpl impl-32bit)
			(op1 double-const) (op2 double-const))
  (make-instance 'double-const :value (+ (value op1) (value op2))))

(defmethod c+-internal ((cimpl impl-32bit)
			(op1 cfloat-const) (op2 cfloat-const))
  (make-instance 'cfloat-const :value (+ (value op1) (value op2))))

;;; Shortcuts so we don't have to write methods for all the integer types
(defmethod c+-internal ((cimpl impl-32bit) (op1 unsigned) (op2 unsigned))
  (let ((bytespec (byte (type-width cimpl op1) 0))
	(result (+ (value op1) (value op2))))
    (make-instance (class-of op1) :value (ldb bytespec result))))

(defmethod c+-internal ((cimpl impl-32bit)
			(op1 cinteger-super)
			(op2 cinteger-super))
  (let ((result (+ (value op1) (value op2))))
    (make-instance (class-of op1)
		   :value (truncate-with-sign result
					      (type-width cimpl op1)))))

(defmacro def-c-op (c-func float-func int-func)
  (let ((internal-op (intern (concatenate 'simple-string (string c-func)
					  "-INTERNAL")
			     :cparse)))
    `(progn
      (defun ,c-func (cimpl op1 op2)
	(multiple-value-bind (op1-converted op2-converted)
	    (convert-operands cimpl op1 op2)
	  (,internal-op cimpl op1-converted op2-converted)))
      (defgeneric ,internal-op (cimpl op1 op2))
      ,@(when float-func
	  `((defmethod ,internal-op ((cimpl impl-32bit)
				     (op1 double-const) (op2 double-const))
	      (make-instance 'double-const
	       :value (,float-func (value op1) (value op2))))
	    (defmethod ,internal-op ((cimpl impl-32bit)
				     (op1 cfloat-const) (op2 cfloat-const))
	      (make-instance 'cfloat-const
	       :value (,float-func (value op1) (value op2))))))
      ;; Shortcuts so we don't have to write methods for all the integer types
      ,@(when int-func
	  `((defmethod ,internal-op ((cimpl impl-32bit) (op1 unsigned)
				     (op2 unsigned))
	      (let ((bytespec (byte (type-width cimpl op1) 0))
		    (result (,int-func (value op1) (value op2))))
		(make-instance (class-of op1) :value (ldb bytespec result))))
	    (defmethod ,internal-op ((cimpl impl-32bit)
				     (op1 cinteger-super)
				     (op2 cinteger-super))
	      (let ((result (,int-func (value op1) (value op2))))
		(make-instance (class-of op1)
			       :value
			       (truncate-with-sign result
						   (type-width cimpl
							       op1))))))))))


(def-c-op c- - -)
(def-c-op c* * *)
(def-c-op c/ / truncate)
(def-c-op c>> nil (lambda (x y) (ash x (- y))))
(def-c-op c<< nil (lambda (x y) (ash x y)))

(macrolet ((frob-boolean-op (op)
	     (let ((generic-name (intern (concatenate 'string
						      "C" (symbol-name op))))
		   (internal-name (intern (concatenate 'string
						       "%C"
						       (symbol-name op)))))
	       `(progn
		  (defun ,internal-name (x y)
		    (if (,op x y) 1 0))
		  (def-c-op ,generic-name ,internal-name ,internal-name)))))
  (frob-boolean-op >)
  (frob-boolean-op <)
  (frob-boolean-op <=)
  (frob-boolean-op >=)
  (frob-boolean-op =))

(def-c-op c!=
  (lambda (x y) (if (not (= x y)) 1 0))
  (lambda (x y) (if (not (= x y)) 1 0)))

(def-c-op c& nil (lambda (x y) (logand x y)))
(def-c-op c-logxor nil (lambda (x y) (logxor x y)))
(def-c-op c-logior nil (lambda (x y) (logior x y)))
(def-c-op c-and nil (lambda (x y)
		      (if (and (not (= x 0)) (not (= y 0)))
			  1
			  0)))
(def-c-op c-or nil (lambda (x y)
		     (if (or (not (= x 0)) (not (= y 0)))
			 1
			 0)))

;;; Unary ops

(defun cneg (cimpl op)
  (cneg-internal cimpl op))

(defgeneric cneg-internal (cimpl op))

(defmethod cneg-internal (cimpl (op c-const))
  (declare (ignore cimpl))
  (make-instance (class-of op) :value (- (value op))))

(defun c! (cimpl op)
  (c!-internal cimpl op))

(defgeneric c!-internal (cimpl op))

(defmethod c!-internal (cimpl (op c-const))
  (make-instance 'int-const
		 :value (if (= (value op) 0)
			    1
			    0)))

(defun c~ (cimpl op)
  (c~-internal cimpl op))

(defgeneric c~-internal (cimpl op))

(defmethod c~-internal (cimpl (op cinteger-super))
  (make-instance (class-of op)
		 :value (truncate-with-sign (lognot (value op))
					    (type-width cimpl op))))
