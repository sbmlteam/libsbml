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

(in-package "CMU-ALIEN")

(defmacro with-temp-file ((file-name &optional (base "/tmp/tmp~D~C"))
			  &body body)
  "Creates a temporary file and binds the name to FILE-NAME.  BASE is a format
string template for the temporary filename; it needs to accept a ~D and a ~C
argument."
  `(let ((,file-name (system::pick-temporary-file-name ,base)))
     (unwind-protect
	  (progn
	    ,@body)
       (delete-file ,file-name))))

(eval-when (compile load eval)
(defun c-name-to-lisp-name (str)
  (let ((lisp-name (string-upcase str)))
    (nsubstitute #\- #\_ lisp-name)))

(defgeneric intern-as-lisp (csym &optional package))

(defmethod intern-as-lisp ((csym symbol) &optional (package *package*))
  (intern (c-name-to-lisp-name (symbol-name csym)) package))

(defmethod intern-as-lisp ((csym string) &optional (package *package*))
  (intern (c-name-to-lisp-name csym) package))
)

(define-condition alien-not-implemented (cparse-error) ())

(defvar *struct-tags*)

(defun make-alien-defs (files &key cpp-args extra-cpp-lines)
  "Returns a form that declares, via def-alien-type, def-alien-routine
and def-alien-variable the types, functions and variables found in
FILES.

:CPP-ARGS is a list of command-line arguments for the C
preprocessor (for example, '(\"-I/usr/X11R6/include\").

:EXTRA-CPP-LINES are added to the file passed to the C preprocessor
and can be used for extra macro definitions, #includes, etc."
  (with-temp-file (cpp-file "/tmp/tmp~D~C.c")
    (let ((cpp-file-stream (open cpp-file
				 :direction :output :if-exists :overwrite))
	  (default-dir (namestring (ext:default-directory)))
	  (files (if (listp files)
		     files
		     (list files))))
      (dolist (line extra-cpp-lines)
	(write-line line cpp-file-stream))
      (dolist (file files)
	(format cpp-file-stream
		"#include \"~A\"~%"
		(if (eql (char file 0) #\/)
		    file
		    (concatenate 'string default-dir file))))
      (close cpp-file-stream))
    (with-open-stream
	(cpp-stream (ext:process-output
		     (ext:run-program "gcc"
				      (append cpp-args (list "-E" cpp-file))
				      :output :stream
				      :input t :wait nil)))
      (let ((defs nil)
	    (*struct-tags* (make-hash-table)))
	(flet ((stmt-fun (decls scope stream)
		 (declare (ignore scope stream))
		 (handler-case (setf defs (nconc (do-decls decls) defs))
		   (alien-not-implemented (condition)
		     (format *error-output* "~A~%Ignoring and moving on.~%"
			     condition)))))
	  (cparse-stream cpp-stream :stmt-fun #'stmt-fun)
	  `(progn ,@(nreverse defs)))))))

(defun do-decls (decls)
  (loop
   for (type-decl name) in decls
   collect (do-decl type-decl name)))

(defgeneric do-decl (type-decl name))

(defmethod do-decl ((type-decl t) name)
  (declare (ignore name))
  (error 'alien-not-implemented
	 :format-string "cmu-alien can't handle ~S yet (if ever)"
	 :format-arguments (list type-decl)))

(defmethod do-decl ((type-decl typedef-type) name)
  (let ((def-type (do-type-declarator (defined-type type-decl))))
    (unless (eq def-type 'c-call:void)
      `(def-alien-type ,(intern-as-lisp name) ,def-type))))

(defgeneric do-type-declarator (type-decl))

(defmethod do-type-declarator ((type-decl t))
  (error 'alien-not-implemented
	 :format-string "cmu-alien can't handle ~S yet (if ever)"
	 :format-arguments (list type-decl)))

(defmethod do-type-declarator ((type-decl enum-type))
  (let ((key-package (find-package "KEYWORD"))
	(tag (intern-as-lisp (tag type-decl))))
    (loop
     for (id . val) in (enumerators type-decl)
     collect (list (intern-as-lisp id key-package)
		   val)
     into enums
     finally (return `(enum ,tag ,@enums)))))

(macrolet ((frob-prim-type
	       (cparse-type
		&optional (c-call-type (intern-as-lisp
					(symbol-name cparse-type)
					(find-package "C-CALL"))))
	     `(defmethod do-type-declarator ((type-decl ,cparse-type))
		',c-call-type)))
  (frob-prim-type void)
  (frob-prim-type char)
  (frob-prim-type unsigned-char)
  (frob-prim-type signed-char c-call:char)
  (frob-prim-type short)
  (frob-prim-type unsigned-short)
  (frob-prim-type int)
  (frob-prim-type unsigned-int)
  (frob-prim-type long)
  (frob-prim-type unsigned-long)
  (frob-prim-type cfloat c-call:float)
  (frob-prim-type double))

(defun do-func-args-result (func-decl)
  (values (loop for (type arg) in (args func-decl)
		collect (list (intern-as-lisp arg)
			      (do-type-declarator type)))
	  (do-type-declarator (return-type func-decl))))

(defmethod do-decl ((type-decl func-type) name)
  (let ((lisp-name (intern-as-lisp name)))
    (multiple-value-bind (fun-args result)
	(do-func-args-result type-decl)
      `(progn
	 (declaim (inline ,lisp-name))
	 (def-alien-routine ,lisp-name
	     ,result
	   ,@fun-args)))))

;;; func-type when seen in typedefs, pointers and stuff
(defmethod do-type-declarator ((type-decl func-type))
  (multiple-value-bind (fun-args result)
      (do-func-args-result type-decl)
    `(function ,result ,@fun-args)))


(defmethod do-type-declarator ((type-decl pointer-type))
  (let ((to-type (do-type-declarator (to type-decl))))
    (if (eq to-type 'c-call:void)
	'(* t)
	`(* ,to-type))))

(defmethod do-decl ((type-decl compound-type) name)
  (unless name
    `(def-alien-type nil ,(do-type-declarator type-decl))))

(defgeneric compound-kind (type-decl))
(defmethod compound-kind ((type-decl struct-type))
  'struct)
(defmethod compound-kind ((type-decl union-type))
  'union)

(defmethod do-type-declarator ((type-decl compound-type))
  (let ((tag (intern-as-lisp (tag type-decl)))
	(kind (compound-kind type-decl)))
    (if (gethash tag *struct-tags*)
	`(,kind ,tag)
	(progn
	  (setf (gethash tag *struct-tags*) t)
	  (loop for mem in (and (slot-boundp type-decl 'members)
				(members type-decl))
		collect (list (intern-as-lisp (cadr mem))
			      (do-type-declarator (car mem)))
		into struct-members
		finally (return `(,kind ,(intern-as-lisp (tag type-decl))
				  ,@struct-members)))))))

;;; Anything other than a function declaration, structure or union or
;;; enum, or typedef is a variable declaration.

(defmethod do-decl ((type-decl c-type) name)
  `(def-alien-variable ,(intern-as-lisp name) ,(do-type-declarator type-decl)))

(defmethod do-type-declarator ((type-decl array-type))
  (let ((dimension (when (len type-decl)
		     (value (len type-decl)))))
    `(array ,(do-type-declarator (of type-decl)) ,dimension)))

