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

(in-package "CPARSE")

(defparameter *cparse-version* "0.2.4")

(defparameter *end-of-file* (list nil))

(defparameter *cparse-package* (find-package "CPARSE"))

(defvar *compiler-implementation* nil)

(defvar *cparse-debug* nil)

(defclass lookahead-stream ()
  ((stream :accessor stream :initarg :stream)
   (lookahead-tok
    :documentation "Holds the last token created by consume.
Conceptually, the head of a lookahead stack of tokens")
   (lookahead-tok-stack :initform nil
    :documentation "A stack of tokens that have been \"unread.\"")
   (file-name :accessor file-name :initarg :file-name)
   (line-number :accessor line-number :initarg :line-number :initform 1)
   (unread-stack :accessor unread-stack
		 :initform (make-array 10
				       :element-type 'character
				       :adjustable t
				       :fill-pointer 0))))

(defmethod initialize-instance :after ((lstream lookahead-stream)
				       &key &allow-other-keys)
  (with-slots (stream) lstream
    (loop for first-char = (peek-char nil stream)
	  while (eql first-char #\#)
	  do (parse-line-comment lstream))))

;;; Error reporting using line number information stored in the
;;; lookahead-stream.  We define a *current-lstream* special variable,
;;; even though it isn't used by any of the parsing functions!  We
;;; probably want to move in that direction.

(defvar *current-lstream*)

(define-condition cparse-error (error)
  ((file-name :reader cparse-error-file-name :initarg :file-name
	      :initform (when (boundp '*current-lstream*)
			  (file-name *current-lstream*)))
   (line-number :reader cparse-error-line-number :initarg :line-number
		:initform (when (boundp '*current-lstream*)
			    (line-number *current-lstream*)))
   (format-string :reader cparse-error-format-string
		  :initarg :format-string
		  :initform "parse error")
   (format-arguments :reader cparse-error-format-arguments
		     :initarg :format-arguments
		     :initform nil))
  (:report (lambda (condition stream)
	     (format stream "~A ~D: ~?~%"
		     (cparse-error-file-name condition)
		     (cparse-error-line-number condition)
		     (cparse-error-format-string condition)
		     (cparse-error-format-arguments condition)))))

(defun cparse-error (format-string &rest format-args)
  (error 'cparse-error
	 :format-string format-string
	 :format-arguments format-args))

;;; Basic read-char/unread-char/peek-char for lstreams which have
;;; unlimited unread ability.

(defgeneric %read-char (stream &optional eof-errorp eof-value recursive-p))

(defmethod %read-char ((s lookahead-stream)
		       &optional (eof-errorp t) eof-value recursive-p)
  (with-slots (stream unread-stack) s
    (if (eql (fill-pointer unread-stack) 0)
	(read-char stream eof-errorp eof-value recursive-p)
	(vector-pop unread-stack))))

(defgeneric %unread-char (c stream))

(defmethod %unread-char ((c character) (s lookahead-stream))
  (vector-push-extend c (unread-stack s))
  nil)

(defgeneric %peek-char (peek-type stream
			&optional eof-errorp eof-value recursive-p))

(defmethod %peek-char ((peek-type null) (s lookahead-stream)
		       &optional (eof-errorp t) eof-value recursive-p)
  (with-slots (stream unread-stack) s
    (if (eql (fill-pointer unread-stack) 0)
	(peek-char nil stream eof-errorp eof-value recursive-p)
	(aref unread-stack (1- (fill-pointer unread-stack))))))

;;; Implement the peek-char behavior of seeking ahead for a character,
;;; discarding others along the way.

(defmethod %peek-char ((peek-type character) (s lookahead-stream)
		       &optional (eof-errorp t) eof-value recursive-p)
  (with-slots (stream unread-stack) s
    (unless (eql (fill-pointer unread-stack) 0)
      (let ((pos (position peek-type unread-stack :from-end t)))
	(cond (pos
	       ;; top of stack?
	       (unless (eql (1+ pos) (fill-pointer unread-stack)) 
		 (setf (fill-pointer unread-stack) (1+ pos)))
	       (return-from %peek-char peek-type))
	      ;; Not found, drain unread-stack
	      (t (setf (fill-pointer unread-stack) 0)))))
    (peek-char peek-type stream eof-errorp eof-value recursive-p)))

;;; Just like read-line, but deals with the unread stack.
;;; XXX Doesn't deal with eof properly...

(defgeneric %read-line (stream &optional eof-errorp eof-value recursive-p))

(defmethod %read-line ((s lookahead-stream)
		       &optional (eof-errorp t) eof-value recursive-p)
  (with-slots (stream unread-stack) s
    (let ((newline-pos (position #\Newline unread-stack :from-end t)))
      (cond (newline-pos
	     (multiple-value-prog1
		 (values (reverse (subseq unread-stack (1+ newline-pos))) nil)
	       (setf (fill-pointer unread-stack) newline-pos)))
	    (t (let ((partial-line nil))
		 (when (> (fill-pointer unread-stack) 0)
		   (setf partial-line (reverse unread-stack)
			 (fill-pointer unread-stack) 0))
		 (multiple-value-bind (line-read newline-missing-p)
		     (read-line stream eof-errorp eof-value recursive-p)
		   (values (concatenate 'string partial-line line-read)
			   newline-missing-p))))))))

(defgeneric look (stream)
  (:documentation "Get the most recently read token from LSTREAM,
without changing its state (except at initialization)."))

(defmethod look ((lstream lookahead-stream))
  (with-slots (lookahead-tok) lstream
    (if (not (slot-boundp lstream 'lookahead-tok))
	(setf lookahead-tok (get-token lstream)))
    lookahead-tok))

(defgeneric consume (stream)
  (:documentation "Get a new token from LSTREAM."))

(defmethod consume ((lstream lookahead-stream))
  (with-slots (lookahead-tok lookahead-tok-stack) lstream
    ;; returns lookahead-tok
    (setf lookahead-tok (if lookahead-tok-stack
			    (pop lookahead-tok-stack)
			    (get-token lstream)))))

(defgeneric push-back (token stream)
  (:documentation "Push TOKEN back on STREAM.  The old lookahead token
is saved to be returned in the future by CONSUME."))

(defmethod push-back (token (lstream lookahead-stream))
  (with-slots (lookahead-tok lookahead-tok-stack) lstream
    (push lookahead-tok lookahead-tok-stack)
    (setf lookahead-tok token)))

;;; Check if the stream is at end-of-file.
(defgeneric eof-p (lstream))

(defmethod eof-p ((lstream lookahead-stream))
  (let* ((maybe-eof (%peek-char nil lstream nil *end-of-file*)))
    (eq *end-of-file* maybe-eof)))

(defun whitespace-p (char)
  (member char '(#\Space #\Newline #\Tab #\Page)))

;;; Fancy parsing of "# 1 "/usr/include/sys/types.h" 1 3" statements
;;; in C files that have been run through the C preprocessor.

(defun readc (lstream)
  (let ((the-char (%read-char lstream)))
    (if (eql the-char #\Newline)
	(loop for next-char = (%peek-char nil lstream nil nil)
	      while (eql next-char #\#)
	      do (parse-line-comment lstream)
	      finally (when next-char
			(incf (line-number lstream)))))
    the-char))

;;; XXX Gotta deal with #pragma
(defun parse-line-comment (lstream)
  (with-slots (file-name line-number) lstream
    ;; Eat Initial #
    (%read-char lstream)
    ;; Use the lisp reader to parse line number and file name.  Gross, but
    ;; avoids complications in recursively invoking the cparse tokenizer at
    ;; this point, besides the fact that cparse doesn't parse strings yet.
    (let ((comment-line (%read-line lstream))
	  next-pos)
      (multiple-value-setq (line-number next-pos)
	(read-from-string comment-line))
      (setf file-name (read-from-string comment-line t nil :start next-pos)))))

(defun unreadc (c lstream)
  (when (characterp c)
    (if (char= c #\newline)
	(decf (line-number lstream)))
    (%unread-char c lstream)))

;;; A superclass for all our types.  We can hang our own print-object method
;;; off it and stuff.

(defclass cparse-object ()
  ())

;;; Obviously there are ways to do this in other CLOSes and MOPs, but I
;;; don't know what they are.

#+PCL
(defmethod print-object ((obj cparse-object) stream)
  (let ((slots (mapcan #'(lambda (slot-def)
			   (let ((name (pcl:slot-definition-name slot-def)))
			     (if (slot-boundp obj name)
				 (list name (slot-value obj name))
				 nil)))
		       (pcl:class-slots (class-of obj)))))
    (print-unreadable-object  (obj stream :type t)
      (format stream "~<~@{~W ~@_~W~^ ~_~}~:>" slots))))

;;; C types, as opposed to the parse trees that produce them.

(defmacro defc (class-name supers slots &rest class-options)
  (let ((new-slots (mapcar #'(lambda (slot)
			       (multiple-value-bind (name args)
				   (cond ((symbolp slot)
					  (values slot nil))
					 ((listp slot)
					  (values (car slot) (cdr slot)))
					 (t
					  (error "Invalid slot ~S" slot)))
				 `(,name :accessor ,name
				   :initarg ,(intern (string name)
						     "KEYWORD")
				   ,@args)))
			   slots)))
     `(defclass ,class-name ,supers
	,new-slots
	,@class-options)))

;;; Classes for constant numbers

(defc integer-object (cparse-object)
  ((value)
   (base)
   (digits)))

(defc unsigned-object (unsigned integer-object)
  ())

(defc float-object (cparse-object)
  ((value)
   (size)))

(defun get-integer-base (lstream base)
  (loop
   with result = 0
   for c = (readc lstream)
   for digit = (and (characterp c) (digit-char-p c base))
   until (or (eql c *end-of-file*) (null digit))
   count t into digits
   do (progn
	(if (= result 0)
	    (setq result digit)
	    (setq result (+ (* result base) digit))))
   finally
   (unreadc c lstream)
   (return (values result base digits))))

(defun get-integer (lstream &optional base)
  "Returns integer value along with the specified base."
  (let ((c (readc lstream)))
    (if (eq c *end-of-file*)
	*end-of-file*
	(multiple-value-bind (number base digits)
	    (if (eql c #\0)
		(let ((next-c (readc lstream)))
		  (cond ((or (eql next-c #\x) (eql next-c #\X))
			 (if (or (null base) (eql base 16))
			     (get-integer-base lstream 16)
			     (cparse-error "Unexpected characters 0~C"
					   next-c)))
			(base
			 (unreadc next-c lstream)
			 (get-integer-base lstream base))
			((digit-char-p next-c 8)
			 (unreadc next-c lstream)
			 (get-integer-base lstream 8))
			((or (eq next-c *end-of-file*)
			     (not (digit-char-p next-c 8)))
			 (unreadc next-c lstream)
			 (values 0 10 1))
			(t (cparse-error
				   "Malformed number"))))
		(progn
		  (unreadc c lstream)
		  (get-integer-base lstream 10)))
	  (make-instance 'integer-object :value number :base base
			 :digits digits)))))

(defun parse-char (lstream)
  (let ((chars (make-string 5))
	(octnums "01234567")
	(specials "abfnrtv'\"\\?")
	(special-translations '(#\bel #\backspace #\page 
				#\newline #\return #\tab #\vt #\' #\" #\\ #\?))
	val
	)
    (setf (elt chars 0) (readc lstream)) ;chop starting '
    (do ((i 1 (1+ i))
	 (c (readc lstream) (readc lstream))
	 )
	((or (char-equal c #\')
	     (> i 4) ;here are only byte-chars allowed: maximum is \xFF
	     )
	 (progn (setf (elt chars i) c)
		(setf chars (subseq chars 0 (1+ i))))
	 )
      (setf (elt chars i) c))
    (cond ((not (char-equal (elt chars 0) #\'))
	   (cparse-error "Char constant does not start with ' but with ~a" (elt chars 0))
	   )
	  ((not (char-equal (elt chars 1) #\\)) ;; Simple char
	   (make-instance 'char-const :value (char-code (elt chars 1)))
	   )
	  ((char-equal (elt chars 2) #\x)
	   (let ((*read-base* 16)
		 val)
	     (setq val (read-from-string (subseq chars 3)))
	     (when (> val 255)
	       (cparse-error "Char constant out of range ~a" chars))
	     (make-instance 'char-const :value val)
	     ))
	  ((find (elt chars 2) octnums :test #'char-equal)
	   (let ((*read-base* 8)
		 val)
	     (setq val (read-from-string (subseq chars 2)))
	     (when (> val 255)
	       (cparse-error "Char constant out of range ~a" chars))
	     (make-instance 'char-const :value val)
	     )
	   )
	  ((find (elt chars 2) special-chars :test #'char-equal)
	   (make-instance 'char-const :value (char-code 
					      (elt special-translations
						   (position (elt chars 2)
							     special-chars :test #'char-equal))))
	   )
	  (t
	   (cparse-error "Unidentified char constant: ~a"
			 chars)
	   )))
  )
  

;;; return either a positive integer or float
(defun parse-number (lstream)
  (let* ((maybe-point (readc lstream))
	 (looking-at-float (eql maybe-point #\.))
	 mantissa
	 frac
	 exponent)
    (if (not looking-at-float)
	(unreadc maybe-point lstream))
    (let ((first-num (get-integer lstream (and looking-at-float 10))))
      (if (or (and (typep first-num 'integer-object)
		   (not (eql (base first-num) 10))))
	  (return-from parse-number (make-int-const-w/type first-num
							   lstream))
	  (if looking-at-float
	      (setq mantissa 0 frac first-num)
	      (setq mantissa first-num)))
      (setq maybe-point (readc lstream))
      (if (eql maybe-point #\.)
	  (if looking-at-float
	      (cparse-error  "Illegal number syntax")
	      (setq frac (get-integer lstream 10)))
	  (progn
	    (unreadc maybe-point lstream)
	    (unless looking-at-float
	      (return-from parse-number (make-int-const-w/type first-num
							       lstream)))))
      (let ((maybe-exponent (readc lstream)))
	(if (or (eql maybe-exponent #\e) (eql maybe-exponent #\E))
	    (let ((sign-char (readc lstream))
		  (sign 1))
	      (cond ((eql sign-char #\-)
		     (setq sign -1))
		    ((eql sign-char #\+))
		    (t (unreadc sign-char lstream)))
	      (let ((exp1 (get-integer lstream 10)))
		(setq exponent (make-instance 'integer-object
					      :value (* sign (value exp1))
					      :base 10
					      :digits (digits exp1)))))
	    (setq exponent 0)))
      (let* ((maybe-type (readc lstream))
	     (float-type (case maybe-type
			   ((#\f #\F) '|float|)
			   (t (unreadc maybe-type lstream)
			      '|double|))))
	(make-float-const mantissa frac exponent float-type)))))

(defun make-int-const-w/type (int-obj lstream)
  "Use possible trailing type modifier to construct the appropriate
integer constant."
  (let ((char1 (readc lstream))
	(char2 nil)
	(char3 nil))
    (unless (or (equalp char1 #\u) (equalp char1 #\l))
      (unreadc char1 lstream)
      (return-from make-int-const-w/type
	(make-instance 'int-const :value (value int-obj))))
    (setq char2 (readc lstream))
    (if (equalp char2 #\l)
	(when (equalp char1 #\u)
	  (setq char3 (readc lstream))
	  (unless (equalp char3 #\l)
	    (unreadc char3 lstream)))
	(unreadc char2 lstream))
    (make-instance (cond ((equalp char3 #\l)
			  'unsigned-long-long-const)
			 ((equalp char2 #\l)
			  (if (equalp char1 #\u)
			      'unsigned-long-const
			      'long-long-const))
			 ((equalp char1 #\l)
			  'long-const)
			 (t 'unsigned-int-const))
		   :value (value int-obj))))

(defun make-float-const (mant frac exp float-type)
  (let* ((proto-float (if (eq float-type '|float|)
			  1.0
			  1d0))
	 (mantissa (if (typep mant 'integer-object)
		       (float (value mant) proto-float)
		       (float mant proto-float)))
	 (fraction (if (typep frac 'integer-object)
		       (float (/ (value frac) (expt 10 (digits frac)))
			      proto-float)
		       (float 0 proto-float)))
	 (exponent (if (typep exp 'integer-object)
		       (value exp)
		       0)))
    (make-instance (ecase float-type
		     (float
		      'cfloat-const)
		     (double
		      'double-const))
		   :value (* (+ mantissa fraction)
			     (expt (float 10.0 proto-float) exponent)))))

(defun get-special-char (lstream)
  (flet ((make-op (&rest chars)
	   (intern (apply #'concatenate
			  'simple-string
			  (mapcar #'(lambda (c) (string c)) chars))
		   *cparse-package*)))
    ;; Check first for characters that could end the file, to avoid
    ;; reading eof. 
    (let ((char1 (readc lstream)))
      (if (member char1 '(#\; #\}))
	  (make-op char1)
	  (let ((char2 (readc lstream)))
	    (cond ((and (or (eql char1 #\>) (eql char1 #\<))
			(eql char1 char2))
		   (let ((char3 (readc lstream)))
		     (if (eql char3 #\=)
			 (make-op char1 char2 "=")
			 (progn
			   (unreadc char3 lstream)
			   (make-op char1 char2)))))
		  ((or (and (eql char1 #\<)
			    (eql char2 #\=))
		       (and (eql char1 #\>)
			    (eql char2 #\=))
		       (and (eql char1 #\+)
			    (member char2 '(#\+ #\=)))
		       (and (eql char1 #\-)
			    (member char2 '(#\- #\=)))
		       (and (eql char1 #\&)
			    (member char2 '(#\& #\=)))
		       (and (eql char1 #\|)
			    (member char2 '(#\| #\=)))
		       (and (member char1 '(#\* #\/ #\^ #\% #\~ #\= #\!))
			    (eql char2 #\=)))
		   (make-op char1 char2))
		  (t (unreadc char2 lstream)
		     (make-op char1))))))))

;;; Intern C keywords in the CPARSE package; everything else is
;;; interned in the current package.

(defparameter +c-keywords+ (make-hash-table :test #'equal))

(let ((keywords '("float" "double" "typedef" "extern" "void"
		  "char" "int" "long" "const" "volatile" "signed"
		  "unsigned"  "short" "struct" "union" "enum"
		  "__attribute__" "__mode__" ; gcc extension
		  "sizeof")))
  (loop for keyword in keywords
	do (setf (gethash keyword +c-keywords+) (intern keyword))))

;;; Copy the string so that get-token can keep on reusing its
;;; non-simple string.  Potentially cons garbage in the bug case.

(defun intern-token (str)
  (let* ((the-str #+hash-fill-bug (copy-seq str) #-hash-fill-bug str)
	 (keyword (gethash the-str +c-keywords+)))
    (if keyword
	keyword
	(intern #+hash-fill-bug the-str #-hash-fill-bug (copy-seq str)))))

(let ((tok (make-array 32 :element-type 'character
		       :adjustable t :fill-pointer 0)))
  (defun get-token (lstream)
    (setf (fill-pointer tok) 0)
    (loop
     for c = (readc lstream)
     with state = :begin
     do (case state
	  ((:begin)
	   (cond
	     ((whitespace-p c))		;
	     ((digit-char-p c)
	      (unreadc c lstream)
	      (return (parse-number lstream)))
	     ((or (alpha-char-p c) (eql c #\_))
	      (setf state :id)
	      (vector-push-extend c tok))
	     ((eql c #\')
	      (unreadc c lstream)
	      (return (parse-char lstream))
	      )
	     (t
	      (unreadc c lstream)
	      (return-from get-token (get-special-char lstream)))))
	  ((:id)
	   (cond
	     ((or (alphanumericp c) (eql c #\_))
	      (vector-push-extend c tok))
	     (t (loop-finish)))))
     finally (return (progn
		       (unreadc c lstream)
		       (case state
			 ((:number)
			  (cparse-error
				 "How did we get in :number state?"))
			 ((:id)
			  (intern-token tok))
			 (t (cparse-error
			     "Don't know what to return for state ~S.~%"
			     state))))))))
      

;;; Shallow copy of a type
(defgeneric %copy-type (type new))

(defun make-empty-type (type)
  (make-instance (class-of type)))

(defun copy-type (type &optional (new (make-empty-type type)))
  (%copy-type type new))

(defc c-type (cparse-object)
  ((qualifiers :initform nil)))

(defmethod %copy-type ((type c-type) new)
  (setf (qualifiers new) (qualifiers type))
  new)

(macrolet ((frob-prim-type (type)
	     (let ((cparse-type (intern (concatenate 'string "CPARSE-"
						     (symbol-name type)))))
	       `(defc ,cparse-type (,type c-type)
		  ()))))
  (frob-prim-type void)
  (frob-prim-type char)
  (frob-prim-type unsigned-char)
  (frob-prim-type signed-char)
  (frob-prim-type short)
  (frob-prim-type unsigned-short)
  (frob-prim-type int)
  (frob-prim-type unsigned-int)
  (frob-prim-type long)
  (frob-prim-type unsigned-long)
  (frob-prim-type cfloat)
  (frob-prim-type double))

(defc pointer-type (c-type)
  ((to :type c-type)))

(defmethod %copy-type :after ((type pointer-type) new)
  (setf (to new) (to type)))

(defc array-type (c-type)
  ((of :type c-type)
   (len :type (or fixnum null))))

(defmethod %copy-type :after ((type array-type) new)
  (setf (of new) (of type)
	(len new) (len type)))

(defc func-type (c-type)
  ((return-type :type c-type)
   (args :initform nil)))

(defmethod %copy-type :after ((type func-type) new)
  (setf (return-type new) (return-type type)
	(args new) (args type)))


;;; The class for scopes.  A scope exists for "outer" or file level.
;;; Structures and unions (and functions, someday) introduce local scope too.

(defc scope (cparse-object)
  ((objects :initform (make-hash-table))
   (struct-tags :initform (make-hash-table))
   (struct-members :initform (make-hash-table))
   (outer-scope)))

(defmethod %copy-type :after ((type scope) new)
  (setf (objects new) (objects type)
	(struct-tags new) (struct-tags type)
	(struct-members new) (struct-members type)
	(outer-scope new) (outer-scope type)))

(defvar *current-scope* nil)

(defun get-scope (type key &optional (scope *current-scope*))
  (gethash key (slot-value scope type)))

(defun (setf get-scope) (val type key &optional (scope *current-scope*))
  (setf (gethash key (slot-value scope type)) val))

(defun lookup (type key)
  (loop for scope = *current-scope* then (outer-scope scope)
	while scope
	do (multiple-value-bind (val found)
	       (gethash key (slot-value scope type))
	     (when found
	       (return (values val t))))
	finally (return (values nil nil))))

;;; Super class for struct-type and union type, since they're pretty much the
;;; same.
(defc compound-type (c-type scope)
  ((tag :initform nil)
   (members)
   (size :initform nil)
   (alignment :initform nil)))

(defmethod initialize-instance :after ((type compound-type)
				       &key &allow-other-keys)
  (with-slots (tag outer-scope) type
    (when (and tag outer-scope)
      (setf (get-scope 'struct-tags tag outer-scope) type))))

(defmethod %copy-type :after ((type compound-type) new)
  (setf (members new) (members type)
	(size new) (size type)
	(alignment new) (alignment type)))

(defclass struct-type (compound-type)
  ())

(defclass union-type (compound-type)
  ())

;;; Enums.  enumerators is an assoc list of (identifier . value).
(defc enum-type (c-type)
  ((tag :initform nil)
   (enumerators :initform nil)))

(defmethod %copy-type :after ((type enum-type) new)
  (setf (enumerators new) (enumerators type)))

;;; Type under construction i.e., haven't seen enough of it yet.
(defc incomplete-type ()
  ((real-type)))

;;; stdargs indicator in function arguments
(defc stdarg-type ()
  ())

;;; typedef-type isn't a real kind of type; rather, an identifier with
;;; this type names a type.

(defc typedef-type (cparse-object)
  ((defined-type)))

(defun cparse-stream (stream &key
		      file-name
		      ((:compiler *compiler-implementation*)
		       (make-instance 'impl-32bit))
		      scope
		      (stmt-fun #'(lambda (decls scope lstream)
				    (declare (ignore decls scope lstream))
				    (values))))
  "Parse STREAM for C language constructs.  Arguments are:
:file-name - Sets the file name in error messages.
:compiler - Binds *compiler-implementation* to an object of class
compiler-impl that controls the implementation of C arithmetic.
Default is an object of type 'impl-32bit.
:scope - A scope object, possibly the result of an earlier run of
cparse-stream.
:stmt-fun - that is called for every statement with (parse-tree scope lstream)."
  (let* ((lstream (make-instance 'lookahead-stream
				 :stream stream
				 :file-name file-name))
	 (*current-lstream* lstream)
	 (*current-scope* (or scope (make-instance 'scope :outer-scope nil))))
    (handler-case
	(let ((file-name-for-debug nil))
	  (loop
	   (funcall stmt-fun (cparse-stmt lstream) *current-scope* lstream)
	   (when *cparse-debug*
	     (unless (eql file-name-for-debug (file-name lstream))
	       (format *error-output* "~A: " (file-name lstream)))
	     (format *error-output* "~S " (line-number lstream)))
	   (handler-case (consume lstream)
	     (end-of-file ()
	       (return-from cparse-stream *current-scope*)))))
      (end-of-file ()
	(format t "~A ~A: unexpected end of file~%"
		(file-name lstream) (line-number lstream))))))

(defun cparse-stmt (lstream)
  (when (eq (look lstream) '|typedef|)
    (consume lstream)
    (return-from cparse-stmt
      (mapcar #'(lambda (typedef)
		  (let ((name (cadr typedef))
			(typedef-obj
			 (make-instance 'typedef-type
					:defined-type (car typedef))))
		    (setf (get-scope 'objects name) typedef-obj)
		    (list typedef-obj name)))
	      (cparse-stmt lstream))))
  (when (eq (look lstream) '|extern|)
    (consume lstream))
  (let ((decl-type (parse-decl-type lstream)))
    (loop
     collect (multiple-value-bind (declarator id)
		 (parse-declarator lstream decl-type)
	       (setf (get-scope 'objects id) declarator)
	       (list declarator id))
     do (case (look lstream)
	  (|,| (consume lstream))
	  (|:| (warn "Ignoring Bitfield ~a ~a ~a" 
		     (consume lstream) 
		     (consume lstream) lstream))
	  (|;|
	   ;; Don't (consume lstream); let the caller do it so it can
	   ;; check for eof in a sane manner.
	   (loop-finish))
	  (t (cparse-error "Unexpected token at end of statement: ~S~%"
			   (look lstream)))))))


;;; Map an ugly control problem into an ugly data problem :)  The tail of each
;;; element list is the type if the length qualifier is (nothing long short)
(defparameter +size-alist+ '((|void| void)
			     (|char| char)
			     (|int| int long short)
			     (|float| cfloat)
			     (|double| double long-double)
			     (nil nil long short)))

;;; Same deal for signed/unsigned: (nothing signed unsigned)
(defparameter +signed-alist+ '((void cparse-void)
			       (char cparse-char cparse-signed-char
				cparse-unsigned-char)
			       (short cparse-short cparse-short
				cparse-unsigned-short)
			       (int cparse-int cparse-int cparse-unsigned-int)
			       (long cparse-long cparse-long
				cparse-unsigned-long)
			       (cfloat cparse-cfloat)
			       (double cparse-double)
			       (long-double cparse-long-double)
			       (nil nil cparse-int cparse-unsigned-int)))
(defparameter +prim-types+
  '(|void| |char| |int| |float| |double|))

(defparameter prim-qualifiers '(|const| |volatile|))

(defparameter +length-decls+ '(|short| |long|))
(defparameter +sign-decls+ '(|signed| |unsigned|))

(defun make-prim-type (qualifiers keywords)
  (let ((len (intersection keywords +length-decls+ :test #'eq))
	(sign (intersection keywords +sign-decls+ :test #'eq))
	(ptype (intersection keywords +prim-types+ :test #'eq))
	(augmented-type nil))
    (unless (and (<= (length sign) 1) (<= (length ptype) 1))
      (cparse-error "Illegal declarators: ~S" keywords))
    (if (and (null ptype) (equal len '(|long| |long|)))
	(setq augmented-type (if (eq (car sign) '|unsigned|)
				 'cparse-unsigned-long-long
				 'cparse-long-long))
	(let* ((size-map (cdr (assoc (car ptype) +size-alist+ )))
	       (sized-type (case (car len)
			     ((nil)
			      (car size-map))
			     (|long|
			      (cadr size-map))
			     (|short|
			      (caddr size-map))))
	       (sign-map (cdr (assoc sized-type +signed-alist+))))
	  (setq augmented-type (case (car sign)
				 ((nil)
				  (car sign-map))
				 (|signed|
				  (cadr sign-map))
				 (|unsigned|
				  (caddr sign-map))))
	  #+nil(format t "ptype ~S len ~S size-map ~S sized-type ~S sign-map ~S augmented-type ~S"
		  ptype len size-map sized-type sign-map augmented-type)))
    (unless augmented-type
      (cparse-error "Not a legal set of type keywords: ~S" keywords))
    (make-instance augmented-type :qualifiers qualifiers)))

;;; The list of non-qualifier keywords that appear in a declaration
;;; and don't cause any interesting behavior in their own right.

(defparameter +decl-keywords+ (append +prim-types+ +length-decls+
				      +sign-decls+))

(defun parse-decl-type (lstream)
  (let ((maybe-typedef)
	(typedef-type nil)
	(qualifiers nil)
	(keywords nil))
    ;; add qualifiers, check that no other modifiers were specified.
    (flet ((do-qualifiers (type)	
	     (when keywords
	       (cparse-error
		       "Illegal qualifier with struct, union, or enum"))
	     (if qualifiers
		 (let ((new-type (copy-type type)))
		   (setf (qualifiers new-type) qualifiers)
		   new-type)
		 type)))
      (loop
       for token = (look lstream) then (consume lstream)
       do (cond ((member token +decl-keywords+ :test #'eq)
		 (push token keywords))
		;; use value of setq
		((and (setq maybe-typedef (lookup 'objects token))
		      (typep maybe-typedef 'typedef-type))
		 (setq typedef-type (defined-type maybe-typedef)))
		((member token prim-qualifiers :test #'eq)
		 (pushnew token qualifiers))
		((or (eq token '|struct|) (eq token '|union|))
		 (return-from parse-decl-type
		   (do-qualifiers (parse-struct-union lstream))))
		((eq token '|enum|)
		 (return-from parse-decl-type
		   (do-qualifiers (parse-enum lstream))))
		(t (loop-finish))))
      
      (if typedef-type
	(do-qualifiers typedef-type)
	(make-prim-type qualifiers keywords)))))

(defun parse-declarator (lstream decl-type)
  (let (new-type
	id
	standin-type)
    (case (look lstream)
      (*
       (multiple-value-setq (new-type id) (parse-pointer lstream decl-type)))
      (|(|
	 (consume lstream)
	 (setq standin-type (make-instance 'c-type))
	 (multiple-value-setq (new-type id)
	   (parse-declarator lstream standin-type))
	 (if (not (eq '|)| (look lstream)))
	     (cparse-error  "Expected ) but got ~S~%" (look lstream))
	     (consume lstream)))))
  (multiple-value-bind (new-type id)
      (case (look lstream)
	(*
	 (parse-pointer lstream decl-type))
	(|(|
	 (consume lstream)
	 (multiple-value-prog1
	     (parse-declarator lstream decl-type)
	   (if (not (eq '|)| (look lstream)))
	       (cparse-error  "Expected ) but got ~S~%" (look lstream))
	       (consume lstream))))
	(t (parse-id lstream decl-type)))
    (values (case (look lstream)
	      (|[|
	       (parse-array lstream decl-type))
	      (|(|
	       (parse-function lstream decl-type))
	      (t new-type))
	    id)))

(defun parse-declarator (lstream decl-type)
  (let ((standin-type nil))
    (multiple-value-bind (new-type id)
	(case (look lstream)
	  (*
	   (parse-pointer lstream decl-type))
	  (|(|
	   (consume lstream)
	   (setq standin-type (make-instance 'c-type))
	   (multiple-value-prog1
	       (parse-declarator lstream standin-type)
	     (if (not (eq '|)| (look lstream)))
		 (cparse-error  "Expected ) but got ~S~%" (look lstream))
		 (consume lstream))))
	  (t (parse-id lstream decl-type)))
      (let* ((return-type (if standin-type ;return type of function or array of
			      decl-type
			      new-type))
	     (array-or-function (case (look lstream)
				 (|[|
				  (parse-array lstream return-type))
				 (|(|
				  (parse-function lstream return-type))
				 (t return-type))))
	(if standin-type
	   (progn
	     (change-class standin-type
			   (#+PCL pcl::class-of #-pcl class-of
				  array-or-function))
	     (copy-type array-or-function standin-type)
	     (values new-type id))
	   (values array-or-function id))))))
  

(defun parse-pointer (lstream decl-type)
  (let ((qualifiers nil))
    (loop
     for maybe-qualifier = (consume lstream) ;eat initial *
     while (member maybe-qualifier '(|const| |volatile|))
     do (pushnew maybe-qualifier qualifiers))
    (let ((ptype (make-instance 'pointer-type
				:to decl-type
				:qualifiers qualifiers)))
      (parse-declarator lstream ptype))))

(defun identifierp (id)
  (and (symbolp id)
       id
       (loop
	with name = (string id)
	for c across (the simple-string name)
	for first = t then nil
	always (or (and first (or (alpha-char-p c) (char= c #\_)))
		   (and  (not first) (or (alphanumericp c) (char= c #\_)))))))

(defun parse-id (lstream decl-type)
  (let ((id (look lstream)))
    (if (identifierp id)
	(progn
	  (consume lstream)
	  (values decl-type id))
	(values decl-type nil))))

;;; Processes multi-dimensional arrays too
(defun parse-array (lstream decl-type)
  (let* ((dimension-tok (consume lstream)) ; Eat [
	 (dimension (if (eq dimension-tok '|]|)
			nil
			(parse-expression lstream))))
    (if (not (eq (look lstream) '|]|))
	(cparse-error "Malformed array: expected ], got ~S" (look lstream)))
    (make-instance 'array-type
		   :len dimension
		   :of (if (eq (consume lstream) '|[|)
			   (parse-array lstream decl-type)
			   decl-type))))

(defun parse-function (lstream decl-type)
  (consume lstream)			;Eat (
  (let ((args (parse-function-args lstream)))
    (consume lstream)
    (make-instance 'func-type
		   :return-type decl-type
		   :args (if (and (eql (length args) 1)
				  (typep (caar args) 'cparse-void))
			     nil
			     args))))

(defun parse-func-decl-type (lstream)
  (if (eq (look lstream) '|.|)
      (if (and (eq (consume lstream) '|.|)
	       (eq (consume lstream) '|.|))
	  (progn
	    (consume lstream)
	    (make-instance 'stdarg-type))
	  (cparse-error "Malformed function argument. Token: ~S~%"
			(look lstream)))
      (parse-decl-type lstream)))

(defun parse-function-args (lstream)
  (if (eq (look lstream) '|)|)
      (progn
	(return-from parse-function-args nil)))
  (loop
   for decl-type = (parse-func-decl-type lstream)
   collect (multiple-value-list (parse-declarator lstream decl-type))
   do (case (look lstream)
	(|,| (consume lstream))
	(|)| (loop-finish))
	(t (cparse-error "Unexpected token at end of function args: ~S"
			 (look lstream))))))

;;; lookahead token is either |union| or |struct|
(defun parse-struct-union (lstream)
  (let* ((type-class (if (eq (look lstream) '|struct|)
			 'struct-type
			 'union-type))
	 (maybe-compound-tag (consume lstream))
	 (compound-tag (and (identifierp maybe-compound-tag)
			    maybe-compound-tag))
	 (defined-type (or (and compound-tag
				(lookup 'struct-tags compound-tag))
			   (make-instance type-class
					  :tag compound-tag
					  :outer-scope *current-scope*))))
    (when compound-tag
      (consume lstream))
    (when (eq (look lstream) '{)
      (cond ((null defined-type)
	     (setf defined-type (make-instance type-class)))
	    ((slot-boundp defined-type 'members)
	     (cparse-error "Multiple definitions of struct/union ~S"
			   compound-tag)))
      (let ((*current-scope* defined-type))
	(with-slots (members size alignment) defined-type
	  (setf members	(parse-struct-members lstream))
	  (multiple-value-bind (siz align offsets)
	      (compute-compound-size defined-type)
	    (setf size siz
		  alignment align)
	    (setf members (add-elems-to-lists offsets members))))))
    defined-type))

(defun add-elems-to-lists (elems lists)
  (loop for list on lists
	for head = elems then next
	for next = (cdr elems) then (cdr next)
	do (progn
	     (setf (cdr head) nil)
	     (setf (car list) (nconc (car list) head))))
  lists)


(defun round-to-alignment (addr align)
  (* (ceiling addr align) align))

(defgeneric compute-compound-size (type))

(defmethod compute-compound-size ((type struct-type))
  (loop for (decl) in (members type)
	for decl-size = (sizeof decl)
	for decl-align = (alignof decl)
	for decl-offset = 0 then (round-to-alignment struct-size decl-align)
	for struct-size = decl-size then (+ decl-offset decl-size)
	maximize decl-align into max-alignment
	collect decl-offset into member-offsets
	finally (return (values (round-to-alignment struct-size max-alignment)
				max-alignment
				member-offsets))))

(defmethod compute-compound-size ((type union-type))
  (loop for (decl) in (members type)
	for decl-size = (sizeof decl)
	for decl-align = (alignof decl)
	maximize decl-align into max-alignment
	maximize decl-size into union-size
	collect 0 into member-offsets
	finally (return (values (round-to-alignment union-size max-alignment)
				max-alignment
				member-offsets))))

(defun parse-struct-members (lstream)
  (consume lstream)			; Eat {
  (loop
   append (prog1
	      (cparse-stmt lstream)
	    (consume lstream))		; Eat ;
   until (eq (look lstream) '|}|)
   finally (consume lstream)))

(defun parse-enum (lstream)
  (consume lstream)			; Eat enum
  (let ((maybe-enum-tag (look lstream)))
    (when (identifierp maybe-enum-tag)
      (let ((existing-enum (lookup 'struct-tags maybe-enum-tag)))
	(when existing-enum
	  (if (typep existing-enum 'enum-type)
	      (return-from parse-enum existing-enum)
	      (cparse-error "~S is not an enum tag."
			    maybe-enum-tag)))))
    (let ((enum (make-instance 'enum-type)))
      (when (identifierp maybe-enum-tag)
	(setf (tag enum) maybe-enum-tag)
	(setf (get-scope 'struct-tags maybe-enum-tag) enum)
	(consume lstream))
      (unless (eq '{ (look lstream))
	(cparse-error "In enum definition expected { but got ~S~%"
		      (look lstream)))
      (setf (enumerators enum) (parse-enumerators lstream))
      enum)))

(defun parse-enumerators (lstream)
  (consume lstream)			; Eat {
  (let ((enum-val 0))
    (flet ((parse-one-enum ()
	     (let ((id (look lstream)))
	       (when (eq id '})
		 (consume lstream)
		 (return-from parse-one-enum nil))
	       (unless (identifierp id)
		 (cparse-error "expected an identifier, but got ~S~%" id))
	       (let ((seperator (consume lstream)))
		 (when (eq seperator '=)
		   (consume lstream)
		   (let ((enum-val-obj (parse-expression lstream)))
		     (cond ((typep enum-val-obj 'char-const)
			    (setq enum-val-obj (promote
						*compiler-implementation* enum-val-obj)))
			   ((not (typep enum-val-obj 'int-const)) ;XXX
			    (cparse-error "non-integral expression in enum: ~S~%"
					  enum-val)))
		     (setq enum-val (value enum-val-obj)))
		   (setf seperator (look lstream)))
		 (if (or (eq seperator '|,|) (eq seperator '}))
		     (progn
		       (when (eq seperator '|,|)
			 (consume lstream))
		       (prog1 `(,id . ,enum-val)
			 (incf enum-val)))
		     (cparse-error "In enum, expected , or } but got ~S~%"
				   seperator))))))
      (loop for enum-def = (parse-one-enum)
	    while enum-def
	    collect enum-def))))

(defun cparse-string (str &rest args &key &allow-other-keys)
  "Parse string STR.  See cparse-stream for other arguments."
  (with-input-from-string (str-stream str)
    (apply #'cparse-stream str-stream args)))

(defun  cparse-file (file-name &rest args &key &allow-other-keys)
  "Parse the contents of file FILE-NAME.  See cparse-stream for other
arguments." 
  (with-open-file (stream file-name)
    (apply #'cparse-stream stream :file-name file-name args)))

(defgeneric do-statement (implementation statement))

(defclass cmucl-implementation (cparse-object)
  ())

(defmethod do-statement ((implementation cmucl-implementation) statement)
  (print statement))

;;; parser for constant C expressions.  Whee!

;;; An expression in the parse tree.  We'll have to make this up as we
;;; go along...

(defc cexpr (cparse-object)
  ())

(defc cast-expr (cexpr)
  ((explicit-type)
   (expr)))

(defc address-expr (cexpr)
  ((base)
   (index)))

(defc pointer-expr (cexpr)
  ((address)))

(defc struct-ref-expr (cexpr)
  ((base)
   (slot)))

(defc union-ref-expr (cexpr)
  ((base)
   (slot)))

(defun eat-or-error (sym lstream)
  (unless (eq sym (look lstream))
    (cparse-error "Expected token ~A, but got ~A" sym (look lstream)))
  (consume lstream))

(defun parse-expression (lstream)
  (conditional-expression lstream))

;;; Parse operators at a certain precedence.

(defmacro def-expr-expression (expr-fun term-fun op-list)
  (let ((op-alist (mapcar #'(lambda (op)
			      (cons (car op) (cadr op)))
			  op-list)))
    `(let ((op-alist ',op-alist))
       (defun ,expr-fun (lstream)
	 (loop
	  with result = (,term-fun lstream)
	  for op = (look lstream)
	  for op-fun = (cdr (assoc op op-alist))
	  for term = (when op-fun
		       (consume lstream)
		       (,term-fun lstream))
	  while op-fun
	  do (setq result (funcall op-fun *compiler-implementation*
				   result term))
	  finally (return result))))))

(def-expr-expression mult-expression parse-cast-expression
  ((* c*) (/ c/)))
(def-expr-expression add-expression mult-expression ((+ c+) (- c-)))
(def-expr-expression shift-expression add-expression ((>> c>>) (<< c<<)))
(def-expr-expression relational-expression shift-expression
  ((> c>) (< c<) (>= c>=) (<= c<=)))
(def-expr-expression equality-expression relational-expression
  ((== c=) (!= c!=)))
(def-expr-expression and-expression equality-expression ((& c&)))
(def-expr-expression exclusive-or-expression and-expression ((^ c-logxor)))
(def-expr-expression inclusive-or-expression exclusive-or-expression
  ((\| c-logior)))
(def-expr-expression logical-and-expression inclusive-or-expression
  ((&& c-and)))
(def-expr-expression logical-or-expression logical-and-expression
  ((\|\| c-or)))

(defun conditional-expression (lstream)
  (let ((term (logical-or-expression lstream)))
    (if (eql (look lstream) '?)
	(progn
	  (consume lstream)
	  (let ((true-case (parse-expression lstream)))
	    (eat-or-error '|:| lstream)
	    (let ((false-case (conditional-expression lstream)))
	      (if (zerop term)
		  false-case
		  true-case))))
	term)))

(defun parse-cast-expression (lstream)
  (let ((initial (look lstream)))
    (cond ((eq initial '|(|)
	   (consume lstream)
	   (if (maybe-type-name lstream)
	       (let ((decl (parse-type-name lstream)))
		 (eat-or-error '|)| lstream)
		 (make-instance 'cast-expr :explicit-type decl
				:expr (parse-cast-expression lstream)))
	       (progn
		 (push-back '|(| lstream)
		 (parse-unary-expression lstream))))
	  (t (parse-unary-expression lstream)))))

(defun parse-unary-expression (lstream)
  (let ((initial (look lstream)))
    (cond ((eq initial '|sizeof|)
	   (parse-sizeof lstream))
	  ((eq initial '+)
	   (parse-cast-expression lstream))
	  ((eq initial '-)
	   (cneg *compiler-implementation* (parse-cast-expression lstream)))
	  ((eq initial '~)
	   (c~ *compiler-implementation* (parse-cast-expression lstream)))
	  ((eq initial '!)
	   (c! *compiler-implementation* (parse-cast-expression lstream)))
	  ((or (eq initial '*) (eq initial '&))
	   (cparse-error "Can't parse ~A yet." initial))
	  ((or (eq initial '++) (eq initial '--))
	   (cparse-error
	    "increment or decrement operator not allowed in a constant
expression."))
	  (t (parse-postfix-expression lstream)))))

(defun parse-postfix-expression (lstream)
  (let ((current-expr (parse-primary-expression lstream)))
    (loop for postfix-tok = (look lstream)
	  while (member postfix-tok '(|[| |(| |.| -> ++ --) :test #'eq)
	  do (case postfix-tok
	       (|[|
		(consume lstream)
		(let ((array-expr (parse-expression lstream)))
		  (eat-or-error '|]| lstream)
		  (setq current-expr
			(make-instance
			 'pointer-expr
			 :address (make-instance 'address-expr
						 :base current-expr
						 :index array-expr)))))
	       (t
		(cparse-error "Can't parse ~A yet" postfix-tok)))
	  finally (return current-expr))))

(defun parse-primary-expression (lstream)
  (let ((initial (look lstream)))
    (cond ((eql initial '|(|)
	   (consume lstream)
	   (multiple-value-prog1 (parse-expression lstream)
	     (when (not (eql '|)| (look lstream)))
	       (cparse-error "Didn't see closing parens on expression"))
	     (consume lstream)))	; eat ')
	  ;; XXX This should do the right thing with unsigned and
	  ;; integer constants...
	  ((eql initial '|'|)
	   (consume lstream)
	   (break)
	   )
	  ((typep initial 'c-const)
	   (consume lstream)
	   initial)
	  (t (cparse-error "Can't parse ~S" initial)))))

;;; See if the token could be part of a type name

(defun maybe-type-name (lstream)
  (let ((tok (look lstream)))
    (or (member tok +decl-keywords+)
	(member tok '(|struct| |union| |enum|))
	(let ((maybe-typedef-type (lookup 'objects tok)))
	  (typep maybe-typedef-type 'typedef-type)))))

(defun parse-type-name (lstream)
  (let ((decl-type (parse-decl-type lstream)))
    (multiple-value-bind (declarator id)
	(parse-declarator lstream decl-type)
      (when id
	(cparse-error "type name has id ~A" id))
      declarator)))

(defun parse-sizeof (lstream)
  (let* ((initial (consume lstream))
	 (sized-type (cond ((eq initial '|(|)
			    (consume lstream)
			    (if (maybe-type-name lstream)
				(prog1
				    (parse-type-name lstream)
				  (eat-or-error '|)| lstream))
				(progn
				  (push-back initial lstream)
				  (parse-unary-expression lstream))))
			   (t (parse-unary-expression lstream)))))
    (make-instance 'int-const (sizeof sized-type))))

(defgeneric sizeof (type))

(defgeneric alignof (type))

;;; The default - get the typewidth from ctype.lisp
(defmethod sizeof ((type c-type))
  (/ (type-width *compiler-implementation* type) 8))

(defmethod alignof ((type c-type))
  (/ (type-alignment *compiler-implementation* type) 8))

(defmethod sizeof ((type compound-type))
  (size type))

(defmethod alignof ((type compound-type))
  (alignment type))

(defmethod sizeof ((type array-type))
  (* (sizeof (of type)) (value (len type))))

(defmethod alignof ((type array-type))
  (alignof (of type)))

(defmethod sizeof ((type pointer-type))
  (/ (pointer-width *compiler-implementation*) 8))

(defmethod alignof ((type pointer-type))
  (/ (pointer-alignment *compiler-implementation*) 8))

(let ((dummy-int (make-instance 'int)))
  (defmethod sizeof ((type enum-type))
    (/ (type-width *compiler-implementation* dummy-int) 8))
  (defmethod alignof ((type enum-type))
    (/ (type-alignment *compiler-implementation* dummy-int) 8)))

;;; For testing
(defun cparse-const (stream)
  (let ((lstream (make-instance 'lookahead-stream :stream stream)))
    (values (parse-expression lstream) (look lstream))))

(defun test-const (str)
  (with-input-from-string (stream str)
    (cparse-const stream)))

(defun uplow-to-dash (name)
  (do* ((i 0 (1+ i))
	(last-char nil char)
	(char (aref name i)(ignore-errors (aref name i)))
	(res))
      ((> i (- (length name) 1)) res)
    (setf res 
      (cond ((and (> i 0)
		  (char-equal char #\_)) (format nil "~a-" res))
	    ((not last-char) (format nil "~a" (char-downcase char)))
	    ((and (char>= last-char #\a)
		  (char<= last-char #\z)
		  (char<= char #\Z)
		  (char>= char #\A))
	     (format nil "~a-~a" res (char-downcase char)))
	    (t (format nil "~a~a" res (char-downcase char))))))
  )
