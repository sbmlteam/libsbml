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

(defpackage "CL-UNIT"
  (:use "COMMON-LISP")
  (:export "UNIT-TEST-SUITE"
	   "ADD-TEST" "DEFTEST" "RUN-TESTS" "ERROR-STREAM"
	   "LOG-STREAM" "UNIT-TEST" "TESTS" "RUN-TESTS" "CLEAN-UP"
	   "RUN-ONE-TEST" "RUN-TEST"))

(in-package "CL-UNIT")

(defclass unit-test-suite ()
  ((error-stream :accessor error-stream :initarg :error-stream
		 :initform *standard-output*)
   (log-stream :accessor log-stream :initarg :log-stream
	       :initform *standard-output*)
   (tests :accessor tests :initform nil)
   (debug-on-error :accessor debug-on-error :initarg :debug-on-error
		   :initform nil)))

;;; Clean up test state

(defgeneric clean-up (test))

(defmethod clean-up ((test unit-test-suite))
  nil)

(defgeneric add-test (test-obj name func))

(defmethod add-test ((test-obj unit-test-suite) name func)
  (let ((existing (assoc name (tests test-obj) :test #'equal)))
    (if existing
	(setf (cdr existing) func)
	(setf (tests test-obj) (nconc (tests test-obj) `((,name . ,func)))))))


(defmacro deftest ((name suite) (test-suite) &body body)
  (let* ((lambda-args-body `((,test-suite)
			     (let ((error-stream (error-stream ,test-suite))
				   (log-stream (log-stream ,test-suite)))
			       (declare (ignorable error-stream log-stream))
			       ,@body)))
	 (defun-form (when (symbolp name)
		       `((defun ,name ,@lambda-args-body))))
	 (fun-object (if (symbolp name)
			 `#',name
			 `#'(lambda ,@lambda-args-body))))
    `(eval-when (load eval)
       ,@defun-form
       (add-test ,suite ',name ,fun-object))))

(defgeneric run-tests (test-suite &key debug-on-error verbose))

(defmethod run-tests ((suite unit-test-suite) &key debug-on-error verbose)
  (let ((err-stream (error-stream suite)))
    (setf (debug-on-error suite) debug-on-error)
    (let ((failures 0)
	  (total 0))
      (loop for (name . func) in (tests suite)
	    do (progn
		 (incf total)
		 (multiple-value-bind (test-val status)
		     (run-one-test suite name func)
		   (format t "~A ~:[failed~;succeeded~]: ~S~%"
			   name status test-val)
		   (unless status
		     (incf failures)))))
      (format err-stream "~D/~D tests failed.~%" failures total))))

(defgeneric run-one-test (suite name func))

(defmethod run-one-test ((suite unit-test-suite) name func)
  (if (debug-on-error suite)
      (restart-case (values (funcall func suite) t)
	(continue-tests ()
	  :report "Continue with tests"
	  (values nil nil)))
      (handler-case (values (funcall func suite) t)
	(error (condition)
	  (format (error-stream suite) "~&Test ~A: failed:~A~%"
		  name condition)
	  (values nil nil)))))

(defgeneric run-test (name suite &key))

(defmethod run-test (name (suite unit-test-suite)
			  &key initargs)
  (let ((err-stream (error-stream suite))
	(func (cdr (assoc name (tests suite) :test #'equal))))
    (unless func
      (format err-stream "No test named ~A.  Eit.~%" name)
      (return-from run-test nil))
    (setf (debug-on-error suite) t)
    (multiple-value-bind (test-val status)
	(run-one-test suite name func)
      (declare (ignore test-val))
      (unless status
	(format err-stream "Test ~A failed." name)))))




