;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: UFFI -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          function.lisp
;;;; Purpose:       UFFI source to C function definitions
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

(defun process-function-args (args)
  (if (null args)
      #+(or lispworks cmu sbcl scl cormanlisp (and mcl (not openmcl)) clisp) nil
      #+allegro '(:void)
      #+openmcl (values nil nil)

      ;; args not null
      #+(or lispworks allegro cmu sbcl scl (and mcl (not openmcl)) cormanlisp clisp)
      (let (processed)
	(dolist (arg args)
	  (push (process-one-function-arg arg) processed))
	(nreverse processed))
      #+openmcl
      (let ((processed nil)
	    (params nil))
	(dolist (arg args)
	  (let ((name (car arg))
		(type (convert-from-uffi-type (cadr arg) :routine)))
	    ;;(when (and (listp type) (eq (car type) :address))
	    ;;(setf type :address))
	    (push name params)
	    (push type processed)
	    (push name processed)))
	(values (nreverse params) (nreverse processed)))
    ))

(defun process-one-function-arg (arg)
  (let ((name (car arg))
	(type (convert-from-uffi-type (cadr arg) :routine)))
    #+(or cmu sbcl scl)
    (list name type :in)
    #+(or allegro lispworks (and mcl (not openmcl)) clisp)
    (if (and (listp type) (listp (car type)))
	(append (list name) type)
      (list name type))
    #+openmcl
    (declare (ignore name type))
    ))    


(defun allegro-convert-return-type (type)
  (if (and (listp type) (not (listp (car type))))
      (list type)
    type))

;; name is either a string representing foreign name, or a list
;; of foreign-name as a string and lisp name as a symbol
(defmacro def-function (names args &key module returning)
  #+(or cmu sbcl scl allegro mcl cormanlisp clisp) (declare (ignore module))
  
  (let* ((result-type (convert-from-uffi-type returning :return))
	 (function-args (process-function-args args))
	 (foreign-name (if (atom names) names (car names)))
	 (lisp-name (if (atom names) (make-lisp-name names) (cadr names))))

    ;; todo: calling-convention :stdcall for cormanlisp
    #+:clisp 
    `(ffi:def-call-out ,lisp-name
	 (:name ,foreign-name)
       (:arguments ,@function-args)
       (:return-type ,result-type)
       (:language :stdc)
       )
    #+allegro
    `(ff:def-foreign-call (,lisp-name ,foreign-name)
	 ,function-args
       :returning ,(allegro-convert-return-type result-type)
       :call-direct t
       :strings-convert nil)
    #+(or cmu scl)
    `(alien:def-alien-routine (,foreign-name ,lisp-name)
	 ,result-type
       ,@function-args)
    #+sbcl
    `(sb-alien:define-alien-routine (,foreign-name ,lisp-name)
	 ,result-type
       ,@function-args)
    #+lispworks
    `(fli:define-foreign-function (,lisp-name ,foreign-name :source)
	 ,function-args
       ,@(if module (list :module module) (values))
       :result-type ,result-type
      :language :ansi-c
       :calling-convention :cdecl)
    #+(and mcl (not openmcl))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (ccl:define-entry-point (,lisp-name ,foreign-name)
         ,function-args
         ,result-type))
    #+openmcl
    (declare (ignore function-args))
    #+(and openmcl darwinppc-target)
    (setf foreign-name (concatenate 'string "_" foreign-name))
    #+openmcl
    (multiple-value-bind (params args) (process-function-args args)
      `(defun ,lisp-name ,params
         (ccl::external-call ,foreign-name ,@args ,result-type)))
    #+cormanlisp
    `(ct:defun-dll ,lisp-name (,function-args)
       :return-type ,result-type
       ,@(if module (list :library-name module) (values))
       :entry-name ,foreign-name
       :linkage-type ,calling-convention) ; we need :pascal
    ))




