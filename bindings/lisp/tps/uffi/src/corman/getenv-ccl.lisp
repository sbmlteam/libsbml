;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          getenv-ccl.cl
;;;; Purpose:       cormanlisp version
;;;; Programmer:    "Joe Marshall" <prunesquallor@attbi.com>
;;;; Date Started:  Feb 2002
;;;;
`;;;; $Id$
;;;;
;;;; This file, part of UFFI, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; UFFI users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package :cl-user)

(ct:defun-dll c-getenv ((lpname LPSTR)
			(lpbuffer LPSTR)
			(nsize LPDWORD))
  :library-name "kernel32.dll"
  :return-type DWORD
  :entry-name "GetEnvironmentVariableA"
  :linkage-type :pascal)

(defun getenv (name)
  (let ((nsizebuf (ct:malloc (sizeof :long)))
        (buffer (ct:malloc 1))
        (cname (ct:lisp-string-to-c-string name)))
    (setf (ct:cref lpdword nsizebuf 0) 0)
    (let* ((needed-size (c-getenv cname buffer nsizebuf))
           (buffer1 (ct:malloc (1+ needed-size))))
      (setf (ct:cref lpdword nsizebuf 0) needed-size)
      (prog1 (if (zerop (c-getenv cname buffer1 nsizebuf)) 
                 nil
               (ct:c-string-to-lisp-string buffer1))
        (ct:free buffer1)
        (ct:free nsizebuf)))))

(defun cl:user-homedir-pathname (&optional host)
  (cond ((or (stringp host)
             (and (consp host)
                  (every #'stringp host))) nil)
        ((or (eq host :unspecific)
             (null host))
         (let ((homedrive (getenv "HOMEDRIVE"))
               (homepath  (getenv "HOMEPATH")))
           (parse-namestring
             (if (and (stringp homedrive)
                      (stringp homepath)
                      (= (length homedrive) 2)
                      (> (length homepath) 0))
                 (concatenate 'string homedrive homepath "\\")
                 "C:\\"))))
        (t (error "HOST must be a string, list of strings, NIL or :unspecific"))))

;|
(uffi:def-function ("getenv" c-getenv) 
    ((name :cstring))
  :returning :cstring)

(defun my-getenv (key)
  "Returns an environment variable, or NIL if it does not exist"
  (check-type key string)
  (uffi:with-cstring (key-native key)
    (uffi:convert-from-cstring (c-getenv key-native))))
    
#examples-uffi
(progn
  (flet ((print-results (str)
	   (format t "~&(getenv ~S) => ~S" str (my-getenv str))))
    (print-results "USER")
    (print-results "_FOO_")))


#test-uffi
(progn
  (util.test:test (my-getenv "_FOO_") nil :fail-info "Error retrieving non-existent getenv")
  (util.test:test (and (stringp (my-getenv "USER"))
		       (< 0 (length (my-getenv "USER"))))
		  t :fail-info "Error retrieving getenv")
)

