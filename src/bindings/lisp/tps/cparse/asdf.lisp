
(in-package :cparse)

;; classes to use cparse for compilation of c-headers in 
(defvar *preprocessor* "gcc -E ")
(defclass c-header-file (asdf:c-source-file)
  ((preprocessor :accessor preprocessor :initarg :preprocessor :initform *preprocessor*)
   (preproc-options :accessor preproc-options  :initarg :preproc-options
		    :initform "")

   )
 
  )

(defmethod shared-initialize ((c c-header-file) slot-names &rest initargs)
  (declare (ignorable slot-names))
  (call-next-method)
  (with-slots (preprocessor preproc-options) c
    (when (and preprocessor 
	       (not (stringp preprocessor)))
      (setf preprocessor (eval preprocessor)))
    (when (and preproc-options 
	       (not (stringp preproc-options)))
      (setf preproc-options (eval preproc-options)))
    )
  )
  

(defmethod asdf:source-file-type ((c c-header-file) (s asdf:module)) "h")

(defmethod asdf:perform ((o asdf:compile-op)(c c-header-file))
   
  (let* (
	(finished nil)
	(header (asdf::component-pathname c))
	(preheader (first (asdf:output-files o c)))
	(cmd (format nil  "~a ~a ~a> ~a" 
			  (preprocessor c) 
			  (preproc-options c)
			  (cl:namestring header)
			  (cl:namestring preheader)))
	ret
	 )
    (unwind-protect
	(progn
	  (when (or (not (probe-file preheader))
		    (> (file-write-date header)
		       (file-write-date preheader)))
	    (when (> (setf ret (asdf:run-shell-command cmd))
		     0)
	      (format cl::*error-output* "; Preprocessor command ~a ~&returned error ~d" cmd ret)
	      (error 'asdf:compile-error :component c :operation o)))
;(setf *cparse-debug* t)
	  (setf finished t))
      (unless finished 
	(delete-file (first (asdf::output-files o c)))))
    )
  )

(defmethod asdf:perform ((o asdf:load-op)(c c-header-file))
  nil)

(defmethod asdf:output-files ((o asdf:compile-op)(c c-header-file))
  (list (make-pathname :name (format nil "~a-pre" 
				     (pathname-name 
				      (asdf:component-pathname c)))
		       :defaults (asdf:component-pathname c))
	)
  )

(defclass c-preproc-header-file (asdf:c-source-file)
  (
   (cparser :accessor cparser :initarg :cparser :initform 'cparse::uffi-cparser)
   (options :accessor options :initarg :parser-options)
   (lisp-file :accessor lisp-file :initarg :lisp-file :initform nil)
   )
 
  )


  

(defmethod asdf:source-file-type ((c c-preproc-header-file) (s asdf:module)) 
  "h")

(defmethod asdf:perform ((o asdf:compile-op)(c c-preproc-header-file))
   
  (let* (
	(finished nil)
	(header (asdf::component-pathname c))
	ret
	(p ))
    (unwind-protect
	(progn
	  
;(setf *cparse-debug* t)
	  (setf p (apply #'make-instance 
		  `(,(cparser c)
		    :infile ,header
		    :outfile ,(first (asdf::output-files o c))
		    ;; defined in boolean-functions
		    ,@(options c)
		    )))
	  (cparse::parse p)
	  (setf finished t))
      (unless finished 
	(delete-file (first (asdf::output-files o c)))))
    )
  )

(defmethod asdf:perform ((o asdf:load-op)(c c-preproc-header-file))
  nil)

(defmethod asdf:output-files ((o asdf:compile-op)(c c-preproc-header-file))
  (list 
   (make-pathname :name (or (lisp-file c)
			    (format nil "~a-h"
				    (pathname-name 
				     (asdf:component-pathname c)))
			    )
		  :type "lisp" 
		  :defaults (asdf:component-pathname c)))
  )

