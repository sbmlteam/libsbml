#|
\file    sbml-reader.lisp
\brief   Example reader for SBML using libsbmlc
\author  Martin Ginkel <mginkel@mpi-mageburg.mpg.de>

$Id$
$Source$

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


(in-package :libsbml)


(defclass sbml-reader ()
  ((document :accessor document :initform nil
	     :documentation "The document-pointer to libsbmlc")
   (model :accessor model :initform nil
	  :documentation "The model pointer to libsbmlc")
   (version :accessor version :initform 2.1
	    :documentation "Level and version of the sbml-document as (+ level (* version 0.1))")
   (functions :accessor functions :initform (make-hash-table :test #'equal)
	      :documentation "Table mapping from function names to functions")
   (units :accessor units :initform (make-hash-table :test #'equal)
	  :documentation "Table mapping from unit-definition names to units")
   (contexts :accessor contexts :initform nil
	     :documentation "stack of contexts for name identification")
   (vars-in-context :accessor vars-in-context :initform (make-hash-table)
		    :documentation 
		    "Table to map from SIDs to objects depending on
context. This identifies parameter, species and compartments in kineticlaws
and models which are contexts."
		    )
   (filename :accessor filename :initform nil :initarg :filename
:documentation "Input file name to use.")
   (errors :accessor errors :initform nil
:documentation "String of concatentated errors from XML-Parsing")
   )
  (:documentation "Reader for importing SBML into Promot"))

(defmethod initialize-instance ((self sbml-reader)&key filename &allow-other-keys)
  (call-next-method)
  (when filename
    (read-file self filename))
  #+:allegro (excl:schedule-finalization self #'cleanup)
  )

(defmethod read-parse-message (p &key prefix filename stream)
  (format stream "~a:~d:~d ~a: ~a.~%" 
	  (or filename "")
	  (parse-message-get-line p)
	  (parse-message-get-column p)
	  (or prefix "")
	  (parse-message-get-message p))
  )

(defmethod read-file ((self sbml-reader) file-name)
  "reads in the file named by `file-name'.
 therefore a document is created, the parser is run, and eventual 
warnings and errors are put into (errors self)."
  (cleanup self)
  
  (with-slots (document model version filename) self
    (setf filename (pathname file-name)
	  document (read-sbml (namestring (truename filename))))
    (let ((fatals (read-element-list document #'sbmldocument-get-num-fatals #'sbmldocument-get-fatal))
	  (errors (read-element-list document #'sbmldocument-get-num-errors #'sbmldocument-get-error))
	  (warnings (read-element-list document #'sbmldocument-get-num-warnings #'sbmldocument-get-warning)))
      (when (or errors fatals warnings)
	(setf (errors self)
	  (with-output-to-string (s)
	    (dolist (e fatals)
	      (read-parse-message e :prefix "Fatal" :stream s :filename filename))
	    (dolist (e errors)
	      (read-parse-message e :prefix "Fatal" :stream s :filename filename))
	    (dolist (e warnings)
	      (read-parse-message e :prefix "Fatal" :stream s :filename filename))
	    ))
;#+:promot-development(break (errors self))
	))
    (setf 
	model (sbmldocument-get-model document)
	version (+ (sbmldocument-get-level document) 
		   (/
		    (sbmldocument-get-version document)
		    10.0))
	))
  )

(defmethod cleanup ((self sbml-reader))
  "Clean internal table structures, and frees document, if already set"
  (with-slots (document model version functions vars-in-context contexts) self
    (when (and (slot-boundp self 'document)
	       document)
      (unwind-protect
	  (progn
	    (sbmldocument-free document)
	    (clrhash functions)
	    (clrhash vars-in-context)
	    )
	(slot-makunbound self 'document)
	(setf model nil
	      version 1.1
	      contexts nil))
      )
    ))

(defmethod model-name ((self sbml-reader))
  "Returns either the defined name of the model, or if unset or set to empty
  string it derives a meaningful name from the filename"
  (with-slots (model filename) self
    (let ((name (or (model-get-id model)
		    (model-get-name model))))
      (when (or (null name)
		(string-equal name ""))
	(unless filename
	  (setf filename (pathname (symbol-name (gensym "sbml-model")))))
	(setq name (pathname-name (pathname filename))))
      name))
  )

(defmethod get-context ((self sbml-reader))
  "gets the current context (TOS) for naming in the sbml (model or reaction pointer)"
  (car (contexts self)))
(defmethod push-context ((self sbml-reader) c)
  "pushes a new context to the context stack"
  (push c (contexts self)))
(defmethod push-stop-context ((self sbml-reader))
  "pushes a stop-context to the context-stack, that is:
get-var will only search above this context.
Used in variable resolution for functions, not to find
variables in the whole model."
  (push-context self nil))
(defmethod pop-context ((self sbml-reader))
  "Removes to top context from the stack."
  (pop (contexts self)))

(defmethod add-var ((self sbml-reader) varid &optional (value t))
  "Adds a variable (named) varid and value to the current context."
  (with-slots (vars-in-context) self
    (let* ((context (get-context self))
	   (contexttab (gethash context vars-in-context)))
      (unless contexttab
	(setf contexttab
	  (setf (gethash context vars-in-context) (make-hash-table :test #'equal))))
      (if (gethash varid contexttab)
	  (warn "Duplicate definition of ~a in context ~a ignored" varid context)
	(setf (gethash varid contexttab) value))
      value
      )
    )
  )

(defmethod get-var ((self sbml-reader) varid &optional context)
  "Searches for variable varid in all contexts down the context stack.
If context is given, the search is limited to that context"
  (with-slots (vars-in-context contexts) self
    (cond (context 
	   (let* ((contexttab (gethash context vars-in-context)))
	     (when contexttab
	       (gethash varid contexttab))))
	  (t (dolist (c contexts)
	       ;; nil in contextlist is a stop-context
	       (unless c (return-from get-var nil))
	       (let* ((contexttab (gethash c vars-in-context))
		      (v (when contexttab
			   (gethash varid contexttab))))
		 (when v
		   (return-from get-var v))))
	     nil)))
  )

(defmethod add-function ((self sbml-reader) name 
			 &optional (value 
				    (intern 
				     (gensym (string-upcase name)))))
  (with-slots (functions) self
    (cond ((gethash name functions)
	   (warn "Duplicate definition of function ~a, function ignored." name)
	   (gethash name functions))
	  (t (setf (gethash name functions)
	       value)))))

(defmethod get-function ((self sbml-reader) name)
  "Find a function object. "
  (with-slots (functions) self
    (let ((func (gethash name functions)))
      (cond (func func)
	    (t (warn "unknown function ~a." name)
	       nil))))
  )

(defmethod add-unitdef ((self sbml-reader) name 
			 &optional (value 
				    (intern 
				     (gensym (string-upcase name)))))
  (with-slots (unitdefs) self
    (cond ((gethash name unitdefs)
	   (warn "Duplicate definition of unit ~a, unit ignored." name)
	   (gethash name unitdefs))
	  (t (setf (gethash name unitdefs)
	       value)))))

(defmethod get-unitdef ((self sbml-reader) name)
  "Find a function object. "
  (with-slots (unitdefs) self
    (let ((func (gethash name unitdefs)))
      (cond (func func)
	    (t (warn "unknown unit ~a." name)
	       nil))))
  )


(defmethod read-math ((self sbml-reader) m)
  "Read one AST node, and dispatch reading of children"
  (let ((children 
	 (mapcar #'(lambda (c)
		     (read-math self c)
		     )
		 (read-element-list m 
				    #'astnode-get-num-children
				    #'astnode-get-child))))
    (define-math self 
	m
      (astnode-get-type m)
      children
      )
    ))

(defconstant +lisp-operator-map+
    `((,+ast-plus+ . +)
      (,+ast-minus+ . -)
      (,+ast-times+ . *)
      (,+ast-divide+ . /)
      (,+ast-power+ . expt)
      
      (,+ast-function-abs+ . abs)
      (,+ast-function-arccos+ . acos)
      (,+ast-function-arccosh+ . acosh)
      ;; +ast-function-arccot+
      ;; +ast-function-arccoth+
      ;; +ast-function-arccsc+
      ;; +ast-function-arccsch+
      (,+ast-function-arcsin+ . asin)
      (,+ast-function-arcsinh+ . asinh)
      (,+ast-function-arctan+ . atan)
      (,+ast-function-arctanh+ . atanh)
      (,+ast-function-ceiling+ . ceiling)
      (,+ast-function-cos+ . cos)
      (,+ast-function-cosh+ . cosh)
      ;; +ast-function-cot+
      ;; +ast-function-coth+
      ;; +ast-function-csc+
      ;; +ast-function-csch+
      (,+ast-function-exp+ . exp)
      ;; +ast-function-factorial+
      (,+ast-function-floor+ . floor)
      (,+ast-function-ln+ . log)
      ;; +ast-function-log+
      ;; +ast-function-piecewise+
      (,+ast-function-power+ . expt)
      ;; +ast-function-root+ . 
      ;; +ast-function-sec+
      ;; +ast-function-sech+
      (,+ast-function-sin+ .   sin)
      (,+ast-function-sinh+ . sinh)
      (,+ast-function-tan+ .   tan)
      (,+ast-function-tanh+ . tanh)

      (,+ast-logical-and+ . and)
      (,+ast-logical-not+ . not)
      (,+ast-logical-or+ .   or)
      ;; +ast-logical-xor+ 
      
      (,+ast-relational-eq+ .   =)
      (,+ast-relational-geq+ . >=)
      (,+ast-relational-gt+ .   >)
      (,+ast-relational-leq+ . <=)
      (,+ast-relational-lt+ .   <)
      (,+ast-relational-neq+ . /=))
  "List of conses for the simplest case of mapping AST-operators to lisp
functions, what can't be found here, will be done in define-math")


(defmethod define-math ((self sbml-reader) ast type children)
  "define a Lisp expression from ast with type and children
(already converted). This dispatches the translation via type."
  (let ((op (cdr (find type +lisp-operator-map+ :key #'car))))
    (cond ((and op children) `(,op ,@children))
	  (t
	   (const-case 
	    type
	    (+ast-integer+ (astnode-get-integer ast))
	    (+ast-real+  (astnode-get-real ast))
	    (+ast-real-e+ (* (astnode-get-mantissa ast)
			     (expt 10 (astnode-get-exponent ast))))
	    (+ast-rational+ (/ (astnode-get-numerator ast)
			       (astnode-get-denominator ast)))
	    (+ast-constant-e+      (exp 1))
	    (+ast-constant-false+  nil)
	    (+ast-constant-pi+     pi)
	    (+ast-constant-true+   t)
	    (+ast-name+           (astnode-get-name ast))
	    (+ast-function-piecewise+ 
	     `(cond ,@(loop for piece 
			  on children by #'cddr
			  collect (if (> (length piece) 1) 
				      (subseq piece 0 2) 
				    (cons t piece))))
	     )
	    (+ast-function-root+ 
	     (if (= 2 (first children))
		 `(sqrt ,(second children))
	       `(expt ,(second children) (/ 1 ,(first children))))
	     )
	    (+ast-logical-xor+ 
	     ;; retranslation to (or (and a b)
	     ;;                      (and (not a) (not b)))
	     `(or (and ,@(mapcar #'(lambda (c) c) children))
		  (and ,@(mapcar #'(lambda (c) `(not ,c)) 
				 children))))
	    (+ast-function-log+ 
	     ;; base is second arg in lisp
	     `(log ,(second children) ,(first children)))
	    (+ast-function+       
	     `(,(get-function self 
			      (astnode-get-name ast))
	       ,@children)
	     )
	    (+ast-lambda+         
             `(lambda ,(butlast children) ,(first (last children))))
	    (t (error "unknown ast-type ~a" type))
	    )
	   ))
    )
  )
#|
unhandled types
+AST-NAME-DELAY+ 
+AST-NAME-TIME+ 

+AST-FUNCTION-ARCCOT+ 
+AST-FUNCTION-ARCCOTH+ 
+AST-FUNCTION-ARCCSC+ 
+AST-FUNCTION-ARCCSCH+ 
+AST-FUNCTION-ARCSEC+ 
+AST-FUNCTION-ARCSECH+ 
+AST-FUNCTION-COT+ 
+AST-FUNCTION-COTH+ 
+AST-FUNCTION-CSC+ 
+AST-FUNCTION-CSCH+ 
+AST-FUNCTION-FACTORIAL+ 

+AST-FUNCTION-SEC+ 
+AST-FUNCTION-SECH+ 

|#




(defmethod start-element-definition ((self sbml-reader))
  "Hook method to do some jobs before actually start processing the
element description"
  )

(defmethod read-model ((self sbml-reader) &optional filename)
  "Process the elements of the model in order of appearance.
Transforming data from libsbmlc is done in read-xxx, which in turn
calls generation in define-xxx (where xxx is function, unit, compartment ...)
This method returns them as a list of lists for 
fuctions, units, compartments, ... . The elements of these lists are
the return values of define-xxx."
  (when filename
    (read-file self filename))
  (let (functions units compartments species parameters rules reactions events)
    (with-slots (model) self
      (push-context self model)
      (start-element-definition self)
      ;; Don't use the automatic conversion
      ;; It will not return and the lisp crashes if the model has errors!
;        (when (< (version self) 2.0) ;; normalize l1 -> l2
;  	;; Adding modifiers!
;  	(sbml-convert-to-l2 (model self))
;  	)
      
      #+:test(debug-write "~%~d Funktions~%" (model-get-num-function-definitions model))
      ;; fundef
      (setf functions 
	(mapcar #'(lambda (e)
		    (read-function-definition self e))
		(read-element-list model 
				   #'model-get-num-function-definitions 
				   #'model-get-function-definition)))

      #+:test(debug-write "~%~d Units~%" (model-get-num-unit-definitions model))
      ;; unit
      (setf units
	(mapcar #'(lambda (e)
		    (read-unit-definition self e))
		(read-element-list model #'model-get-num-unit-definitions #'model-get-unit-definition)
		))
      #+:test(debug-write "~%~d Compartments~%" (model-get-num-compartments model))
      ;; comp
      (setf compartments
	(mapcar #'(lambda (e)
		    (read-compartment self e))
		(read-element-list model #'model-get-num-compartments #'model-get-compartment)
		))
      #+:test(debug-write "~%~d Species~%" (model-get-num-species model))
      ;; species
      (setf species
	(mapcar #'(lambda (e)
		    (read-species self e))
		(read-element-list model #'model-get-num-species #'model-get-species)
		))
      #+:test(debug-write "~%~d global Parameters~%" (model-get-num-parameters model))
      ;; params
      (setf parameters
	(mapcar #'(lambda (e)
		    (read-parameter self e))
		(read-element-list model #'model-get-num-parameters #'model-get-parameter)
		))
      #+:test(debug-write "~%~d Rules~%" (model-get-num-rules model))
      ;; rules
      (setf rules
	(mapcar #'(lambda (e)
		    (read-rule self e))
		(read-element-list model #'model-get-num-rules #'model-get-rule)
		))
      #+:test(debug-write "~%~d Reactions~%" (model-get-num-reactions model))
      ;; reactions
      (setf reactions
	(mapcar #'(lambda (e)
		    (read-reaction self e))
		(read-element-list model #'model-get-num-reactions #'model-get-reaction))
	)
      #+:test(debug-write "~%~d Events~%" (model-get-num-events model))
      ;;events
      (setf events
	(mapcar #'(lambda (e)
		    (read-event self e))
		(read-element-list model #'model-get-num-events 
				   #'model-get-event))
	)
      (when (errors self)
	(format t (errors self))
	)
      (define-model self (model-get-id model) (model-get-name model) functions units compartments species parameters rules reactions events)
      )
    )
  )

(defmethod define-model ((self sbml-reader)
			 id
			 name
			 functions units compartments species parameters rules reactions events)
  (new model :id id :name name 
       :functions functions :units units :compartments compartments
       :species species :parameters parameters :rules rules
       :reactions reactions :events events))


(defmethod read-function-definition ((self sbml-reader) fundef)
  "Read one function definition"
  #+:test(debug-write "f")
  (let* ((id (function-definition-get-id fundef))
	(args `(,self 
		,id 
		,(read-math self (function-definition-get-math fundef))
		,@(when (function-definition-is-set-name fundef)
		    `(:name ,(function-definition-get-name fundef)))
		))
	)
    (add-function 
     self id 
     (apply 
      #'define-function
	 args
	 )
    ))
  )

(defmethod define-function ((self sbml-reader) id  math 
			    &rest args 
			    &key (name nil name?))
  "Define one function. This should be implemented by subclasses"
  (apply #'make-instance (append `(function-definition :id ,id :math ,math) 
				 args)))

(defconstant +unit-kind-map+
    `((,+unit-kind-ampere+ . :ampere) 
      (,+unit-kind-becquerel+ . :becquerel) 
      (,+unit-kind-candela+ . :candela) 
      (,+unit-kind-celsius+ . :celsius) 
      (,+unit-kind-coulomb+ . :coulomb) 
      (,+unit-kind-dimensionless+ . :dimensionless) 
      (,+unit-kind-farad+ . :farad) 
      (,+unit-kind-gram+ . :gram) 
      (,+unit-kind-gray+ . :gray) 
      (,+unit-kind-henry+ . :henry) 
      (,+unit-kind-hertz+ . :hertz) 
      (,+unit-kind-item+ . :item) 
      (,+unit-kind-joule+ . :joule) 
      (,+unit-kind-katal+ . :katal) 
      (,+unit-kind-kelvin+ . :kelvin) 
      (,+unit-kind-kilogram+ . :kilogram) 
      (,+unit-kind-liter+ . :liter) 
      (,+unit-kind-litre+ . :litre) 
      (,+unit-kind-lumen+ . :lumen) 
      (,+unit-kind-lux+ . :lux) 
      (,+unit-kind-meter+ . :meter) 
      (,+unit-kind-metre+ . :metre) 
      (,+unit-kind-mole+ . :mole) 
      (,+unit-kind-newton+ . :newton) 
      (,+unit-kind-ohm+ . :ohm) 
      (,+unit-kind-pascal+ . :pascal) 
      (,+unit-kind-radian+ . :radian) 
      (,+unit-kind-second+ . :second) 
      (,+unit-kind-siemens+ . :siemens) 
      (,+unit-kind-sievert+ . :sievert) 
      (,+unit-kind-steradian+ . :steradian) 
      (,+unit-kind-tesla+ . :tesla) 
      (,+unit-kind-volt+ . :volt) 
      (,+unit-kind-watt+ . :watt) 
      (,+unit-kind-weber+ . :weber) 
      (,+unit-kind-invalid+ . :invalid))
  "Mapping list with conses translating the enum-constants from libsbml to keywords"
  )

(defmethod read-unit ((self sbml-reader) u)
  "read one unit"
  (let ((kind (cdr (find (unit-get-kind u) +unit-kind-map+ 
			 :key #'car :test #'=)
		   ))
	(scale (unit-get-scale u))
	(exponent (unit-get-exponent u))
	(multiplier (unit-get-multiplier u))
	(offset (unit-get-offset u)))
    (define-unit self kind exponent scale multiplier offset)))

(defmethod define-unit ((self sbml-reader) kind exponent scale multiplier 
			offset)
  "Define one unit"
  (new unit :kind kind :exponent exponent :scale scale :multiplier multiplier 
       :offset offset))

(defmethod read-unit-definition ((self sbml-reader) ud)
  "Read one unit definition."
  #+:test(debug-write "u")
  (let ((id (when (unit-definition-is-set-id ud)
	      (unit-definition-get-id ud)))
	(name (when (unit-definition-is-set-name ud)
		(unit-definition-get-name ud)))
	(units (mapcar #'(lambda (u)
			   (read-unit self u)
			   )
		       (read-element-list ud
					  #'unit-definition-get-num-units 
					  #'unit-definition-get-unit)))
	)
    (when (and name 
	       (not id)
	       (< (version self) 2.0)) ;; normalize l1 -> l2
      (setf id name))
    (add-unitdef self id
		 (define-unit-definition self id name units))
    ))

(defmethod define-unit-definition ((self sbml-reader) id name units)
  "Define one unit definition object"
  (new unit-definition :id id :name name :units units))


(defmethod read-compartment ((self sbml-reader) comp)
  "Read the values for one compartment"
  #+:test(debug-write "c")
  (let ((id (compartment-get-id comp))
	(name (compartment-get-name comp))
	)
    (when (and name 
	       (not id) 
	       (< (version self) 2.0)) ;; normalize l1 -> l2
      (setf id name))
    (add-var self id
	     (define-compartment 
		 self 
		 id name 
		 (compartment-get-spatial-dimensions comp)
		 (if (compartment-is-set-size comp)
		     (compartment-get-size comp)
		   1.0d0)
		 (when (compartment-is-set-units comp)
		     (compartment-get-units comp)
		   )
		 (when (compartment-is-set-outside comp)
		   (compartment-get-outside comp))
		 (compartment-get-constant comp)))
    )
  )

(defmethod define-compartment ((self sbml-reader)id name dims size units outside constant)
  "Defines a compartment object, should be implemented by subclasses."
  (new compartment :id id :name name :spatial-dimensions dims 
       :size size :units units :outside outside :constant constant)
  )

(defmethod read-species ((self sbml-reader) s)
  "Read values of one species"
  #+:test(debug-write "s")
  (let ((id (species-get-id s))
	(name (species-get-name s))
	)
    (when (and name 
	       (not id) 
	       (< (version self) 2.0)) ;; normalize l1 -> l2
      (sbml-convert-name-to-id s);; for finding modifiers later
      (setf id name))
    (add-var self id 
	     (define-species self
		 id
	       name
	       (species-get-compartment s)
	       (when (species-is-set-initial-amount s)
		 (species-get-initial-amount s))
	       (when (species-is-set-initial-concentration s)
		 (species-get-initial-concentration s))
	       (species-get-substance-units s)
	       (species-get-spatial-size-units s)
	       (species-get-has-only-substance-units s)
; 	       (and (not (species-is-set-spatial-size-units s))
; 		    (species-is-set-substance-units s))
	       (species-get-boundary-condition s)
	       (when (species-is-set-charge s)
		 (species-get-charge s))
	       (species-get-constant s)
	       ))
    )
  )

(defmethod define-species ((self sbml-reader) 
			   id 
			   name 
			   compartment 
			   initial-amount
			   initial-concentration 
			   substance-units
			   spatial-size-units 
			   has-only-substance-units 
			   boundary-condition 
			   charge 
			   constant)
  "Define a species object"
;   (format t "~a ~a ~a ~a ~a ~a ~a ~a ~a ~a ~a~%" id name 
; 	  compartment 
; 	  initial-amount
; 	  initial-concentration 
; 	  substance-units
; 	  spatial-size-units 
; 	  has-only-substance-units 
; 	  boundary-condition 
; 	  charge 
; 	  constant)
;   (list :species id name compartment initial-amount initial-concentration
; 	substance-units spatial-size-units has-only-substance-units boundary-condition charge constant)
  (new species :id id :name name :compartment compartment :initial-amount
  initial-amount :initial-concentration initial-concentration :substance-units substance-units :spatial-size-units spatial-size-units :has-only-substance-units has-only-substance-units :boundary-condition boundary-condition :charge charge :constant constant)
  )


(defmethod read-parameter ((self sbml-reader) p)
  "Read values for one parameter definition from lisbmlc.
call define-parameter in the current context."
  #+:test(debug-write "p")
  (let ((id (parameter-get-id p))
	(name (parameter-get-name p))
	)
    (when (and name 
	       (not id) 
	       (< (version self) 2.0)) ;; normalize l1 -> l2
      (setf id name))
    (add-var 
     self id
     (define-parameter self 
	 id 
       name 
       (when (parameter-is-set-value p)
	 (parameter-get-value p))
       (when (parameter-is-set-units p)
	 (parameter-get-units p))
       (parameter-get-constant p)
       ))
    )
  )

(defmethod define-parameter ((self sbml-reader) id name value units constant
			     )
  "Define one parameter object in the current context.
Should be implemented by subclass."
  ;;(format t "~a ~a ~a ~a ~a ~%" id name value units constant)
  ;;(list :parameter id name value units constant)
  (new parameter :id id :name name :value value :units units :constant constant)
  )

(defconstant +rule-type-map+ `((,+sbml-assignment-rule+ . :assignment)
			       (,+sbml-species-concentration-rule+ . :species-concentration)
			       (,+sbml-compartment-volume-rule+ . :compartment-volume)
			       (,+sbml-parameter-rule+ . :parameter)
			       (,+sbml-algebraic-rule+ . :algebraic)
			       (,+sbml-rate-rule+ . :rate)))

(defmethod read-rule ((self sbml-reader) r)
  "Read values for one rule from libsbmlc. The code here normalizes from the 
various types defined in level 1 to the 3 types of level 2.
Then it calls define-rule."
  #+:test(debug-write "R")
  (let ((type (cdr (find (sbase-get-type-code r) +rule-type-map+ :key #'car)))
	(var (const-case (sbase-get-type-code r)
			 (+sbml-parameter-rule+ (parameter-rule-get-name r))
			 (+sbml-compartment-volume-rule+
			  (compartment-volume-rule-get-compartment r))
			 (+sbml-species-concentration-rule+
			  (species-concentration-rule-get-species r))
			 (+sbml-assignment-rule+ 
			  (assignment-rule-get-variable r))
			 (+sbml-rate-rule+
			  (rate-rule-get-variable r))))
	(math)
	)
    (when (< (version self) 2.0)
      (when (find type '(:compartment-volume :parameter :species-concentration))
	(setf type (if (= (assignment-rule-get-type r) +rule-type-rate+)
		       :rate
		     :assignment
		     )))
      (rule-set-math-from-formula r))
    (if (not (rule-is-set-math r))
	(warn "Ignoring broken formula math `~a'" (rule-get-formula r))
      (setf math (read-math self (rule-get-math r)))
      )
    (define-rule self 
	math
      var
      type)
    )
  )

(defmethod define-rule ((self sbml-reader) math variable type)
  "Create one rule object. This should be implemented in a subclass."
  ;;(format t "~a ~a ~a ~%" math variable type)
  ;;(list :rule math variable type)
  (case type
    (:assignment (new assignment-rule :math math :variable variable))
    (:rate       (new rate-rule :math math :variable variable))
    (t           (new algebraic-rule :math math)))
  )

(defmethod read-species-reference ((self sbml-reader) s)
  "Read values for one species reference"
  (let* ((species-id (species-reference-get-species s))
	 st st-math denominator
	 sr)
    (when (/= (sbase-get-type-code s) +sbml-modifier-species-reference+)
      (setf st (species-reference-get-stoichiometry s)
	    st-math (when (species-reference-is-set-stoichiometry-math s)
		      (species-reference-get-stoichiometry-math s))
	    denominator (species-reference-get-denominator s)))
    (setf sr (define-species-reference self
		 species-id
	       st
	       st-math
	       denominator))
    
    sr)
  )

(defmethod define-species-reference ((self sbml-reader) 
				     species
				     stoichiometry
				     stoichiometry-math
				     denominator
				     )
  ;;(list :species-reference species stoichiometry stoichiometry-math denominator)
  (if (or stoichiometry stoichiometry-math)
      (new species-reference 
	   :species species :stoichiometry stoichiometry :stoichiometry stoichiometry-math :denominator denominator)
    (new modifier-species-reference 
	 :species species))
  )

(defmethod read-kinetic-law ((self sbml-reader) k)
  (when (< (version self) 2.0)
    ;; Done in read-reaction
    ;;(kinetic-law-set-math-from-formula k)
    (unless ;(= 0 (kinetic-law-get-math k))
	(kinetic-law-is-set-math k)
      (warn "Ignoring broken formula math in kinetic law `~a'" 
	    (kinetic-law-get-formula k))
      (return-from read-kinetic-law nil)
      )
    )
  (unwind-protect
      (let ((dummy (push-context self k))
	    ;; reading of parameters must be called before the math,
	    ;; to ensure the resolution of parameter-names
	    (pars (mapcar #'(lambda (p) 
			      (read-parameter self p))
			  (read-element-list k #'kinetic-law-get-num-parameters #'kinetic-law-get-parameter))))
	(prepare-define-kinetic-law self pars)
	(define-kinetic-law self
	    (read-math self (kinetic-law-get-math k))
	  pars
	  (when (kinetic-law-is-set-time-units k)
	    (kinetic-law-get-time-units k))
	  (when (kinetic-law-is-set-substance-units k)
	    (kinetic-law-get-substance-units k))
	  )
	)
    (pop-context self))
  )

(defmethod define-kinetic-law ((self sbml-reader)
			       math
			       parameters
			       time-units
			       substance-units)
  ;;(list :kinetic-law math parameters time-units substance-units)
  (new kinetic-law :math math :parameters parameters :time-units time-units :substance-units substance-units)
  )

(defmethod trace-math-for-names ((self sbml-reader) m)
  (let ((children 
	 (flatten
	  (mapcar #'(lambda (c)
		      (trace-math-for-names self c))
		  (read-element-list m 
				     #'astnode-get-num-children
				     #'astnode-get-child)))))
    (cond ((= (astnode-get-type m) +ast-name+)
           (cons (astnode-get-name m) children))
	  (t children))))

(defmethod read-reaction ((self sbml-reader) r)
  #+:test(debug-write "r")
  (push-context self r)
  (unwind-protect
      (let* ((id (reaction-get-id r))
	     (name (reaction-get-name r))
	     (known-species)
	     (reactants (mapcar #'(lambda (s) 
				    (push (species-reference-get-species s) known-species)
				    (read-species-reference self s))
				(read-element-list r #'reaction-get-num-reactants #'reaction-get-reactant)))
	     (products (mapcar #'(lambda (s) 
				   (push (species-reference-get-species s) known-species)
				   (read-species-reference self s))
			       (read-element-list r #'reaction-get-num-products #'reaction-get-product)))
	     modifiers
	     (kl (when (reaction-is-set-kinetic-law r)
		   (reaction-get-kinetic-law r)))
	     )
 	(when (< (version self) 2.0) ;; normalize l1 -> l2
 	  (setf id name)
	  (when kl
	    (kinetic-law-set-math-from-formula kl)
	    (sbml-add-modifiers-to-reaction r (model self)))
 	  )
	(when kl
	  (setf modifiers (mapcar #'(lambda (s) (read-species-reference self s))
				  (read-element-list r #'reaction-get-num-modifiers #'reaction-get-modifier))))
; 	(when (and (< (version self) 2.0)
; 		   (not modifiers)
; 		   kl)
; 	  ;; Try to find modifiers
; 	  (dolist (n (trace-math-for-names self (kinetic-law-get-math kl)))
; 	    (let ((v (model-get-species-by-id (model self) n)))
; 	      (when (and (/= 0 v)
; 			 (not (find n known-species :test #'string-equal)))
; 		(push n known-species)
; 		(push (define-species-reference self n nil nil nil) modifiers)
; 		)
; 	      )
; 	    )
; 	  )
; 	(when (string-equal "PFK" id)
; 	  (break))
	(prepare-define-reaction self reactants products modifiers)
	(define-reaction self 
	    id 
	  name 
	  reactants
	  products
	  modifiers
	  (when kl
	    (read-kinetic-law self kl))
	  (reaction-get-reversible r)
	  (reaction-is-set-fast r)
	  (reaction-get-fast r)
	  )
	)
    (pop-context self))
  )

(defmethod prepare-define-reaction ((self sbml-reader) r p m)
  
  )

(defmethod prepare-define-kinetic-law ((self sbml-reader) pars)
  
  )

(defmethod define-reaction ((self sbml-reader) 
			    id 
			    name 
			    reactants
			    products
			    modifiers
			    kinetic-law
			    
			    reversible
			    has-fast
			    fast)
;   (format t "~a ~a ~a ~a ~a ~a ~a ~a~%" 			    
; 	  id name 
; 	  reactants
; 	  products
; 	  modifiers
; 	  kinetic-law
; 	  reversible
; 	  fast)
;  (list :reaction id name reactants products modifiers kinetic-law reversible fast )
  (new reaction :id id :name name :reactants reactants :products products :modifiers modifiers :kinetic-law kinetic-law :reversible reversible :fast fast )
  )

(defmethod read-event ((self sbml-reader) e)
  #+:test(debug-write "e")
  (let ((id (event-get-id e))
	(name (event-get-name e))
	)
    (when (and name 
	       (not id) 
	       (< (version self) 2.0)) ;; normalize l1 -> l2
      (setf id name))
    (define-event self
	id name
	(when (event-is-set-trigger e)
	  (read-math self (event-get-trigger e)))
	(when (event-is-set-delay e)
	  (read-math self (event-get-delay e)))
	(event-get-time-units e)
	(mapcar #'(lambda (ea)
		    (read-event-assingment self ea)
		    )
		(read-element-list e #'event-get-num-event-assignments
				   #'event-get-event-assignment))
	)
    )
  )

(defmethod define-event ((self sbml-reader)
			 id name
			 trigger
			 delay
			 time-units
			 event-assignments)
  
  ;;(format t "~a ~a ~a ~a ~a ~a ~%" id name trigger delay time-units event-assignments)
  ;;(list :event id name trigger delay time-units event-assignments)
  (new event  :id id :name name :trigger trigger :delay delay :time-units time-units :event-assignments event-assignments)
  )
(defmethod read-event-assingment ((self sbml-reader) e)
  (define-event-assignment 
      self 
      (event-assignment-get-variable e)
    (read-math self (event-assignment-get-math e)))
  )

(defmethod define-event-assignment ((self sbml-reader)
				    variable
				    math)
  ;;(format t "~a ~a~%" variable math)
  ;;`(,variable ,math)
  (new event-assignment :variable variable :math math)
  )


#|
(read-model (new sbml-reader 
		 :filename "promot:src;sbml;models;l1v1-minimal.xml"))

;; Will load with warnings about unknown parameters t and k
(read-model (new sbml-reader 
		 :filename "promot:src;sbml;models;l1v1-rules.xml"))

;; Still don't care about units
(read-model (new sbml-reader 
		 :filename "promot:src;sbml;models;l1v1-units.xml"))

;; Assignment rules 
(read-model (new sbml-reader :filename "promot:src;sbml;models;l2v1-assignment.xml"))

(read-model (new sbml-reader :filename "promot:src;sbml;models;mapk.xml"))

(read-model (new sbml-reader :filename "promot:src;sbml;models;YeastGlycolysis.xml"))

(read-model (new sbml-reader :filename "promot:src;sbml;models;mapk2.xml"))
(read-model (new sbml-reader :filename "promot:src;sbml;models;trypan.xml"))
(read-model (new sbml-reader :filename "promot:src;sbml;models;trypan2.xml"))
(read-model (new sbml-reader :filename "promot:src;sbml;models;Metabolism2001Hel.xml"))
(read-model (new sbml-reader :filename "promot:src;sbml;models;Metabolism2000Teusink.xml"))
;; 8.7 seconds
(read-model (new sbml-reader :stem-kinetic-pars t :filename "/home/ginkel/Projects/Promot/promot/src/sbml/models/100Yeast.xml"
		 ;;"promot:src;sbml;models;100Yeast.xml"
		 ))
;; Many errors with missing species and compartments
(read-model (new sbml-reader :stem-kinetic-pars t :filename "promot:src;sbml;models;yeast1-insilico.xml"))
|#
