#|
\file    utilities.lisp
\brief   utilities for the Lisp-binding of libsbml
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


(unless (fboundp 'flatten)
  (defun flatten (o)
    "recursivly removes all parentheses from list `l'"
    (cond ((not (listp o)) (list o))
	  ((null o) o)
	  (t (append (flatten (car o)) (flatten (cdr o))))
	  )))

(defmacro const-case (testform &body clauses)
  "this is like a case, but it evaluates all clause-heads at compile-time.
The macro is mainly used to switch with the type-constants of libsbml."
  `(case ,testform
     ,@(mapcar #'(lambda (clause)
		   (cons (eval (car clause)) (cdr clause))
		   )
	       clauses)
     )
  )

(defun read-element-list (element
			  numfunc 
			  getter)
  "Reading of a list of sub-elements contained in element.
Element is given as a pointer of libsbmlc. 
First (numfunc element) is used to retrieve the number `n' of elements available,
then (getter element i) is used for i=0..n-1 to retrieve the single subelements.
Their pointers are returned as a list"
  (let* ((num (funcall numfunc element))
	 (res (make-list num)))
    ;; todo optimize
    #|
    (do ((i 0 (1+ i))
         (cons res (cdr cons))
         )
        ((>= i num) res)
      (setf (car cons) (funcall getter element i)))
    |#
    (dotimes (i num)
      (setf (elt res i) (funcall getter element i))
      )
    res)
  )

(cl:defmacro defc (name supers slots)
  `(cl:defclass ,name ,supers 
     ,(cl:mapcar 
       #'(cl:lambda (slot)
	   (cl:let ((name (cl::car slot)))
	     (cl:append slot (cl:list :accessor name 
				      :initarg (cl:intern (symbol-name name)
							  (cl:find-package :keyword)))))
	   )
       slots)))

(defmacro new (class &rest args)
  (let ((class (typecase class
		 (class class)
		 (symbol (list 'quote class))
		 (t      class))
	       ))
    `(make-instance ,class ,@args)))
