#|
\file    sbml.lisp
\brief   SBML-Classes in Lisp, output of sbml-reader
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


(defc sbase ()
  ())

(defmethod print-object ((self sbase) s)
  (print-unreadable-object (self s :identity t :type t)
    (let ((*print-case* :downcase)
	  (max-length 10)
	  (dots (intern "..."))
	  )
      (declare (special *print-case*))
    (format s "~{~@<~{:~a ~s~}~^ ~@:>~}"
	    (remove nil
		    (mapcar #'(lambda (s)
				(let ((name (
					     #-cmu clos:slot-definition-name 
					     #+cmu pcl:slot-definition-name 
					     s)))
				  (when (slot-boundp self name)
				    (let ((val (slot-value self name))
					  )
				      (when (and (listp val)
						 (> (length val) max-length))
					(setf val (append (subseq val 0
								  max-length)
							  (list dots))))
				    `(,name ,val)))
				  )
				)
			    (
			     #-:cmu clos:class-slots 
			     #+:cmu PCL:class-slots 
			     (class-of self))))
	    ))
    )
  t
  )

(defc identified
    (sbase)
  ((id)
   (name)))


(defc model
    (identified)
  (
   (functions)
   (units)
   (compartments)
   (species)
   (parameters)
   (rules)
   (reactions)
   (events))
  )

(defc function-definition
    (identified)
  (
   (math)))


(defc unit-definition 
    (identified)
  ((units)))

(defc unit
    (sbase)
  ((kind)
   (scale)
   (exponent)
   (multiplier)
   (offset)))

(defc compartment (identified)
  ((spatial-dimensions)
   (size)
   (units)
   (outside)
   (constant)))

(defc species (identified)
  ((compartment)
   (initial-amount)
   (initial-concentration)
   (substance-units)
   (spatial-size-units)
   (has-only-substance-units)
   (boundary-condition)
   (charge)
   (constant)))

(defc parameter (identified)
  ((value)
   (units)
   (constant)))

(defc rule (sbase)
  ((math)))
(defc algebraic-rule (sbase)
  ())

(defc assignment-rule (rule)
  ((variable)))

(defc rate-rule (rule)
  ((variable)))

(defc reaction (identified)
  ((reactants)
   (products)
   (modifiers)
   (kinetic-law)
   (reversible)
   (fast)))

(defc kinetic-law (sbase)
  ((math)
   (parameters)
   (time-units)
   (substance-units)))

(defc simple-species-reference (sbase)
  ((species)))

(defc species-reference (simple-species-reference)
  ((stoichiometry)
   (denominator)
   (stoichiometry-math)))

(defc modifier-species-reference (simple-species-reference)
  ())

(defc event (identified)
  ((time-units)
   (trigger)
   (delay)
   (assignments)))

(defc event-assignment (sbase)
  ((variable)
   (math)))
