/**
 * \file    ConstraintSet.cpp
 * \brief   A list of LocalConstraints<T>, all of the same type T
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2005 California Institute of Technology and
 * Japan Science and Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 2.1 of the License, or
 * any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY, WITHOUT EVEN THE IMPLIED WARRANTY OF
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  The software and
 * documentation provided hereunder is on an "as is" basis, and the
 * California Institute of Technology and Japan Science and Technology
 * Corporation have no obligations to provide maintenance, support,
 * updates, enhancements or modifications.  In no event shall the
 * California Institute of Technology or the Japan Science and Technology
 * Corporation be liable to any party for direct, indirect, special,
 * incidental or consequential damages, including lost profits, arising
 * out of the use of this software and its documentation, even if the
 * California Institute of Technology and/or Japan Science and Technology
 * Corporation have been advised of the possibility of such damage.  See
 * the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 *
 * The original code contained here was initially developed by:
 *
 *     Ben Bornstein
 *     The Systems Biology Markup Language Development Group
 *     ERATO Kitano Symbiotic Systems Project
 *     Control and Dynamical Systems, MC 107-81
 *     California Institute of Technology
 *     Pasadena, CA, 91125, USA
 *
 *     http://www.cds.caltech.edu/erato
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s):
 */


#include <algorithm>
#include <functional>

#include "sbml/SBMLTypes.h"

#include "Validator.h"
#include "ConstraintSet.h"


using namespace std;


/**
 * Function Object: Applies a LocalConstraint<T> to an SBML object of type
 * T.
 */
template <typename T>
struct Apply : public unary_function<LocalConstraint<T>*, void>
{
  Apply (const Model& m, const T& obj, Validator& v) :
    model(m), object(obj), validator(v) { }


  void operator() (LocalConstraint<T>* constraint)
  {
    if (!constraint->holds(model, object))
      validator.logMessage
      (
        ParseMessage(  constraint->getId     ()
                     , constraint->getMessage()
                     , object.getLine        ()
                     , object.getColumn      () )
      );
  }


  const Model& model;
  const T&     object;
  Validator&   validator;
};


/**
 * Applies all Constraints in this ConstraintSet to the given SBML object
 * of type T.
 */
template <typename T>
void
ConstraintSet<T>::applyTo (const Model& m, const T& x, Validator& v)
{
  for_each(mConstraints.begin(), mConstraints.end(), Apply<T>(m, x, v));
}


/**
 * Function Object: Logs a ParseMessage to the given Validator.  It's used
 * by Apply<GlobalConstraints>() to log multiple messages on constraint
 * failure.
 */
struct LogMessageTo : public unary_function<const ParseMessage&, void>
{
  LogMessageTo (Validator& v) : validator(v) { }


  void operator() (const ParseMessage& msg)
  {
    validator.logMessage(msg);
  }


  Validator& validator;
};


/**
 * Function Object: Applies a GlobalConstraint to an SBML Model.
 */
template <>
struct Apply<GlobalConstraint>: public unary_function<GlobalConstraint*, void>
{
  Apply (const Model& m, Validator& v) : model(m), validator(v) { }


  void operator() (GlobalConstraint* constraint)
  {
    if (!constraint->holds(model))
    {
      const list<ParseMessage>& messages = constraint->getMessages();
      for_each(messages.begin(), messages.end(), LogMessageTo(validator));

      constraint->reset();
    }
  }


  const Model& model;
  Validator&   validator;
};


/**
 * Applies all GlobalConstraints in this ConstraintSet to the given SBML
 * Model.
 */
void
ConstraintSet<GlobalConstraint>::applyTo (const Model& m, Validator& v)
{
  Apply<GlobalConstraint> apply(m, v);
  for_each( mConstraints.begin(), mConstraints.end(), apply);
}




/*
 * Generate the following ConstraintSets immediately so linking succeeds.
 */
template class ConstraintSet< SBMLDocument             >;
template class ConstraintSet< Model                    >;
template class ConstraintSet< FunctionDefinition       >;
template class ConstraintSet< UnitDefinition           >;
template class ConstraintSet< Unit                     >;
template class ConstraintSet< Compartment              >;
template class ConstraintSet< Species                  >;
template class ConstraintSet< Parameter                >;
template class ConstraintSet< Rule                     >;
template class ConstraintSet< AlgebraicRule            >;
template class ConstraintSet< AssignmentRule           >;
template class ConstraintSet< SpeciesConcentrationRule >;
template class ConstraintSet< CompartmentVolumeRule    >;
template class ConstraintSet< ParameterRule            >;
template class ConstraintSet< RateRule                 >;
template class ConstraintSet< Reaction                 >;
template class ConstraintSet< KineticLaw               >;
template class ConstraintSet< SimpleSpeciesReference   >;
template class ConstraintSet< SpeciesReference         >;
template class ConstraintSet< ModifierSpeciesReference >;
template class ConstraintSet< Event                    >;
template class ConstraintSet< EventAssignment          >;
