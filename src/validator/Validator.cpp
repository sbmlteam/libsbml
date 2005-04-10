/**
 * \file    Validator.cpp
 * \brief   Base class for all SBML Validators
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


#include "sbml/SBMLTypes.h"

#include "LocalConstraint.h"
#include "ValidatingVisitor.h"
#include "Validator.h"


Validator::Validator ()
{
}


Validator::~Validator ()
{
}


/**
 * Adds the given Contraint to this validator.
 */
void
Validator::addConstraint (Constraint* c)
{
#define ADD_CONSTRAINT_IF(Type)                  \
  if (dynamic_cast< LocalConstraint<Type>* >(c)) \
  {                                              \
    m ## Type ## Constraints.add( static_cast< LocalConstraint<Type>* >(c) ); \
    return;                                      \
  }


  if (dynamic_cast<GlobalConstraint*>(c))
  {
    mGlobalConstraints.add( static_cast<GlobalConstraint*>(c) );
    return;
  }

  ADD_CONSTRAINT_IF( SBMLDocument             );
  ADD_CONSTRAINT_IF( Model                    );
  ADD_CONSTRAINT_IF( FunctionDefinition       );
  ADD_CONSTRAINT_IF( UnitDefinition           );
  ADD_CONSTRAINT_IF( Unit                     );
  ADD_CONSTRAINT_IF( Compartment              );
  ADD_CONSTRAINT_IF( Species                  );
  ADD_CONSTRAINT_IF( Parameter                );
  ADD_CONSTRAINT_IF( Rule                     );
  ADD_CONSTRAINT_IF( AlgebraicRule            );
  ADD_CONSTRAINT_IF( AssignmentRule           );
  ADD_CONSTRAINT_IF( SpeciesConcentrationRule );
  ADD_CONSTRAINT_IF( CompartmentVolumeRule    ); 
  ADD_CONSTRAINT_IF( ParameterRule            );
  ADD_CONSTRAINT_IF( RateRule                 );
  ADD_CONSTRAINT_IF( Reaction                 );
  ADD_CONSTRAINT_IF( KineticLaw               );
  ADD_CONSTRAINT_IF( SimpleSpeciesReference   );
  ADD_CONSTRAINT_IF( SpeciesReference         );
  ADD_CONSTRAINT_IF( ModifierSpeciesReference );
  ADD_CONSTRAINT_IF( Event                    );
  ADD_CONSTRAINT_IF( EventAssignment          );

#undef ADD_CONSTRAINT_IF
}


/**
 * Clears the Validator's list of messages.
 *
 * If you are validating multiple SBML documents with the same Validator,
 * call this method after you have processed the list of messages from the
 * last Validation run and before validating the next document.
 */
void
Validator::clearMessages ()
{
  mMessages.clear();
}


/**
 * @return a list of messages logged during validation.
 */
const std::list<ParseMessage>&
Validator::getMessages () const
{
  return mMessages;
}


/**
 * Adds the given message to this list of Validators messages.
 */
void
Validator::logMessage (const ParseMessage& msg)
{
  mMessages.push_back(msg);
}


/**
 * Validates the given SBMLDocument.  Error messages logged during
 * validation may be retrieved via <code>getMessages()</code>.
 *
 * @return the number of validation errors that occurred.
 */
unsigned int
Validator::validate (const SBMLDocument& d)
{
  using namespace std;
  Model* m = const_cast<SBMLDocument&>(d).getModel();

  if (m != NULL)
  {
    ValidatingVisitor vv(*this, *m);
    d.accept(vv);
  }

  return mMessages.size();
}


/**
 * Validates the given SBMLDocument.  Error messages logged during
 * validation may be retrieved via <code>getMessages()</code>.
 *
 * @return the number of validation errors that occurred.
 */
unsigned int
Validator::validate (const std::string& filename)
{
  SBMLReader    reader;
  SBMLDocument& d = *reader.readSBML(filename);

  return validate(d);
}
