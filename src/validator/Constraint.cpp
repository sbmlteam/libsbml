/**
 * \file    Constraint.cpp
 * \brief   Base class for all SBML Validator Constraints
 * \author  Ben Bornstein
 * 
 * $Id$
 * $Source$
 */
/* Copyright 2005 California Institute of Technology and Japan Science and
 * Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is
 * provided in the file named "LICENSE.txt" included with this software
 * distribution.  It is also available online at
 * http://sbml.org/software/libsbml/license.html
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 */


#include <sbml/validator/Constraint.h>


using namespace std;


VConstraint::VConstraint (unsigned int id, Validator& v) :
    mId       ( id   )
  , mSeverity ( 2    )
  , mValidator( v    )
  , mLogMsg   ( true )
{
}


VConstraint::~VConstraint ()
{
}


/**
 * @return the id of this Constraint.
 */
unsigned int
VConstraint::getId () const
{
  return mId;
}


/**
 * @return the severity for violating this Constraint.
 */
unsigned int
VConstraint::getSeverity () const
{
  return mSeverity;
}


/**
 * Logs a constraint failure to the validator for the given SBML object.
 */
void
VConstraint::logFailure (const SBase& object)
{
  logFailure(object, msg);
}


/**
 * Logs a constraint failure to the validator for the given SBML object.
 * The parameter message is used instead of the constraint's member
 * variable msg.
 */
void
VConstraint::logFailure (const SBase& object, const std::string& message)
{
  mValidator.logMessage
  (
   XMLError( mId, message, XMLError::Error, mValidator.getCategory(),
             object.getLine(), object.getColumn() )
            
  );
}
