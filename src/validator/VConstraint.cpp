/**
 * @file    VConstraint.cpp
 * @brief   Base class for all SBML Validator Constraints
 * @author  Ben Bornstein
 * 
 * $Id$
 * $Source$
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2007 California Institute of Technology.
 * Copyright 2002-2005 California Institute of Technology and
 *                     Japan Science and Technology Corporation.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution and
 * also available online as http://sbml.org/software/libsbml/license.html
 *----------------------------------------------------------------------- -->*/

#include <sbml/validator/VConstraint.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */


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


/** @cond doxygen-libsbml-internal */
/**
 * Logs a constraint failure to the validator for the given SBML object.
 */
void
VConstraint::logFailure (const SBase& object)
{
  logFailure(object, msg);
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/**
 * Logs a constraint failure to the validator for the given SBML object.
 * The parameter message is used instead of the constraint's member
 * variable msg.
 */
void
VConstraint::logFailure (const SBase& object, const std::string& message)
{
  mValidator.logFailure
    ( SBMLError( mId, object.getLevel(), object.getVersion(),
               message, object.getLine(), object.getColumn(),
                  SBMLError::Error, SBMLError::SBML ));

}
/** @endcond doxygen-libsbml-internal */
