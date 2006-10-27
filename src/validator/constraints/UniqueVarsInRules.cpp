/**
 * \file    UniqueVarsInRules.cpp
 * \brief   Ensures variables for all Rules are unique
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


#include <string>

#include <sbml/Model.h>
#include <sbml/Rule.h>

#include "UniqueVarsInRules.h"


using namespace std;


static const char* PREAMBLE =
    "The value of the 'variable' field in all <assignmentRule> and "
    "<rateRule> definitions must be unique across the set of all such rule "
    "definitions in a model. (References: L2V1 Section 4.8.4; L2V2 Section "
    "4.11.3.)";


/**
 * Creates a new Constraint with the given constraint id.
 */
UniqueVarsInRules::UniqueVarsInRules (unsigned int id, Validator& v) :
  UniqueIdBase(id, v)
{
}


/**
 * Destroys this Constraint.
 */
UniqueVarsInRules::~UniqueVarsInRules ()
{
}


/**
 * @return the fieldname ("variable") to use when logging constraint
 * violations.
 */
const char*
UniqueVarsInRules::getFieldname ()
{
  return "variable";
}


/**
 * @return the preamble to use when logging constraint violations.
 */
const char*
UniqueVarsInRules::getPreamble ()
{
  return PREAMBLE;
}


/**
 * Checks that all ids on KineticLawParameters are unique.
 */
void
UniqueVarsInRules::doCheck (const Model& m)
{
  for (unsigned int n = 0; n < m.getNumRules(); ++n) checkId( *m.getRule(n) );
}
