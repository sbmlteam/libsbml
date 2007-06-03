/**
 * @file    UniqueVarsInInitialAssignmentsAndRules.cpp
 * @brief   Ensures unique variables assigned by rules and events
 * @author  Sarah Keating
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


#include <sbml/Model.h>
#include <sbml/Rule.h>
#include <sbml/Event.h>
#include <sbml/EventAssignment.h>

#include "UniqueVarsInInitialAssignmentsAndRules.h"

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */


static const char* PREAMBLE =
    "The value of a 'symbol' field in any <initialAssignment> definition "
    "cannot also appear as the value of a 'variable' field in an "
    "<assignmentRule>. (References: L2V2 Section 4.10.)";


/**
 * Creates a new Constraint with the given constraint id.
 */
UniqueVarsInInitialAssignmentsAndRules::UniqueVarsInInitialAssignmentsAndRules (unsigned int id, Validator& v) :
  UniqueIdBase(id, v)
{
}


/**
 * Destroys this Constraint.
 */
UniqueVarsInInitialAssignmentsAndRules::~UniqueVarsInInitialAssignmentsAndRules ()
{
}


/**
 * @return the preamble to use when logging constraint violations.
 */
const char*
UniqueVarsInInitialAssignmentsAndRules::getPreamble ()
{
  return PREAMBLE;
}


/**
 * Checks that all ids on the following Model objects are unique:
 * event assignments and assignment rules.
 */
void
UniqueVarsInInitialAssignmentsAndRules::doCheck (const Model& m)
{
  unsigned int n, nr;

  for (n = 0; n < m.getNumInitialAssignments(); ++n)
  {
    checkId(*m.getInitialAssignment(n));

    for (nr = 0; nr < m.getNumRules(); ++nr) 
    {
      const Rule* r = m.getRule(nr);

      if (r->isAssignment()) {
        checkId( *m.getRule(nr) );
      }
    }

    mIdObjectMap.clear();
    
  }  
}
