/**
 * @file    UniqueSymbolsInInitialAssignments.cpp
 * @brief   Ensures the ids for all UnitDefinitions in a Model are unique
 * @author  Ben Bornstein
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
#include <sbml/UnitDefinition.h>

#include "UniqueSymbolsInInitialAssignments.h"

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */


static const char* PREAMBLE =
    "A given identifier cannot appear as the value of more than one 'symbol' "
    "field across the set of <initialAssignment>s in a model. (References: "
    "L2V2 Section 4.10.)";


/**
 * Creates a new Constraint with the given constraint id.
 */
UniqueSymbolsInInitialAssignments::UniqueSymbolsInInitialAssignments ( unsigned int id,
                                                           Validator& v ) :
  UniqueIdBase(id, v)
{
}


/**
 * Destroys this Constraint.
 */
UniqueSymbolsInInitialAssignments::~UniqueSymbolsInInitialAssignments ()
{
}


/**
 * @return the preamble to use when logging constraint violations.
 */
const char*
UniqueSymbolsInInitialAssignments::getPreamble ()
{
  return PREAMBLE;
}


/**
 * Checks that all ids on UnitDefinitions are unique.
 */
void
UniqueSymbolsInInitialAssignments::doCheck (const Model& m)
{
  unsigned int n, size;


  size = m.getNumInitialAssignments();
  for (n = 0; n < size; ++n) checkId( *m.getInitialAssignment(n) );
}
