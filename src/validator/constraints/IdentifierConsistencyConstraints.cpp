/**
 * @file    IdentifierConsistencyConstraints.cpp
 * @brief   IdentifierConsistency check constraints.  See SBML Wiki
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


#ifndef AddingConstraintsToValidator


#include "UniqueIdsForUnitDefinitions.h"
#include "UniqueIdsInKineticLaw.h"
#include "UniqueIdsInModel.h"
#include "UniqueVarsInEventAssignments.h"
#include "UniqueVarsInRules.h"
#include "UniqueVarsInEventsAndRules.h"
#include "UniqueMetaId.h"


#endif


#include <sbml/validator/ConstraintMacros.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxgen-ignored */



// General Identifier validation 
EXTERN_CONSTRAINT( 10301, UniqueIdsInModel             )
EXTERN_CONSTRAINT( 10302, UniqueIdsForUnitDefinitions  )
EXTERN_CONSTRAINT( 10303, UniqueIdsInKineticLaw        )
EXTERN_CONSTRAINT( 10304, UniqueVarsInRules            )
EXTERN_CONSTRAINT( 10305, UniqueVarsInEventAssignments )
EXTERN_CONSTRAINT( 10306, UniqueVarsInEventsAndRules   )
EXTERN_CONSTRAINT( 10307, UniqueMetaId                 )

// 10308: SBO term - caught at read
// 10309: syntax of metid - caught at read but not finished TO DO
// 10310: syntax of id - caught at read

