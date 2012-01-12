/**
 * @cond doxygen-libsbml-internal
 *
 * @file    IdentifierConsistencyConstraints.cpp
 * @brief   IdentifierConsistency check constraints.  See SBML Wiki
 * @author  Ben Bornstein
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2009-2012 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EBML-EBI), Hinxton, UK
 *  
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 *     Pasadena, CA, USA 
 *  
 * Copyright (C) 2002-2005 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. Japan Science and Technology Agency, Japan
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ---------------------------------------------------------------------- -->*/

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

/** @endcond */


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
// 10311: syntax of UnitSId - caught at read

/** @endcond */

