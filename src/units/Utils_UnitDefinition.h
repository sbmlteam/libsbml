/**
 * @cond doxygen-libsbml-internal
 *
 * @file    Utils_UnitDefinition.h
 * @brief   Functions acting on a unit definition
 * @author  Sarah Keating
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

#ifndef Utils_UnitDefinition_h
#define Utils_UnitDefinition_h

#include <sbml/common/extern.h>

#include <sbml/UnitDefinition.h>
#include <sbml/Unit.h>
#include <sbml/UnitKind.h>
#include <sbml/ListOf.h>
#include <sbml/util/util.h>

#include "UnitKindList.h"
#include "Utils_Unit.h"

#ifdef __cplusplus

/** 
 * returns the unitDefinition simplified
 */
LIBSBML_EXTERN
void simplifyUnitDefinition(UnitDefinition *);

/** 
 * returns the unitDefinition with unit kinds in alphabetical order
 */
LIBSBML_EXTERN
void orderUnitDefinition(UnitDefinition *);

/**
 * returns a unitDefinition which is the 
 * argument converted to SI units
 */
LIBSBML_EXTERN
UnitDefinition * convertToSI(UnitDefinition *);

LIBSBML_EXTERN
UnitDefinition * convertToSI(const UnitDefinition *);

/** 
 * returns true if unit definitions are identical
 */
LIBSBML_EXTERN
int areIdentical(UnitDefinition *, UnitDefinition *);

LIBSBML_EXTERN
int areIdentical(const UnitDefinition *, const UnitDefinition *);

/** 
 * returns true if unit definitions are equivalent
 * unit definitions will equivalent if they contain
 * equivalent units
 */
LIBSBML_EXTERN
int areEquivalent(const UnitDefinition *, const UnitDefinition *);

LIBSBML_EXTERN
int areEquivalent(const UnitDefinition *, UnitDefinition *);

/** 
 * combines the unitDefinitions 
 */
LIBSBML_EXTERN
void combine(UnitDefinition *, UnitDefinition *);


#endif
#endif  /* Utils_UnitDefinition_h */


/** @endcond doxygen-libsbml-internal */
