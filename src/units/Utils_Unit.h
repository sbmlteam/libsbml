/**
 * @cond doxygen-libsbml-internal
 *
 * @file    Utils_Unit.h
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

#ifndef Utils_Unit_h
#define Utils_Unit_h

#include <math.h>

#include <sbml/common/extern.h>

#include <sbml/UnitDefinition.h>
#include <sbml/Unit.h>
#include <sbml/ListOf.h>

#include "UnitKindList.h"

#ifdef __cplusplus

/** 
 * alters the multiplier so that scale = 0
 * eg 1 mm can be expressed as multipier = 1 scale = -3 exponent = 1
 * or as multiplier = 0.001 scale = 0
 */
LIBSBML_EXTERN
void removeScale(Unit *);

/** 
 * multiplies the first unit by the second
 * this function applies both units are of the same kind
 */
LIBSBML_EXTERN
void mergeUnits(Unit * unit1, Unit * unit2);

/**
 * returns a unitdefinition which is the 
 * argument converted to SI units
 */
LIBSBML_EXTERN
UnitDefinition * convertUnitToSI(Unit *);
LIBSBML_EXTERN
UnitDefinition * convertUnitToSI(const Unit *);

/** 
 * returns true if units are identical
 */
LIBSBML_EXTERN
int areIdentical(Unit *, Unit *);

/** 
 * returns true if units are equivalent
 */
LIBSBML_EXTERN
int areEquivalent(Unit *, Unit *);

#endif /* __cplusplus */

BEGIN_C_DECLS

/** 
 * alters the multiplier so that scale = 0
 * eg 1 mm can be expressed as multipier = 1 scale = -3 exponent = 1
 * or as multiplier = 0.001 scale = 0
 */  
LIBSBML_EXTERN
void Unit_removeScale(Unit_t *);


END_C_DECLS



#endif  /* Utils_Unit_h */

/** @endcond doxygen-libsbml-internal */
