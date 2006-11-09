/**
 * \file    Utils_UnitDefinition.h
 * \brief   Functions acting on a unit definition
 * \author  Sarah Keating
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
