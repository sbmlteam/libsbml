/**
 * @cond doxygen-libsbml-internal
 *
 * @file    OverdeterminedValidator.cpp
 * @brief   Checks an SBML model for consistency of equations
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

#include <sbml/SBMLTypes.h>
#include <sbml/units/UnitFormulaFormatter.h>

#include <sbml/validator/OverdeterminedValidator.h>


/*
 * Compile ConsistencyConstraints
 */
#include "constraints/OverdeterminedConstraints.cpp"


/*
 * Initializes this Validator with a set of Constraints.
 */
void
OverdeterminedValidator::init ()
{
#define  AddingConstraintsToValidator 1
#include "constraints/OverdeterminedConstraints.cpp"
}


/** @endcond doxygen-libsbml-internal */
