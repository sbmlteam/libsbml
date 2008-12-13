/**
 * @cond doxygen-libsbml-internal
 *
 * @file    L2v4CompatibilityValidator.cpp
 * @brief   Checks whether an SBML model can be converted to L2V3
 * @author  Sarah Keating
 *
 * $Id: L2v4CompatibilityValidator.cpp 7249 2008-06-26 22:48:40Z mhucka $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/trunk/libsbml/src/validator/L2v4CompatibilityValidator.cpp $
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
#include "L2v4CompatibilityValidator.h"


/*
 * Compile L2v4CompatibilityConstraints
 */
#include "constraints/L2v4CompatibilityConstraints.cpp"


/*
 * Initializes this Validator with a set of Constraints.
 */
void
L2v4CompatibilityValidator::init ()
{
#define  AddingConstraintsToValidator 1
#include "constraints/L2v4CompatibilityConstraints.cpp"
}


/** @endcond doxygen-libsbml-internal */
