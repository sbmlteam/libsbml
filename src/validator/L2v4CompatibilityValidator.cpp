/**
 * @cond doxygen-libsbml-internal
 *
 * @file    L2v4CompatibilityValidator.cpp
 * @brief   Checks whether an SBML model can be converted to L2V3
 * @author  Sarah Keating
 *
 * $Id$
 * $HeadURL$
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2010 California Institute of Technology.
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

LIBSBML_CPP_NAMESPACE_BEGIN

/*
 * Initializes this Validator with a set of Constraints.
 */
void
L2v4CompatibilityValidator::init ()
{
#define  AddingConstraintsToValidator 1
#include "constraints/L2v4CompatibilityConstraints.cpp"
}

LIBSBML_CPP_NAMESPACE_END

/** @endcond doxygen-libsbml-internal */
