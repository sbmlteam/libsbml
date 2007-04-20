/**
 * @file    L2v1CompatibilityValidator.cpp
 * @brief   Checks whether an SBML model can be converted from L2 to L1
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


#include <sbml/SBMLTypes.h>
#include <sbml/validator/L2v1CompatibilityValidator.h>


/*
 * Compile L2v1CompatibilityConstraints
 */
#include "constraints/L2v1CompatibilityConstraints.cpp"


/**
 * Initializes this Validator with a set of Constraints.
 */
void
L2v1CompatibilityValidator::init ()
{
#define  AddingConstraintsToValidator 1
#include "constraints/L2v1CompatibilityConstraints.cpp"
}
