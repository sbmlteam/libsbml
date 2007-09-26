/**
 * @file    ModelingPracticeConstraints.cpp
 * @brief   ModelingPractice check constraints.  
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



#ifndef AddingConstraintsToValidator
#include <sbml/SBMLTypes.h>
#include <sbml/validator/VConstraint.h>
#endif

#include <sbml/validator/ConstraintMacros.h>

#include "LocalParameterShadowsIdInModel.h"
/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */



// Compartment validation

START_CONSTRAINT (80501, Compartment, c)
{
  pre( c.getSpatialDimensions() != 0 );
  
  //msg =
  //  "It is recommended that the size of a compartment is set.";

  inv( c.isSetSize() == true );
}
END_CONSTRAINT

EXTERN_CONSTRAINT( 81121, LocalParameterShadowsIdInModel             )



