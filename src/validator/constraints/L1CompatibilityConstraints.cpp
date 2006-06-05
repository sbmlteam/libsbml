/**
 * \file    L1CompatibilityConstraints.cpp
 * \brief   L1 compatibility for conversion from L2
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


#ifndef AddingConstraintsToValidator
#include <sbml/SBMLTypes.h>
#include <sbml/validator/Constraint.h>
#include <math.h>
#endif


#include <sbml/validator/ConstraintMacros.h>


using namespace std;


START_CONSTRAINT (2000, Model, x)
{
  msg =
    "A Model with Events cannot be represented in Level 1.";

  inv( m.getNumEvents() == 0 );
}
END_CONSTRAINT


START_CONSTRAINT (2001, Compartment, c)
{
  msg =
    "Compartment with spatialDimensions other than three cannot be "
    "represented in Level 1.";

  inv( c.getSpatialDimensions() == 3 );
}
END_CONSTRAINT


/**
 * Taken out as strictly speaking not true L1 models will not list
 * modifiers within a reaction but they are present in the ListOfSpecies
 *
START_CONSTRAINT (2002, Reaction, r)
{
  msg =
    "A Reaction containing modifiers "
  "cannot be represented in Level 1.";

  inv( r.getNumModifiers() == 0);
}
END_CONSTRAINT
*/


START_CONSTRAINT (2003, SpeciesReference, sr) 
{
  msg =
    "A SpeciesReference containing a non-integer or non-rational "
    "'stoichiometryMath' subelement cannot be represented in Level 1.";

  /* doesnt apply if the SpeciesReference is a modifier */
  pre(!sr.isModifier());

  pre( sr.isSetStoichiometryMath() );

  inv_or( sr.getStoichiometryMath()->isInteger()  );
  inv_or( sr.getStoichiometryMath()->isRational() );
}
END_CONSTRAINT


START_CONSTRAINT (2004, Unit, u)
{
  msg =
    "A Unit containing multipliers or offsets cannot be represented in "
    "Level 1.";


  inv( u.getMultiplier() == 1.0 );
  inv( u.getOffset() == 0.0 );
}
END_CONSTRAINT


START_CONSTRAINT (2005, Species, s)
{
  msg =
    "A Species that does not identify its compartment cannot be "
    "represented in Level 1.";


  inv( s.isSetCompartment() );
}
END_CONSTRAINT


START_CONSTRAINT (2006, SpeciesReference, sr)
{
  msg =
    "A SpeciesReference containing a non-integer 'stoichiometry' subelement "
    "cannot be represented in Level 1.";


  pre( !sr.isSetStoichiometryMath() );
  inv( floor(sr.getStoichiometry()) == sr.getStoichiometry() );
}
END_CONSTRAINT


START_CONSTRAINT (2007, Model, x)
{
  msg =
    "Conversion of a model with FunctionDefinitions to Level 1 is not yet "
    "supported.";

  inv( m.getNumFunctionDefinitions() == 0 );
}
END_CONSTRAINT
