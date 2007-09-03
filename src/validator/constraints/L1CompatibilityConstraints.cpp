/**
 * @file    L1CompatibilityConstraints.cpp
 * @brief   L1 compatibility for conversion from L2
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
#include <sbml/SBase.h>
#include <sbml/validator/VConstraint.h>
#include <math.h>
#endif


#include <sbml/validator/ConstraintMacros.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */


START_CONSTRAINT (91001, Model, x)
{
  msg =
    "A model with <event> definitions cannot be represented in SBML Level 1.";

  inv( m.getNumEvents() == 0 );
}
END_CONSTRAINT


START_CONSTRAINT (91002, Model, x)
{
  msg =
    "Conversion of a model with <functionDefinition>s to SBML Level 1 is not "
    "yet supported.";

  inv( m.getNumFunctionDefinitions() == 0 );
}
END_CONSTRAINT


START_CONSTRAINT (91003, Model, x)
{
  msg =
    "Conversion of a model with <constraint>s to SBML Level 1 may result "
    "in loss of information.";

  inv( m.getNumConstraints() == 0 );
}
END_CONSTRAINT


START_CONSTRAINT (91004, Model, x)
{
  msg =
    "Conversion of a model with <initialAssignment>s to SBML Level 1 is not "
    "yet supported.";

  inv( m.getNumInitialAssignments() == 0 );
}
END_CONSTRAINT


START_CONSTRAINT (91005, Model, x)
{
  msg =
    "<speciesType> definitions cannot be represented in SBML Level 1.";

  inv( m.getNumSpeciesTypes() == 0 );
}
END_CONSTRAINT


START_CONSTRAINT (91006, Model, x)
{
  msg =
    "<compartmentType> definitions cannot be represented in SBML Level 1.";

  inv( m.getNumCompartmentTypes() == 0 );
}
END_CONSTRAINT


START_CONSTRAINT (91007, Compartment, c)
{
  msg =
    "A <compartment> with 'spatialDimensions' not equal to 3 cannot be "
    "represented in SBML Level 1.";

  inv( c.getSpatialDimensions() == 3 );
}
END_CONSTRAINT


START_CONSTRAINT (91008, SpeciesReference, sr) 
{
  msg =
    "A <peciesReference> containing a non-integer or non-rational "
    "<stoichiometryMath> subelement cannot be represented in SBML Level 1.";

  /* doesnt apply if the SpeciesReference is a modifier */
  pre(!sr.isModifier());

  pre( sr.isSetStoichiometryMath() );

  inv_or( sr.getStoichiometryMath()->getMath()->isInteger()  );
  inv_or( sr.getStoichiometryMath()->getMath()->isRational() );
}
END_CONSTRAINT


START_CONSTRAINT (91009, SpeciesReference, sr)
{
  msg =
    "A <peciesReference> containing a non-integer 'stoichiometry' attribute "
    "value cannot be represented in SBML Level 1.";


  pre( !sr.isSetStoichiometryMath() );
  inv( floor(sr.getStoichiometry()) == sr.getStoichiometry() );
}
END_CONSTRAINT


START_CONSTRAINT (91010, Unit, u)
{
  msg =
    "A <unit> containing multipliers or offsets cannot be represented in "
    "SBML Level 1.";


  inv( u.getMultiplier() == 1.0 );
  inv( u.getOffset() == 0.0 );
}
END_CONSTRAINT


START_CONSTRAINT (91011, Species, s)
{
  msg =
    "A <species> that does not identify its compartment cannot be "
    "represented in SBML Level 1.";


  inv( s.isSetCompartment() );
}
END_CONSTRAINT


START_CONSTRAINT (91012, Species, s)
{
  msg =
    "The information represented by the value of a 'spatialSizeUnit' "
    "attribute on a <species> definition cannot be represented in "
    "SBML Level 1.";


  inv( !s.isSetSpatialSizeUnits() );
}
END_CONSTRAINT


START_CONSTRAINT (91013, Model, x)
{
  msg =
    "SBO terms cannot be represented directly in SBML Level 1.";

  inv( m.getSBOTerm() == -1 );
}
END_CONSTRAINT


START_CONSTRAINT (91013, UnitDefinition, ud)
{
  msg =
    "SBO terms cannot be represented directly in SBML Level 1.";

  inv( ud.getSBOTerm() == -1 );
}
END_CONSTRAINT


START_CONSTRAINT (91013, Unit, u)
{
  msg =
    "SBO terms cannot be represented directly in SBML Level 1.";

  inv( u.getSBOTerm() == -1 );
}
END_CONSTRAINT


START_CONSTRAINT (91013, Compartment, c)
{
  msg =
    "SBO terms cannot be represented directly in SBML Level 1.";

  inv( c.getSBOTerm() == -1 );
}
END_CONSTRAINT


START_CONSTRAINT (91013, Species, s)
{
  msg =
    "SBO terms cannot be represented directly in SBML Level 1.";

  inv( s.getSBOTerm() == -1 );
}
END_CONSTRAINT


START_CONSTRAINT (91013, Parameter, p)
{
  msg =
    "SBO terms cannot be represented directly in SBML Level 1.";

  inv( p.getSBOTerm() == -1 );
}
END_CONSTRAINT


START_CONSTRAINT (91013, AssignmentRule, r)
{
  msg =
    "SBO terms cannot be represented directly in SBML Level 1.";

  inv( r.getSBOTerm() == -1 );
}
END_CONSTRAINT


START_CONSTRAINT (91013, RateRule, r)
{
  msg =
    "SBO terms cannot be represented directly in SBML Level 1.";

  inv( r.getSBOTerm() == -1 );
}
END_CONSTRAINT


START_CONSTRAINT (91013, AlgebraicRule, r)
{
  msg =
    "SBO terms cannot be represented directly in SBML Level 1.";

  inv( r.getSBOTerm() == -1 );
}
END_CONSTRAINT


START_CONSTRAINT (91013, Reaction, r)
{
  msg =
    "SBO terms cannot be represented directly in SBML Level 1.";

  inv( r.getSBOTerm() == -1 );
}
END_CONSTRAINT


START_CONSTRAINT (91013, SpeciesReference, sr)
{
  msg =
    "SBO terms cannot be represented directly in SBML Level 1.";

  inv( sr.getSBOTerm() == -1 );
}
END_CONSTRAINT


START_CONSTRAINT (91013, KineticLaw, kl)
{
  msg =
    "SBO terms cannot be represented directly in SBML Level 1.";

  inv( kl.getSBOTerm() == -1 );
}
END_CONSTRAINT

