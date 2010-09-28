/**
 * @cond doxygen-libsbml-internal
 *
 * @file    L2v4CompatibilityConstraints.cpp
 * @brief   L2v4 compatibility for conversion
 * @author  Sarah Keating
 *
 * $Id$
 * $HeadURL$
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
#include <sbml/util/List.h>
#include "DuplicateTopLevelAnnotation.h"
#endif


#include <sbml/validator/ConstraintMacros.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond */


START_CONSTRAINT (95001, Unit, u)
{
  //msg =
  //  "The 'offset' attribute on <unit> previously available in SBML Level 2 "
  //  "Version 1, has been removed as of SBML Level 2 Version 2. (References: "
  //  "L2V2 Section 4.4.)";

  inv( u.getOffset() == 0.0 );
}
END_CONSTRAINT

START_CONSTRAINT (95002, KineticLaw, kl)
{
  //msg =
  //  "The 'timeUnits' attribute on <kineticLaw>, previously available in SBML "
  //  "Level 1 and Level 2 Version 1, has been removed as of SBML Level 2 "
  //  "Version 2. In SBML Level 2 Version 3, the time units of a reaction rate "
  //  "expression are those of the global 'time' units of the model. "
  //  "(References: L2V2 Section 4.13.5.)";

  inv( kl.isSetTimeUnits() == false );
}
END_CONSTRAINT

START_CONSTRAINT (95003, KineticLaw, kl)
{
  //msg =
  //  "The 'substanceUnits' attribute on <kineticLaw>, previously available in "
  //  "SBML Level 1 and Level 2 Version 1, has been removed as of SBML Level 2 "
  //  "Version 2. In SBML Level 2 Version 3, the substance units of a reaction "
  //  "rate expression are those of the global 'substance' units of the model. "
  //  "(References: L2V2 Section 4.13.5.)";

  inv( kl.isSetSubstanceUnits() == false );
}
END_CONSTRAINT

START_CONSTRAINT (95004, Species, s)
{
  //msg =
  //  "The 'spatialSizeUnits' attribute on <species>, previously available "
  //  "in SBML Level 2 versions prior to Version 3, has been removed as "
  //  "of SBML Level 2 Version 3. (References: L2V3 Section 4.8.)";

  inv( s.isSetSpatialSizeUnits() == false);
}
END_CONSTRAINT

START_CONSTRAINT (95005, Event, e)
{
  //msg =
  //  "The 'timeUnits' attribute on <event>, previously available in SBML "
  //  "Level 2 versions prior to Version 3, has been removed as of SBML "
  //  "Level 2 Version 3. (References: L2V3 Section 4.14.)";

  inv( e.isSetTimeUnits() == false);

}
END_CONSTRAINT

START_CONSTRAINT (95006, Model, m1)
{
  // if the model was earlier than L2V4 the model sbo term will not
  // be valid in l2v4
  pre( m1.getLevel() == 2 );
  pre( m1.getVersion() < 4);

  inv( !m1.isSetSBOTerm());
}
END_CONSTRAINT

EXTERN_CONSTRAINT(95007, DuplicateTopLevelAnnotation)

START_CONSTRAINT (92009, Compartment, c)
{
  inv_or( c.getSpatialDimensions() == 3 );
  inv_or( c.getSpatialDimensions() == 2 );
  inv_or( c.getSpatialDimensions() == 1 );
  inv_or( c.getSpatialDimensionsAsDouble() == 0.0 );
}
END_CONSTRAINT


START_CONSTRAINT (92010, SpeciesReference, sr) 
{
  //msg =
  //  "A <speciesReference> containing a non-integer or non-rational "
  //  "<stoichiometryMath> subelement cannot be represented in SBML Level 1.";

  /* doesnt apply if the SpeciesReference is a modifier */
  pre(!sr.isModifier());

  if (sr.getLevel() > 2)
  {
    inv( sr.getConstant());
  }
}
END_CONSTRAINT


START_CONSTRAINT (91015, Model, x)
{
  pre (m.getLevel() > 2);
  inv( !m.isSetConversionFactor() );
}
END_CONSTRAINT


START_CONSTRAINT (91015, Species, s)
{
  pre (s.getLevel() > 2);
  inv( !s.isSetConversionFactor() );
}
END_CONSTRAINT


START_CONSTRAINT (91016, Reaction, r)
{
  pre (r.getLevel() > 2);
  inv( !r.isSetCompartment() );
}
END_CONSTRAINT


START_CONSTRAINT (91017, Model, x)
{
  pre (m.getLevel() > 2);
  pre (m.isSetExtentUnits());

  std::string extent = m.getExtentUnits();
  const UnitDefinition * ud = m.getUnitDefinition(extent);
  if (ud)
  {
    UnitDefinition *ud1 = new UnitDefinition(m.getSBMLNamespaces());
    for (unsigned int i = 0; i < ud->getNumUnits(); i++)
    {
      Unit * u = new Unit(m.getSBMLNamespaces());
      u->setKind(ud->getUnit(i)->getKind());
      u->setScale(ud->getUnit(i)->getScale());
      u->setExponent(ud->getUnit(i)->getExponent());
      ud1->addUnit(u);
    }
  
    inv( ud1->isVariantOfSubstance());
  }
  else
  {
    inv_or( extent == "mole" );
    inv_or( extent == "item" );
  }
}
END_CONSTRAINT


START_CONSTRAINT (92011, Event, e)
{
  pre (e.getLevel() > 2);
  inv( !e.isSetPriority() );
}
END_CONSTRAINT


START_CONSTRAINT (92012, Event, e)
{
  pre (e.getLevel() > 2);
  pre (e.isSetTrigger());

  inv( e.getTrigger()->getPersistent() == true );
}
END_CONSTRAINT


START_CONSTRAINT (92013, Event, e)
{
  pre (e.getLevel() > 2);
  pre (e.isSetTrigger());

  inv( e.getTrigger()->getInitialValue() == true );
}
END_CONSTRAINT


/** @endcond */
