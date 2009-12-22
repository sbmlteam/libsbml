/**
 * @file    L3v1CompatibilityConstraints.cpp
 * @brief   L3 compatibility for conversion from L2
 * @author  Sarah Keating
 *
 * $Id:  $
 * $HeadURL:  $
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
#include "DuplicateTopLevelAnnotation.h"
#endif


#include <sbml/validator/ConstraintMacros.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */


START_CONSTRAINT (96001, Model, x)
{
  // no speciesType
  inv( m.getNumSpeciesTypes() == 0 );
}
END_CONSTRAINT


START_CONSTRAINT (96002, Model, x)
{
  // no compartmentType

  inv( m.getNumCompartmentTypes() == 0 );
}
END_CONSTRAINT


START_CONSTRAINT (96003, Unit, u)
{
  // no offset

  inv( u.getOffset() == 0.0 );
}
END_CONSTRAINT


START_CONSTRAINT (96004, KineticLaw, kl)
{
  // no TimeUnits

  inv( kl.isSetTimeUnits() == false );
}
END_CONSTRAINT


START_CONSTRAINT (96005, KineticLaw, kl)
{
  // no SubstanceUnits

  inv( kl.isSetSubstanceUnits() == false );
}
END_CONSTRAINT


START_CONSTRAINT (96006, Species, s)
{
  // no spatialSizeUnits
  inv( s.isSetSpatialSizeUnits() == false);
}
END_CONSTRAINT


START_CONSTRAINT (96007, Event, e)
{
  // no TimeUnits

  inv( e.isSetTimeUnits() == false);

}
END_CONSTRAINT


START_CONSTRAINT (96008, Model, m1)
{
  // if the model was earlier than L2V4 the model sbo term will not
  // be valid in l2v4
  pre( m1.getLevel() == 2 );
  pre( m1.getVersion() < 4);

  inv( !m1.isSetSBOTerm());
}
END_CONSTRAINT


EXTERN_CONSTRAINT(96009, DuplicateTopLevelAnnotation)


START_CONSTRAINT (96010, Compartment, c)
{
  // no outside

  inv( c.isSetOutside() == false);
}
END_CONSTRAINT


