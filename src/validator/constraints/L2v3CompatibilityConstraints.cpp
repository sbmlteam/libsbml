/**
 * @file    L2v3CompatibilityConstraints.cpp
 * @brief   L2v3 compatibility for conversion
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
#include <sbml/util/List.h>
#endif


#include <sbml/validator/ConstraintMacros.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */


START_CONSTRAINT (94001, Unit, u)
{
  msg =
    "The 'offset' attribute on <unit> previously available in SBML Level 2 "
    "Version 1, has been removed as of SBML Level 2 Version 2. (References: "
    "L2V2 Section 4.4.)";

  inv( u.getOffset() == 0.0 );
}
END_CONSTRAINT

START_CONSTRAINT (94002, KineticLaw, kl)
{
  msg =
    "The 'timeUnits' attribute on <kineticLaw>, previously available in SBML "
    "Level 1 and Level 2 Version 1, has been removed as of SBML Level 2 "
    "Version 2. In SBML Level 2 Version 2, the time units of a reaction rate "
    "expression are those of the global 'time' units of the model. "
    "(References: L2V2 Section 4.13.5.)";

  inv( kl.isSetTimeUnits() == false );
}
END_CONSTRAINT

START_CONSTRAINT (94003, KineticLaw, kl)
{
  msg =
    "The 'substanceUnits' attribute on <kineticLaw>, previously available in "
    "SBML Level 1 and Level 2 Version 1, has been removed as of SBML Level 2 "
    "Version 2. In SBML Level 2 Version 2, the substance units of a reaction "
    "rate expression are those of the global 'substance' units of the model. "
    "(References: L2V2 Section 4.13.5.)";

  inv( kl.isSetSubstanceUnits() == false );
}
END_CONSTRAINT

START_CONSTRAINT (94004, Species, s)
{
  msg =
    "The 'spatialSizeUnits' attribute on <species>, previously available "
    "in SBML Level 2 versions prior to Version 3, has been removed as "
    "of SBML Level 2 Version 3. (References: L2V3 Section 4.8.)";

  inv( s.isSetSpatialSizeUnits() == false);
}
END_CONSTRAINT

START_CONSTRAINT (94005, Event, e)
{
  msg =
    "The 'timeUnits' attribute on <event>, previously available in SBML "
    "Level 2 versions prior to Version 3, has been removed as of SBML "
    "Level 2 Version 3. (References: L2V3 Section 4.14.)";

  inv( e.isSetTimeUnits() == false);

}
END_CONSTRAINT
