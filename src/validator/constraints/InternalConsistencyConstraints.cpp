/**
 * @file    InternalConsistencyConstraints.cpp
 * @brief   Consistency check constraints.  See SBML Wiki
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

//#include <string>
#include <cstring>

#include <sbml/SBMLTypes.h>
#include <sbml/SBMLTypeCodes.h>
#include <sbml/SBO.h>
#include <sbml/validator/VConstraint.h>
#include <sbml/units/UnitFormulaFormatter.h>
#include <sbml/units/FormulaUnitsData.h>

#include <sbml/util/List.h>

#include "CompartmentOutsideCycles.h"
#include "FunctionDefinitionVars.h"

#include "UniqueIdsForUnitDefinitions.h"
#include "UniqueIdsInKineticLaw.h"
#include "UniqueIdsInModel.h"
#include "UniqueVarsInEventAssignments.h"
#include "UniqueVarsInRules.h"
#include "UniqueVarsInEventsAndRules.h"
#include "UniqueMetaId.h"

#include "FunctionReferredToExists.h"
#include "SpeciesReactionOrRule.h"
#include "UniqueSpeciesTypesInCompartment.h"
#include "UniqueSymbolsInInitialAssignments.h"
#include "UniqueVarsInInitialAssignmentsAndRules.h"
#include "StoichiometryMathVars.h"
#include "KineticLawVars.h"
#include "AssignmentCycles.h"

//#include "FormulaUnitsCheck.h"

//#include "PowerUnitsCheck.h"
//#include "ExponentUnitsCheck.h"
#include "ArgumentsUnitsCheck.h"

#include "LogicalArgsMathCheck.h"
#include "NumericArgsMathCheck.h"
#include "PieceBooleanMathCheck.h"
#include "PiecewiseValueMathCheck.h"
#include "EqualityArgsMathCheck.h"
#include "FunctionApplyMathCheck.h"
#include "CiElementMathCheck.h"
#include "LambdaMathCheck.h"
#include "NumericReturnMathCheck.h"
#include "LocalParameterMathCheck.h"
#include "NumberArgsMathCheck.h"

#include "OverDeterminedCheck.h"

#endif


#include <sbml/validator/ConstraintMacros.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */



// Compartment validation

START_CONSTRAINT (99901, Compartment, c)
{
  // level 1 compartment spatial dimensions should be 3
  pre( c.getLevel() == 1);
  
  inv( c.getSpatialDimensions() == 3 );
}
END_CONSTRAINT


START_CONSTRAINT (99902, Compartment, c)
{
  // level 1 and L2V1 compartment shouldnt have compartmentType
  pre( c.getLevel() == 1 || (c.getLevel() == 2 && c.getVersion() == 1));
  
  inv( c.isSetCompartmentType() == false );
}
END_CONSTRAINT


START_CONSTRAINT (99903, Compartment, c)
{
  // level 1 compartment constant didnt exist and should be false
  pre( c.getLevel() == 1);
  
  inv( c.getConstant() == false );
}
END_CONSTRAINT

START_CONSTRAINT (99904, Compartment, c)
{
  // level 1 metaid didnt exist
  pre( c.getLevel() == 1);
  
  inv( c.isSetMetaId() == false );
}
END_CONSTRAINT

START_CONSTRAINT (99905, Compartment, c)
{
  // level 1; l2v1; l2v2 sboTerm didnt exist
  pre( c.getLevel() == 1 || (c.getLevel() == 2 && c.getVersion() < 3));
  
  inv( c.isSetSBOTerm() == false );
}
END_CONSTRAINT

START_CONSTRAINT (99906, Compartment, c)
{
  // level 1 units check gets missed in check consistency
  pre( c.getLevel() == 1 && c.isSetUnits())
  
  const string&         units = c.getUnits();
  const UnitDefinition* defn  = m.getUnitDefinition(units);

  inv_or( units == "volume" );
  inv_or( units == "litre"  );
  inv_or( units == "liter"  );
  inv_or( defn  != NULL && defn->isVariantOfVolume() );
}
END_CONSTRAINT

START_CONSTRAINT (99907, Compartment, c)
{
  // level 1 version volume required
  pre( c.getLevel() == 1 && c.getVersion() == 1)
  
  inv( c.isSetVolume() == true );
}
END_CONSTRAINT
