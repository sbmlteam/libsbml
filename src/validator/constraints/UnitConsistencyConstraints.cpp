/**
 * @file    UnitConsistencyConstraints.cpp
 * @brief   UnitConsistency check constraints.  See SBML Wiki
 * @author  Ben Bornstein
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

//#include <string>

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

#include "PowerUnitsCheck.h"
#include "ExponentUnitsCheck.h"
#include "ArgumentsUnitsCheck.h"
#include "ArgumentsUnitsCheckWarnings.h"

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


// General Unit validation


EXTERN_CONSTRAINT(10501, ArgumentsUnitsCheck)
EXTERN_CONSTRAINT(99502, ArgumentsUnitsCheckWarnings)
EXTERN_CONSTRAINT(99503, PowerUnitsCheck)
EXTERN_CONSTRAINT(99504, ExponentUnitsCheck)

START_CONSTRAINT (10511, AssignmentRule, ar)
{
  const string& variable = ar.getVariable();
  const Compartment* c = m.getCompartment(variable);

  pre ( c != NULL);
  pre ( ar.isSetMath() == 1 );

  //if (ar.getLevel() == 2)
  //{
  //  //msg =
  //  //  "When the 'variable' in an <assignmentRule> refers to a <compartment>, "
  //  //  "the units of the rule's right-hand side must be consistent with the "
  //  //  "units of that compartment's size. (References: L2V2 Section 4.11.3; "
  //  //  "L2V3 Section 4.11.3.)";
  //}
  //else
  //{
  //  msg =
  //    "In a level 1 model this implies that in a <compartmentVolumeRule>, "
  //    "the units of the rule's right-hand side must be consistent with the "
  //    "units of that compartment's volume. Expected units are ";
  //  msg += printUnits(variableUnits->getUnitDefinition());
  //  msg += " but the units returned by the <compartmentVolumeRule>'s formula are ";
  //  msg += printUnits(formulaUnits->getUnitDefinition());
  //  msg += ".";
  //}


  const FormulaUnitsData * variableUnits = 
                              m.getFormulaUnitsData(variable, SBML_COMPARTMENT);
  const FormulaUnitsData * formulaUnits = 
                          m.getFormulaUnitsData(variable, SBML_ASSIGNMENT_RULE);

  pre ( formulaUnits != 0 );
  pre ( variableUnits != 0); 

  /* check that the formula is okay 
     ie has no parameters with undeclared units */
  pre ( formulaUnits->getContainsParametersWithUndeclaredUnits() == 0
	|| (formulaUnits->getContainsParametersWithUndeclaredUnits() == 1
	    && formulaUnits->getCanIgnoreUndeclaredUnits() == 1) );

  if (ar.getLevel() == 1)
  {
    msg =
      "In a level 1 model this implies that in a <compartmentVolumeRule>, "
      "the units of the rule's right-hand side must be consistent with the "
      "units of that <compartment>'s volume. Expected units are ";
    msg += printUnits(variableUnits->getUnitDefinition());
    msg += " but the units returned by the <compartmentVolumeRule>'s formula are ";
    msg += printUnits(formulaUnits->getUnitDefinition());
    msg += ".";
  }
  else
  {
    msg =  " Expected units are ";
    msg += printUnits(variableUnits->getUnitDefinition());
    msg += " but the units returned by the <assignmentRule>'s <math> expression are ";
    msg += printUnits(formulaUnits->getUnitDefinition());
    msg += ".";
  }

  inv (areEquivalent(formulaUnits->getUnitDefinition(), 
                          variableUnits->getUnitDefinition()) == 1);

}
END_CONSTRAINT


START_CONSTRAINT (10512, AssignmentRule, ar)
{
  const string& variable = ar.getVariable();
  const Species * s = m.getSpecies(variable);

  pre ( s != NULL);
  pre ( ar.isSetMath() == 1 );

  //if (ar.getLevel() == 2)
  //{
  //  //msg =
  //  //  "When the 'variable' in an <assignmentRule> refers to a <species>, the "
  //  //  "units of the rule's right-hand side must be consistent with the units "
  //  //  "of the species' quantity. (References: L2V2 Section 4.11.3; L2V3 "
  //  //  "Section 4.11.3.)";
  //}
  //else
  //{
  //  msg =
  //    "In a level 1 model this implies that in a <speciesConcentrationRule>, the "
  //    "units of the rule's right-hand side must be consistent with the units "
  //    "of the species' quantity.";
  //}


  const FormulaUnitsData * variableUnits = 
                                  m.getFormulaUnitsData(variable, SBML_SPECIES);
  const FormulaUnitsData * formulaUnits = 
                          m.getFormulaUnitsData(variable, SBML_ASSIGNMENT_RULE);

  pre ( formulaUnits != 0 );
  pre ( variableUnits != 0); 

  /* check that the formula is okay 
     ie has no parameters with undeclared units */
  pre (formulaUnits->getContainsParametersWithUndeclaredUnits() == 0
    || (formulaUnits->getContainsParametersWithUndeclaredUnits() == 1 &&
        formulaUnits->getCanIgnoreUndeclaredUnits() == 1));

  if (ar.getLevel() == 1)
  {
    msg =
      "In a level 1 model this implies that in a <speciesConcentrationRule>, "
      "the units of the rule's right-hand side must be consistent with the "
      "units of that <species> quantity. Expected units are ";
    msg += printUnits(variableUnits->getUnitDefinition());
    msg += " but the units returned by the <speciesConcentrationRule>'s formula are ";
    msg += printUnits(formulaUnits->getUnitDefinition());
    msg += ".";
  }
  else
  {
    msg =  " Expected units are ";
    msg += printUnits(variableUnits->getUnitDefinition());
    msg += " but the units returned by the <assignmentRule>'s <math> expression are ";
    msg += printUnits(formulaUnits->getUnitDefinition());
    msg += ".";
  }

  if (ar.getLevel() == 1)
  {
    /* need to adapt for the fact that in level 1 the assignment rule was
     * for speciesConcetration although species only had substance units
     */

    inv (areEquivalent(formulaUnits->getUnitDefinition(), 
                        variableUnits->getL1SpeciesConcUnitDefinition()) == 1);

  }
  else
  {
    inv (areEquivalent(formulaUnits->getUnitDefinition(), 
                            variableUnits->getUnitDefinition()) == 1);
  }
}
END_CONSTRAINT


START_CONSTRAINT (10513, AssignmentRule, ar)
{
  const string& variable = ar.getVariable();
  const Parameter* p = m.getParameter(variable);

  pre ( p != NULL);
  pre ( ar.isSetMath() == 1 );
  /* check that the parameter has units declared */
  pre ( p->isSetUnits());

  //if (ar.getLevel() == 2)
  //{
  //  //msg =
  //  //  "When the 'variable' in an <assignmentRule> refers to a <parameter>, the "
  //  //  "units of the rule's right-hand side must be consistent with the units "
  //  //  "declared for that parameter. (References: L2V2 Section 4.11.3; L2V3 "
  //  //  "Section 4.11.3.)";
  //}
  //else
  //{
  //  msg =
  //    "In a level 1 model this implies that in a <parameterRule>, the "
  //    "units of the rule's right-hand side must be consistent with the units "
  //    "declared for that parameter.";
  //}

  const FormulaUnitsData * variableUnits = 
                                m.getFormulaUnitsData(variable, SBML_PARAMETER);
  const FormulaUnitsData * formulaUnits = 
                          m.getFormulaUnitsData(variable, SBML_ASSIGNMENT_RULE);

  pre ( formulaUnits != 0 );
  pre ( variableUnits != 0); 

  /* check that the formula is okay 
     ie has no parameters with undeclared units */
  pre (formulaUnits->getContainsParametersWithUndeclaredUnits() == 0
    || (formulaUnits->getContainsParametersWithUndeclaredUnits() == 1 &&
        formulaUnits->getCanIgnoreUndeclaredUnits() == 1));

  if (ar.getLevel() == 1)
  {
    msg =
      "In a level 1 model this implies that in a <parameterRule>, "
      "the units of the rule's right-hand side must be consistent with the "
      "units declared for that <parameter>. Expected units are ";
    msg += printUnits(variableUnits->getUnitDefinition());
    msg += " but the units returned by the <parameterRule>'s formula are ";
    msg += printUnits(formulaUnits->getUnitDefinition());
    msg += ".";
  }
  else
  {
    msg =  " Expected units are ";
    msg += printUnits(variableUnits->getUnitDefinition());
    msg += " but the units returned by the <assignmentRule>'s <math> expression are ";
    msg += printUnits(formulaUnits->getUnitDefinition());
    msg += ".";
  }

  inv (areEquivalent(formulaUnits->getUnitDefinition(), 
                          variableUnits->getUnitDefinition()) == 1);
}
END_CONSTRAINT


START_CONSTRAINT (10521, InitialAssignment, ia)
{
  const string& variable = ia.getSymbol();
  const Compartment* c = m.getCompartment(variable);

  pre ( c != NULL);
  pre ( ia.isSetMath() == 1 );

  //msg =
  //  "When the 'variable' in an <initialAssignment> refers to a "
  //  "<compartment>, the units of the <initialAssignment>'s <math> expression "
  //  "must be consistent with the units of that compartment's size. "
  //  "(References: L2V2 Section 4.10.4; L2V3 Section 4.10.)";


  const FormulaUnitsData * variableUnits = 
                              m.getFormulaUnitsData(variable, SBML_COMPARTMENT);
  const FormulaUnitsData * formulaUnits = 
                       m.getFormulaUnitsData(variable, SBML_INITIAL_ASSIGNMENT);

  pre ( formulaUnits != 0 );
  pre ( variableUnits != 0); 

  /* check that the formula is okay 
     ie has no parameters with undeclared units */
  pre (formulaUnits->getContainsParametersWithUndeclaredUnits() == 0
    || (formulaUnits->getContainsParametersWithUndeclaredUnits() == 1 &&
        formulaUnits->getCanIgnoreUndeclaredUnits() == 1));

  msg =  "Expected units are ";
  msg += printUnits(variableUnits->getUnitDefinition());
  msg += " but the units returned by the <initialAssignment>'s <math> expression are ";
  msg += printUnits(formulaUnits->getUnitDefinition());
  msg += ".";

  inv (areEquivalent(formulaUnits->getUnitDefinition(), 
                          variableUnits->getUnitDefinition()) == 1);
}
END_CONSTRAINT


START_CONSTRAINT (10522, InitialAssignment, ia)
{
  const string& variable = ia.getSymbol();
  const Species * s = m.getSpecies(variable);

  pre ( s != NULL);
  pre ( ia.isSetMath() == 1 );

  //msg =
  //  "When the 'variable' in an <initialAssignment> refers to a <species>, "
  //  "the units of the <initialAssignment>'s <math> expression must be "
  //  "consistent with the units of that species' quantity. (References: L2V2 "
  //  "Section 4.10.4; L2V3 Section 4.11.3.)";
   

  const FormulaUnitsData * variableUnits = 
                                  m.getFormulaUnitsData(variable, SBML_SPECIES);
  const FormulaUnitsData * formulaUnits = 
                       m.getFormulaUnitsData(variable, SBML_INITIAL_ASSIGNMENT);

  pre ( formulaUnits != 0 );
  pre ( variableUnits != 0); 

  /* check that the formula is okay 
     ie has no parameters with undeclared units */
  pre (formulaUnits->getContainsParametersWithUndeclaredUnits() == 0
    || (formulaUnits->getContainsParametersWithUndeclaredUnits() == 1 &&
        formulaUnits->getCanIgnoreUndeclaredUnits() == 1));

  msg =  "Expected units are ";
  msg += printUnits(variableUnits->getUnitDefinition());
  msg += " but the units returned by the <initialAssignment>'s <math> expression are ";
  msg += printUnits(formulaUnits->getUnitDefinition());
  msg += ".";

  inv (areEquivalent(formulaUnits->getUnitDefinition(), 
                          variableUnits->getUnitDefinition()) == 1);
}
END_CONSTRAINT


START_CONSTRAINT (10523, InitialAssignment, ia)
{
  const string& variable = ia.getSymbol();
  const Parameter* p = m.getParameter(variable);

  pre ( p != NULL);
  pre ( ia.isSetMath() == 1 );
  /* check that the parameter has units declared */
  pre ( p->isSetUnits());

  //msg =
  //  "When the 'variable' in an <initialAssignment> refers to a <parameter>, "
  //  "the units of the <initialAssignment>'s <math> expression must be "
  //  "consistent with the units declared for that parameter. (References: "
  //  "L2V2 Section 4.10.4; L2V3 Section 4.11.3.)";
   
  const FormulaUnitsData * variableUnits = 
                                m.getFormulaUnitsData(variable, SBML_PARAMETER);
  const FormulaUnitsData * formulaUnits = 
                       m.getFormulaUnitsData(variable, SBML_INITIAL_ASSIGNMENT);
  
  pre ( formulaUnits != 0 );
  pre ( variableUnits != 0); 

  /* check that the formula is okay 
     ie has no parameters with undeclared units */
  pre (formulaUnits->getContainsParametersWithUndeclaredUnits() == 0
    || (formulaUnits->getContainsParametersWithUndeclaredUnits() == 1 &&
        formulaUnits->getCanIgnoreUndeclaredUnits() == 1));

  msg =  "Expected units are ";
  msg += printUnits(variableUnits->getUnitDefinition());
  msg += " but the units returned by the <initialAssignment>'s <math> expression are ";
  msg += printUnits(formulaUnits->getUnitDefinition());
  msg += ".";

  inv (areEquivalent(formulaUnits->getUnitDefinition(), 
                          variableUnits->getUnitDefinition()) == 1);
}
END_CONSTRAINT


START_CONSTRAINT (10531, RateRule, rr)
{
  const string& variable = rr.getVariable();
  const Compartment* c = m.getCompartment(variable);

  pre ( c != NULL);
  pre ( rr.isSetMath() == 1 );

  //if (rr.getLevel() == 2)
  //{
  //  //msg =
  //  //  "When the 'variable' in a <rateRule> definition refers to a "
  //  //  "<compartment>, the units of the rule's right-hand side must be of the "
  //  //  "form _x per time_, where _x_ is either the 'units' in that "
  //  //  "<compartment> definition, or (in the absence of explicit units declared "
  //  //  "for the compartment size) the default units for that compartment, and "
  //  //  "_time_ refers to the units of time for the model. (References: L2V2 "
  //  //  "Section 4.11.4; L2V3 Section 4.11.4.)";
  //}
  //else
  //{
  //  msg =
  //    "In a level 1 model this implies that "
  //    "when a <compartmentVolumeRule> definition is of type 'rate' "
  //    "the units of the rule's right-hand side must be of the "
  //    "form _x per time_, where _x_ is either the 'units' in that "
  //    "<compartment> definition, or (in the absence of explicit units declared "
  //    "for the compartment volume) the default units for that compartment, and "
  //    "_time_ refers to the units of time for the model.";
  //}
   

  const FormulaUnitsData * variableUnits = 
                              m.getFormulaUnitsData(variable, SBML_COMPARTMENT);
  const FormulaUnitsData * formulaUnits = 
                                m.getFormulaUnitsData(variable, SBML_RATE_RULE);

  pre ( formulaUnits != 0 );
  pre ( variableUnits != 0); 

  /* check that the formula is okay 
     ie has no parameters with undeclared units */
  pre (formulaUnits->getContainsParametersWithUndeclaredUnits() == 0
    || (formulaUnits->getContainsParametersWithUndeclaredUnits() == 1 &&
        formulaUnits->getCanIgnoreUndeclaredUnits() == 1));

  if (rr.getLevel() == 1)
  {
    msg =
    "In a level 1 model this implies that "
    "when a <compartmentVolumeRule> definition is of type 'rate' "
    "the units of the rule's right-hand side must be of the "
    "form _x per time_, where _x_ is either the 'units' in that "
    "<compartment> definition, or (in the absence of explicit units declared "
    "for the compartment volume) the default units for that compartment, and "
    "_time_ refers to the units of time for the model. Expected units are ";    
    msg += printUnits(variableUnits->getUnitDefinition());
    msg += " but the units returned by the <compartmentVolumeRule>'s formula are ";
    msg += printUnits(formulaUnits->getUnitDefinition());
    msg += ".";
  }
  else
  {
    msg =  " Expected units are ";
    msg += printUnits(variableUnits->getUnitDefinition());
    msg += " but the units returned by the <rateRule>'s <math> expression are ";
    msg += printUnits(formulaUnits->getUnitDefinition());
    msg += ".";
  }

  inv (areEquivalent(formulaUnits->getUnitDefinition(), 
                               variableUnits->getPerTimeUnitDefinition()) == 1);

}
END_CONSTRAINT


START_CONSTRAINT (10532, RateRule, rr)
{
  const string& variable = rr.getVariable();
  const Species* s = m.getSpecies(variable);

  pre ( s != NULL);
  pre ( rr.isSetMath() == 1 );

  //if (rr.getLevel() == 2)
  //{
  //  //msg =
  //  //  "When the 'variable' in a <rateRule> definition refers to a <species>, "
  //  //  "the units of the rule's right-hand side must be of the form _x per "
  //  //  "time_, where _x_ is the units of that species' quantity, and _time_ "
  //  //  "refers to the units of time for the model. (References: L2V2 Section "
  //  //  "4.11.4; L2V3 Section 4.11.4.)";
  //}
  //else
  //{
  //  msg =
  //    "In a level 1 model this implies that "
  //    "when a <speciesConcentrationRule> definition is of type 'rate' "
  //    "the units of the rule's right-hand side must be of the form _x per "
  //    "time_, where _x_ is the units of that species' quantity, and _time_ "
  //    "refers to the units of time for the model.";
  //}
 
  const FormulaUnitsData * variableUnits = 
                                  m.getFormulaUnitsData(variable, SBML_SPECIES);
  const FormulaUnitsData * formulaUnits = 
                                m.getFormulaUnitsData(variable, SBML_RATE_RULE);

  pre ( formulaUnits != 0 );
  pre ( variableUnits != 0); 

  /* check that the formula is okay 
     ie has no parameters with undeclared units */
  pre (formulaUnits->getContainsParametersWithUndeclaredUnits() == 0
    || (formulaUnits->getContainsParametersWithUndeclaredUnits() == 1 &&
        formulaUnits->getCanIgnoreUndeclaredUnits() == 1));

  if (rr.getLevel() == 1)
  {
    msg =
    "In a level 1 model this implies that "
    "when a <speciesConcentrationRule> definition is of type 'rate' "
    "the units of the rule's right-hand side must be of the form _x per "
    "time_, where _x_ is the units of that species' quantity, and _time_ "
    "refers to the units of time for the model. Expected units are ";    
    msg += printUnits(variableUnits->getUnitDefinition());
    msg += " but the units returned by the <speciesConcentrationRule>'s formula are ";
    msg += printUnits(formulaUnits->getUnitDefinition());
    msg += ".";
  }
  else
  {
    msg =  " Expected units are ";
    msg += printUnits(variableUnits->getUnitDefinition());
    msg += " but the units returned by the <rateRule>'s <math> expression are ";
    msg += printUnits(formulaUnits->getUnitDefinition());
    msg += ".";
  }

  if (rr.getLevel() == 1)
  {
    /* need to adapt for the fact that in level 1 the assignment rule was
     * for speciesConcetration although species only had substance units
     */

    inv (areEquivalent(formulaUnits->getUnitDefinition(), 
                variableUnits->getL1SpeciesConcPerTimeUnitDefinition()) == 1);

  }
  else
  {
    inv (areEquivalent(formulaUnits->getUnitDefinition(), 
                            variableUnits->getPerTimeUnitDefinition()) == 1);
  }
}
END_CONSTRAINT


START_CONSTRAINT (10533, RateRule, rr)
{
  const string& variable = rr.getVariable();
  const Parameter* p = m.getParameter(variable);

  pre ( p != NULL);
  pre ( rr.isSetMath() == 1 );
  /* check that the parameter has units declared */
  pre ( p->isSetUnits());

  //if (rr.getLevel() == 2)
  //{
  //  //msg =
  //  //  "When the 'variable' in a <rateRule> definition refers to a <parameter>, "
  //  //  "the units of the rule's right-hand side must be of the form _x per "
  //  //  "time_, where _x_ is the 'units' in that <parameter> definition, and "
  //  //  "_time_ refers to the units of time for the model. (References: L2V2 "
  //  //  "Section 4.11.4; L2V3 Section 4.11.4.)";
  //}
  //else
  //{
  //  msg =
  //    "In a level 1 model this implies that "
  //    "when a <parameterRule> definition has type 'rate' "
  //    "the units of the rule's right-hand side must be of the form _x per "
  //    "time_, where _x_ is the 'units' in that <parameter> definition, and "
  //    "_time_ refers to the units of time for the model.";
  //}

  const FormulaUnitsData * variableUnits = 
                                m.getFormulaUnitsData(variable, SBML_PARAMETER);
  const FormulaUnitsData * formulaUnits = 
                                m.getFormulaUnitsData(variable, SBML_RATE_RULE);

  pre ( formulaUnits != 0 );
  pre ( variableUnits != 0); 

  /* check that the formula is okay 
     ie has no parameters with undeclared units */
  pre (formulaUnits->getContainsParametersWithUndeclaredUnits() == 0
    || (formulaUnits->getContainsParametersWithUndeclaredUnits() == 1 &&
        formulaUnits->getCanIgnoreUndeclaredUnits() == 1));

  if (rr.getLevel() == 1)
  {
    msg =
    "In a level 1 model this implies that "
    "when a <parameterRule> definition has type 'rate' "
    "the units of the rule's right-hand side must be of the form _x per "
    "time_, where _x_ is the 'units' in that <parameter> definition, and "
    "_time_ refers to the units of time for the model. Expected units are ";    
    msg += printUnits(variableUnits->getUnitDefinition());
    msg += " but the units returned by the <parameterRule>'s formula are ";
    msg += printUnits(formulaUnits->getUnitDefinition());
    msg += ".";
  }
  else
  {
    msg =  " Expected units are ";
    msg += printUnits(variableUnits->getUnitDefinition());
    msg += " but the units returned by the <rateRule>'s <math> expression are ";
    msg += printUnits(formulaUnits->getUnitDefinition());
    msg += ".";
  }

  inv (areEquivalent(formulaUnits->getUnitDefinition(), 
                              variableUnits->getPerTimeUnitDefinition()) == 1);
}
END_CONSTRAINT


START_CONSTRAINT (10541, KineticLaw, kl)
{
  //msg =
  //  "The units of the 'math' formula in a <kineticLaw> definition must be "
  //  "the equivalent of _substance per time_. (References: L2V2 Section "
  //  "4.13.5.)";

  pre ( kl.isSetMath() == 1 );

  const FormulaUnitsData * formulaUnits = 
                            m.getFormulaUnitsData(kl.getId(), SBML_KINETIC_LAW);
  const FormulaUnitsData * variableUnits = 
                           m.getFormulaUnitsData("subs_per_time", SBML_UNKNOWN);

  pre ( formulaUnits != 0 );
  pre ( variableUnits != 0); 

  /* check that the formula is okay 
     ie has no parameters with undeclared units */
  pre (formulaUnits->getContainsParametersWithUndeclaredUnits() == 0
    || (formulaUnits->getContainsParametersWithUndeclaredUnits() == 1 &&
        formulaUnits->getCanIgnoreUndeclaredUnits() == 1));

  msg =  "Expected units are ";
  msg += printUnits(variableUnits->getUnitDefinition());
  msg += " but the units returned by the <kineticLaw>'s <math> expression are ";
  msg += printUnits(formulaUnits->getUnitDefinition());
  msg += ".";


  inv (areEquivalent(formulaUnits->getUnitDefinition(), 
                                      variableUnits->getUnitDefinition()) == 1);
}
END_CONSTRAINT


START_CONSTRAINT (10551, Event, e)
{
  //msg =
  //  "When a value for <delay> is given in a <event> definition, the units of "
  //  "the delay formula must correspond to either the value of 'timeUnits' in "
  //  "the <event> or (if no 'timeUnits' are given), the model's default units "
  //  "of time. (References: L2V2 Section 4.14; L2V3 Section 4.14.3.)";

  pre ( e.isSetDelay() == 1 );

  const FormulaUnitsData * formulaUnits = 
                                  m.getFormulaUnitsData(e.getId(), SBML_EVENT);

  pre ( formulaUnits != 0 );

  /* check that the formula is okay 
     ie has no parameters with undeclared units */
  pre (formulaUnits->getContainsParametersWithUndeclaredUnits() == 0
    || (formulaUnits->getContainsParametersWithUndeclaredUnits() == 1 &&
        formulaUnits->getCanIgnoreUndeclaredUnits() == 1));

  msg =  "Expected units are ";
  msg += printUnits(formulaUnits->getEventTimeUnitDefinition());
  msg += " but the units returned by the <event>'s <delay> are ";
  msg += printUnits(formulaUnits->getUnitDefinition());
  msg += ".";

  inv (areIdentical(formulaUnits->getUnitDefinition(), 
                              formulaUnits->getEventTimeUnitDefinition()) == 1);

}
END_CONSTRAINT


START_CONSTRAINT (10561, EventAssignment, ea)
{
  //msg =
  //  "When the 'variable' in an <eventAssignment> refers to a <compartment>, "
  //  "the units of the <eventAssignment>'s <math> expression must be consistent "
  //  "with the units of that compartment's size. (References: L2V2 Section "
  //  "4.14.2; L2V3 Section 4.14.4.)";


  const string& variable = ea.getVariable();
  const Compartment* c = m.getCompartment(variable);

  pre ( c != NULL);
  pre ( ea.isSetMath() == 1 );

  const FormulaUnitsData * variableUnits = 
                              m.getFormulaUnitsData(variable, SBML_COMPARTMENT);
  const FormulaUnitsData * formulaUnits = 
                         m.getFormulaUnitsData(variable, SBML_EVENT_ASSIGNMENT);

  pre ( formulaUnits != 0 );
  pre ( variableUnits != 0); 

  /* check that the formula is okay 
     ie has no parameters with undeclared units */
  pre (formulaUnits->getContainsParametersWithUndeclaredUnits() == 0
    || (formulaUnits->getContainsParametersWithUndeclaredUnits() == 1 &&
        formulaUnits->getCanIgnoreUndeclaredUnits() == 1));

  msg =  "Expected units are ";
  msg += printUnits(variableUnits->getUnitDefinition());
  msg += " but the units returned by the <eventAssignment>'s <math> expression are ";
  msg += printUnits(formulaUnits->getUnitDefinition());
  msg += ".";

  inv (areEquivalent(formulaUnits->getUnitDefinition(), 
                          variableUnits->getUnitDefinition()) == 1);

}
END_CONSTRAINT


START_CONSTRAINT (10562, EventAssignment, ea)
{
  //msg =
  //  "When the 'variable' in an <eventAssignment> refers to a <species>, the "
  //  "units of the <eventAssignment>'s <math> expression must be consistent "
  //  "with the units of the species' quantity. (References: L2V2 Section "
  //  "4.14.2; L2V3 Section 4.14.4.)";
   

  const string& variable = ea.getVariable();
  const Species * s = m.getSpecies(variable);

  pre ( s != NULL);
  pre ( ea.isSetMath() == 1 );

  const FormulaUnitsData * variableUnits = 
                                  m.getFormulaUnitsData(variable, SBML_SPECIES);
  const FormulaUnitsData * formulaUnits = 
                         m.getFormulaUnitsData(variable, SBML_EVENT_ASSIGNMENT);

  pre ( formulaUnits != 0 );
  pre ( variableUnits != 0); 

  /* check that the formula is okay 
     ie has no parameters with undeclared units */
  pre (formulaUnits->getContainsParametersWithUndeclaredUnits() == 0
    || (formulaUnits->getContainsParametersWithUndeclaredUnits() == 1 &&
        formulaUnits->getCanIgnoreUndeclaredUnits() == 1));

  msg =  "Expected units are ";
  msg += printUnits(variableUnits->getUnitDefinition());
  msg += " but the units returned by the <eventAssignment>'s <math> expression are ";
  msg += printUnits(formulaUnits->getUnitDefinition());
  msg += ".";

  inv (areEquivalent(formulaUnits->getUnitDefinition(), 
                          variableUnits->getUnitDefinition()) == 1);
}
END_CONSTRAINT


START_CONSTRAINT (10563, EventAssignment, ea)
{
  //msg =
  //  "When the 'variable' in an <eventAssignment> refers to a <parameter>, the "
  //  "units of the <eventAssignment>'s <math> expression must be consistent "
  //  "with the units declared for that parameter. (References: L2V2 Section "
  //  "4.14.2; L2V3 Section 4.14.4)";
   

  const string& variable = ea.getVariable();
  const Parameter* p = m.getParameter(variable);

  pre ( p != NULL);
  pre ( ea.isSetMath() == 1 );
  /* check that the parameter has units declared */
  pre ( p->isSetUnits());

  const FormulaUnitsData * variableUnits = 
                              m.getFormulaUnitsData(variable, SBML_PARAMETER);
  const FormulaUnitsData * formulaUnits = 
                       m.getFormulaUnitsData(variable, SBML_EVENT_ASSIGNMENT);

  pre ( formulaUnits != 0 );
  pre ( variableUnits != 0); 

  /* check that the formula is okay 
     ie has no parameters with undeclared units */
  pre (formulaUnits->getContainsParametersWithUndeclaredUnits() == 0
    || (formulaUnits->getContainsParametersWithUndeclaredUnits() == 1 &&
        formulaUnits->getCanIgnoreUndeclaredUnits() == 1));

  msg =  "Expected units are ";
  msg += printUnits(variableUnits->getUnitDefinition());
  msg += " but the units returned by the <eventAssignment>'s <math> expression are ";
  msg += printUnits(formulaUnits->getUnitDefinition());
  msg += ".";

  inv (areEquivalent(formulaUnits->getUnitDefinition(), 
                          variableUnits->getUnitDefinition()) == 1);
}
END_CONSTRAINT

