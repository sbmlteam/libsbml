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

/** @endcond doxgen-ignored */


// General Unit validation


EXTERN_CONSTRAINT(10501, ArgumentsUnitsCheck)

START_CONSTRAINT (10511, AssignmentRule, ar)
{
  msg =
    "When the 'variable' in an <assignmentRule> refers to a <compartment>, "
    "the units of the rule's right-hand side must be consistent with the "
    "units of that compartment's size. (References: L2V2 Section 4.11.3.)";


  const string& variable = ar.getVariable();
  const Compartment* c = m.getCompartment(variable);

  pre ( c != NULL);
  pre ( ar.isSetMath() == 1 );

  const FormulaUnitsData * variableUnits = m.getFormulaUnitsData(variable, SBML_COMPARTMENT);
  const FormulaUnitsData * formulaUnits = m.getFormulaUnitsData(variable, SBML_ASSIGNMENT_RULE);

  pre ( formulaUnits != 0 );
  pre ( variableUnits != 0); 

  /* if the formula is dimensionless then we assume that the user
   * intends it to have default units
   * so shouldnt fail the constraint
   *
   * this changed for l2v3
   *
  if (formulaUnits->getUnitDefinition()->getNumUnits() == 1)
  {
    pre (strcmp(UnitKind_toString(formulaUnits->getUnitDefinition()->getUnit(0)->getKind()), 
                                                                       "dimensionless")); 
  }
  */

  /* check that the formula is okay ie has no parameters with undeclared units */
  pre ( formulaUnits->getContainsParametersWithUndeclaredUnits() == 0
	|| (formulaUnits->getContainsParametersWithUndeclaredUnits() == 1
	    && formulaUnits->getCanIgnoreUndeclaredUnits() == 1) );

  inv (areEquivalent(formulaUnits->getUnitDefinition(), 
                          variableUnits->getUnitDefinition()) == 1);

}
END_CONSTRAINT


START_CONSTRAINT (10512, AssignmentRule, ar)
{
  msg =
    "When the 'variable' in an <assignmentRule> refers to a <species>, the "
    "units of the rule's right-hand side must be consistent with the units "
    "of the species' quantity. (References: L2V2 Section 4.11.3.)";
   

  const string& variable = ar.getVariable();
  const Species * s = m.getSpecies(variable);

  pre ( s != NULL);
  pre ( ar.isSetMath() == 1 );

  const FormulaUnitsData * variableUnits = m.getFormulaUnitsData(variable, SBML_SPECIES);
  const FormulaUnitsData * formulaUnits = m.getFormulaUnitsData(variable, SBML_ASSIGNMENT_RULE);

  pre ( formulaUnits != 0 );
  pre ( variableUnits != 0); 

  /* if the formula is dimensionless then we assume that the user
   * intends it to have default units
   * so shouldnt fail the constraint
   *
   * this changed for l2v3
   *
  if (formulaUnits->getUnitDefinition()->getNumUnits() == 1)
  {
    pre (strcmp(UnitKind_toString(formulaUnits->getUnitDefinition()->getUnit(0)->getKind()), 
                                                                       "dimensionless")); 
  }
  */

  /* check that the formula is okay ie has no parameters with undeclared units */
  pre (formulaUnits->getContainsParametersWithUndeclaredUnits() == 0
    || (formulaUnits->getContainsParametersWithUndeclaredUnits() == 1 &&
        formulaUnits->getCanIgnoreUndeclaredUnits() == 1));

  inv (areEquivalent(formulaUnits->getUnitDefinition(), 
                          variableUnits->getUnitDefinition()) == 1);
}
END_CONSTRAINT


START_CONSTRAINT (10513, AssignmentRule, ar)
{
  msg =
    "When the 'variable' in an <assignmentRule> refers to a <parameter>, the "
    "units of the rule's right-hand side must be consistent with the units "
    "declared for that parameter. (References: L2V2 Section 4.11.3.)";
   

  const string& variable = ar.getVariable();
  const Parameter* p = m.getParameter(variable);

  pre ( p != NULL);
  pre ( ar.isSetMath() == 1 );
  /* check that the parameter has units declared */
  pre ( p->isSetUnits());

  const FormulaUnitsData * variableUnits = m.getFormulaUnitsData(variable, SBML_PARAMETER);
  const FormulaUnitsData * formulaUnits = m.getFormulaUnitsData(variable, SBML_ASSIGNMENT_RULE);

  pre ( formulaUnits != 0 );
  pre ( variableUnits != 0); 

  /* if the formula is dimensionless then we assume that the user
   * intends it to have default units
   * so shouldnt fail the constraint
   *
   * this changed for l2v3
   *
  if (formulaUnits->getUnitDefinition()->getNumUnits() == 1)
  {
    pre (strcmp(UnitKind_toString(formulaUnits->getUnitDefinition()->getUnit(0)->getKind()), 
                                                                       "dimensionless")); 
  }
   */

  /* check that the formula is okay ie has no parameters with undeclared units */
  pre (formulaUnits->getContainsParametersWithUndeclaredUnits() == 0
    || (formulaUnits->getContainsParametersWithUndeclaredUnits() == 1 &&
        formulaUnits->getCanIgnoreUndeclaredUnits() == 1));

  inv (areEquivalent(formulaUnits->getUnitDefinition(), 
                          variableUnits->getUnitDefinition()) == 1);
}
END_CONSTRAINT


START_CONSTRAINT (10521, InitialAssignment, ia)
{
  msg =
    "When the 'variable' in an <initialAssignment> refers to a "
    "<compartment>, the units of the <initialAssignment>'s 'math' expression "
    "must be consistent with the units of that compartment's size. "
    "(References: L2V2 Section 4.10.)";


  const string& variable = ia.getSymbol();
  const Compartment* c = m.getCompartment(variable);

  pre ( c != NULL);
  pre ( ia.isSetMath() == 1 );

  const FormulaUnitsData * variableUnits = m.getFormulaUnitsData(variable, SBML_COMPARTMENT);
  const FormulaUnitsData * formulaUnits = 
                                    m.getFormulaUnitsData(variable, SBML_INITIAL_ASSIGNMENT);

  pre ( formulaUnits != 0 );
  pre ( variableUnits != 0); 

  /* if the formula is dimensionless then we assume that the user
   * intends it to have default units
   * so shouldnt fail the constraint
   *
   * this changed for l2v3
   *
  if (formulaUnits->getUnitDefinition()->getNumUnits() == 1)
  {
    pre (strcmp(UnitKind_toString(formulaUnits->getUnitDefinition()->getUnit(0)->getKind()), 
                                                                       "dimensionless")); 
  }
  */

  /* check that the formula is okay ie has no parameters with undeclared units */
  pre (formulaUnits->getContainsParametersWithUndeclaredUnits() == 0
    || (formulaUnits->getContainsParametersWithUndeclaredUnits() == 1 &&
        formulaUnits->getCanIgnoreUndeclaredUnits() == 1));

  inv (areEquivalent(formulaUnits->getUnitDefinition(), 
                          variableUnits->getUnitDefinition()) == 1);
}
END_CONSTRAINT


START_CONSTRAINT (10522, InitialAssignment, ia)
{
  msg =
    "When the 'variable' in an <initialAssignment> refers to a <species>, "
    "the units of the <initialAssignment>'s 'math' expression must be "
    "consistent with the units of that species' quantity. (References: L2V2 "
    "Section 4.11.3.)";
   

  const string& variable = ia.getSymbol();
  const Species * s = m.getSpecies(variable);

  pre ( s != NULL);
  pre ( ia.isSetMath() == 1 );

  const FormulaUnitsData * variableUnits = m.getFormulaUnitsData(variable, SBML_SPECIES);
  const FormulaUnitsData * formulaUnits = 
                                    m.getFormulaUnitsData(variable, SBML_INITIAL_ASSIGNMENT);

  pre ( formulaUnits != 0 );
  pre ( variableUnits != 0); 

  /* if the formula is dimensionless then we assume that the user
   * intends it to have default units
   * so shouldnt fail the constraint
   *
   * this changed for l2v3
   *
  if (formulaUnits->getUnitDefinition()->getNumUnits() == 1)
  {
    pre (strcmp(UnitKind_toString(formulaUnits->getUnitDefinition()->getUnit(0)->getKind()), 
                                                                       "dimensionless")); 
  }
  */

  /* check that the formula is okay ie has no parameters with undeclared units */
  pre (formulaUnits->getContainsParametersWithUndeclaredUnits() == 0
    || (formulaUnits->getContainsParametersWithUndeclaredUnits() == 1 &&
        formulaUnits->getCanIgnoreUndeclaredUnits() == 1));

  inv (areEquivalent(formulaUnits->getUnitDefinition(), 
                          variableUnits->getUnitDefinition()) == 1);
}
END_CONSTRAINT


START_CONSTRAINT (10523, InitialAssignment, ia)
{
  msg =
    "When the 'variable' in an <initialAssignment> refers to a <parameter>, "
    "the units of the <initialAssignment>'s 'math' expression must be "
    "consistent with the units declared for that parameter. (References: "
    "L2V2 Section 4.11.3.)";
   

  const string& variable = ia.getSymbol();
  const Parameter* p = m.getParameter(variable);

  pre ( p != NULL);
  pre ( ia.isSetMath() == 1 );
  /* check that the parameter has units declared */
  pre ( p->isSetUnits());

  const FormulaUnitsData * variableUnits = m.getFormulaUnitsData(variable, SBML_PARAMETER);
  const FormulaUnitsData * formulaUnits = 
                                    m.getFormulaUnitsData(variable, SBML_INITIAL_ASSIGNMENT);
  
  pre ( formulaUnits != 0 );
  pre ( variableUnits != 0); 

  /* if the formula is dimensionless then we assume that the user
   * intends it to have default units
   * so shouldnt fail the constraint
   *
   * this changed for l2v3
   *
  if (formulaUnits->getUnitDefinition()->getNumUnits() == 1)
  {
    pre (strcmp(UnitKind_toString(formulaUnits->getUnitDefinition()->getUnit(0)->getKind()), 
                                                                       "dimensionless")); 
  }
  */

  /* check that the formula is okay ie has no parameters with undeclared units */
  pre (formulaUnits->getContainsParametersWithUndeclaredUnits() == 0
    || (formulaUnits->getContainsParametersWithUndeclaredUnits() == 1 &&
        formulaUnits->getCanIgnoreUndeclaredUnits() == 1));

  inv (areEquivalent(formulaUnits->getUnitDefinition(), 
                          variableUnits->getUnitDefinition()) == 1);
}
END_CONSTRAINT


START_CONSTRAINT (10531, RateRule, rr)
{
  msg =
    "When the 'variable' in a <rateRule> definition refers to a "
    "<compartment>, the units of the rule's right-hand side must be of the "
    "form _x per time_, where _x_ is either the 'units' in that "
    "<compartment> definition, or (in the absence of explicit units declared "
    "for the compartment size) the default units for that compartment, and "
    "_time_ refers to the units of time for the model. (References: L2V2 "
    "Section 4.11.4.)";
   

  const string& variable = rr.getVariable();
  const Compartment* c = m.getCompartment(variable);

  pre ( c != NULL);
  pre ( rr.isSetMath() == 1 );

  const FormulaUnitsData * variableUnits = m.getFormulaUnitsData(variable, SBML_COMPARTMENT);
  const FormulaUnitsData * formulaUnits = m.getFormulaUnitsData(variable, SBML_RATE_RULE);

  pre ( formulaUnits != 0 );
  pre ( variableUnits != 0); 

  /* if the formula is dimensionless then we assume that the user
   * intends it to have default units
   * so shouldnt fail the constraint
   *
   * this changed for l2v3
   *
  if (formulaUnits->getUnitDefinition()->getNumUnits() == 1)
  {
    pre (strcmp(UnitKind_toString(formulaUnits->getUnitDefinition()->getUnit(0)->getKind()), 
                                                                       "dimensionless")); 
  }
  */

  /* check that the formula is okay ie has no parameters with undeclared units */
  pre (formulaUnits->getContainsParametersWithUndeclaredUnits() == 0
    || (formulaUnits->getContainsParametersWithUndeclaredUnits() == 1 &&
        formulaUnits->getCanIgnoreUndeclaredUnits() == 1));

  inv (areEquivalent(formulaUnits->getUnitDefinition(), 
                                           variableUnits->getPerTimeUnitDefinition()) == 1);

}
END_CONSTRAINT


START_CONSTRAINT (10532, RateRule, rr)
{
  msg =
    "When the 'variable' in a <rateRule> definition refers to a <species>, "
    "the units of the rule's right-hand side must be of the form _x per "
    "time_, where _x_ is the units of that species' quantity, and _time_ "
    "refers to the units of time for the model. (References: L2V2 Section "
    "4.11.4.)";
 
  const string& variable = rr.getVariable();
  const Species* s = m.getSpecies(variable);

  pre ( s != NULL);
  pre ( rr.isSetMath() == 1 );

  const FormulaUnitsData * variableUnits = m.getFormulaUnitsData(variable, SBML_SPECIES);
  const FormulaUnitsData * formulaUnits = m.getFormulaUnitsData(variable, SBML_RATE_RULE);

  pre ( formulaUnits != 0 );
  pre ( variableUnits != 0); 

  /* if the formula is dimensionless then we assume that the user
   * intends it to have default units
   * so shouldnt fail the constraint
   *
   * this changed for l2v3
   *
  if (formulaUnits->getUnitDefinition()->getNumUnits() == 1)
  {
    pre (strcmp(UnitKind_toString(formulaUnits->getUnitDefinition()->getUnit(0)->getKind()), 
                                                                       "dimensionless")); 
  }
  */

  /* check that the formula is okay ie has no parameters with undeclared units */
  pre (formulaUnits->getContainsParametersWithUndeclaredUnits() == 0
    || (formulaUnits->getContainsParametersWithUndeclaredUnits() == 1 &&
        formulaUnits->getCanIgnoreUndeclaredUnits() == 1));

  inv (areEquivalent(formulaUnits->getUnitDefinition(), 
                                           variableUnits->getPerTimeUnitDefinition()) == 1);
}
END_CONSTRAINT


START_CONSTRAINT (10533, RateRule, rr)
{
  msg =
    "When the 'variable' in a <rateRule> definition refers to a <parameter>, "
    "the units of the rule's right-hand side must be of the form _x per "
    "time_, where _x_ is the 'units' in that <parameter> definition, and "
    "_time_ refers to the units of time for the model. (References: L2V2 "
    "Section 4.11.4.)";
   

  const string& variable = rr.getVariable();
  const Parameter* p = m.getParameter(variable);

  pre ( p != NULL);
  pre ( rr.isSetMath() == 1 );
  /* check that the parameter has units declared */
  pre ( p->isSetUnits());

  const FormulaUnitsData * variableUnits = m.getFormulaUnitsData(variable, SBML_PARAMETER);
  const FormulaUnitsData * formulaUnits = m.getFormulaUnitsData(variable, SBML_RATE_RULE);

  pre ( formulaUnits != 0 );
  pre ( variableUnits != 0); 

  /* if the formula is dimensionless then we assume that the user
   * intends it to have default units
   * so shouldnt fail the constraint
   *
   * this changed for l2v3
   *
  if (formulaUnits->getUnitDefinition()->getNumUnits() == 1)
  {
    pre (strcmp(UnitKind_toString(formulaUnits->getUnitDefinition()->getUnit(0)->getKind()), 
                                                                       "dimensionless")); 
  }
  */

  /* check that the formula is okay ie has no parameters with undeclared units */
  pre (formulaUnits->getContainsParametersWithUndeclaredUnits() == 0
    || (formulaUnits->getContainsParametersWithUndeclaredUnits() == 1 &&
        formulaUnits->getCanIgnoreUndeclaredUnits() == 1));

  inv (areEquivalent(formulaUnits->getUnitDefinition(), 
                                           variableUnits->getPerTimeUnitDefinition()) == 1);
}
END_CONSTRAINT


START_CONSTRAINT (10541, KineticLaw, kl)
{
  msg =
    "The units of the 'math' formula in a <kineticLaw> definition must be "
    "the equivalent of _substance per time_. (References: L2V2 Section "
    "4.13.5.)";

  pre ( kl.isSetMath() == 1 );

  const FormulaUnitsData * formulaUnits = m.getFormulaUnitsData(kl.getId(), 
                                                                          SBML_KINETIC_LAW);
  const FormulaUnitsData * variableUnits = m.getFormulaUnitsData("subs_per_time", 
                                                                              SBML_UNKNOWN);

  pre ( formulaUnits != 0 );
  pre ( variableUnits != 0); 

  /* if the formula is dimensionless then we assume that the user
   * intends it to have default units
   * so shouldnt fail the constraint
   *
   * this changed for l2v3
   *
  if (formulaUnits->getUnitDefinition()->getNumUnits() == 1)
  {
    pre (strcmp(UnitKind_toString(formulaUnits->getUnitDefinition()->getUnit(0)->getKind()), 
                                                                       "dimensionless")); 
  }
  */

  /* check that the formula is okay ie has no parameters with undeclared units */
  pre (formulaUnits->getContainsParametersWithUndeclaredUnits() == 0
    || (formulaUnits->getContainsParametersWithUndeclaredUnits() == 1 &&
        formulaUnits->getCanIgnoreUndeclaredUnits() == 1));

  inv (areEquivalent(formulaUnits->getUnitDefinition(), 
                                           variableUnits->getUnitDefinition()) == 1);
}
END_CONSTRAINT


START_CONSTRAINT (10551, Event, e)
{
  msg =
    "When a value for 'delay' is given in a <event> definition, the units of "
    "the delay formula must correspond to either the value of 'timeUnits' in "
    "the <event> or (if no 'timeUnits' are given), the model's default units "
    "of time. (References: L2V2 Section 4.14.)";

  pre ( e.isSetDelay() == 1 );

  const FormulaUnitsData * formulaUnits = m.getFormulaUnitsData(e.getId(), SBML_EVENT);

  pre ( formulaUnits != 0 );

  /* if the formula is dimensionless then we assume that the user
   * intends it to have default units
   * so shouldnt fail the constraint
   *
   * this changed for l2v3
   *
  if (formulaUnits->getUnitDefinition()->getNumUnits() == 1)
  {
    pre (strcmp(UnitKind_toString(formulaUnits->getUnitDefinition()->getUnit(0)->getKind()), 
                                                                       "dimensionless")); 
  }
  */

  /* check that the formula is okay ie has no parameters with undeclared units */
  pre (formulaUnits->getContainsParametersWithUndeclaredUnits() == 0
    || (formulaUnits->getContainsParametersWithUndeclaredUnits() == 1 &&
        formulaUnits->getCanIgnoreUndeclaredUnits() == 1));

  inv (areIdentical(formulaUnits->getUnitDefinition(), 
                                        formulaUnits->getEventTimeUnitDefinition()) == 1);

}
END_CONSTRAINT


START_CONSTRAINT (10561, EventAssignment, ea)
{
  msg =
    "When the 'variable' in an <eventAssignment> refers to a <compartment>, "
    "the units of the eventAssignment's math expression must be consistent with the "
    "units of that compartment's size. (References: L2V2 Section 4.14.2.)";


  const string& variable = ea.getVariable();
  const Compartment* c = m.getCompartment(variable);

  pre ( c != NULL);
  pre ( ea.isSetMath() == 1 );

  const FormulaUnitsData * variableUnits = m.getFormulaUnitsData(variable, SBML_COMPARTMENT);
  const FormulaUnitsData * formulaUnits = m.getFormulaUnitsData(variable, SBML_EVENT_ASSIGNMENT);

  pre ( formulaUnits != 0 );
  pre ( variableUnits != 0); 

  /* if the formula is dimensionless then we assume that the user
   * intends it to have default units
   * so shouldnt fail the constraint
   *
   * this changed for l2v3
   *
  if (formulaUnits->getUnitDefinition()->getNumUnits() == 1)
  {
    pre (strcmp(UnitKind_toString(formulaUnits->getUnitDefinition()->getUnit(0)->getKind()), 
                                                                       "dimensionless")); 
  }
  */

  /* check that the formula is okay ie has no parameters with undeclared units */
  pre (formulaUnits->getContainsParametersWithUndeclaredUnits() == 0
    || (formulaUnits->getContainsParametersWithUndeclaredUnits() == 1 &&
        formulaUnits->getCanIgnoreUndeclaredUnits() == 1));

  inv (areEquivalent(formulaUnits->getUnitDefinition(), 
                          variableUnits->getUnitDefinition()) == 1);

}
END_CONSTRAINT


START_CONSTRAINT (10562, EventAssignment, ea)
{
  msg =
    "When the 'variable' in an <eventAssignment> refers to a <species>, the "
    "units of the eventAssignment's math expression must be consistent with the units "
    "of the species' quantity. (References: L2V2 Section 4.14.2.)";
   

  const string& variable = ea.getVariable();
  const Species * s = m.getSpecies(variable);

  pre ( s != NULL);
  pre ( ea.isSetMath() == 1 );

  const FormulaUnitsData * variableUnits = m.getFormulaUnitsData(variable, SBML_SPECIES);
  const FormulaUnitsData * formulaUnits = m.getFormulaUnitsData(variable, SBML_EVENT_ASSIGNMENT);

  pre ( formulaUnits != 0 );
  pre ( variableUnits != 0); 

  /* if the formula is dimensionless then we assume that the user
   * intends it to have default units
   * so shouldnt fail the constraint
   *
   * this changed for l2v3
   *
  if (formulaUnits->getUnitDefinition()->getNumUnits() == 1)
  {
    pre (strcmp(UnitKind_toString(formulaUnits->getUnitDefinition()->getUnit(0)->getKind()), 
                                                                       "dimensionless")); 
  }
  */

  /* check that the formula is okay ie has no parameters with undeclared units */
  pre (formulaUnits->getContainsParametersWithUndeclaredUnits() == 0
    || (formulaUnits->getContainsParametersWithUndeclaredUnits() == 1 &&
        formulaUnits->getCanIgnoreUndeclaredUnits() == 1));

  inv (areEquivalent(formulaUnits->getUnitDefinition(), 
                          variableUnits->getUnitDefinition()) == 1);
}
END_CONSTRAINT


START_CONSTRAINT (10563, EventAssignment, ea)
{
  msg =
    "When the 'variable' in an <eventAssignment> refers to a <parameter>, the "
    "units of the eventAssignment's math expression must be consistent with the units "
    "declared for that parameter. (References: L2V2 Section 4.11.3.)";
   

  const string& variable = ea.getVariable();
  const Parameter* p = m.getParameter(variable);

  pre ( p != NULL);
  pre ( ea.isSetMath() == 1 );
  /* check that the parameter has units declared */
  pre ( p->isSetUnits());

  const FormulaUnitsData * variableUnits = m.getFormulaUnitsData(variable, SBML_PARAMETER);
  const FormulaUnitsData * formulaUnits = m.getFormulaUnitsData(variable, SBML_EVENT_ASSIGNMENT);

  pre ( formulaUnits != 0 );
  pre ( variableUnits != 0); 

  /* if the formula is dimensionless then we assume that the user
   * intends it to have default units
   * so shouldnt fail the constraint
   *
   * this changed for l2v3
   *
  if (formulaUnits->getUnitDefinition()->getNumUnits() == 1)
  {
    pre (strcmp(UnitKind_toString(formulaUnits->getUnitDefinition()->getUnit(0)->getKind()), 
                                                                       "dimensionless")); 
  }
  */

  /* check that the formula is okay ie has no parameters with undeclared units */
  pre (formulaUnits->getContainsParametersWithUndeclaredUnits() == 0
    || (formulaUnits->getContainsParametersWithUndeclaredUnits() == 1 &&
        formulaUnits->getCanIgnoreUndeclaredUnits() == 1));

  inv (areEquivalent(formulaUnits->getUnitDefinition(), 
                          variableUnits->getUnitDefinition()) == 1);
}
END_CONSTRAINT


// General model validation

EXTERN_CONSTRAINT( 10601, OverDeterminedCheck)
