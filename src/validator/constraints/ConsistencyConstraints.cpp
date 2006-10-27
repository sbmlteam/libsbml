/**
 * \file    ConsistencyConstraints.cpp
 * \brief   Consistency check constraints.  See SBML Wiki
 * \author  Ben Bornstein
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

#include <string>

#include <sbml/SBMLTypes.h>
#include <sbml/validator/Constraint.h>
#include <sbml/units/UnitFormulaFormatter.h>

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

#include "OverDeterminedCheck.h"

#endif


#include <sbml/validator/ConstraintMacros.h>


using namespace std;

// General XML validation

// 10101: utf-8 - caught at read
// 10102: undfeined element - caught at read

//General MathML validation

// 10201: namespace - TO DO
// 10202: elements - caught at read
// 10203: encoding - caught at read
// 10204: url - caught at read
// 10205: time/delay url - caught at read
// 10206: type - caught at read
// 10207: values for type - caught at read

EXTERN_CONSTRAINT( 10208, LambdaMathCheck        )
EXTERN_CONSTRAINT( 10209, LogicalArgsMathCheck   )
EXTERN_CONSTRAINT( 10210, NumericArgsMathCheck   )
EXTERN_CONSTRAINT( 10211, EqualityArgsMathCheck  )
EXTERN_CONSTRAINT( 10212, PiecewiseValueMathCheck)
EXTERN_CONSTRAINT( 10213, PieceBooleanMathCheck  )
EXTERN_CONSTRAINT( 10214, FunctionApplyMathCheck )
EXTERN_CONSTRAINT( 10215, CiElementMathCheck     )


// 10216: id of local parameter not visibe - TO DO

EXTERN_CONSTRAINT( 10217, NumericReturnMathCheck )


// General Identifier validation 
EXTERN_CONSTRAINT( 10301, UniqueIdsInModel             )
EXTERN_CONSTRAINT( 10302, UniqueIdsForUnitDefinitions  )
EXTERN_CONSTRAINT( 10303, UniqueIdsInKineticLaw        )
EXTERN_CONSTRAINT( 10304, UniqueVarsInRules            )
EXTERN_CONSTRAINT( 10305, UniqueVarsInEventAssignments )
EXTERN_CONSTRAINT( 10306, UniqueVarsInEventsAndRules   )
EXTERN_CONSTRAINT( 10307, UniqueMetaId                 )

// 10308: SBO term - caught at read
// 10309: syntax of metid - TO DO
// 10310: sntax of id - TO DO


// General annotation validation

// 10401: namespace -  TO DO
// 10402: single namespace - TO DO
// 10403: not sbml namespace - TO DO

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

  // get the unitDefinition from the compartment and  
  // that returned by the math formula 
  //
  UnitDefinition * variableUnits = new UnitDefinition();
  UnitDefinition * formulaUnits = new UnitDefinition();

  UnitFormulaFormatter *unitFormat = new UnitFormulaFormatter(&m);

  formulaUnits  = unitFormat->getUnitDefinition(ar.getMath());
  variableUnits = unitFormat->getUnitDefinitionFromCompartment(c);
  
  pre (unitFormat->hasUndeclaredUnits(ar.getMath()) == 0
    || unitFormat->getCanIgnoreUndeclaredUnits() == 1);

  inv (areEquivalent(formulaUnits, variableUnits) == 1)

  delete unitFormat;
  delete variableUnits;
  delete formulaUnits;
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

  // get the unitDefinition from the species and  
  // that returned by the math formula 
  //
  UnitDefinition * variableUnits = new UnitDefinition();
  UnitDefinition * formulaUnits = new UnitDefinition();

  UnitFormulaFormatter *unitFormat = new UnitFormulaFormatter(&m);

  formulaUnits  = unitFormat->getUnitDefinition(ar.getMath());
  variableUnits = unitFormat->getUnitDefinitionFromSpecies(s);
  
  pre (unitFormat->hasUndeclaredUnits(ar.getMath()) == 0
    || unitFormat->getCanIgnoreUndeclaredUnits() == 1);

  inv (areEquivalent(formulaUnits, variableUnits) == 1)

  delete unitFormat;
  delete variableUnits;
  delete formulaUnits;
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

  // get the unitDefinition from the parameter and  
  // that returned by the math formula 
  //
  UnitDefinition * variableUnits = new UnitDefinition();
  UnitDefinition * formulaUnits = new UnitDefinition();

  UnitFormulaFormatter *unitFormat = new UnitFormulaFormatter(&m);

  formulaUnits  = unitFormat->getUnitDefinition(ar.getMath());
  variableUnits = unitFormat->getUnitDefinitionFromParameter(p);

  // special case where no units have been declared for parameter
  pre (variableUnits->getNumUnits() != 0);

  inv (areEquivalent(formulaUnits, variableUnits) == 1)

  delete unitFormat;
  delete variableUnits;
  delete formulaUnits;
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

  // get the unitDefinition from the compartment and  
  // that returned by the math formula 
  //
  UnitDefinition * variableUnits = new UnitDefinition();
  UnitDefinition * formulaUnits = new UnitDefinition();

  UnitFormulaFormatter *unitFormat = new UnitFormulaFormatter(&m);

  formulaUnits  = unitFormat->getUnitDefinition(ia.getMath());
  variableUnits = unitFormat->getUnitDefinitionFromCompartment(c);
  
  pre (unitFormat->hasUndeclaredUnits(ia.getMath()) == 0
    || unitFormat->getCanIgnoreUndeclaredUnits() == 1);

  inv (areEquivalent(formulaUnits, variableUnits) == 1)

  delete unitFormat;
  delete variableUnits;
  delete formulaUnits;
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

  // get the unitDefinition from the species and  
  // that returned by the math formula 
  //
  UnitDefinition * variableUnits = new UnitDefinition();
  UnitDefinition * formulaUnits = new UnitDefinition();

  UnitFormulaFormatter *unitFormat = new UnitFormulaFormatter(&m);

  formulaUnits  = unitFormat->getUnitDefinition(ia.getMath());
  variableUnits = unitFormat->getUnitDefinitionFromSpecies(s);
  
  pre (unitFormat->hasUndeclaredUnits(ia.getMath()) == 0
    || unitFormat->getCanIgnoreUndeclaredUnits() == 1);

  inv (areEquivalent(formulaUnits, variableUnits) == 1)

  delete unitFormat;
  delete variableUnits;
  delete formulaUnits;
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

  // get the unitDefinition from the parameter and  
  // that returned by the math formula 
  //
  UnitDefinition * variableUnits = new UnitDefinition();
  UnitDefinition * formulaUnits = new UnitDefinition();

  UnitFormulaFormatter *unitFormat = new UnitFormulaFormatter(&m);

  formulaUnits  = unitFormat->getUnitDefinition(ia.getMath());
  variableUnits = unitFormat->getUnitDefinitionFromParameter(p);

  // special case where no units have been declared for parameter
  pre (variableUnits->getNumUnits() != 0);

  inv (areEquivalent(formulaUnits, variableUnits) == 1)

  delete unitFormat;
  delete variableUnits;
  delete formulaUnits;
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

  // get the unitDefinition from the compartment and  
  // that returned by the math formula 
  //
  UnitDefinition * variableUnits = new UnitDefinition();
  UnitDefinition * formulaUnits = new UnitDefinition();
  Unit * time = new Unit("second", -1);

  UnitFormulaFormatter *unitFormat = new UnitFormulaFormatter(&m);

  formulaUnits  = unitFormat->getUnitDefinition(rr.getMath());
  variableUnits = unitFormat->getUnitDefinitionFromCompartment(c);
  
  pre (unitFormat->hasUndeclaredUnits(rr.getMath()) == 0
    || unitFormat->getCanIgnoreUndeclaredUnits() == 1);

  // add per time to the units from the compartment
  variableUnits->addUnit(time);

  inv (areEquivalent(formulaUnits, variableUnits) == 1)

  delete unitFormat;
  delete variableUnits;
  delete formulaUnits;
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

  // get the unitDefinition from the species and  
  // that returned by the math formula 
  //
  UnitDefinition * variableUnits = new UnitDefinition();
  UnitDefinition * formulaUnits = new UnitDefinition();
  Unit * time = new Unit("second", -1);

  UnitFormulaFormatter *unitFormat = new UnitFormulaFormatter(&m);

  formulaUnits  = unitFormat->getUnitDefinition(rr.getMath());
  variableUnits = unitFormat->getUnitDefinitionFromSpecies(s);
  
  pre (unitFormat->hasUndeclaredUnits(rr.getMath()) == 0
    || unitFormat->getCanIgnoreUndeclaredUnits() == 1);


  // add per time to the units from the species
  variableUnits->addUnit(time);

  inv (areEquivalent(formulaUnits, variableUnits) == 1)
  
  delete unitFormat;
  delete variableUnits;
  delete formulaUnits;
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

  // get the unitDefinition from the parameter and  
  // that returned by the math formula 
  //
  UnitDefinition * variableUnits = new UnitDefinition();
  UnitDefinition * formulaUnits = new UnitDefinition();
  Unit * time = new Unit("second", -1);

  UnitFormulaFormatter *unitFormat = new UnitFormulaFormatter(&m);

  formulaUnits  = unitFormat->getUnitDefinition(rr.getMath());
  variableUnits = unitFormat->getUnitDefinitionFromParameter(p);

  // special case where no units have been declared for parameter
  pre (variableUnits->getNumUnits() != 0);
  
  // add per time to the units from the parameter
  variableUnits->addUnit(time);

  inv (areEquivalent(formulaUnits, variableUnits) == 1)
  
  delete unitFormat;
  delete variableUnits;
  delete formulaUnits;
}
END_CONSTRAINT


START_CONSTRAINT (10541, KineticLaw, kl)
{
  msg =
    "The units of the 'math' formula in a <kineticLaw> definition must be "
    "the equivalent of _substance per time_. (References: L2V2 Section "
    "4.13.5.)";

  pre ( kl.isSetMath() == 1 );

  UnitFormulaFormatter *unitFormat = new UnitFormulaFormatter(&m);

  pre (unitFormat->hasUndeclaredUnits(kl.getMath()) == 0
    || unitFormat->getCanIgnoreUndeclaredUnits() == 1);

  UnitDefinition * formulaUnits = new UnitDefinition();
  
  UnitDefinition * SubsTime = new UnitDefinition();
  Unit * subs = new Unit("mole");
  Unit * time = new Unit("second", -1);
  SubsTime->addUnit(subs);
  SubsTime->addUnit(time);


  formulaUnits  = unitFormat->getUnitDefinition(kl.getMath());

  inv (areEquivalent(formulaUnits, SubsTime) == 1)

  delete unitFormat;
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

  UnitFormulaFormatter *unitFormat = new UnitFormulaFormatter(&m);

  pre (unitFormat->hasUndeclaredUnits(e.getDelay()) == 0
    || unitFormat->getCanIgnoreUndeclaredUnits() == 1);
  
  UnitDefinition * formulaUnits = new UnitDefinition();
  formulaUnits  = unitFormat->getUnitDefinition(e.getDelay());

  UnitDefinition * timeUnits = 
    unitFormat->getUnitDefinitionFromEventTime(&e);

    inv (areIdentical(formulaUnits, timeUnits) == 1)

  delete unitFormat;
}
END_CONSTRAINT


// General model validation

EXTERN_CONSTRAINT( 10601, OverDeterminedCheck)


// Model validation

// 20201: must constain model - caught prioir to validation

// 20202: ordering - caught at read

// 20203: no empty listOf - TO DO

START_CONSTRAINT (20204, Model, x)
{
  msg =
    "If a model defines any <species>, then the model must also define at "
    "least one <compartment>. This is an implication of the fact that the "
    "'compartment' field on <species> is not optional. (References: L2V1 "
    "Section 4.5; Section 4.8.3.)";

  pre( m.getNumSpecies()      > 0 );
  inv( m.getNumCompartments() > 0 );
}
END_CONSTRAINT


// FunctionDefinition validation

START_CONSTRAINT (20301, FunctionDefinition, fd)
{
  msg =
    "The top-level element within 'math' in a <functionDefinition> must be "
    "'lambda'. (References: L2V1 Section 4.3.2; L2V2 Section 4.3.2.)";

  pre( fd.isSetMath()           );
  inv( fd.getMath()->isLambda() );
}
END_CONSTRAINT


EXTERN_CONSTRAINT(20302, FunctionReferredToExists)


START_CONSTRAINT (20303, FunctionDefinition, fd)
{
  msg =
    "Inside the 'lambda' of a <functionDefinition>, the identifier of that "
    "<functionDefinition> cannot appear as the value of a 'ci' element. SBML "
    "functions are not permitted to be recursive. (References: L2V2 Section "
    "4.3.2.)";

  pre( fd.isSetMath()            );
  pre( fd.getBody() != NULL      );
  pre( fd.getNumArguments() != 0 );
  
  const string  id = fd.getId();

  List* variables = fd.getBody()->getListOfNodes( ASTNode_isFunction );
  for (unsigned int n = 0; n < variables->getSize(); ++n)
  {
    ASTNode* node = static_cast<ASTNode*>( variables->get(n) );
    const char *   name = node->getName() ? node->getName() : "";

    inv(strcmp(name, id.c_str()));
 }

}
END_CONSTRAINT


EXTERN_CONSTRAINT(20304, FunctionDefinitionVars)


START_CONSTRAINT (20305, FunctionDefinition, fd)
{
  msg =
    "The value type returned by a <functionDefinition>'s 'lambda' must be "
    "either boolean or numeric. (References: L2V2 Section 3.5.8.)";

  pre( fd.isSetMath()           );
  pre( fd.getBody() != NULL      );

  inv_or( fd.getBody()->isBoolean() );
  inv_or( fd.getBody()->isNumber()  );
  inv_or( fd.getBody()->isFunction());
  inv_or( fd.getBody()->isOperator());
}
END_CONSTRAINT


// Unit and UnitDefinition validation

// NOTE: This constraint also applies to L1 Models (replacing name with id).
START_CONSTRAINT (20401, UnitDefinition, ud)
{
  msg =
    "The value of the 'id' field in a <unitDefinition> must not be identical "
    "to any unit predefined in SBML. That is, the identifier must not be the "
    "same as a value from the 'UnitKind' enumeration (i.e., 'ampere' 'gram' "
    "'katal' 'metre' 'second' 'watt' 'becquerel' 'gray' 'kelvin' 'mole' "
    "'siemens' 'weber' 'candela' 'henry' 'kilogram' 'newton' 'sievert' "
    "'coulomb' 'hertz' 'litre' 'ohm' 'steradian' 'dimensionless' 'item' "
    "'lumen' 'pascal' 'tesla' 'farad' 'joule' 'lux' 'radian' 'volt'.) "
    "(References: L2V1 erratum 14; L2V2 Section 4.4.2.)";
    

  inv( Unit::isUnitKind( ud.getId() ) == false );
}
END_CONSTRAINT


// NOTE: This constraint also applies to L1 Models.
START_CONSTRAINT (20402, UnitDefinition, ud)
{
  msg =
    "Redefinitions of the built-in unit 'substance' must be based on the "
    "units 'mole', 'item', 'gram', 'kilogram', or 'dimensionless'. More "
    "formally, a <unitDefinition> for 'substance' must simplify to a single "
    "<unit> whose 'kind' field has a value of 'mole', 'item', 'gram', "
    "'kilogram', or 'dimensionless', and whose 'exponent' field has a value "
    "of '1'. (References: L2V1 Section 4.4.3; L2V2 Section 4.4.3.)";

  pre( ud.getId() == "substance" );

  inv( ud.getNumUnits() == 1                              );
  inv( ud.getUnit(0)->getExponent() == 1                  );

    /* dimensionless/gram/kilogram are allowable in L2V2 */
  if (  ud.getLevel() == 2 
    &&  ud.getVersion() == 2)
  {
    inv_or ( ud.getUnit(0)->isMole());
    inv_or ( ud.getUnit(0)->isItem() );
    inv_or ( ud.getUnit(0)->isGram() );
    inv_or ( ud.getUnit(0)->isKilogram() );
    inv_or ( ud.getUnit(0)->isDimensionless());
  }
  else
  {
    inv( ud.getUnit(0)->isMole() || ud.getUnit(0)->isItem() );
  }

}
END_CONSTRAINT


// NOTE: This constraint also applies to L1 Models.
START_CONSTRAINT (20403, UnitDefinition, ud)
{
  msg =
    "Redefinitions of the built-in unit 'length' must be based on the unit "
    "'metre' or 'dimensionless'. More formally, a <unitDefinition> for "
    "'length' must simplify to a single <unit> in which either (a) the "
    "'kind' field has a value of 'metre' and the 'exponent' field has a "
    "value of '1', or (b) the 'kind' field has a value of 'dimensionless' "
    "with any 'exponent' value. (References: L2V1 Section 4.4.3; L2V2 "
    "Section 4.4.3.)";

  pre( ud.getId() == "length" );

  inv( ud.getNumUnits() == 1             );

  /* dimensionless is allowable in L2V2 */
  if (  ud.getLevel() == 2 
    &&  ud.getVersion() == 2
    &&  !ud.getUnit(0)->isMetre())
  {
    inv(ud.getUnit(0)->isDimensionless());
  }
  else
  {
    inv( ud.getUnit(0)->isMetre()          );
    inv( ud.getUnit(0)->getExponent() == 1 );
  }
}
END_CONSTRAINT


// NOTE: This constraint also applies to L1 Models.
START_CONSTRAINT (20404, UnitDefinition, ud)
{
  msg =
    "Redefinitions of the built-in unit 'area' must be based on squared "
    "'metre's or 'dimensionless'. More formally, a <unitDefinition> for "
    "'area' must simplify to a single <unit> in which either (a) the 'kind' "
    "field has a value of 'metre' and the 'exponent' field has a value of "
    "'2', or (b) the 'kind' field has a value of 'dimensionless' with any "
    "'exponent' value. (References: L2V1 Section 4.4.3; L2V2 Section 4.4.3.)";

  pre( ud.getId() == "area" );

  inv( ud.getNumUnits() == 1             );

  /* dimensionless is allowable in L2V2 */
  if (  ud.getLevel() == 2 
    &&  ud.getVersion() == 2
    &&  !ud.getUnit(0)->isMetre())
  {
    inv(ud.getUnit(0)->isDimensionless());
  }
  else
  {
    inv( ud.getUnit(0)->isMetre()          );
    inv( ud.getUnit(0)->getExponent() == 2 );
  }
}
END_CONSTRAINT


// NOTE: This constraint also applies to L1 Models.
START_CONSTRAINT (20405, UnitDefinition, ud)
{
  msg =
    "Redefinitions of the built-in unit 'time' must be based on 'second'. "
    "More formally, a <unitDefinition> for 'time' must simplify to a single "
    "<unit> in which either (a) the 'kind' field has a value of 'second' and "
    "the 'exponent' field has a value of '1', or (b) the 'kind' field has a "
    "value of 'dimensionless' with any 'exponent' value. (References: L2V1 "
    "Section 4.4.3; L2V2 Section 4.4.3.)";

  pre( ud.getId() == "time" );

  inv( ud.getNumUnits() == 1             );

  /* dimensionless is allowable in L2V2 */
  if (  ud.getLevel() == 2 
    &&  ud.getVersion() == 2
    &&  !ud.getUnit(0)->isSecond())
  {
    inv(ud.getUnit(0)->isDimensionless());
  }
  else
  {
    inv( ud.getUnit(0)->isSecond()         );
    inv( ud.getUnit(0)->getExponent() == 1 );
  }
}
END_CONSTRAINT


// NOTE: This constraint also applies to L1 Models.
START_CONSTRAINT (20406, UnitDefinition, ud)
{
  msg =
    "Redefinitions of the built-in unit 'volume' must be based on 'litre', "
    "'metre' or 'dimensionless'. More formally, a <unitDefinition> for "
    "'volume' must simplify to a single <unit> in which the 'kind' field "
    "value is either 'litre', 'metre', or 'dimensionless'. Additional "
    "constraints apply if the kind is 'litre' or 'metre'. (References: L2V1 "
    "Section 4.4.3; L2V2 Section 4.4.3.)";

  pre( ud.getId() == "volume" );

  inv( ud.getNumUnits() == 1 );
  
  /* dimensionless is allowable in L2V2 */
  if (  ud.getLevel() == 2 
    &&  ud.getVersion() == 2)
  {
    inv( ud.getUnit(0)->isLitre() 
      || ud.getUnit(0)->isMetre() 
      || ud.getUnit(0)->isDimensionless() );
  }
  else
  {
    inv( ud.getUnit(0)->isLitre() || ud.getUnit(0)->isMetre() );
  }
}
END_CONSTRAINT


// NOTE: This constraint also applies to L1 Models.
START_CONSTRAINT (20407, UnitDefinition, ud)
{
  msg =
    "If a <unitDefinition> for 'volume' simplifies to a <unit> in which the "
    "'kind' field value is 'litre', then its 'exponent' field value must be "
    "'1'. (References: L2V1 Section 4.4.3; L2V2 Section 4.4.3.)";

  pre( ud.getId()       == "volume" );
  pre( ud.getNumUnits() == 1        );
  pre( ud.getUnit(0)->isLitre()     );

  inv( ud.getUnit(0)->getExponent() == 1 );
}
END_CONSTRAINT


// NOTE: This constraint also applies to L1 Models.
START_CONSTRAINT (20408, UnitDefinition, ud)
{
  msg =
    "If a <unitDefinition> for 'volume' simplifies to a <unit> in which the "
    "'kind' field value is 'metre', then its 'exponent' field value must be "
    "'3'. (References: L2V1 Section 4.4.3; L2V2 Section 4.4.3.)";

  pre( ud.getId()       == "volume" );
  pre( ud.getNumUnits() == 1        );
  pre( ud.getUnit(0)->isMetre()     );

  inv( ud.getUnit(0)->getExponent() == 3 );
}
END_CONSTRAINT


// 20409: ListofUnits cannot be empty - TO DO


START_CONSTRAINT (20410, UnitDefinition, ud)
{
  msg =
    "The value of the 'kind' field of a <unit> can only be one of the "
    "predefined units enumerated by 'UnitKind'; that is, the SBML unit "
    "system is not hierarchical and user-defined units cannot be defined "
    "using other user-defined units. (References: L2V2 Section 4.4.2.)";

  for (unsigned int n = 0; n < ud.getNumUnits(); ++n)
  {
    inv(Unit::isUnitKind(UnitKind_toString(ud.getUnit(n)->getKind())));
  }
}
END_CONSTRAINT


START_CONSTRAINT (20411, UnitDefinition, ud)
{
  msg =
    "The 'offset' field on <unit> previously available in SBML Level 2 "
    "Version 1, has been removed as of SBML Level 2 Version 2. (References: "
    "L2V2 Section 4.4.)";

  pre( ud.getLevel() == 2 && ud.getVersion() == 2 );

  for (unsigned int n = 0; n < ud.getNumUnits(); ++n)
  {
    inv(ud.getUnit(n)->getOffset() == 0);
  }
}
END_CONSTRAINT


START_CONSTRAINT (20412, Unit, u)
{
  msg =
    "The predefined unit 'Celsius', previously available in SBML Level 1 and "
    "Level 2 Version 1, has been removed as of SBML Level 2 Version 2. "
    "(References: L2V2 Section 4.4.)";

  pre( u.getLevel() == 2 && u.getVersion() == 2 );
  inv( u.isCelsius() == false );
}
END_CONSTRAINT

/*
// TO DO
// NEED TO CATCH CELSIUS ON A PARAMETER
START_CONSTRAINT (20412, Parameter, p)
{
  msg =
    "The predefined unit 'Celsius', previously available in SBML Level 1 and "
    "Level 2 Version 1, has been removed as of SBML Level 2 Version 2. "
    "(References: L2V2 Section 4.4.)";

  pre( p.getLevel() == 2 && p.getVersion() == 2 && p.isSetUnits()    );
  inv( UnitKind_forName( p.getUnits().c_str() ) != UNIT_KIND_CELSIUS );
}
END_CONSTRAINT
*/

// Compartment validation

START_CONSTRAINT (20501, Compartment, c)
{
  msg =
    "The size of a <compartment> must not be set if the compartment's "
    "'spatialDimensions' field has value '0'. (References: L2V1 Section "
    "4.5.3; L2V2 Section 4.7.5.)";

  pre( c.getSpatialDimensions() == 0 );
  inv( c.isSetSize() == false );
}
END_CONSTRAINT


START_CONSTRAINT (20502, Compartment, c)
{
  msg =
    "If a <compartment> definition has a 'spatialDimensions' value of '0', "
    "then its 'units' field must not be set. If the compartment has no "
    "dimensions, then no units can be associated with a non-existent size. "
    "(References: L2V1 Section 4.5.4; Section 4.7.5.)";

  pre( c.getSpatialDimensions() == 0 );
  
  inv( c.isSetUnits() == false       );
}
END_CONSTRAINT


START_CONSTRAINT (20503, Compartment, c)
{
  msg =
    "If a <compartment> definition has a 'spatialDimensions' value of '0', "
    "then its 'constant' field value must either default to or be set to "
    "'true'. If the compartment has no dimensions, then its size can never "
    "change. (References: L2V1 Section 4.5.5; L2V2 Section 4.7.6.)";

  pre( c.getSpatialDimensions() == 0 );
  inv( c.getConstant() == true       );
}
END_CONSTRAINT


// NOTE: This constraint also applies to L1 Models.
START_CONSTRAINT (20504, Compartment, c)
{
  msg =
    "The 'outside' field value of a <compartment> must be the identifier of "
    "another <compartment> defined in the model. (References: L2V1 Section "
    "4.5.6; Section 4.7.7.)";

  pre( c.isSetOutside() );
  inv( m.getCompartment( c.getOutside() ) != NULL );
}
END_CONSTRAINT


// NOTE: This constraint also applies to L1 Models.
EXTERN_CONSTRAINT(20505, CompartmentOutsideCycles)


START_CONSTRAINT (20506, Compartment, c)
{
  msg =
    "The 'outside' field value of a <compartment> cannot be a compartment "
    "whose 'spatialDimensions' value is '0', unless both compartments have "
    "'spatialDimensions'='0'. Simply put, a zero-dimensional compartment "
    "cannot enclose compartments that have anything other than zero "
    "dimensions themselves. (References: L2V2 Section 4.7.7.)";

  pre( c.isSetOutside() && c.getSpatialDimensions() == 0 );

  inv( m.getCompartment( c.getOutside() )->getSpatialDimensions() == 0 );
}
END_CONSTRAINT


START_CONSTRAINT (20507, Compartment, c)
{
  msg =
    "The value of the 'units' field on a <compartment> having "
    "'spatialDimensions' of '1' must be either 'length', 'metre', "
    "'dimensionless', or the identifier of a <unitDefinition> based on "
    "either 'metre' (with 'exponent' equal to '1') or 'dimensionless'. "
    "(References: L2V1 Section 4.5.4; L2V2 Section 4.7.5.)";

  pre( c.getSpatialDimensions() == 1 );
  pre( c.isSetUnits()                );

  const string&         units = c.getUnits();
  const UnitDefinition* defn  = m.getUnitDefinition(units);

  /* dimensionless is allowable in L2V2 */
  if (  c.getLevel() == 2 
    &&  c.getVersion() == 2)
  {
    inv_or( units == "length" );
    inv_or( units == "metre"  );
    inv_or( units == "dimensionless"  );
    inv_or( defn  != NULL && defn->isVariantOfLength() );
    inv_or( defn  != NULL && defn->isVariantOfDimensionless() );
  }
  else
  {
    inv_or( units == "length" );
    inv_or( units == "metre"  );
    inv_or( defn  != NULL && defn->isVariantOfLength() );
  }
}
END_CONSTRAINT


START_CONSTRAINT (20508, Compartment, c)
{
  msg =
    "The value of the 'units' field on a <compartment> having "
    "'spatialDimensions' of '2' must be either 'area', 'dimensionless', or "
    "the identifier of a <unitDefinition> based on either 'metre' (with "
    "'exponent' equal to '2') or 'dimensionless'. (References: L2V1 Section "
    "4.5.4; L2V2 Section 4.7.5.)";

  pre( c.getSpatialDimensions() == 2 );
  pre( c.isSetUnits()                );

  const string&         units = c.getUnits();
  const UnitDefinition* defn  = m.getUnitDefinition(units);

  /* dimensionless is allowable in L2V2 */
  if (  c.getLevel() == 2 
    &&  c.getVersion() == 2)
  {
    inv_or( units == "area" );
    inv_or( units == "dimensionless"  );
    inv_or( defn  != NULL && defn->isVariantOfArea() );
    inv_or( defn  != NULL && defn->isVariantOfDimensionless() );
  }
  else
  {
    inv_or( units == "area" );
    inv_or( defn  != NULL && defn->isVariantOfArea() );
  }
}
END_CONSTRAINT


START_CONSTRAINT (20509, Compartment, c)
{
  msg =
    "The value of the 'units' field on a <compartment> having "
    "'spatialDimensions' of '3' must be either 'volume', 'litre', or the "
    "identifier of a <unitDefinition> based on either 'litre', 'metre' (with "
    "'exponent' equal to '3'), or 'dimensionless'. (References: L2V1 Section "
    "4.5.4; L2V2 Section 4.7.5.)";

  pre( c.getSpatialDimensions() == 3 );
  pre( c.isSetUnits()                );

  const string&         units = c.getUnits();
  const UnitDefinition* defn  = m.getUnitDefinition(units);

  /* dimensionless is allowable in L2V2 */
  if (  c.getLevel() == 2 
    &&  c.getVersion() == 2)
  {
    inv_or( units == "volume" );
    inv_or( units == "litre"  );
    inv_or( units == "dimensionless"  );
    inv_or( defn  != NULL && defn->isVariantOfVolume() );
    inv_or( defn  != NULL && defn->isVariantOfDimensionless() );
  }
  else
  {
    inv_or( units == "volume" );
    inv_or( units == "litre"  );
    inv_or( defn  != NULL && defn->isVariantOfVolume() );
  }
}
END_CONSTRAINT


START_CONSTRAINT (20510, Compartment, c)
{
  msg =
    "CompartmentType '" + c.getCompartmentType() + "' is undefined. "
    "If the 'compartmentType' field is given a value in a <compartment> "
    "definition, it must contain the identifier of an existing "
    "<compartmentType>. (References: L2V2 Section 4.7.2.)";

  pre( c.getLevel() == 2 && c.getVersion() == 2 );
  pre( c.isSetCompartmentType());

  inv( m.getCompartmentType( c.getCompartmentType() ) != NULL );
}
END_CONSTRAINT

// Species validation

// NOTE: This constraint also applies to L1 Models.
START_CONSTRAINT (20601, Species, s)
{
  msg =
    "Compartment '" + s.getCompartment() + "' is undefined. "
    "The value of 'compartment' in a <species> definition must be the "
    "identifier of an existing <compartment> defined in the model. "
    "(References: L2V1 Section 4.6.2; Section 4.8.3.)";

  pre( s.isSetCompartment() );
  inv( m.getCompartment( s.getCompartment() ) != NULL );
}
END_CONSTRAINT


START_CONSTRAINT (20602, Species, s)
{
  msg =
    "If a <species> definition sets 'hasOnlySubstanceUnits' to 'true', then "
    "it must not have a value for 'spatialSizeUnits'. (References: L2V1 "
    "Section 4.6.4; L2V2 Section 4.8.5.)";

  pre( s.getHasOnlySubstanceUnits() == true );
  inv( !s.isSetSpatialSizeUnits()           );
}
END_CONSTRAINT


START_CONSTRAINT (20603, Species, s)
{
  msg =
    "A <species> definition must not set 'spatialSizeUnits' if the "
    "<compartment> in which it is located has a 'spatialDimensions' value of "
    "'0'. (References: L2V1 Section 4.6.4; L2V2 Section 4.8.5.)";


  const Compartment* c = m.getCompartment( s.getCompartment() );

  pre( c != NULL && c->getSpatialDimensions() == 0 );
  inv( !s.isSetSpatialSizeUnits()                  );
}
END_CONSTRAINT


START_CONSTRAINT (20604, Species, s)
{
  msg =
    "If a <species> located in a <compartment> whose 'spatialDimensions' is "
    "set to '0', then that <species> definition cannot set "
    "'initialConcentration'. (References: L2V1 Section 4.6.3; L2V2 Section "
    "4.8.4.)";


  const Compartment* c = m.getCompartment( s.getCompartment() );

  pre( c != NULL && c->getSpatialDimensions() == 0 );
  inv( !s.isSetInitialConcentration()              );
}
END_CONSTRAINT


START_CONSTRAINT (20605, Species, s)
{
  msg =
    "If a <species> is located in a <compartment> whose 'spatialDimensions' "
    "has value '1', then that <species> definition can only set "
    "'spatialSizeUnits' to a value of 'length', 'metre', 'dimensionless', or "
    "the identifier of a <unitDefinition> derived from 'metre' (with an "
    "'exponent' value of '1') or 'dimensionless'. (References: L2V1 Section "
    "4.6.4; L2V2 Section 4.8.5.)";


  const Compartment* c = m.getCompartment( s.getCompartment() );

  pre( c != NULL && c->getSpatialDimensions() == 1 );
  pre( s.isSetSpatialSizeUnits() );

  const string&         units = s.getSpatialSizeUnits();
  const UnitDefinition* defn  = m.getUnitDefinition(units);

  /* dimensionless is allowable in L2V2 */
  if (s.getLevel() == 2 &&  s.getVersion() == 2)
  {
    inv_or( units == "length" );
    inv_or( units == "metre"  );
    inv_or( units == "dimensionless"  );
    inv_or( defn  != NULL && defn->isVariantOfLength() );
    inv_or( defn  != NULL && defn->isVariantOfDimensionless() );
  }
  else
  {
    inv_or( units == "length" );
    inv_or( units == "metre"  );
    inv_or( defn  != NULL && defn->isVariantOfLength() );
  }
}
END_CONSTRAINT


START_CONSTRAINT (20606, Species, s)
{
  msg =
    "If a <species> is located in a <compartment> whose 'spatialDimensions' "
    "has value '2', then that <species> definition can only set "
    "'spatialSizeUnits' to a value of 'area', 'dimensionless', or the "
    "identifier of a <unitDefinition> derived from either 'metre' (with an "
    "'exponent' value of '2') or 'dimensionless'. (References: L2V1 Section "
    "4.6.4; L2V2 Section 4.8.5.)";


  const Compartment* c = m.getCompartment( s.getCompartment() );

  pre( c != NULL && c->getSpatialDimensions() == 2 );
  pre( s.isSetSpatialSizeUnits() );

  const string&         units = s.getSpatialSizeUnits();
  const UnitDefinition* defn  = m.getUnitDefinition(units);

  /* dimensionless is allowable in L2V2 */
  if (s.getLevel() == 2 &&  s.getVersion() == 2)
  {
    inv_or( units == "area" );
    inv_or( units == "dimensionless"  );
    inv_or( defn  != NULL && defn->isVariantOfArea() );
    inv_or( defn  != NULL && defn->isVariantOfDimensionless() );
  }
  else
  {
    inv_or( units == "area" );
    inv_or( defn  != NULL && defn->isVariantOfArea() );
  }
}
END_CONSTRAINT


START_CONSTRAINT (20607, Species, s)
{
  msg =
    "If a <species> is located in a <compartment> whose 'spatialDimensions' "
    "has value '3', then that <species> definition can only set "
    "'spatialSizeUnits' to a value of 'volume', 'litre', 'dimensionless', or "
    "the identifier of a <unitDefinition> derived from either 'litre', "
    "'metre' (with an 'exponent' value of '3') or 'dimensionless'. "
    "(References: L2V1 Section 4.6.4; L2V2 Section 4.8.5.)";


  const Compartment* c = m.getCompartment( s.getCompartment() );

  pre( c != NULL && c->getSpatialDimensions() == 3 );
  pre( s.isSetSpatialSizeUnits() );

  const string&         units = s.getSpatialSizeUnits();
  const UnitDefinition* defn  = m.getUnitDefinition(units);

  /* dimensionless is allowable in L2V2 */
  if (s.getLevel() == 2 &&  s.getVersion() == 2)
  {
    inv_or( units == "volume" );
    inv_or( units == "litre"  );
    inv_or( units == "dimensionless"  );
    inv_or( defn  != NULL && defn->isVariantOfVolume() );
    inv_or( defn  != NULL && defn->isVariantOfDimensionless() );
  }
  else
  {
    inv_or( units == "volume" );
    inv_or( units == "litre"  );
    inv_or( defn  != NULL && defn->isVariantOfVolume() );
  }
}
END_CONSTRAINT


START_CONSTRAINT (20608, Species, s)
{
  msg =
    "The value of a <species>'s 'substanceUnits' field can only be one of "
    "the following: 'substance', 'mole', 'item', 'gram', 'kilogram', "
    "'dimensionless', or the identifier of a <unitDefinition> derived from "
    "'mole' (with an 'exponent' of '1'), 'item' (with an 'exponent' of '1'), "
    "'gram' (with an 'exponent' of '1'), 'kilogram' (with an 'exponent' of "
    "'1'), or 'dimensionless'. (References: L2V1 Section 4.6.4; L2V2 Section "
    "4.8.5.)";


  pre( s.isSetSubstanceUnits() );

  const string&         units = s.getSubstanceUnits();
  const UnitDefinition* defn  = m.getUnitDefinition(units);

  /* dimensionless/gram/kilogram are allowable in L2V2 */
  if (s.getLevel() == 2 &&  s.getVersion() == 2)
  {
    inv_or( units == "substance"      );
    inv_or( units == "item"           );
    inv_or( units == "mole"           );
    inv_or( units == "dimensionless"  );
    inv_or( units == "gram"           );
    inv_or( units == "kilogram"       );
    inv_or( defn  != NULL && defn->isVariantOfSubstance()     );
    inv_or( defn  != NULL && defn->isVariantOfDimensionless() );
    inv_or( defn  != NULL && defn->isVariantOfMass()          );
  }
  else
  {
    inv_or( units == "substance" );
    inv_or( units == "item"      );
    inv_or( units == "mole"      );
    inv_or( defn  != NULL && defn->isVariantOfSubstance() );
  }
}
END_CONSTRAINT


START_CONSTRAINT (20609, Species, s)
{
  msg =
    "A <species> cannot set values for both 'initialConcentration' and "
    "'initialAmount' because they are mutually exclusive. (References: L2V1 "
    "Section 4.6.3; L2V2 Section 4.8.4.)";

  pre(  s.isSetInitialAmount()        );
  inv( !s.isSetInitialConcentration() );
}
END_CONSTRAINT


EXTERN_CONSTRAINT(20610, SpeciesReactionOrRule)


// 20611 missing as it is a repeat of 21112


START_CONSTRAINT (20612, Species, s)
{
  msg =
    "SpeciesType '" + s.getSpeciesType() + "' is undefined. "
    "The value of 'speciesType' in a <species> definition must be the "
    "identifier of an existing <speciesType>. (References: L2V2 Section "
    "4.8.2.)";

  pre( s.getLevel() == 2 && s.getVersion() == 2 );
  pre( s.isSetSpeciesType() );

  inv( m.getSpeciesType( s.getSpeciesType() ) != NULL );
}
END_CONSTRAINT


EXTERN_CONSTRAINT(20613, UniqueSpeciesTypesInCompartment)

// Parameter validation

// NOTE: This constraint also applies to L1 Models.
START_CONSTRAINT (20701, Parameter, p)
{
  msg =
    "The 'units' in a <parameter> definition must be a value chosen from "
    "among the following: a value from the 'UnitKind' enumeration (e.g., "
    "'litre', 'mole', 'metre', etc.), a built-in unit (e.g., 'substance', "
    "'time', etc.), or the identifier of a <unitDefinition> in the model. "
    "(References: L2V1 Section 4.7.3; L2V2 Section 4.9.3.)";


  pre( p.isSetUnits() );

  const string& units = p.getUnits();

  inv_or( Unit::isUnitKind(units)    );
  inv_or( Unit::isBuiltIn(units)     );
  inv_or( m.getUnitDefinition(units) );
}
END_CONSTRAINT


// InitialAssignment validation

START_CONSTRAINT (20801, InitialAssignment, ia)
{
  msg =
    "The value of 'symbol' in an <initialAssignment> definition must be the "
    "identifier of an existing <compartment>, <species>, or <parameter> "
    "defined in the model. (References: L2V2 Section 4.10.)";


  pre( ia.isSetSymbol() );

  const string& id = ia.getSymbol();

  inv_or( m.getCompartment(id) );
  inv_or( m.getSpecies    (id) );
  inv_or( m.getParameter  (id) );
}
END_CONSTRAINT


EXTERN_CONSTRAINT(20802, UniqueSymbolsInInitialAssignments)
EXTERN_CONSTRAINT(20803, UniqueVarsInInitialAssignmentsAndRules)


// Assignment and rate Rule validation

START_CONSTRAINT (20901, AssignmentRule, r)
{
  msg =
    "The value of an <assignmentRule>'s 'variable' must be the identifier of "
    "an existing <compartment>, <species>, or globally-defined <parameter>. "
    "(References: L2V1 Section 4.8.2; L2V2 Section 4.11.3.)";


  pre( r.isSetVariable() );

  const string& id = r.getVariable();

  inv_or( m.getCompartment(id) );
  inv_or( m.getSpecies    (id) );
  inv_or( m.getParameter  (id) );
}
END_CONSTRAINT


START_CONSTRAINT (20902, RateRule, r)
{
  msg =
    "The value of a <rateRule>'s 'variable' must be the identifier of an "
    "existing <compartment>, <species>, or globally-defined <parameter>. "
    "(References: L2V1 Section 4.8.3; L2V2 Section 4.11.4.)";


  pre( r.isSetVariable() );

  const string& id = r.getVariable();

  inv_or( m.getCompartment(id) );
  inv_or( m.getSpecies    (id) );
  inv_or( m.getParameter  (id) );
}
END_CONSTRAINT


START_CONSTRAINT (20903, AssignmentRule, r)
{
  msg =
    "Any <compartment>, <species> or <parameter> whose identifier is the "
    "value of a 'variable' field in an <assignmentRule>, must have a value "
    "of 'false' for 'constant'. (References: L2V1 Section 4.8.4; L2V2 "
    "Section 4.11.3.)";


  pre( r.isSetVariable() );
  pre( r.getLevel() == 2);

  const string& id = r.getVariable();

  const Compartment* c = m.getCompartment(id);
  const Species*     s = m.getSpecies    (id);
  const Parameter*   p = m.getParameter  (id);

  pre( c || s || p );

  inv_or( c && c->getConstant() == false );
  inv_or( s && s->getConstant() == false );
  inv_or( p && p->getConstant() == false );
}
END_CONSTRAINT


START_CONSTRAINT (20904, RateRule, r)
{
  msg =
    "Any <compartment>, <species> or <parameter> whose identifier is the "
    "value of a 'variable' field in an <rateRule>, must have a value of "
    "'false' for 'constant'. (References: L2V1 Section 4.8.4; L2V2 Section "
    "4.11.4.)";


  pre( r.isSetVariable() );

  const string& id = r.getVariable();

  const Compartment* c = m.getCompartment(id);
  const Species*     s = m.getSpecies    (id);
  const Parameter*   p = m.getParameter  (id);

  pre( c || s || p );

  inv_or( c && c->getConstant() == false );
  inv_or( s && s->getConstant() == false );
  inv_or( p && p->getConstant() == false );
}
END_CONSTRAINT


// 20905 is missing as it is a repeat of 10304
EXTERN_CONSTRAINT(20906, AssignmentCycles)


// Constraint validation

START_CONSTRAINT (21001, Constraint, c)
{
  msg =
    "A <constraint> 'math' expression must evaluate to a value of type "
    "'boolean'. (References: L2V2 Section 4.12.)";

  pre( c.isSetMath() );
  inv( m.isBoolean( c.getMath() ) );
}
END_CONSTRAINT


// 21002: ordering - caught at read


//Reaction validation

// NOTE: This constraint also applies to L1 Models.
START_CONSTRAINT (21101, Reaction, r)
{
  msg =
    "A <reaction> definition must contain at least one <speciesReference>, "
    "either in its 'listOfReactants' or its 'listOfProducts'. A reaction "
    "without any reactant or product species is not permitted, regardless of "
    "whether the reaction has any modifier species. (References: L2V2 "
    "Section 4.13.3.)";

  inv( r.getNumReactants() > 0 || r.getNumProducts() > 0 );
}
END_CONSTRAINT


// 21102: ordering - caught at read
// 21103: non empty listOf - TO DO
// 21104: non empty listOf - TO DO
// 21105: non empty listOf - TO DO


// SpeciesReference Validation

START_CONSTRAINT (21111, SpeciesReference, sr)
{
  msg =
    "Species '" + sr.getSpecies() + "' is undefined. "
    "The value of a <speciesReference> 'species' field must be the "
    "identifier of an existing <species> in the model. (References: L2V1 "
    "Section 4.9.5; L2V2 Section 4.13.3.)";

  inv( m.getSpecies( sr.getSpecies() ) != NULL );
}
END_CONSTRAINT


START_CONSTRAINT (21112, SpeciesReference, sr)
{
  msg =
    "The value of a <speciesReference>'s 'species' field must not be the "
    "identifier of a <species> having both a 'constant' field value of "
    "'true' and a 'boundaryCondition' field value of 'false'. (References: "
    "L2V1 Section 4.6.5; L2V2 Section 4.8.6.)";

  /* doesnt apply if the SpeciesReference is a modifier */
  pre(!sr.isModifier());

  const Species* s = m.getSpecies( sr.getSpecies() );

  pre( s != NULL );
  inv( ! (s->getConstant() == true && s->getBoundaryCondition() == false) ); 
}
END_CONSTRAINT


START_CONSTRAINT (21113, SpeciesReference, sr)
{
  msg =
    "A <speciesReference> must not have a value for both 'stoichiometry' and "
    "'stoichiometryMath'; they are mutually exclusive. (References: L2V1 "
    "Section 4.9.5; L2V2 Section 4.13.3.)";

  /* doesnt apply if the SpeciesReference is a modifier */
  pre(!sr.isModifier());

  pre( sr.isSetStoichiometryMath()  );
  inv( sr.getStoichiometry() == 1.0 );
}
END_CONSTRAINT


// KineticLaw validation

EXTERN_CONSTRAINT(21121, KineticLawVars)


// 21122: ordering - caught at read
// 21123: non empty list - TO DO


START_CONSTRAINT (21124, KineticLaw, kl)
{
  msg =
    "The 'constant' field on a <parameter> local to a <kineticLaw> cannot "
    "have a value other than 'true'. The values of parameters local to "
    "<kineticLaw> definitions cannot be changed, and therefore they are "
    "always constant. (References: L2V2 Section 4.13.5.)";

  pre(kl.getNumParameters() != 0);

  for (unsigned int n = 0; n < kl.getNumParameters(); ++n)
  {
    inv(kl.getParameter(n)->getConstant() == true);
  }
}
END_CONSTRAINT


// StoichiometryMath validation

EXTERN_CONSTRAINT(21131, StoichiometryMathVars)


// Event validation

START_CONSTRAINT (21201, Event, e)
{
  msg =
    "An <event> object must have a 'trigger'. (References: L2V1 Section "
    "4.10.2; L2V2 Section 4.14.)";

  inv( e.isSetTrigger() != 0 );
}
END_CONSTRAINT


START_CONSTRAINT (21202, Event, e)
{
  msg =
    "An <event> 'trigger' expression must evaluate to a value of type "
    "'boolean'. (References: L2V1 Section 4.10.2; L2V2 Section 4.14.)";

  pre( e.isSetTrigger() );
  inv( m.isBoolean( e.getTrigger() ) );
}
END_CONSTRAINT


START_CONSTRAINT (21203, Event, e)
{
  msg =
    "An <event> object must have at least one <eventAssignment> object in "
    "its 'listOfEventAssignments'. (References: L2V1 Section 4.10.5; L2V2 "
    "Section 4.14.)";

  inv( e.getNumEventAssignments() != 0 );
}
END_CONSTRAINT


START_CONSTRAINT (21204, Event, e)
{
  msg =
    "The value of an <event>'s 'timeUnits' must be 'time', 'second', "
    "'dimensionless', or the identifier of a <unitDefinition> derived from "
    "either 'second' (with an 'exponent' value of '1') or 'dimensionless'. "
    "(References: L2V1 Section 4.10.4; L2V2 Section 4.14.)";

  pre( e.isSetTimeUnits() );

  const string&         units = e.getTimeUnits();
  const UnitDefinition* defn  = m.getUnitDefinition(units);

  /* dimensionless is allowable in L2V2 */
  if (  e.getLevel() == 2 
    &&  e.getVersion() == 2)
  {
    inv_or( units == "time" );
    inv_or( units == "second"  );
    inv_or( units == "dimensionless"  );
    inv_or( defn  != NULL && defn->isVariantOfTime() );
    inv_or( defn  != NULL && defn->isVariantOfDimensionless() );
  }
  else
  {
    inv_or( units == "time"   );
    inv_or( units == "second" );
    inv_or( defn  != NULL && defn->isVariantOfTime() );
  }
}
END_CONSTRAINT


// 21205: ordering - caught at read


//EventAssignment validation

START_CONSTRAINT (21211, EventAssignment, ea)
{
  msg = 
    "The value of 'variable' in an <eventAssignment> can only be the "
    "identifier of a <compartment>, <species>, or model-wide <parameter> "
    "definition. (References: L2V1 Section 4.10.5; L2V2 Section 4.14.)";


  pre( ea.isSetVariable() );

  const string& id = ea.getVariable();

  inv_or( m.getCompartment(id) );
  inv_or( m.getSpecies    (id) );
  inv_or( m.getParameter  (id) );
}
END_CONSTRAINT


START_CONSTRAINT (21212, EventAssignment, ea)
{
  msg =
    "Any <compartment>, <species> or <parameter> definition whose identifier "
    "is used as the value of 'variable' in an <eventAssignment> must have a "
    "value of 'false' for its 'constant' field. (References: L2V1 Section "
    "4.10.5; L2V2 Section 4.14.)";


  pre( ea.isSetVariable() );

  const string& id = ea.getVariable();

  const Compartment* c = m.getCompartment(id);
  const Species*     s = m.getSpecies    (id);
  const Parameter*   p = m.getParameter  (id);

  pre( c || s || p );

  inv_or( c && c->getConstant() == false );
  inv_or( s && s->getConstant() == false );
  inv_or( p && p->getConstant() == false );
}
END_CONSTRAINT


// NEED TO SORT OUT NUMBERS
START_CONSTRAINT (1604, KineticLaw, kl)
{
  msg =
    "A KineticLaw's substanceUnits must be 'substance', 'item', 'mole', or "
    "the id of a UnitDefinition that defines a variant of 'item' or 'mole' "
    "(L2v1 Section 4.9.7).";


  pre( kl.isSetSubstanceUnits() );

  const string&         units = kl.getSubstanceUnits();
  const UnitDefinition* defn  = m.getUnitDefinition(units);

  /* dimensionless is allowable in L2V2 */
  if (  kl.getLevel() == 2 
    &&  kl.getVersion() == 2)
  {
 /*   removed in l2v2 - need to think about
 
    inv_or( units == "substance" );
    inv_or( units == "item"  );
    inv_or( units == "mole"      );
    inv_or( units == "dimensionless"  );
    inv_or( defn  != NULL && defn->isVariantOfSubstance() );
    inv_or( defn  != NULL && defn->isVariantOfDimensionless() );
 */
  }
  else
  {
    inv_or( units == "substance" );
    inv_or( units == "item"      );
    inv_or( units == "mole"      );
    inv_or( defn  != NULL && defn->isVariantOfSubstance() );
  }
}
END_CONSTRAINT


START_CONSTRAINT (1605, KineticLaw, kl)
{
  msg =
    "A KineticLaw's timeUnits must be 'time', 'second', or the id of a "
    "UnitDefnition that defines a variant of 'second' with exponent='1' "
    "(L2v1 Section 4.9.7).";


  pre( kl.isSetTimeUnits() );

  const string&         units = kl.getTimeUnits();
  const UnitDefinition* defn  = m.getUnitDefinition(units);

  /* dimensionless is allowable in L2V2 */
  if (  kl.getLevel() == 2 
    &&  kl.getVersion() == 2)
  {
 /*   removed in l2v2 - need to think about
 
    inv_or( units == "time" );
    inv_or( units == "second"  );
    inv_or( units == "dimensionless"  );
    inv_or( defn  != NULL && defn->isVariantOfTime() );
    inv_or( defn  != NULL && defn->isVariantOfDimensionless() );
*/
  }
  else
  {
    inv_or( units == "time"   );
    inv_or( units == "second" );
    inv_or( defn  != NULL && defn->isVariantOfTime() );
  }
}
END_CONSTRAINT

START_CONSTRAINT (1611, KineticLaw, kl)
{
  msg =
    "In a Level 1 model only predefined functions are permitted "
     "within the KineticLaw formula. (L1V2 Appendix C)";

  pre (m.getLevel() == 1);

  pre (kl.isSetFormula() == 1);

  FormulaTokenizer_t * ft = FormulaTokenizer_create (kl.getFormula().c_str());
  Token_t * t = FormulaTokenizer_nextToken (ft);

  const Compartment * c;
  const Species * s;
  const Parameter * p, * p1;

  /* loop through each token of the formula
   * if it has type TT_NAME then it is either the id of some component
   * of the model or the name of a function in which case 
   * need to check whether it is defined
   */
  while (t->type != TT_END)
  {
    if (t->type == TT_NAME)
    {
      c = m.getCompartment(t->value.name);
      s = m.getSpecies    (t->value.name);
      p = m.getParameter  (t->value.name);
      p1 = kl.getParameter(t->value.name);

      if (!c && !s && !p && !p1)
      {
        inv_or (strcmp(t->value.name, "abs") == 0);
        inv_or (strcmp(t->value.name, "acos") == 0);
        inv_or (strcmp(t->value.name, "asin") == 0);
        inv_or (strcmp(t->value.name, "atan") == 0);
        inv_or (strcmp(t->value.name, "ceil") == 0);
        inv_or (strcmp(t->value.name, "cos") == 0);
        inv_or (strcmp(t->value.name, "exp") == 0);
        inv_or (strcmp(t->value.name, "floor") == 0);
        inv_or (strcmp(t->value.name, "log") == 0);
        inv_or (strcmp(t->value.name, "log10") == 0);
        inv_or (strcmp(t->value.name, "pow") == 0);
        inv_or (strcmp(t->value.name, "sqr") == 0);
        inv_or (strcmp(t->value.name, "sqrt") == 0);
        inv_or (strcmp(t->value.name, "sin") == 0);
        inv_or (strcmp(t->value.name, "tan") == 0);
      }
    }

    t = FormulaTokenizer_nextToken(ft);
  }
}
END_CONSTRAINT




