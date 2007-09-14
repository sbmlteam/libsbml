/**
 * @file    SBOConsistencyConstraints.cpp
 * @brief   SBOConsistency check constraints.  See SBML Wiki
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

/** @endcond doxygen-ignored */



// General SBO validation

START_CONSTRAINT(10701, Model, m1)
{
  pre (m1.getLevel() == 2 && m1.getVersion() > 1);
  pre(m1.isSetSBOTerm());

  //msg = 
  //  "The value of the 'sboTerm' attribute on a <model> must be an SBO "
  //  "identifier (http://www.biomodels.net/SBO/) referring to a modeling "
  //  "framework defined in SBO (i.e., terms derived from SBO:0000004, "
  //  "\"modeling framework\"). "
  //  "(References: L2V2 Section 4.2.1; L2V3 Section 4.2.2.)";


  inv(SBO::isModellingFramework(m1.getSBOTerm()));
}
END_CONSTRAINT


START_CONSTRAINT(10702, FunctionDefinition, fd)
{
  pre (fd.getLevel() == 2 && fd.getVersion() > 1);
  pre(fd.isSetSBOTerm());

  //msg = 
  //  "The value of the 'sboTerm' attribute on a <functionDefinition> must be "
  //  "an SBO identifier (http://www.biomodels.net/SBO/) referring to a "
  //  "mathematical expression (i.e., terms derived from SBO:0000064, "
  //  "\"mathematical expression\"). "
  //  "(References: L2V2 Section 4.3.3; L2V3 Section 4.3.3.)";

  inv(SBO::isMathematicalExpression(fd.getSBOTerm()));
}
END_CONSTRAINT


START_CONSTRAINT(10703, Parameter, p)
{
  pre (p.getLevel() == 2 && p.getVersion() > 1);
  pre(p.isSetSBOTerm());

  //msg = 
  //  "The value of the 'sboTerm' attribute on a <parameter> must be an "
  //  "SBO identifier (http://www.biomodels.net/SBO/) referring to a "
  //  "quantitative parameter defined in SBO (i.e., terms derived from "
  //  "SBO:0000002, \"quantitative parameter\"). "
  //  "(References: L2V2 Section 4.9.5; L2V3 Section 4.9.5.)";

  inv(SBO::isQuantitativeParameter(p.getSBOTerm()));
}
END_CONSTRAINT


START_CONSTRAINT(10704, InitialAssignment, ia)
{
  pre (ia.getLevel() == 2 && ia.getVersion() > 1);
  pre(ia.isSetSBOTerm());

  //msg = 
  //  "The value of the 'sboTerm' attribute on an <initialAssignment> must "
  //  "be an SBO identifier (http://www.biomodels.net/SBO/) referring to a "
  //  "mathematical expression (i.e., terms derived from SBO:0000064, "
  //  "\"mathematical expression\"). "
  //  "(References: L2V2 Section 4.10.3; L2V3 Section 4.10.3.)";

  inv(SBO::isMathematicalExpression(ia.getSBOTerm()));
}
END_CONSTRAINT


START_CONSTRAINT(10705, AssignmentRule, r)
{
  pre (r.getLevel() == 2 && r.getVersion() > 1);
  pre(r.isSetSBOTerm());

  //msg = 
  //  "The value of the 'sboTerm' attribute on a rule must be an SBO identifier "
  //  "(http://www.biomodels.net/SBO/) referring to a mathematical expression "
  //  "(i.e., terms derived from SBO:0000064, \"mathematical expression\"). "
  //  "Note: This applies to Algebraic Rules in addition to Rate and Assignment "
  //  "Rules. (References: L2V2 Section 4.11.1; L2V3 Section 4.11.1.)";

  inv(SBO::isMathematicalExpression(r.getSBOTerm()));
}
END_CONSTRAINT


START_CONSTRAINT(10705, RateRule, r)
{
  pre (r.getLevel() == 2 && r.getVersion() > 1);
  pre(r.isSetSBOTerm());

  //msg = 
    //"The value of the 'sboTerm' attribute on a rule must be an SBO identifier "
    //"(http://www.biomodels.net/SBO/) referring to a mathematical expression "
    //"(i.e., terms derived from SBO:0000064, \"mathematical expression\"). Note: "
    //"This applies to Algebraic Rules in addition to Rate and Assignment Rules. "
    //"(References: L2V2 Section 4.11.1; L2V3 Section 4.11.1.)";

  inv(SBO::isMathematicalExpression(r.getSBOTerm()));
}
END_CONSTRAINT


START_CONSTRAINT(10705, AlgebraicRule, r)
{
  pre (r.getLevel() == 2 && r.getVersion() > 1);
  pre(r.isSetSBOTerm());

  //msg = 
  //  "The value of the 'sboTerm' attribute on a rule must be an SBO identifier "
  //  "(http://www.biomodels.net/SBO/) referring to a mathematical expression "
  //  "(i.e., terms derived from SBO:0000064, \"mathematical expression\"). Note: "
  //  "This applies to Algebraic Rules in addition to Rate and Assignment Rules. "
  //  "(References: L2V2 Section 4.11.1; L2V3 Section 4.11.1.)";

  inv(SBO::isMathematicalExpression(r.getSBOTerm()));
}
END_CONSTRAINT


START_CONSTRAINT(10706, Constraint, c)
{
  pre (c.getLevel() == 2 && c.getVersion() > 1);
  pre(c.isSetSBOTerm());

  //msg = 
  //  "The value of the 'sboTerm' attribute on a <constraint> must be an SBO "
  //  "identifier (http://www.biomodels.net/SBO/) referring to a mathematical "
  //  "expression (i.e., terms derived from SBO:0000064, \"mathematical "
  //  "expression\"). "
  //  "(References: L2V2 Section 4.12.3; L2V3 Section 4.12.3.)";

  inv(SBO::isMathematicalExpression(c.getSBOTerm()));
}
END_CONSTRAINT


START_CONSTRAINT(10707, Reaction, r)
{
  pre (r.getLevel() == 2 && r.getVersion() > 1);
  pre(r.isSetSBOTerm());

  //msg = 
  //  "The value of the 'sboTerm' attribute on a <reaction> must be an SBO "
  //  "identifier (http://www.biomodels.net/SBO/) referring to an event defined "
  //  "in SBO (i.e., terms derived from SBO:0000231, \"event\"). "
  //  "(References: L2V2 Section 4.13.1; L2V3 Section 4.13.1)";

  inv(SBO::isEvent(r.getSBOTerm()));
}
END_CONSTRAINT


START_CONSTRAINT(10708, SpeciesReference, sr)
{
   pre (sr.getLevel() == 2 && sr.getVersion() > 1);
   pre(sr.isSetSBOTerm());

   //msg = 
   //  "The value of the 'sboTerm' attribute on a <speciesReference> "
   //  "or <modifierSpeciesReference> must be an SBO "
   //  "identifier (http://www.biomodels.net/SBO/) referring to a participant "
   //  "role. The appropriate term depends on whether the object is a reactant, "
   //  "product or modifier. If a reactant, then it should be a term in the "
   //  "SBO:0000010, \"reactant\" hierarchy; if a product, then it should be a "
   //  "term in the SBO:0000011, \"product\" hierarchy; and if a modifier, then "
   //  "it should be a term in the SBO:0000019, \"modifier\" hierarchy. "
   //  "(References: L2V2 Section 4.13.2; L2V3 Sections 4.13.1 and 5.)";

  if (!sr.isModifier())
  {
    inv_or(SBO::isProduct(sr.getSBOTerm()));
    inv_or(SBO::isReactant(sr.getSBOTerm()));
  }
  else
  {
    inv(SBO::isModifier(sr.getSBOTerm()));
  }
}
END_CONSTRAINT


START_CONSTRAINT(10709, KineticLaw, kl)
{
  pre (kl.getLevel() == 2 && kl.getVersion() > 1);
  pre(kl.isSetSBOTerm());

  //msg = 
  //  "The value of the 'sboTerm' attribute on a <kineticLaw> must be an SBO "
  //  "identifier (http://www.biomodels.net/SBO/) referring rate law defined "
  //  "in SBO (i.e., terms derived from SBO:0000001, \"rate law\"). "
  //  "(References: L2V2 Section 4.13.5.)";

  inv(SBO::isRateLaw(kl.getSBOTerm()));
}
END_CONSTRAINT


START_CONSTRAINT(10710, Event, e)
{
  pre (e.getLevel() == 2 && e.getVersion() > 1);
  pre(e.isSetSBOTerm());

  //msg = 
  //  "The value of the 'sboTerm' attribute on an <event> must be an SBO "
  //  "identifier (http://www.biomodels.net/SBO/) referring to an event "
  //  "defined in SBO (i.e., terms derived from SBO:0000231, \"event\"). "
  //  "(References: L2V2 Section 4.14.1; L2V3 Section 4.14.1.)";
 
  inv(SBO::isEvent(e.getSBOTerm()));
}
END_CONSTRAINT


START_CONSTRAINT(10711, EventAssignment, ea)
{
  pre (ea.getLevel() == 2 && ea.getVersion() > 1);
  pre(ea.isSetSBOTerm());

  //msg = 
  //  "The value of the 'sboTerm' attribute on an <eventAssignment> must be an "
  //  "SBO identifier (http://www.biomodels.net/SBO/) referring to a "
  //  "mathematical expression (i.e., terms derived from SBO:0000064, "
  //  "\"mathematical expression\"). "
  //  "(References: L2V2 Section 4.14.2; L2V3 Section 4.14.1.)";

  inv(SBO::isMathematicalExpression(ea.getSBOTerm()));
}
END_CONSTRAINT


START_CONSTRAINT(10712, Compartment, c)
{
  pre (c.getLevel() == 2 && c.getVersion() == 3);
  pre(c.isSetSBOTerm());

  //msg = 
  //  "The value of the 'sboTerm' attribute on a <compartment> must be an SBO "
  //  "identifier (http://www.biomodels.net/SBO/) referring to a participant "
  //  "physical type (i.e., terms derived from SBO:0000236, \"participant "
  //  "physical type\"). (References: L2V3 Section 5.2.2.)";

  inv(SBO::isPhysicalParticipant(c.getSBOTerm()));
}
END_CONSTRAINT


START_CONSTRAINT(10713, Species, s)
{
  pre (s.getLevel() == 2 && s.getVersion() == 3);
  pre(s.isSetSBOTerm());

  //msg = 
  //  "The value of the 'sboTerm' attribute on a <species> must be an SBO "
  //  "identifier (http://www.biomodels.net/SBO/) referring to a participant "
  //  "physical type (i.e., terms derived from SBO:0000236, \"participant "
  //  "physical type\"). (References: L2V3 Section 5.2.2.)";

  inv(SBO::isPhysicalParticipant(s.getSBOTerm()));
}
END_CONSTRAINT


START_CONSTRAINT(10714, CompartmentType, c)
{
  pre (c.getLevel() == 2 && c.getVersion() == 3);
  pre(c.isSetSBOTerm());

  //msg = 
  //  "The value of the 'sboTerm' attribute on a <compartmentType> must be an SBO "
  //  "identifier (http://www.biomodels.net/SBO/) referring to a participant "
  //  "physical type (i.e., terms derived from SBO:0000236, \"participant "
  //  "physical type\"). (References: L2V3 Section 5.2.2.)";

  inv(SBO::isPhysicalParticipant(c.getSBOTerm()));
}
END_CONSTRAINT


START_CONSTRAINT(10715, SpeciesType, s)
{
  pre (s.getLevel() == 2 && s.getVersion() == 3);
  pre(s.isSetSBOTerm());

  //msg = 
  //  "The value of the 'sboTerm' attribute on a <speciesType> must be an SBO "
  //  "identifier (http://www.biomodels.net/SBO/) referring to a participant "
  //  "physical type (i.e., terms derived from SBO:0000236, \"participant "
  //  "physical type\"). (References: L2V3 Section 5.2.2.)";

  inv(SBO::isPhysicalParticipant(s.getSBOTerm()));
}
END_CONSTRAINT


START_CONSTRAINT(10716, Trigger, t)
{
  pre (t.getLevel() == 2 && t.getVersion() == 3);
  pre(t.isSetSBOTerm());

  //msg = 
  //  "The value of the 'sboTerm' attribute on a <trigger> must be an SBO "
  //  "identifier (http://www.biomodels.net/SBO/) referring to a mathematical "
  //  "expression (i.e., terms derived from SBO:0000064, \"mathematical "
  //  "expression\"). (References: L2V3 Section 5.2.2.)";

  inv(SBO::isMathematicalExpression(t.getSBOTerm()));
}
END_CONSTRAINT


START_CONSTRAINT(10717, Delay, d)
{
  pre (d.getLevel() == 2 && d.getVersion() == 3);
  pre(d.isSetSBOTerm());

  //msg = 
  //  "The value of the 'sboTerm' attribute on a <delay> must be an SBO "
  //  "identifier (http://www.biomodels.net/SBO/) referring to a mathematical "
  //  "expression (i.e., terms derived from SBO:0000064, \"mathematical "
  //  "expression\"). (References: L2V3 Section 5.2.2.)";

  inv(SBO::isMathematicalExpression(d.getSBOTerm()));
}
END_CONSTRAINT

