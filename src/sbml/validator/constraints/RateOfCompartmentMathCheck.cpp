/**
 * @cond doxygenLibsbmlInternal
 *
 * @file    RateOfCompartmentMathCheck.cpp
 * @brief   checks &lt;ci&gt; element is the id of a component
 * @author  Sarah Keating
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2019 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *  
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 *     Pasadena, CA, USA 
 *  
 * Copyright (C) 2002-2005 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. Japan Science and Technology Agency, Japan
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ---------------------------------------------------------------------- -->*/

#include <sbml/Model.h>
#include <sbml/Compartment.h>
#include <sbml/Species.h>
#include <sbml/Parameter.h>
#include <sbml/UnitDefinition.h>
#include <sbml/Event.h>
#include <sbml/Reaction.h>
#include <sbml/EventAssignment.h>
#include <sbml/SpeciesReference.h>
#include <sbml/Rule.h>
#include <sbml/math/FormulaFormatter.h>
#include <sbml/SBMLTypeCodes.h>

#include <sbml/units/UnitFormulaFormatter.h>

#include "RateOfCompartmentMathCheck.h"
#include "OverDeterminedCheck.h"

/** @cond doxygenIgnored */
using namespace std;
/** @endcond */

LIBSBML_CPP_NAMESPACE_BEGIN
#ifdef __cplusplus

static const char* PREAMBLE = "";

/*
 * Creates a new Constraint with the given @p id.
 */
RateOfCompartmentMathCheck::RateOfCompartmentMathCheck (unsigned int id, Validator& v) : MathMLBase(id, v)
{
}


/*
 * Destroys this Constraint.
 */
RateOfCompartmentMathCheck::~RateOfCompartmentMathCheck ()
{
}


/*
 * @return the preamble to use when logging constraint violations.
 */
const char*
RateOfCompartmentMathCheck::getPreamble ()
{
  return PREAMBLE;
}


/*
  * Checks the MathML of the ASTnode 
  * is appropriate for the function being performed
  *
  * If an inconsistency is found, an error message is logged.
  */
void
RateOfCompartmentMathCheck::checkMath (const Model& m, const ASTNode& node, const SBase & sb)
{
  /* only check for l3v2 and above */
  if (m.getLevel() != 3) return;
  if (m.getVersion() < 2) return;

  ASTNodeType_t type = node.getType();
    
  switch (type) 
  {
    case AST_FUNCTION_RATE_OF:
      checkCiElement(m, node, sb);
      break;

    default:

      checkChildren(m, node, sb);
      break;

  }
}

  
/*
  * Checks any &lt;ci&gt; elements in the MathML of the ASTnode 
  * contain the id of an appropriate component of the model
  *
  * If an inconsistency is found, an error message is logged.
  */
void 
RateOfCompartmentMathCheck::checkCiElement (const Model& m, 
                                        const ASTNode& node, 
                                        const SBase & sb)
{
  ASTNode* child = node.getChild(0);
  if (child->getType() != AST_NAME)
  {
    return;
  }

  std::string name = child->getName();

  // only need to check if the rateOf targets a species
  // which hasOnlySubstanceUnits = false
  const Species *s = m.getSpecies(name);
  if (s == NULL) return;
  if (s->getHasOnlySubstanceUnits() == true) return;

  std::string compartment = s->getCompartment();
  const AssignmentRule * ar = m.getAssignmentRuleByVariable(compartment);

  if (ar != NULL  )
  {
    logMathConflict(node, sb);
  }
  else 
  {
    if (getNumAlgebraicRules(m) > 0)
    {
      matchEquations(m);

      // check algebraic rules
      for (unsigned int i = 0; i < m.getNumRules(); i++)
      {
        if (m.getRule(i)->isAlgebraic())
        {
          ostringstream oss;
          oss << "rule_" << i;
          std::string rule = oss.str();
          if (matchExists(compartment, rule))
          {
             logAlgebraicRuleDependency(node, sb);          
          }
        }
      }

  }
  }

}


/*
 * @return the error message to use when logging constraint violations.
 * This method is called by logFailure.
 *
 * Returns a message that the given @p id and its corresponding object are
 * in  conflict with an object previously defined.
 */
const string
RateOfCompartmentMathCheck::getMessage (const ASTNode& node, const SBase& object)
{

  ostringstream oss_msg;

  //oss_msg << getPreamble();
  char * formula = SBML_formulaToString(&node);
  oss_msg << "The formula '" << formula;
  oss_msg << "' in the " << getFieldname() << " element of the <" << object.getElementName();
  oss_msg << "> ";
  switch(object.getTypeCode()) {
  case SBML_INITIAL_ASSIGNMENT:
  case SBML_EVENT_ASSIGNMENT:
  case SBML_ASSIGNMENT_RULE:
  case SBML_RATE_RULE:
    //LS DEBUG:  could use other attribute values, or 'isSetActualId'.
    break;
  default:
    if (object.isSetId()) {
      oss_msg << "with id '" << object.getId() << "' ";
    }
    break;
  }
  oss_msg << "uses the species'" << node.getChild(0)->getName();
  oss_msg << "' whose compartment is referenced as the variable in an assignmentRule.";
  safe_free(formula);

  return oss_msg.str();
}

void 
RateOfCompartmentMathCheck::logAlgebraicRuleDependency (const ASTNode & node, 
                                             const SBase & sb)
{
  char * formula = SBML_formulaToString(&node);
  msg = "The formula '"; 
  msg += formula;
  msg += "' in the ";
  msg += getFieldname();
  msg += " element of the <" + sb.getElementName();
  msg += "> ";
  switch(sb.getTypeCode()) {
  case SBML_INITIAL_ASSIGNMENT:
  case SBML_EVENT_ASSIGNMENT:
  case SBML_ASSIGNMENT_RULE:
  case SBML_RATE_RULE:
    //LS DEBUG:  could use other attribute values, or 'isSetActualId'.
    break;
  default:
    if (sb.isSetId()) {
      msg += "with id '";
      msg += sb.getId() + "' ";
    }
    break;
  }
  msg += "uses the species'";
  msg += node.getChild(0)->getName();
  msg += "' whose compartment is assigned via an algebraicRule.";
  safe_free(formula);

  logFailure(sb, msg);

}



#endif /* __cplusplus */
LIBSBML_CPP_NAMESPACE_END
/** @endcond */

