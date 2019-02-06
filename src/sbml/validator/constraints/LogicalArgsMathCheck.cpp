/**
 * @cond doxygenLibsbmlInternal
 *
 * @file    LogicalArgsMathCheck.cpp
 * @brief   Ensures arguments to logical operators are consistent.
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

#include <sbml/units/UnitFormulaFormatter.h>

#include "LogicalArgsMathCheck.h"

/** @cond doxygenIgnored */
using namespace std;
/** @endcond */

LIBSBML_CPP_NAMESPACE_BEGIN

static const char* PREAMBLE =
    "The arguments of the MathML logical operators 'and', 'or', 'xor', and "
    "'not' must have Boolean values. (References: L2V2 Section 3.5.8.)";


/*
 * Creates a new Constraint with the given @p id.
 */
LogicalArgsMathCheck::LogicalArgsMathCheck (unsigned int id, Validator& v) : MathMLBase(id, v)
{
}


/*
 * Destroys this Constraint.
 */
LogicalArgsMathCheck::~LogicalArgsMathCheck ()
{
}


/*
 * @return the preamble to use when logging constraint violations.
 */
const char*
LogicalArgsMathCheck::getPreamble ()
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
LogicalArgsMathCheck::checkMath (const Model& m, const ASTNode& node, const SBase & sb)
{
  // does not apply in L3V2 for general consistency checking
  // BUT we want to use it for telling a converter that this occurs in L3V2
  if (this->mValidator.getCategory() == LIBSBML_CAT_MATHML_CONSISTENCY)
  {
    if (m.getLevel() == 3 && m.getVersion() > 1) return;
  }
  else
  {
    if (m.getLevel() != 3) return;
    else if (m.getVersion() == 1) return;
  }

  ASTNodeType_t type = node.getType();

  switch (type) 
  {
    case AST_LOGICAL_AND:
    case AST_LOGICAL_NOT:
    case AST_LOGICAL_OR:
    case AST_LOGICAL_XOR:

      checkMathFromLogical(m, node, sb);
      break;


    case AST_FUNCTION:

      checkFunction(m, node, sb);
      break;

    default:

      checkChildren(m, node, sb);
      break;

  }
}

  
/*
  * Checks that the arguments to logical operators are all boolean
  *
  * If not, an error message is logged.
  */
void 
LogicalArgsMathCheck::checkMathFromLogical (const Model&m, const ASTNode& node, 
                                                const SBase & sb)
{
  unsigned int n;
  
  for (n = 0; n < node.getNumChildren(); n++)
  {
    if (node.getChild(n)->isUserFunction())
    {
      checkMath(m, *(node.getChild(n)), sb);
    }
    else if (!node.getChild(n)->isBoolean())
    {
      logMathConflict(node, sb);
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
LogicalArgsMathCheck::getMessage (const ASTNode& node, const SBase& object)
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
  oss_msg << "uses an argument to a logical operator that is not Boolean.";
  safe_free(formula);

  return oss_msg.str();
}

LIBSBML_CPP_NAMESPACE_END
/** @endcond */
