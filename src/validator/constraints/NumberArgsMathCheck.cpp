/**
 * @file    NumberArgsMathCheck.cpp
 * @brief   Ensures number of arguments to functions are appropriate.
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

#include "NumberArgsMathCheck.h"

static const char* PREAMBLE =
    "A MathML operator must be supplied the number of arguments "
    "appropriate for that operator. (References: SBML L2v3 Section 3.4.1.)";

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */


/**
 * Creates a new Constraint with the given id.
 */
NumberArgsMathCheck::NumberArgsMathCheck (unsigned int id, Validator& v) : MathMLBase(id, v)
{
}


/**
 * Destroys this Constraint.
 */
NumberArgsMathCheck::~NumberArgsMathCheck ()
{
}


/**
 * @return the preamble to use when logging constraint violations.
 */
const char*
NumberArgsMathCheck::getPreamble ()
{
  return PREAMBLE;
}


/**
  * Checks the MathML of the ASTnode 
  * is appropriate for the function being performed
  *
  * If an inconsistency is found, an error message is logged.
  */
void
NumberArgsMathCheck::checkMath (const Model& m, const ASTNode& node, const SBase & sb)
{
  ASTNodeType_t type = node.getType();

  switch (type) 
  {
  case AST_FUNCTION_ABS:
  case AST_FUNCTION_ARCCOS:
  case AST_FUNCTION_ARCCOSH:
  case AST_FUNCTION_ARCCOT:
  case AST_FUNCTION_ARCCOTH:
  case AST_FUNCTION_ARCCSC:
  case AST_FUNCTION_ARCCSCH:
  case AST_FUNCTION_ARCSEC:
  case AST_FUNCTION_ARCSECH:
  case AST_FUNCTION_ARCSIN:
  case AST_FUNCTION_ARCSINH:
  case AST_FUNCTION_ARCTAN:
  case AST_FUNCTION_ARCTANH:
  case AST_FUNCTION_CEILING:
  case AST_FUNCTION_COS:
  case AST_FUNCTION_COSH:
  case AST_FUNCTION_COT:
  case AST_FUNCTION_COTH:
  case AST_FUNCTION_CSC:
  case AST_FUNCTION_CSCH:
  case AST_FUNCTION_EXP:
  case AST_FUNCTION_FACTORIAL:
  case AST_FUNCTION_FLOOR:
  case AST_FUNCTION_LN:
  case AST_FUNCTION_SEC:
  case AST_FUNCTION_SECH:
  case AST_FUNCTION_SIN:
  case AST_FUNCTION_SINH:
  case AST_FUNCTION_TAN:
  case AST_FUNCTION_TANH:
  case AST_LOGICAL_NOT:

    checkUnary(m, node, sb);
      break;

    case AST_DIVIDE:
    case AST_POWER:
    case AST_RELATIONAL_NEQ:
    case AST_FUNCTION_DELAY:
    case AST_FUNCTION_POWER:
    case AST_FUNCTION_LOG:       // a log ASTNode has a child for base

    checkBinary(m, node, sb);
      break;

    case AST_TIMES:
    case AST_PLUS:
    case AST_LOGICAL_AND:
    case AST_LOGICAL_OR:
    case AST_LOGICAL_XOR:
    case AST_RELATIONAL_EQ:
    case AST_RELATIONAL_GEQ:
    case AST_RELATIONAL_GT:
    case AST_RELATIONAL_LEQ:
    case AST_RELATIONAL_LT:
    case AST_FUNCTION_PIECEWISE:

     checkNary(m, node, sb);
      break;

    case AST_FUNCTION_ROOT:
    case AST_MINUS:
      
      checkSpecialCases(m, node, sb);
      break;

    case AST_FUNCTION:

      if (m.getFunctionDefinition(node.getName()))
      {
        /* functiondefinition math */
        const ASTNode * fdMath = m.getFunctionDefinition(node.getName())->getMath();
        if (fdMath != NULL)
        {
        /* We have a definition for this function.  Does the defined number
	          of arguments equal the number used here? */

          if (node.getNumChildren() + 1 != fdMath->getNumChildren())
	        {
            logMathConflict(node, sb);
	        }
        }

      }
      break;

    default:

      checkChildren(m, node, sb);
      break;

  }

}

/**
  * Checks that the function has only one argument
  */
void NumberArgsMathCheck::checkUnary(const Model& m, 
                                     const ASTNode& node, const SBase & sb)
{
  if (node.getNumChildren() != 1)
  {
    logMathConflict(node, sb);
  }
  
  checkMath(m, *node.getLeftChild(), sb);
}

/**
  * Checks that the function has exactly two arguments
  */
void NumberArgsMathCheck::checkBinary(const Model& m, 
                                      const ASTNode& node, const SBase & sb)
{
  if (node.getNumChildren() != 2)
  {
    logMathConflict(node, sb);
  }

  for (unsigned int n = 0; n < node.getNumChildren(); n++)
  {
    checkMath(m, *node.getChild(n), sb);
  }
}

/**
  * Checks that the function at least two arguments
  */
void NumberArgsMathCheck::checkNary(const Model& m, 
                                    const ASTNode& node, const SBase & sb)
{
  if (node.getNumChildren() < 2)
  {
    logMathConflict(node, sb);
  }

  for (unsigned int n = 0; n < node.getNumChildren(); n++)
  {
    checkMath(m, *node.getChild(n), sb);
  }
}

/**
  * Checks that the functions have either one or two arguments
  */
void NumberArgsMathCheck::checkSpecialCases(const Model& m, 
                                            const ASTNode& node, const SBase & sb)
{
  if (node.getNumChildren() < 1 || node.getNumChildren() > 2)
  {
    logMathConflict(node, sb);
  }

  for (unsigned int n = 0; n < node.getNumChildren(); n++)
  {
    checkMath(m, *node.getChild(n), sb);
  }
}


  
/**
 * @return the error message to use when logging constraint violations.
 * This method is called by logFailure.
 *
 * Returns a message that the given id and its corresponding object are
 * in  conflict with an object previously defined.
 */
const string
NumberArgsMathCheck::getMessage (const ASTNode& node, const SBase& object)
{

  ostringstream msg;

  //msg << getPreamble();

  msg << "\nThe formula '" << SBML_formulaToString(&node);
  msg << "' in the " << getFieldname() << " element of the " << getTypename(object);
  msg << " has an inappropriate number of arguments.";

  return msg.str();
}
