/**
 * @file    EqualityArgsMathCheck.cpp
 * @brief   Ensures arguments to eq and neq are consistent.
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

#include "EqualityArgsMathCheck.h"

static const char* PREAMBLE =
    "The values of all arguments to 'eq' and 'neq' operators should have the "
    "same type (either all boolean or all numeric). (References: L2V2 "
    "Section 3.5.8.)";

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */


/**
 * Creates a new Constraint with the given id.
 */
EqualityArgsMathCheck::EqualityArgsMathCheck (unsigned int id, Validator& v) : MathMLBase(id, v)
{
}


/**
 * Destroys this Constraint.
 */
EqualityArgsMathCheck::~EqualityArgsMathCheck ()
{
}


/**
 * @return the preamble to use when logging constraint violations.
 */
const char*
EqualityArgsMathCheck::getPreamble ()
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
EqualityArgsMathCheck::checkMath (const Model& m, const ASTNode& node, const SBase & sb)
{
  ASTNodeType_t type = node.getType();

  /* check arguments of eq or neq */
  switch (type) 
  {
    case AST_RELATIONAL_EQ:
    case AST_RELATIONAL_NEQ:

      checkArgs(m, node, sb);
      break;


    case AST_FUNCTION:

      checkFunction(m, node, sb);
      break;

    default:

      checkChildren(m, node, sb);
      break;

  }
}

  
/**
  * Checks that the arguments to eq or neq are consistent
  * i.e. have same type both boolean or both numeric
  *
  * If an inconsistency is found, an error message is logged.
  */
void 
EqualityArgsMathCheck::checkArgs (const Model& m, 
                                        const ASTNode& node, 
                                        const SBase & sb)
{
  /* arguments must return consistent value types */
  if (returnsNumeric(m, node.getLeftChild()) && 
     !returnsNumeric(m, node.getRightChild()))
  {
    logMathConflict(node, sb);
  }
  else if (node.getLeftChild()->isBoolean() && 
          !node.getRightChild()->isBoolean())
  {
    logMathConflict(node, sb);
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
EqualityArgsMathCheck::getMessage (const ASTNode& node, const SBase& object)
{

  ostringstream msg;

  msg << getPreamble();

  msg << "\nThe formula '" << SBML_formulaToString(&node);
  msg << "' in the " << getFieldname() << " element of the " << getTypename(object);
  msg << " uses arguments that should be either both numeric or both boolean.";

  return msg.str();
}
