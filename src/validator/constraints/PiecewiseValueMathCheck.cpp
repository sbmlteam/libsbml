/**
 * \file    PiecewiseValueMathCheck.cpp
 * \brief   Ensures types returned by branches of a piecewise are consistent.
 * \author  Sarah Keating
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

#include "PiecewiseValueMathCheck.h"

static const char* PREAMBLE =
    "The types of values within 'piecewise' operators should all be "
    "consistent: the set of expressions that make up the first arguments of "
    "the 'piece' and 'otherwise' operators within the same 'piecewise' "
    "operator should all return values of the same type. (References: L2V2 "
    "Section 3.5.8.)";

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxgen-ignored */


/**
 * Creates a new Constraint with the given id.
 */
PiecewiseValueMathCheck::PiecewiseValueMathCheck (unsigned int id, Validator& v) : MathMLBase(id, v)
{
}


/**
 * Destroys this Constraint.
 */
PiecewiseValueMathCheck::~PiecewiseValueMathCheck ()
{
}


/**
 * @return the preamble to use when logging constraint violations.
 */
const char*
PiecewiseValueMathCheck::getPreamble ()
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
PiecewiseValueMathCheck::checkMath (const Model& m, const ASTNode& node, const SBase & sb)
{
  ASTNodeType_t type = node.getType();

  switch (type) 
  {
    case AST_FUNCTION_PIECEWISE:

      checkPiecewiseArgs(m, node, sb);
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
  * Checks that the arguments of the branches of a piecewise are consistent
  *
  * If not, an error message is logged.
  */
void 
PiecewiseValueMathCheck::checkPiecewiseArgs (const Model& m, const ASTNode& node, 
                                                  const SBase & sb)
{
  /* arguments must return consistent types */
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
PiecewiseValueMathCheck::getMessage (const ASTNode& node, const SBase& object)
{

  ostringstream msg;

  msg << getPreamble();

  msg << "\nThe piecewise formula ";
  msg << "in the " << getFieldname() << " element of the " << getTypename(object);
  msg << " returns arguments '" << SBML_formulaToString(node.getLeftChild());
  msg << "' and '" << SBML_formulaToString(node.getRightChild());
  msg << "' which have different value types.";

  return msg.str();
}
