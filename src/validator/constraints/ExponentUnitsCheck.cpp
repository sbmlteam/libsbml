/**
 * @file    ExponentUnitsCheck.cpp
 * @brief   Ensures math units are consistent.
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

#include "ExponentUnitsCheck.h"

static const char* PREAMBLE =
  "The use of non-integral exponents may result in incorrect units.";

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxgen-ignored */


/**
 * Creates a new Constraint with the given id.
 */
ExponentUnitsCheck::ExponentUnitsCheck (unsigned int id, Validator& v) : UnitsBase(id, v)
{
}


/**
 * Destroys this Constraint.
 */
ExponentUnitsCheck::~ExponentUnitsCheck ()
{
}

/**
 * @return the preamble to use when logging constraint violations.
 */
const char*
ExponentUnitsCheck::getPreamble ()
{
  return PREAMBLE;
}




/**
  * Checks that the units of the result of the assignment rule
  * are consistent with variable being assigned
  *
  * If an inconsistent variable is found, an error message is logged.
  */
void
ExponentUnitsCheck::checkUnits (const Model& m, const ASTNode& node, const SBase & sb)
{
  ASTNodeType_t type = node.getType();

  switch (type) 
  {
    case AST_FUNCTION_ROOT:

      checkUnitsFromRoot(m, node, sb);
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
  * Checks that the units of the power function are consistent
  *
  * If inconsistent units are found, an error message is logged.
  */
void 
ExponentUnitsCheck::checkUnitsFromRoot (const Model& m, 
                                        const ASTNode& node, 
                                        const SBase & sb)
{
  /* root (v, n) = v^1/n 
   * the exponent of the resulting unit must be integral
   */

  int root = 1;
  unsigned int n;
  UnitDefinition * tempUD;
  UnitFormulaFormatter *unitFormat = new UnitFormulaFormatter(&m);

  tempUD = unitFormat->getUnitDefinition(node.getLeftChild());
  ASTNode * child = node.getRightChild();
    
  /* exponent must have integral form */
  if (!child->isInteger())
  {
    if (!child->isReal()) 
    {
      logUnitConflict(node, sb);
    }
    else if (ceil(child->getReal()) != child->getReal())
    {
      logUnitConflict(node, sb);
    }
    else 
    {
      root = (int) child->getReal();
    }
  }
  else
  {
    root = child->getInteger();
  }
  
  for (n = 0; n < tempUD->getNumUnits(); n++)
  {
    if (tempUD->getUnit(n)->getExponent() % root != 0)
    {
      logUnitConflict(node, sb);
    }
  }

  checkUnits(m, *node.getLeftChild(), sb);
}


/**
 * @return the error message to use when logging constraint violations.
 * This method is called by logFailure.
 *
 * Returns a message that the given id and its corresponding object are
 * in  conflict with an object previously defined.
 */
const string
ExponentUnitsCheck::getMessage (const ASTNode& node, const SBase& object)
{

  ostringstream msg;

  msg << getPreamble();

  msg << "The formula '" << SBML_formulaToString(&node);
  msg << "' in the " << getFieldname() << " element of the " << getTypename(object);
  msg << " produces an exponent that is not an integer and thus may produce ";
  msg << "invalid units.";

  return msg.str();
}
