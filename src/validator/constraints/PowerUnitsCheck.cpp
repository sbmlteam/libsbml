/**
 * \file    PowerUnitsCheck.cpp
 * \brief   Ensures math units are consistent.
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


#include <string>
#include <sstream>
#include <math.h>

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

#include <sbml/util/List.h>

#include "PowerUnitsCheck.h"

static const char* PREAMBLE =
  "A math expression using power with non-integer units may result in incorrect units.";


using namespace std;


/**
 * Creates a new Constraint with the given id.
 */
PowerUnitsCheck::PowerUnitsCheck (unsigned int id, Validator& v) : UnitsBase(id, v)
{
}


/**
 * Destroys this Constraint.
 */
PowerUnitsCheck::~PowerUnitsCheck ()
{
}

/**
 * @return the preamble to use when logging constraint violations.
 */
const char*
PowerUnitsCheck::getPreamble ()
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
PowerUnitsCheck::checkUnits (const Model& m, const ASTNode& node, const SBase & sb)
{
  ASTNodeType_t type = node.getType();

  switch (type) 
  {
    case AST_DIVIDE:
      checkForPowersBeingDivided(m, node, sb);
      break;
    case AST_POWER:
    case AST_FUNCTION_POWER:

      checkUnitsFromPower(m, node, sb);
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
PowerUnitsCheck::checkUnitsFromPower (const Model& m, 
                                        const ASTNode& node, 
                                        const SBase & sb)
{
  double value;
  /* power (v, n) = v^n 
   * if v has units other than dimensionless then
   * n must be an integer
   */
  UnitDefinition *dim = new UnitDefinition();
  Unit *unit = new Unit("dimensionless");
  dim->addUnit(unit);

  UnitFormulaFormatter *unitFormat = new UnitFormulaFormatter(&m);

  UnitDefinition *tempUD, *tempUD1;
  tempUD = unitFormat->getUnitDefinition(node.getLeftChild());
  ASTNode *child = node.getRightChild();
  tempUD1 = unitFormat->getUnitDefinition(child);

  if (!areEquivalent(dim, tempUD)) 
  {
    /* 'v' does not have units of dimensionless. */

    /* If the power 'n' is a parameter, check if its units are either
     * undeclared or declared as dimensionless.  If either is the case,
     * the value of 'n' must be an integer.
     */

    const Parameter *param = NULL;

    if (child->isName())
    {
      /* Parameters may be declared in two places (the model and the
       * kinetic law's local parameter list), so we have to check both.
       */

      if (sb.getTypeCode() == SBML_KINETIC_LAW)
      {
	      const KineticLaw* kl = dynamic_cast<const KineticLaw*>(&sb);

	      /* First try local parameters and if null is returned, try
	      * the global parameters */
	      if (kl != NULL)
	      {
	        param = kl->getParameter(child->getName());
	      }
      }

	    if (param == NULL)
	    {
	      param = m.getParameter(child->getName());
	    }
      
    }

    if (param != NULL)
    {
      /* We found a parameter with this identifier. */

      if (areEquivalent(dim, tempUD1) || unitFormat->hasUndeclaredUnits(child))
      {
        value = param->getValue();
        if (value != 0)
        {
          if (ceil(value) != value)
          {
            logUnitConflict(node, sb);
          }
        }

      }
      else
      {
	/* No parameter definition found for child->getName() */
        logUnitConflict(node, sb);
      }
    }
    else if (child->isFunction() || child->isOperator())
    {
      /* cannot test whether the value will be appropriate */
      if (!areEquivalent(dim, tempUD1))
      {
        logUnitConflict(node, sb);
      }
    }
    /* power must be an integer
     * but need to check that it is not a real
     * number that is integral
     * i.e. mathml <cn> 2 </cn> will record a "real"
     */
    else if (!child->isInteger())
    {
      if (!child->isReal()) 
      {
        logUnitConflict(node, sb);
      }
      else if (ceil(child->getReal()) != child->getReal())
      {
        logUnitConflict(node, sb);
      }
    }
  }
  else if (!areEquivalent(dim, tempUD1)) 
  {
    /* power (3, k) */
    logUnitConflict(node, sb);
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
PowerUnitsCheck::getMessage (const ASTNode& node, const SBase& object)
{

  ostringstream msg;

  msg << getPreamble();

  msg << "The formula '" << SBML_formulaToString(&node);
  msg << "' in the " << getFieldname() << " element of the " << getTypename(object);
  msg << " contains a power that is not an integer and thus may produce ";
  msg << "invalid units.";

  return msg.str();
}


void 
PowerUnitsCheck::checkForPowersBeingDivided (const Model& m, const ASTNode& node, 
                              const SBase & sb)
{
  ASTNode* left = node.getLeftChild();
  ASTNode* right = node.getRightChild();

  if (left->getType() == AST_POWER || left->getType() == AST_FUNCTION_POWER)
  {
    if (right->getType() == AST_POWER || right->getType() == AST_FUNCTION_POWER)
    {
      /* have a power divided by a power */
      /* check whether objects have same units */
        UnitFormulaFormatter *unitFormat = new UnitFormulaFormatter(&m);

        UnitDefinition *tempUD, *tempUD1, *tempUD2, *tempUD3;
        tempUD = unitFormat->getUnitDefinition(left->getLeftChild());
        tempUD1 = unitFormat->getUnitDefinition(right->getLeftChild());
        tempUD2 = unitFormat->getUnitDefinition(right->getRightChild());
        tempUD3 = unitFormat->getUnitDefinition(left->getRightChild());

        if (!areEquivalent(tempUD, tempUD1))
        {
          checkChildren(m, node, sb);
        }
        else
        {
          if(!areEquivalent(tempUD2, tempUD3))
          {
            logUnitConflict(node, sb);
          }
          else
          {
            /* create an ASTNode with pow(object, left_exp - right_exp) */
            ASTNode *newPower = new ASTNode(AST_POWER);
            ASTNode * newMinus = new ASTNode(AST_MINUS);
            newMinus->addChild(left->getRightChild()->deepCopy());
            newMinus->addChild(right->getRightChild()->deepCopy());
            newPower->addChild(left->getLeftChild()->deepCopy());
            newPower->addChild(newMinus);

            checkUnitsFromPower(m, *newPower, sb);

            delete newPower;
          }
        }

    }
    else
    {
      checkChildren(m, node, sb);
    }
  }
  else 
  {
    checkChildren(m, node, sb);
  }
}