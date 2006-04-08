/**
 * \file    FormulaUnitsCheck.cpp
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

#include "FormulaUnitsCheck.h"


using namespace std;


/**
 * Creates a new Constraint with the given id.
 */
FormulaUnitsCheck::FormulaUnitsCheck (unsigned int id, Validator& v) :
  TConstraint<Model>(id, v)
{
}


/**
 * Destroys this Constraint.
 */
FormulaUnitsCheck::~FormulaUnitsCheck ()
{
}


/**
  * Checks that the units of the result of the assignment rule
  * are consistent with variable being assigned
  *
  * @return true if units are consistent, false otherwise.
  */
void
FormulaUnitsCheck::check_ (const Model& m, const Model& object)
{
  unsigned int n, ea, sr;
  /* check all math within a model */

  for (n = 0; n < m.getNumRules(); n++)
  {
    if (m.getRule(n)->isSetMath())
    {
      checkUnits(m, m.getRule(n)->getMath(), m.getRule(n));
    }
  }

  for (n = 0; n < m.getNumReactions(); n++)
  {
    if (m.getReaction(n)->isSetKineticLaw())
    {
      if (m.getReaction(n)->getKineticLaw()->isSetMath())
      {
        checkUnits(m, m.getReaction(n)->getKineticLaw()->getMath(), 
          m.getReaction(n)->getKineticLaw());
      }
    }
    for (sr = 0; sr < m.getReaction(n)->getNumProducts(); sr++)
    {
      if (m.getReaction(n)->getProduct(sr)->isSetStoichiometryMath())
      {
        checkUnits(m, 
          m.getReaction(n)->getProduct(sr)->getStoichiometryMath(), 
          m.getReaction(n)->getProduct(sr));
      }
    }
    for (sr = 0; sr < m.getReaction(n)->getNumReactants(); sr++)
    {
      if (m.getReaction(n)->getReactant(sr)->isSetStoichiometryMath())
      {
        checkUnits(m, 
          m.getReaction(n)->getReactant(sr)->getStoichiometryMath(), 
          m.getReaction(n)->getReactant(sr));
      }
    }
  }

  for (n = 0; n < m.getNumEvents(); n++)
  {
    if (m.getEvent(n)->isSetTrigger())
    {
      checkUnits(m, m.getEvent(n)->getTrigger(), m.getEvent(n));
    }
    if (m.getEvent(n)->isSetDelay())
    {
      checkUnits(m, m.getEvent(n)->getDelay(), m.getEvent(n));
    }
    for (ea = 0; ea < m.getEvent(n)->getNumEventAssignments(); ea++)
    {
      if (m.getEvent(n)->getEventAssignment(ea)->isSetMath())
      {
        checkUnits(m, m.getEvent(n)->getEventAssignment(ea)->getMath(), 
          m.getEvent(n)->getEventAssignment(ea));
      }
    }
  }

}


/**
  * Checks that the units of the result of the assignment rule
  * are consistent with variable being assigned
  *
  * If an inconsistent variable is found, an error message is logged.
  */
void
FormulaUnitsCheck::checkUnits (const Model& m, const ASTNode* node, const SBase * sb)
{
  ASTNodeType_t type = node->getType();

  switch (type) 
  {
    /* functions that act on same units */
    case AST_PLUS:
    case AST_MINUS:
    case AST_FUNCTION_ABS:
    case AST_FUNCTION_CEILING:
    case AST_FUNCTION_FLOOR:
   
      checkSameUnitsAsArgs(m, node, sb);
      break;

    /* functions that have no restrictions on units 
     * but may have children*/
    case AST_TIMES:
    case AST_DIVIDE:
    case AST_LAMBDA:
    case AST_LOGICAL_AND:
    case AST_LOGICAL_NOT:
    case AST_LOGICAL_OR:
    case AST_LOGICAL_XOR:
    case AST_RELATIONAL_EQ:
    case AST_RELATIONAL_GEQ:
    case AST_RELATIONAL_GT:
    case AST_RELATIONAL_LEQ:
    case AST_RELATIONAL_LT:
    case AST_RELATIONAL_NEQ:
 
      checkChildren(m, node, sb);
      break;

    /* functions that have no restrictions on units */
    case AST_INTEGER:
    case AST_REAL:
    case AST_REAL_E:
    case AST_RATIONAL:
    case AST_NAME:
    case AST_NAME_TIME:
    case AST_CONSTANT_E:
    case AST_CONSTANT_FALSE:
    case AST_CONSTANT_PI:
    case AST_CONSTANT_TRUE:

      break;

    case AST_FUNCTION:

      checkFunction(m, node, sb);
      break;

    /* functions that take a single dimensionless argument */
    /* inverse hyerbolic functions */
    case AST_FUNCTION_ARCCOSH:
    case AST_FUNCTION_ARCCOTH:
    case AST_FUNCTION_ARCCSCH:
    case AST_FUNCTION_ARCSECH:
    case AST_FUNCTION_ARCSINH:
    case AST_FUNCTION_ARCTANH:
    
    /* inverse trig functions */
    case AST_FUNCTION_ARCCOS:
    case AST_FUNCTION_ARCCOT:
    case AST_FUNCTION_ARCCSC:
    case AST_FUNCTION_ARCSEC:
    case AST_FUNCTION_ARCSIN:
    case AST_FUNCTION_ARCTAN: 
  
    /* hyperbolic functions */
    case AST_FUNCTION_COSH:
    case AST_FUNCTION_COTH:
    case AST_FUNCTION_CSCH:
    case AST_FUNCTION_SECH:
    case AST_FUNCTION_SINH:
    case AST_FUNCTION_TANH: 

    /* trigonometry functions */
    case AST_FUNCTION_COS:
    case AST_FUNCTION_COT:
    case AST_FUNCTION_CSC:
    case AST_FUNCTION_SEC:
    case AST_FUNCTION_SIN:
    case AST_FUNCTION_TAN: 

    /* logarithmic functions */
    case AST_FUNCTION_EXP:
    case AST_FUNCTION_LN:
    case AST_FUNCTION_LOG:

    case AST_FUNCTION_FACTORIAL:

      checkDimensionlessArgs(m, node, sb);
      break;

    case AST_FUNCTION_DELAY:

      checkUnitsFromDelay(m, node, sb);
      break;

    case AST_FUNCTION_PIECEWISE:
      
      checkUnitsFromPiecewise(m, node, sb);
      break;

    case AST_POWER:
    case AST_FUNCTION_POWER:

      checkUnitsFromPower(m, node, sb);
      break;

    case AST_FUNCTION_ROOT:

      checkUnitsFromRoot(m, node, sb);
      break;

    case AST_UNKNOWN:
    default:
      logUnknown(node, sb);
      break;
  }
}

/**
  * Checks that the units of the children of ASTnode 
  * are appropriate for the function being performed
  * forces recursion through the AST tree
  *
  * If inconsistent units are found, an error message is logged.
  */
void 
FormulaUnitsCheck::checkChildren (const Model& m, 
                                  const ASTNode* node, 
                                  const SBase * sb)
{
  unsigned int n;

  /* check fucntions that should only have one child */
  if (node->getType() == AST_LOGICAL_NOT)
  {
    if (node->getNumChildren() != 1)
      logInconsistentNumber(node, sb, 1);
  }
  else if (node->getType() == AST_RELATIONAL_NEQ)
  {
    if (node->getNumChildren() != 2)
      logInconsistentNumber(node, sb, 2);
  }

  for(n = 0; n < node->getNumChildren(); n++)
  {
    checkUnits(m, node->getChild(n), sb);
  }
}
  
/**
  * Checks that the units of function definition 
  * are appropriate for the function being performed
  * forces recursion through the AST tree
  *
  * If inconsistent units are found, an error message is logged.
  */
void 
FormulaUnitsCheck::checkFunction (const Model& m, 
                                  const ASTNode* node, 
                                  const SBase * sb)
{
  unsigned int i, nodeCount;
  const ASTNode * fdMath;
  ASTNode *newMath;

  /**
    * find corresponding func def which will have
    * the formula as the rightChild of ASTNode
    */
  fdMath = m.getFunctionDefinition(node->getName())
    ->getMath()->getRightChild();
  
  /* if function has no variables then this will be null */
  if (fdMath == NULL)
  {
    newMath = m.getFunctionDefinition(node->getName())
    ->getMath()->getLeftChild();
  }
  else
  {
    /**
      * create a new ASTNode of this type but with the children
      * from the original function
      */
    newMath = new ASTNode(fdMath->getType());
    nodeCount = 0;
    for (i = 0; i < fdMath->getNumChildren(); i++)
    {
      if (fdMath->getChild(i)->isName())
      {
        newMath->addChild(node->getChild(nodeCount));
        nodeCount++;
      }
      else
      {
        newMath->addChild(fdMath->getChild(i));
      }
    }
  }
  checkUnits(m, newMath, sb);
}
 
/**
  * Checks that the units of the power function are consistent
  *
  * If inconsistent units are found, an error message is logged.
  */
void 
FormulaUnitsCheck::checkUnitsFromPower (const Model& m, 
                                        const ASTNode* node, 
                                        const SBase * sb)
{
  /* power (v, n) = v^n 
   * if v has units other than dimensionless then
   * n must be an integer
   */
  UnitDefinition * dim = new UnitDefinition();
  UnitDefinition * tempUD, *tempUD1;
  Unit * unit = new Unit("dimensionless");
  dim->addUnit(unit);
  
  UnitFormulaFormatter *unitFormat = new UnitFormulaFormatter(&m);

  tempUD = unitFormat->getUnitDefinition(node->getLeftChild());
  ASTNode * child = node->getRightChild();
  tempUD1 = unitFormat->getUnitDefinition(child);
  
  if (!areEquivalent(dim, tempUD)) 
  {
    /* power must be an integer
     * but need to check that it is not a real
     * number that is integral
     * i.e. mathml <cn> 2 </cn> will record a "real"
     */
    if (!child->isInteger())
    {
      if (!child->isReal()) 
      {
        logInconsistentPower(node, sb);
      }
      else if (ceil(child->getReal()) != child->getReal())
      {
        logInconsistentPower(node, sb);
      }
    }
  }
  else if (!areEquivalent(dim, tempUD1)) 
  {
    /* power (3, k) */
    logInconsistentPower(node, sb);
  }

  checkUnits(m, node->getLeftChild(), sb);
}

/**
  * Checks that the units of the root function are consistent
  *
  * If inconsistent units are found, an error message is logged.
  */
void 
FormulaUnitsCheck::checkUnitsFromRoot (const Model& m, 
                                        const ASTNode* node, 
                                        const SBase * sb)
{
  /* root (v, n) = v^1/n 
   * the exponent of the resulting unit must be integral
   */

  int root;
  unsigned int n;
  UnitDefinition * tempUD;
  UnitFormulaFormatter *unitFormat = new UnitFormulaFormatter(&m);

  tempUD = unitFormat->getUnitDefinition(node->getLeftChild());
  ASTNode * child = node->getRightChild();
    
  /* exponent must have integral form */
  if (!child->isInteger())
  {
    if (!child->isReal()) 
    {
      logInconsistentPower(node, sb);
    }
    else if (ceil(child->getReal()) != child->getReal())
    {
      logInconsistentPower(node, sb);
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
      logInconsistentRoot(node, sb);
    }
  }

  checkUnits(m, node->getLeftChild(), sb);
}

/**
  * Checks that the units of the delay function are consistent
  *
  * If inconsistent units are found, an error message is logged.
  */
void 
FormulaUnitsCheck::checkUnitsFromDelay (const Model& m, 
                                        const ASTNode* node, 
                                        const SBase * sb)
{
  /* delay(x, t) 
   * no restrictions on units of x
   * but t must have units of time
   */
  UnitDefinition * time = new UnitDefinition();
  UnitDefinition * tempUD;
  Unit * unit = new Unit("second");
  time->addUnit(unit);
  
  UnitFormulaFormatter *unitFormat = new UnitFormulaFormatter(&m);

  tempUD = unitFormat->getUnitDefinition(node->getRightChild());
  
  if (!areEquivalent(time, tempUD)) 
  {
    logInconsistentDelay(node, sb);
  }

  checkUnits(m, node->getLeftChild(), sb);
}
/**
  * Checks that the units of the piecewise function are consistent
  *
  * If inconsistent units are found, an error message is logged.
  */
void 
FormulaUnitsCheck::checkUnitsFromPiecewise (const Model& m, 
                                        const ASTNode* node, 
                                        const SBase * sb)
{
  /* piecewise(x, y, z)
   * x and z must have same units
   * y must be dimensionless
   */
  unsigned int n;
  UnitDefinition * dim = new UnitDefinition();
  UnitDefinition * tempUD, * tempUD1;
  Unit * unit = new Unit("dimensionless");
  dim->addUnit(unit);
  
  UnitFormulaFormatter *unitFormat = new UnitFormulaFormatter(&m);

  tempUD = unitFormat->getUnitDefinition(node->getRightChild());
  tempUD1 = unitFormat->getUnitDefinition(node->getLeftChild());
  
  if (!areEquivalent(tempUD, tempUD1)) 
  {
    logInconsistentPiecewise(node, sb);
  }

  tempUD = unitFormat->getUnitDefinition(node->getChild(1));

  if (!areEquivalent(tempUD, dim)) 
  {
    logInconsistentPiecewiseCondition(node, sb);
  }
 
  for(n = 0; n < node->getNumChildren(); n++)
  {
    checkUnits(m, node->getChild(n), sb);
  }

}
/**
  * Checks that the units of the function are consistent
  * for a function returning value with same units as argument(s)
  *
  * If inconsistent units are found, an error message is logged.
  */
void 
FormulaUnitsCheck::checkSameUnitsAsArgs (const Model& m, 
                                              const ASTNode* node, 
                                              const SBase * sb)
{
  UnitDefinition * ud;
  UnitDefinition * tempUD;
  unsigned int n;
  unsigned int i = 0;
  UnitFormulaFormatter *unitFormat = new UnitFormulaFormatter(&m);

  ud = unitFormat->getUnitDefinition(node->getChild(i));

  /* get the first child that is not a parameter with undeclared units */
  while (unitFormat->hasUndeclaredUnits(node->getChild(i)) && 
    i < node->getNumChildren()-1)
  {
    i++;
    ud = unitFormat->getUnitDefinition(node->getChild(i));
  }


  /* check that all children have the same units 
   * unless one of the children is a parameter with undeclared units 
   * which is not tested */
  for (n = i+1; n < node->getNumChildren(); n++)
  {
    tempUD = unitFormat->getUnitDefinition(node->getChild(n));

    if (!unitFormat->hasUndeclaredUnits(node->getChild(n)))
    {
      if (!areIdentical(ud, tempUD))
      {
        logInconsistentSameUnits(node, sb);
      }
    }
  }

  for (n = 0; n < node->getNumChildren(); n++)
  {
    checkUnits(m, node->getChild(n), sb);
  }
}

/**
  * Checks that the units of the arguments 
  * of the function are dimensionless
  * and that there is only one argument
  *
  * If inconsistent units are found, an error message is logged.
  */
void 
FormulaUnitsCheck::checkDimensionlessArgs (const Model& m, 
                                           const ASTNode* node, 
                                           const SBase * sb)
{
  UnitDefinition * dim = new UnitDefinition();
  UnitDefinition * tempUD;
  Unit * unit = new Unit("dimensionless");
  dim->addUnit(unit);
  
  UnitFormulaFormatter *unitFormat = new UnitFormulaFormatter(&m);

  tempUD = unitFormat->getUnitDefinition(node->getChild(0));
  
  if (node->getNumChildren() != 1)
  {
    logInconsistentNumber(node, sb, 1);
  }
  else if (!areEquivalent(dim, tempUD)) 
  {
    logInconsistentDimensionless(node, sb);
  }

}


/**
  * Logs a message about an unknown astnode
  */
void 
FormulaUnitsCheck::logUnknown (const ASTNode *  node, const SBase * sb)
{
  msg = "The math formula ";

  msg += SBML_formulaToString(node);
  msg += " is not recognised and therefore unit consistency ";
  msg += "cannot be established.";
  
  logFailure(*sb);
}


/**
  * Logs a message about a power function
  */
void 
FormulaUnitsCheck::logInconsistentPower (const ASTNode * node, 
                                         const SBase * sb)
{
  msg = "The formula ";

  msg += SBML_formulaToString(node);
  msg += " contains a power that is not an integer and thus may produce ";
  msg += "invalid units.";
  
  logFailure(*sb);
}

/**
  * Logs a message about a root function
  */
void 
FormulaUnitsCheck::logInconsistentRoot (const ASTNode * node, 
                                         const SBase * sb)
{
  msg = "The formula ";

  msg += SBML_formulaToString(node);
  msg += " when applied produces an exponent that is not an integer";
  msg += " and thus may produce invalid units.";
  
  logFailure(*sb);
}

/**
* Logs a message about a function that should return same units
* as the arguments
*/
void 
FormulaUnitsCheck::logInconsistentSameUnits (const ASTNode * node, 
                                             const SBase * sb)
{
  msg = "The formula ";

  msg += SBML_formulaToString(node);
  msg += " uses a function ";
  msg += " which can only act on variables with the same units.";
  
  logFailure(*sb);

}

/**
* Logs a message about a function that should have dmensionless
* as the arguments
*/
void 
FormulaUnitsCheck::logInconsistentDimensionless (const ASTNode * node, 
                                                 const SBase * sb)
{
  msg = "The formula ";

  msg += SBML_formulaToString(node);
  msg += " uses a function ";
  msg += " which can only act on dimensionless variables.";
  
  logFailure(*sb);

}

  /**
  * Logs a message about a function that should 
  * have only 1 or 2 arguments
  */
void 
FormulaUnitsCheck::logInconsistentNumber (const ASTNode * node, 
                                          const SBase * sb,
                                          int number)
{
  msg = "The formula ";

  msg += SBML_formulaToString(node);
  msg += " uses a function";
  if (number == 1) 
    msg += " which can only act on a single argument.";
  else
    msg += " which can only act on two arguments.";
  
  logFailure(*sb);

}

/**
* Logs a message about a delay function that should have time units
*/
void 
FormulaUnitsCheck::logInconsistentDelay (const ASTNode * node, 
                                          const SBase * sb)
{
  msg = "The formula ";

  msg += SBML_formulaToString(node);
  msg += " uses a delay function";
  msg += " with a delta t value that does not have units of time.";
  
  logFailure(*sb);

}

/**
* Logs a message about a piecewise function that should same units
*/
void 
FormulaUnitsCheck::logInconsistentPiecewise (const ASTNode * node, 
                                          const SBase * sb)
{
  msg = "The formula ";

  msg += SBML_formulaToString(node);
  msg += " uses a piecewise function";
  msg += " where different branches return different units.";
  
  logFailure(*sb);

}

/**
* Logs a message about the conditional part of a piecewise function 
* that should have dimensionless units
*/
void 
FormulaUnitsCheck::logInconsistentPiecewiseCondition (const ASTNode * node, 
                                          const SBase * sb)
{
  msg = "The formula ";

  msg += SBML_formulaToString(node);
  msg += " uses a piecewise function";
  msg += " where the conditional statement is not dimensionless.";
  
  logFailure(*sb);

}
