/**
 * \file    UnitFormulaFormatter.cpp
 * \brief   Formats an AST formula tree as a unit definition
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


#include "UnitFormulaFormatter.h"

/**
 *  constructs a UnitFormulaFormatter
 */
UnitFormulaFormatter::UnitFormulaFormatter(const Model *m)
{
  model = m;
  undeclaredUnits = 0;
  canIgnoreUndeclaredUnits = 0;
}

/**
 *  destructor
 */
UnitFormulaFormatter::~UnitFormulaFormatter()
{
}

/**
  * visits the ASTNode and returns the unitDefinition of the formula
  * this function is really a dispatcher to the other
  * UnitFormulaFormatter::getUnitdefinition functions
  */
UnitDefinition * 
UnitFormulaFormatter::getUnitDefinition(const ASTNode * node)
{  
  UnitDefinition * ud = NULL;

  if (node == NULL)
    return ud;


  ASTNodeType_t type = node->getType();

  switch (type) 
  {
  /* functions that return a dimensionless result */
    case AST_FUNCTION_FACTORIAL:

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

    /* boolean */
    case AST_LOGICAL_AND:
    case AST_LOGICAL_NOT:
    case AST_LOGICAL_OR:
    case AST_LOGICAL_XOR:
    case AST_CONSTANT_FALSE:
    case AST_CONSTANT_TRUE:

    /* relational */
    case AST_RELATIONAL_EQ:
    case AST_RELATIONAL_GEQ:
    case AST_RELATIONAL_GT:
    case AST_RELATIONAL_LEQ:
    case AST_RELATIONAL_LT:
    case AST_RELATIONAL_NEQ:

      ud = getUnitDefinitionFromDimensionlessReturnFunction(node);
      break;

  /* functions that return same units */
    case AST_PLUS:
    case AST_MINUS:
    case AST_FUNCTION_ABS:
    case AST_FUNCTION_CEILING:
    case AST_FUNCTION_FLOOR:
  
      ud = getUnitDefinitionFromArgUnitsReturnFunction(node);
      break;

  /* power functions */
    case AST_POWER:
    case AST_FUNCTION_POWER:
  
      ud = getUnitDefinitionFromPower(node);
      break;

  /* times functions */
    case AST_TIMES:
  
      ud = getUnitDefinitionFromTimes(node);
      break;

  /* divide functions */
    case AST_DIVIDE:
  
      ud = getUnitDefinitionFromDivide(node);
      break;

  /* piecewise functions */
    case AST_FUNCTION_PIECEWISE:
  
      ud = getUnitDefinitionFromPiecewise(node);
      break;

  /* root functions */
    case AST_FUNCTION_ROOT:
  
      ud = getUnitDefinitionFromRoot(node);
      break;

  /* functions */
    case AST_LAMBDA:
    case AST_FUNCTION:
  
      ud = getUnitDefinitionFromFunction(node);
      break;
    
  /* delay */
    case AST_FUNCTION_DELAY:
  
      ud = getUnitDefinitionFromDelay(node);
      break;


  /* others */

    /* numbers */
    case AST_INTEGER:
    case AST_REAL:
    case AST_REAL_E:
    case AST_RATIONAL:

    /* time */
    case AST_NAME_TIME:

    /* constants */
    case AST_CONSTANT_E:
    case AST_CONSTANT_PI:

    /* name of another component in the model */
    case AST_NAME:

      ud = getUnitDefinitionFromOther(node);
      break;

    case AST_UNKNOWN:
    default:
    
      ud = new UnitDefinition();
      break;
  }
  // as a safety catch 
  if (ud == NULL)
  {
    ud = new UnitDefinition();
  }

  simplifyUnitDefinition(ud);

  return ud;
}

/** 
  * returns the unitDefinition for the ASTNode from a function
  */
UnitDefinition * 
UnitFormulaFormatter::getUnitDefinitionFromFunction(const ASTNode * node)
{ 
  UnitDefinition * ud;
  unsigned int i, nodeCount;
  Unit * unit;
  const ASTNode * fdMath;
  ASTNode *newMath;

  if(node->getType() == AST_FUNCTION)
  {
    if (model->getFunctionDefinition(node->getName()))
    {
      /**
      * find corresponding func def which will have
      * the formula as the rightChild of ASTNode
      */
      fdMath = model->getFunctionDefinition(node->getName())->getMath()
                        ->getRightChild();
      /* if function has no variables then this will be null */
      if (fdMath == NULL)
      {
        newMath = model->getFunctionDefinition(node->getName())->getMath()
                          ->getLeftChild();
      }
      else
      {
        /**
        * create a new ASTNode of this type but with the children
        * from the original function
        */

        /* need to catch case where a functionDefinition merely returns the argument */
        if (fdMath->getType() == AST_NAME)
        {
          newMath = node->getLeftChild();
        }
        else
        {
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
      }
      ud = getUnitDefinition(newMath);
    }
    else
    {
      ud = new UnitDefinition();
    }
  }
  else
  {
    /**
     * function is a lambda function - which wont have any units
     */
    unit = new Unit("dimensionless");
    ud   = new UnitDefinition();
    
    ud->addUnit(unit);
  }
  
  return ud;
}

/** 
  * returns the unitDefinition for the ASTNode from a times function
  */
UnitDefinition * 
UnitFormulaFormatter::getUnitDefinitionFromTimes(const ASTNode * node)
{ 
  UnitDefinition * ud;
  UnitDefinition * tempUD;
  int numChildren = node->getNumChildren();
  int n = 0;
  unsigned int i;

  ud = getUnitDefinition(node->getChild(n));

  for(n = 1; n < numChildren; n++)
  {
    tempUD = getUnitDefinition(node->getChild(n));
    for (i = 0; i < tempUD->getNumUnits(); i++)
    {
      ud->addUnit(tempUD->getUnit(i));
    }
  }

  return ud;
}

/** 
  * returns the unitDefinition for the ASTNode from a divide function
  */
UnitDefinition * 
UnitFormulaFormatter::getUnitDefinitionFromDivide(const ASTNode * node)
{ 
  UnitDefinition * ud;
  UnitDefinition * tempUD;
  unsigned int i;
  Unit * unit;

  ud = getUnitDefinition(node->getLeftChild());

  if (node->getNumChildren() == 1)
    return ud;
  tempUD = getUnitDefinition(node->getRightChild());
  for (i = 0; i < tempUD->getNumUnits(); i++)
  {
    unit = tempUD->getUnit(i);
    unit->setExponent(-1 * unit->getExponent());
    ud->addUnit(unit);
  }

  return ud;
}

/** 
  * returns the unitDefinition for the ASTNode from a power function
  */
UnitDefinition * 
UnitFormulaFormatter::getUnitDefinitionFromPower(const ASTNode * node)
{ 
  UnitDefinition * ud;
  /* this only works is the exponent is an integer - 
   * since a unit can only have an integral exponent 
   * but the mathml might do something like
   * pow(sqrt(m) * 2, 2) - which would be okay
   * unless we challenge the sqrt(m) !!
   */

  UnitDefinition * tempUD;
  unsigned int i;
  Unit * unit;
  ASTNode * child;

  tempUD = getUnitDefinition(node->getLeftChild());
  ud = new UnitDefinition();
  
  if (node->getNumChildren() == 1)
    return ud;

  child = node->getRightChild();
  for (i = 0; i < tempUD->getNumUnits(); i++)
  {
    unit = tempUD->getUnit(i);
    // need to check type
    if (child->isInteger()) 
    {
      unit->setExponent(child->getInteger() * unit->getExponent());
    }
    else 
    {
      unit->setExponent((int)(child->getReal())* unit->getExponent());
    }
    ud->addUnit(unit);
  }

  return ud;
}

/** 
  * returns the unitDefinition for the ASTNode from 
  * a piecewise function
  */
UnitDefinition * 
UnitFormulaFormatter::getUnitDefinitionFromPiecewise(const ASTNode * node)
{ 
  UnitDefinition * ud;
  
  ud = getUnitDefinition(node->getLeftChild());

  return ud;
}

/** 
  * returns the unitDefinition for the ASTNode from a root function
  */
UnitDefinition * 
UnitFormulaFormatter::getUnitDefinitionFromRoot(const ASTNode * node)
{ 
  UnitDefinition * ud;
/* this only works is the exponent is an integer - 
   * since a unit can only have an integral exponent 
   * but the mathml might do something like
   * pow(sqrt(m) * 2, 2) - which would be okay
   * unless we challenge the sqrt(m) !!
   */

  UnitDefinition * tempUD;
  unsigned int i;
  Unit * unit;
  ASTNode * child;

  tempUD = getUnitDefinition(node->getRightChild());
  ud = new UnitDefinition();

  if (node->getNumChildren() == 1)
    return ud;

  child = node->getLeftChild();
  for (i = 0; i < tempUD->getNumUnits(); i++)
  {
    unit = tempUD->getUnit(i);
    // need to check type
    if (child->isInteger()) 
    {
      unit->setExponent(unit->getExponent()/child->getInteger());
    }
    else 
    {
      unit->setExponent((int)(unit->getExponent()/child->getReal()));
    }
    ud->addUnit(unit);
  }

  return ud;
}

/** 
  * returns the unitDefinition for the ASTNode from 
  * a delay function
  */
UnitDefinition * 
UnitFormulaFormatter::getUnitDefinitionFromDelay(const ASTNode * node)
{ 
  UnitDefinition * ud;
  
  ud = getUnitDefinition(node->getLeftChild());

  return ud;
}

/** 
  * returns the unitDefinition for the ASTNode from 
  * a function returning dimensionless value
  */
UnitDefinition * 
UnitFormulaFormatter::getUnitDefinitionFromDimensionlessReturnFunction(const ASTNode *node)
{ 
  UnitDefinition * ud;
  Unit *unit;
    
  unit = new Unit("dimensionless");
  ud   = new UnitDefinition();
    
  ud->addUnit(unit);

  return ud;
}

/** 
  * returns the unitDefinition for the ASTNode from 
  * a function returning value with same units as argument(s)
  */
UnitDefinition * 
UnitFormulaFormatter::getUnitDefinitionFromArgUnitsReturnFunction(const ASTNode * node)
{ 
  UnitDefinition * ud;
  unsigned int i = 0;
  unsigned int n = 0;
 
  /* save any existing value of undeclaredUnits/canIgnoreUndeclaredUnits */
  unsigned int originalUndeclaredValue = undeclaredUnits;
  unsigned int currentIgnore = canIgnoreUndeclaredUnits;
  unsigned int currentUndeclared = undeclaredUnits;

  /* get first arg that is not a parameter with undeclared units */
  ud = getUnitDefinition(node->getChild(i));
  while (hasUndeclaredUnits(node->getChild(i)) && i < node->getNumChildren()-1)
  {
    if (originalUndeclaredValue == 1)
      currentIgnore = 0;
    else
      currentIgnore = 1;

    currentUndeclared = 1;

    i++;
    ud = getUnitDefinition(node->getChild(i));
  }

  /* loop thru remain children to determine undeclaredUnit status */
  if (undeclaredUnits == 1 && i == node->getNumChildren()-1)
  {
    /* all children are parameters with undeclared units */
    currentIgnore = 0;
  }
  else
  {
    for (n = i+1; n < node->getNumChildren(); n++)
    {
      if (hasUndeclaredUnits(node->getChild(n)))
      {
        currentUndeclared = 1;
        currentIgnore = 1;
      }
    }
  }

  /* restore original value of undeclaredUnits */
  undeclaredUnits = currentUndeclared;
  canIgnoreUndeclaredUnits = currentIgnore;

  return ud;
}

/** 
  * returns the unitDefinition for the ASTNode from anything else
  */
UnitDefinition * 
UnitFormulaFormatter::getUnitDefinitionFromOther(const ASTNode * node)
{ 
  UnitDefinition * ud = NULL;
  Unit * unit;

  unsigned int n, p, found;

  const KineticLaw * kl;

  /** 
   * ASTNode represents a number, a constant, TIME, DELAY, or
   * the name of another element of the model
   */

  if ((node->isNumber()) || (node->getType() == AST_CONSTANT_E))
  {
    unit = new Unit("dimensionless");
    ud   = new UnitDefinition();
    
    ud->addUnit(unit);
  }
  else if (node->getType() == AST_CONSTANT_PI)
  {
    unit = new Unit("radian");
    ud   = new UnitDefinition();
    
    ud->addUnit(unit);
  }
  else if (node->isName())
  {
    if (node->getType() == AST_NAME_TIME)
    {
      unit = new Unit("second");
      ud   = new UnitDefinition();
      
      ud->addUnit(unit);
    }
    /* must be the name of a compartment, species or parameter */
    else
    {
      found = 0;
      n = 0;
      while (found == 0 && n < model->getNumCompartments())
      {
        if (!strcmp(node->getName(), model->getCompartment(n)->getId().c_str()))
        {
          ud = getUnitDefinitionFromCompartment(model->getCompartment(n));
          found = 1;
          break;
        }
        else
        {
          n++;
        }
      }

      n = 0;
      while (found == 0 && n < model->getNumSpecies())
      {
        if (!strcmp(node->getName(), model->getSpecies(n)->getId().c_str()))
        {
          ud = getUnitDefinitionFromSpecies(model->getSpecies(n));
          found = 1;
          break;
        }
        else
        {
          n++;
        }
      }

      n = 0;
      while (found == 0 && n < model->getNumParameters())
      {
        if (!strcmp(node->getName(), model->getParameter(n)->getId().c_str()))
        {
          ud = getUnitDefinitionFromParameter(model->getParameter(n));
          found = 1;
          break;
        }
        else
        {
          n++;
        }
      }
      
      n = 0;
      while (found == 0 && n < model->getNumReactions())
      {
        if (model->getReaction(n)->isSetKineticLaw())
        {
          kl = model->getReaction(n)->getKineticLaw();
          for (p = 0; p < kl->getNumParameters(); p++)
          {
            if (!strcmp(node->getName(), kl->getParameter(p)->getId().c_str()))
            {
              ud = getUnitDefinitionFromParameter(kl->getParameter(p));
              found = 1;
              break;
            }
          }
        }
        n++;
      }
    }
  }

  /* catch case where a user has used a name in a formula that 
   * has not been declared anywhere in the model
   * return a unit definition with no units
   */
  if (ud == NULL)
  {
    ud = new UnitDefinition();
  }
  return ud;
}

/** 
  * returns the unitDefinition for the units of the compartment
  */
UnitDefinition * 
UnitFormulaFormatter::getUnitDefinitionFromCompartment(const Compartment * compartment)
{
  UnitDefinition * ud = NULL;
  const UnitDefinition * tempUD;
  Unit * unit;
  unsigned int n, p;

  const char * units = compartment->getUnits().c_str();

  /* no units declared implies they default to the value appropriate
   * to the spatialDimensions of the compartment 
   * noting that it is possible that these have been overridden
   * using builtin units 
   */
  if (!strcmp(units, ""))
  {
    switch (compartment->getSpatialDimensions())
    {
      case 0:
        unit = new Unit("dimensionless");
        ud   = new UnitDefinition();
      
        ud->addUnit(unit);
        break;
      case 1: 
        /* check for builtin unit length redefined */
        tempUD = model->getUnitDefinition("length");
        if (!tempUD) 
        {
          unit = new Unit("metre");
          ud   = new UnitDefinition();
        
          ud->addUnit(unit);
        }
        else
        {
          ud   = new UnitDefinition();

          unit = new Unit(tempUD->getUnit(0)->getKind());
          unit->setMultiplier(tempUD->getUnit(0)->getMultiplier());
          unit->setScale(tempUD->getUnit(0)->getScale());
          unit->setExponent(tempUD->getUnit(0)->getExponent());
          unit->setOffset(tempUD->getUnit(0)->getOffset());

          ud->addUnit(unit);
        }
        break;
      case 2:
        /* check for builtin unit area redefined */
        tempUD = model->getUnitDefinition("area");
        if (!tempUD) 
        {
          unit = new Unit("metre", 2);
          ud   = new UnitDefinition();
          
          ud->addUnit(unit);
        }
        else
        {
          ud   = new UnitDefinition();

          unit = new Unit(tempUD->getUnit(0)->getKind());
          unit->setMultiplier(tempUD->getUnit(0)->getMultiplier());
          unit->setScale(tempUD->getUnit(0)->getScale());
          unit->setExponent(tempUD->getUnit(0)->getExponent());
          unit->setOffset(tempUD->getUnit(0)->getOffset());

          ud->addUnit(unit);
        }
        break;
      default:
        /* check for builtin unit volume redefined */
        tempUD = model->getUnitDefinition("volume");
        if (!tempUD) 
        {
          unit = new Unit("litre");
          ud   = new UnitDefinition();
        
          ud->addUnit(unit);
        }
        else
        {
          ud   = new UnitDefinition();

          unit = new Unit(tempUD->getUnit(0)->getKind());
          unit->setMultiplier(tempUD->getUnit(0)->getMultiplier());
          unit->setScale(tempUD->getUnit(0)->getScale());
          unit->setExponent(tempUD->getUnit(0)->getExponent());
          unit->setOffset(tempUD->getUnit(0)->getOffset());

          ud->addUnit(unit);
        }
        break;
    }
  }
  else
  {
    /* units can be a predefined unit kind
    * a unit definition id or a builtin unit
    */
    if (UnitKind_isValidUnitKindString(units))
    {
      unit = new Unit(units);
      ud   = new UnitDefinition();
      
      ud->addUnit(unit);
    }
    else 
    {
      for (n = 0; n < model->getNumUnitDefinitions(); n++)
      {
        if (!strcmp(units, model->getUnitDefinition(n)->getId().c_str()))
        {
          ud = new UnitDefinition();
          
          for (p = 0; p < model->getUnitDefinition(n)->getNumUnits(); p++)
          {
            unit = new Unit(model->getUnitDefinition(n)->getUnit(p)->getKind());
            unit->setMultiplier(model->getUnitDefinition(n)->getUnit(p)->getMultiplier());
            unit->setScale(model->getUnitDefinition(n)->getUnit(p)->getScale());
            unit->setExponent(model->getUnitDefinition(n)->getUnit(p)->getExponent());
            unit->setOffset(model->getUnitDefinition(n)->getUnit(p)->getOffset());

            ud->addUnit(unit);
          }
        }
      }
    }
    /* now check for builtin units 
     * this check is left until now as it is possible for a builtin 
     * unit to be reassigned using a unit definition and thus will have 
     * been picked up above
     */
    if (Unit_isBuiltIn(units) && ud == NULL)
    {
      ud   = new UnitDefinition();

      if (!strcmp(units, "volume"))
      {
        unit = new Unit("litre");
        ud->addUnit(unit);
      }
      else if (!strcmp(units, "area"))
      {
        unit = new Unit("metre", 2);
        ud->addUnit(unit);
      }
      else if (!strcmp(units, "length"))
      {
        unit = new Unit("metre");
        ud->addUnit(unit);
      }
    }
  }

  // as a safety catch 
  if (ud == NULL)
  {
    ud = new UnitDefinition();
  }

  return ud;
}

/** 
  * returns the unitDefinition for the units of the species
  */
UnitDefinition * 
UnitFormulaFormatter::getUnitDefinitionFromSpecies(const Species * species)
{
  UnitDefinition * ud = NULL;
  const UnitDefinition * tempUd;
  UnitDefinition *subsUD = NULL;
  UnitDefinition *sizeUD = NULL;
  Unit * unit;
  const Compartment * c;
  unsigned int n, p;

  const char * units        = species->getSubstanceUnits().c_str();
  const char * spatialUnits = species->getSpatialSizeUnits().c_str();


  /* deal with substance units */
 
  /* no units declared implies they default to the value substance */
  if (!strcmp(units, ""))
  {
    /* check for builtin unit substance redefined */
    tempUd = model->getUnitDefinition("substance");
    if (!tempUd) 
    {
      unit = new Unit("mole");
      subsUD   = new UnitDefinition();

      subsUD->addUnit(unit);
    }
    else
    {
      subsUD   = new UnitDefinition();

      unit = new Unit(tempUd->getUnit(0)->getKind());
      unit->setMultiplier(tempUd->getUnit(0)->getMultiplier());
      unit->setScale(tempUd->getUnit(0)->getScale());
      unit->setExponent(tempUd->getUnit(0)->getExponent());
      unit->setOffset(tempUd->getUnit(0)->getOffset());

      subsUD->addUnit(unit);

    }
  }
  else
  {
    /* units can be a predefined unit kind
    * a unit definition id or a builtin unit
    */
    if (UnitKind_isValidUnitKindString(units))
    {
      unit = new Unit(units);
      subsUD   = new UnitDefinition();
      
      subsUD->addUnit(unit);
    }
    else 
    {
      for (n = 0; n < model->getNumUnitDefinitions(); n++)
      {
        if (!strcmp(units, model->getUnitDefinition(n)->getId().c_str()))
        {
          subsUD = new UnitDefinition();
          
          for (p = 0; p < model->getUnitDefinition(n)->getNumUnits(); p++)
          {
            unit = new Unit(model->getUnitDefinition(n)->getUnit(p)->getKind());
            unit->setMultiplier(model->getUnitDefinition(n)->getUnit(p)->getMultiplier());
            unit->setScale(model->getUnitDefinition(n)->getUnit(p)->getScale());
            unit->setExponent(model->getUnitDefinition(n)->getUnit(p)->getExponent());
            unit->setOffset(model->getUnitDefinition(n)->getUnit(p)->getOffset());

            subsUD->addUnit(unit);
          }
        }
      }
    }
    /* now check for builtin units 
     * this check is left until now as it is possible for a builtin 
     * unit to be reassigned using a unit definition and thus will have 
     * been picked up above
     */
    if (Unit_isBuiltIn(units) && subsUD == NULL)
    {
      subsUD   = new UnitDefinition();

      if (!strcmp(units, "substance"))
      {
        unit = new Unit("mole");
        subsUD->addUnit(unit);
      }
    }
  }
  if (species->getHasOnlySubstanceUnits())
  {
    ud = subsUD;
    return ud;
  }

  /* get the compartment containing the species */
  c = model->getCompartment(species->getCompartment().c_str());

  if (c->getSpatialDimensions() == 0)
  {
    ud = subsUD;
    return ud;
  }

  /* deal with spatial size units */

  /* no units declared implies they default to the value of compartment size */
  if (!strcmp(spatialUnits, ""))
  {
    sizeUD   = getUnitDefinitionFromCompartment(c);
  }
  else
  {
    if (UnitKind_isValidUnitKindString(spatialUnits))
    {
      unit = new Unit(spatialUnits);
      sizeUD   = new UnitDefinition();
      
      sizeUD->addUnit(unit);
    }
    else 
    {
      for (n = 0; n < model->getNumUnitDefinitions(); n++)
      {
        if (!strcmp(spatialUnits, model->getUnitDefinition(n)->getId().c_str()))
        {
          sizeUD = new UnitDefinition();
          
          for (p = 0; p < model->getUnitDefinition(n)->getNumUnits(); p++)
          {
            unit = new Unit(model->getUnitDefinition(n)->getUnit(p)->getKind());
            unit->setMultiplier(model->getUnitDefinition(n)->getUnit(p)->getMultiplier());
            unit->setScale(model->getUnitDefinition(n)->getUnit(p)->getScale());
            unit->setExponent(model->getUnitDefinition(n)->getUnit(p)->getExponent());
            unit->setOffset(model->getUnitDefinition(n)->getUnit(p)->getOffset());

            sizeUD->addUnit(unit);
          }
        }
      }
    }
    /* now check for builtin units 
     * this check is left until now as it is possible for a builtin 
     * unit to be reassigned using a unit definition and thus will have 
     * been picked up above
     */
    if (Unit_isBuiltIn(spatialUnits) && sizeUD == NULL)
    {
      sizeUD   = new UnitDefinition();

      if (!strcmp(spatialUnits, "volume"))
      {
        unit = new Unit("litre");
        sizeUD->addUnit(unit);
      }
      else if (!strcmp(spatialUnits, "area"))
      {
        unit = new Unit("metre", 2);
        sizeUD->addUnit(unit);
      }
      else if (!strcmp(spatialUnits, "length"))
      {
        unit = new Unit("metre");
        sizeUD->addUnit(unit);
      }
    }
  }

  /* units of the species are units substance/size */
  ud = subsUD;

  for (n = 0; n < sizeUD->getNumUnits(); n++)
  {
    unit = sizeUD->getUnit(n);
    unit->setExponent(-1 * unit->getExponent());

    ud->addUnit(unit);
  }

  // as a safety catch 
  if (ud == NULL)
  {
    ud = new UnitDefinition();
  }

  return ud;
}

/** 
  * returns the unitDefinition for the units of the parameter
  */
UnitDefinition * 
UnitFormulaFormatter::getUnitDefinitionFromParameter(const Parameter * parameter)
{
  UnitDefinition * ud = NULL;
  Unit * unit;
  unsigned int n, p;

  const char * units = parameter->getUnits().c_str();

 /* no units declared */
  if (!strcmp(units, ""))
  {
    ud   = new UnitDefinition();
    undeclaredUnits = 1;
    canIgnoreUndeclaredUnits = 0;
  }
  else
  {
    /* units can be a predefined unit kind
    * a unit definition id or a builtin unit
    */

    if (UnitKind_isValidUnitKindString(units))
    {
      unit = new Unit(units);
      ud   = new UnitDefinition();
      
      ud->addUnit(unit);
    }
    else 
    {
      for (n = 0; n < model->getNumUnitDefinitions(); n++)
      {
        if (!strcmp(units, model->getUnitDefinition(n)->getId().c_str()))
        {
          ud = new UnitDefinition();
          
          for (p = 0; p < model->getUnitDefinition(n)->getNumUnits(); p++)
          {
            unit = new Unit(model->getUnitDefinition(n)->getUnit(p)->getKind());
            unit->setMultiplier(model->getUnitDefinition(n)->getUnit(p)->getMultiplier());
            unit->setScale(model->getUnitDefinition(n)->getUnit(p)->getScale());
            unit->setExponent(model->getUnitDefinition(n)->getUnit(p)->getExponent());
            unit->setOffset(model->getUnitDefinition(n)->getUnit(p)->getOffset());

            ud->addUnit(unit);
          }
        }
      }
    }
    /* now check for builtin units 
     * this check is left until now as it is possible for a builtin 
     * unit to be reassigned using a unit definition and thus will have 
     * been picked up above
     */
    if (Unit_isBuiltIn(units) && ud == NULL)
    {
      ud   = new UnitDefinition();

      if (!strcmp(units, "substance"))
      {
        unit = new Unit("mole");
        ud->addUnit(unit);
      }
      else if (!strcmp(units, "volume"))
      {
        unit = new Unit("litre");
        ud->addUnit(unit);
      }
      else if (!strcmp(units, "area"))
      {
        unit = new Unit("metre", 2);
        ud->addUnit(unit);
      }
      else if (!strcmp(units, "length"))
      {
        unit = new Unit("metre");
        ud->addUnit(unit);
      }
      else if (!strcmp(units, "time"))
      {
        unit = new Unit("second");
        ud->addUnit(unit);
      }
    }

  }
  // as a safety catch 
  if (ud == NULL)
  {
    ud = new UnitDefinition();
  }

  return ud;
}

/** 
  * returns the unitDefinition for the time units of the event
  */
UnitDefinition * 
UnitFormulaFormatter::getUnitDefinitionFromEventTime(const Event * event)
{
  UnitDefinition * ud = NULL;
  const UnitDefinition * tempUd;
  Unit * unit;
  unsigned int n, p;

  const char * units = event->getTimeUnits().c_str();

 /* no units declared */
  if (!strcmp(units, ""))
  {
    /* defaults to time
    * check for redefinition of time
    */
    tempUd = model->getUnitDefinition("time");

    if (tempUd == NULL) {
      unit = new Unit("second");
      ud   = new UnitDefinition();
      
      ud->addUnit(unit);
    }
  }
  else
  {
    /* units can be a predefined unit kind
    * a unit definition id or a builtin unit
    */

    if (UnitKind_isValidUnitKindString(units))
    {
      unit = new Unit(units);
      ud   = new UnitDefinition();
      
      ud->addUnit(unit);
    }
    else 
    {
      for (n = 0; n < model->getNumUnitDefinitions(); n++)
      {
        if (!strcmp(units, model->getUnitDefinition(n)->getId().c_str()))
        {
          ud = new UnitDefinition();
          
          for (p = 0; p < model->getUnitDefinition(n)->getNumUnits(); p++)
          {
            unit = new Unit(model->getUnitDefinition(n)->getUnit(p)->getKind());
            unit->setMultiplier(model->getUnitDefinition(n)->getUnit(p)->getMultiplier());
            unit->setScale(model->getUnitDefinition(n)->getUnit(p)->getScale());
            unit->setExponent(model->getUnitDefinition(n)->getUnit(p)->getExponent());
            unit->setOffset(model->getUnitDefinition(n)->getUnit(p)->getOffset());

            ud->addUnit(unit);
          }
        }
      }
    }
    /* now check for builtin units 
     * this check is left until now as it is possible for a builtin 
     * unit to be reassigned using a unit definition and thus will have 
     * been picked up above
     */
    if (Unit_isBuiltIn(units) && ud == NULL)
    {
      ud   = new UnitDefinition();

      if (!strcmp(units, "time"))
      {
        unit = new Unit("second");
        ud->addUnit(unit);
      }
    }

  }
  // as a safety catch 
  if (ud == NULL)
  {
    ud = new UnitDefinition();
  }

  return ud;
}

/** 
  * returns 1 if the math contains 
  * a parameter that has undeclared units 0 otherwise
  */
unsigned int 
UnitFormulaFormatter::hasUndeclaredUnits(const ASTNode * node)
{
  undeclaredUnits = 0;
  canIgnoreUndeclaredUnits = 0;

  /* This was assigned but never used -- why?  2007-02-12 <mhucka@caltech.edu>
  UnitDefinition * ud = getUnitDefinition(node);
  */

  return undeclaredUnits;
}
/** 
  * returns canIgnoreUndeclaredUnits value
  */
unsigned int 
UnitFormulaFormatter::getCanIgnoreUndeclaredUnits()
{
  return canIgnoreUndeclaredUnits;
}


/** 
  * returns undeclaredUnits value
  */
unsigned int 
UnitFormulaFormatter::getUndeclaredUnits()
{
  return undeclaredUnits;
}

/** 
  * resets the undeclaredUnits and canIgnoreUndeclaredUnits flags
  * since these will different for each math formula
  */
void 
UnitFormulaFormatter::resetFlags()
{
  undeclaredUnits = 0;
  canIgnoreUndeclaredUnits = 0;
}


