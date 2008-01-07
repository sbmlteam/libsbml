/**
 * @file    UnitFormulaFormatter.cpp
 * @brief   Formats an AST formula tree as a unit definition
 * @author  Sarah Keating
 *
 * $Id$
 * $Source$
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2007 California Institute of Technology.
 * Copyright 2002-2005 California Institute of Technology and
 *                     Japan Science and Technology Corporation.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution and
 * also available online as http://sbml.org/software/libsbml/license.html
 *----------------------------------------------------------------------- -->*/

#include <sbml/units/UnitFormulaFormatter.h>


/**
 *  constructs a UnitFormulaFormatter
 */
UnitFormulaFormatter::UnitFormulaFormatter(const Model *m)
{
  model = (Model *) (m->clone());
  mContainsUndeclaredUnits = false;
  mCanIgnoreUndeclaredUnits = 2;
  depthRecursiveCall = 0;
}

/**
 *  destructor
 */
UnitFormulaFormatter::~UnitFormulaFormatter()
{
  delete model;
}

/**
  * visits the ASTNode and returns the unitDefinition of the formula
  * this function is really a dispatcher to the other
  * UnitFormulaFormatter::getUnitdefinition functions
  */
UnitDefinition * 
UnitFormulaFormatter::getUnitDefinition(const ASTNode * node, 
                                        bool inKL, int reactNo)
{  
  /** 
    * returns a copy of existing UnitDefinition* object (if any) that 
    * corresponds to a given ASTNode*. 
    * (This is for avoiding redundant recursive calls.)
    */

  map<const ASTNode*, UnitDefinition*>::iterator it = unitDefinitionMap.find(node);
  if(it != unitDefinitionMap.end()) {
    return static_cast<UnitDefinition*>(it->second->clone());
  }

    
  UnitDefinition * ud = NULL;

  if (node == NULL)
  {
    return ud;
  }

  ++depthRecursiveCall;


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

      ud = getUnitDefinitionFromDimensionlessReturnFunction(node, inKL, reactNo);
      break;

  /* functions that return same units */
    case AST_PLUS:
    case AST_MINUS:
    case AST_FUNCTION_ABS:
    case AST_FUNCTION_CEILING:
    case AST_FUNCTION_FLOOR:
  
      ud = getUnitDefinitionFromArgUnitsReturnFunction(node, inKL, reactNo);
      break;

  /* power functions */
    case AST_POWER:
    case AST_FUNCTION_POWER:
  
      ud = getUnitDefinitionFromPower(node, inKL, reactNo);
      break;

  /* times functions */
    case AST_TIMES:
  
      ud = getUnitDefinitionFromTimes(node, inKL, reactNo);
      break;

  /* divide functions */
    case AST_DIVIDE:
  
      ud = getUnitDefinitionFromDivide(node, inKL, reactNo);
      break;

  /* piecewise functions */
    case AST_FUNCTION_PIECEWISE:
  
      ud = getUnitDefinitionFromPiecewise(node, inKL, reactNo);
      break;

  /* root functions */
    case AST_FUNCTION_ROOT:
  
      ud = getUnitDefinitionFromRoot(node, inKL, reactNo);
      break;

  /* functions */
    case AST_LAMBDA:
    case AST_FUNCTION:
  
      ud = getUnitDefinitionFromFunction(node, inKL, reactNo);
      break;
    
  /* delay */
    case AST_FUNCTION_DELAY:
  
      ud = getUnitDefinitionFromDelay(node, inKL, reactNo);
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

      ud = getUnitDefinitionFromOther(node, inKL, reactNo);
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

  --depthRecursiveCall;

  if ( depthRecursiveCall != 0 )
  {
    if (unitDefinitionMap.end() == unitDefinitionMap.find(node))
    {
      /* adds a pair of ASTNode* (node) and UnitDefinition* (ud) to the UnitDefinitionMap */
      unitDefinitionMap.insert(pair<const ASTNode*, 
        UnitDefinition*>(node,static_cast<UnitDefinition*>(ud->clone())));
      undeclaredUnitsMap.insert(pair<const ASTNode*, 
        bool>(node,mContainsUndeclaredUnits));
      canIgnoreUndeclaredUnitsMap.insert(pair<const ASTNode*, 
        unsigned int>(node,mCanIgnoreUndeclaredUnits));
    }
  }
  else
  {
    /** 
      * Clears two map objects because all recursive call has finished.
      */ 
    map<const ASTNode*, UnitDefinition*>::iterator it = unitDefinitionMap.begin();
    while( it != unitDefinitionMap.end() )
    {
      delete it->second;
      ++it;
    }
    unitDefinitionMap.clear();
    undeclaredUnitsMap.clear();
    canIgnoreUndeclaredUnitsMap.clear();
  }

  return ud;
}

/** 
  * returns the unitDefinition for the ASTNode from a function
  */
UnitDefinition * 
UnitFormulaFormatter::getUnitDefinitionFromFunction(const ASTNode * node, 
                                        bool inKL, int reactNo)
{ 
  UnitDefinition * ud;
  unsigned int i, nodeCount;
  Unit * unit;
  ASTNode * fdMath;
  // ASTNode *newMath;
  //bool needDelete = false;
  unsigned int noBvars;

  if(node->getType() == AST_FUNCTION)
  {
    const FunctionDefinition *fd = model->getFunctionDefinition(node->getName());
    if (fd)
    {
      noBvars = fd->getNumArguments();
      if (noBvars == 0)
      {
        fdMath = fd->getMath()->getLeftChild()->deepCopy();
      }
      else
      {
        fdMath = fd->getMath()->getRightChild()->deepCopy();
      }

      for (i = 0, nodeCount = 0; i < noBvars; i++, nodeCount++)
      {
        fdMath->ReplaceArgument(fd->getArgument(i)->getName(), 
                                            node->getChild(nodeCount));
      }
      ///**
      //* find corresponding func def which will have
      //* the formula as the rightChild of ASTNode
      //*/
      //fdMath = model->getFunctionDefinition(node->getName())->getMath()
      //                  ->getRightChild();
      ///* if function has no variables then this will be null */
      //if (fdMath == NULL)
      //{
      //  newMath = model->getFunctionDefinition(node->getName())->getMath()
      //                    ->getLeftChild();
      //}
      //else
      //{
      //  /**
      //  * create a new ASTNode of this type but with the children
      //  * from the original function
      //  */

      //  /* need to catch case where a functionDefinition merely returns the argument */
      //  if (fdMath->getType() == AST_NAME)
      //  {
      //    newMath = node->getLeftChild();
      //  }
      //  else
      //  {
      //    newMath = new ASTNode(fdMath->getType());
      //    needDelete = true;
      //    newMath->ReplaceArgument(
      //    nodeCount = 0;
      //    for (i = 0; i < fdMath->getNumChildren(); i++)
      //    {
      //      if (fdMath->getChild(i)->isName())
      //      {
      //        newMath->addChild(node->getChild(nodeCount)->deepCopy());
      //        nodeCount++;
      //      }
      //      else
      //      {
      //        newMath->addChild(fdMath->getChild(i)->deepCopy());
      //      }
      //    }
      //  }
      //}
      ud = getUnitDefinition(fdMath, inKL, reactNo);
      delete fdMath;
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

    delete unit;
  }
  
  return ud;
}

/** 
  * returns the unitDefinition for the ASTNode from a times function
  */
UnitDefinition * 
UnitFormulaFormatter::getUnitDefinitionFromTimes(const ASTNode * node, 
                                        bool inKL, int reactNo)
{ 
  UnitDefinition * ud;
  UnitDefinition * tempUD;
  int numChildren = node->getNumChildren();
  int n = 0;
  unsigned int i;
  unsigned int currentIgnore = mCanIgnoreUndeclaredUnits;;

  ud = getUnitDefinition(node->getChild(n), inKL, reactNo);
  if (mCanIgnoreUndeclaredUnits == 0) currentIgnore = 0;

  for(n = 1; n < numChildren; n++)
  {
    tempUD = getUnitDefinition(node->getChild(n), inKL, reactNo);
    if (mCanIgnoreUndeclaredUnits == 0) currentIgnore = 0;
    for (i = 0; i < tempUD->getNumUnits(); i++)
    {
      ud->addUnit(tempUD->getUnit(i));
    }
    delete tempUD;
  }

  mCanIgnoreUndeclaredUnits = currentIgnore;
  return ud;
}

/** 
  * returns the unitDefinition for the ASTNode from a divide function
  */
UnitDefinition * 
UnitFormulaFormatter::getUnitDefinitionFromDivide(const ASTNode * node, 
                                        bool inKL, int reactNo)
{ 
  UnitDefinition * ud;
  UnitDefinition * tempUD;
  unsigned int i;
  Unit * unit;

  ud = getUnitDefinition(node->getLeftChild(), inKL, reactNo);

  if (node->getNumChildren() == 1)
    return ud;
  tempUD = getUnitDefinition(node->getRightChild(), inKL, reactNo);
  for (i = 0; i < tempUD->getNumUnits(); i++)
  {
    unit = tempUD->getUnit(i);
    unit->setExponent(-1 * unit->getExponent());
    ud->addUnit(unit);
  }
  delete tempUD;

  return ud;
}

/** 
  * returns the unitDefinition for the ASTNode from a power function
  */
UnitDefinition * 
UnitFormulaFormatter::getUnitDefinitionFromPower(const ASTNode * node,
                                                 bool inKL, int reactNo)
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
  unsigned int newExp = 0;
  Unit * unit;
  ASTNode * child;
  unsigned int found = 0;

  tempUD = getUnitDefinition(node->getLeftChild(), inKL, reactNo);
  ud = new UnitDefinition();
  
  if (node->getNumChildren() == 1)
    return ud;

  /** 
    * if the value set is not an integer need to flag as not to check units
    */
  double value = 0.0;

  child = node->getRightChild();
  for (i = 0; i < tempUD->getNumUnits(); i++)
  {
    unit = tempUD->getUnit(i);
    // need to check type
    if (child->isInteger()) 
    {
      unit->setExponent(child->getInteger() * unit->getExponent());
    }
    else if (child->isName())
    {
      /**
       * if the child is a name then it will be a parameter 
       * and may be global or local if we are in a kineticLaw
       * (possible a species/compartment but unlikely)
       */
      if (inKL == 1)
      {
        if (model->getReaction(reactNo)->
          getKineticLaw()->getParameter(child->getName()))
        {
          value = (model->getReaction(reactNo)->
            getKineticLaw()->getParameter(child->getName()))->getValue();
          found = 1;
        }
      }
      
      if (found == 0) // not local parameter
      {
        if (model->getParameter(child->getName()))
        {
          value = model->getParameter(child->getName())->getValue();
        }
        else if (model->getCompartment(child->getName()))
        {
          value = model->getCompartment(child->getName())->getSize();
        }
        else if (model->getSpecies(child->getName()))
        {
          value = model->getSpecies(child->getName())->getInitialConcentration();
        }

      }
      
      if (floor(value) != value)
        mContainsUndeclaredUnits = true;
      
      newExp = (int) (value);

      unit->setExponent(newExp * unit->getExponent());
    }
    else if (child->isReal())
    {
      value = child->getReal();
      if (floor(value) != value)
        mContainsUndeclaredUnits = true;
      newExp = (int) (value);
      
      unit->setExponent(newExp * unit->getExponent());
    }
    ud->addUnit(unit);
  }

  delete tempUD;

  return ud;
}

/** 
  * returns the unitDefinition for the ASTNode from 
  * a piecewise function
  */
UnitDefinition * 
UnitFormulaFormatter::getUnitDefinitionFromPiecewise(const ASTNode * node, 
                                        bool inKL, int reactNo)
{ 
  UnitDefinition * ud;
  
  ud = getUnitDefinition(node->getLeftChild(), inKL, reactNo);

  return ud;
}

/** 
  * returns the unitDefinition for the ASTNode from a root function
  */
UnitDefinition * 
UnitFormulaFormatter::getUnitDefinitionFromRoot(const ASTNode * node, 
                                        bool inKL, int reactNo)
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

  tempUD = getUnitDefinition(node->getRightChild(), inKL, reactNo);
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

  delete tempUD;

  return ud;
}

/** 
  * returns the unitDefinition for the ASTNode from 
  * a delay function
  */
UnitDefinition * 
UnitFormulaFormatter::getUnitDefinitionFromDelay(const ASTNode * node, 
                                        bool inKL, int reactNo)
{ 
  UnitDefinition * ud;
  
  ud = getUnitDefinition(node->getLeftChild(), inKL, reactNo);

  return ud;
}

/** 
  * returns the unitDefinition for the ASTNode from 
  * a function returning dimensionless value
  */
UnitDefinition * 
UnitFormulaFormatter::getUnitDefinitionFromDimensionlessReturnFunction(
                                const ASTNode *node, bool inKL, int reactNo)
{ 
  UnitDefinition * ud;
  Unit *unit;
    
  unit = new Unit("dimensionless");
  ud   = new UnitDefinition();
    
  ud->addUnit(unit);

  delete unit;

  return ud;
}

/** 
  * returns the unitDefinition for the ASTNode from 
  * a function returning value with same units as argument(s)
  */
UnitDefinition * 
UnitFormulaFormatter::getUnitDefinitionFromArgUnitsReturnFunction(const ASTNode * node, 
                                        bool inKL, int reactNo)
{ 
  UnitDefinition * ud;
  UnitDefinition * tempUd;
  unsigned int i = 0;
  unsigned int n = 0;
 
  /* save any existing value of undeclaredUnits/canIgnoreUndeclaredUnits */
  unsigned int originalIgnore = mCanIgnoreUndeclaredUnits;
  bool originalUndeclaredValue = mContainsUndeclaredUnits;
  unsigned int currentIgnore = mCanIgnoreUndeclaredUnits;
  bool currentUndeclared = mContainsUndeclaredUnits;

  /* get first arg that is not a parameter with undeclared units */
  ud = getUnitDefinition(node->getChild(i), inKL, reactNo);
  while (getContainsUndeclaredUnits() && i < node->getNumChildren()-1)
  {
    if (originalUndeclaredValue == 1)
      currentIgnore = 0;
    else
      currentIgnore = 1;

    currentUndeclared = 1;

    i++;
    delete ud;
    resetFlags();
    ud = getUnitDefinition(node->getChild(i), inKL, reactNo);
  }

  /* loop thru remain children to determine undeclaredUnit status */
  if (mContainsUndeclaredUnits && i == node->getNumChildren()-1)
  {
    /* all children are parameters with undeclared units */
    currentIgnore = 0;
  }
  else
  {
    for (n = i+1; n < node->getNumChildren(); n++)
    {
      resetFlags();
      tempUd = getUnitDefinition(node->getChild(n), inKL, reactNo);
      if (getContainsUndeclaredUnits())
      {
        currentUndeclared = 1;
        currentIgnore = 1;
      }
      delete tempUd;
    }
  }

  /* restore original value of undeclaredUnits */
  mContainsUndeclaredUnits = currentUndeclared;
  /* temporary HACK while I figure this out */
  if (originalIgnore == 2)
  {
    mCanIgnoreUndeclaredUnits = currentIgnore;
  }
  


  return ud;
}

/** 
  * returns the unitDefinition for the ASTNode from anything else
  */
UnitDefinition * 
UnitFormulaFormatter::getUnitDefinitionFromOther(const ASTNode * node,
    bool inKL, int reactNo)
{ 
  UnitDefinition * ud = NULL;
  Unit * unit;

  unsigned int n, found;

  const KineticLaw * kl;

  /** 
   * ASTNode represents a number, a constant, TIME, DELAY, or
   * the name of another element of the model
   */

  if ((node->isNumber()) || (node->getType() == AST_CONSTANT_E))
  {
    ud   = new UnitDefinition();
    mContainsUndeclaredUnits = true;
    mCanIgnoreUndeclaredUnits = 0;
  }
  else if (node->getType() == AST_CONSTANT_PI)
  {
    unit = new Unit("radian");
    ud   = new UnitDefinition();
    
    ud->addUnit(unit);
    delete unit;
  }
  else if (node->isName())
  {
    if (node->getType() == AST_NAME_TIME)
    {
      unit = new Unit("second");
      ud   = new UnitDefinition();
      
      ud->addUnit(unit);
      delete unit;
    }
    /* must be the name of a compartment, species or parameter */
    else
    {
      found = 0;
      n = 0;
      if (inKL)
      {
        if (model->getReaction(reactNo)->isSetKineticLaw())
        {
          kl = model->getReaction(reactNo)->getKineticLaw();
          ud = getUnitDefinitionFromParameter(kl->getParameter(node->getName()));
          if (ud)
          {
            found = 1;
          }
        }
      }
      if (found == 0)// && n < model->getNumCompartments())
      {
        ud = getUnitDefinitionFromCompartment(model->getCompartment(node->getName()));
        if (ud)
        {
          found = 1;
        }
      }

      if (found == 0)//&& n < model->getNumSpecies())
      {
        ud = getUnitDefinitionFromSpecies(model->getSpecies(node->getName()));
        if (ud)
        {
          found = 1;
        }
      }

      if (found == 0 )//&& n < model->getNumParameters())
      {
        ud = getUnitDefinitionFromParameter(model->getParameter(node->getName()));
        if (ud)
        {
          found = 1;
        }
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
UnitFormulaFormatter::getUnitDefinitionFromCompartment
                                             (const Compartment * compartment)
{
  if (!compartment)
  {
    return NULL;
  }

  UnitDefinition * ud = NULL;
  const UnitDefinition * tempUD;
  Unit * unit = NULL;
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

    delete unit;
  }
  else
  {
    /* units can be a predefined unit kind
    * a unit definition id or a builtin unit
    */
    if (UnitKind_isValidUnitKindString(units, compartment->getLevel(), compartment->getVersion()))
    {
      unit = new Unit(units);
      ud   = new UnitDefinition();
      
      ud->addUnit(unit);

      delete unit;
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

            delete unit;
          }
        }
      }
    }
    /* now check for builtin units 
     * this check is left until now as it is possible for a builtin 
     * unit to be reassigned using a unit definition and thus will have 
     * been picked up above
     */
    if (Unit_isBuiltIn(units, model->getLevel()) && ud == NULL)
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

      delete unit;
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
  if (!species)
  {
    return NULL;
  }
  
  UnitDefinition * ud = NULL;
  const UnitDefinition * tempUd;
  UnitDefinition *subsUD = NULL;
  UnitDefinition *sizeUD = NULL;
  Unit * unit = NULL;
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

    delete unit;
  }
  else
  {
    /* units can be a predefined unit kind
    * a unit definition id or a builtin unit
    */
    if (UnitKind_isValidUnitKindString(units, species->getLevel(), species->getVersion()))
    {
      unit = new Unit(units);
      subsUD   = new UnitDefinition();
      
      subsUD->addUnit(unit);

      delete unit;
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

            delete unit;
          }
        }
      }
    }
    /* now check for builtin units 
     * this check is left until now as it is possible for a builtin 
     * unit to be reassigned using a unit definition and thus will have 
     * been picked up above
     */
    if (Unit_isBuiltIn(units, model->getLevel()) && subsUD == NULL)
    {
      subsUD   = new UnitDefinition();

      if (!strcmp(units, "substance"))
      {
        unit = new Unit("mole");
        subsUD->addUnit(unit);

        delete unit;
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
    if (UnitKind_isValidUnitKindString(spatialUnits, species->getLevel(), 
                                                     species->getVersion()))
    {
      unit = new Unit(spatialUnits);
      sizeUD   = new UnitDefinition();
      
      sizeUD->addUnit(unit);

      delete unit;
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

            delete unit;
          }
        }
      }
    }
    /* now check for builtin units 
     * this check is left until now as it is possible for a builtin 
     * unit to be reassigned using a unit definition and thus will have 
     * been picked up above
     */
    if (Unit_isBuiltIn(spatialUnits, model->getLevel()) && sizeUD == NULL)
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

      delete unit;
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

  delete sizeUD;

  return ud;
}

/** 
  * returns the unitDefinition for the units of the parameter
  */
UnitDefinition * 
UnitFormulaFormatter::getUnitDefinitionFromParameter(const Parameter * parameter)
{
  if (!parameter)
  {
    return NULL;
  }

  UnitDefinition * ud = NULL;
  Unit * unit = NULL;
  unsigned int n, p;

  const char * units = parameter->getUnits().c_str();

 /* no units declared */
  if (!strcmp(units, ""))
  {
    ud   = new UnitDefinition();
    mContainsUndeclaredUnits = true;
    mCanIgnoreUndeclaredUnits = 0;
  }
  else
  {
    /* units can be a predefined unit kind
    * a unit definition id or a builtin unit
    */

    if (UnitKind_isValidUnitKindString(units, parameter->getLevel(), parameter->getVersion()))
    {
      unit = new Unit(units);
      ud   = new UnitDefinition();
      
      ud->addUnit(unit);

      delete unit;
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

            delete unit;
          }
        }
      }
    }
    /* now check for builtin units 
     * this check is left until now as it is possible for a builtin 
     * unit to be reassigned using a unit definition and thus will have 
     * been picked up above
     */
    if (Unit_isBuiltIn(units, model->getLevel()) && ud == NULL)
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

      delete unit;
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
  if (!event)
  {
    return NULL;
  }
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

      delete unit;
    }
  }
  else
  {
    /* units can be a predefined unit kind
    * a unit definition id or a builtin unit
    */

    if (UnitKind_isValidUnitKindString(units, event->getLevel(), event->getVersion()))
    {
      unit = new Unit(units);
      ud   = new UnitDefinition();
      
      ud->addUnit(unit);

      delete unit;
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

            delete unit;
          }
        }
      }
    }
    /* now check for builtin units 
     * this check is left until now as it is possible for a builtin 
     * unit to be reassigned using a unit definition and thus will have 
     * been picked up above
     */
    if (Unit_isBuiltIn(units, model->getLevel()) && ud == NULL)
    {
      ud   = new UnitDefinition();

      if (!strcmp(units, "time"))
      {
        unit = new Unit("second");
        ud->addUnit(unit);

        delete unit;
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
  * returns canIgnoreUndeclaredUnits value
  */
bool 
UnitFormulaFormatter::getCanIgnoreUndeclaredUnits()
{
  if (mCanIgnoreUndeclaredUnits == 2
    || mCanIgnoreUndeclaredUnits == 0)
    return false;
  else
    return true;
}


/** 
  * returns undeclaredUnits value
  */
bool 
UnitFormulaFormatter::getContainsUndeclaredUnits()
{
  return mContainsUndeclaredUnits;
}

/** 
  * resets the undeclaredUnits and canIgnoreUndeclaredUnits flags
  * since these will different for each math formula
  */
void 
UnitFormulaFormatter::resetFlags()
{
  mContainsUndeclaredUnits = false;
  mCanIgnoreUndeclaredUnits = 2;
}
