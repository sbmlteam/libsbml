/**
 * \file    UnitFormulaFormatter.cpp
 * \brief   Formats an AST formula tree as a unit definition
 * \author  Sarah Keating
 *
 * $Id$
 * $Source$
 */
/* Copyright 2003 California Institute of Technology and
 * Japan Science and Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 2.1 of the License, or
 * any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY, WITHOUT EVEN THE IMPLIED WARRANTY OF
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  The software and
 * documentation provided hereunder is on an "as is" basis, and the
 * California Institute of Technology and Japan Science and Technology
 * Corporation have no obligations to provide maintenance, support,
 * updates, enhancements or modifications.  In no event shall the
 * California Institute of Technology or the Japan Science and Technology
 * Corporation be liable to any party for direct, indirect, special,
 * incidental or consequential damages, including lost profits, arising
 * out of the use of this software and its documentation, even if the
 * California Institute of Technology and/or Japan Science and Technology
 * Corporation have been advised of the possibility of such damage.  See
 * the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 *
 * The original code contained here was initially developed by:
 *
 *     Sarah Keating
 *
 *     The SBML Team
 *     Science and Technology Research Institute
 *     University of Hertfordshire
 *     Hatfield, UK
 *
 *     http://sbml.org
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s):
 */


#include "UnitFormulaFormatter.h"

/**
 *  constructs a UnitFormulaFormatter
 */
LIBSBML_EXTERN
UnitFormulaFormatter::UnitFormulaFormatter(const Model *m)
{
  model = m;
  undeclaredUnits = 0;
}

/**
 *  destructor
 */
LIBSBML_EXTERN
UnitFormulaFormatter::~UnitFormulaFormatter()
{
}
/** 
 * returns true (non-zero) if math represents a function
 */ 
int 
UnitFormulaFormatter::isFunction(const ASTNode * node)
{
  return
    node->isFunction   () ||
    node->isLambda     () ||
    node->isLogical    () ||
    node->isOperator   () ||
    node->isRelational ();
}

/** 
 * returns true (non-zero) if node represents an inverse trigonometric function 
 */ 
int 
UnitFormulaFormatter::isInverseTrigFunction(const ASTNode * node)
{  
  int inverseTrig = 0;
  ASTNodeType_t type = node->getType();

  if ((type == AST_FUNCTION_ARCCOS) ||
      (type == AST_FUNCTION_ARCCOT) ||
      (type == AST_FUNCTION_ARCCSC) ||
      (type == AST_FUNCTION_ARCSEC) ||
      (type == AST_FUNCTION_ARCSIN) ||
      (type == AST_FUNCTION_ARCTAN)) 
  {
    inverseTrig = 1;
  }

  return inverseTrig;
}

/** 
 * returns true (non-zero) if node represents a times function 
 */ 
int 
UnitFormulaFormatter::isTimes(const ASTNode * node)
{
  return (node->getType() == AST_TIMES);
}

/** 
 * returns true (non-zero) if node represents a division function 
 */ 
int 
UnitFormulaFormatter::isDivide(const ASTNode * node)
{
  return (node->getType() == AST_DIVIDE);
}

/** 
 * returns true (non-zero) if node represents a power function  
 */ 
int 
UnitFormulaFormatter::isPower(const ASTNode * node)
{
  int power = 0;
  ASTNodeType_t type = node->getType();

  if ((type == AST_POWER) ||
      (type == AST_FUNCTION_POWER)) 
  {
    power = 1;
  }

  return power;
}

/** 
 * returns true (non-zero) if node represents a piecewise function 
 */ 
int 
UnitFormulaFormatter::isPiecewise(const ASTNode * node)
{
  return (node->getType() == AST_FUNCTION_PIECEWISE);
}

/**
 * returns true (non-zero) if node represents a root function 
 */ 
int 
UnitFormulaFormatter::isRoot(const ASTNode * node)
{
  return (node->getType() == AST_FUNCTION_ROOT);
}

/**
 * returns true (non-zero) if node represents a function 
 * that has a dimensionless return value
 */ 
int 
UnitFormulaFormatter::isDimensionlessReturnFunction(const ASTNode * node)
{
  int dimensionless = 0;
  ASTNodeType_t type = node->getType();

  /* inverse hyperbolic functions */
  if ((type == AST_FUNCTION_ARCCOSH) ||
      (type == AST_FUNCTION_ARCCOTH) ||
      (type == AST_FUNCTION_ARCCSCH) ||
      (type == AST_FUNCTION_ARCSECH) ||
      (type == AST_FUNCTION_ARCSINH) ||
      (type == AST_FUNCTION_ARCTANH)) 
  {
    dimensionless = 1;
  }
  /* hyperbolic functions */
  else if  ((type == AST_FUNCTION_COSH) ||
            (type == AST_FUNCTION_COTH) ||
            (type == AST_FUNCTION_CSCH) ||
            (type == AST_FUNCTION_SECH) ||
            (type == AST_FUNCTION_SINH) ||
            (type == AST_FUNCTION_TANH)) 
  {
    dimensionless = 1;
  }
  /* trigonometry functions */
  else if  ((type == AST_FUNCTION_COS) ||
            (type == AST_FUNCTION_COT) ||
            (type == AST_FUNCTION_CSC) ||
            (type == AST_FUNCTION_SEC) ||
            (type == AST_FUNCTION_SIN) ||
            (type == AST_FUNCTION_TAN)) 
  {
    dimensionless = 1;
  }
  /* logarithmic functions */
  else if  ((type == AST_FUNCTION_EXP) ||
            (type == AST_FUNCTION_LN) ||
            (type == AST_FUNCTION_LOG)) 
  {
    dimensionless = 1;
  }

  return dimensionless;
}

/** 
 * returns true (non-zero) if node represents a function 
 * that has same units as the function arguments
 */ 
int 
UnitFormulaFormatter::isArgUnitsReturnFunction(const ASTNode * node)
{
  int sameUnits = 0;
  ASTNodeType_t type = node->getType();

  /* value functions */
  if ((type == AST_FUNCTION_ABS)     ||
      (type == AST_FUNCTION_CEILING) ||
      (type == AST_FUNCTION_FLOOR)) 
  {
    sameUnits = 1;
  }
  /* operator functions */
  else if ((type == AST_PLUS) ||
           (type == AST_MINUS)) 
  {
    sameUnits = 1;
  }

  return sameUnits;
}

/**
  * visits the ASTNode and returns the unitDefinition of the formula
  * this function is really a dispatcher to the other
  * UnitFormulaFormatter::getUnitdefinition functions
  */
LIBSBML_EXTERN
UnitDefinition * 
UnitFormulaFormatter::getUnitDefinition(const ASTNode * node)
{  
  UnitDefinition * ud;

  if (node->getType() == AST_UNKNOWN)
  {
    ud = new UnitDefinition("unknown");
  }
  else if (node->isBoolean())
  {
    ud = getUnitDefinitionFromDimensionlessReturnFunction(node);
  }
  else if (isDimensionlessReturnFunction(node))
  {
    ud = getUnitDefinitionFromDimensionlessReturnFunction(node);
  }
  else if (isInverseTrigFunction(node))
  {
    ud = getUnitDefinitionFromDimensionlessReturnFunction(node);
  }
  else if (isArgUnitsReturnFunction(node))
  {
    ud = getUnitDefinitionFromArgUnitsReturnFunction(node);
  }
  else if (isPower(node))
  {
    ud = getUnitDefinitionFromPower(node);
  }
  else if (isTimes(node))
  {
    ud = getUnitDefinitionFromTimes(node);
  }
  else if (isDivide(node))
  {
    ud = getUnitDefinitionFromDivide(node);
  }
  else if (isPiecewise(node))
  {
    ud = getUnitDefinitionFromPiecewise(node);
  }
  else if (isRoot(node))
  {
    ud = getUnitDefinitionFromRoot(node);
  }
  else if (isFunction(node))
  {
    ud = getUnitDefinitionFromFunction(node);
  }
  else
  {
    ud = getUnitDefinitionFromOther(node);
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
  unsigned int n, i;
  Unit * unit;
  const ASTNode * fdMath;
  ASTNode *newMath;

  if(node->getType() == AST_FUNCTION)
  {
    /**
     * find corresponding func def which will have
     * the formula as the rightChild of ASTNode
     */
    for (n = 0; n < model->getNumFunctionDefinitions(); n++)
    {
      if (!strcmp(node->getName(), model->getFunctionDefinition(n)->getId().c_str()))
      {
        fdMath = model->getFunctionDefinition(n)->getMath()->getRightChild();
        
        /**
         * create a new ASTNode of this type but with the children
         * from the original function
         */
        newMath = new ASTNode(fdMath->getType());
        for (i = 0; i < node->getNumChildren(); i++)
        {
          newMath->addChild(node->getChild(i));
        }
        
        ud = getUnitDefinition(newMath);
      }
      break;
    }
  }
  else
  {
    /**
     * function is a lambda function - which wont have any units
     */
    unit = new Unit("dimensionless");
    ud   = new UnitDefinition("lambda_function");
    
    ud->addUnit(*unit);
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
      ud->addUnit(*(tempUD->getUnit(i)));
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

  tempUD = getUnitDefinition(node->getRightChild());
  for (i = 0; i < tempUD->getNumUnits(); i++)
  {
    unit = tempUD->getUnit(i);
    unit->setExponent(-1 * unit->getExponent());
    ud->addUnit(*unit);
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
  ud = new UnitDefinition("power");

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
    ud->addUnit(*unit);
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
  ud = new UnitDefinition("root");

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
    ud->addUnit(*unit);
  }

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
  ud   = new UnitDefinition("dimless_ret_func");
    
  ud->addUnit(*unit);

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
 
  /* get first arg that is not a parameter with undeclared units */
  ud = getUnitDefinition(node->getChild(i));
  while (hasUndeclaredUnits(node->getChild(i)))
  {
    i++;
    ud = getUnitDefinition(node->getChild(i));
  }

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

  KineticLaw * kl;

  /** 
   * ASTNode represents a number, a constant, TIME, DELAY, or
   * the name of another element of the model
   */

  if ((node->isNumber()) || (node->getType() == AST_CONSTANT_E))
  {
    unit = new Unit("dimensionless");
    ud   = new UnitDefinition("number");
    
    ud->addUnit(*unit);
  }
  else if (node->getType() == AST_CONSTANT_PI)
  {
    unit = new Unit("radian");
    ud   = new UnitDefinition("PI");
    
    ud->addUnit(*unit);
  }
  else if (node->isName())
  {
    if (node->getType() == AST_NAME_TIME)
    {
      unit = new Unit("second");
      ud   = new UnitDefinition("time");
      
      ud->addUnit(*unit);
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
    ud = new UnitDefinition("empty");
  }
  return ud;
}

/** 
  * returns the unitDefinition for the units of the compartment
  */
LIBSBML_EXTERN
UnitDefinition * 
UnitFormulaFormatter::getUnitDefinitionFromCompartment(const Compartment * compartment)
{
  UnitDefinition * ud = NULL;
  UnitDefinition * tempUD;
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
        ud   = new UnitDefinition(compartment->getId());
      
        ud->addUnit(*unit);
        break;
      case 1: 
        /* check for builtin unit length redefined */
        tempUD = model->getUnitDefinition("length");
        if (!tempUD) 
        {
          unit = new Unit("metre");
          ud   = new UnitDefinition(compartment->getId());
        
          ud->addUnit(*unit);
        }
        else
        {
          ud   = new UnitDefinition("length");

          unit = new Unit(tempUD->getUnit(0)->getKind());
          unit->setMultiplier(tempUD->getUnit(0)->getMultiplier());
          unit->setScale(tempUD->getUnit(0)->getScale());
          unit->setExponent(tempUD->getUnit(0)->getExponent());
          unit->setOffset(tempUD->getUnit(0)->getOffset());

          ud->addUnit(*unit);
        }
        break;
      case 2:
        /* check for builtin unit area redefined */
        tempUD = model->getUnitDefinition("area");
        if (!tempUD) 
        {
          unit = new Unit("metre", 2);
          ud   = new UnitDefinition(compartment->getId());
          
          ud->addUnit(*unit);
        }
        else
        {
          ud   = new UnitDefinition("area");

          unit = new Unit(tempUD->getUnit(0)->getKind());
          unit->setMultiplier(tempUD->getUnit(0)->getMultiplier());
          unit->setScale(tempUD->getUnit(0)->getScale());
          unit->setExponent(tempUD->getUnit(0)->getExponent());
          unit->setOffset(tempUD->getUnit(0)->getOffset());

          ud->addUnit(*unit);
        }
        break;
      default:
        /* check for builtin unit volume redefined */
        tempUD = model->getUnitDefinition("volume");
        if (!tempUD) 
        {
          unit = new Unit("litre");
          ud   = new UnitDefinition(compartment->getId());
        
          ud->addUnit(*unit);
        }
        else
        {
          ud   = new UnitDefinition("volume");

          unit = new Unit(tempUD->getUnit(0)->getKind());
          unit->setMultiplier(tempUD->getUnit(0)->getMultiplier());
          unit->setScale(tempUD->getUnit(0)->getScale());
          unit->setExponent(tempUD->getUnit(0)->getExponent());
          unit->setOffset(tempUD->getUnit(0)->getOffset());

          ud->addUnit(*unit);
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
      ud   = new UnitDefinition(compartment->getId());
      
      ud->addUnit(*unit);
    }
    else 
    {
      for (n = 0; n < model->getNumUnitDefinitions(); n++)
      {
        if (!strcmp(units, model->getUnitDefinition(n)->getId().c_str()))
        {
          ud = new UnitDefinition(model->getUnitDefinition(n)->getId(), 
            model->getUnitDefinition(n)->getName());
          
          for (p = 0; p < model->getUnitDefinition(n)->getNumUnits(); p++)
          {
            unit = new Unit(model->getUnitDefinition(n)->getUnit(p)->getKind());
            unit->setMultiplier(model->getUnitDefinition(n)->getUnit(p)->getMultiplier());
            unit->setScale(model->getUnitDefinition(n)->getUnit(p)->getScale());
            unit->setExponent(model->getUnitDefinition(n)->getUnit(p)->getExponent());
            unit->setOffset(model->getUnitDefinition(n)->getUnit(p)->getOffset());

            ud->addUnit(*unit);
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
      ud   = new UnitDefinition(compartment->getId());

      if (!strcmp(units, "volume"))
      {
        unit = new Unit("litre");
        ud->addUnit(*unit);
      }
      else if (!strcmp(units, "area"))
      {
        unit = new Unit("metre", 2);
        ud->addUnit(*unit);
      }
      else if (!strcmp(units, "length"))
      {
        unit = new Unit("metre");
        ud->addUnit(*unit);
      }
    }
  }

  return ud;
}

/** 
  * returns the unitDefinition for the units of the species
  */
LIBSBML_EXTERN
UnitDefinition * 
UnitFormulaFormatter::getUnitDefinitionFromSpecies(const Species * species)
{
  UnitDefinition * ud;
  UnitDefinition *subsUD = NULL;
  UnitDefinition *sizeUD = NULL;
  Unit * unit;
  Compartment * c;
  unsigned int n, p;

  const char * units        = species->getSubstanceUnits().c_str();
  const char * spatialUnits = species->getSpatialSizeUnits().c_str();


  /* deal with substance units */
 
  /* no units declared implies they default to the value substance */
  if (!strcmp(units, ""))
  {
    /* check for builtin unit substance redefined */
    ud = model->getUnitDefinition("substance");
    if (!ud) 
    {
      unit = new Unit("mole");
      subsUD   = new UnitDefinition("species_subs");

      subsUD->addUnit(*unit);
    }
    else
    {
      subsUD   = new UnitDefinition("substance");

      unit = new Unit(ud->getUnit(0)->getKind());
      unit->setMultiplier(ud->getUnit(0)->getMultiplier());
      unit->setScale(ud->getUnit(0)->getScale());
      unit->setExponent(ud->getUnit(0)->getExponent());
      unit->setOffset(ud->getUnit(0)->getOffset());

      subsUD->addUnit(*unit);

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
      subsUD   = new UnitDefinition("species_subs");
      
      subsUD->addUnit(*unit);
    }
    else 
    {
      for (n = 0; n < model->getNumUnitDefinitions(); n++)
      {
        if (!strcmp(units, model->getUnitDefinition(n)->getId().c_str()))
        {
          subsUD = new UnitDefinition("species_subs");
          
          for (p = 0; p < model->getUnitDefinition(n)->getNumUnits(); p++)
          {
            unit = new Unit(model->getUnitDefinition(n)->getUnit(p)->getKind());
            unit->setMultiplier(model->getUnitDefinition(n)->getUnit(p)->getMultiplier());
            unit->setScale(model->getUnitDefinition(n)->getUnit(p)->getScale());
            unit->setExponent(model->getUnitDefinition(n)->getUnit(p)->getExponent());
            unit->setOffset(model->getUnitDefinition(n)->getUnit(p)->getOffset());

            subsUD->addUnit(*unit);
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
      subsUD   = new UnitDefinition("species_subs");

      if (!strcmp(units, "substance"))
      {
        unit = new Unit("mole");
        subsUD->addUnit(*unit);
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
      sizeUD   = new UnitDefinition("species_subs");
      
      sizeUD->addUnit(*unit);
    }
    else 
    {
      for (n = 0; n < model->getNumUnitDefinitions(); n++)
      {
        if (!strcmp(spatialUnits, model->getUnitDefinition(n)->getId().c_str()))
        {
          sizeUD = new UnitDefinition("species_subs");
          
          for (p = 0; p < model->getUnitDefinition(n)->getNumUnits(); p++)
          {
            unit = new Unit(model->getUnitDefinition(n)->getUnit(p)->getKind());
            unit->setMultiplier(model->getUnitDefinition(n)->getUnit(p)->getMultiplier());
            unit->setScale(model->getUnitDefinition(n)->getUnit(p)->getScale());
            unit->setExponent(model->getUnitDefinition(n)->getUnit(p)->getExponent());
            unit->setOffset(model->getUnitDefinition(n)->getUnit(p)->getOffset());

            sizeUD->addUnit(*unit);
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
      sizeUD   = new UnitDefinition("species_subs");

      if (!strcmp(spatialUnits, "volume"))
      {
        unit = new Unit("litre");
        sizeUD->addUnit(*unit);
      }
      else if (!strcmp(spatialUnits, "area"))
      {
        unit = new Unit("metre", 2);
        sizeUD->addUnit(*unit);
      }
      else if (!strcmp(spatialUnits, "length"))
      {
        unit = new Unit("metre");
        sizeUD->addUnit(*unit);
      }
    }
  }

  /* units of the species are units substance/size */
  ud = subsUD;

  for (n = 0; n < sizeUD->getNumUnits(); n++)
  {
    unit = sizeUD->getUnit(n);
    unit->setExponent(-1 * unit->getExponent());

    ud->addUnit(*unit);
  }

  return ud;
}

/** 
  * returns the unitDefinition for the units of the parameter
  */
LIBSBML_EXTERN
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
    ud   = new UnitDefinition(parameter->getId());
    undeclaredUnits = 1;
  }
  else
  {
    /* units can be a predefined unit kind
    * a unit definition id or a builtin unit
    */

    if (UnitKind_isValidUnitKindString(units))
    {
      unit = new Unit(units);
      ud   = new UnitDefinition(parameter->getId());
      
      ud->addUnit(*unit);
    }
    else 
    {
      for (n = 0; n < model->getNumUnitDefinitions(); n++)
      {
        if (!strcmp(units, model->getUnitDefinition(n)->getId().c_str()))
        {
          ud = new UnitDefinition(model->getUnitDefinition(n)->getId(), 
            model->getUnitDefinition(n)->getName());
          
          for (p = 0; p < model->getUnitDefinition(n)->getNumUnits(); p++)
          {
            unit = new Unit(model->getUnitDefinition(n)->getUnit(p)->getKind());
            unit->setMultiplier(model->getUnitDefinition(n)->getUnit(p)->getMultiplier());
            unit->setScale(model->getUnitDefinition(n)->getUnit(p)->getScale());
            unit->setExponent(model->getUnitDefinition(n)->getUnit(p)->getExponent());
            unit->setOffset(model->getUnitDefinition(n)->getUnit(p)->getOffset());

            ud->addUnit(*unit);
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
      ud   = new UnitDefinition(parameter->getId());

      if (!strcmp(units, "substance"))
      {
        unit = new Unit("mole");
        ud->addUnit(*unit);
      }
      else if (!strcmp(units, "volume"))
      {
        unit = new Unit("litre");
        ud->addUnit(*unit);
      }
      else if (!strcmp(units, "area"))
      {
        unit = new Unit("metre", 2);
        ud->addUnit(*unit);
      }
      else if (!strcmp(units, "length"))
      {
        unit = new Unit("metre");
        ud->addUnit(*unit);
      }
      else if (!strcmp(units, "time"))
      {
        unit = new Unit("second");
        ud->addUnit(*unit);
      }
    }

  }

  return ud;
}

/** 
  * returns 1 if the math contains 
  * a parameter that has undeclared units 0 otherwise
  */
LIBSBML_EXTERN
unsigned int 
UnitFormulaFormatter::hasUndeclaredUnits(const ASTNode * node)
{
  undeclaredUnits = 0;
  UnitDefinition * ud = getUnitDefinition(node);

  return undeclaredUnits;
}



