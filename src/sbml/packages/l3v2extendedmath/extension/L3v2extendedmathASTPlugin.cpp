/**
 * @file L3v2extendedmathASTPlugin.cpp
 * @brief Implementation of the L3v2extendedmathASTPlugin class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2019 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 * 3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 * Pasadena, CA, USA
 *
 * Copyright (C) 2002-2005 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. Japan Science and Technology Agency, Japan
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation. A copy of the license agreement is provided in the
 * file named "LICENSE.txt" included with this software distribution and also
 * available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 */
#include <sbml/packages/l3v2extendedmath/extension/L3v2extendedmathASTPlugin.h>
#include <sbml/math/ASTNode.h>
#include <sbml/SBMLNamespaces.h>
#include <sbml/units/UnitFormulaFormatter.h>
#include <sbml/Unit.h>
#include <sbml/validator/constraints/ArgumentsUnitsCheck.h>
#include <sbml/packages/l3v2extendedmath/extension/L3v2extendedmathExtension.h>

/** @cond doxygenIgnored */

using namespace std;

/** @endcond */
LIBSBML_CPP_NAMESPACE_BEGIN

#ifdef __cplusplus


void
L3v2extendedmathASTPlugin::populateNodeTypes()
{
  vector<unsigned int> zero, one, two;
  zero.push_back(0);
  one.push_back(1);
  two.push_back(2);

  ASTNodeValues_t node;
  node.type = AST_FUNCTION_MAX;
  node.name = "max";
  node.csymbolURL = "";
  node.isFunction = true;
  node.allowedChildrenType = ALLOWED_CHILDREN_ANY;
  node.numAllowedChildren = zero;

  mPkgASTNodeValues.push_back(node);

  node.type = AST_FUNCTION_MIN;
  node.name = "min";
  node.csymbolURL = "";
  node.isFunction = true;
  node.allowedChildrenType = ALLOWED_CHILDREN_ANY;
  node.numAllowedChildren = zero;

  mPkgASTNodeValues.push_back(node);

  node.type = AST_FUNCTION_QUOTIENT;
  node.name = "quotient";
  node.csymbolURL = "";
  node.isFunction = true;
  node.allowedChildrenType = ALLOWED_CHILDREN_EXACTLY;
  node.numAllowedChildren = two;

  mPkgASTNodeValues.push_back(node);

  node.type = AST_FUNCTION_RATE_OF;
  node.name = "rateOf";
  node.csymbolURL = "http://www.sbml.org/sbml/symbols/rateOf";
  node.isFunction = true;
  node.allowedChildrenType = ALLOWED_CHILDREN_EXACTLY;
  node.numAllowedChildren = one;

  mPkgASTNodeValues.push_back(node);

  node.type = AST_FUNCTION_REM;
  node.name = "rem";
  node.csymbolURL = "";
  node.isFunction = true;
  node.allowedChildrenType = ALLOWED_CHILDREN_EXACTLY;
  node.numAllowedChildren = two;

  mPkgASTNodeValues.push_back(node);

  node.type = AST_LOGICAL_IMPLIES;
  node.name = "implies";
  node.csymbolURL = "";
  node.isFunction = true;
  node.allowedChildrenType = ALLOWED_CHILDREN_EXACTLY;
  node.numAllowedChildren = two;

  mPkgASTNodeValues.push_back(node);
}
L3v2extendedmathASTPlugin::L3v2extendedmathASTPlugin() :
  ASTBasePlugin()
{
  mExtendedMathType = EM_L3V2;
  populateNodeTypes();
}

L3v2extendedmathASTPlugin::~L3v2extendedmathASTPlugin()
{
}


/*
* Constructor
*/
L3v2extendedmathASTPlugin::L3v2extendedmathASTPlugin(const std::string &uri)
  : ASTBasePlugin(uri)
{
  mExtendedMathType = EM_L3V2;
  populateNodeTypes();
}

L3v2extendedmathASTPlugin::L3v2extendedmathASTPlugin(const L3v2extendedmathASTPlugin& orig)
  : ASTBasePlugin(orig)
{
  
}


L3v2extendedmathASTPlugin&
L3v2extendedmathASTPlugin::operator=(const L3v2extendedmathASTPlugin& rhs)
{
  if (&rhs != this)
  {
    this->ASTBasePlugin::operator = (rhs);
  }

  return *this;
}


L3v2extendedmathASTPlugin*
L3v2extendedmathASTPlugin::clone() const
{
  return new L3v2extendedmathASTPlugin(*this);
}

bool L3v2extendedmathASTPlugin::hasCorrectNamespace(SBMLNamespaces * namespaces) const
{
  if (namespaces == NULL)
  {
    return false;
  }
  L3v2extendedmathExtension ext;
  const string l3v2x_ns = ext.getXmlnsL3V1V1();
  if (namespaces->getLevel() == 3)
  {
    if (namespaces->getVersion() > 1)
    {
      return true;
    }
    else
    {
      XMLNamespaces * xmlns = namespaces->getNamespaces();
      // need to match the extension ns with one of the namespaces
      if (xmlns->hasURI(l3v2x_ns))
      {
        return true;
      }
    }
  }
  return false;
}

int L3v2extendedmathASTPlugin::checkNumArguments(const ASTNode* node, std::stringstream& error) const
{
  //Default:  0 nothing is known about the function.  
  // Return '1' for the correct number of arguments, 
  // '-1' for the incorrect number of arguments (and set 'error').

  ASTNodeType_t type = node->getType();
  unsigned int numChildren = node->getNumChildren();
  // rateOf is a special case where we want to check its child argument
  if (type != AST_FUNCTION_RATE_OF || numChildren != 1)
  {
    return ASTBasePlugin::checkNumArguments(node, error);
  }

  // so we have rateOf with one child - check it is a ci element

  ASTNode* child = node->getChild(0);
  if (child->getType() == AST_NAME)
  {
    return 1;
  }

  error << "The function 'rateOf' takes exactly one argument, which must be the identifier of an element in the model.";
  return -1;
}

double L3v2extendedmathASTPlugin::evaluateASTNode(const ASTNode * node, const Model * m) const
{
  double result = numeric_limits<double>::quiet_NaN();
  switch(node->getType()) {
  case AST_FUNCTION_REM:
    if (node->getNumChildren() < 2) result = 0.0;
    else
    {
      double dividend = SBMLTransforms::evaluateASTNode(node->getChild(0), m);
      double divisor = SBMLTransforms::evaluateASTNode(node->getChild(1), m);
      double quotient = floor(dividend / divisor);

      result = dividend - (quotient * divisor);
    }
    break;

  case AST_FUNCTION_MIN:
    result = SBMLTransforms::evaluateASTNode(node->getChild(0), m);
    for (unsigned int j = 1; j < node->getNumChildren(); j++)
    {
      double nextValue = SBMLTransforms::evaluateASTNode(node->getChild(j), m);
      if (nextValue < result) result = nextValue;
    }
    break;

  case AST_FUNCTION_MAX:
    result = SBMLTransforms::evaluateASTNode(node->getChild(0), m);
    for (unsigned int j = 1; j < node->getNumChildren(); j++)
    {
      double nextValue = SBMLTransforms::evaluateASTNode(node->getChild(j), m);
      if (nextValue > result) result = nextValue;
    }
    break;

  case AST_LOGICAL_IMPLIES:
  {
    if (node->getNumChildren() == 0)
      result = 0.0;
    else if (node->getNumChildren() == 1)
      result = SBMLTransforms::evaluateASTNode(node->getChild(0), m);
    else
      result = (double)((!(SBMLTransforms::evaluateASTNode(node->getChild(0), m)))
        || (SBMLTransforms::evaluateASTNode(node->getChild(1), m)));
  }
  break;

  case AST_FUNCTION_QUOTIENT:
    if (node->getNumChildren() < 2) result = 0.0;
    else 
    {      
      result = floor(SBMLTransforms::evaluateASTNode(node->getChild(0), m) /
        SBMLTransforms::evaluateASTNode(node->getChild(1), m));
    }
    break;

  case AST_FUNCTION_RATE_OF:
  default:
    result = numeric_limits<double>::quiet_NaN();
    break;

  }
  return result;
}

/** 
* returns the unitDefinition for the ASTNode from a rem function
*/
UnitDefinition * 
L3v2extendedmathASTPlugin::getUnitDefinitionFromRem(UnitFormulaFormatter* uff, const ASTNode * node, 
  bool inKL, int reactNo) const
{ 
  UnitDefinition * ud;

  ud = uff->getUnitDefinition(node->getLeftChild(), inKL, reactNo);

  return ud;
}


/** 
* returns the unitDefinition for the ASTNode from a rateOf function
*/
UnitDefinition * 
L3v2extendedmathASTPlugin::getUnitDefinitionFromRateOf(UnitFormulaFormatter* uff, const ASTNode * node, bool inKL, int reactNo) const
{ 
  UnitDefinition * ud;
  UnitDefinition * tempUD;
  unsigned int i;
  Unit * unit;

  ud = uff->getUnitDefinition(node->getLeftChild(), inKL, reactNo);

  tempUD = uff->getTimeUnitDefinition();

  for (i = 0; i < tempUD->getNumUnits(); i++)
  {
    unit = tempUD->getUnit(i)->clone();
    unit->setExponentUnitChecking(-1 * unit->getExponentUnitChecking());
    ud->addUnit(unit);
    delete unit;
  }

  delete tempUD;

  return ud;
}


UnitDefinition * L3v2extendedmathASTPlugin::getUnitDefinitionFromPackage(UnitFormulaFormatter* uff, const ASTNode * node, bool inKL, int reactNo) const
{
  UnitDefinition* ud = NULL;
  switch (node->getType())
  {
  case AST_FUNCTION_MIN:
  case AST_FUNCTION_MAX:
    ud = uff->getUnitDefinitionFromArgUnitsReturnFunction(node, inKL, reactNo);
    break;

  case AST_FUNCTION_REM:
    ud = getUnitDefinitionFromRem(uff, node, inKL, reactNo);
    break;

  case AST_FUNCTION_RATE_OF:
    ud = getUnitDefinitionFromRateOf(uff, node, inKL, reactNo);
    break;

  case AST_FUNCTION_QUOTIENT:
    ud = uff->getUnitDefinitionFromDivide(node, inKL, reactNo);
    break;

  case AST_LOGICAL_IMPLIES:
    ud = uff->getUnitDefinitionFromDimensionlessReturnFunction(node, inKL, reactNo);
    break;

  default:
    break;
  }
  return ud;
}

bool L3v2extendedmathASTPlugin::isLogical(ASTNodeType_t type) const
{
  if (type == AST_LOGICAL_IMPLIES)
  {
    return true;
  }
  return false;
}

int L3v2extendedmathASTPlugin::allowedInFunctionDefinition(ASTNodeType_t type) const
{
  if (type == AST_FUNCTION_RATE_OF)
  {
    return 0;
  }
  if (defines(type))
  {
    return 1;
  }
  return -1;
}

//void L3v2extendedmathASTPlugin::checkUnits(ArgumentsUnitsCheck * auc, const Model & m, const ASTNode & node, const SBase & sb, bool inKL, int reactNo) const
//{
//  switch (node.getType()) {
//
//  case AST_FUNCTION_MAX:
//  case AST_FUNCTION_MIN:
//
//    auc->checkSameUnitsAsArgs(m, node, sb, inKL, reactNo);
//  }
//}

#endif /* __cplusplus */


/** @cond doxygenIgnored */
/** @endcond */
LIBSBML_CPP_NAMESPACE_END

