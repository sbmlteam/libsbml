/**
 * @file DistribASTPlugin.cpp
 * @brief Implementation of the DistribASTPlugin class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
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
#include <sbml/packages/distrib/extension/DistribASTPlugin.h>
#include <sbml/math/ASTNode.h>
#include <sbml/SBMLNamespaces.h>
#include <sbml/xml/XMLNamespaces.h>
#include <sbml/math/L3ParserSettings.h>
#include <sbml/packages/distrib/extension/DistribExtension.h>

/** @cond doxygenIgnored */

using namespace std;

/** @endcond */
LIBSBML_CPP_NAMESPACE_BEGIN

#ifdef __cplusplus


void
DistribASTPlugin::populateNodeTypes()
{
  vector<unsigned int> one, two, onethree, twofour, twoonefour;
  one.push_back(1);
  two.push_back(2);
  onethree.push_back(1);
  onethree.push_back(3);
  twofour.push_back(2);
  twofour.push_back(4);
  //The first argument should be 'the number of allowed arguments in the old annotation version'.
  twoonefour.push_back(2);
  twoonefour.push_back(1);
  twoonefour.push_back(4);

  ASTNodeValues_t node;
  node.type = AST_DISTRIB_FUNCTION_NORMAL;
  node.name = "normal";
  node.csymbolURL = "http://www.sbml.org/sbml/symbols/distrib/normal";
  node.isFunction = true;
  node.allowedChildrenType = ALLOWED_CHILDREN_EXACTLY;
  node.numAllowedChildren = twofour;

  mPkgASTNodeValues.push_back(node);

  node.type = AST_DISTRIB_FUNCTION_UNIFORM;
  node.name = "uniform";
  node.csymbolURL = "http://www.sbml.org/sbml/symbols/distrib/uniform";
  node.isFunction = true;
  node.allowedChildrenType = ALLOWED_CHILDREN_EXACTLY;
  node.numAllowedChildren = two;

  mPkgASTNodeValues.push_back(node);

  node.type = AST_DISTRIB_FUNCTION_BERNOULLI;
  node.name = "bernoulli";
  node.csymbolURL = "http://www.sbml.org/sbml/symbols/distrib/bernoulli";
  node.isFunction = true;
  node.allowedChildrenType = ALLOWED_CHILDREN_EXACTLY;
  node.numAllowedChildren = one;

  mPkgASTNodeValues.push_back(node);

  node.type = AST_DISTRIB_FUNCTION_BINOMIAL;
  node.name = "binomial";
  node.csymbolURL = "http://www.sbml.org/sbml/symbols/distrib/binomial";
  node.isFunction = true;
  node.allowedChildrenType = ALLOWED_CHILDREN_EXACTLY;
  node.numAllowedChildren = twofour;

  mPkgASTNodeValues.push_back(node);

  node.type = AST_DISTRIB_FUNCTION_CAUCHY;
  node.name = "cauchy";
  node.csymbolURL = "http://www.sbml.org/sbml/symbols/distrib/cauchy";
  node.isFunction = true;
  node.allowedChildrenType = ALLOWED_CHILDREN_EXACTLY;
  node.numAllowedChildren = twoonefour;

  mPkgASTNodeValues.push_back(node);

  node.type = AST_DISTRIB_FUNCTION_CHISQUARE;
  node.name = "chisquare";
  node.csymbolURL = "http://www.sbml.org/sbml/symbols/distrib/chisquare";
  node.isFunction = true;
  node.allowedChildrenType = ALLOWED_CHILDREN_EXACTLY;
  node.numAllowedChildren = onethree;

  mPkgASTNodeValues.push_back(node);

  node.type = AST_DISTRIB_FUNCTION_EXPONENTIAL;
  node.name = "exponential";
  node.csymbolURL = "http://www.sbml.org/sbml/symbols/distrib/exponential";
  node.isFunction = true;
  node.allowedChildrenType = ALLOWED_CHILDREN_EXACTLY;
  node.numAllowedChildren = onethree;

  mPkgASTNodeValues.push_back(node);

  node.type = AST_DISTRIB_FUNCTION_GAMMA;
  node.name = "gamma";
  node.csymbolURL = "http://www.sbml.org/sbml/symbols/distrib/gamma";
  node.isFunction = true;
  node.allowedChildrenType = ALLOWED_CHILDREN_EXACTLY;
  node.numAllowedChildren = twofour;

  mPkgASTNodeValues.push_back(node);

  node.type = AST_DISTRIB_FUNCTION_LAPLACE;
  node.name = "laplace";
  node.csymbolURL = "http://www.sbml.org/sbml/symbols/distrib/laplace";
  node.isFunction = true;
  node.allowedChildrenType = ALLOWED_CHILDREN_EXACTLY;
  node.numAllowedChildren = twoonefour;

  mPkgASTNodeValues.push_back(node);

  node.type = AST_DISTRIB_FUNCTION_LOGNORMAL;
  node.name = "lognormal";
  node.csymbolURL = "http://www.sbml.org/sbml/symbols/distrib/lognormal";
  node.isFunction = true;
  node.allowedChildrenType = ALLOWED_CHILDREN_EXACTLY;
  node.numAllowedChildren = twofour;

  mPkgASTNodeValues.push_back(node);

  node.type = AST_DISTRIB_FUNCTION_POISSON;
  node.name = "poisson";
  node.csymbolURL = "http://www.sbml.org/sbml/symbols/distrib/poisson";
  node.isFunction = true;
  node.allowedChildrenType = ALLOWED_CHILDREN_EXACTLY;
  node.numAllowedChildren = onethree;

  mPkgASTNodeValues.push_back(node);

  node.type = AST_DISTRIB_FUNCTION_RAYLEIGH;
  node.name = "rayleigh";
  node.csymbolURL = "http://www.sbml.org/sbml/symbols/distrib/rayleigh";
  node.isFunction = true;
  node.allowedChildrenType = ALLOWED_CHILDREN_EXACTLY;
  node.numAllowedChildren = onethree;

  mPkgASTNodeValues.push_back(node);

}

DistribASTPlugin::DistribASTPlugin() :
  ASTBasePlugin()
{
  mExtendedMathType = EM_DISTRIB;
  populateNodeTypes();
}

DistribASTPlugin::~DistribASTPlugin()
{
}


int DistribASTPlugin::allowedInFunctionDefinition(ASTNodeType_t type) const
{
  if (defines(type))
  {
    return 1;
  }
  return -1;
}

/*
* Constructor
*/
DistribASTPlugin::DistribASTPlugin(const std::string& uri)
  : ASTBasePlugin(uri)
{
  mExtendedMathType = EM_DISTRIB;
  populateNodeTypes();
}

DistribASTPlugin::DistribASTPlugin(const DistribASTPlugin& orig)
  : ASTBasePlugin(orig)
{

}


DistribASTPlugin&
DistribASTPlugin::operator=(const DistribASTPlugin& rhs)
{
  if (&rhs != this)
  {
    this->ASTBasePlugin::operator = (rhs);
  }

  return *this;
}


DistribASTPlugin*
DistribASTPlugin::clone() const
{
  return new DistribASTPlugin(*this);
}


bool DistribASTPlugin::hasCorrectNamespace(SBMLNamespaces * namespaces) const
{
  if (namespaces == NULL)
  {
    return false;
  }
  DistribExtension ext;
  const string distribns = ext.getXmlnsL3V1V1();

  XMLNamespaces* xmlns = namespaces->getNamespaces();
  if (xmlns->hasURI(distribns))
  {
    return true;
  }

  return false;
}

double DistribASTPlugin::evaluateASTNode(const ASTNode * node, const Model * m) const
{
  //Alternatively, we could actually evaluate the draws from the distributions?  Or return means?
  return numeric_limits<double>::quiet_NaN();
}

UnitDefinition * DistribASTPlugin::getUnitDefinitionFromPackage(UnitFormulaFormatter* uff, const ASTNode * node, bool inKL, int reactNo) const
{
  //OK, this *definitely* could be actually coded, so we could do unit checking on draws from distributions.  But it will be a fair amount of work to get right.
  return NULL;
}

#endif /* __cplusplus */


/** @cond doxygenIgnored */
/** @endcond */
LIBSBML_CPP_NAMESPACE_END

