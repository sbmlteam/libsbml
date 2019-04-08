/**
 * @cond doxygenLibsbmlInternal
 *
 * @file    ASTBasePlugin.cpp
 * @brief   Implementation of ASTBasePlugin, the base class of extension 
 *          entities plugged in SBase derived classes in the SBML Core package.
 * @author  Sarah Keating
 * @author  Lucian Smith
 *
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2019 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2012 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *  
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 *     Pasadena, CA, USA 
 *  
 * Copyright (C) 2002-2005 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. Japan Science and Technology Agency, Japan
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 */


#include <sbml/common/libsbml-config-common.h>
#include <sbml/extension/ASTBasePlugin.h>

#include <sbml/extension/SBMLExtensionRegistry.h>

#ifdef __cplusplus

#include <string>
#include <sstream>
#include <iostream>
#include <vector>
#include <map>


#include <sbml/math/ASTNode.h>

using namespace std;

LIBSBML_CPP_NAMESPACE_BEGIN

// from extended math
bool emStrCmp(const string& lhs, const string& rhs, bool strCmpIsCaseSensitive=false)
{
  if (strCmpIsCaseSensitive) {
    return lhs==rhs;
  }
  if (lhs.size() != rhs.size()) return false;

  for (size_t i = 0; i < lhs.size(); ++i)
  {
    if (toupper(lhs[i]) != toupper(rhs[i])) return false;
  }
  return true;

}

const std::string& 
ASTBasePlugin::getStringFor(ASTNodeType_t type) const
{
  for (size_t t = 0; t < mPkgASTNodeValues.size(); t++)
  {
    if (mPkgASTNodeValues[t].type == type)
    {
      return mPkgASTNodeValues[t].name;
    }
  }
  static const std::string empty = "";
  return empty;
}

const char* 
ASTBasePlugin::getConstCharFor(ASTNodeType_t type) const
{
  const char * name = NULL;
  for (size_t t = 0; t < mPkgASTNodeValues.size(); t++)
  {
    if (mPkgASTNodeValues[t].type == type)
    {
      name = mPkgASTNodeValues[t].name.c_str();
    }
  }
  return name;
}

const ASTNodeValues_t *
ASTBasePlugin::getASTNodeValue(unsigned int n) const
{
  if (n < mPkgASTNodeValues.size())
  {
    return &(mPkgASTNodeValues[n]);
  }
  return NULL;
}



const char* 
ASTBasePlugin::getConstCharCsymbolURLFor(ASTNodeType_t type) const
{
  const char * name = NULL;
  for (size_t t = 0; t < mPkgASTNodeValues.size(); t++)
  {
    if (mPkgASTNodeValues[t].type == type)
    {
      if (!mPkgASTNodeValues[t].csymbolURL.empty())
      {
        name = mPkgASTNodeValues[t].csymbolURL.c_str();
      }
    }
  }
  return name;
}

ASTNodeType_t 
ASTBasePlugin::getASTNodeTypeFor(const std::string& symbol) const
{
  for (size_t t = 0; t < mPkgASTNodeValues.size(); t++)
  {
    if (emStrCmp(mPkgASTNodeValues[t].name, symbol))
    {
      return mPkgASTNodeValues[t].type;
    }
  }
  return AST_UNKNOWN;
}
ASTNodeType_t ASTBasePlugin::getASTNodeTypeForCSymbolURL(const std::string& url) const
{
  for (size_t t = 0; t < mPkgASTNodeValues.size(); t++)
  {
    if (emStrCmp(mPkgASTNodeValues[t].csymbolURL, url))
    {
      return  mPkgASTNodeValues[t].type;
    }
  }
  return AST_UNKNOWN;
}

void addNumTo(int num, stringstream& error)
{
  switch (num)
  {
  case 1:
    error << "one";
    break;
  case 2:
    error << "two";
    break;
  case 3:
    error << "three";
    break;
  case 4:
    error << "four";
    break;
  default:
    error << num;
    break;
  }
}

bool 
ASTBasePlugin::hasCorrectNamespace(SBMLNamespaces* namespaces) const
{
  return false;
}

bool ASTBasePlugin::defines(ASTNodeType_t type) const
{
  for (size_t t = 0; t < mPkgASTNodeValues.size(); t++)
  {
    if (mPkgASTNodeValues[t].type == type)
    {
      return true;
    }
  }
  return false;
}

bool ASTBasePlugin::defines(const std::string& name, bool strCmpIsCaseSensitive) const
{
  for (size_t t = 0; t < mPkgASTNodeValues.size(); t++)
  {
    if (emStrCmp(mPkgASTNodeValues[t].name, name, strCmpIsCaseSensitive))
    {
      return true;
    }
  }
  return false;
}

bool ASTBasePlugin::isFunction(ASTNodeType_t type) const
{
  for (size_t t = 0; t < mPkgASTNodeValues.size(); t++)
  {
    if (mPkgASTNodeValues[t].type == type)
    {
      return mPkgASTNodeValues[t].isFunction;
    }
  }
  return false;
}

bool ASTBasePlugin::isLogical(ASTNodeType_t type) const
{
  return false;
}

bool ASTBasePlugin::isMathMLNodeTag(const string& name) const
{
  return false;
}

bool ASTBasePlugin::isMathMLNodeTag(ASTNodeType_t type) const
{
  return false;
}

ExtendedMathType_t ASTBasePlugin::getExtendedMathType() const
{
  return mExtendedMathType;
}

double ASTBasePlugin::evaluateASTNode(const ASTNode * node, const Model * m) const
{
  return numeric_limits<double>::quiet_NaN();
}

UnitDefinition * ASTBasePlugin::getUnitDefinitionFromPackage(UnitFormulaFormatter* uff, const ASTNode * node, bool inKL, int reactNo) const
{
  return NULL;
}

int ASTBasePlugin::allowedInFunctionDefinition(ASTNodeType_t type) const
{
  return -1;
}



/*
 * Constructor
 */
ASTBasePlugin::ASTBasePlugin(const std::string &uri)
  : mSBMLExt(SBMLExtensionRegistry::getInstance().getExtensionInternal(uri))
  , mParentASTNode(NULL)
  , mURI(uri)
  , mSBMLNS(NULL)
  , mPrefix("")
  , mExtendedMathType(EM_UNKNOWN)
{
  mPkgASTNodeValues.clear();
}

/*
 * Constructor
 */
ASTBasePlugin::ASTBasePlugin()
  : mSBMLExt(NULL)
  , mParentASTNode(NULL)
  , mURI("")
  , mSBMLNS(NULL)
  , mPrefix("")
  , mExtendedMathType(EM_UNKNOWN)
{
  mPkgASTNodeValues.clear();
}




/*
 * Copy constructor. Creates a copy of this ASTBasePlugin object.
 */
ASTBasePlugin::ASTBasePlugin(const ASTBasePlugin& orig)
  : mSBMLExt(orig.mSBMLExt)
  , mParentASTNode(NULL) // 
  , mURI(orig.mURI)
  , mSBMLNS(NULL)
  , mPrefix(orig.mPrefix)
  , mExtendedMathType(orig.mExtendedMathType)
  , mPkgASTNodeValues(orig.mPkgASTNodeValues)
{
  if (orig.mSBMLNS) {
    mSBMLNS = orig.mSBMLNS->clone();
  }
}



/*
 * Destroy this object.
 */
ASTBasePlugin::~ASTBasePlugin()
{
  if (mSBMLNS != NULL)
    delete mSBMLNS;
  mPkgASTNodeValues.clear();
}


/*
 * Assignment operator for ASTBasePlugin.
 */
ASTBasePlugin&
ASTBasePlugin::operator=(const ASTBasePlugin& orig)
{
  mSBMLExt = orig.mSBMLExt;
  mParentASTNode = orig.mParentASTNode;  // 0 should be set to mSBML and mParentASTNode?
  mURI = orig.mURI;
  mPrefix = orig.mPrefix;
  mExtendedMathType = orig.mExtendedMathType;

  delete mSBMLNS;
  if (orig.mSBMLNS)
    mSBMLNS = orig.mSBMLNS->clone();
  else
    mSBMLNS = NULL;

  mPkgASTNodeValues = orig.mPkgASTNodeValues;

  return *this;
}

ASTBasePlugin*
ASTBasePlugin::clone() const
{
  return new ASTBasePlugin(*this);
}


/*
 * Sets the given SBMLDocument as a parent document.
 */
int
ASTBasePlugin::setSBMLExtension(const SBMLExtension* ext)
{
  mSBMLExt = ext;
  return LIBSBML_OPERATION_SUCCESS;
}




/*
 * Sets the parent SBML object of this plugin object to
 * this object and child elements (if any).
 * (Creates a child-parent relationship by this plugin object)
 */
void
ASTBasePlugin::connectToParent(ASTNode* astbase)
{
  mParentASTNode = astbase;
}




/*
 *
 * (Extension)
 *
 * Sets the XML namespace to which this element belongs to.
 * For example, all elements that belong to SBML Level 3 Version 1 Core
 * must set the namespace to "http://www.sbml.org/sbml/level3/version1/core";
 * all elements that belong to Layout Extension Version 1 for SBML Level 3
 * Version 1 Core must set the namespace to
 * "http://www.sbml.org/sbml/level3/version1/layout/version1/"
 *
 */
int
ASTBasePlugin::setElementNamespace(const std::string &uri)
{
  mURI = uri;

  return LIBSBML_OPERATION_SUCCESS;
}


int
ASTBasePlugin::setPrefix(const std::string &prefix)
{
  mPrefix = prefix;

  return LIBSBML_OPERATION_SUCCESS;
}

/*
 * Returns the parent element.
 */
ASTNode*
ASTBasePlugin::getParentASTObject()
{
  return mParentASTNode;
}


/*
 * Returns the parent element.
 */
const ASTNode*
ASTBasePlugin::getParentASTObject() const
{
  return mParentASTNode;
}



/*
 * Enables/Disables the given package with child elements in this plugin
 * object (if any).
 */
void
ASTBasePlugin::enablePackageInternal(const std::string&,
  const std::string&, bool)
{
  // do nothing.
}


bool
ASTBasePlugin::stripPackage(const std::string&, bool)
{
  return true;
}



/*
 * Returns the SBML level of this plugin object.
 *
 * @return the SBML level of this plugin object.
 */
unsigned int
ASTBasePlugin::getLevel() const
{
  return (mSBMLExt != NULL) ? mSBMLExt->getLevel(getURI()) : 0;
}


/*
 * Returns the SBML version of this plugin object.
 *
 * @return the SBML version of this plugin object.
 */
unsigned int
ASTBasePlugin::getVersion() const
{
  return (mSBMLExt != NULL) ? mSBMLExt->getVersion(getURI()) : 0;
}


/* open doxygen comment */
SBMLNamespaces *
ASTBasePlugin::getSBMLNamespaces() const
{
  if (mSBMLNS == NULL)
    const_cast<ASTBasePlugin*>(this)->mSBMLNS = new SBMLNamespaces();
  return mSBMLNS;

}
/* end doxygen comment */


/*
 * Returns the package version of this plugin object.
 *
 * @return the package version of this plugin object.
 */
unsigned int
ASTBasePlugin::getPackageVersion() const
{
  return (mSBMLExt == NULL) ? 0 : mSBMLExt->getPackageVersion(getURI());
}





/*
 * Returns the namespace URI of this element.
 */
const std::string&
ASTBasePlugin::getElementNamespace() const
{
  return mURI;
}

std::string
ASTBasePlugin::getURI() const
{
  if (mSBMLExt == NULL)
    return getElementNamespace();

  const std::string &package = (mSBMLExt != NULL) ? mSBMLExt->getName() : std::string("");

  SBMLNamespaces* sbmlns = getSBMLNamespaces();

  if (sbmlns == NULL)
    return getElementNamespace();

  if (package == "" || package == "core")
    return sbmlns->getURI();

  std::string packageURI = sbmlns->getNamespaces()->getURI(package);
  if (!packageURI.empty())
    return packageURI;

  return getElementNamespace();
}

/*
 * Returns the prefix bound to this element.
 */
const std::string&
ASTBasePlugin::getPrefix() const
{

  return mPrefix;
}


/*
 * Returns the package name of this plugin object.
 */
const std::string&
ASTBasePlugin::getPackageName() const
{
  static string empty;
  return (mSBMLExt != NULL) ? mSBMLExt->getName() : empty;
}
void
ASTBasePlugin::renameSIdRefs(const std::string&, const std::string&)
{
}


void
ASTBasePlugin::renameUnitSIdRefs(const std::string&, const std::string&)
{
}


void
ASTBasePlugin::replaceIDWithFunction(const std::string&, const ASTNode*)
{
}


bool
ASTBasePlugin::isPackageInfixFunction() const
{
  //Should be overridden by actual plugins.
  return false;
}

bool
ASTBasePlugin::hasPackageOnlyInfixSyntax() const
{
  //Should be overridden by actual plugins.
  return false;
}

int ASTBasePlugin::getL3PackageInfixPrecedence() const
{
  //Should be overridden by actual plugins.
  return -1;
}

bool ASTBasePlugin::hasUnambiguousPackageInfixGrammar(const ASTNode *) const
{
  return false;
}

void ASTBasePlugin::visitPackageInfixSyntax(const ASTNode *,
  const ASTNode *,
  StringBuffer_t  *,
  const L3ParserSettings*) const
{
  //Any plugin that has its own infix syntax for anything will need to override this.
}

int ASTBasePlugin::checkNumArguments(const ASTNode* node, std::stringstream& error) const
{
  //Default:  0 nothing is known about the function.  
  // Return '1' for the correct number of arguments, 
  // '-1' for the incorrect number of arguments (and set 'error').

  // if we are using the base function to report we have stripped the function name
  // as more involved packages (arrays) has its own sentence
  if (error.str().empty())
  {
    string name = node->getName();
    error << "The function '" << name << "' takes ";

  }
  ASTNodeType_t type = node->getType();
  AllowedChildrenType_t allowedType = ALLOWED_CHILDREN_UNKNOWN;
  std::vector<unsigned int> allowedNumber;
  for (size_t t = 0; t < mPkgASTNodeValues.size(); t++)
  {
    if (mPkgASTNodeValues[t].type == type)
    {
      allowedType = mPkgASTNodeValues[t].allowedChildrenType;
      allowedNumber = mPkgASTNodeValues[t].numAllowedChildren;
      break;
    }
  }
  if (allowedType == ALLOWED_CHILDREN_UNKNOWN)
  {
    return 0;
  }

  unsigned int numChildren = node->getNumChildren();
  switch (allowedType)
  {
  case ALLOWED_CHILDREN_ANY:
    return 1;
  case ALLOWED_CHILDREN_ATLEAST:
    if (numChildren >= allowedNumber.at(0))
    {
      return 1;
    }
    error << "at least ";
    addNumTo(allowedNumber.at(0), error);
    error << " argument";
    if (allowedNumber.size() > 1 || allowedNumber.at(0) > 1)
    {
      error << "s";
    }
    error << ", but " << numChildren << " were found.";
    return -1;
  case ALLOWED_CHILDREN_EXACTLY:
    error << "exactly ";
    for (size_t n = 0; n < allowedNumber.size(); n++)
    {
      if (numChildren == allowedNumber.at(n))
      {
        return 1;
      }
      if (n > 0)
      {
        error << " or ";
      }
      addNumTo(allowedNumber.at(n), error);
    }
    error << " argument";
    if (allowedNumber.size() > 1 || allowedNumber.at(0) > 1)
    {
      error << "s";
    }
    error << ", but " << numChildren << " were found.";
    return -1;
  default:
    return 0;
  }
  return 0;
}

bool 
ASTBasePlugin::hasCorrectNumArguments(const ASTNode* function) const
{
  bool correctNum = false;

  stringstream error;
  int i = checkNumArguments(function, error);
  if (i > -1)
  {
    correctNum = true;
  }

  return correctNum;
}


ASTNode*
ASTBasePlugin::parsePackageInfix(L3ParserGrammarLineType_t,
  vector<ASTNode*> *, vector<std::string*> *,
  vector<double> *) const
{
  return NULL;
}


ASTNodeType_t
ASTBasePlugin::getPackageFunctionFor(const std::string& function, bool strCmpIsCaseSensitive) const
{
  for (size_t t = 0; t < mPkgASTNodeValues.size(); t++)
  {
    if (emStrCmp(mPkgASTNodeValues[t].name, function, strCmpIsCaseSensitive))
    {
      ASTNodeType_t ret = mPkgASTNodeValues[t].type;
      if (mPkgASTNodeValues[t].isFunction)
      {
        return ret;
      }
      return AST_UNKNOWN;
    }
  }
  return AST_UNKNOWN;
}


LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */

/** @endcond */
