/**
 * @cond doxygenLibsbmlInternal
 *
 * @file    ASTBasePlugin.cpp
 * @brief   Implementation of ASTBasePlugin, the base class of extension 
 *          entities plugged in SBase derived classes in the SBML Core package.
 * @author  Sarah Keating
 *
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
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

#ifndef LIBSBML_USE_LEGACY_MATH

#include <sbml/extension/SBMLExtensionRegistry.h>

#ifdef __cplusplus

#include <string>
#include <sstream>
#include <iostream>

using namespace std;

LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Constructor
 */
ASTBasePlugin::ASTBasePlugin (const std::string &uri)
 : mSBMLExt(SBMLExtensionRegistry::getInstance().getExtensionInternal(uri))
  ,mParentASTNode(NULL)
  ,mURI(uri)
  ,mSBMLNS(NULL)
  ,mPrefix("")
{
}

/*
 * Constructor
 */
ASTBasePlugin::ASTBasePlugin ()
 : mSBMLExt(NULL)
  ,mParentASTNode(NULL)
  ,mURI("")
  ,mSBMLNS(NULL)
  ,mPrefix("")
{
}




/*
 * Copy constructor. Creates a copy of this ASTBasePlugin object.
 */
ASTBasePlugin::ASTBasePlugin(const ASTBasePlugin& orig)
  : mSBMLExt(orig.mSBMLExt)
   ,mParentASTNode(NULL) // 
   ,mURI(orig.mURI)
   ,mSBMLNS(NULL)
   ,mPrefix(orig.mPrefix)
{
  if (orig.mSBMLNS) {
    mSBMLNS = orig.mSBMLNS->clone();
  }
}



/*
 * Destroy this object.
 */
ASTBasePlugin::~ASTBasePlugin ()
{
  if (mSBMLNS != NULL)
  delete mSBMLNS;
}


/*
 * Assignment operator for ASTBasePlugin.
 */
ASTBasePlugin& 
ASTBasePlugin::operator=(const ASTBasePlugin& orig)
{
  mSBMLExt = orig.mSBMLExt;
  mParentASTNode  = orig.mParentASTNode;  // 0 should be set to mSBML and mParentASTNode?
  mURI     = orig.mURI;
  mPrefix  = orig.mPrefix;

  delete mSBMLNS;
  if (orig.mSBMLNS)
    mSBMLNS = orig.mSBMLNS->clone();
  else
    mSBMLNS = NULL;

  return *this;
}

ASTBasePlugin*
ASTBasePlugin::clone () const
{
  return new ASTBasePlugin(*this);  
}


/*
 * Sets the given SBMLDocument as a parent document.
 */
int 
ASTBasePlugin::setSBMLExtension (const SBMLExtension* ext)
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
ASTBasePlugin::connectToParent (ASTBase* astbase)
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
ASTBase*
ASTBasePlugin::getParentASTObject ()
{
  return mParentASTNode;
}


/*
 * Returns the parent element.
 */
const ASTBase*
ASTBasePlugin::getParentASTObject () const
{
  return mParentASTNode;
}



bool 
ASTBasePlugin::isSetMath() const
{
  return false;
}


const ASTBase * 
ASTBasePlugin::getMath() const
{
  return NULL;
}


void
ASTBasePlugin::createMath(int )
{
  // do nothing
}


int 
ASTBasePlugin::addChild(ASTBase *)
{ 
  return LIBSBML_INVALID_OBJECT; 
}


ASTBase* 
ASTBasePlugin::getChild (unsigned int) const
{ 
  return NULL; 
}


unsigned int 
ASTBasePlugin::getNumChildren() const
{ 
  return 0; 
}


int 
ASTBasePlugin::insertChild(unsigned int, ASTBase*)
{ 
  return LIBSBML_INVALID_OBJECT; 
}


int 
ASTBasePlugin::prependChild(ASTBase*)
{ 
  return LIBSBML_INVALID_OBJECT; 
}


int 
ASTBasePlugin::removeChild(unsigned int)
{ 
  return LIBSBML_INVALID_OBJECT; 
}


int 
ASTBasePlugin::replaceChild(unsigned int, ASTBase*, bool)
{ 
  return LIBSBML_INVALID_OBJECT; 
}

int 
ASTBasePlugin::swapChildren(ASTFunction*)
{ 
  return LIBSBML_INVALID_OBJECT; 
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





bool
ASTBasePlugin::read(XMLInputStream&, const std::string&,
                    const XMLToken&)
{
  return false;
}

void
ASTBasePlugin::addExpectedAttributes(ExpectedAttributes&,
                                     XMLInputStream&, int)
{
}

bool 
ASTBasePlugin::readAttributes(const XMLAttributes&,
                       const ExpectedAttributes&,
                               XMLInputStream&, const XMLToken&,
                               int)
{
  return true;
}


void
ASTBasePlugin::writeAttributes(XMLOutputStream&, int) const
{
}

void
ASTBasePlugin::writeXMLNS(XMLOutputStream&) const
{
}

bool 
ASTBasePlugin::isNumberNode(int) const
{
  return false;
}


bool 
ASTBasePlugin::isFunctionNode(int) const
{
  return false;
}


bool 
ASTBasePlugin::representsUnaryFunction(int) const
{
  return false;
}


bool 
ASTBasePlugin::representsBinaryFunction(int) const
{
  return false;
}


bool 
ASTBasePlugin::representsNaryFunction(int) const
{
  return false;
}


bool 
ASTBasePlugin::isLogical(int) const
{
  return false;
}


bool 
ASTBasePlugin::isConstantNumber(int) const
{
  return false;
}


bool 
ASTBasePlugin::isCSymbolFunction(int) const
{
  return false;
}


bool 
ASTBasePlugin::isCSymbolNumber(int) const
{
  return false;
}


bool 
ASTBasePlugin::isName(int) const
{
  return false;
}


bool 
ASTBasePlugin::isNumber(int) const
{
  return false;
}


bool 
ASTBasePlugin::isOperator(int) const
{
  return false;
}


bool 
ASTBasePlugin::isRelational(int) const
{
  return false;
}


bool 
ASTBasePlugin::representsQualifier(int) const
{
  return false;
}


bool
ASTBasePlugin::hasCorrectNumberArguments(int) const
{
  return true;
}


bool
ASTBasePlugin::isWellFormedNode(int) const
{
  return true;
}


bool 
ASTBasePlugin::isFunction(int) const
{
  return false;
}


bool 
ASTBasePlugin::isTopLevelMathMLFunctionNodeTag(const std::string&) const
{
  return false;
}


bool 
ASTBasePlugin::isTopLevelMathMLNumberNodeTag(const std::string&) const
{
  return false;
}


int 
ASTBasePlugin::getTypeFromName(const std::string&) const
{
  return AST_UNKNOWN;
}


const char * 
ASTBasePlugin::getNameFromType(int) const
{
  return "AST_unknown";
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

void ASTBasePlugin::visitPackageInfixSyntax ( const ASTNode *,
                                         const ASTNode *,
                                         StringBuffer_t  *,
                                         const L3ParserSettings*) const
{
  //Any plugin that has its own infix syntax for anything will need to override this.
}

int ASTBasePlugin::checkNumArguments(const ASTNode*, std::stringstream&) const
{
  //Default:  nothing is known about the function.  Return '1' for the correct number of arguments, '-1' for the incorrect number of arguments (and set 'error').
  return 0;
}

ASTNode*
ASTBasePlugin::parsePackageInfix(L3ParserGrammarLineType_t,
    vector<ASTNode*> *, vector<std::string*> *,
    vector<double> *) const
{
  return NULL;
}


int 
ASTBasePlugin::getPackageFunctionFor(const std::string&) const
{
  return AST_UNKNOWN;
}


LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */

#else
#include <sbml/extension/SBMLExtensionRegistry.h>

#ifdef __cplusplus

#include <string>
#include <sstream>
#include <iostream>

using namespace std;

LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Constructor
 */
ASTBasePlugin::ASTBasePlugin(const std::string &uri)
  : mSBMLExt(SBMLExtensionRegistry::getInstance().getExtensionInternal(uri))
  , mParentASTNode(NULL)
  , mURI(uri)
  , mSBMLNS(NULL)
  , mPrefix("")
{
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
{
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

  delete mSBMLNS;
  if (orig.mSBMLNS)
    mSBMLNS = orig.mSBMLNS->clone();
  else
    mSBMLNS = NULL;

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



bool
ASTBasePlugin::isSetMath() const
{
  return false;
}


const ASTNode *
ASTBasePlugin::getMath() const
{
  return NULL;
}


void
ASTBasePlugin::createMath(int)
{
  // do nothing
}


int
ASTBasePlugin::addChild(ASTNode *)
{
  return LIBSBML_INVALID_OBJECT;
}


ASTNode*
ASTBasePlugin::getChild(unsigned int) const
{
  return NULL;
}


unsigned int
ASTBasePlugin::getNumChildren() const
{
  return 0;
}


int
ASTBasePlugin::insertChild(unsigned int, ASTNode*)
{
  return LIBSBML_INVALID_OBJECT;
}


int
ASTBasePlugin::prependChild(ASTNode*)
{
  return LIBSBML_INVALID_OBJECT;
}


int
ASTBasePlugin::removeChild(unsigned int)
{
  return LIBSBML_INVALID_OBJECT;
}


int
ASTBasePlugin::replaceChild(unsigned int, ASTNode*, bool)
{
  return LIBSBML_INVALID_OBJECT;
}

//int
//ASTBasePlugin::swapChildren(ASTFunction*)
//{
//  return LIBSBML_INVALID_OBJECT;
//}
//
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





bool
ASTBasePlugin::read(XMLInputStream&, const std::string&,
  const XMLToken&)
{
  return false;
}

void
ASTBasePlugin::addExpectedAttributes(ExpectedAttributes&,
  XMLInputStream&, int)
{
}

bool
ASTBasePlugin::readAttributes(const XMLAttributes&,
  const ExpectedAttributes&,
  XMLInputStream&, const XMLToken&,
  int)
{
  return true;
}


void
ASTBasePlugin::writeAttributes(XMLOutputStream&, int) const
{
}

void
ASTBasePlugin::writeXMLNS(XMLOutputStream&) const
{
}

bool
ASTBasePlugin::isNumberNode(int) const
{
  return false;
}


bool
ASTBasePlugin::isFunctionNode(int) const
{
  return false;
}


bool
ASTBasePlugin::representsUnaryFunction(int) const
{
  return false;
}


bool
ASTBasePlugin::representsBinaryFunction(int) const
{
  return false;
}


bool
ASTBasePlugin::representsNaryFunction(int) const
{
  return false;
}


bool
ASTBasePlugin::isLogical(int) const
{
  return false;
}


bool
ASTBasePlugin::isConstantNumber(int) const
{
  return false;
}


bool
ASTBasePlugin::isCSymbolFunction(int) const
{
  return false;
}


bool
ASTBasePlugin::isCSymbolNumber(int) const
{
  return false;
}


bool
ASTBasePlugin::isName(int) const
{
  return false;
}


bool
ASTBasePlugin::isNumber(int) const
{
  return false;
}


bool
ASTBasePlugin::isOperator(int) const
{
  return false;
}


bool
ASTBasePlugin::isRelational(int) const
{
  return false;
}


bool
ASTBasePlugin::representsQualifier(int) const
{
  return false;
}


bool
ASTBasePlugin::hasCorrectNumberArguments(int) const
{
  return true;
}


bool
ASTBasePlugin::isWellFormedNode(int) const
{
  return true;
}


bool
ASTBasePlugin::isFunction(int) const
{
  return false;
}


bool
ASTBasePlugin::isTopLevelMathMLFunctionNodeTag(const std::string&) const
{
  return false;
}


bool
ASTBasePlugin::isTopLevelMathMLNumberNodeTag(const std::string&) const
{
  return false;
}


int
ASTBasePlugin::getTypeFromName(const std::string&) const
{
  return AST_UNKNOWN;
}


const char *
ASTBasePlugin::getNameFromType(int) const
{
  return "AST_unknown";
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

int ASTBasePlugin::checkNumArguments(const ASTNode*, std::stringstream&) const
{
  //Default:  nothing is known about the function.  Return '1' for the correct number of arguments, '-1' for the incorrect number of arguments (and set 'error').
  return 0;
}

ASTNode*
ASTBasePlugin::parsePackageInfix(L3ParserGrammarLineType_t,
  vector<ASTNode*> *, vector<std::string*> *,
  vector<double> *) const
{
  return NULL;
}


int
ASTBasePlugin::getPackageFunctionFor(const std::string&) const
{
  return AST_UNKNOWN;
}


LIBSBML_CPP_NAMESPACE_END

#endif  /* __cplusplus */

#endif /* LIBSBML_USE_LEGACY_MATH */
/** @endcond */
