/**
 * @file   FbcAssociation.cpp
 * @brief  Implementation of the FbcAssociation class
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
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
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations:
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


#include <sbml/packages/fbc/sbml/FbcAssociation.h>
#include <sbml/packages/fbc/extension/FbcModelPlugin.h>
#include <sbml/packages/fbc/validator/FbcSBMLError.h>
#include <sbml/util/ElementFilter.h>

#include <sbml/packages/fbc/sbml/FbcAnd.h>
#include <sbml/packages/fbc/sbml/FbcOr.h>
#include <sbml/packages/fbc/sbml/GeneProductRef.h>

#include <sbml/math/FormulaParser.h>
#include <sbml/util/util.h>

using namespace std;


#ifdef __cplusplus

LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new FbcAssociation with the given level, version, and package version.
 */
FbcAssociation::FbcAssociation (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : SBase(level, version)
  , mElementName("fbcAssociation")
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new FbcPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new FbcAssociation with the given FbcPkgNamespaces object.
 */
FbcAssociation::FbcAssociation (FbcPkgNamespaces* fbcns)
  : SBase(fbcns)
  , mElementName("fbcAssociation")
{
  // set the element namespace of this object
  setElementNamespace(fbcns->getURI());

  // load package extensions bound with this object (if any) 
  loadPlugins(fbcns);
}


/*
 * Copy constructor for FbcAssociation.
 */
FbcAssociation::FbcAssociation (const FbcAssociation& orig)
  : SBase(orig)
{
  mElementName = orig.mElementName;
}


/*
 * Assignment for FbcAssociation.
 */
FbcAssociation&
FbcAssociation::operator=(const FbcAssociation& rhs)
{
  if (&rhs != this)
  {
    SBase::operator=(rhs);
    mElementName = rhs.mElementName;
  }
  return *this;
}


/*
 * Clone for FbcAssociation.
 */
FbcAssociation*
FbcAssociation::clone () const
{
  return new FbcAssociation(*this);
}


/*
 * Destructor for FbcAssociation.
 */
FbcAssociation::~FbcAssociation ()
{
}


/*
 * Return @c true if of type FbcAnd.
 */
bool
FbcAssociation::isFbcAnd() const
{
  return dynamic_cast<const FbcAnd*>(this) != NULL;
}


/*
 * Return @c true if of type FbcOr.
 */
bool
FbcAssociation::isFbcOr() const
{
  return dynamic_cast<const FbcOr*>(this) != NULL;
}


/*
 * Return @c true if of type GeneProductRef.
 */
bool
FbcAssociation::isGeneProductRef() const
{
  return dynamic_cast<const GeneProductRef*>(this) != NULL;
}


/*
 * Returns the XML element name of this object
 */
const std::string&
FbcAssociation::getElementName () const
{
  return mElementName;
}


/** @cond doxygenLibsbmlInternal */
/*
 * Sets the element name for this object
 */
void
FbcAssociation::setElementName(const std::string& name)
{
  mElementName = name;
}
/** @endcond */


/*
 * Returns the libSBML type code for this SBML object.
 */
int
FbcAssociation::getTypeCode () const
{
  return SBML_FBC_ASSOCIATION;
}


/*
 * check if all the required attributes are set
 */
bool
FbcAssociation::hasRequiredAttributes () const
{
  bool allPresent = true;

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
FbcAssociation::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);
  SBase::writeExtensionElements(stream);
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor.
 */
bool
FbcAssociation::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


  /** @endcond */


FbcAssociation* toAssociation(const ASTNode* node, FbcModelPlugin* plugin, bool usingId, bool addMissingGP);

void addChildren(FbcAssociation* association, const ASTNode* node, 
                 const ASTNode *current, FbcModelPlugin* plugin, 
                 bool usingId, bool addMissingGP)
{

  if (node->getType() == AST_TIMES || node->getType() == AST_PLUS)
  {
    for (unsigned int i = 0; i < node->getNumChildren(); ++i)
    {
      ASTNode* astChild = node->getChild(i);
      if (astChild->getType() == current->getType())
      {
        addChildren(association, astChild, node, plugin, usingId, addMissingGP);
        continue;
      }

      FbcAssociation* child = toAssociation(astChild, plugin, usingId, addMissingGP);
      if (child == NULL)
        continue;

      FbcAnd* andAssociation = dynamic_cast<FbcAnd*>(association);
      if (andAssociation != NULL)
      {
        andAssociation->addAssociation(child);
      }
      else
      {
        FbcOr* orAssociation = dynamic_cast<FbcOr*>(association);
        if (orAssociation != NULL)
          orAssociation->addAssociation(child);
      }
      delete child;
    }
  }
  else{
    FbcAssociation* child = toAssociation(node, plugin, usingId, addMissingGP);
    if (child == NULL)
      return;

    FbcAnd* andAssociation = dynamic_cast<FbcAnd*>(association);    
    if (andAssociation != NULL)
    {
      andAssociation->addAssociation(child);
    }
    else
    {
      FbcOr* orAssociation = dynamic_cast<FbcOr*>(association);
      if (orAssociation != NULL)
        orAssociation->addAssociation(child);
    }

    delete child;

  }
}


FbcAssociation* toAssociation(const ASTNode* node, FbcModelPlugin* plugin, 
                               bool usingId, bool addMissingGP)
{
  if (node == NULL)
    return NULL;

  if (node->getType() == AST_NAME)
  {
    std::string name = node->getName();
    if (!usingId)
    {
      replaceAllSubStrings(name, "__MINUS__", "-");
      replaceAllSubStrings(name, "__COLON__", ":");
      replaceAllSubStrings(name, "__DOT__", ".");
      replaceAllSubStrings(name, "__ONE__", "1");
      replaceAllSubStrings(name, "__TWO__", "2");
      replaceAllSubStrings(name, "__THREE__", "3");
      replaceAllSubStrings(name, "__FOUR__", "4");
      replaceAllSubStrings(name, "__FIVE__", "5");
      replaceAllSubStrings(name, "__SIX__", "6");
      replaceAllSubStrings(name, "__SEVEN__", "7");
      replaceAllSubStrings(name, "__EIGHT__", "8");
      replaceAllSubStrings(name, "__NINE__", "9");
      replaceAllSubStrings(name, "__ZERO__", "0");
    }

    GeneProduct* prod = NULL;
    if (usingId)
    {
      prod = plugin->getGeneProduct(name);
    }
    else
    {
      prod = plugin->getGeneProductByLabel(node->getName());
      if (prod == NULL)
        prod = plugin->getGeneProductByLabel(name);
    }
    string id;
    if (prod == NULL)
    {
      if (usingId)
      {
        id = name;
      }
      else
      {
        string base("gp_");
        base += node->getName();
        id = base;
        int count = 0;
        while (plugin->getGeneProduct(id))
        {
          stringstream str;  str << base << "_" << ++count;
          id = str.str();
        }
      }
      if (addMissingGP)
      {
        prod = plugin->createGeneProduct();
        if (usingId)
        {
          prod->setId(name);
          prod->setLabel(name);
        }
        else
        {
          prod->setId(id);
          prod->setLabel(name);
        }
      }
    }
    else
    {
      id = prod->getId();
    }

    GeneProductRef* a = new GeneProductRef();
    a->setGeneProduct(id);
    return a;
  }
  else if (node->getType() == AST_PLUS)
  {
    FbcOr* a = new FbcOr();
    addChildren(a, node, node, plugin, usingId, addMissingGP);
    return a;
  }
  else if (node->getType() == AST_TIMES)
  {
    FbcAnd* a = new FbcAnd();
    addChildren(a, node, node, plugin, usingId, addMissingGP);
    return a;
  }
  return NULL;
}




FbcAssociation* 
FbcAssociation::parseFbcInfixAssociation(const std::string& association, FbcModelPlugin* plugin,
                                          bool usingId, bool addMissingGP)
{
  std::string tweaked(association);
  replaceAllSubStrings(tweaked, " and ", " * ");
  replaceAllSubStrings(tweaked, " AND ", " * ");
  replaceAllSubStrings(tweaked, " or ", " + ");
  replaceAllSubStrings(tweaked, " OR ", " + ");
  if (!usingId)
  {
    replaceAllSubStrings(tweaked, "-", "__MINUS__");
    replaceAllSubStrings(tweaked, ":", "__COLON__");
    replaceAllSubStrings(tweaked, ".", "__DOT__");
    replaceAllSubStrings(tweaked, "1", "__ONE__");
    replaceAllSubStrings(tweaked, "2", "__TWO__");
    replaceAllSubStrings(tweaked, "3", "__THREE__");
    replaceAllSubStrings(tweaked, "4", "__FOUR__");
    replaceAllSubStrings(tweaked, "5", "__FIVE__");
    replaceAllSubStrings(tweaked, "6", "__SIX__");
    replaceAllSubStrings(tweaked, "7", "__SEVEN__");
    replaceAllSubStrings(tweaked, "8", "__EIGHT__");
    replaceAllSubStrings(tweaked, "9", "__NINE__");
    replaceAllSubStrings(tweaked, "0", "__ZERO__");
  }

  ASTNode* node = SBML_parseFormula(tweaked.c_str());

  if (node == NULL)
    return NULL;

  FbcAssociation* result = toAssociation(node, plugin, usingId, addMissingGP);

  delete node;

  return result;


}

std::string
FbcAssociation::toInfix(bool usingId) const
{
  return "";
}


/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument.
 */
void
FbcAssociation::setSBMLDocument (SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
FbcAssociation::enablePackageInternal(const std::string& pkgURI,
             const std::string& pkgPrefix, bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);
}


  /** @endcond */

/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this Association.
 */
int
FbcAssociation::getAttribute(const std::string& attributeName, bool& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this Association.
 */
int
FbcAssociation::getAttribute(const std::string& attributeName, int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this Association.
 */
int
FbcAssociation::getAttribute(const std::string& attributeName,
                          double& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this Association.
 */
int
FbcAssociation::getAttribute(const std::string& attributeName,
                          unsigned int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this Association.
 */
int
FbcAssociation::getAttribute(const std::string& attributeName,
                          std::string& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */




/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this Association's attribute "attributeName"
 * is set.
 */
bool
FbcAssociation::isSetAttribute(const std::string& attributeName) const
{
  bool value = SBase::isSetAttribute(attributeName);

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Association.
 */
int
FbcAssociation::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Association.
 */
int
FbcAssociation::setAttribute(const std::string& attributeName, int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Association.
 */
int
FbcAssociation::setAttribute(const std::string& attributeName, double value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Association.
 */
int
FbcAssociation::setAttribute(const std::string& attributeName,
                          unsigned int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this Association.
 */
int
FbcAssociation::setAttribute(const std::string& attributeName,
                          const std::string& value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */




/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this Association.
 */
int
FbcAssociation::unsetAttribute(const std::string& attributeName)
{
  int value = SBase::unsetAttribute(attributeName);

  return value;
}

/** @endcond */



  /** @cond doxygenLibsbmlInternal */

/*
 * Get the list of expected attributes for this element.
 */
void
FbcAssociation::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
FbcAssociation::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  unsigned int numErrs;

  /* look to see whether an unknown attribute error was logged
   * during the read of the ListOfFbcAssociations - which will have
   * happened immediately prior to this read
  */

  ListOfFbcAssociations* listOf = dynamic_cast<ListOfFbcAssociations*>(getParentSBMLObject());
  unsigned int listOfSize = listOf != NULL ? listOf->size() : 0;

  if (getErrorLog() != NULL && listOfSize < 2)
  {
    numErrs = getErrorLog()->getNumErrors();
    for (int n = (int)numErrs-1; n >= 0; n--)
    {
      if (getErrorLog()->getError((unsigned int)n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details =
              getErrorLog()->getError((unsigned int)n)->getMessage();
        getErrorLog()->remove(UnknownPackageAttribute);
        getErrorLog()->logPackageError("fbc", FbcUnknown,
                  getPackageVersion(), sbmlLevel, sbmlVersion, details, getLine(), getColumn());
      }
      else if (getErrorLog()->getError((unsigned int)n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                   getErrorLog()->getError((unsigned int)n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("fbc", FbcUnknown,
                  getPackageVersion(), sbmlLevel, sbmlVersion, details, getLine(), getColumn());
      }
      else if (getErrorLog()->getError((unsigned int)n)->getErrorId() == NotSchemaConformant)
      {
        getErrorLog()->remove(NotSchemaConformant);
      }
    }
  }

  SBase::readAttributes(attributes, expectedAttributes);

  // look to see whether an unknown attribute error was logged
  if (getErrorLog() != NULL)
  {
    numErrs = getErrorLog()->getNumErrors();
    unsigned int pkgErr = FbcGeneProdRefAllowedAttribs;
    unsigned int coreErr = FbcGeneProdRefAllowedCoreAttribs;
    if (this->isFbcAnd())
    {
      coreErr = FbcAndAllowedCoreAttributes;
    }
    else if (this->isFbcOr())
    {
      coreErr = FbcOrAllowedCoreAttributes;
    }
    for (int n = (int)numErrs-1; n >= 0; n--)
    {
      if (getErrorLog()->getError((unsigned int)n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details =
                          getErrorLog()->getError((unsigned int)n)->getMessage();
        getErrorLog()->remove(UnknownPackageAttribute);
        getErrorLog()->logPackageError("fbc", pkgErr,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details, getLine(), getColumn());
      }
      else if (getErrorLog()->getError((unsigned int)n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                          getErrorLog()->getError((unsigned int)n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("fbc", coreErr,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details, getLine(), getColumn());
      }
      else if (getErrorLog()->getError((unsigned int)n)->getErrorId() == NotSchemaConformant)
      {
        getErrorLog()->remove(NotSchemaConformant);
      }
    }
  }

}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write values of XMLAttributes to the output stream.
 */
  void
FbcAssociation::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);
  SBase::writeExtensionAttributes(stream);

}


  /** @endcond */


/*
 * Constructor 
 */
ListOfFbcAssociations::ListOfFbcAssociations(unsigned int level, 
                        unsigned int version, 
                        unsigned int pkgVersion)
 : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new FbcPkgNamespaces(level, version, pkgVersion)); 
}


/*
 * Constructor 
 */
ListOfFbcAssociations::ListOfFbcAssociations(FbcPkgNamespaces* fbcns)
  : ListOf(fbcns)
{
  setElementNamespace(fbcns->getURI());
}


/*
 * Returns a deep copy of this ListOfFbcAssociations 
 */
ListOfFbcAssociations* 
ListOfFbcAssociations::clone () const
 {
  return new ListOfFbcAssociations(*this);
}


/*
 * Get a FbcAssociation from the ListOfFbcAssociations by index.
 */
FbcAssociation*
ListOfFbcAssociations::get(unsigned int n)
{
  return static_cast<FbcAssociation*>(ListOf::get(n));
}


/*
 * Get a FbcAssociation from the ListOfFbcAssociations by index.
 */
const FbcAssociation*
ListOfFbcAssociations::get(unsigned int n) const
{
  return static_cast<const FbcAssociation*>(ListOf::get(n));
}


/*
 * Get a FbcAssociation from the ListOfFbcAssociations by id.
 */
FbcAssociation*
ListOfFbcAssociations::get(const std::string& sid)
{
  return const_cast<FbcAssociation*>(
    static_cast<const ListOfFbcAssociations&>(*this).get(sid));
}


/*
 * Get a FbcAssociation from the ListOfFbcAssociations by id.
 */
const FbcAssociation*
ListOfFbcAssociations::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<FbcAssociation>(sid) );
  return (result == mItems.end()) ? 0 : static_cast <FbcAssociation*> (*result);
}


/*
 * Adds a copy the given FbcAssociation to this ListOfFbcAssociations.
 *
 * @param fa the FbcAssociation object to add.
 *
 * @copydetails doc_returns_success_code
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 */
int
ListOfFbcAssociations::addFbcAssociation(const FbcAssociation* fa)
{
  if (fa == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (fa->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != fa->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != fa->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const SBase *>(fa)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    return append(fa);
  }
}


/*
 * Get the number of FbcAssociation objects in this ListOfFbcAssociations.
 *
 * @return the number of FbcAssociation objects in this ListOfFbcAssociations
 */
unsigned int 
ListOfFbcAssociations::getNumFbcAssociations() const
{
  return size();
}


/*
 * Creates a new FbcAnd object, adds it to this ListOfFbcAssociations
 * and returns the FbcAnd object created. 
 *
 * @return a new FbcAnd object instance
 *
 * @see addAnd(const FbcAssociation* fa)
 */
FbcAnd* 
ListOfFbcAssociations::createAnd()
{
  FbcAnd* fa = NULL;

  try
  {
    FBC_CREATE_NS_WITH_VERSION(fbcns, getSBMLNamespaces(), getPackageVersion());
    fa = new FbcAnd(fbcns);
    delete fbcns;
  }
  catch (...)
  {
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * do nothing
     */
  }

  if(fa != NULL)
  {
    appendAndOwn(fa);
  }

  return fa;
}


/*
 * Creates a new FbcOr object, adds it to this ListOfFbcAssociations
 * and returns the FbcOr object created. 
 *
 * @return a new FbcOr object instance
 *
 * @see addOr(const FbcAssociation* fa)
 */
FbcOr* 
ListOfFbcAssociations::createOr()
{
  FbcOr* fo = NULL;

  try
  {
    FBC_CREATE_NS_WITH_VERSION(fbcns, getSBMLNamespaces(), getPackageVersion());
    fo = new FbcOr(fbcns);
    delete fbcns;
  }
  catch (...)
  {
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * do nothing
     */
  }

  if(fo != NULL)
  {
    appendAndOwn(fo);
  }

  return fo;
}


/*
 * Creates a new GeneProductRef object, adds it to this ListOfFbcAssociations
 * and returns the GeneProductRef object created. 
 *
 * @return a new GeneProductRef object instance
 *
 * @see addGeneProductRef(const FbcAssociation* fa)
 */
GeneProductRef* 
ListOfFbcAssociations::createGeneProductRef()
{
  GeneProductRef* gpr = NULL;

  try
  {
    FBC_CREATE_NS_WITH_VERSION(fbcns, getSBMLNamespaces(), getPackageVersion());
    gpr = new GeneProductRef(fbcns);
    delete fbcns;
  }
  catch (...)
  {
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * do nothing
     */
  }

  if(gpr != NULL)
  {
    appendAndOwn(gpr);
  }

  return gpr;
}

/*
 * Removes the nth FbcAssociation from this ListOfFbcAssociations
 */
FbcAssociation*
ListOfFbcAssociations::remove(unsigned int n)
{
  return static_cast<FbcAssociation*>(ListOf::remove(n));
}


/*
 * Removes the FbcAssociation from this ListOfFbcAssociations with the given identifier
 */
FbcAssociation*
ListOfFbcAssociations::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<FbcAssociation>(sid) );

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <FbcAssociation*> (item);
}


/*
 * Returns the XML element name of this object
 */
const std::string&
ListOfFbcAssociations::getElementName () const
{
  static const string name = "listOfFbcAssociations";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
ListOfFbcAssociations::getTypeCode () const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the objects in this LIST_OF.
 */
int
ListOfFbcAssociations::getItemTypeCode () const
{
  return SBML_FBC_ASSOCIATION;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * Creates a new FbcAssociation in this ListOfFbcAssociations
 */
SBase*
ListOfFbcAssociations::createObject(XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase* object = NULL;

  if (name == "fbcAssociation")
  {
    FBC_CREATE_NS_WITH_VERSION(fbcns, getSBMLNamespaces(), getPackageVersion());
    object = new FbcAssociation(fbcns);
    appendAndOwn(object);
    delete fbcns;
  }

  if (name == "and")
  {
    FBC_CREATE_NS_WITH_VERSION(fbcns, getSBMLNamespaces(), getPackageVersion());
    object = new FbcAnd(fbcns);
    appendAndOwn(object);
    delete fbcns;
  }

  if (name == "or")
  {
    FBC_CREATE_NS_WITH_VERSION(fbcns, getSBMLNamespaces(), getPackageVersion());
    object = new FbcOr(fbcns);
    appendAndOwn(object);
    delete fbcns;
  }

  if (name == "geneProductRef")
  {
    FBC_CREATE_NS_WITH_VERSION(fbcns, getSBMLNamespaces(), getPackageVersion());
    object = new GeneProductRef(fbcns);
    appendAndOwn(object);
    delete fbcns;
  }

  return object;
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write the namespace for the Fbc package.
 */
void
ListOfFbcAssociations::writeXMLNS(XMLOutputStream& stream) const
{
  XMLNamespaces xmlns;

  std::string prefix = getPrefix();

  if (prefix.empty())
  {
    XMLNamespaces* thisxmlns = getNamespaces();
    if (thisxmlns && thisxmlns->hasURI(FbcExtension::getXmlnsL3V1V1()))
    {
      xmlns.add(FbcExtension::getXmlnsL3V1V1(),prefix);
    }
  }

  stream << xmlns;
}


  /** @endcond */

bool
ListOfFbcAssociations::isValidTypeForList(SBase * item)
{
    int code = item->getTypeCode();
    return code == getItemTypeCode() || code == SBML_FBC_AND || code == SBML_FBC_OR || code == SBML_FBC_GENEPRODUCTREF ;
}


#endif /* __cplusplus */

LIBSBML_EXTERN
FbcAssociation_t *
FbcAssociation_create(unsigned int level, unsigned int version,
                      unsigned int pkgVersion)
{
  return new FbcAssociation(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
FbcAssociation_free(FbcAssociation_t * fa)
{
  if (fa != NULL)
    delete fa;
}


LIBSBML_EXTERN
FbcAssociation_t *
FbcAssociation_clone(FbcAssociation_t * fa)
{
  if (fa != NULL)
  {
    return static_cast<FbcAssociation_t*>(fa->clone());
  }
  else
  {
    return NULL;
  }
}


LIBSBML_EXTERN
int
FbcAssociation_hasRequiredAttributes(const FbcAssociation_t * fa)
{
  return (fa != NULL) ? static_cast<int>(fa->hasRequiredAttributes()) : 0;
}


LIBSBML_EXTERN
FbcAssociation_t *
ListOfFbcAssociations_getById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfFbcAssociations *>(lo)->get(sid) : NULL;
}


LIBSBML_EXTERN
FbcAssociation_t *
ListOfFbcAssociations_removeById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfFbcAssociations *>(lo)->remove(sid) : NULL;
}


LIBSBML_EXTERN
char *
FbcAssociation_toInfix(const FbcAssociation_t * fa)
{
  return (fa != NULL) ? safe_strdup(fa->toInfix().c_str()) : NULL;
}


FbcAssociation_t*
FbcAssociation_parseFbcInfixAssociation(const char * infix, SBasePlugin_t* plugin)
{
  if (infix == NULL || plugin == NULL)
  {
    return NULL;
  }

  return FbcAssociation::parseFbcInfixAssociation(infix, static_cast<FbcModelPlugin*>(plugin));
}


LIBSBML_CPP_NAMESPACE_END



