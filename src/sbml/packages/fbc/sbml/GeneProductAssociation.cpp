/**
 * @file   GeneProductAssociation.cpp
 * @brief  Implementation of the GeneProductAssociation class
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


#include <sbml/packages/fbc/sbml/GeneProductAssociation.h>
#include <sbml/packages/fbc/extension/FbcModelPlugin.h>
#include <sbml/packages/fbc/validator/FbcSBMLError.h>
#include <sbml/util/ElementFilter.h>

#include <sbml/packages/fbc/sbml/FbcAnd.h>
#include <sbml/packages/fbc/sbml/FbcOr.h>
#include <sbml/packages/fbc/sbml/GeneProductRef.h>
#include <sbml/packages/fbc/sbml/FbcAssociation.h>

#include <sbml/Model.h>


using namespace std;


#ifdef __cplusplus

LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new GeneProductAssociation with the given level, version, and package version.
 */
GeneProductAssociation::GeneProductAssociation (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : SBase(level, version)
//  , mId ("")
//  , mName ("")
  , mAssociation (NULL)
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new FbcPkgNamespaces(level, version, pkgVersion));

  // connect to child objects
  connectToChild();
}


/*
 * Creates a new GeneProductAssociation with the given FbcPkgNamespaces object.
 */
GeneProductAssociation::GeneProductAssociation (FbcPkgNamespaces* fbcns)
  : SBase(fbcns)
//  , mId ("")
//  , mName ("")
  , mAssociation (NULL)
{
  // set the element namespace of this object
  setElementNamespace(fbcns->getURI());

  // connect to child objects
  connectToChild();

  // load package extensions bound with this object (if any) 
  loadPlugins(fbcns);
}


/*
 * Copy constructor for GeneProductAssociation.
 */
GeneProductAssociation::GeneProductAssociation (const GeneProductAssociation& orig)
  : SBase(orig)
{
  mId  = orig.mId;
  mName  = orig.mName;
  if (orig.mAssociation != NULL)
  {
    mAssociation = orig.mAssociation->clone();
  }
  else
  {
    mAssociation = NULL;
  }

  // connect to child objects
  connectToChild();
}


/*
 * Assignment for GeneProductAssociation.
 */
GeneProductAssociation&
GeneProductAssociation::operator=(const GeneProductAssociation& rhs)
{
  if (&rhs != this)
  {
    SBase::operator=(rhs);
    mId  = rhs.mId;
    mName  = rhs.mName;
    if (rhs.mAssociation != NULL)
    {
      mAssociation = rhs.mAssociation->clone();
    }
    else
    {
      mAssociation = NULL;
    }

    // connect to child objects
    connectToChild();
  }
  return *this;
}


/*
 * Clone for GeneProductAssociation.
 */
GeneProductAssociation*
GeneProductAssociation::clone () const
{
  return new GeneProductAssociation(*this);
}


/*
 * Destructor for GeneProductAssociation.
 */
GeneProductAssociation::~GeneProductAssociation ()
{
  delete mAssociation;
  mAssociation = NULL;
}


/*
 * Returns the value of the "id" attribute of this GeneProductAssociation.
 */
const std::string&
GeneProductAssociation::getId() const
{
  return mId;
}


/*
 * Returns the value of the "name" attribute of this GeneProductAssociation.
 */
const std::string&
GeneProductAssociation::getName() const
{
  return mName;
}


/*
 * Returns the value of the "association" attribute of this GeneProductAssociation.
 */
const FbcAssociation*
GeneProductAssociation::getAssociation() const
{
  return mAssociation;
}


/*
 * Returns the value of the "association" attribute of this GeneProductAssociation.
 */
FbcAssociation*
GeneProductAssociation::getAssociation()
{
  return mAssociation;
}


/*
 * Creates a new "association" element of this GeneProductAssociation and returns it.
 */
FbcAnd*
GeneProductAssociation::createAnd()
{
  if (mAssociation != NULL) delete mAssociation;
  FBC_CREATE_NS_WITH_VERSION(fbcns, getSBMLNamespaces(), getPackageVersion());
  mAssociation = new FbcAnd(fbcns);
  delete fbcns;
  connectToChild();
  return static_cast<FbcAnd*>(mAssociation);
}


/*
 * Creates a new "association" element of this GeneProductAssociation and returns it.
 */
FbcOr*
GeneProductAssociation::createOr()
{
  if (mAssociation != NULL) delete mAssociation;
  FBC_CREATE_NS_WITH_VERSION(fbcns, getSBMLNamespaces(), getPackageVersion());
  mAssociation = new FbcOr(fbcns);
  delete fbcns;
  connectToChild();
  return static_cast<FbcOr*>(mAssociation);
}


/*
 * Creates a new "association" element of this GeneProductAssociation and returns it.
 */
GeneProductRef*
GeneProductAssociation::createGeneProductRef()
{
  if (mAssociation != NULL) delete mAssociation;
  FBC_CREATE_NS_WITH_VERSION(fbcns, getSBMLNamespaces(), getPackageVersion());
  mAssociation = new GeneProductRef(fbcns);
  delete fbcns;
  connectToChild();
  return static_cast<GeneProductRef*>(mAssociation);
}


/*
 * Returns true/false if id is set.
 */
bool
GeneProductAssociation::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Returns true/false if name is set.
 */
bool
GeneProductAssociation::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Returns true/false if association is set.
 */
bool
GeneProductAssociation::isSetAssociation() const
{
  return (mAssociation != NULL);
}


/*
 * Sets id and returns value indicating success.
 */
int
GeneProductAssociation::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets name and returns value indicating success.
 */
int
GeneProductAssociation::setName(const std::string& name)
{
  {
    mName = name;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets association and returns value indicating success.
 */
int
GeneProductAssociation::setAssociation(FbcAssociation* association)
{
  if (mAssociation == association)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (association == NULL)
  {
    delete mAssociation;
    mAssociation = NULL;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    delete mAssociation;
    mAssociation = (association != NULL) ?
      static_cast<FbcAssociation*>(association->clone()) : NULL;
    if (mAssociation != NULL)
    {
      mAssociation->setElementName("association");
      mAssociation->connectToParent(this);
    }
    return LIBSBML_OPERATION_SUCCESS;
  }
}


int GeneProductAssociation::setAssociation(const std::string& association, 
                                           bool usingId, bool addMissingGP)
{
  SBMLDocument* doc = getSBMLDocument();
  if (doc == NULL)
    return LIBSBML_INVALID_OBJECT;
  Model* model = doc->getModel();
  if (model == NULL)
    return LIBSBML_INVALID_OBJECT;
  FbcModelPlugin* plugin = dynamic_cast<FbcModelPlugin*>(model->getPlugin("fbc"));
  if (plugin == NULL)
    return LIBSBML_INVALID_OBJECT;

  FbcAssociation* newAssociation = FbcAssociation::parseFbcInfixAssociation(association, plugin, usingId, addMissingGP);
  if (newAssociation == NULL)
    return LIBSBML_OPERATION_FAILED;

  /* this clones the association so we need to free the memory */
  int result = setAssociation(newAssociation);

  if (result == LIBSBML_OPERATION_SUCCESS)
    delete newAssociation;

  return result;
}

/*
 * Unsets id and returns value indicating success.
 */
int
GeneProductAssociation::unsetId()
{
  mId.erase();

  if (mId.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets name and returns value indicating success.
 */
int
GeneProductAssociation::unsetName()
{
  mName.erase();

  if (mName.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets association and returns value indicating success.
 */
int
GeneProductAssociation::unsetAssociation()
{
  delete mAssociation;
  mAssociation = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Returns the XML element name of this object
 */
const std::string&
GeneProductAssociation::getElementName () const
{
  static const string name = "geneProductAssociation";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
GeneProductAssociation::getTypeCode () const
{
  return SBML_FBC_GENEPRODUCTASSOCIATION;
}


/*
 * check if all the required attributes are set
 */
bool
GeneProductAssociation::hasRequiredAttributes () const
{
  bool allPresent = true;

  return allPresent;
}


/*
 * check if all the required elements are set
 */
bool
GeneProductAssociation::hasRequiredElements () const
{
  bool allPresent = true;

  if (isSetAssociation() == false)
    allPresent = false;

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
GeneProductAssociation::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);
  if (isSetAssociation() == true)
  {
    mAssociation->write(stream);
  }
  SBase::writeExtensionElements(stream);
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor.
 */
bool
GeneProductAssociation::accept (SBMLVisitor& v) const
{
  v.visit(*this);

/* VISIT CHILDREN */
  if (mAssociation != NULL)
  {
    mAssociation->accept(v);
  }
  v.leave(*this);

  return true;
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument.
 */
void
GeneProductAssociation::setSBMLDocument (SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
  if ( mAssociation != NULL)
    mAssociation->setSBMLDocument(d);
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
   * Connects to child elements.
 */
void
GeneProductAssociation::connectToChild()
{
  SBase::connectToChild();

  if (mAssociation != NULL)
    mAssociation->connectToParent(this);
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
GeneProductAssociation::enablePackageInternal(const std::string& pkgURI,
             const std::string& pkgPrefix, bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);
}


  /** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this
 * GeneProductAssociation.
 */
int
GeneProductAssociation::getAttribute(const std::string& attributeName,
                                     bool& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this
 * GeneProductAssociation.
 */
int
GeneProductAssociation::getAttribute(const std::string& attributeName,
                                     int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this
 * GeneProductAssociation.
 */
int
GeneProductAssociation::getAttribute(const std::string& attributeName,
                                     double& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this
 * GeneProductAssociation.
 */
int
GeneProductAssociation::getAttribute(const std::string& attributeName,
                                     unsigned int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this
 * GeneProductAssociation.
 */
int
GeneProductAssociation::getAttribute(const std::string& attributeName,
                                     std::string& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "id")
  {
    value = getId();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "name")
  {
    value = getName();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this GeneProductAssociation's attribute
 * "attributeName" is set.
 */
bool
GeneProductAssociation::isSetAttribute(const std::string& attributeName) const
{
  bool value = SBase::isSetAttribute(attributeName);

  if (attributeName == "id")
  {
    value = isSetId();
  }
  else if (attributeName == "name")
  {
    value = isSetName();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * GeneProductAssociation.
 */
int
GeneProductAssociation::setAttribute(const std::string& attributeName,
                                     bool value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * GeneProductAssociation.
 */
int
GeneProductAssociation::setAttribute(const std::string& attributeName,
                                     int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * GeneProductAssociation.
 */
int
GeneProductAssociation::setAttribute(const std::string& attributeName,
                                     double value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * GeneProductAssociation.
 */
int
GeneProductAssociation::setAttribute(const std::string& attributeName,
                                     unsigned int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this
 * GeneProductAssociation.
 */
int
GeneProductAssociation::setAttribute(const std::string& attributeName,
                                     const std::string& value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  if (attributeName == "id")
  {
    return_value = setId(value);
  }
  else if (attributeName == "name")
  {
    return_value = setName(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this
 * GeneProductAssociation.
 */
int
GeneProductAssociation::unsetAttribute(const std::string& attributeName)
{
  int value = SBase::unsetAttribute(attributeName);

  if (attributeName == "id")
  {
    value = unsetId();
  }
  else if (attributeName == "name")
  {
    value = unsetName();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates and returns an new "elementName" object in this
 * GeneProductAssociation.
 */
SBase*
GeneProductAssociation::createChildObject(const std::string& elementName)
{
  FbcAssociation* obj = NULL;

  if (elementName == "and")
  {
    return createAnd();
  }
  else if (elementName == "or")
  {
    return createOr();
  }
  else if (elementName == "geneProductRef")
  {
    return createGeneProductRef();
  }

  return obj;
}

/** @endcond */


/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this GeneProductAssociation.
 */
unsigned int
GeneProductAssociation::getNumObjects(const std::string& elementName)
{
  unsigned int n = 0;

  if (elementName == "association" || elementName == "and"
    || elementName == "or" || elementName == "geneProductRef")
  {
    if (isSetAssociation())
      n = 1;
  }

  return n;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the nth object of "objectName" in this GeneProductAssociation.
 */
SBase*
GeneProductAssociation::getObject(const std::string& elementName,
                                  unsigned int index)
{
  SBase* obj = NULL;

  if (elementName == "association" || elementName == "and"
    || elementName == "or" || elementName == "geneProductRef")
  {
    return getAssociation();
  }

  return obj;
}

/** @endcond */

/*
 * Returns the first child element that has the given @p id in the model-wide
 * SId namespace, or @c NULL if no such object is found.
 */
SBase*
GeneProductAssociation::getElementBySId(const std::string& id)
{
  if (id.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  obj = mAssociation->getElementBySId(id);

  if (obj != NULL)
  {
    return obj;
  }

  return obj;
}


/*
 * Returns the first child element that has the given @p metaid, or @c NULL if
 * no such object is found.
 */
SBase*
GeneProductAssociation::getElementByMetaId(const std::string& metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mAssociation->getMetaId() == metaid)
  {
    return mAssociation;
  }

  obj = mAssociation->getElementByMetaId(metaid);

  if (obj != NULL)
  {
    return obj;
  }

  return obj;
}


/*
 * Returns a List of all child SBase objects, including those nested to an
 * arbitrary depth.
 */
List*
GeneProductAssociation::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;


  ADD_FILTERED_POINTER(ret, sublist, mAssociation, filter);

  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}





  /** @cond doxygenLibsbmlInternal */

/*
 * creates object.
 */
SBase*
GeneProductAssociation::createObject(XMLInputStream& stream)
{
  SBase* object = NULL;

  const string& name = stream.peek().getName();

  FBC_CREATE_NS_WITH_VERSION(fbcns, getSBMLNamespaces(), getPackageVersion());

  if (mAssociation != NULL)
  {
    std::string details = "The GeneProductAssociation ";
    if (isSetId())
    {
      details += "with id '";
      details += getId();
      details += "' ";
    }
    details += "already has a child element and the <";
    details += name;
    details += "> element will overwrite it.";

    // only have one child element
    getErrorLog()->logPackageError("fbc", FbcGeneProdAssocContainsOneElement,
                    getPackageVersion(), getLevel(), getVersion(), 
                    details, getLine(), getColumn());

  }
  delete mAssociation;

  if (name == "and")
  {
    mAssociation = new FbcAnd(fbcns);
    mAssociation->setElementName(name);
    object = mAssociation;
  }
  else if (name == "or")
  {
    mAssociation = new FbcOr(fbcns);
    mAssociation->setElementName(name);
    object = mAssociation;
  }
  else if (name == "geneProductRef")
  {
    mAssociation = new GeneProductRef(fbcns);
    mAssociation->setElementName(name);
    object = mAssociation;
  }

  delete fbcns;

  connectToChild();


  return object;
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Get the list of expected attributes for this element.
 */
void
GeneProductAssociation::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("id");
  attributes.add("name");
}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
GeneProductAssociation::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  unsigned int numErrs;

  SBase::readAttributes(attributes, expectedAttributes);

  // look to see whether an unknown attribute error was logged
  if (getErrorLog() != NULL)
  {
    numErrs = getErrorLog()->getNumErrors();
    for (int n = (int)numErrs-1; n >= 0; n--)
    {
      if (getErrorLog()->getError((unsigned int)n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details =
                          getErrorLog()->getError((unsigned int)n)->getMessage();
        getErrorLog()->remove(UnknownPackageAttribute);
        getErrorLog()->logPackageError("fbc", FbcGeneProdAssocAllowedAttribs,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details, getLine(), getColumn());
      }
      else if (getErrorLog()->getError((unsigned int)n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                          getErrorLog()->getError((unsigned int)n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("fbc", FbcGeneProdAssocAllowedCoreAttribs,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details, getLine(), getColumn());
      }
      else if (getErrorLog()->getError((unsigned int)n)->getErrorId() == NotSchemaConformant)
      {
        getErrorLog()->remove(NotSchemaConformant);
      }
    }
  }

  bool assigned = false;

  //
  // id SId  ( use = "optional" )
  //
  assigned = attributes.readInto("id", mId);

   if (assigned == true)
  {
    // check string is not empty and correct syntax

    if (mId.empty() == true)
    {
      logEmptyString(mId, getLevel(), getVersion(), "<GeneProductAssociation>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mId) == false && getErrorLog() != NULL)
    {
      getErrorLog()->logError(FbcGeneProdAssocIdSyntax, getLevel(), getVersion(), 
        "The syntax of the attribute id='" + mId + "' does not conform.", getLine(), getColumn());
    }
  }

  //
  // name string   ( use = "optional" )
  //
  assigned = attributes.readInto("name", mName);

  if (assigned == true)
  {
    // check string is not empty

    if (mName.empty() == true)
    {
      logEmptyString(mName, getLevel(), getVersion(), "<GeneProductAssociation>");
    }
  }

}


  /** @endcond */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write values of XMLAttributes to the output stream.
 */
  void
GeneProductAssociation::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetId() == true)
    stream.writeAttribute("id", getPrefix(), mId);

  if (isSetName() == true)
    stream.writeAttribute("name", getPrefix(), mName);


  //
  // (EXTENSION)
  //
  SBase::writeExtensionAttributes(stream);
  }


  /** @endcond */


LIBSBML_EXTERN
GeneProductAssociation_t *
GeneProductAssociation_create(unsigned int level, unsigned int version,
                              unsigned int pkgVersion)
{
  return new GeneProductAssociation(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
GeneProductAssociation_free(GeneProductAssociation_t * gpa)
{
  if (gpa != NULL)
    delete gpa;
}


LIBSBML_EXTERN
GeneProductAssociation_t *
GeneProductAssociation_clone(GeneProductAssociation_t * gpa)
{
  if (gpa != NULL)
  {
    return static_cast<GeneProductAssociation_t*>(gpa->clone());
  }
  else
  {
    return NULL;
  }
}


LIBSBML_EXTERN
const char *
GeneProductAssociation_getId(const GeneProductAssociation_t * gpa)
{
  return (gpa != NULL && gpa->isSetId()) ? gpa->getId().c_str() : NULL;
}


LIBSBML_EXTERN
const char *
GeneProductAssociation_getName(const GeneProductAssociation_t * gpa)
{
  return (gpa != NULL && gpa->isSetName()) ? gpa->getName().c_str() : NULL;
}


LIBSBML_EXTERN
FbcAssociation_t*
GeneProductAssociation_getAssociation(GeneProductAssociation_t * gpa)
{
  if (gpa == NULL)
    return NULL;

  return (FbcAssociation_t*)gpa->getAssociation();
}


LIBSBML_EXTERN
FbcAnd_t *
GeneProductAssociation_createAnd(GeneProductAssociation_t * gpa)
{
  return  (gpa != NULL) ? gpa->createAnd() : NULL;
}

LIBSBML_EXTERN
FbcOr_t *
GeneProductAssociation_createOr(GeneProductAssociation_t * gpa)
{
  return  (gpa != NULL) ? gpa->createOr() : NULL;
}

LIBSBML_EXTERN
GeneProductRef_t *
GeneProductAssociation_createGeneProductRef(GeneProductAssociation_t * gpa)
{
  return  (gpa != NULL) ? gpa->createGeneProductRef() : NULL;
}

LIBSBML_EXTERN
int
GeneProductAssociation_isSetId(const GeneProductAssociation_t * gpa)
{
  return (gpa != NULL) ? static_cast<int>(gpa->isSetId()) : 0;
}


LIBSBML_EXTERN
int
GeneProductAssociation_isSetName(const GeneProductAssociation_t * gpa)
{
  return (gpa != NULL) ? static_cast<int>(gpa->isSetName()) : 0;
}


LIBSBML_EXTERN
int
GeneProductAssociation_isSetAssociation(const GeneProductAssociation_t * gpa)
{
  return (gpa != NULL) ? static_cast<int>(gpa->isSetAssociation()) : 0;
}


LIBSBML_EXTERN
int
GeneProductAssociation_setId(GeneProductAssociation_t * gpa, const char * id)
{
  if (gpa != NULL)
    return (id == NULL) ? gpa->setId("") : gpa->setId(id);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
GeneProductAssociation_setName(GeneProductAssociation_t * gpa, const char * name)
{
  if (gpa != NULL)
    return (name == NULL) ? gpa->setName("") : gpa->setName(name);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
GeneProductAssociation_setAssociation(GeneProductAssociation_t * gpa, FbcAssociation_t* association)
{
  return (gpa != NULL) ? gpa->setAssociation(association) : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
GeneProductAssociation_unsetId(GeneProductAssociation_t * gpa)
{
  return (gpa != NULL) ? gpa->unsetId() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
GeneProductAssociation_unsetName(GeneProductAssociation_t * gpa)
{
  return (gpa != NULL) ? gpa->unsetName() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
GeneProductAssociation_hasRequiredAttributes(const GeneProductAssociation_t * gpa)
{
  return (gpa != NULL) ? static_cast<int>(gpa->hasRequiredAttributes()) : 0;
}


LIBSBML_EXTERN
int
GeneProductAssociation_hasRequiredElements(const GeneProductAssociation_t * gpa)
{
  return (gpa != NULL) ? static_cast<int>(gpa->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END

#endif /* __cplusplus */



