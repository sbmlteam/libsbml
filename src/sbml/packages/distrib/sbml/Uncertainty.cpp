/**
 * @file:   Uncertainty.cpp
 * @brief:  Implementation of the Uncertainty class
 * @author: SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2014 jointly by the following organizations:
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


#include <sbml/packages/distrib/sbml/Uncertainty.h>
#include <sbml/packages/distrib/validator/DistribSBMLError.h>

#include <sbml/util/ElementFilter.h>


using namespace std;


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new Uncertainty with the given level, version, and package version.
 */
Uncertainty::Uncertainty (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : SBase(level, version)
   ,mId ("")
   ,mName ("")
   ,mUncertML (NULL)
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new DistribPkgNamespaces(level, version, pkgVersion));

  // connect to child objects
  connectToChild();
}


/*
 * Creates a new Uncertainty with the given DistribPkgNamespaces object.
 */
Uncertainty::Uncertainty (DistribPkgNamespaces* distribns)
  : SBase(distribns)
   ,mId ("")
   ,mName ("")
   ,mUncertML (NULL)
{
  // set the element namespace of this object
  setElementNamespace(distribns->getURI());

  // connect to child objects
  connectToChild();

  // load package extensions bound with this object (if any) 
  loadPlugins(distribns);
}


/*
 * Copy constructor for Uncertainty.
 */
Uncertainty::Uncertainty (const Uncertainty& orig)
  : SBase(orig)
{
  if (&orig == NULL)
  {
    throw SBMLConstructorException("Null argument to copy constructor");
  }
  else
  {
    mId  = orig.mId;
    mName  = orig.mName;
    if (orig.mUncertML != NULL)
    {
      mUncertML = orig.mUncertML->clone();
    }
    else
    {
      mUncertML = NULL;
    }

    // connect to child objects
    connectToChild();
  }
}


/*
 * Assignment for Uncertainty.
 */
Uncertainty&
Uncertainty::operator=(const Uncertainty& rhs)
{
  if (&rhs == NULL)
  {
    throw SBMLConstructorException("Null argument to assignment");
  }
  else if (&rhs != this)
  {
    SBase::operator=(rhs);
    mId  = rhs.mId;
    mName  = rhs.mName;
    if (rhs.mUncertML != NULL)
    {
      mUncertML = rhs.mUncertML->clone();
    }
    else
    {
      mUncertML = NULL;
    }

    // connect to child objects
    connectToChild();
  }
  return *this;
}


/*
 * Clone for Uncertainty.
 */
Uncertainty*
Uncertainty::clone () const
{
  return new Uncertainty(*this);
}


/*
 * Destructor for Uncertainty.
 */
Uncertainty::~Uncertainty ()
{
  delete mUncertML;
}


/*
 * Returns the value of the "id" attribute of this Uncertainty.
 */
const std::string&
Uncertainty::getId() const
{
  return mId;
}


/*
 * Returns the value of the "name" attribute of this Uncertainty.
 */
const std::string&
Uncertainty::getName() const
{
  return mName;
}


/*
 * Returns the value of the "UncertML" attribute of this Uncertainty.
 */
const UncertMLNode*
Uncertainty::getUncertML() const
{
  return mUncertML;
}


/*
 * Returns true/false if id is set.
 */
bool
Uncertainty::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Returns true/false if name is set.
 */
bool
Uncertainty::isSetName() const
{
  return (mName.empty() == false);
}


/*
 * Returns true/false if UncertML is set.
 */
bool
Uncertainty::isSetUncertML() const
{
  return (mUncertML != NULL);
}


/*
 * Sets id and returns value indicating success.
 */
int
Uncertainty::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets name and returns value indicating success.
 */
int
Uncertainty::setName(const std::string& name)
{
  if (&(name) == NULL)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mName = name;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets UncertML and returns value indicating success.
 */
int
Uncertainty::setUncertML(UncertMLNode* uncertML)
{
  if (mUncertML == uncertML)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (uncertML == NULL)
  {
    delete mUncertML;
    mUncertML = NULL;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    delete mUncertML;
    mUncertML = (uncertML != NULL) ?
      static_cast<UncertMLNode*>(uncertML->clone()) : NULL;
    //if (mUncertML != NULL)
    //{
    //  mUncertML->connectToParent(this);
    //}
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Unsets id and returns value indicating success.
 */
int
Uncertainty::unsetId()
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
Uncertainty::unsetName()
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
 * Unsets UncertML and returns value indicating success.
 */
int
Uncertainty::unsetUncertML()
{
  delete mUncertML;
  mUncertML = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


List*
Uncertainty::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  // ADD_FILTERED_ELEMENT(ret, sublist, mUncertML, filter);

  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}


/*
 * Returns the XML element name of this object
 */
const std::string&
Uncertainty::getElementName () const
{
  static const string name = "uncertainty";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
Uncertainty::getTypeCode () const
{
  return SBML_DISTRIB_UNCERTAINTY;
}


/*
 * check if all the required attributes are set
 */
bool
Uncertainty::hasRequiredAttributes () const
{
  bool allPresent = true;

  return allPresent;
}


/*
 * check if all the required elements are set
 */
bool
Uncertainty::hasRequiredElements () const
{
  bool allPresent = true;

  if (isSetUncertML() == false)
    allPresent = false;

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
Uncertainty::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

  if (isSetUncertML() == true)
  {
    mUncertML->write(stream);
  }
  SBase::writeExtensionElements(stream);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor.
 */
bool
Uncertainty::accept (SBMLVisitor& v) const
{
  v.visit(*this);

/* VISIT CHILDREN */

  v.leave(*this);

  return true;
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument.
 */
void
Uncertainty::setSBMLDocument (SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
   * Connects to child elements.
 */
void
Uncertainty::connectToChild()
{
  SBase::connectToChild();
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
Uncertainty::enablePackageInternal(const std::string& pkgURI,
             const std::string& pkgPrefix, bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * creates object.
 */
SBase*
Uncertainty::createObject(XMLInputStream& stream)
{
  //const string& name = stream.peek().getName();
  SBase* object = NULL;

  DISTRIB_CREATE_NS(distribns, getSBMLNamespaces());
  delete distribns;

  return object;
}


bool
Uncertainty::readOtherXML(XMLInputStream& stream)
{
  const string& name = stream.peek().getName();

  if (name == "UncertML")
  {
    delete mUncertML;
    XMLNode * xml = new XMLNode(stream);
    mUncertML = new UncertMLNode(xml);
    delete xml;
    return true;
  }

  return false;
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Get the list of expected attributes for this element.
 */
void
Uncertainty::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("id");
  attributes.add("name");
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
Uncertainty::readAttributes (const XMLAttributes& attributes,
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
    for (int n = numErrs-1; n >= 0; n--)
    {
      if (getErrorLog()->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details =
                          getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownPackageAttribute);
        getErrorLog()->logPackageError("distrib", DistribUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details);
      }
      else if (getErrorLog()->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                          getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("distrib", DistribUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details);
      }
    }
  }

  bool assigned = false;

  //
  // id string   ( use = "optional" )
  //
  assigned = attributes.readInto("id", mId);

  if (assigned == true)
  {
    // check string is not empty

    if (mId.empty() == true)
    {
      logEmptyString(mId, getLevel(), getVersion(), "<Uncertainty>");
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
      logEmptyString(mName, getLevel(), getVersion(), "<Uncertainty>");
    }
  }

}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write values of XMLAttributes to the output stream.
 */
  void
Uncertainty::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetId() == true)
    stream.writeAttribute("id", getPrefix(), mId);

  if (isSetName() == true)
    stream.writeAttribute("name", getPrefix(), mName);

  SBase::writeExtensionAttributes(stream);

}


  /** @endcond doxygenLibsbmlInternal */


/*
 * 
 */
LIBSBML_EXTERN
Uncertainty_t *
Uncertainty_create(unsigned int level, unsigned int version,
                   unsigned int pkgVersion)
{
  return new Uncertainty(level, version, pkgVersion);
}


/*
 * 
 */
LIBSBML_EXTERN
void
Uncertainty_free(Uncertainty_t * u)
{
  if (u != NULL)
    delete u;
}


/*
 *
 */
LIBSBML_EXTERN
Uncertainty_t *
Uncertainty_clone(Uncertainty_t * u)
{
  if (u != NULL)
  {
    return static_cast<Uncertainty_t*>(u->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 *
 */
LIBSBML_EXTERN
char *
Uncertainty_getId(Uncertainty_t * u)
{
  if (u == NULL)
    return NULL;

  return u->getId().empty() ? NULL : safe_strdup(u->getId().c_str());
}


/*
 *
 */
LIBSBML_EXTERN
char *
Uncertainty_getName(Uncertainty_t * u)
{
  if (u == NULL)
    return NULL;

  return u->getName().empty() ? NULL : safe_strdup(u->getName().c_str());
}


/*
 *
 */
LIBSBML_EXTERN
int
Uncertainty_isSetId(Uncertainty_t * u)
{
  return (u != NULL) ? static_cast<int>(u->isSetId()) : 0;
}


/*
 *
 */
LIBSBML_EXTERN
int
Uncertainty_isSetName(Uncertainty_t * u)
{
  return (u != NULL) ? static_cast<int>(u->isSetName()) : 0;
}


/*
 *
 */
LIBSBML_EXTERN
int
Uncertainty_setId(Uncertainty_t * u, const char * id)
{
  return (u != NULL) ? u->setId(id) : LIBSBML_INVALID_OBJECT;
}


/*
 *
 */
LIBSBML_EXTERN
int
Uncertainty_setName(Uncertainty_t * u, const char * name)
{
  return (u != NULL) ? u->setName(name) : LIBSBML_INVALID_OBJECT;
}


/*
 *
 */
LIBSBML_EXTERN
int
Uncertainty_unsetId(Uncertainty_t * u)
{
  return (u != NULL) ? u->unsetId() : LIBSBML_INVALID_OBJECT;
}


/*
 *
 */
LIBSBML_EXTERN
int
Uncertainty_unsetName(Uncertainty_t * u)
{
  return (u != NULL) ? u->unsetName() : LIBSBML_INVALID_OBJECT;
}


/*
 *
 */
LIBSBML_EXTERN
int
Uncertainty_hasRequiredAttributes(Uncertainty_t * u)
{
  return (u != NULL) ? static_cast<int>(u->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


