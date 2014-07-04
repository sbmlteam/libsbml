/**
 * @file:   ParametricObject.cpp
 * @brief:  Implementation of the ParametricObject class
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


#include <sbml/packages/spatial/sbml/ParametricObject.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new ParametricObject with the given level, version, and package version.
 */
ParametricObject::ParametricObject (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : SBase(level, version)
  , mId ("")
  , mPolygonType ("")
  , mDomain ("")
  , mPolygonObject (NULL)
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version, pkgVersion));

  // connect to child objects
  connectToChild();
}


/*
 * Creates a new ParametricObject with the given SpatialPkgNamespaces object.
 */
ParametricObject::ParametricObject (SpatialPkgNamespaces* spatialns)
  : SBase(spatialns)
  , mId ("")
  , mPolygonType ("")
  , mDomain ("")
  , mPolygonObject (NULL)
{
  // set the element namespace of this object
  setElementNamespace(spatialns->getURI());

  // connect to child objects
  connectToChild();

  // load package extensions bound with this object (if any) 
  loadPlugins(spatialns);
}


/*
 * Copy constructor for ParametricObject.
 */
ParametricObject::ParametricObject (const ParametricObject& orig)
  : SBase(orig)
{
  if (&orig == NULL)
  {
    throw SBMLConstructorException("Null argument to copy constructor");
  }
  else
  {
    mId  = orig.mId;
    mPolygonType  = orig.mPolygonType;
    mDomain  = orig.mDomain;
    if (orig.mPolygonObject != NULL)
    {
      mPolygonObject = orig.mPolygonObject->clone();
    }
    else
    {
      mPolygonObject = NULL;
    }

    // connect to child objects
    connectToChild();
  }
}


/*
 * Assignment for ParametricObject.
 */
ParametricObject&
ParametricObject::operator=(const ParametricObject& rhs)
{
  if (&rhs == NULL)
  {
    throw SBMLConstructorException("Null argument to assignment");
  }
  else if (&rhs != this)
  {
    SBase::operator=(rhs);
    mId  = rhs.mId;
    mPolygonType  = rhs.mPolygonType;
    mDomain  = rhs.mDomain;
    if (rhs.mPolygonObject != NULL)
    {
      mPolygonObject = rhs.mPolygonObject->clone();
    }
    else
    {
      mPolygonObject = NULL;
    }

    // connect to child objects
    connectToChild();
  }
  return *this;
}


/*
 * Clone for ParametricObject.
 */
ParametricObject*
ParametricObject::clone () const
{
  return new ParametricObject(*this);
}


/*
 * Destructor for ParametricObject.
 */
ParametricObject::~ParametricObject ()
{
  delete mPolygonObject;
  mPolygonObject = NULL;
}


/*
 * Returns the value of the "id" attribute of this ParametricObject.
 */
const std::string&
ParametricObject::getId() const
{
  return mId;
}


/*
 * Returns the value of the "polygonType" attribute of this ParametricObject.
 */
const std::string&
ParametricObject::getPolygonType() const
{
  return mPolygonType;
}


/*
 * Returns the value of the "domain" attribute of this ParametricObject.
 */
const std::string&
ParametricObject::getDomain() const
{
  return mDomain;
}


/*
 * Returns the value of the "polygonObject" attribute of this ParametricObject.
 */
const PolygonObject*
ParametricObject::getPolygonObject() const
{
  return mPolygonObject;
}


/*
 * Returns the value of the "polygonObject" attribute of this ParametricObject.
 */
PolygonObject*
ParametricObject::getPolygonObject()
{
  return mPolygonObject;
}


/*
 * Creates a new "polygonObject" element of this ParametricObject and returns it.
 */
PolygonObject*
ParametricObject::createPolygonObject()
{
  if (mPolygonObject != NULL) delete mPolygonObject;
  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
  mPolygonObject = new PolygonObject(spatialns);
  delete spatialns;
  connectToChild();
  return mPolygonObject;
}


/*
 * Returns true/false if id is set.
 */
bool
ParametricObject::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Returns true/false if polygonType is set.
 */
bool
ParametricObject::isSetPolygonType() const
{
  return (mPolygonType.empty() == false);
}


/*
 * Returns true/false if domain is set.
 */
bool
ParametricObject::isSetDomain() const
{
  return (mDomain.empty() == false);
}


/*
 * Returns true/false if polygonObject is set.
 */
bool
ParametricObject::isSetPolygonObject() const
{
  return (mPolygonObject != NULL);
}


/*
 * Sets id and returns value indicating success.
 */
int
ParametricObject::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets polygonType and returns value indicating success.
 */
int
ParametricObject::setPolygonType(const std::string& polygonType)
{
  if (&(polygonType) == NULL)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mPolygonType = polygonType;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets domain and returns value indicating success.
 */
int
ParametricObject::setDomain(const std::string& domain)
{
  if (&(domain) == NULL)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else if (!(SyntaxChecker::isValidInternalSId(domain)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mDomain = domain;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets polygonObject and returns value indicating success.
 */
int
ParametricObject::setPolygonObject(PolygonObject* polygonObject)
{
  if (mPolygonObject == polygonObject)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else if (polygonObject == NULL)
  {
    delete mPolygonObject;
    mPolygonObject = NULL;
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    delete mPolygonObject;
    mPolygonObject = (polygonObject != NULL) ?
      static_cast<PolygonObject*>(polygonObject->clone()) : NULL;
    if (mPolygonObject != NULL)
    {
      mPolygonObject->connectToParent(this);
    }
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Unsets id and returns value indicating success.
 */
int
ParametricObject::unsetId()
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
 * Unsets polygonType and returns value indicating success.
 */
int
ParametricObject::unsetPolygonType()
{
  mPolygonType.erase();

  if (mPolygonType.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets domain and returns value indicating success.
 */
int
ParametricObject::unsetDomain()
{
  mDomain.erase();

  if (mDomain.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets polygonObject and returns value indicating success.
 */
int
ParametricObject::unsetPolygonObject()
{
  delete mPolygonObject;
  mPolygonObject = NULL;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * rename attributes that are SIdRefs or instances in math
 */
void
ParametricObject::renameSIdRefs(const std::string& oldid, const std::string& newid)
{
  if (isSetDomain() == true && mDomain == oldid)
  {
    setDomain(newid);
  }

}


List*
ParametricObject::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_POINTER(ret, sublist, mPolygonObject, filter);

  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}


/*
 * Returns the XML element name of this object
 */
const std::string&
ParametricObject::getElementName () const
{
  static const string name = "parametricObject";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
ParametricObject::getTypeCode () const
{
  return SBML_SPATIAL_PARAMETRICOBJECT;
}


/*
 * check if all the required attributes are set
 */
bool
ParametricObject::hasRequiredAttributes () const
{
  bool allPresent = true;

  if (isSetId() == false)
    allPresent = false;

  if (isSetPolygonType() == false)
    allPresent = false;

  if (isSetDomain() == false)
    allPresent = false;

  return allPresent;
}


/*
 * check if all the required elements are set
 */
bool
ParametricObject::hasRequiredElements () const
{
  bool allPresent = true;

  if (isSetPolygonObject() == false)
    allPresent = false;

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
ParametricObject::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);
  if (isSetPolygonObject() == true)
  {
    mPolygonObject->write(stream);
  }
  SBase::writeExtensionElements(stream);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor.
 */
bool
ParametricObject::accept (SBMLVisitor& v) const
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
ParametricObject::setSBMLDocument (SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
  if ( mPolygonObject != NULL)
    mPolygonObject->setSBMLDocument(d);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
   * Connects to child elements.
 */
void
ParametricObject::connectToChild()
{
  SBase::connectToChild();

  if (mPolygonObject != NULL)
    mPolygonObject->connectToParent(this);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
ParametricObject::enablePackageInternal(const std::string& pkgURI,
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
ParametricObject::createObject(XMLInputStream& stream)
{
  SBase* object = NULL;

  const string& name = stream.peek().getName();

  SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());

  if (name == "polygonObject")
  {
    mPolygonObject = new PolygonObject(spatialns);
    object = mPolygonObject;
  }

  delete spatialns;

  connectToChild();


  return object;
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Get the list of expected attributes for this element.
 */
void
ParametricObject::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("id");
  attributes.add("polygonType");
  attributes.add("domain");
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
ParametricObject::readAttributes (const XMLAttributes& attributes,
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
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details);
      }
      else if (getErrorLog()->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                          getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details);
      }
    }
  }

  bool assigned = false;

  //
  // id SId  ( use = "required" )
  //
  assigned = attributes.readInto("id", mId);

   if (assigned == true)
  {
    // check string is not empty and correct syntax

    if (mId.empty() == true)
    {
      logEmptyString(mId, getLevel(), getVersion(), "<ParametricObject>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mId) == false && getErrorLog() != NULL)
    {
      getErrorLog()->logError(InvalidIdSyntax, getLevel(), getVersion(), 
        "The syntax of the attribute id='" + mId + "' does not conform.");
    }
  }
  else
  {
    std::string message = "Spatial attribute 'id' is missing.";
    getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                   getPackageVersion(), sbmlLevel, sbmlVersion, message);
  }

  //
  // polygonType string   ( use = "required" )
  //
  assigned = attributes.readInto("polygonType", mPolygonType);

  if (assigned == true)
  {
    // check string is not empty

    if (mPolygonType.empty() == true)
    {
      logEmptyString(mPolygonType, getLevel(), getVersion(), "<ParametricObject>");
    }
  }
  else
  {
    std::string message = "Spatial attribute 'polygonType' is missing.";
    getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                   getPackageVersion(), sbmlLevel, sbmlVersion, message);
  }

  //
  // domain SIdRef   ( use = "required" )
  //
  assigned = attributes.readInto("domain", mDomain);

  if (assigned == true)
  {
    // check string is not empty and correct syntax

    if (mDomain.empty() == true)
    {
      logEmptyString(mDomain, getLevel(), getVersion(), "<ParametricObject>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mDomain) == false && getErrorLog() != NULL)
    {
      getErrorLog()->logError(InvalidIdSyntax, getLevel(), getVersion(), 
        "The syntax of the attribute domain='" + mDomain + "' does not conform.");
    }
  }
  else
  {
    std::string message = "Spatial attribute 'domain' is missing.";
    getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                   getPackageVersion(), sbmlLevel, sbmlVersion, message);
  }

}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write values of XMLAttributes to the output stream.
 */
  void
ParametricObject::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetId() == true)
    stream.writeAttribute("id", getPrefix(), mId);

  if (isSetPolygonType() == true)
    stream.writeAttribute("polygonType", getPrefix(), mPolygonType);

  if (isSetDomain() == true)
    stream.writeAttribute("domain", getPrefix(), mDomain);

}


  /** @endcond doxygenLibsbmlInternal */


LIBSBML_EXTERN
ParametricObject_t *
ParametricObject_create(unsigned int level, unsigned int version,
                        unsigned int pkgVersion)
{
  return new ParametricObject(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
ParametricObject_free(ParametricObject_t * po)
{
  if (po != NULL)
    delete po;
}


LIBSBML_EXTERN
ParametricObject_t *
ParametricObject_clone(ParametricObject_t * po)
{
  if (po != NULL)
  {
    return static_cast<ParametricObject_t*>(po->clone());
  }
  else
  {
    return NULL;
  }
}


LIBSBML_EXTERN
const char *
ParametricObject_getId(const ParametricObject_t * po)
{
	return (po != NULL && po->isSetId()) ? po->getId().c_str() : NULL;
}


LIBSBML_EXTERN
const char *
ParametricObject_getPolygonType(const ParametricObject_t * po)
{
	return (po != NULL && po->isSetPolygonType()) ? po->getPolygonType().c_str() : NULL;
}


LIBSBML_EXTERN
const char *
ParametricObject_getDomain(const ParametricObject_t * po)
{
	return (po != NULL && po->isSetDomain()) ? po->getDomain().c_str() : NULL;
}


LIBSBML_EXTERN
PolygonObject_t*
ParametricObject_getPolygonObject(ParametricObject_t * po)
{
	if (po == NULL)
		return NULL;

	return (PolygonObject_t*)po->getPolygonObject();
}


LIBSBML_EXTERN
PolygonObject_t*
ParametricObject_createPolygonObject(ParametricObject_t * po)
{
	if (po == NULL)
		return NULL;

	return (PolygonObject_t*)po->createPolygonObject();
}


LIBSBML_EXTERN
int
ParametricObject_isSetId(const ParametricObject_t * po)
{
  return (po != NULL) ? static_cast<int>(po->isSetId()) : 0;
}


LIBSBML_EXTERN
int
ParametricObject_isSetPolygonType(const ParametricObject_t * po)
{
  return (po != NULL) ? static_cast<int>(po->isSetPolygonType()) : 0;
}


LIBSBML_EXTERN
int
ParametricObject_isSetDomain(const ParametricObject_t * po)
{
  return (po != NULL) ? static_cast<int>(po->isSetDomain()) : 0;
}


LIBSBML_EXTERN
int
ParametricObject_isSetPolygonObject(const ParametricObject_t * po)
{
  return (po != NULL) ? static_cast<int>(po->isSetPolygonObject()) : 0;
}


LIBSBML_EXTERN
int
ParametricObject_setId(ParametricObject_t * po, const char * id)
{
  if (po != NULL)
    return (id == NULL) ? po->setId("") : po->setId(id);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
ParametricObject_setPolygonType(ParametricObject_t * po, const char * polygonType)
{
  if (po != NULL)
    return (polygonType == NULL) ? po->setPolygonType("") : po->setPolygonType(polygonType);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
ParametricObject_setDomain(ParametricObject_t * po, const char * domain)
{
  if (po != NULL)
    return (domain == NULL) ? po->setDomain("") : po->setDomain(domain);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
ParametricObject_setPolygonObject(ParametricObject_t * po, PolygonObject_t* polygonObject)
{
	return (po != NULL) ? po->setPolygonObject(polygonObject) : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
ParametricObject_unsetId(ParametricObject_t * po)
{
  return (po != NULL) ? po->unsetId() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
ParametricObject_unsetPolygonType(ParametricObject_t * po)
{
  return (po != NULL) ? po->unsetPolygonType() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
ParametricObject_unsetDomain(ParametricObject_t * po)
{
  return (po != NULL) ? po->unsetDomain() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
ParametricObject_hasRequiredAttributes(const ParametricObject_t * po)
{
  return (po != NULL) ? static_cast<int>(po->hasRequiredAttributes()) : 0;
}


LIBSBML_EXTERN
int
ParametricObject_hasRequiredElements(const ParametricObject_t * po)
{
	return (po != NULL) ? static_cast<int>(po->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


