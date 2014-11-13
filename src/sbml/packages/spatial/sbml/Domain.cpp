/**
 * @file:   Domain.cpp
 * @brief:  Implementation of the Domain class
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


#include <sbml/packages/spatial/sbml/Domain.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new Domain with the given level, version, and package version.
 */
Domain::Domain (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : SBase(level, version)
  , mId ("")
  , mDomainType ("")
  , mInteriorPoints (level, version, pkgVersion)
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version, pkgVersion));

  // connect to child objects
  connectToChild();
}


/*
 * Creates a new Domain with the given SpatialPkgNamespaces object.
 */
Domain::Domain (SpatialPkgNamespaces* spatialns)
  : SBase(spatialns)
  , mId ("")
  , mDomainType ("")
  , mInteriorPoints (spatialns)
{
  // set the element namespace of this object
  setElementNamespace(spatialns->getURI());

  // connect to child objects
  connectToChild();

  // load package extensions bound with this object (if any) 
  loadPlugins(spatialns);
}


/*
 * Copy constructor for Domain.
 */
Domain::Domain (const Domain& orig)
  : SBase(orig)
{
  if (&orig == NULL)
  {
    throw SBMLConstructorException("Null argument to copy constructor");
  }
  else
  {
    mId  = orig.mId;
    mDomainType  = orig.mDomainType;
    mInteriorPoints  = orig.mInteriorPoints;

    // connect to child objects
    connectToChild();
  }
}


/*
 * Assignment for Domain.
 */
Domain&
Domain::operator=(const Domain& rhs)
{
  if (&rhs == NULL)
  {
    throw SBMLConstructorException("Null argument to assignment");
  }
  else if (&rhs != this)
  {
    SBase::operator=(rhs);
    mId  = rhs.mId;
    mDomainType  = rhs.mDomainType;
    mInteriorPoints  = rhs.mInteriorPoints;

    // connect to child objects
    connectToChild();
  }
  return *this;
}


/*
 * Clone for Domain.
 */
Domain*
Domain::clone () const
{
  return new Domain(*this);
}


/*
 * Destructor for Domain.
 */
Domain::~Domain ()
{
}


/*
 * Returns the value of the "id" attribute of this Domain.
 */
const std::string&
Domain::getId() const
{
  return mId;
}


/*
 * Returns the value of the "domainType" attribute of this Domain.
 */
const std::string&
Domain::getDomainType() const
{
  return mDomainType;
}


/*
 * Returns true/false if id is set.
 */
bool
Domain::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Returns true/false if domainType is set.
 */
bool
Domain::isSetDomainType() const
{
  return (mDomainType.empty() == false);
}


/*
 * Sets id and returns value indicating success.
 */
int
Domain::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets domainType and returns value indicating success.
 */
int
Domain::setDomainType(const std::string& domainType)
{
  if (&(domainType) == NULL)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else if (!(SyntaxChecker::isValidInternalSId(domainType)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mDomainType = domainType;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Unsets id and returns value indicating success.
 */
int
Domain::unsetId()
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
 * Unsets domainType and returns value indicating success.
 */
int
Domain::unsetDomainType()
{
  mDomainType.erase();

  if (mDomainType.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Returns the  "ListOfInteriorPoints" in this Domain object.
 */
const ListOfInteriorPoints*
Domain::getListOfInteriorPoints() const
{
  return &mInteriorPoints;
}


/*
 * Returns the  "ListOfInteriorPoints" in this Domain object.
 */
ListOfInteriorPoints*
Domain::getListOfInteriorPoints()
{
  return &mInteriorPoints;
}


/*
 * Removes the nth InteriorPoint from the ListOfInteriorPoints.
 */
InteriorPoint*
Domain::removeInteriorPoint(unsigned int n)
{
	return mInteriorPoints.remove(n);
}


/*
 * Removes the a InteriorPoint with given id from the ListOfInteriorPoints.
 */
InteriorPoint*
Domain::removeInteriorPoint(const std::string& sid)
{
	return mInteriorPoints.remove(sid);
}


/*
 * Return the nth InteriorPoint in the ListOfInteriorPoints within this Domain.
 */
InteriorPoint*
Domain::getInteriorPoint(unsigned int n)
{
	return mInteriorPoints.get(n);
}


/*
 * Return the nth InteriorPoint in the ListOfInteriorPoints within this Domain.
 */
const InteriorPoint*
Domain::getInteriorPoint(unsigned int n) const
{
	return mInteriorPoints.get(n);
}


/*
 * Return a InteriorPoint from the ListOfInteriorPoints by id.
 */
InteriorPoint*
Domain::getInteriorPoint(const std::string& sid)
{
	return mInteriorPoints.get(sid);
}


/*
 * Return a InteriorPoint from the ListOfInteriorPoints by id.
 */
const InteriorPoint*
Domain::getInteriorPoint(const std::string& sid) const
{
	return mInteriorPoints.get(sid);
}


/*
 * Adds a copy the given "InteriorPoint" to this Domain.
 *
 * @param ip; the InteriorPoint object to add
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 */
int
Domain::addInteriorPoint(const InteriorPoint* ip)
{
  if (ip == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (ip->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != ip->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != ip->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const SBase *>(ip)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    mInteriorPoints.append(ip);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Get the number of InteriorPoint objects in this Domain.
 *
 * @return the number of InteriorPoint objects in this Domain
 */
unsigned int
Domain::getNumInteriorPoints() const
{
  return mInteriorPoints.size();
}


/*
 * Creates a new InteriorPoint object, adds it to this Domains
 * Domain and returns the InteriorPoint object created. 
 *
 * @return a new InteriorPoint object instance
 *
 * @see addInteriorPoint(const InteriorPoint* ip)
 */
InteriorPoint*
Domain::createInteriorPoint()
{
  InteriorPoint* ip = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    ip = new InteriorPoint(spatialns);
    delete spatialns;
  }
  catch (...)
  {
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * do nothing
     */
  }

  if(ip != NULL)
  {
    mInteriorPoints.appendAndOwn(ip);
  }

  return ip;
}


/*
 * rename attributes that are SIdRefs or instances in math
 */
void
Domain::renameSIdRefs(const std::string& oldid, const std::string& newid)
{
  if (isSetDomainType() == true && mDomainType == oldid)
  {
    setDomainType(newid);
  }

}


List*
Domain::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;


  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}


/*
 * Returns the XML element name of this object
 */
const std::string&
Domain::getElementName () const
{
  static const string name = "domain";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
Domain::getTypeCode () const
{
  return SBML_SPATIAL_DOMAIN;
}


/*
 * check if all the required attributes are set
 */
bool
Domain::hasRequiredAttributes () const
{
  bool allPresent = true;

  if (isSetId() == false)
    allPresent = false;

  if (isSetDomainType() == false)
    allPresent = false;

  return allPresent;
}


/*
 * check if all the required elements are set
 */
bool
Domain::hasRequiredElements () const
{
  bool allPresent = true;

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
Domain::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);
  if (getNumInteriorPoints() > 0)
  {
    mInteriorPoints.write(stream);
  }

  SBase::writeExtensionElements(stream);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor.
 */
bool
Domain::accept (SBMLVisitor& v) const
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
Domain::setSBMLDocument (SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
  mInteriorPoints.setSBMLDocument(d);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
   * Connects to child elements.
 */
void
Domain::connectToChild()
{
  SBase::connectToChild();

  mInteriorPoints.connectToParent(this);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
Domain::enablePackageInternal(const std::string& pkgURI,
             const std::string& pkgPrefix, bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);
  mInteriorPoints.enablePackageInternal(pkgURI, pkgPrefix, flag);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * creates object.
 */
SBase*
Domain::createObject(XMLInputStream& stream)
{
  SBase* object = NULL;

  const string& name = stream.peek().getName();

  if (name == "listOfInteriorPoints")
  {
    object = &mInteriorPoints;
  }
  connectToChild();


  return object;
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Get the list of expected attributes for this element.
 */
void
Domain::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("id");
  attributes.add("domainType");
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
Domain::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  unsigned int numErrs;

  /* look to see whether an unknown attribute error was logged
   * during the read of the listOfDomains - which will have
   * happened immediately prior to this read
  */

  if (getErrorLog() != NULL &&
      static_cast<ListOfDomains*>(getParentSBMLObject())->size() < 2)
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
                  getPackageVersion(), sbmlLevel, sbmlVersion, details, getLine(), getColumn());
      }
      else if (getErrorLog()->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                   getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                  getPackageVersion(), sbmlLevel, sbmlVersion, details, getLine(), getColumn());
      }
    }
  }

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
                       getPackageVersion(), sbmlLevel, sbmlVersion, details, getLine(), getColumn());
      }
      else if (getErrorLog()->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details =
                          getErrorLog()->getError(n)->getMessage();
        getErrorLog()->remove(UnknownCoreAttribute);
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, details, getLine(), getColumn());
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
      logEmptyString(mId, getLevel(), getVersion(), "<Domain>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mId) == false && getErrorLog() != NULL)
    {
      getErrorLog()->logError(InvalidIdSyntax, getLevel(), getVersion(), 
        "The syntax of the attribute id='" + mId + "' does not conform.", getLine(), getColumn());
    }
  }
  else
  {
    std::string message = "Spatial attribute 'id' is missing from 'domain' object.";
    getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                   getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
  }

  //
  // domainType SIdRef   ( use = "required" )
  //
  assigned = attributes.readInto("domainType", mDomainType);

  if (assigned == true)
  {
    // check string is not empty and correct syntax

    if (mDomainType.empty() == true)
    {
      logEmptyString(mDomainType, getLevel(), getVersion(), "<Domain>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mDomainType) == false && getErrorLog() != NULL)
    {
      getErrorLog()->logError(InvalidIdSyntax, getLevel(), getVersion(), 
        "The syntax of the attribute domainType='" + mDomainType + "' does not conform.");
    }
  }
  else
  {
    std::string message = "Spatial attribute 'domainType' is missing from 'domain' object.";
    getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                   getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
  }

}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write values of XMLAttributes to the output stream.
 */
  void
Domain::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetId() == true)
    stream.writeAttribute("id", getPrefix(), mId);

  if (isSetDomainType() == true)
    stream.writeAttribute("domainType", getPrefix(), mDomainType);

}


  /** @endcond doxygenLibsbmlInternal */


/*
 * Constructor 
 */
ListOfDomains::ListOfDomains(unsigned int level, 
                unsigned int version, 
                unsigned int pkgVersion)
 : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version, pkgVersion)); 
}


/*
 * Constructor 
 */
ListOfDomains::ListOfDomains(SpatialPkgNamespaces* spatialns)
  : ListOf(spatialns)
{
  setElementNamespace(spatialns->getURI());
}


/*
 * Returns a deep copy of this ListOfDomains 
 */
ListOfDomains* 
ListOfDomains::clone () const
 {
  return new ListOfDomains(*this);
}


/*
 * Get a Domain from the ListOfDomains by index.
*/
Domain*
ListOfDomains::get(unsigned int n)
{
  return static_cast<Domain*>(ListOf::get(n));
}


/*
 * Get a Domain from the ListOfDomains by index.
 */
const Domain*
ListOfDomains::get(unsigned int n) const
{
  return static_cast<const Domain*>(ListOf::get(n));
}


/*
 * Get a Domain from the ListOfDomains by id.
 */
Domain*
ListOfDomains::get(const std::string& sid)
{
	return const_cast<Domain*>(
    static_cast<const ListOfDomains&>(*this).get(sid));
}


/*
 * Get a Domain from the ListOfDomains by id.
 */
const Domain*
ListOfDomains::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<Domain>(sid) );
  return (result == mItems.end()) ? 0 : static_cast <Domain*> (*result);
}


/**
 * Adds a copy the given "Domain" to this ListOfDomains.
 *
 * @param d; the Domain object to add
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 */
int
ListOfDomains::addDomain(const Domain* d)
{
  if (d == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (d->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != d->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != d->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const SBase *>(d)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
	append(d);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/**
 * Get the number of Domain objects in this ListOfDomains.
 *
 * @return the number of Domain objects in this ListOfDomains
 */
unsigned int 
ListOfDomains::getNumDomains() const
{
	return size();
}

/**
 * Creates a new Domain object, adds it to this ListOfDomains
 * Domain and returns the Domain object created. 
 *
 * @return a new Domain object instance
 *
 * @see addDomain(const Domain* d)
 */
Domain* 
ListOfDomains::createDomain()
{
  Domain* d = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    d = new Domain(spatialns);
    delete spatialns;
  }
  catch (...)
  {
    /* here we do not create a default object as the level/version must
     * match the parent object
     *
     * do nothing
     */
  }

  if(d != NULL)
  {
    appendAndOwn(d);
  }

  return d;
}

/*
 * Removes the nth Domain from this ListOfDomains
 */
Domain*
ListOfDomains::remove(unsigned int n)
{
  return static_cast<Domain*>(ListOf::remove(n));
}


/*
 * Removes the Domain from this ListOfDomains with the given identifier
 */
Domain*
ListOfDomains::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<Domain>(sid) );

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

	return static_cast <Domain*> (item);
}


/*
 * Returns the XML element name of this object
 */
const std::string&
ListOfDomains::getElementName () const
{
  static const string name = "listOfDomains";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
ListOfDomains::getTypeCode () const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the objects in this LIST_OF.
 */
int
ListOfDomains::getItemTypeCode () const
{
  return SBML_SPATIAL_DOMAIN;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * Creates a new Domain in this ListOfDomains
 */
SBase*
ListOfDomains::createObject(XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase* object = NULL;

  if (name == "domain")
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    object = new Domain(spatialns);
    appendAndOwn(object);
    delete spatialns;
  }

  return object;
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write the namespace for the Spatial package.
 */
void
ListOfDomains::writeXMLNS(XMLOutputStream& stream) const
{
  XMLNamespaces xmlns;

  std::string prefix = getPrefix();

  if (prefix.empty())
  {
    XMLNamespaces* thisxmlns = getNamespaces();
    if (thisxmlns && thisxmlns->hasURI(SpatialExtension::getXmlnsL3V1V1()))
    {
      xmlns.add(SpatialExtension::getXmlnsL3V1V1(),prefix);
    }
  }

  stream << xmlns;
}


  /** @endcond doxygenLibsbmlInternal */


LIBSBML_EXTERN
Domain_t *
Domain_create(unsigned int level, unsigned int version,
              unsigned int pkgVersion)
{
  return new Domain(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
Domain_free(Domain_t * d)
{
  if (d != NULL)
    delete d;
}


LIBSBML_EXTERN
Domain_t *
Domain_clone(Domain_t * d)
{
  if (d != NULL)
  {
    return static_cast<Domain_t*>(d->clone());
  }
  else
  {
    return NULL;
  }
}


LIBSBML_EXTERN
const char *
Domain_getId(const Domain_t * d)
{
	return (d != NULL && d->isSetId()) ? d->getId().c_str() : NULL;
}


LIBSBML_EXTERN
const char *
Domain_getDomainType(const Domain_t * d)
{
	return (d != NULL && d->isSetDomainType()) ? d->getDomainType().c_str() : NULL;
}


LIBSBML_EXTERN
int
Domain_isSetId(const Domain_t * d)
{
  return (d != NULL) ? static_cast<int>(d->isSetId()) : 0;
}


LIBSBML_EXTERN
int
Domain_isSetDomainType(const Domain_t * d)
{
  return (d != NULL) ? static_cast<int>(d->isSetDomainType()) : 0;
}


LIBSBML_EXTERN
int
Domain_setId(Domain_t * d, const char * id)
{
  if (d != NULL)
    return (id == NULL) ? d->setId("") : d->setId(id);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
Domain_setDomainType(Domain_t * d, const char * domainType)
{
  if (d != NULL)
    return (domainType == NULL) ? d->setDomainType("") : d->setDomainType(domainType);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
Domain_unsetId(Domain_t * d)
{
  return (d != NULL) ? d->unsetId() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
Domain_unsetDomainType(Domain_t * d)
{
  return (d != NULL) ? d->unsetDomainType() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
Domain_addInteriorPoint(Domain_t * d, InteriorPoint_t * ip)
{
	return  (d != NULL) ? d->addInteriorPoint(ip) : LIBSBML_INVALID_OBJECT;
}

LIBSBML_EXTERN
InteriorPoint_t *
Domain_createInteriorPoint(Domain_t * d)
{
	return  (d != NULL) ? d->createInteriorPoint() : NULL;
}

LIBSBML_EXTERN
ListOf_t *
Domain_getListOfInteriorPoints(Domain_t * d)
{
	return  (d != NULL) ? (ListOf_t *)d->getListOfInteriorPoints() : NULL;
}

LIBSBML_EXTERN
InteriorPoint_t *
Domain_getInteriorPoint(Domain_t * d, unsigned int n)
{
	return  (d != NULL) ? d->getInteriorPoint(n) : NULL;
}

LIBSBML_EXTERN
InteriorPoint_t *
Domain_getInteriorPointById(Domain_t * d, const char * sid)
{
	return  (d != NULL) ? d->getInteriorPoint(sid) : NULL;
}

LIBSBML_EXTERN
unsigned int
Domain_getNumInteriorPoints(Domain_t * d)
{
	return  (d != NULL) ? d->getNumInteriorPoints() : SBML_INT_MAX;
}

LIBSBML_EXTERN
InteriorPoint_t *
Domain_removeInteriorPoint(Domain_t * d, unsigned int n)
{
	return  (d != NULL) ? d->removeInteriorPoint(n) : NULL;
}

LIBSBML_EXTERN
InteriorPoint_t *
Domain_removeInteriorPointById(Domain_t * d, const char * sid)
{
	return  (d != NULL) ? d->removeInteriorPoint(sid) : NULL;
}

LIBSBML_EXTERN
int
Domain_hasRequiredAttributes(const Domain_t * d)
{
  return (d != NULL) ? static_cast<int>(d->hasRequiredAttributes()) : 0;
}


LIBSBML_EXTERN
int
Domain_hasRequiredElements(const Domain_t * d)
{
	return (d != NULL) ? static_cast<int>(d->hasRequiredElements()) : 0;
}


/*
 *
 */
LIBSBML_EXTERN
Domain_t *
ListOfDomains_getById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfDomains *>(lo)->get(sid) : NULL;
}


/*
 *
 */
LIBSBML_EXTERN
Domain_t *
ListOfDomains_removeById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfDomains *>(lo)->remove(sid) : NULL;
}




LIBSBML_CPP_NAMESPACE_END


