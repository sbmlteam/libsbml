/**
 * @file:   AdjacentDomains.cpp
 * @brief:  Implementation of the AdjacentDomains class
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


#include <sbml/packages/spatial/sbml/AdjacentDomains.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new AdjacentDomains with the given level, version, and package version.
 */
AdjacentDomains::AdjacentDomains (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : SBase(level, version)
  , mId ("")
  , mDomain1 ("")
  , mDomain2 ("")
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new AdjacentDomains with the given SpatialPkgNamespaces object.
 */
AdjacentDomains::AdjacentDomains (SpatialPkgNamespaces* spatialns)
  : SBase(spatialns)
  , mId ("")
  , mDomain1 ("")
  , mDomain2 ("")
{
  // set the element namespace of this object
  setElementNamespace(spatialns->getURI());

  // load package extensions bound with this object (if any) 
  loadPlugins(spatialns);
}


/*
 * Copy constructor for AdjacentDomains.
 */
AdjacentDomains::AdjacentDomains (const AdjacentDomains& orig)
  : SBase(orig)
{
  if (&orig == NULL)
  {
    throw SBMLConstructorException("Null argument to copy constructor");
  }
  else
  {
    mId  = orig.mId;
    mDomain1  = orig.mDomain1;
    mDomain2  = orig.mDomain2;
  }
}


/*
 * Assignment for AdjacentDomains.
 */
AdjacentDomains&
AdjacentDomains::operator=(const AdjacentDomains& rhs)
{
  if (&rhs == NULL)
  {
    throw SBMLConstructorException("Null argument to assignment");
  }
  else if (&rhs != this)
  {
    SBase::operator=(rhs);
    mId  = rhs.mId;
    mDomain1  = rhs.mDomain1;
    mDomain2  = rhs.mDomain2;
  }
  return *this;
}


/*
 * Clone for AdjacentDomains.
 */
AdjacentDomains*
AdjacentDomains::clone () const
{
  return new AdjacentDomains(*this);
}


/*
 * Destructor for AdjacentDomains.
 */
AdjacentDomains::~AdjacentDomains ()
{
}


/*
 * Returns the value of the "id" attribute of this AdjacentDomains.
 */
const std::string&
AdjacentDomains::getId() const
{
  return mId;
}


/*
 * Returns the value of the "domain1" attribute of this AdjacentDomains.
 */
const std::string&
AdjacentDomains::getDomain1() const
{
  return mDomain1;
}


/*
 * Returns the value of the "domain2" attribute of this AdjacentDomains.
 */
const std::string&
AdjacentDomains::getDomain2() const
{
  return mDomain2;
}


/*
 * Returns true/false if id is set.
 */
bool
AdjacentDomains::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Returns true/false if domain1 is set.
 */
bool
AdjacentDomains::isSetDomain1() const
{
  return (mDomain1.empty() == false);
}


/*
 * Returns true/false if domain2 is set.
 */
bool
AdjacentDomains::isSetDomain2() const
{
  return (mDomain2.empty() == false);
}


/*
 * Sets id and returns value indicating success.
 */
int
AdjacentDomains::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets domain1 and returns value indicating success.
 */
int
AdjacentDomains::setDomain1(const std::string& domain1)
{
  if (&(domain1) == NULL)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else if (!(SyntaxChecker::isValidInternalSId(domain1)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mDomain1 = domain1;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets domain2 and returns value indicating success.
 */
int
AdjacentDomains::setDomain2(const std::string& domain2)
{
  if (&(domain2) == NULL)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else if (!(SyntaxChecker::isValidInternalSId(domain2)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mDomain2 = domain2;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Unsets id and returns value indicating success.
 */
int
AdjacentDomains::unsetId()
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
 * Unsets domain1 and returns value indicating success.
 */
int
AdjacentDomains::unsetDomain1()
{
  mDomain1.erase();

  if (mDomain1.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets domain2 and returns value indicating success.
 */
int
AdjacentDomains::unsetDomain2()
{
  mDomain2.erase();

  if (mDomain2.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * rename attributes that are SIdRefs or instances in math
 */
void
AdjacentDomains::renameSIdRefs(const std::string& oldid, const std::string& newid)
{
  if (isSetDomain1() == true && mDomain1 == oldid)
  {
    setDomain1(newid);
  }

  if (isSetDomain2() == true && mDomain2 == oldid)
  {
    setDomain2(newid);
  }

}


/*
 * Returns the XML element name of this object
 */
const std::string&
AdjacentDomains::getElementName () const
{
  static const string name = "adjacentDomains";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
AdjacentDomains::getTypeCode () const
{
  return SBML_SPATIAL_ADJACENTDOMAINS;
}


/*
 * check if all the required attributes are set
 */
bool
AdjacentDomains::hasRequiredAttributes () const
{
  bool allPresent = true;

  if (isSetId() == false)
    allPresent = false;

  if (isSetDomain1() == false)
    allPresent = false;

  if (isSetDomain2() == false)
    allPresent = false;

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
AdjacentDomains::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);
  SBase::writeExtensionElements(stream);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor.
 */
bool
AdjacentDomains::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument.
 */
void
AdjacentDomains::setSBMLDocument (SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
AdjacentDomains::enablePackageInternal(const std::string& pkgURI,
             const std::string& pkgPrefix, bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Get the list of expected attributes for this element.
 */
void
AdjacentDomains::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("id");
  attributes.add("domain1");
  attributes.add("domain2");
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
AdjacentDomains::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  unsigned int numErrs;

  /* look to see whether an unknown attribute error was logged
   * during the read of the listOfAdjacentDomains - which will have
   * happened immediately prior to this read
  */

  if (getErrorLog() != NULL &&
      static_cast<ListOfAdjacentDomains*>(getParentSBMLObject())->size() < 2)
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
      logEmptyString(mId, getLevel(), getVersion(), "<AdjacentDomains>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mId) == false && getErrorLog() != NULL)
    {
      getErrorLog()->logError(InvalidIdSyntax, getLevel(), getVersion(), 
        "The syntax of the attribute id='" + mId + "' does not conform.", getLine(), getColumn());
    }
  }
  else
  {
    std::string message = "Spatial attribute 'id' is missing from 'adjacentDomains' object.";
    getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                   getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
  }

  //
  // domain1 SIdRef   ( use = "required" )
  //
  assigned = attributes.readInto("domain1", mDomain1);

  if (assigned == true)
  {
    // check string is not empty and correct syntax

    if (mDomain1.empty() == true)
    {
      logEmptyString(mDomain1, getLevel(), getVersion(), "<AdjacentDomains>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mDomain1) == false && getErrorLog() != NULL)
    {
      getErrorLog()->logError(InvalidIdSyntax, getLevel(), getVersion(), 
        "The syntax of the attribute domain1='" + mDomain1 + "' does not conform.");
    }
  }
  else
  {
    std::string message = "Spatial attribute 'domain1' is missing from 'adjacentDomains' object.";
    getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                   getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
  }

  //
  // domain2 SIdRef   ( use = "required" )
  //
  assigned = attributes.readInto("domain2", mDomain2);

  if (assigned == true)
  {
    // check string is not empty and correct syntax

    if (mDomain2.empty() == true)
    {
      logEmptyString(mDomain2, getLevel(), getVersion(), "<AdjacentDomains>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mDomain2) == false && getErrorLog() != NULL)
    {
      getErrorLog()->logError(InvalidIdSyntax, getLevel(), getVersion(), 
        "The syntax of the attribute domain2='" + mDomain2 + "' does not conform.");
    }
  }
  else
  {
    std::string message = "Spatial attribute 'domain2' is missing from 'adjacentDomains' object.";
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
AdjacentDomains::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetId() == true)
    stream.writeAttribute("id", getPrefix(), mId);

  if (isSetDomain1() == true)
    stream.writeAttribute("domain1", getPrefix(), mDomain1);

  if (isSetDomain2() == true)
    stream.writeAttribute("domain2", getPrefix(), mDomain2);

}


  /** @endcond doxygenLibsbmlInternal */


/*
 * Constructor 
 */
ListOfAdjacentDomains::ListOfAdjacentDomains(unsigned int level, 
                        unsigned int version, 
                        unsigned int pkgVersion)
 : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version, pkgVersion)); 
}


/*
 * Constructor 
 */
ListOfAdjacentDomains::ListOfAdjacentDomains(SpatialPkgNamespaces* spatialns)
  : ListOf(spatialns)
{
  setElementNamespace(spatialns->getURI());
}


/*
 * Returns a deep copy of this ListOfAdjacentDomains 
 */
ListOfAdjacentDomains* 
ListOfAdjacentDomains::clone () const
 {
  return new ListOfAdjacentDomains(*this);
}


/*
 * Get a AdjacentDomains from the ListOfAdjacentDomains by index.
*/
AdjacentDomains*
ListOfAdjacentDomains::get(unsigned int n)
{
  return static_cast<AdjacentDomains*>(ListOf::get(n));
}


/*
 * Get a AdjacentDomains from the ListOfAdjacentDomains by index.
 */
const AdjacentDomains*
ListOfAdjacentDomains::get(unsigned int n) const
{
  return static_cast<const AdjacentDomains*>(ListOf::get(n));
}


/*
 * Get a AdjacentDomains from the ListOfAdjacentDomains by id.
 */
AdjacentDomains*
ListOfAdjacentDomains::get(const std::string& sid)
{
	return const_cast<AdjacentDomains*>(
    static_cast<const ListOfAdjacentDomains&>(*this).get(sid));
}


/*
 * Get a AdjacentDomains from the ListOfAdjacentDomains by id.
 */
const AdjacentDomains*
ListOfAdjacentDomains::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<AdjacentDomains>(sid) );
  return (result == mItems.end()) ? 0 : static_cast <AdjacentDomains*> (*result);
}


/**
 * Adds a copy the given "AdjacentDomains" to this ListOfAdjacentDomains.
 *
 * @param ad; the AdjacentDomains object to add
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 */
int
ListOfAdjacentDomains::addAdjacentDomains(const AdjacentDomains* ad)
{
  if (ad == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (ad->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != ad->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != ad->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const SBase *>(ad)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
	append(ad);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/**
 * Get the number of AdjacentDomains objects in this ListOfAdjacentDomains.
 *
 * @return the number of AdjacentDomains objects in this ListOfAdjacentDomains
 */
unsigned int 
ListOfAdjacentDomains::getNumAdjacentDomains() const
{
	return size();
}

/**
 * Creates a new AdjacentDomains object, adds it to this ListOfAdjacentDomains
 * AdjacentDomains and returns the AdjacentDomains object created. 
 *
 * @return a new AdjacentDomains object instance
 *
 * @see addAdjacentDomains(const AdjacentDomains* ad)
 */
AdjacentDomains* 
ListOfAdjacentDomains::createAdjacentDomains()
{
  AdjacentDomains* ad = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    ad = new AdjacentDomains(spatialns);
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

  if(ad != NULL)
  {
    appendAndOwn(ad);
  }

  return ad;
}

/*
 * Removes the nth AdjacentDomains from this ListOfAdjacentDomains
 */
AdjacentDomains*
ListOfAdjacentDomains::remove(unsigned int n)
{
  return static_cast<AdjacentDomains*>(ListOf::remove(n));
}


/*
 * Removes the AdjacentDomains from this ListOfAdjacentDomains with the given identifier
 */
AdjacentDomains*
ListOfAdjacentDomains::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<AdjacentDomains>(sid) );

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

	return static_cast <AdjacentDomains*> (item);
}


/*
 * Returns the XML element name of this object
 */
const std::string&
ListOfAdjacentDomains::getElementName () const
{
  static const string name = "listOfAdjacentDomains";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
ListOfAdjacentDomains::getTypeCode () const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the objects in this LIST_OF.
 */
int
ListOfAdjacentDomains::getItemTypeCode () const
{
  return SBML_SPATIAL_ADJACENTDOMAINS;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * Creates a new AdjacentDomains in this ListOfAdjacentDomains
 */
SBase*
ListOfAdjacentDomains::createObject(XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase* object = NULL;

  if (name == "adjacentDomains")
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    object = new AdjacentDomains(spatialns);
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
ListOfAdjacentDomains::writeXMLNS(XMLOutputStream& stream) const
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
AdjacentDomains_t *
AdjacentDomains_create(unsigned int level, unsigned int version,
                       unsigned int pkgVersion)
{
  return new AdjacentDomains(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
AdjacentDomains_free(AdjacentDomains_t * ad)
{
  if (ad != NULL)
    delete ad;
}


LIBSBML_EXTERN
AdjacentDomains_t *
AdjacentDomains_clone(AdjacentDomains_t * ad)
{
  if (ad != NULL)
  {
    return static_cast<AdjacentDomains_t*>(ad->clone());
  }
  else
  {
    return NULL;
  }
}


LIBSBML_EXTERN
const char *
AdjacentDomains_getId(const AdjacentDomains_t * ad)
{
	return (ad != NULL && ad->isSetId()) ? ad->getId().c_str() : NULL;
}


LIBSBML_EXTERN
const char *
AdjacentDomains_getDomain1(const AdjacentDomains_t * ad)
{
	return (ad != NULL && ad->isSetDomain1()) ? ad->getDomain1().c_str() : NULL;
}


LIBSBML_EXTERN
const char *
AdjacentDomains_getDomain2(const AdjacentDomains_t * ad)
{
	return (ad != NULL && ad->isSetDomain2()) ? ad->getDomain2().c_str() : NULL;
}


LIBSBML_EXTERN
int
AdjacentDomains_isSetId(const AdjacentDomains_t * ad)
{
  return (ad != NULL) ? static_cast<int>(ad->isSetId()) : 0;
}


LIBSBML_EXTERN
int
AdjacentDomains_isSetDomain1(const AdjacentDomains_t * ad)
{
  return (ad != NULL) ? static_cast<int>(ad->isSetDomain1()) : 0;
}


LIBSBML_EXTERN
int
AdjacentDomains_isSetDomain2(const AdjacentDomains_t * ad)
{
  return (ad != NULL) ? static_cast<int>(ad->isSetDomain2()) : 0;
}


LIBSBML_EXTERN
int
AdjacentDomains_setId(AdjacentDomains_t * ad, const char * id)
{
  if (ad != NULL)
    return (id == NULL) ? ad->setId("") : ad->setId(id);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
AdjacentDomains_setDomain1(AdjacentDomains_t * ad, const char * domain1)
{
  if (ad != NULL)
    return (domain1 == NULL) ? ad->setDomain1("") : ad->setDomain1(domain1);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
AdjacentDomains_setDomain2(AdjacentDomains_t * ad, const char * domain2)
{
  if (ad != NULL)
    return (domain2 == NULL) ? ad->setDomain2("") : ad->setDomain2(domain2);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
AdjacentDomains_unsetId(AdjacentDomains_t * ad)
{
  return (ad != NULL) ? ad->unsetId() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
AdjacentDomains_unsetDomain1(AdjacentDomains_t * ad)
{
  return (ad != NULL) ? ad->unsetDomain1() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
AdjacentDomains_unsetDomain2(AdjacentDomains_t * ad)
{
  return (ad != NULL) ? ad->unsetDomain2() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
AdjacentDomains_hasRequiredAttributes(const AdjacentDomains_t * ad)
{
  return (ad != NULL) ? static_cast<int>(ad->hasRequiredAttributes()) : 0;
}


/*
 *
 */
LIBSBML_EXTERN
AdjacentDomains_t *
ListOfAdjacentDomains_getById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfAdjacentDomains *>(lo)->get(sid) : NULL;
}


/*
 *
 */
LIBSBML_EXTERN
AdjacentDomains_t *
ListOfAdjacentDomains_removeById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfAdjacentDomains *>(lo)->remove(sid) : NULL;
}




LIBSBML_CPP_NAMESPACE_END


