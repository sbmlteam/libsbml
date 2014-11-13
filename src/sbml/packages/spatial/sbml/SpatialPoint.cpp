/**
 * @file:   SpatialPoint.cpp
 * @brief:  Implementation of the SpatialPoint class
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


#include <sbml/packages/spatial/sbml/SpatialPoint.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new SpatialPoint with the given level, version, and package version.
 */
SpatialPoint::SpatialPoint (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : SBase(level, version)
  , mId ("")
  , mCoord1 (numeric_limits<double>::quiet_NaN())
  , mIsSetCoord1 (false)
  , mCoord2 (numeric_limits<double>::quiet_NaN())
  , mIsSetCoord2 (false)
  , mCoord3 (numeric_limits<double>::quiet_NaN())
  , mIsSetCoord3 (false)
  , mDomain ("")
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new SpatialPoint with the given SpatialPkgNamespaces object.
 */
SpatialPoint::SpatialPoint (SpatialPkgNamespaces* spatialns)
  : SBase(spatialns)
  , mId ("")
  , mCoord1 (numeric_limits<double>::quiet_NaN())
  , mIsSetCoord1 (false)
  , mCoord2 (numeric_limits<double>::quiet_NaN())
  , mIsSetCoord2 (false)
  , mCoord3 (numeric_limits<double>::quiet_NaN())
  , mIsSetCoord3 (false)
  , mDomain ("")
{
  // set the element namespace of this object
  setElementNamespace(spatialns->getURI());

  // load package extensions bound with this object (if any) 
  loadPlugins(spatialns);
}


/*
 * Copy constructor for SpatialPoint.
 */
SpatialPoint::SpatialPoint (const SpatialPoint& orig)
  : SBase(orig)
{
  if (&orig == NULL)
  {
    throw SBMLConstructorException("Null argument to copy constructor");
  }
  else
  {
    mId  = orig.mId;
    mCoord1  = orig.mCoord1;
    mIsSetCoord1  = orig.mIsSetCoord1;
    mCoord2  = orig.mCoord2;
    mIsSetCoord2  = orig.mIsSetCoord2;
    mCoord3  = orig.mCoord3;
    mIsSetCoord3  = orig.mIsSetCoord3;
    mDomain  = orig.mDomain;
  }
}


/*
 * Assignment for SpatialPoint.
 */
SpatialPoint&
SpatialPoint::operator=(const SpatialPoint& rhs)
{
  if (&rhs == NULL)
  {
    throw SBMLConstructorException("Null argument to assignment");
  }
  else if (&rhs != this)
  {
    SBase::operator=(rhs);
    mId  = rhs.mId;
    mCoord1  = rhs.mCoord1;
    mIsSetCoord1  = rhs.mIsSetCoord1;
    mCoord2  = rhs.mCoord2;
    mIsSetCoord2  = rhs.mIsSetCoord2;
    mCoord3  = rhs.mCoord3;
    mIsSetCoord3  = rhs.mIsSetCoord3;
    mDomain  = rhs.mDomain;
  }
  return *this;
}


/*
 * Clone for SpatialPoint.
 */
SpatialPoint*
SpatialPoint::clone () const
{
  return new SpatialPoint(*this);
}


/*
 * Destructor for SpatialPoint.
 */
SpatialPoint::~SpatialPoint ()
{
}


/*
 * Returns the value of the "id" attribute of this SpatialPoint.
 */
const std::string&
SpatialPoint::getId() const
{
  return mId;
}


/*
 * Returns the value of the "coord1" attribute of this SpatialPoint.
 */
double
SpatialPoint::getCoord1() const
{
  return mCoord1;
}


/*
 * Returns the value of the "coord2" attribute of this SpatialPoint.
 */
double
SpatialPoint::getCoord2() const
{
  return mCoord2;
}


/*
 * Returns the value of the "coord3" attribute of this SpatialPoint.
 */
double
SpatialPoint::getCoord3() const
{
  return mCoord3;
}


/*
 * Returns the value of the "domain" attribute of this SpatialPoint.
 */
const std::string&
SpatialPoint::getDomain() const
{
  return mDomain;
}


/*
 * Returns true/false if id is set.
 */
bool
SpatialPoint::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Returns true/false if coord1 is set.
 */
bool
SpatialPoint::isSetCoord1() const
{
  return mIsSetCoord1;
}


/*
 * Returns true/false if coord2 is set.
 */
bool
SpatialPoint::isSetCoord2() const
{
  return mIsSetCoord2;
}


/*
 * Returns true/false if coord3 is set.
 */
bool
SpatialPoint::isSetCoord3() const
{
  return mIsSetCoord3;
}


/*
 * Returns true/false if domain is set.
 */
bool
SpatialPoint::isSetDomain() const
{
  return (mDomain.empty() == false);
}


/*
 * Sets id and returns value indicating success.
 */
int
SpatialPoint::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets coord1 and returns value indicating success.
 */
int
SpatialPoint::setCoord1(double coord1)
{
  mCoord1 = coord1;
  mIsSetCoord1 = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets coord2 and returns value indicating success.
 */
int
SpatialPoint::setCoord2(double coord2)
{
  mCoord2 = coord2;
  mIsSetCoord2 = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets coord3 and returns value indicating success.
 */
int
SpatialPoint::setCoord3(double coord3)
{
  mCoord3 = coord3;
  mIsSetCoord3 = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets domain and returns value indicating success.
 */
int
SpatialPoint::setDomain(const std::string& domain)
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
 * Unsets id and returns value indicating success.
 */
int
SpatialPoint::unsetId()
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
 * Unsets coord1 and returns value indicating success.
 */
int
SpatialPoint::unsetCoord1()
{
  mCoord1 = numeric_limits<double>::quiet_NaN();
  mIsSetCoord1 = false;

  if (isSetCoord1() == false)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets coord2 and returns value indicating success.
 */
int
SpatialPoint::unsetCoord2()
{
  mCoord2 = numeric_limits<double>::quiet_NaN();
  mIsSetCoord2 = false;

  if (isSetCoord2() == false)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets coord3 and returns value indicating success.
 */
int
SpatialPoint::unsetCoord3()
{
  mCoord3 = numeric_limits<double>::quiet_NaN();
  mIsSetCoord3 = false;

  if (isSetCoord3() == false)
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
SpatialPoint::unsetDomain()
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
 * rename attributes that are SIdRefs or instances in math
 */
void
SpatialPoint::renameSIdRefs(const std::string& oldid, const std::string& newid)
{
  if (isSetDomain() == true && mDomain == oldid)
  {
    setDomain(newid);
  }

}


/*
 * Returns the XML element name of this object
 */
const std::string&
SpatialPoint::getElementName () const
{
  static const string name = "spatialPoint";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
SpatialPoint::getTypeCode () const
{
  return SBML_SPATIAL_SPATIALPOINT;
}


/*
 * check if all the required attributes are set
 */
bool
SpatialPoint::hasRequiredAttributes () const
{
  bool allPresent = true;

  if (isSetId() == false)
    allPresent = false;

  if (isSetCoord1() == false)
    allPresent = false;

  if (isSetDomain() == false)
    allPresent = false;

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
SpatialPoint::writeElements (XMLOutputStream& stream) const
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
SpatialPoint::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument.
 */
void
SpatialPoint::setSBMLDocument (SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
SpatialPoint::enablePackageInternal(const std::string& pkgURI,
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
SpatialPoint::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("id");
  attributes.add("coord1");
  attributes.add("coord2");
  attributes.add("coord3");
  attributes.add("domain");
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
SpatialPoint::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  unsigned int numErrs;

  /* look to see whether an unknown attribute error was logged
   * during the read of the listOfSpatialPoints - which will have
   * happened immediately prior to this read
  */

  if (getErrorLog() != NULL &&
      static_cast<ListOfSpatialPoints*>(getParentSBMLObject())->size() < 2)
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
      logEmptyString(mId, getLevel(), getVersion(), "<SpatialPoint>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mId) == false && getErrorLog() != NULL)
    {
      getErrorLog()->logError(InvalidIdSyntax, getLevel(), getVersion(), 
        "The syntax of the attribute id='" + mId + "' does not conform.", getLine(), getColumn());
    }
  }
  else
  {
    std::string message = "Spatial attribute 'id' is missing from 'spatialPoint' object.";
    getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                   getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
  }

  //
  // coord1 double   ( use = "required" )
  //
  numErrs = getErrorLog()->getNumErrors();
  mIsSetCoord1 = attributes.readInto("coord1", mCoord1);

  if (mIsSetCoord1 == false)
  {
    if (getErrorLog() != NULL)
    {
      if (getErrorLog()->getNumErrors() == numErrs + 1 &&
              getErrorLog()->contains(XMLAttributeTypeMismatch))
      {
        getErrorLog()->remove(XMLAttributeTypeMismatch);
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                     getPackageVersion(), sbmlLevel, sbmlVersion, "", getLine(), getColumn());
      }
      else
      {
        std::string message = "Spatial attribute 'coord1' is missing from 'spatialPoint' object.";
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
      }
    }
  }

  //
  // coord2 double   ( use = "optional" )
  //
  numErrs = getErrorLog()->getNumErrors();
  mIsSetCoord2 = attributes.readInto("coord2", mCoord2);

  if (mIsSetCoord2 == false)
  {
    if (getErrorLog() != NULL)
    {
      if (getErrorLog()->getNumErrors() == numErrs + 1 &&
              getErrorLog()->contains(XMLAttributeTypeMismatch))
      {
        getErrorLog()->remove(XMLAttributeTypeMismatch);
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                     getPackageVersion(), sbmlLevel, sbmlVersion, "", getLine(), getColumn());
      }
    }
  }

  //
  // coord3 double   ( use = "optional" )
  //
  numErrs = getErrorLog()->getNumErrors();
  mIsSetCoord3 = attributes.readInto("coord3", mCoord3);

  if (mIsSetCoord3 == false)
  {
    if (getErrorLog() != NULL)
    {
      if (getErrorLog()->getNumErrors() == numErrs + 1 &&
              getErrorLog()->contains(XMLAttributeTypeMismatch))
      {
        getErrorLog()->remove(XMLAttributeTypeMismatch);
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                     getPackageVersion(), sbmlLevel, sbmlVersion, "", getLine(), getColumn());
      }
    }
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
      logEmptyString(mDomain, getLevel(), getVersion(), "<SpatialPoint>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mDomain) == false && getErrorLog() != NULL)
    {
      getErrorLog()->logError(InvalidIdSyntax, getLevel(), getVersion(), 
        "The syntax of the attribute domain='" + mDomain + "' does not conform.");
    }
  }
  else
  {
    std::string message = "Spatial attribute 'domain' is missing from 'spatialPoint' object.";
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
SpatialPoint::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetId() == true)
    stream.writeAttribute("id", getPrefix(), mId);

  if (isSetCoord1() == true)
    stream.writeAttribute("coord1", getPrefix(), mCoord1);

  if (isSetCoord2() == true)
    stream.writeAttribute("coord2", getPrefix(), mCoord2);

  if (isSetCoord3() == true)
    stream.writeAttribute("coord3", getPrefix(), mCoord3);

  if (isSetDomain() == true)
    stream.writeAttribute("domain", getPrefix(), mDomain);

}


  /** @endcond doxygenLibsbmlInternal */


/*
 * Constructor 
 */
ListOfSpatialPoints::ListOfSpatialPoints(unsigned int level, 
                      unsigned int version, 
                      unsigned int pkgVersion)
 : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version, pkgVersion)); 
}


/*
 * Constructor 
 */
ListOfSpatialPoints::ListOfSpatialPoints(SpatialPkgNamespaces* spatialns)
  : ListOf(spatialns)
{
  setElementNamespace(spatialns->getURI());
}


/*
 * Returns a deep copy of this ListOfSpatialPoints 
 */
ListOfSpatialPoints* 
ListOfSpatialPoints::clone () const
 {
  return new ListOfSpatialPoints(*this);
}


/*
 * Get a SpatialPoint from the ListOfSpatialPoints by index.
*/
SpatialPoint*
ListOfSpatialPoints::get(unsigned int n)
{
  return static_cast<SpatialPoint*>(ListOf::get(n));
}


/*
 * Get a SpatialPoint from the ListOfSpatialPoints by index.
 */
const SpatialPoint*
ListOfSpatialPoints::get(unsigned int n) const
{
  return static_cast<const SpatialPoint*>(ListOf::get(n));
}


/*
 * Get a SpatialPoint from the ListOfSpatialPoints by id.
 */
SpatialPoint*
ListOfSpatialPoints::get(const std::string& sid)
{
	return const_cast<SpatialPoint*>(
    static_cast<const ListOfSpatialPoints&>(*this).get(sid));
}


/*
 * Get a SpatialPoint from the ListOfSpatialPoints by id.
 */
const SpatialPoint*
ListOfSpatialPoints::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<SpatialPoint>(sid) );
  return (result == mItems.end()) ? 0 : static_cast <SpatialPoint*> (*result);
}


/**
 * Adds a copy the given "SpatialPoint" to this ListOfSpatialPoints.
 *
 * @param sp; the SpatialPoint object to add
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 */
int
ListOfSpatialPoints::addSpatialPoint(const SpatialPoint* sp)
{
  if (sp == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (sp->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != sp->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != sp->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const SBase *>(sp)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
	append(sp);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/**
 * Get the number of SpatialPoint objects in this ListOfSpatialPoints.
 *
 * @return the number of SpatialPoint objects in this ListOfSpatialPoints
 */
unsigned int 
ListOfSpatialPoints::getNumSpatialPoints() const
{
	return size();
}

/**
 * Creates a new SpatialPoint object, adds it to this ListOfSpatialPoints
 * SpatialPoint and returns the SpatialPoint object created. 
 *
 * @return a new SpatialPoint object instance
 *
 * @see addSpatialPoint(const SpatialPoint* sp)
 */
SpatialPoint* 
ListOfSpatialPoints::createSpatialPoint()
{
  SpatialPoint* sp = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    sp = new SpatialPoint(spatialns);
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

  if(sp != NULL)
  {
    appendAndOwn(sp);
  }

  return sp;
}

/*
 * Removes the nth SpatialPoint from this ListOfSpatialPoints
 */
SpatialPoint*
ListOfSpatialPoints::remove(unsigned int n)
{
  return static_cast<SpatialPoint*>(ListOf::remove(n));
}


/*
 * Removes the SpatialPoint from this ListOfSpatialPoints with the given identifier
 */
SpatialPoint*
ListOfSpatialPoints::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<SpatialPoint>(sid) );

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

	return static_cast <SpatialPoint*> (item);
}


/*
 * Returns the XML element name of this object
 */
const std::string&
ListOfSpatialPoints::getElementName () const
{
  static const string name = "listOfSpatialPoints";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
ListOfSpatialPoints::getTypeCode () const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the objects in this LIST_OF.
 */
int
ListOfSpatialPoints::getItemTypeCode () const
{
  return SBML_SPATIAL_SPATIALPOINT;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * Creates a new SpatialPoint in this ListOfSpatialPoints
 */
SBase*
ListOfSpatialPoints::createObject(XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase* object = NULL;

  if (name == "spatialPoint")
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    object = new SpatialPoint(spatialns);
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
ListOfSpatialPoints::writeXMLNS(XMLOutputStream& stream) const
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
SpatialPoint_t *
SpatialPoint_create(unsigned int level, unsigned int version,
                    unsigned int pkgVersion)
{
  return new SpatialPoint(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
SpatialPoint_free(SpatialPoint_t * sp)
{
  if (sp != NULL)
    delete sp;
}


LIBSBML_EXTERN
SpatialPoint_t *
SpatialPoint_clone(SpatialPoint_t * sp)
{
  if (sp != NULL)
  {
    return static_cast<SpatialPoint_t*>(sp->clone());
  }
  else
  {
    return NULL;
  }
}


LIBSBML_EXTERN
const char *
SpatialPoint_getId(const SpatialPoint_t * sp)
{
	return (sp != NULL && sp->isSetId()) ? sp->getId().c_str() : NULL;
}


LIBSBML_EXTERN
double
SpatialPoint_getCoord1(const SpatialPoint_t * sp)
{
	return (sp != NULL) ? sp->getCoord1() : numeric_limits<double>::quiet_NaN();
}


LIBSBML_EXTERN
double
SpatialPoint_getCoord2(const SpatialPoint_t * sp)
{
	return (sp != NULL) ? sp->getCoord2() : numeric_limits<double>::quiet_NaN();
}


LIBSBML_EXTERN
double
SpatialPoint_getCoord3(const SpatialPoint_t * sp)
{
	return (sp != NULL) ? sp->getCoord3() : numeric_limits<double>::quiet_NaN();
}


LIBSBML_EXTERN
const char *
SpatialPoint_getDomain(const SpatialPoint_t * sp)
{
	return (sp != NULL && sp->isSetDomain()) ? sp->getDomain().c_str() : NULL;
}


LIBSBML_EXTERN
int
SpatialPoint_isSetId(const SpatialPoint_t * sp)
{
  return (sp != NULL) ? static_cast<int>(sp->isSetId()) : 0;
}


LIBSBML_EXTERN
int
SpatialPoint_isSetCoord1(const SpatialPoint_t * sp)
{
  return (sp != NULL) ? static_cast<int>(sp->isSetCoord1()) : 0;
}


LIBSBML_EXTERN
int
SpatialPoint_isSetCoord2(const SpatialPoint_t * sp)
{
  return (sp != NULL) ? static_cast<int>(sp->isSetCoord2()) : 0;
}


LIBSBML_EXTERN
int
SpatialPoint_isSetCoord3(const SpatialPoint_t * sp)
{
  return (sp != NULL) ? static_cast<int>(sp->isSetCoord3()) : 0;
}


LIBSBML_EXTERN
int
SpatialPoint_isSetDomain(const SpatialPoint_t * sp)
{
  return (sp != NULL) ? static_cast<int>(sp->isSetDomain()) : 0;
}


LIBSBML_EXTERN
int
SpatialPoint_setId(SpatialPoint_t * sp, const char * id)
{
  if (sp != NULL)
    return (id == NULL) ? sp->setId("") : sp->setId(id);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpatialPoint_setCoord1(SpatialPoint_t * sp, double coord1)
{
  if (sp != NULL)
    return sp->setCoord1(coord1);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpatialPoint_setCoord2(SpatialPoint_t * sp, double coord2)
{
  if (sp != NULL)
    return sp->setCoord2(coord2);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpatialPoint_setCoord3(SpatialPoint_t * sp, double coord3)
{
  if (sp != NULL)
    return sp->setCoord3(coord3);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpatialPoint_setDomain(SpatialPoint_t * sp, const char * domain)
{
  if (sp != NULL)
    return (domain == NULL) ? sp->setDomain("") : sp->setDomain(domain);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpatialPoint_unsetId(SpatialPoint_t * sp)
{
  return (sp != NULL) ? sp->unsetId() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpatialPoint_unsetCoord1(SpatialPoint_t * sp)
{
  return (sp != NULL) ? sp->unsetCoord1() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpatialPoint_unsetCoord2(SpatialPoint_t * sp)
{
  return (sp != NULL) ? sp->unsetCoord2() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpatialPoint_unsetCoord3(SpatialPoint_t * sp)
{
  return (sp != NULL) ? sp->unsetCoord3() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpatialPoint_unsetDomain(SpatialPoint_t * sp)
{
  return (sp != NULL) ? sp->unsetDomain() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SpatialPoint_hasRequiredAttributes(const SpatialPoint_t * sp)
{
  return (sp != NULL) ? static_cast<int>(sp->hasRequiredAttributes()) : 0;
}


/*
 *
 */
LIBSBML_EXTERN
SpatialPoint_t *
ListOfSpatialPoints_getById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfSpatialPoints *>(lo)->get(sid) : NULL;
}


/*
 *
 */
LIBSBML_EXTERN
SpatialPoint_t *
ListOfSpatialPoints_removeById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfSpatialPoints *>(lo)->remove(sid) : NULL;
}




LIBSBML_CPP_NAMESPACE_END


