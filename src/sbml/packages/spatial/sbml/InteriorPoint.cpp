/**
 * @file:   InteriorPoint.cpp
 * @brief:  Implementation of the InteriorPoint class
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


#include <sbml/packages/spatial/sbml/InteriorPoint.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new InteriorPoint with the given level, version, and package version.
 */
InteriorPoint::InteriorPoint (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : SBase(level, version)
  , mCoord1 (numeric_limits<double>::quiet_NaN())
  , mIsSetCoord1 (false)
  , mCoord2 (numeric_limits<double>::quiet_NaN())
  , mIsSetCoord2 (false)
  , mCoord3 (numeric_limits<double>::quiet_NaN())
  , mIsSetCoord3 (false)
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new InteriorPoint with the given SpatialPkgNamespaces object.
 */
InteriorPoint::InteriorPoint (SpatialPkgNamespaces* spatialns)
  : SBase(spatialns)
  , mCoord1 (numeric_limits<double>::quiet_NaN())
  , mIsSetCoord1 (false)
  , mCoord2 (numeric_limits<double>::quiet_NaN())
  , mIsSetCoord2 (false)
  , mCoord3 (numeric_limits<double>::quiet_NaN())
  , mIsSetCoord3 (false)
{
  // set the element namespace of this object
  setElementNamespace(spatialns->getURI());

  // load package extensions bound with this object (if any) 
  loadPlugins(spatialns);
}


/*
 * Copy constructor for InteriorPoint.
 */
InteriorPoint::InteriorPoint (const InteriorPoint& orig)
  : SBase(orig)
{
  if (&orig == NULL)
  {
    throw SBMLConstructorException("Null argument to copy constructor");
  }
  else
  {
    mCoord1  = orig.mCoord1;
    mIsSetCoord1  = orig.mIsSetCoord1;
    mCoord2  = orig.mCoord2;
    mIsSetCoord2  = orig.mIsSetCoord2;
    mCoord3  = orig.mCoord3;
    mIsSetCoord3  = orig.mIsSetCoord3;
  }
}


/*
 * Assignment for InteriorPoint.
 */
InteriorPoint&
InteriorPoint::operator=(const InteriorPoint& rhs)
{
  if (&rhs == NULL)
  {
    throw SBMLConstructorException("Null argument to assignment");
  }
  else if (&rhs != this)
  {
    SBase::operator=(rhs);
    mCoord1  = rhs.mCoord1;
    mIsSetCoord1  = rhs.mIsSetCoord1;
    mCoord2  = rhs.mCoord2;
    mIsSetCoord2  = rhs.mIsSetCoord2;
    mCoord3  = rhs.mCoord3;
    mIsSetCoord3  = rhs.mIsSetCoord3;
  }
  return *this;
}


/*
 * Clone for InteriorPoint.
 */
InteriorPoint*
InteriorPoint::clone () const
{
  return new InteriorPoint(*this);
}


/*
 * Destructor for InteriorPoint.
 */
InteriorPoint::~InteriorPoint ()
{
}


/*
 * Returns the value of the "coord1" attribute of this InteriorPoint.
 */
double
InteriorPoint::getCoord1() const
{
  return mCoord1;
}


/*
 * Returns the value of the "coord2" attribute of this InteriorPoint.
 */
double
InteriorPoint::getCoord2() const
{
  return mCoord2;
}


/*
 * Returns the value of the "coord3" attribute of this InteriorPoint.
 */
double
InteriorPoint::getCoord3() const
{
  return mCoord3;
}


/*
 * Returns true/false if coord1 is set.
 */
bool
InteriorPoint::isSetCoord1() const
{
  return mIsSetCoord1;
}


/*
 * Returns true/false if coord2 is set.
 */
bool
InteriorPoint::isSetCoord2() const
{
  return mIsSetCoord2;
}


/*
 * Returns true/false if coord3 is set.
 */
bool
InteriorPoint::isSetCoord3() const
{
  return mIsSetCoord3;
}


/*
 * Sets coord1 and returns value indicating success.
 */
int
InteriorPoint::setCoord1(double coord1)
{
  mCoord1 = coord1;
  mIsSetCoord1 = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets coord2 and returns value indicating success.
 */
int
InteriorPoint::setCoord2(double coord2)
{
  mCoord2 = coord2;
  mIsSetCoord2 = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets coord3 and returns value indicating success.
 */
int
InteriorPoint::setCoord3(double coord3)
{
  mCoord3 = coord3;
  mIsSetCoord3 = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets coord1 and returns value indicating success.
 */
int
InteriorPoint::unsetCoord1()
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
InteriorPoint::unsetCoord2()
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
InteriorPoint::unsetCoord3()
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
 * Returns the XML element name of this object
 */
const std::string&
InteriorPoint::getElementName () const
{
  static const string name = "interiorPoint";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
InteriorPoint::getTypeCode () const
{
  return SBML_SPATIAL_INTERIORPOINT;
}


/*
 * check if all the required attributes are set
 */
bool
InteriorPoint::hasRequiredAttributes () const
{
  bool allPresent = true;

  if (isSetCoord1() == false)
    allPresent = false;

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
InteriorPoint::writeElements (XMLOutputStream& stream) const
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
InteriorPoint::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument.
 */
void
InteriorPoint::setSBMLDocument (SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
InteriorPoint::enablePackageInternal(const std::string& pkgURI,
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
InteriorPoint::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("coord1");
  attributes.add("coord2");
  attributes.add("coord3");
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
InteriorPoint::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  unsigned int numErrs;

  /* look to see whether an unknown attribute error was logged
   * during the read of the listOfInteriorPoints - which will have
   * happened immediately prior to this read
  */

  if (getErrorLog() != NULL &&
      static_cast<ListOfInteriorPoints*>(getParentSBMLObject())->size() < 2)
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

  //bool assigned = false;

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
        std::string message = "Spatial attribute 'coord1' is missing from 'interiorPoint' object.";
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

}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Write values of XMLAttributes to the output stream.
 */
  void
InteriorPoint::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetCoord1() == true)
    stream.writeAttribute("coord1", getPrefix(), mCoord1);

  if (isSetCoord2() == true)
    stream.writeAttribute("coord2", getPrefix(), mCoord2);

  if (isSetCoord3() == true)
    stream.writeAttribute("coord3", getPrefix(), mCoord3);

}


  /** @endcond doxygenLibsbmlInternal */


/*
 * Constructor 
 */
ListOfInteriorPoints::ListOfInteriorPoints(unsigned int level, 
                       unsigned int version, 
                       unsigned int pkgVersion)
 : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version, pkgVersion)); 
}


/*
 * Constructor 
 */
ListOfInteriorPoints::ListOfInteriorPoints(SpatialPkgNamespaces* spatialns)
  : ListOf(spatialns)
{
  setElementNamespace(spatialns->getURI());
}


/*
 * Returns a deep copy of this ListOfInteriorPoints 
 */
ListOfInteriorPoints* 
ListOfInteriorPoints::clone () const
 {
  return new ListOfInteriorPoints(*this);
}


/*
 * Get a InteriorPoint from the ListOfInteriorPoints by index.
*/
InteriorPoint*
ListOfInteriorPoints::get(unsigned int n)
{
  return static_cast<InteriorPoint*>(ListOf::get(n));
}


/*
 * Get a InteriorPoint from the ListOfInteriorPoints by index.
 */
const InteriorPoint*
ListOfInteriorPoints::get(unsigned int n) const
{
  return static_cast<const InteriorPoint*>(ListOf::get(n));
}


/*
 * Get a InteriorPoint from the ListOfInteriorPoints by id.
 */
InteriorPoint*
ListOfInteriorPoints::get(const std::string& sid)
{
	return const_cast<InteriorPoint*>(
    static_cast<const ListOfInteriorPoints&>(*this).get(sid));
}


/*
 * Get a InteriorPoint from the ListOfInteriorPoints by id.
 */
const InteriorPoint*
ListOfInteriorPoints::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<InteriorPoint>(sid) );
  return (result == mItems.end()) ? 0 : static_cast <InteriorPoint*> (*result);
}


/**
 * Adds a copy the given "InteriorPoint" to this ListOfInteriorPoints.
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
ListOfInteriorPoints::addInteriorPoint(const InteriorPoint* ip)
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
	append(ip);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/**
 * Get the number of InteriorPoint objects in this ListOfInteriorPoints.
 *
 * @return the number of InteriorPoint objects in this ListOfInteriorPoints
 */
unsigned int 
ListOfInteriorPoints::getNumInteriorPoints() const
{
	return size();
}

/**
 * Creates a new InteriorPoint object, adds it to this ListOfInteriorPoints
 * InteriorPoint and returns the InteriorPoint object created. 
 *
 * @return a new InteriorPoint object instance
 *
 * @see addInteriorPoint(const InteriorPoint* ip)
 */
InteriorPoint* 
ListOfInteriorPoints::createInteriorPoint()
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
    appendAndOwn(ip);
  }

  return ip;
}

/*
 * Removes the nth InteriorPoint from this ListOfInteriorPoints
 */
InteriorPoint*
ListOfInteriorPoints::remove(unsigned int n)
{
  return static_cast<InteriorPoint*>(ListOf::remove(n));
}


/*
 * Removes the InteriorPoint from this ListOfInteriorPoints with the given identifier
 */
InteriorPoint*
ListOfInteriorPoints::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<InteriorPoint>(sid) );

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

	return static_cast <InteriorPoint*> (item);
}


/*
 * Returns the XML element name of this object
 */
const std::string&
ListOfInteriorPoints::getElementName () const
{
  static const string name = "listOfInteriorPoints";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
ListOfInteriorPoints::getTypeCode () const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the objects in this LIST_OF.
 */
int
ListOfInteriorPoints::getItemTypeCode () const
{
  return SBML_SPATIAL_INTERIORPOINT;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * Creates a new InteriorPoint in this ListOfInteriorPoints
 */
SBase*
ListOfInteriorPoints::createObject(XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase* object = NULL;

  if (name == "interiorPoint")
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    object = new InteriorPoint(spatialns);
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
ListOfInteriorPoints::writeXMLNS(XMLOutputStream& stream) const
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
InteriorPoint_t *
InteriorPoint_create(unsigned int level, unsigned int version,
                     unsigned int pkgVersion)
{
  return new InteriorPoint(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
InteriorPoint_free(InteriorPoint_t * ip)
{
  if (ip != NULL)
    delete ip;
}


LIBSBML_EXTERN
InteriorPoint_t *
InteriorPoint_clone(InteriorPoint_t * ip)
{
  if (ip != NULL)
  {
    return static_cast<InteriorPoint_t*>(ip->clone());
  }
  else
  {
    return NULL;
  }
}


LIBSBML_EXTERN
double
InteriorPoint_getCoord1(const InteriorPoint_t * ip)
{
	return (ip != NULL) ? ip->getCoord1() : numeric_limits<double>::quiet_NaN();
}


LIBSBML_EXTERN
double
InteriorPoint_getCoord2(const InteriorPoint_t * ip)
{
	return (ip != NULL) ? ip->getCoord2() : numeric_limits<double>::quiet_NaN();
}


LIBSBML_EXTERN
double
InteriorPoint_getCoord3(const InteriorPoint_t * ip)
{
	return (ip != NULL) ? ip->getCoord3() : numeric_limits<double>::quiet_NaN();
}


LIBSBML_EXTERN
int
InteriorPoint_isSetCoord1(const InteriorPoint_t * ip)
{
  return (ip != NULL) ? static_cast<int>(ip->isSetCoord1()) : 0;
}


LIBSBML_EXTERN
int
InteriorPoint_isSetCoord2(const InteriorPoint_t * ip)
{
  return (ip != NULL) ? static_cast<int>(ip->isSetCoord2()) : 0;
}


LIBSBML_EXTERN
int
InteriorPoint_isSetCoord3(const InteriorPoint_t * ip)
{
  return (ip != NULL) ? static_cast<int>(ip->isSetCoord3()) : 0;
}


LIBSBML_EXTERN
int
InteriorPoint_setCoord1(InteriorPoint_t * ip, double coord1)
{
  if (ip != NULL)
    return ip->setCoord1(coord1);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
InteriorPoint_setCoord2(InteriorPoint_t * ip, double coord2)
{
  if (ip != NULL)
    return ip->setCoord2(coord2);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
InteriorPoint_setCoord3(InteriorPoint_t * ip, double coord3)
{
  if (ip != NULL)
    return ip->setCoord3(coord3);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
InteriorPoint_unsetCoord1(InteriorPoint_t * ip)
{
  return (ip != NULL) ? ip->unsetCoord1() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
InteriorPoint_unsetCoord2(InteriorPoint_t * ip)
{
  return (ip != NULL) ? ip->unsetCoord2() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
InteriorPoint_unsetCoord3(InteriorPoint_t * ip)
{
  return (ip != NULL) ? ip->unsetCoord3() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
InteriorPoint_hasRequiredAttributes(const InteriorPoint_t * ip)
{
  return (ip != NULL) ? static_cast<int>(ip->hasRequiredAttributes()) : 0;
}


/*
 *
 */
LIBSBML_EXTERN
InteriorPoint_t *
ListOfInteriorPoints_getById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfInteriorPoints *>(lo)->get(sid) : NULL;
}


/*
 *
 */
LIBSBML_EXTERN
InteriorPoint_t *
ListOfInteriorPoints_removeById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfInteriorPoints *>(lo)->remove(sid) : NULL;
}




LIBSBML_CPP_NAMESPACE_END


