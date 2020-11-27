/**
 * @file InteriorPoint.cpp
 * @brief Implementation of the InteriorPoint class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
 *
 * Copyright (C) 2019 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 * 3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 * Pasadena, CA, USA
 *
 * Copyright (C) 2002-2005 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. Japan Science and Technology Agency, Japan
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation. A copy of the license agreement is provided in the
 * file named "LICENSE.txt" included with this software distribution and also
 * available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 */
#include <sbml/packages/spatial/sbml/InteriorPoint.h>
#include <sbml/packages/spatial/sbml/ListOfInteriorPoints.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new InteriorPoint using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
InteriorPoint::InteriorPoint(unsigned int level,
                             unsigned int version,
                             unsigned int pkgVersion)
  : SBase(level, version)
  , mCoord1 (util_NaN())
  , mIsSetCoord1 (false)
  , mCoord2 (util_NaN())
  , mIsSetCoord2 (false)
  , mCoord3 (util_NaN())
  , mIsSetCoord3 (false)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version,
    pkgVersion));
}


/*
 * Creates a new InteriorPoint using the given SpatialPkgNamespaces object.
 */
InteriorPoint::InteriorPoint(SpatialPkgNamespaces *spatialns)
  : SBase(spatialns)
  , mCoord1 (util_NaN())
  , mIsSetCoord1 (false)
  , mCoord2 (util_NaN())
  , mIsSetCoord2 (false)
  , mCoord3 (util_NaN())
  , mIsSetCoord3 (false)
{
  setElementNamespace(spatialns->getURI());
  loadPlugins(spatialns);
}


/*
 * Copy constructor for InteriorPoint.
 */
InteriorPoint::InteriorPoint(const InteriorPoint& orig)
  : SBase( orig )
  , mCoord1 ( orig.mCoord1 )
  , mIsSetCoord1 ( orig.mIsSetCoord1 )
  , mCoord2 ( orig.mCoord2 )
  , mIsSetCoord2 ( orig.mIsSetCoord2 )
  , mCoord3 ( orig.mCoord3 )
  , mIsSetCoord3 ( orig.mIsSetCoord3 )
{
}


/*
 * Assignment operator for InteriorPoint.
 */
InteriorPoint&
InteriorPoint::operator=(const InteriorPoint& rhs)
{
  if (&rhs != this)
  {
    SBase::operator=(rhs);
    mCoord1 = rhs.mCoord1;
    mIsSetCoord1 = rhs.mIsSetCoord1;
    mCoord2 = rhs.mCoord2;
    mIsSetCoord2 = rhs.mIsSetCoord2;
    mCoord3 = rhs.mCoord3;
    mIsSetCoord3 = rhs.mIsSetCoord3;
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this InteriorPoint object.
 */
InteriorPoint*
InteriorPoint::clone() const
{
  return new InteriorPoint(*this);
}


/*
 * Destructor for InteriorPoint.
 */
InteriorPoint::~InteriorPoint()
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
 * Predicate returning @c true if this InteriorPoint's "coord1" attribute is
 * set.
 */
bool
InteriorPoint::isSetCoord1() const
{
  return mIsSetCoord1;
}


/*
 * Predicate returning @c true if this InteriorPoint's "coord2" attribute is
 * set.
 */
bool
InteriorPoint::isSetCoord2() const
{
  return mIsSetCoord2;
}


/*
 * Predicate returning @c true if this InteriorPoint's "coord3" attribute is
 * set.
 */
bool
InteriorPoint::isSetCoord3() const
{
  return mIsSetCoord3;
}


/*
 * Sets the value of the "coord1" attribute of this InteriorPoint.
 */
int
InteriorPoint::setCoord1(double coord1)
{
  mCoord1 = coord1;
  mIsSetCoord1 = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "coord2" attribute of this InteriorPoint.
 */
int
InteriorPoint::setCoord2(double coord2)
{
  mCoord2 = coord2;
  mIsSetCoord2 = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "coord3" attribute of this InteriorPoint.
 */
int
InteriorPoint::setCoord3(double coord3)
{
  mCoord3 = coord3;
  mIsSetCoord3 = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "coord1" attribute of this InteriorPoint.
 */
int
InteriorPoint::unsetCoord1()
{
  mCoord1 = util_NaN();
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
 * Unsets the value of the "coord2" attribute of this InteriorPoint.
 */
int
InteriorPoint::unsetCoord2()
{
  mCoord2 = util_NaN();
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
 * Unsets the value of the "coord3" attribute of this InteriorPoint.
 */
int
InteriorPoint::unsetCoord3()
{
  mCoord3 = util_NaN();
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
 * Returns the XML element name of this InteriorPoint object.
 */
const std::string&
InteriorPoint::getElementName() const
{
  static const string name = "interiorPoint";
  return name;
}


/*
 * Returns the libSBML type code for this InteriorPoint object.
 */
int
InteriorPoint::getTypeCode() const
{
  return SBML_SPATIAL_INTERIORPOINT;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * InteriorPoint object have been set.
 */
bool
InteriorPoint::hasRequiredAttributes() const
{
  bool allPresent = true;

  if (isSetCoord1() == false)
  {
    allPresent = false;
  }

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
InteriorPoint::writeElements(XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
InteriorPoint::accept(SBMLVisitor& v) const
{
  return v.visit(*this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
InteriorPoint::setSBMLDocument(SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
InteriorPoint::enablePackageInternal(const std::string& pkgURI,
                                     const std::string& pkgPrefix,
                                     bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this InteriorPoint.
 */
int
InteriorPoint::getAttribute(const std::string& attributeName,
                            bool& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this InteriorPoint.
 */
int
InteriorPoint::getAttribute(const std::string& attributeName,
                            int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this InteriorPoint.
 */
int
InteriorPoint::getAttribute(const std::string& attributeName,
                            double& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "coord1")
  {
    value = getCoord1();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "coord2")
  {
    value = getCoord2();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "coord3")
  {
    value = getCoord3();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this InteriorPoint.
 */
int
InteriorPoint::getAttribute(const std::string& attributeName,
                            unsigned int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this InteriorPoint.
 */
int
InteriorPoint::getAttribute(const std::string& attributeName,
                            std::string& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this InteriorPoint's attribute
 * "attributeName" is set.
 */
bool
InteriorPoint::isSetAttribute(const std::string& attributeName) const
{
  bool value = SBase::isSetAttribute(attributeName);

  if (attributeName == "coord1")
  {
    value = isSetCoord1();
  }
  else if (attributeName == "coord2")
  {
    value = isSetCoord2();
  }
  else if (attributeName == "coord3")
  {
    value = isSetCoord3();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this InteriorPoint.
 */
int
InteriorPoint::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this InteriorPoint.
 */
int
InteriorPoint::setAttribute(const std::string& attributeName, int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this InteriorPoint.
 */
int
InteriorPoint::setAttribute(const std::string& attributeName, double value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  if (attributeName == "coord1")
  {
    return_value = setCoord1(value);
  }
  else if (attributeName == "coord2")
  {
    return_value = setCoord2(value);
  }
  else if (attributeName == "coord3")
  {
    return_value = setCoord3(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this InteriorPoint.
 */
int
InteriorPoint::setAttribute(const std::string& attributeName,
                            unsigned int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this InteriorPoint.
 */
int
InteriorPoint::setAttribute(const std::string& attributeName,
                            const std::string& value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this InteriorPoint.
 */
int
InteriorPoint::unsetAttribute(const std::string& attributeName)
{
  int value = SBase::unsetAttribute(attributeName);

  if (attributeName == "coord1")
  {
    value = unsetCoord1();
  }
  else if (attributeName == "coord2")
  {
    value = unsetCoord2();
  }
  else if (attributeName == "coord3")
  {
    value = unsetCoord3();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
InteriorPoint::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("coord1");

  attributes.add("coord2");

  attributes.add("coord3");
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
InteriorPoint::readAttributes(const XMLAttributes& attributes,
                              const ExpectedAttributes& expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  if (log && getParentSBMLObject() &&
    static_cast<ListOfInteriorPoints*>(getParentSBMLObject())->size() < 2)
  {
    numErrs = log->getNumErrors();
    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("spatial", SpatialInteriorPointAllowedAttributes,
          pkgVersion, level, version, details);
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("spatial",
          SpatialDomainLOInteriorPointsAllowedCoreAttributes, pkgVersion, level,
            version, details);
      }
    }
  }

  SBase::readAttributes(attributes, expectedAttributes);

  if (log)
  {
    numErrs = log->getNumErrors();

    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("spatial", SpatialInteriorPointAllowedAttributes,
          pkgVersion, level, version, details);
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("spatial",
          SpatialInteriorPointAllowedCoreAttributes, pkgVersion, level, version,
            details);
      }
    }
  }

  // 
  // coord1 double (use = "required" )
  // 

  numErrs = log->getNumErrors();
  mIsSetCoord1 = attributes.readInto("coord1", mCoord1);

  if ( mIsSetCoord1 == false)
  {
    if (log->getNumErrors() == numErrs + 1 &&
      log->contains(XMLAttributeTypeMismatch))
    {
      log->remove(XMLAttributeTypeMismatch);
      std::string message = "Spatial attribute 'coord1' from the "
        "<InteriorPoint> element must be an integer.";
      log->logPackageError("spatial", SpatialInteriorPointCoord1MustBeDouble,
        pkgVersion, level, version, message);
    }
    else
    {
      std::string message = "Spatial attribute 'coord1' is missing from the "
        "<InteriorPoint> element.";
      log->logPackageError("spatial", SpatialInteriorPointAllowedAttributes,
        pkgVersion, level, version, message);
    }
  }

  // 
  // coord2 double (use = "optional" )
  // 

  numErrs = log->getNumErrors();
  mIsSetCoord2 = attributes.readInto("coord2", mCoord2);

  if ( mIsSetCoord2 == false)
  {
    if (log->getNumErrors() == numErrs + 1 &&
      log->contains(XMLAttributeTypeMismatch))
    {
      log->remove(XMLAttributeTypeMismatch);
      std::string message = "Spatial attribute 'coord2' from the "
        "<InteriorPoint> element must be an integer.";
      log->logPackageError("spatial", SpatialInteriorPointCoord2MustBeDouble,
        pkgVersion, level, version, message);
    }
  }

  // 
  // coord3 double (use = "optional" )
  // 

  numErrs = log->getNumErrors();
  mIsSetCoord3 = attributes.readInto("coord3", mCoord3);

  if ( mIsSetCoord3 == false)
  {
    if (log->getNumErrors() == numErrs + 1 &&
      log->contains(XMLAttributeTypeMismatch))
    {
      log->remove(XMLAttributeTypeMismatch);
      std::string message = "Spatial attribute 'coord3' from the "
        "<InteriorPoint> element must be an integer.";
      log->logPackageError("spatial", SpatialInteriorPointCoord3MustBeDouble,
        pkgVersion, level, version, message);
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
InteriorPoint::writeAttributes(XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetCoord1() == true)
  {
    stream.writeAttribute("coord1", getPrefix(), mCoord1);
  }

  if (isSetCoord2() == true)
  {
    stream.writeAttribute("coord2", getPrefix(), mCoord2);
  }

  if (isSetCoord3() == true)
  {
    stream.writeAttribute("coord3", getPrefix(), mCoord3);
  }

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new InteriorPoint_t using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
LIBSBML_EXTERN
InteriorPoint_t *
InteriorPoint_create(unsigned int level,
                     unsigned int version,
                     unsigned int pkgVersion)
{
  return new InteriorPoint(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this InteriorPoint_t object.
 */
LIBSBML_EXTERN
InteriorPoint_t*
InteriorPoint_clone(const InteriorPoint_t* ip)
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


/*
 * Frees this InteriorPoint_t object.
 */
LIBSBML_EXTERN
void
InteriorPoint_free(InteriorPoint_t* ip)
{
  if (ip != NULL)
  {
    delete ip;
  }
}


/*
 * Returns the value of the "coord1" attribute of this InteriorPoint_t.
 */
LIBSBML_EXTERN
double
InteriorPoint_getCoord1(const InteriorPoint_t * ip)
{
  return (ip != NULL) ? ip->getCoord1() : util_NaN();
}


/*
 * Returns the value of the "coord2" attribute of this InteriorPoint_t.
 */
LIBSBML_EXTERN
double
InteriorPoint_getCoord2(const InteriorPoint_t * ip)
{
  return (ip != NULL) ? ip->getCoord2() : util_NaN();
}


/*
 * Returns the value of the "coord3" attribute of this InteriorPoint_t.
 */
LIBSBML_EXTERN
double
InteriorPoint_getCoord3(const InteriorPoint_t * ip)
{
  return (ip != NULL) ? ip->getCoord3() : util_NaN();
}


/*
 * Predicate returning @c 1 (true) if this InteriorPoint_t's "coord1" attribute
 * is set.
 */
LIBSBML_EXTERN
int
InteriorPoint_isSetCoord1(const InteriorPoint_t * ip)
{
  return (ip != NULL) ? static_cast<int>(ip->isSetCoord1()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this InteriorPoint_t's "coord2" attribute
 * is set.
 */
LIBSBML_EXTERN
int
InteriorPoint_isSetCoord2(const InteriorPoint_t * ip)
{
  return (ip != NULL) ? static_cast<int>(ip->isSetCoord2()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this InteriorPoint_t's "coord3" attribute
 * is set.
 */
LIBSBML_EXTERN
int
InteriorPoint_isSetCoord3(const InteriorPoint_t * ip)
{
  return (ip != NULL) ? static_cast<int>(ip->isSetCoord3()) : 0;
}


/*
 * Sets the value of the "coord1" attribute of this InteriorPoint_t.
 */
LIBSBML_EXTERN
int
InteriorPoint_setCoord1(InteriorPoint_t * ip, double coord1)
{
  return (ip != NULL) ? ip->setCoord1(coord1) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "coord2" attribute of this InteriorPoint_t.
 */
LIBSBML_EXTERN
int
InteriorPoint_setCoord2(InteriorPoint_t * ip, double coord2)
{
  return (ip != NULL) ? ip->setCoord2(coord2) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "coord3" attribute of this InteriorPoint_t.
 */
LIBSBML_EXTERN
int
InteriorPoint_setCoord3(InteriorPoint_t * ip, double coord3)
{
  return (ip != NULL) ? ip->setCoord3(coord3) : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "coord1" attribute of this InteriorPoint_t.
 */
LIBSBML_EXTERN
int
InteriorPoint_unsetCoord1(InteriorPoint_t * ip)
{
  return (ip != NULL) ? ip->unsetCoord1() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "coord2" attribute of this InteriorPoint_t.
 */
LIBSBML_EXTERN
int
InteriorPoint_unsetCoord2(InteriorPoint_t * ip)
{
  return (ip != NULL) ? ip->unsetCoord2() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "coord3" attribute of this InteriorPoint_t.
 */
LIBSBML_EXTERN
int
InteriorPoint_unsetCoord3(InteriorPoint_t * ip)
{
  return (ip != NULL) ? ip->unsetCoord3() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * InteriorPoint_t object have been set.
 */
LIBSBML_EXTERN
int
InteriorPoint_hasRequiredAttributes(const InteriorPoint_t * ip)
{
  return (ip != NULL) ? static_cast<int>(ip->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


