/**
 * @file:   CSGRotation.cpp
 * @brief:  Implementation of the CSGRotation class
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


#include <sbml/packages/spatial/sbml/CSGRotation.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new CSGRotation with the given level, version, and package version.
 */
CSGRotation::CSGRotation (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : CSGTransformation(level, version)
  , mRotateAxisX (numeric_limits<double>::quiet_NaN())
  , mIsSetRotateAxisX (false)
  , mRotateAxisY (numeric_limits<double>::quiet_NaN())
  , mIsSetRotateAxisY (false)
  , mRotateAxisZ (numeric_limits<double>::quiet_NaN())
  , mIsSetRotateAxisZ (false)
  , mRotateAngleInRadians (numeric_limits<double>::quiet_NaN())
  , mIsSetRotateAngleInRadians (false)
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new CSGRotation with the given SpatialPkgNamespaces object.
 */
CSGRotation::CSGRotation (SpatialPkgNamespaces* spatialns)
  : CSGTransformation(spatialns)
  , mRotateAxisX (numeric_limits<double>::quiet_NaN())
  , mIsSetRotateAxisX (false)
  , mRotateAxisY (numeric_limits<double>::quiet_NaN())
  , mIsSetRotateAxisY (false)
  , mRotateAxisZ (numeric_limits<double>::quiet_NaN())
  , mIsSetRotateAxisZ (false)
  , mRotateAngleInRadians (numeric_limits<double>::quiet_NaN())
  , mIsSetRotateAngleInRadians (false)
{
  // set the element namespace of this object
  setElementNamespace(spatialns->getURI());

  // load package extensions bound with this object (if any) 
  loadPlugins(spatialns);
}


/*
 * Copy constructor for CSGRotation.
 */
CSGRotation::CSGRotation (const CSGRotation& orig)
  : CSGTransformation(orig)
{
  if (&orig == NULL)
  {
    throw SBMLConstructorException("Null argument to copy constructor");
  }
  else
  {
    mRotateAxisX  = orig.mRotateAxisX;
    mIsSetRotateAxisX  = orig.mIsSetRotateAxisX;
    mRotateAxisY  = orig.mRotateAxisY;
    mIsSetRotateAxisY  = orig.mIsSetRotateAxisY;
    mRotateAxisZ  = orig.mRotateAxisZ;
    mIsSetRotateAxisZ  = orig.mIsSetRotateAxisZ;
    mRotateAngleInRadians  = orig.mRotateAngleInRadians;
    mIsSetRotateAngleInRadians  = orig.mIsSetRotateAngleInRadians;
  }
}


/*
 * Assignment for CSGRotation.
 */
CSGRotation&
CSGRotation::operator=(const CSGRotation& rhs)
{
  if (&rhs == NULL)
  {
    throw SBMLConstructorException("Null argument to assignment");
  }
  else if (&rhs != this)
  {
    CSGTransformation::operator=(rhs);
    mRotateAxisX  = rhs.mRotateAxisX;
    mIsSetRotateAxisX  = rhs.mIsSetRotateAxisX;
    mRotateAxisY  = rhs.mRotateAxisY;
    mIsSetRotateAxisY  = rhs.mIsSetRotateAxisY;
    mRotateAxisZ  = rhs.mRotateAxisZ;
    mIsSetRotateAxisZ  = rhs.mIsSetRotateAxisZ;
    mRotateAngleInRadians  = rhs.mRotateAngleInRadians;
    mIsSetRotateAngleInRadians  = rhs.mIsSetRotateAngleInRadians;
  }
  return *this;
}


/*
 * Clone for CSGRotation.
 */
CSGRotation*
CSGRotation::clone () const
{
  return new CSGRotation(*this);
}


/*
 * Destructor for CSGRotation.
 */
CSGRotation::~CSGRotation ()
{
}


/*
 * Returns the value of the "rotateAxisX" attribute of this CSGRotation.
 */
double
CSGRotation::getRotateAxisX() const
{
  return mRotateAxisX;
}


/*
 * Returns the value of the "rotateAxisY" attribute of this CSGRotation.
 */
double
CSGRotation::getRotateAxisY() const
{
  return mRotateAxisY;
}


/*
 * Returns the value of the "rotateAxisZ" attribute of this CSGRotation.
 */
double
CSGRotation::getRotateAxisZ() const
{
  return mRotateAxisZ;
}


/*
 * Returns the value of the "rotateAngleInRadians" attribute of this CSGRotation.
 */
double
CSGRotation::getRotateAngleInRadians() const
{
  return mRotateAngleInRadians;
}


/*
 * Returns true/false if rotateAxisX is set.
 */
bool
CSGRotation::isSetRotateAxisX() const
{
  return mIsSetRotateAxisX;
}


/*
 * Returns true/false if rotateAxisY is set.
 */
bool
CSGRotation::isSetRotateAxisY() const
{
  return mIsSetRotateAxisY;
}


/*
 * Returns true/false if rotateAxisZ is set.
 */
bool
CSGRotation::isSetRotateAxisZ() const
{
  return mIsSetRotateAxisZ;
}


/*
 * Returns true/false if rotateAngleInRadians is set.
 */
bool
CSGRotation::isSetRotateAngleInRadians() const
{
  return mIsSetRotateAngleInRadians;
}


/*
 * Sets rotateAxisX and returns value indicating success.
 */
int
CSGRotation::setRotateAxisX(double rotateAxisX)
{
  mRotateAxisX = rotateAxisX;
  mIsSetRotateAxisX = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets rotateAxisY and returns value indicating success.
 */
int
CSGRotation::setRotateAxisY(double rotateAxisY)
{
  mRotateAxisY = rotateAxisY;
  mIsSetRotateAxisY = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets rotateAxisZ and returns value indicating success.
 */
int
CSGRotation::setRotateAxisZ(double rotateAxisZ)
{
  mRotateAxisZ = rotateAxisZ;
  mIsSetRotateAxisZ = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets rotateAngleInRadians and returns value indicating success.
 */
int
CSGRotation::setRotateAngleInRadians(double rotateAngleInRadians)
{
  mRotateAngleInRadians = rotateAngleInRadians;
  mIsSetRotateAngleInRadians = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets rotateAxisX and returns value indicating success.
 */
int
CSGRotation::unsetRotateAxisX()
{
  mRotateAxisX = numeric_limits<double>::quiet_NaN();
  mIsSetRotateAxisX = false;

  if (isSetRotateAxisX() == false)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets rotateAxisY and returns value indicating success.
 */
int
CSGRotation::unsetRotateAxisY()
{
  mRotateAxisY = numeric_limits<double>::quiet_NaN();
  mIsSetRotateAxisY = false;

  if (isSetRotateAxisY() == false)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets rotateAxisZ and returns value indicating success.
 */
int
CSGRotation::unsetRotateAxisZ()
{
  mRotateAxisZ = numeric_limits<double>::quiet_NaN();
  mIsSetRotateAxisZ = false;

  if (isSetRotateAxisZ() == false)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets rotateAngleInRadians and returns value indicating success.
 */
int
CSGRotation::unsetRotateAngleInRadians()
{
  mRotateAngleInRadians = numeric_limits<double>::quiet_NaN();
  mIsSetRotateAngleInRadians = false;

  if (isSetRotateAngleInRadians() == false)
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
CSGRotation::getElementName () const
{
  static const string name = "csgRotation";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
CSGRotation::getTypeCode () const
{
  return SBML_SPATIAL_CSGROTATION;
}


/*
 * check if all the required attributes are set
 */
bool
CSGRotation::hasRequiredAttributes () const
{
  bool allPresent = CSGTransformation::hasRequiredAttributes();

  if (isSetRotateAxisX() == false)
    allPresent = false;

  if (isSetRotateAngleInRadians() == false)
    allPresent = false;

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
CSGRotation::writeElements (XMLOutputStream& stream) const
{
  CSGTransformation::writeElements(stream);
  SBase::writeExtensionElements(stream);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor.
 */
bool
CSGRotation::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument.
 */
void
CSGRotation::setSBMLDocument (SBMLDocument* d)
{
  CSGTransformation::setSBMLDocument(d);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
CSGRotation::enablePackageInternal(const std::string& pkgURI,
             const std::string& pkgPrefix, bool flag)
{
  CSGTransformation::enablePackageInternal(pkgURI, pkgPrefix, flag);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * creates object.
 */
SBase*
CSGRotation::createObject(XMLInputStream& stream)
{
  SBase* object = CSGTransformation::createObject(stream);

  connectToChild();


  return object;
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Get the list of expected attributes for this element.
 */
void
CSGRotation::addExpectedAttributes(ExpectedAttributes& attributes)
{
  CSGTransformation::addExpectedAttributes(attributes);

  attributes.add("rotateAxisX");
  attributes.add("rotateAxisY");
  attributes.add("rotateAxisZ");
  attributes.add("rotateAngleInRadians");
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
CSGRotation::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  unsigned int numErrs;

  CSGTransformation::readAttributes(attributes, expectedAttributes);

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
  // rotateAxisX double   ( use = "required" )
  //
  numErrs = getErrorLog()->getNumErrors();
  mIsSetRotateAxisX = attributes.readInto("rotateAxisX", mRotateAxisX);

  if (mIsSetRotateAxisX == false)
  {
    if (getErrorLog() != NULL)
    {
      if (getErrorLog()->getNumErrors() == numErrs + 1 &&
              getErrorLog()->contains(XMLAttributeTypeMismatch))
      {
        getErrorLog()->remove(XMLAttributeTypeMismatch);
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                     getPackageVersion(), sbmlLevel, sbmlVersion);
      }
      else
      {
        std::string message = "Spatial attribute 'rotateAxisX' is missing.";
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, message);
      }
    }
  }

  //
  // rotateAxisY double   ( use = "optional" )
  //
  numErrs = getErrorLog()->getNumErrors();
  mIsSetRotateAxisY = attributes.readInto("rotateAxisY", mRotateAxisY);

  if (mIsSetRotateAxisY == false)
  {
    if (getErrorLog() != NULL)
    {
      if (getErrorLog()->getNumErrors() == numErrs + 1 &&
              getErrorLog()->contains(XMLAttributeTypeMismatch))
      {
        getErrorLog()->remove(XMLAttributeTypeMismatch);
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                     getPackageVersion(), sbmlLevel, sbmlVersion);
      }
    }
  }

  //
  // rotateAxisZ double   ( use = "optional" )
  //
  numErrs = getErrorLog()->getNumErrors();
  mIsSetRotateAxisZ = attributes.readInto("rotateAxisZ", mRotateAxisZ);

  if (mIsSetRotateAxisZ == false)
  {
    if (getErrorLog() != NULL)
    {
      if (getErrorLog()->getNumErrors() == numErrs + 1 &&
              getErrorLog()->contains(XMLAttributeTypeMismatch))
      {
        getErrorLog()->remove(XMLAttributeTypeMismatch);
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                     getPackageVersion(), sbmlLevel, sbmlVersion);
      }
    }
  }

  //
  // rotateAngleInRadians double   ( use = "required" )
  //
  numErrs = getErrorLog()->getNumErrors();
  mIsSetRotateAngleInRadians = attributes.readInto("rotateAngleInRadians", mRotateAngleInRadians);

  if (mIsSetRotateAngleInRadians == false)
  {
    if (getErrorLog() != NULL)
    {
      if (getErrorLog()->getNumErrors() == numErrs + 1 &&
              getErrorLog()->contains(XMLAttributeTypeMismatch))
      {
        getErrorLog()->remove(XMLAttributeTypeMismatch);
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                     getPackageVersion(), sbmlLevel, sbmlVersion);
      }
      else
      {
        std::string message = "Spatial attribute 'rotateAngleInRadians' is missing.";
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, message);
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
CSGRotation::writeAttributes (XMLOutputStream& stream) const
{
  CSGTransformation::writeAttributes(stream);

  if (isSetRotateAxisX() == true)
    stream.writeAttribute("rotateAxisX", getPrefix(), mRotateAxisX);

  if (isSetRotateAxisY() == true)
    stream.writeAttribute("rotateAxisY", getPrefix(), mRotateAxisY);

  if (isSetRotateAxisZ() == true)
    stream.writeAttribute("rotateAxisZ", getPrefix(), mRotateAxisZ);

  if (isSetRotateAngleInRadians() == true)
    stream.writeAttribute("rotateAngleInRadians", getPrefix(), mRotateAngleInRadians);

}


  /** @endcond doxygenLibsbmlInternal */


LIBSBML_EXTERN
CSGRotation_t *
CSGRotation_create(unsigned int level, unsigned int version,
                   unsigned int pkgVersion)
{
  return new CSGRotation(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
CSGRotation_free(CSGRotation_t * csgr)
{
  if (csgr != NULL)
    delete csgr;
}


LIBSBML_EXTERN
CSGRotation_t *
CSGRotation_clone(CSGRotation_t * csgr)
{
  if (csgr != NULL)
  {
    return static_cast<CSGRotation_t*>(csgr->clone());
  }
  else
  {
    return NULL;
  }
}


LIBSBML_EXTERN
double
CSGRotation_getRotateAxisX(const CSGRotation_t * csgr)
{
	return (csgr != NULL) ? csgr->getRotateAxisX() : numeric_limits<double>::quiet_NaN();
}


LIBSBML_EXTERN
double
CSGRotation_getRotateAxisY(const CSGRotation_t * csgr)
{
	return (csgr != NULL) ? csgr->getRotateAxisY() : numeric_limits<double>::quiet_NaN();
}


LIBSBML_EXTERN
double
CSGRotation_getRotateAxisZ(const CSGRotation_t * csgr)
{
	return (csgr != NULL) ? csgr->getRotateAxisZ() : numeric_limits<double>::quiet_NaN();
}


LIBSBML_EXTERN
double
CSGRotation_getRotateAngleInRadians(const CSGRotation_t * csgr)
{
	return (csgr != NULL) ? csgr->getRotateAngleInRadians() : numeric_limits<double>::quiet_NaN();
}


LIBSBML_EXTERN
int
CSGRotation_isSetRotateAxisX(const CSGRotation_t * csgr)
{
  return (csgr != NULL) ? static_cast<int>(csgr->isSetRotateAxisX()) : 0;
}


LIBSBML_EXTERN
int
CSGRotation_isSetRotateAxisY(const CSGRotation_t * csgr)
{
  return (csgr != NULL) ? static_cast<int>(csgr->isSetRotateAxisY()) : 0;
}


LIBSBML_EXTERN
int
CSGRotation_isSetRotateAxisZ(const CSGRotation_t * csgr)
{
  return (csgr != NULL) ? static_cast<int>(csgr->isSetRotateAxisZ()) : 0;
}


LIBSBML_EXTERN
int
CSGRotation_isSetRotateAngleInRadians(const CSGRotation_t * csgr)
{
  return (csgr != NULL) ? static_cast<int>(csgr->isSetRotateAngleInRadians()) : 0;
}


LIBSBML_EXTERN
int
CSGRotation_setRotateAxisX(CSGRotation_t * csgr, double rotateAxisX)
{
  if (csgr != NULL)
    return csgr->setRotateAxisX(rotateAxisX);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CSGRotation_setRotateAxisY(CSGRotation_t * csgr, double rotateAxisY)
{
  if (csgr != NULL)
    return csgr->setRotateAxisY(rotateAxisY);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CSGRotation_setRotateAxisZ(CSGRotation_t * csgr, double rotateAxisZ)
{
  if (csgr != NULL)
    return csgr->setRotateAxisZ(rotateAxisZ);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CSGRotation_setRotateAngleInRadians(CSGRotation_t * csgr, double rotateAngleInRadians)
{
  if (csgr != NULL)
    return csgr->setRotateAngleInRadians(rotateAngleInRadians);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CSGRotation_unsetRotateAxisX(CSGRotation_t * csgr)
{
  return (csgr != NULL) ? csgr->unsetRotateAxisX() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CSGRotation_unsetRotateAxisY(CSGRotation_t * csgr)
{
  return (csgr != NULL) ? csgr->unsetRotateAxisY() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CSGRotation_unsetRotateAxisZ(CSGRotation_t * csgr)
{
  return (csgr != NULL) ? csgr->unsetRotateAxisZ() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CSGRotation_unsetRotateAngleInRadians(CSGRotation_t * csgr)
{
  return (csgr != NULL) ? csgr->unsetRotateAngleInRadians() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CSGRotation_hasRequiredAttributes(const CSGRotation_t * csgr)
{
  return (csgr != NULL) ? static_cast<int>(csgr->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


