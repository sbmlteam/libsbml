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
  , mRotateX (numeric_limits<double>::quiet_NaN())
  , mIsSetRotateX (false)
  , mRotateY (numeric_limits<double>::quiet_NaN())
  , mIsSetRotateY (false)
  , mRotateZ (numeric_limits<double>::quiet_NaN())
  , mIsSetRotateZ (false)
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
  , mRotateX (numeric_limits<double>::quiet_NaN())
  , mIsSetRotateX (false)
  , mRotateY (numeric_limits<double>::quiet_NaN())
  , mIsSetRotateY (false)
  , mRotateZ (numeric_limits<double>::quiet_NaN())
  , mIsSetRotateZ (false)
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
    mRotateX  = orig.mRotateX;
    mIsSetRotateX  = orig.mIsSetRotateX;
    mRotateY  = orig.mRotateY;
    mIsSetRotateY  = orig.mIsSetRotateY;
    mRotateZ  = orig.mRotateZ;
    mIsSetRotateZ  = orig.mIsSetRotateZ;
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
    mRotateX  = rhs.mRotateX;
    mIsSetRotateX  = rhs.mIsSetRotateX;
    mRotateY  = rhs.mRotateY;
    mIsSetRotateY  = rhs.mIsSetRotateY;
    mRotateZ  = rhs.mRotateZ;
    mIsSetRotateZ  = rhs.mIsSetRotateZ;
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
 * Returns the value of the "rotateX" attribute of this CSGRotation.
 */
double
CSGRotation::getRotateX() const
{
  return mRotateX;
}


/*
 * Returns the value of the "rotateY" attribute of this CSGRotation.
 */
double
CSGRotation::getRotateY() const
{
  return mRotateY;
}


/*
 * Returns the value of the "rotateZ" attribute of this CSGRotation.
 */
double
CSGRotation::getRotateZ() const
{
  return mRotateZ;
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
 * Returns true/false if rotateX is set.
 */
bool
CSGRotation::isSetRotateX() const
{
  return mIsSetRotateX;
}


/*
 * Returns true/false if rotateY is set.
 */
bool
CSGRotation::isSetRotateY() const
{
  return mIsSetRotateY;
}


/*
 * Returns true/false if rotateZ is set.
 */
bool
CSGRotation::isSetRotateZ() const
{
  return mIsSetRotateZ;
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
 * Sets rotateX and returns value indicating success.
 */
int
CSGRotation::setRotateX(double rotateX)
{
  mRotateX = rotateX;
  mIsSetRotateX = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets rotateY and returns value indicating success.
 */
int
CSGRotation::setRotateY(double rotateY)
{
  mRotateY = rotateY;
  mIsSetRotateY = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets rotateZ and returns value indicating success.
 */
int
CSGRotation::setRotateZ(double rotateZ)
{
  mRotateZ = rotateZ;
  mIsSetRotateZ = true;
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
 * Unsets rotateX and returns value indicating success.
 */
int
CSGRotation::unsetRotateX()
{
  mRotateX = numeric_limits<double>::quiet_NaN();
  mIsSetRotateX = false;

  if (isSetRotateX() == false)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets rotateY and returns value indicating success.
 */
int
CSGRotation::unsetRotateY()
{
  mRotateY = numeric_limits<double>::quiet_NaN();
  mIsSetRotateY = false;

  if (isSetRotateY() == false)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets rotateZ and returns value indicating success.
 */
int
CSGRotation::unsetRotateZ()
{
  mRotateZ = numeric_limits<double>::quiet_NaN();
  mIsSetRotateZ = false;

  if (isSetRotateZ() == false)
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

  if (isSetRotateX() == false)
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

  attributes.add("rotateX");
  attributes.add("rotateY");
  attributes.add("rotateZ");
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
  // rotateX double   ( use = "required" )
  //
  numErrs = getErrorLog()->getNumErrors();
  mIsSetRotateX = attributes.readInto("rotateX", mRotateX);

  if (mIsSetRotateX == false)
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
        std::string message = "Spatial attribute 'rotateX' is missing from 'csgRotation' object.";
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
      }
    }
  }

  //
  // rotateY double   ( use = "optional" )
  //
  numErrs = getErrorLog()->getNumErrors();
  mIsSetRotateY = attributes.readInto("rotateY", mRotateY);

  if (mIsSetRotateY == false)
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
  // rotateZ double   ( use = "optional" )
  //
  numErrs = getErrorLog()->getNumErrors();
  mIsSetRotateZ = attributes.readInto("rotateZ", mRotateZ);

  if (mIsSetRotateZ == false)
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
                     getPackageVersion(), sbmlLevel, sbmlVersion, "", getLine(), getColumn());
      }
      else
      {
        std::string message = "Spatial attribute 'rotateAngleInRadians' is missing from 'csgRotation' object.";
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
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

  if (isSetRotateX() == true)
    stream.writeAttribute("rotateX", getPrefix(), mRotateX);

  if (isSetRotateY() == true)
    stream.writeAttribute("rotateY", getPrefix(), mRotateY);

  if (isSetRotateZ() == true)
    stream.writeAttribute("rotateZ", getPrefix(), mRotateZ);

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
CSGRotation_getRotateX(const CSGRotation_t * csgr)
{
	return (csgr != NULL) ? csgr->getRotateX() : numeric_limits<double>::quiet_NaN();
}


LIBSBML_EXTERN
double
CSGRotation_getRotateY(const CSGRotation_t * csgr)
{
	return (csgr != NULL) ? csgr->getRotateY() : numeric_limits<double>::quiet_NaN();
}


LIBSBML_EXTERN
double
CSGRotation_getRotateZ(const CSGRotation_t * csgr)
{
	return (csgr != NULL) ? csgr->getRotateZ() : numeric_limits<double>::quiet_NaN();
}


LIBSBML_EXTERN
double
CSGRotation_getRotateAngleInRadians(const CSGRotation_t * csgr)
{
	return (csgr != NULL) ? csgr->getRotateAngleInRadians() : numeric_limits<double>::quiet_NaN();
}


LIBSBML_EXTERN
int
CSGRotation_isSetRotateX(const CSGRotation_t * csgr)
{
  return (csgr != NULL) ? static_cast<int>(csgr->isSetRotateX()) : 0;
}


LIBSBML_EXTERN
int
CSGRotation_isSetRotateY(const CSGRotation_t * csgr)
{
  return (csgr != NULL) ? static_cast<int>(csgr->isSetRotateY()) : 0;
}


LIBSBML_EXTERN
int
CSGRotation_isSetRotateZ(const CSGRotation_t * csgr)
{
  return (csgr != NULL) ? static_cast<int>(csgr->isSetRotateZ()) : 0;
}


LIBSBML_EXTERN
int
CSGRotation_isSetRotateAngleInRadians(const CSGRotation_t * csgr)
{
  return (csgr != NULL) ? static_cast<int>(csgr->isSetRotateAngleInRadians()) : 0;
}


LIBSBML_EXTERN
int
CSGRotation_setRotateX(CSGRotation_t * csgr, double rotateX)
{
  if (csgr != NULL)
    return csgr->setRotateX(rotateX);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CSGRotation_setRotateY(CSGRotation_t * csgr, double rotateY)
{
  if (csgr != NULL)
    return csgr->setRotateY(rotateY);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CSGRotation_setRotateZ(CSGRotation_t * csgr, double rotateZ)
{
  if (csgr != NULL)
    return csgr->setRotateZ(rotateZ);
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
CSGRotation_unsetRotateX(CSGRotation_t * csgr)
{
  return (csgr != NULL) ? csgr->unsetRotateX() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CSGRotation_unsetRotateY(CSGRotation_t * csgr)
{
  return (csgr != NULL) ? csgr->unsetRotateY() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CSGRotation_unsetRotateZ(CSGRotation_t * csgr)
{
  return (csgr != NULL) ? csgr->unsetRotateZ() : LIBSBML_INVALID_OBJECT;
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


