/**
 * @file CSGRotation.cpp
 * @brief Implementation of the CSGRotation class.
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
#include <sbml/packages/spatial/sbml/CSGRotation.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new CSGRotation using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
CSGRotation::CSGRotation(unsigned int level,
                         unsigned int version,
                         unsigned int pkgVersion)
  : CSGTransformation(level, version, pkgVersion)
  , mRotateX (util_NaN())
  , mIsSetRotateX (false)
  , mRotateY (util_NaN())
  , mIsSetRotateY (false)
  , mRotateZ (util_NaN())
  , mIsSetRotateZ (false)
  , mRotateAngleInRadians (util_NaN())
  , mIsSetRotateAngleInRadians (false)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version,
    pkgVersion));
}


/*
 * Creates a new CSGRotation using the given SpatialPkgNamespaces object.
 */
CSGRotation::CSGRotation(SpatialPkgNamespaces *spatialns)
  : CSGTransformation(spatialns)
  , mRotateX (util_NaN())
  , mIsSetRotateX (false)
  , mRotateY (util_NaN())
  , mIsSetRotateY (false)
  , mRotateZ (util_NaN())
  , mIsSetRotateZ (false)
  , mRotateAngleInRadians (util_NaN())
  , mIsSetRotateAngleInRadians (false)
{
  setElementNamespace(spatialns->getURI());
  loadPlugins(spatialns);
}


/*
 * Copy constructor for CSGRotation.
 */
CSGRotation::CSGRotation(const CSGRotation& orig)
  : CSGTransformation( orig )
  , mRotateX ( orig.mRotateX )
  , mIsSetRotateX ( orig.mIsSetRotateX )
  , mRotateY ( orig.mRotateY )
  , mIsSetRotateY ( orig.mIsSetRotateY )
  , mRotateZ ( orig.mRotateZ )
  , mIsSetRotateZ ( orig.mIsSetRotateZ )
  , mRotateAngleInRadians ( orig.mRotateAngleInRadians )
  , mIsSetRotateAngleInRadians ( orig.mIsSetRotateAngleInRadians )
{
}


/*
 * Assignment operator for CSGRotation.
 */
CSGRotation&
CSGRotation::operator=(const CSGRotation& rhs)
{
  if (&rhs != this)
  {
    CSGTransformation::operator=(rhs);
    mRotateX = rhs.mRotateX;
    mIsSetRotateX = rhs.mIsSetRotateX;
    mRotateY = rhs.mRotateY;
    mIsSetRotateY = rhs.mIsSetRotateY;
    mRotateZ = rhs.mRotateZ;
    mIsSetRotateZ = rhs.mIsSetRotateZ;
    mRotateAngleInRadians = rhs.mRotateAngleInRadians;
    mIsSetRotateAngleInRadians = rhs.mIsSetRotateAngleInRadians;
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this CSGRotation object.
 */
CSGRotation*
CSGRotation::clone() const
{
  return new CSGRotation(*this);
}


/*
 * Destructor for CSGRotation.
 */
CSGRotation::~CSGRotation()
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
 * Returns the value of the "rotateAngleInRadians" attribute of this
 * CSGRotation.
 */
double
CSGRotation::getRotateAngleInRadians() const
{
  return mRotateAngleInRadians;
}


/*
 * Predicate returning @c true if this CSGRotation's "rotateX" attribute is
 * set.
 */
bool
CSGRotation::isSetRotateX() const
{
  return mIsSetRotateX;
}


/*
 * Predicate returning @c true if this CSGRotation's "rotateY" attribute is
 * set.
 */
bool
CSGRotation::isSetRotateY() const
{
  return mIsSetRotateY;
}


/*
 * Predicate returning @c true if this CSGRotation's "rotateZ" attribute is
 * set.
 */
bool
CSGRotation::isSetRotateZ() const
{
  return mIsSetRotateZ;
}


/*
 * Predicate returning @c true if this CSGRotation's "rotateAngleInRadians"
 * attribute is set.
 */
bool
CSGRotation::isSetRotateAngleInRadians() const
{
  return mIsSetRotateAngleInRadians;
}


/*
 * Sets the value of the "rotateX" attribute of this CSGRotation.
 */
int
CSGRotation::setRotateX(double rotateX)
{
  mRotateX = rotateX;
  mIsSetRotateX = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "rotateY" attribute of this CSGRotation.
 */
int
CSGRotation::setRotateY(double rotateY)
{
  mRotateY = rotateY;
  mIsSetRotateY = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "rotateZ" attribute of this CSGRotation.
 */
int
CSGRotation::setRotateZ(double rotateZ)
{
  mRotateZ = rotateZ;
  mIsSetRotateZ = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "rotateAngleInRadians" attribute of this CSGRotation.
 */
int
CSGRotation::setRotateAngleInRadians(double rotateAngleInRadians)
{
  mRotateAngleInRadians = rotateAngleInRadians;
  mIsSetRotateAngleInRadians = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "rotateX" attribute of this CSGRotation.
 */
int
CSGRotation::unsetRotateX()
{
  mRotateX = util_NaN();
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
 * Unsets the value of the "rotateY" attribute of this CSGRotation.
 */
int
CSGRotation::unsetRotateY()
{
  mRotateY = util_NaN();
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
 * Unsets the value of the "rotateZ" attribute of this CSGRotation.
 */
int
CSGRotation::unsetRotateZ()
{
  mRotateZ = util_NaN();
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
 * Unsets the value of the "rotateAngleInRadians" attribute of this
 * CSGRotation.
 */
int
CSGRotation::unsetRotateAngleInRadians()
{
  mRotateAngleInRadians = util_NaN();
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
 * Returns the XML element name of this CSGRotation object.
 */
const std::string&
CSGRotation::getElementName() const
{
  static const string name = "csgRotation";
  return name;
}


/*
 * Returns the libSBML type code for this CSGRotation object.
 */
int
CSGRotation::getTypeCode() const
{
  return SBML_SPATIAL_CSGROTATION;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * CSGRotation object have been set.
 */
bool
CSGRotation::hasRequiredAttributes() const
{
  bool allPresent = CSGTransformation::hasRequiredAttributes();

  if (isSetRotateX() == false)
  {
    allPresent = false;
  }

  if (isSetRotateAngleInRadians() == false)
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
CSGRotation::writeElements(XMLOutputStream& stream) const
{
  CSGTransformation::writeElements(stream);

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
CSGRotation::accept(SBMLVisitor& v) const
{
  return v.visit(*this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
CSGRotation::setSBMLDocument(SBMLDocument* d)
{
  CSGTransformation::setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
CSGRotation::enablePackageInternal(const std::string& pkgURI,
                                   const std::string& pkgPrefix,
                                   bool flag)
{
  CSGTransformation::enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CSGRotation.
 */
int
CSGRotation::getAttribute(const std::string& attributeName, bool& value) const
{
  int return_value = CSGTransformation::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CSGRotation.
 */
int
CSGRotation::getAttribute(const std::string& attributeName, int& value) const
{
  int return_value = CSGTransformation::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CSGRotation.
 */
int
CSGRotation::getAttribute(const std::string& attributeName,
                          double& value) const
{
  int return_value = CSGTransformation::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "rotateX")
  {
    value = getRotateX();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "rotateY")
  {
    value = getRotateY();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "rotateZ")
  {
    value = getRotateZ();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "rotateAngleInRadians")
  {
    value = getRotateAngleInRadians();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CSGRotation.
 */
int
CSGRotation::getAttribute(const std::string& attributeName,
                          unsigned int& value) const
{
  int return_value = CSGTransformation::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CSGRotation.
 */
int
CSGRotation::getAttribute(const std::string& attributeName,
                          std::string& value) const
{
  int return_value = CSGTransformation::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this CSGRotation's attribute "attributeName"
 * is set.
 */
bool
CSGRotation::isSetAttribute(const std::string& attributeName) const
{
  bool value = CSGTransformation::isSetAttribute(attributeName);

  if (attributeName == "rotateX")
  {
    value = isSetRotateX();
  }
  else if (attributeName == "rotateY")
  {
    value = isSetRotateY();
  }
  else if (attributeName == "rotateZ")
  {
    value = isSetRotateZ();
  }
  else if (attributeName == "rotateAngleInRadians")
  {
    value = isSetRotateAngleInRadians();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CSGRotation.
 */
int
CSGRotation::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = CSGTransformation::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CSGRotation.
 */
int
CSGRotation::setAttribute(const std::string& attributeName, int value)
{
  int return_value = CSGTransformation::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CSGRotation.
 */
int
CSGRotation::setAttribute(const std::string& attributeName, double value)
{
  int return_value = CSGTransformation::setAttribute(attributeName, value);

  if (attributeName == "rotateX")
  {
    return_value = setRotateX(value);
  }
  else if (attributeName == "rotateY")
  {
    return_value = setRotateY(value);
  }
  else if (attributeName == "rotateZ")
  {
    return_value = setRotateZ(value);
  }
  else if (attributeName == "rotateAngleInRadians")
  {
    return_value = setRotateAngleInRadians(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CSGRotation.
 */
int
CSGRotation::setAttribute(const std::string& attributeName,
                          unsigned int value)
{
  int return_value = CSGTransformation::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CSGRotation.
 */
int
CSGRotation::setAttribute(const std::string& attributeName,
                          const std::string& value)
{
  int return_value = CSGTransformation::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this CSGRotation.
 */
int
CSGRotation::unsetAttribute(const std::string& attributeName)
{
  int value = CSGTransformation::unsetAttribute(attributeName);

  if (attributeName == "rotateX")
  {
    value = unsetRotateX();
  }
  else if (attributeName == "rotateY")
  {
    value = unsetRotateY();
  }
  else if (attributeName == "rotateZ")
  {
    value = unsetRotateZ();
  }
  else if (attributeName == "rotateAngleInRadians")
  {
    value = unsetRotateAngleInRadians();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
CSGRotation::createObject(XMLInputStream& stream)
{
  SBase* obj = CSGTransformation::createObject(stream);

  connectToChild();

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
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

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
CSGRotation::readAttributes(const XMLAttributes& attributes,
                            const ExpectedAttributes& expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  CSGTransformation::readAttributes(attributes, expectedAttributes);

  if (log)
  {
    numErrs = log->getNumErrors();

    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("spatial", SpatialCSGRotationAllowedAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("spatial",
          SpatialCSGRotationAllowedCoreAttributes, pkgVersion, level, version,
            details, getLine(), getColumn());
      }
    }
  }

  // 
  // rotateX double (use = "required" )
  // 

  numErrs = log->getNumErrors();
  mIsSetRotateX = attributes.readInto("rotateX", mRotateX);

  if ( mIsSetRotateX == false)
  {
    if (log->getNumErrors() == numErrs + 1 &&
      log->contains(XMLAttributeTypeMismatch))
    {
      log->remove(XMLAttributeTypeMismatch);
      std::string message = "Spatial attribute 'rotateX' from the <csgRotation> "
        "element must be an integer.";
      log->logPackageError("spatial", SpatialCSGRotationRotateXMustBeDouble,
        pkgVersion, level, version, message, getLine(), getColumn());
    }
    else
    {
      std::string message = "Spatial attribute 'rotateX' is missing from the "
        "<csgRotation> element.";
      log->logPackageError("spatial", SpatialCSGRotationAllowedAttributes,
        pkgVersion, level, version, message, getLine(), getColumn());
    }
  }

  // 
  // rotateY double (use = "optional" )
  // 

  numErrs = log->getNumErrors();
  mIsSetRotateY = attributes.readInto("rotateY", mRotateY);

  if ( mIsSetRotateY == false)
  {
    if (log->getNumErrors() == numErrs + 1 &&
      log->contains(XMLAttributeTypeMismatch))
    {
      log->remove(XMLAttributeTypeMismatch);
      std::string message = "Spatial attribute 'rotateY' from the <csgRotation> "
        "element must be an integer.";
      log->logPackageError("spatial", SpatialCSGRotationRotateYMustBeDouble,
        pkgVersion, level, version, message, getLine(), getColumn());
    }
  }

  // 
  // rotateZ double (use = "optional" )
  // 

  numErrs = log->getNumErrors();
  mIsSetRotateZ = attributes.readInto("rotateZ", mRotateZ);

  if ( mIsSetRotateZ == false)
  {
    if (log->getNumErrors() == numErrs + 1 &&
      log->contains(XMLAttributeTypeMismatch))
    {
      log->remove(XMLAttributeTypeMismatch);
      std::string message = "Spatial attribute 'rotateZ' from the <csgRotation> "
        "element must be an integer.";
      log->logPackageError("spatial", SpatialCSGRotationRotateZMustBeDouble,
        pkgVersion, level, version, message, getLine(), getColumn());
    }
  }

  // 
  // rotateAngleInRadians double (use = "required" )
  // 

  numErrs = log->getNumErrors();
  mIsSetRotateAngleInRadians = attributes.readInto("rotateAngleInRadians",
    mRotateAngleInRadians);

  if ( mIsSetRotateAngleInRadians == false)
  {
    if (log->getNumErrors() == numErrs + 1 &&
      log->contains(XMLAttributeTypeMismatch))
    {
      log->remove(XMLAttributeTypeMismatch);
      std::string message = "Spatial attribute 'rotateAngleInRadians' from the "
        "<csgRotation> element must be an integer.";
      log->logPackageError("spatial",
        SpatialCSGRotationRotateAngleInRadiansMustBeDouble, pkgVersion, level,
          version, message, getLine(), getColumn());
    }
    else
    {
      std::string message = "Spatial attribute 'rotateAngleInRadians' is "
        "missing from the <csgRotation> element.";
      log->logPackageError("spatial", SpatialCSGRotationAllowedAttributes,
        pkgVersion, level, version, message, getLine(), getColumn());
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
CSGRotation::writeAttributes(XMLOutputStream& stream) const
{
  CSGTransformation::writeAttributes(stream);

  if (isSetRotateX() == true)
  {
    stream.writeAttribute("rotateX", getPrefix(), mRotateX);
  }

  if (isSetRotateY() == true)
  {
    stream.writeAttribute("rotateY", getPrefix(), mRotateY);
  }

  if (isSetRotateZ() == true)
  {
    stream.writeAttribute("rotateZ", getPrefix(), mRotateZ);
  }

  if (isSetRotateAngleInRadians() == true)
  {
    stream.writeAttribute("rotateAngleInRadians", getPrefix(),
      mRotateAngleInRadians);
  }

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new CSGRotation_t using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
LIBSBML_EXTERN
CSGRotation_t *
CSGRotation_create(unsigned int level,
                   unsigned int version,
                   unsigned int pkgVersion)
{
  return new CSGRotation(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this CSGRotation_t object.
 */
LIBSBML_EXTERN
CSGRotation_t*
CSGRotation_clone(const CSGRotation_t* csgr)
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


/*
 * Frees this CSGRotation_t object.
 */
LIBSBML_EXTERN
void
CSGRotation_free(CSGRotation_t* csgr)
{
  if (csgr != NULL)
  {
    delete csgr;
  }
}


/*
 * Returns the value of the "rotateX" attribute of this CSGRotation_t.
 */
LIBSBML_EXTERN
double
CSGRotation_getRotateX(const CSGRotation_t * csgr)
{
  return (csgr != NULL) ? csgr->getRotateX() : util_NaN();
}


/*
 * Returns the value of the "rotateY" attribute of this CSGRotation_t.
 */
LIBSBML_EXTERN
double
CSGRotation_getRotateY(const CSGRotation_t * csgr)
{
  return (csgr != NULL) ? csgr->getRotateY() : util_NaN();
}


/*
 * Returns the value of the "rotateZ" attribute of this CSGRotation_t.
 */
LIBSBML_EXTERN
double
CSGRotation_getRotateZ(const CSGRotation_t * csgr)
{
  return (csgr != NULL) ? csgr->getRotateZ() : util_NaN();
}


/*
 * Returns the value of the "rotateAngleInRadians" attribute of this
 * CSGRotation_t.
 */
LIBSBML_EXTERN
double
CSGRotation_getRotateAngleInRadians(const CSGRotation_t * csgr)
{
  return (csgr != NULL) ? csgr->getRotateAngleInRadians() : util_NaN();
}


/*
 * Predicate returning @c 1 (true) if this CSGRotation_t's "rotateX" attribute
 * is set.
 */
LIBSBML_EXTERN
int
CSGRotation_isSetRotateX(const CSGRotation_t * csgr)
{
  return (csgr != NULL) ? static_cast<int>(csgr->isSetRotateX()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this CSGRotation_t's "rotateY" attribute
 * is set.
 */
LIBSBML_EXTERN
int
CSGRotation_isSetRotateY(const CSGRotation_t * csgr)
{
  return (csgr != NULL) ? static_cast<int>(csgr->isSetRotateY()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this CSGRotation_t's "rotateZ" attribute
 * is set.
 */
LIBSBML_EXTERN
int
CSGRotation_isSetRotateZ(const CSGRotation_t * csgr)
{
  return (csgr != NULL) ? static_cast<int>(csgr->isSetRotateZ()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this CSGRotation_t's
 * "rotateAngleInRadians" attribute is set.
 */
LIBSBML_EXTERN
int
CSGRotation_isSetRotateAngleInRadians(const CSGRotation_t * csgr)
{
  return (csgr != NULL) ? static_cast<int>(csgr->isSetRotateAngleInRadians()) :
    0;
}


/*
 * Sets the value of the "rotateX" attribute of this CSGRotation_t.
 */
LIBSBML_EXTERN
int
CSGRotation_setRotateX(CSGRotation_t * csgr, double rotateX)
{
  return (csgr != NULL) ? csgr->setRotateX(rotateX) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "rotateY" attribute of this CSGRotation_t.
 */
LIBSBML_EXTERN
int
CSGRotation_setRotateY(CSGRotation_t * csgr, double rotateY)
{
  return (csgr != NULL) ? csgr->setRotateY(rotateY) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "rotateZ" attribute of this CSGRotation_t.
 */
LIBSBML_EXTERN
int
CSGRotation_setRotateZ(CSGRotation_t * csgr, double rotateZ)
{
  return (csgr != NULL) ? csgr->setRotateZ(rotateZ) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "rotateAngleInRadians" attribute of this
 * CSGRotation_t.
 */
LIBSBML_EXTERN
int
CSGRotation_setRotateAngleInRadians(CSGRotation_t * csgr,
                                    double rotateAngleInRadians)
{
  return (csgr != NULL) ? csgr->setRotateAngleInRadians(rotateAngleInRadians) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "rotateX" attribute of this CSGRotation_t.
 */
LIBSBML_EXTERN
int
CSGRotation_unsetRotateX(CSGRotation_t * csgr)
{
  return (csgr != NULL) ? csgr->unsetRotateX() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "rotateY" attribute of this CSGRotation_t.
 */
LIBSBML_EXTERN
int
CSGRotation_unsetRotateY(CSGRotation_t * csgr)
{
  return (csgr != NULL) ? csgr->unsetRotateY() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "rotateZ" attribute of this CSGRotation_t.
 */
LIBSBML_EXTERN
int
CSGRotation_unsetRotateZ(CSGRotation_t * csgr)
{
  return (csgr != NULL) ? csgr->unsetRotateZ() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "rotateAngleInRadians" attribute of this
 * CSGRotation_t.
 */
LIBSBML_EXTERN
int
CSGRotation_unsetRotateAngleInRadians(CSGRotation_t * csgr)
{
  return (csgr != NULL) ? csgr->unsetRotateAngleInRadians() :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * CSGRotation_t object have been set.
 */
LIBSBML_EXTERN
int
CSGRotation_hasRequiredAttributes(const CSGRotation_t * csgr)
{
  return (csgr != NULL) ? static_cast<int>(csgr->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


