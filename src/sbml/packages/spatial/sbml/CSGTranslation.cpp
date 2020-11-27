/**
 * @file CSGTranslation.cpp
 * @brief Implementation of the CSGTranslation class.
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
#include <sbml/packages/spatial/sbml/CSGTranslation.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new CSGTranslation using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
CSGTranslation::CSGTranslation(unsigned int level,
                               unsigned int version,
                               unsigned int pkgVersion)
  : CSGTransformation(level, version, pkgVersion)
  , mTranslateX (util_NaN())
  , mIsSetTranslateX (false)
  , mTranslateY (util_NaN())
  , mIsSetTranslateY (false)
  , mTranslateZ (util_NaN())
  , mIsSetTranslateZ (false)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version,
    pkgVersion));
}


/*
 * Creates a new CSGTranslation using the given SpatialPkgNamespaces object.
 */
CSGTranslation::CSGTranslation(SpatialPkgNamespaces *spatialns)
  : CSGTransformation(spatialns)
  , mTranslateX (util_NaN())
  , mIsSetTranslateX (false)
  , mTranslateY (util_NaN())
  , mIsSetTranslateY (false)
  , mTranslateZ (util_NaN())
  , mIsSetTranslateZ (false)
{
  setElementNamespace(spatialns->getURI());
  loadPlugins(spatialns);
}


/*
 * Copy constructor for CSGTranslation.
 */
CSGTranslation::CSGTranslation(const CSGTranslation& orig)
  : CSGTransformation( orig )
  , mTranslateX ( orig.mTranslateX )
  , mIsSetTranslateX ( orig.mIsSetTranslateX )
  , mTranslateY ( orig.mTranslateY )
  , mIsSetTranslateY ( orig.mIsSetTranslateY )
  , mTranslateZ ( orig.mTranslateZ )
  , mIsSetTranslateZ ( orig.mIsSetTranslateZ )
{
}


/*
 * Assignment operator for CSGTranslation.
 */
CSGTranslation&
CSGTranslation::operator=(const CSGTranslation& rhs)
{
  if (&rhs != this)
  {
    CSGTransformation::operator=(rhs);
    mTranslateX = rhs.mTranslateX;
    mIsSetTranslateX = rhs.mIsSetTranslateX;
    mTranslateY = rhs.mTranslateY;
    mIsSetTranslateY = rhs.mIsSetTranslateY;
    mTranslateZ = rhs.mTranslateZ;
    mIsSetTranslateZ = rhs.mIsSetTranslateZ;
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this CSGTranslation object.
 */
CSGTranslation*
CSGTranslation::clone() const
{
  return new CSGTranslation(*this);
}


/*
 * Destructor for CSGTranslation.
 */
CSGTranslation::~CSGTranslation()
{
}


/*
 * Returns the value of the "translateX" attribute of this CSGTranslation.
 */
double
CSGTranslation::getTranslateX() const
{
  return mTranslateX;
}


/*
 * Returns the value of the "translateY" attribute of this CSGTranslation.
 */
double
CSGTranslation::getTranslateY() const
{
  return mTranslateY;
}


/*
 * Returns the value of the "translateZ" attribute of this CSGTranslation.
 */
double
CSGTranslation::getTranslateZ() const
{
  return mTranslateZ;
}


/*
 * Predicate returning @c true if this CSGTranslation's "translateX" attribute
 * is set.
 */
bool
CSGTranslation::isSetTranslateX() const
{
  return mIsSetTranslateX;
}


/*
 * Predicate returning @c true if this CSGTranslation's "translateY" attribute
 * is set.
 */
bool
CSGTranslation::isSetTranslateY() const
{
  return mIsSetTranslateY;
}


/*
 * Predicate returning @c true if this CSGTranslation's "translateZ" attribute
 * is set.
 */
bool
CSGTranslation::isSetTranslateZ() const
{
  return mIsSetTranslateZ;
}


/*
 * Sets the value of the "translateX" attribute of this CSGTranslation.
 */
int
CSGTranslation::setTranslateX(double translateX)
{
  mTranslateX = translateX;
  mIsSetTranslateX = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "translateY" attribute of this CSGTranslation.
 */
int
CSGTranslation::setTranslateY(double translateY)
{
  mTranslateY = translateY;
  mIsSetTranslateY = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "translateZ" attribute of this CSGTranslation.
 */
int
CSGTranslation::setTranslateZ(double translateZ)
{
  mTranslateZ = translateZ;
  mIsSetTranslateZ = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "translateX" attribute of this CSGTranslation.
 */
int
CSGTranslation::unsetTranslateX()
{
  mTranslateX = util_NaN();
  mIsSetTranslateX = false;

  if (isSetTranslateX() == false)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "translateY" attribute of this CSGTranslation.
 */
int
CSGTranslation::unsetTranslateY()
{
  mTranslateY = util_NaN();
  mIsSetTranslateY = false;

  if (isSetTranslateY() == false)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "translateZ" attribute of this CSGTranslation.
 */
int
CSGTranslation::unsetTranslateZ()
{
  mTranslateZ = util_NaN();
  mIsSetTranslateZ = false;

  if (isSetTranslateZ() == false)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Returns the XML element name of this CSGTranslation object.
 */
const std::string&
CSGTranslation::getElementName() const
{
  static const string name = "csgTranslation";
  return name;
}


/*
 * Returns the libSBML type code for this CSGTranslation object.
 */
int
CSGTranslation::getTypeCode() const
{
  return SBML_SPATIAL_CSGTRANSLATION;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * CSGTranslation object have been set.
 */
bool
CSGTranslation::hasRequiredAttributes() const
{
  bool allPresent = CSGTransformation::hasRequiredAttributes();

  if (isSetTranslateX() == false)
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
CSGTranslation::writeElements(XMLOutputStream& stream) const
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
CSGTranslation::accept(SBMLVisitor& v) const
{
  return v.visit(*this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
CSGTranslation::setSBMLDocument(SBMLDocument* d)
{
  CSGTransformation::setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
CSGTranslation::enablePackageInternal(const std::string& pkgURI,
                                      const std::string& pkgPrefix,
                                      bool flag)
{
  CSGTransformation::enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CSGTranslation.
 */
int
CSGTranslation::getAttribute(const std::string& attributeName,
                             bool& value) const
{
  int return_value = CSGTransformation::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CSGTranslation.
 */
int
CSGTranslation::getAttribute(const std::string& attributeName,
                             int& value) const
{
  int return_value = CSGTransformation::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CSGTranslation.
 */
int
CSGTranslation::getAttribute(const std::string& attributeName,
                             double& value) const
{
  int return_value = CSGTransformation::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "translateX")
  {
    value = getTranslateX();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "translateY")
  {
    value = getTranslateY();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "translateZ")
  {
    value = getTranslateZ();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CSGTranslation.
 */
int
CSGTranslation::getAttribute(const std::string& attributeName,
                             unsigned int& value) const
{
  int return_value = CSGTransformation::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CSGTranslation.
 */
int
CSGTranslation::getAttribute(const std::string& attributeName,
                             std::string& value) const
{
  int return_value = CSGTransformation::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this CSGTranslation's attribute
 * "attributeName" is set.
 */
bool
CSGTranslation::isSetAttribute(const std::string& attributeName) const
{
  bool value = CSGTransformation::isSetAttribute(attributeName);

  if (attributeName == "translateX")
  {
    value = isSetTranslateX();
  }
  else if (attributeName == "translateY")
  {
    value = isSetTranslateY();
  }
  else if (attributeName == "translateZ")
  {
    value = isSetTranslateZ();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CSGTranslation.
 */
int
CSGTranslation::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = CSGTransformation::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CSGTranslation.
 */
int
CSGTranslation::setAttribute(const std::string& attributeName, int value)
{
  int return_value = CSGTransformation::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CSGTranslation.
 */
int
CSGTranslation::setAttribute(const std::string& attributeName, double value)
{
  int return_value = CSGTransformation::setAttribute(attributeName, value);

  if (attributeName == "translateX")
  {
    return_value = setTranslateX(value);
  }
  else if (attributeName == "translateY")
  {
    return_value = setTranslateY(value);
  }
  else if (attributeName == "translateZ")
  {
    return_value = setTranslateZ(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CSGTranslation.
 */
int
CSGTranslation::setAttribute(const std::string& attributeName,
                             unsigned int value)
{
  int return_value = CSGTransformation::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CSGTranslation.
 */
int
CSGTranslation::setAttribute(const std::string& attributeName,
                             const std::string& value)
{
  int return_value = CSGTransformation::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this CSGTranslation.
 */
int
CSGTranslation::unsetAttribute(const std::string& attributeName)
{
  int value = CSGTransformation::unsetAttribute(attributeName);

  if (attributeName == "translateX")
  {
    value = unsetTranslateX();
  }
  else if (attributeName == "translateY")
  {
    value = unsetTranslateY();
  }
  else if (attributeName == "translateZ")
  {
    value = unsetTranslateZ();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
CSGTranslation::createObject(XMLInputStream& stream)
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
CSGTranslation::addExpectedAttributes(ExpectedAttributes& attributes)
{
  CSGTransformation::addExpectedAttributes(attributes);

  attributes.add("translateX");

  attributes.add("translateY");

  attributes.add("translateZ");
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
CSGTranslation::readAttributes(const XMLAttributes& attributes,
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
        log->logPackageError("spatial", SpatialCSGTranslationAllowedAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("spatial",
          SpatialCSGTranslationAllowedCoreAttributes, pkgVersion, level, version,
            details, getLine(), getColumn());
      }
    }
  }

  // 
  // translateX double (use = "required" )
  // 

  numErrs = log->getNumErrors();
  mIsSetTranslateX = attributes.readInto("translateX", mTranslateX);

  if ( mIsSetTranslateX == false)
  {
    if (log->getNumErrors() == numErrs + 1 &&
      log->contains(XMLAttributeTypeMismatch))
    {
      log->remove(XMLAttributeTypeMismatch);
      std::string message = "Spatial attribute 'translateX' from the "
        "<csgTranslation> element must be an integer.";
      log->logPackageError("spatial",
        SpatialCSGTranslationTranslateXMustBeDouble, pkgVersion, level, version,
          message, getLine(), getColumn());
    }
    else
    {
      std::string message = "Spatial attribute 'translateX' is missing from the "
        "<csgTranslation> element.";
      log->logPackageError("spatial", SpatialCSGTranslationAllowedAttributes,
        pkgVersion, level, version, message, getLine(), getColumn());
    }
  }

  // 
  // translateY double (use = "optional" )
  // 

  numErrs = log->getNumErrors();
  mIsSetTranslateY = attributes.readInto("translateY", mTranslateY);

  if ( mIsSetTranslateY == false)
  {
    if (log->getNumErrors() == numErrs + 1 &&
      log->contains(XMLAttributeTypeMismatch))
    {
      log->remove(XMLAttributeTypeMismatch);
      std::string message = "Spatial attribute 'translateY' from the "
        "<csgTranslation> element must be an integer.";
      log->logPackageError("spatial",
        SpatialCSGTranslationTranslateYMustBeDouble, pkgVersion, level, version,
          message, getLine(), getColumn());
    }
  }

  // 
  // translateZ double (use = "optional" )
  // 

  numErrs = log->getNumErrors();
  mIsSetTranslateZ = attributes.readInto("translateZ", mTranslateZ);

  if ( mIsSetTranslateZ == false)
  {
    if (log->getNumErrors() == numErrs + 1 &&
      log->contains(XMLAttributeTypeMismatch))
    {
      log->remove(XMLAttributeTypeMismatch);
      std::string message = "Spatial attribute 'translateZ' from the "
        "<csgTranslation> element must be an integer.";
      log->logPackageError("spatial",
        SpatialCSGTranslationTranslateZMustBeDouble, pkgVersion, level, version,
          message, getLine(), getColumn());
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
CSGTranslation::writeAttributes(XMLOutputStream& stream) const
{
  CSGTransformation::writeAttributes(stream);

  if (isSetTranslateX() == true)
  {
    stream.writeAttribute("translateX", getPrefix(), mTranslateX);
  }

  if (isSetTranslateY() == true)
  {
    stream.writeAttribute("translateY", getPrefix(), mTranslateY);
  }

  if (isSetTranslateZ() == true)
  {
    stream.writeAttribute("translateZ", getPrefix(), mTranslateZ);
  }

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new CSGTranslation_t using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
LIBSBML_EXTERN
CSGTranslation_t *
CSGTranslation_create(unsigned int level,
                      unsigned int version,
                      unsigned int pkgVersion)
{
  return new CSGTranslation(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this CSGTranslation_t object.
 */
LIBSBML_EXTERN
CSGTranslation_t*
CSGTranslation_clone(const CSGTranslation_t* csgt)
{
  if (csgt != NULL)
  {
    return static_cast<CSGTranslation_t*>(csgt->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this CSGTranslation_t object.
 */
LIBSBML_EXTERN
void
CSGTranslation_free(CSGTranslation_t* csgt)
{
  if (csgt != NULL)
  {
    delete csgt;
  }
}


/*
 * Returns the value of the "translateX" attribute of this CSGTranslation_t.
 */
LIBSBML_EXTERN
double
CSGTranslation_getTranslateX(const CSGTranslation_t * csgt)
{
  return (csgt != NULL) ? csgt->getTranslateX() : util_NaN();
}


/*
 * Returns the value of the "translateY" attribute of this CSGTranslation_t.
 */
LIBSBML_EXTERN
double
CSGTranslation_getTranslateY(const CSGTranslation_t * csgt)
{
  return (csgt != NULL) ? csgt->getTranslateY() : util_NaN();
}


/*
 * Returns the value of the "translateZ" attribute of this CSGTranslation_t.
 */
LIBSBML_EXTERN
double
CSGTranslation_getTranslateZ(const CSGTranslation_t * csgt)
{
  return (csgt != NULL) ? csgt->getTranslateZ() : util_NaN();
}


/*
 * Predicate returning @c 1 (true) if this CSGTranslation_t's "translateX"
 * attribute is set.
 */
LIBSBML_EXTERN
int
CSGTranslation_isSetTranslateX(const CSGTranslation_t * csgt)
{
  return (csgt != NULL) ? static_cast<int>(csgt->isSetTranslateX()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this CSGTranslation_t's "translateY"
 * attribute is set.
 */
LIBSBML_EXTERN
int
CSGTranslation_isSetTranslateY(const CSGTranslation_t * csgt)
{
  return (csgt != NULL) ? static_cast<int>(csgt->isSetTranslateY()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this CSGTranslation_t's "translateZ"
 * attribute is set.
 */
LIBSBML_EXTERN
int
CSGTranslation_isSetTranslateZ(const CSGTranslation_t * csgt)
{
  return (csgt != NULL) ? static_cast<int>(csgt->isSetTranslateZ()) : 0;
}


/*
 * Sets the value of the "translateX" attribute of this CSGTranslation_t.
 */
LIBSBML_EXTERN
int
CSGTranslation_setTranslateX(CSGTranslation_t * csgt, double translateX)
{
  return (csgt != NULL) ? csgt->setTranslateX(translateX) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "translateY" attribute of this CSGTranslation_t.
 */
LIBSBML_EXTERN
int
CSGTranslation_setTranslateY(CSGTranslation_t * csgt, double translateY)
{
  return (csgt != NULL) ? csgt->setTranslateY(translateY) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "translateZ" attribute of this CSGTranslation_t.
 */
LIBSBML_EXTERN
int
CSGTranslation_setTranslateZ(CSGTranslation_t * csgt, double translateZ)
{
  return (csgt != NULL) ? csgt->setTranslateZ(translateZ) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "translateX" attribute of this CSGTranslation_t.
 */
LIBSBML_EXTERN
int
CSGTranslation_unsetTranslateX(CSGTranslation_t * csgt)
{
  return (csgt != NULL) ? csgt->unsetTranslateX() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "translateY" attribute of this CSGTranslation_t.
 */
LIBSBML_EXTERN
int
CSGTranslation_unsetTranslateY(CSGTranslation_t * csgt)
{
  return (csgt != NULL) ? csgt->unsetTranslateY() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "translateZ" attribute of this CSGTranslation_t.
 */
LIBSBML_EXTERN
int
CSGTranslation_unsetTranslateZ(CSGTranslation_t * csgt)
{
  return (csgt != NULL) ? csgt->unsetTranslateZ() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * CSGTranslation_t object have been set.
 */
LIBSBML_EXTERN
int
CSGTranslation_hasRequiredAttributes(const CSGTranslation_t * csgt)
{
  return (csgt != NULL) ? static_cast<int>(csgt->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


