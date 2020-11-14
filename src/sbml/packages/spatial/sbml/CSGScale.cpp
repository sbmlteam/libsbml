/**
 * @file CSGScale.cpp
 * @brief Implementation of the CSGScale class.
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
#include <sbml/packages/spatial/sbml/CSGScale.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new CSGScale using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
CSGScale::CSGScale(unsigned int level,
                   unsigned int version,
                   unsigned int pkgVersion)
  : CSGTransformation(level, version, pkgVersion)
  , mScaleX (util_NaN())
  , mIsSetScaleX (false)
  , mScaleY (util_NaN())
  , mIsSetScaleY (false)
  , mScaleZ (util_NaN())
  , mIsSetScaleZ (false)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version,
    pkgVersion));
}


/*
 * Creates a new CSGScale using the given SpatialPkgNamespaces object.
 */
CSGScale::CSGScale(SpatialPkgNamespaces *spatialns)
  : CSGTransformation(spatialns)
  , mScaleX (util_NaN())
  , mIsSetScaleX (false)
  , mScaleY (util_NaN())
  , mIsSetScaleY (false)
  , mScaleZ (util_NaN())
  , mIsSetScaleZ (false)
{
  setElementNamespace(spatialns->getURI());
  loadPlugins(spatialns);
}


/*
 * Copy constructor for CSGScale.
 */
CSGScale::CSGScale(const CSGScale& orig)
  : CSGTransformation( orig )
  , mScaleX ( orig.mScaleX )
  , mIsSetScaleX ( orig.mIsSetScaleX )
  , mScaleY ( orig.mScaleY )
  , mIsSetScaleY ( orig.mIsSetScaleY )
  , mScaleZ ( orig.mScaleZ )
  , mIsSetScaleZ ( orig.mIsSetScaleZ )
{
}


/*
 * Assignment operator for CSGScale.
 */
CSGScale&
CSGScale::operator=(const CSGScale& rhs)
{
  if (&rhs != this)
  {
    CSGTransformation::operator=(rhs);
    mScaleX = rhs.mScaleX;
    mIsSetScaleX = rhs.mIsSetScaleX;
    mScaleY = rhs.mScaleY;
    mIsSetScaleY = rhs.mIsSetScaleY;
    mScaleZ = rhs.mScaleZ;
    mIsSetScaleZ = rhs.mIsSetScaleZ;
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this CSGScale object.
 */
CSGScale*
CSGScale::clone() const
{
  return new CSGScale(*this);
}


/*
 * Destructor for CSGScale.
 */
CSGScale::~CSGScale()
{
}


/*
 * Returns the value of the "scaleX" attribute of this CSGScale.
 */
double
CSGScale::getScaleX() const
{
  return mScaleX;
}


/*
 * Returns the value of the "scaleY" attribute of this CSGScale.
 */
double
CSGScale::getScaleY() const
{
  return mScaleY;
}


/*
 * Returns the value of the "scaleZ" attribute of this CSGScale.
 */
double
CSGScale::getScaleZ() const
{
  return mScaleZ;
}


/*
 * Predicate returning @c true if this CSGScale's "scaleX" attribute is set.
 */
bool
CSGScale::isSetScaleX() const
{
  return mIsSetScaleX;
}


/*
 * Predicate returning @c true if this CSGScale's "scaleY" attribute is set.
 */
bool
CSGScale::isSetScaleY() const
{
  return mIsSetScaleY;
}


/*
 * Predicate returning @c true if this CSGScale's "scaleZ" attribute is set.
 */
bool
CSGScale::isSetScaleZ() const
{
  return mIsSetScaleZ;
}


/*
 * Sets the value of the "scaleX" attribute of this CSGScale.
 */
int
CSGScale::setScaleX(double scaleX)
{
  mScaleX = scaleX;
  mIsSetScaleX = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "scaleY" attribute of this CSGScale.
 */
int
CSGScale::setScaleY(double scaleY)
{
  mScaleY = scaleY;
  mIsSetScaleY = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "scaleZ" attribute of this CSGScale.
 */
int
CSGScale::setScaleZ(double scaleZ)
{
  mScaleZ = scaleZ;
  mIsSetScaleZ = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "scaleX" attribute of this CSGScale.
 */
int
CSGScale::unsetScaleX()
{
  mScaleX = util_NaN();
  mIsSetScaleX = false;

  if (isSetScaleX() == false)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "scaleY" attribute of this CSGScale.
 */
int
CSGScale::unsetScaleY()
{
  mScaleY = util_NaN();
  mIsSetScaleY = false;

  if (isSetScaleY() == false)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "scaleZ" attribute of this CSGScale.
 */
int
CSGScale::unsetScaleZ()
{
  mScaleZ = util_NaN();
  mIsSetScaleZ = false;

  if (isSetScaleZ() == false)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Returns the XML element name of this CSGScale object.
 */
const std::string&
CSGScale::getElementName() const
{
  static const string name = "csgScale";
  return name;
}


/*
 * Returns the libSBML type code for this CSGScale object.
 */
int
CSGScale::getTypeCode() const
{
  return SBML_SPATIAL_CSGSCALE;
}


/*
 * Predicate returning @c true if all the required attributes for this CSGScale
 * object have been set.
 */
bool
CSGScale::hasRequiredAttributes() const
{
  bool allPresent = CSGTransformation::hasRequiredAttributes();

  if (isSetScaleX() == false)
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
CSGScale::writeElements(XMLOutputStream& stream) const
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
CSGScale::accept(SBMLVisitor& v) const
{
  return v.visit(*this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
CSGScale::setSBMLDocument(SBMLDocument* d)
{
  CSGTransformation::setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
CSGScale::enablePackageInternal(const std::string& pkgURI,
                                const std::string& pkgPrefix,
                                bool flag)
{
  CSGTransformation::enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CSGScale.
 */
int
CSGScale::getAttribute(const std::string& attributeName, bool& value) const
{
  int return_value = CSGTransformation::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CSGScale.
 */
int
CSGScale::getAttribute(const std::string& attributeName, int& value) const
{
  int return_value = CSGTransformation::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CSGScale.
 */
int
CSGScale::getAttribute(const std::string& attributeName, double& value) const
{
  int return_value = CSGTransformation::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "scaleX")
  {
    value = getScaleX();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "scaleY")
  {
    value = getScaleY();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "scaleZ")
  {
    value = getScaleZ();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CSGScale.
 */
int
CSGScale::getAttribute(const std::string& attributeName,
                       unsigned int& value) const
{
  int return_value = CSGTransformation::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Gets the value of the "attributeName" attribute of this CSGScale.
 */
int
CSGScale::getAttribute(const std::string& attributeName,
                       std::string& value) const
{
  int return_value = CSGTransformation::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this CSGScale's attribute "attributeName" is
 * set.
 */
bool
CSGScale::isSetAttribute(const std::string& attributeName) const
{
  bool value = CSGTransformation::isSetAttribute(attributeName);

  if (attributeName == "scaleX")
  {
    value = isSetScaleX();
  }
  else if (attributeName == "scaleY")
  {
    value = isSetScaleY();
  }
  else if (attributeName == "scaleZ")
  {
    value = isSetScaleZ();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CSGScale.
 */
int
CSGScale::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = CSGTransformation::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CSGScale.
 */
int
CSGScale::setAttribute(const std::string& attributeName, int value)
{
  int return_value = CSGTransformation::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CSGScale.
 */
int
CSGScale::setAttribute(const std::string& attributeName, double value)
{
  int return_value = CSGTransformation::setAttribute(attributeName, value);

  if (attributeName == "scaleX")
  {
    return_value = setScaleX(value);
  }
  else if (attributeName == "scaleY")
  {
    return_value = setScaleY(value);
  }
  else if (attributeName == "scaleZ")
  {
    return_value = setScaleZ(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CSGScale.
 */
int
CSGScale::setAttribute(const std::string& attributeName, unsigned int value)
{
  int return_value = CSGTransformation::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this CSGScale.
 */
int
CSGScale::setAttribute(const std::string& attributeName,
                       const std::string& value)
{
  int return_value = CSGTransformation::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this CSGScale.
 */
int
CSGScale::unsetAttribute(const std::string& attributeName)
{
  int value = CSGTransformation::unsetAttribute(attributeName);

  if (attributeName == "scaleX")
  {
    value = unsetScaleX();
  }
  else if (attributeName == "scaleY")
  {
    value = unsetScaleY();
  }
  else if (attributeName == "scaleZ")
  {
    value = unsetScaleZ();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
CSGScale::createObject(XMLInputStream& stream)
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
CSGScale::addExpectedAttributes(ExpectedAttributes& attributes)
{
  CSGTransformation::addExpectedAttributes(attributes);

  attributes.add("scaleX");

  attributes.add("scaleY");

  attributes.add("scaleZ");
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
CSGScale::readAttributes(const XMLAttributes& attributes,
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
        log->logPackageError("spatial", SpatialCSGScaleAllowedAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("spatial", SpatialCSGScaleAllowedCoreAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
      }
    }
  }

  // 
  // scaleX double (use = "required" )
  // 

  numErrs = log->getNumErrors();
  mIsSetScaleX = attributes.readInto("scaleX", mScaleX);

  if ( mIsSetScaleX == false)
  {
    if (log->getNumErrors() == numErrs + 1 &&
      log->contains(XMLAttributeTypeMismatch))
    {
      log->remove(XMLAttributeTypeMismatch);
      std::string message = "Spatial attribute 'scaleX' from the <csgScale> "
        "element must be an integer.";
      log->logPackageError("spatial", SpatialCSGScaleScaleXMustBeDouble,
        pkgVersion, level, version, message, getLine(), getColumn());
    }
    else
    {
      std::string message = "Spatial attribute 'scaleX' is missing from the "
        "<csgScale> element.";
      log->logPackageError("spatial", SpatialCSGScaleAllowedAttributes,
        pkgVersion, level, version, message, getLine(), getColumn());
    }
  }

  // 
  // scaleY double (use = "optional" )
  // 

  numErrs = log->getNumErrors();
  mIsSetScaleY = attributes.readInto("scaleY", mScaleY);

  if ( mIsSetScaleY == false)
  {
    if (log->getNumErrors() == numErrs + 1 &&
      log->contains(XMLAttributeTypeMismatch))
    {
      log->remove(XMLAttributeTypeMismatch);
      std::string message = "Spatial attribute 'scaleY' from the <csgScale> "
        "element must be an integer.";
      log->logPackageError("spatial", SpatialCSGScaleScaleYMustBeDouble,
        pkgVersion, level, version, message, getLine(), getColumn());
    }
  }

  // 
  // scaleZ double (use = "optional" )
  // 

  numErrs = log->getNumErrors();
  mIsSetScaleZ = attributes.readInto("scaleZ", mScaleZ);

  if ( mIsSetScaleZ == false)
  {
    if (log->getNumErrors() == numErrs + 1 &&
      log->contains(XMLAttributeTypeMismatch))
    {
      log->remove(XMLAttributeTypeMismatch);
      std::string message = "Spatial attribute 'scaleZ' from the <csgScale> "
        "element must be an integer.";
      log->logPackageError("spatial", SpatialCSGScaleScaleZMustBeDouble,
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
CSGScale::writeAttributes(XMLOutputStream& stream) const
{
  CSGTransformation::writeAttributes(stream);

  if (isSetScaleX() == true)
  {
    stream.writeAttribute("scaleX", getPrefix(), mScaleX);
  }

  if (isSetScaleY() == true)
  {
    stream.writeAttribute("scaleY", getPrefix(), mScaleY);
  }

  if (isSetScaleZ() == true)
  {
    stream.writeAttribute("scaleZ", getPrefix(), mScaleZ);
  }

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new CSGScale_t using the given SBML Level, Version and
 * &ldquo;spatial&rdquo; package version.
 */
LIBSBML_EXTERN
CSGScale_t *
CSGScale_create(unsigned int level,
                unsigned int version,
                unsigned int pkgVersion)
{
  return new CSGScale(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this CSGScale_t object.
 */
LIBSBML_EXTERN
CSGScale_t*
CSGScale_clone(const CSGScale_t* csgs)
{
  if (csgs != NULL)
  {
    return static_cast<CSGScale_t*>(csgs->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this CSGScale_t object.
 */
LIBSBML_EXTERN
void
CSGScale_free(CSGScale_t* csgs)
{
  if (csgs != NULL)
  {
    delete csgs;
  }
}


/*
 * Returns the value of the "scaleX" attribute of this CSGScale_t.
 */
LIBSBML_EXTERN
double
CSGScale_getScaleX(const CSGScale_t * csgs)
{
  return (csgs != NULL) ? csgs->getScaleX() : util_NaN();
}


/*
 * Returns the value of the "scaleY" attribute of this CSGScale_t.
 */
LIBSBML_EXTERN
double
CSGScale_getScaleY(const CSGScale_t * csgs)
{
  return (csgs != NULL) ? csgs->getScaleY() : util_NaN();
}


/*
 * Returns the value of the "scaleZ" attribute of this CSGScale_t.
 */
LIBSBML_EXTERN
double
CSGScale_getScaleZ(const CSGScale_t * csgs)
{
  return (csgs != NULL) ? csgs->getScaleZ() : util_NaN();
}


/*
 * Predicate returning @c 1 (true) if this CSGScale_t's "scaleX" attribute is
 * set.
 */
LIBSBML_EXTERN
int
CSGScale_isSetScaleX(const CSGScale_t * csgs)
{
  return (csgs != NULL) ? static_cast<int>(csgs->isSetScaleX()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this CSGScale_t's "scaleY" attribute is
 * set.
 */
LIBSBML_EXTERN
int
CSGScale_isSetScaleY(const CSGScale_t * csgs)
{
  return (csgs != NULL) ? static_cast<int>(csgs->isSetScaleY()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this CSGScale_t's "scaleZ" attribute is
 * set.
 */
LIBSBML_EXTERN
int
CSGScale_isSetScaleZ(const CSGScale_t * csgs)
{
  return (csgs != NULL) ? static_cast<int>(csgs->isSetScaleZ()) : 0;
}


/*
 * Sets the value of the "scaleX" attribute of this CSGScale_t.
 */
LIBSBML_EXTERN
int
CSGScale_setScaleX(CSGScale_t * csgs, double scaleX)
{
  return (csgs != NULL) ? csgs->setScaleX(scaleX) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "scaleY" attribute of this CSGScale_t.
 */
LIBSBML_EXTERN
int
CSGScale_setScaleY(CSGScale_t * csgs, double scaleY)
{
  return (csgs != NULL) ? csgs->setScaleY(scaleY) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "scaleZ" attribute of this CSGScale_t.
 */
LIBSBML_EXTERN
int
CSGScale_setScaleZ(CSGScale_t * csgs, double scaleZ)
{
  return (csgs != NULL) ? csgs->setScaleZ(scaleZ) : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "scaleX" attribute of this CSGScale_t.
 */
LIBSBML_EXTERN
int
CSGScale_unsetScaleX(CSGScale_t * csgs)
{
  return (csgs != NULL) ? csgs->unsetScaleX() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "scaleY" attribute of this CSGScale_t.
 */
LIBSBML_EXTERN
int
CSGScale_unsetScaleY(CSGScale_t * csgs)
{
  return (csgs != NULL) ? csgs->unsetScaleY() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "scaleZ" attribute of this CSGScale_t.
 */
LIBSBML_EXTERN
int
CSGScale_unsetScaleZ(CSGScale_t * csgs)
{
  return (csgs != NULL) ? csgs->unsetScaleZ() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * CSGScale_t object have been set.
 */
LIBSBML_EXTERN
int
CSGScale_hasRequiredAttributes(const CSGScale_t * csgs)
{
  return (csgs != NULL) ? static_cast<int>(csgs->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


