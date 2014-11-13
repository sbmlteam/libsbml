/**
 * @file:   CSGScale.cpp
 * @brief:  Implementation of the CSGScale class
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


#include <sbml/packages/spatial/sbml/CSGScale.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new CSGScale with the given level, version, and package version.
 */
CSGScale::CSGScale (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : CSGTransformation(level, version)
  , mScaleX (numeric_limits<double>::quiet_NaN())
  , mIsSetScaleX (false)
  , mScaleY (numeric_limits<double>::quiet_NaN())
  , mIsSetScaleY (false)
  , mScaleZ (numeric_limits<double>::quiet_NaN())
  , mIsSetScaleZ (false)
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new CSGScale with the given SpatialPkgNamespaces object.
 */
CSGScale::CSGScale (SpatialPkgNamespaces* spatialns)
  : CSGTransformation(spatialns)
  , mScaleX (numeric_limits<double>::quiet_NaN())
  , mIsSetScaleX (false)
  , mScaleY (numeric_limits<double>::quiet_NaN())
  , mIsSetScaleY (false)
  , mScaleZ (numeric_limits<double>::quiet_NaN())
  , mIsSetScaleZ (false)
{
  // set the element namespace of this object
  setElementNamespace(spatialns->getURI());

  // load package extensions bound with this object (if any) 
  loadPlugins(spatialns);
}


/*
 * Copy constructor for CSGScale.
 */
CSGScale::CSGScale (const CSGScale& orig)
  : CSGTransformation(orig)
{
  if (&orig == NULL)
  {
    throw SBMLConstructorException("Null argument to copy constructor");
  }
  else
  {
    mScaleX  = orig.mScaleX;
    mIsSetScaleX  = orig.mIsSetScaleX;
    mScaleY  = orig.mScaleY;
    mIsSetScaleY  = orig.mIsSetScaleY;
    mScaleZ  = orig.mScaleZ;
    mIsSetScaleZ  = orig.mIsSetScaleZ;
  }
}


/*
 * Assignment for CSGScale.
 */
CSGScale&
CSGScale::operator=(const CSGScale& rhs)
{
  if (&rhs == NULL)
  {
    throw SBMLConstructorException("Null argument to assignment");
  }
  else if (&rhs != this)
  {
    CSGTransformation::operator=(rhs);
    mScaleX  = rhs.mScaleX;
    mIsSetScaleX  = rhs.mIsSetScaleX;
    mScaleY  = rhs.mScaleY;
    mIsSetScaleY  = rhs.mIsSetScaleY;
    mScaleZ  = rhs.mScaleZ;
    mIsSetScaleZ  = rhs.mIsSetScaleZ;
  }
  return *this;
}


/*
 * Clone for CSGScale.
 */
CSGScale*
CSGScale::clone () const
{
  return new CSGScale(*this);
}


/*
 * Destructor for CSGScale.
 */
CSGScale::~CSGScale ()
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
 * Returns true/false if scaleX is set.
 */
bool
CSGScale::isSetScaleX() const
{
  return mIsSetScaleX;
}


/*
 * Returns true/false if scaleY is set.
 */
bool
CSGScale::isSetScaleY() const
{
  return mIsSetScaleY;
}


/*
 * Returns true/false if scaleZ is set.
 */
bool
CSGScale::isSetScaleZ() const
{
  return mIsSetScaleZ;
}


/*
 * Sets scaleX and returns value indicating success.
 */
int
CSGScale::setScaleX(double scaleX)
{
  mScaleX = scaleX;
  mIsSetScaleX = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets scaleY and returns value indicating success.
 */
int
CSGScale::setScaleY(double scaleY)
{
  mScaleY = scaleY;
  mIsSetScaleY = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets scaleZ and returns value indicating success.
 */
int
CSGScale::setScaleZ(double scaleZ)
{
  mScaleZ = scaleZ;
  mIsSetScaleZ = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets scaleX and returns value indicating success.
 */
int
CSGScale::unsetScaleX()
{
  mScaleX = numeric_limits<double>::quiet_NaN();
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
 * Unsets scaleY and returns value indicating success.
 */
int
CSGScale::unsetScaleY()
{
  mScaleY = numeric_limits<double>::quiet_NaN();
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
 * Unsets scaleZ and returns value indicating success.
 */
int
CSGScale::unsetScaleZ()
{
  mScaleZ = numeric_limits<double>::quiet_NaN();
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
 * Returns the XML element name of this object
 */
const std::string&
CSGScale::getElementName () const
{
  static const string name = "csgScale";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
CSGScale::getTypeCode () const
{
  return SBML_SPATIAL_CSGSCALE;
}


/*
 * check if all the required attributes are set
 */
bool
CSGScale::hasRequiredAttributes () const
{
  bool allPresent = CSGTransformation::hasRequiredAttributes();

  if (isSetScaleX() == false)
    allPresent = false;

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
CSGScale::writeElements (XMLOutputStream& stream) const
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
CSGScale::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument.
 */
void
CSGScale::setSBMLDocument (SBMLDocument* d)
{
  CSGTransformation::setSBMLDocument(d);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
CSGScale::enablePackageInternal(const std::string& pkgURI,
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
CSGScale::createObject(XMLInputStream& stream)
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
CSGScale::addExpectedAttributes(ExpectedAttributes& attributes)
{
  CSGTransformation::addExpectedAttributes(attributes);

  attributes.add("scaleX");
  attributes.add("scaleY");
  attributes.add("scaleZ");
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
CSGScale::readAttributes (const XMLAttributes& attributes,
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
  // scaleX double   ( use = "required" )
  //
  numErrs = getErrorLog()->getNumErrors();
  mIsSetScaleX = attributes.readInto("scaleX", mScaleX);

  if (mIsSetScaleX == false)
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
        std::string message = "Spatial attribute 'scaleX' is missing from 'csgScale' object.";
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
      }
    }
  }

  //
  // scaleY double   ( use = "optional" )
  //
  numErrs = getErrorLog()->getNumErrors();
  mIsSetScaleY = attributes.readInto("scaleY", mScaleY);

  if (mIsSetScaleY == false)
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
  // scaleZ double   ( use = "optional" )
  //
  numErrs = getErrorLog()->getNumErrors();
  mIsSetScaleZ = attributes.readInto("scaleZ", mScaleZ);

  if (mIsSetScaleZ == false)
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
CSGScale::writeAttributes (XMLOutputStream& stream) const
{
  CSGTransformation::writeAttributes(stream);

  if (isSetScaleX() == true)
    stream.writeAttribute("scaleX", getPrefix(), mScaleX);

  if (isSetScaleY() == true)
    stream.writeAttribute("scaleY", getPrefix(), mScaleY);

  if (isSetScaleZ() == true)
    stream.writeAttribute("scaleZ", getPrefix(), mScaleZ);

}


  /** @endcond doxygenLibsbmlInternal */


LIBSBML_EXTERN
CSGScale_t *
CSGScale_create(unsigned int level, unsigned int version,
                unsigned int pkgVersion)
{
  return new CSGScale(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
CSGScale_free(CSGScale_t * csgs)
{
  if (csgs != NULL)
    delete csgs;
}


LIBSBML_EXTERN
CSGScale_t *
CSGScale_clone(CSGScale_t * csgs)
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


LIBSBML_EXTERN
double
CSGScale_getScaleX(const CSGScale_t * csgs)
{
	return (csgs != NULL) ? csgs->getScaleX() : numeric_limits<double>::quiet_NaN();
}


LIBSBML_EXTERN
double
CSGScale_getScaleY(const CSGScale_t * csgs)
{
	return (csgs != NULL) ? csgs->getScaleY() : numeric_limits<double>::quiet_NaN();
}


LIBSBML_EXTERN
double
CSGScale_getScaleZ(const CSGScale_t * csgs)
{
	return (csgs != NULL) ? csgs->getScaleZ() : numeric_limits<double>::quiet_NaN();
}


LIBSBML_EXTERN
int
CSGScale_isSetScaleX(const CSGScale_t * csgs)
{
  return (csgs != NULL) ? static_cast<int>(csgs->isSetScaleX()) : 0;
}


LIBSBML_EXTERN
int
CSGScale_isSetScaleY(const CSGScale_t * csgs)
{
  return (csgs != NULL) ? static_cast<int>(csgs->isSetScaleY()) : 0;
}


LIBSBML_EXTERN
int
CSGScale_isSetScaleZ(const CSGScale_t * csgs)
{
  return (csgs != NULL) ? static_cast<int>(csgs->isSetScaleZ()) : 0;
}


LIBSBML_EXTERN
int
CSGScale_setScaleX(CSGScale_t * csgs, double scaleX)
{
  if (csgs != NULL)
    return csgs->setScaleX(scaleX);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CSGScale_setScaleY(CSGScale_t * csgs, double scaleY)
{
  if (csgs != NULL)
    return csgs->setScaleY(scaleY);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CSGScale_setScaleZ(CSGScale_t * csgs, double scaleZ)
{
  if (csgs != NULL)
    return csgs->setScaleZ(scaleZ);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CSGScale_unsetScaleX(CSGScale_t * csgs)
{
  return (csgs != NULL) ? csgs->unsetScaleX() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CSGScale_unsetScaleY(CSGScale_t * csgs)
{
  return (csgs != NULL) ? csgs->unsetScaleY() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CSGScale_unsetScaleZ(CSGScale_t * csgs)
{
  return (csgs != NULL) ? csgs->unsetScaleZ() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CSGScale_hasRequiredAttributes(const CSGScale_t * csgs)
{
  return (csgs != NULL) ? static_cast<int>(csgs->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


