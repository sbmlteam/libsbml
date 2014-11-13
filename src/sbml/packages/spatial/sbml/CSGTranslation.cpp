/**
 * @file:   CSGTranslation.cpp
 * @brief:  Implementation of the CSGTranslation class
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


#include <sbml/packages/spatial/sbml/CSGTranslation.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new CSGTranslation with the given level, version, and package version.
 */
CSGTranslation::CSGTranslation (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : CSGTransformation(level, version)
  , mTranslateX (numeric_limits<double>::quiet_NaN())
  , mIsSetTranslateX (false)
  , mTranslateY (numeric_limits<double>::quiet_NaN())
  , mIsSetTranslateY (false)
  , mTranslateZ (numeric_limits<double>::quiet_NaN())
  , mIsSetTranslateZ (false)
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new CSGTranslation with the given SpatialPkgNamespaces object.
 */
CSGTranslation::CSGTranslation (SpatialPkgNamespaces* spatialns)
  : CSGTransformation(spatialns)
  , mTranslateX (numeric_limits<double>::quiet_NaN())
  , mIsSetTranslateX (false)
  , mTranslateY (numeric_limits<double>::quiet_NaN())
  , mIsSetTranslateY (false)
  , mTranslateZ (numeric_limits<double>::quiet_NaN())
  , mIsSetTranslateZ (false)
{
  // set the element namespace of this object
  setElementNamespace(spatialns->getURI());

  // load package extensions bound with this object (if any) 
  loadPlugins(spatialns);
}


/*
 * Copy constructor for CSGTranslation.
 */
CSGTranslation::CSGTranslation (const CSGTranslation& orig)
  : CSGTransformation(orig)
{
  if (&orig == NULL)
  {
    throw SBMLConstructorException("Null argument to copy constructor");
  }
  else
  {
    mTranslateX  = orig.mTranslateX;
    mIsSetTranslateX  = orig.mIsSetTranslateX;
    mTranslateY  = orig.mTranslateY;
    mIsSetTranslateY  = orig.mIsSetTranslateY;
    mTranslateZ  = orig.mTranslateZ;
    mIsSetTranslateZ  = orig.mIsSetTranslateZ;
  }
}


/*
 * Assignment for CSGTranslation.
 */
CSGTranslation&
CSGTranslation::operator=(const CSGTranslation& rhs)
{
  if (&rhs == NULL)
  {
    throw SBMLConstructorException("Null argument to assignment");
  }
  else if (&rhs != this)
  {
    CSGTransformation::operator=(rhs);
    mTranslateX  = rhs.mTranslateX;
    mIsSetTranslateX  = rhs.mIsSetTranslateX;
    mTranslateY  = rhs.mTranslateY;
    mIsSetTranslateY  = rhs.mIsSetTranslateY;
    mTranslateZ  = rhs.mTranslateZ;
    mIsSetTranslateZ  = rhs.mIsSetTranslateZ;
  }
  return *this;
}


/*
 * Clone for CSGTranslation.
 */
CSGTranslation*
CSGTranslation::clone () const
{
  return new CSGTranslation(*this);
}


/*
 * Destructor for CSGTranslation.
 */
CSGTranslation::~CSGTranslation ()
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
 * Returns true/false if translateX is set.
 */
bool
CSGTranslation::isSetTranslateX() const
{
  return mIsSetTranslateX;
}


/*
 * Returns true/false if translateY is set.
 */
bool
CSGTranslation::isSetTranslateY() const
{
  return mIsSetTranslateY;
}


/*
 * Returns true/false if translateZ is set.
 */
bool
CSGTranslation::isSetTranslateZ() const
{
  return mIsSetTranslateZ;
}


/*
 * Sets translateX and returns value indicating success.
 */
int
CSGTranslation::setTranslateX(double translateX)
{
  mTranslateX = translateX;
  mIsSetTranslateX = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets translateY and returns value indicating success.
 */
int
CSGTranslation::setTranslateY(double translateY)
{
  mTranslateY = translateY;
  mIsSetTranslateY = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets translateZ and returns value indicating success.
 */
int
CSGTranslation::setTranslateZ(double translateZ)
{
  mTranslateZ = translateZ;
  mIsSetTranslateZ = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets translateX and returns value indicating success.
 */
int
CSGTranslation::unsetTranslateX()
{
  mTranslateX = numeric_limits<double>::quiet_NaN();
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
 * Unsets translateY and returns value indicating success.
 */
int
CSGTranslation::unsetTranslateY()
{
  mTranslateY = numeric_limits<double>::quiet_NaN();
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
 * Unsets translateZ and returns value indicating success.
 */
int
CSGTranslation::unsetTranslateZ()
{
  mTranslateZ = numeric_limits<double>::quiet_NaN();
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
 * Returns the XML element name of this object
 */
const std::string&
CSGTranslation::getElementName () const
{
  static const string name = "csgTranslation";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
CSGTranslation::getTypeCode () const
{
  return SBML_SPATIAL_CSGTRANSLATION;
}


/*
 * check if all the required attributes are set
 */
bool
CSGTranslation::hasRequiredAttributes () const
{
  bool allPresent = CSGTransformation::hasRequiredAttributes();

  if (isSetTranslateX() == false)
    allPresent = false;

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
CSGTranslation::writeElements (XMLOutputStream& stream) const
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
CSGTranslation::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument.
 */
void
CSGTranslation::setSBMLDocument (SBMLDocument* d)
{
  CSGTransformation::setSBMLDocument(d);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
CSGTranslation::enablePackageInternal(const std::string& pkgURI,
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
CSGTranslation::createObject(XMLInputStream& stream)
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
CSGTranslation::addExpectedAttributes(ExpectedAttributes& attributes)
{
  CSGTransformation::addExpectedAttributes(attributes);

  attributes.add("translateX");
  attributes.add("translateY");
  attributes.add("translateZ");
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
CSGTranslation::readAttributes (const XMLAttributes& attributes,
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
  // translateX double   ( use = "required" )
  //
  numErrs = getErrorLog()->getNumErrors();
  mIsSetTranslateX = attributes.readInto("translateX", mTranslateX);

  if (mIsSetTranslateX == false)
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
        std::string message = "Spatial attribute 'translateX' is missing from 'csgTranslation' object.";
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
      }
    }
  }

  //
  // translateY double   ( use = "optional" )
  //
  numErrs = getErrorLog()->getNumErrors();
  mIsSetTranslateY = attributes.readInto("translateY", mTranslateY);

  if (mIsSetTranslateY == false)
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
  // translateZ double   ( use = "optional" )
  //
  numErrs = getErrorLog()->getNumErrors();
  mIsSetTranslateZ = attributes.readInto("translateZ", mTranslateZ);

  if (mIsSetTranslateZ == false)
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
CSGTranslation::writeAttributes (XMLOutputStream& stream) const
{
  CSGTransformation::writeAttributes(stream);

  if (isSetTranslateX() == true)
    stream.writeAttribute("translateX", getPrefix(), mTranslateX);

  if (isSetTranslateY() == true)
    stream.writeAttribute("translateY", getPrefix(), mTranslateY);

  if (isSetTranslateZ() == true)
    stream.writeAttribute("translateZ", getPrefix(), mTranslateZ);

}


  /** @endcond doxygenLibsbmlInternal */


LIBSBML_EXTERN
CSGTranslation_t *
CSGTranslation_create(unsigned int level, unsigned int version,
                      unsigned int pkgVersion)
{
  return new CSGTranslation(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
CSGTranslation_free(CSGTranslation_t * csgt)
{
  if (csgt != NULL)
    delete csgt;
}


LIBSBML_EXTERN
CSGTranslation_t *
CSGTranslation_clone(CSGTranslation_t * csgt)
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


LIBSBML_EXTERN
double
CSGTranslation_getTranslateX(const CSGTranslation_t * csgt)
{
	return (csgt != NULL) ? csgt->getTranslateX() : numeric_limits<double>::quiet_NaN();
}


LIBSBML_EXTERN
double
CSGTranslation_getTranslateY(const CSGTranslation_t * csgt)
{
	return (csgt != NULL) ? csgt->getTranslateY() : numeric_limits<double>::quiet_NaN();
}


LIBSBML_EXTERN
double
CSGTranslation_getTranslateZ(const CSGTranslation_t * csgt)
{
	return (csgt != NULL) ? csgt->getTranslateZ() : numeric_limits<double>::quiet_NaN();
}


LIBSBML_EXTERN
int
CSGTranslation_isSetTranslateX(const CSGTranslation_t * csgt)
{
  return (csgt != NULL) ? static_cast<int>(csgt->isSetTranslateX()) : 0;
}


LIBSBML_EXTERN
int
CSGTranslation_isSetTranslateY(const CSGTranslation_t * csgt)
{
  return (csgt != NULL) ? static_cast<int>(csgt->isSetTranslateY()) : 0;
}


LIBSBML_EXTERN
int
CSGTranslation_isSetTranslateZ(const CSGTranslation_t * csgt)
{
  return (csgt != NULL) ? static_cast<int>(csgt->isSetTranslateZ()) : 0;
}


LIBSBML_EXTERN
int
CSGTranslation_setTranslateX(CSGTranslation_t * csgt, double translateX)
{
  if (csgt != NULL)
    return csgt->setTranslateX(translateX);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CSGTranslation_setTranslateY(CSGTranslation_t * csgt, double translateY)
{
  if (csgt != NULL)
    return csgt->setTranslateY(translateY);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CSGTranslation_setTranslateZ(CSGTranslation_t * csgt, double translateZ)
{
  if (csgt != NULL)
    return csgt->setTranslateZ(translateZ);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CSGTranslation_unsetTranslateX(CSGTranslation_t * csgt)
{
  return (csgt != NULL) ? csgt->unsetTranslateX() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CSGTranslation_unsetTranslateY(CSGTranslation_t * csgt)
{
  return (csgt != NULL) ? csgt->unsetTranslateY() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CSGTranslation_unsetTranslateZ(CSGTranslation_t * csgt)
{
  return (csgt != NULL) ? csgt->unsetTranslateZ() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CSGTranslation_hasRequiredAttributes(const CSGTranslation_t * csgt)
{
  return (csgt != NULL) ? static_cast<int>(csgt->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


