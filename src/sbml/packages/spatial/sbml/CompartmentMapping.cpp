/**
 * @file:   CompartmentMapping.cpp
 * @brief:  Implementation of the CompartmentMapping class
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


#include <sbml/packages/spatial/sbml/CompartmentMapping.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new CompartmentMapping with the given level, version, and package version.
 */
CompartmentMapping::CompartmentMapping (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : SBase(level, version)
  , mId ("")
  , mDomainType ("")
  , mUnitSize (numeric_limits<double>::quiet_NaN())
  , mIsSetUnitSize (false)
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new CompartmentMapping with the given SpatialPkgNamespaces object.
 */
CompartmentMapping::CompartmentMapping (SpatialPkgNamespaces* spatialns)
  : SBase(spatialns)
  , mId ("")
  , mDomainType ("")
  , mUnitSize (numeric_limits<double>::quiet_NaN())
  , mIsSetUnitSize (false)
{
  // set the element namespace of this object
  setElementNamespace(spatialns->getURI());

  // load package extensions bound with this object (if any) 
  loadPlugins(spatialns);
}


/*
 * Copy constructor for CompartmentMapping.
 */
CompartmentMapping::CompartmentMapping (const CompartmentMapping& orig)
  : SBase(orig)
{
  if (&orig == NULL)
  {
    throw SBMLConstructorException("Null argument to copy constructor");
  }
  else
  {
    mId  = orig.mId;
    mDomainType  = orig.mDomainType;
    mUnitSize  = orig.mUnitSize;
    mIsSetUnitSize  = orig.mIsSetUnitSize;
  }
}


/*
 * Assignment for CompartmentMapping.
 */
CompartmentMapping&
CompartmentMapping::operator=(const CompartmentMapping& rhs)
{
  if (&rhs == NULL)
  {
    throw SBMLConstructorException("Null argument to assignment");
  }
  else if (&rhs != this)
  {
    SBase::operator=(rhs);
    mId  = rhs.mId;
    mDomainType  = rhs.mDomainType;
    mUnitSize  = rhs.mUnitSize;
    mIsSetUnitSize  = rhs.mIsSetUnitSize;
  }
  return *this;
}


/*
 * Clone for CompartmentMapping.
 */
CompartmentMapping*
CompartmentMapping::clone () const
{
  return new CompartmentMapping(*this);
}


/*
 * Destructor for CompartmentMapping.
 */
CompartmentMapping::~CompartmentMapping ()
{
}


/*
 * Returns the value of the "id" attribute of this CompartmentMapping.
 */
const std::string&
CompartmentMapping::getId() const
{
  return mId;
}


/*
 * Returns the value of the "domainType" attribute of this CompartmentMapping.
 */
const std::string&
CompartmentMapping::getDomainType() const
{
  return mDomainType;
}


/*
 * Returns the value of the "unitSize" attribute of this CompartmentMapping.
 */
double
CompartmentMapping::getUnitSize() const
{
  return mUnitSize;
}


/*
 * Returns true/false if id is set.
 */
bool
CompartmentMapping::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Returns true/false if domainType is set.
 */
bool
CompartmentMapping::isSetDomainType() const
{
  return (mDomainType.empty() == false);
}


/*
 * Returns true/false if unitSize is set.
 */
bool
CompartmentMapping::isSetUnitSize() const
{
  return mIsSetUnitSize;
}


/*
 * Sets id and returns value indicating success.
 */
int
CompartmentMapping::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets domainType and returns value indicating success.
 */
int
CompartmentMapping::setDomainType(const std::string& domainType)
{
  if (&(domainType) == NULL)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else if (!(SyntaxChecker::isValidInternalSId(domainType)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mDomainType = domainType;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets unitSize and returns value indicating success.
 */
int
CompartmentMapping::setUnitSize(double unitSize)
{
  mUnitSize = unitSize;
  mIsSetUnitSize = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets id and returns value indicating success.
 */
int
CompartmentMapping::unsetId()
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
 * Unsets domainType and returns value indicating success.
 */
int
CompartmentMapping::unsetDomainType()
{
  mDomainType.erase();

  if (mDomainType.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets unitSize and returns value indicating success.
 */
int
CompartmentMapping::unsetUnitSize()
{
  mUnitSize = numeric_limits<double>::quiet_NaN();
  mIsSetUnitSize = false;

  if (isSetUnitSize() == false)
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
CompartmentMapping::renameSIdRefs(const std::string& oldid, const std::string& newid)
{
  if (isSetDomainType() == true && mDomainType == oldid)
  {
    setDomainType(newid);
  }

}


/*
 * Returns the XML element name of this object
 */
const std::string&
CompartmentMapping::getElementName () const
{
  static const string name = "compartmentMapping";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
CompartmentMapping::getTypeCode () const
{
  return SBML_SPATIAL_COMPARTMENTMAPPING;
}


/*
 * check if all the required attributes are set
 */
bool
CompartmentMapping::hasRequiredAttributes () const
{
  bool allPresent = true;

  if (isSetId() == false)
    allPresent = false;

  if (isSetDomainType() == false)
    allPresent = false;

  if (isSetUnitSize() == false)
    allPresent = false;

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
CompartmentMapping::writeElements (XMLOutputStream& stream) const
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
CompartmentMapping::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument.
 */
void
CompartmentMapping::setSBMLDocument (SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
CompartmentMapping::enablePackageInternal(const std::string& pkgURI,
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
CompartmentMapping::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("id");
  attributes.add("domainType");
  attributes.add("unitSize");
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
CompartmentMapping::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  unsigned int numErrs;

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
      logEmptyString(mId, getLevel(), getVersion(), "<CompartmentMapping>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mId) == false && getErrorLog() != NULL)
    {
      getErrorLog()->logError(InvalidIdSyntax, getLevel(), getVersion(), 
        "The syntax of the attribute id='" + mId + "' does not conform.", getLine(), getColumn());
    }
  }
  else
  {
    std::string message = "Spatial attribute 'id' is missing from 'compartmentMapping' object.";
    getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                   getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
  }

  //
  // domainType SIdRef   ( use = "required" )
  //
  assigned = attributes.readInto("domainType", mDomainType);

  if (assigned == true)
  {
    // check string is not empty and correct syntax

    if (mDomainType.empty() == true)
    {
      logEmptyString(mDomainType, getLevel(), getVersion(), "<CompartmentMapping>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mDomainType) == false && getErrorLog() != NULL)
    {
      getErrorLog()->logError(InvalidIdSyntax, getLevel(), getVersion(), 
        "The syntax of the attribute domainType='" + mDomainType + "' does not conform.");
    }
  }
  else
  {
    std::string message = "Spatial attribute 'domainType' is missing from 'compartmentMapping' object.";
    getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                   getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
  }

  //
  // unitSize double   ( use = "required" )
  //
  numErrs = getErrorLog()->getNumErrors();
  mIsSetUnitSize = attributes.readInto("unitSize", mUnitSize);

  if (mIsSetUnitSize == false)
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
        std::string message = "Spatial attribute 'unitSize' is missing from 'compartmentMapping' object.";
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
CompartmentMapping::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetId() == true)
    stream.writeAttribute("id", getPrefix(), mId);

  if (isSetDomainType() == true)
    stream.writeAttribute("domainType", getPrefix(), mDomainType);

  if (isSetUnitSize() == true)
    stream.writeAttribute("unitSize", getPrefix(), mUnitSize);

}


  /** @endcond doxygenLibsbmlInternal */


LIBSBML_EXTERN
CompartmentMapping_t *
CompartmentMapping_create(unsigned int level, unsigned int version,
                          unsigned int pkgVersion)
{
  return new CompartmentMapping(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
CompartmentMapping_free(CompartmentMapping_t * cm)
{
  if (cm != NULL)
    delete cm;
}


LIBSBML_EXTERN
CompartmentMapping_t *
CompartmentMapping_clone(CompartmentMapping_t * cm)
{
  if (cm != NULL)
  {
    return static_cast<CompartmentMapping_t*>(cm->clone());
  }
  else
  {
    return NULL;
  }
}


LIBSBML_EXTERN
const char *
CompartmentMapping_getId(const CompartmentMapping_t * cm)
{
	return (cm != NULL && cm->isSetId()) ? cm->getId().c_str() : NULL;
}


LIBSBML_EXTERN
const char *
CompartmentMapping_getDomainType(const CompartmentMapping_t * cm)
{
	return (cm != NULL && cm->isSetDomainType()) ? cm->getDomainType().c_str() : NULL;
}


LIBSBML_EXTERN
double
CompartmentMapping_getUnitSize(const CompartmentMapping_t * cm)
{
	return (cm != NULL) ? cm->getUnitSize() : numeric_limits<double>::quiet_NaN();
}


LIBSBML_EXTERN
int
CompartmentMapping_isSetId(const CompartmentMapping_t * cm)
{
  return (cm != NULL) ? static_cast<int>(cm->isSetId()) : 0;
}


LIBSBML_EXTERN
int
CompartmentMapping_isSetDomainType(const CompartmentMapping_t * cm)
{
  return (cm != NULL) ? static_cast<int>(cm->isSetDomainType()) : 0;
}


LIBSBML_EXTERN
int
CompartmentMapping_isSetUnitSize(const CompartmentMapping_t * cm)
{
  return (cm != NULL) ? static_cast<int>(cm->isSetUnitSize()) : 0;
}


LIBSBML_EXTERN
int
CompartmentMapping_setId(CompartmentMapping_t * cm, const char * id)
{
  if (cm != NULL)
    return (id == NULL) ? cm->setId("") : cm->setId(id);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CompartmentMapping_setDomainType(CompartmentMapping_t * cm, const char * domainType)
{
  if (cm != NULL)
    return (domainType == NULL) ? cm->setDomainType("") : cm->setDomainType(domainType);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CompartmentMapping_setUnitSize(CompartmentMapping_t * cm, double unitSize)
{
  if (cm != NULL)
    return cm->setUnitSize(unitSize);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CompartmentMapping_unsetId(CompartmentMapping_t * cm)
{
  return (cm != NULL) ? cm->unsetId() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CompartmentMapping_unsetDomainType(CompartmentMapping_t * cm)
{
  return (cm != NULL) ? cm->unsetDomainType() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CompartmentMapping_unsetUnitSize(CompartmentMapping_t * cm)
{
  return (cm != NULL) ? cm->unsetUnitSize() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
CompartmentMapping_hasRequiredAttributes(const CompartmentMapping_t * cm)
{
  return (cm != NULL) ? static_cast<int>(cm->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


