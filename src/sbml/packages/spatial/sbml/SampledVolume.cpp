/**
 * @file:   SampledVolume.cpp
 * @brief:  Implementation of the SampledVolume class
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


#include <sbml/packages/spatial/sbml/SampledVolume.h>
#include <sbml/packages/spatial/validator/SpatialSBMLError.h>
#include <sbml/util/ElementFilter.h>


using namespace std;


LIBSBML_CPP_NAMESPACE_BEGIN


/*
 * Creates a new SampledVolume with the given level, version, and package version.
 */
SampledVolume::SampledVolume (unsigned int level, unsigned int version, unsigned int pkgVersion)
  : SBase(level, version)
  , mId ("")
  , mDomainType ("")
  , mSampledValue (numeric_limits<double>::quiet_NaN())
  , mIsSetSampledValue (false)
  , mMinValue (numeric_limits<double>::quiet_NaN())
  , mIsSetMinValue (false)
  , mMaxValue (numeric_limits<double>::quiet_NaN())
  , mIsSetMaxValue (false)
{
  // set an SBMLNamespaces derived object of this package
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version, pkgVersion));
}


/*
 * Creates a new SampledVolume with the given SpatialPkgNamespaces object.
 */
SampledVolume::SampledVolume (SpatialPkgNamespaces* spatialns)
  : SBase(spatialns)
  , mId ("")
  , mDomainType ("")
  , mSampledValue (numeric_limits<double>::quiet_NaN())
  , mIsSetSampledValue (false)
  , mMinValue (numeric_limits<double>::quiet_NaN())
  , mIsSetMinValue (false)
  , mMaxValue (numeric_limits<double>::quiet_NaN())
  , mIsSetMaxValue (false)
{
  // set the element namespace of this object
  setElementNamespace(spatialns->getURI());

  // load package extensions bound with this object (if any) 
  loadPlugins(spatialns);
}


/*
 * Copy constructor for SampledVolume.
 */
SampledVolume::SampledVolume (const SampledVolume& orig)
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
    mSampledValue  = orig.mSampledValue;
    mIsSetSampledValue  = orig.mIsSetSampledValue;
    mMinValue  = orig.mMinValue;
    mIsSetMinValue  = orig.mIsSetMinValue;
    mMaxValue  = orig.mMaxValue;
    mIsSetMaxValue  = orig.mIsSetMaxValue;
  }
}


/*
 * Assignment for SampledVolume.
 */
SampledVolume&
SampledVolume::operator=(const SampledVolume& rhs)
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
    mSampledValue  = rhs.mSampledValue;
    mIsSetSampledValue  = rhs.mIsSetSampledValue;
    mMinValue  = rhs.mMinValue;
    mIsSetMinValue  = rhs.mIsSetMinValue;
    mMaxValue  = rhs.mMaxValue;
    mIsSetMaxValue  = rhs.mIsSetMaxValue;
  }
  return *this;
}


/*
 * Clone for SampledVolume.
 */
SampledVolume*
SampledVolume::clone () const
{
  return new SampledVolume(*this);
}


/*
 * Destructor for SampledVolume.
 */
SampledVolume::~SampledVolume ()
{
}


/*
 * Returns the value of the "id" attribute of this SampledVolume.
 */
const std::string&
SampledVolume::getId() const
{
  return mId;
}


/*
 * Returns the value of the "domainType" attribute of this SampledVolume.
 */
const std::string&
SampledVolume::getDomainType() const
{
  return mDomainType;
}


/*
 * Returns the value of the "sampledValue" attribute of this SampledVolume.
 */
double
SampledVolume::getSampledValue() const
{
  return mSampledValue;
}


/*
 * Returns the value of the "minValue" attribute of this SampledVolume.
 */
double
SampledVolume::getMinValue() const
{
  return mMinValue;
}


/*
 * Returns the value of the "maxValue" attribute of this SampledVolume.
 */
double
SampledVolume::getMaxValue() const
{
  return mMaxValue;
}


/*
 * Returns true/false if id is set.
 */
bool
SampledVolume::isSetId() const
{
  return (mId.empty() == false);
}


/*
 * Returns true/false if domainType is set.
 */
bool
SampledVolume::isSetDomainType() const
{
  return (mDomainType.empty() == false);
}


/*
 * Returns true/false if sampledValue is set.
 */
bool
SampledVolume::isSetSampledValue() const
{
  return mIsSetSampledValue;
}


/*
 * Returns true/false if minValue is set.
 */
bool
SampledVolume::isSetMinValue() const
{
  return mIsSetMinValue;
}


/*
 * Returns true/false if maxValue is set.
 */
bool
SampledVolume::isSetMaxValue() const
{
  return mIsSetMaxValue;
}


/*
 * Sets id and returns value indicating success.
 */
int
SampledVolume::setId(const std::string& id)
{
  return SyntaxChecker::checkAndSetSId(id, mId);
}


/*
 * Sets domainType and returns value indicating success.
 */
int
SampledVolume::setDomainType(const std::string& domainType)
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
 * Sets sampledValue and returns value indicating success.
 */
int
SampledVolume::setSampledValue(double sampledValue)
{
  mSampledValue = sampledValue;
  mIsSetSampledValue = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets minValue and returns value indicating success.
 */
int
SampledVolume::setMinValue(double minValue)
{
  mMinValue = minValue;
  mIsSetMinValue = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets maxValue and returns value indicating success.
 */
int
SampledVolume::setMaxValue(double maxValue)
{
  mMaxValue = maxValue;
  mIsSetMaxValue = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets id and returns value indicating success.
 */
int
SampledVolume::unsetId()
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
SampledVolume::unsetDomainType()
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
 * Unsets sampledValue and returns value indicating success.
 */
int
SampledVolume::unsetSampledValue()
{
  mSampledValue = numeric_limits<double>::quiet_NaN();
  mIsSetSampledValue = false;

  if (isSetSampledValue() == false)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets minValue and returns value indicating success.
 */
int
SampledVolume::unsetMinValue()
{
  mMinValue = numeric_limits<double>::quiet_NaN();
  mIsSetMinValue = false;

  if (isSetMinValue() == false)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets maxValue and returns value indicating success.
 */
int
SampledVolume::unsetMaxValue()
{
  mMaxValue = numeric_limits<double>::quiet_NaN();
  mIsSetMaxValue = false;

  if (isSetMaxValue() == false)
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
SampledVolume::renameSIdRefs(const std::string& oldid, const std::string& newid)
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
SampledVolume::getElementName () const
{
  static const string name = "sampledVolume";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
SampledVolume::getTypeCode () const
{
  return SBML_SPATIAL_SAMPLEDVOLUME;
}


/*
 * check if all the required attributes are set
 */
bool
SampledVolume::hasRequiredAttributes () const
{
  bool allPresent = true;

  if (isSetId() == false)
    allPresent = false;

  if (isSetDomainType() == false)
    allPresent = false;

  if (isSetSampledValue() == false)
    allPresent = false;

  return allPresent;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * write contained elements
 */
void
SampledVolume::writeElements (XMLOutputStream& stream) const
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
SampledVolume::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument.
 */
void
SampledVolume::setSBMLDocument (SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Enables/Disables the given package with this element.
 */
void
SampledVolume::enablePackageInternal(const std::string& pkgURI,
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
SampledVolume::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("id");
  attributes.add("domainType");
  attributes.add("sampledValue");
  attributes.add("minValue");
  attributes.add("maxValue");
}


  /** @endcond doxygenLibsbmlInternal */


  /** @cond doxygenLibsbmlInternal */

/*
 * Read values from the given XMLAttributes set into their specific fields.
 */
void
SampledVolume::readAttributes (const XMLAttributes& attributes,
                             const ExpectedAttributes& expectedAttributes)
{
  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  unsigned int numErrs;

  /* look to see whether an unknown attribute error was logged
   * during the read of the listOfSampledVolumes - which will have
   * happened immediately prior to this read
  */

  if (getErrorLog() != NULL &&
      static_cast<ListOfSampledVolumes*>(getParentSBMLObject())->size() < 2)
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
      logEmptyString(mId, getLevel(), getVersion(), "<SampledVolume>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mId) == false && getErrorLog() != NULL)
    {
      getErrorLog()->logError(InvalidIdSyntax, getLevel(), getVersion(), 
        "The syntax of the attribute id='" + mId + "' does not conform.", getLine(), getColumn());
    }
  }
  else
  {
    std::string message = "Spatial attribute 'id' is missing from 'sampledVolume' object.";
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
      logEmptyString(mDomainType, getLevel(), getVersion(), "<SampledVolume>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mDomainType) == false && getErrorLog() != NULL)
    {
      getErrorLog()->logError(InvalidIdSyntax, getLevel(), getVersion(), 
        "The syntax of the attribute domainType='" + mDomainType + "' does not conform.");
    }
  }
  else
  {
    std::string message = "Spatial attribute 'domainType' is missing from 'sampledVolume' object.";
    getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                   getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
  }

  //
  // sampledValue double   ( use = "required" )
  //
  numErrs = getErrorLog()->getNumErrors();
  mIsSetSampledValue = attributes.readInto("sampledValue", mSampledValue);

  if (mIsSetSampledValue == false)
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
        std::string message = "Spatial attribute 'sampledValue' is missing from 'sampledVolume' object.";
        getErrorLog()->logPackageError("spatial", SpatialUnknownError,
                       getPackageVersion(), sbmlLevel, sbmlVersion, message, getLine(), getColumn());
      }
    }
  }

  //
  // minValue double   ( use = "optional" )
  //
  numErrs = getErrorLog()->getNumErrors();
  mIsSetMinValue = attributes.readInto("minValue", mMinValue);

  if (mIsSetMinValue == false)
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
  // maxValue double   ( use = "optional" )
  //
  numErrs = getErrorLog()->getNumErrors();
  mIsSetMaxValue = attributes.readInto("maxValue", mMaxValue);

  if (mIsSetMaxValue == false)
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
SampledVolume::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetId() == true)
    stream.writeAttribute("id", getPrefix(), mId);

  if (isSetDomainType() == true)
    stream.writeAttribute("domainType", getPrefix(), mDomainType);

  if (isSetSampledValue() == true)
    stream.writeAttribute("sampledValue", getPrefix(), mSampledValue);

  if (isSetMinValue() == true)
    stream.writeAttribute("minValue", getPrefix(), mMinValue);

  if (isSetMaxValue() == true)
    stream.writeAttribute("maxValue", getPrefix(), mMaxValue);

}


  /** @endcond doxygenLibsbmlInternal */


/*
 * Constructor 
 */
ListOfSampledVolumes::ListOfSampledVolumes(unsigned int level, 
                       unsigned int version, 
                       unsigned int pkgVersion)
 : ListOf(level, version)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level, version, pkgVersion)); 
}


/*
 * Constructor 
 */
ListOfSampledVolumes::ListOfSampledVolumes(SpatialPkgNamespaces* spatialns)
  : ListOf(spatialns)
{
  setElementNamespace(spatialns->getURI());
}


/*
 * Returns a deep copy of this ListOfSampledVolumes 
 */
ListOfSampledVolumes* 
ListOfSampledVolumes::clone () const
 {
  return new ListOfSampledVolumes(*this);
}


/*
 * Get a SampledVolume from the ListOfSampledVolumes by index.
*/
SampledVolume*
ListOfSampledVolumes::get(unsigned int n)
{
  return static_cast<SampledVolume*>(ListOf::get(n));
}


/*
 * Get a SampledVolume from the ListOfSampledVolumes by index.
 */
const SampledVolume*
ListOfSampledVolumes::get(unsigned int n) const
{
  return static_cast<const SampledVolume*>(ListOf::get(n));
}


/*
 * Get a SampledVolume from the ListOfSampledVolumes by id.
 */
SampledVolume*
ListOfSampledVolumes::get(const std::string& sid)
{
	return const_cast<SampledVolume*>(
    static_cast<const ListOfSampledVolumes&>(*this).get(sid));
}


/*
 * Get a SampledVolume from the ListOfSampledVolumes by id.
 */
const SampledVolume*
ListOfSampledVolumes::get(const std::string& sid) const
{
  vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<SampledVolume>(sid) );
  return (result == mItems.end()) ? 0 : static_cast <SampledVolume*> (*result);
}


/**
 * Adds a copy the given "SampledVolume" to this ListOfSampledVolumes.
 *
 * @param sv; the SampledVolume object to add
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_INVALID_ATTRIBUTE_VALUE
 */
int
ListOfSampledVolumes::addSampledVolume(const SampledVolume* sv)
{
  if (sv == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (sv->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != sv->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != sv->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const SBase *>(sv)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
	append(sv);
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/**
 * Get the number of SampledVolume objects in this ListOfSampledVolumes.
 *
 * @return the number of SampledVolume objects in this ListOfSampledVolumes
 */
unsigned int 
ListOfSampledVolumes::getNumSampledVolumes() const
{
	return size();
}

/**
 * Creates a new SampledVolume object, adds it to this ListOfSampledVolumes
 * SampledVolume and returns the SampledVolume object created. 
 *
 * @return a new SampledVolume object instance
 *
 * @see addSampledVolume(const SampledVolume* sv)
 */
SampledVolume* 
ListOfSampledVolumes::createSampledVolume()
{
  SampledVolume* sv = NULL;

  try
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    sv = new SampledVolume(spatialns);
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

  if(sv != NULL)
  {
    appendAndOwn(sv);
  }

  return sv;
}

/*
 * Removes the nth SampledVolume from this ListOfSampledVolumes
 */
SampledVolume*
ListOfSampledVolumes::remove(unsigned int n)
{
  return static_cast<SampledVolume*>(ListOf::remove(n));
}


/*
 * Removes the SampledVolume from this ListOfSampledVolumes with the given identifier
 */
SampledVolume*
ListOfSampledVolumes::remove(const std::string& sid)
{
  SBase* item = NULL;
  vector<SBase*>::iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<SampledVolume>(sid) );

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

	return static_cast <SampledVolume*> (item);
}


/*
 * Returns the XML element name of this object
 */
const std::string&
ListOfSampledVolumes::getElementName () const
{
  static const string name = "listOfSampledVolumes";
  return name;
}


/*
 * Returns the libSBML type code for this SBML object.
 */
int
ListOfSampledVolumes::getTypeCode () const
{
  return SBML_LIST_OF;
}


/*
 * Returns the libSBML type code for the objects in this LIST_OF.
 */
int
ListOfSampledVolumes::getItemTypeCode () const
{
  return SBML_SPATIAL_SAMPLEDVOLUME;
}


  /** @cond doxygenLibsbmlInternal */

/*
 * Creates a new SampledVolume in this ListOfSampledVolumes
 */
SBase*
ListOfSampledVolumes::createObject(XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase* object = NULL;

  if (name == "sampledVolume")
  {
    SPATIAL_CREATE_NS(spatialns, getSBMLNamespaces());
    object = new SampledVolume(spatialns);
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
ListOfSampledVolumes::writeXMLNS(XMLOutputStream& stream) const
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
SampledVolume_t *
SampledVolume_create(unsigned int level, unsigned int version,
                     unsigned int pkgVersion)
{
  return new SampledVolume(level, version, pkgVersion);
}


LIBSBML_EXTERN
void
SampledVolume_free(SampledVolume_t * sv)
{
  if (sv != NULL)
    delete sv;
}


LIBSBML_EXTERN
SampledVolume_t *
SampledVolume_clone(SampledVolume_t * sv)
{
  if (sv != NULL)
  {
    return static_cast<SampledVolume_t*>(sv->clone());
  }
  else
  {
    return NULL;
  }
}


LIBSBML_EXTERN
const char *
SampledVolume_getId(const SampledVolume_t * sv)
{
	return (sv != NULL && sv->isSetId()) ? sv->getId().c_str() : NULL;
}


LIBSBML_EXTERN
const char *
SampledVolume_getDomainType(const SampledVolume_t * sv)
{
	return (sv != NULL && sv->isSetDomainType()) ? sv->getDomainType().c_str() : NULL;
}


LIBSBML_EXTERN
double
SampledVolume_getSampledValue(const SampledVolume_t * sv)
{
	return (sv != NULL) ? sv->getSampledValue() : numeric_limits<double>::quiet_NaN();
}


LIBSBML_EXTERN
double
SampledVolume_getMinValue(const SampledVolume_t * sv)
{
	return (sv != NULL) ? sv->getMinValue() : numeric_limits<double>::quiet_NaN();
}


LIBSBML_EXTERN
double
SampledVolume_getMaxValue(const SampledVolume_t * sv)
{
	return (sv != NULL) ? sv->getMaxValue() : numeric_limits<double>::quiet_NaN();
}


LIBSBML_EXTERN
int
SampledVolume_isSetId(const SampledVolume_t * sv)
{
  return (sv != NULL) ? static_cast<int>(sv->isSetId()) : 0;
}


LIBSBML_EXTERN
int
SampledVolume_isSetDomainType(const SampledVolume_t * sv)
{
  return (sv != NULL) ? static_cast<int>(sv->isSetDomainType()) : 0;
}


LIBSBML_EXTERN
int
SampledVolume_isSetSampledValue(const SampledVolume_t * sv)
{
  return (sv != NULL) ? static_cast<int>(sv->isSetSampledValue()) : 0;
}


LIBSBML_EXTERN
int
SampledVolume_isSetMinValue(const SampledVolume_t * sv)
{
  return (sv != NULL) ? static_cast<int>(sv->isSetMinValue()) : 0;
}


LIBSBML_EXTERN
int
SampledVolume_isSetMaxValue(const SampledVolume_t * sv)
{
  return (sv != NULL) ? static_cast<int>(sv->isSetMaxValue()) : 0;
}


LIBSBML_EXTERN
int
SampledVolume_setId(SampledVolume_t * sv, const char * id)
{
  if (sv != NULL)
    return (id == NULL) ? sv->setId("") : sv->setId(id);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SampledVolume_setDomainType(SampledVolume_t * sv, const char * domainType)
{
  if (sv != NULL)
    return (domainType == NULL) ? sv->setDomainType("") : sv->setDomainType(domainType);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SampledVolume_setSampledValue(SampledVolume_t * sv, double sampledValue)
{
  if (sv != NULL)
    return sv->setSampledValue(sampledValue);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SampledVolume_setMinValue(SampledVolume_t * sv, double minValue)
{
  if (sv != NULL)
    return sv->setMinValue(minValue);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SampledVolume_setMaxValue(SampledVolume_t * sv, double maxValue)
{
  if (sv != NULL)
    return sv->setMaxValue(maxValue);
  else
    return LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SampledVolume_unsetId(SampledVolume_t * sv)
{
  return (sv != NULL) ? sv->unsetId() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SampledVolume_unsetDomainType(SampledVolume_t * sv)
{
  return (sv != NULL) ? sv->unsetDomainType() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SampledVolume_unsetSampledValue(SampledVolume_t * sv)
{
  return (sv != NULL) ? sv->unsetSampledValue() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SampledVolume_unsetMinValue(SampledVolume_t * sv)
{
  return (sv != NULL) ? sv->unsetMinValue() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SampledVolume_unsetMaxValue(SampledVolume_t * sv)
{
  return (sv != NULL) ? sv->unsetMaxValue() : LIBSBML_INVALID_OBJECT;
}


LIBSBML_EXTERN
int
SampledVolume_hasRequiredAttributes(const SampledVolume_t * sv)
{
  return (sv != NULL) ? static_cast<int>(sv->hasRequiredAttributes()) : 0;
}


/*
 *
 */
LIBSBML_EXTERN
SampledVolume_t *
ListOfSampledVolumes_getById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfSampledVolumes *>(lo)->get(sid) : NULL;
}


/*
 *
 */
LIBSBML_EXTERN
SampledVolume_t *
ListOfSampledVolumes_removeById(ListOf_t * lo, const char * sid)
{
  if (lo == NULL)
    return NULL;

  return (sid != NULL) ? static_cast <ListOfSampledVolumes *>(lo)->remove(sid) : NULL;
}




LIBSBML_CPP_NAMESPACE_END


