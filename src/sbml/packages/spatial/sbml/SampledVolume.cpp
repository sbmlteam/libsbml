/**
 * @file    SampledVolume.cpp
 * @brief   Implementation of SampledVolume, the SBase derived class of spatial package.
 * @author  
 *
 * $Id: SampledVolume.cpp 10670 2010-01-16 12:10:06Z  $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/SampledVolume.cpp $
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2009 California Institute of Technology.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *------------------------------------------------------------------------- -->
 */

#include <iostream>
#include <limits>

#include <sbml/SBMLVisitor.h>
#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLToken.h>
#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>

#include <sbml/packages/spatial/sbml/SampledVolume.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>

using namespace std;

LIBSBML_CPP_NAMESPACE_BEGIN

/*
 * Creates a new SampledVolume with the given level, version, and package version.
 */
SampledVolume::SampledVolume (unsigned int level, unsigned int version, unsigned int pkgVersion) 
  : SBase (level,version)
   , mSpatialId("")
   , mDomainType("")
   , mMinValue (0.0)
   , mSampledValue (0.0)
   , mMaxValue (0.0)
   , mIsSetMinValue (false)
   , mIsSetSampledValue (false)
   , mIsSetMaxValue (false)
{
  // set an SBMLNamespaces derived object (SpatialPkgNamespaces) of this package.
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level,version,pkgVersion));  
}


/*
 * Creates a new SampledVolume with the given SpatialPkgNamespaces object.
 */
SampledVolume::SampledVolume(SpatialPkgNamespaces* spatialns)
 : SBase(spatialns)
  , mSpatialId("")
  , mDomainType("")
  , mMinValue (0.0)
  , mSampledValue (0.0)
  , mMaxValue (0.0)
  , mIsSetMinValue (false)
  , mIsSetSampledValue (false)
  , mIsSetMaxValue (false)
{
  //
  // set the element namespace of this object
  //
  setElementNamespace(spatialns->getURI());

  // load package extensions bound with this object (if any) 
  loadPlugins(spatialns);
}


/*
 * Copy constructor.
 */
SampledVolume::SampledVolume(const SampledVolume& source) : SBase(source)
{
  this->mSpatialId=source.mSpatialId;
  this->mDomainType=source.mDomainType;
  this->mSampledValue=source.mSampledValue;
  this->mMinValue=source.mMinValue;
  this->mMaxValue=source.mMaxValue;
  this->mIsSetSampledValue=source.mIsSetSampledValue;
  this->mIsSetMinValue=source.mIsSetMinValue;
  this->mIsSetMaxValue=source.mIsSetMaxValue;
}

/*
 * Assignment operator.
 */
SampledVolume& SampledVolume::operator=(const SampledVolume& source)
{
  if(&source!=this)
  {
    this->SBase::operator=(source);
	this->mSpatialId = source.mSpatialId;
	this->mDomainType = source.mDomainType;
    this->mSampledValue = source.mSampledValue;
	this->mMinValue = source.mMinValue;
	this->mMaxValue = source.mMaxValue;
    this->mIsSetSampledValue = source.mIsSetSampledValue;
	this->mIsSetMinValue = source.mIsSetMinValue;
	this->mIsSetMaxValue = source.mIsSetMaxValue;
  }
  
  return *this;
}

/*
 * Destructor.
 */ 
SampledVolume::~SampledVolume ()
{
}

/*
  * Returns the value of the "spatialId" attribute of this SampledVolume.
  */
const std::string& 
SampledVolume::getSpatialId () const
{
  return mSpatialId;
}

/*
  * Returns the value of the "domainType" attribute of this SampledVolume.
  */
const std::string& 
SampledVolume::getDomainType () const
{
  return mDomainType;
}

/*
 * @return value of "sampledValue" attribute of this SampledVolume
 */
double
SampledVolume::getSampledValue () const
{
  return mSampledValue;
}

/*
 * @return value of "minValue" attribute of this SampledVolume
 */
double
SampledVolume::getMinValue () const
{
  return mMinValue;
}
/*
 * @return value of "maxValue" attribute of this SampledVolume
 */
double
SampledVolume::getMaxValue () const
{
  return mMaxValue;
}

/*
  * Predicate returning @c true or @c false depending on whether this
  * SampledVolume's "spatialId" attribute has been set.
  */
bool 
SampledVolume::isSetSpatialId () const
{
  return (mSpatialId.empty() == false);
}

/*
  * Predicate returning @c true or @c false depending on whether this
  * SampledVolume's "domainType" attribute has been set.
  */
bool 
SampledVolume::isSetDomainType () const
{
  return (mDomainType.empty() == false);
}

/*
 * @return true if the "sampledValue" of this SampledVolume has been set, false
 * otherwise.
 */
bool
SampledVolume::isSetSampledValue () const
{
  return mIsSetSampledValue;
}

/*
 * @return true if the "minValue" of this SampledVolume has been set, false
 * otherwise.
 */
bool
SampledVolume::isSetMinValue () const
{
  return mIsSetMinValue;
}

/*
 * @return true if the "maxValue" of this SampledVolume has been set, false
 * otherwise.
 */
bool
SampledVolume::isSetMaxValue () const
{
  return mIsSetMaxValue;
}

/*
  * Sets the value of the "spatialId" attribute of this SampledVolume.
  */
int 
SampledVolume::setSpatialId (const std::string& spatialId)
{
  return SyntaxChecker::checkAndSetSId(spatialId ,mSpatialId);
}

/*
  * Sets the value of the "domainType" attribute of this SampledVolume.
  */
int 
SampledVolume::setDomainType (const std::string& domainType)
{
  return SyntaxChecker::checkAndSetSId(domainType ,mDomainType);
}

/*
 * Sets the "sampledValue" field of this SampledVolume to value.
 */
int
SampledVolume::setSampledValue (double value)
{
  mSampledValue      = value;
  mIsSetSampledValue = true;
  return LIBSBML_OPERATION_SUCCESS;
}

/*
 * Sets the "minValue" field of this SampledVolume to value.
 */
int
SampledVolume::setMinValue (double value)
{
  mMinValue      = value;
  mIsSetMinValue = true;
  return LIBSBML_OPERATION_SUCCESS;
}

/*
 * Sets the "maxValue" field of this SampledVolume to value.
 */
int
SampledVolume::setMaxValue (double value)
{
  mMaxValue      = value;
  mIsSetMaxValue = true;
  return LIBSBML_OPERATION_SUCCESS;
}

 /*
  * Unsets the value of the "spatialId" attribute of this SampledVolume.
  */
int 
SampledVolume::unsetSpatialId ()
{
  mSpatialId.erase();
  if (mSpatialId.empty())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}

/*
  * Unsets the value of the "domainType" attribute of this SampledVolume.
  */
int 
SampledVolume::unsetDomainType ()
{
  mDomainType.erase();
  if (mDomainType.empty())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}

/*
 * Unsets the "sampledValue" of this SampledVolume.
 */
int
SampledVolume::unsetSampledValue ()
{
  mSampledValue      = numeric_limits<double>::quiet_NaN();
  mIsSetSampledValue = false;
  
  if (!isSetSampledValue())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}

/*
 * Unsets the "minValue" of this SampledVolume.
 */
int
SampledVolume::unsetMinValue ()
{
  mMinValue      = numeric_limits<double>::quiet_NaN();
  mIsSetMinValue = false;
  
  if (!isSetMinValue())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}

/*
 * Unsets the "maxValue" of this SampledVolume.
 */
int
SampledVolume::unsetMaxValue ()
{
  mMaxValue      = numeric_limits<double>::quiet_NaN();
  mIsSetMaxValue = false;
  
  if (!isSetMaxValue())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}

/*
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const std::string&
SampledVolume::getElementName () const
{
  static const std::string name = "sampledVolume";
  return name;
}


/*
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
SampledVolume::createObject (XMLInputStream& stream)
{
  // return 0;
  SBase*        object = 0;

  object=SBase::createObject(stream);
  
  return object;

}

/*
 * Subclasses should override this method to get the list of
 * expected attributes.
 * This function is invoked from corresponding readAttributes()
 * function.
 */
void
SampledVolume::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);
  
  attributes.add("spatialId");
  attributes.add("domainType");
  attributes.add("minValue");
  attributes.add("maxValue");
}

/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
SampledVolume::readAttributes (const XMLAttributes& attributes,
                        const ExpectedAttributes& expectedAttributes)
{
  SBase::readAttributes(attributes,expectedAttributes);

  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  bool assigned = attributes.readInto("spatialId", mSpatialId, getErrorLog(), true, getLine(), getColumn());
  if (assigned && mSpatialId.empty())
  {
    logEmptyString(mSpatialId, sbmlLevel, sbmlVersion, "<SampledVolume>");
  }
  if (!SyntaxChecker::isValidSBMLSId(mSpatialId)) logError(InvalidIdSyntax);

  assigned = attributes.readInto("domainType", mDomainType, getErrorLog(), true, getLine(), getColumn());
  if (assigned && mDomainType.empty())
  {
    logEmptyString(mDomainType, sbmlLevel, sbmlVersion, "<SampledVolume>");
  }
  if (!SyntaxChecker::isValidSBMLSId(mDomainType)) logError(InvalidIdSyntax);

  mIsSetSampledValue = attributes.readInto("sampledValue", mSampledValue, getErrorLog(), true, getLine(), getColumn());
  mIsSetMinValue = attributes.readInto("minValue", mMinValue, getErrorLog(), true, getLine(), getColumn());
  mIsSetMaxValue = attributes.readInto("maxValue", mMaxValue, getErrorLog(), true, getLine(), getColumn());
}


/*
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.
 */
void
SampledVolume::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  stream.writeAttribute("spatialId",   getPrefix(), mSpatialId);
  stream.writeAttribute("domainType",   getPrefix(), mDomainType);
  stream.writeAttribute("sampledValue", getPrefix(), mSampledValue);
  stream.writeAttribute("minValue", getPrefix(), mMinValue);
  stream.writeAttribute("maxValue", getPrefix(), mMaxValue);


  //
  // (EXTENSION)
  //
  SBase::writeExtensionAttributes(stream);
}


/*
 * Subclasses should override this method to write out their contained
 * SBML objects as XML elements.  Be sure to call your parents
 * implementation of this method as well.
 */
void
SampledVolume::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

  //
  // (EXTENSION)
  //
  SBase::writeExtensionElements(stream);
}


/*
 * @return the typecode (int) of this SBML object or SBML_UNKNOWN
 * (default).
 *
 * @see getElementName()
 */
int
SampledVolume::getTypeCode () const
{
	return SBML_SPATIAL_SAMPLEDVOLUME;
}

SampledVolume*
SampledVolume::clone() const
{
    return new SampledVolume(*this);
}


/*
 * Accepts the given SBMLVisitor.
 */
bool
SampledVolume::accept (SBMLVisitor& v) const
{
  // return false;
  return v.visit(*this);
}


/*
 * Ctor.
 */
ListOfSampledVolumes::ListOfSampledVolumes(SpatialPkgNamespaces* spatialns)
 : ListOf(spatialns)
{
  //
  // set the element namespace of this object
  //
  setElementNamespace(spatialns->getURI());
}


/*
 * Ctor.
 */
ListOfSampledVolumes::ListOfSampledVolumes(unsigned int level, unsigned int version, unsigned int pkgVersion)
 : ListOf(level,version)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level,version,pkgVersion));
};


/*
 * @return a (deep) copy of this ListOfSampledVolumes.
 */
ListOfSampledVolumes*
ListOfSampledVolumes::clone () const
{
  return new ListOfSampledVolumes(*this);
}

/**
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
ListOfSampledVolumes::createObject (XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase*        object = 0;


  if (name == "sampledVolume")
  {
      SPATIAL_CREATE_NS(spatialns, this->getSBMLNamespaces());
	  object = new SampledVolume(spatialns);
	  appendAndOwn(object);
      //mItems.push_back(object);
  }

  return object;
}


/* return nth item in list */
SampledVolume *
ListOfSampledVolumes::get(unsigned int n)
{
  return static_cast<SampledVolume*>(ListOf::get(n));
}


/* return nth item in list */
const SampledVolume *
ListOfSampledVolumes::get(unsigned int n) const
{
  return static_cast<const SampledVolume*>(ListOf::get(n));
}


/* return item by spatialId */
SampledVolume*
ListOfSampledVolumes::get (const std::string& spatialId)
{
  return const_cast<SampledVolume*>( 
    static_cast<const ListOfSampledVolumes&>(*this).get(spatialId) );
}


/* return item by spatialId */
const SampledVolume*
ListOfSampledVolumes::get (const std::string& spatialId) const
{
  vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<SampledVolume>(spatialId) );
  return (result == mItems.end()) ? 0 : static_cast <SampledVolume*> (*result);
}


/* Removes the nth item from this list */
SampledVolume*
ListOfSampledVolumes::remove (unsigned int n)
{
   return static_cast<SampledVolume*>(ListOf::remove(n));
}


/* Removes item in this list by spatialId */
SampledVolume*
ListOfSampledVolumes::remove (const std::string& spatialId)
{
  SBase* item = 0;
  vector<SBase*>::iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<SampledVolume>(spatialId) );

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <SampledVolume*> (item);
}


/*
 * @return the typecode (int) of SBML objects contained in this ListOf or
 * SBML_UNKNOWN (default).
 */
int
ListOfSampledVolumes::getItemTypeCode () const
{
	return SBML_SPATIAL_SAMPLEDVOLUME;
}

/*
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const std::string&
ListOfSampledVolumes::getElementName () const
{
  static const std::string name = "listOfSampledVolumes";
  return name;
}


LIBSBML_CPP_NAMESPACE_END

