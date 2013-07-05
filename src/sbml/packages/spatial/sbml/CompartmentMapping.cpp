/**
 * @file    CompartmentMapping.cpp
 * @brief   Implementation of CompartmentMapping, the SBase derived class of spatial package.
 * @author  
 *
 * $Id: CompartmentMapping.cpp 10670 2010-01-16 12:10:06Z ajouraku $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/CompartmentMapping.cpp $
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

#include <sbml/packages/spatial/sbml/CompartmentMapping.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>

using namespace std;

LIBSBML_CPP_NAMESPACE_BEGIN

/*
 * Creates a new CompartmentMapping with the given level, version, and package version.
 */
CompartmentMapping::CompartmentMapping (unsigned int level, unsigned int version, unsigned int pkgVersion) 
  : SBase (level,version)
   , mSpatialId("")
   , mCompartment("")
   , mDomainType("")
   , mUnitSize (0.0)
   , mIsSetUnitSize (false)
{
  // set an SBMLNamespaces derived object (SpatialPkgNamespaces) of this package.
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level,version,pkgVersion));  
}


/*
 * Creates a new CompartmentMapping with the given SpatialPkgNamespaces object.
 */
CompartmentMapping::CompartmentMapping(SpatialPkgNamespaces* spatialns)
 : SBase(spatialns)
  , mSpatialId("")
  , mCompartment("")
  , mDomainType("")
  , mUnitSize (0.0)
  , mIsSetUnitSize (false)
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
CompartmentMapping::CompartmentMapping(const CompartmentMapping& source) : SBase(source)
{
  this->mSpatialId=source.mSpatialId;
  this->mDomainType=source.mDomainType;
  this->mCompartment=source.mCompartment;
  this->mUnitSize=source.mUnitSize;
  this->mIsSetUnitSize=source.mIsSetUnitSize;
}

/*
 * Assignment operator.
 */
CompartmentMapping& CompartmentMapping::operator=(const CompartmentMapping& source)
{
  if(&source!=this)
  {
    this->SBase::operator=(source);
    this->mSpatialId = source.mSpatialId;
	this->mDomainType = source.mDomainType;
	this->mCompartment = source.mCompartment;
	this->mUnitSize=source.mUnitSize;
    this->mIsSetUnitSize=source.mIsSetUnitSize;
  }
  
  return *this;
}

/*
 * Destructor.
 */ 
CompartmentMapping::~CompartmentMapping ()
{
}

/*
  * Returns the value of the "spatialId" attribute of this CompartmentMapping.
  */
const std::string& 
CompartmentMapping::getSpatialId () const
{
  return mSpatialId;
}

/*
  * Returns the value of the "domainType" attribute of this CompartmentMapping.
  */
const std::string& 
CompartmentMapping::getDomainType () const
{
  return mDomainType;
}

/*
  * Returns the value of the "compartment" attribute of this CompartmentMapping.
  */
const std::string& 
CompartmentMapping::getCompartment () const
{
  return mCompartment;
}

/*
 * @return value of "unitSize" attribute of this CompartmentMapping
 */
double
CompartmentMapping::getUnitSize () const
{
  return mUnitSize;
}

/*
  * Predicate returning @c true or @c false depending on whether this
  * CompartmentMapping's "spatialId" attribute has been set.
  */
bool 
CompartmentMapping::isSetSpatialId () const
{
  return (mSpatialId.empty() == false);
}

/*
  * Predicate returning @c true or @c false depending on whether this
  * CompartmentMapping's "domainType" attribute has been set.
  */
bool 
CompartmentMapping::isSetDomainType () const
{
  return (mDomainType.empty() == false);
}

/*
  * Predicate returning @c true or @c false depending on whether this
  * CompartmentMapping's "compartment" attribute has been set.
  */
bool 
CompartmentMapping::isSetCompartment () const
{
  return (mCompartment.empty() == false);
}

/*
 * @return true if the "unitSize" of this CompartmentMapping has been set, false
 * otherwise.
 */
bool
CompartmentMapping::isSetUnitSize () const
{
  return mIsSetUnitSize;
}

/*
  * Sets the value of the "spatialId" attribute of this CompartmentMapping.
  */
int 
CompartmentMapping::setSpatialId (const std::string& spatialId)
{
  return SyntaxChecker::checkAndSetSId(spatialId ,mSpatialId);
}

/*
  * Sets the value of the "domainType" attribute of this CompartmentMapping.
  */
int 
CompartmentMapping::setDomainType (const std::string& domainType)
{
  return SyntaxChecker::checkAndSetSId(domainType ,mDomainType);
}

/*
  * Sets the value of the "compartment" attribute of this CompartmentMapping.
  */
int 
CompartmentMapping::setCompartment (const std::string& compartment)
{
  return SyntaxChecker::checkAndSetSId(compartment ,mCompartment);
}

/*
 * Sets the "unitSize" field of this CompartmentMapping to value.
 */
int
CompartmentMapping::setUnitSize (double value)
{
  mUnitSize      = value;
  mIsSetUnitSize = true;
  return LIBSBML_OPERATION_SUCCESS;
}

/*
  * Unsets the value of the "spatialId" attribute of this CompartmentMapping.
  */
int 
CompartmentMapping::unsetSpatialId ()
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
  * Unsets the value of the "domainType" attribute of this CompartmentMapping.
  */
int 
CompartmentMapping::unsetDomainType ()
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
  * Unsets the value of the "compartment" attribute of this CompartmentMapping.
  */
int 
CompartmentMapping::unsetCompartment ()
{
  mCompartment.erase();
  if (mCompartment.empty())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}

/*
 * Unsets the "unitSize" of this CompartmentMapping.
 */
int
CompartmentMapping::unsetUnitSize ()
{
  mUnitSize      = numeric_limits<double>::quiet_NaN();
  mIsSetUnitSize = false;
  
  if (!isSetUnitSize())
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
CompartmentMapping::getElementName () const
{
  static const std::string name = "compartmentMapping";
  return name;
}


/*
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
CompartmentMapping::createObject (XMLInputStream& stream)
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
CompartmentMapping::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("spatialId");
  attributes.add("compartment");
  attributes.add("domainType");
  attributes.add("implicit");
}


/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
CompartmentMapping::readAttributes (const XMLAttributes& attributes,
                        const ExpectedAttributes& expectedAttributes)
{
  SBase::readAttributes(attributes,expectedAttributes);

  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  bool assigned = attributes.readInto("spatialId", mSpatialId, getErrorLog(), true, getLine(), getColumn());
  if (assigned && mSpatialId.empty())
  {
    logEmptyString(mSpatialId, sbmlLevel, sbmlVersion, "<CompartmentMapping>");
  }
  if (!SyntaxChecker::isValidSBMLSId(mSpatialId)) logError(InvalidIdSyntax);

  assigned = attributes.readInto("compartment", mCompartment, getErrorLog(), true, getLine(), getColumn());
  if (assigned && mCompartment.empty())
  {
    logEmptyString(mCompartment, sbmlLevel, sbmlVersion, "<CompartmentMapping>");
  }
  if (!SyntaxChecker::isValidSBMLSId(mCompartment)) logError(InvalidIdSyntax);

  assigned = attributes.readInto("domainType", mDomainType, getErrorLog(), true, getLine(), getColumn());
  if (assigned && mDomainType.empty())
  {
    logEmptyString(mDomainType, sbmlLevel, sbmlVersion, "<CompartmentMapping>");
  }
  if (!SyntaxChecker::isValidSBMLSId(mDomainType)) logError(InvalidIdSyntax);
  
  mIsSetUnitSize = attributes.readInto("unitSize", mUnitSize, getErrorLog(), true, getLine(), getColumn());
}


/*
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.
 */
void
CompartmentMapping::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  stream.writeAttribute("spatialId",   getPrefix(), mSpatialId);
  stream.writeAttribute("compartment",   getPrefix(), mCompartment);
  stream.writeAttribute("domainType",   getPrefix(), mDomainType);  
  stream.writeAttribute("unitSize", getPrefix(), mUnitSize);


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
CompartmentMapping::writeElements (XMLOutputStream& stream) const
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
CompartmentMapping::getTypeCode () const
{
	return SBML_SPATIAL_COMPARTMENTMAPPING;
}

CompartmentMapping*
CompartmentMapping::clone() const
{
    return new CompartmentMapping(*this);
}


/*
 * Accepts the given SBMLVisitor.
 */
bool
CompartmentMapping::accept (SBMLVisitor& v) const
{
  // return false;
  return v.visit(*this);
}


/*
 * Ctor.
 */
ListOfCompartmentMappings::ListOfCompartmentMappings(SpatialPkgNamespaces* spatialns)
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
ListOfCompartmentMappings::ListOfCompartmentMappings(unsigned int level, unsigned int version, unsigned int pkgVersion)
 : ListOf(level,version)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level,version,pkgVersion));
};


/*
 * @return a (deep) copy of this ListOfCompartmentMappings.
 */
ListOfCompartmentMappings*
ListOfCompartmentMappings::clone () const
{
  return new ListOfCompartmentMappings(*this);
}

/**
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
ListOfCompartmentMappings::createObject (XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase*        object = 0;


  if (name == "compartmentMapping")
  {
      SPATIAL_CREATE_NS(spatialns, this->getSBMLNamespaces());
	  object = new CompartmentMapping(spatialns);
	  appendAndOwn(object);
      //mItems.push_back(object);
  }

  return object;
}


/* return nth item in list */
CompartmentMapping *
ListOfCompartmentMappings::get(unsigned int n)
{
  return static_cast<CompartmentMapping*>(ListOf::get(n));
}


/* return nth item in list */
const CompartmentMapping *
ListOfCompartmentMappings::get(unsigned int n) const
{
  return static_cast<const CompartmentMapping*>(ListOf::get(n));
}


/* return item by spatialId */
CompartmentMapping*
ListOfCompartmentMappings::get (const std::string& spatialId)
{
  return const_cast<CompartmentMapping*>( 
    static_cast<const ListOfCompartmentMappings&>(*this).get(spatialId) );
}


/* return item by spatialId */
const CompartmentMapping*
ListOfCompartmentMappings::get (const std::string& spatialId) const
{
  vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<CompartmentMapping>(spatialId) );
  return (result == mItems.end()) ? 0 : static_cast <CompartmentMapping*> (*result);
}


/* Removes the nth item from this list */
CompartmentMapping*
ListOfCompartmentMappings::remove (unsigned int n)
{
   return static_cast<CompartmentMapping*>(ListOf::remove(n));
}


/* Removes item in this list by spatialId */
CompartmentMapping*
ListOfCompartmentMappings::remove (const std::string& spatialId)
{
  SBase* item = 0;
  vector<SBase*>::iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<CompartmentMapping>(spatialId) );

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <CompartmentMapping*> (item);
}


/*
 * @return the typecode (int) of SBML objects contained in this ListOf or
 * SBML_UNKNOWN (default).
 */
int
ListOfCompartmentMappings::getItemTypeCode () const
{
	return SBML_SPATIAL_COMPARTMENTMAPPING;
}


/*
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const std::string&
ListOfCompartmentMappings::getElementName () const
{
  static const std::string name = "listOfCompartmentMappings";
  return name;
}


LIBSBML_CPP_NAMESPACE_END

