/**
 * @file    SpatialPoint.cpp
 * @brief   Implementation of SpatialPoint, the SBase derived class of spatial package.
 * @author  
 *
 * $Id: SpatialPoint.cpp 10670 2010-01-16 12:10:06Z ajouraku $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/SpatialPoint.cpp $
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

#include <sbml/packages/spatial/sbml/SpatialPoint.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>

using namespace std;

LIBSBML_CPP_NAMESPACE_BEGIN

/*
 * Creates a new SpatialPoint with the given level, version, and package version.
 */
SpatialPoint::SpatialPoint (unsigned int level, unsigned int version, unsigned int pkgVersion) 
  : SBase (level,version)
   , mSpatialId("")
   , mDomain("")
   , mCoord1(0.0)
   , mCoord2 (0.0)
   , mCoord3 (0.0)
   , mIsSetCoord1 (false)
   , mIsSetCoord2 (false)
   , mIsSetCoord3 (false)

{
  // set an SBMLNamespaces derived object (SpatialPkgNamespaces) of this package.
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level,version,pkgVersion));  
}


/*
 * Creates a new SpatialPoint with the given SpatialPkgNamespaces object.
 */
SpatialPoint::SpatialPoint(SpatialPkgNamespaces* spatialns)
 : SBase(spatialns)
  , mSpatialId("")
  , mDomain("")
  , mCoord1(0.0)
  , mCoord2 (0.0)
  , mCoord3 (0.0)
  , mIsSetCoord1 (false)
  , mIsSetCoord2 (false)
  , mIsSetCoord3 (false)
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
SpatialPoint::SpatialPoint(const SpatialPoint& source) : SBase(source)
{
  this->mSpatialId=source.mSpatialId;
  this->mDomain=source.mDomain;
  this->mCoord1=source.mCoord1;
  this->mCoord2=source.mCoord2;
  this->mCoord3=source.mCoord3;
  this->mIsSetCoord1=source.mIsSetCoord1;
  this->mIsSetCoord2=source.mIsSetCoord2;
  this->mIsSetCoord3=source.mIsSetCoord3;
}

/*
 * Assignment operator.
 */
SpatialPoint& SpatialPoint::operator=(const SpatialPoint& source)
{
  if(&source!=this)
  {
    this->SBase::operator=(source);
    this->mSpatialId = source.mSpatialId;
	this->mDomain = source.mDomain;
	this->mCoord1 = source.mCoord1;
	this->mCoord2 = source.mCoord2;
	this->mCoord3 = source.mCoord3;
	this->mIsSetCoord1 = source.mIsSetCoord1;
	this->mIsSetCoord2 = source.mIsSetCoord2;
	this->mIsSetCoord3 = source.mIsSetCoord3;
  }

    return *this;
}

/*
 * Destructor.
 */ 
SpatialPoint::~SpatialPoint ()
{
}


/*
  * Returns the value of the "spatialId" attribute of this SpatialPoint.
  */
const std::string& 
SpatialPoint::getSpatialId () const
{
  return mSpatialId;
}

/*
  * Returns the value of the "domain" attribute of this SpatialPoint.
  */
const std::string& 
SpatialPoint::getDomain () const
{
  return mDomain;
}

/*
  * Returns the value of the "coord1" attribute of this SpatialPoint.
  */
double 
SpatialPoint::getCoord1 () const
{
  return mCoord1;
}

/*
 * @return value of "coord2" attribute of this SpatialPoint
 */
double
SpatialPoint::getCoord2 () const
{
  return mCoord2;
}
/*
 * @return value of "coord3" attribute of this SpatialPoint
 */
double
SpatialPoint::getCoord3 () const
{
  return mCoord3;
}

/*
  * Predicate returning @c true or @c false depending on whether this
  * SpatialPoint's "spatialId" attribute has been set.
  */
bool 
SpatialPoint::isSetSpatialId () const
{
  return (mSpatialId.empty() == false);
}

/*
  * Predicate returning @c true or @c false depending on whether this
  * SpatialPoint's "domain" attribute has been set.
  */
bool 
SpatialPoint::isSetDomain () const
{
  return (mDomain.empty() == false);
}

/*
  * Predicate returning @c true or @c false depending on whether this
  * SpatialPoint's "coord1" attribute has been set.
  */
bool 
SpatialPoint::isSetCoord1 () const
{
  return mIsSetCoord1;
}

/*
 * @return true if the "coord2" of this SpatialPoint has been set, false
 * otherwise.
 */
bool
SpatialPoint::isSetCoord2 () const
{
  return mIsSetCoord2;
}

/*
 * @return true if the "mCoord3" of this SpatialPoint has been set, false
 * otherwise.
 */
bool
SpatialPoint::isSetCoord3 () const
{
  return mIsSetCoord3;
}

/*
  * Sets the value of the "spatialId" attribute of this SpatialPoint.
  */
int 
SpatialPoint::setSpatialId (const std::string& spatialId)
{
  return SyntaxChecker::checkAndSetSId(spatialId ,mSpatialId);
}

/*
  * Sets the value of the "domain" attribute of this SpatialPoint.
  */
int 
SpatialPoint::setDomain (const std::string& domain)
{
  return SyntaxChecker::checkAndSetSId(domain ,mDomain);
}

/*
  * Sets the value of the "coord1" attribute of this SpatialPoint.
  */
int 
SpatialPoint::setCoord1 (double coord1)
{
  if (coord1 < 0)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mCoord1 = coord1;
    mIsSetCoord1  = true;
    return LIBSBML_OPERATION_SUCCESS;
  }
}

/*
 * Sets the "coord2" field of this SpatialPoint to value.
 */
int
SpatialPoint::setCoord2 (double value)
{
  mCoord2      = value;
  mIsSetCoord2 = true;
  return LIBSBML_OPERATION_SUCCESS;
}

/*
 * Sets the "coord3" field of this SpatialPoint to value.
 */
int
SpatialPoint::setCoord3 (double value)
{
  mCoord3      = value;
  mIsSetCoord3 = true;
  return LIBSBML_OPERATION_SUCCESS;
}

/*
  * Unsets the value of the "spatialId" attribute of this SpatialPoint.
  */
int 
SpatialPoint::unsetSpatialId ()
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
  * Unsets the value of the "domain" attribute of this SpatialPoint.
  */
int 
SpatialPoint::unsetDomain ()
{
  mDomain.erase();
  if (mDomain.empty())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}

/*
  * Unsets the value of the "coord1" attribute of this SpatialPoint.
  */
int 
SpatialPoint::unsetCoord1 ()
{
  mCoord1      = numeric_limits<double>::quiet_NaN();
  mIsSetCoord1 = false;
  
  if (!isSetCoord1())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}

/*
 * Unsets the "coord2" of this SpatialPoint.
 */
int
SpatialPoint::unsetCoord2 ()
{
  mCoord2      = numeric_limits<double>::quiet_NaN();
  mIsSetCoord2 = false;
  
  if (!isSetCoord2())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}

/*
 * Unsets the "coord3" of this SpatialPoint.
 */
int
SpatialPoint::unsetCoord3 ()
{
  mCoord3      = numeric_limits<double>::quiet_NaN();
  mIsSetCoord3 = false;
  
  if (!isSetCoord3())
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
SpatialPoint::getElementName () const
{
  static const std::string name = "spatialPoint";
  return name;
}


/*
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
SpatialPoint::createObject (XMLInputStream& stream)
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
SpatialPoint::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("spatialId");
  attributes.add("domain");
  attributes.add("coord1");
  attributes.add("coord2");
  attributes.add("coord3");
}

/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
SpatialPoint::readAttributes (const XMLAttributes& attributes,
                        const ExpectedAttributes& expectedAttributes)
{
  SBase::readAttributes(attributes,expectedAttributes);

  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  bool assigned = attributes.readInto("spatialId", mSpatialId, getErrorLog(), true, getLine(), getColumn());
  if (assigned && mSpatialId.empty())
  {
    logEmptyString(mSpatialId, sbmlLevel, sbmlVersion, "<SpatialPoint>");
  }
  if (!SyntaxChecker::isValidSBMLSId(mSpatialId)) logError(InvalidIdSyntax);

  assigned = attributes.readInto("domain", mDomain, getErrorLog(), true, getLine(), getColumn());
  if (assigned && mDomain.empty())
  {
    logEmptyString(mDomain, sbmlLevel, sbmlVersion, "<SpatialPoint>");
  }
  if (!SyntaxChecker::isValidSBMLSId(mDomain)) logError(InvalidIdSyntax);

  mIsSetCoord1 = attributes.readInto("coord1", mCoord1, getErrorLog(), true, getLine(), getColumn());
  mIsSetCoord2 = attributes.readInto("coord2", mCoord2, getErrorLog(), true, getLine(), getColumn());
  mIsSetCoord3 = attributes.readInto("coord3", mCoord3, getErrorLog(), true, getLine(), getColumn());

}

/*
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.
 */
void
SpatialPoint::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  stream.writeAttribute("spatialId", getPrefix(), mSpatialId);
  stream.writeAttribute("domain", getPrefix(), mDomain);  
  stream.writeAttribute("coord1", getPrefix(), mCoord1);
  stream.writeAttribute("coord2", getPrefix(), mCoord2);
  stream.writeAttribute("coord3", getPrefix(), mCoord3);

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
SpatialPoint::writeElements (XMLOutputStream& stream) const
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
SpatialPoint::getTypeCode () const
{
	return SBML_SPATIAL_SPATIALPOINT;
}

SpatialPoint*
SpatialPoint::clone() const
{
    return new SpatialPoint(*this);
}


/*
 * Accepts the given SBMLVisitor.
 */
bool
SpatialPoint::accept (SBMLVisitor& v) const
{
  // return false;
  return v.visit(*this);
}


/*
 * Ctor.
 */
ListOfSpatialPoints::ListOfSpatialPoints(SpatialPkgNamespaces* spatialns)
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
ListOfSpatialPoints::ListOfSpatialPoints(unsigned int level, unsigned int version, unsigned int pkgVersion)
 : ListOf(level,version)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level,version,pkgVersion));
};


/*
 * @return a (deep) copy of this ListOfUnitDefinitions.
 */
ListOfSpatialPoints*
ListOfSpatialPoints::clone () const
{
  return new ListOfSpatialPoints(*this);
}

/**
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
ListOfSpatialPoints::createObject (XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase*        object = 0;


  if (name == "spatialPoint")
  {
      SPATIAL_CREATE_NS(spatialns, this->getSBMLNamespaces());
	  object = new SpatialPoint(spatialns);
	  appendAndOwn(object);
      //mItems.push_back(object);
  }

  return object;
}


/* return nth item in list */
SpatialPoint *
ListOfSpatialPoints::get(unsigned int n)
{
  return static_cast<SpatialPoint*>(ListOf::get(n));
}


/* return nth item in list */
const SpatialPoint *
ListOfSpatialPoints::get(unsigned int n) const
{
  return static_cast<const SpatialPoint*>(ListOf::get(n));
}


/* return item by spatialId */
SpatialPoint*
ListOfSpatialPoints::get (const std::string& spatialId)
{
  return const_cast<SpatialPoint*>( 
    static_cast<const ListOfSpatialPoints&>(*this).get(spatialId) );
}


/* return item by spatialId */
const SpatialPoint*
ListOfSpatialPoints::get (const std::string& spatialId) const
{
  vector<SBase*>::const_iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<SpatialPoint>(spatialId) );
  return (result == mItems.end()) ? 0 : static_cast <SpatialPoint*> (*result);
}


/* Removes the nth item from this list */
SpatialPoint*
ListOfSpatialPoints::remove (unsigned int n)
{
   return static_cast<SpatialPoint*>(ListOf::remove(n));
}


/* Removes item in this list by spatialId */
SpatialPoint*
ListOfSpatialPoints::remove (const std::string& spatialId)
{
  SBase* item = 0;
  vector<SBase*>::iterator result;

  result = find_if( mItems.begin(), mItems.end(), IdEq<SpatialPoint>(spatialId) );

  if (result != mItems.end())
  {
    item = *result;
    mItems.erase(result);
  }

  return static_cast <SpatialPoint*> (item);
}


/*
 * @return the typecode (int) of SBML objects contained in this ListOf or
 * SBML_UNKNOWN (default).
 */
int
ListOfSpatialPoints::getItemTypeCode () const
{
	return SBML_SPATIAL_SPATIALPOINT;
}


/*
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const std::string&
ListOfSpatialPoints::getElementName () const
{
  static const std::string name = "listOfSpatialPoints";
  return name;
}


LIBSBML_CPP_NAMESPACE_END

