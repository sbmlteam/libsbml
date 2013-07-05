/**
 * @file    Boundary.cpp
 * @brief   Implementation of Boundary, the SBase derived class of spatial package.
 * @author  
 *
 * $Id: Boundary.cpp 10670 2010-01-16 12:10:06Z ajouraku $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/Boundary.cpp $
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

#include <sbml/packages/spatial/sbml/Boundary.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>

using namespace std;

LIBSBML_CPP_NAMESPACE_BEGIN

/*
 * Only subclasses may create Boundary.
 * 
 * Creates a new Boundary with the given level, version, and package version.
 */
Boundary::Boundary (SBMLSpatialTypeCode_t type, unsigned int level, unsigned int version, unsigned int pkgVersion) 
  : SBase (level,version)
   , mSpatialId("")
   , mType (type)
   , mValue (0.0)
   , mIsSetValue (false)
{
  // set an SBMLNamespaces derived object (SpatialPkgNamespaces) of this package.
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level,version,pkgVersion));  
}


/*
 * Creates a new Boundary with the given SpatialPkgNamespaces object.
 */
Boundary::Boundary(SBMLSpatialTypeCode_t type, SpatialPkgNamespaces* spatialns)
 : SBase(spatialns)
  , mSpatialId("")
  , mType (type)
  , mValue (0.0)
  , mIsSetValue (false)
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
Boundary::Boundary(const Boundary& source) : SBase(source)
{
  this->mSpatialId=source.mSpatialId;
  this->mValue=source.mValue;
  this->mIsSetValue=source.mIsSetValue;
  this->mType=source.mType;
}

/*
 * Assignment operator.
 */
Boundary& Boundary::operator=(const Boundary& source)
{
  if(&source!=this)
  {
    this->SBase::operator=(source);
	this->mSpatialId = source.mSpatialId;
	this->mValue = source.mValue;
	this->mIsSetValue = source.mIsSetValue;
	this->mType = source.mType;
  }
  
  return *this;
}


/*
 * Destructor.
 */ 
Boundary::~Boundary ()
{
}

/*
  * Returns the value of the "spatialId" attribute of this Boundary.
  */
const std::string& 
Boundary::getSpatialId () const
{
  return mSpatialId;
}

/*
 * @return value of "value" attribute of this Boundary
 */
double
Boundary::getValue () const
{
  return mValue;
}

/*
  * Predicate returning @c true or @c false depending on whether this
  * Boundary's "spatialId" attribute has been set.
  */
bool 
Boundary::isSetSpatialId () const
{
  return (mSpatialId.empty() == false);
}

/*
 * @return true if the "value" of this Boundary has been set, false
 * otherwise.
 */
bool
Boundary::isSetValue () const
{
  return mIsSetValue;
}

/*
  * Sets the value of the "spatialId" attribute of this Boundary.
  */
int 
Boundary::setSpatialId (const std::string& spatialId)
{
  return SyntaxChecker::checkAndSetSId(spatialId ,mSpatialId);
}

/*
 * Sets the "value" field of this Boundary to value.
 */
int
Boundary::setValue (double value)
{
  mValue      = value;
  mIsSetValue = true;
  return LIBSBML_OPERATION_SUCCESS;
}

 /*
  * Unsets the value of the "spatialId" attribute of this Boundary.
  */
int 
Boundary::unsetSpatialId ()
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
 * Unsets the "value" of this Boundary.
 */
int
Boundary::unsetValue ()
{
  mValue      = numeric_limits<double>::quiet_NaN();
  mIsSetValue = false;
  
  if (!isSetValue())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}

/*
 * @return the type of this Boundary, BOUNDARY_TYPE_MIN or
 * BOUNDARY_TYPE_MAX
 */
BoundaryType_t 
Boundary::getType () const
{
  if (mType == SBML_SPATIAL_BOUNDARYMIN) return BOUNDARY_TYPE_MIN;
  if (mType == SBML_SPATIAL_BOUNDARYMAX) return BOUNDARY_TYPE_MAX;
  return BOUNDARY_TYPE_INVALID;
}

/*
 * @return true if this Boundary is min, false otherwise.
 */
bool
Boundary::isBoundaryMin () const
{
	return (mType == SBML_SPATIAL_BOUNDARYMIN);
}

/*
 * @return true if this Boundary is max, false otherwise.
 */
bool
Boundary::isBoundaryMax () const
{
	return (mType == SBML_SPATIAL_BOUNDARYMAX);
}

/*
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const std::string&
Boundary::getElementName () const
{
  static const std::string boundaryMin = "boundaryMin";
  static const std::string boundaryMax = "boundaryMax";
  static const std::string unknown	   = "unknownBoundaryType";

  if ( isBoundaryMin() ) {
	  return boundaryMin;
  } else if ( isBoundaryMax() ) {
	  return boundaryMax;
  }
  return unknown;
}


/*
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 
SBase*
Boundary::createObject (XMLInputStream& stream)
{
  // return 0;
  SBase*        object = 0;

  object=SBase::createObject(stream);
  
  return object;
}
*/

/*
 * Subclasses should override this method to get the list of
 * expected attributes.
 * This function is invoked from corresponding readAttributes()
 * function.
 */
void
Boundary::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);
  
  attributes.add("spatialId");
  attributes.add("value");
}


/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
Boundary::readAttributes (const XMLAttributes& attributes,
                        const ExpectedAttributes& expectedAttributes)
{
  SBase::readAttributes(attributes,expectedAttributes);

  const unsigned int sbmlLevel   = getLevel  ();
  const unsigned int sbmlVersion = getVersion();

  bool assigned = attributes.readInto("spatialId", mSpatialId, getErrorLog(), true, getLine(), getColumn());
  if (assigned && mSpatialId.empty())
  {
    logEmptyString(mSpatialId, sbmlLevel, sbmlVersion, "<Boundary>");
  }
  if (!SyntaxChecker::isValidSBMLSId(mSpatialId)) logError(InvalidIdSyntax);

  mIsSetValue = attributes.readInto("value", mValue, getErrorLog(), true, getLine(), getColumn());
}


/*
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.
 */
void
Boundary::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  stream.writeAttribute("spatialId",   getPrefix(), mSpatialId);
  stream.writeAttribute("value", getPrefix(), mValue);

  //
  // (EXTENSION)
  //
  SBase::writeExtensionAttributes(stream);
}


/*
 * @return the typecode (int) of this SBML object or SBML_UNKNOWN
 * (default).
 *
 * @see getElementName()
 */
int
Boundary::getTypeCode () const
{
	return mType;
}

Boundary*
Boundary::clone() const
{
    return new Boundary(*this);
}


/*
 * Accepts the given SBMLVisitor.
 */
bool
Boundary::accept (SBMLVisitor& v) const
{
  // return false;
  return v.visit(*this);
}

bool 
Boundary::hasRequiredAttributes() const
{
  bool allPresent = true;

  /* required attributes for Boundary:(spatialId & value) */

  return allPresent;
}

/*
 * BoundaryMin
 */ 
BoundaryMin::BoundaryMin (unsigned int level, unsigned int version, unsigned int pkgVersion) :
Boundary(SBML_SPATIAL_BOUNDARYMIN, level, version, pkgVersion)
{
  if (!hasValidLevelVersionNamespaceCombination())
    throw SBMLConstructorException();
}


BoundaryMin::BoundaryMin (SpatialPkgNamespaces* spatialns) :
Boundary(SBML_SPATIAL_BOUNDARYMIN, spatialns)
{
  if (!hasValidLevelVersionNamespaceCombination())
  {
    std::string err(getElementName());
    XMLNamespaces* xmlns = spatialns->getNamespaces();
    if (xmlns)
    {
      std::ostringstream oss;
      XMLOutputStream xos(oss);
      xos << *xmlns;
      err.append(oss.str());
    }
    throw SBMLConstructorException(err);
  }

  loadPlugins(spatialns);
}

/*
 * Destroys this BoundaryMin.
 */
BoundaryMin::~BoundaryMin ()
{
}

/*
 * @return a (deep) copy of this BoundaryMin.
 */
BoundaryMin*
BoundaryMin::clone () const
{
  return new BoundaryMin(*this);
}


/*
 * Accepts the given SBMLVisitor.
 *
 * @return the result of calling <code>v.visit()</code>, which indicates
 * whether or not the Visitor would like to visit the CoordinateComponent's next Boundary
 * (if available).
 */
bool
BoundaryMin::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}

bool 
BoundaryMin::hasRequiredAttributes() const
{
  bool allPresent = Boundary::hasRequiredAttributes();

  return allPresent;
}

/*
 * BoundaryMax
 */
BoundaryMax::BoundaryMax (unsigned int level, unsigned int version, unsigned int pkgVersion) :
Boundary(SBML_SPATIAL_BOUNDARYMAX, level, version, pkgVersion)
{
  if (!hasValidLevelVersionNamespaceCombination())
    throw SBMLConstructorException();
}


BoundaryMax::BoundaryMax (SpatialPkgNamespaces * spatialns) :
Boundary(SBML_SPATIAL_BOUNDARYMAX, spatialns)
{
  if (!hasValidLevelVersionNamespaceCombination())
  {
    std::string err(getElementName());
    XMLNamespaces* xmlns = spatialns->getNamespaces();
    if (xmlns)
    {
      std::ostringstream oss;
      XMLOutputStream xos(oss);
      xos << *xmlns;
      err.append(oss.str());
    }
    throw SBMLConstructorException(err);
  }

  loadPlugins(spatialns);
}

/*
 * Destroys this BoundaryMax.
 */
BoundaryMax::~BoundaryMax ()
{
}

/*
 * @return a (deep) copy of this BoundaryMax.
 */
BoundaryMax*
BoundaryMax::clone () const
{
  return new BoundaryMax(*this);
}


/*
 * Accepts the given SBMLVisitor.
 *
 * @return the result of calling <code>v.visit()</code>, which indicates
 * whether or not the Visitor would like to visit the CoordinateComponent's next Boundary
 * (if available).
 */
bool
BoundaryMax::accept (SBMLVisitor& v) const
{
  return v.visit(*this);
}

bool 
BoundaryMax::hasRequiredAttributes() const
{
  bool allPresent = Boundary::hasRequiredAttributes();

  return allPresent;
}

LIBSBML_CPP_NAMESPACE_END
