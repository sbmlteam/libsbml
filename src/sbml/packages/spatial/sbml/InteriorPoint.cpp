/**
 * @file    InteriorPoint.cpp
 * @brief   Implementation of InteriorPoint, the SBase derived class of spatial package.
 * @author  
 *
 * $Id: InteriorPoint.cpp 10670 2010-01-16 12:10:06Z ajouraku $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/InteriorPoint.cpp $
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

#include <sbml/packages/spatial/sbml/InteriorPoint.h>
#include <sbml/packages/spatial/extension/SpatialExtension.h>

using namespace std;

LIBSBML_CPP_NAMESPACE_BEGIN

/*
 * Creates a new InteriorPoint with the given level, version, and package version.
 */
InteriorPoint::InteriorPoint (unsigned int level, unsigned int version, unsigned int pkgVersion) 
  : SBase (level,version)
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
 * Creates a new InteriorPoint with the given SpatialPkgNamespaces object.
 */
InteriorPoint::InteriorPoint(SpatialPkgNamespaces* spatialns)
 : SBase(spatialns)
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
InteriorPoint::InteriorPoint(const InteriorPoint& source) : SBase(source)
{
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
InteriorPoint& InteriorPoint::operator=(const InteriorPoint& source)
{
  if(&source!=this)
  {
    this->SBase::operator=(source);
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
InteriorPoint::~InteriorPoint ()
{
}

/*
  * Returns the value of the "coord1" attribute of this InteriorPoint.
  */
double 
InteriorPoint::getCoord1 () const
{
  return mCoord1;
}

/*
 * @return value of "coord2" attribute of this InteriorPoint
 */
double
InteriorPoint::getCoord2 () const
{
  return mCoord2;
}
/*
 * @return value of "coord3" attribute of this InteriorPoint
 */
double
InteriorPoint::getCoord3 () const
{
  return mCoord3;
}

/*
  * Predicate returning @c true or @c false depending on whether this
  * InteriorPoint's "coord1" attribute has been set.
  */
bool 
InteriorPoint::isSetCoord1 () const
{
  return mIsSetCoord1;
}

/*
 * @return true if the "coord2" of this InteriorPoint has been set, false
 * otherwise.
 */
bool
InteriorPoint::isSetCoord2 () const
{
  return mIsSetCoord2;
}

/*
 * @return true if the "mCoord3" of this InteriorPoint has been set, false
 * otherwise.
 */
bool
InteriorPoint::isSetCoord3 () const
{
  return mIsSetCoord3;
}

/*
  * Sets the value of the "coord1" attribute of this InteriorPoint.
  */
int 
InteriorPoint::setCoord1 (double coord1)
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
 * Sets the "coord2" field of this InteriorPoint to value.
 */
int
InteriorPoint::setCoord2 (double value)
{
  mCoord2      = value;
  mIsSetCoord2 = true;
  return LIBSBML_OPERATION_SUCCESS;
}

/*
 * Sets the "coord3" field of this InteriorPoint to value.
 */
int
InteriorPoint::setCoord3 (double value)
{
  mCoord3      = value;
  mIsSetCoord3 = true;
  return LIBSBML_OPERATION_SUCCESS;
}

/*
  * Unsets the value of the "coord1" attribute of this InteriorPoint.
  */
int 
InteriorPoint::unsetCoord1 ()
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
 * Unsets the "coord2" of this InteriorPoint.
 */
int
InteriorPoint::unsetCoord2 ()
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
 * Unsets the "coord3" of this InteriorPoint.
 */
int
InteriorPoint::unsetCoord3 ()
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
InteriorPoint::getElementName () const
{
  static const std::string name = "interiorPoint";
  return name;
}

/*
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
InteriorPoint::createObject (XMLInputStream& stream)
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
InteriorPoint::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

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
InteriorPoint::readAttributes (const XMLAttributes& attributes,
                        const ExpectedAttributes& expectedAttributes)
{
  SBase::readAttributes(attributes,expectedAttributes);

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
InteriorPoint::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

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
InteriorPoint::writeElements (XMLOutputStream& stream) const
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
InteriorPoint::getTypeCode () const
{
	return SBML_SPATIAL_INTERIORPOINT;
}

InteriorPoint*
InteriorPoint::clone() const
{
    return new InteriorPoint(*this);
}


/*
 * Accepts the given SBMLVisitor.
 */
bool
InteriorPoint::accept (SBMLVisitor& v) const
{
  // return false;
  return v.visit(*this);
}


/*
 * Ctor.
 */
ListOfInteriorPoints::ListOfInteriorPoints(SpatialPkgNamespaces* spatialns)
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
ListOfInteriorPoints::ListOfInteriorPoints(unsigned int level, unsigned int version, unsigned int pkgVersion)
 : ListOf(level,version)
{
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level,version,pkgVersion));
};


/*
 * @return a (deep) copy of this ListOfUnitDefinitions.
 */
ListOfInteriorPoints*
ListOfInteriorPoints::clone () const
{
  return new ListOfInteriorPoints(*this);
}

/**
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
ListOfInteriorPoints::createObject (XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase*        object = 0;


  if (name == "interiorPoint")
  {
      SPATIAL_CREATE_NS(spatialns, this->getSBMLNamespaces());
	  object = new InteriorPoint(spatialns);
	  appendAndOwn(object);
      //mItems.push_back(object);
  }

  return object;
}


/* return nth item in list */
InteriorPoint *
ListOfInteriorPoints::get(unsigned int n)
{
  return static_cast<InteriorPoint*>(ListOf::get(n));
}


/* return nth item in list */
const InteriorPoint *
ListOfInteriorPoints::get(unsigned int n) const
{
  return static_cast<const InteriorPoint*>(ListOf::get(n));
}


/* Removes the nth item from this list */
InteriorPoint*
ListOfInteriorPoints::remove (unsigned int n)
{
   return static_cast<InteriorPoint*>(ListOf::remove(n));
}


/*
 * @return the typecode (int) of SBML objects contained in this ListOf or
 * SBML_UNKNOWN (default).
 */
int
ListOfInteriorPoints::getItemTypeCode () const
{
	return SBML_SPATIAL_INTERIORPOINT;
}


/*
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const std::string&
ListOfInteriorPoints::getElementName () const
{
  static const std::string name = "listOfInteriorPoints";
  return name;
}


LIBSBML_CPP_NAMESPACE_END

