/**
 * @file    CSGRotation.cpp
 * @brief   Implementation of CSGRotation, the SBase derived class of spatial package.
 * @author  
 *
 * $Id: CSGRotation.cpp $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/CSGRotation.cpp $
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

#include <sbml/packages/spatial/extension/SpatialExtension.h>

#include <sbml/SBase.h>
#include <sbml/packages/spatial/sbml/CSGRotation.h>

using namespace std;

LIBSBML_CPP_NAMESPACE_BEGIN

/*
 * Creates a new CSGRotation with the given level, version, and package version.
 */
CSGRotation::CSGRotation (unsigned int level, unsigned int version, unsigned int pkgVersion) 
		: CSGTransformation (SBML_SPATIAL_CSGROTATION, level,version)
		, mRotationAxisX(0.0)
		, mRotationAxisY(0.0)
		, mRotationAxisZ(0.0)
		, mRotationAngleInRadians(0.0)
		, mIsSetRotationAxisX (false)
		, mIsSetRotationAxisY (false)
		, mIsSetRotationAxisZ (false)
		, mIsSetRotationAngleInRadians (false)
{
  mType = SBML_SPATIAL_CSGROTATION;
  // set an SBMLNamespaces derived object (SpatialPkgNamespaces) of this package.
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level,version,pkgVersion)); 

}


/*
 * Creates a new CSGRotation with the given SpatialPkgNamespaces object.
 */
CSGRotation::CSGRotation(SpatialPkgNamespaces* spatialns)
	: CSGTransformation (SBML_SPATIAL_CSGROTATION, spatialns)
		, mRotationAxisX(0.0)
		, mRotationAxisY(0.0)
		, mRotationAxisZ(0.0)
		, mRotationAngleInRadians(0.0)
		, mIsSetRotationAxisX (false)
		, mIsSetRotationAxisY (false)
		, mIsSetRotationAxisZ (false)
		, mIsSetRotationAngleInRadians (false)
{
  mType = SBML_SPATIAL_CSGROTATION;
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
CSGRotation::CSGRotation(const CSGRotation& source) : CSGTransformation(source)
{
  if (source.mChild)
  {
	  mChild = static_cast<CSGNode*>( source.mChild->clone() );
  }
  this->mRotationAxisX=source.mRotationAxisX;
  this->mRotationAxisY=source.mRotationAxisY;
  this->mRotationAxisZ=source.mRotationAxisZ;
  this->mRotationAngleInRadians=source.mRotationAngleInRadians;
  this->mIsSetRotationAxisX=source.mIsSetRotationAxisX;
  this->mIsSetRotationAxisY=source.mIsSetRotationAxisY;
  this->mIsSetRotationAxisZ=source.mIsSetRotationAxisZ;
  this->mIsSetRotationAngleInRadians=source.mIsSetRotationAngleInRadians;

  connectToChild();

}


/*
 * Assignment operator.
 */
CSGRotation& CSGRotation::operator=(const CSGRotation& source)
{
  if(&source!=this)
  {
	this->CSGTransformation::operator=(source);
    this->mRotationAxisX=source.mRotationAxisX;
    this->mRotationAxisY=source.mRotationAxisY;
    this->mRotationAxisZ=source.mRotationAxisZ;
	this->mRotationAngleInRadians=source.mRotationAngleInRadians;
    this->mIsSetRotationAxisX=source.mIsSetRotationAxisX;
    this->mIsSetRotationAxisY=source.mIsSetRotationAxisY;
    this->mIsSetRotationAxisZ=source.mIsSetRotationAxisZ;
	this->mIsSetRotationAngleInRadians=source.mIsSetRotationAngleInRadians;
  }
  
  return *this;
}

/*
 * Destructor.
 */ 
CSGRotation::~CSGRotation ()
{
	// destroy 'mChild'
	if (mChild) delete mChild;
}


 /*
  * Returns the value of the "rotationAxisX" attribute of this CSGRotation.
  */
double 
CSGRotation::getRotationAxisX () const
{
  return mRotationAxisX;
}

 /*
  * Returns the value of the "rotationAxisY" attribute of this CSGRotation.
  */
double 
CSGRotation::getRotationAxisY () const
{
  return mRotationAxisY;
}

 /*
  * Returns the value of the "rotationAxisZ" attribute of this CSGRotation.
  */
double 
CSGRotation::getRotationAxisZ () const
{
  return mRotationAxisZ;
}

 /*
  * Returns the value of the "rotationAngleInRadians" attribute of this CSGRotation.
  */
double 
CSGRotation::getRotationAngleInRadians () const
{
	return mRotationAngleInRadians;
}


 /*
  * Predicate returning @c true or @c false depending on whether this
  * CSGRotation's "rotationAxisX" attribute has been set.
  */
bool 
CSGRotation::isSetRotationAxisX () const
{
  return mIsSetRotationAxisX;
}

/*
 * @return true if the "rotationAxisY" of this CSGRotation has been set, false
 * otherwise.
 */
bool
CSGRotation::isSetRotationAxisY () const
{
  return mIsSetRotationAxisY;
}

/*
 * @return true if the "rotationAxisZ" of this CSGRotation has been set, false
 * otherwise.
 */
bool
CSGRotation::isSetRotationAxisZ () const
{
  return mIsSetRotationAxisZ;
}

/*
 * @return true if the "rotationAngleInRadians" of this CSGRotation has been set, false
 * otherwise.
 */
bool
CSGRotation::isSetRotationAngleInRadians () const
{
  return mIsSetRotationAngleInRadians;
}

 /*
  * Sets the value of the "rotationAxisX" attribute of this CSGRotation.
  */
int 
CSGRotation::setRotationAxisX (double val)
{
  if (val < 0)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mRotationAxisX = val;
    mIsSetRotationAxisX  = true;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


 /*
  * Sets the value of the "rotationAxisY" attribute of this CSGRotation.
  */
int 
CSGRotation::setRotationAxisY (double val)
{
  if (val < 0)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mRotationAxisY = val;
    mIsSetRotationAxisY  = true;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


 /*
  * Sets the value of the "rotationAxisX" attribute of this CSGRotation.
  */
int 
CSGRotation::setRotationAxisZ (double val)
{
  if (val < 0)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mRotationAxisZ = val;
    mIsSetRotationAxisZ  = true;
    return LIBSBML_OPERATION_SUCCESS;
  }
}

 /*
  * Sets the value of the "rotationAngleInRadians" attribute of this CSGRotation.
  */
int 
CSGRotation::setRotationAngleInRadians (double val)
{
  if (val < 0)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
	mRotationAngleInRadians = val;
	mIsSetRotationAngleInRadians  = true;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
  * Unsets the value of the "rotationAxisX" attribute of this CSGRotation.
  */
int 
CSGRotation::unsetRotationAxisX ()
{
  mRotationAxisX      = numeric_limits<double>::quiet_NaN();
  mIsSetRotationAxisX = false;
  
  if (!isSetRotationAxisX())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}

/*
  * Unsets the value of the "rotationAxisY" attribute of this CSGRotation.
  */
int 
CSGRotation::unsetRotationAxisY ()
{
  mRotationAxisY      = numeric_limits<double>::quiet_NaN();
  mIsSetRotationAxisY = false;
  
  if (!isSetRotationAxisY())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}

/*
  * Unsets the value of the "rotationAxisZ" attribute of this CSGRotation.
  */
int 
CSGRotation::unsetRotationAxisZ ()
{
  mRotationAxisZ      = numeric_limits<double>::quiet_NaN();
  mIsSetRotationAxisZ = false;
  
  if (!isSetRotationAxisZ())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}

/*
  * Unsets the value of the "rotationAngleInRadians" attribute of this CSGRotation.
  */
int 
CSGRotation::unsetRotationAngleInRadians ()
{
  mRotationAngleInRadians      = numeric_limits<double>::quiet_NaN();
  mIsSetRotationAngleInRadians = false;
  
  if (!isSetRotationAngleInRadians())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}

/*
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 
SBase*
CSGRotation::createObject (XMLInputStream& stream)
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
CSGRotation::addExpectedAttributes(ExpectedAttributes& attributes)
{
  CSGTransformation::addExpectedAttributes(attributes);
  
  attributes.add("rotationAxisX");
  attributes.add("rotationAxisY");
  attributes.add("rotationAxisZ");
  attributes.add("rotationAngleInRadians");
}


/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
CSGRotation::readAttributes (const XMLAttributes& attributes,
                        const ExpectedAttributes& expectedAttributes)
{
  CSGTransformation::readAttributes(attributes,expectedAttributes);

  mIsSetRotationAxisX = attributes.readInto("rotationAxisX", mRotationAxisX, getErrorLog(), true, getLine(), getColumn());
  mIsSetRotationAxisY = attributes.readInto("rotationAxisY", mRotationAxisY, getErrorLog(), true, getLine(), getColumn());
  mIsSetRotationAxisZ = attributes.readInto("rotationAxisZ", mRotationAxisZ, getErrorLog(), true, getLine(), getColumn());
  mIsSetRotationAngleInRadians = attributes.readInto("rotationAngleInRadians", mRotationAngleInRadians, getErrorLog(), true, getLine(), getColumn());

}


/*
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.
 */
void
CSGRotation::writeAttributes (XMLOutputStream& stream) const
{
  CSGTransformation::writeAttributes(stream);

  stream.writeAttribute("rotationAxisX", getPrefix(), mRotationAxisX);
  stream.writeAttribute("rotationAxisY", getPrefix(), mRotationAxisY);
  stream.writeAttribute("rotationAxisZ", getPrefix(), mRotationAxisZ);
  stream.writeAttribute("rotationAngleInRadians", getPrefix(), mRotationAngleInRadians);

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
CSGRotation::writeElements (XMLOutputStream& stream) const
{
	CSGTransformation::writeElements(stream);

  //
  // (EXTENSION)
  //
  SBase::writeExtensionElements(stream);
}


CSGRotation*
CSGRotation::clone() const
{
    return new CSGRotation(*this);
}


/*
 * Accepts the given SBMLVisitor.
 */
bool
CSGRotation::accept (SBMLVisitor& v) const
{
  // return false;
  return v.visit(*this);
  
}

/*
 * Sets the parent SBMLDocument of this SBML object.
 */
void
CSGRotation::setSBMLDocument (SBMLDocument* d)
{
  CSGTransformation::setSBMLDocument(d);
}

/*
 * Enables/Disables the given package with this element and child
 * elements (if any).
 * (This is an internal implementation for enablePakcage function)
 */
void
CSGRotation::enablePackageInternal(const std::string& pkgURI,
                             const std::string& pkgPrefix, bool flag)
{
  CSGTransformation::enablePackageInternal(pkgURI,pkgPrefix,flag);
}

LIBSBML_CPP_NAMESPACE_END
