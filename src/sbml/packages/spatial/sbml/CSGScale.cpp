/**
 * @file    CSGScale.cpp
 * @brief   Implementation of CSGScale, the SBase derived class of spatial package.
 * @author  
 *
 * $Id: CSGScale.cpp   $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/CSGScale.cpp $
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
#include <sbml/packages/spatial/sbml/CSGScale.h>

using namespace std;

LIBSBML_CPP_NAMESPACE_BEGIN

/*
 * Creates a new CSGScale with the given level, version, and package version.
 */
CSGScale::CSGScale (unsigned int level, unsigned int version, unsigned int pkgVersion) 
		: CSGTransformation (SBML_SPATIAL_CSGSCALE, level,version)
		, mScaleX(0.0)
		, mScaleY(0.0)
		, mScaleZ(0.0)
		, mIsSetScaleX (false)
		, mIsSetScaleY (false)
		, mIsSetScaleZ (false)
{
  mType = SBML_SPATIAL_CSGSCALE;
  // set an SBMLNamespaces derived object (SpatialPkgNamespaces) of this package.
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level,version,pkgVersion)); 
}


/*
 * Creates a new CSGScale with the given SpatialPkgNamespaces object.
 */
CSGScale::CSGScale(SpatialPkgNamespaces* spatialns)
	: CSGTransformation (SBML_SPATIAL_CSGSCALE, spatialns)
		, mScaleX(0.0)
		, mScaleY(0.0)
		, mScaleZ(0.0)
		, mIsSetScaleX (false)
		, mIsSetScaleY (false)
		, mIsSetScaleZ (false)
{
  mType = SBML_SPATIAL_CSGSCALE;
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
CSGScale::CSGScale(const CSGScale& source) : CSGTransformation(source)
{
  if (source.mChild)
  {
	  mChild = static_cast<CSGNode*>( source.mChild->clone() );
  }
  this->mScaleX=source.mScaleX;
  this->mScaleY=source.mScaleY;
  this->mScaleZ=source.mScaleZ;
  this->mIsSetScaleX=source.mIsSetScaleX;
  this->mIsSetScaleY=source.mIsSetScaleY;
  this->mIsSetScaleZ=source.mIsSetScaleZ;

  connectToChild();

}


/*
 * Assignment operator.
 */
CSGScale& CSGScale::operator=(const CSGScale& source)
{
  if(&source!=this)
  {
	this->CSGTransformation::operator=(source);
    this->mScaleX=source.mScaleX;
    this->mScaleY=source.mScaleY;
    this->mScaleZ=source.mScaleZ;
    this->mIsSetScaleX=source.mIsSetScaleX;
    this->mIsSetScaleY=source.mIsSetScaleY;
    this->mIsSetScaleZ=source.mIsSetScaleZ;
  }
  
  return *this;
}

/*
 * Destructor.
 */ 
CSGScale::~CSGScale ()
{
	// destroy 'mChild'
	if (mChild) delete mChild;
}


 /*
  * Returns the value of the "scaleX" attribute of this CSGScale.
  */
double 
CSGScale::getScaleX () const
{
  return mScaleX;
}

 /*
  * Returns the value of the "scaleY" attribute of this CSGScale.
  */
double 
CSGScale::getScaleY () const
{
  return mScaleY;
}

 /*
  * Returns the value of the "scaleZ" attribute of this CSGScale.
  */
double 
CSGScale::getScaleZ () const
{
  return mScaleZ;
}



 /*
  * Predicate returning @c true or @c false depending on whether this
  * CSGScale's "scaleX" attribute has been set.
  */
bool 
CSGScale::isSetScaleX () const
{
  return mIsSetScaleX;
}

/*
 * @return true if the "scaleY" of this CSGScale has been set, false
 * otherwise.
 */
bool
CSGScale::isSetScaleY () const
{
  return mIsSetScaleY;
}

/*
 * @return true if the "scaleZ" of this CSGScale has been set, false
 * otherwise.
 */
bool
CSGScale::isSetScaleZ () const
{
  return mIsSetScaleZ;
}

 /*
  * Sets the value of the "scaleX" attribute of this CSGScale.
  */
int 
CSGScale::setScaleX (double val)
{
  if (val < 0)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mScaleX = val;
    mIsSetScaleX  = true;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


 /*
  * Sets the value of the "scaleY" attribute of this CSGScale.
  */
int 
CSGScale::setScaleY (double val)
{
  if (val < 0)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mScaleY = val;
    mIsSetScaleY  = true;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


 /*
  * Sets the value of the "scaleX" attribute of this CSGScale.
  */
int 
CSGScale::setScaleZ (double val)
{
  if (val < 0)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mScaleZ = val;
    mIsSetScaleZ  = true;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
  * Unsets the value of the "scaleX" attribute of this CSGScale.
  */
int 
CSGScale::unsetScaleX ()
{
  mScaleX      = numeric_limits<double>::quiet_NaN();
  mIsSetScaleX = false;
  
  if (!isSetScaleX())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}

/*
  * Unsets the value of the "scaleY" attribute of this CSGScale.
  */
int 
CSGScale::unsetScaleY ()
{
  mScaleY      = numeric_limits<double>::quiet_NaN();
  mIsSetScaleY = false;
  
  if (!isSetScaleY())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}

/*
  * Unsets the value of the "scaleZ" attribute of this CSGScale.
  */
int 
CSGScale::unsetScaleZ ()
{
  mScaleZ      = numeric_limits<double>::quiet_NaN();
  mIsSetScaleZ = false;
  
  if (!isSetScaleZ())
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
CSGScale::createObject (XMLInputStream& stream)
{
  // return 0;
  
  SBase*        object = 0;

  object=CSGTransformation::createObject(stream);
  
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
CSGScale::addExpectedAttributes(ExpectedAttributes& attributes)
{
  CSGTransformation::addExpectedAttributes(attributes);
  
  attributes.add("scaleX");
  attributes.add("scaleY");
  attributes.add("scaleZ");
}


/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
CSGScale::readAttributes (const XMLAttributes& attributes,
                        const ExpectedAttributes& expectedAttributes)
{
  CSGTransformation::readAttributes(attributes,expectedAttributes);

  mIsSetScaleX = attributes.readInto("scaleX", mScaleX, getErrorLog(), true, getLine(), getColumn());
  mIsSetScaleY = attributes.readInto("scaleY", mScaleY, getErrorLog(), true, getLine(), getColumn());
  mIsSetScaleZ = attributes.readInto("scaleZ", mScaleZ, getErrorLog(), true, getLine(), getColumn());

}


/*
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.
 */
void
CSGScale::writeAttributes (XMLOutputStream& stream) const
{
  CSGTransformation::writeAttributes(stream);

  stream.writeAttribute("scaleX", getPrefix(), mScaleX);
  stream.writeAttribute("scaleY", getPrefix(), mScaleY);
  stream.writeAttribute("scaleZ", getPrefix(), mScaleZ);

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
CSGScale::writeElements (XMLOutputStream& stream) const
{
  CSGTransformation::writeElements(stream);

  //
  // (EXTENSION)
  //
  SBase::writeExtensionElements(stream);
}


CSGScale*
CSGScale::clone() const
{
    return new CSGScale(*this);
}


/*
 * Accepts the given SBMLVisitor.
 */
bool
CSGScale::accept (SBMLVisitor& v) const
{
  // return false;
  return v.visit(*this);
  
}

/*
 * Sets the parent SBMLDocument of this SBML object.
 */
void
CSGScale::setSBMLDocument (SBMLDocument* d)
{
  CSGTransformation::setSBMLDocument(d);
}

/*
 * Enables/Disables the given package with this element and child
 * elements (if any).
 * (This is an internal implementation for enablePakcage function)
 */
void
CSGScale::enablePackageInternal(const std::string& pkgURI,
                             const std::string& pkgPrefix, bool flag)
{
  CSGTransformation::enablePackageInternal(pkgURI,pkgPrefix,flag);
}

LIBSBML_CPP_NAMESPACE_END
