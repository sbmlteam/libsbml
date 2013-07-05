/**
 * @file    CSGTranslation.cpp
 * @brief   Implementation of CSGTranslation, the SBase derived class of spatial package.
 * @author  
 *
 * $Id: CSGTranslation.cpp 10670 2010-01-16 12:10:06Z  $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/sbml/CSGTranslation.cpp $
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
#include <sbml/packages/spatial/sbml/CSGTranslation.h>

using namespace std;

LIBSBML_CPP_NAMESPACE_BEGIN

/*
 * Creates a new CSGTranslation with the given level, version, and package version.
 */
CSGTranslation::CSGTranslation (unsigned int level, unsigned int version, unsigned int pkgVersion) 
		: CSGTransformation (SBML_SPATIAL_CSGTRANSLATION, level,version)
		, mTranslateX(0.0)
		, mTranslateY(0.0)
		, mTranslateZ(0.0)
		, mIsSetTranslateX (false)
		, mIsSetTranslateY (false)
		, mIsSetTranslateZ (false)
{
	mType = SBML_SPATIAL_CSGTRANSLATION;
  // set an SBMLNamespaces derived object (SpatialPkgNamespaces) of this package.
  setSBMLNamespacesAndOwn(new SpatialPkgNamespaces(level,version,pkgVersion)); 

}


/*
 * Creates a new CSGTranslation with the given SpatialPkgNamespaces object.
 */
CSGTranslation::CSGTranslation(SpatialPkgNamespaces* spatialns)
	: CSGTransformation (SBML_SPATIAL_CSGTRANSLATION, spatialns)
		, mTranslateX(0.0)
		, mTranslateY(0.0)
		, mTranslateZ(0.0)
		, mIsSetTranslateX (false)
		, mIsSetTranslateY (false)
		, mIsSetTranslateZ (false)
{
  mType = SBML_SPATIAL_CSGTRANSLATION;
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
CSGTranslation::CSGTranslation(const CSGTranslation& source) : CSGTransformation(source)
{
  if (source.mChild)
  {
	  mChild = static_cast<CSGNode*>( source.mChild->clone() );
  }

  this->mTranslateX=source.mTranslateX;
  this->mTranslateY=source.mTranslateY;
  this->mTranslateZ=source.mTranslateZ;
  this->mIsSetTranslateX=source.mIsSetTranslateX;
  this->mIsSetTranslateY=source.mIsSetTranslateY;
  this->mIsSetTranslateZ=source.mIsSetTranslateZ;

  connectToChild();

}


/*
 * Assignment operator.
 */
CSGTranslation& CSGTranslation::operator=(const CSGTranslation& source)
{
  if(&source!=this)
  {
    this->CSGTransformation::operator=(source);
    this->mTranslateX=source.mTranslateX;
    this->mTranslateY=source.mTranslateY;
    this->mTranslateZ=source.mTranslateZ;
    this->mIsSetTranslateX=source.mIsSetTranslateX;
    this->mIsSetTranslateY=source.mIsSetTranslateY;
    this->mIsSetTranslateZ=source.mIsSetTranslateZ;
  }
  
  return *this;
}

/*
 * Destructor.
 */ 
CSGTranslation::~CSGTranslation ()
{
	// destroy 'mChild'
	if (mChild) delete mChild;
}


 /*
  * Returns the value of the "translateX" attribute of this CSGTranslation.
  */
double 
CSGTranslation::getTranslateX () const
{
  return mTranslateX;
}

 /*
  * Returns the value of the "translateY" attribute of this CSGTranslation.
  */
double 
CSGTranslation::getTranslateY () const
{
  return mTranslateY;
}

 /*
  * Returns the value of the "translateZ" attribute of this CSGTranslation.
  */
double 
CSGTranslation::getTranslateZ () const
{
  return mTranslateZ;
}


 /*
  * Predicate returning @c true or @c false depending on whether this
  * CSGTranslation's "translateX" attribute has been set.
  */
bool 
CSGTranslation::isSetTranslateX () const
{
  return mIsSetTranslateX;
}

/*
 * @return true if the "translateY" of this CSGTranslation has been set, false
 * otherwise.
 */
bool
CSGTranslation::isSetTranslateY () const
{
  return mIsSetTranslateY;
}

/*
 * @return true if the "translateZ" of this CSGTranslation has been set, false
 * otherwise.
 */
bool
CSGTranslation::isSetTranslateZ () const
{
  return mIsSetTranslateZ;
}

 /*
  * Sets the value of the "translateX" attribute of this CSGTranslation.
  */
int 
CSGTranslation::setTranslateX (double val)
{
  if (val < 0)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mTranslateX = val;
    mIsSetTranslateX  = true;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


 /*
  * Sets the value of the "translateY" attribute of this CSGTranslation.
  */
int 
CSGTranslation::setTranslateY (double val)
{
  if (val < 0)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mTranslateY = val;
    mIsSetTranslateY  = true;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


 /*
  * Sets the value of the "translateX" attribute of this CSGTranslation.
  */
int 
CSGTranslation::setTranslateZ (double val)
{
  if (val < 0)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mTranslateZ = val;
    mIsSetTranslateZ  = true;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
  * Unsets the value of the "translateX" attribute of this CSGTranslation.
  */
int 
CSGTranslation::unsetTranslateX ()
{
  mTranslateX      = numeric_limits<double>::quiet_NaN();
  mIsSetTranslateX = false;
  
  if (!isSetTranslateX())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}

/*
  * Unsets the value of the "translateY" attribute of this CSGTranslation.
  */
int 
CSGTranslation::unsetTranslateY ()
{
  mTranslateY      = numeric_limits<double>::quiet_NaN();
  mIsSetTranslateY = false;
  
  if (!isSetTranslateY())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}

/*
  * Unsets the value of the "translateZ" attribute of this CSGTranslation.
  */
int 
CSGTranslation::unsetTranslateZ ()
{
  mTranslateZ      = numeric_limits<double>::quiet_NaN();
  mIsSetTranslateZ = false;
  
  if (!isSetTranslateZ())
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
CSGTranslation::createObject (XMLInputStream& stream)
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
CSGTranslation::addExpectedAttributes(ExpectedAttributes& attributes)
{
  CSGTransformation::addExpectedAttributes(attributes);
  
  attributes.add("translateX");
  attributes.add("translateY");
  attributes.add("translateZ");
}


/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
CSGTranslation::readAttributes (const XMLAttributes& attributes,
                        const ExpectedAttributes& expectedAttributes)
{
  CSGTransformation::readAttributes(attributes,expectedAttributes);

  mIsSetTranslateX = attributes.readInto("translateX", mTranslateX, getErrorLog(), true, getLine(), getColumn());
  mIsSetTranslateY = attributes.readInto("translateY", mTranslateY, getErrorLog(), true, getLine(), getColumn());
  mIsSetTranslateZ = attributes.readInto("translateZ", mTranslateZ, getErrorLog(), true, getLine(), getColumn());

}


/*
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.
 */
void
CSGTranslation::writeAttributes (XMLOutputStream& stream) const
{
  CSGTransformation::writeAttributes(stream);

  stream.writeAttribute("translateX", getPrefix(), mTranslateX);
  stream.writeAttribute("translateY", getPrefix(), mTranslateY);
  stream.writeAttribute("translateZ", getPrefix(), mTranslateZ);

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
CSGTranslation::writeElements (XMLOutputStream& stream) const
{
  CSGTransformation::writeElements(stream);

  //
  // (EXTENSION)
  //
  SBase::writeExtensionElements(stream);
}


CSGTranslation*
CSGTranslation::clone() const
{
    return new CSGTranslation(*this);
}


/*
 * Accepts the given SBMLVisitor.
 */
bool
CSGTranslation::accept (SBMLVisitor& v) const
{
  // return false;
  return v.visit(*this);
  
}

/*
 * Sets the parent SBMLDocument of this SBML object.
 */
void
CSGTranslation::setSBMLDocument (SBMLDocument* d)
{
  CSGTransformation::setSBMLDocument(d);
}

/*
 * Enables/Disables the given package with this element and child
 * elements (if any).
 * (This is an internal implementation for enablePakcage function)
 */
void
CSGTranslation::enablePackageInternal(const std::string& pkgURI,
                             const std::string& pkgPrefix, bool flag)
{
  CSGTransformation::enablePackageInternal(pkgURI,pkgPrefix,flag);
}

LIBSBML_CPP_NAMESPACE_END
