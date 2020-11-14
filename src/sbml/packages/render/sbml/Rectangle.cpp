/**
 * @file    Rectangle.cpp
 * @brief Implementation of the Rectangle class.
 * @author  Ralph Gauges
 * @author  Frank T. Bergmann
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
 *
 * Copyright (C) 2019 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2011-2013 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *  
 * Copyright 2010 Ralph Gauges
 *     Group for the modeling of biological processes 
 *     University of Heidelberg
 *     Im Neuenheimer Feld 267
 *     69120 Heidelberg
 *     Germany
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ---------------------------------------------------------------------- -->*/

#include <sbml/packages/render/sbml/Rectangle.h>
#include <sbml/packages/layout/util/LayoutAnnotation.h>
#include <sbml/packages/layout/util/LayoutUtilities.h>
#include <sbml/packages/render/extension/RenderExtension.h>
#include <sbml/packages/render/validator/RenderSBMLError.h>

#ifndef OMIT_DEPRECATED
#ifdef DEPRECATION_WARNINGS
#include <iostream>
#endif // DEPRECATION_WARNINGS
#endif // OMIT_DEPRECATED

using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new Rectangle using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 */
Rectangle::Rectangle(unsigned int level,
                     unsigned int version,
                     unsigned int pkgVersion)
  : GraphicalPrimitive2D(level,version, pkgVersion)
  , mX(RelAbsVector(0.0,0.0))
  , mY(RelAbsVector(0.0,0.0))
  , mZ(RelAbsVector(0.0,0.0))
  , mWidth(RelAbsVector(0.0,0.0))
  , mHeight(RelAbsVector(0.0,0.0))
  , mRX(RelAbsVector(0.0,0.0))
  , mRY(RelAbsVector(0.0,0.0))
  , mRatio(util_NaN())
  , mIsSetRatio(false)

{
  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(level, version, pkgVersion));
  connectToChild();
}


/*
 * Creates a new Rectangle using the given RenderPkgNamespaces object.
 */
Rectangle::Rectangle (RenderPkgNamespaces* renderns)
  : GraphicalPrimitive2D(renderns)
  , mX(RelAbsVector(0.0,0.0))
  , mY(RelAbsVector(0.0,0.0))
  , mZ(RelAbsVector(0.0,0.0))
  , mWidth(RelAbsVector(0.0,0.0))
  , mHeight(RelAbsVector(0.0,0.0))
  , mRX(RelAbsVector(0.0,0.0))
  , mRY(RelAbsVector(0.0,0.0))
  , mRatio(util_NaN())
  , mIsSetRatio(false)
{
  setElementNamespace(renderns->getURI());
  connectToChild();
  loadPlugins(renderns);
}


/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new Rectangle object from the given XMLNode object.
 * The XMLNode object has to contain a valid XML representation of a 
 * Rectangle object as defined in the render extension specification.
 * This method is normally called when render information is read from a file and 
 * should normally not have to be called explicitly.
 *
 * @param node the XMLNode object reference that describes the Rectangle
 * object to be instantiated.
 */
Rectangle::Rectangle(const XMLNode& node, unsigned int l2version)
  : GraphicalPrimitive2D(node, l2version)
  , mRatio(util_NaN())
  , mIsSetRatio(false)
{
  ExpectedAttributes ea;
    addExpectedAttributes(ea);
    this->readAttributes(node.getAttributes(), ea);

    
  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(2,l2version));  

  connectToChild();
}
/** @endcond */





#ifndef OMIT_DEPRECATED
/** @cond doxygenLibsbmlInternal */
/*
 * Instantiates a new Rectangle object.
 * All attributes are set as described for the default constructor
 * of GraphicalPrimitive2D.
 *
 * The id is set to the given string and all rectangle specific attributes are set to 0.
 *
 * @param id id string for the rectangle
 *
 * This constructor is deprecated. The new libsbml API only has
 * constructors which take the SBML level and version or one that takes
 * an SBMLNamespaces object.
 */
Rectangle::Rectangle(RenderPkgNamespaces* renderns, const std::string& id)
  : GraphicalPrimitive2D(renderns, id)
  , mX(RelAbsVector(0.0,0.0))
  , mY(RelAbsVector(0.0,0.0))
  , mZ(RelAbsVector(0.0,0.0))
  , mWidth(RelAbsVector(0.0,0.0))
  , mHeight(RelAbsVector(0.0,0.0))
  , mRX(RelAbsVector(0.0,0.0))
  , mRY(RelAbsVector(0.0,0.0))
  , mRatio(util_NaN())
  , mIsSetRatio(false)

{
#ifdef DEPRECATION_WARNINGS
    std::cerr << "Warning. Rectangle::Rectangle(const std::string& id) is deprecated." << std::endl;
#endif // DEPRECATION_WARNINGS
        // set the element namespace of this object
  setElementNamespace(renderns->getURI());

  // connect child elements to this element.
  connectToChild();

  // load package extensions bound with this object (if any) 
  loadPlugins(renderns);
}
/** @endcond */
#endif // OMIT_DEPRECATED

#ifndef OMIT_DEPRECATED
/** @cond doxygenLibsbmlInternal */
/*
 * Instantiates a new Rectangle object.
 * All attributes are set as described for the default constructor
 * of GraphicalPrimitive2D.
 *
 * The id is set to the given string and all rectangle specific attributes
 * are set to the given values.
 *
 * @param id id string for the rectangle
 * @param x x coordinate of the position 
 * @param y y coordinate of the position 
 * @param z z coordinate of the position 
 * @param w w width
 * @param h h height
 *
 * This constructor is deprecated. The new libsbml API only has
 * constructors which take the SBML level and version or one that takes
 * an SBMLNamespaces object.
 */
Rectangle::Rectangle(RenderPkgNamespaces* renderns, const std::string& id,const RelAbsVector& x,const RelAbsVector& y,const RelAbsVector& z,const RelAbsVector& w,const RelAbsVector& h)
  : GraphicalPrimitive2D(renderns, id)
  , mX(x)
  , mY(y)
  , mZ(z)
  , mWidth(w)
  , mHeight(h)
  , mRX(RelAbsVector(0.0,0.0))
  , mRY(RelAbsVector(0.0,0.0))
  , mRatio(util_NaN())
  , mIsSetRatio(false)

{
#ifdef DEPRECATION_WARNINGS
    std::cerr << "Warning. Rectangle::Rectangle(const std::string& id,const RelAbsVector& x,const RelAbsVector& y,const RelAbsVector& z,const RelAbsVector& w,const RelAbsVector& h) is deprecated." << std::endl;
#endif // DEPRECATION_WARNINGS
        // set the element namespace of this object
  setElementNamespace(renderns->getURI());

  // connect child elements to this element.
  connectToChild();

  // load package extensions bound with this object (if any) 
  loadPlugins(renderns);
}
/** @endcond */
#endif // OMIT_DEPRECATED

#ifndef OMIT_DEPRECATED
/** @cond doxygenLibsbmlInternal */
/*
 * Instantiates a new Rectangle object.
 * All attributes are set as described for the default constructor
 * of GraphicalPrimitive2D.
 *
 * The id is set to the given string and all rectangle specific attributes
 * are set to the given values. The z coordinate of the position is set to 0.
 *
 * @param id id string for the rectangle
 * @param x x coordinate of the position 
 * @param y y coordinate of the position 
 * @param w w width
 * @param h h height
 *
 * This constructor is deprecated. The new libsbml API only has
 * constructors which take the SBML level and version or one that takes
 * an SBMLNamespaces object.
 */
Rectangle::Rectangle(RenderPkgNamespaces* renderns, const std::string& id,const RelAbsVector& x,const RelAbsVector& y,const RelAbsVector& w,const RelAbsVector& h)
  : GraphicalPrimitive2D(renderns, id)
  , mX(x)
  , mY(y)
  , mZ(RelAbsVector(0.0,0.0))
  , mWidth(w)
  , mHeight(h)
  , mRX(RelAbsVector(0.0,0.0))
  , mRY(RelAbsVector(0.0,0.0))
  , mRatio(util_NaN())
  , mIsSetRatio(false)
{
#ifdef DEPRECATION_WARNINGS
    std::cerr << "Warning. Rectangle::Rectangle(const std::string& id,const RelAbsVector& x,const RelAbsVector& y,const RelAbsVector& w,const RelAbsVector& h) is deprecated." << std::endl;
#endif // DEPRECATION_WARNINGS
        // set the element namespace of this object
  setElementNamespace(renderns->getURI());

  // connect child elements to this element.
  connectToChild();

  // load package extensions bound with this object (if any) 
  loadPlugins(renderns);
}
/** @endcond */
#endif // OMIT_DEPRECATED

/*
 * Copy constructor for Rectangle.
 */
Rectangle::Rectangle(const Rectangle& orig)
  : GraphicalPrimitive2D( orig )
  , mX ( orig.mX )
  , mY ( orig.mY )
  , mZ ( orig.mZ )
  , mWidth ( orig.mWidth )
  , mHeight ( orig.mHeight )
  , mRX ( orig.mRX )
  , mRY ( orig.mRY )
  , mRatio ( orig.mRatio )
  , mIsSetRatio ( orig.mIsSetRatio )
{
  connectToChild();
}


/*
 * Assignment operator for Rectangle.
 */
Rectangle&
Rectangle::operator=(const Rectangle& rhs)
{
  if (&rhs != this)
  {
    GraphicalPrimitive2D::operator=(rhs);
    mRatio = rhs.mRatio;
    mIsSetRatio = rhs.mIsSetRatio;
    mX = rhs.mX;
    mY = rhs.mY;
    mZ = rhs.mZ;
    mWidth = rhs.mWidth;
    mHeight = rhs.mHeight;
    mRX = rhs.mRX;
    mRY = rhs.mRY;

    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this Rectangle object.
 */
Rectangle*
Rectangle::clone() const
{
  return new Rectangle(*this);
}


/*
 * Destructor for Rectangle.
 */
Rectangle::~Rectangle()
{
}


/*
 * Returns the value of the "ratio" attribute of this Rectangle.
 */
double
Rectangle::getRatio() const
{
  return mRatio;
}


/*
 * Predicate returning @c true if this Rectangle's "ratio" attribute is set.
 */
bool
Rectangle::isSetRatio() const
{
  return mIsSetRatio;
}


/*
 * Sets the value of the "ratio" attribute of this Rectangle.
 */
int
Rectangle::setRatio(double ratio)
{
  mRatio = ratio;
  mIsSetRatio = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "ratio" attribute of this Rectangle.
 */
int
Rectangle::unsetRatio()
{
  mRatio = util_NaN();
  mIsSetRatio = false;

  if (isSetRatio() == false)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Returns the value of the "x" element of this Rectangle.
 */
const RelAbsVector& 
Rectangle::getX() const
{
    return this->mX;
}


/*
 * Returns the value of the "x" element of this Rectangle.
 */
RelAbsVector& 
Rectangle::getX()
{
    return this->mX;
}


/*
 * Returns the value of the "y" element of this Rectangle.
 */
const RelAbsVector& 
Rectangle::getY() const
{
    return this->mY;
}


/*
 * Returns the value of the "y" element of this Rectangle.
 */
RelAbsVector& 
Rectangle::getY()
{
  return this->mY;
}


/*
 * Returns the value of the "z" element of this Rectangle.
 */
const RelAbsVector& 
Rectangle::getZ() const
{
    return this->mZ;
}


/*
 * Returns the value of the "z" element of this Rectangle.
 */
RelAbsVector& 
Rectangle::getZ()
{
  return this->mZ;
}


/*
 * Returns the value of the "width" element of this Rectangle.
 */
const RelAbsVector& 
Rectangle::getWidth() const
{
    return this->mWidth;
}


/*
 * Returns the value of the "width" element of this Rectangle.
 */
RelAbsVector& 
Rectangle::getWidth()
{
  return this->mWidth;
}


/*
 * Returns the value of the "height" element of this Rectangle.
 */
const RelAbsVector& 
Rectangle::getHeight() const
{
    return this->mHeight;
}


/*
 * Returns the value of the "height" element of this Rectangle.
 */
RelAbsVector& 
Rectangle::getHeight()
{
  return this->mHeight;
}


/*
 * Returns the value of the "rX" element of this Rectangle.
 */
const RelAbsVector&
Rectangle::getRX() const
{
  return mRX;
}


/*
 * Returns the corner radius along the x axis
 */
const RelAbsVector& 
Rectangle::getRadiusX() const
{
    return this->mRX;
}


/*
 * Returns the value of the "rX" element of this Rectangle.
 */
RelAbsVector&
Rectangle::getRX()
{
  return mRX;
}


/*
* Returns the corner radius along the x axis
*/
RelAbsVector& 
Rectangle::getRadiusX()
{
  return this->mRX;
}


/*
 * Returns the value of the "rY" element of this Rectangle.
 */
const RelAbsVector&
Rectangle::getRY() const
{
  return mRY;
}


/*
 * Returns the corner radius along the y axis
 */
const RelAbsVector& 
Rectangle::getRadiusY() const
{
    return this->mRY;
}


/*
 * Returns the value of the "rY" element of this Rectangle.
 */
RelAbsVector&
Rectangle::getRY()
{
  return mRY;
}


/*
* Returns the corner radius along the y axis
*/
RelAbsVector& 
Rectangle::getRadiusY()
{
  return this->mRY;
}


/*
* Predicate returning @c true if this Rectangle's "x" element is set.
*/
bool
Rectangle::isSetX() const
{
  return mX.isSetCoordinate();
}


/*
* Predicate returning @c true if this Rectangle's "y" element is set.
*/
bool
Rectangle::isSetY() const
{
  return mY.isSetCoordinate();
}


/*
* Predicate returning @c true if this Rectangle's "z" element is set.
*/
bool
Rectangle::isSetZ() const
{
  return mZ.isSetCoordinate();
}


/*
* Predicate returning @c true if this Rectangle's "width" element is set.
*/
bool
Rectangle::isSetWidth() const
{
  return mWidth.isSetCoordinate();
}


/*
* Predicate returning @c true if this Rectangle's "height" element is set.
*/
bool
Rectangle::isSetHeight() const
{
  return mHeight.isSetCoordinate();
}


/*
* Predicate returning @c true if this Rectangle's "rX" element is set.
*/
bool
Rectangle::isSetRX() const
{
  return mRX.isSetCoordinate();
}


/*
* Predicate returning @c true if this Rectangle's "rX" element is set.
*/
bool
Rectangle::isSetRadiusX() const
{
  return isSetRX();
}


/*
* Predicate returning @c true if this Rectangle's "rY" element is set.
*/
bool
Rectangle::isSetRY() const
{
  return mRY.isSetCoordinate();
}


/*
* Predicate returning @c true if this Rectangle's "rY" element is set.
*/
bool
Rectangle::isSetRadiusY() const
{
  return isSetRY();
}


/** @cond doxygenLibsbmlInternal */
/*
 * Sets the position and the size of the Rectangle within the viewport.
 *
 * @param x x coordinate of the position 
 * @param y y coordinate of the position 
 * @param z z coordinate of the position 
 * @param w w width
 * @param h h height
 */
void Rectangle::setCoordinatesAndSize(const RelAbsVector& x,const RelAbsVector& y,const RelAbsVector& z,const RelAbsVector& w,const RelAbsVector& h)
{
    this->setCoordinates(x,y,z);
    this->setSize(w,h);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the position of the Rectangle within the viewport.
 *
 * @param x x coordinate of the position 
 * @param y y coordinate of the position 
 * @param z z coordinate of the position 
 */
void Rectangle::setCoordinates(const RelAbsVector& x,const RelAbsVector& y,const RelAbsVector& z)
{
    this->mX=x;
    this->mY=y;
    this->mZ=z;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the size of the Rectangle 
 *
 * @param w w width
 * @param h h height
 */
void Rectangle::setSize(const RelAbsVector& w,const RelAbsVector& h)
{
    this->mWidth=w;
    this->mHeight=h;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the two corner radii of the rectangle
 *
 * @param rx corner radius along the x axis
 * @param ry corner radius along the y axis
 */
void Rectangle::setRadii(const RelAbsVector& rx,const RelAbsVector& ry)
{
    this->mRX=rx;
    this->mRY=ry;
}
/** @endcond */


/*
 * Sets the x position of the Rectangle within the viewport.
 */
int 
Rectangle::setX(const RelAbsVector& x)
{
  this->mX = x;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
* Sets the y position of the Rectangle within the viewport.
*/
int 
Rectangle::setY(const RelAbsVector& y)
{
  this->mY = y;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
* Sets the z position of the Rectangle within the viewport.
*
* @param z z coordinate of the position
*/
int 
Rectangle::setZ(const RelAbsVector& z)
{
  this->mZ = z;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
* Sets the width of the Rectangle
*/
int 
Rectangle::setWidth(const RelAbsVector& w)
{
  this->mWidth = w;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
* Sets the height of the Rectangle
*/
int 
Rectangle::setHeight(const RelAbsVector& h)
{
  this->mHeight = h;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
* Sets the corner radius along the x axis
*/
int 
Rectangle::setRadiusX(const RelAbsVector& rx)
{
  this->mRX = rx;
  return LIBSBML_OPERATION_SUCCESS;
}


int
Rectangle::setRX(const RelAbsVector& rx)
{
  this->mRX = rx;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
* Sets the corner radius along the y axis
*/
int 
Rectangle::setRadiusY(const RelAbsVector& ry)
{
  this->mRY = ry;
  return LIBSBML_OPERATION_SUCCESS;
}


int
Rectangle::setRY(const RelAbsVector& ry)
{
  this->mRY = ry;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "x" element of this Rectangle.
 */
int
Rectangle::unsetX()
{
  mX.unsetCoordinate();
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "y" element of this Rectangle.
 */
int
Rectangle::unsetY()
{
  mY.unsetCoordinate();
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "z" element of this Rectangle.
 */
int
Rectangle::unsetZ()
{
  mZ.unsetCoordinate();
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "width" element of this Rectangle.
 */
int
Rectangle::unsetWidth()
{
  mWidth.unsetCoordinate();
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "height" element of this Rectangle.
 */
int
Rectangle::unsetHeight()
{
  mHeight.unsetCoordinate();
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "rX" element of this Rectangle.
 */
int
Rectangle::unsetRadiusX()
{
  return unsetRX();
}


int
Rectangle::unsetRX()
{
  mRX.unsetCoordinate();
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "rY" element of this Rectangle.
 */
int
Rectangle::unsetRadiusY()
{
  return unsetRY();
}


int
Rectangle::unsetRY()
{
  mRY.unsetCoordinate();
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Returns the XML element name of this Rectangle object.
 */
const std::string&
Rectangle::getElementName() const
{
  static const string name = "rectangle";
  return name;
}


/*
 * Returns the libSBML type code for this Rectangle object.
 */
int
Rectangle::getTypeCode() const
{
  return SBML_RENDER_RECTANGLE;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * Rectangle object have been set.
 */
bool
Rectangle::hasRequiredAttributes() const
{
  bool allPresent = GraphicalPrimitive2D::hasRequiredAttributes();

  if (isSetX() == false)
  {
    allPresent = false;
  }

  if (isSetY() == false)
  {
    allPresent = false;
  }

  if (isSetHeight() == false)
  {
    allPresent = false;
  }
  if (isSetWidth() == false)
  {
    allPresent = false;
  }

  return allPresent;
}


/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
Rectangle::accept(SBMLVisitor& v) const
{
  v.visit(*this);
  //render - FIX_ME

  //if (mX != NULL)
  //{
  //  mX->accept(v);
  //}

  //if (mY != NULL)
  //{
  //  mY->accept(v);
  //}

  //if (mZ != NULL)
  //{
  //  mZ->accept(v);
  //}

  //if (mWidth != NULL)
  //{
  //  mWidth->accept(v);
  //}

  //if (mHeight != NULL)
  //{
  //  mHeight->accept(v);
  //}

  //if (mRX != NULL)
  //{
  //  mRX->accept(v);
  //}

  //if (mRY != NULL)
  //{
  //  mRY->accept(v);
  //}

  v.leave(*this);
  return true;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */
/*
 * Creates an XMLNode object from this Rectangle object.
 *
 * @return the XMLNode with the XML representation for the 
 * Rectangle object.
 */
XMLNode Rectangle::toXML() const
{
  return getXmlNodeForSBase(this);
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
void
Rectangle::addExpectedAttributes(ExpectedAttributes& attributes)
{
  GraphicalPrimitive2D::addExpectedAttributes(attributes);

  attributes.add("x");
  attributes.add("y");
  attributes.add("z");
  attributes.add("width");
  attributes.add("height");
  attributes.add("rx");
  attributes.add("ry");
  attributes.add("ratio");
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
Rectangle::readAttributes(const XMLAttributes& attributes,
                          const ExpectedAttributes& expectedAttributes)
{
    unsigned int level = getLevel();
    unsigned int version = getVersion();
    unsigned int pkgVersion = getPackageVersion();
    unsigned int numErrs;
    bool assigned = false;
    SBMLErrorLog* log = getErrorLog();

    GraphicalPrimitive2D::readAttributes(attributes, expectedAttributes);

    if (log)
    {
      numErrs = log->getNumErrors();

      for (int n = numErrs-1; n >= 0; n--)
      {
        if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
        {
          const std::string details = log->getError(n)->getMessage();
          log->remove(UnknownPackageAttribute);
          log->logPackageError("render", RenderRectangleAllowedAttributes,
            pkgVersion, level, version, details, getLine(), getColumn());
        }
        else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
        {
          const std::string details = log->getError(n)->getMessage();
          log->remove(UnknownCoreAttribute);
          log->logPackageError("render", RenderRectangleAllowedCoreAttributes,
            pkgVersion, level, version, details, getLine(), getColumn());
        }
      }
    }

    string elplusid = "<rectangle> element";
    if (!getId().empty()) {
      elplusid += " with the id '" + mId + "'";
    }
    // 
    // ratio double (use = "optional" )
    // 

    if (log) numErrs = log->getNumErrors();
    mIsSetRatio = attributes.readInto("ratio", mRatio);

    if ( mIsSetRatio == false && log)
    {
      if (log->getNumErrors() == numErrs + 1 &&
        log->contains(XMLAttributeTypeMismatch))
      {
        log->remove(XMLAttributeTypeMismatch);
          std::string message = "Render attribute 'ratio' from the " + elplusid +
            " must be a double.";
        log->logPackageError("render", RenderRectangleRatioMustBeDouble,
          pkgVersion, level, version, message, getLine(), getColumn());
      }
    }

    std::string s;
    RelAbsVector v = RelAbsVector();

    //
    // x RelAbsVector (use = required) 
    //
    s="";
    assigned = attributes.readInto("x", s, this->getErrorLog(), false, getLine(), getColumn());
    if (!assigned && log)
    {
      std::string message = "The required attribute 'x' is missing from the "
        + elplusid + ".";
      log->logPackageError("render", RenderRectangleAllowedAttributes,
        pkgVersion, level, version, message, getLine(), getColumn());
    }
    else
    {
      v.setCoordinate(s);
      if (!(v.isSetCoordinate()) && log)
      {
        std::string message = "The syntax '" + s + "' of the attribute 'x' on the "
          + elplusid + " does not conform to the syntax of a RelAbsVector type.";
        log->logPackageError("render", RenderRectangleXMustBeRelAbsVector,
          pkgVersion, level, version, message, getLine(), getColumn());

      }
      else
      {
        this->setX(v);
      }
      v.erase();
    }

    //
    // y RelAbsVector (use = required) 
    //
    s = "";
    assigned = attributes.readInto("y", s, this->getErrorLog(), false, getLine(), getColumn());
    if (!assigned && log)
    {
      std::string message = "The required attribute 'y' is missing from the "
        + elplusid + ".";
      log->logPackageError("render", RenderRectangleAllowedAttributes,
        pkgVersion, level, version, message, getLine(), getColumn());
    }
    else
    {
      v.setCoordinate(s);
      if (!(v.isSetCoordinate()) && log)
      {
        std::string message = "The syntax '" + s + "' of the attribute 'y' on the "
          + elplusid + " does not conform to the syntax of a RelAbsVector type.";
        log->logPackageError("render", RenderRectangleYMustBeRelAbsVector,
          pkgVersion, level, version, message, getLine(), getColumn());

      }
      else
      {
        this->setY(v);
      }
      v.erase();
    }

    //
    // z RelAbsVector (use = optional) 
    //

    s="";
    assigned = attributes.readInto("z", s, getErrorLog(), false, getLine(), getColumn());
    if (!assigned)
    {
        this->mZ=RelAbsVector(0.0,0.0);
    }
    else
    {
      v.setCoordinate(s);
      if (!(v.isSetCoordinate()) && log)
      {
        std::string message = "The syntax '" + s + "' of the attribute 'z' on the "
          + elplusid + " does not conform to the syntax of a RelAbsVector type.";
        log->logPackageError("render", RenderRectangleZMustBeRelAbsVector,
          pkgVersion, level, version, message, getLine(), getColumn());

      }
      else
      {
        this->setZ(v);
      }
      v.erase();
    }

    //
    // height RelAbsVector (use = required) 
    //
    s = "";
    assigned = attributes.readInto("height", s, this->getErrorLog(), false, getLine(), getColumn());
    if (!assigned && log)
    {
      std::string message = "The required attribute 'height' is missing from the "
        + elplusid + ".";
      log->logPackageError("render", RenderRectangleAllowedAttributes,
        pkgVersion, level, version, message, getLine(), getColumn());
    }
    else
    {
      v.setCoordinate(s);
      if (!(v.isSetCoordinate()) && log)
      {
        std::string message = "The syntax '" + s + "' of the attribute 'height' on the "
          + elplusid + " does not conform to the syntax of a RelAbsVector type.";
        log->logPackageError("render", RenderRectangleHeightMustBeRelAbsVector,
          pkgVersion, level, version, message, getLine(), getColumn());

      }
      else
      {
        this->setHeight(v);
      }
      v.erase();
    }


    //
    // width RelAbsVector (use = required) 
    //
    s = "";
    assigned = attributes.readInto("width", s, this->getErrorLog(), false, getLine(), getColumn());
    if (!assigned && log)
    {
      std::string message = "The required attribute 'width' is missing from the "
        + elplusid + ".";
      log->logPackageError("render", RenderRectangleAllowedAttributes,
        pkgVersion, level, version, message, getLine(), getColumn());
    }
    else
    {
      v.setCoordinate(s);
      if (!(v.isSetCoordinate()) && log)
      {
        std::string message = "The syntax '" + s + "' of the attribute 'width' on the "
          + elplusid + " does not conform to the syntax of a RelAbsVector type.";
        log->logPackageError("render", RenderRectangleWidthMustBeRelAbsVector,
          pkgVersion, level, version, message, getLine(), getColumn());

      }
      else
      {
        this->setWidth(v);
      }
      v.erase();
    }

    //
    // rx RelAbsVector (use = optional) 
    //

    s = "";
    assigned = attributes.readInto("rx", s, getErrorLog(), false, getLine(), getColumn());
    if (!assigned)
    {
      this->mRX = RelAbsVector(0.0, 0.0);
    }
    else
    {
      v.setCoordinate(s);
      if (!(v.isSetCoordinate()) && log)
      {
        std::string message = "The syntax '" + s + "' of the attribute 'rx' on the "
          + elplusid + " does not conform to the syntax of a RelAbsVector type.";
        log->logPackageError("render", RenderRectangleRXMustBeRelAbsVector,
          pkgVersion, level, version, message, getLine(), getColumn());

      }
      else
      {
        this->setRX(v);
      }
      v.erase();
    }

    //
    // ry RelAbsVector (use = optional) 
    //

    s = "";
    assigned = attributes.readInto("ry", s, getErrorLog(), false, getLine(), getColumn());
    if (!assigned)
    {
      if (isSetRX())
      {
        setRY(getRX());
      }
      else
      {
        this->mRY = RelAbsVector(0.0, 0.0);
      }
    }
    else
    {
      v.setCoordinate(s);
      if (!(v.isSetCoordinate()) && log)
      {
        std::string message = "The syntax '" + s + "' of the attribute 'ry' on the "
          + elplusid + " does not conform to the syntax of a RelAbsVector type.";
        log->logPackageError("render", RenderRectangleRYMustBeRelAbsVector,
          pkgVersion, level, version, message, getLine(), getColumn());

      }
      else
      {
        this->setRY(v);
        if (!isSetRX())
        {
          setRX(v);
        }
      }
      v.erase();
    }


}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
* Subclasses should override this method to write their XML attributes
* to the XMLOutputStream.  Be sure to call your parents implementation
* of this method as well.  For example:
*
*   SBase::writeAttributes(stream);
*   stream.writeAttribute( "id"  , mId   );
*   stream.writeAttribute( "name", mName );
*   ...
*/
void Rectangle::writeAttributes(XMLOutputStream& stream) const
{
  GraphicalPrimitive2D::writeAttributes(stream);
  std::ostringstream os;
  os << this->mX;
  stream.writeAttribute("x", getPrefix(), os.str());
  os.str("");
  os << this->mY;
  stream.writeAttribute("y", getPrefix(), os.str());
  os.str("");
  os << this->mWidth;
  stream.writeAttribute("width", getPrefix(), os.str());
  os.str("");
  os << this->mHeight;
  stream.writeAttribute("height", getPrefix(), os.str());
  RelAbsVector tmp(0.0, 0.0);
  if (this->mZ != tmp)
  {
    os.str("");
    os << this->mZ;
    stream.writeAttribute("z", getPrefix(), os.str());
  }
  if (this->mRX != tmp)
  {
    os.str("");
    os << this->mRX;
    stream.writeAttribute("rx", getPrefix(), os.str());
  }
  if (this->mRY != tmp)
  {
    os.str("");
    os << this->mRY;
    stream.writeAttribute("ry", getPrefix(), os.str());
  }

  if (isSetRatio() == true)
  {
    stream.writeAttribute("ratio", getPrefix(), mRatio);
  }

}
/** @endcond */

#endif /* __cplusplus */


/*
 * Creates a new Rectangle_t using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 */
LIBSBML_EXTERN
Rectangle_t *
Rectangle_create(unsigned int level,
                 unsigned int version,
                 unsigned int pkgVersion)
{
  return new Rectangle(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this Rectangle_t object.
 */
LIBSBML_EXTERN
Rectangle_t*
Rectangle_clone(const Rectangle_t* r)
{
  if (r != NULL)
  {
    return static_cast<Rectangle_t*>(r->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this Rectangle_t object.
 */
LIBSBML_EXTERN
void
Rectangle_free(Rectangle_t* r)
{
  if (r != NULL)
  {
    delete r;
  }
}


/*
 * Returns the value of the "ratio" attribute of this Rectangle_t.
 */
LIBSBML_EXTERN
double
Rectangle_getRatio(const Rectangle_t * r)
{
  return (r != NULL) ? r->getRatio() : util_NaN();
}


/*
 * Predicate returning @c 1 (true) if this Rectangle_t's "ratio" attribute is
 * set.
 */
LIBSBML_EXTERN
int
Rectangle_isSetRatio(const Rectangle_t * r)
{
  return (r != NULL) ? static_cast<int>(r->isSetRatio()) : 0;
}


/*
 * Sets the value of the "ratio" attribute of this Rectangle_t.
 */
LIBSBML_EXTERN
int
Rectangle_setRatio(Rectangle_t * r, double ratio)
{
  return (r != NULL) ? r->setRatio(ratio) : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "ratio" attribute of this Rectangle_t.
 */
LIBSBML_EXTERN
int
Rectangle_unsetRatio(Rectangle_t * r)
{
  return (r != NULL) ? r->unsetRatio() : LIBSBML_INVALID_OBJECT;
}


/*
 * Returns the value of the "x" element of this Rectangle_t.
 */
LIBSBML_EXTERN
RelAbsVector_t*
Rectangle_getX(const Rectangle_t * r)
{
  if (r == NULL)
  {
    return NULL;
  }

  return (RelAbsVector_t*)(&(r->getX()));
}


/*
 * Returns the value of the "y" element of this Rectangle_t.
 */
LIBSBML_EXTERN
RelAbsVector_t*
Rectangle_getY(const Rectangle_t * r)
{
  if (r == NULL)
  {
    return NULL;
  }

  return (RelAbsVector_t*)(&(r->getY()));
}


/*
 * Returns the value of the "z" element of this Rectangle_t.
 */
LIBSBML_EXTERN
RelAbsVector_t*
Rectangle_getZ(const Rectangle_t * r)
{
  if (r == NULL)
  {
    return NULL;
  }

  return (RelAbsVector_t*)(&(r->getZ()));
}


/*
 * Returns the value of the "width" element of this Rectangle_t.
 */
LIBSBML_EXTERN
RelAbsVector_t*
Rectangle_getWidth(const Rectangle_t * r)
{
  if (r == NULL)
  {
    return NULL;
  }

  return (RelAbsVector_t*)(&(r->getWidth()));
}


/*
 * Returns the value of the "height" element of this Rectangle_t.
 */
LIBSBML_EXTERN
RelAbsVector_t*
Rectangle_getHeight(const Rectangle_t * r)
{
  if (r == NULL)
  {
    return NULL;
  }

  return (RelAbsVector_t*)(&(r->getHeight()));
}


/*
 * Returns the value of the "rX" element of this Rectangle_t.
 */
LIBSBML_EXTERN
RelAbsVector_t*
Rectangle_getRX(const Rectangle_t * r)
{
  if (r == NULL)
  {
    return NULL;
  }

  return (RelAbsVector_t*)(&(r->getRX()));
}


/*
 * Returns the value of the "rY" element of this Rectangle_t.
 */
LIBSBML_EXTERN
RelAbsVector_t*
Rectangle_getRY(const Rectangle_t * r)
{
  if (r == NULL)
  {
    return NULL;
  }

  return (RelAbsVector_t*)(&(r->getRY()));
}


/*
 * Predicate returning @c 1 (true) if this Rectangle_t's "x" element is set.
 */
LIBSBML_EXTERN
int
Rectangle_isSetX(const Rectangle_t * r)
{
  return (r != NULL) ? static_cast<int>(r->isSetX()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this Rectangle_t's "y" element is set.
 */
LIBSBML_EXTERN
int
Rectangle_isSetY(const Rectangle_t * r)
{
  return (r != NULL) ? static_cast<int>(r->isSetY()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this Rectangle_t's "z" element is set.
 */
LIBSBML_EXTERN
int
Rectangle_isSetZ(const Rectangle_t * r)
{
  return (r != NULL) ? static_cast<int>(r->isSetZ()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this Rectangle_t's "width" element is
 * set.
 */
LIBSBML_EXTERN
int
Rectangle_isSetWidth(const Rectangle_t * r)
{
  return (r != NULL) ? static_cast<int>(r->isSetWidth()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this Rectangle_t's "height" element is
 * set.
 */
LIBSBML_EXTERN
int
Rectangle_isSetHeight(const Rectangle_t * r)
{
  return (r != NULL) ? static_cast<int>(r->isSetHeight()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this Rectangle_t's "rX" element is set.
 */
LIBSBML_EXTERN
int
Rectangle_isSetRX(const Rectangle_t * r)
{
  return (r != NULL) ? static_cast<int>(r->isSetRX()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this Rectangle_t's "rY" element is set.
 */
LIBSBML_EXTERN
int
Rectangle_isSetRY(const Rectangle_t * r)
{
  return (r != NULL) ? static_cast<int>(r->isSetRY()) : 0;
}


/*
 * Sets the value of the "x" element of this Rectangle_t.
 */
LIBSBML_EXTERN
int
Rectangle_setX(Rectangle_t * r, const RelAbsVector_t* x)
{
  return (r != NULL) ? r->setX(*x) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "y" element of this Rectangle_t.
 */
LIBSBML_EXTERN
int
Rectangle_setY(Rectangle_t * r, const RelAbsVector_t* y)
{
  return (r != NULL) ? r->setY(*y) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "z" element of this Rectangle_t.
 */
LIBSBML_EXTERN
int
Rectangle_setZ(Rectangle_t * r, const RelAbsVector_t* z)
{
  return (r != NULL) ? r->setZ(*z) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "width" element of this Rectangle_t.
 */
LIBSBML_EXTERN
int
Rectangle_setWidth(Rectangle_t * r, const RelAbsVector_t* width)
{
  return (r != NULL) ? r->setWidth(*width) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "height" element of this Rectangle_t.
 */
LIBSBML_EXTERN
int
Rectangle_setHeight(Rectangle_t * r, const RelAbsVector_t* height)
{
  return (r != NULL) ? r->setHeight(*height) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "rX" element of this Rectangle_t.
 */
LIBSBML_EXTERN
int
Rectangle_setRX(Rectangle_t * r, const RelAbsVector_t* rX)
{
  return (r != NULL) ? r->setRX(*rX) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "rY" element of this Rectangle_t.
 */
LIBSBML_EXTERN
int
Rectangle_setRY(Rectangle_t * r, const RelAbsVector_t* rY)
{
  return (r != NULL) ? r->setRY(*rY) : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "x" element of this Rectangle_t.
 */
LIBSBML_EXTERN
int
Rectangle_unsetX(Rectangle_t * r)
{
  return (r != NULL) ? r->unsetX() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "y" element of this Rectangle_t.
 */
LIBSBML_EXTERN
int
Rectangle_unsetY(Rectangle_t * r)
{
  return (r != NULL) ? r->unsetY() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "z" element of this Rectangle_t.
 */
LIBSBML_EXTERN
int
Rectangle_unsetZ(Rectangle_t * r)
{
  return (r != NULL) ? r->unsetZ() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "width" element of this Rectangle_t.
 */
LIBSBML_EXTERN
int
Rectangle_unsetWidth(Rectangle_t * r)
{
  return (r != NULL) ? r->unsetWidth() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "height" element of this Rectangle_t.
 */
LIBSBML_EXTERN
int
Rectangle_unsetHeight(Rectangle_t * r)
{
  return (r != NULL) ? r->unsetHeight() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "rX" element of this Rectangle_t.
 */
LIBSBML_EXTERN
int
Rectangle_unsetRX(Rectangle_t * r)
{
  return (r != NULL) ? r->unsetRX() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "rY" element of this Rectangle_t.
 */
LIBSBML_EXTERN
int
Rectangle_unsetRY(Rectangle_t * r)
{
  return (r != NULL) ? r->unsetRY() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * Rectangle_t object have been set.
 */
LIBSBML_EXTERN
int
Rectangle_hasRequiredAttributes(const Rectangle_t * r)
{
  return (r != NULL) ? static_cast<int>(r->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


