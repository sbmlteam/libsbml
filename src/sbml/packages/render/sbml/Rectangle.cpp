/**
 * @file    Rectangle.cpp
 * @brief   class for representing a rectangle with or
 *          without rounded corners
 * @author  Ralph Gauges
 * @author  Frank T. Bergmann
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
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

#include "Rectangle.h"
#include <sbml/packages/layout/util/LayoutAnnotation.h>
#include <sbml/packages/layout/util/LayoutUtilities.h>
#include <sbml/packages/render/extension/RenderExtension.h>
#ifndef OMIT_DEPRECATED
#ifdef DEPRECATION_WARNINGS
#include <iostream>
#endif // DEPRECATION_WARNINGS
#endif // OMIT_DEPRECATED

LIBSBML_CPP_NAMESPACE_BEGIN

const std::string Rectangle::ELEMENT_NAME="rectangle";

/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new Rectangle object with the given SBML level
 * and SBML version.
 *
 * @param level SBML level of the new object
 * @param level SBML version of the new object
 */
Rectangle::Rectangle (unsigned int level, unsigned int version, unsigned int pkgVersion) 
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
    if (!hasValidLevelVersionNamespaceCombination())
        throw SBMLConstructorException();
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new Rectangle object with the given SBMLNamespaces.
 *
 * @param sbmlns The SBML namespace for the object.
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
    if (!hasValidLevelVersionNamespaceCombination())
        throw SBMLConstructorException();
        // set the element namespace of this object
  setElementNamespace(renderns->getURI());

  // connect child elements to this element.
  connectToChild();

  // load package extensions bound with this object (if any) 
  loadPlugins(renderns);
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new Rectangle object from the given XMLNode object.
 * The XMLNode object has to contain a valid XML representation of a 
 * Rectangle object as defined in the render extension specification.
 * This method is normally called when render information is read from a file and 
 * should normally not have to be called explicitely.
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


/*
 * Destroy this object.
 */
Rectangle::~Rectangle ()
{
}



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
void Rectangle::readAttributes (const XMLAttributes& attributes, const ExpectedAttributes& expectedAttributes)
{
  this->GraphicalPrimitive2D::readAttributes(attributes, expectedAttributes);
    bool rxSet=false;;  
    std::string s;
    attributes.readInto("x", s, getErrorLog(), false, getLine(), getColumn());
    this->mX=RelAbsVector(s);
    attributes.readInto("y", s, getErrorLog(), false, getLine(), getColumn());
    this->mY=RelAbsVector(s);
    if(attributes.readInto("z", s, getErrorLog(), false, getLine(), getColumn()))
    {
        this->mZ=RelAbsVector(s);
    }
    else
    {
        this->mZ=RelAbsVector(0.0,0.0);
    }
    attributes.readInto("width", s, getErrorLog(), false, getLine(), getColumn());
    this->mWidth=RelAbsVector(s);
    attributes.readInto("height", s, getErrorLog(), false, getLine(), getColumn());
    this->mHeight=RelAbsVector(s);
    if(attributes.readInto("rx", s, getErrorLog(), false, getLine(), getColumn()))
    {
        rxSet=true;  
        this->mRX=RelAbsVector(s);
    }
    else
    {
        this->mRX=RelAbsVector(0.0,0.0);
    }
    if(attributes.readInto("ry", s, getErrorLog(), false, getLine(), getColumn()))
    {
        this->mRY=RelAbsVector(s);
        if(!rxSet)
        {
            this->mRX=this->mRY;
        }
    }
    else
    {
        if(rxSet)
        {
            this->mRY=this->mRX;
        }
        else
        {
            this->mRY=RelAbsVector(0.0,0.0);
        }
    }

    mIsSetRatio = attributes.readInto("ratio", mRatio);
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



/** @cond doxygenLibsbmlInternal */
/*
 * Returns the x coordinate of the rectangles position
 *
 * @return const reference to RelAbsVector that represents the x position
 */
const RelAbsVector& Rectangle::getX() const
{
    return this->mX;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the y coordinate of the rectangles position
 *
 * @return const reference to RelAbsVector that represents the y position
 */
const RelAbsVector& Rectangle::getY() const
{
    return this->mY;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the z coordinate of the rectangles position
 *
 * @return const reference to RelAbsVector that represents the z position
 */
const RelAbsVector& Rectangle::getZ() const
{
    return this->mZ;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the with of the rectangle
 *
 * @return const reference to the RelAbsVector that represents the width
 */
const RelAbsVector& Rectangle::getWidth() const
{
    return this->mWidth;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the height of the rectangle
 *
 * @return const reference to the RelAbsVector that represents the height
 */
const RelAbsVector& Rectangle::getHeight() const
{
    return this->mHeight;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the corner radius along the x axis
 *
 * @return const reference to the RelAbsVector that corner radius along the x axis
 */
const RelAbsVector& Rectangle::getRadiusX() const
{
    return this->mRX;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Returns the corner radius along the y axis
 *
 * @return const reference to the RelAbsVector that corner radius along the y axis
 */
const RelAbsVector& Rectangle::getRadiusY() const
{
    return this->mRY;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the x coordinate of the rectangles position
 *
 * @return reference to RelAbsVector that represents the x position
 */
RelAbsVector& Rectangle::getX()
{
    return this->mX;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the y coordinate of the rectangles position
 *
 * @return reference to RelAbsVector that represents the y position
 */
RelAbsVector& Rectangle::getY()
{
    return this->mY;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the z coordinate of the rectangles position
 *
 * @return reference to RelAbsVector that represents the z position
 */
RelAbsVector& Rectangle::getZ()
{
    return this->mZ;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the with of the rectangle
 *
 * @return reference to the RelAbsVector that represents the width
 */
RelAbsVector& Rectangle::getWidth()
{
    return this->mWidth;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the height of the rectangle
 *
 * @return reference to the RelAbsVector that represents the height
 */
RelAbsVector& Rectangle::getHeight()
{
    return this->mHeight;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the corner radius along the x axis
 *
 * @return reference to the RelAbsVector that corner radius along the x axis
 */
RelAbsVector& Rectangle::getRadiusX()
{
    return this->mRX;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Returns the corner radius along the y axis
 *
 * @return reference to the RelAbsVector that corner radius along the y axis
 */
RelAbsVector& Rectangle::getRadiusY()
{
    return this->mRY;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the libSBML type code for this %SBML object.
 * 
 * @if clike LibSBML attaches an identifying code to every
 * kind of SBML object.  These are known as <em>SBML type codes</em>.
 * The set of possible type codes is defined in the enumeration
 * #SBMLTypeCode_t.  The names of the type codes all begin with the
 * characters @c SBML_. @endif@if java LibSBML attaches an
 * identifying code to every kind of SBML object.  These are known as
 * <em>SBML type codes</em>.  In other languages, the set of type codes
 * is stored in an enumeration; in the Java language interface for
 * libSBML, the type codes are defined as static integer constants in
 * interface class {@link libsbmlConstants}.  The names of the type codes
 * all begin with the characters @c SBML_. @endif
 *
 * @return the SBML type code for this object, or @c SBML_UNKNOWN (default).
 *
 * @see getElementName()
 */
int Rectangle::getTypeCode() const
{
    return SBML_RENDER_RECTANGLE;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Accepts the given SBMLVisitor for this instance of Rectangle.
 *
 * @param v the SBMLVisitor instance to be used.
 *
 * @return the result of calling <code>v.visit()</code>.
 */
bool Rectangle::accept(SBMLVisitor& /*visitor*/) const
{
    return false;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the XML element name of this object.
 *
 * This is overridden by subclasses to return a string appropriate to the
 * SBML component.  For example, Model defines it as returning "model",
 * CompartmentType defines it as returning "compartmentType", etc.
 */
const std::string& Rectangle::getElementName() const
{
  static std::string name = Rectangle::ELEMENT_NAME;
  return name;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Creates and returns a deep copy of this Rectangle object.
 * 
 * @return a (deep) copy of this Rectangle object
 */
Rectangle* Rectangle::clone() const
{
    return new Rectangle(*this);
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
void Rectangle::writeAttributes (XMLOutputStream& stream) const
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
    RelAbsVector tmp(0.0,0.0);
    if(this->mZ!=tmp)
    {
        os.str("");
        os << this->mZ;
        stream.writeAttribute("z", getPrefix(), os.str());
    }
    if(this->mRX!=tmp)
    {
        os.str("");
        os << this->mRX;
        stream.writeAttribute("rx", getPrefix(), os.str());
    }
    if(this->mRY!=tmp)
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
/*
 * Sets the siwidth of the Rectangle 
 *
 * @param w w width
 */
void Rectangle::setWidth(const RelAbsVector& w)
{
    this->mWidth=w;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Sets the height of the Rectangle 
 *
 * @param h h height
 */
void Rectangle::setHeight(const RelAbsVector& h)
{
    this->mHeight=h;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the corner radius along the x axis
 *
 * @param rx corner radius along the x axis
 */
void Rectangle::setRadiusX(const RelAbsVector& rx)
{
    this->mRX=rx;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the corner radius along the y axis
 *
 * @param ry corner radius along the y axis
 */
void Rectangle::setRadiusY(const RelAbsVector& ry)
{
    this->mRY=ry;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the x position of the Rectangle within the viewport.
 *
 * @param x x coordinate of the position 
 */
void Rectangle::setX(const RelAbsVector& x)
{
    this->mX=x;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the y position of the Rectangle within the viewport.
 *
 * @param y y coordinate of the position 
 */
void Rectangle::setY(const RelAbsVector& y)
{
    this->mY=y;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the z position of the Rectangle within the viewport.
 *
 * @param z z coordinate of the position 
 */
void Rectangle::setZ(const RelAbsVector& z)
{
    this->mZ=z;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/* function returns true if component has all the required
 * attributes
 */
bool Rectangle::hasRequiredAttributes() const
{
    bool result = this->GraphicalPrimitive2D::hasRequiredAttributes();
    // the position should not contain NaN
    result = result && 
        (this->mX.getAbsoluteValue() == this->mX.getAbsoluteValue()) &&
        (this->mX.getRelativeValue() == this->mX.getRelativeValue());
    result = result && 
        (this->mY.getAbsoluteValue() == this->mY.getAbsoluteValue()) &&
        (this->mY.getRelativeValue() == this->mY.getRelativeValue());
    result = result && 
        (this->mZ.getAbsoluteValue() == this->mZ.getAbsoluteValue()) &&
        (this->mZ.getRelativeValue() == this->mZ.getRelativeValue());
    // the dimensions should not contain NaN
    result = result && 
        (this->mWidth.getAbsoluteValue() == this->mWidth.getAbsoluteValue()) &&
        (this->mWidth.getRelativeValue() == this->mWidth.getRelativeValue());
    result = result && 
        (this->mHeight.getAbsoluteValue() == this->mHeight.getAbsoluteValue()) &&
        (this->mHeight.getRelativeValue() == this->mHeight.getRelativeValue());
    // the corner radii should not contain NaN
    result = result && 
        (this->mRX.getAbsoluteValue() == this->mRX.getAbsoluteValue()) &&
        (this->mRX.getRelativeValue() == this->mRX.getRelativeValue());
    result = result && 
        (this->mRY.getAbsoluteValue() == this->mRY.getAbsoluteValue()) &&
        (this->mRY.getRelativeValue() == this->mRY.getRelativeValue());
    return result;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/* function returns true if component has all the required
 * elements
 */
bool Rectangle::hasRequiredElements() const 
{
    bool result = this->GraphicalPrimitive2D::hasRequiredElements();
    return result;
}
/** @endcond */


LIBSBML_CPP_NAMESPACE_END 
