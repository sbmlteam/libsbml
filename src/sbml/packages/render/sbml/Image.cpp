/**
 * @file    Image.cpp
 * @brief   class representing a bitmap image
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

#include "Image.h"
#include <sbml/packages/layout/util/LayoutAnnotation.h>
#include <sbml/packages/layout/util/LayoutUtilities.h>
#include <sbml/packages/render/extension/RenderExtension.h>
#ifndef OMIT_DEPRECATED
#ifdef DEPRECATION_WARNINGS
#include <iostream>
#endif // DEPRECATION_WARNINGS
#endif // OMIT_DEPRECATED

LIBSBML_CPP_NAMESPACE_BEGIN

const std::string Image::ELEMENT_NAME="image";

/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new Image object with the given SBML level
 * and SBML version.
 *
 * @param level SBML level of the new object
 * @param level SBML version of the new object
 */
Image::Image (unsigned int level, unsigned int version, unsigned int pkgVersion) : 
    Transformation2D(level,version, pkgVersion)
////    ,mId("")
    ,mX(RelAbsVector(0.0,0.0))
    ,mY(RelAbsVector(0.0,0.0))
    ,mZ(RelAbsVector(0.0,0.0))
    ,mWidth(RelAbsVector(0.0,0.0))
    ,mHeight(RelAbsVector(0.0,0.0))
    ,mHRef("")
{
    if (!hasValidLevelVersionNamespaceCombination())
        throw SBMLConstructorException();
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new Image object with the given SBMLNamespaces.
 *
 * @param sbmlns The SBML namespace for the object.
 */
Image::Image (RenderPkgNamespaces* renderns):
    Transformation2D(renderns)
////    ,mId("")
    ,mX(RelAbsVector(0.0,0.0))
    ,mY(RelAbsVector(0.0,0.0))
    ,mZ(RelAbsVector(0.0,0.0))
    ,mWidth(RelAbsVector(0.0,0.0))
    ,mHeight(RelAbsVector(0.0,0.0))
    ,mHRef("")
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
 * Creates a new Image object from the given XMLNode object.
 * The XMLNode object has to contain a valid XML representation of a 
 * Image object as defined in the render extension specification.
 * This method is normally called when render information is read from a file and 
 * should normally not have to be called explicitely.
 *
 * @param node the XMLNode object reference that describes the Image
 * object to be instantiated.
 */
Image::Image(const XMLNode& node, unsigned int l2version):Transformation2D(node, l2version)
////    ,mId("")
    ,mX(RelAbsVector(0.0,0.0))
    ,mY(RelAbsVector(0.0,0.0))
    ,mZ(RelAbsVector(0.0,0.0))
    ,mWidth(RelAbsVector(0.0,0.0))
                                  ,mHeight(RelAbsVector(0.0,0.0))
                                  ,mHRef("")
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
Image::~Image ()
{
}



/** @cond doxygenLibsbmlInternal */
void
Image::addExpectedAttributes(ExpectedAttributes& attributes)
{
  Transformation2D::addExpectedAttributes(attributes);

  attributes.add("id");
  attributes.add("x");
  attributes.add("y");
  attributes.add("z");
  attributes.add("width");
  attributes.add("height");
  attributes.add("href");
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
void Image::readAttributes (const XMLAttributes& attributes, const ExpectedAttributes& expectedAttributes)
{
    this->Transformation2D::readAttributes(attributes, expectedAttributes);
    std::string s;
    attributes.readInto("id", s, getErrorLog(), false, getLine(), getColumn());
    this->setId(s);
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
    attributes.readInto("href", this->mHRef, getErrorLog(), false, getLine(), getColumn());
}
/** @endcond */



#ifndef OMIT_DEPRECATED
/** @cond doxygenLibsbmlInternal */
/*
 * Instantiates an Image object with the given @p id.
 * The image reference is unset, the position and the dimensions
 * values of the image are set to 0.
 *
 * For the image to be valid, the reference has to be set and it has to 
 * have dimensions different from and larger than 0.
 *
 * This constructor is deprecated. The new libsbml API only has
 * constructors which take the SBML level and version or one that takes
 * an SBMLNamespaces object.
 */
    Image::Image(RenderPkgNamespaces* renderns, const std::string& id)
    :Transformation2D(renderns)
//    ,mId(id)
    ,mX(RelAbsVector(0.0,0.0))
    ,mY(RelAbsVector(0.0,0.0))
    ,mZ(RelAbsVector(0.0,0.0))
    ,mWidth(RelAbsVector(0.0,0.0))
     ,mHeight(RelAbsVector(0.0,0.0))
     ,mHRef("")
{
#ifdef DEPRECATION_WARNINGS
    std::cerr << "Warning. Image::Image(const std::string& id) is deprecated." << std::endl;
#endif // DEPRECATION_WARNINGS
  setId(id);

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
 * Sets the dimensions of the image.
 * The dimensions can be set as relative values or absolute values, or 
 * a combination of both.
 *
 * @param width the width of the image when rendered
 * @param height the height of the image when rendered
 */
void Image::setDimensions(const RelAbsVector& width,const RelAbsVector& height)
{
    this->mWidth=width;
    this->mHeight=height;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the width of the image when rendered.
 * The width can be set as relative values or absolute values, or 
 * a combination of both.
 *
 * @param width the width of the image when rendered
 */
void Image::setWidth(const RelAbsVector& width)
{
    this->mWidth=width;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the height of the image when rendered.
 * The height can be set as relative values or absolute values, or 
 * a combination of both.
 *
 * @param height the height of the image when rendered
 */
void Image::setHeight(const RelAbsVector& height)
{
    this->mHeight=height;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns a reference to the width of the image.
 *
 * @return reference to the width
 */
RelAbsVector& Image::getWidth()
{
    return this->mWidth;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns a reference to the height of the image.
 *
 * @return reference to the height
 */
RelAbsVector& Image::getHeight()
{
    return this->mHeight;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns a const reference to the width of the image.
 *
 * @return const reference to the width
 */
const RelAbsVector& Image::getWidth() const
{
    return this->mWidth;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns a const reference to the height of the image.
 *
 * @return const reference to the height
 */
const RelAbsVector& Image::getHeight() const
{
    return this->mHeight;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Sets the reference to the image location.
 * Relative paths are relative to the document that contains the render information.
 * The path should be the location to a JPEG or PNG bitmap image, other formats are
 * currently not supported.
 *
 * @param ref A URL string that specifies where the image is located on the disk.
 */
void Image::setImageReference(const std::string& ref)
{
    this->mHRef=ref;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the image reference URL string.
 *
 * @return THe path to the image data as a string.
 */
const std::string& Image::getImageReference() const
{
    return this->mHRef;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Sets the position of the image relative to its viewport.
 * The position can either be specified in relative or in absolut coordinates
 * or a combination of both.
 * The z coordinatee can be omitted. In that case it is set to 0.
 *
 * @param x x coordinate of the image position
 * @param y y coordinate of the image position
 * @param z z coordinate of the image position
 */
void Image::setCoordinates(const RelAbsVector& x,const RelAbsVector& y,const RelAbsVector& z)
{
    this->mX=x;
    this->mY=y;
    this->mZ=z;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the x coordinate of the image position.
 * The position can either be specified in relative or in absolut coordinates
 * or a combination of both.
 *
 * @param x x coordinate of the image position
 */
void Image::setX(const RelAbsVector& coord)
{
    this->mX=coord;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the y coordinate of the image position.
 * The position can either be specified in relative or in absolut coordinates
 * or a combination of both.
 *
 * @param y y coordinate of the image position
 */
void Image::setY(const RelAbsVector& coord)
{
    this->mY=coord;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the z coordinate of the image position.
 * The position can either be specified in relative or in absolut coordinates
 * or a combination of both.
 *
 * @param z z coordinate of the image position
 */
void Image::setZ(const RelAbsVector& coord)
{
    this->mZ=coord;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns a const reference to the x coordinate of the image position.
 *
 * @return const reference to the x coordinate of the image position.
 */
const RelAbsVector& Image::getX() const
{
    return this->mX;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns a const reference to the y coordinate of the image position.
 *
 * @return const reference to the y coordinate of the image position.
 */
const RelAbsVector& Image::getY() const
{
    return this->mY;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns a const reference to the z coordinate of the image position.
 *
 * @return const reference to the z coordinate of the image position.
 */
const RelAbsVector& Image::getZ() const
{
    return this->mZ;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns a reference to the x coordinate of the image position.
 *
 * @return reference to the x coordinate of the image position.
 */
RelAbsVector& Image::getX()
{
    return this->mX;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns a reference to the y coordinate of the image position.
 *
 * @return reference to the y coordinate of the image position.
 */
RelAbsVector& Image::getY()
{
    return this->mY;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns a reference to the z coordinate of the image position.
 *
 * @return reference to the z coordinate of the image position.
 */
RelAbsVector& Image::getZ()
{
    return this->mZ;
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
int Image::getTypeCode() const
{
    return SBML_RENDER_IMAGE;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Accepts the given SBMLVisitor.
 *
 * @return the result of calling <code>v.visit()</code>, which indicates
 * whether or not the Visitor would like to visit the SBML object's next
 * sibling object (if available).
 */
bool Image::accept(SBMLVisitor& /*visitor*/) const
{
    return false;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the XML element name of this object, which for
 * Image, is always @c "image".
 * 
 * @return the name of this element, i.e., @c "image".
 */
const std::string& Image::getElementName() const
{
  static std::string name = Image::ELEMENT_NAME;
  return name;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Creates and returns a deep copy of this Image object.
 *
 * @return a (deep) copy of this Image.
 */
Image* Image::clone() const
{
    return new Image(*this);
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
void Image::writeAttributes (XMLOutputStream& stream) const
{
    Transformation2D::writeAttributes(stream);
    if(this->isSetId())
    {
        stream.writeAttribute("id", getPrefix(), this->getId());
    }
    std::ostringstream os;
    os << this->mX;
    stream.writeAttribute("x", getPrefix(), os.str());
    os.str("");
    os << this->mY;
    stream.writeAttribute("y", getPrefix(), os.str());
    RelAbsVector tmp(0.0,0.0);
    if(this->mZ!=tmp)
    {
        os.str("");
        os << this->mZ;
        stream.writeAttribute("z", getPrefix(), os.str());
    }
    os.str("");
    os << this->mWidth;
    stream.writeAttribute("width", getPrefix(), os.str());
    os.str("");
    os << this->mHeight;
    stream.writeAttribute("height", getPrefix(), os.str());
    stream.writeAttribute("href", getPrefix(), mHRef);
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Creates an XMLNode object from this Image object.
 *
 * @return the XMLNode with the XML representation for the 
 * Image object.
 */
XMLNode Image::toXML() const
{
  return getXmlNodeForSBase(this);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns true if the image reference has been set.
 * The image reference is considered set if the string does not
 * only contain whitespace characters.
 *
 * @return true if the image reference has been set.
 */
bool Image::isSetImageReference() const
{
    std::string space=" \t\n\r";
    // the string may not be empty and it may not only contain whitespaces
    return (!this->mHRef.empty() && (this->mHRef.find_first_not_of(space)!=std::string::npos));
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the value of the "id" attribute of this Image.
 *
 * @return the id of the Image
 */
const std::string& Image::getId () const
{
    return mId;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Predicate returning @c true or @c false depending on whether this
 * Image's "id" attribute has been set.
 *
 * @return returns true or false depending on whether the id on the 
 * Image has been set.
 */
bool Image::isSetId () const
{
    return (mId.empty() == false);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the value of the "id" attribute of this Image.
 *
 * @param id the new id for the Image 
 *
 * @return status if the operation succeeded
 */
int Image::setId (const std::string& id)
{
    if (!(SyntaxChecker::isValidSBMLSId(id)))
    {
        return LIBSBML_INVALID_ATTRIBUTE_VALUE;
    }
    else
    {
        mId = id;
        return LIBSBML_OPERATION_SUCCESS;
    }
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Unsets the value of the "id" attribute of this Image.
 */
int Image::unsetId ()
{
    mId.erase();
    if (mId.empty())
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/* function returns true if component has all the required
 * attributes
 */
bool Image::hasRequiredAttributes() const
{
    bool result = this->Transformation2D::hasRequiredAttributes();
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
    result = result && (this->mHRef.find_first_not_of(" \n\r\t") != std::string::npos);
    return result;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/* function returns true if component has all the required
 * elements
 */
bool Image::hasRequiredElements() const 
{
    bool result = this->Transformation2D::hasRequiredElements();
    return result;
}
/** @endcond */


LIBSBML_CPP_NAMESPACE_END 
