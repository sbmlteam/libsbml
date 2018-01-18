/**
 * @file    RenderCubicBezier.cpp
 * @brief   class for representing cubic bezier elements within a
 *          render curve.
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

#include "RenderCubicBezier.h"

#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLToken.h>
#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>
#ifndef OMIT_DEPRECATED
#ifdef DEPRECATION_WARNINGS
#include <iostream>
#endif // DEPRECATION_WARNINGS
#endif // OMIT_DEPRECATED
#include <sbml/packages/layout/util/LayoutAnnotation.h>
#include <sbml/packages/render/extension/RenderExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN


/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new RenderCubicBezier object with the given SBML level
 * and SBML version.
 *
 * @param level SBML level of the new object
 * @param level SBML version of the new object
 */
RenderCubicBezier::RenderCubicBezier (unsigned int level, unsigned int version, unsigned int pkgVersion) : 
    RenderPoint(level,version,pkgVersion)
{
    if (!hasValidLevelVersionNamespaceCombination())
        throw SBMLConstructorException();

  RenderPkgNamespaces* renderns = new RenderPkgNamespaces(level, version, pkgVersion);
  setSBMLNamespacesAndOwn(renderns);  

  connectToChild();

  loadPlugins(renderns);

}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new RenderCubicBezier object with the given SBMLNamespaces.
 *
 * @param sbmlns The SBML namespace for the object.
 */
RenderCubicBezier::RenderCubicBezier (RenderPkgNamespaces* renderns):
    RenderPoint(renderns)
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
 * Creates a CubicBezier with the given points.
 *
 * @param bp1_x x coordinatee of the first base point.
 * @param bp1_y y coordinatee of the first base point.
 * @param bp1_z z coordinatee of the first base point.
 * @param bp1_x x coordinatee of the second base point.
 * @param bp1_y y coordinatee of the second base point.
 * @param bp1_z z coordinatee of the second base point.
 * @param bp1_x x coordinatee of the end point.
 * @param bp1_y y coordinatee of the end point.
 * @param bp1_z z coordinatee of the end point.
 */
RenderCubicBezier::RenderCubicBezier (RenderPkgNamespaces* renderns, const RelAbsVector& bp1_x,const RelAbsVector& bp1_y,const RelAbsVector& bp1_z,const RelAbsVector& bp2_x,const RelAbsVector& bp2_y,const RelAbsVector& bp2_z,const RelAbsVector& end_x,const RelAbsVector& end_y,const RelAbsVector& end_z):RenderPoint(renderns, end_x,end_y,end_z),
    mBasePoint1_X(bp1_x),
    mBasePoint1_Y(bp1_y),
    mBasePoint1_Z(bp1_z),
    mBasePoint2_X(bp2_x),
    mBasePoint2_Y(bp2_y),
    mBasePoint2_Z(bp2_z)
{
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
 * Copy constructor for RenderCubicBezier objects.
 */
RenderCubicBezier::RenderCubicBezier(const RenderCubicBezier& orig):RenderPoint(orig)
{
    mBasePoint1_X=orig.mBasePoint1_X;
    mBasePoint1_Y=orig.mBasePoint1_Y;
    mBasePoint1_Z=orig.mBasePoint1_Z;
    mBasePoint2_X=orig.mBasePoint2_X;
    mBasePoint2_Y=orig.mBasePoint2_Y;
    mBasePoint2_Z=orig.mBasePoint2_Z;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Assignment operator for RenderCubicBezier objects.
 */
RenderCubicBezier& RenderCubicBezier::operator=(const RenderCubicBezier& orig)
{
    if(&orig!=this)
    {
        this->RenderPoint::operator=(orig);
        mBasePoint1_X=orig.mBasePoint1_X;
        mBasePoint1_Y=orig.mBasePoint1_Y;
        mBasePoint1_Z=orig.mBasePoint1_Z;
        mBasePoint2_X=orig.mBasePoint2_X;
        mBasePoint2_Y=orig.mBasePoint2_Y;
        mBasePoint2_Z=orig.mBasePoint2_Z;
    }
    return *this;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Comparison operator for RenderCubicBezier objects.
 */
bool RenderCubicBezier::operator==(const RenderCubicBezier& left) const
{
    return (this->RenderPoint::operator==(left) &&   
            this->mBasePoint1_X == left.mBasePoint1_X &&
            this->mBasePoint1_Y == left.mBasePoint1_Y &&
            this->mBasePoint1_Z == left.mBasePoint1_Z &&
            this->mBasePoint2_X == left.mBasePoint2_X &&
            this->mBasePoint2_Y == left.mBasePoint2_Y &&
            this->mBasePoint2_Z == left.mBasePoint2_Z
           );
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new RenderCubicBezier object from the given XMLNode object.
 * The XMLNode object has to contain a valid XML representation of a 
 * RenderCubicBezier object as defined in the render extension specification.
 * This method is normally called when render information is read from a file and 
 * should normally not have to be called explicitely.
 *
 * @param node the XMLNode object reference that describes the RenderCubicBezier
 * object to be instantiated.
 */
RenderCubicBezier::RenderCubicBezier(const XMLNode& node, unsigned int l2version)
:RenderPoint(node, l2version)
{
    const XMLAttributes& attributes=node.getAttributes();
    const XMLNode* child;
     ExpectedAttributes ea;
    addExpectedAttributes(ea);
    this->readAttributes(attributes, ea);
    unsigned int n=0,nMax = node.getNumChildren();
    while(n<nMax)
    {
        child=&node.getChild(n);
        const std::string& childName=child->getName();
        if(childName=="annotation")
        {
            this->mAnnotation=new XMLNode(node);
        }
        else if(childName=="notes")
        {
            this->mNotes=new XMLNode(node);
        }
        else
        {
            //throw;
        }
        ++n;
    }    

    
  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(2,l2version));  

  connectToChild();
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Destroys the RenderCubicBezier object.
 */ 
RenderCubicBezier::~RenderCubicBezier ()
{
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the x value of the first base point of the curve (the one closer to the
 * starting point) as a const reference.
 *
 * @return const reference to x value of first base point
 */ 
const RelAbsVector& RenderCubicBezier::basePoint1_X() const
{
    return this->mBasePoint1_X;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the y value of the first base point of the curve (the one closer to the
 * starting point) as a const reference.
 *
 * @return const reference to y value of first base point
 */ 
const RelAbsVector& RenderCubicBezier::basePoint1_Y() const
{
    return this->mBasePoint1_Y;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the z value of the first base point of the curve (the one closer to the
 * starting point) as a const reference.
 *
 * @return const reference to z value of first base point
 */ 
const RelAbsVector& RenderCubicBezier::basePoint1_Z() const
{
    return this->mBasePoint1_Z;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Returns the x value of the second base point of the curve (the one further from the
 * starting point) as a const reference.
 *
 * @return const reference to x value of second base point
 */ 
const RelAbsVector& RenderCubicBezier::basePoint2_X() const
{
    return this->mBasePoint2_X;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the y value of the second base point of the curve (the one further from the
 * starting point) as a const reference.
 *
 * @return const reference to y value of second base point
 */ 
const RelAbsVector& RenderCubicBezier::basePoint2_Y() const
{
    return this->mBasePoint2_Y;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the z value of the second base point of the curve (the one further from the
 * starting point) as a const reference.
 *
 * @return const reference to z value of second base point
 */ 
const RelAbsVector& RenderCubicBezier::basePoint2_Z() const
{
    return this->mBasePoint2_Z;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Returns the x value of the first base point of the curve (the one closer to the
 * starting point) as a reference.
 *
 * @return reference to x value of first base point
 */ 
RelAbsVector& RenderCubicBezier::basePoint1_X()
{
    return this->mBasePoint1_X;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the y value of the first base point of the curve (the one closer to the
 * starting point) as a reference.
 *
 * @return reference to y value of first base point
 */ 
RelAbsVector& RenderCubicBezier::basePoint1_Y()
{
    return this->mBasePoint1_Y;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the z value of the first base point of the curve (the one closer to the
 * starting point) as a reference.
 *
 * @return reference to z value of first base point
 */ 
RelAbsVector& RenderCubicBezier::basePoint1_Z()
{
    return this->mBasePoint1_Z;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Returns the x value of the second base point of the curve (the one further from the
 * starting point) as a reference.
 *
 * @return reference to x value of second base point
 */ 
RelAbsVector& RenderCubicBezier::basePoint2_X()
{
    return this->mBasePoint2_X;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the y value of the second base point of the curve (the one further from the
 * starting point) as a reference.
 *
 * @return reference to y value of second base point
 */ 
RelAbsVector& RenderCubicBezier::basePoint2_Y()
{
    return this->mBasePoint2_Y;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the z value of the second base point of the curve (the one further from the
 * starting point) as a reference.
 *
 * @return reference to z value of second base point
 */ 
RelAbsVector& RenderCubicBezier::basePoint2_Z()
{
    return this->mBasePoint2_Z;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Sets the x value of the first base point of the curve (the one closer to the
 * starting point).
 *
 * @param x x coordinate of first base point.
 */ 
void RenderCubicBezier::setBasePoint1_X(const RelAbsVector& v)
{
    this->mBasePoint1_X=v;;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the y value of the first base point of the curve (the one closer to the
 * starting point).
 *
 * @param y y coordinate of first base point.
 */ 
void RenderCubicBezier::setBasePoint1_Y(const RelAbsVector& v)
{
    this->mBasePoint1_Y=v;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the z value of the first base point of the curve (the one closer to the
 * starting point).
 *
 * @param z z coordinate of first base point.
 */ 
void RenderCubicBezier::setBasePoint1_Z(const RelAbsVector& v)
{
    this->mBasePoint1_Z=v;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Sets the x value of the second base point of the curve (the one further from the
 * starting point).
 *
 * @param x value of second base point.
 */ 
void RenderCubicBezier::setBasePoint2_X(const RelAbsVector& v)
{
    this->mBasePoint2_X=v;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the y value of the second base point of the curve (the one further from the
 * starting point).
 *
 * @param y value of second base point.
 */ 
void RenderCubicBezier::setBasePoint2_Y(const RelAbsVector& v)
{
    this->mBasePoint2_Y=v;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the z value of the second base point of the curve (the one further from the
 * starting point).
 *
 * @param z value of second base point.
 */ 
void RenderCubicBezier::setBasePoint2_Z(const RelAbsVector& v)
{
    this->mBasePoint2_Z=v;
}
/** @endcond */




/** @cond doxygenLibsbmlInternal */
/*
 * Returns the XML element name of this object, which for
 * RenderCubicBezier, is always @c "element".
 * 
 * @return the name of this element, i.e., @c "element".
 */
const std::string& RenderCubicBezier::getElementName () const 
{
  static std::string name = "element";
  return name;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Creates and returns a deep copy of the RenderCubicBezier object.
 *
 * @return a (deep) copy of this RenderCubicBezier
 */
RenderCubicBezier* 
RenderCubicBezier::clone () const
{
    return new RenderCubicBezier(*this);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
void
RenderCubicBezier::addExpectedAttributes(ExpectedAttributes& attributes)
{
  RenderPoint::addExpectedAttributes(attributes);

  attributes.add("basePoint1_x");
  attributes.add("basePoint1_y");
  attributes.add("basePoint1_z");
  attributes.add("basePoint2_x");
  attributes.add("basePoint2_y");
  attributes.add("basePoint2_z");
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
void RenderCubicBezier::readAttributes (const XMLAttributes& attributes, const ExpectedAttributes& expectedAttributes)
{

  RenderPoint::readAttributes(attributes, expectedAttributes);
    std::string s;
    if(attributes.readInto("basePoint1_x",s, getErrorLog(), false, getLine(), getColumn()))
    {
        this->mBasePoint1_X=RelAbsVector(s);
    }
    else
    {
        this->mBasePoint1_X=RelAbsVector(std::numeric_limits<double>::quiet_NaN(),std::numeric_limits<double>::quiet_NaN());   
    }
    if(attributes.readInto("basePoint1_y",s, getErrorLog(), false, getLine(), getColumn()))
    {
        this->mBasePoint1_Y=RelAbsVector(s);
    }
    else
    {
        this->mBasePoint1_Y=RelAbsVector(std::numeric_limits<double>::quiet_NaN(),std::numeric_limits<double>::quiet_NaN());   
    }
    if(attributes.readInto("basePoint1_z",s, getErrorLog(), false, getLine(), getColumn()))
    {
        this->mBasePoint1_Z=RelAbsVector(s);
    }
    else
    {
        this->mBasePoint1_Z=RelAbsVector(0.0,0.0);
    }
    if(attributes.readInto("basePoint2_x",s, getErrorLog(), false, getLine(), getColumn()))
    {
        this->mBasePoint2_X=RelAbsVector(s);
    }
    else
    {
        this->mBasePoint2_X=RelAbsVector(std::numeric_limits<double>::quiet_NaN(),std::numeric_limits<double>::quiet_NaN());   
    }
    if(attributes.readInto("basePoint2_y",s, getErrorLog(), false, getLine(), getColumn()))
    {
        this->mBasePoint2_Y=RelAbsVector(s);
    }
    else
    {
        this->mBasePoint2_Y=RelAbsVector(std::numeric_limits<double>::quiet_NaN(),std::numeric_limits<double>::quiet_NaN());   
    }
    if(attributes.readInto("basePoint2_z",s, getErrorLog(), false, getLine(), getColumn()))
    {
        this->mBasePoint2_Z=RelAbsVector(s);
    }
    else
    {
        this->mBasePoint2_Z=RelAbsVector(0.0,0.0);
    }
}
/** @endcond */


void 
RenderCubicBezier::writeXMLNS (XMLOutputStream& stream) const
{
  XMLNamespaces xmlns;
  xmlns.add(LayoutExtension::getXmlnsXSI(), "xsi");
  stream << xmlns;
}

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
void RenderCubicBezier::writeAttributes (XMLOutputStream& stream) const
{
    SBase::writeAttributes(stream);
    XMLTriple triple("type","","xsi");
    stream.writeAttribute(triple,std::string("RenderCubicBezier"));
    std::ostringstream os;
    os << this->mXOffset;
    stream.writeAttribute("x", getPrefix(), os.str());
    os.str("");
    os << this->mYOffset;
    stream.writeAttribute("y", getPrefix(), os.str());
    if(this->mZOffset!=RelAbsVector(0.0,0.0))
    {
        os.str("");
        os << this->mZOffset;
        stream.writeAttribute("z", getPrefix(), os.str());
    }
    os.str("");
    os << this->mBasePoint1_X;
    stream.writeAttribute("basePoint1_x", getPrefix(), os.str());
    os.str("");
    os << this->mBasePoint1_Y;
    stream.writeAttribute("basePoint1_y", getPrefix(), os.str());
    if(this->mBasePoint1_Z!=RelAbsVector(0.0,0.0))
    {
        os.str("");
        os << this->mBasePoint1_Z;
        stream.writeAttribute("basePoint1_z", getPrefix(), os.str());
    }
    os.str("");
    os << this->mBasePoint2_X;
    stream.writeAttribute("basePoint2_x", getPrefix(), os.str());
    os.str("");
    os << this->mBasePoint2_Y;
    stream.writeAttribute("basePoint2_y", getPrefix(), os.str());
    if(this->mBasePoint2_Z!=RelAbsVector(0.0,0.0))
    {
        os.str("");
        os << this->mBasePoint2_Z;
        stream.writeAttribute("basePoint2_z", getPrefix(), os.str());
    }
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the libSBML type code for the objects contained in this ListOf
 * (i.e., ColorDefinition objects, if the list is non-empty).
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
 * @return the SBML type code for the objects contained in this ListOf
 * instance, or @c SBML_UNKNOWN (default).
 *
 * @see getElementName()
 */ 
int
RenderCubicBezier::getTypeCode () const
{
    return SBML_RENDER_CUBICBEZIER;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Creates an XMLNode object from this RenderCubicBezier object.
 *
 * @return the XMLNode with the XML representation for the 
 * RenderCubicBezier object.
 */
XMLNode RenderCubicBezier::toXML(const std::string& name) const
{
  return getXmlNodeForSBase(this);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the first basepoint to the given coordinatees.
 *
 * @param x coordinate of second base point.
 * @param y coordinate of second base point.
 * @param z coordinate of second base point.
 * If the z coodinate is omitted, it is set to 0.
 */ 
void RenderCubicBezier::setBasePoint1(const RelAbsVector& x, const RelAbsVector& y, const RelAbsVector& z)
{
    this->mBasePoint1_X=x;
    this->mBasePoint1_Y=y;
    this->mBasePoint1_Z=z;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the second basepoint to the given coordinatees.
 *
 * @param x coordinate of second base point.
 * @param y coordinate of second base point.
 * @param z coordinate of second base point.
 * If the z coodinate is omitted, it is set to 0.
 */ 
void RenderCubicBezier::setBasePoint2(const RelAbsVector& x, const RelAbsVector& y, const RelAbsVector& z)
{
    this->mBasePoint2_X=x;
    this->mBasePoint2_Y=y;
    this->mBasePoint2_Z=z;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/* function returns true if component has all the required
 * attributes
 */
bool RenderCubicBezier::hasRequiredAttributes() const
{
    bool result = this->RenderPoint::hasRequiredAttributes();
    result = result && 
        (this->mBasePoint1_X.getAbsoluteValue() == this->mBasePoint1_X.getAbsoluteValue()) &&
        (this->mBasePoint1_X.getRelativeValue() == this->mBasePoint1_X.getRelativeValue());
    result = result && 
        (this->mBasePoint1_Y.getAbsoluteValue() == this->mBasePoint1_Y.getAbsoluteValue()) &&
        (this->mBasePoint1_Y.getRelativeValue() == this->mBasePoint1_Y.getRelativeValue());
    result = result && 
        (this->mBasePoint1_Z.getAbsoluteValue() == this->mBasePoint1_Z.getAbsoluteValue()) &&
        (this->mBasePoint1_Z.getRelativeValue() == this->mBasePoint1_Z.getRelativeValue());
    result = result && 
        (this->mBasePoint2_X.getAbsoluteValue() == this->mBasePoint2_X.getAbsoluteValue()) &&
        (this->mBasePoint2_X.getRelativeValue() == this->mBasePoint2_X.getRelativeValue());
    result = result && 
        (this->mBasePoint2_Y.getAbsoluteValue() == this->mBasePoint2_Y.getAbsoluteValue()) &&
        (this->mBasePoint2_Y.getRelativeValue() == this->mBasePoint2_Y.getRelativeValue());
    result = result && 
        (this->mBasePoint2_Z.getAbsoluteValue() == this->mBasePoint2_Z.getAbsoluteValue()) &&
        (this->mBasePoint2_Z.getRelativeValue() == this->mBasePoint2_Z.getRelativeValue());
    return result;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/* function returns true if component has all the required
 * elements
 */
bool RenderCubicBezier::hasRequiredElements() const 
{
    bool result = this->RenderPoint::hasRequiredElements();
    return result;
}
/** @endcond */

LIBSBML_CPP_NAMESPACE_END  
