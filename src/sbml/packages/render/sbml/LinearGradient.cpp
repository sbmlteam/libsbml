/**
 * @file    LinearGradient.cpp
 * @brief   class representing a linear gradient
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

#include "LinearGradient.h"
#ifndef OMIT_DEPRECATED
#ifdef DEPRECATION_WARNINGS
#include <iostream>
#endif // DEPRECATION_WARNINGS
#endif // OMIT_DEPRECATED
#include <sbml/packages/layout/util/LayoutAnnotation.h>
#include <sbml/packages/layout/util/LayoutUtilities.h>
#include <sbml/packages/render/extension/RenderExtension.h>

LIBSBML_CPP_NAMESPACE_BEGIN

const std::string LinearGradient::ELEMENT_NAME="linearGradient";

/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new LinearGradient object with the given SBML level
 * and SBML version.
 *
 * @param level SBML level of the new object
 * @param level SBML version of the new object
 */
LinearGradient::LinearGradient (unsigned int level, unsigned int version, unsigned int pkgVersion) : 
    GradientBase(level,version, pkgVersion)
    ,mX1(RelAbsVector(0.0,0.0))
    ,mY1(RelAbsVector(0.0,0.0))
    ,mZ1(RelAbsVector(0.0,0.0))
    ,mX2(RelAbsVector(0.0,100.0))
    ,mY2(RelAbsVector(0.0,100.0))
    ,mZ2(RelAbsVector(0.0,100.0))
{
    if (!hasValidLevelVersionNamespaceCombination())
        throw SBMLConstructorException();
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new LinearGradient object with the given SBMLNamespaces.
 *
 * @param sbmlns The SBML namespace for the object.
 */
LinearGradient::LinearGradient (RenderPkgNamespaces* renderns):
    GradientBase(renderns)
    ,mX1(RelAbsVector(0.0,0.0))
    ,mY1(RelAbsVector(0.0,0.0))
    ,mZ1(RelAbsVector(0.0,0.0))
    ,mX2(RelAbsVector(0.0,100.0))
    ,mY2(RelAbsVector(0.0,100.0))
    ,mZ2(RelAbsVector(0.0,100.0))
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
 * Creates a new LinearGradient object from the given XMLNode object.
 * The XMLNode object has to contain a valid XML representation of a 
 * LinearGradient object as defined in the render extension specification.
 * This method is normally called when render information is read from a file and 
 * should normally not have to be called explicitely.
 *
 * @param node the XMLNode object reference that describes the LinearGradient
 * object to be instantiated.
 */
LinearGradient::LinearGradient(const XMLNode& node, unsigned int l2version):GradientBase(node, l2version)
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
LinearGradient::~LinearGradient ()
{
}


#ifndef OMIT_DEPRECATED
/** @cond doxygenLibsbmlInternal */
/*
 * Constructor which creates a LinearGradient with no gradient stops.
 * The id is set to the given value.
 * The LinearGradient object is invalid until it has an id and at least two 
 * gradient stops.
 * The start and the end of the linear gradient vector are set to (0,0,0).
 * A linear gradient with a vector of length zero should also be considered invalid.
 *
 * @param id the new id for the LinearGradient.
 *
 * This constructor is deprecated. The new libsbml API only has
 * constructors which take the SBML level and version or one that takes
 * an SBMLNamespaces object.
 */
    LinearGradient::LinearGradient(RenderPkgNamespaces* renderns, const std::string& id)
    :GradientBase(renderns, id)
    ,mX1(RelAbsVector(0.0,0.0))
    ,mY1(RelAbsVector(0.0,0.0))
    ,mZ1(RelAbsVector(0.0,0.0))
    ,mX2(RelAbsVector(0.0,100.0))
    ,mY2(RelAbsVector(0.0,100.0))
     ,mZ2(RelAbsVector(0.0,100.0))
{
#ifdef DEPRECATION_WARNINGS
    std::cerr << "Warning. LinearGradient::LinearGradient(const std::string& id) is deprecated." << std::endl;
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
 * Sets the 3D coordinates for the start and the end point of the linear gradient vector.
 * Each value can be a combination of absolute and relative value and is represented by 
 * a RelAbsVector object.
 *
 * @param x1 x value of the start point of the linear gradient vector
 * @param y1 y value of the start point of the linear gradient vector
 * @param z1 z value of the start point of the linear gradient vector
 * @param x2 x value of the end point of the linear gradient vector
 * @param y2 y value of the end point of the linear gradient vector
 * @param z2 z value of the end point of the linear gradient vector
 */
void LinearGradient::setCoordinates(const RelAbsVector& x1,const RelAbsVector& y1,const RelAbsVector& z1
        ,const RelAbsVector& x2,const RelAbsVector& y2,const RelAbsVector& z2)
{
    this->setPoint1(x1,y1,z1);
    this->setPoint2(x2,y2,z2);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the 2D coordinates for the start and the end point of the linear gradient vector.
 * The z values are automatically set to 0.
 * Each value can be a combination of absolute and relative value and is represented by 
 * a RelAbsVector object.
 *
 * @param x1 x value of the start point of the linear gradient vector
 * @param y1 y value of the start point of the linear gradient vector
 * @param x2 x value of the end point of the linear gradient vector
 * @param y2 y value of the end point of the linear gradient vector
 */
void LinearGradient::setCoordinates(const RelAbsVector& x1,const RelAbsVector& y1,const RelAbsVector& x2,const RelAbsVector& y2)
{
    this->setCoordinates(x1,y1,RelAbsVector(0.0,0.0),x2,y2,RelAbsVector(0.0,100.0));
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the coordinates for the start point of the linear gradient vector.
 *
 * Each value can be a combination of absolute and relative value and is represented by 
 * a RelAbsVector object.
 *
 * The z value can be omitted. In that case it is set to 0.
 *
 * @param x x value of the start point of the linear gradient vector
 * @param y y value of the start point of the linear gradient vector
 * @param z z value of the start point of the linear gradient vector
 *
 */
void LinearGradient::setPoint1(const RelAbsVector& x,const RelAbsVector& y,const RelAbsVector& z)
{
    this->mX1=x;
    this->mY1=y;
    this->mZ1=z;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the coordinates for the end point of the linear gradient vector.
 *
 * Each value can be a combination of absolute and relative value and is represented by 
 * a RelAbsVector object.
 *
 * The z value can be omitted. In that case it is set to 0.
 *
 * @param x x value of the end point of the linear gradient vector
 * @param y y value of the end point of the linear gradient vector
 * @param z z value of the end point of the linear gradient vector
 *
 */
void LinearGradient::setPoint2(const RelAbsVector& x,const RelAbsVector& y,const RelAbsVector& z)
{
    this->mX2=x;
    this->mY2=y;
    this->mZ2=z;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the x coordinate for the start point as a const reference.
 *
 * @return RelAbsVector that represents the x value of the start point.
 */
const RelAbsVector& LinearGradient::getXPoint1() const
{
    return this->mX1;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the y coordinate for the start point as a const reference.
 *
 * @return RelAbsVector that represents the y value of the start point.
 */
const RelAbsVector& LinearGradient::getYPoint1() const
{
    return this->mY1;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the z coordinate for the start point as a const reference.
 *
 * @return RelAbsVector that represents the z value of the start point.
 */
const RelAbsVector& LinearGradient::getZPoint1() const
{
    return this->mZ1;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the x coordinate for the end point as a const reference.
 *
 * @return RelAbsVector that represents the x value of the start point.
 */
const RelAbsVector& LinearGradient::getXPoint2() const
{
    return this->mX2;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the y coordinate for the end point as a const reference.
 *
 * @return RelAbsVector that represents the y value of the start point.
 */
const RelAbsVector& LinearGradient::getYPoint2() const
{
    return this->mY2;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the z coordinate for the end point as a const reference.
 *
 * @return RelAbsVector that represents the z value of the start point.
 */
const RelAbsVector& LinearGradient::getZPoint2() const
{
    return this->mZ2;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the x coordinate for the start point as a reference.
 *
 * @return RelAbsVector that represents the x value of the start point.
 */
RelAbsVector& LinearGradient::getXPoint1()
{
    return this->mX1;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the y coordinate for the start point as a reference.
 *
 * @return RelAbsVector that represents the y value of the start point.
 */
RelAbsVector& LinearGradient::getYPoint1()
{
    return this->mY1;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the z coordinate for the start point as a reference.
 *
 * @return RelAbsVector that represents the z value of the start point.
 */
RelAbsVector& LinearGradient::getZPoint1()
{
    return this->mZ1;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the x coordinate for the end point as a reference.
 *
 * @return RelAbsVector that represents the x value of the start point.
 */
RelAbsVector& LinearGradient::getXPoint2()
{
    return this->mX2;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the y coordinate for the end point as a reference.
 *
 * @return RelAbsVector that represents the y value of the start point.
 */
RelAbsVector& LinearGradient::getYPoint2()
{
    return this->mY2;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the z coordinate for the end point as a reference.
 *
 * @return RelAbsVector that represents the z value of the start point.
 */
RelAbsVector& LinearGradient::getZPoint2()
{
    return this->mZ2;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
void
LinearGradient::addExpectedAttributes(ExpectedAttributes& attributes)
{
  GradientBase::addExpectedAttributes(attributes);

  attributes.add("x1");
  attributes.add("y1");
  attributes.add("z1");
  attributes.add("x2");
  attributes.add("y2");
  attributes.add("z2");
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
void LinearGradient::readAttributes (const XMLAttributes& attributes, const ExpectedAttributes& expectedAttributes)
{
    this->GradientBase::readAttributes(attributes, expectedAttributes);
    std::string s;
    std::string delim(" \t\r\n");
    if(!attributes.readInto("x1",s, getErrorLog(), false, getLine(), getColumn()) || s.find_first_not_of(delim)==std::string::npos)
    {
        // default 0%
        this->mX1=RelAbsVector(0.0,0.0);   
    }
    else
    {
        this->mX1=RelAbsVector(s);
    }
    if(!attributes.readInto("y1",s, getErrorLog(), false, getLine(), getColumn()) || s.find_first_not_of(delim)==std::string::npos)
    {
        // default 0%
        this->mY1=RelAbsVector(0.0,0.0);   
    }
    else
    {
        this->mY1=RelAbsVector(s);
    }
    if(!attributes.readInto("z1", s, getErrorLog(), false, getLine(), getColumn()) || s.find_first_not_of(delim)==std::string::npos)
    {
        // default 0%
        this->mZ1=RelAbsVector(0.0,0.0);   
    }
    else
    {
        this->mZ1=RelAbsVector(s);
    }
    if(!attributes.readInto("x2",s, getErrorLog(), false, getLine(), getColumn()) || s.find_first_not_of(delim)==std::string::npos)
    {
        // default 100%
        this->mX2=RelAbsVector(0.0,100.0);   
    }
    else
    {
        this->mX2=RelAbsVector(s);
    }
    if(!attributes.readInto("y2",s, getErrorLog(), false, getLine(), getColumn()) || s.find_first_not_of(delim)==std::string::npos)
    {
        // default 100%
        this->mY2=RelAbsVector(0.0,100.0);   
    }
    else
    {
        this->mY2=RelAbsVector(s);
    }
    if(!attributes.readInto("z2", s, getErrorLog(), false, getLine(), getColumn()) || s.find_first_not_of(delim)==std::string::npos)
    {
        // default 100%
        this->mZ2=RelAbsVector(0.0,100.0);   
    }
    else
    {
        this->mZ2=RelAbsVector(s);
    }
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
int LinearGradient::getTypeCode() const
{
    return SBML_RENDER_LINEARGRADIENT;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Accepts the given SBMLVisitor for this instance of LinearGradient.
 *
 * @param v the SBMLVisitor instance to be used.
 *
 * @return the result of calling <code>v.visit()</code>.
 */
bool LinearGradient::accept(SBMLVisitor& /*visitor*/) const
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
const std::string& LinearGradient::getElementName() const
{
  static std::string name = LinearGradient::ELEMENT_NAME;
  return name;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Creates and returns a deep copy of this LinearGradient object.
 * 
 * @return a (deep) copy of this LinearGradient object
 */
LinearGradient* LinearGradient::clone() const
{
    return new LinearGradient(*this);
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
void LinearGradient::writeAttributes (XMLOutputStream& stream) const
{
    GradientBase::writeAttributes(stream);
    std::ostringstream os;
    RelAbsVector tmp(0.0,0.0);
    if(this->mX1 != tmp)
    {
        os << this->mX1;
        stream.writeAttribute("x1", getPrefix(), os.str());
    }
    os.str("");
    if(this->mY1 != tmp)
    {
        os << this->mY1;
        stream.writeAttribute("y1", getPrefix(), os.str());
    }
    if(this->mZ1!=tmp)
    {
        os.str("");
        os << this->mZ1;
        stream.writeAttribute("z1", getPrefix(), os.str());
    }
    tmp=RelAbsVector(0.0,100.0);
    os.str("");
    if(this->mX2 != tmp)
    {
        os << this->mX2;
        stream.writeAttribute("x2", getPrefix(), os.str());
    }
    os.str("");
    if(this->mY2 != tmp)
    {
        os << this->mY2;
        stream.writeAttribute("y2", getPrefix(), os.str());
    }
    if(this->mZ2!=tmp)
    {
        os.str("");
        os << this->mZ2;
        stream.writeAttribute("z2", getPrefix(), os.str());
    }
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Creates an XMLNode object from this LinearGradient object.
 *
 * @return the XMLNode with the XML representation for the 
 * LinearGradient object.
 */
XMLNode LinearGradient::toXML() const
{
  return getXmlNodeForSBase(this);
}
/** @endcond */

LIBSBML_CPP_NAMESPACE_END  
