/**
 * @file    RadialGradient.cpp
 * @brief   class for representing a radial gradient object
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

#include "RadialGradient.h"

#include <limits>
#ifndef OMIT_DEPRECATED
#ifdef DEPRECATION_WARNINGS
#include <iostream>
#endif // DEPRECATION_WARNINGS
#endif // OMIT_DEPRECATED
#include <sbml/packages/layout/util/LayoutAnnotation.h>
#include <sbml/packages/layout/util/LayoutUtilities.h>
#include <sbml/packages/render/extension/RenderExtension.h>

LIBSBML_CPP_NAMESPACE_BEGIN

const std::string RadialGradient::ELEMENT_NAME="radialGradient";

/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new RadialGradient object with the given SBML level
 * and SBML version.
 *
 * @param level SBML level of the new object
 * @param level SBML version of the new object
 */
RadialGradient::RadialGradient (unsigned int level, unsigned int version, unsigned int pkgVersion) : 
    GradientBase(level,version, pkgVersion)
    ,mCX(RelAbsVector(0.0,50.0))
    ,mCY(RelAbsVector(0.0,50.0))
    ,mCZ(RelAbsVector(0.0,50.0))
    ,mRadius(RelAbsVector(0.0,50.0))
    ,mFX(RelAbsVector(0.0,50.0))
    ,mFY(RelAbsVector(0.0,50.0))
    ,mFZ(RelAbsVector(0.0,50.0))
{
    if (!hasValidLevelVersionNamespaceCombination())
        throw SBMLConstructorException();

}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new RadialGradient object with the given SBMLNamespaces.
 *
 * @param sbmlns The SBML namespace for the object.
 */
RadialGradient::RadialGradient (RenderPkgNamespaces* renderns):
    GradientBase(renderns)
    ,mCX(RelAbsVector(0.0,50.0))
    ,mCY(RelAbsVector(0.0,50.0))
    ,mCZ(RelAbsVector(0.0,50.0))
    ,mRadius(RelAbsVector(0.0,50.0))
    ,mFX(RelAbsVector(0.0,50.0))
    ,mFY(RelAbsVector(0.0,50.0))
    ,mFZ(RelAbsVector(0.0,50.0))
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
 * Creates a new RadialGradient object from the given XMLNode object.
 * The XMLNode object has to contain a valid XML representation of a 
 * RadialGradient object as defined in the render extension specification.
 * This method is normally called when render information is read from a file and 
 * should normally not have to be called explicitely.
 *
 * @param node the XMLNode object reference that describes the RadialGradient
 * object to be instantiated.
 */
RadialGradient::RadialGradient(const XMLNode& node, unsigned int l2version):GradientBase(node, l2version)
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
RadialGradient::~RadialGradient ()
{
}



/** @cond doxygenLibsbmlInternal */
void
RadialGradient::addExpectedAttributes(ExpectedAttributes& attributes)
{
  GradientBase::addExpectedAttributes(attributes);

  attributes.add("cx");
  attributes.add("cy");
  attributes.add("cz");
  attributes.add("fx");
  attributes.add("fy");
  attributes.add("fz");
  attributes.add("r");
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
void RadialGradient::readAttributes (const XMLAttributes& attributes, const ExpectedAttributes& expectedAttributes)
{
    this->GradientBase::readAttributes(attributes, expectedAttributes);
    std::string s;
    std::string delim(" \t\r\n");
    if(!attributes.readInto("cx",s, getErrorLog(), false, getLine(), getColumn())  || s.find_first_not_of(delim)==std::string::npos)
    {
        // default 50%
        this->mCX=RelAbsVector(0.0,50.0);   
    }
    else
    { 
        this->mCX=RelAbsVector(s);
    }
    if(!attributes.readInto("cy",s, getErrorLog(), false, getLine(), getColumn())  || s.find_first_not_of(delim)==std::string::npos)
    {
        // default 50%
        this->mCY=RelAbsVector(0.0,50.0);   
    }
    else
    {
        this->mCY=RelAbsVector(s);
    }
    if(!attributes.readInto("cz", s, getErrorLog(), false, getLine(), getColumn()) || s.find_first_not_of(delim)==std::string::npos)
    {
        // default 50%
        this->mCZ=RelAbsVector(0.0,50.0);   
    }
    else
    {
        this->mCZ=RelAbsVector(s);
    }
    if(!attributes.readInto("fx",s, getErrorLog(), false, getLine(), getColumn()) || s.find_first_not_of(delim)==std::string::npos)
    {
        this->mFX=this->mCX;
    }
    else
    {
        this->mFX=RelAbsVector(s);
    }
    if(!attributes.readInto("fy",s, getErrorLog(), false, getLine(), getColumn()) || s.find_first_not_of(delim)==std::string::npos)
    {
        this->mFY=mCY;
    }
    else
    {
        this->mFY=RelAbsVector(s);
    }
    if(!attributes.readInto("fz", s, getErrorLog(), false, getLine(), getColumn()) || s.find_first_not_of(delim)==std::string::npos)
    {
        this->mFZ=mCZ;
    }
    else
    {
        this->mFZ=RelAbsVector(s);
    }
    if(!attributes.readInto("r", s, getErrorLog(), false, getLine(), getColumn()) || s.find_first_not_of(delim)==std::string::npos)
    {
        // default 50%
        this->mRadius=RelAbsVector(0.0,50.0);   
    }
    else
    {
        this->mRadius=RelAbsVector(s);
    }
}
/** @endcond */



#ifndef OMIT_DEPRECATED
/** @cond doxygenLibsbmlInternal */
/*
 * Constructor which creates a RadialGradient with no gradient stops.
 * The id is set to the given value.
 * The RadialGradient object is invalid until it has an id and at least two 
 * gradient stops.
 * The start and the end of the linear gradient vector are set to (0,0,0).
 * A linear gradient with a vector of length zero should also be considered invalid.
 *
 * @param id the new id for the RadialGradient.
 *
 * This constructor is deprecated. The new libsbml API only has
 * constructors which take the SBML level and version or one that takes
 * an SBMLNamespaces object.
 */
RadialGradient::RadialGradient(RenderPkgNamespaces* renderns, const std::string& id)
    :GradientBase(renderns, id)
    ,mCX(RelAbsVector(0.0,50.0))
    ,mCY(RelAbsVector(0.0,50.0))
    ,mCZ(RelAbsVector(0.0,50.0))
    ,mRadius(RelAbsVector(0.0,50.0))
    ,mFX(RelAbsVector(0.0,50.0))
    ,mFY(RelAbsVector(0.0,50.0))
     ,mFZ(RelAbsVector(0.0,50.0))
{
#ifdef DEPRECATION_WARNINGS
    std::cerr << "Warning. RadialGradient::RadialGradient(const std::string& id) is deprecated." << std::endl;
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
 * Sets the 3D coordinates for the center and the focal
 * point as well as the radius.
 * Each value can be a combination of absolute and relative value and is represented by 
 * a RelAbsVector object.
 *
 * @param x x value of the center point of the radial gradient vector
 * @param y y value of the center point of the radial gradient vector
 * @param z z value of the center point of the radial gradient vector
 * @param r x value of the radius of the radial gradient vector
 * @param fx x value of the focal point of the radial gradient vector
 * @param fy y value of the focal point of the radial gradient vector
 * @param fz z value of the focal point of the radial gradient vector
 */
void RadialGradient::setCoordinates(const RelAbsVector& x,const RelAbsVector& y,const RelAbsVector& z
        ,const RelAbsVector& r,const RelAbsVector& fx,const RelAbsVector& fy,const RelAbsVector& fz)
{
    this->setCenter(x,y,z);
    this->setRadius(r);
    this->setFocalPoint(fx,fy,fz);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the 2D coordinates for the center and the focal
 * point as well as the radius.
 * The z values are automatically set to 0.
 * Each value can be a combination of absolute and relative value and is represented by 
 * a RelAbsVector object.
 *
 * @param x x value of the center point of the radial gradient vector
 * @param y y value of the center point of the radial gradient vector
 * @param r x value of the radius of the radial gradient vector
 * @param fx x value of the focal point of the radial gradient vector
 * @param fy y value of the focal point of the radial gradient vector
 */
void RadialGradient::setCoordinates(const RelAbsVector& x,const RelAbsVector& y,const RelAbsVector& r,const RelAbsVector& fx,const RelAbsVector& fy)
{
    this->setCoordinates(x,y,RelAbsVector(0.0,50.0),r,fx,fy,RelAbsVector(0.0,50.0));
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the coordinates for the center point.
 *
 * @param x x value of the center point of the radial gradient vector
 * @param y y value of the center point of the radial gradient vector
 * @param z z value of the center point of the radial gradient vector
 * The z argument can be omitted. In that case it is set to 0.
 */
void RadialGradient::setCenter(const RelAbsVector& x,const RelAbsVector& y,const RelAbsVector& z)
{
    this->mCX=x;
    this->mCY=y;
    this->mCZ=z;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the coordinates for the focal point.
 *
 * @param x x value of the focal point of the radial gradient vector
 * @param y y value of the focal point of the radial gradient vector
 * @param z z value of the focal point of the radial gradient vector.
 * The z argument can be omitted. In that case it is set to 0.
 */
void RadialGradient::setFocalPoint(const RelAbsVector& x,const RelAbsVector& y,const RelAbsVector& z)
{
    this->mFX=x;
    this->mFY=y;
    this->mFZ=z;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the radius of the radial gradient.
 *
 * @param r radius of the radial gradient vector.
 */
void RadialGradient::setRadius(const RelAbsVector& r)
{
    this->mRadius=r;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the x coordinate for the center point as a const reference.
 *
 * @return const reference to the x coordinatee of the center point.
 */
const RelAbsVector& RadialGradient::getCenterX() const
{
    return this->mCX;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the y coordinate for the center point as a const reference.
 *
 * @return const reference to the y coordinatee of the center point.
 */
const RelAbsVector& RadialGradient::getCenterY() const
{
    return this->mCY;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the z coordinate for the center point as a const reference.
 *
 * @return const reference to the z coordinatee of the center point.
 */
const RelAbsVector& RadialGradient::getCenterZ() const
{
    return this->mCZ;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the x coordinate for the focal point as a const reference.
 *
 * @return const reference to the x coordinatee of the focal point.
 */
const RelAbsVector& RadialGradient::getFocalPointX() const
{
    return this->mFX;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the y coordinate for the focal point as a const reference.
 *
 * @return const reference to the y coordinatee of the focal point.
 */
const RelAbsVector& RadialGradient::getFocalPointY() const
{
    return this->mFY;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the z coordinate for the focal point as a const reference.
 *
 * @return const reference to the z coordinatee of the focal point.
 */
const RelAbsVector& RadialGradient::getFocalPointZ() const
{
    return this->mFZ;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the radius as a const reference.
 *
 * @return const reference to the radius 
 */
const RelAbsVector& RadialGradient::getRadius() const
{
    return this->mRadius;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the x coordinate for the center point as a reference.
 *
 * @return reference to the x coordinatee of the center point.
 */
RelAbsVector& RadialGradient::getCenterX()
{
    return this->mCX;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the y coordinate for the center point as a reference.
 *
 * @return reference to the y coordinatee of the center point.
 */
RelAbsVector& RadialGradient::getCenterY()
{
    return this->mCY;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the z coordinate for the center point as a reference.
 *
 * @return reference to the z coordinatee of the center point.
 */
RelAbsVector& RadialGradient::getCenterZ()
{
    return this->mCZ;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the x coordinate for the focal point as a reference.
 *
 * @return reference to the x coordinatee of the focal point.
 */
RelAbsVector& RadialGradient::getFocalPointX()
{
    return this->mFX;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the y coordinate for the focal point as a reference.
 *
 * @return reference to the y coordinatee of the focal point.
 */
RelAbsVector& RadialGradient::getFocalPointY()
{
    return this->mFY;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the z coordinate for the focal point as a reference.
 *
 * @return reference to the z coordinatee of the focal point.
 */
RelAbsVector& RadialGradient::getFocalPointZ()
{
    return this->mFZ;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the radius as a reference.
 *
 * @return reference to the radius 
 */
RelAbsVector& RadialGradient::getRadius()
{
    return this->mRadius;
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
int RadialGradient::getTypeCode() const
{
    return SBML_RENDER_RADIALGRADIENT;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Accepts the given SBMLVisitor for this instance of RadialGradient.
 *
 * @param v the SBMLVisitor instance to be used.
 *
 * @return the result of calling <code>v.visit()</code>.
 */
bool RadialGradient::accept(SBMLVisitor& /*visitor*/) const
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
const std::string& RadialGradient::getElementName() const
{
  static std::string name = RadialGradient::ELEMENT_NAME;
  return name;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Creates and returns a deep copy of this RadialGradient object.
 * 
 * @return a (deep) copy of this RadialGradient object
 */
RadialGradient* RadialGradient::clone() const
{
    return new RadialGradient(*this);
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
void RadialGradient::writeAttributes (XMLOutputStream& stream) const
{
  GradientBase::writeAttributes(stream);
  std::ostringstream os;
  RelAbsVector dflt(0.0,50.0);
  if(this->mCX != dflt)
  {
      os << this->mCX;
      stream.writeAttribute("cx", getPrefix(), os.str());
  }
  os.str("");
  if(this->mCY != dflt)
  {
      os << this->mCY;
      stream.writeAttribute("cy", getPrefix(), os.str());
  }
  if(this->mCZ!=dflt)
  {
      os.str("");
      os << this->mCZ;
      stream.writeAttribute("cz", getPrefix(), os.str());
  }
  os.str("");
  if(this->mFX!=this->mCX)
  {
      os << this->mFX;
      stream.writeAttribute("fx", getPrefix(), os.str());
  }
  os.str("");
  if(this->mFY!=this->mCY)
  {
      os << this->mFY;
      stream.writeAttribute("fy", getPrefix(), os.str());
  }
  if(this->mFZ!=this->mCZ)
  {
      os.str("");
      os << this->mFZ;
      stream.writeAttribute("fz", getPrefix(), os.str());
  }
  os.str("");
  if(this->mRadius != dflt)
  {
      os  << this->mRadius;
      stream.writeAttribute("r", getPrefix(), os.str());
  }
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Creates an XMLNode object from this RadialGradient object.
 *
 * @return the XMLNode with the XML representation for the 
 * RadialGradient object.
 */
XMLNode RadialGradient::toXML() const
{
  return getXmlNodeForSBase(this);
}
/** @endcond */

LIBSBML_CPP_NAMESPACE_END  
