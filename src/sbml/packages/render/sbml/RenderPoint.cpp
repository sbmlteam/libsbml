/**
 * @file    RenderPoint.cpp
 * @brief   class for representing points in the render extension.
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

#include <sstream>
#include <limits>
#ifndef OMIT_DEPRECATED
#ifdef DEPRECATION_WARNINGS
#include <iostream>
#endif // DEPRECATION_WARNINGS
#endif // OMIT_DEPRECATED

#include "RenderPoint.h"
#include <sbml/packages/layout/util/LayoutAnnotation.h>
#include <sbml/packages/layout/util/LayoutUtilities.h>
#include <sbml/packages/render/extension/RenderExtension.h>
#include <sbml/SBMLErrorLog.h>
#include <sbml/SBMLVisitor.h>
#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLToken.h>
#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>


LIBSBML_CPP_NAMESPACE_BEGIN

/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new RenderPoint object with the given SBML level
 * and SBML version.
 *
 * @param level SBML level of the new object
 * @param level SBML version of the new object
 */
RenderPoint::RenderPoint (unsigned int level, unsigned int version, unsigned int pkgVersion) : 
    SBase(level,version)
    ,mXOffset(RelAbsVector(0.0,0.0))
    ,mYOffset(RelAbsVector(0.0,0.0))
    ,mZOffset(RelAbsVector(0.0,0.0))
    ,mElementName("element")
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
 * Creates a new RenderPoint object with the given SBMLNamespaces.
 *
 * @param sbmlns The SBML namespace for the object.
 */
RenderPoint::RenderPoint (RenderPkgNamespaces* renderns):
    SBase(renderns)
    ,mXOffset(RelAbsVector(0.0,0.0))
    ,mYOffset(RelAbsVector(0.0,0.0))
    ,mZOffset(RelAbsVector(0.0,0.0))
    ,mElementName("element")
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
 * Copy constructor for RenderPoint objects.
 */
RenderPoint::RenderPoint(const RenderPoint& orig):SBase(orig)
{
    this->mXOffset=orig.mXOffset;
    this->mYOffset=orig.mYOffset;
    this->mZOffset=orig.mZOffset;
    this->mElementName=orig.mElementName;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Assignment operator for RenderPoint objects.
 */
RenderPoint& RenderPoint::operator=(const RenderPoint& orig)
{
    if(&orig!=this)
    {
        this->SBase::operator=(orig);
        this->mXOffset=orig.mXOffset;
        this->mYOffset=orig.mYOffset;
        this->mZOffset=orig.mZOffset;
        this->mElementName=orig.mElementName;
    }
    return *this;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new point with the given ccordinates.
 *
 * @param x x coordinate of the RenderPoint object
 * @param y y coordinate of the RenderPoint object
 * @param z z coordinate of the RenderPoint object
 * If the z value is omitted, it is set to 0.
 */ 
RenderPoint::RenderPoint(RenderPkgNamespaces* renderns, const RelAbsVector& x, const RelAbsVector& y, const RelAbsVector& z) :
    SBase  (renderns)
    , mXOffset(x)
    , mYOffset(y)
    , mZOffset(z)
    , mElementName("element")  
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
 * Sets the Z offset to 0.0.
 */
void RenderPoint::initDefaults ()
{
    this->setZ(0.0);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new RenderPoint object from the given XMLNode object.
 * The XMLNode object has to contain a valid XML representation of a 
 * RenderPoint object as defined in the render extension specification.
 * This method is normally called when render information is read from a file and 
 * should normally not have to be called explicitely.
 *
 * @param node the XMLNode object reference that describes the RenderPoint
 * object to be instantiated.
 */
RenderPoint::RenderPoint(const XMLNode& node, unsigned int l2version) : SBase(2, l2version)
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
 * Destroys the RenderPoint object.
 */ 
RenderPoint::~RenderPoint()
{
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Sets the coordinates of the RenderPoint to the given values.
 *
 * @param x x coordinate to be set.
 * @param y y coordinate to be set.
 * @param z z coordinate to be set. If the z coordinate is omitted, it is set to 0.
 */ 
    void
RenderPoint::setCoordinates (const RelAbsVector& x, const RelAbsVector& y, const RelAbsVector& z)
{
    this->setX(x);
    this->setY(y);
    this->setZ(z);
}
/** @endcond */

#ifndef OMIT_DEPRECATED
/** @cond doxygenLibsbmlInternal */
/*
 * Sets the coordinates of the RenderPoint to the given values.
 * This method is deprecated, please use setCoordinates. 
 *
 * @param x x coordinate to be set.
 * @param y y coordinate to be set.
 * @param z z coordinate to be set. If the z coordinate is omitted, it is set to 0.
 */ 
    void
RenderPoint::setOffsets (const RelAbsVector& x, const RelAbsVector& y, const RelAbsVector& z)
{
#ifdef DEPRECATION_WARNINGS
    std::cerr << "Warning. \"void RenderPoint::setOffsets (const RelAbsVector& x, const RelAbsVector& y, const RelAbsVector& z)\" is deprecated." << std::endl;
#endif // DEPRECATION_WARNINGS
    this->setCoordinates(x,y,z);
}
/** @endcond */
#endif // OMIT_DEPRECATED



/** @cond doxygenLibsbmlInternal */
/*
 * Sets the x ccordiante of the RenderPoint object.
 *
 * @param x x coordinate to be set.
 */ 
    void
RenderPoint::setX (const RelAbsVector& x)
{
    this->mXOffset = x;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Sets the y ccordiante of the RenderPoint object.
 *
 * @param y y coordinate to be set.
 */ 
    void
RenderPoint::setY (const RelAbsVector& y)
{
    this->mYOffset = y;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Sets the z ccordiante of the RenderPoint object.
 *
 * @param z z coordinate to be set.
 */ 
    void
RenderPoint::setZ (const RelAbsVector& z)
{
    this->mZOffset = z;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Returns the x coordinate of the RenderPoint as a const reference.
 *
 * @return const reference to x coordinate.
 */ 
const RelAbsVector&
RenderPoint::x() const
{
    return this->mXOffset;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Returns the y coordinate of the RenderPoint as a const reference.
 *
 * @return const reference to y coordinate.
 */ 
const RelAbsVector&
RenderPoint::y() const
{
    return this->mYOffset;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Returns the z coordinate of the RenderPoint as a const reference.
 *
 * @return const reference to z coordinate.
 */ 
const RelAbsVector&
RenderPoint::z() const
{
    return this->mZOffset;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Returns the x coordinate of the RenderPoint as a reference.
 *
 * @return reference to x coordinate.
 */ 
    RelAbsVector&
RenderPoint::x()
{
    return this->mXOffset;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Returns the y coordinate of the RenderPoint as a reference.
 *
 * @return reference to y coordinate.
 */ 
    RelAbsVector&
RenderPoint::y()
{
    return this->mYOffset;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Returns the z coordinate of the RenderPoint as a reference.
 *
 * @return reference to z coordinate.
 */ 
    RelAbsVector&
RenderPoint::z()
{
    return this->mZOffset;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Subclasses should override this method to write out their contained
 * SBML objects as XML elements.  Be sure to call your parents
 * implementation of this method as well.  For example:
 *
 *   SBase::writeElements(stream);
 *   mReactants.write(stream);
 *   mProducts.write(stream);
 *   ...
 */
void RenderPoint::writeElements (XMLOutputStream& stream) const
{
    SBase::writeElements(stream);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the element name which is returned by getElementName.
 * RenderPoint objects can have different element names depending on context.
 *
 * @param name the string with the element name to be set.
 */
void RenderPoint::setElementName(const std::string& name)
{
    this->mElementName=name;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the XML element name of this object, which for
 * RenderPoint, depends on the context.
 * The name that is returned has to be set with 
 * setElementName.
 * 
 * @return the name of this element  (@see setElementName)
 */
const std::string& RenderPoint::getElementName () const 
{
  static std::string name = "element";
  return name;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Creates and returns a deep copy of this RenderPoint object.
 * 
 * @return a (deep) copy of this RenderPoint object
 */
RenderPoint* 
RenderPoint::clone () const
{
    return new RenderPoint(*this);
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
SBase*
RenderPoint::createObject (XMLInputStream& stream)
{
    SBase*        object = NULL;

    return object;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
void
RenderPoint::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("xsi:type");
  attributes.add("x");
  attributes.add("y");
  attributes.add("z");
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
void 
RenderPoint::writeXMLNS (XMLOutputStream& stream) const
{
  XMLNamespaces xmlns;
  xmlns.add(LayoutExtension::getXmlnsXSI(), "xsi");
  stream << xmlns;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
void RenderPoint::readAttributes (const XMLAttributes& attributes, const ExpectedAttributes& expectedAttributes)
{
  SBase::readAttributes(attributes, expectedAttributes);
    std::string s;
    if(attributes.readInto("x",s, getErrorLog(), false, getLine(), getColumn()))
    {
        this->mXOffset=RelAbsVector(s);
    }
    else
    {
        this->mXOffset=RelAbsVector(std::numeric_limits<double>::quiet_NaN(),std::numeric_limits<double>::quiet_NaN());   
    }
    if(attributes.readInto("y",s, getErrorLog(), false, getLine(), getColumn()))
    {
        this->mYOffset=RelAbsVector(s);
    }
    else
    {
        this->mYOffset=RelAbsVector(std::numeric_limits<double>::quiet_NaN(),std::numeric_limits<double>::quiet_NaN());   
    }
    if(attributes.readInto("z",s, getErrorLog(), false, getLine(), getColumn()))
    {
        this->mZOffset=RelAbsVector(s);
    }
    else
    {
        this->mZOffset=RelAbsVector(0.0,0.0);
    }
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
void RenderPoint::writeAttributes (XMLOutputStream& stream) const
{
    SBase::writeAttributes(stream);
    XMLTriple triple("type","","xsi");
    stream.writeAttribute(triple,std::string("RenderPoint"));
    std::ostringstream os;
    os << mXOffset;
    stream.writeAttribute("x", getPrefix(), os.str());
    os.str("");
    os << mYOffset;
    stream.writeAttribute("y", getPrefix(), os.str());
    if(this->mZOffset!=RelAbsVector(0.0,0.0))
    {
        os.str("");  
        os << mZOffset;
        stream.writeAttribute("z", getPrefix(), os.str());
    }
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
XMLNode RenderPoint::toXML(const std::string& name) const
{
  return getXmlNodeForSBase(this);
}
/** @endcond */


int
RenderPoint::getTypeCode () const
{
    return SBML_RENDER_POINT;
}


bool RenderPoint::accept (SBMLVisitor& v) const
{
    //v.visit(*this);
    return false;
}


/** @cond doxygenLibsbmlInternal */
/*
 * Comparison operator for RenderPoint objects.
 */
bool RenderPoint::operator==(const RenderPoint& left) const
{
    return (this->mXOffset == left.mXOffset && this->mYOffset == left.mYOffset && this->mZOffset == left.mZOffset);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/* function returns true if component has all the required
 * attributes
 */
bool RenderPoint::hasRequiredAttributes() const
{
    bool result = this->SBase::hasRequiredAttributes();
    // the offsets should not be NaN
    result = result && 
        (this->mXOffset.getAbsoluteValue() == this->mXOffset.getAbsoluteValue()) &&
        (this->mXOffset.getRelativeValue() == this->mXOffset.getRelativeValue());
    result = result && 
        (this->mYOffset.getAbsoluteValue() == this->mYOffset.getAbsoluteValue()) &&
        (this->mYOffset.getRelativeValue() == this->mYOffset.getRelativeValue());
    result = result && 
        (this->mZOffset.getAbsoluteValue() == this->mZOffset.getAbsoluteValue()) &&
        (this->mZOffset.getRelativeValue() == this->mZOffset.getRelativeValue());
    return result;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/* function returns true if component has all the required
 * elements
 */
bool RenderPoint::hasRequiredElements() const 
{
    bool result = this->SBase::hasRequiredElements();
    return result;
}
/** @endcond */

LIBSBML_CPP_NAMESPACE_END 
