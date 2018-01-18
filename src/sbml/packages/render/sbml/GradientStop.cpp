/**
 * @file    GradientStop.cpp
 * @brief   class representing a stop in a gradient definition
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

#include "GradientStop.h"
#include <sbml/xml/XMLInputStream.h>

#include <sstream>
#include <assert.h>
#ifndef OMIT_DEPRECATED
#ifdef DEPRECATION_WARNINGS
#include <iostream>
#endif // DEPRECATION_WARNINGS
#endif // OMIT_DEPRECATED
#include <sbml/packages/layout/util/LayoutAnnotation.h> 
#include <sbml/packages/layout/util/LayoutUtilities.h>
#include <sbml/packages/render/extension/RenderExtension.h>

LIBSBML_CPP_NAMESPACE_BEGIN

const std::string ListOfGradientStops::ELEMENT_NAME="listOfGradientStops"; 
const std::string GradientStop::ELEMENT_NAME="stop"; 

/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new GradientStop object with the given SBML level
 * and SBML version.
 *
 * @param level SBML level of the new object
 * @param level SBML version of the new object
 */
GradientStop::GradientStop (unsigned int level, unsigned int version, unsigned int pkgVersion) : 
    SBase(level,version)
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
 * Creates a new GradientStop object with the given SBMLNamespaces.
 *
 * @param sbmlns The SBML namespace for the object.
 */
GradientStop::GradientStop (RenderPkgNamespaces* renderns):
    SBase(renderns)
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


/*
 * Destroy this object.
 */
GradientStop::~GradientStop ()
{
}



/*
 * Ctor.
 */
ListOfGradientStops::ListOfGradientStops(RenderPkgNamespaces* renderns)
 : ListOf(renderns)
{
  //
  // set the element namespace of this object
  //
  setElementNamespace(renderns->getURI());
}


/*
 * Ctor.
 */
ListOfGradientStops::ListOfGradientStops(unsigned int level, unsigned int version, unsigned int pkgVersion)
 : ListOf(level,version)
{
  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(level,version,pkgVersion));
};

/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new ListOfGradientStops object from the given XMLNode object.
 * The XMLNode object has to contain a valid XML representation of a 
 * ListOfGradientStops object as defined in the render extension specification.
 * This method is normally called when render information is read from a file and 
 * should normally not have to be called explicitely.
 *
 * @param node the XMLNode object reference that describes the ListOfGradientStops
 * object to be instantiated.
 */
ListOfGradientStops::ListOfGradientStops(const XMLNode& node, unsigned int l2version)
  : ListOf(2, l2version)
{
    const XMLAttributes& attributes=node.getAttributes();
    const XMLNode* child;
    mURI = RenderExtension::getXmlnsL3V1V1();    

    ExpectedAttributes ea;
    addExpectedAttributes(ea);

    this->readAttributes(attributes, ea);
    unsigned int n=0,nMax = node.getNumChildren();
    while(n<nMax)
    {
        child=&node.getChild(n);
        const std::string& childName=child->getName();
        if(childName=="gradientStop")
        {
            GradientStop* stop=new GradientStop(*child);
            this->appendAndOwn(stop);
        }
        else if(childName=="annotation")
        {
            this->mAnnotation=new XMLNode(*child);
        }
        else if(childName=="notes")
        {
            this->mNotes=new XMLNode(*child);
        }
        ++n;
    }

  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(2,l2version));  

  connectToChild();
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new GradientStop object from the given XMLNode object.
 * The XMLNode object has to contain a valid XML representation of a 
 * GradientStop object as defined in the render extension specification.
 * This method is normally called when render information is read from a file and 
 * should normally not have to be called explicitely.
 *
 * @param node the XMLNode object reference that describes the GradientStop
 * object to be instantiated.
 */
GradientStop::GradientStop(const XMLNode& node, unsigned int l2version) : SBase(2, l2version)
{
    const XMLAttributes& attributes=node.getAttributes();
    const XMLNode* child;
    mURI = RenderExtension::getXmlnsL3V1V1();
    
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
            this->mAnnotation=new XMLNode(*child);
        }
        else if(childName=="notes")
        {
            this->mNotes=new XMLNode(*child);
        }
        ++n;
    }

    
  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(2,l2version));  

  connectToChild();
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
void
GradientStop::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("offset");
  attributes.add("stop-color");
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
void GradientStop::readAttributes (const XMLAttributes& attributes, const ExpectedAttributes& expectedAttributes)
{
    SBase::readAttributes(attributes, expectedAttributes);
    std::string s;
    attributes.readInto("offset", s, getErrorLog(), false, getLine(), getColumn());
    this->mOffset=RelAbsVector(s);
    attributes.readInto("stop-color", this->mStopColor, getErrorLog(), false, getLine(), getColumn());
}
/** @endcond */




/** @cond doxygenLibsbmlInternal */
/*
 * Returns the offset of the gradient.
 *
 * @return a const reference to the offset of the gradient stop.
 */
const RelAbsVector& GradientStop::getOffset() const
{
    return this->mOffset;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the offset of the gradient.
 *
 * @return a reference to the offset of the gradient stop.
 */
RelAbsVector& GradientStop::getOffset()
{
    return this->mOffset;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the offset for the gradient stop.
 *
 * @param abs the absolute value of the offset.
 *
 * @param rel the relative value of the offset.
 */
void GradientStop::setOffset(double abs,double rel)
{
    this->mOffset=RelAbsVector(abs,rel);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the offset to the value specified by the given string.
 * The string has to represent a combination of an absolute 
 * and relative value.
 * Valid value string would e.g. be "45.0", "30%" or
 * "10+5%". If the value is a combination of both relative and 
 * absolute value, the absolute value has to come before the relative
 * value. Number can be given as integer values or floating point values
 * and the two components can be combined by '+' or '-'. Depending on
 * whethr the relative value should be added or subtracted from the 
 * absolute value.
 * If the given string is not valid, the offset will have an absolute 
 * and a relative value of NaN.
 *
 * @param a string representing a valid offset value.
 */
void GradientStop::setOffset(const std::string& co)
{
    this->mOffset=RelAbsVector(co);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the offset to the given vector object.
 *
 * @param offset The RelAbsVector object that specifies the
 * offset of the gradient stop.
 */
void GradientStop::setOffset(const RelAbsVector& co)
{
    this->mOffset=co;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the stop color id or the value string.
 * Since ids can not start with the '#' character,
 * this is the way to determine if the gradient stop 
 * uses a color value or a color id.
 *
 * @return the color id or value string
 */
const std::string& GradientStop::getStopColor() const
{
    return this->mStopColor;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the stop color id or the stop color value.
 *
 * @param color Either the id of a ColorDefinition object, or a color
 * value string.
 */
void GradientStop::setStopColor(const std::string& id)
{
    this->mStopColor=id;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Creates a deep copy of the ListOfGradientStops object.
 *
 * @return a (deep) copy of this ListOfGradientStops
 */
ListOfGradientStops* ListOfGradientStops::clone () const
{
    return new ListOfGradientStops(*this);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Copy constructor; creates a copy of the given ListOfGradientStops object.
 *
 * @param the ListOfGradientStops object to be copied.
 */
ListOfGradientStops::ListOfGradientStops(const ListOfGradientStops& source):ListOf(source)
{
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Assignment operator for ListOfGradientStops objects.
 */
ListOfGradientStops& ListOfGradientStops::operator=(const ListOfGradientStops& source)
{
    if(&source!=this)
    {
        this->ListOf::operator=(source);
    }
    return *this;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the libSBML type code for the objects contained in this ListOf
 * (i.e., GradientStop objects, if the list is non-empty).
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
int ListOfGradientStops::getItemTypeCode () const
{
    return SBML_RENDER_GRADIENT_STOP;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the XML element name of this object, which for
 * ListOfGradientStops, is always @c "listOfGradientStops".
 * 
 * @return the name of this element, i.e., @c "listOfGradientStops".
 */
const std::string& ListOfGradientStops::getElementName () const
{
  static std::string name = ListOfGradientStops::ELEMENT_NAME;
  return name;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Creates an XMLNode object from this ListOfGradientStops object.
 *
 * @return the XMLNode with the XML representation for the 
 * ListOfGradientStops object.
 */
XMLNode ListOfGradientStops::toXML() const
{
  return getXmlNodeForSBase(this);
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase* ListOfGradientStops::createObject (XMLInputStream& stream)
{
    const std::string& name   = stream.peek().getName();
    SBase*        object = NULL;

    if (name == "stop")
    {
      RENDER_CREATE_NS(renderns, this->getSBMLNamespaces());
      object = new GradientStop(renderns);
      if(object != NULL) this->mItems.push_back(object);
	 delete renderns;
    }
    return object;
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
void GradientStop::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);
  std::ostringstream os;
  os << this->mOffset;
  stream.writeAttribute("offset", getPrefix(), os.str());
  stream.writeAttribute("stop-color", getPrefix(), this->mStopColor);
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Creates an XMLNode object from this GradientStop object.
 *
 * @return the XMLNode with the XML representation for the 
 * GradientStop object.
 */
XMLNode GradientStop::toXML() const
{
  return getXmlNodeForSBase(this);
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
int GradientStop::getTypeCode() const
{
    return SBML_RENDER_GRADIENT_STOP;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Accepts the given SBMLVisitor for this instance of Group.
 *
 * @param v the SBMLVisitor instance to be used.
 *
 * @return the result of calling <code>v.visit()</code>.
 */
bool GradientStop::accept(SBMLVisitor& /*visitor*/) const
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
const std::string& GradientStop::getElementName() const
{
  static std::string name = GradientStop::ELEMENT_NAME;
  return name;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Creates and returns a deep copy of this GradientStop object.
 * 
 * @return a (deep) copy of this GradientStop object
 */
GradientStop* GradientStop::clone() const
{
    return new GradientStop(*this);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns a pointer to the GradientStop with the given index or NULL if
 * the index is invalid.
 * 
 * @param i index of the GradientStop object to be returned
 * 
 * @return pointer to the GradientStop at the given index or NULL.
 */
GradientStop* ListOfGradientStops::get(unsigned int i)
{
    return static_cast<GradientStop*>(this->ListOf::get(i));
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns a const pointer to the GradientStop with the given index or NULL if
 * the index is invalid.
 * 
 * @param i index of the GradientStop object to be returned
 * 
 * @return const pointer to the GradientStop at the given index or NULL.
 */
const GradientStop* ListOfGradientStops::get(unsigned int i) const
{
    return static_cast<const GradientStop*>(this->ListOf::get(i));
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/* Removes the nth item from this list */
    GradientStop*
ListOfGradientStops::remove (unsigned int n)
{
    return static_cast<GradientStop*>(ListOf::remove(n));
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/* function returns true if component has all the required
 * attributes
 */
bool GradientStop::hasRequiredAttributes() const
{
    bool result = this->SBase::hasRequiredAttributes();
    result = result && 
        (this->mOffset.getRelativeValue() == this->mOffset.getRelativeValue()) &&
        (this->mOffset.getAbsoluteValue() == this->mOffset.getAbsoluteValue());
    result = result && (this->mStopColor.find_first_not_of(" \t\n\r") != std::string::npos);
    return result;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/* function returns true if component has all the required
 * elements
 */
bool GradientStop::hasRequiredElements() const 
{
    bool result = this->SBase::hasRequiredElements();
    return result;
}
/** @endcond */


LIBSBML_CPP_NAMESPACE_END 
