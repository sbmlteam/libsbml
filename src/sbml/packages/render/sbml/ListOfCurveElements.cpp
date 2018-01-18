/**
 * @file    ListOfCurveElements.cpp
 * @brief   storage class for curve elements
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

#include "ListOfCurveElements.h"
#include "RenderPoint.h"
#include "RenderCubicBezier.h"

#include <sbml/xml/XMLInputStream.h>
#include <sbml/packages/layout/util/LayoutAnnotation.h>
#include <sbml/packages/render/extension/RenderExtension.h>

#include <assert.h>

LIBSBML_CPP_NAMESPACE_BEGIN

const std::string ListOfCurveElements::ELEMENT_NAME="listOfElements";

/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new ListOfCurveElements object from the given XMLNode object.
 * The XMLNode object has to contain a valid XML representation of a 
 * ListOfCurveElements object as defined in the render extension specification.
 * This method is normally called when render information is read from a file and 
 * should normally not have to be called explicitely.
 *
 * @param node the XMLNode object reference that describes the ListOfCurveElements
 * object to be instantiated.
 */
ListOfCurveElements::ListOfCurveElements(const XMLNode& node, unsigned int l2version)
  : ListOf(2, l2version)
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
        if(childName=="element")
        {
            RenderPoint* ls=NULL;
            const XMLAttributes& innerAttributes=child->getAttributes();
            int typeIndex=innerAttributes.getIndex("type");
            if(typeIndex==-1 || innerAttributes.getURI(typeIndex)!="http://www.w3.org/2001/XMLSchema-instance")
            {
                // throw
                ++n;
                continue;
            }
            if(innerAttributes.getValue(typeIndex)=="RenderCubicBezier")
            {
                ls=new RenderCubicBezier(*child);
            }
            else
            {
                ls=new RenderPoint(*child);
            }
            this->appendAndOwn(ls);
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
 * Creates and returns a deep copy of the ListOfCurveElements object.
 *
 * @return a (deep) copy of this ListOfCurveElements
 */
ListOfCurveElements* ListOfCurveElements::clone () const
{
    return new ListOfCurveElements(*this);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Copy constructor for ListOfCurveElements objects.
 */
ListOfCurveElements::ListOfCurveElements(const ListOfCurveElements& source):ListOf(source)
{
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Assignment operator for ListOfCurveElements objects.
 */
ListOfCurveElements& ListOfCurveElements::operator=(const ListOfCurveElements& source)
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
 * (i.e., GradientDefinition objects, if the list is non-empty).
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
int ListOfCurveElements::getItemTypeCode () const
{
    return SBML_RENDER_LINESEGMENT;
}
/** @endcond */


bool ListOfCurveElements::isValidTypeForList(SBase * item)
{
  if (item == NULL) return false;
  int typeCode = item->getTypeCode();
  if (typeCode == SBML_RENDER_LINESEGMENT || 
      typeCode == SBML_RENDER_POINT ||
      typeCode == SBML_RENDER_CUBICBEZIER)
      return true;
  return false;
}

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the XML element name of this object, which for
 * ListOfCurveElements, is always @c "listOfCurveElements".
 * 
 * @return the name of this element, i.e., @c "listOfCurveElements".
 */
const std::string& ListOfCurveElements::getElementName () const
{
  static std::string name = ListOfCurveElements::ELEMENT_NAME;
  return name;
}
/** @endcond */


/*
 * Ctor.
 */
ListOfCurveElements::ListOfCurveElements(RenderPkgNamespaces* renderns)
 : ListOf(renderns)
{
     // set the element namespace of this object
  setElementNamespace(renderns->getURI());

  // connect child elements to this element.
  connectToChild();

  // load package extensions bound with this object (if any) 
  loadPlugins(renderns);
}


/*
 * Ctor.
 */
ListOfCurveElements::ListOfCurveElements(unsigned int level, unsigned int version, unsigned int pkgVersion)
 : ListOf(level,version)
{
  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(level,version,pkgVersion));
};

/** @cond doxygenLibsbmlInternal */
/*
 * Creates an XMLNode object from this ListOfCurveElements object.
 *
 * @return the XMLNode with the XML representation for the 
 * ListOfCurveElements object.
 */
XMLNode ListOfCurveElements::toXML() const
{
  return getXmlNodeForSBase(this);
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase* ListOfCurveElements::createObject (XMLInputStream& stream)
{
    const std::string& name   = stream.peek().getName();
    SBase*        object = NULL;
    
    RENDER_CREATE_NS(renderns, this->getSBMLNamespaces());

    if (name == "element")
    {
        std::string type = "RenderPoint";
        // find the correct type attribute
        int index=stream.peek().getAttributes().getIndex("type","http://www.w3.org/2001/XMLSchema-instance");
        if(index != -1)
        {
          type=stream.peek().getAttributes().getValue(index);
        }

        if(type=="RenderPoint")
        {
            object = new RenderPoint(renderns);
            if(object != NULL)
            {
              static_cast<RenderPoint*>(object)->setElementName("element");
            }
        }
        else if(type=="RenderCubicBezier")
        {
            object = new RenderCubicBezier(renderns);
            if(object != NULL)
            {
              static_cast<RenderCubicBezier*>(object)->setElementName("element");
            }
        }
        if(object) this->mItems.push_back(object);
    }
    delete renderns;
    return object;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Removes the RenderPoint with the given index and returns a pointer to the 
 * removed object. The caller is responsible for freeing the associated memory.
 * 
 * @param i index of the RenderPoint object to be removed
 * 
 * @return pointer to the removed RenderPoint or NULL if the index
 *  was not valid.
 */
RenderPoint* ListOfCurveElements::remove(unsigned int i)
{
    return static_cast<RenderPoint*>(this->ListOf::remove(i));
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Returns a pointer to the RenderPoint with the given index or NULL if
 * the index is invalid.
 * 
 * @param i index of the RenderPoint object to be returned
 * 
 * @return pointer to the RenderPoint at the given index or NULL.
 */
RenderPoint* ListOfCurveElements::get(unsigned int i)
{
    return static_cast<RenderPoint*>(this->ListOf::get(i));
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns a const pointer to the RenderPoint with the given index or NULL if
 * the index is invalid.
 * 
 * @param i index of the RenderPoint object to be returned
 * 
 * @return const pointer to the RenderPoint at the given index or NULL.
 */
const RenderPoint* ListOfCurveElements::get(unsigned int i) const
{
    return static_cast<const RenderPoint*>(this->ListOf::get(i));
}
/** @endcond */




LIBSBML_CPP_NAMESPACE_END 
