/**
 * @file    RenderCurve.cpp
 * @brief   class for representing curves in the render extension
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

#include "RenderCurve.h"
#include "RenderPoint.h"
#include "RenderCubicBezier.h"
#include <sbml/packages/layout/util/LayoutAnnotation.h>
#include <sbml/packages/render/extension/RenderExtension.h>

#include <sbml/util/ElementFilter.h>

#ifndef OMIT_DEPRECATED
#ifdef DEPRECATION_WARNINGS
#include <iostream>
#endif // DEPRECATION_WARNINGS
#endif // OMIT_DEPRECATED

#include <sbml/xml/XMLInputStream.h>

LIBSBML_CPP_NAMESPACE_BEGIN

const std::string RenderCurve::ELEMENT_NAME="curve";


/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new RenderCurve object with the given SBML level
 * and SBML version.
 *
 * @param level SBML level of the new object
 * @param level SBML version of the new object
 */
RenderCurve::RenderCurve (unsigned int level, unsigned int version, unsigned int pkgVersion) : 
    GraphicalPrimitive1D(level,version, pkgVersion)
    ,mStartHead("")
    ,mEndHead("")
    ,mListOfElements(level, version, pkgVersion)
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
 * Creates a new RenderCurve object with the given SBMLNamespaces.
 *
 * @param sbmlns The SBML namespace for the object.
 */
RenderCurve::RenderCurve (RenderPkgNamespaces* renderns):
    GraphicalPrimitive1D(renderns)
    ,mStartHead("")
    ,mEndHead("")
    ,mListOfElements(renderns)
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
 * Creates a new RenderCurve object from the given XMLNode object.
 * The XMLNode object has to contain a valid XML representation of a 
 * RenderCurve object as defined in the render extension specification.
 * This method is normally called when render information is read from a file and 
 * should normally not have to be called explicitely.
 *
 * @param node the XMLNode object reference that describes the RenderCurve
 * object to be instantiated.
 */
RenderCurve::RenderCurve(const XMLNode& node, unsigned int l2version)
  : GraphicalPrimitive1D(node, l2version)
  , mListOfElements(node, l2version)
{
    const XMLAttributes& attributes=node.getAttributes();
     ExpectedAttributes ea;
    addExpectedAttributes(ea);
    this->readAttributes(attributes, ea);
    const XMLNode* child;	
    unsigned int n=0,nMax = node.getNumChildren();
    while(n<nMax)
    {
        child=&node.getChild(n);
        const std::string& childName=child->getName();
        if(childName=="listOfElements")
        {
            this->mListOfElements=ListOfCurveElements(*child);
        }
        ++n;
    }
    
  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(2,l2version));  

  connectToChild();
}
/** @endcond */


/*
 * Destroy this object.
 */
RenderCurve::~RenderCurve ()
{
}



List*
RenderCurve::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_LIST(ret, sublist, mListOfElements, filter);

  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}


/** @cond doxygenLibsbmlInternal */
void
RenderCurve::addExpectedAttributes(ExpectedAttributes& attributes)
{
  GraphicalPrimitive1D::addExpectedAttributes(attributes);

  attributes.add("startHead");
  attributes.add("endHead");

}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
void RenderCurve::readAttributes (const XMLAttributes& attributes, const ExpectedAttributes& expectedAttributes)
{
  GraphicalPrimitive1D::readAttributes(attributes, expectedAttributes);
    std::string s;
    attributes.readInto("startHead", this->mStartHead, getErrorLog(), false, getLine(), getColumn());
    attributes.readInto("endHead", this->mEndHead, getErrorLog(), false, getLine(), getColumn());
}
/** @endcond */




#ifndef OMIT_DEPRECATED
/** @cond doxygenLibsbmlInternal */
/*
 * Constructor with id.
 */
/*
 * Instantiates an empty curve object with the given @p id.
 * The decorations  are unset and there are no curve elements.
 *
 * This constructor is deprecated. The new libsbml API only has
 * constructors which take the SBML level and version or one that takes
 * an SBMLNamespaces object.
 */
RenderCurve::RenderCurve(RenderPkgNamespaces* renderns, const std::string& id)
  : GraphicalPrimitive1D(renderns, id)
  , mStartHead("")
  , mEndHead("")
  , mListOfElements(renderns)
{
#ifdef DEPRECATION_WARNINGS
    std::cerr << "Warning. RenderCurve::RenderCurve(const std::string& id) is deprecated." << std::endl;
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
 * Sets the id of the start head.
 *
 * @param The id of a LineEnding object to be applied to the start of the curve.
 */
void RenderCurve::setStartHead(const std::string& startHead)
{
    this->mStartHead=startHead;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Sets the id of the end head.
 *
 * @param The id of a LineEnding object to be applied to the end of the curve.
 */
void RenderCurve::setEndHead(const std::string& endHead)
{
    this->mEndHead=endHead;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Returns the id of the LineEnding object to be applied to the start of the curve.
 *
 * @return id of the LineEnding for the start of the curve.
 */
const std::string& RenderCurve::getStartHead() const
{
    return this->mStartHead;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Returns the id of the LineEnding object to be applied to the end of the curve.
 *
 * @return id of the LineEnding for the end of the curve.
 */
const std::string& RenderCurve::getEndHead() const
{
    return this->mEndHead;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Returns the number of curve segments.
 *
 * @return number of elements in the curve.
 */
unsigned int RenderCurve::getNumElements() const
{
    return this->mListOfElements.size();
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new bezier element.
 * The element is added to and owned by the curve.
 *
 * @return The newly created RenderCubicBezier object.
 */
RenderCubicBezier* RenderCurve::createCubicBezier()
{
    RenderCubicBezier* pRenderCubicBezier=NULL;
    try
    {
      RENDER_CREATE_NS(renderns, this->getSBMLNamespaces());
      pRenderCubicBezier = new RenderCubicBezier(renderns);
	 delete renderns;
    }
    catch (...)
    {
        /* here we do not create a default object as the level/version must
         * match the parent object
         *
         * so do nothing
         */
    }


    if(pRenderCubicBezier != NULL)
    {
        this->mListOfElements.appendAndOwn(pRenderCubicBezier);
    }
    return pRenderCubicBezier;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new point element.
 * The element is added to and owned by the curve.
 *
 * @return The newly created RenderCubicBezier object.
 */
RenderPoint* RenderCurve::createPoint()
{
    RenderPoint* pRenderPoint=NULL;
    try
    {
      RENDER_CREATE_NS(renderns, this->getSBMLNamespaces());
      pRenderPoint = new RenderPoint(renderns);
	 delete renderns;
    }
    catch (...)
    {
        /* here we do not create a default object as the level/version must
         * match the parent object
         *
         * so do nothing
         */
    }


    if(pRenderPoint != NULL)
    {
        this->mListOfElements.appendAndOwn(pRenderPoint);
    }
    return pRenderPoint;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Returns a const pointer to the curve segment with the given index or NULL if
 * the id is invalid.
 *
 * @param index the index of the curve element to be returned
 *
 * @return a const pointer to the curve element with the given index or NULL 
 * if the index was out of bounds.
 */
const RenderPoint* RenderCurve::getElement(unsigned int index) const
{
    return (index < this->mListOfElements.size())?static_cast<const RenderPoint*>(this->mListOfElements.get(index)):NULL;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns a pointer to the curve segment with the given index or NULL if
 * the id is invalid.
 *
 * @param index the index of the curve element to be returned
 *
 * @return a pointer to the curve element with the given index or NULL 
 * if the index was out of bounds.
 */
RenderPoint* RenderCurve::getElement(unsigned int index)
{
    return (index < this->mListOfElements.size())?static_cast<RenderPoint*>(this->mListOfElements.get(index)):NULL;
}
/** @endcond */

#ifndef OMIT_DEPRECATED
/** @cond doxygenLibsbmlInternal */
/*
 * Returns a const pointer to the curve segment with the given index or NULL if
 * the id is invalid.
 *
 * This method call is deprecated, please use getElement instead.
 *
 * @param index the index of the curve element to be returned
 *
 * @return a const pointer to the curve element with the given index or NULL 
 * if the index was out of bounds.
 */
const RenderPoint* RenderCurve::getCurveElement(unsigned int index) const
{
#ifdef DEPRECATION_WARNINGS
    std::cerr << "Warning. \"const RenderPoint* RenderCurve::getCurveElement(unsigned int index) const\" is deprecated." << std::endl;
#endif // DEPRECATION_WARNINGS
    return this->getElement(index);
}
/** @endcond */
#endif // OMIT_DEPRECATED

#ifndef OMIT_DEPRECATED
/** @cond doxygenLibsbmlInternal */
/*
 * Returns a pointer to the curve segment with the given index or NULL if
 * the id is invalid.
 *
 * This method call is deprecated, please use getElement instead.
 *
 * @param index the index of the curve element to be returned
 *
 * @return a pointer to the curve element with the given index or NULL 
 * if the index was out of bounds.
 */
RenderPoint* RenderCurve::getCurveElement(unsigned int index)
{
#ifdef DEPRECATION_WARNINGS
    std::cerr << "Warning. \"RenderPoint* RenderCurve::getCurveElement(unsigned int index)\" is deprecated." << std::endl;
#endif // DEPRECATION_WARNINGS
    return this->getElement(index);
}
/** @endcond */
#endif // OMIT_DEPRECATED


/** @cond doxygenLibsbmlInternal */
/*
 * Adds a copy of the given curve segment to the end of the list of
 * curve segments.
 *
 * @param cs pointer to the RenderPoint object to be added to the end of the curve elements list.
 *
 * @return integer value indicating success/failure of the
 * function.  @if clike The value is drawn from the
 * enumeration #OperationReturnValues_t. @endif The possible values
 * returned by this function are:
 * @li LIBSBML_OPERATION_SUCCESS
 * @li LIBSBML_LEVEL_MISMATCH
 * @li LIBSBML_VERSION_MISMATCH
 * @li LIBSBML_OPERATION_FAILED
 *
 * @note This method should be used with some caution.  The fact that
 * this method @em copies the object passed to it means that the caller
 * will be left holding a physically different object instance than the
 * one contained in this RenderCurve.  Changes made to the original object
 * instance (such as resetting attribute values) will <em>not affect the
 * instance in the RenderCurve</em>.  In addition, the caller should make
 * sure to free the original object if it is no longer being used, or
 * else a memory leak will result.  Please see RenderCurve::createPoint()
 * or RenderCurve::createCubicBezier()
 * for methods that do not lead to these issues.
 *
 * @see createPoint()
 * @see createCubicBezier()
 */
int RenderCurve::addElement(const RenderPoint* cs)
{
    if (cs == NULL)
    {
        return LIBSBML_OPERATION_FAILED;
    }
    else if (!(cs->hasRequiredAttributes()) || !(cs->hasRequiredElements()))
    {
        return LIBSBML_INVALID_OBJECT;
    }
    else if (getLevel() != cs->getLevel())
    {
        return LIBSBML_LEVEL_MISMATCH;
    }
    else if (getVersion() != cs->getVersion())
    {
        return LIBSBML_VERSION_MISMATCH;
    }
    else
    {

        this->mListOfElements.append(cs);

        return LIBSBML_OPERATION_SUCCESS;
    }
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Removes the curve segment with the given index.
 * If the index is valid, a pointer to the removed element is returned
 * and the caller is responsible for deleting the object.
 * If the index is not valid, @c NULL is returned.
 *
 * @param i index of element to be removed.
 *
 * @return pointer to removed element.
 */
RenderPoint* RenderCurve::removeElement(unsigned int i)
{
    RenderPoint* pReturn=NULL;
    if(i < this->mListOfElements.size())
    {
        pReturn=dynamic_cast<RenderPoint*>(this->mListOfElements.remove(i));
    }
    return pReturn;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Returns a const pointer to the list of curve segments.
 *
 * @return const pointer to the ListOfCurveElements object for the RenderCurve.
 */
const ListOfCurveElements* RenderCurve::getListOfElements() const
{
    return &(this->mListOfElements);
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Returns a pointer to the list of curve segments.
 *
 * @return pointer to the ListOfCurveElements object for the RenderCurve.
 */
ListOfCurveElements* RenderCurve::getListOfElements()
{
    return &(this->mListOfElements);
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
int RenderCurve::getTypeCode() const
{
    return SBML_RENDER_CURVE;
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
bool RenderCurve::accept(SBMLVisitor& /*visitor*/) const
{
    return false;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Returns the XML element name of this object, which for
 * RenderCurve, is always @c "curve".
 * 
 * @return the name of this element, i.e., @c "curve".
 */
const std::string& RenderCurve::getElementName() const
{
  static std::string name = RenderCurve::ELEMENT_NAME;
  return name;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Creates and returns a deep copy of the RenderCurve object.
 *
 * @return a (deep) copy of this RenderCurve
 */
RenderCurve* RenderCurve::clone() const
{
    return new RenderCurve(*this);
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Returns true if the start head is set or false otherwise.
 * The start decoration is considered set if the string is not empty and if
 * it is not the string "none"
 *
 * @return true is the start decoration id is set
 */
bool RenderCurve::isSetStartHead() const
{
    return (!this->mStartHead.empty() && this->mStartHead!="none");
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Returns true if the end head is set or false otherwise.
 * The end decoration is considered set if the string is not empty and if
 * it is not the string "none"
 *
 * @return true is the end decoration id is set
 */
bool RenderCurve::isSetEndHead() const
{
    return (!this->mEndHead.empty() && this->mEndHead!="none");
}
/** @endcond */


void
RenderCurve::renameSIdRefs(const std::string& oldid, const std::string& newid)
{
  SBase::renameSIdRefs(oldid, newid);
  if (isSetStartHead() && mStartHead == oldid) 
  {
    setStartHead(newid);
  }

  if (isSetEndHead() && mEndHead == oldid) 
  {
    setEndHead(newid);
  }

}

 /** @cond doxygenLibsbmlInternal */
/*
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase* RenderCurve::createObject (XMLInputStream& stream)
{
  const std::string& name   = stream.peek().getName();
  SBase*        object = NULL;

  if (name == "listOfElements")
  {
    object = &this->mListOfElements;
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
void RenderCurve::writeAttributes (XMLOutputStream& stream) const
{
  GraphicalPrimitive1D::writeAttributes(stream);
  if(this->isSetStartHead())
  {
      stream.writeAttribute("startHead", getPrefix(), this->mStartHead);
  }
  if(this->isSetEndHead())
  {
      stream.writeAttribute("endHead", getPrefix(), this->mEndHead);
  }
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Creates an XMLNode object from this RenderCurve object.
 *
 * @return the XMLNode with the XML representation for the 
 * RenderCurve object.
 */
XMLNode RenderCurve::toXML() const
{
  return getXmlNodeForSBase(this);
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
void RenderCurve::writeElements (XMLOutputStream& stream) const
{
    GraphicalPrimitive1D::writeElements(stream);
    this->mListOfElements.write(stream);
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Sets the parent SBMLDocument of this SBML object.
 *
 * @param d The SBMLDocument to set on the objects and it's children if there are any.
 */
    void
RenderCurve::setSBMLDocument (SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
  this->mListOfElements.setSBMLDocument(d);
}
/** @endcond */


/*
 * Sets this SBML object to child SBML objects (if any).
 * (Creates a child-parent relationship by the parent)
 */
void
RenderCurve::connectToChild()
{
  GraphicalPrimitive1D::connectToChild();
  mListOfElements.connectToParent(this);
}

/*
 * Enables/Disables the given package with this element and child
 * elements (if any).
 * (This is an internal implementation for enablePakcage function)
 */
void
RenderCurve::enablePackageInternal(const std::string& pkgURI,
                                     const std::string& pkgPrefix, bool flag)
{
  SBase::enablePackageInternal(pkgURI,pkgPrefix,flag);

  mListOfElements.enablePackageInternal(pkgURI,pkgPrefix,flag);
}


/** @cond doxygenLibsbmlInternal */
/*
 * Sets the parent SBML object of this SBML object.
 *
 * @param sb the SBML object to use
 */
    void 
RenderCurve::setParentSBMLObject (SBase* sb)
{
    this->mParentSBMLObject = sb;
}
/** @endcond */



LIBSBML_CPP_NAMESPACE_END 
