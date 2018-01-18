/**
 * @file    LineEnding.cpp
 * @brief   class representing line endings,e.g. arrow heads
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

#include "LineEnding.h"
#include <algorithm>
#include <functional>
#include <sstream>
#include <assert.h>
#ifndef OMIT_DEPRECATED
#ifdef DEPRECATION_WARNINGS
#include <iostream>
#endif // DEPRECATION_WARNINGS
#endif // OMIT_DEPRECATED
#include <sbml/xml/XMLInputStream.h>

#include <sbml/packages/layout/util/LayoutAnnotation.h>
#include <sbml/packages/layout/util/LayoutUtilities.h>
#include <sbml/packages/render/extension/RenderExtension.h>

#include <sbml/util/ElementFilter.h>

LIBSBML_CPP_NAMESPACE_BEGIN

const std::string ListOfLineEndings::ELEMENT_NAME="listOfLineEndings";
const std::string LineEnding::ELEMENT_NAME="lineEnding";

/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new LineEnding object with the given SBML level
 * and SBML version.
 *
 * @param level SBML level of the new object
 * @param level SBML version of the new object
 */
LineEnding::LineEnding (unsigned int level, unsigned int version, unsigned int pkgVersion) : 
    GraphicalPrimitive2D(level,version, pkgVersion)
////    ,mId("")
    ,mEnableRotationalMapping(true)
    ,mBoundingBox(level, version, pkgVersion)
    ,mGroup(level,version, pkgVersion)
{
    //this->mBoundingBox.setIsInRenderContext(true);
    if (!hasValidLevelVersionNamespaceCombination())
        throw SBMLConstructorException();
      connectToChild();
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new LineEnding object with the given SBMLNamespaces.
 *
 * @param sbmlns The SBML namespace for the object.
 */
LineEnding::LineEnding (RenderPkgNamespaces* renderns):
    GraphicalPrimitive2D(renderns)
////    ,mId("")
    ,mEnableRotationalMapping(true)
    ,mBoundingBox(renderns->getLevel(), renderns->getVersion())
    ,mGroup(renderns)     
{
    //this->mBoundingBox.setIsInRenderContext(true);
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
LineEnding::~LineEnding ()
{
}


List*
LineEnding::getAllElements(ElementFilter *filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_ELEMENT(ret, sublist, mBoundingBox, filter);  
  ADD_FILTERED_ELEMENT(ret, sublist, mGroup, filter);  

  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}

/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new ListOfLineEndings object from the given XMLNode object.
 * The XMLNode object has to contain a valid XML representation of a 
 * ListOfLineEndings object as defined in the render extension specification.
 * This method is normally called when render information is read from a file and 
 * should normally not have to be called explicitely.
 *
 * @param node the XMLNode object reference that describes the ListOfLineEndings
 * object to be instantiated.
 */
    ListOfLineEndings::ListOfLineEndings(const XMLNode& node, unsigned int l2version)       
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
        if(childName=="lineEnding")
        {
            LineEnding* le=new LineEnding(*child, l2version);
            this->appendAndOwn(le);
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


/*
 * Ctor.
 */
ListOfLineEndings::ListOfLineEndings(RenderPkgNamespaces* renderns)
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
ListOfLineEndings::ListOfLineEndings(unsigned int level, unsigned int version, unsigned int pkgVersion)
 : ListOf(level,version)
{
  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(level,version,pkgVersion));
};

/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new LineEnding object from the given XMLNode object.
 * The XMLNode object has to contain a valid XML representation of a 
 * LineEnding object as defined in the render extension specification.
 * This method is normally called when render information is read from a file and 
 * should normally not have to be called explicitely.
 *
 * @param node the XMLNode object reference that describes the LineEnding
 * object to be instantiated.
 */
LineEnding::LineEnding(const XMLNode& node, unsigned int l2version)
  : GraphicalPrimitive2D(node, l2version)
  , mBoundingBox(2, l2version)
  , mGroup(2, l2version)
{
    const XMLNode* child;
    const XMLAttributes& attributes=node.getAttributes();
     ExpectedAttributes ea;
    addExpectedAttributes(ea);
    this->readAttributes(attributes, ea);
    unsigned int n=0,nMax = node.getNumChildren();
    while(n<nMax)
    {
        child=&node.getChild(n);
        const std::string& childName=child->getName();
        if(childName=="boundingBox")
        {
            this->mBoundingBox=BoundingBox(*child);
            //this->mBoundingBox.setIsInRenderContext(true);
        }
        else if(childName=="g")
        {
            this->mGroup=RenderGroup(*child);
        }
        ++n;
    }

    
  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(2,l2version));  

  connectToChild();

}
/** @endcond */

/*
 * Copy constructor.
 */
LineEnding::LineEnding(const LineEnding& other) : 
    GraphicalPrimitive2D(other)
//  , mId(other.mId)
  , mEnableRotationalMapping(other.mEnableRotationalMapping)
  , mBoundingBox(other.mBoundingBox)
  , mGroup(other.mGroup)
{    
  setId(other.mId);

    connectToChild();
}

#ifndef OMIT_DEPRECATED
/** @cond doxygenLibsbmlInternal */
/*
 * Constructor which creates a LineEnding with an empty group object,
 * and a viewport with a size of 0.
 * The id is set to the given value.
 * In order to get a valid object, the group object has to be valid,
 * the group object has to have descendants other than groups and
 * the viewport has to have a positive size.
 *
 * @param id The id for the LineEnding.
 *
 *
 * This constructor is deprecated. The new libsbml API only has
 * constructors which take the SBML level and version or one that takes
 * an SBMLNamespaces object.
 */
LineEnding::LineEnding(RenderPkgNamespaces* renderns, const std::string& id) :
    GraphicalPrimitive2D(renderns)
//    ,mId(id)
    ,mEnableRotationalMapping(true)
    ,mGroup(renderns)    
{
    //this->mBoundingBox.setParentSBMLObject(this);
    //this->mBoundingBox.setIsInRenderContext(true);
#ifdef DEPRECATION_WARNINGS
    std::cerr << "Warning. LineEnding::LineEnding(const std::string& id) is deprecated." << std::endl;
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
 * Sets whether rotational mapping is to be done or not.
 * This flag determines whether the LineEnding is rotated
 * according to the direction of the curve when it is applied.
 * For details on this, see the render extension specification.
 *
 * @param enable Boolean flag that specifies whether rotational mapping
 * for the line ending is to be enabled or not.
 */
void LineEnding::setEnableRotationalMapping(bool enable)
{
    this->mEnableRotationalMapping=enable;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns whether rotational mapping is enabled or not.
 *
 * @return bool value that specifies if rotational mapping is 
 * enabled for the LineEnding or not.
 */
bool LineEnding::getIsEnabledRotationalMapping() const
{
    return this->mEnableRotationalMapping;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the viewport for the LineEnding.
 *
 * @param box The viewport bounding box for the LineEnding.
 */
void LineEnding::setBoundingBox(const BoundingBox* box)
{
    this->mBoundingBox=*box;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns a pointer to the viewport bounding box.
 *
 * @return pointer to the viewport bounding box.
 */
BoundingBox* LineEnding::getBoundingBox()
{
    return &this->mBoundingBox;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns a const pointer to the viewport bounding box.
 *
 * @return const pointer to the viewport bounding box.
 */
const BoundingBox* LineEnding::getBoundingBox() const
{
    return &this->mBoundingBox;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the group of the LineEnding to a copy of the given group.
 *
 * @param group const pointer to the group to be set for the bounding box.
 * The group object is copied.
 */
void LineEnding::setGroup(const RenderGroup* group)
{
    this->mGroup=*group;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns a pointer to the group object.
 *
 * @return pointer to the group object
 */
RenderGroup* LineEnding::getGroup()
{
    return &this->mGroup;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Returns a const pointer to the group object.
 *
 * @return const pointer to the group object
 */
const RenderGroup* LineEnding::getGroup() const
{
    return &this->mGroup;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Creates and returns a deep copy of the ListOfLineEndings object.
 *
 * @return a (deep) copy of this ListOfLineEndings
 */
ListOfLineEndings* ListOfLineEndings::clone () const
{
    return new ListOfLineEndings(*this);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Copy constructor. Creates a copy of this ListOfLineEndings object.
 */
ListOfLineEndings::ListOfLineEndings(const ListOfLineEndings& source):ListOf(source)
{
}

/** @cond doxygenLibsbmlInternal */
/*
 * Assignment operator for ListOfLineEndings objects.
 */
ListOfLineEndings& ListOfLineEndings::operator=(const ListOfLineEndings& source)
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
 * (i.e., LineEnding objects, if the list is non-empty).
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
int ListOfLineEndings::getItemTypeCode () const
{
    return SBML_RENDER_LINEENDING;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the XML element name of this object, which for
 * ListOfLineEndings, is always @c "listOfLineEndings".
 * 
 * @return the name of this element, i.e., @c "listOfLineEndings".
 */
const std::string& ListOfLineEndings::getElementName () const
{
  static std::string name = ListOfLineEndings::ELEMENT_NAME;
  return name;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Creates an XMLNode object from this ListOfLineEndings object.
 *
 * @return the XMLNode with the XML representation for the 
 * ListOfLineEndings object.
 */
XMLNode ListOfLineEndings::toXML() const
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
void LineEnding::writeElements (XMLOutputStream& stream) const
{
    SBase::writeElements(stream);
    this->mBoundingBox.write(stream);
    this->mGroup.write(stream);
}

void 
LineEnding::writeXMLNS (XMLOutputStream& stream) const
{    
  XMLNamespaces xmlns;
  xmlns.add(mBoundingBox.getURI(), mBoundingBox.getPrefix());
  stream << xmlns;

}

/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase* LineEnding::createObject (XMLInputStream& stream)
{
    const std::string& name   = stream.peek().getName();
    SBase*        object = NULL ;


    if (name == "boundingBox")
    {
        object = &this->mBoundingBox;
    }
    else if (name == "g")
    {
        object = &this->mGroup;
    }
    return object;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase* ListOfLineEndings::createObject (XMLInputStream& stream)
{
    const std::string& name   = stream.peek().getName();
    SBase*        object = NULL;


    if (name == "lineEnding")
    {
      RENDER_CREATE_NS(renderns, this->getSBMLNamespaces());
      object = new LineEnding(renderns);
      if (object != NULL) this->mItems.push_back(object);
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
void LineEnding::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);
  stream.writeAttribute("id", getPrefix(), this->mId);
  if(this->mEnableRotationalMapping==false)
  {
      stream.writeAttribute("enableRotationalMapping", getPrefix(), std::string("false"));
  }
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Creates an XMLNode object from this LineEnding object.
 *
 * @return the XMLNode with the XML representation for the 
 * LineEnding object.
 */
XMLNode LineEnding::toXML() const
{
  return getXmlNodeForSBase(this);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
void
LineEnding::addExpectedAttributes(ExpectedAttributes& attributes)
{
  GraphicalPrimitive2D::addExpectedAttributes(attributes);

  attributes.add("id");
  attributes.add("enableRotationalMapping");
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
void LineEnding::readAttributes(const XMLAttributes& attributes, const ExpectedAttributes& expectedAttributes)
{
  GraphicalPrimitive2D::readAttributes(attributes, expectedAttributes);
    attributes.readInto("id", this->mId, getErrorLog(), false, getLine(), getColumn());
    if(!attributes.readInto("enableRotationalMapping", this->mEnableRotationalMapping, getErrorLog(), false, getLine(), getColumn()))
    {
        this->mEnableRotationalMapping=true;
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
int LineEnding::getTypeCode() const
{
    return SBML_RENDER_LINEENDING;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Accepts the given SBMLVisitor for this instance of LineEnding.
 *
 * @param v the SBMLVisitor instance to be used.
 *
 * @return the result of calling <code>v.visit()</code>.
 */
bool LineEnding::accept(SBMLVisitor& /*visitor*/) const
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
const std::string& LineEnding::getElementName() const
{
  static std::string name = LineEnding::ELEMENT_NAME;
  return name;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Creates and returns a deep copy of this LineEnding object.
 * 
 * @return a (deep) copy of this LineEnding object
 */
LineEnding* LineEnding::clone() const
{
    return new LineEnding(*this);
}

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the value of the "id" attribute of this GraphicalPrimitive.
 *
 * @return the id of the GraphicalPrimitive
 */
const std::string& LineEnding::getId () const
{
    return mId;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Predicate returning @c true or @c false depending on whether this
 * GraphicalPrimitive's "id" attribute has been set.
 *
 * @return returns true or false depending on whether the id on the 
 * GraphicalPrimitive has been set.
 */
bool LineEnding::isSetId () const
{
    return (mId.empty() == false);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the value of the "id" attribute of this GraphicalPrimitive.
 *
 * @param id the new id for the GraphicalPrimitive 
 *
 * @return status if the operation succeeded
 */
int LineEnding::setId (const std::string& id)
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
 * Unsets the value of the "id" attribute of this LineEnding.
 */
int LineEnding::unsetId ()
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
/*
 * Returns a pointer to the LineEnding with the given index or NULL if
 * the index is invalid.
 * 
 * @param i index of the LineEnding object to be returned
 * 
 * @return pointer to the LineEnding at the given index or NULL.
 */
LineEnding* ListOfLineEndings::get(unsigned int i)
{
    return static_cast<LineEnding*>(this->ListOf::get(i));
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns a const pointer to the LineEnding with the given index or NULL if
 * the index is invalid.
 * 
 * @param i index of the LineEnding object to be returned
 * 
 * @return const pointer to the LineEnding at the given index or NULL.
 */
const LineEnding* ListOfLineEndings::get(unsigned int i) const
{
    return static_cast<const LineEnding*>(this->ListOf::get(i));
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Used by ListOf::get() to lookup an SBase based by its id.
 */
struct IdEqLineEnding : public std::unary_function<SBase*, bool>
{
    const std::string& id;

    IdEqLineEnding (const std::string& id) : id(id) { }
    bool operator() (SBase* sb) 
    { return static_cast <LineEnding *> (sb)->getId() == id; }
};
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Returns a pointer to the LineEnding with the given @p id or @c NULL if
 * the id is invalid.
 * 
 * @param id id of the LineEnding object to be returned
 * 
 * @return pointer to the LineEnding at the given @p id or @c NULL.
 */
LineEnding* ListOfLineEndings::get(const std::string& id)
{
    return const_cast<LineEnding*>( 
            static_cast<const ListOfLineEndings*>(this)->get(id) );
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Returns a const pointer to the LineEnding with the given @p id or @c NULL if
 * the id is invalid.
 * 
 * @param id id of the LineEnding object to be returned
 * 
 * @return const pointer to the LineEnding at the given @p id or @c NULL.
 */
const LineEnding* ListOfLineEndings::get(const std::string& id) const
{
    std::vector<SBase*>::const_iterator result;

    result = std::find_if( mItems.begin(), mItems.end(), IdEqLineEnding(id) );
    return (result == mItems.end()) ? 0 : static_cast <LineEnding*> (*result);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/* Removes the nth item from this list */
    LineEnding*
ListOfLineEndings::remove (unsigned int n)
{
    return static_cast<LineEnding*>(ListOf::remove(n));
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/* Removes item in this list by id */
    LineEnding*
ListOfLineEndings::remove (const std::string& sid)
{
    SBase* item = NULL;
    std::vector<SBase*>::iterator result;

    result = std::find_if( mItems.begin(), mItems.end(), IdEqLineEnding(sid) );

    if (result != mItems.end())
    {
        item = *result;
        mItems.erase(result);
    }

    return static_cast <LineEnding*> (item);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the parent SBMLDocument of this SBML object.
 *
 * @param d The SBMLDocument to set on the objects and it's children if there are any.
 */
    void
LineEnding::setSBMLDocument (SBMLDocument* d)
{
    GraphicalPrimitive2D::setSBMLDocument(d);
    this->mBoundingBox.setSBMLDocument(d);
    this->mGroup.setSBMLDocument(d);
}
/** @endcond */

/*
 * Sets this SBML object to child SBML objects (if any).
 * (Creates a child-parent relationship by the parent)
 */
void
LineEnding::connectToChild()
{
  GraphicalPrimitive2D::connectToChild();
  mBoundingBox.connectToParent(this);
  mGroup.connectToParent(this);
}

/*
 * Enables/Disables the given package with this element and child
 * elements (if any).
 * (This is an internal implementation for enablePakcage function)
 */
void
LineEnding::enablePackageInternal(const std::string& pkgURI,
                                     const std::string& pkgPrefix, bool flag)
{
  GraphicalPrimitive2D::enablePackageInternal(pkgURI,pkgPrefix,flag);

  mBoundingBox.enablePackageInternal(pkgURI,pkgPrefix,flag);
  mGroup.enablePackageInternal(pkgURI,pkgPrefix,flag);
}





/** @cond doxygenLibsbmlInternal */
/*
 * Sets the parent SBML object of this SBML object.
 *
 * @param sb the SBML object to use
 */
    void 
LineEnding::setParentSBMLObject (SBase* sb)
{
    this->mParentSBMLObject = sb;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/* function returns true if component has all the required
 * attributes
 */
bool LineEnding::hasRequiredAttributes() const
{
    bool result = this->GraphicalPrimitive2D::hasRequiredAttributes();
    result = result &&
        (mBoundingBox.getPosition()->x() == mBoundingBox.getPosition()->x()) &&
        (mBoundingBox.getPosition()->y() == mBoundingBox.getPosition()->y()) &&
        (mBoundingBox.getDimensions()->getWidth() == mBoundingBox.getDimensions()->getWidth()) &&
        (mBoundingBox.getDimensions()->getHeight() == mBoundingBox.getDimensions()->getHeight());
    result = result && this->isSetId();
    return result;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/* function returns true if component has all the required
 * elements
 */
bool LineEnding::hasRequiredElements() const 
{
    bool result = this->GraphicalPrimitive2D::hasRequiredElements();
    return result;
}
/** @endcond */



LIBSBML_CPP_NAMESPACE_END 
