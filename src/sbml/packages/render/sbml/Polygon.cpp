/**
 * @file    Polygon.cpp
 * @brief   class for representing a polygon
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

#include "Polygon.h"
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

const std::string Polygon::ELEMENT_NAME="polygon";

/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new Polygon object with the given SBML level
 * and SBML version.
 *
 * @param level SBML level of the new object
 * @param level SBML version of the new object
 */
Polygon::Polygon (unsigned int level, unsigned int version, unsigned int pkgVersion) 
  : GraphicalPrimitive2D(level,version, pkgVersion)
  , mListOfElements(level, version, pkgVersion)
{
    if (!hasValidLevelVersionNamespaceCombination())
        throw SBMLConstructorException();
      connectToChild();
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new Polygon object with the given SBMLNamespaces.
 *
 * @param sbmlns The SBML namespace for the object.
 */
Polygon::Polygon (RenderPkgNamespaces* renderns)
  : GraphicalPrimitive2D(renderns)
  , mListOfElements(renderns)
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
 * Creates a new Polygon object from the given XMLNode object.
 * The XMLNode object has to contain a valid XML representation of a 
 * Polygon object as defined in the render extension specification.
 * This method is normally called when render information is read from a file and 
 * should normally not have to be called explicitely.
 *
 * @param node the XMLNode object reference that describes the Polygon
 * object to be instantiated.
 */
Polygon::Polygon(const XMLNode& node, unsigned int l2version)
  : GraphicalPrimitive2D(node, l2version)
  , mListOfElements(node, l2version)
{
   ExpectedAttributes ea;
    addExpectedAttributes(ea);
    this->readAttributes(node.getAttributes(), ea);
    const XMLNode* child;
    unsigned int n=0,nMax = node.getNumChildren();
    RENDER_CREATE_NS(renderns, this->getSBMLNamespaces());
    while(n<nMax)
    {
        child=&node.getChild(n);
        const std::string& childName=child->getName();
        if(childName=="listOfElements")
        {
            this->mListOfElements=ListOfCurveElements(*child);
        }
        // keep to read old version
        else if(childName=="listOfCurveSegments")
        {
            // we have to read that in differently
            unsigned int i=0,iMax = child->getNumChildren();
            const XMLNode* child2;
            while(i<iMax)
            {
                child2=&child->getChild(i);
                const std::string& childName2=child2->getName();
                if(childName2=="curveSegment")
                {
                    const XMLAttributes& innerAttributes=child2->getAttributes();
                    int typeIndex=innerAttributes.getIndex("type");
                    if(typeIndex==-1 || innerAttributes.getURI(typeIndex)!="http://www.w3.org/2001/XMLSchema-instance")
                    {
                        // throw
                        ++i;
                        continue;
                    }
                    // read the elements of a cubic bezier
                    // if it is the first element, we need the start point,
                    // else we only need the two basepoints and the
                    // endpoint
                    unsigned int j,jMax=child2->getNumChildren();
                    bool startSet=false;
                    bool endSet=false;
                    RenderPoint start(renderns);
                    RenderPoint* end = new RenderPoint(renderns);
                    for(j=0;j<jMax;++j)
                    {
                        const XMLNode* child3=&child2->getChild(j);
                        std::string childName3=child3->getName();
                        if(childName3=="start")
                        {
                            start=RenderPoint(*child3);
                            startSet=true;

                        }
                        // add the basepoints and the endpoint
                        else if(childName3=="end")
                        {
                          delete end; 
                            end=new RenderPoint(*child3);
                            endSet=true;

                            if (jMax > 2)
                            {
                              RelAbsVector x = end->x();
                              RelAbsVector y = end->y();
                              RelAbsVector z = end->z();

                              delete end;
                              RenderCubicBezier* cend = new RenderCubicBezier(renderns);
                              cend->setX(x);
                              cend->setY(y);
                              cend->setZ(z);

                              RenderPoint* basePoint1 = new RenderPoint(child2->getChild("basePoint1"));
                              cend->setBasePoint1(basePoint1->x(), basePoint1->y(), basePoint1->z());
                              delete basePoint1;
                              
                              RenderPoint* basePoint2 = new RenderPoint(child2->getChild("basePoint2"));
                              cend->setBasePoint2(basePoint2->x(), basePoint2->y(), basePoint2->z());
                              delete basePoint2;

                              end = cend;



                            }

                        }
                    }
                    if(!startSet || !endSet)
                    {
                        // skip this point
                        // TODO this is an error
                        continue;
                    }
                    if(this->mListOfElements.size()==0)
                    {
                        // add the start point
                        this->mListOfElements.appendAndOwn(new RenderPoint(start));
                    }
                    // add the end point
                    this->mListOfElements.appendAndOwn(end);                    

                }
                else if(childName2=="annotation")
                {
                    this->mListOfElements.setAnnotation(new XMLNode(*child));
                }
                else if(childName2=="notes")
                {
                    this->mListOfElements.setNotes(new XMLNode(*child));
                }
                ++i;
            }
        }
        ++n;
    }

    
  delete renderns;
  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(2,l2version));  

  connectToChild();
}
/** @endcond */


/*
 * Destroy this object.
 */
Polygon::~Polygon ()
{
}


#ifndef OMIT_DEPRECATED
/** @cond doxygenLibsbmlInternal */
/*
 * Instanciates a polygon with the given @p id and no elements.
 * All attributes inherited from GraphicalPrimitive are set as described
 * in the corresponding constructor of that class (@see GraphicalPrimitive2D)
 *
 * @param id id string for the polygon
 *
 * This constructor is deprecated. The new libsbml API only has
 * constructors which take the SBML level and version or one that takes
 * an SBMLNamespaces object.
 */
Polygon::Polygon(RenderPkgNamespaces* renderns, const std::string& id)
  : GraphicalPrimitive2D(renderns, id)
  , mListOfElements(renderns)
{
#ifdef DEPRECATION_WARNINGS
    std::cerr << "Warning. Polygon::Polygon(const std::string& id) is deprecated." << std::endl;
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

List*
Polygon::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_LIST(ret, sublist, mListOfElements, filter);

  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}


/** @cond doxygenLibsbmlInternal */
/*
 * Returns the number of segments.
 *
 * @return number of elements in the polygon.
 */
unsigned int Polygon::getNumElements() const
{
    return this->mListOfElements.size();
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns a const pointer to the list of segments.
 *
 * @return const pointer to the ListOfCurveElements object for the Polygon.
 */
ListOfCurveElements* Polygon::getListOfElements()
{
    return &(this->mListOfElements);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns a pointer to the list of segments.
 *
 * @return pointer to the ListOfCurveElements object for the Polygon.
 */
const ListOfCurveElements* Polygon::getListOfElements() const
{
    return &(this->mListOfElements);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new point element.
 * The element is added to and owned by the polygon.
 *
 * @return The newly created RenderCubicBezier object.
 */
RenderPoint* Polygon::createPoint()
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
 * Creates a new bezier element.
 * The element is added to and owned by the polygon.
 *
 * @return The newly created RenderCubicBezier object.
 */
RenderCubicBezier* Polygon::createCubicBezier()
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
 * Returns a pointer to the segment with the given index or NULL if
 * the id is invalid.
 *
 * @param index the index of the element to be returned
 *
 * @return a pointer to the element with the given index or NULL 
 * if the index was out of bounds.
 */
RenderPoint* Polygon::getElement(unsigned int index)
{
    return (index<this->mListOfElements.size())?static_cast<RenderPoint*>(this->mListOfElements.get(index)):NULL;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns a const pointer to the segment with the given index or NULL if
 * the id is invalid.
 *
 * @param index the index of the element to be returned
 *
 * @return a const pointer to the element with the given index or NULL 
 * if the index was out of bounds.
 */
const RenderPoint* Polygon::getElement(unsigned int index) const
{
    return (index<this->mListOfElements.size())?static_cast<const RenderPoint*>(this->mListOfElements.get(index)):NULL;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Adds a copy of the given segment to the end of the list of
 * segments.
 *
 * @param cs pointer to the RenderPoint object to be added to the end of the elements list.
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
 * one contained in this Polygon.  Changes made to the original object
 * instance (such as resetting attribute values) will <em>not affect the
 * instance in the Polygon</em>.  In addition, the caller should make
 * sure to free the original object if it is no longer being used, or
 * else a memory leak will result.  Please see Polygon::createPoint()
 * or Polygon::createCubicBezier()
 * for methods that do not lead to these issues.
 *
 * @see createPoint()
 * @see createCubicBezier()
 */
int Polygon::addElement(const RenderPoint* ls)
{
    if (ls == NULL)
    {
        return LIBSBML_OPERATION_FAILED;
    }
    else if (!(ls->hasRequiredAttributes()) || !(ls->hasRequiredElements()))
    {
        return LIBSBML_INVALID_OBJECT;
    }
    else if (getLevel() != ls->getLevel())
    {
        return LIBSBML_LEVEL_MISMATCH;
    }
    else if (getVersion() != ls->getVersion())
    {
        return LIBSBML_VERSION_MISMATCH;
    }
    else
    {

        this->mListOfElements.append(ls);

        return LIBSBML_OPERATION_SUCCESS;
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
int Polygon::getTypeCode() const
{
    return SBML_RENDER_POLYGON;
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
bool Polygon::accept(SBMLVisitor& /*visitor*/) const
{
    return false;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the XML element name of this object, which for
 * Polygon, is always @c "polygon".
 * 
 * @return the name of this element, i.e., @c "polygon".
 */	
const std::string& Polygon::getElementName() const
{
  static std::string name = Polygon::ELEMENT_NAME;
  return name;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Creates and returns a deep copy of the Polygon object.
 *
 * @return a (deep) copy of this Polygon
 */
Polygon* Polygon::clone() const
{
    return new Polygon(*this);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
void
Polygon::addExpectedAttributes(ExpectedAttributes& attributes)
{
  GraphicalPrimitive2D::addExpectedAttributes(attributes);

}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
void Polygon::readAttributes (const XMLAttributes& attributes, const ExpectedAttributes& expectedAttributes)
{
    this->GraphicalPrimitive2D::readAttributes(attributes,expectedAttributes);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase* Polygon::createObject (XMLInputStream& stream)
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
void Polygon::writeAttributes (XMLOutputStream& stream) const
{
  GraphicalPrimitive2D::writeAttributes(stream);
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Creates an XMLNode object from this Polygon object.
 *
 * @return the XMLNode with the XML representation for the 
 * Polygon object.
 */
XMLNode Polygon::toXML() const
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
void Polygon::writeElements (XMLOutputStream& stream) const
{
    GraphicalPrimitive2D::writeElements(stream);
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
Polygon::setSBMLDocument (SBMLDocument* d)
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
Polygon::connectToChild()
{
  GraphicalPrimitive2D::connectToChild();
  mListOfElements.connectToParent(this);
}

/*
 * Enables/Disables the given package with this element and child
 * elements (if any).
 * (This is an internal implementation for enablePakcage function)
 */
void
Polygon::enablePackageInternal(const std::string& pkgURI,
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
Polygon::setParentSBMLObject (SBase* sb)
{
    this->mParentSBMLObject = sb;
}
/** @endcond */


LIBSBML_CPP_NAMESPACE_END 
