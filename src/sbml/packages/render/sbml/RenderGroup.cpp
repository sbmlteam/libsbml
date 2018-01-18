/**
 * @file    RenderGroup.cpp
 * @brief   class for grouping render elements together
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

#include "RenderGroup.h"

#include <sbml/util/ElementFilter.h>

#include <algorithm>
#include <functional>
#include <limits>
#include <sstream>
#include <assert.h>
#ifndef OMIT_DEPRECATED
#ifdef DEPRECATION_WARNINGS
#include <iostream>
#endif // DEPRECATION_WARNINGS
#endif // OMIT_DEPRECATED

#include "RenderCurve.h"
#include "Polygon.h"
#include "Ellipse.h"
#include "Rectangle.h"
#include "Text.h"
#include "Image.h"
#include "RenderPoint.h"
#include "RenderCubicBezier.h"

#include <sbml/xml/XMLInputStream.h>

#include <sbml/packages/layout/util/LayoutAnnotation.h>
#include <sbml/packages/render/extension/RenderExtension.h>

LIBSBML_CPP_NAMESPACE_BEGIN

const std::string RenderGroup::ELEMENT_NAME="g";
const std::string ListOfDrawables::ELEMENT_NAME="listOfDrawables";

/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new RenderGroup object with the given SBML level
 * and SBML version.
 *
 * @param level SBML level of the new object
 * @param level SBML version of the new object
 */
RenderGroup::RenderGroup (unsigned int level, unsigned int version, unsigned int pkgVersion) 
  : GraphicalPrimitive2D(level,version, pkgVersion)
  , mFontFamily("")
  , mFontSize(RelAbsVector(std::numeric_limits<double>::quiet_NaN(),std::numeric_limits<double>::quiet_NaN()))
  , mFontWeight(Text::WEIGHT_UNSET)
  , mFontStyle(Text::STYLE_UNSET)
  , mTextAnchor(Text::ANCHOR_UNSET)
  , mVTextAnchor(Text::ANCHOR_UNSET)
  , mStartHead("")
  , mEndHead("")
  , mElements(level, version, pkgVersion)
{
    if (!hasValidLevelVersionNamespaceCombination())
        throw SBMLConstructorException();
      connectToChild();
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new RenderGroup object with the given SBMLNamespaces.
 *
 * @param sbmlns The SBML namespace for the object.
 */
RenderGroup::RenderGroup (RenderPkgNamespaces* renderns):
    GraphicalPrimitive2D(renderns),
    mFontFamily(""),
    mFontSize(RelAbsVector(std::numeric_limits<double>::quiet_NaN(),std::numeric_limits<double>::quiet_NaN())),
    mFontWeight(Text::WEIGHT_UNSET),
    mFontStyle(Text::STYLE_UNSET),
    mTextAnchor(Text::ANCHOR_UNSET),
    mVTextAnchor(Text::ANCHOR_UNSET),
    mStartHead(""),
    mEndHead("")
    , mElements(renderns)
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

    
List*
RenderGroup::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_LIST(ret, sublist, mElements, filter);

  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}


/** @cond doxygenLibsbmlInternal */
/*
 * Subclasses should override this method to get the list of
 * expected attributes.
 * This function is invoked from corresponding readAttributes()
 * function.
 */
void
RenderGroup::addExpectedAttributes(ExpectedAttributes& attributes)
{
  GraphicalPrimitive2D::addExpectedAttributes(attributes);

  attributes.add("startHead");
  attributes.add("endHead");
  attributes.add("font-family");
  attributes.add("font-size");
  attributes.add("font-weight");
  attributes.add("font-style");
  attributes.add("text-anchor");
  attributes.add("vtext-anchor");
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void RenderGroup::readAttributes (const XMLAttributes& attributes, const ExpectedAttributes& expectedAttributes)
{
  GraphicalPrimitive2D::readAttributes(attributes, expectedAttributes);
    std::string s;
    double NaN=std::numeric_limits<double>::quiet_NaN();
    attributes.readInto("startHead", this->mStartHead, getErrorLog(), false, getLine(), getColumn());
    attributes.readInto("endHead", this->mEndHead, getErrorLog(), false, getLine(), getColumn());
    attributes.readInto("font-family", this->mFontFamily, getErrorLog(), false, getLine(), getColumn());
    Text::FONT_WEIGHT fw=Text::WEIGHT_UNSET;
    Text::FONT_STYLE fs=Text::STYLE_UNSET;
    Text::TEXT_ANCHOR ta=Text::ANCHOR_UNSET;
    Text::TEXT_ANCHOR vta=Text::ANCHOR_UNSET;
    if(attributes.readInto("font-size", s, getErrorLog(), false, getLine(), getColumn()))
    {
        this->mFontSize=RelAbsVector(s);
    }
    else
    {
        this->mFontSize=RelAbsVector(NaN,NaN);
    }
    if(attributes.readInto("font-weight", s, getErrorLog(), false, getLine(), getColumn()))
    {
        if(s=="bold")
        {
            fw=Text::WEIGHT_BOLD;
        }
        else if (s == "normal")
        {
          fw = Text::WEIGHT_NORMAL;
        }
    }
    if(attributes.readInto("font-style", s, getErrorLog(), false, getLine(), getColumn()))
    {
        if(s=="italic")
        {
            fs=Text::STYLE_ITALIC;
        }
        else if (s == "normal")
        {
          fs = Text::STYLE_NORMAL;
        }
    }
    if(attributes.readInto("text-anchor", s, getErrorLog(), false, getLine(), getColumn()))
    {
        if(s=="end")
        {
            ta=Text::ANCHOR_END;
        }
        else if(s=="middle")
        {
            ta=Text::ANCHOR_MIDDLE;
        }
        else if(s=="start")
        {
            ta=Text::ANCHOR_START;
        }
    }
    if(attributes.readInto("vtext-anchor", s, getErrorLog(), false, getLine(), getColumn()))
    {
        if(s=="bottom")
        {
            vta=Text::ANCHOR_BOTTOM;
        }
        else if(s=="middle")
        {
            vta=Text::ANCHOR_MIDDLE;
        }
        else if(s=="top")
        {
            vta=Text::ANCHOR_TOP;
        }
        /*
        else if(s=="baseline")
        {
            vta=Text::ANCHOR_BASELINE;
        }
        */
    }
    this->setFontWeight(fw);
    this->setFontStyle(fs);
    this->setTextAnchor(ta);
    this->setVTextAnchor(vta);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new RenderGroup object from the given XMLNode object.
 * The XMLNode object has to contain a valid XML representation of a 
 * RenderGroup object as defined in the render extension specification.
 * This method is normally called when render information is read from a file and 
 * should normally not have to be called explicitely.
 *
 * @param node the XMLNode object reference that describes the RenderGroup
 * object to be instantiated.
 */
RenderGroup::RenderGroup(const XMLNode& node, unsigned int l2version)
  : GraphicalPrimitive2D(node, l2version)
  , mElements(2, l2version)
{  
    ExpectedAttributes ea;
    addExpectedAttributes(ea);
    const XMLAttributes& attributes=node.getAttributes();
    const XMLNode* child;
    this->readAttributes(attributes, ea);
    unsigned int n=0,nMax = node.getNumChildren();
    while(n<nMax)
    {
        child=&node.getChild(n);
        const std::string& childName=child->getName();
        if(childName=="g")
        {
            RenderGroup* g=new RenderGroup(*child);
            mElements.appendAndOwn(g);
        }
        else if(childName=="curve")
        {
            // check whether this is an old curve or a new curve
            unsigned int i,iMax=child->getNumChildren();
            for(i=0;i<iMax;++i)
            {
                // we check only if the new element name is there
                // if that is the case, we just assume it is a new curve
                if(child->getChild(i).getName()=="listOfElements")
                {
                    RenderCurve* c=new RenderCurve(*child);
                    mElements.appendAndOwn(c);
                    break;
                }
                else if(child->getChild(i).getName()=="listOfCurveSegments")
                {
                    this->importOldCurve(*child);
                    break;
                }
            }
        }
        else if(childName=="polygon")
        {
            Polygon* p=new Polygon(*child);
            mElements.appendAndOwn(p);
        }
        else if(childName=="rectangle")
        {
            Rectangle* r=new Rectangle(*child);
            mElements.appendAndOwn(r);
        }
        else if(childName=="ellipse")
        {
            Ellipse* e=new Ellipse(*child);
            mElements.appendAndOwn(e);
        }
        else if(childName=="text")
        {
            Text* t=new Text(*child);
            mElements.appendAndOwn(t);
        }
        else if(childName=="image")
        {
            Image* i=new Image(*child);
            mElements.appendAndOwn(i);
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




#ifndef OMIT_DEPRECATED
/** @cond doxygenLibsbmlInternal */
/*
 * Instantiates a new RenderGroup object.
 * All attributes are set as described for the default constructor
 * of GraphicalPrimitive2D.
 * All the font rendering attributes and the curve decorations
 * are unset. The id is set to the given string.
 *
 * @param id the id for the RenderGroup object.
 *
 * This constructor is deprecated. The new libsbml API only has
 * constructors which take the SBML level and version or one that takes
 * an SBMLNamespaces object.
 */
RenderGroup::RenderGroup(RenderPkgNamespaces* renderns, const std::string& id):
    GraphicalPrimitive2D(renderns, id),
    mFontFamily(""),
    mFontSize(RelAbsVector(std::numeric_limits<double>::quiet_NaN(),std::numeric_limits<double>::quiet_NaN())),
    mFontWeight(Text::WEIGHT_UNSET),
    mFontStyle(Text::STYLE_UNSET),
    mTextAnchor(Text::ANCHOR_UNSET),
    mVTextAnchor(Text::ANCHOR_UNSET),
    mStartHead(""),
    mEndHead("")
    , mElements(renderns)
{
#ifdef DEPRECATION_WARNINGS
    std::cerr << "Warning. RenderGroup::RenderGroup(const std::string& id) is deprecated." << std::endl;
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

/*
 * Sets the font family.
 *
 * @param family The name of the font family, e.g. Helvetica
 */
void RenderGroup::setFontFamily(const std::string& family)
{
    this->mFontFamily=family;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the font size.
 * Normally this is an absolute value, e.g. 18 for a 18pt font.
 * It is however allowed the specify the font size in terms of relative values
 * in relation to the current viewport. In most cases the viewport will be the 
 * dimensions of a bounding box of a layout object.
 *
 * @param size the new font size.
 */
void RenderGroup::setFontSize(const RelAbsVector& size)
{
    this->mFontSize=size;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the font weight.
 * Valid values are Text::WEIGHT_UNSET, Text::WEIGHT_NORMAL or
 * Text::WEIGHT_BOLD.
 *
 * @param weight The new text weight to be set.
 */
void RenderGroup::setFontWeight(Text::FONT_WEIGHT weight)
{
    this->mFontWeight=weight;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the font style.
 * Valid values are Text::STYLE_UNSET, Text::STYLE_NORMAL or
 * Text::STYLE_ITALIC
 *
 * @param style The new font style to be set.
 */
void RenderGroup::setFontStyle(Text::FONT_STYLE style)
{
    this->mFontStyle=style;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the text anchor.
 * This is defines the horizontal text position.
 * Valid values are Text::ANCHOR_UNSET, Text::ANCHOR_START,
 * Text::ANCHOR_MIDDLE and Text::ANCHOR_END.
 * Text::ANCHOR_BASELINE is not a valid value for the
 * text-anchor flag. If you set the text anchor to 
 * Text::ANCHOR_BASELINE, it will be set to Text::ANCHOR_UNSET.
 *
 * @param anchor The new horizontal alignment flag.
 */
void RenderGroup::setTextAnchor(Text::TEXT_ANCHOR anchor)
{
   /* 
   if(anchor == Text::ANCHOR_BASELINE)
   {
     this->mTextAnchor=Text::ANCHOR_UNSET;
   }
   else
   */
   {
     this->mTextAnchor=anchor;
   }
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the vertical text anchor.
 * This is defines the vertical text position.
 * Valid values are Text::ANCHOR_UNSET, Text::ANCHOR_TOP,
 * Text::ANCHOR_MIDDLE, Text::ANCHOR_BOTTOM and
 * Text::ANCHOR_BASELINE.
 *
 * @param anchor The new vertical alignment flag.
 */
void RenderGroup::setVTextAnchor(Text::TEXT_ANCHOR anchor)
{
    this->mVTextAnchor=anchor;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the id of the start head.
 *
 * @param The id of a LineEnding object to be applied to the start of curve children.
 */
void RenderGroup::setStartHead(const std::string& id)
{
    this->mStartHead=id;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the id of the end head.
 *
 * @param The id of a LineEnding object to be applied to the end of curve children. 
 */
void RenderGroup::setEndHead(const std::string& id)
{
    this->mEndHead=id;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the font family.
 *
 * @return The name of the font family to be used for text rendering.
 */
const std::string& RenderGroup::getFontFamily() const
{
    return this->mFontFamily;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the font size as a reference.
 *
 * @return A reference to the size to be used for rendering text.
 */
RelAbsVector& RenderGroup::getFontSize() 
{
    return this->mFontSize;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the font size as a const reference.
 *
 * @return A const reference to the size to be used for rendering text.
 */
const RelAbsVector& RenderGroup::getFontSize() const
{
    return this->mFontSize;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the font weight.
 *
 * @return font weight used to render text children
 */
Text::FONT_WEIGHT RenderGroup::getFontWeight() const
{
    return this->mFontWeight;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the font style.
 *
 * @return font style used to render text children
 */
Text::FONT_STYLE RenderGroup::getFontStyle() const
{
    return this->mFontStyle;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the text anchor.
 *
 * @return the horizontal text alignment flag
 */
Text::TEXT_ANCHOR RenderGroup::getTextAnchor() const
{
    return this->mTextAnchor;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the vertical text anchor.
 *
 * @return the vertical text alignment flag
 */
Text::TEXT_ANCHOR RenderGroup::getVTextAnchor() const
{
    return this->mVTextAnchor;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the id of the LineEnding object to be applied to the start of the curve.
 *
 * @return id of the LineEnding for the start of curves.
 */
const std::string& RenderGroup::getStartHead() const
{
    return this->mStartHead;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the id of the LineEnding object to be applied to the end of the curve.
 *
 * @return id of the LineEnding for the end of curves.
 */
const std::string& RenderGroup::getEndHead() const
{
    return this->mEndHead;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Creates and returns a deep copy of this RenderGroup object.
 * 
 * @return a (deep) copy of this RenderGroup object
 */
RenderGroup* RenderGroup::clone() const
{
    return new RenderGroup(*this);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the XML element name of this object, which for
 * RenderGroup, is always @c "g".
 * 
 * @return the name of this element, i.e., @c "g".
 */
const std::string& RenderGroup::getElementName() const
{
  static std::string name = RenderGroup::ELEMENT_NAME;
  return name;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Accepts the given SBMLVisitor for this instance of RenderGroup.
 *
 * @param v the SBMLVisitor instance to be used.
 *
 * @return the result of calling <code>v.visit()</code>.
 */
bool RenderGroup::accept(SBMLVisitor& visitor) const
{
    return false;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the number of children in the group.
 *
 * @return The number of child elements in the group.
 */
unsigned int RenderGroup::getNumElements() const
{
    return this->mElements.size();
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns a const pointer to the list of  elements.
 *
 * @return const pointer to the list of children
 */
const ListOfDrawables* RenderGroup::getListOfElements() const
{
    return &this->mElements;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the name of this element.
 * 
 * @return the name of the element, in this case "listOfDrawables"
 */
const std::string& ListOfDrawables::getElementName() const
{
  static std::string name = ListOfDrawables::ELEMENT_NAME;
  return name;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Returns the list of  elements.
 *
 * @return pointer to the list of children
 */
ListOfDrawables* RenderGroup::getListOfElements()
{
    return &this->mElements;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Returns pointer to the element with the given @p id.
 * If there is no such element, @c NULL is returned.
 * Since the id on all those object is optional, this routine
 * might not be as helpful as similar routines in other classes.
 *
 * @param id id of element to be returned
 *
 * @return pointer to element with id or NULL if
 * there is no object with that id
 */
Transformation2D* RenderGroup::getElement(const std::string& id)
{
    return this->mElements.get(id);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns const pointer to the element with given index.
 * If there is no such element, @c NULL is returned.
 * Since the id on all those object is optional, this routine
 * might not be as helpful as similar routines in other classes.
 *
 * @param id id of element to be returned
 *
 * @return pointer to element with the given @p id or @c NULL if
 *  there is no object with that id
 */
const Transformation2D* RenderGroup::getElement(const std::string& id) const
{
    return this->mElements.get(id);
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Returns pointer to the element with index n.
 * If there is no such element, @c NULL is returned.
 *
 * @param index index of element to be returned
 *
 * @return pointer to element with index index or NULL if
 * index is out of bounds.
 */
Transformation2D* RenderGroup::getElement(unsigned int n)
{
    if(n < this->mElements.size())
    {
        return this->mElements.get(n);
    }
    else
    {
        return NULL;
    }
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Returns const pointer to the element with index n.
 * If there is no such element, @c NULL is returned.
 *
 * @param index index of element to be returned
 *
 * @return pointer to element with index index or NULL if
 * index is out of bounds.
 */
const Transformation2D* RenderGroup::getElement(unsigned int n) const 
{
    if(n < this->mElements.size())
    {
        return this->mElements.get(n);
    }
    else
    {
        return NULL;
    }
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Creates an image object and adds it to the end of the list of child
 * elements. The new element is owned by the group.
 *
 * @return pointer to the new Image child.
 */
Image* RenderGroup::createImage()
{
    Image* pImage=NULL;
    try
    {
      RENDER_CREATE_NS(renderns, this->getSBMLNamespaces());
      pImage = new Image(renderns);
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


    if(pImage != NULL)
    {
        this->mElements.appendAndOwn(pImage);
    }
    return pImage;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Creates an group object and adds it to the end of the list of child
 * elements The new element is owned by the group..
 *
 * @return pointer to the new RenderGroup child.
 */
RenderGroup* RenderGroup::createGroup()
{
    RenderGroup* pRenderGroup=NULL;
    try
    {
      RENDER_CREATE_NS(renderns, this->getSBMLNamespaces());
      pRenderGroup = new RenderGroup(renderns);
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


    if(pRenderGroup != NULL)
    {
        this->mElements.appendAndOwn(pRenderGroup);
    }
    return pRenderGroup;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Creates a rectangle object and adds it to the end of the list of child
 * elements The new element is owned by the group..
 *
 * @return pointer to the new Rectangle child.
 */
Rectangle* RenderGroup::createRectangle()
{
    Rectangle* pRectangle=NULL;
    try
    {
      RENDER_CREATE_NS(renderns, this->getSBMLNamespaces());
      pRectangle = new Rectangle(renderns);
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


    if(pRectangle != NULL)
    {
        this->mElements.appendAndOwn(pRectangle);
    }
    return pRectangle;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Creates an ellipse object and adds it to the end of the list of child
 * elements The new element is owned by the group..
 *
 * @return pointer to the new Ellipse child.
 */
Ellipse* RenderGroup::createEllipse()
{
    Ellipse* pEllipse=NULL;
    try
    {
      RENDER_CREATE_NS(renderns, this->getSBMLNamespaces());
      pEllipse = new Ellipse(renderns);
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


    if(pEllipse != NULL)
    {
        this->mElements.appendAndOwn(pEllipse);
    }
    return pEllipse;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Creates a curve object and adds it to the end of the list of child
 * elements The new element is owned by the group..
 *
 * @return pointer to the new RenderCurve child.
 */
RenderCurve* RenderGroup::createCurve()
{
    RenderCurve* pRenderCurve=NULL;
    try
    {
      RENDER_CREATE_NS(renderns, this->getSBMLNamespaces());
      pRenderCurve = new RenderCurve(renderns);
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


    if(pRenderCurve != NULL)
    {
        this->mElements.appendAndOwn(pRenderCurve);
    }
    return pRenderCurve;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Creates a polygon object and adds it to the end of the list of child
 * elements The new element is owned by the group..
 *
 * @return pointer to the new Polygon child.
 */
Polygon* RenderGroup::createPolygon()
{
    Polygon* pPolygon=NULL;
    try
    {
      RENDER_CREATE_NS(renderns, this->getSBMLNamespaces());
      pPolygon = new Polygon(renderns);
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


    if(pPolygon != NULL)
    {
        this->mElements.appendAndOwn(pPolygon);
    }
    return pPolygon;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Creates a text object and adds it to the end of the list of child
 * elements The new element is owned by the group..
 *
 * @return pointer to the new Text child.
 */
Text* RenderGroup::createText()
{
    Text* pText=NULL;
    try
    {
      RENDER_CREATE_NS(renderns, this->getSBMLNamespaces());
      pText = new Text(renderns);
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


    if(pText != NULL)
    {
        this->mElements.appendAndOwn(pText);
    }
    return pText;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Adds a copy of the given element to the end of the list of children elements.
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
 * one contained in this RenderGroup.  Changes made to the original object
 * instance (such as resetting attribute values) will <em>not affect the
 * instance in the RenderGroup</em>.  In addition, the caller should make
 * sure to free the original object if it is no longer being used, or
 * else a memory leak will result.  Please see RenderGroup::createXXX()
 * for methods that do not lead to these issues.
 *
 * @see createEllipse()
 * @see createRectangle()
 * @see createPolygon()
 * @see createText()
 * @see createCurve()
 * @see createRenderGroup()
 * @see createImage()
 */
int RenderGroup::addChildElement(const Transformation2D* pChild)
{
    if (pChild == NULL)
    {
        return LIBSBML_OPERATION_FAILED;
    }
    else if (!(pChild->hasRequiredAttributes()) || !(pChild->hasRequiredElements()))
    {
        return LIBSBML_INVALID_OBJECT;
    }
    else if (getLevel() != pChild->getLevel())
    {
        return LIBSBML_LEVEL_MISMATCH;
    }
    else if (getVersion() != pChild->getVersion())
    {
        return LIBSBML_VERSION_MISMATCH;
    }
    else
    {

        this->mElements.append(pChild);

        return LIBSBML_OPERATION_SUCCESS;
    }
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Destroys the RenderGroup object and all it's children.
 */
RenderGroup::~RenderGroup()
{
    // the destructor of ListOf deletes the lements, so we don't have to do it here
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
bool RenderGroup::isSetStartHead() const
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
bool RenderGroup::isSetEndHead() const
{
    return (!this->mEndHead.empty() && this->mEndHead!="none");
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Returns true if the font family has been set or false otherwise.
 *
 * @return true if the font family string is not empty
 */
bool RenderGroup::isSetFontFamily() const
{
    return !this->mFontFamily.empty();
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns true if the font size has been set or false otherwise.
 *
 * @return true if the RelAbsVector specifying the font size does not
 * contain NaN either as the absolute or the relative value.
 */
bool RenderGroup::isSetFontSize() const
{
    return (this->mFontSize.getAbsoluteValue()==this->mFontSize.getAbsoluteValue() && this->mFontSize.getRelativeValue()==this->mFontSize.getRelativeValue());
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns true if the font weight has been set or false otherwise.
 *
 * @return true is the flag is not Text::WEIGHT_UNSET
 */
bool RenderGroup::isSetFontWeight() const
{
    return (this->mFontWeight!=Text::WEIGHT_UNSET);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns true if the font style has been set or false otherwise.
 *
 * @return true is the flag is not Text::STYLE_UNSET
 */
bool RenderGroup::isSetFontStyle() const
{
    return (this->mFontStyle!=Text::STYLE_UNSET);
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
void RenderGroup::writeAttributes (XMLOutputStream& stream) const
{
  GraphicalPrimitive2D::writeAttributes(stream);
    if(this->isSetFontSize())
    {
        std::ostringstream os;
        os << this->getFontSize();
        stream.writeAttribute("font-size", getPrefix(), os.str());
    }
    if(this->isSetFontFamily())
    {
        stream.writeAttribute("font-family", getPrefix(), this->mFontFamily);
    }
    switch(this->mFontStyle)    
    {
        default:
        case Text::STYLE_UNSET:
            break;
        case Text::STYLE_NORMAL:
            stream.writeAttribute("font-style", getPrefix(), std::string("normal"));
            break;
        case Text::STYLE_ITALIC:
            stream.writeAttribute("font-style", getPrefix(), std::string("italic"));
            break;
    }
    switch(this->mFontStyle)
    {
        default:
        case Text::WEIGHT_UNSET:
            break;
        case Text::WEIGHT_NORMAL:
            stream.writeAttribute("font-weight", getPrefix(), std::string("normal"));
            break;
        case Text::WEIGHT_BOLD:
            stream.writeAttribute("font-weight", getPrefix(), std::string("bold"));
            break;
    }
    switch(this->mTextAnchor)
    {
        case Text::ANCHOR_START:
            stream.writeAttribute("text-anchor", getPrefix(), std::string("start"));
            break;
        case Text::ANCHOR_END:
            stream.writeAttribute("text-anchor", getPrefix(), std::string("end"));
            break;
        case Text::ANCHOR_MIDDLE:
            stream.writeAttribute("text-anchor", getPrefix(), std::string("middle"));
            break;
        case Text::ANCHOR_UNSET:
        default:
            break;
    }
    switch(this->mVTextAnchor)
    {
        case Text::ANCHOR_TOP:
            stream.writeAttribute("vtext-anchor", getPrefix(), std::string("top"));
            break;
        case Text::ANCHOR_BOTTOM:
            stream.writeAttribute("vtext-anchor", getPrefix(), std::string("bottom"));
            break;
        case Text::ANCHOR_MIDDLE:
            stream.writeAttribute("vtext-anchor", getPrefix(), std::string("middle"));
            break;
            
        case Text::ANCHOR_BASELINE:
            stream.writeAttribute("vtext-anchor",std::string("baseline"));
            break;
        default:
        case Text::ANCHOR_UNSET:
            break;
    }
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
 * Creates an XMLNode object from this RenderGroup object.
 *
 * @return the XMLNode with the XML representation for the 
 * RenderGroup object.
 */
XMLNode RenderGroup::toXML() const
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
void RenderGroup::writeElements (XMLOutputStream& stream) const
{
    GraphicalPrimitive2D::writeElements(stream);
    unsigned int i,iMax=this->mElements.size();
    for(i=0;i<iMax;++i)
    {
      this->mElements.get(i)->write(stream);
    }
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Adds the text rendering attributes of the given RenderGroup object
 * to the given XMLAttributes object.
 */
void RenderGroup::addTextAttributes(const RenderGroup& group,XMLAttributes& att)
{
    if(group.isSetFontSize())
    {
        std::ostringstream os;
        os << group.getFontSize();
        att.add("font-size",os.str());
    }
    if(group.isSetFontFamily())
    {
        att.add("font-family",group.mFontFamily);
    }
    switch(group.mFontStyle)
    {
        default:
        case Text::STYLE_UNSET:
            break;
        case Text::STYLE_NORMAL:
            att.add("font-style","normal");
            break;
        case Text::STYLE_ITALIC:
            att.add("font-style","italic");
            break;
    }
    switch(group.mFontStyle)
    {
        default:
        case Text::WEIGHT_UNSET:
            break;
        case Text::WEIGHT_NORMAL:
            att.add("font-weight","normal");
            break;
        case Text::WEIGHT_BOLD:
            att.add("font-weight","bold");
            break;
    }
    switch(group.mTextAnchor)
    {
        case Text::ANCHOR_START:
            att.add("text-anchor","start");
            break;
        case Text::ANCHOR_END:
            att.add("text-anchor","end");
            break;
        case Text::ANCHOR_MIDDLE:
            att.add("text-anchor","middle");
            break;
        default:
        case Text::ANCHOR_UNSET:
            break;
    }
    switch(group.mVTextAnchor)
    {
        case Text::ANCHOR_TOP:
            att.add("vtext-anchor","top");
            break;
        case Text::ANCHOR_BOTTOM:
            att.add("vtext-anchor","bottom");
            break;
        case Text::ANCHOR_MIDDLE:
            att.add("vtext-anchor","middle");
            break;
        case Text::ANCHOR_BASELINE:
            att.add("vtext-anchor","baseline");
            break;
        default:
        case Text::ANCHOR_UNSET:
            break;
    }
    if(group.isSetStartHead())
    {
        att.add("startHead",group.mStartHead);
    }
    if(group.isSetEndHead())
    {
        att.add("endHead",group.mEndHead);
    }
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * This methods imports a curve in the old format into the new format.
 * Since the old curves could have gaps, the original curve might have to be
 * split into several new curves.
 */
void RenderGroup::importOldCurve(const XMLNode& node)
{
    const XMLAttributes& curveAttributes=node.getAttributes();
    const XMLNode* child;
    unsigned int n=0,nMax = node.getNumChildren();
    //const XMLNode* pOrigAnnotation=NULL;
    //const XMLNode* pOrigNotes=NULL;
    RENDER_CREATE_NS(renderns, this->getSBMLNamespaces());
    while(n<nMax)
    {
        child=&node.getChild(n);    
        std::string childName=child->getName();
        if(childName=="listOfCurveSegments")
        {
            unsigned int i,iMax=child->getNumChildren();
            const XMLNode* child2=NULL;
            RenderPoint start(renderns);
            RenderPoint end(renderns);
            RenderPoint lastEnd(renderns);
            RenderPoint bp1(renderns);
            RenderPoint bp2(renderns);
            bool startSet=false;
            bool endSet=false;
            bool bp1Set=false;
            bool bp2Set=false;
            std::string childName2;
            RenderCurve* pCurve=new RenderCurve(renderns);
            // read the attributes
            ExpectedAttributes ea;
            pCurve->readAttributes(curveAttributes, ea);
            for(i=0;i<iMax;++i)
            {
                child2=&child->getChild(i);
                childName2=child2->getName();
                if(childName2=="curveSegment")
                {
                    startSet=false;
                    endSet=false;
                    bp1Set=false;
                    bp2Set=false;
                    const XMLAttributes& innerAttributes=child2->getAttributes();
                    int typeIndex=innerAttributes.getIndex("type");
                    if(typeIndex==-1 || innerAttributes.getURI(typeIndex)!="http://www.w3.org/2001/XMLSchema-instance")
                    {
                        continue;
                    }
                    unsigned int j,jMax=child2->getNumChildren();
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
                            end=RenderPoint(*child3);
                            endSet=true;
                        }
                        else if(innerAttributes.getValue(typeIndex)=="CubicBezier"  && childName3=="basePoint1")
                        {
                            bp1=RenderPoint(*child3);
                            bp1Set=true;
                        }
                        else if(innerAttributes.getValue(typeIndex)=="CubicBezier" && childName3=="basePoint2")
                        {
                            bp2=RenderPoint(*child3);
                            bp2Set=true;
                        }
                    }
                    if(!startSet || !endSet)
                    {
                        // skip this point
                        // TODO this is an error
                        continue;
                    }
                    if(pCurve->getNumElements()==0)
                    {
                        // add the start point
                        pCurve->addElement(&start);
                    }
                    else
                    {
                        // check if start is identical to lastEnd
                        // if not, we have to start a new curve
                        if(!(start == lastEnd))
                        {
                            if(pCurve->getNumElements() > 1)
                            {
                                // add the curve to the goup     
                                this->mElements.appendAndOwn(pCurve);
                            }
                            else
                            {
                                delete pCurve;
                            }
                            // we have do clear the endHead on the
                            // preceeding curve
                            pCurve->setEndHead("none");
                            pCurve=new RenderCurve(renderns);
                            // read the attributes
                            pCurve->readAttributes(curveAttributes, ea);
                            // we have to clear the start head on this
                            // curve
                            pCurve->setStartHead("none");
                            pCurve->addElement(&start);
                        }
                    }
                    if(innerAttributes.getValue(typeIndex)=="CubicBezier")
                    {
                        if(!bp1Set)
                        {
                            if(bp2Set)
                            {
                                // basepoint1 is the same as basepoint2
                                bp1=bp2;
                            }
                            else
                            {
                                bp1=RenderPoint(renderns);
                                bp1.setCoordinates(RelAbsVector((start.x().getAbsoluteValue()+end.x().getAbsoluteValue())/2.0,(start.x().getRelativeValue()+end.x().getRelativeValue())/2.0)
                                        ,RelAbsVector((start.y().getAbsoluteValue()+end.y().getAbsoluteValue())/2.0,(start.y().getRelativeValue()+end.y().getRelativeValue())/2.0)
                                        ,RelAbsVector((start.z().getAbsoluteValue()+end.z().getAbsoluteValue())/2.0,(start.z().getRelativeValue()+end.z().getRelativeValue())/2.0)
                                        );
                                bp1Set=true;    
                            }
                        }
                        if(!bp2Set)
                        {
                            // at this time bp1 has to be set 
                            bp2=bp1;   
                        }
                        // add the cubic bezier element
                        RenderCubicBezier* pBezier = new RenderCubicBezier(renderns); 
                        pBezier->setCoordinates(end.x(),end.y(),end.z());
                        pBezier->setBasePoint1(bp1.x(),bp1.y(),bp1.z());
                        pBezier->setBasePoint2(bp2.x(),bp2.y(),bp2.z());
                        pCurve->addElement(pBezier);
                        delete pBezier;
                        lastEnd=end;
                    }
                    else
                    {
                        // add the end point
                        pCurve->addElement(&end);
                        lastEnd=end;
                    }
                }	
            }
            if(pCurve->getNumElements() > 1)
            {
                // add the curve to the goup     
                this->mElements.appendAndOwn(pCurve);
            }
        }
        //else if(childName=="annotation")
        //{
        //    pOrigAnnotation=child;
        //}
        //else if(childName=="notes")
        //{
        //    pOrigNotes=child;
        //}
        ++n;
    }
    delete renderns;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns true if the horizonal alignment attribute has been set.
 *
 * @return true is flag is not Text::ANCHOR_UNSET
 */
bool RenderGroup::isSetTextAnchor() const
{
    return this->mTextAnchor!=Text::ANCHOR_UNSET;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns true if the vertical alignment attribute has been set.
 *
 * @return true is flag is not Text::ANCHOR_UNSET
 */
bool RenderGroup::isSetVTextAnchor() const
{
    return this->mVTextAnchor!=Text::ANCHOR_UNSET;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Creates and returns a deep copy of the ListOfDrawables object.
 *
 * @return a (deep) copy of this ListOfDrawables
 */
ListOfDrawables* ListOfDrawables::clone () const
{
    return new ListOfDrawables(*this);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Copy constructor. Creates a copy of this ListOfDrawables object.
 */
ListOfDrawables::ListOfDrawables(const ListOfDrawables& source)
  : ListOf(source)
{
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Assignment operator for ListOfColroDefinitions objects.
 */
ListOfDrawables& ListOfDrawables::operator=(const ListOfDrawables& source)
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
 * Returns a pointer to the Transformation2D with the given index or NULL if
 * the index is invalid.
 * 
 * @param i index of the Transformation2D object to be returned
 * 
 * @return pointer to the Transformation2D at the given index or NULL.
 */
Transformation2D* ListOfDrawables::get(unsigned int i)
{
    return static_cast<Transformation2D*>(this->ListOf::get(i));
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns a const pointer to the Transformation2D with the given index or NULL if
 * the index is invalid.
 * 
 * @param i index of the Transformation2D object to be returned
 * 
 * @return const pointer to the Transformation2D at the given index or NULL.
 */
const Transformation2D* ListOfDrawables::get(unsigned int i) const
{
    return static_cast<const Transformation2D*>(this->ListOf::get(i));
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Used by ListOf::get() to lookup an SBase based by its id.
 */
struct IdEqTransformation2D : public std::unary_function<SBase*, bool>
{
    const std::string& id;

    IdEqTransformation2D (const std::string& id) : id(id) { }
    bool operator() (SBase* sb) 
    {
        const GraphicalPrimitive1D* pP=dynamic_cast<const GraphicalPrimitive1D*>(sb);
        if(pP != NULL)
        {
          return pP->getId() == id;
        }
        else
        {
            const Image* pI=dynamic_cast<const Image*>(sb);
            if(pI != NULL)
            {
                return pI->getId() == id;
            }
        }
        return false;
    }
};
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Returns a pointer to the Transformation2D with the given @p id or @c NULL if
 * the id is invalid.
 * 
 * @param id id of the Transformation2D object to be returned
 * 
 * @return pointer to the Transformation2D at the given @p id or @c NULL.
 */
Transformation2D* ListOfDrawables::get(const std::string& id)
{
    return const_cast<Transformation2D*>( 
            static_cast<const ListOfDrawables*>(this)->get(id) );
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Returns a const pointer to the Transformation2D with the given @p id or @c NULL if
 * the id is invalid.
 * 
 * @param id id of the Transformation2D object to be returned
 * 
 * @return const pointer to the Transformation2D at the given @p id or @c NULL.
 */
const Transformation2D* ListOfDrawables::get(const std::string& id) const
{
    std::vector<SBase*>::const_iterator result;

    result = std::find_if( mItems.begin(), mItems.end(), IdEqTransformation2D(id) );
    return (result == mItems.end()) ? 0 : static_cast <Transformation2D*> (*result);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/* Removes the nth item from this list */
    Transformation2D*
ListOfDrawables::remove (unsigned int n)
{
    return static_cast<Transformation2D*>(ListOf::remove(n));
}
/** @endcond */



/*
 * Ctor.
 */
ListOfDrawables::ListOfDrawables(RenderPkgNamespaces* renderns)
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
ListOfDrawables::ListOfDrawables(unsigned int level, unsigned int version, unsigned int pkgVersion)
 : ListOf(level,version)
{
  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(level,version,pkgVersion));
};
/** @cond doxygenLibsbmlInternal */
/* Removes item in this list by id */
    Transformation2D*
ListOfDrawables::remove (const std::string& sid)
{
    SBase* item = NULL;
    std::vector<SBase*>::iterator result;

    result = std::find_if( mItems.begin(), mItems.end(), IdEqTransformation2D(sid) );

    if (result != mItems.end())
    {
        item = *result;
        mItems.erase(result);
    }

    return static_cast <Transformation2D*> (item);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the parent SBMLDocument of this SBML object.
 *
 * @param d The SBMLDocument to set on the objects and it's children if there are any.
 */
    void
RenderGroup::setSBMLDocument (SBMLDocument* d)
{
    SBase::setSBMLDocument(d);
    this->mElements.setSBMLDocument(d);
}
/** @endcond */


/*
 * Sets this SBML object to child SBML objects (if any).
 * (Creates a child-parent relationship by the parent)
 */
void
RenderGroup::connectToChild()
{
  GraphicalPrimitive2D::connectToChild();
  mElements.connectToParent(this);
}

/*
 * Enables/Disables the given package with this element and child
 * elements (if any).
 * (This is an internal implementation for enablePakcage function)
 */
void
RenderGroup::enablePackageInternal(const std::string& pkgURI,
                                     const std::string& pkgPrefix, bool flag)
{
  SBase::enablePackageInternal(pkgURI,pkgPrefix,flag);

  mElements.enablePackageInternal(pkgURI,pkgPrefix,flag);
}




/** @cond doxygenLibsbmlInternal */
/*
 * Sets the parent SBML object of this SBML object.
 *
 * @param sb the SBML object to use
 */
    void 
RenderGroup::setParentSBMLObject (SBase* sb)
{
    this->mParentSBMLObject = sb;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase* ListOfDrawables::createObject (XMLInputStream& stream)
{
    const std::string& name   = stream.peek().getName();
    SBase*        object = NULL;
    
    RENDER_CREATE_NS(renderns, this->getSBMLNamespaces());

    if (name == "g")
    {
       object = new RenderGroup(renderns);
    }
    else if(name=="curve")
    {
       // only newstyle cures will be supported for L3 
       object = new RenderCurve(renderns);
    }
    else if(name=="polygon")
    {
       object = new Polygon(renderns);
    }
    else if(name=="rectangle")
    {
       object = new Rectangle(renderns);
    }
    else if(name=="ellipse")
    {
       object = new Ellipse(renderns);
    }
    else if(name=="text")
    {
       object = new Text(renderns);
    }
    else if(name=="image")
    {
       object = new Image(renderns);
    } 
    if(object) this->mItems.push_back(object);
    delete renderns;
    return object;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase* RenderGroup::createObject (XMLInputStream& stream)
{
    // the actual objects are created in the corresponding method
    // of ListOfDrawables
    return this->mElements.createObject(stream);
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
int
RenderGroup::getTypeCode () const
{
  return SBML_RENDER_GROUP;
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
int ListOfDrawables::getItemTypeCode () const
{
    return SBML_RENDER_TRANSFORMATION2D;
}
/** @endcond */


bool ListOfDrawables::isValidTypeForList(SBase * item)
{
  if (item == NULL) return false;
  int typeCode = item->getTypeCode();

  return (
    typeCode == SBML_RENDER_CURVE ||
    typeCode == SBML_RENDER_ELLIPSE ||
    typeCode == SBML_RENDER_GROUP ||
    typeCode == SBML_RENDER_IMAGE ||
    typeCode == SBML_RENDER_POLYGON ||
    typeCode == SBML_RENDER_RECTANGLE ||
    typeCode == SBML_RENDER_TEXT    
    );
}

LIBSBML_CPP_NAMESPACE_END 
