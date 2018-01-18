/**
 * @file    Text.cpp
 * @brief   class for representing a text element in the render extension
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

#include "Text.h"

#include <limits>
#include <sbml/packages/layout/util/LayoutAnnotation.h>
#include <sbml/packages/layout/util/LayoutUtilities.h>
#include <sbml/packages/render/extension/RenderExtension.h>
#ifndef OMIT_DEPRECATED
#ifdef DEPRECATION_WARNINGS
#include <iostream>
#endif // DEPRECATION_WARNINGS
#endif // OMIT_DEPRECATED

#include <sbml/xml/XMLInputStream.h>

LIBSBML_CPP_NAMESPACE_BEGIN

const std::string Text::ELEMENT_NAME="text";

/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new Text object with the given SBML level
 * and SBML version.
 *
 * @param level SBML level of the new object
 * @param level SBML version of the new object
 */
Text::Text (unsigned int level, unsigned int version, unsigned int pkgVersion) : 
    GraphicalPrimitive1D(level,version, pkgVersion),
    mX(RelAbsVector(0.0,0.0)),
    mY(RelAbsVector(0.0,0.0)),
    mZ(RelAbsVector(0.0,0.0)),
    mFontFamily(""),
    mFontSize(RelAbsVector(std::numeric_limits<double>::quiet_NaN(),std::numeric_limits<double>::quiet_NaN())),
    mFontWeight(Text::WEIGHT_UNSET),
    mFontStyle(Text::STYLE_UNSET),
    mTextAnchor(Text::ANCHOR_UNSET),
    mVTextAnchor(Text::ANCHOR_UNSET),
    mText("")
{
    if (!hasValidLevelVersionNamespaceCombination())
        throw SBMLConstructorException();
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new Text object with the given SBMLNamespaces.
 *
 * @param sbmlns The SBML namespace for the object.
 */
Text::Text (RenderPkgNamespaces* renderns)
  : GraphicalPrimitive1D(renderns)
  , mX(RelAbsVector(0.0,0.0))
  , mY(RelAbsVector(0.0,0.0))
  , mZ(RelAbsVector(0.0,0.0))
  , mFontFamily("")
  , mFontSize(RelAbsVector(std::numeric_limits<double>::quiet_NaN(),std::numeric_limits<double>::quiet_NaN()))
  , mFontWeight(Text::WEIGHT_UNSET)
  , mFontStyle(Text::STYLE_UNSET)
  , mTextAnchor(Text::ANCHOR_UNSET)
  , mVTextAnchor(Text::ANCHOR_UNSET)
  , mText("")
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
 * Creates a new Text object from the given XMLNode object.
 * The XMLNode object has to contain a valid XML representation of a 
 * Text object as defined in the render extension specification.
 * This method is normally called when render information is read from a file and 
 * should normally not have to be called explicitely.
 *
 * @param node the XMLNode object reference that describes the Text
 * object to be instantiated.
 */
Text::Text(const XMLNode& node, unsigned int l2version):GraphicalPrimitive1D(node, l2version)
{
    const XMLAttributes& attributes=node.getAttributes();
 
    ExpectedAttributes ea;
    addExpectedAttributes(ea);

    this->readAttributes(attributes,ea);
    unsigned int i,iMax=node.getNumChildren();
    for(i=0;i<iMax;++i)
    {
        if(node.getChild(i).isText())
        {
            mText=node.getChild(i).getCharacters();
            break;
        }
    }

    
  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(2,l2version));  

  connectToChild();
}
/** @endcond */


/*
 * Destroy this object.
 */
Text::~Text ()
{
}



/** @cond doxygenLibsbmlInternal */
void
Text::addExpectedAttributes(ExpectedAttributes& attributes)
{
  GraphicalPrimitive1D::addExpectedAttributes(attributes);

  attributes.add("x");
  attributes.add("y");
  attributes.add("z");
  attributes.add("font-family");
  attributes.add("font-size");
  attributes.add("font-weight");
  attributes.add("font-style");
  attributes.add("text-anchor");
  attributes.add("vtext-anchor");

}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
void Text::readAttributes (const XMLAttributes& attributes, const ExpectedAttributes& expectedAttributes)
{
    GraphicalPrimitive1D::readAttributes(attributes,expectedAttributes);
    std::string s;
    double NaN=std::numeric_limits<double>::quiet_NaN();
    attributes.readInto("font-family", this->mFontFamily, getErrorLog(), false, getLine(), getColumn());
    Text::FONT_WEIGHT fw=Text::WEIGHT_UNSET;
    Text::FONT_STYLE fs=Text::STYLE_UNSET;
    Text::TEXT_ANCHOR ta=Text::ANCHOR_UNSET;
    Text::TEXT_ANCHOR vta=Text::ANCHOR_UNSET;
    attributes.readInto("x",s, getErrorLog(), false, getLine(), getColumn());
    this->mX=RelAbsVector(s);
    attributes.readInto("y",s, getErrorLog(), false, getLine(), getColumn());
    this->mY=RelAbsVector(s);
    if(attributes.readInto("z",s, getErrorLog(), false, getLine(), getColumn()))
    {
        this->mZ=RelAbsVector(s);
    }
    else
    {
        this->mZ=RelAbsVector(0.0,0.0);
    }
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
        
        else if(s=="baseline")
        {
          vta=Text::ANCHOR_BASELINE;
        }
        
    }
    this->setTextAnchor(ta);
    this->setVTextAnchor(vta);
    this->setFontWeight(fw);
    this->setFontStyle(fs);

    
}
/** @endcond */

void 
Text::setElementText(const std::string &text) 
{
  this->setText(text);
}

#ifndef OMIT_DEPRECATED
/** @cond doxygenLibsbmlInternal */
/*
 * Instantiates a new Text object with the given @p id and position offset.
 * The position offset coordinates can be omitted and will be set to 0 in
 * that case.
 *
 * All attributes are set as described for the default constructor
 * of GraphicalPrimitive1D.
 * All the font rendering attributes as well 
 * as the text to be rendered are unset.
 *
 * @param id id string for the Text object
 * @param x x coordinate of the position offset
 * @param y y coordinate of the position offset
 * @param z z coordinate of the position offset
 *
 * This constructor is deprecated. The new libsbml API only has
 * constructors which take the SBML level and version or one that takes
 * an SBMLNamespaces object.
 */
Text::Text(RenderPkgNamespaces* renderns, const std::string& id,const RelAbsVector& x,const RelAbsVector& y,const RelAbsVector& z):
    GraphicalPrimitive1D(renderns, id),
    mX(x),
    mY(y),
    mZ(z),
    mFontFamily(""),
    mFontSize(RelAbsVector(std::numeric_limits<double>::quiet_NaN(),std::numeric_limits<double>::quiet_NaN())),
    mFontWeight(Text::WEIGHT_UNSET),
    mFontStyle(Text::STYLE_UNSET),
    mTextAnchor(Text::ANCHOR_UNSET),
    mVTextAnchor(Text::ANCHOR_UNSET),
    mText("")
{
#ifdef DEPRECATION_WARNINGS
    std::cerr << "Warning. Text::Text(const std::string& id,const RelAbsVector& x,const RelAbsVector& y,const RelAbsVector& z) is deprecated." << std::endl;
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
 * Sets the position of the text within the viewport.
 * This is like an offset that is applied after alignment.
 * If the z coordinate is omitted, it is set to 0.
 *
 * @param x x coordinate of the position offset
 * @param y y coordinate of the position offset
 * @param z z coordinate of the position offset
 */
void Text::setCoordinates(const RelAbsVector& x,const RelAbsVector& y,const RelAbsVector& z)
{
    this->mX=x;
    this->mY=y;
    this->mZ=z;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the x position of the text within the viewport.
 * This is like an offset that is applied after alignment.
 *
 * @param x x coordinate of the position offset
 */
void Text::setX(const RelAbsVector& coord)
{
    this->mX=coord;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the y position of the text within the viewport.
 * This is like an offset that is applied after alignment.
 *
 * @param y y coordinate of the position offset
 */
void Text::setY(const RelAbsVector& coord)
{
    this->mY=coord;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the z position of the text within the viewport.
 * This is like an offset that is applied after alignment.
 *
 * @param z z coordinate of the position offset
 */
void Text::setZ(const RelAbsVector& coord)
{
    this->mZ=coord;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the x position offset as a const reference.
 * This offset is applied after alignment.
 *
 * @return const reference of x position offset
 */
const RelAbsVector& Text::getX() const
{
    return this->mX;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the y position offset as a const reference.
 * This offset is applied after alignment.
 *
 * @return const reference of y position offset
 */
const RelAbsVector& Text::getY() const
{
    return this->mY;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the z position offset as a const reference.
 * This offset is applied after alignment.
 *
 * @return const reference of z position offset
 */
const RelAbsVector& Text::getZ() const
{
    return this->mZ;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the x position offset as a reference.
 * This offset is applied after alignment.
 *
 * @return reference of x position offset
 */
RelAbsVector& Text::getX()
{
    return this->mX;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the y position offset as a reference.
 * This offset is applied after alignment.
 *
 * @return reference of y position offset
 */
RelAbsVector& Text::getY()
{
    return this->mY;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the z position offset as a reference.
 * This offset is applied after alignment.
 *
 * @return reference of z position offset
 */
RelAbsVector& Text::getZ()
{
    return this->mZ;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the font family.
 *
 * @param family The name of the font family, e.g. Helvetica
 */
void Text::setFontFamily(const std::string& family)
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
void Text::setFontSize(const RelAbsVector& size)
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
void Text::setFontWeight(Text::FONT_WEIGHT weight)
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
void Text::setFontStyle(Text::FONT_STYLE style)
{
    this->mFontStyle=style;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the text anchor.
 * This is defines the horizontal text position.
 * Valid values are Text::ANCHOR_UNSET, Text::ANCHOR_START,
 * Text::ANCHOR_MIDDLE and Text_ANCHOR_END.
 * Text::ANCHOR_BASELINE is not a valid value
 * for the text-anchor attribute. If you set the text anchor to 
 * Text::ANCHOR_BASELINE, it will be set to Text::ANCHOR_UNSET.
 *
 * @param anchor The new horizontal alignment flag.
 */
void Text::setTextAnchor(Text::TEXT_ANCHOR anchor)
{
    
   if(anchor == Text::ANCHOR_BASELINE)
   {
     this->mTextAnchor=Text::ANCHOR_UNSET;
   }
   else   
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
void Text::setVTextAnchor(Text::TEXT_ANCHOR anchor)
{
    this->mVTextAnchor=anchor;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the font family.
 *
 * @return The name of the font family to be used for text rendering.
 */
const std::string& Text::getFontFamily() const
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
RelAbsVector& Text::getFontSize()
{
    return this->mFontSize;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the font size as a const reference.
 *
 * @return const reference to the size to be used for rendering text.
 */
const RelAbsVector& Text::getFontSize() const
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
Text::FONT_WEIGHT Text::getFontWeight() const
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
Text::FONT_STYLE Text::getFontStyle() const
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
Text::TEXT_ANCHOR Text::getTextAnchor() const
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
Text::TEXT_ANCHOR Text::getVTextAnchor() const
{
    return this->mVTextAnchor;
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
 * This method is purely abstract and has to be implemented by derived
 * classes.
 *
 * @see getElementName()
 */
int Text::getTypeCode() const
{
    return SBML_RENDER_TEXT;
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
bool Text::accept(SBMLVisitor& /*visitor*/) const
{
    return false;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Creates and returns a deep copy of this Text object.
 * 
 * @return a (deep) copy of this Text object
 */
Text* Text::clone() const
{
    return new Text(*this);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the XML element name of this object, which for
 * Text, is always @c "text".
 * 
 * @return the name of this element, i.e., @c "text".
 */
const std::string& Text::getElementName() const
{
  static std::string name = Text::ELEMENT_NAME;
  return name;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns the text for the Text object.
 *
 * @return the text string to be rendered for the Text object.
 */
const std::string& Text::getText() const
{
    return this->mText;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Sets the text for the text element.
 *
 * @param text The text to be rendered for the Text object.
 */
void Text::setText(const std::string& text)
{
    this->mText=text;
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns true if the text is set to something else than the empty string.
 *
 * @return true if the text is not empty.
 */
bool Text::isSetText() const
{
    return !this->mText.empty();
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Returns true if the font family has been set or false otherwise.
 *
 * @return true if the font family string is not empty
 */
bool Text::isSetFontFamily() const
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
bool Text::isSetFontSize() const
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
bool Text::isSetFontWeight() const
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
bool Text::isSetFontStyle() const
{
    return (this->mFontStyle!=Text::STYLE_UNSET);
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Returns true if the horizonal alignment attribute has been set.
 *
 * @return true is flag is not Text::ANCHOR_UNSET
 */
bool Text::isSetTextAnchor() const
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
bool Text::isSetVTextAnchor() const
{
    return this->mVTextAnchor!=Text::ANCHOR_UNSET;
}
/** @endcond */



/** @cond doxygenLibsbmlInternal */
/*
 * Adds the text rendering attributes of the given Text object
 * to the given XMLAttributes object.
 */
void Text::addTextAttributes(const Text& text,XMLAttributes& att)
{
    if(text.isSetFontFamily())
    {
        att.add("font-family",text.mFontFamily);
    }
    if(text.isSetFontSize())
    {
        std::ostringstream os;
        os << text.getFontSize();
        att.add("font-size",os.str());
    }
    switch(text.mFontStyle)
    {
        // if it has been set to normal, we
        // have to write it because otherwise it is assumed to
        // be inherited
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
    switch(text.mFontStyle)
    {
        default:
        case Text::WEIGHT_UNSET:
            break;
        case Text::WEIGHT_NORMAL:
            // if it has been set to normal, we
            // have to write it because otherwise it is assumed to
            // be inherited
            att.add("font-weight","normal");
            break;
        case Text::WEIGHT_BOLD:
            att.add("font-weight","bold");
            break;
    }
    if(text.isSetTextAnchor())
    {
        switch(text.mTextAnchor)
        {
            // if it has been set to normal, we
            // have to write it because otherwise it is assumed to
            // be inherited
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
    }
    if(text.isSetVTextAnchor())
    {
        switch(text.mVTextAnchor)
        {
            // if it has been set to normal, we
            // have to write it because otherwise it is assumed to
            // be inherited
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
    }
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
void Text::writeAttributes (XMLOutputStream& stream) const
{
  GraphicalPrimitive1D::writeAttributes(stream);
  std::ostringstream os;
  RelAbsVector tmp(0.0,0.0);
  // x and y are not optional
  os.str("");
  os << this->mX;
  stream.writeAttribute("x", getPrefix(), os.str());
  os.str("");
  os << this->mY;
  stream.writeAttribute("y", getPrefix(), os.str());
  if(this->mZ!=tmp)
  {
      os.str("");
      os << this->mZ;
      stream.writeAttribute("z", getPrefix(), os.str());
  }
  if(this->isSetFontFamily())
  {
      stream.writeAttribute("font-family", getPrefix(), this->mFontFamily);
  }
  if(this->isSetFontSize())
  {
      std::ostringstream os;
      os << this->getFontSize();
      stream.writeAttribute("font-size", getPrefix(), os.str());
  }
  switch(this->mFontStyle)
  {
      // if it has been set to normal, we
      // have to write it because otherwise it is assumed to
      // be inherited
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
          // if it has been set to normal, we
          // have to write it because otherwise it is assumed to
          // be inherited
          stream.writeAttribute("font-weight", getPrefix(), std::string("normal"));
          break;
      case Text::WEIGHT_BOLD:
          stream.writeAttribute("font-weight", getPrefix(), std::string("bold"));
          break;
  }
  if(this->isSetTextAnchor())
  {
      switch(this->mTextAnchor)
      {
          // if it has been set to normal, we
          // have to write it because otherwise it is assumed to
          // be inherited
          case Text::ANCHOR_START:
              stream.writeAttribute("text-anchor", getPrefix(), std::string("start"));
              break;
          case Text::ANCHOR_END:
              stream.writeAttribute("text-anchor", getPrefix(), std::string("end"));
              break;
          case Text::ANCHOR_MIDDLE:
              stream.writeAttribute("text-anchor", getPrefix(), std::string("middle"));
              break;
          default:
          case Text::ANCHOR_UNSET:
              break;
      }
  }
  if(this->isSetVTextAnchor())
  {
      switch(this->mVTextAnchor)
      {
          // if it has been set to normal, we
          // have to write it because otherwise it is assumed to
          // be inherited
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
  }
}
/** @endcond */

/** @cond doxygenLibsbmlInternal */
/*
 * Writes (serializes) this SBML object by writing it to XMLOutputStream.
 */
void
Text::write (XMLOutputStream& stream) const
{
    
  stream.startElement( getElementName(), getPrefix());

  writeXMLNS(stream);
  writeAttributes( stream );
  // in addition to attributes we need to write the characters
  stream << this->getText();

  stream.endElement( getElementName(), getPrefix());
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Creates an Text object from this Group object.
 *
 * @return the XMLNode with the XML representation for the 
 * Text object.
 */
XMLNode Text::toXML() const
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
void Text::writeElements (XMLOutputStream& stream) const
{
    SBase::writeElements(stream);
    stream << this->getText();
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/* function returns true if component has all the required
 * attributes
 */
bool Text::hasRequiredAttributes() const
{
    bool result = this->GraphicalPrimitive1D::hasRequiredAttributes();
    // the position should not contain NaN
    result = result && 
        (this->mX.getAbsoluteValue() == this->mX.getAbsoluteValue()) &&
        (this->mX.getRelativeValue() == this->mX.getRelativeValue());
    result = result && 
        (this->mY.getAbsoluteValue() == this->mY.getAbsoluteValue()) &&
        (this->mY.getRelativeValue() == this->mY.getRelativeValue());
    result = result && 
        (this->mZ.getAbsoluteValue() == this->mZ.getAbsoluteValue()) &&
        (this->mZ.getRelativeValue() == this->mZ.getRelativeValue());

    return result;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/* function returns true if component has all the required
 * elements
 */
bool Text::hasRequiredElements() const 
{
    bool result = this->GraphicalPrimitive1D::hasRequiredElements();
    return result;
}
/** @endcond */

const char* FONT_WEIGHT_STRINGS[] =
{
  "unset",
  "normal",
  "bold",
  "invalid"
};


const char* FONT_STYLE_STRINGS[] =
{
  "unset",
  "normal",
  "italic",
  "invalid"
};

const char* TEXT_ANCHOR_STRINGS[] =
{
  "unset",
  "start",
  "middle", 
  "end",
  "top",
  "bottom",
  "baseline",
  "invalid"
};


LIBSBML_EXTERN
Text::TEXT_ANCHOR 
TextAnchor_fromString(const char * name)
{
  if (name != NULL)
  {
    const Text::TEXT_ANCHOR  lo = Text::ANCHOR_UNSET;
    const Text::TEXT_ANCHOR  hi = Text::ANCHOR_BASELINE;

    return (Text::TEXT_ANCHOR)util_bsearchStringsI(TEXT_ANCHOR_STRINGS, name, lo, hi);
  }

  return Text::ANCHOR_UNSET;
}

LIBSBML_EXTERN
const char * 
TextAnchor_toString(Text::TEXT_ANCHOR anchor)
{
  if ((anchor < Text::ANCHOR_UNSET) || (anchor > Text::ANCHOR_BASELINE))
  {
    anchor = Text::ANCHOR_UNSET;
  }

  return TEXT_ANCHOR_STRINGS[anchor];
}


LIBSBML_EXTERN 
Text::FONT_WEIGHT 
FontWeight_fromString(const char * name)
{
  if (name != NULL)
  {
    const Text::FONT_WEIGHT  lo = Text::WEIGHT_UNSET;
    const Text::FONT_WEIGHT  hi = Text::WEIGHT_BOLD;

    return (Text::FONT_WEIGHT)util_bsearchStringsI(FONT_WEIGHT_STRINGS, name, lo, hi);
  }

  return Text::WEIGHT_UNSET;
}

LIBSBML_EXTERN 
const char *
FontWeight_toString(Text::FONT_WEIGHT weight)
{
  if ((weight < Text::WEIGHT_UNSET) || (weight > Text::WEIGHT_BOLD))
  {
    weight = Text::WEIGHT_UNSET;
  }

  return FONT_WEIGHT_STRINGS[weight];
}

LIBSBML_EXTERN 
Text::FONT_STYLE 
FontStyle_fromString(const char * name)
{
  if (name != NULL)
  {
    const Text::FONT_STYLE  lo = Text::STYLE_UNSET;
    const Text::FONT_STYLE  hi = Text::STYLE_ITALIC;

    return (Text::FONT_STYLE) util_bsearchStringsI(FONT_STYLE_STRINGS, name, lo, hi);
  }

  return Text::STYLE_UNSET;
}

LIBSBML_EXTERN
const char * 
FontStyle_toString(Text::FONT_STYLE style)
{
  if ((style < Text::STYLE_UNSET) || (style > Text::STYLE_ITALIC))
  {
    style = Text::STYLE_UNSET;
  }

  return FONT_STYLE_STRINGS[style];
}


LIBSBML_CPP_NAMESPACE_END
