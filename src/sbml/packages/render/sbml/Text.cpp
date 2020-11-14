/**
 * @file    Text.cpp
 * @brief Implementation of the Text class.
 * @author  Ralph Gauges
 * @author  Frank T. Bergmann
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
 *
 * Copyright (C) 2019 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
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

#include <sbml/packages/render/sbml/Text.h>
#include <sbml/packages/render/validator/RenderSBMLError.h>

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

using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new Text using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 */
Text::Text(unsigned int level, unsigned int version, unsigned int pkgVersion)
 :  GraphicalPrimitive1D(level,version, pkgVersion),
    mX(RelAbsVector(0.0,0.0)),
    mY(RelAbsVector(0.0,0.0)),
    mZ(RelAbsVector(0.0,0.0)),
    mFontFamily(""),
    mFontSize(RelAbsVector(std::numeric_limits<double>::quiet_NaN(),std::numeric_limits<double>::quiet_NaN())),
    mFontWeight(FONT_WEIGHT_UNSET),
    mFontStyle(FONT_STYLE_UNSET),
    mTextAnchor(H_TEXTANCHOR_UNSET),
    mVTextAnchor(V_TEXTANCHOR_UNSET),
    mText("")
{
  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(level, version, pkgVersion));
  connectToChild();
}


/*
 * Creates a new Text using the given RenderPkgNamespaces object.
 */
Text::Text (RenderPkgNamespaces* renderns)
  : GraphicalPrimitive1D(renderns),
  mX(RelAbsVector(0.0, 0.0)),
  mY(RelAbsVector(0.0, 0.0)),
  mZ(RelAbsVector(0.0, 0.0)),
  mFontFamily(""),
  mFontSize(RelAbsVector(std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN())),
  mFontWeight(FONT_WEIGHT_UNSET),
  mFontStyle(FONT_STYLE_UNSET),
  mTextAnchor(H_TEXTANCHOR_UNSET),
  mVTextAnchor(V_TEXTANCHOR_UNSET),
  mText("")
{
  setElementNamespace(renderns->getURI());
  connectToChild();
  loadPlugins(renderns);
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
/*
 * Creates a new Text object from the given XMLNode object.
 * The XMLNode object has to contain a valid XML representation of a 
 * Text object as defined in the render extension specification.
 * This method is normally called when render information is read from a file and 
 * should normally not have to be called explicitly.
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



/*
 * Copy constructor for Text.
 */
Text::Text(const Text& orig)
  : GraphicalPrimitive1D( orig )
  , mX ( orig.mX )
  , mY ( orig.mY )
  , mZ ( orig.mZ )
  , mFontFamily ( orig.mFontFamily )
  , mFontSize(orig.mFontSize)
  , mFontWeight ( orig.mFontWeight )
  , mFontStyle ( orig.mFontStyle )
  , mTextAnchor ( orig.mTextAnchor )
  , mVTextAnchor ( orig.mVTextAnchor )
{
  connectToChild();
}


/*
 * Assignment operator for Text.
 */
Text&
Text::operator=(const Text& rhs)
{
  if (&rhs != this)
  {
    GraphicalPrimitive1D::operator=(rhs);
    mFontFamily = rhs.mFontFamily;
    mFontWeight = rhs.mFontWeight;
    mFontStyle = rhs.mFontStyle;
    mTextAnchor = rhs.mTextAnchor;
    mVTextAnchor = rhs.mVTextAnchor;
    mX = rhs.mX;
    mY = rhs.mY;
    mZ = rhs.mZ;
    mFontSize = rhs.mFontSize;

      connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this Text object.
 */
Text*
Text::clone() const
{
  return new Text(*this);
}


/*
 * Destructor for Text.
 */
Text::~Text()
{
}


/*
 * Returns the value of the "font-family" attribute of this Text.
 */
const std::string&
Text::getFontFamily() const
{
  return mFontFamily;
}


int
Text::getFontWeight() const
{
  return mFontWeight;
}


/*
 * Returns the value of the "font-weight" attribute of this Text.
 */
std::string
Text::getFontWeightAsString() const
{
  std::string code_str = FontWeight_toString((FontWeight_t)(mFontWeight));
  return code_str;
}


/*
 * Returns the value of the "font-style" attribute of this Text.
 */
int
Text::getFontStyle() const
{
  return mFontStyle;
}


/*
 * Returns the value of the "font-style" attribute of this Text.
 */
std::string
Text::getFontStyleAsString() const
{
  std::string code_str = FontStyle_toString((FontStyle_t)(mFontStyle));
  return code_str;
}


/*
 * Returns the value of the "text-anchor" attribute of this Text.
 */
int
Text::getTextAnchor() const
{
  return mTextAnchor;
}


/*
 * Returns the value of the "text-anchor" attribute of this Text.
 */
std::string
Text::getTextAnchorAsString() const
{
  std::string code_str = HTextAnchor_toString((HTextAnchor_t)(mTextAnchor));
  return code_str;
}


/*
 * Returns the value of the "vtext-anchor" attribute of this Text.
 */
int
Text::getVTextAnchor() const
{
  return mVTextAnchor;
}


/*
 * Returns the value of the "vtext-anchor" attribute of this Text.
 */
std::string
Text::getVTextAnchorAsString() const
{
  std::string code_str = VTextAnchor_toString((VTextAnchor_t)(mVTextAnchor));
  return code_str;
}


/*
 * Predicate returning @c true if this Text's "font-family" attribute is set.
 */
bool
Text::isSetFontFamily() const
{
  return (mFontFamily.empty() == false);
}


/*
 * Predicate returning @c true if this Text's "font-weight" attribute is set.
 */
bool 
Text::isSetFontWeight() const
{
  return (mFontWeight != FONT_WEIGHT_INVALID && mFontWeight != FONT_WEIGHT_UNSET);
}


/*
 * Predicate returning @c true if this Text's "font-style" attribute is set.
 */
bool
Text::isSetFontStyle() const
{
  return (mFontStyle != FONT_STYLE_INVALID && mFontStyle != FONT_STYLE_UNSET);
}


/*
 * Predicate returning @c true if this Text's "text-anchor" attribute is set.
 */
bool
Text::isSetTextAnchor() const
{
  return (mTextAnchor != H_TEXTANCHOR_INVALID && mTextAnchor != H_TEXTANCHOR_UNSET);
}


/*
 * Predicate returning @c true if this Text's "vtext-anchor" attribute is set.
 */
bool
Text::isSetVTextAnchor() const
{
  return (mVTextAnchor != V_TEXTANCHOR_INVALID && mVTextAnchor != V_TEXTANCHOR_UNSET);
}


/*
 * Sets the value of the "font-family" attribute of this Text.
 */
int
Text::setFontFamily(const std::string& fontFamily)
{
  mFontFamily = fontFamily;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "font-weight" attribute of this Text.
 */
int
Text::setFontWeight(const FontWeight_t fontWeight)
{
  if (FontWeight_isValid(fontWeight) == 0)
  {
    mFontWeight = FONT_WEIGHT_INVALID;
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mFontWeight = fontWeight;
    return LIBSBML_OPERATION_SUCCESS;
  }
}

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
  this->mFontWeight = weight;
}
/** @endcond */


/*
 * Sets the value of the "font-weight" attribute of this Text.
 */
int
Text::setFontWeight(const std::string& fontWeight)
{
  mFontWeight = FontWeight_fromString(fontWeight.c_str());

  if (mFontWeight == FONT_WEIGHT_INVALID)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }

  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "font-style" attribute of this Text.
 */
int
Text::setFontStyle(const FontStyle_t fontStyle)
{
  if (FontStyle_isValid(fontStyle) == 0)
  {
    mFontStyle = FONT_STYLE_INVALID;
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mFontStyle = fontStyle;
    return LIBSBML_OPERATION_SUCCESS;
  }
}

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
  this->mFontStyle = style;
}
/** @endcond */

/*
 * Sets the value of the "font-style" attribute of this Text.
 */
int
Text::setFontStyle(const std::string& fontStyle)
{
  mFontStyle = FontStyle_fromString(fontStyle.c_str());

  if (mFontStyle == FONT_STYLE_INVALID)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }

  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "text-anchor" attribute of this Text.
 */
int
Text::setTextAnchor(const HTextAnchor_t textAnchor)
{
  if (HTextAnchor_isValid(textAnchor) == 0)
  {
    mTextAnchor = H_TEXTANCHOR_INVALID;
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mTextAnchor = textAnchor;
    return LIBSBML_OPERATION_SUCCESS;
  }
}

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

  if (anchor == Text::ANCHOR_BASELINE)
  {
    this->mTextAnchor = Text::ANCHOR_UNSET;
  }
  else
  {
    this->mTextAnchor = anchor;
  }
}
/** @endcond */

/*
 * Sets the value of the "text-anchor" attribute of this Text.
 */
int
Text::setTextAnchor(const std::string& textAnchor)
{
  mTextAnchor = HTextAnchor_fromString(textAnchor.c_str());

  if (mTextAnchor == H_TEXTANCHOR_INVALID)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }

  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "vtext-anchor" attribute of this Text.
 */
int
Text::setVTextAnchor(const VTextAnchor_t vtextAnchor)
{
  if (VTextAnchor_isValid(vtextAnchor) == 0)
  {
    mVTextAnchor = V_TEXTANCHOR_INVALID;
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mVTextAnchor = vtextAnchor;
    return LIBSBML_OPERATION_SUCCESS;
  }
}

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
  this->mVTextAnchor = anchor;
}
/** @endcond */

/*
 * Sets the value of the "vtext-anchor" attribute of this Text.
 */
int
Text::setVTextAnchor(const std::string& vtextAnchor)
{
  mVTextAnchor = VTextAnchor_fromString(vtextAnchor.c_str());

  if (mVTextAnchor == V_TEXTANCHOR_INVALID)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }

  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "font-family" attribute of this Text.
 */
int
Text::unsetFontFamily()
{
  mFontFamily.erase();

  if (mFontFamily.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "font-weight" attribute of this Text.
 */
int
Text::unsetFontWeight()
{
  mFontWeight = FONT_WEIGHT_INVALID;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "font-style" attribute of this Text.
 */
int
Text::unsetFontStyle()
{
  mFontStyle = FONT_STYLE_INVALID;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "text-anchor" attribute of this Text.
 */
int
Text::unsetTextAnchor()
{
  mTextAnchor = H_TEXTANCHOR_INVALID;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "vtext-anchor" attribute of this Text.
 */
int
Text::unsetVTextAnchor()
{
  mVTextAnchor = V_TEXTANCHOR_INVALID;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
* Returns the x position offset as a const reference.
*/
const RelAbsVector& 
Text::getX() const
{
  return this->mX;
}


RelAbsVector& 
Text::getX()
{
  return this->mX;
}


/*
* Returns the y position offset as a const reference.
*/
const RelAbsVector& 
Text::getY() const
{
  return this->mY;
}


RelAbsVector& 
Text::getY()
{
  return this->mY;
}


/*
* Returns the z position offset as a const reference.
*/
const RelAbsVector& 
Text::getZ() const
{
  return this->mZ;
}


RelAbsVector& Text::getZ()
{
  return this->mZ;
}


/*
* Returns the font size as a const reference.
*
* @return const reference to the size to be used for rendering text.
*/
const RelAbsVector& 
Text::getFontSize() const
{
  return this->mFontSize;
}


RelAbsVector&
Text::getFontSize()
{
  return this->mFontSize;
}


/*
 * Predicate returning @c true if this Text's "x" element is set.
 */
bool
Text::isSetX() const
{
  return mX.isSetCoordinate();
}


/*
 * Predicate returning @c true if this Text's "y" element is set.
 */
bool
Text::isSetY() const
{
  return mY.isSetCoordinate();
}


/*
 * Predicate returning @c true if this Text's "z" element is set.
 */
bool
Text::isSetZ() const
{
  return mZ.isSetCoordinate();
}


/*
 * Predicate returning @c true if this Text's "font-size" element is set.
 */
bool
Text::isSetFontSize() const
{
  return mFontSize.isSetCoordinate();
}


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


/*
 * Sets the x position of the text within the viewport.
 */
int 
Text::setX(const RelAbsVector& coord)
{
    this->mX=coord;
    return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the y position of the text within the viewport.
 */
int
Text::setY(const RelAbsVector& coord)
{
    this->mY=coord;
    return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the z position of the text within the viewport.
 */
int 
Text::setZ(const RelAbsVector& coord)
{
    this->mZ=coord;
    return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the font size.
 */
int
Text::setFontSize(const RelAbsVector& size)
{
    this->mFontSize=size;
    return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "x" element of this Text.
 */
int
Text::unsetX()
{
  mX.unsetCoordinate();
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "y" element of this Text.
 */
int
Text::unsetY()
{
  mY.unsetCoordinate();
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "z" element of this Text.
 */
int
Text::unsetZ()
{
  mZ.unsetCoordinate();
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "font-size" element of this Text.
 */
int
Text::unsetFontSize()
{
  mFontSize.unsetCoordinate();
  return LIBSBML_OPERATION_SUCCESS;
}


/*
* Returns the text for the Text object.
*/
const std::string& 
Text::getText() const
{
  return this->mText;
}


/*
* Returns true if the text is set to something else than the empty string.
*/
bool 
Text::isSetText() const
{
  return !this->mText.empty();
}


/*
* Sets the text for the text element.
*/
int 
Text::setText(const std::string& text)
{
  this->mText = text;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
* Unsets the text for the text element.
*/
int
Text::unsetText()
{
  this->mText.clear();
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Returns the XML element name of this Text object.
 */
const std::string&
Text::getElementName() const
{
  static const string name = "text";
  return name;
}


/*
 * Returns the libSBML type code for this Text object.
 */
int
Text::getTypeCode() const
{
  return SBML_RENDER_TEXT;
}


/*
 * Predicate returning @c true if all the required attributes for this Text
 * object have been set.
 */
bool
Text::hasRequiredAttributes() const
{
  bool allPresent = GraphicalPrimitive1D::hasRequiredAttributes();

  if (isSetX() == false)
  {
    allPresent = false;
  }

  if (isSetY() == false)
  {
    allPresent = false;
  }

  return allPresent;
}


/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
Text::accept(SBMLVisitor& v) const
{
  v.visit(*this);
  //render - FIX_ME

  //if (mX != NULL)
  //{
  //  mX->accept(v);
  //}

  //if (mY != NULL)
  //{
  //  mY->accept(v);
  //}

  //if (mZ != NULL)
  //{
  //  mZ->accept(v);
  //}

  //if (mFontSize != NULL)
  //{
  //  mFontSize->accept(v);
  //}

  v.leave(*this);
  return true;
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

/*
 * Reads the expected attributes into the member data variables
 */
void
Text::readAttributes(const XMLAttributes& attributes,
                     const ExpectedAttributes& expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  GraphicalPrimitive1D::readAttributes(attributes, expectedAttributes);

  if (log)
  {
    numErrs = log->getNumErrors();

    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("render", RenderTextAllowedAttributes, pkgVersion,
          level, version, details, getLine(), getColumn());
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("render", RenderTextAllowedCoreAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
      }
    }
  }

  string elplusid = "<text> element ";
  if (!getId().empty()) {
    elplusid += "with the id '" + mId + "' ";
  }

  // 
  // font-family string (use = "optional" )
  // 

  assigned = attributes.readInto("font-family", mFontFamily);

  if (assigned == true)
  {
    if (mFontFamily.empty() == true)
    {
      if (log)
        logEmptyString(mFontFamily, level, version, "<Text>");
    }
  }

  // 
  // font-weight enum (use = "optional" )
  // 

  std::string fontWeight;
  assigned = attributes.readInto("font-weight", fontWeight);

  mFontWeight = FONT_WEIGHT_UNSET;
  if (assigned == true)
  {
    if (fontWeight.empty() == true)
    {
      if (log) 
        logEmptyString(fontWeight, level, version, "<Text>");
    }
    else
    {
      mFontWeight = FontWeight_fromString(fontWeight.c_str());

      if (FontWeight_isValid((FontWeight_t)(mFontWeight)) == 0)
      {
        std::string msg = "The font-weight on the ";
        msg += elplusid;
        msg += "is '" + fontWeight + "', which is not a valid option.";

        if (log) 
          log->logPackageError("render",
          RenderTextFontWeightMustBeFontWeightEnum, pkgVersion, level, version,
            msg, getLine(), getColumn());
        mFontWeight = FONT_WEIGHT_UNSET;
      }
    }
  }

  // 
  // font-style enum (use = "optional" )
  // 

  std::string fontStyle;
  assigned = attributes.readInto("font-style", fontStyle);

  mFontStyle = FONT_STYLE_UNSET;
  if (assigned == true)
  {
    if (fontStyle.empty() == true)
    {
      if (log) 
        logEmptyString(fontStyle, level, version, "<Text>");
      mFontStyle = FONT_STYLE_UNSET;
    }
    else
    {
      mFontStyle = FontStyle_fromString(fontStyle.c_str());

      if (FontStyle_isValid((FontStyle_t)(mFontStyle)) == 0)
      {
        std::string msg = "The font-style on the ";
        msg += elplusid;
        msg += "is '" + fontStyle + "', which is not a valid option.";

        if (log) 
          log->logPackageError("render", RenderTextFontStyleMustBeFontStyleEnum,
          pkgVersion, level, version, msg, getLine(), getColumn());
        mFontStyle = FONT_STYLE_UNSET;
      }
    }
  }

  // 
  // text-anchor enum (use = "optional" )
  // 

  mTextAnchor = H_TEXTANCHOR_UNSET;
  std::string textAnchor;
  assigned = attributes.readInto("text-anchor", textAnchor);

  if (assigned == true)
  {
    if (textAnchor.empty() == true)
    {
      if (log) 
        logEmptyString(textAnchor, level, version, "<Text>");
      mTextAnchor = H_TEXTANCHOR_UNSET;
    }
    else
    {
      mTextAnchor = HTextAnchor_fromString(textAnchor.c_str());

      if (HTextAnchor_isValid((HTextAnchor_t)(mTextAnchor)) == 0)
      {
        std::string msg = "The text-anchor on the ";
        msg += elplusid;
        msg += "is '" + textAnchor + "', which is not a valid option.";

        if (log)
          log->logPackageError("render",
          RenderTextTextAnchorMustBeHTextAnchorEnum, pkgVersion, level, version,
            msg, getLine(), getColumn());
        mTextAnchor = H_TEXTANCHOR_UNSET;
      }
    }
  }

  // 
  // vtext-anchor enum (use = "optional" )
  // 

  std::string vtextAnchor;
  assigned = attributes.readInto("vtext-anchor", vtextAnchor);

  mVTextAnchor = V_TEXTANCHOR_UNSET;
  if (assigned == true)
  {
    if (vtextAnchor.empty() == true)
    {
      if (log)
        logEmptyString(vtextAnchor, level, version, "<Text>");
      mVTextAnchor = V_TEXTANCHOR_UNSET;
    }
    else
    {
      mVTextAnchor = VTextAnchor_fromString(vtextAnchor.c_str());

      if (VTextAnchor_isValid((VTextAnchor_t)(mVTextAnchor)) == 0)
      {
        std::string msg = "The vtext-anchor on the ";
        msg += elplusid;
        msg += "is '" + vtextAnchor + "', which is not a valid option.";

        if (log)
          log->logPackageError("render",
          RenderTextVtextAnchorMustBeVTextAnchorEnum, pkgVersion, level, version,
            msg, getLine(), getColumn());
        mVTextAnchor = V_TEXTANCHOR_UNSET;
      }
    }
  }
 
  std::string s;
  RelAbsVector v = RelAbsVector();
  
  //
  // x RelAbsVector (use = required) 
  //
  assigned = attributes.readInto("x", s, this->getErrorLog(), false, getLine(), getColumn());
  if (!assigned && log)
  {
    std::string message = "The required attribute 'x' is missing from the "
      + elplusid + ".";
    log->logPackageError("render", RenderTextAllowedAttributes,
      pkgVersion, level, version, message, getLine(), getColumn());
  }
  else
  {
    v.setCoordinate(s);
    if (!(v.isSetCoordinate()) && log)
    {
      std::string message = "The syntax '" + s + "' of the attribute 'x' on the "
        + elplusid + " does not conform to the syntax of a RelAbsVector type.";
      log->logPackageError("render", RenderTextXMustBeRelAbsVector,
        pkgVersion, level, version, message, getLine(), getColumn());

    }
    else
    {
      this->setX(v);
    }
    v.erase();
  }

  
  //
  // y RelAbsVector (use = required) 
  //
  assigned = attributes.readInto("y", s, this->getErrorLog(), false, getLine(), getColumn());
  if (!assigned && log)
  {
    std::string message = "The required attribute 'y' is missing from the "
      + elplusid + ".";
    log->logPackageError("render", RenderTextAllowedAttributes,
      pkgVersion, level, version, message, getLine(), getColumn());
  }
  else
  {
    v.setCoordinate(s);
    if (!(v.isSetCoordinate()) && log)
    {
      std::string message = "The syntax '" + s + "' of the attribute 'y' on the "
        + elplusid + " does not conform to the syntax of a RelAbsVector type.";
      log->logPackageError("render", RenderTextYMustBeRelAbsVector,
        pkgVersion, level, version, message, getLine(), getColumn());

    }
    else
    {
      this->setY(v);
    }
    v.erase();
  }

  //
  // z RelAbsVector (use = optional) 
  //

  s = "";
  assigned = attributes.readInto("z", s, getErrorLog(), false, getLine(), getColumn());
  if (!assigned)
  {
    this->mZ = RelAbsVector(0.0, 0.0);
  }
  else
  {
    v.setCoordinate(s);
    if (!(v.isSetCoordinate()) && log)
    {
      std::string message = "The syntax '" + s + "' of the attribute 'z' on the "
        + elplusid + " does not conform to the syntax of a RelAbsVector type.";
      log->logPackageError("render", RenderTextZMustBeRelAbsVector,
        pkgVersion, level, version, message, getLine(), getColumn());

    }
    else
    {
      this->setZ(v);
    }
    v.erase();
  }

  //
  // font-size RelAbsVector (use = optional) 
  //

  s = "";
  assigned = attributes.readInto("font-size", s, getErrorLog(), false, getLine(), getColumn());
  if (!assigned)
  {
    this->mFontSize = RelAbsVector(std::numeric_limits<double>::quiet_NaN(), 
      std::numeric_limits<double>::quiet_NaN());
  }
  else
  {
    v.setCoordinate(s);
    if (!(v.isSetCoordinate()) && log)
    {
      std::string message = "The syntax '" + s + "' of the attribute 'font-size' on the "
        + elplusid + " does not conform to the syntax of a RelAbsVector type.";
      log->logPackageError("render", RenderTextFontSizeMustBeRelAbsVector,
        pkgVersion, level, version, message, getLine(), getColumn());

    }
    else
    {
      this->setFontSize(v);
    }
    v.erase();
  }
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
 */
void Text::writeElements (XMLOutputStream& stream) const
{
    SBase::writeElements(stream);
    stream << this->getText();
}
/** @endcond */



/** @cond doxygenLibsbmlInternal */
void
Text::setElementText(const std::string &text)
{
  this->setText(text);
}
/** @endcond */


#endif /* __cplusplus */


/*
 * Creates a new Text_t using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 */
LIBSBML_EXTERN
Text_t *
Text_create(unsigned int level, unsigned int version, unsigned int pkgVersion)
{
  return new Text(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this Text_t object.
 */
LIBSBML_EXTERN
Text_t*
Text_clone(const Text_t* t)
{
  if (t != NULL)
  {
    return static_cast<Text_t*>(t->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this Text_t object.
 */
LIBSBML_EXTERN
void
Text_free(Text_t* t)
{
  if (t != NULL)
  {
    delete t;
  }
}


/*
 * Returns the value of the "font-family" attribute of this Text_t.
 */
LIBSBML_EXTERN
char *
Text_getFontFamily(const Text_t * t)
{
  if (t == NULL)
  {
    return NULL;
  }

  return t->getFontFamily().empty() ? NULL :
    safe_strdup(t->getFontFamily().c_str());
}


/*
 * Returns the value of the "font-weight" attribute of this Text_t.
 */
LIBSBML_EXTERN
FontWeight_t
Text_getFontWeight(const Text_t * t)
{
  if (t == NULL)
  {
    return FONT_WEIGHT_INVALID;
  }

  return (FontWeight_t)(t->getFontWeight());
}


/*
 * Returns the value of the "font-weight" attribute of this Text_t.
 */
LIBSBML_EXTERN
char *
Text_getFontWeightAsString(const Text_t * t)
{
  return (char*)(FontWeight_toString((FontWeight_t)(t->getFontWeight())));
}


/*
 * Returns the value of the "font-style" attribute of this Text_t.
 */
LIBSBML_EXTERN
FontStyle_t
Text_getFontStyle(const Text_t * t)
{
  if (t == NULL)
  {
    return FONT_STYLE_INVALID;
  }

  return (FontStyle_t)(t->getFontStyle());
}


/*
 * Returns the value of the "font-style" attribute of this Text_t.
 */
LIBSBML_EXTERN
char *
Text_getFontStyleAsString(const Text_t * t)
{
  return (char*)(FontStyle_toString((FontStyle_t)(t->getFontStyle())));
}


/*
 * Returns the value of the "text-anchor" attribute of this Text_t.
 */
LIBSBML_EXTERN
HTextAnchor_t
Text_getTextAnchor(const Text_t * t)
{
  if (t == NULL)
  {
    return H_TEXTANCHOR_INVALID;
  }

  return (HTextAnchor_t)(t->getTextAnchor());
}


/*
 * Returns the value of the "text-anchor" attribute of this Text_t.
 */
LIBSBML_EXTERN
char *
Text_getTextAnchorAsString(const Text_t * t)
{
  return (char*)(HTextAnchor_toString((HTextAnchor_t)(t->getTextAnchor())));
}


/*
 * Returns the value of the "vtext-anchor" attribute of this Text_t.
 */
LIBSBML_EXTERN
VTextAnchor_t
Text_getVTextAnchor(const Text_t * t)
{
  if (t == NULL)
  {
    return V_TEXTANCHOR_INVALID;
  }

  return (VTextAnchor_t)(t->getVTextAnchor());
}


/*
 * Returns the value of the "vtext-anchor" attribute of this Text_t.
 */
LIBSBML_EXTERN
char *
Text_getVTextAnchorAsString(const Text_t * t)
{
  return (char*)(VTextAnchor_toString((VTextAnchor_t)(t->getVTextAnchor())));
}


/*
 * Predicate returning @c 1 (true) if this Text_t's "font-family" attribute is
 * set.
 */
LIBSBML_EXTERN
int
Text_isSetFontFamily(const Text_t * t)
{
  return (t != NULL) ? static_cast<int>(t->isSetFontFamily()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this Text_t's "font-weight" attribute is
 * set.
 */
LIBSBML_EXTERN
int
Text_isSetFontWeight(const Text_t * t)
{
  return (t != NULL) ? static_cast<int>(t->isSetFontWeight()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this Text_t's "font-style" attribute is
 * set.
 */
LIBSBML_EXTERN
int
Text_isSetFontStyle(const Text_t * t)
{
  return (t != NULL) ? static_cast<int>(t->isSetFontStyle()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this Text_t's "text-anchor" attribute is
 * set.
 */
LIBSBML_EXTERN
int
Text_isSetTextAnchor(const Text_t * t)
{
  return (t != NULL) ? static_cast<int>(t->isSetTextAnchor()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this Text_t's "vtext-anchor" attribute is
 * set.
 */
LIBSBML_EXTERN
int
Text_isSetVTextAnchor(const Text_t * t)
{
  return (t != NULL) ? static_cast<int>(t->isSetVTextAnchor()) : 0;
}


/*
 * Sets the value of the "font-family" attribute of this Text_t.
 */
LIBSBML_EXTERN
int
Text_setFontFamily(Text_t * t, const char * fontFamily)
{
  return (t != NULL) ? t->setFontFamily(fontFamily) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "font-weight" attribute of this Text_t.
 */
LIBSBML_EXTERN
int
Text_setFontWeight(Text_t * t, FontWeight_t fontWeight)
{
  return (t != NULL) ? t->setFontWeight(fontWeight) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "font-weight" attribute of this Text_t.
 */
LIBSBML_EXTERN
int
Text_setFontWeightAsString(Text_t * t, const char * fontWeight)
{
  return (t != NULL) ? t->setFontWeight(fontWeight): LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "font-style" attribute of this Text_t.
 */
LIBSBML_EXTERN
int
Text_setFontStyle(Text_t * t, FontStyle_t fontStyle)
{
  return (t != NULL) ? t->setFontStyle(fontStyle) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "font-style" attribute of this Text_t.
 */
LIBSBML_EXTERN
int
Text_setFontStyleAsString(Text_t * t, const char * fontStyle)
{
  return (t != NULL) ? t->setFontStyle(fontStyle): LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "text-anchor" attribute of this Text_t.
 */
LIBSBML_EXTERN
int
Text_setTextAnchor(Text_t * t, HTextAnchor_t textAnchor)
{
  return (t != NULL) ? t->setTextAnchor(textAnchor) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "text-anchor" attribute of this Text_t.
 */
LIBSBML_EXTERN
int
Text_setTextAnchorAsString(Text_t * t, const char * textAnchor)
{
  return (t != NULL) ? t->setTextAnchor(textAnchor): LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "vtext-anchor" attribute of this Text_t.
 */
LIBSBML_EXTERN
int
Text_setVTextAnchor(Text_t * t, VTextAnchor_t vtextAnchor)
{
  return (t != NULL) ? t->setVTextAnchor(vtextAnchor) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "vtext-anchor" attribute of this Text_t.
 */
LIBSBML_EXTERN
int
Text_setVTextAnchorAsString(Text_t * t, const char * vtextAnchor)
{
  return (t != NULL) ? t->setVTextAnchor(vtextAnchor): LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "font-family" attribute of this Text_t.
 */
LIBSBML_EXTERN
int
Text_unsetFontFamily(Text_t * t)
{
  return (t != NULL) ? t->unsetFontFamily() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "font-weight" attribute of this Text_t.
 */
LIBSBML_EXTERN
int
Text_unsetFontWeight(Text_t * t)
{
  return (t != NULL) ? t->unsetFontWeight() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "font-style" attribute of this Text_t.
 */
LIBSBML_EXTERN
int
Text_unsetFontStyle(Text_t * t)
{
  return (t != NULL) ? t->unsetFontStyle() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "text-anchor" attribute of this Text_t.
 */
LIBSBML_EXTERN
int
Text_unsetTextAnchor(Text_t * t)
{
  return (t != NULL) ? t->unsetTextAnchor() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "vtext-anchor" attribute of this Text_t.
 */
LIBSBML_EXTERN
int
Text_unsetVTextAnchor(Text_t * t)
{
  return (t != NULL) ? t->unsetVTextAnchor() : LIBSBML_INVALID_OBJECT;
}


/*
 * Returns the value of the "x" element of this Text_t.
 */
LIBSBML_EXTERN
RelAbsVector_t*
Text_getX(const Text_t * t)
{
  if (t == NULL)
  {
    return NULL;
  }

  return (RelAbsVector_t*)(&(t->getX()));
}


/*
 * Returns the value of the "y" element of this Text_t.
 */
LIBSBML_EXTERN
RelAbsVector_t*
Text_getY(const Text_t * t)
{
  if (t == NULL)
  {
    return NULL;
  }

  return (RelAbsVector_t*)(&(t->getY()));
}


/*
 * Returns the value of the "z" element of this Text_t.
 */
LIBSBML_EXTERN
RelAbsVector_t*
Text_getZ(const Text_t * t)
{
  if (t == NULL)
  {
    return NULL;
  }

  return (RelAbsVector_t*)(&(t->getZ()));
}


/*
 * Returns the value of the "font-size" element of this Text_t.
 */
LIBSBML_EXTERN
RelAbsVector_t*
Text_getFontSize(const Text_t * t)
{
  if (t == NULL)
  {
    return NULL;
  }

  return (RelAbsVector_t*)(&(t->getFontSize()));
}


/*
 * Predicate returning @c 1 (true) if this Text_t's "x" element is set.
 */
LIBSBML_EXTERN
int
Text_isSetX(const Text_t * t)
{
  return (t != NULL) ? static_cast<int>(t->isSetX()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this Text_t's "y" element is set.
 */
LIBSBML_EXTERN
int
Text_isSetY(const Text_t * t)
{
  return (t != NULL) ? static_cast<int>(t->isSetY()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this Text_t's "z" element is set.
 */
LIBSBML_EXTERN
int
Text_isSetZ(const Text_t * t)
{
  return (t != NULL) ? static_cast<int>(t->isSetZ()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this Text_t's "font-size" element is set.
 */
LIBSBML_EXTERN
int
Text_isSetFontSize(const Text_t * t)
{
  return (t != NULL) ? static_cast<int>(t->isSetFontSize()) : 0;
}


/*
 * Sets the value of the "x" element of this Text_t.
 */
LIBSBML_EXTERN
int
Text_setX(Text_t * t, const RelAbsVector_t* x)
{
  return (t != NULL) ? t->setX(*x) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "y" element of this Text_t.
 */
LIBSBML_EXTERN
int
Text_setY(Text_t * t, const RelAbsVector_t* y)
{
  return (t != NULL) ? t->setY(*y) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "z" element of this Text_t.
 */
LIBSBML_EXTERN
int
Text_setZ(Text_t * t, const RelAbsVector_t* z)
{
  return (t != NULL) ? t->setZ(*z) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "font-size" element of this Text_t.
 */
LIBSBML_EXTERN
int
Text_setFontSize(Text_t * t, const RelAbsVector_t* fontSize)
{
  return (t != NULL) ? t->setFontSize(*fontSize) : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "x" element of this Text_t.
 */
LIBSBML_EXTERN
int
Text_unsetX(Text_t * t)
{
  return (t != NULL) ? t->unsetX() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "y" element of this Text_t.
 */
LIBSBML_EXTERN
int
Text_unsetY(Text_t * t)
{
  return (t != NULL) ? t->unsetY() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "z" element of this Text_t.
 */
LIBSBML_EXTERN
int
Text_unsetZ(Text_t * t)
{
  return (t != NULL) ? t->unsetZ() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "font-size" element of this Text_t.
 */
LIBSBML_EXTERN
int
Text_unsetFontSize(Text_t * t)
{
  return (t != NULL) ? t->unsetFontSize() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * Text_t object have been set.
 */
LIBSBML_EXTERN
int
Text_hasRequiredAttributes(const Text_t * t)
{
  return (t != NULL) ? static_cast<int>(t->hasRequiredAttributes()) : 0;
}


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



LIBSBML_CPP_NAMESPACE_END


