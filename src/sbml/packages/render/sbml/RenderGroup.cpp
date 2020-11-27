/**
 * @file    RenderGroup.cpp
 * @brief Implementation of the RenderGroup class.
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

#include <sbml/packages/render/sbml/RenderGroup.h>
#include <sbml/packages/render/validator/RenderSBMLError.h>
#include <sbml/util/ElementFilter.h>

#ifndef OMIT_DEPRECATED
#ifdef DEPRECATION_WARNINGS
#include <iostream>
#endif // DEPRECATION_WARNINGS
#endif // OMIT_DEPRECATED

#include <sbml/packages/render/sbml/Image.h>
#include <sbml/packages/render/sbml/Ellipse.h>
#include <sbml/packages/render/sbml/Rectangle.h>
#include <sbml/packages/render/sbml/Polygon.h>
#include <sbml/packages/render/sbml/ListOfDrawables.h>
#include <sbml/packages/render/sbml/LineEnding.h>
#include <sbml/packages/render/sbml/ListOfLineEndings.h>
#include <sbml/packages/render/sbml/Text.h>
#include <sbml/packages/render/sbml/RenderCurve.h>

#include <sbml/xml/XMLInputStream.h>

#include <sbml/packages/layout/util/LayoutAnnotation.h>
#include <sbml/packages/render/extension/RenderExtension.h>


using namespace std;

LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


class Text;

/*
 * Creates a new RenderGroup using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 */
RenderGroup::RenderGroup(unsigned int level,
                         unsigned int version,
                         unsigned int pkgVersion)
  : GraphicalPrimitive2D(level, version, pkgVersion)
  , mStartHead ("")
  , mEndHead ("")
  , mFontFamily ("")
  , mFontWeight(FONT_WEIGHT_UNSET)
  , mFontStyle(FONT_STYLE_UNSET)
  , mTextAnchor(H_TEXTANCHOR_UNSET)
  , mVTextAnchor(V_TEXTANCHOR_UNSET)
  , mFontSize(RelAbsVector(std::numeric_limits<double>::quiet_NaN(),std::numeric_limits<double>::quiet_NaN()))
  , mElements(level, version, pkgVersion)
  , mElementName("g")
{
  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(level, version, pkgVersion));
  connectToChild();
}


/*
 * Creates a new RenderGroup using the given RenderPkgNamespaces object.
 */
RenderGroup::RenderGroup(RenderPkgNamespaces *renderns)
  : GraphicalPrimitive2D(renderns)
  , mStartHead ("")
  , mEndHead ("")
  , mFontFamily ("")
  , mFontWeight (FONT_WEIGHT_UNSET)
  , mFontStyle (FONT_STYLE_UNSET)
  , mTextAnchor(H_TEXTANCHOR_UNSET)
  , mVTextAnchor(V_TEXTANCHOR_UNSET)
  , mFontSize(RelAbsVector(std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN()))
  , mElements(renderns)
  , mElementName("g")
{
  setElementNamespace(renderns->getURI());
  connectToChild();
  loadPlugins(renderns);
}


/** @cond doxygenLibsbmlInternal */
/*
* Creates a new RenderGroup object from the given XMLNode object.
* The XMLNode object has to contain a valid XML representation of a
* RenderGroup object as defined in the render extension specification.
* This method is normally called when render information is read from a file and
* should normally not have to be called explicitly.
*
* @param node the XMLNode object reference that describes the RenderGroup
* object to be instantiated.
*/
RenderGroup::RenderGroup(const XMLNode& node, unsigned int l2version)
  : GraphicalPrimitive2D(node, l2version)
  , mElements()
{
  ExpectedAttributes ea;
  addExpectedAttributes(ea);
  const XMLAttributes& attributes = node.getAttributes();
  const XMLNode* child;
  this->readAttributes(attributes, ea);
  unsigned int n = 0, nMax = node.getNumChildren();
  while (n<nMax)
  {
    child = &node.getChild(n);
    const std::string& childName = child->getName();
    if (childName == "g")
    {
      RenderGroup* g = new RenderGroup(*child);
      mElements.appendAndOwn(g);
    }
    else if (childName == "curve")
    {
      // check whether this is an old curve or a new curve
      unsigned int i, iMax = child->getNumChildren();
      for (i = 0; i<iMax; ++i)
      {
        // we check only if the new element name is there
        // if that is the case, we just assume it is a new curve
        if (child->getChild(i).getName() == "listOfElements")
        {
          RenderCurve* c = new RenderCurve(*child);
          mElements.appendAndOwn(c);
          break;
        }
        else if (child->getChild(i).getName() == "listOfCurveSegments")
        {
          this->importOldCurve(*child);
          break;
        }
      }
    }
    else if (childName == "polygon")
    {
      Polygon* p = new Polygon(*child);
      mElements.appendAndOwn(p);
    }
    else if (childName == "rectangle")
    {
      Rectangle* r = new Rectangle(*child);
      mElements.appendAndOwn(r);
    }
    else if (childName == "ellipse")
    {
      Ellipse* e = new Ellipse(*child);
      mElements.appendAndOwn(e);
    }
    else if (childName == "text")
    {
      Text* t = new Text(*child);
      mElements.appendAndOwn(t);
    }
    else if (childName == "image")
    {
      Image* i = new Image(*child);
      mElements.appendAndOwn(i);
    }
    else if (childName == "annotation")
    {
      this->mAnnotation = new XMLNode(*child);
    }
    else if (childName == "notes")
    {
      this->mNotes = new XMLNode(*child);
    }
    ++n;
  }


  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(2, l2version));

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
RenderGroup::RenderGroup(RenderPkgNamespaces* renderns, const std::string& id) :
  GraphicalPrimitive2D(renderns, id),
  mStartHead(""),
  mEndHead(""),
  mFontFamily(""),
  mFontWeight(Text::WEIGHT_UNSET),
  mFontStyle(Text::STYLE_UNSET),
  mTextAnchor(Text::ANCHOR_UNSET),
  mVTextAnchor(Text::ANCHOR_UNSET),
  mFontSize(RelAbsVector(std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN())),
  mElements(renderns)
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
 * Copy constructor for RenderGroup.
 */
RenderGroup::RenderGroup(const RenderGroup& orig)
  : GraphicalPrimitive2D( orig )
  , mStartHead ( orig.mStartHead )
  , mEndHead ( orig.mEndHead )
  , mFontFamily ( orig.mFontFamily )
  , mFontWeight ( orig.mFontWeight )
  , mFontStyle ( orig.mFontStyle )
  , mTextAnchor ( orig.mTextAnchor )
  , mVTextAnchor ( orig.mVTextAnchor )
  , mFontSize ( orig.mFontSize )
  , mElements ( orig.mElements )
  , mElementName ( orig.mElementName )
{
  connectToChild();
}


/*
 * Assignment operator for RenderGroup.
 */
RenderGroup&
RenderGroup::operator=(const RenderGroup& rhs)
{
  if (&rhs != this)
  {
    GraphicalPrimitive2D::operator=(rhs);
    mStartHead = rhs.mStartHead;
    mEndHead = rhs.mEndHead;
    mFontFamily = rhs.mFontFamily;
    mFontWeight = rhs.mFontWeight;
    mFontStyle = rhs.mFontStyle;
    mTextAnchor = rhs.mTextAnchor;
    mVTextAnchor = rhs.mVTextAnchor;
    mFontSize = rhs.mFontSize;
    mElements = rhs.mElements;
    mElementName = rhs.mElementName;
    connectToChild();
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this RenderGroup object.
 */
RenderGroup*
RenderGroup::clone() const
{
  return new RenderGroup(*this);
}


/*
 * Destructor for RenderGroup.
 */
RenderGroup::~RenderGroup()
{
}


/*
 * Returns the value of the "startHead" attribute of this RenderGroup.
 */
const std::string&
RenderGroup::getStartHead() const
{
  return mStartHead;
}


/*
 * Returns the value of the "endHead" attribute of this RenderGroup.
 */
const std::string&
RenderGroup::getEndHead() const
{
  return mEndHead;
}


/*
 * Returns the value of the "font-family" attribute of this RenderGroup.
 */
const std::string&
RenderGroup::getFontFamily() const
{
  return mFontFamily;
}


/*
 * Returns the value of the "font-weight" attribute of this RenderGroup.
 */
int
RenderGroup::getFontWeight() const
{
  return mFontWeight;
}


/*
 * Returns the value of the "font-weight" attribute of this RenderGroup.
 */
std::string
RenderGroup::getFontWeightAsString() const
{
  std::string code_str = FontWeight_toString((FontWeight_t)(mFontWeight));
  return code_str;
}


/*
 * Returns the value of the "font-style" attribute of this RenderGroup.
 */
int
RenderGroup::getFontStyle() const
{
  return mFontStyle;
}


/*
 * Returns the value of the "font-style" attribute of this RenderGroup.
 */
std::string
RenderGroup::getFontStyleAsString() const
{
  std::string code_str = FontStyle_toString((FontStyle_t)(mFontStyle));
  return code_str;
}


/*
 * Returns the value of the "text-anchor" attribute of this RenderGroup.
 */
int
RenderGroup::getTextAnchor() const
{
  return mTextAnchor;
}


/*
 * Returns the value of the "text-anchor" attribute of this RenderGroup.
 */
std::string
RenderGroup::getTextAnchorAsString() const
{
  std::string code_str = HTextAnchor_toString((HTextAnchor_t)(mTextAnchor));
  return code_str;
}


/** @cond doxygenLibsbmlInternal */
int
RenderGroup::getVtextAnchor() const
{
  return mVTextAnchor;
}
/** @endcond */


/** @cond doxygenLibsbmlInternal */
int
RenderGroup::getVTextAnchor() const
{
  return mVTextAnchor;
}
/** @endcond */

/*
 * Returns the value of the "vtext-anchor" attribute of this RenderGroup.
 */
std::string
RenderGroup::getVTextAnchorAsString() const
{
  std::string code_str = VTextAnchor_toString((VTextAnchor_t)(mVTextAnchor));
  return code_str;
}


/** @cond doxygenLibsbmlInternal */
std::string
RenderGroup::getVtextAnchorAsString() const
{
  return getVTextAnchorAsString();
}
/** @endcond */


/*
* Returns the value of the "font-size" element of this RenderPoint.
*/
const RelAbsVector&
RenderGroup::getFontSize() const
{
  return this->mFontSize;
}


/*
* Returns the value of the "font-size" element of this RenderPoint.
*/
RelAbsVector&
RenderGroup::getFontSize()
{
  return this->mFontSize;
}


/*
 * Predicate returning @c true if this RenderGroup's "startHead" attribute is
 * set.
 */
bool
RenderGroup::isSetStartHead() const
{
  return (mStartHead.empty() == false && mStartHead != "none");
}


/*
 * Predicate returning @c true if this RenderGroup's "endHead" attribute is
 * set.
 */
bool
RenderGroup::isSetEndHead() const
{
  return (mEndHead.empty() == false && mEndHead != "none");
}


/*
 * Predicate returning @c true if this RenderGroup's "font-family" attribute is
 * set.
 */
bool
RenderGroup::isSetFontFamily() const
{
  return (mFontFamily.empty() == false);
}


/*
 * Predicate returning @c true if this RenderGroup's "font-weight" attribute is
 * set.
 */
bool
RenderGroup::isSetFontWeight() const
{
  return (mFontWeight != FONT_WEIGHT_INVALID && mFontWeight != FONT_WEIGHT_UNSET);
}


/*
 * Predicate returning @c true if this RenderGroup's "font-style" attribute is
 * set.
 */
bool
RenderGroup::isSetFontStyle() const
{
  return (mFontStyle != FONT_STYLE_INVALID && mFontStyle != FONT_STYLE_UNSET);
}


/*
 * Predicate returning @c true if this RenderGroup's "text-anchor" attribute is
 * set.
 */
bool
RenderGroup::isSetTextAnchor() const
{
  return (mTextAnchor != H_TEXTANCHOR_INVALID && mTextAnchor != H_TEXTANCHOR_UNSET);
}


/*
 * Predicate returning @c true if this RenderGroup's "vtext-anchor" attribute
 * is set.
 */
bool
RenderGroup::isSetVTextAnchor() const
{
  return (mVTextAnchor != V_TEXTANCHOR_INVALID && mVTextAnchor != V_TEXTANCHOR_UNSET);
}


/** @cond doxygenLibsbmlInternal */
bool
RenderGroup::isSetVtextAnchor() const
{
  return isSetVTextAnchor();
}
/** @endcond */


/*
* Returns true if the font size has been set or false otherwise.
*/
bool RenderGroup::isSetFontSize() const
{
  return mFontSize.isSetCoordinate();
}


/*
 * Sets the value of the "startHead" attribute of this RenderGroup.
 */
int
RenderGroup::setStartHead(const std::string& startHead)
{
  if (!(SyntaxChecker::isValidInternalSId(startHead)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mStartHead = startHead;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "endHead" attribute of this RenderGroup.
 */
int
RenderGroup::setEndHead(const std::string& endHead)
{
  if (!(SyntaxChecker::isValidInternalSId(endHead)))
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mEndHead = endHead;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "font-family" attribute of this RenderGroup.
 */
int
RenderGroup::setFontFamily(const std::string& fontFamily)
{
  mFontFamily = fontFamily;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "font-weight" attribute of this RenderGroup.
 */
int
RenderGroup::setFontWeight(const FontWeight_t fontWeight)
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
int RenderGroup::setFontWeight(Text::FONT_WEIGHT weight)
{
  mFontWeight = weight;
  return LIBSBML_OPERATION_SUCCESS;
}
/** @endcond */


/*
 * Sets the value of the "font-weight" attribute of this RenderGroup.
 */
int
RenderGroup::setFontWeight(const std::string& fontWeight)
{
  mFontWeight = FontWeight_fromString(fontWeight.c_str());

  if (mFontWeight == FONT_WEIGHT_INVALID)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }

  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "font-style" attribute of this RenderGroup.
 */
int
RenderGroup::setFontStyle(const FontStyle_t fontStyle)
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
int RenderGroup::setFontStyle(Text::FONT_STYLE style)
{
  mFontStyle = style;
  return LIBSBML_OPERATION_SUCCESS;
}
/** @endcond */


/*
 * Sets the value of the "font-style" attribute of this RenderGroup.
 */
int
RenderGroup::setFontStyle(const std::string& fontStyle)
{
  mFontStyle = FontStyle_fromString(fontStyle.c_str());

  if (mFontStyle == FONT_STYLE_INVALID)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }

  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "text-anchor" attribute of this RenderGroup.
 */
int
RenderGroup::setTextAnchor(const HTextAnchor_t textAnchor)
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
*/
int RenderGroup::setTextAnchor(Text::TEXT_ANCHOR  anchor)
{
  mTextAnchor = anchor;
  return LIBSBML_OPERATION_SUCCESS;
}
/** @endcond */


/*
 * Sets the value of the "text-anchor" attribute of this RenderGroup.
 */
int
RenderGroup::setTextAnchor(const std::string& textAnchor)
{
  mTextAnchor = HTextAnchor_fromString(textAnchor.c_str());

  if (mTextAnchor == H_TEXTANCHOR_INVALID)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }

  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "vtext-anchor" attribute of this RenderGroup.
 */
int
RenderGroup::setVTextAnchor(const VTextAnchor_t vtextAnchor)
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
int
RenderGroup::setVtextAnchor(const VTextAnchor_t vtextAnchor)
{
  return setVTextAnchor(vtextAnchor);
}
/** @endcond */

int RenderGroup::setVTextAnchor(Text::TEXT_ANCHOR anchor)
{
  mVTextAnchor = anchor;
  return LIBSBML_OPERATION_SUCCESS;
}

/** @cond doxygenLibsbmlInternal */
int RenderGroup::setVtextAnchor(Text::TEXT_ANCHOR anchor)
{
  return setVTextAnchor(anchor);
}
/** @endcond */


/*
 * Sets the value of the "vtext-anchor" attribute of this RenderGroup.
 */
int
RenderGroup::setVTextAnchor(const std::string& vtextAnchor)
{
  mVTextAnchor = VTextAnchor_fromString(vtextAnchor.c_str());

  if (mVTextAnchor == V_TEXTANCHOR_INVALID)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }

  return LIBSBML_OPERATION_SUCCESS;
}


/** @cond doxygenLibsbmlInternal */
int
RenderGroup::setVtextAnchor(const std::string& vtextAnchor)
{
  return setVTextAnchor(vtextAnchor);
}
/** @endcond */


/*
* Sets the font size.
*/
int RenderGroup::setFontSize(const RelAbsVector& size)
{
  mFontSize = size;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "startHead" attribute of this RenderGroup.
 */
int
RenderGroup::unsetStartHead()
{
  mStartHead.erase();

  if (mStartHead.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "endHead" attribute of this RenderGroup.
 */
int
RenderGroup::unsetEndHead()
{
  mEndHead.erase();

  if (mEndHead.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "font-family" attribute of this RenderGroup.
 */
int
RenderGroup::unsetFontFamily()
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
 * Unsets the value of the "font-weight" attribute of this RenderGroup.
 */
int
RenderGroup::unsetFontWeight()
{
  mFontWeight = FONT_WEIGHT_INVALID;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "font-style" attribute of this RenderGroup.
 */
int
RenderGroup::unsetFontStyle()
{
  mFontStyle = FONT_STYLE_INVALID;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "text-anchor" attribute of this RenderGroup.
 */
int
RenderGroup::unsetTextAnchor()
{
  mTextAnchor = H_TEXTANCHOR_INVALID;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "vtext-anchor" attribute of this RenderGroup.
 */
int
RenderGroup::unsetVTextAnchor()
{
  mVTextAnchor = V_TEXTANCHOR_INVALID;
  return LIBSBML_OPERATION_SUCCESS;
}


/** @cond doxygenLibsbmlInternal */
int
RenderGroup::unsetVtextAnchor()
{
  return unsetVTextAnchor();
}
/** @endcond */


/*
 * Unsets the value of the "font-size" element of this RenderGroup.
 */
int
RenderGroup::unsetFontSize()
{
  mFontSize.unsetCoordinate();
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Returns the ListOfDrawables from this RenderGroup.
 */
const ListOfDrawables*
RenderGroup::getListOfElements() const
{
  return &mElements;
}


/*
 * Returns the ListOfDrawables from this RenderGroup.
 */
ListOfDrawables*
RenderGroup::getListOfElements()
{
  return &mElements;
}


/*
 * Get a Transformation2D from the RenderGroup.
 */
Transformation2D*
RenderGroup::getElement(unsigned int n)
{
  return mElements.get(n);
}


/*
 * Get a Transformation2D from the RenderGroup.
 */
const Transformation2D*
RenderGroup::getElement(unsigned int n) const
{
  return mElements.get(n);
}


/*
* Returns pointer to the element with the given @p id.
*/
Transformation2D* RenderGroup::getElement(const std::string& id)
{
  return this->mElements.get(id);
}


/*
* Returns const pointer to the element with given index.
*/
const Transformation2D* RenderGroup::getElement(const std::string& id) const
{
  return this->mElements.get(id);
}


/*
 * Adds a copy of the given Transformation2D to this RenderGroup.
 */
int
RenderGroup::addElement(const Transformation2D* td)
{
  if (td == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (td->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != td->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != td->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(td)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    return mElements.append(td);
  }
}


/*
* Adds a copy of the given Transformation2D to this RenderGroup.
*/
int
RenderGroup::addChildElement(const Transformation2D* td)
{
  if (td == NULL)
  {
    return LIBSBML_OPERATION_FAILED;
  }
  else if (td->hasRequiredAttributes() == false)
  {
    return LIBSBML_INVALID_OBJECT;
  }
  else if (getLevel() != td->getLevel())
  {
    return LIBSBML_LEVEL_MISMATCH;
  }
  else if (getVersion() != td->getVersion())
  {
    return LIBSBML_VERSION_MISMATCH;
  }
  else if (matchesRequiredSBMLNamespacesForAddition(static_cast<const
    SBase*>(td)) == false)
  {
    return LIBSBML_NAMESPACES_MISMATCH;
  }
  else
  {
    return mElements.append(td);
  }
}


/*
 * Get the number of Transformation2D objects in this RenderGroup.
 */
unsigned int
RenderGroup::getNumElements() const
{
  return mElements.size();
}


/*
 * Creates a new Image object, adds it to this RenderGroup object and returns
 * the Image object created.
 */
Image*
RenderGroup::createImage()
{
  Image* i = NULL;

  try
  {
    RENDER_CREATE_NS(renderns, getSBMLNamespaces());
    i = new Image(renderns);
    delete renderns;
  }
  catch (...)
  {
  }

  if (i != NULL)
  {
    mElements.appendAndOwn(i);
  }

  return i;
}


/*
 * Creates a new Ellipse object, adds it to this RenderGroup object and returns
 * the Ellipse object created.
 */
Ellipse*
RenderGroup::createEllipse()
{
  Ellipse* e = NULL;

  try
  {
    RENDER_CREATE_NS(renderns, getSBMLNamespaces());
    e = new Ellipse(renderns);
    delete renderns;
  }
  catch (...)
  {
  }

  if (e != NULL)
  {
    mElements.appendAndOwn(e);
  }

  return e;
}


/*
 * Creates a new Rectangle object, adds it to this RenderGroup object and
 * returns the Rectangle object created.
 */
Rectangle*
RenderGroup::createRectangle()
{
  Rectangle* r = NULL;

  try
  {
    RENDER_CREATE_NS(renderns, getSBMLNamespaces());
    r = new Rectangle(renderns);
    delete renderns;
  }
  catch (...)
  {
  }

  if (r != NULL)
  {
    mElements.appendAndOwn(r);
  }

  return r;
}


/*
 * Creates a new Polygon object, adds it to this RenderGroup object and returns
 * the Polygon object created.
 */
Polygon*
RenderGroup::createPolygon()
{
  Polygon* p = NULL;

  try
  {
    RENDER_CREATE_NS(renderns, getSBMLNamespaces());
    p = new Polygon(renderns);
    delete renderns;
  }
  catch (...)
  {
  }

  if (p != NULL)
  {
    mElements.appendAndOwn(p);
  }

  return p;
}


/*
 * Creates a new RenderGroup object, adds it to this RenderGroup object and
 * returns the RenderGroup object created.
 */
RenderGroup*
RenderGroup::createGroup()
{
  RenderGroup* rg = NULL;

  try
  {
    RENDER_CREATE_NS(renderns, getSBMLNamespaces());
    rg = new RenderGroup(renderns);
    delete renderns;
  }
  catch (...)
  {
  }

  if (rg != NULL)
  {
    mElements.appendAndOwn(rg);
  }

  return rg;
}


/*
 * Creates a new LineEnding object, adds it to this RenderGroup object and
 * returns the LineEnding object created.
 */
LineEnding*
RenderGroup::createLineEnding()
{
  LineEnding* le = NULL;

  try
  {
    RENDER_CREATE_NS(renderns, getSBMLNamespaces());
    le = new LineEnding(renderns);
    delete renderns;
  }
  catch (...)
  {
  }

  if (le != NULL)
  {
    mElements.appendAndOwn(le);
  }

  return le;
}


/*
 * Creates a new Text object, adds it to this RenderGroup object and returns
 * the Text object created.
 */
Text*
RenderGroup::createText()
{
  Text* t = NULL;

  try
  {
    RENDER_CREATE_NS(renderns, getSBMLNamespaces());
    t = new Text(renderns);
    delete renderns;
  }
  catch (...)
  {
  }

  if (t != NULL)
  {
    mElements.appendAndOwn(t);
  }

  return t;
}


/*
 * Creates a new RenderCurve object, adds it to this RenderGroup object and
 * returns the RenderCurve object created.
 */
RenderCurve*
RenderGroup::createCurve()
{
  RenderCurve* rc = NULL;

  try
  {
    RENDER_CREATE_NS(renderns, getSBMLNamespaces());
    rc = new RenderCurve(renderns);
    delete renderns;
  }
  catch (...)
  {
  }

  if (rc != NULL)
  {
    mElements.appendAndOwn(rc);
  }

  return rc;
}


/*
 * Removes the nth Transformation2D from this RenderGroup and returns a pointer
 * to it.
 */
Transformation2D*
RenderGroup::removeElement(unsigned int n)
{
  return mElements.remove(n);
}


/*
* Removes the nth Transformation2D from this RenderGroup and returns a pointer
* to it.
*/
Transformation2D*
RenderGroup::removeElement(const std::string& sid)
{
  return mElements.remove(sid);
}


/*
 * @copydoc doc_renamesidref_common
 */
void
RenderGroup::renameSIdRefs(const std::string& oldid, const std::string& newid)
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


/*
 * Returns the XML element name of this RenderGroup object.
 */
const std::string&
RenderGroup::getElementName() const
{
  static std::string name = "g";
  return name;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the XML name of this RenderGroup object.
 */
void
RenderGroup::setElementName(const std::string& name)
{
  mElementName = name;
}

/** @endcond */


/*
 * Returns the libSBML type code for this RenderGroup object.
 */
int
RenderGroup::getTypeCode() const
{
  return SBML_RENDER_GROUP;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * RenderGroup object have been set.
 */
bool
RenderGroup::hasRequiredAttributes() const
{
  bool allPresent = GraphicalPrimitive2D::hasRequiredAttributes();

  return allPresent;
}


/*
 * Predicate returning @c true if all the required elements for this
 * RenderGroup object have been set.
 */
bool
RenderGroup::hasRequiredElements() const
{
  bool allPresent = GraphicalPrimitive2D::hasRequiredElements();

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
RenderGroup::writeElements(XMLOutputStream& stream) const
{
  GraphicalPrimitive2D::writeElements(stream);

  for (unsigned int i = 0; i < getNumElements(); i++)
  {
    getElement(i)->write(stream);
  }

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
RenderGroup::accept(SBMLVisitor& v) const
{
  v.visit(*this);

  mElements.accept(v);

  v.leave(*this);
  return true;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
RenderGroup::setSBMLDocument(SBMLDocument* d)
{
  GraphicalPrimitive2D::setSBMLDocument(d);

  mElements.setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Connects to child elements
 */
void
RenderGroup::connectToChild()
{
  GraphicalPrimitive2D::connectToChild();

  mElements.connectToParent(this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
RenderGroup::enablePackageInternal(const std::string& pkgURI,
                                   const std::string& pkgPrefix,
                                   bool flag)
{
  GraphicalPrimitive2D::enablePackageInternal(pkgURI, pkgPrefix, flag);

  mElements.enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this RenderGroup.
 */
int
RenderGroup::getAttribute(const std::string& attributeName, bool& value) const
{
  int return_value = GraphicalPrimitive2D::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this RenderGroup.
 */
int
RenderGroup::getAttribute(const std::string& attributeName, int& value) const
{
  int return_value = GraphicalPrimitive2D::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this RenderGroup.
 */
int
RenderGroup::getAttribute(const std::string& attributeName,
                          double& value) const
{
  int return_value = GraphicalPrimitive2D::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this RenderGroup.
 */
int
RenderGroup::getAttribute(const std::string& attributeName,
                          unsigned int& value) const
{
  int return_value = GraphicalPrimitive2D::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this RenderGroup.
 */
int
RenderGroup::getAttribute(const std::string& attributeName,
                          std::string& value) const
{
  int return_value = GraphicalPrimitive2D::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "startHead")
  {
    value = getStartHead();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "endHead")
  {
    value = getEndHead();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "font-family")
  {
    value = getFontFamily();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "font-weight")
  {
    value = getFontWeightAsString();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "font-style")
  {
    value = getFontStyleAsString();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "text-anchor")
  {
    value = getTextAnchorAsString();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "vtext-anchor")
  {
    value = getVTextAnchorAsString();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this RenderGroup's attribute "attributeName"
 * is set.
 */
bool
RenderGroup::isSetAttribute(const std::string& attributeName) const
{
  bool value = GraphicalPrimitive2D::isSetAttribute(attributeName);

  if (attributeName == "startHead")
  {
    value = isSetStartHead();
  }
  else if (attributeName == "endHead")
  {
    value = isSetEndHead();
  }
  else if (attributeName == "font-family")
  {
    value = isSetFontFamily();
  }
  else if (attributeName == "font-weight")
  {
    value = isSetFontWeight();
  }
  else if (attributeName == "font-style")
  {
    value = isSetFontStyle();
  }
  else if (attributeName == "text-anchor")
  {
    value = isSetTextAnchor();
  }
  else if (attributeName == "vtext-anchor")
  {
    value = isSetVTextAnchor();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this RenderGroup.
 */
int
RenderGroup::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = GraphicalPrimitive2D::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this RenderGroup.
 */
int
RenderGroup::setAttribute(const std::string& attributeName, int value)
{
  int return_value = GraphicalPrimitive2D::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this RenderGroup.
 */
int
RenderGroup::setAttribute(const std::string& attributeName, double value)
{
  int return_value = GraphicalPrimitive2D::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this RenderGroup.
 */
int
RenderGroup::setAttribute(const std::string& attributeName,
                          unsigned int value)
{
  int return_value = GraphicalPrimitive2D::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this RenderGroup.
 */
int
RenderGroup::setAttribute(const std::string& attributeName,
                          const std::string& value)
{
  int return_value = GraphicalPrimitive2D::setAttribute(attributeName, value);

  if (attributeName == "startHead")
  {
    return_value = setStartHead(value);
  }
  else if (attributeName == "endHead")
  {
    return_value = setEndHead(value);
  }
  else if (attributeName == "font-family")
  {
    return_value = setFontFamily(value);
  }
  else if (attributeName == "font-weight")
  {
    return_value = setFontWeight(value);
  }
  else if (attributeName == "font-style")
  {
    return_value = setFontStyle(value);
  }
  else if (attributeName == "text-anchor")
  {
    return_value = setTextAnchor(value);
  }
  else if (attributeName == "vtext-anchor")
  {
    return_value = setVTextAnchor(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this RenderGroup.
 */
int
RenderGroup::unsetAttribute(const std::string& attributeName)
{
  int value = GraphicalPrimitive2D::unsetAttribute(attributeName);

  if (attributeName == "startHead")
  {
    value = unsetStartHead();
  }
  else if (attributeName == "endHead")
  {
    value = unsetEndHead();
  }
  else if (attributeName == "font-family")
  {
    value = unsetFontFamily();
  }
  else if (attributeName == "font-weight")
  {
    value = unsetFontWeight();
  }
  else if (attributeName == "font-style")
  {
    value = unsetFontStyle();
  }
  else if (attributeName == "text-anchor")
  {
    value = unsetTextAnchor();
  }
  else if (attributeName == "vtext-anchor")
  {
    value = unsetVTextAnchor();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Creates and returns an new "elementName" object in this RenderGroup.
 */
SBase*
RenderGroup::createChildObject(const std::string& elementName)
{
  GraphicalPrimitive2D* obj = NULL;

  if (elementName == "image")
  {
    return createImage();
  }
  else if (elementName == "ellipse")
  {
    return createEllipse();
  }
  else if (elementName == "rectangle")
  {
    return createRectangle();
  }
  else if (elementName == "polygon")
  {
    return createPolygon();
  }
  else if (elementName == "g")
  {
    return createGroup();
  }
  else if (elementName == "lineEnding")
  {
    return createLineEnding();
  }
  else if (elementName == "text")
  {
    return createText();
  }
  else if (elementName == "curve")
  {
    return createCurve();
  }

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds a new "elementName" object to this RenderGroup.
 */
int
RenderGroup::addChildObject(const std::string& elementName,
                            const SBase* element)
{
  if (elementName == "image" && element->getTypeCode() ==
    SBML_RENDER_IMAGE)
  {
    return addElement((const Transformation2D*)(element));
  }
  else if (elementName == "ellipse" && element->getTypeCode() ==
    SBML_RENDER_ELLIPSE)
  {
    return addElement((const Transformation2D*)(element));
  }
  else if (elementName == "rectangle" && element->getTypeCode() ==
    SBML_RENDER_RECTANGLE)
  {
    return addElement((const Transformation2D*)(element));
  }
  else if (elementName == "polygon" && element->getTypeCode() ==
    SBML_RENDER_POLYGON)
  {
    return addElement((const Transformation2D*)(element));
  }
  else if (elementName == "g" && element->getTypeCode() == SBML_RENDER_GROUP)
  {
    return addElement((const Transformation2D*)(element));
  }
  else if (elementName == "lineEnding" && element->getTypeCode() ==
    SBML_RENDER_LINEENDING)
  {
    return addElement((const Transformation2D*)(element));
  }
  else if (elementName == "text" && element->getTypeCode() == SBML_RENDER_TEXT)
  {
    return addElement((const Transformation2D*)(element));
  }
  else if (elementName == "curve" && element->getTypeCode() ==
    SBML_RENDER_CURVE)
  {
    return addElement((const Transformation2D*)(element));
  }

  return LIBSBML_OPERATION_FAILED;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Removes and returns the new "elementName" object with the given id in this
 * RenderGroup.
 */
SBase*
RenderGroup::removeChildObject(const std::string& elementName,
                               const std::string& id)
{
  if (elementName == "image")
  {
    for (unsigned int i = 0; i < getNumElements(); i++)
    {
      if (getElement(i)->getId() == id)
      {
        return removeElement(i);
      }
    }
  }
  else if (elementName == "ellipse")
  {
    for (unsigned int i = 0; i < getNumElements(); i++)
    {
      if (getElement(i)->getId() == id)
      {
        return removeElement(i);
      }
    }
  }
  else if (elementName == "rectangle")
  {
    for (unsigned int i = 0; i < getNumElements(); i++)
    {
      if (getElement(i)->getId() == id)
      {
        return removeElement(i);
      }
    }
  }
  else if (elementName == "polygon")
  {
    for (unsigned int i = 0; i < getNumElements(); i++)
    {
      if (getElement(i)->getId() == id)
      {
        return removeElement(i);
      }
    }
  }
  else if (elementName == "g")
  {
    for (unsigned int i = 0; i < getNumElements(); i++)
    {
      if (getElement(i)->getId() == id)
      {
        return removeElement(i);
      }
    }
  }
  else if (elementName == "lineEnding")
  {
    for (unsigned int i = 0; i < getNumElements(); i++)
    {
      if (getElement(i)->getId() == id)
      {
        return removeElement(i);
      }
    }
  }
  else if (elementName == "text")
  {
    for (unsigned int i = 0; i < getNumElements(); i++)
    {
      if (getElement(i)->getId() == id)
      {
        return removeElement(i);
      }
    }
  }
  else if (elementName == "curve")
  {
    for (unsigned int i = 0; i < getNumElements(); i++)
    {
      if (getElement(i)->getId() == id)
      {
        return removeElement(i);
      }
    }
  }

  return NULL;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the number of "elementName" in this RenderGroup.
 */
unsigned int
RenderGroup::getNumObjects(const std::string& elementName)
{
  unsigned int n = 0;

  if (elementName == "element")
  {
    return getNumElements();
  }

  return n;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the nth object of "objectName" in this RenderGroup.
 */
SBase*
RenderGroup::getObject(const std::string& elementName, unsigned int index)
{
  SBase* obj = NULL;

  if (elementName == "element")
  {
    return getElement(index);
  }

  return obj;
}

/** @endcond */


/*
 * Returns the first child element that has the given @p id in the model-wide
 * SId namespace, or @c NULL if no such object is found.
 */
SBase*
RenderGroup::getElementBySId(const std::string& id)
{
  if (id.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  obj = mElements.getElementBySId(id);

  if (obj != NULL)
  {
    return obj;
  }

  return obj;
}


/*
 * Returns the first child element that has the given @p metaid, or @c NULL if
 * no such object is found.
 */
SBase*
RenderGroup::getElementByMetaId(const std::string& metaid)
{
  if (metaid.empty())
  {
    return NULL;
  }

  SBase* obj = NULL;

  if (mElements.getMetaId() == metaid)
  {
    return &mElements;
  }

  obj = mElements.getElementByMetaId(metaid);

  if (obj != NULL)
  {
    return obj;
  }

  return obj;
}


/*
 * Returns a List of all child SBase objects, including those nested to an
 * arbitrary depth.
 */
List*
RenderGroup::getAllElements(ElementFilter* filter)
{
  List* ret = new List();
  List* sublist = NULL;

  ADD_FILTERED_LIST(ret, sublist, mElements, filter);

  ADD_FILTERED_FROM_PLUGIN(ret, sublist, filter);

  return ret;
}


XMLNode RenderGroup::toXML() const
{
  return getXmlNodeForSBase(this);
}


/** @cond doxygenLibsbmlInternal */

/*
 * Creates a new object from the next XMLToken on the XMLInputStream
 */
SBase*
RenderGroup::createObject(XMLInputStream& stream)
{
  SBase* obj = GraphicalPrimitive2D::createObject(stream);
  
  obj = mElements.createObject(stream);
  
  connectToChild();

  return obj;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
RenderGroup::addExpectedAttributes(ExpectedAttributes& attributes)
{
  GraphicalPrimitive2D::addExpectedAttributes(attributes);

  attributes.add("startHead");

  attributes.add("endHead");

  attributes.add("font-family");

  attributes.add("font-weight");

  attributes.add("font-style");

  attributes.add("text-anchor");

  attributes.add("vtext-anchor");

  attributes.add("font-size");
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
RenderGroup::readAttributes(const XMLAttributes& attributes,
                            const ExpectedAttributes& expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  GraphicalPrimitive2D::readAttributes(attributes, expectedAttributes);

  if (log)
  {
    numErrs = log->getNumErrors();

    for (int n = numErrs-1; n >= 0; n--)
    {
      if (log->getError(n)->getErrorId() == UnknownPackageAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownPackageAttribute);
        log->logPackageError("render", RenderRenderGroupAllowedAttributes,
          pkgVersion, level, version, details);
      }
      else if (log->getError(n)->getErrorId() == UnknownCoreAttribute)
      {
        const std::string details = log->getError(n)->getMessage();
        log->remove(UnknownCoreAttribute);
        log->logPackageError("render", RenderRenderGroupAllowedCoreAttributes,
          pkgVersion, level, version, details, getLine(), getColumn());
      }
    }
  }

  // 
  // startHead SIdRef (use = "optional" )
  // 

  assigned = attributes.readInto("startHead", mStartHead);

  if (assigned == true)
  {
    if (log && mStartHead.empty() == true)
    {
      logEmptyString(mStartHead, level, version, "<RenderGroup>");
    }
    else if (log && SyntaxChecker::isValidSBMLSId(mStartHead) == false)
    {
      std::string msg = "The startHead attribute on the <" + getElementName() +
        ">";
      if (isSetId())
      {
        msg += " with id '" + getId() + "'";
      }

      msg += " is '" + mStartHead + "', which does not conform to the syntax.";
      log->logPackageError("render",
        RenderRenderGroupStartHeadMustBeLineEnding, pkgVersion, level, version,
          msg, getLine(), getColumn());
    }
  }

  // 
  // endHead SIdRef (use = "optional" )
  // 

  assigned = attributes.readInto("endHead", mEndHead);

  if (assigned == true)
  {
    if (log && mEndHead.empty() == true)
    {
      logEmptyString(mEndHead, level, version, "<RenderGroup>");
    }
    else if (log && SyntaxChecker::isValidSBMLSId(mEndHead) == false)
    {
      std::string msg = "The endHead attribute on the <" + getElementName() +
        ">";
      if (isSetId())
      {
        msg += " with id '" + getId() + "'";
      }

      msg += " is '" + mEndHead + "', which does not conform to the syntax.";
      log->logPackageError("render", RenderRenderGroupEndHeadMustBeLineEnding,
        pkgVersion, level, version, msg, getLine(), getColumn());
    }
  }

  // 
  // font-family string (use = "optional" )
  // 

  assigned = attributes.readInto("font-family", mFontFamily);

  if (assigned == true)
  {
    if (log && mFontFamily.empty() == true)
    {
      logEmptyString(mFontFamily, level, version, "<RenderGroup>");
    }
  }

  // 
  // font-weight enum (use = "optional" )
  // 

  std::string fontWeight;
  assigned = attributes.readInto("font-weight", fontWeight);

  if (assigned == true)
  {
    if (fontWeight.empty() == true)
    {
      if (log)
      {
        logEmptyString(fontWeight, level, version, "<RenderGroup>");
      }
    }
    else
    {
      mFontWeight = FontWeight_fromString(fontWeight.c_str());

      if (log && FontWeight_isValid((FontWeight_t)(mFontWeight)) == 0)
      {
        std::string msg = "The font-weight on the <RenderGroup> ";

        if (isSetId())
        {
          msg += "with id '" + getId() + "'";
        }

        msg += "is '" + fontWeight + "', which is not a valid option.";

        log->logPackageError("render",
          RenderRenderGroupFontWeightMustBeFontWeightEnum, pkgVersion, level,
            version, msg, getLine(), getColumn());
      }
    }
  }
  else
  {
    setFontWeight(FONT_WEIGHT_UNSET);
  }

  // 
  // font-style enum (use = "optional" )
  // 

  std::string fontStyle;
  assigned = attributes.readInto("font-style", fontStyle);

  if (assigned == true)
  {
    if (log && fontStyle.empty() == true)
    {
      logEmptyString(fontStyle, level, version, "<RenderGroup>");
    }
    else
    {
      mFontStyle = FontStyle_fromString(fontStyle.c_str());

      if (log && FontStyle_isValid((FontStyle_t)(mFontStyle)) == 0)
      {
        std::string msg = "The font-style on the <RenderGroup> ";

        if (isSetId())
        {
          msg += "with id '" + getId() + "'";
        }

        msg += "is '" + fontStyle + "', which is not a valid option.";

        log->logPackageError("render",
          RenderRenderGroupFontStyleMustBeFontStyleEnum, pkgVersion, level,
            version, msg, getLine(), getColumn());
      }
    }
  }
  else
  {
    setFontStyle(FONT_STYLE_UNSET);
  }

  // 
  // text-anchor enum (use = "optional" )
  // 

  std::string textAnchor;
  assigned = attributes.readInto("text-anchor", textAnchor);

  if (assigned == true)
  {
    if (log && textAnchor.empty() == true)
    {
      logEmptyString(textAnchor, level, version, "<RenderGroup>");
    }
    else
    {
      mTextAnchor = HTextAnchor_fromString(textAnchor.c_str());

      if (log && HTextAnchor_isValid((HTextAnchor_t)(mTextAnchor)) == 0)
      {
        std::string msg = "The text-anchor on the <RenderGroup> ";

        if (isSetId())
        {
          msg += "with id '" + getId() + "'";
        }

        msg += "is '" + textAnchor + "', which is not a valid option.";

        log->logPackageError("render",
          RenderRenderGroupTextAnchorMustBeHTextAnchorEnum, pkgVersion, level,
            version, msg, getLine(), getColumn());
      }
    }
  }
  else
  {
    setTextAnchor(H_TEXTANCHOR_UNSET);
  }

  // 
  // vtext-anchor enum (use = "optional" )
  // 

  std::string vtextAnchor;
  assigned = attributes.readInto("vtext-anchor", vtextAnchor);

  if (assigned == true)
  {
    if (log && vtextAnchor.empty() == true)
    {
      logEmptyString(vtextAnchor, level, version, "<RenderGroup>");
    }
    else
    {
      mVTextAnchor = VTextAnchor_fromString(vtextAnchor.c_str());

      if (log && VTextAnchor_isValid((VTextAnchor_t)(mVTextAnchor)) == 0)
      {
        std::string msg = "The vtext-anchor on the <RenderGroup> ";

        if (isSetId())
        {
          msg += "with id '" + getId() + "'";
        }

        msg += "is '" + vtextAnchor + "', which is not a valid option.";

        log->logPackageError("render",
          RenderRenderGroupVtextAnchorMustBeVTextAnchorEnum, pkgVersion, level,
            version, msg, getLine(), getColumn());
      }
    }
  }
  else
  {
    setVTextAnchor(V_TEXTANCHOR_UNSET);
  }

  //
  // font-size RelAbsVector (use = optional) 
  //

  string s = "";
  RelAbsVector v;
  assigned = attributes.readInto("font-size", s, getErrorLog(), false, getLine(), getColumn());
  if (!assigned)
  {
    this->mFontSize = RelAbsVector(std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::quiet_NaN());
  }
  else
  {
    v.setCoordinate(s);
    if (!(v.isSetCoordinate()))
    {
      if (log)
      {
        std::string message = "The syntax '" + s + "' of the attribute 'font-size' on the ";
        if (isSetId())
        {
          message += "with id '" + getId() + "'";
        }
        message += " does not conform to the syntax of a RelAbsVector type.";
        log->logPackageError("render", RenderRenderGroupFontSizeMustBeRelAbsVector,
          pkgVersion, level, version, message, getLine(), getColumn());
      }

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
 * Writes the attributes to the stream
 */
void
RenderGroup::writeAttributes(XMLOutputStream& stream) const
{
  GraphicalPrimitive2D::writeAttributes(stream);

  if (isSetStartHead() == true)
  {
    stream.writeAttribute("startHead", getPrefix(), mStartHead);
  }

  if (isSetEndHead() == true)
  {
    stream.writeAttribute("endHead", getPrefix(), mEndHead);
  }

  if (isSetFontFamily() == true)
  {
    stream.writeAttribute("font-family", getPrefix(), mFontFamily);
  }

  if (isSetFontWeight() == true)
  {
    stream.writeAttribute("font-weight", getPrefix(),
      FontWeight_toString((FontWeight_t)(mFontWeight)));
  }

  if (isSetFontStyle() == true)
  {
    stream.writeAttribute("font-style", getPrefix(),
      FontStyle_toString((FontStyle_t)(mFontStyle)));
  }

  if (isSetTextAnchor() == true)
  {
    stream.writeAttribute("text-anchor", getPrefix(),
      HTextAnchor_toString((HTextAnchor_t)(mTextAnchor)));
  }

  if (isSetVTextAnchor() == true)
  {
    stream.writeAttribute("vtext-anchor", getPrefix(),
      VTextAnchor_toString((VTextAnchor_t)(mVTextAnchor)));
  }

  if (this->isSetFontSize())
  {
    std::ostringstream os;
    os << this->getFontSize();
    stream.writeAttribute("font-size", getPrefix(), os.str());
  }

  SBase::writeExtensionAttributes(stream);
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
  const XMLAttributes& curveAttributes = node.getAttributes();
  const XMLNode* child;
  unsigned int n = 0, nMax = node.getNumChildren();
  //const XMLNode* pOrigAnnotation=NULL;
  //const XMLNode* pOrigNotes=NULL;
  RENDER_CREATE_NS(renderns, this->getSBMLNamespaces());
  while (n<nMax)
  {
    child = &node.getChild(n);
    std::string childName = child->getName();
    if (childName == "listOfCurveSegments")
    {
      unsigned int i, iMax = child->getNumChildren();
      const XMLNode* child2 = NULL;
      RenderPoint start(renderns);
      RenderPoint end(renderns);
      RenderPoint lastEnd(renderns);
      RenderPoint bp1(renderns);
      RenderPoint bp2(renderns);
      bool startSet = false;
      bool endSet = false;
      bool bp1Set = false;
      bool bp2Set = false;
      std::string childName2;
      RenderCurve* pCurve = new RenderCurve(renderns);
      // read the attributes
      ExpectedAttributes ea;
      pCurve->readAttributes(curveAttributes, ea);
      for (i = 0; i<iMax; ++i)
      {
        child2 = &child->getChild(i);
        childName2 = child2->getName();
        if (childName2 == "curveSegment")
        {
          startSet = false;
          endSet = false;
          bp1Set = false;
          bp2Set = false;
          const XMLAttributes& innerAttributes = child2->getAttributes();
          int typeIndex = innerAttributes.getIndex("type");
          if (typeIndex == -1 || innerAttributes.getURI(typeIndex) != "http://www.w3.org/2001/XMLSchema-instance")
          {
            continue;
          }
          unsigned int j, jMax = child2->getNumChildren();
          for (j = 0; j<jMax; ++j)
          {
            const XMLNode* child3 = &child2->getChild(j);
            std::string childName3 = child3->getName();
            if (childName3 == "start")
            {
              start = RenderPoint(*child3);
              startSet = true;

            }
            // add the basepoints and the endpoint
            else if (childName3 == "end")
            {
              end = RenderPoint(*child3);
              endSet = true;
            }
            else if (innerAttributes.getValue(typeIndex) == "CubicBezier"  && childName3 == "basePoint1")
            {
              bp1 = RenderPoint(*child3);
              bp1Set = true;
            }
            else if (innerAttributes.getValue(typeIndex) == "CubicBezier" && childName3 == "basePoint2")
            {
              bp2 = RenderPoint(*child3);
              bp2Set = true;
            }
          }
          if (!startSet || !endSet)
          {
            // skip this point
            // TODO this is an error
            continue;
          }
          if (pCurve->getNumElements() == 0)
          {
            // add the start point
            pCurve->addElement(&start);
          }
          else
          {
            // check if start is identical to lastEnd
            // if not, we have to start a new curve
            if (!(start == lastEnd))
            {
              if (pCurve->getNumElements() > 1)
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
              pCurve = new RenderCurve(renderns);
              // read the attributes
              pCurve->readAttributes(curveAttributes, ea);
              // we have to clear the start head on this
              // curve
              pCurve->setStartHead("none");
              pCurve->addElement(&start);
            }
          }
          if (innerAttributes.getValue(typeIndex) == "CubicBezier")
          {
            if (!bp1Set)
            {
              if (bp2Set)
              {
                // basepoint1 is the same as basepoint2
                bp1 = bp2;
              }
              else
              {
                bp1 = RenderPoint(renderns);
                bp1.setCoordinates(RelAbsVector((start.x().getAbsoluteValue() + end.x().getAbsoluteValue()) / 2.0, (start.x().getRelativeValue() + end.x().getRelativeValue()) / 2.0)
                  , RelAbsVector((start.y().getAbsoluteValue() + end.y().getAbsoluteValue()) / 2.0, (start.y().getRelativeValue() + end.y().getRelativeValue()) / 2.0)
                  , RelAbsVector((start.z().getAbsoluteValue() + end.z().getAbsoluteValue()) / 2.0, (start.z().getRelativeValue() + end.z().getRelativeValue()) / 2.0)
                  );
                bp1Set = true;
              }
            }
            if (!bp2Set)
            {
              // at this time bp1 has to be set 
              bp2 = bp1;
            }
            // add the cubic bezier element
            RenderCubicBezier* pBezier = new RenderCubicBezier(renderns);
            pBezier->setCoordinates(end.x(), end.y(), end.z());
            pBezier->setBasePoint1(bp1.x(), bp1.y(), bp1.z());
            pBezier->setBasePoint2(bp2.x(), bp2.y(), bp2.z());
            pCurve->addElement(pBezier);
            delete pBezier;
            lastEnd = end;
          }
          else
          {
            // add the end point
            pCurve->addElement(&end);
            lastEnd = end;
          }
        }
      }
      if (pCurve->getNumElements() > 1)
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
* Adds the text rendering attributes of the given RenderGroup object
* to the given XMLAttributes object.
*/
void RenderGroup::addTextAttributes(const RenderGroup& group, XMLAttributes& att)
{
  if (group.isSetFontSize())
  {
    std::ostringstream os;
    os << group.getFontSize();
    att.add("font-size", os.str());
  }
  if (group.isSetFontFamily())
  {
    att.add("font-family", group.mFontFamily);
  }
  switch (group.mFontStyle)
  {
  default:
  case Text::STYLE_UNSET:
    break;
  case Text::STYLE_NORMAL:
    att.add("font-style", "normal");
    break;
  case Text::STYLE_ITALIC:
    att.add("font-style", "italic");
    break;
  }
  switch (group.mFontStyle)
  {
  default:
  case Text::WEIGHT_UNSET:
    break;
  case Text::WEIGHT_NORMAL:
    att.add("font-weight", "normal");
    break;
  case Text::WEIGHT_BOLD:
    att.add("font-weight", "bold");
    break;
  }
  switch (group.mTextAnchor)
  {
  case Text::ANCHOR_START:
    att.add("text-anchor", "start");
    break;
  case Text::ANCHOR_END:
    att.add("text-anchor", "end");
    break;
  case Text::ANCHOR_MIDDLE:
    att.add("text-anchor", "middle");
    break;
  default:
  case Text::ANCHOR_UNSET:
    break;
  }
  switch (group.mVTextAnchor)
  {
  case Text::ANCHOR_TOP:
    att.add("vtext-anchor", "top");
    break;
  case Text::ANCHOR_BOTTOM:
    att.add("vtext-anchor", "bottom");
    break;
  case Text::ANCHOR_MIDDLE:
    att.add("vtext-anchor", "middle");
    break;
  case Text::ANCHOR_BASELINE:
    att.add("vtext-anchor", "baseline");
    break;
  default:
  case Text::ANCHOR_UNSET:
    break;
  }
  if (group.isSetStartHead())
  {
    att.add("startHead", group.mStartHead);
  }
  if (group.isSetEndHead())
  {
    att.add("endHead", group.mEndHead);
  }
}
/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new RenderGroup_t using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 */
LIBSBML_EXTERN
RenderGroup_t *
RenderGroup_create(unsigned int level,
                   unsigned int version,
                   unsigned int pkgVersion)
{
  return new RenderGroup(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this RenderGroup_t object.
 */
LIBSBML_EXTERN
RenderGroup_t*
RenderGroup_clone(const RenderGroup_t* rg)
{
  if (rg != NULL)
  {
    return static_cast<RenderGroup_t*>(rg->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this RenderGroup_t object.
 */
LIBSBML_EXTERN
void
RenderGroup_free(RenderGroup_t* rg)
{
  if (rg != NULL)
  {
    delete rg;
  }
}


/*
 * Returns the value of the "startHead" attribute of this RenderGroup_t.
 */
LIBSBML_EXTERN
char *
RenderGroup_getStartHead(const RenderGroup_t * rg)
{
  if (rg == NULL)
  {
    return NULL;
  }

  return rg->getStartHead().empty() ? NULL :
    safe_strdup(rg->getStartHead().c_str());
}


/*
 * Returns the value of the "endHead" attribute of this RenderGroup_t.
 */
LIBSBML_EXTERN
char *
RenderGroup_getEndHead(const RenderGroup_t * rg)
{
  if (rg == NULL)
  {
    return NULL;
  }

  return rg->getEndHead().empty() ? NULL :
    safe_strdup(rg->getEndHead().c_str());
}


/*
 * Returns the value of the "font-family" attribute of this RenderGroup_t.
 */
LIBSBML_EXTERN
char *
RenderGroup_getFontFamily(const RenderGroup_t * rg)
{
  if (rg == NULL)
  {
    return NULL;
  }

  return rg->getFontFamily().empty() ? NULL :
    safe_strdup(rg->getFontFamily().c_str());
}


/*
 * Returns the value of the "font-weight" attribute of this RenderGroup_t.
 */
LIBSBML_EXTERN
FontWeight_t
RenderGroup_getFontWeight(const RenderGroup_t * rg)
{
  if (rg == NULL)
  {
    return FONT_WEIGHT_INVALID;
  }

  return (FontWeight_t)(rg->getFontWeight());
}


/*
 * Returns the value of the "font-weight" attribute of this RenderGroup_t.
 */
LIBSBML_EXTERN
char *
RenderGroup_getFontWeightAsString(const RenderGroup_t * rg)
{
  return (char*)(FontWeight_toString((FontWeight_t)(rg->getFontWeight())));
}


/*
 * Returns the value of the "font-style" attribute of this RenderGroup_t.
 */
LIBSBML_EXTERN
FontStyle_t
RenderGroup_getFontStyle(const RenderGroup_t * rg)
{
  if (rg == NULL)
  {
    return FONT_STYLE_INVALID;
  }

  return (FontStyle_t)(rg->getFontStyle());
}


/*
 * Returns the value of the "font-style" attribute of this RenderGroup_t.
 */
LIBSBML_EXTERN
char *
RenderGroup_getFontStyleAsString(const RenderGroup_t * rg)
{
  return (char*)(FontStyle_toString((FontStyle_t)(rg->getFontStyle())));
}


/*
 * Returns the value of the "text-anchor" attribute of this RenderGroup_t.
 */
LIBSBML_EXTERN
HTextAnchor_t
RenderGroup_getTextAnchor(const RenderGroup_t * rg)
{
  if (rg == NULL)
  {
    return H_TEXTANCHOR_INVALID;
  }

  return (HTextAnchor_t)(rg->getTextAnchor());
}


/*
 * Returns the value of the "text-anchor" attribute of this RenderGroup_t.
 */
LIBSBML_EXTERN
char *
RenderGroup_getTextAnchorAsString(const RenderGroup_t * rg)
{
  return (char*)(HTextAnchor_toString((HTextAnchor_t)(rg->getTextAnchor())));
}


/*
 * Returns the value of the "vtext-anchor" attribute of this RenderGroup_t.
 */
LIBSBML_EXTERN
VTextAnchor_t
RenderGroup_getVTextAnchor(const RenderGroup_t * rg)
{
  if (rg == NULL)
  {
    return V_TEXTANCHOR_INVALID;
  }

  return (VTextAnchor_t)(rg->getVTextAnchor());
}


/** @cond doxygenLibsbmlInternal */
LIBSBML_EXTERN
VTextAnchor_t
RenderGroup_getVtextAnchor(const RenderGroup_t * rg)
{
  return RenderGroup_getVTextAnchor(rg);
}
/** @endcond */


/*
 * Returns the value of the "vtext-anchor" attribute of this RenderGroup_t.
 */
LIBSBML_EXTERN
char *
RenderGroup_getVTextAnchorAsString(const RenderGroup_t * rg)
{
  return (char*)(VTextAnchor_toString((VTextAnchor_t)(rg->getVTextAnchor())));
}

/** @cond doxygenLibsbmlInternal */
LIBSBML_EXTERN
char *
RenderGroup_getVtextAnchorAsString(const RenderGroup_t * rg)
{
  return RenderGroup_getVTextAnchorAsString(rg);
}
/** @endcond */


/*
 * Predicate returning @c 1 (true) if this RenderGroup_t's "startHead"
 * attribute is set.
 */
LIBSBML_EXTERN
int
RenderGroup_isSetStartHead(const RenderGroup_t * rg)
{
  return (rg != NULL) ? static_cast<int>(rg->isSetStartHead()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this RenderGroup_t's "endHead" attribute
 * is set.
 */
LIBSBML_EXTERN
int
RenderGroup_isSetEndHead(const RenderGroup_t * rg)
{
  return (rg != NULL) ? static_cast<int>(rg->isSetEndHead()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this RenderGroup_t's "font-family"
 * attribute is set.
 */
LIBSBML_EXTERN
int
RenderGroup_isSetFontFamily(const RenderGroup_t * rg)
{
  return (rg != NULL) ? static_cast<int>(rg->isSetFontFamily()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this RenderGroup_t's "font-weight"
 * attribute is set.
 */
LIBSBML_EXTERN
int
RenderGroup_isSetFontWeight(const RenderGroup_t * rg)
{
  return (rg != NULL) ? static_cast<int>(rg->isSetFontWeight()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this RenderGroup_t's "font-style"
 * attribute is set.
 */
LIBSBML_EXTERN
int
RenderGroup_isSetFontStyle(const RenderGroup_t * rg)
{
  return (rg != NULL) ? static_cast<int>(rg->isSetFontStyle()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this RenderGroup_t's "text-anchor"
 * attribute is set.
 */
LIBSBML_EXTERN
int
RenderGroup_isSetTextAnchor(const RenderGroup_t * rg)
{
  return (rg != NULL) ? static_cast<int>(rg->isSetTextAnchor()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this RenderGroup_t's "vtext-anchor"
 * attribute is set.
 */
LIBSBML_EXTERN
int
RenderGroup_isSetVTextAnchor(const RenderGroup_t * rg)
{
  return (rg != NULL) ? static_cast<int>(rg->isSetVTextAnchor()) : 0;
}

/** @cond doxygenLibsbmlInternal */
LIBSBML_EXTERN
int
RenderGroup_isSetVtextAnchor(const RenderGroup_t * rg)
{
  return RenderGroup_isSetVTextAnchor(rg);
}
/** @endcond */


/*
 * Sets the value of the "startHead" attribute of this RenderGroup_t.
 */
LIBSBML_EXTERN
int
RenderGroup_setStartHead(RenderGroup_t * rg, const char * startHead)
{
  return (rg != NULL) ? rg->setStartHead(startHead) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "endHead" attribute of this RenderGroup_t.
 */
LIBSBML_EXTERN
int
RenderGroup_setEndHead(RenderGroup_t * rg, const char * endHead)
{
  return (rg != NULL) ? rg->setEndHead(endHead) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "font-family" attribute of this RenderGroup_t.
 */
LIBSBML_EXTERN
int
RenderGroup_setFontFamily(RenderGroup_t * rg, const char * fontFamily)
{
  return (rg != NULL) ? rg->setFontFamily(fontFamily) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "font-weight" attribute of this RenderGroup_t.
 */
LIBSBML_EXTERN
int
RenderGroup_setFontWeight(RenderGroup_t * rg, FontWeight_t fontWeight)
{
  return (rg != NULL) ? rg->setFontWeight(fontWeight) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "font-weight" attribute of this RenderGroup_t.
 */
LIBSBML_EXTERN
int
RenderGroup_setFontWeightAsString(RenderGroup_t * rg, const char * fontWeight)
{
  return (rg != NULL) ? rg->setFontWeight(fontWeight): LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "font-style" attribute of this RenderGroup_t.
 */
LIBSBML_EXTERN
int
RenderGroup_setFontStyle(RenderGroup_t * rg, FontStyle_t fontStyle)
{
  return (rg != NULL) ? rg->setFontStyle(fontStyle) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "font-style" attribute of this RenderGroup_t.
 */
LIBSBML_EXTERN
int
RenderGroup_setFontStyleAsString(RenderGroup_t * rg, const char * fontStyle)
{
  return (rg != NULL) ? rg->setFontStyle(fontStyle): LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "text-anchor" attribute of this RenderGroup_t.
 */
LIBSBML_EXTERN
int
RenderGroup_setTextAnchor(RenderGroup_t * rg, HTextAnchor_t textAnchor)
{
  return (rg != NULL) ? rg->setTextAnchor(textAnchor) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "text-anchor" attribute of this RenderGroup_t.
 */
LIBSBML_EXTERN
int
RenderGroup_setTextAnchorAsString(RenderGroup_t * rg, const char * textAnchor)
{
  return (rg != NULL) ? rg->setTextAnchor(textAnchor): LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "vtext-anchor" attribute of this RenderGroup_t.
 */
LIBSBML_EXTERN
int
RenderGroup_setVTextAnchor(RenderGroup_t * rg, VTextAnchor_t vtextAnchor)
{
  return (rg != NULL) ? rg->setVTextAnchor(vtextAnchor) :
    LIBSBML_INVALID_OBJECT;
}


/** @cond doxygenLibsbmlInternal */
LIBSBML_EXTERN
int
RenderGroup_setVtextAnchor(RenderGroup_t * rg, VTextAnchor_t vtextAnchor)
{
  return RenderGroup_setVTextAnchor(rg, vtextAnchor);
}
/** @endcond */


/*
 * Sets the value of the "vtext-anchor" attribute of this RenderGroup_t.
 */
LIBSBML_EXTERN
int
RenderGroup_setVTextAnchorAsString(RenderGroup_t * rg,
                                   const char * vtextAnchor)
{
  return (rg != NULL) ? rg->setVTextAnchor(vtextAnchor):
    LIBSBML_INVALID_OBJECT;
}


/** @cond doxygenLibsbmlInternal */
LIBSBML_EXTERN
int
RenderGroup_setVtextAnchorAsString(RenderGroup_t * rg,
  const char * vtextAnchor)
{
  return RenderGroup_setVTextAnchorAsString(rg, vtextAnchor);
}
/** @endcond */


/*
 * Unsets the value of the "startHead" attribute of this RenderGroup_t.
 */
LIBSBML_EXTERN
int
RenderGroup_unsetStartHead(RenderGroup_t * rg)
{
  return (rg != NULL) ? rg->unsetStartHead() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "endHead" attribute of this RenderGroup_t.
 */
LIBSBML_EXTERN
int
RenderGroup_unsetEndHead(RenderGroup_t * rg)
{
  return (rg != NULL) ? rg->unsetEndHead() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "font-family" attribute of this RenderGroup_t.
 */
LIBSBML_EXTERN
int
RenderGroup_unsetFontFamily(RenderGroup_t * rg)
{
  return (rg != NULL) ? rg->unsetFontFamily() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "font-weight" attribute of this RenderGroup_t.
 */
LIBSBML_EXTERN
int
RenderGroup_unsetFontWeight(RenderGroup_t * rg)
{
  return (rg != NULL) ? rg->unsetFontWeight() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "font-style" attribute of this RenderGroup_t.
 */
LIBSBML_EXTERN
int
RenderGroup_unsetFontStyle(RenderGroup_t * rg)
{
  return (rg != NULL) ? rg->unsetFontStyle() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "text-anchor" attribute of this RenderGroup_t.
 */
LIBSBML_EXTERN
int
RenderGroup_unsetTextAnchor(RenderGroup_t * rg)
{
  return (rg != NULL) ? rg->unsetTextAnchor() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "vtext-anchor" attribute of this RenderGroup_t.
 */
LIBSBML_EXTERN
int
RenderGroup_unsetVTextAnchor(RenderGroup_t * rg)
{
  return (rg != NULL) ? rg->unsetVTextAnchor() : LIBSBML_INVALID_OBJECT;
}


/** @cond doxygenLibsbmlInternal */
LIBSBML_EXTERN
int
RenderGroup_unsetVtextAnchor(RenderGroup_t * rg)
{
  return RenderGroup_unsetVTextAnchor(rg);
}
/** @endcond */


/*
 * Returns the value of the "font-size" element of this RenderGroup_t.
 */
LIBSBML_EXTERN
const RelAbsVector_t*
RenderGroup_getFontSize(const RenderGroup_t * rg)
{
  if (rg == NULL)
  {
    return NULL;
  }

  return (RelAbsVector_t*)(&(rg->getFontSize()));
}


/*
 * Predicate returning @c 1 (true) if this RenderGroup_t's "font-size" element
 * is set.
 */
LIBSBML_EXTERN
int
RenderGroup_isSetFontSize(const RenderGroup_t * rg)
{
  return (rg != NULL) ? static_cast<int>(rg->isSetFontSize()) : 0;
}


/*
 * Sets the value of the "font-size" element of this RenderGroup_t.
 */
LIBSBML_EXTERN
int
RenderGroup_setFontSize(RenderGroup_t * rg, const RelAbsVector_t* fontSize)
{
  return (rg != NULL) ? rg->setFontSize(*fontSize) : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "font-size" element of this RenderGroup_t.
 */
LIBSBML_EXTERN
int
RenderGroup_unsetFontSize(RenderGroup_t * rg)
{
  return (rg != NULL) ? rg->unsetFontSize() : LIBSBML_INVALID_OBJECT;
}


/*
 * Returns a ListOf_t * containing Transformation2D_t objects from this
 * RenderGroup_t.
 */
LIBSBML_EXTERN
ListOf_t*
RenderGroup_getListOfElements(RenderGroup_t* rg)
{
  return (rg != NULL) ? rg->getListOfElements() : NULL;
}


/*
 * Get a Transformation2D_t from the RenderGroup_t.
 */
LIBSBML_EXTERN
Transformation2D_t*
RenderGroup_getElement(RenderGroup_t* rg, unsigned int n)
{
  return (rg != NULL) ? rg->getElement(n) : NULL;
}


/*
 * Adds a copy of the given Transformation2D_t to this RenderGroup_t.
 */
LIBSBML_EXTERN
int
RenderGroup_addElement(RenderGroup_t* rg, const Transformation2D_t* td)
{
  return (rg != NULL) ? rg->addElement(td) : LIBSBML_INVALID_OBJECT;
}


/*
 * Get the number of Transformation2D_t objects in this RenderGroup_t.
 */
LIBSBML_EXTERN
unsigned int
RenderGroup_getNumElements(RenderGroup_t* rg)
{
  return (rg != NULL) ? rg->getNumElements() : SBML_INT_MAX;
}


/*
 * Creates a new Image_t object, adds it to this RenderGroup_t object and
 * returns the Image_t object created.
 */
LIBSBML_EXTERN
Image_t*
RenderGroup_createImage(RenderGroup_t* rg)
{
  return (rg != NULL) ? rg->createImage() : NULL;
}


/*
 * Creates a new Ellipse_t object, adds it to this RenderGroup_t object and
 * returns the Ellipse_t object created.
 */
LIBSBML_EXTERN
Ellipse_t*
RenderGroup_createEllipse(RenderGroup_t* rg)
{
  return (rg != NULL) ? rg->createEllipse() : NULL;
}


/*
 * Creates a new Rectangle_t object, adds it to this RenderGroup_t object and
 * returns the Rectangle_t object created.
 */
LIBSBML_EXTERN
Rectangle_t*
RenderGroup_createRectangle(RenderGroup_t* rg)
{
  return (rg != NULL) ? rg->createRectangle() : NULL;
}


/*
 * Creates a new Polygon_t object, adds it to this RenderGroup_t object and
 * returns the Polygon_t object created.
 */
LIBSBML_EXTERN
Polygon_t*
RenderGroup_createPolygon(RenderGroup_t* rg)
{
  return (rg != NULL) ? rg->createPolygon() : NULL;
}


/*
 * Creates a new RenderGroup_t object, adds it to this RenderGroup_t object and
 * returns the RenderGroup_t object created.
 */
LIBSBML_EXTERN
RenderGroup_t*
RenderGroup_createGroup(RenderGroup_t* rg)
{
  return (rg != NULL) ? rg->createGroup() : NULL;
}


/*
 * Creates a new LineEnding_t object, adds it to this RenderGroup_t object and
 * returns the LineEnding_t object created.
 */
LIBSBML_EXTERN
LineEnding_t*
RenderGroup_createLineEnding(RenderGroup_t* rg)
{
  return (rg != NULL) ? rg->createLineEnding() : NULL;
}


/*
 * Creates a new Text_t object, adds it to this RenderGroup_t object and
 * returns the Text_t object created.
 */
LIBSBML_EXTERN
Text_t*
RenderGroup_createText(RenderGroup_t* rg)
{
  return (rg != NULL) ? rg->createText() : NULL;
}


/*
 * Creates a new RenderCurve_t object, adds it to this RenderGroup_t object and
 * returns the RenderCurve_t object created.
 */
LIBSBML_EXTERN
RenderCurve_t*
RenderGroup_createCurve(RenderGroup_t* rg)
{
  return (rg != NULL) ? rg->createCurve() : NULL;
}


/*
 * Removes the nth Transformation2D_t from this RenderGroup_t and returns a
 * pointer to it.
 */
LIBSBML_EXTERN
Transformation2D_t*
RenderGroup_removeElement(RenderGroup_t* rg, unsigned int n)
{
  return (rg != NULL) ? rg->removeElement(n) : NULL;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * RenderGroup_t object have been set.
 */
LIBSBML_EXTERN
int
RenderGroup_hasRequiredAttributes(const RenderGroup_t * rg)
{
  return (rg != NULL) ? static_cast<int>(rg->hasRequiredAttributes()) : 0;
}


/*
 * Predicate returning @c 1 (true) if all the required elements for this
 * RenderGroup_t object have been set.
 */
LIBSBML_EXTERN
int
RenderGroup_hasRequiredElements(const RenderGroup_t * rg)
{
  return (rg != NULL) ? static_cast<int>(rg->hasRequiredElements()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


