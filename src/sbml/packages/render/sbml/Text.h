/**
 * @file    Text.h
 * @brief Definition of the Text class.
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
 * ------------------------------------------------------------------------ -->
 *
 * @class Text
 * @sbmlbrief{render} Representation of text elements.
 *
 * The Text class represents text to be rendered in the context of a style.
 * The Text class inherits all attributes and methods from its base class
 * GraphicalPrimitive1D.
 *
 * The text also holds a string for the actual text that is to be rendered
 * for the Text object.
 *
 * Additional attributes specify how the text is to be rendered, for example
 * which font family is to be used and how the text is to be aligned within
 * the viewport.
 */

#ifndef Text_H__
#define Text_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/render/common/renderfwd.h>


#ifdef __cplusplus


#include <string>


#include <sbml/packages/render/sbml/GraphicalPrimitive1D.h>
#include <sbml/packages/render/extension/RenderExtension.h>
#include <sbml/packages/render/sbml/RelAbsVector.h>
#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLAttributes.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN Text : public GraphicalPrimitive1D
{
public:
  /** @cond doxygenLibsbmlInternal */
  enum FONT_WEIGHT
  {
    WEIGHT_UNSET,  /*!<The weight is not set, and may be anything.*/
    WEIGHT_NORMAL, /*!<The weight is 'normal', thinner and/or lighter than bold text.*/
    WEIGHT_BOLD,   /*!<The weight is 'bold', thicker and/or darker than normal text.*/
    WEIGHT_INVALID /*!<The weight is an unknown or invalid value.*/
  };
  /** @endcond */

  /** @cond doxygenLibsbmlInternal */
  enum FONT_STYLE
  {
    STYLE_UNSET,  /*!<The font style is not set, and may be anything. */
    STYLE_NORMAL, /*!<The font style is 'normal', or upright and not slanted. */
    STYLE_ITALIC, /*!<The font style is 'italic', or slanted. */
    STYLE_INVALID /*!<The font style is an unknown or invalid value. */
  };
  /** @endcond */

  /** @cond doxygenLibsbmlInternal */
  enum TEXT_ANCHOR
  {
    ANCHOR_UNSET=0,  /*!<The text anchor is unset. */
    ANCHOR_START=1,  /*!<The text anchor is "start": the start of the text is aligned to the horizontal center of the box.*/
    ANCHOR_MIDDLE=2, /*!<The text anchor is "middle": the horizontal center of the text is aligned with the horizontal center of the box. */
    ANCHOR_END=3,    /*!<The text anchor is "end": the end of the text is aligned with the horizontal center of the box. */
    ANCHOR_TOP=1,    /*!<The text anchor is "top": the top of the text is aligned with the vertical center of the box. */
    ANCHOR_BOTTOM=3, /*!<The text anchor is "bottom": the bottom of the text is aligned with the vertical center of the box. */
    ANCHOR_BASELINE=4, /*!<The text anchor is "baseline": the baseline of the text is aligned with the vertical center of the box. */
    ANCHOR_INVALID   /*!<The text anchor is an unknown or invalid value. */
  };
  /** @endcond */


protected:
  /** @cond doxygenLibsbmlInternal */
  RelAbsVector mX;
  RelAbsVector mY;
  RelAbsVector mZ;
  std::string mFontFamily; 
  RelAbsVector mFontSize;
  int mFontWeight; 
  int mFontStyle; 
  int mTextAnchor; 
  int mVTextAnchor; 
  std::string mText;

  /** @endcond */

public:

  /**
   * Creates a new Text using the given SBML Level, Version and
   * &ldquo;render&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this Text.
   *
   * @param version an unsigned int, the SBML Version to assign to this Text.
   *
   * @param pkgVersion an unsigned int, the SBML Render Version to assign to
   * this Text.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  Text(unsigned int level = RenderExtension::getDefaultLevel(),
       unsigned int version = RenderExtension::getDefaultVersion(),
       unsigned int pkgVersion = RenderExtension::getDefaultPackageVersion());


  /**
   * Creates a new Text using the given RenderPkgNamespaces object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param renderns the RenderPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  Text(RenderPkgNamespaces *renderns);


  /**
   * Creates a new Text object from the given XMLNode object.
   * The XMLNode object has to contain a valid XML representation of a 
   * Text object as defined in the render extension specification.
   * This method is normally called when render information is read from a file and 
   * should normally not have to be called explicitly.
   *
   * @param node the XMLNode object reference that describes the Text
   * object to be instantiated.
   *
   * @param l2version an integer indicating the version of SBML Level&nbsp;2
   */
  Text(const XMLNode& node, unsigned int l2version=4);


#ifndef OMIT_DEPRECATED
  /**
   * Instantiates a new Text object with the given @p id and position offset.
   * The position offset coordinates can be omitted and will be set to 0 in
   * that case.
   *
   * All attributes are set as described for the default constructor
   * of GraphicalPrimitive1D.
   * All the font rendering attributes as well 
   * as the text to be rendered are unset.
   *
   * @param renderns the SBMLNamespaces object for the SBML "render" package
   * @param id id string for the Text object
   * @param x x coordinate of the position offset
   * @param y y coordinate of the position offset
   * @param z z coordinate of the position offset
   *
   * @copydetails doc_warning_deprecated_constructor
   */
  Text(RenderPkgNamespaces* renderns, const std::string& id,const RelAbsVector& x=RelAbsVector(0.0,0.0),const RelAbsVector& y=RelAbsVector(0.0,0.0),const RelAbsVector& z=RelAbsVector(0.0,0.0));
#endif // OMIT_DEPRECATED

  /**
   * Copy constructor for Text.
   *
   * @param orig the Text instance to copy.
   */
  Text(const Text& orig);


  /**
   * Assignment operator for Text.
   *
   * @param rhs the Text object whose values are to be used as the basis of the
   * assignment.
   */
  Text& operator=(const Text& rhs);


  /**
   * Creates and returns a deep copy of this Text object.
   *
   * @return a (deep) copy of this Text object.
   */
  virtual Text* clone() const;


  /**
   * Destructor for Text.
   */
  virtual ~Text();


  /**
   * Returns the value of the "font-family" attribute of this Text.
   *
   * @return the value of the "font-family" attribute of this Text as a string.
   */
  const std::string& getFontFamily() const;


  /**
   * Returns the value of the "font-weight" attribute of this Text.
   *
   * @copydetails doc_render_font_weight
   *
   * @return the value of the "font-weight" attribute of this Text object.
   * @if clike The value is drawn from the enumeration #FontWeight_t.@endif@~
   * The possible values returned by this method are:
   * @li @sbmlconstant{FONT_WEIGHT_BOLD, FontWeight_t}
   * @li @sbmlconstant{FONT_WEIGHT_NORMAL, FontWeight_t}
   */
  int getFontWeight() const;


  /**
   * Returns the value of the "font-weight" attribute of this Text.
   *
   * @copydetails doc_render_font_weight
   *
   * @return the value of the "font-weight" attribute of this Text as a string.
   * The possible values returned by this method are:
   * @li @c "bold"
   * @li @c "normal"
   * @li @c "(Unknown FontWeight value)"
   */
  std::string getFontWeightAsString() const;


  /**
   * Returns the value of the "font-style" attribute of this Text.
   *
   * @copydetails doc_render_font_style
   *
   * @return the value of the "font-style" attribute of this Text object.
   * @if clike The value is drawn from the enumeration #FontStyle_t.@endif@~
   * The possible values returned by this method are:
   * @li @sbmlconstant{FONT_STYLE_ITALIC, FontStyle_t}
   * @li @sbmlconstant{FONT_STYLE_NORMAL, FontStyle_t}
   */
  int getFontStyle() const;


  /**
   * Returns the value of the "font-style" attribute of this Text.
   *
   * @copydetails doc_render_font_style
   *
   * @return the value of the "font-style" attribute of this Text as a string.
   * The possible values returned by this method are:
   * @li @c "italic"
   * @li @c "normal"
   * @li @c "(Unknown FontStyle value)"
   */
  std::string getFontStyleAsString() const;


  /**
   * Returns the value of the "text-anchor" attribute of this Text.
   *
   * @copydetails doc_render_text_anchor
   *
   * @return the value of the "text-anchor" attribute of this Text object.
   * @if clike The value is drawn from the enumeration #HTextAnchor_t.
   * @endif@~ The possible values returned by this method are:
   * @li @sbmlconstant{H_TEXTANCHOR_START, HTextAnchor_t}
   * @li @sbmlconstant{H_TEXTANCHOR_MIDDLE, HTextAnchor_t}
   * @li @sbmlconstant{H_TEXTANCHOR_END, HTextAnchor_t}
   */
  int getTextAnchor() const;


  /**
   * Returns the value of the "text-anchor" attribute of this Text.
   *
   * @copydetails doc_render_text_anchor
   *
   * @return the value of the "text-anchor" attribute of this Text as a string.
   * The possible values returned by this method are:
   * @li @c "start"
   * @li @c "middle"
   * @li @c "end"
   * @li @c "(Unknown HTextAnchor value)"
   */
  std::string getTextAnchorAsString() const;


  /**
   * Returns the value of the "vtext-anchor" attribute of this Text.
   *
   * @copydetails doc_render_vtext_anchor
   *
   * @return the value of the "vtext-anchor" attribute of this Text object.
   * @if clike The value is drawn from the enumeration #VTextAnchor_t.
   * @endif@~ The possible values returned by this method are:
   * @li @sbmlconstant{V_TEXTANCHOR_TOP, VTextAnchor_t}
   * @li @sbmlconstant{V_TEXTANCHOR_MIDDLE, VTextAnchor_t}
   * @li @sbmlconstant{V_TEXTANCHOR_BOTTOM, VTextAnchor_t}
   * @li @sbmlconstant{V_TEXTANCHOR_BASELINE, VTextAnchor_t}
   */
  int getVTextAnchor() const;


  /**
   * Returns the value of the "vtext-anchor" attribute of this Text.
   *
   * @copydetails doc_render_vtext_anchor
   *
   * @return the value of the "vtext-anchor" attribute of this Text as a
   * string.
   * The possible values returned by this method are:
   * @li @c "top"
   * @li @c "middle"
   * @li @c "bottom"
   * @li @c "baseline"
   * @li @c "(Unknown VTextAnchor value)"
   */
  std::string getVTextAnchorAsString() const;


  /**
   * Predicate returning @c true if this Text's "font-family" attribute is set.
   *
   * @return @c true if this Text's "font-family" attribute has been set,
   * otherwise @c false is returned.
   */
  bool isSetFontFamily() const;


  /**
   * Predicate returning @c true if this Text's "font-weight" attribute is set.
   *
   * @copydetails doc_render_font_weight
   *
   * @return @c true if this Text's "font-weight" attribute has been set,
   * otherwise @c false is returned.
   */
  bool isSetFontWeight() const;


  /**
   * Predicate returning @c true if this Text's "font-style" attribute is set.
   *
   * @copydetails doc_render_font_style
   *
   * @return @c true if this Text's "font-style" attribute has been set,
   * otherwise @c false is returned.
   */
  bool isSetFontStyle() const;


  /**
   * Predicate returning @c true if this Text's "text-anchor" attribute is set.
   *
   * @copydetails doc_render_text_anchor
   *
   * @return @c true if this Text's "text-anchor" attribute has been set,
   * otherwise @c false is returned.
   */
  bool isSetTextAnchor() const;


  /**
   * Predicate returning @c true if this Text's "vtext-anchor" attribute is
   * set.
   *
   * @copydetails doc_render_vtext_anchor
   *
   * @return @c true if this Text's "vtext-anchor" attribute has been set,
   * otherwise @c false is returned.
   */
  bool isSetVTextAnchor() const;


  /**
   * Sets the value of the "font-family" attribute of this Text.
   *
   * @param fontFamily std::string& value of the "font-family" attribute to be
   * set.
   * Calling this function with @p fontFamily = @c NULL or an empty string is
   * equivalent to calling unsetFontFamily().
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int setFontFamily(const std::string& fontFamily);


  /**
   * Sets the value of the "font-weight" attribute of this Text.
   *
   * @copydetails doc_render_font_weight
   *
   * @param fontWeight @if clike #FontWeight_t@else int@endif@~ value of the
   * "font-weight" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setFontWeight(const FontWeight_t fontWeight);

  /** @cond doxygenLibsbmlInternal */

  void setFontWeight(Text::FONT_WEIGHT weight);

  /** @endcond */


  /**
   * Sets the value of the "font-weight" attribute of this Text.
   *
   * @copydetails doc_render_font_weight
   *
   * @param fontWeight the value of the "font-weight" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setFontWeight(const std::string& fontWeight);


  /**
   * Sets the value of the "font-style" attribute of this Text.
   *
   * @copydetails doc_render_font_style
   *
   * @param fontStyle @if clike #FontStyle_t@else int@endif@~ value of the
   * "font-style" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setFontStyle(const FontStyle_t fontStyle);


  /** @cond doxygenLibsbmlInternal */

  void setFontStyle(Text::FONT_STYLE style);

  /** @endcond */
  /**
   * Sets the value of the "font-style" attribute of this Text.
   *
   * @copydetails doc_render_font_style
   *
   * @param fontStyle the "font-style" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setFontStyle(const std::string& fontStyle);


  /**
   * Sets the value of the "text-anchor" attribute of this Text.
   *
   * @copydetails doc_render_text_anchor
   *
   * @param textAnchor @if clike #HTextAnchor_t@else int@endif@~ value of the
   * "text-anchor" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setTextAnchor(const HTextAnchor_t textAnchor);


  /** @cond doxygenLibsbmlInternal */

  void setTextAnchor(Text::TEXT_ANCHOR anchor);

  /** @endcond */
  
  /**
   * Sets the value of the "text-anchor" attribute of this Text.
   *
   * @copydetails doc_render_text_anchor
   *
   * @param textAnchor the value of the "text-anchor" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setTextAnchor(const std::string& textAnchor);


  /**
   * Sets the value of the "vtext-anchor" attribute of this Text.
   *
   * @copydetails doc_render_vtext_anchor
   *
   * @param vtextAnchor @if clike #VTextAnchor_t@else int@endif@~ value of the
   * "vtext-anchor" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setVTextAnchor(const VTextAnchor_t vtextAnchor);


  /** @cond doxygenLibsbmlInternal */

  void setVTextAnchor(Text::TEXT_ANCHOR anchor);

  /** @endcond */

  /**
   * Sets the value of the "vtext-anchor" attribute of this Text.
   *
   * @copydetails doc_render_vtext_anchor
   *
   * @param vtextAnchor the value of the "vtext-anchor" attribute to be
   * set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setVTextAnchor(const std::string& vtextAnchor);


  /**
   * Unsets the value of the "font-family" attribute of this Text.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetFontFamily();


  /**
   * Unsets the value of the "font-weight" attribute of this Text.
   *
   * @copydetails doc_render_font_weight
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int unsetFontWeight();


  /**
   * Unsets the value of the "font-style" attribute of this Text.
   *
   * @copydetails doc_render_font_style
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int unsetFontStyle();


  /**
   * Unsets the value of the "text-anchor" attribute of this Text.
   *
   * @copydetails doc_render_text_anchor
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int unsetTextAnchor();


  /**
   * Unsets the value of the "vtext-anchor" attribute of this Text.
   *
   * @copydetails doc_render_vtext_anchor
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int unsetVTextAnchor();


  /**
   * Returns the x position offset as a const reference.
   * This offset is applied after alignment.
   *
   * @return const reference of x position offset
   */
  const RelAbsVector& getX() const;


  /**
   * Returns the x position offset as a reference.
   * This offset is applied after alignment.
   *
   * @return reference of x position offset
   */
  RelAbsVector& getX();


  /**
   * Returns the y position offset as a const reference.
   * This offset is applied after alignment.
   *
   * @return const reference of y position offset
   */
  const RelAbsVector& getY() const;


  /**
   * Returns the y position offset as a reference.
   * This offset is applied after alignment.
   *
   * @return reference of y position offset
   */
  RelAbsVector& getY();


  /**
   * Returns the z position offset as a const reference.
   * This offset is applied after alignment.
   *
   * @return const reference of z position offset
   */
  const RelAbsVector& getZ() const;


  /**
   * Returns the z position offset as a reference.
   * This offset is applied after alignment.
   *
   * @return reference of z position offset
   */
  RelAbsVector& getZ();


  /**
   * Returns the font size as a const reference.
   *
   * @return const reference to the size to be used for rendering text.
   */
  const RelAbsVector& getFontSize() const;


  /**
   * Returns the font size as a reference.
   *
   * @return A reference to the size to be used for rendering text.
   */
  RelAbsVector& getFontSize();


  /**
   * Predicate returning @c true if this Text's "x" element is set.
   *
   * @return @c true if this Text's "x" element has been set, otherwise
   * @c false is returned.
   */
  bool isSetX() const;


  /**
   * Predicate returning @c true if this Text's "y" element is set.
   *
   * @return @c true if this Text's "y" element has been set, otherwise
   * @c false is returned.
   */
  bool isSetY() const;


  /**
   * Predicate returning @c true if this Text's "z" element is set.
   *
   * @return @c true if this Text's "z" element has been set, otherwise
   * @c false is returned.
   */
  bool isSetZ() const;


  /**
   * Predicate returning @c true if this Text's "font-size" element is set.
   *
   * @return @c true if this Text's "font-size" element has been set, otherwise
   * @c false is returned.
   */
  bool isSetFontSize() const;


  /**
   * Sets the position of the text within the viewport.
   * This is like an offset that is applied after alignment.
   * If the z coordinate is omitted, it is set to 0.
   *
   * @param x x coordinate of the position offset
   * @param y y coordinate of the position offset
   * @param z z coordinate of the position offset
   */
  void setCoordinates(const RelAbsVector& x,const RelAbsVector& y,const RelAbsVector& z=RelAbsVector(0.0,0.0));

  /**
   * Sets the x position of the text within the viewport.
   * This is like an offset that is applied after alignment.
   *
   * @param x x coordinate of the position offset
   */
  int setX(const RelAbsVector& x);

  /**
   * Sets the y position of the text within the viewport.
   * This is like an offset that is applied after alignment.
   *
   * @param y y coordinate of the position offset
   */
  int setY(const RelAbsVector& y);

  /**
   * Sets the z position of the text within the viewport.
   * This is like an offset that is applied after alignment.
   *
   * @param z z coordinate of the position offset
   */
  int setZ(const RelAbsVector& z);

  /**
   * Sets the font size.
   * Normally this is an absolute value, e.g. 18 for a 18pt font.
   * It is however allowed the specify the font size in terms of relative values
   * in relation to the current viewport. In most cases the viewport will be the 
   * dimensions of a bounding box of a layout object.
   *
   * @param size the new font size.
   */
  int setFontSize(const RelAbsVector& size);

  /**
   * Unsets the value of the "x" element of this Text.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetX();


  /**
   * Unsets the value of the "y" element of this Text.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetY();


  /**
   * Unsets the value of the "z" element of this Text.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetZ();


  /**
   * Unsets the value of the "font-size" element of this Text.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetFontSize();


  /**
   * Returns the text for the Text object.
   *
   * @return the text string to be rendered for the Text object.
   */
  const std::string& getText() const;

  
  /**
   * Returns @c true if the text is set to something else than the empty string.
   *
   * @return @c true if the text is not empty.
   */
  bool isSetText() const;

  
  /**
   * Sets the text for the text element.
   *
   * @param text The text to be rendered for the Text object.
   */
  int setText(const std::string& text);

  
  /**
   * Unsets the text for the text element.
   */
  int unsetText();

  /**
   * Returns the XML element name of this Text object.
   *
   * For Text, the XML element name is always @c "text".
   *
   * @return the name of this element, i.e. @c "text".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this Text object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_RENDER_TEXT, SBMLRenderTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this Text
   * object have been set.
   *
   * @return @c true to indicate that all the required attributes of this Text
   * have been set, otherwise @c false is returned.
   */
  virtual bool hasRequiredAttributes() const;


  /** @cond doxygenLibsbmlInternal */

  /**
   * Accepts the given SBMLVisitor
   */
  virtual bool accept(SBMLVisitor& v) const;

  /** @endcond */
  /**
   * Creates an Text object from this Group object.
   *
   * @return the XMLNode with the XML representation for the 
   * Text object.
   */
  XMLNode toXML() const;


protected:


  /** @cond doxygenLibsbmlInternal */

  /**
   * Adds the expected attributes for this element
   */
  virtual void addExpectedAttributes(ExpectedAttributes& attributes);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Reads the expected attributes into the member data variables
   */
  virtual void readAttributes(const XMLAttributes& attributes,
                              const ExpectedAttributes& expectedAttributes);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Writes the attributes to the stream
   */
  virtual void writeAttributes(XMLOutputStream& stream) const;

  /** @endcond */

  /** @cond doxygenLibsbmlInternal */
  /** 
   * When overridden allows SBase elements to use the text included in between
   * the elements tags. The default implementation does nothing.
   * 
   * @param text the text string found between the element tags.
   */ 
  virtual void setElementText(const std::string &text);

  /** @cond doxygenLibsbmlInternal */
  /*
   * Writes (serializes) this SBML object by writing it to XMLOutputStream.
   */
  void write (XMLOutputStream& stream) const;
  /** @endcond */

  /**
   * Adds the text rendering attributes of the given Text object
   * to the given XMLAttributes object.
   */
  static void addTextAttributes(const Text& text,XMLAttributes& att);
  /** @endcond */


  /** @cond doxygenLibsbmlInternal */
  /**
   * Subclasses should override this method to write out their contained
   * SBML objects as XML elements.  Be sure to call your parents
   * implementation of this method as well.  For example:
   *
   *   SBase::writeElements(stream);
   *   mReactants.write(stream);
   *   mProducts.write(stream);
   *   ...
   */
  virtual void writeElements (XMLOutputStream& stream) const;
  /** @endcond */
};

LIBSBML_CPP_NAMESPACE_END

#endif /* __cplusplus */




#ifndef SWIG




LIBSBML_CPP_NAMESPACE_BEGIN




BEGIN_C_DECLS


/**
 * Creates a new Text_t using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this Text_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this Text_t.
 *
 * @param pkgVersion an unsigned int, the SBML Render Version to assign to this
 * Text_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof Text_t
 */
LIBSBML_EXTERN
Text_t *
Text_create(unsigned int level,
            unsigned int version,
            unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this Text_t object.
 *
 * @param t the Text_t structure.
 *
 * @return a (deep) copy of this Text_t object.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof Text_t
 */
LIBSBML_EXTERN
Text_t*
Text_clone(const Text_t* t);


/**
 * Frees this Text_t object.
 *
 * @param t the Text_t structure.
 *
 * @memberof Text_t
 */
LIBSBML_EXTERN
void
Text_free(Text_t* t);


/**
 * Returns the value of the "font-family" attribute of this Text_t.
 *
 * @param t the Text_t structure whose font-family is sought.
 *
 * @return the value of the "font-family" attribute of this Text_t as a pointer
 * to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof Text_t
 */
LIBSBML_EXTERN
char *
Text_getFontFamily(const Text_t * t);


/**
 * Returns the value of the "font-weight" attribute of this Text_t.
 *
 * @copydetails doc_render_font_weight
 *
 * @param t the Text_t structure whose font-weight is sought.
 *
 * @return the value of the "font-weight" attribute of this Text_t as a
 * #FontWeight_t.
 * The possible values returned by this method are:
 * @li @sbmlconstant{FONT_WEIGHT_UNSET, FontWeight_t}
 * @li @sbmlconstant{FONT_WEIGHT_BOLD, FontWeight_t}
 * @li @sbmlconstant{FONT_WEIGHT_NORMAL, FontWeight_t}
 * @li @sbmlconstant{FONT_WEIGHT_INVALID, FontWeight_t}
 *
 * @memberof Text_t
 */
LIBSBML_EXTERN
FontWeight_t
Text_getFontWeight(const Text_t * t);


/**
 * Returns the value of the "font-weight" attribute of this Text_t.
 *
 * @copydetails doc_render_font_weight
 *
 * @param t the Text_t structure whose font-weight is sought.
 *
 * @return the value of the "font-weight" attribute of this Text_t as a const
 * char *.
 * The possible values returned by this method are:
 * @li @c "bold"
 * @li @c "normal"
 * @li @c "(Unknown FontWeight value)"
 *
 * @copydetails doc_returned_unowned_char
 *
 * @memberof Text_t
 */
LIBSBML_EXTERN
char *
Text_getFontWeightAsString(const Text_t * t);


/**
 * Returns the value of the "font-style" attribute of this Text_t.
 *
 * @copydetails doc_render_font_style
 *
 * @param t the Text_t structure whose font-style is sought.
 *
 * @return the value of the "font-style" attribute of this Text_t as a
 * #FontStyle_t.
 * The possible values returned by this method are:
 * @li @sbmlconstant{FONT_STYLE_UNSET, FontStyle_t}
 * @li @sbmlconstant{FONT_STYLE_ITALIC, FontStyle_t}
 * @li @sbmlconstant{FONT_STYLE_NORMAL, FontStyle_t}
 * @li @sbmlconstant{FONT_STYLE_INVALID, FontStyle_t}
 *
 * @memberof Text_t
 */
LIBSBML_EXTERN
FontStyle_t
Text_getFontStyle(const Text_t * t);


/**
 * Returns the value of the "font-style" attribute of this Text_t.
 *
 * @copydetails doc_render_font_style
 *
 * @param t the Text_t structure whose font-style is sought.
 *
 * @return the value of the "font-style" attribute of this Text_t as a
 * <code>const char *</code>.
 * The possible values returned by this method are:
 * @li @c "italic"
 * @li @c "normal"
 * @li @c "(Unknown FontStyle value)"
 *
 * @copydetails doc_returned_unowned_char
 *
 * @memberof Text_t
 */
LIBSBML_EXTERN
char *
Text_getFontStyleAsString(const Text_t * t);


/**
 * Returns the value of the "text-anchor" attribute of this Text_t.
 *
 * @copydetails doc_render_text_anchor
 *
 * @param t the Text_t structure whose text-anchor is sought.
 *
 * @return the value of the "text-anchor" attribute of this Text_t as a
 * #HTextAnchor_t.
 * The possible values returned by this method are:
 * @li @sbmlconstant{H_TEXTANCHOR_UNSET, HTextAnchor_t}
 * @li @sbmlconstant{H_TEXTANCHOR_START, HTextAnchor_t}
 * @li @sbmlconstant{H_TEXTANCHOR_MIDDLE, HTextAnchor_t}
 * @li @sbmlconstant{H_TEXTANCHOR_END, HTextAnchor_t}
 * @li @sbmlconstant{H_TEXTANCHOR_INVALID, HTextAnchor_t}
 *
 * @memberof Text_t
 */
LIBSBML_EXTERN
HTextAnchor_t
Text_getTextAnchor(const Text_t * t);


/**
 * Returns the value of the "text-anchor" attribute of this Text_t.
 *
 * @copydetails doc_render_text_anchor
 *
 * @param t the Text_t structure whose text-anchor is sought.
 *
 * @return the value of the "text-anchor" attribute of this Text_t as a
 * <code>const char *</code>.
 * The possible values returned by this method are:
 * @li @c "start"
 * @li @c "middle"
 * @li @c "end"
 * @li @c "(Unknown HTextAnchor value)"
 *
 * @copydetails doc_returned_unowned_char
 *
 * @memberof Text_t
 */
LIBSBML_EXTERN
char *
Text_getTextAnchorAsString(const Text_t * t);


/**
 * Returns the value of the "vtext-anchor" attribute of this Text_t.
 *
 * @copydetails doc_render_vtext_anchor 
 *
 * @param t the Text_t structure whose vtext-anchor is sought.
 *
 * @return the value of the "vtext-anchor" attribute of this Text_t as a
 * #VTextAnchor_t.
 * The possible values returned by this method are:
 * @li @sbmlconstant{V_TEXTANCHOR_UNSET, VTextAnchor_t}
 * @li @sbmlconstant{V_TEXTANCHOR_TOP, VTextAnchor_t}
 * @li @sbmlconstant{V_TEXTANCHOR_MIDDLE, VTextAnchor_t}
 * @li @sbmlconstant{V_TEXTANCHOR_BOTTOM, VTextAnchor_t}
 * @li @sbmlconstant{V_TEXTANCHOR_BASELINE, VTextAnchor_t}
 * @li @sbmlconstant{V_TEXTANCHOR_INVALID, VTextAnchor_t}
 *
 * @memberof Text_t
 */
LIBSBML_EXTERN
VTextAnchor_t
Text_getVTextAnchor(const Text_t * t);


/**
 * Returns the value of the "vtext-anchor" attribute of this Text_t.
 *
 * @copydetails doc_render_vtext_anchor
 *
 * @param t the Text_t structure whose vtext-anchor is sought.
 *
 * @return the value of the "vtext-anchor" attribute of this Text_t as a const
 * char *.
 * The possible values returned by this method are:
 * @li @c "top"
 * @li @c "middle"
 * @li @c "bottom"
 * @li @c "baseline"
 * @li @c "(Unknown VTextAnchor value)"
 *
 * @copydetails doc_returned_unowned_char
 *
 * @memberof Text_t
 */
LIBSBML_EXTERN
char *
Text_getVTextAnchorAsString(const Text_t * t);


/**
 * Predicate returning @c 1 (true) if this Text_t's "font-family" attribute is
 * set.
 *
 * @param t the Text_t structure.
 *
 * @return @c 1 (true) if this Text_t's "font-family" attribute has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof Text_t
 */
LIBSBML_EXTERN
int
Text_isSetFontFamily(const Text_t * t);


/**
 * Predicate returning @c 1 (true) if this Text_t's "font-weight" attribute is
 * set.
 *
 * @copydetails doc_render_font_weight
 *
 * @param t the Text_t structure.
 *
 * @return @c 1 (true) if this Text_t's "font-weight" attribute has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof Text_t
 */
LIBSBML_EXTERN
int
Text_isSetFontWeight(const Text_t * t);


/**
 * Predicate returning @c 1 (true) if this Text_t's "font-style" attribute is
 * set.
 *
 * @copydetails doc_render_font_style
 *
 * @param t the Text_t structure.
 *
 * @return @c 1 (true) if this Text_t's "font-style" attribute has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof Text_t
 */
LIBSBML_EXTERN
int
Text_isSetFontStyle(const Text_t * t);


/**
 * Predicate returning @c 1 (true) if this Text_t's "text-anchor" attribute is
 * set.
 *
 * @copydetails doc_render_text_anchor
 *
 * @param t the Text_t structure.
 *
 * @return @c 1 (true) if this Text_t's "text-anchor" attribute has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof Text_t
 */
LIBSBML_EXTERN
int
Text_isSetTextAnchor(const Text_t * t);


/**
 * Predicate returning @c 1 (true) if this Text_t's "vtext-anchor" attribute is
 * set.
 *
 * @copydetails doc_render_vtext_anchor
 *
 * @param t the Text_t structure.
 *
 * @return @c 1 (true) if this Text_t's "vtext-anchor" attribute has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof Text_t
 */
LIBSBML_EXTERN
int
Text_isSetVTextAnchor(const Text_t * t);


/**
 * Sets the value of the "font-family" attribute of this Text_t.
 *
 * @param t the Text_t structure.
 *
 * @param fontFamily const char * value of the "font-family" attribute to be
 * set.
 * Calling this function with @p fontFamily = @c NULL or an empty string is
 * equivalent to calling Text_unsetFontFamily().
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Text_t
 */
LIBSBML_EXTERN
int
Text_setFontFamily(Text_t * t, const char * fontFamily);


/**
 * Sets the value of the "font-weight" attribute of this Text_t.
 *
 * @copydetails doc_render_font_weight
 *
 * @param t the Text_t structure.
 *
 * @param fontWeight FontWeight_t value of the "font-weight" attribute to be
 * set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Text_t
 */
LIBSBML_EXTERN
int
Text_setFontWeight(Text_t * t, FontWeight_t fontWeight);


/**
 * Sets the value of the "font-weight" attribute of this Text_t.
 *
 * @copydetails doc_render_font_weight
 *
 * @param t the Text_t structure.
 *
 * @param fontWeight const char * of the "font-weight" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Text_t
 */
LIBSBML_EXTERN
int
Text_setFontWeightAsString(Text_t * t, const char * fontWeight);


/**
 * Sets the value of the "font-style" attribute of this Text_t.
 *
 * @copydetails doc_render_font_style
 *
 * @param t the Text_t structure.
 *
 * @param fontStyle #FontStyle_t value of the "font-style" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Text_t
 */
LIBSBML_EXTERN
int
Text_setFontStyle(Text_t * t, FontStyle_t fontStyle);


/**
 * Sets the value of the "font-style" attribute of this Text_t.
 *
 * @copydetails doc_render_font_style
 *
 * @param t the Text_t structure.
 *
 * @param fontStyle const char * of the "font-style" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Text_t
 */
LIBSBML_EXTERN
int
Text_setFontStyleAsString(Text_t * t, const char * fontStyle);


/**
 * Sets the value of the "text-anchor" attribute of this Text_t.
 *
 * @copydetails doc_render_text_anchor
 *
 * @param t the Text_t structure.
 *
 * @param textAnchor #HTextAnchor_t value of the "text-anchor" attribute to be
 * set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Text_t
 */
LIBSBML_EXTERN
int
Text_setTextAnchor(Text_t * t, HTextAnchor_t textAnchor);


/**
 * Sets the value of the "text-anchor" attribute of this Text_t.
 *
 * @copydetails doc_render_text_anchor
 *
 * @param t the Text_t structure.
 *
 * @param textAnchor const char * of the "text-anchor" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Text_t
 */
LIBSBML_EXTERN
int
Text_setTextAnchorAsString(Text_t * t, const char * textAnchor);


/**
 * Sets the value of the "vtext-anchor" attribute of this Text_t.
 *
 * @copydetails doc_render_vtext_anchor
 *
 * @param t the Text_t structure.
 *
 * @param vtextAnchor #VTextAnchor_t value of the "vtext-anchor" attribute to be
 * set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Text_t
 */
LIBSBML_EXTERN
int
Text_setVTextAnchor(Text_t * t, VTextAnchor_t vtextAnchor);


/**
 * Sets the value of the "vtext-anchor" attribute of this Text_t.
 *
 * @copydetails doc_render_vtext_anchor
 *
 * @param t the Text_t structure.
 *
 * @param vtextAnchor const char * of the "vtext-anchor" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Text_t
 */
LIBSBML_EXTERN
int
Text_setVTextAnchorAsString(Text_t * t, const char * vtextAnchor);


/**
 * Unsets the value of the "font-family" attribute of this Text_t.
 *
 * @param t the Text_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Text_t
 */
LIBSBML_EXTERN
int
Text_unsetFontFamily(Text_t * t);


/**
 * Unsets the value of the "font-weight" attribute of this Text_t.
 *
 * @copydetails doc_render_font_weight
 *
 * @param t the Text_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Text_t
 */
LIBSBML_EXTERN
int
Text_unsetFontWeight(Text_t * t);


/**
 * Unsets the value of the "font-style" attribute of this Text_t.
 *
 * @copydetails doc_render_font_style
 *
 * @param t the Text_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Text_t
 */
LIBSBML_EXTERN
int
Text_unsetFontStyle(Text_t * t);


/**
 * Unsets the value of the "text-anchor" attribute of this Text_t.
 *
 * @copydetails doc_render_text_anchor
 *
 * @param t the Text_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Text_t
 */
LIBSBML_EXTERN
int
Text_unsetTextAnchor(Text_t * t);


/**
 * Unsets the value of the "vtext-anchor" attribute of this Text_t.
 *
 * @copydetails doc_render_vtext_anchor
 *
 * @param t the Text_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Text_t
 */
LIBSBML_EXTERN
int
Text_unsetVTextAnchor(Text_t * t);


/**
 * Returns the value of the "x" element of this Text_t.
 *
 * @param t the Text_t structure whose x is sought.
 *
 * @return the value of the "x" element of this Text_t as a RelAbsVector_t.
 *
 * @memberof Text_t
 */
LIBSBML_EXTERN
RelAbsVector_t*
Text_getX(const Text_t * t);


/**
 * Returns the value of the "y" element of this Text_t.
 *
 * @param t the Text_t structure whose y is sought.
 *
 * @return the value of the "y" element of this Text_t as a RelAbsVector_t.
 *
 * @memberof Text_t
 */
LIBSBML_EXTERN
RelAbsVector_t*
Text_getY(const Text_t * t);


/**
 * Returns the value of the "z" element of this Text_t.
 *
 * @param t the Text_t structure whose z is sought.
 *
 * @return the value of the "z" element of this Text_t as a RelAbsVector_t.
 *
 * @memberof Text_t
 */
LIBSBML_EXTERN
RelAbsVector_t*
Text_getZ(const Text_t * t);


/**
 * Returns the value of the "font-size" element of this Text_t.
 *
 * @param t the Text_t structure whose font-size is sought.
 *
 * @return the value of the "font-size" element of this Text_t as a
 * RelAbsVector_t.
 *
 * @memberof Text_t
 */
LIBSBML_EXTERN
RelAbsVector_t*
Text_getFontSize(const Text_t * t);


/**
 * Predicate returning @c 1 (true) if this Text_t's "x" element is set.
 *
 * @param t the Text_t structure.
 *
 * @return @c 1 (true) if this Text_t's "x" element has been set, otherwise
 * @c 0 (false) is returned.
 *
 * @memberof Text_t
 */
LIBSBML_EXTERN
int
Text_isSetX(const Text_t * t);


/**
 * Predicate returning @c 1 (true) if this Text_t's "y" element is set.
 *
 * @param t the Text_t structure.
 *
 * @return @c 1 (true) if this Text_t's "y" element has been set, otherwise
 * @c 0 (false) is returned.
 *
 * @memberof Text_t
 */
LIBSBML_EXTERN
int
Text_isSetY(const Text_t * t);


/**
 * Predicate returning @c 1 (true) if this Text_t's "z" element is set.
 *
 * @param t the Text_t structure.
 *
 * @return @c 1 (true) if this Text_t's "z" element has been set, otherwise
 * @c 0 (false) is returned.
 *
 * @memberof Text_t
 */
LIBSBML_EXTERN
int
Text_isSetZ(const Text_t * t);


/**
 * Predicate returning @c 1 (true) if this Text_t's "font-size" element is set.
 *
 * @param t the Text_t structure.
 *
 * @return @c 1 (true) if this Text_t's "font-size" element has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof Text_t
 */
LIBSBML_EXTERN
int
Text_isSetFontSize(const Text_t * t);


/**
 * Sets the value of the "x" element of this Text_t.
 *
 * @param t the Text_t structure.
 *
 * @param x RelAbsVector_t value of the "x" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Text_t
 */
LIBSBML_EXTERN
int
Text_setX(Text_t * t, const RelAbsVector_t* x);


/**
 * Sets the value of the "y" element of this Text_t.
 *
 * @param t the Text_t structure.
 *
 * @param y RelAbsVector_t value of the "y" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Text_t
 */
LIBSBML_EXTERN
int
Text_setY(Text_t * t, const RelAbsVector_t* y);


/**
 * Sets the value of the "z" element of this Text_t.
 *
 * @param t the Text_t structure.
 *
 * @param z RelAbsVector_t value of the "z" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Text_t
 */
LIBSBML_EXTERN
int
Text_setZ(Text_t * t, const RelAbsVector_t* z);


/**
 * Sets the value of the "font-size" element of this Text_t.
 *
 * @param t the Text_t structure.
 *
 * @param fontSize RelAbsVector_t value of the "font-size" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Text_t
 */
LIBSBML_EXTERN
int
Text_setFontSize(Text_t * t, const RelAbsVector_t* fontSize);


/**
 * Unsets the value of the "x" element of this Text_t.
 *
 * @param t the Text_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Text_t
 */
LIBSBML_EXTERN
int
Text_unsetX(Text_t * t);


/**
 * Unsets the value of the "y" element of this Text_t.
 *
 * @param t the Text_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Text_t
 */
LIBSBML_EXTERN
int
Text_unsetY(Text_t * t);


/**
 * Unsets the value of the "z" element of this Text_t.
 *
 * @param t the Text_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Text_t
 */
LIBSBML_EXTERN
int
Text_unsetZ(Text_t * t);


/**
 * Unsets the value of the "font-size" element of this Text_t.
 *
 * @param t the Text_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof Text_t
 */
LIBSBML_EXTERN
int
Text_unsetFontSize(Text_t * t);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * Text_t object have been set.
 *
 * @param t the Text_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * Text_t have been set, otherwise @c 0 (false) is returned.
 *
 * @note The required attributes for the Text_t object are:
 * @li "x"
 * @li "y"
 *
 * @memberof Text_t
 */
LIBSBML_EXTERN
int
Text_hasRequiredAttributes(const Text_t * t);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */
LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS

LIBSBML_EXTERN
Text::TEXT_ANCHOR
TextAnchor_fromString(const char* str);

LIBSBML_EXTERN
const char *
TextAnchor_toString(Text::TEXT_ANCHOR anchor);

END_C_DECLS
LIBSBML_CPP_NAMESPACE_END


#endif /* Text_H__ */
