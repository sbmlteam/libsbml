/**
 * @file DefaultValues.h
 * @brief Definition of the DefaultValues class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
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
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 * 3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 * Pasadena, CA, USA
 *
 * Copyright (C) 2002-2005 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. Japan Science and Technology Agency, Japan
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation. A copy of the license agreement is provided in the
 * file named "LICENSE.txt" included with this software distribution and also
 * available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 *
 * @class DefaultValues
 * @sbmlbrief{render} Encoding of default values.
 *
 * The SBMl Render package originally (pre-SBML Level 3) specified default
 * values and inheritance in a similar fashion to the specification used by
 * SVG. However, in order to comply with the SBML development guidelines for
 * Level&nbsp;3 packages, the Render package specification introduced a new
 * class of objects, DefaultValues, to encode these values within a
 * model. The DefaultValues class objets can occur as a child of either the
 * ListOfGlobalRenderInformation or a ListOfLocalRenderInformation.
 *
 * The values from DefaultValues objects are to be taken as default source
 * for the values of any optional attribute that is not explicitly declared.
 * If an attribute has not been declared, either explicitly on an element or
 * using the DefaultValues class then software reading the XML may chose how
 * they handle the attribute.  Please see the SBML Level&nbsp;3 Render package
 * specification for more information.
 *
 * Note that the DefaultValues associated with a ListOfLocalRenderInformation
 * will override DefaultValues declared on the ListOfGlobalRenderInformation.
 */

#ifndef DefaultValues_H__
#define DefaultValues_H__


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/packages/render/common/renderfwd.h>
#include <sbml/packages/render/sbml/GradientBase.h>
#include <sbml/packages/render/sbml/GraphicalPrimitive2D.h>
#include <sbml/packages/render/sbml/Text.h>
#include <sbml/packages/render/sbml/RelAbsVector.h>


#ifdef __cplusplus


#include <string>


#include <sbml/SBase.h>
#include <sbml/packages/render/extension/RenderExtension.h>


LIBSBML_CPP_NAMESPACE_BEGIN


class LIBSBML_EXTERN DefaultValues : public SBase
{
protected:

  /** @cond doxygenLibsbmlInternal */

  std::string mBackgroundColor;
  int mSpreadMethod;
  RelAbsVector mLinearGradient_x1;
  RelAbsVector mLinearGradient_y1;
  RelAbsVector mLinearGradient_z1;
  RelAbsVector mLinearGradient_x2;
  RelAbsVector mLinearGradient_y2;
  RelAbsVector mLinearGradient_z2;
  RelAbsVector mRadialGradient_cx;
  RelAbsVector mRadialGradient_cy;
  RelAbsVector mRadialGradient_cz;
  RelAbsVector mRadialGradient_r;
  RelAbsVector mRadialGradient_fx;
  RelAbsVector mRadialGradient_fy;
  RelAbsVector mRadialGradient_fz;
  std::string mFill;
  int mFillRule;
  RelAbsVector mDefault_z;
  std::string mStroke;
  double mStrokeWidth;
  bool mIsSetStrokeWidth;
  std::string mFontFamily;
  RelAbsVector mFontSize;
  int mFontWeight;
  int mFontStyle;
  int mTextAnchor;
  int mVTextAnchor;
  std::string mStartHead;
  std::string mEndHead;
  bool mEnableRotationalMapping;
  bool mIsSetEnableRotationalMapping;

  /** @endcond */

public:

  /**
   * Creates a DefaultValues object using the given SBML Level, Version and
   * &ldquo;render&rdquo; package version.
   *
   * @param level an unsigned int, the SBML Level to assign to this
   * DefaultValues.
   *
   * @param version an unsigned int, the SBML Version to assign to this
   * DefaultValues.
   *
   * @param pkgVersion an unsigned int, the SBML Render Version to assign to
   * this DefaultValues.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  DefaultValues(unsigned int level = RenderExtension::getDefaultLevel(),
                unsigned int version = RenderExtension::getDefaultVersion(),
                unsigned int pkgVersion =
                  RenderExtension::getDefaultPackageVersion());


  /**
   * Creates a DefaultValues object using the given RenderPkgNamespaces
   * object.
   *
   * @copydetails doc_what_are_sbml_package_namespaces
   *
   * @param renderns the RenderPkgNamespaces object.
   *
   * @copydetails doc_note_setting_lv_pkg
   */
  DefaultValues(RenderPkgNamespaces *renderns);


  /**
   * Copy constructor for DefaultValues.
   *
   * @param orig the DefaultValues instance to copy.
   */
  DefaultValues(const DefaultValues& orig);


  /**
   * Assignment operator for DefaultValues.
   *
   * @param rhs the DefaultValues object whose values are to be used as the
   * basis of the assignment.
   */
  DefaultValues& operator=(const DefaultValues& rhs);


  /**
   * Creates and returns a deep copy of this DefaultValues object.
   *
   * @return a (deep) copy of this DefaultValues object.
   */
  virtual DefaultValues* clone() const;


  /**
   * Destructor for DefaultValues.
   */
  virtual ~DefaultValues();


  /**
   * Returns the value of the "backgroundColor" attribute of this
   * DefaultValues object.
   *
   * @return the value of the "backgroundColor" attribute of this
   * DefaultValues as a string.
   */
  const std::string& getBackgroundColor() const;


  /**
   * Returns the value of the "spreadMethod" attribute of this DefaultValues
   * object.
   *
   * @return the value of the "spreadMethod" attribute of this DefaultValues
   * object as a GradientBase::SPREADMETHOD.
   *
   */
  GradientSpreadMethod_t getSpreadMethod() const;


  /**
   * Returns the value of the "spreadMethod" attribute of this DefaultValues
   * object.
   *
   * @return the value of the "spreadMethod" attribute of this DefaultValues
   * object as a string.
   *
   */
  std::string getSpreadMethodAsString() const;


  /**
   * Returns the value of the "linearGradient_x1" attribute of this
   * DefaultValues object.
   *
   * @return the value of the "linearGradient_x1" attribute of this
   * DefaultValues object as a string.
   */
  const RelAbsVector& getLinearGradient_x1() const;


  /**
   * Returns the value of the "linearGradient_y1" attribute of this
   * DefaultValues object.
   *
   * @return the value of the "linearGradient_y1" attribute of this
   * DefaultValues object as a string.
   */
  const RelAbsVector& getLinearGradient_y1() const;


  /**
   * Returns the value of the "linearGradient_z1" attribute of this
   * DefaultValues object.
   *
   * @return the value of the "linearGradient_z1" attribute of this
   * DefaultValues object as a string.
   */
  const RelAbsVector& getLinearGradient_z1() const;


  /**
   * Returns the value of the "linearGradient_x2" attribute of this
   * DefaultValues object.
   *
   * @return the value of the "linearGradient_x2" attribute of this
   * DefaultValues object as a string.
   */
  const RelAbsVector& getLinearGradient_x2() const;


  /**
   * Returns the value of the "linearGradient_y2" attribute of this
   * DefaultValues object.
   *
   * @return the value of the "linearGradient_y2" attribute of this
   * DefaultValues object as a string.
   */
  const RelAbsVector& getLinearGradient_y2() const;


  /**
   * Returns the value of the "linearGradient_z2" attribute of this
   * DefaultValues object.
   *
   * @return the value of the "linearGradient_z2" attribute of this
   * DefaultValues object as a string.
   */
  const RelAbsVector& getLinearGradient_z2() const;


  /**
   * Returns the value of the "radialGradient_cx" attribute of this
   * DefaultValues object.
   *
   * @return the value of the "radialGradient_cx" attribute of this
   * DefaultValues object as a string.
   */
  const RelAbsVector& getRadialGradient_cx() const;


  /**
   * Returns the value of the "radialGradient_cy" attribute of this
   * DefaultValues object.
   *
   * @return the value of the "radialGradient_cy" attribute of this
   * DefaultValues object as a string.
   */
  const RelAbsVector& getRadialGradient_cy() const;


  /**
   * Returns the value of the "radialGradient_cz" attribute of this
   * DefaultValues object.
   *
   * @return the value of the "radialGradient_cz" attribute of this
   * DefaultValues object as a string.
   */
  const RelAbsVector& getRadialGradient_cz() const;


  /**
   * Returns the value of the "radialGradient_r" attribute of this
   * DefaultValues object.
   *
   * @return the value of the "radialGradient_r" attribute of this
   * DefaultValues object as a string.
   */
  const RelAbsVector& getRadialGradient_r() const;


  /**
   * Returns the value of the "radialGradient_fx" attribute of this
   * DefaultValues object.
   *
   * @return the value of the "radialGradient_fx" attribute of this
   * DefaultValues object as a string.
   */
  const RelAbsVector& getRadialGradient_fx() const;


  /**
   * Returns the value of the "radialGradient_fy" attribute of this
   * DefaultValues object.
   *
   * @return the value of the "radialGradient_fy" attribute of this
   * DefaultValues object as a string.
   */
  const RelAbsVector& getRadialGradient_fy() const;


  /**
   * Returns the value of the "radialGradient_fz" attribute of this
   * DefaultValues object.
   *
   * @return the value of the "radialGradient_fz" attribute of this
   * DefaultValues object as a string.
   */
  const RelAbsVector& getRadialGradient_fz() const;


  /**
   * Returns the value of the "fill" attribute of this DefaultValues object.
   *
   * @return the value of the "fill" attribute of this DefaultValues object
   * as a string.
   */
  const std::string& getFill() const;


  /**
   * Returns the value of the "fill-rule" attribute of this DefaultValues object.
   *
   * @copydetails doc_render_fill_rule
   *
   * @return the value of the "fill-rule" attribute of this DefaultValues
   * object as @if clike a #FillRule_t @else an int@endif@~ value.
   */
  int getFillRule() const;


  /**
   * Returns the value of the "fill-rule" attribute of this DefaultValues object.
   *
   * @copydetails doc_render_fill_rule
   *
   * @return the value of the "fill-rule" attribute of this DefaultValues
   * object as a string.
   */
  std::string getFillRuleAsString() const;


  /**
   * Returns the value of the "default_z" attribute of this DefaultValues object.
   *
   * @return the value of the "default_z" attribute of this DefaultValues
   * object as a string.
   */
  const RelAbsVector& getDefault_z() const;


  /**
   * Returns the value of the "stroke" attribute of this DefaultValues object.
   *
   * @return the value of the "stroke" attribute of this DefaultValues object
   * as a string.
   */
  const std::string& getStroke() const;


  /**
   * Returns the value of the "stroke-width" attribute of this DefaultValues object.
   *
   * @return the value of the "stroke-width" attribute of this DefaultValues
   * object as a string.
   */
  double getStrokeWidth() const;


  /**
   * Returns the value of the "font-family" attribute of this DefaultValues object.
   *
   * @return the value of the "font-family" attribute of this DefaultValues
   * object as a string.
   */
  const std::string& getFontFamily() const;


  /**
   * Returns the value of the "font-size" attribute of this DefaultValues object.
   *
   * @return the value of the "font-size" attribute of this DefaultValues
   * object as a string.
   */
  const RelAbsVector& getFontSize() const;


  /**
   * Returns the value of the "font-weight" attribute of this DefaultValues object.
   *
   * @copydetails doc_render_font_weight
   *
   * @return the value of the "font-weight" attribute of this DefaultValues
   * object as a Text::FONT_WEIGHT.
   *
   */
  FontWeight_t getFontWeight() const;


  /**
   * Returns the value of the "font-weight" attribute of this DefaultValues object.
   *
   * @copydetails doc_render_font_weight
   *
   * @return the value of the "font-weight" attribute of this DefaultValues
   * object as a string.
   *
   */
  std::string getFontWeightAsString() const;


  /**
   * Returns the value of the "font-style" attribute of this DefaultValues
   * object.
   *
   * @copydetails doc_render_font_style
   *
   * @return the value of the "font-style" attribute of this DefaultValues
   * object as a Text::FONT_STYLE.
   *
   */
  FontStyle_t getFontStyle() const;


  /**
   * Returns the value of the "font-style" attribute of this DefaultValues
   * object.
   *
   * @copydetails doc_render_font_style
   *
   * @return the value of the "font-style" attribute of this DefaultValues
   * object as a string.
   *
   */
  std::string getFontStyleAsString() const;


  /**
   * Returns the value of the "text-anchor" attribute of this DefaultValues
   * object.
   *
   * @copydetails doc_render_text_anchor
   *
   * @return the value of the "text-anchor" attribute of this DefaultValues
   * object as a Text::TEXT_ANCHOR.
   *
   */
  HTextAnchor_t getTextAnchor() const;


  /**
   * Returns the value of the "text-anchor" attribute of this DefaultValues
   * object.
   *
   * @copydetails doc_render_text_anchor
   *
   * @return the value of the "text-anchor" attribute of this DefaultValues
   * object as a string.
   *
   */
  std::string getTextAnchorAsString() const;


  /**
   * Returns the value of the "vtext-anchor" attribute of this DefaultValues
   * object.
   *
   * @copydetails doc_render_vtext_anchor
   *
   * @return the value of the "vtext-anchor" attribute of this DefaultValues
   * object as a VTextAnchor_t.
   *
   */
  VTextAnchor_t getVTextAnchor() const;


  /**
   * Returns the value of the "vtext-anchor" attribute of this DefaultValues
   * object.
   *
   * @copydetails doc_render_vtext_anchor
   *
   * @return the value of the "vtext-anchor" attribute of this DefaultValues
   * object as a string.
   *
   */
  std::string getVTextAnchorAsString() const;


  /**
   * Returns the value of the "startHead" attribute of this DefaultValues
   * object.
   *
   * @return the value of the "startHead" attribute of this DefaultValues
   * object as a string.
   */
  const std::string& getStartHead() const;


  /**
   * Returns the value of the "endHead" attribute of this DefaultValues
   * object.
   *
   * @return the value of the "endHead" attribute of this DefaultValues
   * object as a string.
   */
  const std::string& getEndHead() const;


  /**
   * Returns the value of the "enableRotationalMapping" attribute of this
   * DefaultValues object.
   *
   * @return the value of the "enableRotationalMapping" attribute of this
   * DefaultValues object as a boolean.
   */
  bool getEnableRotationalMapping() const;


  /**
   * Predicate returning @c true if this DefaultValues object's
   * "backgroundColor" attribute is set.
   *
   * @return @c true if this DefaultValues object's "backgroundColor"
   * attribute has been set, otherwise @c false is returned.
   */
  bool isSetBackgroundColor() const;


  /**
   * Predicate returning @c true if this DefaultValues object's
   * "spreadMethod" attribute is set.
   *
   * @return @c true if this DefaultValues object's "spreadMethod" attribute
   * has been set, otherwise @c false is returned.
   *
   */
  bool isSetSpreadMethod() const;


  /**
   * Predicate returning @c true if this DefaultValues object's
   * "linearGradient_x1" attribute is set.
   *
   * @return @c true if this DefaultValues object's "linearGradient_x1"
   * attribute has been set, otherwise @c false is returned.
   */
  bool isSetLinearGradient_x1() const;


  /**
   * Predicate returning @c true if this DefaultValues object's
   * "linearGradient_y1" attribute is set.
   *
   * @return @c true if this DefaultValues object's "linearGradient_y1"
   * attribute has been set, otherwise @c false is returned.
   */
  bool isSetLinearGradient_y1() const;


  /**
   * Predicate returning @c true if this DefaultValues object's
   * "linearGradient_z1" attribute is set.
   *
   * @return @c true if this DefaultValues object's "linearGradient_z1"
   * attribute has been set, otherwise @c false is returned.
   */
  bool isSetLinearGradient_z1() const;


  /**
   * Predicate returning @c true if this DefaultValues object's
   * "linearGradient_x2" attribute is set.
   *
   * @return @c true if this DefaultValues object's "linearGradient_x2"
   * attribute has been set, otherwise @c false is returned.
   */
  bool isSetLinearGradient_x2() const;


  /**
   * Predicate returning @c true if this DefaultValues object's
   * "linearGradient_y2" attribute is set.
   *
   * @return @c true if this DefaultValues object's "linearGradient_y2"
   * attribute has been set, otherwise @c false is returned.
   */
  bool isSetLinearGradient_y2() const;


  /**
   * Predicate returning @c true if this DefaultValues object's
   * "linearGradient_z2" attribute is set.
   *
   * @return @c true if this DefaultValues object's "linearGradient_z2"
   * attribute has been set, otherwise @c false is returned.
   */
  bool isSetLinearGradient_z2() const;


  /**
   * Predicate returning @c true if this DefaultValues object's
   * "radialGradient_cx" attribute is set.
   *
   * @return @c true if this DefaultValues object's "radialGradient_cx"
   * attribute has been set, otherwise @c false is returned.
   */
  bool isSetRadialGradient_cx() const;


  /**
   * Predicate returning @c true if this DefaultValues object's
   * "radialGradient_cy" attribute is set.
   *
   * @return @c true if this DefaultValues object's "radialGradient_cy"
   * attribute has been set, otherwise @c false is returned.
   */
  bool isSetRadialGradient_cy() const;


  /**
   * Predicate returning @c true if this DefaultValues object's
   * "radialGradient_cz" attribute is set.
   *
   * @return @c true if this DefaultValues object's "radialGradient_cz"
   * attribute has been set, otherwise @c false is returned.
   */
  bool isSetRadialGradient_cz() const;


  /**
   * Predicate returning @c true if this DefaultValues object's
   * "radialGradient_r" attribute is set.
   *
   * @return @c true if this DefaultValues object's "radialGradient_r"
   * attribute has been set, otherwise @c false is returned.
   */
  bool isSetRadialGradient_r() const;


  /**
   * Predicate returning @c true if this DefaultValues object's
   * "radialGradient_fx" attribute is set.
   *
   * @return @c true if this DefaultValues object's "radialGradient_fx"
   * attribute has been set, otherwise @c false is returned.
   */
  bool isSetRadialGradient_fx() const;


  /**
   * Predicate returning @c true if this DefaultValues object's
   * "radialGradient_fy" attribute is set.
   *
   * @return @c true if this DefaultValues object's "radialGradient_fy"
   * attribute has been set, otherwise @c false is returned.
   */
  bool isSetRadialGradient_fy() const;


  /**
   * Predicate returning @c true if this DefaultValues object's
   * "radialGradient_fz" attribute is set.
   *
   * @return @c true if this DefaultValues object's "radialGradient_fz"
   * attribute has been set, otherwise @c false is returned.
   */
  bool isSetRadialGradient_fz() const;


  /**
   * Predicate returning @c true if this DefaultValues object's "fill"
   * attribute is set.
   *
   * @return @c true if this DefaultValues object's "fill" attribute has been
   * set, otherwise @c false is returned.
   */
  bool isSetFill() const;


  /**
   * Predicate returning @c true if this DefaultValues object's "fill-rule"
   * attribute is set.
   *
   * @copydetails doc_render_fill_rule
   *
   * @return @c true if this DefaultValues object's "fill-rule" attribute has
   * been set, otherwise @c false is returned.
   */
  bool isSetFillRule() const;


  /**
   * Predicate returning @c true if this DefaultValues object's "default_z"
   * attribute is set.
   *
   * @return @c true if this DefaultValues object's "default_z" attribute has
   * been set, otherwise @c false is returned.
   */
  bool isSetDefault_z() const;


  /**
   * Predicate returning @c true if this DefaultValues object's "stroke"
   * attribute is set.
   *
   * @return @c true if this DefaultValues object's "stroke" attribute has
   * been set, otherwise @c false is returned.
   */
  bool isSetStroke() const;


  /**
   * Predicate returning @c true if this DefaultValues object's "stroke-width"
   * attribute is set.
   *
   * @return @c true if this DefaultValues object's "stroke-width" attribute
   * has been set, otherwise @c false is returned.
   */
  bool isSetStrokeWidth() const;


  /**
   * Predicate returning @c true if this DefaultValues object's "font-family"
   * attribute is set.
   *
   * @return @c true if this DefaultValues object's "font-family" attribute
   * has been set, otherwise @c false is returned.
   */
  bool isSetFontFamily() const;


  /**
   * Predicate returning @c true if this DefaultValues object's "font-size"
   * attribute is set.
   *
   * @return @c true if this DefaultValues object's "font-size" attribute has
   * been set, otherwise @c false is returned.
   */
  bool isSetFontSize() const;


  /**
   * Predicate returning @c true if this DefaultValues object's "font-weight"
   * attribute is set.
   *
   * @copydetails doc_render_font_weight
   *
   * @return @c true if this DefaultValues object's "font-weight" attribute
   * has been set, otherwise @c false is returned.
   */
  bool isSetFontWeight() const;


  /**
   * Predicate returning @c true if this DefaultValues object's "font-style"
   * attribute is set.
   *
   * @copydetails doc_render_font_style
   *
   * @return @c true if this DefaultValues object's "font-style" attribute
   * has been set, otherwise @c false is returned.
   *
   */
  bool isSetFontStyle() const;


  /**
   * Predicate returning @c true if this DefaultValues object's "text-anchor"
   * attribute is set.
   *
   * @copydetails doc_render_text_anchor
   *
   * @return @c true if this DefaultValues object's "text-anchor" attribute
   * has been set, otherwise @c false is returned.
   *
   */
  bool isSetTextAnchor() const;


  /**
   * Predicate returning @c true if this DefaultValues object's
   * "vtext-anchor" attribute is set.
   *
   * @copydetails doc_render_vtext_anchor
   *
   * @return @c true if this DefaultValues object's "vtext-anchor" attribute
   * has been set, otherwise @c false is returned.
   *
   */
  bool isSetVTextAnchor() const;


  /**
   * Predicate returning @c true if this DefaultValues object's "startHead"
   * attribute is set.
   *
   * @return @c true if this DefaultValues object's "startHead" attribute has
   * been set, otherwise @c false is returned.
   */
  bool isSetStartHead() const;


  /**
   * Predicate returning @c true if this DefaultValues object's "endHead"
   * attribute is set.
   *
   * @return @c true if this DefaultValues object's "endHead" attribute has
   * been set, otherwise @c false is returned.
   */
  bool isSetEndHead() const;


  /**
   * Predicate returning @c true if this DefaultValues object's
   * "enableRotationalMapping" attribute is set.
   *
   * @return @c true if this DefaultValues object's "enableRotationalMapping"
   * attribute has been set, otherwise @c false is returned.
   */
  bool isSetEnableRotationalMapping() const;


  /**
   * Sets the value of the "backgroundColor" attribute of this DefaultValues
   * object.
   *
   * @param backgroundColor std::string& value of the "backgroundColor"
   * attribute to be set.
   *
   * Calling this function with @p backgroundColor = @c NULL or an empty
   * string is equivalent to calling unsetBackgroundColor().
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int setBackgroundColor(const std::string& backgroundColor);


  /**
   * Sets the value of the "spreadMethod" attribute of this DefaultValues
   * object.
   *
   * @param spreadMethod @if clike GradientBase::SPREADMETHOD@else
   * int@endif@~ value of the "spreadMethod" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   *
   */
  int setSpreadMethod(const GradientBase::SPREADMETHOD spreadMethod);


  /**
  * Sets the value of the "spreadMethod" attribute of this DefaultValues.
  *
  * @param spreadMethod @if clike GradientSpreadMethod_t@else int@endif@~ value
  * of the "spreadMethod" attribute to be set.
  *
  * @copydetails doc_returns_success_code
  * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
  * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
  * OperationReturnValues_t}
  *
  */
  int setSpreadMethod(const GradientSpreadMethod_t spreadMethod);


  /**
   * Sets the value of the "spreadMethod" attribute of this DefaultValues object.
   *
   * @param spreadMethod std::string& of the "spreadMethod" attribute to be
   * set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   *
   */
  int setSpreadMethod(const std::string& spreadMethod);


  /**
   * Sets the value of the "linearGradient_x1" attribute of this
   * DefaultValues object.
   *
   * @param linearGradient_x1 RelAbsVector& value of the "linearGradient_x1"
   * attribute to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int setLinearGradient_x1(const RelAbsVector& linearGradient_x1);


  /**
   * Sets the value of the "linearGradient_y1" attribute of this
   * DefaultValues object.
   *
   * @param linearGradient_y1 RelAbsVector& value of the "linearGradient_y1"
   * attribute to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int setLinearGradient_y1(const RelAbsVector& linearGradient_y1);


  /**
   * Sets the value of the "linearGradient_z1" attribute of this
   * DefaultValues object.
   *
   * @param linearGradient_z1 RelAbsVector& value of the "linearGradient_z1"
   * attribute to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int setLinearGradient_z1(const RelAbsVector& linearGradient_z1);


  /**
   * Sets the value of the "linearGradient_x2" attribute of this
   * DefaultValues object.
   *
   * @param linearGradient_x2 RelAbsVector& value of the "linearGradient_x2"
   * attribute to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int setLinearGradient_x2(const RelAbsVector& linearGradient_x2);


  /**
   * Sets the value of the "linearGradient_y2" attribute of this
   * DefaultValues object.
   *
   * @param linearGradient_y2 RelAbsVector& value of the "linearGradient_y2"
   * attribute to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int setLinearGradient_y2(const RelAbsVector& linearGradient_y2);


  /**
   * Sets the value of the "linearGradient_z2" attribute of this
   * DefaultValues object.
   *
   * @param linearGradient_z2 RelAbsVector& value of the "linearGradient_z2"
   * attribute to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int setLinearGradient_z2(const RelAbsVector& linearGradient_z2);


  /**
   * Sets the value of the "radialGradient_cx" attribute of this
   * DefaultValues object.
   *
   * @param radialGradient_cx RelAbsVector& value of the "radialGradient_cx"
   * attribute to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int setRadialGradient_cx(const RelAbsVector& radialGradient_cx);


  /**
   * Sets the value of the "radialGradient_cy" attribute of this
   * DefaultValues object.
   *
   * @param radialGradient_cy RelAbsVector& value of the "radialGradient_cy"
   * attribute to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int setRadialGradient_cy(const RelAbsVector& radialGradient_cy);


  /**
   * Sets the value of the "radialGradient_cz" attribute of this
   * DefaultValues object.
   *
   * @param radialGradient_cz RelAbsVector& value of the "radialGradient_cz"
   * attribute to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int setRadialGradient_cz(const RelAbsVector& radialGradient_cz);


  /**
   * Sets the value of the "radialGradient_r" attribute of this DefaultValues
   * object.
   *
   * @param radialGradient_r RelAbsVector& value of the "radialGradient_r"
   * attribute to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int setRadialGradient_r(const RelAbsVector& radialGradient_r);


  /**
   * Sets the value of the "radialGradient_fx" attribute of this
   * DefaultValues object.
   *
   * @param radialGradient_fx RelAbsVector& value of the "radialGradient_fx"
   * attribute to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int setRadialGradient_fx(const RelAbsVector& radialGradient_fx);


  /**
   * Sets the value of the "radialGradient_fy" attribute of this
   * DefaultValues object.
   *
   * @param radialGradient_fy RelAbsVector& value of the "radialGradient_fy"
   * attribute to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int setRadialGradient_fy(const RelAbsVector& radialGradient_fy);


  /**
   * Sets the value of the "radialGradient_fz" attribute of this
   * DefaultValues object.
   *
   * @param radialGradient_fz RelAbsVector& value of the "radialGradient_fz"
   * attribute to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int setRadialGradient_fz(const RelAbsVector& radialGradient_fz);


  /**
   * Sets the value of the "fill" attribute of this DefaultValues object.
   *
   * @param fill std::string& value of the "fill" attribute to be set.
   *
   * Calling this function with @p fill = @c NULL or an empty string is
   * equivalent to calling unsetFill().
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int setFill(const std::string& fill);


  /**
   * Sets the value of the "fill-rule" attribute of this DefaultValues
   * object.
   *
   * @param fillRule @if clike GraphicalPrimitive2D::FILL_RULE@else
   * int@endif@~ value of the "fill-rule" attribute to be set.
   *
   * @copydetails doc_render_fill_rule
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setFillRule(const GraphicalPrimitive2D::FILL_RULE fillRule);


  /**
  * Sets the value of the "fill-rule" attribute of this DefaultValues object.
  *
  * @copydetails doc_render_fill_rule
  *
  * @param fillRule @if clike GraphicalPrimitive2D::FILL_RULE@else
  * int@endif@~ value of the "fill-rule" attribute to be set.
  *
  * @copydetails doc_returns_success_code
  * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
  * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
  * OperationReturnValues_t}
  */
  int setFillRule(FillRule_t fillRule);


  /**
   * Sets the value of the "fill-rule" attribute of this DefaultValues
   * object.
   *
   * @copydetails doc_render_fill_rule
   *
   * @param fillRule std::string& of the "fill-rule" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setFillRule(const std::string& fillRule);


  /**
   * Sets the value of the "default_z" attribute of this DefaultValues
   * object.
   *
   * @param default_z RelAbsVector& value of the "default_z" attribute to be
   * set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int setDefault_z(const RelAbsVector& default_z);


  /**
   * Sets the value of the "stroke" attribute of this DefaultValues object.
   *
   * @param stroke std::string& value of the "stroke" attribute to be set.
   *
   * Calling this function with @p stroke = @c NULL or an empty string is
   * equivalent to calling unsetStroke().
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int setStroke(const std::string& stroke);


  /**
   * Sets the value of the "stroke-width" attribute of this DefaultValues
   * object.
   *
   * @param strokeWidth std::string& value of the "stroke-width" attribute to
   * be set.
   *
   * Calling this function with @p strokeWidth = @c NULL or an empty string is
   * equivalent to calling unsetStrokeWidth().
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int setStrokeWidth(double strokeWidth);


  /**
   * Sets the value of the "font-family" attribute of this DefaultValues
   * object.
   *
   * @param fontFamily std::string& value of the "font-family" attribute to
   * be set.
   *
   * Calling this function with @p fontFamily = @c NULL or an empty string is
   * equivalent to calling unsetFontFamily().
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int setFontFamily(const std::string& fontFamily);


  /**
   * Sets the value of the "font-size" attribute of this DefaultValues
   * object.
   *
   * @param fontSize RelAbsVector& value of the "font-size" attribute to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int setFontSize(const RelAbsVector& fontSize);


  /**
   * Sets the value of the "font-weight" attribute of this DefaultValues
   * object.
   *
   * @copydetails doc_render_font_weight
   *
   * @param fontWeight @if clike Text::FONT_WEIGHT@else int@endif@~ value of
   * the "font-weight" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setFontWeight(const Text::FONT_WEIGHT fontWeight);


  /**
  * Sets the value of the "font-weight" attribute of this DefaultValues.
  *
  * @copydetails doc_render_font_weight
  *
  * @param fontWeight the @if clike FontWeight_t@else int@endif@~ value of the
  * "font-weight" attribute to be set.
  *
  * @copydetails doc_returns_success_code
  * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
  * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
  * OperationReturnValues_t}
  */
  int setFontWeight(FontWeight_t fontWeight);


  /**
   * Sets the value of the "font-weight" attribute of this DefaultValues.
   *
   * @copydetails doc_render_font_weight
   *
   * @param fontWeight std::string& of the "font-weight" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setFontWeight(const std::string& fontWeight);


  /**
   * Sets the value of the "font-style" attribute of this DefaultValues
   * object.
   *
   * @copydetails doc_render_font_style
   *
   * @param fontStyle @if clike Text::FONT_STYLE@else int@endif@~ value of the
   * "font-style" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setFontStyle(const Text::FONT_STYLE fontStyle);


  /**
  * Sets the value of the "font-style" attribute of this DefaultValues.
  *
  * @copydetails doc_render_font_style
  *
  * @param fontStyle the @if clike FontWeight_t@else int@endif@~ value of the
  * "font-style" attribute to be set.
  *
  * @copydetails doc_returns_success_code
  * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
  * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
  * OperationReturnValues_t}
  */
  int setFontStyle(FontStyle_t fontStyle);


  /**
   * Sets the value of the "font-style" attribute of this DefaultValues.
   *
   * @copydetails doc_render_font_style
   *
   * @param fontStyle std::string& of the "font-style" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setFontStyle(const std::string& fontStyle);


  /**
   * Sets the value of the "text-anchor" attribute of this DefaultValues
   * object.
   *
   * @copydetails doc_render_text_anchor
   *
   * @param textAnchor @if clike Text::TEXT_ANCHOR@else int@endif@~ value of the
   * "text-anchor" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setTextAnchor(const Text::TEXT_ANCHOR textAnchor);


  /**
  * Sets the value of the "text-anchor" attribute of this DefaultValues.
  *
  * @copydetails doc_render_text_anchor
  *
  * @param textAnchor the @if clike  HTextAnchor_t@else int@endif@~ value of the
  * "text-anchor" attribute to be set.
  *
  * @copydetails doc_returns_success_code
  * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
  * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
  * OperationReturnValues_t}
  */
  int setTextAnchor(HTextAnchor_t textAnchor);


  /**
   * Sets the value of the "text-anchor" attribute of this DefaultValues.
   *
   * @copydetails doc_render_text_anchor
   *
   * @param textAnchor std::string& of the "text-anchor" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setTextAnchor(const std::string& textAnchor);


  /**
   * Sets the value of the "vtext-anchor" attribute of this DefaultValues
   * object.
   *
   * @copydetails doc_render_vtext_anchor
   *
   * @param vtextAnchor @if clike Text::TEXT_ANCHOR@else int@endif@~ value of the
   * "vtext-anchor" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setVTextAnchor(const Text::TEXT_ANCHOR vtextAnchor);


  /**
  * Sets the value of the "vtext-anchor" attribute of this DefaultValues.
  *
  * @copydetails doc_render_vtext_anchor
  *
  * @param vtextAnchor the @if clike VTextAnchor_t@else int@endif@~ value of the
  * "vtext-anchor" attribute to be set.
  *
  * @copydetails doc_returns_success_code
  * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
  * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
  * OperationReturnValues_t}
  */
  int setVTextAnchor(VTextAnchor_t vtextAnchor);


  /**
   * Sets the value of the "vtext-anchor" attribute of this DefaultValues.
   *
   * @copydetails doc_render_vtext_anchor
   *
   * @param vtextAnchor std::string& of the "vtext-anchor" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setVTextAnchor(const std::string& vtextAnchor);


  /**
   * Sets the value of the "startHead" attribute of this DefaultValues
   * object.
   *
   * @param startHead std::string& value of the "startHead" attribute to be
   * set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setStartHead(const std::string& startHead);


  /**
   * Sets the value of the "endHead" attribute of this DefaultValues object.
   *
   * @param endHead std::string& value of the "endHead" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setEndHead(const std::string& endHead);


  /**
   * Sets the value of the "enableRotationalMapping" attribute of this
   * DefaultValues object.
   *
   * @param enableRotationalMapping bool value of the
   * "enableRotationalMapping" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setEnableRotationalMapping(bool enableRotationalMapping);


  /**
   * Unsets the value of the "backgroundColor" attribute of this
   * DefaultValues object.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetBackgroundColor();


  /**
   * Unsets the value of the "spreadMethod" attribute of this DefaultValues
   * object.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int unsetSpreadMethod();


  /**
   * Unsets the value of the "linearGradient_x1" attribute of this
   * DefaultValues object.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetLinearGradient_x1();


  /**
   * Unsets the value of the "linearGradient_y1" attribute of this
   * DefaultValues object.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetLinearGradient_y1();


  /**
   * Unsets the value of the "linearGradient_z1" attribute of this
   * DefaultValues object.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetLinearGradient_z1();


  /**
   * Unsets the value of the "linearGradient_x2" attribute of this
   * DefaultValues object.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetLinearGradient_x2();


  /**
   * Unsets the value of the "linearGradient_y2" attribute of this
   * DefaultValues object.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetLinearGradient_y2();


  /**
   * Unsets the value of the "linearGradient_z2" attribute of this
   * DefaultValues object.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetLinearGradient_z2();


  /**
   * Unsets the value of the "radialGradient_cx" attribute of this
   * DefaultValues object.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetRadialGradient_cx();


  /**
   * Unsets the value of the "radialGradient_cy" attribute of this
   * DefaultValues object.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetRadialGradient_cy();


  /**
   * Unsets the value of the "radialGradient_cz" attribute of this
   * DefaultValues object.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetRadialGradient_cz();


  /**
   * Unsets the value of the "radialGradient_r" attribute of this
   * DefaultValues object.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetRadialGradient_r();


  /**
   * Unsets the value of the "radialGradient_fx" attribute of this
   * DefaultValues object.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetRadialGradient_fx();


  /**
   * Unsets the value of the "radialGradient_fy" attribute of this
   * DefaultValues object.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetRadialGradient_fy();


  /**
   * Unsets the value of the "radialGradient_fz" attribute of this
   * DefaultValues object.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetRadialGradient_fz();


  /**
   * Unsets the value of the "fill" attribute of this DefaultValues object.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetFill();


  /**
   * Unsets the value of the "fill-rule" attribute of this DefaultValues
   * object.
   *
   * @copydetails doc_render_fill_rule
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int unsetFillRule();


  /**
   * Unsets the value of the "default_z" attribute of this DefaultValues
   * object.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetDefault_z();


  /**
   * Unsets the value of the "stroke" attribute of this DefaultValues object.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetStroke();


  /**
   * Unsets the value of the "stroke-width" attribute of this DefaultValues
   * object.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetStrokeWidth();


  /**
   * Unsets the value of the "font-family" attribute of this DefaultValues
   * object.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetFontFamily();


  /**
   * Unsets the value of the "font-size" attribute of this DefaultValues
   * object.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetFontSize();


  /**
   * Unsets the value of the "font-weight" attribute of this DefaultValues
   * object.
   *
   * @copydetails doc_render_font_weight
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int unsetFontWeight();


  /**
   * Unsets the value of the "font-style" attribute of this DefaultValues
   * object.
   *
   * @copydetails doc_render_font_style
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int unsetFontStyle();


  /**
   * Unsets the value of the "text-anchor" attribute of this DefaultValues
   * object.
   *
   * @copydetails doc_render_text_anchor
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int unsetTextAnchor();


  /**
   * Unsets the value of the "vtext-anchor" attribute of this DefaultValues
   * object.
   *
   * @copydetails doc_render_vtext_anchor
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   */
  int unsetVTextAnchor();


  /**
   * Unsets the value of the "startHead" attribute of this DefaultValues
   * object.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetStartHead();


  /**
   * Unsets the value of the "endHead" attribute of this DefaultValues
   * object.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetEndHead();


  /**
   * Unsets the value of the "enableRotationalMapping" attribute of this
   * DefaultValues object.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetEnableRotationalMapping();


  /**
   * @copydoc doc_renamesidref_common
   */
  virtual void renameSIdRefs(const std::string& oldid,
                             const std::string& newid);


  /**
   * Returns the XML element name of this DefaultValues object.
   *
   * For DefaultValues object, the XML element name is always @c
   * "defaultValues".
   *
   * @return the name of this element, i.e. @c "defaultValues".
   */
  virtual const std::string& getElementName() const;


  /**
   * Returns the libSBML type code for this DefaultValues object.
   *
   * @copydetails doc_what_are_typecodes
   *
   * @return the SBML type code for this object:
   * @sbmlconstant{SBML_RENDER_DEFAULTS, SBMLRenderTypeCode_t}.
   *
   * @copydetails doc_warning_typecodes_not_unique
   *
   * @see getElementName()
   * @see getPackageName()
   */
  virtual int getTypeCode() const;


  /**
   * Predicate returning @c true if all the required attributes for this
   * DefaultValues object have been set.
   *
   * @return @c true to indicate that all the required attributes of this
   * DefaultValues object have been set, otherwise @c false is returned.
   */
  virtual bool hasRequiredAttributes() const;



  /** @cond doxygenLibsbmlInternal */

  /**
   * Write any contained elements
   */
  virtual void writeElements(XMLOutputStream& stream) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Accepts the given SBMLVisitor
   */
  virtual bool accept(SBMLVisitor& v) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the parent SBMLDocument
   */
  virtual void setSBMLDocument(SBMLDocument* d);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Enables/disables the given package with this element
   */
  virtual void enablePackageInternal(const std::string& pkgURI,
                                     const std::string& pkgPrefix,
                                     bool flag);

  /** @endcond */




  #ifndef SWIG



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the value of the "attributeName" attribute of this DefaultValues object.
   *
   * @param attributeName, the name of the attribute to retrieve.
   *
   * @param value, the address of the value to record.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int getAttribute(const std::string& attributeName, bool& value)
    const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the value of the "attributeName" attribute of this DefaultValues object.
   *
   * @param attributeName, the name of the attribute to retrieve.
   *
   * @param value, the address of the value to record.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int getAttribute(const std::string& attributeName, int& value) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the value of the "attributeName" attribute of this DefaultValues object.
   *
   * @param attributeName, the name of the attribute to retrieve.
   *
   * @param value, the address of the value to record.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int getAttribute(const std::string& attributeName,
                           double& value) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the value of the "attributeName" attribute of this DefaultValues object.
   *
   * @param attributeName, the name of the attribute to retrieve.
   *
   * @param value, the address of the value to record.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int getAttribute(const std::string& attributeName,
                           unsigned int& value) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Returns the value of the "attributeName" attribute of this DefaultValues object.
   *
   * @param attributeName, the name of the attribute to retrieve.
   *
   * @param value, the address of the value to record.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int getAttribute(const std::string& attributeName,
                           std::string& value) const;

  /** @endcond */

  /** @cond doxygenLibsbmlInternal */

  /**
   * Predicate returning @c true if this DefaultValues object's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this DefaultValues object's attribute "attributeName" has been
   * set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this DefaultValues object.
   *
   * @param attributeName, the name of the attribute to set.
   *
   * @param value, the value of the attribute to set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int setAttribute(const std::string& attributeName, bool value);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this DefaultValues object.
   *
   * @param attributeName, the name of the attribute to set.
   *
   * @param value, the value of the attribute to set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int setAttribute(const std::string& attributeName, int value);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this DefaultValues object.
   *
   * @param attributeName, the name of the attribute to set.
   *
   * @param value, the value of the attribute to set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int setAttribute(const std::string& attributeName, double value);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this DefaultValues object.
   *
   * @param attributeName, the name of the attribute to set.
   *
   * @param value, the value of the attribute to set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int setAttribute(const std::string& attributeName,
                           unsigned int value);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this DefaultValues object.
   *
   * @param attributeName, the name of the attribute to set.
   *
   * @param value, the value of the attribute to set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int setAttribute(const std::string& attributeName,
                           const std::string& value);

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Unsets the value of the "attributeName" attribute of this DefaultValues object.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  virtual int unsetAttribute(const std::string& attributeName);

  /** @endcond */




  #endif /* !SWIG */


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


};



LIBSBML_CPP_NAMESPACE_END




#endif /* __cplusplus */




#ifndef SWIG




LIBSBML_CPP_NAMESPACE_BEGIN




BEGIN_C_DECLS


/**
 * Creates a new DefaultValues_t using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 *
 * @param level an unsigned int, the SBML Level to assign to this
 * DefaultValues_t.
 *
 * @param version an unsigned int, the SBML Version to assign to this
 * DefaultValues_t.
 *
 * @param pkgVersion an unsigned int, the SBML Render Version to assign to this
 * DefaultValues_t.
 *
 * @copydetails doc_note_setting_lv_pkg
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
DefaultValues_t *
DefaultValues_create(unsigned int level,
                     unsigned int version,
                     unsigned int pkgVersion);


/**
 * Creates and returns a deep copy of this DefaultValues_t object.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @return a (deep) copy of this DefaultValues_t object.
 *
 * @copydetails doc_warning_returns_owned_pointer
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
DefaultValues_t*
DefaultValues_clone(const DefaultValues_t* dv);


/**
 * Frees this DefaultValues_t object.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
void
DefaultValues_free(DefaultValues_t* dv);


/**
 * Returns the value of the "backgroundColor" attribute of this
 * DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure whose backgroundColor is sought.
 *
 * @return the value of the "backgroundColor" attribute of this DefaultValues_t
 * as a pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
char *
DefaultValues_getBackgroundColor(const DefaultValues_t * dv);


/**
 * Returns the value of the "spreadMethod" attribute of this DefaultValues_t.
 *
 * @copydetails doc_render_spreadMethod
 *
 * @param dv the DefaultValues_t structure whose spreadMethod is sought.
 *
 * @return the value of the "spreadMethod" attribute of this DefaultValues_t as
 * a #GradientSpreadMethod_t.
 * The possible values returned by this method are:
 * @li @sbmlconstant{GRADIENT_SPREADMETHOD_PAD, GradientSpreadMethod_t}
 * @li @sbmlconstant{GRADIENT_SPREADMETHOD_REFLECT, GradientSpreadMethod_t}
 * @li @sbmlconstant{GRADIENT_SPREADMETHOD_REPEAT, GradientSpreadMethod_t}
 * @li @sbmlconstant{GRADIENT_SPREAD_METHOD_INVALID, GradientSpreadMethod_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
GradientSpreadMethod_t
DefaultValues_getSpreadMethod(const DefaultValues_t * dv);


/**
 * Returns the value of the "spreadMethod" attribute of this DefaultValues_t.
 *
 * @copydetails doc_render_spreadMethod
 *
 * @param dv the DefaultValues_t structure whose spreadMethod is sought.
 *
 * @return the value of the "spreadMethod" attribute of this DefaultValues_t as
 * a <code>const char *</code>*.
 * The possible values returned by this method are:
 * @li @c "pad"
 * @li @c "reflect"
 * @li @c "repeat"
 * @li @c "invalid DefaultValuesSpreadMethod"
 *
 * @copydetails doc_returned_unowned_char
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
char *
DefaultValues_getSpreadMethodAsString(const DefaultValues_t * dv);


/**
 * Returns the value of the "fill" attribute of this DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure whose fill is sought.
 *
 * @return the value of the "fill" attribute of this DefaultValues_t as a
 * pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
char *
DefaultValues_getFill(const DefaultValues_t * dv);


/**
 * Returns the value of the "fill-rule" attribute of this DefaultValues_t.
 *
 * @copydetails doc_render_fill_rule
 *
 * @param dv the DefaultValues_t structure whose fill-rule is sought.
 *
 * @return the value of the "fill-rule" attribute of this DefaultValues_t as a
 * #FillRule_t. The possible values returned by this method are:
 * @li @sbmlconstant{FILL_RULE_UNSET, FillRule_t}
 * @li @sbmlconstant{FILL_RULE_NONZERO, FillRule_t}
 * @li @sbmlconstant{FILL_RULE_EVENODD, FillRule_t}
 * @li @sbmlconstant{FILL_RULE_INHERIT, FillRule_t}
 * @li @sbmlconstant{FILL_RULE_INVALID, FillRule_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
FillRule_t
DefaultValues_getFillRule(const DefaultValues_t * dv);


/**
 * Returns the value of the "fill-rule" attribute of this DefaultValues_t.
 *
 * @copydetails doc_render_fill_rule
 *
 * @param dv the DefaultValues_t structure whose fill-rule is sought.
 *
 * @return the value of the "fill-rule" attribute of this DefaultValues_t as a
 * <code>const char *</code>. The possible values returned by this method are:
 * @li @c "unset"
 * @li @c "nonzero"
 * @li @c "evenodd"
 * @li @c "inherit"
 * @li @c "invalid DefaultValuesFill-rule"
 *
 * @copydetails doc_returned_unowned_char
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
char *
DefaultValues_getFillRuleAsString(const DefaultValues_t * dv);


/**
 * Returns the value of the "stroke" attribute of this DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure whose stroke is sought.
 *
 * @return the value of the "stroke" attribute of this DefaultValues_t as a
 * pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
char *
DefaultValues_getStroke(const DefaultValues_t * dv);


/**
 * Returns the value of the "stroke-width" attribute of this DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure whose stroke-width is sought.
 *
 * @return the value of the "stroke-width" attribute of this DefaultValues_t as
 * a double.
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
double
DefaultValues_getStrokeWidth(const DefaultValues_t * dv);


/**
 * Returns the value of the "font-family" attribute of this DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure whose font-family is sought.
 *
 * @return the value of the "font-family" attribute of this DefaultValues_t as
 * a pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
char *
DefaultValues_getFontFamily(const DefaultValues_t * dv);


/**
 * Returns the value of the "font-weight" attribute of this DefaultValues_t.
 *
 * @copydetails doc_render_font_weight
 *
 * @param dv the DefaultValues_t structure whose font-weight is sought.
 *
 * @return the value of the "font-weight" attribute of this DefaultValues_t as
 * a #FontWeight_t. The possible values returned by this method are:
 * @li @sbmlconstant{FONT_WEIGHT_UNSET, FontWeight_t}
 * @li @sbmlconstant{FONT_WEIGHT_BOLD, FontWeight_t}
 * @li @sbmlconstant{FONT_WEIGHT_NORMAL, FontWeight_t}
 * @li @sbmlconstant{FONT_WEIGHT_INVALID, FontWeight_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
FontWeight_t
DefaultValues_getFontWeight(const DefaultValues_t * dv);


/**
 * Returns the value of the "font-weight" attribute of this DefaultValues_t.
 *
 * @copydetails doc_render_font_weight
 *
 * @param dv the DefaultValues_t structure whose font-weight is sought.
 *
 * @return the value of the "font-weight" attribute of this DefaultValues_t as
 * a <code>const char *</code>. The possible values returned by this method are:
 * @li @c "bold"
 * @li @c "normal"
 * @li @c "invalid DefaultValuesFont-weight"
 *
 * @copydetails doc_returned_unowned_char
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
char *
DefaultValues_getFontWeightAsString(const DefaultValues_t * dv);


/**
 * Returns the value of the "font-style" attribute of this DefaultValues_t.
 *
 * @copydetails doc_render_font_style
 *
 * @param dv the DefaultValues_t structure whose font-style is sought.
 *
 * @return the value of the "font-style" attribute of this DefaultValues_t as a
 * #FontStyle_t. The possible values returned by this method are:
 * @li @sbmlconstant{FONT_STYLE_UNSET, FontStyle_t}
 * @li @sbmlconstant{FONT_STYLE_ITALIC, FontStyle_t}
 * @li @sbmlconstant{FONT_STYLE_NORMAL, FontStyle_t}
 * @li @sbmlconstant{FONT_STYLE_INVALID, FontStyle_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
FontStyle_t
DefaultValues_getFontStyle(const DefaultValues_t * dv);


/**
 * Returns the value of the "font-style" attribute of this DefaultValues_t.
 *
 * @copydetails doc_render_font_style
 *
 * @param dv the DefaultValues_t structure whose font-style is sought.
 *
 * @return the value of the "font-style" attribute of this DefaultValues_t as a
 * <code>const char *</code>. The possible values returned by this method are:
 * @li @c "italic"
 * @li @c "normal"
 * @li @c "invalid DefaultValuesFont-style"
 *
 * @copydetails doc_returned_unowned_char
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
char *
DefaultValues_getFontStyleAsString(const DefaultValues_t * dv);


/**
 * Returns the value of the "text-anchor" attribute of this DefaultValues_t.
 *
 * @copydetails doc_render_text_anchor
 *
 * @param dv the DefaultValues_t structure whose text-anchor is sought.
 *
 * @return the value of the "text-anchor" attribute of this DefaultValues_t as
 * a #HTextAnchor_t. The possible values returned by this method are:
 * @li @sbmlconstant{H_TEXTANCHOR_UNSET, HTextAnchor_t}
 * @li @sbmlconstant{H_TEXTANCHOR_START, HTextAnchor_t}
 * @li @sbmlconstant{H_TEXTANCHOR_MIDDLE, HTextAnchor_t}
 * @li @sbmlconstant{H_TEXTANCHOR_END, HTextAnchor_t}
 * @li @sbmlconstant{H_TEXTANCHOR_INVALID, HTextAnchor_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
HTextAnchor_t
DefaultValues_getTextAnchor(const DefaultValues_t * dv);


/**
 * Returns the value of the "text-anchor" attribute of this DefaultValues_t.
 *
 * @copydetails doc_render_text_anchor
 *
 * @param dv the DefaultValues_t structure whose text-anchor is sought.
 *
 * @return the value of the "text-anchor" attribute of this DefaultValues_t as
 * a <code>const char *</code>. The possible values returned by this method are:
 * @li @c "start"
 * @li @c "middle"
 * @li @c "end"
 * @li @c "invalid DefaultValuesText-anchor"
 *
 * @copydetails doc_returned_unowned_char
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
char *
DefaultValues_getTextAnchorAsString(const DefaultValues_t * dv);


/**
 * Returns the value of the "vtext-anchor" attribute of this DefaultValues_t.
 *
 * @copydetails doc_render_vtext_anchor
 *
 * @param dv the DefaultValues_t structure whose vtext-anchor is sought.
 *
 * @return the value of the "vtext-anchor" attribute of this DefaultValues_t as
 * a #VTextAnchor_t. The possible values returned by this method are:
 * @li @sbmlconstant{V_TEXTANCHOR_UNSET, VTextAnchor_t}
 * @li @sbmlconstant{V_TEXTANCHOR_TOP, VTextAnchor_t}
 * @li @sbmlconstant{V_TEXTANCHOR_MIDDLE, VTextAnchor_t}
 * @li @sbmlconstant{V_TEXTANCHOR_BOTTOM, VTextAnchor_t}
 * @li @sbmlconstant{V_TEXTANCHOR_BASELINE, VTextAnchor_t}
 * @li @sbmlconstant{V_TEXTANCHOR_INVALID, VTextAnchor_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
VTextAnchor_t
DefaultValues_getVtextAnchor(const DefaultValues_t * dv);


/**
 * Returns the value of the "vtext-anchor" attribute of this DefaultValues_t.
 *
 * @copydetails doc_render_vtext_anchor
 *
 * @param dv the DefaultValues_t structure whose vtext-anchor is sought.
 *
 * @return the value of the "vtext-anchor" attribute of this DefaultValues_t as
 * a <code>const char *</code>. The possible values returned by this method are:
 * @li @c "top"
 * @li @c "middle"
 * @li @c "bottom"
 * @li @c "baseline"
 * @li @c "invalid DefaultValuesVtext-anchor"
 *
 * @copydetails doc_returned_unowned_char
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
char *
DefaultValues_getVtextAnchorAsString(const DefaultValues_t * dv);


/**
 * Returns the value of the "startHead" attribute of this DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure whose startHead is sought.
 *
 * @return the value of the "startHead" attribute of this DefaultValues_t as a
 * pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
char *
DefaultValues_getStartHead(const DefaultValues_t * dv);


/**
 * Returns the value of the "endHead" attribute of this DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure whose endHead is sought.
 *
 * @return the value of the "endHead" attribute of this DefaultValues_t as a
 * pointer to a string.
 *
 * @copydetails doc_warning_returns_owned_char
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
char *
DefaultValues_getEndHead(const DefaultValues_t * dv);


/**
 * Returns the value of the "enableRotationalMapping" attribute of this
 * DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure whose enableRotationalMapping is
 * sought.
 *
 * @return the value of the "enableRotationalMapping" attribute of this
 * DefaultValues_t as a boolean.
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_getEnableRotationalMapping(const DefaultValues_t * dv);


/**
 * Predicate returning @c 1 (true) if this DefaultValues_t's "backgroundColor"
 * attribute is set.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @return @c 1 (true) if this DefaultValues_t's "backgroundColor" attribute
 * has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_isSetBackgroundColor(const DefaultValues_t * dv);


/**
 * Predicate returning @c 1 (true) if this DefaultValues_t's "spreadMethod"
 * attribute is set.
 *
 * @copydetails doc_render_spreadMethod
 *
 * @param dv the DefaultValues_t structure.
 *
 * @return @c 1 (true) if this DefaultValues_t's "spreadMethod" attribute has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_isSetSpreadMethod(const DefaultValues_t * dv);


/**
 * Predicate returning @c 1 (true) if this DefaultValues_t's "fill" attribute
 * is set.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @return @c 1 (true) if this DefaultValues_t's "fill" attribute has been set,
 * otherwise @c 0 (false) is returned.
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_isSetFill(const DefaultValues_t * dv);


/**
 * Predicate returning @c 1 (true) if this DefaultValues_t's "fill-rule"
 * attribute is set.
 *
 * @copydetails doc_render_fill_rule
 *
 * @param dv the DefaultValues_t structure.
 *
 * @return @c 1 (true) if this DefaultValues_t's "fill-rule" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_isSetFillRule(const DefaultValues_t * dv);


/**
 * Predicate returning @c 1 (true) if this DefaultValues_t's "stroke" attribute
 * is set.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @return @c 1 (true) if this DefaultValues_t's "stroke" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_isSetStroke(const DefaultValues_t * dv);


/**
 * Predicate returning @c 1 (true) if this DefaultValues_t's "stroke-width"
 * attribute is set.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @return @c 1 (true) if this DefaultValues_t's "stroke-width" attribute has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_isSetStrokeWidth(const DefaultValues_t * dv);


/**
 * Predicate returning @c 1 (true) if this DefaultValues_t's "font-family"
 * attribute is set.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @return @c 1 (true) if this DefaultValues_t's "font-family" attribute has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_isSetFontFamily(const DefaultValues_t * dv);


/**
 * Predicate returning @c 1 (true) if this DefaultValues_t's "font-weight"
 * attribute is set.
 *
 * @copydetails doc_render_font_weight
 *
 * @param dv the DefaultValues_t structure.
 *
 * @return @c 1 (true) if this DefaultValues_t's "font-weight" attribute has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_isSetFontWeight(const DefaultValues_t * dv);


/**
 * Predicate returning @c 1 (true) if this DefaultValues_t's "font-style"
 * attribute is set.
 *
 * @copydetails doc_render_font_style
 *
 * @param dv the DefaultValues_t structure.
 *
 * @return @c 1 (true) if this DefaultValues_t's "font-style" attribute has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_isSetFontStyle(const DefaultValues_t * dv);


/**
 * Predicate returning @c 1 (true) if this DefaultValues_t's "text-anchor"
 * attribute is set.
 *
 * @copydetails doc_render_text_anchor
 *
 * @param dv the DefaultValues_t structure.
 *
 * @return @c 1 (true) if this DefaultValues_t's "text-anchor" attribute has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_isSetTextAnchor(const DefaultValues_t * dv);


/**
 * Predicate returning @c 1 (true) if this DefaultValues_t's "vtext-anchor"
 * attribute is set.
 *
 * @copydetails doc_render_vtext_anchor
 *
 * @param dv the DefaultValues_t structure.
 *
 * @return @c 1 (true) if this DefaultValues_t's "vtext-anchor" attribute has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_isSetVtextAnchor(const DefaultValues_t * dv);


/**
 * Predicate returning @c 1 (true) if this DefaultValues_t's "startHead"
 * attribute is set.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @return @c 1 (true) if this DefaultValues_t's "startHead" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_isSetStartHead(const DefaultValues_t * dv);


/**
 * Predicate returning @c 1 (true) if this DefaultValues_t's "endHead"
 * attribute is set.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @return @c 1 (true) if this DefaultValues_t's "endHead" attribute has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_isSetEndHead(const DefaultValues_t * dv);


/**
 * Predicate returning @c 1 (true) if this DefaultValues_t's
 * "enableRotationalMapping" attribute is set.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @return @c 1 (true) if this DefaultValues_t's "enableRotationalMapping"
 * attribute has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_isSetEnableRotationalMapping(const DefaultValues_t * dv);


/**
 * Sets the value of the "backgroundColor" attribute of this DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @param backgroundColor <code>const char *</code>* value of the "backgroundColor"
 * attribute to be set. Calling this function with @p backgroundColor = @c
 * NULL or an empty string is equivalent to calling
 * DefaultValues_unsetBackgroundColor().
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_setBackgroundColor(DefaultValues_t * dv,
                                 const char * backgroundColor);


/**
 * Sets the value of the "spreadMethod" attribute of this DefaultValues_t.
 *
 * @copydetails doc_render_spreadMethod
 *
 * @param dv the DefaultValues_t structure.
 *
 * @param spreadMethod #GradientSpreadMethod_t value of the "spreadMethod"
 * attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_setSpreadMethod(DefaultValues_t * dv,
                              GradientSpreadMethod_t spreadMethod);


/**
 * Sets the value of the "spreadMethod" attribute of this DefaultValues_t.
 *
 * @copydetails doc_render_spreadMethod
 *
 * @param dv the DefaultValues_t structure.
 *
 * @param spreadMethod <code>const char *</code>* of the "spreadMethod" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_setSpreadMethodAsString(DefaultValues_t * dv,
                                      const char * spreadMethod);


/**
 * Sets the value of the "fill" attribute of this DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @param fill <code>const char *</code>* value of the "fill" attribute to be set.
 * Calling this function with @p fill = @c NULL or an empty string is
 * equivalent to calling DefaultValues_unsetFill().
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_setFill(DefaultValues_t * dv, const char * fill);


/**
 * Sets the value of the "fill-rule" attribute of this DefaultValues_t.
 *
 * @copydetails doc_render_fill_rule
 *
 * @param dv the DefaultValues_t structure.
 *
 * @param fillRule #FillRule_t value of the "fill-rule" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_setFillRule(DefaultValues_t * dv, FillRule_t fillRule);


/**
 * Sets the value of the "fill-rule" attribute of this DefaultValues_t.
 *
 * @copydetails doc_render_fill_rule
 *
 * @param dv the DefaultValues_t structure.
 *
 * @param fillRule <code>const char *</code>* of the "fill-rule" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_setFillRuleAsString(DefaultValues_t * dv,
                                  const char * fillRule);


/**
 * Sets the value of the "stroke" attribute of this DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @param stroke <code>const char *</code>* value of the "stroke" attribute to be set.
 * Calling this function with @p stroke = @c NULL or an empty string is
 * equivalent to calling DefaultValues_unsetStroke().
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_setStroke(DefaultValues_t * dv, const char * stroke);


/**
 * Sets the value of the "stroke-width" attribute of this DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @param strokeWidth double value of the "stroke-width" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_setStrokeWidth(DefaultValues_t * dv, double strokeWidth);


/**
 * Sets the value of the "font-family" attribute of this DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @param fontFamily <code>const char *</code>* value of the "font-family" attribute to be
 * set.
 * Calling this function with @p fontFamily = @c NULL or an empty string is
 * equivalent to calling DefaultValues_unsetFontFamily().
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_setFontFamily(DefaultValues_t * dv, const char * fontFamily);


/**
 * Sets the value of the "font-weight" attribute of this DefaultValues_t.
 *
 * @copydetails doc_render_font_weight
 *
 * @param dv the DefaultValues_t structure.
 *
 * @param fontWeight #FontWeight_t value of the "font-weight" attribute to be
 * set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_setFontWeight(DefaultValues_t * dv, FontWeight_t fontWeight);


/**
 * Sets the value of the "font-weight" attribute of this DefaultValues_t.
 *
 * @copydetails doc_render_font_weight
 *
 * @param dv the DefaultValues_t structure.
 *
 * @param fontWeight <code>const char *</code>* of the "font-weight" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_setFontWeightAsString(DefaultValues_t * dv,
                                    const char * fontWeight);


/**
 * Sets the value of the "font-style" attribute of this DefaultValues_t.
 *
 * @copydetails doc_render_font_style
 *
 * @param dv the DefaultValues_t structure.
 *
 * @param fontStyle #FontStyle_t value of the "font-style" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_setFontStyle(DefaultValues_t * dv, FontStyle_t fontStyle);


/**
 * Sets the value of the "font-style" attribute of this DefaultValues_t.
 *
 * @copydetails doc_render_font_style
 *
 * @param dv the DefaultValues_t structure.
 *
 * @param fontStyle <code>const char *</code>* of the "font-style" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_setFontStyleAsString(DefaultValues_t * dv,
                                   const char * fontStyle);


/**
 * Sets the value of the "text-anchor" attribute of this DefaultValues_t.
 *
 * @copydetails doc_render_text_anchor
 *
 * @param dv the DefaultValues_t structure.
 *
 * @param textAnchor #HTextAnchor_t value of the "text-anchor" attribute to be
 * set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_setTextAnchor(DefaultValues_t * dv, HTextAnchor_t textAnchor);


/**
 * Sets the value of the "text-anchor" attribute of this DefaultValues_t.
 *
 * @copydetails doc_render_text_anchor
 *
 * @param dv the DefaultValues_t structure.
 *
 * @param textAnchor <code>const char *</code>* of the "text-anchor" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_setTextAnchorAsString(DefaultValues_t * dv,
                                    const char * textAnchor);


/**
 * Sets the value of the "vtext-anchor" attribute of this DefaultValues_t.
 *
 * @copydetails doc_render_vtext_anchor
 *
 * @param dv the DefaultValues_t structure.
 *
 * @param vtextAnchor #VTextAnchor_t value of the "vtext-anchor" attribute to be
 * set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_setVtextAnchor(DefaultValues_t * dv, VTextAnchor_t vtextAnchor);


/**
 * Sets the value of the "vtext-anchor" attribute of this DefaultValues_t.
 *
 * @copydetails doc_render_vtext_anchor
 *
 * @param dv the DefaultValues_t structure.
 *
 * @param vtextAnchor <code>const char *</code>* of the "vtext-anchor" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_setVtextAnchorAsString(DefaultValues_t * dv,
                                     const char * vtextAnchor);


/**
 * Sets the value of the "startHead" attribute of this DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @param startHead <code>const char *</code>* value of the "startHead" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_setStartHead(DefaultValues_t * dv, const char * startHead);


/**
 * Sets the value of the "endHead" attribute of this DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @param endHead <code>const char *</code>* value of the "endHead" attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_setEndHead(DefaultValues_t * dv, const char * endHead);


/**
 * Sets the value of the "enableRotationalMapping" attribute of this
 * DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @param enableRotationalMapping int value of the "enableRotationalMapping"
 * attribute to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_setEnableRotationalMapping(DefaultValues_t * dv,
                                         int enableRotationalMapping);


/**
 * Unsets the value of the "backgroundColor" attribute of this DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_unsetBackgroundColor(DefaultValues_t * dv);


/**
 * Unsets the value of the "spreadMethod" attribute of this DefaultValues_t.
 *
 * @copydetails doc_render_spreadMethod
 *
 * @param dv the DefaultValues_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_unsetSpreadMethod(DefaultValues_t * dv);


/**
 * Unsets the value of the "fill" attribute of this DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_unsetFill(DefaultValues_t * dv);


/**
 * Unsets the value of the "fill-rule" attribute of this DefaultValues_t.
 *
 * @copydetails doc_render_fill_rule
 *
 * @param dv the DefaultValues_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_unsetFillRule(DefaultValues_t * dv);


/**
 * Unsets the value of the "stroke" attribute of this DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_unsetStroke(DefaultValues_t * dv);


/**
 * Unsets the value of the "stroke-width" attribute of this DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_unsetStrokeWidth(DefaultValues_t * dv);


/**
 * Unsets the value of the "font-family" attribute of this DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_unsetFontFamily(DefaultValues_t * dv);


/**
 * Unsets the value of the "font-weight" attribute of this DefaultValues_t.
 *
 * @copydetails doc_render_font_weight
 *
 * @param dv the DefaultValues_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_unsetFontWeight(DefaultValues_t * dv);


/**
 * Unsets the value of the "font-style" attribute of this DefaultValues_t.
 *
 * @copydetails doc_render_font_style
 *
 * @param dv the DefaultValues_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_unsetFontStyle(DefaultValues_t * dv);


/**
 * Unsets the value of the "text-anchor" attribute of this DefaultValues_t.
 *
 * @copydetails doc_render_text_anchor
 *
 * @param dv the DefaultValues_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_unsetTextAnchor(DefaultValues_t * dv);


/**
 * Unsets the value of the "vtext-anchor" attribute of this DefaultValues_t.
 *
 * @copydetails doc_render_vtext_anchor
 *
 * @param dv the DefaultValues_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_unsetVtextAnchor(DefaultValues_t * dv);


/**
 * Unsets the value of the "startHead" attribute of this DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_unsetStartHead(DefaultValues_t * dv);


/**
 * Unsets the value of the "endHead" attribute of this DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_unsetEndHead(DefaultValues_t * dv);


/**
 * Unsets the value of the "enableRotationalMapping" attribute of this
 * DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_unsetEnableRotationalMapping(DefaultValues_t * dv);


/**
 * Returns the value of the "linearGradient_x1" element of this
 * DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure whose linearGradient_x1 is sought.
 *
 * @return the value of the "linearGradient_x1" element of this DefaultValues_t
 * as a RelAbsVector_t.
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
const RelAbsVector_t*
DefaultValues_getLinearGradient_x1(const DefaultValues_t * dv);


/**
 * Returns the value of the "linearGradient_y1" element of this
 * DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure whose linearGradient_y1 is sought.
 *
 * @return the value of the "linearGradient_y1" element of this DefaultValues_t
 * as a RelAbsVector_t.
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
const RelAbsVector_t*
DefaultValues_getLinearGradient_y1(const DefaultValues_t * dv);


/**
 * Returns the value of the "linearGradient_z1" element of this
 * DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure whose linearGradient_z1 is sought.
 *
 * @return the value of the "linearGradient_z1" element of this DefaultValues_t
 * as a RelAbsVector_t.
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
const RelAbsVector_t*
DefaultValues_getLinearGradient_z1(const DefaultValues_t * dv);


/**
 * Returns the value of the "linearGradient_x2" element of this
 * DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure whose linearGradient_x2 is sought.
 *
 * @return the value of the "linearGradient_x2" element of this DefaultValues_t
 * as a RelAbsVector_t.
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
const RelAbsVector_t*
DefaultValues_getLinearGradient_x2(const DefaultValues_t * dv);


/**
 * Returns the value of the "linearGradient_y2" element of this
 * DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure whose linearGradient_y2 is sought.
 *
 * @return the value of the "linearGradient_y2" element of this DefaultValues_t
 * as a RelAbsVector_t.
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
const RelAbsVector_t*
DefaultValues_getLinearGradient_y2(const DefaultValues_t * dv);


/**
 * Returns the value of the "linearGradient_z2" element of this
 * DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure whose linearGradient_z2 is sought.
 *
 * @return the value of the "linearGradient_z2" element of this DefaultValues_t
 * as a RelAbsVector_t.
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
const RelAbsVector_t*
DefaultValues_getLinearGradient_z2(const DefaultValues_t * dv);


/**
 * Returns the value of the "radialGradient_cx" element of this
 * DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure whose radialGradient_cx is sought.
 *
 * @return the value of the "radialGradient_cx" element of this DefaultValues_t
 * as a RelAbsVector_t.
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
const RelAbsVector_t*
DefaultValues_getRadialGradient_cx(const DefaultValues_t * dv);


/**
 * Returns the value of the "radialGradient_cy" element of this
 * DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure whose radialGradient_cy is sought.
 *
 * @return the value of the "radialGradient_cy" element of this DefaultValues_t
 * as a RelAbsVector_t.
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
const RelAbsVector_t*
DefaultValues_getRadialGradient_cy(const DefaultValues_t * dv);


/**
 * Returns the value of the "radialGradient_cz" element of this
 * DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure whose radialGradient_cz is sought.
 *
 * @return the value of the "radialGradient_cz" element of this DefaultValues_t
 * as a RelAbsVector_t.
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
const RelAbsVector_t*
DefaultValues_getRadialGradient_cz(const DefaultValues_t * dv);


/**
 * Returns the value of the "radialGradient_r" element of this DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure whose radialGradient_r is sought.
 *
 * @return the value of the "radialGradient_r" element of this DefaultValues_t
 * as a RelAbsVector_t.
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
const RelAbsVector_t*
DefaultValues_getRadialGradient_r(const DefaultValues_t * dv);


/**
 * Returns the value of the "radialGradient_fx" element of this
 * DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure whose radialGradient_fx is sought.
 *
 * @return the value of the "radialGradient_fx" element of this DefaultValues_t
 * as a RelAbsVector_t.
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
const RelAbsVector_t*
DefaultValues_getRadialGradient_fx(const DefaultValues_t * dv);


/**
 * Returns the value of the "radialGradient_fy" element of this
 * DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure whose radialGradient_fy is sought.
 *
 * @return the value of the "radialGradient_fy" element of this DefaultValues_t
 * as a RelAbsVector_t.
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
const RelAbsVector_t*
DefaultValues_getRadialGradient_fy(const DefaultValues_t * dv);


/**
 * Returns the value of the "radialGradient_fz" element of this
 * DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure whose radialGradient_fz is sought.
 *
 * @return the value of the "radialGradient_fz" element of this DefaultValues_t
 * as a RelAbsVector_t.
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
const RelAbsVector_t*
DefaultValues_getRadialGradient_fz(const DefaultValues_t * dv);


/**
 * Returns the value of the "default_z" element of this DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure whose default_z is sought.
 *
 * @return the value of the "default_z" element of this DefaultValues_t as a
 * RelAbsVector_t.
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
const RelAbsVector_t*
DefaultValues_getDefault_z(const DefaultValues_t * dv);


/**
 * Returns the value of the "font-size" element of this DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure whose font-size is sought.
 *
 * @return the value of the "font-size" element of this DefaultValues_t as a
 * RelAbsVector_t.
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
const RelAbsVector_t*
DefaultValues_getFontSize(const DefaultValues_t * dv);


/**
 * Predicate returning @c 1 (true) if this DefaultValues_t's
 * "linearGradient_x1" element is set.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @return @c 1 (true) if this DefaultValues_t's "linearGradient_x1" element
 * has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_isSetLinearGradient_x1(const DefaultValues_t * dv);


/**
 * Predicate returning @c 1 (true) if this DefaultValues_t's
 * "linearGradient_y1" element is set.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @return @c 1 (true) if this DefaultValues_t's "linearGradient_y1" element
 * has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_isSetLinearGradient_y1(const DefaultValues_t * dv);


/**
 * Predicate returning @c 1 (true) if this DefaultValues_t's
 * "linearGradient_z1" element is set.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @return @c 1 (true) if this DefaultValues_t's "linearGradient_z1" element
 * has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_isSetLinearGradient_z1(const DefaultValues_t * dv);


/**
 * Predicate returning @c 1 (true) if this DefaultValues_t's
 * "linearGradient_x2" element is set.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @return @c 1 (true) if this DefaultValues_t's "linearGradient_x2" element
 * has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_isSetLinearGradient_x2(const DefaultValues_t * dv);


/**
 * Predicate returning @c 1 (true) if this DefaultValues_t's
 * "linearGradient_y2" element is set.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @return @c 1 (true) if this DefaultValues_t's "linearGradient_y2" element
 * has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_isSetLinearGradient_y2(const DefaultValues_t * dv);


/**
 * Predicate returning @c 1 (true) if this DefaultValues_t's
 * "linearGradient_z2" element is set.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @return @c 1 (true) if this DefaultValues_t's "linearGradient_z2" element
 * has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_isSetLinearGradient_z2(const DefaultValues_t * dv);


/**
 * Predicate returning @c 1 (true) if this DefaultValues_t's
 * "radialGradient_cx" element is set.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @return @c 1 (true) if this DefaultValues_t's "radialGradient_cx" element
 * has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_isSetRadialGradient_cx(const DefaultValues_t * dv);


/**
 * Predicate returning @c 1 (true) if this DefaultValues_t's
 * "radialGradient_cy" element is set.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @return @c 1 (true) if this DefaultValues_t's "radialGradient_cy" element
 * has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_isSetRadialGradient_cy(const DefaultValues_t * dv);


/**
 * Predicate returning @c 1 (true) if this DefaultValues_t's
 * "radialGradient_cz" element is set.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @return @c 1 (true) if this DefaultValues_t's "radialGradient_cz" element
 * has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_isSetRadialGradient_cz(const DefaultValues_t * dv);


/**
 * Predicate returning @c 1 (true) if this DefaultValues_t's "radialGradient_r"
 * element is set.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @return @c 1 (true) if this DefaultValues_t's "radialGradient_r" element has
 * been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_isSetRadialGradient_r(const DefaultValues_t * dv);


/**
 * Predicate returning @c 1 (true) if this DefaultValues_t's
 * "radialGradient_fx" element is set.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @return @c 1 (true) if this DefaultValues_t's "radialGradient_fx" element
 * has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_isSetRadialGradient_fx(const DefaultValues_t * dv);


/**
 * Predicate returning @c 1 (true) if this DefaultValues_t's
 * "radialGradient_fy" element is set.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @return @c 1 (true) if this DefaultValues_t's "radialGradient_fy" element
 * has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_isSetRadialGradient_fy(const DefaultValues_t * dv);


/**
 * Predicate returning @c 1 (true) if this DefaultValues_t's
 * "radialGradient_fz" element is set.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @return @c 1 (true) if this DefaultValues_t's "radialGradient_fz" element
 * has been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_isSetRadialGradient_fz(const DefaultValues_t * dv);


/**
 * Predicate returning @c 1 (true) if this DefaultValues_t's "default_z"
 * element is set.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @return @c 1 (true) if this DefaultValues_t's "default_z" element has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_isSetDefault_z(const DefaultValues_t * dv);


/**
 * Predicate returning @c 1 (true) if this DefaultValues_t's "font-size"
 * element is set.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @return @c 1 (true) if this DefaultValues_t's "font-size" element has been
 * set, otherwise @c 0 (false) is returned.
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_isSetFontSize(const DefaultValues_t * dv);


/**
 * Sets the value of the "linearGradient_x1" element of this DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @param linearGradient_x1 RelAbsVector_t value of the "linearGradient_x1"
 * element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_setLinearGradient_x1(DefaultValues_t * dv,
                                   const RelAbsVector_t* linearGradient_x1);


/**
 * Sets the value of the "linearGradient_y1" element of this DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @param linearGradient_y1 RelAbsVector_t value of the "linearGradient_y1"
 * element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_setLinearGradient_y1(DefaultValues_t * dv,
                                   const RelAbsVector_t* linearGradient_y1);


/**
 * Sets the value of the "linearGradient_z1" element of this DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @param linearGradient_z1 RelAbsVector_t value of the "linearGradient_z1"
 * element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_setLinearGradient_z1(DefaultValues_t * dv,
                                   const RelAbsVector_t* linearGradient_z1);


/**
 * Sets the value of the "linearGradient_x2" element of this DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @param linearGradient_x2 RelAbsVector_t value of the "linearGradient_x2"
 * element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_setLinearGradient_x2(DefaultValues_t * dv,
                                   const RelAbsVector_t* linearGradient_x2);


/**
 * Sets the value of the "linearGradient_y2" element of this DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @param linearGradient_y2 RelAbsVector_t value of the "linearGradient_y2"
 * element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_setLinearGradient_y2(DefaultValues_t * dv,
                                   const RelAbsVector_t* linearGradient_y2);


/**
 * Sets the value of the "linearGradient_z2" element of this DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @param linearGradient_z2 RelAbsVector_t value of the "linearGradient_z2"
 * element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_setLinearGradient_z2(DefaultValues_t * dv,
                                   const RelAbsVector_t* linearGradient_z2);


/**
 * Sets the value of the "radialGradient_cx" element of this DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @param radialGradient_cx RelAbsVector_t value of the "radialGradient_cx"
 * element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_setRadialGradient_cx(DefaultValues_t * dv,
                                   const RelAbsVector_t* radialGradient_cx);


/**
 * Sets the value of the "radialGradient_cy" element of this DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @param radialGradient_cy RelAbsVector_t value of the "radialGradient_cy"
 * element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_setRadialGradient_cy(DefaultValues_t * dv,
                                   const RelAbsVector_t* radialGradient_cy);


/**
 * Sets the value of the "radialGradient_cz" element of this DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @param radialGradient_cz RelAbsVector_t value of the "radialGradient_cz"
 * element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_setRadialGradient_cz(DefaultValues_t * dv,
                                   const RelAbsVector_t* radialGradient_cz);


/**
 * Sets the value of the "radialGradient_r" element of this DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @param radialGradient_r RelAbsVector_t value of the "radialGradient_r"
 * element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_setRadialGradient_r(DefaultValues_t * dv,
                                  const RelAbsVector_t* radialGradient_r);


/**
 * Sets the value of the "radialGradient_fx" element of this DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @param radialGradient_fx RelAbsVector_t value of the "radialGradient_fx"
 * element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_setRadialGradient_fx(DefaultValues_t * dv,
                                   const RelAbsVector_t* radialGradient_fx);


/**
 * Sets the value of the "radialGradient_fy" element of this DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @param radialGradient_fy RelAbsVector_t value of the "radialGradient_fy"
 * element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_setRadialGradient_fy(DefaultValues_t * dv,
                                   const RelAbsVector_t* radialGradient_fy);


/**
 * Sets the value of the "radialGradient_fz" element of this DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @param radialGradient_fz RelAbsVector_t value of the "radialGradient_fz"
 * element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_setRadialGradient_fz(DefaultValues_t * dv,
                                   const RelAbsVector_t* radialGradient_fz);


/**
 * Sets the value of the "default_z" element of this DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @param default_z RelAbsVector_t value of the "default_z" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_setDefault_z(DefaultValues_t * dv,
                           const RelAbsVector_t* default_z);


/**
 * Sets the value of the "font-size" element of this DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @param fontSize RelAbsVector_t value of the "font-size" element to be set.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_setFontSize(DefaultValues_t * dv,
                          const RelAbsVector_t* fontSize);


/**
 * Unsets the value of the "linearGradient_x1" element of this DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_unsetLinearGradient_x1(DefaultValues_t * dv);


/**
 * Unsets the value of the "linearGradient_y1" element of this DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_unsetLinearGradient_y1(DefaultValues_t * dv);


/**
 * Unsets the value of the "linearGradient_z1" element of this DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_unsetLinearGradient_z1(DefaultValues_t * dv);


/**
 * Unsets the value of the "linearGradient_x2" element of this DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_unsetLinearGradient_x2(DefaultValues_t * dv);


/**
 * Unsets the value of the "linearGradient_y2" element of this DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_unsetLinearGradient_y2(DefaultValues_t * dv);


/**
 * Unsets the value of the "linearGradient_z2" element of this DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_unsetLinearGradient_z2(DefaultValues_t * dv);


/**
 * Unsets the value of the "radialGradient_cx" element of this DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_unsetRadialGradient_cx(DefaultValues_t * dv);


/**
 * Unsets the value of the "radialGradient_cy" element of this DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_unsetRadialGradient_cy(DefaultValues_t * dv);


/**
 * Unsets the value of the "radialGradient_cz" element of this DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_unsetRadialGradient_cz(DefaultValues_t * dv);


/**
 * Unsets the value of the "radialGradient_r" element of this DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_unsetRadialGradient_r(DefaultValues_t * dv);


/**
 * Unsets the value of the "radialGradient_fx" element of this DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_unsetRadialGradient_fx(DefaultValues_t * dv);


/**
 * Unsets the value of the "radialGradient_fy" element of this DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_unsetRadialGradient_fy(DefaultValues_t * dv);


/**
 * Unsets the value of the "radialGradient_fz" element of this DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_unsetRadialGradient_fz(DefaultValues_t * dv);


/**
 * Unsets the value of the "default_z" element of this DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_unsetDefault_z(DefaultValues_t * dv);


/**
 * Unsets the value of the "font-size" element of this DefaultValues_t.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @copydetails doc_returns_success_code
 * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
 * @li @sbmlconstant{LIBSBML_INVALID_OBJECT, OperationReturnValues_t}
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_unsetFontSize(DefaultValues_t * dv);


/**
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DefaultValues_t object have been set.
 *
 * @param dv the DefaultValues_t structure.
 *
 * @return @c 1 (true) to indicate that all the required attributes of this
 * DefaultValues_t have been set, otherwise @c 0 (false) is returned.
 *
 * @memberof DefaultValues_t
 */
LIBSBML_EXTERN
int
DefaultValues_hasRequiredAttributes(const DefaultValues_t * dv);




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !DefaultValues_H__ */


