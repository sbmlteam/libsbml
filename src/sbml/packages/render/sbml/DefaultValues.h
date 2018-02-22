/**
 * @file DefaultValues.h
 * @brief Definition of the DefaultValues class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
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
 * @sbmlbrief{render} TODO:Definition of the DefaultValues class.
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
   * Creates a new DefaultValues using the given SBML Level, Version and
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
   * Creates a new DefaultValues using the given RenderPkgNamespaces object.
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
   * DefaultValues.
   *
   * @return the value of the "backgroundColor" attribute of this DefaultValues
   * as a string.
   */
  const std::string& getBackgroundColor() const;


  /**
   * Returns the value of the "spreadMethod" attribute of this DefaultValues.
   *
   * @return the value of the "spreadMethod" attribute of this DefaultValues as
   * a GradientBase::SPREADMETHOD.
   *
   */
  GradientSpreadMethod_t getSpreadMethod() const;


  /**
   * Returns the value of the "spreadMethod" attribute of this DefaultValues.
   *
   * @return the value of the "spreadMethod" attribute of this DefaultValues as
   * a string.
   *
   */
  std::string getSpreadMethodAsString() const;


  /**
   * Returns the value of the "linearGradient_x1" attribute of this
   * DefaultValues.
   *
   * @return the value of the "linearGradient_x1" attribute of this
   * DefaultValues as a string.
   */
  const RelAbsVector& getLinearGradient_x1() const;


  /**
   * Returns the value of the "linearGradient_y1" attribute of this
   * DefaultValues.
   *
   * @return the value of the "linearGradient_y1" attribute of this
   * DefaultValues as a string.
   */
  const RelAbsVector& getLinearGradient_y1() const;


  /**
   * Returns the value of the "linearGradient_z1" attribute of this
   * DefaultValues.
   *
   * @return the value of the "linearGradient_z1" attribute of this
   * DefaultValues as a string.
   */
  const RelAbsVector& getLinearGradient_z1() const;


  /**
   * Returns the value of the "linearGradient_x2" attribute of this
   * DefaultValues.
   *
   * @return the value of the "linearGradient_x2" attribute of this
   * DefaultValues as a string.
   */
  const RelAbsVector& getLinearGradient_x2() const;


  /**
   * Returns the value of the "linearGradient_y2" attribute of this
   * DefaultValues.
   *
   * @return the value of the "linearGradient_y2" attribute of this
   * DefaultValues as a string.
   */
  const RelAbsVector& getLinearGradient_y2() const;


  /**
   * Returns the value of the "linearGradient_z2" attribute of this
   * DefaultValues.
   *
   * @return the value of the "linearGradient_z2" attribute of this
   * DefaultValues as a string.
   */
  const RelAbsVector& getLinearGradient_z2() const;


  /**
   * Returns the value of the "radialGradient_cx" attribute of this
   * DefaultValues.
   *
   * @return the value of the "radialGradient_cx" attribute of this
   * DefaultValues as a string.
   */
  const RelAbsVector& getRadialGradient_cx() const;


  /**
   * Returns the value of the "radialGradient_cy" attribute of this
   * DefaultValues.
   *
   * @return the value of the "radialGradient_cy" attribute of this
   * DefaultValues as a string.
   */
  const RelAbsVector& getRadialGradient_cy() const;


  /**
   * Returns the value of the "radialGradient_cz" attribute of this
   * DefaultValues.
   *
   * @return the value of the "radialGradient_cz" attribute of this
   * DefaultValues as a string.
   */
  const RelAbsVector& getRadialGradient_cz() const;


  /**
   * Returns the value of the "radialGradient_r" attribute of this
   * DefaultValues.
   *
   * @return the value of the "radialGradient_r" attribute of this
   * DefaultValues as a string.
   */
  const RelAbsVector& getRadialGradient_r() const;


  /**
   * Returns the value of the "radialGradient_fx" attribute of this
   * DefaultValues.
   *
   * @return the value of the "radialGradient_fx" attribute of this
   * DefaultValues as a string.
   */
  const RelAbsVector& getRadialGradient_fx() const;


  /**
   * Returns the value of the "radialGradient_fy" attribute of this
   * DefaultValues.
   *
   * @return the value of the "radialGradient_fy" attribute of this
   * DefaultValues as a string.
   */
  const RelAbsVector& getRadialGradient_fy() const;


  /**
   * Returns the value of the "radialGradient_fz" attribute of this
   * DefaultValues.
   *
   * @return the value of the "radialGradient_fz" attribute of this
   * DefaultValues as a string.
   */
  const RelAbsVector& getRadialGradient_fz() const;


  /**
   * Returns the value of the "fill" attribute of this DefaultValues.
   *
   * @return the value of the "fill" attribute of this DefaultValues as a
   * string.
   */
  const std::string& getFill() const;


  /**
   * Returns the value of the "fillRule" attribute of this DefaultValues.
   *
   * @return the value of the "fillRule" attribute of this DefaultValues as a
   * FileRule_t.
   *
   */
  int getFillRule() const;


  /**
   * Returns the value of the "fillRule" attribute of this DefaultValues.
   *
   * @return the value of the "fillRule" attribute of this DefaultValues as a
   * string.
   *
   */
  std::string getFillRuleAsString() const;


  /**
   * Returns the value of the "default_z" attribute of this DefaultValues.
   *
   * @return the value of the "default_z" attribute of this DefaultValues as a
   * string.
   */
  const RelAbsVector& getDefault_z() const;


  /**
   * Returns the value of the "stroke" attribute of this DefaultValues.
   *
   * @return the value of the "stroke" attribute of this DefaultValues as a
   * string.
   */
  const std::string& getStroke() const;


  /**
   * Returns the value of the "strokeWidth" attribute of this DefaultValues.
   *
   * @return the value of the "strokeWidth" attribute of this DefaultValues as
   * a string.
   */
  double getStrokeWidth() const;


  /**
   * Returns the value of the "fontFamily" attribute of this DefaultValues.
   *
   * @return the value of the "fontFamily" attribute of this DefaultValues as a
   * string.
   */
  const std::string& getFontFamily() const;


  /**
   * Returns the value of the "fontSize" attribute of this DefaultValues.
   *
   * @return the value of the "fontSize" attribute of this DefaultValues as a
   * string.
   */
  const RelAbsVector& getFontSize() const;


  /**
   * Returns the value of the "fontWeight" attribute of this DefaultValues.
   *
   * @return the value of the "fontWeight" attribute of this DefaultValues as a
   * Text::FONT_WEIGHT.
   *
   */
  FontWeight_t getFontWeight() const;


  /**
   * Returns the value of the "fontWeight" attribute of this DefaultValues.
   *
   * @return the value of the "fontWeight" attribute of this DefaultValues as a
   * string.
   *
   */
  std::string getFontWeightAsString() const;


  /**
   * Returns the value of the "fontStyle" attribute of this DefaultValues.
   *
   * @return the value of the "fontStyle" attribute of this DefaultValues as a
   * Text::FONT_STYLE.
   *
   */
  FontStyle_t getFontStyle() const;


  /**
   * Returns the value of the "fontStyle" attribute of this DefaultValues.
   *
   * @return the value of the "fontStyle" attribute of this DefaultValues as a
   * string.
   *
   */
  std::string getFontStyleAsString() const;


  /**
   * Returns the value of the "textAnchor" attribute of this DefaultValues.
   *
   * @return the value of the "textAnchor" attribute of this DefaultValues as a
   * Text::TEXT_ANCHOR.
   *
   */
  HTextAnchor_t getTextAnchor() const;


  /**
   * Returns the value of the "textAnchor" attribute of this DefaultValues.
   *
   * @return the value of the "textAnchor" attribute of this DefaultValues as a
   * string.
   *
   */
  std::string getTextAnchorAsString() const;


  /**
   * Returns the value of the "vtextAnchor" attribute of this DefaultValues.
   *
   * @return the value of the "vtextAnchor" attribute of this DefaultValues as
   * a Text::TEXT_ANCHOR.
   *
   */
  VTextAnchor_t getVTextAnchor() const;


  /**
   * Returns the value of the "vtextAnchor" attribute of this DefaultValues.
   *
   * @return the value of the "vtextAnchor" attribute of this DefaultValues as
   * a string.
   *
   */
  std::string getVTextAnchorAsString() const;


  /**
   * Returns the value of the "startHead" attribute of this DefaultValues.
   *
   * @return the value of the "startHead" attribute of this DefaultValues as a
   * string.
   */
  const std::string& getStartHead() const;


  /**
   * Returns the value of the "endHead" attribute of this DefaultValues.
   *
   * @return the value of the "endHead" attribute of this DefaultValues as a
   * string.
   */
  const std::string& getEndHead() const;


  /**
   * Returns the value of the "enableRotationalMapping" attribute of this
   * DefaultValues.
   *
   * @return the value of the "enableRotationalMapping" attribute of this
   * DefaultValues as a boolean.
   */
  bool getEnableRotationalMapping() const;


  /**
   * Predicate returning @c true if this DefaultValues's "backgroundColor"
   * attribute is set.
   *
   * @return @c true if this DefaultValues's "backgroundColor" attribute has
   * been set, otherwise @c false is returned.
   */
  bool isSetBackgroundColor() const;


  /**
   * Predicate returning @c true if this DefaultValues's "spreadMethod"
   * attribute is set.
   *
   * @return @c true if this DefaultValues's "spreadMethod" attribute has been
   * set, otherwise @c false is returned.
   *
   */
  bool isSetSpreadMethod() const;


  /**
   * Predicate returning @c true if this DefaultValues's "linearGradient_x1"
   * attribute is set.
   *
   * @return @c true if this DefaultValues's "linearGradient_x1" attribute has
   * been set, otherwise @c false is returned.
   */
  bool isSetLinearGradient_x1() const;


  /**
   * Predicate returning @c true if this DefaultValues's "linearGradient_y1"
   * attribute is set.
   *
   * @return @c true if this DefaultValues's "linearGradient_y1" attribute has
   * been set, otherwise @c false is returned.
   */
  bool isSetLinearGradient_y1() const;


  /**
   * Predicate returning @c true if this DefaultValues's "linearGradient_z1"
   * attribute is set.
   *
   * @return @c true if this DefaultValues's "linearGradient_z1" attribute has
   * been set, otherwise @c false is returned.
   */
  bool isSetLinearGradient_z1() const;


  /**
   * Predicate returning @c true if this DefaultValues's "linearGradient_x2"
   * attribute is set.
   *
   * @return @c true if this DefaultValues's "linearGradient_x2" attribute has
   * been set, otherwise @c false is returned.
   */
  bool isSetLinearGradient_x2() const;


  /**
   * Predicate returning @c true if this DefaultValues's "linearGradient_y2"
   * attribute is set.
   *
   * @return @c true if this DefaultValues's "linearGradient_y2" attribute has
   * been set, otherwise @c false is returned.
   */
  bool isSetLinearGradient_y2() const;


  /**
   * Predicate returning @c true if this DefaultValues's "linearGradient_z2"
   * attribute is set.
   *
   * @return @c true if this DefaultValues's "linearGradient_z2" attribute has
   * been set, otherwise @c false is returned.
   */
  bool isSetLinearGradient_z2() const;


  /**
   * Predicate returning @c true if this DefaultValues's "radialGradient_cx"
   * attribute is set.
   *
   * @return @c true if this DefaultValues's "radialGradient_cx" attribute has
   * been set, otherwise @c false is returned.
   */
  bool isSetRadialGradient_cx() const;


  /**
   * Predicate returning @c true if this DefaultValues's "radialGradient_cy"
   * attribute is set.
   *
   * @return @c true if this DefaultValues's "radialGradient_cy" attribute has
   * been set, otherwise @c false is returned.
   */
  bool isSetRadialGradient_cy() const;


  /**
   * Predicate returning @c true if this DefaultValues's "radialGradient_cz"
   * attribute is set.
   *
   * @return @c true if this DefaultValues's "radialGradient_cz" attribute has
   * been set, otherwise @c false is returned.
   */
  bool isSetRadialGradient_cz() const;


  /**
   * Predicate returning @c true if this DefaultValues's "radialGradient_r"
   * attribute is set.
   *
   * @return @c true if this DefaultValues's "radialGradient_r" attribute has
   * been set, otherwise @c false is returned.
   */
  bool isSetRadialGradient_r() const;


  /**
   * Predicate returning @c true if this DefaultValues's "radialGradient_fx"
   * attribute is set.
   *
   * @return @c true if this DefaultValues's "radialGradient_fx" attribute has
   * been set, otherwise @c false is returned.
   */
  bool isSetRadialGradient_fx() const;


  /**
   * Predicate returning @c true if this DefaultValues's "radialGradient_fy"
   * attribute is set.
   *
   * @return @c true if this DefaultValues's "radialGradient_fy" attribute has
   * been set, otherwise @c false is returned.
   */
  bool isSetRadialGradient_fy() const;


  /**
   * Predicate returning @c true if this DefaultValues's "radialGradient_fz"
   * attribute is set.
   *
   * @return @c true if this DefaultValues's "radialGradient_fz" attribute has
   * been set, otherwise @c false is returned.
   */
  bool isSetRadialGradient_fz() const;


  /**
   * Predicate returning @c true if this DefaultValues's "fill" attribute is
   * set.
   *
   * @return @c true if this DefaultValues's "fill" attribute has been set,
   * otherwise @c false is returned.
   */
  bool isSetFill() const;


  /**
   * Predicate returning @c true if this DefaultValues's "fillRule" attribute
   * is set.
   *
   * @return @c true if this DefaultValues's "fillRule" attribute has been set,
   * otherwise @c false is returned.
   *
   */
  bool isSetFillRule() const;


  /**
   * Predicate returning @c true if this DefaultValues's "default_z" attribute
   * is set.
   *
   * @return @c true if this DefaultValues's "default_z" attribute has been
   * set, otherwise @c false is returned.
   */
  bool isSetDefault_z() const;


  /**
   * Predicate returning @c true if this DefaultValues's "stroke" attribute is
   * set.
   *
   * @return @c true if this DefaultValues's "stroke" attribute has been set,
   * otherwise @c false is returned.
   */
  bool isSetStroke() const;


  /**
   * Predicate returning @c true if this DefaultValues's "strokeWidth"
   * attribute is set.
   *
   * @return @c true if this DefaultValues's "strokeWidth" attribute has been
   * set, otherwise @c false is returned.
   */
  bool isSetStrokeWidth() const;


  /**
   * Predicate returning @c true if this DefaultValues's "fontFamily" attribute
   * is set.
   *
   * @return @c true if this DefaultValues's "fontFamily" attribute has been
   * set, otherwise @c false is returned.
   */
  bool isSetFontFamily() const;


  /**
   * Predicate returning @c true if this DefaultValues's "fontSize" attribute
   * is set.
   *
   * @return @c true if this DefaultValues's "fontSize" attribute has been set,
   * otherwise @c false is returned.
   */
  bool isSetFontSize() const;


  /**
   * Predicate returning @c true if this DefaultValues's "fontWeight" attribute
   * is set.
   *
   * @return @c true if this DefaultValues's "fontWeight" attribute has been
   * set, otherwise @c false is returned.
   *
   * @copydetails doc_defaultvalues_fontWeight
   */
  bool isSetFontWeight() const;


  /**
   * Predicate returning @c true if this DefaultValues's "fontStyle" attribute
   * is set.
   *
   * @return @c true if this DefaultValues's "fontStyle" attribute has been
   * set, otherwise @c false is returned.
   *
   */
  bool isSetFontStyle() const;


  /**
   * Predicate returning @c true if this DefaultValues's "textAnchor" attribute
   * is set.
   *
   * @return @c true if this DefaultValues's "textAnchor" attribute has been
   * set, otherwise @c false is returned.
   *
   */
  bool isSetTextAnchor() const;


  /**
   * Predicate returning @c true if this DefaultValues's "vtextAnchor"
   * attribute is set.
   *
   * @return @c true if this DefaultValues's "vtextAnchor" attribute has been
   * set, otherwise @c false is returned.
   *
   */
  bool isSetVTextAnchor() const;


  /**
   * Predicate returning @c true if this DefaultValues's "startHead" attribute
   * is set.
   *
   * @return @c true if this DefaultValues's "startHead" attribute has been
   * set, otherwise @c false is returned.
   */
  bool isSetStartHead() const;


  /**
   * Predicate returning @c true if this DefaultValues's "endHead" attribute is
   * set.
   *
   * @return @c true if this DefaultValues's "endHead" attribute has been set,
   * otherwise @c false is returned.
   */
  bool isSetEndHead() const;


  /**
   * Predicate returning @c true if this DefaultValues's
   * "enableRotationalMapping" attribute is set.
   *
   * @return @c true if this DefaultValues's "enableRotationalMapping"
   * attribute has been set, otherwise @c false is returned.
   */
  bool isSetEnableRotationalMapping() const;


  /**
   * Sets the value of the "backgroundColor" attribute of this DefaultValues.
   *
   * @param backgroundColor std::string& value of the "backgroundColor"
   * attribute to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * Calling this function with @p backgroundColor = @c NULL or an empty string
   * is equivalent to calling unsetBackgroundColor().
   */
  int setBackgroundColor(const std::string& backgroundColor);


  /**
   * Sets the value of the "spreadMethod" attribute of this DefaultValues.
   *
   * @param spreadMethod @if clike GradientBase::SPREADMETHOD@else int@endif value
   * of the "spreadMethod" attribute to be set.
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
   * Sets the value of the "linearGradient_x1" attribute of this DefaultValues.
   *
   * @param linearGradient_x1 std::string& value of the "linearGradient_x1"
   * attribute to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * Calling this function with @p linearGradient_x1 = @c NULL or an empty
   * string is equivalent to calling unsetLinearGradient_x1().
   */
  int setLinearGradient_x1(const std::string& linearGradient_x1);


  /**
   * Sets the value of the "linearGradient_y1" attribute of this DefaultValues.
   *
   * @param linearGradient_y1 std::string& value of the "linearGradient_y1"
   * attribute to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * Calling this function with @p linearGradient_y1 = @c NULL or an empty
   * string is equivalent to calling unsetLinearGradient_y1().
   */
  int setLinearGradient_y1(const std::string& linearGradient_y1);


  /**
   * Sets the value of the "linearGradient_z1" attribute of this DefaultValues.
   *
   * @param linearGradient_z1 std::string& value of the "linearGradient_z1"
   * attribute to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * Calling this function with @p linearGradient_z1 = @c NULL or an empty
   * string is equivalent to calling unsetLinearGradient_z1().
   */
  int setLinearGradient_z1(const std::string& linearGradient_z1);


  /**
   * Sets the value of the "linearGradient_x2" attribute of this DefaultValues.
   *
   * @param linearGradient_x2 std::string& value of the "linearGradient_x2"
   * attribute to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * Calling this function with @p linearGradient_x2 = @c NULL or an empty
   * string is equivalent to calling unsetLinearGradient_x2().
   */
  int setLinearGradient_x2(const std::string& linearGradient_x2);


  /**
   * Sets the value of the "linearGradient_y2" attribute of this DefaultValues.
   *
   * @param linearGradient_y2 std::string& value of the "linearGradient_y2"
   * attribute to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * Calling this function with @p linearGradient_y2 = @c NULL or an empty
   * string is equivalent to calling unsetLinearGradient_y2().
   */
  int setLinearGradient_y2(const std::string& linearGradient_y2);


  /**
   * Sets the value of the "linearGradient_z2" attribute of this DefaultValues.
   *
   * @param linearGradient_z2 std::string& value of the "linearGradient_z2"
   * attribute to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * Calling this function with @p linearGradient_z2 = @c NULL or an empty
   * string is equivalent to calling unsetLinearGradient_z2().
   */
  int setLinearGradient_z2(const std::string& linearGradient_z2);


  /**
   * Sets the value of the "radialGradient_cx" attribute of this DefaultValues.
   *
   * @param radialGradient_cx std::string& value of the "radialGradient_cx"
   * attribute to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * Calling this function with @p radialGradient_cx = @c NULL or an empty
   * string is equivalent to calling unsetRadialGradient_cx().
   */
  int setRadialGradient_cx(const std::string& radialGradient_cx);


  /**
   * Sets the value of the "radialGradient_cy" attribute of this DefaultValues.
   *
   * @param radialGradient_cy std::string& value of the "radialGradient_cy"
   * attribute to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * Calling this function with @p radialGradient_cy = @c NULL or an empty
   * string is equivalent to calling unsetRadialGradient_cy().
   */
  int setRadialGradient_cy(const std::string& radialGradient_cy);


  /**
   * Sets the value of the "radialGradient_cz" attribute of this DefaultValues.
   *
   * @param radialGradient_cz std::string& value of the "radialGradient_cz"
   * attribute to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * Calling this function with @p radialGradient_cz = @c NULL or an empty
   * string is equivalent to calling unsetRadialGradient_cz().
   */
  int setRadialGradient_cz(const std::string& radialGradient_cz);


  /**
   * Sets the value of the "radialGradient_r" attribute of this DefaultValues.
   *
   * @param radialGradient_r std::string& value of the "radialGradient_r"
   * attribute to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * Calling this function with @p radialGradient_r = @c NULL or an empty
   * string is equivalent to calling unsetRadialGradient_r().
   */
  int setRadialGradient_r(const std::string& radialGradient_r);


  /**
   * Sets the value of the "radialGradient_fx" attribute of this DefaultValues.
   *
   * @param radialGradient_fx std::string& value of the "radialGradient_fx"
   * attribute to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * Calling this function with @p radialGradient_fx = @c NULL or an empty
   * string is equivalent to calling unsetRadialGradient_fx().
   */
  int setRadialGradient_fx(const std::string& radialGradient_fx);


  /**
   * Sets the value of the "radialGradient_fy" attribute of this DefaultValues.
   *
   * @param radialGradient_fy std::string& value of the "radialGradient_fy"
   * attribute to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * Calling this function with @p radialGradient_fy = @c NULL or an empty
   * string is equivalent to calling unsetRadialGradient_fy().
   */
  int setRadialGradient_fy(const std::string& radialGradient_fy);


  /**
   * Sets the value of the "radialGradient_fz" attribute of this DefaultValues.
   *
   * @param radialGradient_fz std::string& value of the "radialGradient_fz"
   * attribute to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * Calling this function with @p radialGradient_fz = @c NULL or an empty
   * string is equivalent to calling unsetRadialGradient_fz().
   */
  int setRadialGradient_fz(const std::string& radialGradient_fz);


  /**
   * Sets the value of the "fill" attribute of this DefaultValues.
   *
   * @param fill std::string& value of the "fill" attribute to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * Calling this function with @p fill = @c NULL or an empty string is
   * equivalent to calling unsetFill().
   */
  int setFill(const std::string& fill);


  /**
   * Sets the value of the "fillRule" attribute of this DefaultValues.
   *
   * @param fillRule @if clike GraphicalPrimitive2D::FILL_RULE@else int@endif value of the
   * "fillRule" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   *
   * @copydetails doc_defaultvalues_fillRule
   */
  int setFillRule(const GraphicalPrimitive2D::FILL_RULE fillRule);


  /**
  * Sets the value of the "fillRule" attribute of this DefaultValues.
  *
  * @param fillRule @if clike GraphicalPrimitive2D::FILL_RULE@else int@endif value of the
  * "fillRule" attribute to be set.
  *
  * @copydetails doc_returns_success_code
  * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
  * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
  * OperationReturnValues_t}
  *
  * @copydetails doc_defaultvalues_fillRule
  */
  int setFillRule(FillRule_t fillRule);


  /**
   * Sets the value of the "fillRule" attribute of this DefaultValues.
   *
   * @param fillRule std::string& of the "fillRule" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   *
   * @copydetails doc_defaultvalues_fillRule
   */
  int setFillRule(const std::string& fillRule);


  /**
   * Sets the value of the "default_z" attribute of this DefaultValues.
   *
   * @param default_z std::string& value of the "default_z" attribute to be
   * set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * Calling this function with @p default_z = @c NULL or an empty string is
   * equivalent to calling unsetDefault_z().
   */
  int setDefault_z(const std::string& default_z);


  /**
   * Sets the value of the "stroke" attribute of this DefaultValues.
   *
   * @param stroke std::string& value of the "stroke" attribute to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * Calling this function with @p stroke = @c NULL or an empty string is
   * equivalent to calling unsetStroke().
   */
  int setStroke(const std::string& stroke);


  /**
   * Sets the value of the "strokeWidth" attribute of this DefaultValues.
   *
   * @param strokeWidth std::string& value of the "strokeWidth" attribute to be
   * set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * Calling this function with @p strokeWidth = @c NULL or an empty string is
   * equivalent to calling unsetStrokeWidth().
   */
  int setStrokeWidth(double strokeWidth);


  /**
   * Sets the value of the "fontFamily" attribute of this DefaultValues.
   *
   * @param fontFamily std::string& value of the "fontFamily" attribute to be
   * set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * Calling this function with @p fontFamily = @c NULL or an empty string is
   * equivalent to calling unsetFontFamily().
   */
  int setFontFamily(const std::string& fontFamily);


  /**
   * Sets the value of the "fontSize" attribute of this DefaultValues.
   *
   * @param fontSize std::string& value of the "fontSize" attribute to be set.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * Calling this function with @p fontSize = @c NULL or an empty string is
   * equivalent to calling unsetFontSize().
   */
  int setFontSize(const std::string& fontSize);


  /**
   * Sets the value of the "fontWeight" attribute of this DefaultValues.
   *
   * @param fontWeight @if clike Text::FONT_WEIGHT@else int@endif value of the
   * "fontWeight" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   *
   * @copydetails doc_defaultvalues_fontWeight
   */
  int setFontWeight(const Text::FONT_WEIGHT fontWeight);


  /**
   * Sets the value of the "fontWeight" attribute of this DefaultValues.
   *
   * @param fontWeight std::string& of the "fontWeight" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   *
   * @copydetails doc_defaultvalues_fontWeight
   */
  int setFontWeight(const std::string& fontWeight);


  /**
   * Sets the value of the "fontStyle" attribute of this DefaultValues.
   *
   * @param fontStyle @if clike Text::FONT_STYLE@else int@endif value of the
   * "fontStyle" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   *
   * @copydetails doc_defaultvalues_fontStyle
   */
  int setFontStyle(const Text::FONT_STYLE fontStyle);


  /**
   * Sets the value of the "fontStyle" attribute of this DefaultValues.
   *
   * @param fontStyle std::string& of the "fontStyle" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   *
   * @copydetails doc_defaultvalues_fontStyle
   */
  int setFontStyle(const std::string& fontStyle);


  /**
   * Sets the value of the "textAnchor" attribute of this DefaultValues.
   *
   * @param textAnchor @if clike Text::TEXT_ANCHOR@else int@endif value of the
   * "textAnchor" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   *
   * @copydetails doc_defaultvalues_textAnchor
   */
  int setTextAnchor(const Text::TEXT_ANCHOR textAnchor);


  /**
   * Sets the value of the "textAnchor" attribute of this DefaultValues.
   *
   * @param textAnchor std::string& of the "textAnchor" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   *
   * @copydetails doc_defaultvalues_textAnchor
   */
  int setTextAnchor(const std::string& textAnchor);


  /**
   * Sets the value of the "vtextAnchor" attribute of this DefaultValues.
   *
   * @param vtextAnchor @if clike Text::TEXT_ANCHOR@else int@endif value of the
   * "vtextAnchor" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   *
   * @copydetails doc_defaultvalues_vtextAnchor
   */
  int setVTextAnchor(const Text::TEXT_ANCHOR vtextAnchor);


  /**
   * Sets the value of the "vtextAnchor" attribute of this DefaultValues.
   *
   * @param vtextAnchor std::string& of the "vtextAnchor" attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   *
   * @copydetails doc_defaultvalues_vtextAnchor
   */
  int setVTextAnchor(const std::string& vtextAnchor);


  /**
   * Sets the value of the "startHead" attribute of this DefaultValues.
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
   * Sets the value of the "endHead" attribute of this DefaultValues.
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
   * DefaultValues.
   *
   * @param enableRotationalMapping bool value of the "enableRotationalMapping"
   * attribute to be set.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_INVALID_ATTRIBUTE_VALUE,
   * OperationReturnValues_t}
   */
  int setEnableRotationalMapping(bool enableRotationalMapping);


  /**
   * Unsets the value of the "backgroundColor" attribute of this DefaultValues.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetBackgroundColor();


  /**
   * Unsets the value of the "spreadMethod" attribute of this DefaultValues.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * @copydetails doc_defaultvalues_spreadMethod
   */
  int unsetSpreadMethod();


  /**
   * Unsets the value of the "linearGradient_x1" attribute of this
   * DefaultValues.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetLinearGradient_x1();


  /**
   * Unsets the value of the "linearGradient_y1" attribute of this
   * DefaultValues.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetLinearGradient_y1();


  /**
   * Unsets the value of the "linearGradient_z1" attribute of this
   * DefaultValues.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetLinearGradient_z1();


  /**
   * Unsets the value of the "linearGradient_x2" attribute of this
   * DefaultValues.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetLinearGradient_x2();


  /**
   * Unsets the value of the "linearGradient_y2" attribute of this
   * DefaultValues.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetLinearGradient_y2();


  /**
   * Unsets the value of the "linearGradient_z2" attribute of this
   * DefaultValues.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetLinearGradient_z2();


  /**
   * Unsets the value of the "radialGradient_cx" attribute of this
   * DefaultValues.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetRadialGradient_cx();


  /**
   * Unsets the value of the "radialGradient_cy" attribute of this
   * DefaultValues.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetRadialGradient_cy();


  /**
   * Unsets the value of the "radialGradient_cz" attribute of this
   * DefaultValues.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetRadialGradient_cz();


  /**
   * Unsets the value of the "radialGradient_r" attribute of this
   * DefaultValues.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetRadialGradient_r();


  /**
   * Unsets the value of the "radialGradient_fx" attribute of this
   * DefaultValues.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetRadialGradient_fx();


  /**
   * Unsets the value of the "radialGradient_fy" attribute of this
   * DefaultValues.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetRadialGradient_fy();


  /**
   * Unsets the value of the "radialGradient_fz" attribute of this
   * DefaultValues.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetRadialGradient_fz();


  /**
   * Unsets the value of the "fill" attribute of this DefaultValues.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetFill();


  /**
   * Unsets the value of the "fillRule" attribute of this DefaultValues.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * @copydetails doc_defaultvalues_fillRule
   */
  int unsetFillRule();


  /**
   * Unsets the value of the "default_z" attribute of this DefaultValues.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetDefault_z();


  /**
   * Unsets the value of the "stroke" attribute of this DefaultValues.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetStroke();


  /**
   * Unsets the value of the "strokeWidth" attribute of this DefaultValues.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetStrokeWidth();


  /**
   * Unsets the value of the "fontFamily" attribute of this DefaultValues.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetFontFamily();


  /**
   * Unsets the value of the "fontSize" attribute of this DefaultValues.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetFontSize();


  /**
   * Unsets the value of the "fontWeight" attribute of this DefaultValues.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * @copydetails doc_defaultvalues_fontWeight
   */
  int unsetFontWeight();


  /**
   * Unsets the value of the "fontStyle" attribute of this DefaultValues.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * @copydetails doc_defaultvalues_fontStyle
   */
  int unsetFontStyle();


  /**
   * Unsets the value of the "textAnchor" attribute of this DefaultValues.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * @copydetails doc_defaultvalues_textAnchor
   */
  int unsetTextAnchor();


  /**
   * Unsets the value of the "vtextAnchor" attribute of this DefaultValues.
   *
   * @copydetails doc_returns_one_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   *
   * @copydetails doc_defaultvalues_vtextAnchor
   */
  int unsetVTextAnchor();


  /**
   * Unsets the value of the "startHead" attribute of this DefaultValues.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetStartHead();


  /**
   * Unsets the value of the "endHead" attribute of this DefaultValues.
   *
   * @copydetails doc_returns_success_code
   * @li @sbmlconstant{LIBSBML_OPERATION_SUCCESS, OperationReturnValues_t}
   * @li @sbmlconstant{LIBSBML_OPERATION_FAILED, OperationReturnValues_t}
   */
  int unsetEndHead();


  /**
   * Unsets the value of the "enableRotationalMapping" attribute of this
   * DefaultValues.
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
   * For DefaultValues, the XML element name is always @c "defaultValues".
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
   * DefaultValues have been set, otherwise @c false is returned.
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
   * Gets the value of the "attributeName" attribute of this DefaultValues.
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
   * Gets the value of the "attributeName" attribute of this DefaultValues.
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
   * Gets the value of the "attributeName" attribute of this DefaultValues.
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
   * Gets the value of the "attributeName" attribute of this DefaultValues.
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
   * Gets the value of the "attributeName" attribute of this DefaultValues.
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
   * Predicate returning @c true if this DefaultValues's attribute
   * "attributeName" is set.
   *
   * @param attributeName, the name of the attribute to query.
   *
   * @return @c true if this DefaultValues's attribute "attributeName" has been
   * set, otherwise @c false is returned.
   */
  virtual bool isSetAttribute(const std::string& attributeName) const;

  /** @endcond */



  /** @cond doxygenLibsbmlInternal */

  /**
   * Sets the value of the "attributeName" attribute of this DefaultValues.
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
   * Sets the value of the "attributeName" attribute of this DefaultValues.
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
   * Sets the value of the "attributeName" attribute of this DefaultValues.
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
   * Sets the value of the "attributeName" attribute of this DefaultValues.
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
   * Sets the value of the "attributeName" attribute of this DefaultValues.
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
   * Unsets the value of the "attributeName" attribute of this DefaultValues.
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




END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SWIG */




#endif /* !DefaultValues_H__ */


