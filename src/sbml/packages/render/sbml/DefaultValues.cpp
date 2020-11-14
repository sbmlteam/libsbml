/**
 * @file DefaultValues.cpp
 * @brief Implementation of the DefaultValues class.
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
 */
#include <sbml/packages/render/sbml/DefaultValues.h>


using namespace std;



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


/*
 * Creates a new DefaultValues using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 */
DefaultValues::DefaultValues(unsigned int level,
                             unsigned int version,
                             unsigned int pkgVersion)
  : SBase(level, version)
  , mBackgroundColor ("#FFFFFFFF")
  , mSpreadMethod (GRADIENT_SPREADMETHOD_PAD)
  , mLinearGradient_x1 (RelAbsVector(0.0, 0.0))
  , mLinearGradient_y1 (RelAbsVector(0.0, 0.0))
  , mLinearGradient_z1 (RelAbsVector(0.0, 0.0))
  , mLinearGradient_x2 (RelAbsVector(0.0, 100.0))
  , mLinearGradient_y2 (RelAbsVector(0.0, 100.0))
  , mLinearGradient_z2 (RelAbsVector(0.0, 100.0))
  , mRadialGradient_cx (RelAbsVector(0.0, 50.0))
  , mRadialGradient_cy (RelAbsVector(0.0, 50.0))
  , mRadialGradient_cz (RelAbsVector(0.0, 50.0))
  , mRadialGradient_r (RelAbsVector(0.0, 50.0))
  , mRadialGradient_fx (RelAbsVector(0.0, 50.0))
  , mRadialGradient_fy (RelAbsVector(0.0, 50.0))
  , mRadialGradient_fz (RelAbsVector(0.0, 50.0))
  , mFill ("none")
  , mFillRule (FILL_RULE_NONZERO)
  , mDefault_z (RelAbsVector(0.0, 0.0))
  , mStroke ("none")
  , mStrokeWidth (0)
  , mIsSetStrokeWidth(false)
  , mFontFamily ("sans-serif")
  , mFontSize (RelAbsVector(0.0, 0.0))
  , mFontWeight (FONT_WEIGHT_NORMAL)
  , mFontStyle (FONT_STYLE_NORMAL)
  , mTextAnchor (H_TEXTANCHOR_START)
  , mVTextAnchor (V_TEXTANCHOR_TOP)
  , mStartHead ("")
  , mEndHead ("")
  , mEnableRotationalMapping (true)
  , mIsSetEnableRotationalMapping (true)
{
  setSBMLNamespacesAndOwn(new RenderPkgNamespaces(level, version, pkgVersion));
  connectToChild();
}


/*
 * Creates a new DefaultValues using the given RenderPkgNamespaces object.
 */
DefaultValues::DefaultValues(RenderPkgNamespaces *renderns)
  : SBase(renderns)
  , mBackgroundColor("#FFFFFFFF")
  , mSpreadMethod(GRADIENT_SPREADMETHOD_PAD)
  , mLinearGradient_x1(RelAbsVector(0.0, 0.0))
  , mLinearGradient_y1(RelAbsVector(0.0, 0.0))
  , mLinearGradient_z1(RelAbsVector(0.0, 0.0))
  , mLinearGradient_x2(RelAbsVector(0.0, 100.0))
  , mLinearGradient_y2(RelAbsVector(0.0, 100.0))
  , mLinearGradient_z2(RelAbsVector(0.0, 100.0))
  , mRadialGradient_cx(RelAbsVector(0.0, 50.0))
  , mRadialGradient_cy(RelAbsVector(0.0, 50.0))
  , mRadialGradient_cz(RelAbsVector(0.0, 50.0))
  , mRadialGradient_r(RelAbsVector(0.0, 50.0))
  , mRadialGradient_fx(RelAbsVector(0.0, 50.0))
  , mRadialGradient_fy(RelAbsVector(0.0, 50.0))
  , mRadialGradient_fz(RelAbsVector(0.0, 50.0))
  , mFill("none")
  , mFillRule(FILL_RULE_NONZERO)
  , mDefault_z(RelAbsVector(0.0, 0.0))
  , mStroke("none")
  , mStrokeWidth(0)
  , mIsSetStrokeWidth(false)
  , mFontFamily("sans-serif")
  , mFontSize(RelAbsVector(0.0, 0.0))
  , mFontWeight(FONT_WEIGHT_NORMAL)
  , mFontStyle(FONT_STYLE_NORMAL)
  , mTextAnchor(H_TEXTANCHOR_START)
  , mVTextAnchor(V_TEXTANCHOR_TOP)
  , mStartHead("")
  , mEndHead("")
  , mEnableRotationalMapping(true)
  , mIsSetEnableRotationalMapping(true)
{
  setElementNamespace(renderns->getURI());
  connectToChild();
  loadPlugins(renderns);
}


/*
 * Copy constructor for DefaultValues.
 */
DefaultValues::DefaultValues(const DefaultValues& orig)
  : SBase( orig )
  , mBackgroundColor ( orig.mBackgroundColor )
  , mSpreadMethod ( orig.mSpreadMethod )
  , mLinearGradient_x1 ( orig.mLinearGradient_x1 )
  , mLinearGradient_y1 ( orig.mLinearGradient_y1 )
  , mLinearGradient_z1 ( orig.mLinearGradient_z1 )
  , mLinearGradient_x2 ( orig.mLinearGradient_x2 )
  , mLinearGradient_y2 ( orig.mLinearGradient_y2 )
  , mLinearGradient_z2 ( orig.mLinearGradient_z2 )
  , mRadialGradient_cx ( orig.mRadialGradient_cx )
  , mRadialGradient_cy ( orig.mRadialGradient_cy )
  , mRadialGradient_cz ( orig.mRadialGradient_cz )
  , mRadialGradient_r ( orig.mRadialGradient_r )
  , mRadialGradient_fx ( orig.mRadialGradient_fx )
  , mRadialGradient_fy ( orig.mRadialGradient_fy )
  , mRadialGradient_fz ( orig.mRadialGradient_fz )
  , mFill ( orig.mFill )
  , mFillRule ( orig.mFillRule )
  , mDefault_z ( orig.mDefault_z )
  , mStroke ( orig.mStroke )
  , mStrokeWidth ( orig.mStrokeWidth )
  , mIsSetStrokeWidth(orig.mIsSetStrokeWidth)
  , mFontFamily(orig.mFontFamily)
  , mFontSize ( orig.mFontSize )
  , mFontWeight ( orig.mFontWeight )
  , mFontStyle ( orig.mFontStyle )
  , mTextAnchor ( orig.mTextAnchor )
  , mVTextAnchor ( orig.mVTextAnchor )
  , mStartHead ( orig.mStartHead )
  , mEndHead ( orig.mEndHead )
  , mEnableRotationalMapping ( orig.mEnableRotationalMapping )
  , mIsSetEnableRotationalMapping ( orig.mIsSetEnableRotationalMapping )
{
}


/*
 * Assignment operator for DefaultValues.
 */
DefaultValues&
DefaultValues::operator=(const DefaultValues& rhs)
{
  if (&rhs != this)
  {
    SBase::operator=(rhs);
    mBackgroundColor = rhs.mBackgroundColor;
    mSpreadMethod = rhs.mSpreadMethod;
    mLinearGradient_x1 = rhs.mLinearGradient_x1;
    mLinearGradient_y1 = rhs.mLinearGradient_y1;
    mLinearGradient_z1 = rhs.mLinearGradient_z1;
    mLinearGradient_x2 = rhs.mLinearGradient_x2;
    mLinearGradient_y2 = rhs.mLinearGradient_y2;
    mLinearGradient_z2 = rhs.mLinearGradient_z2;
    mRadialGradient_cx = rhs.mRadialGradient_cx;
    mRadialGradient_cy = rhs.mRadialGradient_cy;
    mRadialGradient_cz = rhs.mRadialGradient_cz;
    mRadialGradient_r = rhs.mRadialGradient_r;
    mRadialGradient_fx = rhs.mRadialGradient_fx;
    mRadialGradient_fy = rhs.mRadialGradient_fy;
    mRadialGradient_fz = rhs.mRadialGradient_fz;
    mFill = rhs.mFill;
    mFillRule = rhs.mFillRule;
    mDefault_z = rhs.mDefault_z;
    mStroke = rhs.mStroke;
    mStrokeWidth = rhs.mStrokeWidth;
    mIsSetStrokeWidth = rhs.mIsSetStrokeWidth;
    mFontFamily = rhs.mFontFamily;
    mFontSize = rhs.mFontSize;
    mFontWeight = rhs.mFontWeight;
    mFontStyle = rhs.mFontStyle;
    mTextAnchor = rhs.mTextAnchor;
    mVTextAnchor = rhs.mVTextAnchor;
    mStartHead = rhs.mStartHead;
    mEndHead = rhs.mEndHead;
    mEnableRotationalMapping = rhs.mEnableRotationalMapping;
    mIsSetEnableRotationalMapping = rhs.mIsSetEnableRotationalMapping;
  }

  return *this;
}


/*
 * Creates and returns a deep copy of this DefaultValues object.
 */
DefaultValues*
DefaultValues::clone() const
{
  return new DefaultValues(*this);
}


/*
 * Destructor for DefaultValues.
 */
DefaultValues::~DefaultValues()
{
}


/*
 * Returns the value of the "backgroundColor" attribute of this DefaultValues.
 */
const std::string&
DefaultValues::getBackgroundColor() const
{
  return mBackgroundColor;
}


/*
 * Returns the value of the "spreadMethod" attribute of this DefaultValues.
 */
GradientSpreadMethod_t
DefaultValues::getSpreadMethod() const
{
  return (GradientSpreadMethod_t)(mSpreadMethod);
}


/*
 * Returns the value of the "spreadMethod" attribute of this DefaultValues.
 */
std::string
DefaultValues::getSpreadMethodAsString() const
{
  std::string code_str =
    GradientSpreadMethod_toString((GradientSpreadMethod_t)(mSpreadMethod));
  return code_str;
}


/*
 * Returns the value of the "linearGradient_x1" attribute of this
 * DefaultValues.
 */
const RelAbsVector&
DefaultValues::getLinearGradient_x1() const
{
  return mLinearGradient_x1;
}


/*
 * Returns the value of the "linearGradient_y1" attribute of this
 * DefaultValues.
 */
const RelAbsVector&
DefaultValues::getLinearGradient_y1() const
{
  return mLinearGradient_y1;
}


/*
 * Returns the value of the "linearGradient_z1" attribute of this
 * DefaultValues.
 */
const RelAbsVector&
DefaultValues::getLinearGradient_z1() const
{
  return mLinearGradient_z1;
}


/*
 * Returns the value of the "linearGradient_x2" attribute of this
 * DefaultValues.
 */
const RelAbsVector&
DefaultValues::getLinearGradient_x2() const
{
  return mLinearGradient_x2;
}


/*
 * Returns the value of the "linearGradient_y2" attribute of this
 * DefaultValues.
 */
const RelAbsVector&
DefaultValues::getLinearGradient_y2() const
{
  return mLinearGradient_y2;
}


/*
 * Returns the value of the "linearGradient_z2" attribute of this
 * DefaultValues.
 */
const RelAbsVector&
DefaultValues::getLinearGradient_z2() const
{
  return mLinearGradient_z2;
}


/*
 * Returns the value of the "radialGradient_cx" attribute of this
 * DefaultValues.
 */
const RelAbsVector&
DefaultValues::getRadialGradient_cx() const
{
  return mRadialGradient_cx;
}


/*
 * Returns the value of the "radialGradient_cy" attribute of this
 * DefaultValues.
 */
const RelAbsVector&
DefaultValues::getRadialGradient_cy() const
{
  return mRadialGradient_cy;
}


/*
 * Returns the value of the "radialGradient_cz" attribute of this
 * DefaultValues.
 */
const RelAbsVector&
DefaultValues::getRadialGradient_cz() const
{
  return mRadialGradient_cz;
}


/*
 * Returns the value of the "radialGradient_r" attribute of this DefaultValues.
 */
const RelAbsVector&
DefaultValues::getRadialGradient_r() const
{
  return mRadialGradient_r;
}


/*
 * Returns the value of the "radialGradient_fx" attribute of this
 * DefaultValues.
 */
const RelAbsVector&
DefaultValues::getRadialGradient_fx() const
{
  return mRadialGradient_fx;
}


/*
 * Returns the value of the "radialGradient_fy" attribute of this
 * DefaultValues.
 */
const RelAbsVector&
DefaultValues::getRadialGradient_fy() const
{
  return mRadialGradient_fy;
}


/*
 * Returns the value of the "radialGradient_fz" attribute of this
 * DefaultValues.
 */
const RelAbsVector&
DefaultValues::getRadialGradient_fz() const
{
  return mRadialGradient_fz;
}


/*
 * Returns the value of the "fill" attribute of this DefaultValues.
 */
const std::string&
DefaultValues::getFill() const
{
  return mFill;
}


/*
 * Returns the value of the "fill-rule" attribute of this DefaultValues.
 */
int
DefaultValues::getFillRule() const
{
  return mFillRule;
}


/*
 * Returns the value of the "fill-rule" attribute of this DefaultValues.
 */
std::string
DefaultValues::getFillRuleAsString() const
{
  std::string code_str = FillRule_toString((FillRule_t)(mFillRule));
  return code_str;
}


/*
 * Returns the value of the "default_z" attribute of this DefaultValues.
 */
const RelAbsVector&
DefaultValues::getDefault_z() const
{
  return mDefault_z;
}


/*
 * Returns the value of the "stroke" attribute of this DefaultValues.
 */
const std::string&
DefaultValues::getStroke() const
{
  return mStroke;
}


/*
 * Returns the value of the "stroke-width" attribute of this DefaultValues.
 */
double
DefaultValues::getStrokeWidth() const
{
  return mStrokeWidth;
}


/*
 * Returns the value of the "font-family" attribute of this DefaultValues.
 */
const std::string&
DefaultValues::getFontFamily() const
{
  return mFontFamily;
}


/*
 * Returns the value of the "font-size" attribute of this DefaultValues.
 */
const RelAbsVector&
DefaultValues::getFontSize() const
{
  return mFontSize;
}


/*
 * Returns the value of the "font-weight" attribute of this DefaultValues.
 */
FontWeight_t
DefaultValues::getFontWeight() const
{
  return (FontWeight_t)(mFontWeight);
}


/*
 * Returns the value of the "font-weight" attribute of this DefaultValues.
 */
std::string
DefaultValues::getFontWeightAsString() const
{
  std::string code_str = FontWeight_toString((FontWeight_t)(mFontWeight));
  return code_str;
}


/*
 * Returns the value of the "font-style" attribute of this DefaultValues.
 */
FontStyle_t
DefaultValues::getFontStyle() const
{
  return (FontStyle_t)(mFontStyle);
}


/*
 * Returns the value of the "font-style" attribute of this DefaultValues.
 */
std::string
DefaultValues::getFontStyleAsString() const
{
  std::string code_str = FontStyle_toString((FontStyle_t)(mFontStyle));
  return code_str;
}


/*
 * Returns the value of the "text-anchor" attribute of this DefaultValues.
 */
HTextAnchor_t
DefaultValues::getTextAnchor() const
{
  return (HTextAnchor_t)(mTextAnchor);
}


/*
 * Returns the value of the "text-anchor" attribute of this DefaultValues.
 */
std::string
DefaultValues::getTextAnchorAsString() const
{
  std::string code_str = HTextAnchor_toString((HTextAnchor_t)(mTextAnchor));
  return code_str;
}


/*
 * Returns the value of the "vtext-anchor" attribute of this DefaultValues.
 */
VTextAnchor_t
DefaultValues::getVTextAnchor() const
{
  return (VTextAnchor_t)(mVTextAnchor);
}


/*
 * Returns the value of the "vtext-anchor" attribute of this DefaultValues.
 */
std::string
DefaultValues::getVTextAnchorAsString() const
{
  std::string code_str = VTextAnchor_toString((VTextAnchor_t)(mVTextAnchor));
  return code_str;
}


/*
 * Returns the value of the "startHead" attribute of this DefaultValues.
 */
const std::string&
DefaultValues::getStartHead() const
{
  return mStartHead;
}


/*
 * Returns the value of the "endHead" attribute of this DefaultValues.
 */
const std::string&
DefaultValues::getEndHead() const
{
  return mEndHead;
}


/*
 * Returns the value of the "enableRotationalMapping" attribute of this
 * DefaultValues.
 */
bool
DefaultValues::getEnableRotationalMapping() const
{
  return mEnableRotationalMapping;
}


/*
 * Predicate returning @c true if this DefaultValues's "backgroundColor"
 * attribute is set.
 */
bool
DefaultValues::isSetBackgroundColor() const
{
  return (mBackgroundColor.empty() == false);
}


/*
 * Predicate returning @c true if this DefaultValues's "spreadMethod" attribute
 * is set.
 */
bool
DefaultValues::isSetSpreadMethod() const
{
  return (mSpreadMethod != GradientBase::INVALID);
}


/*
 * Predicate returning @c true if this DefaultValues's "linearGradient_x1"
 * attribute is set.
 */
bool
DefaultValues::isSetLinearGradient_x1() const
{
  return (mLinearGradient_x1.empty() == false);
}


/*
 * Predicate returning @c true if this DefaultValues's "linearGradient_y1"
 * attribute is set.
 */
bool
DefaultValues::isSetLinearGradient_y1() const
{
  return (mLinearGradient_y1.empty() == false);
}


/*
 * Predicate returning @c true if this DefaultValues's "linearGradient_z1"
 * attribute is set.
 */
bool
DefaultValues::isSetLinearGradient_z1() const
{
  return (mLinearGradient_z1.empty() == false);
}


/*
 * Predicate returning @c true if this DefaultValues's "linearGradient_x2"
 * attribute is set.
 */
bool
DefaultValues::isSetLinearGradient_x2() const
{
  return (mLinearGradient_x2.empty() == false);
}


/*
 * Predicate returning @c true if this DefaultValues's "linearGradient_y2"
 * attribute is set.
 */
bool
DefaultValues::isSetLinearGradient_y2() const
{
  return (mLinearGradient_y2.empty() == false);
}


/*
 * Predicate returning @c true if this DefaultValues's "linearGradient_z2"
 * attribute is set.
 */
bool
DefaultValues::isSetLinearGradient_z2() const
{
  return (mLinearGradient_z2.empty() == false);
}


/*
 * Predicate returning @c true if this DefaultValues's "radialGradient_cx"
 * attribute is set.
 */
bool
DefaultValues::isSetRadialGradient_cx() const
{
  return (mRadialGradient_cx.empty() == false);
}


/*
 * Predicate returning @c true if this DefaultValues's "radialGradient_cy"
 * attribute is set.
 */
bool
DefaultValues::isSetRadialGradient_cy() const
{
  return (mRadialGradient_cy.empty() == false);
}


/*
 * Predicate returning @c true if this DefaultValues's "radialGradient_cz"
 * attribute is set.
 */
bool
DefaultValues::isSetRadialGradient_cz() const
{
  return (mRadialGradient_cz.empty() == false);
}


/*
 * Predicate returning @c true if this DefaultValues's "radialGradient_r"
 * attribute is set.
 */
bool
DefaultValues::isSetRadialGradient_r() const
{
  return (mRadialGradient_r.empty() == false);
}


/*
 * Predicate returning @c true if this DefaultValues's "radialGradient_fx"
 * attribute is set.
 */
bool
DefaultValues::isSetRadialGradient_fx() const
{
  return (mRadialGradient_fx.empty() == false);
}


/*
 * Predicate returning @c true if this DefaultValues's "radialGradient_fy"
 * attribute is set.
 */
bool
DefaultValues::isSetRadialGradient_fy() const
{
  return (mRadialGradient_fy.empty() == false);
}


/*
 * Predicate returning @c true if this DefaultValues's "radialGradient_fz"
 * attribute is set.
 */
bool
DefaultValues::isSetRadialGradient_fz() const
{
  return (mRadialGradient_fz.empty() == false);
}


/*
 * Predicate returning @c true if this DefaultValues's "fill" attribute is set.
 */
bool
DefaultValues::isSetFill() const
{
  return (mFill.empty() == false);
}


/*
 * Predicate returning @c true if this DefaultValues's "fill-rule" attribute is
 * set.
 */
bool
DefaultValues::isSetFillRule() const
{
  return (mFillRule != GraphicalPrimitive2D::INVALID);
}


/*
 * Predicate returning @c true if this DefaultValues's "default_z" attribute is
 * set.
 */
bool
DefaultValues::isSetDefault_z() const
{
  return (mDefault_z.empty() == false);
}


/*
 * Predicate returning @c true if this DefaultValues's "stroke" attribute is
 * set.
 */
bool
DefaultValues::isSetStroke() const
{
  return (mStroke.empty() == false);
}


/*
 * Predicate returning @c true if this DefaultValues's "stroke-width" attribute
 * is set.
 */
bool
DefaultValues::isSetStrokeWidth() const
{
  return mIsSetStrokeWidth;
}


/*
 * Predicate returning @c true if this DefaultValues's "font-family" attribute
 * is set.
 */
bool
DefaultValues::isSetFontFamily() const
{
  return (mFontFamily.empty() == false);
}


/*
 * Predicate returning @c true if this DefaultValues's "font-size" attribute is
 * set.
 */
bool
DefaultValues::isSetFontSize() const
{
  return (mFontSize.empty() == false);
}


/*
 * Predicate returning @c true if this DefaultValues's "font-weight" attribute
 * is set.
 */
bool
DefaultValues::isSetFontWeight() const
{
  return (mFontWeight != Text::WEIGHT_INVALID );
}


/*
 * Predicate returning @c true if this DefaultValues's "font-style" attribute is
 * set.
 */
bool
DefaultValues::isSetFontStyle() const
{
  return (mFontStyle != Text::STYLE_INVALID);
}


/*
 * Predicate returning @c true if this DefaultValues's "text-anchor" attribute
 * is set.
 */
bool
DefaultValues::isSetTextAnchor() const
{
  return (mTextAnchor != Text::ANCHOR_INVALID);
}


/*
 * Predicate returning @c true if this DefaultValues's "vtext-anchor" attribute
 * is set.
 */
bool
DefaultValues::isSetVTextAnchor() const
{
  return (mVTextAnchor != Text::ANCHOR_INVALID);
}


/*
 * Predicate returning @c true if this DefaultValues's "startHead" attribute is
 * set.
 */
bool
DefaultValues::isSetStartHead() const
{
  return (mStartHead.empty() == false);
}


/*
 * Predicate returning @c true if this DefaultValues's "endHead" attribute is
 * set.
 */
bool
DefaultValues::isSetEndHead() const
{
  return (mEndHead.empty() == false);
}


/*
 * Predicate returning @c true if this DefaultValues's
 * "enableRotationalMapping" attribute is set.
 */
bool
DefaultValues::isSetEnableRotationalMapping() const
{
  return mIsSetEnableRotationalMapping;
}


/*
 * Sets the value of the "backgroundColor" attribute of this DefaultValues.
 */
int
DefaultValues::setBackgroundColor(const std::string& backgroundColor)
{
  mBackgroundColor = backgroundColor;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "spreadMethod" attribute of this DefaultValues.
 */
int
DefaultValues::setSpreadMethod(const GradientBase::SPREADMETHOD spreadMethod)
{
  mSpreadMethod = spreadMethod;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "spreadMethod" attribute of this DefaultValues.
 */
int
DefaultValues::setSpreadMethod(const GradientSpreadMethod_t spreadMethod)
{
  if (GradientSpreadMethod_isValid(spreadMethod) == 0)
  {
    mSpreadMethod = GRADIENT_SPREAD_METHOD_INVALID;
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mSpreadMethod = spreadMethod;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "spreadMethod" attribute of this DefaultValues.
 */
int
DefaultValues::setSpreadMethod(const std::string& spreadMethod)
{
  mSpreadMethod = SpreadMethod_fromString(spreadMethod.c_str());
  if (mSpreadMethod == GradientBase::INVALID)
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "linearGradient_x1" attribute of this DefaultValues.
 */
int
DefaultValues::setLinearGradient_x1(const RelAbsVector& linearGradient_x1)
{
  mLinearGradient_x1 = linearGradient_x1;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "linearGradient_y1" attribute of this DefaultValues.
 */
int
DefaultValues::setLinearGradient_y1(const RelAbsVector& linearGradient_y1)
{
  mLinearGradient_y1 = linearGradient_y1;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "linearGradient_z1" attribute of this DefaultValues.
 */
int
DefaultValues::setLinearGradient_z1(const RelAbsVector& linearGradient_z1)
{
  mLinearGradient_z1 = linearGradient_z1;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "linearGradient_x2" attribute of this DefaultValues.
 */
int
DefaultValues::setLinearGradient_x2(const RelAbsVector& linearGradient_x2)
{
  mLinearGradient_x2 = linearGradient_x2;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "linearGradient_y2" attribute of this DefaultValues.
 */
int
DefaultValues::setLinearGradient_y2(const RelAbsVector& linearGradient_y2)
{
  mLinearGradient_y2 = linearGradient_y2;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "linearGradient_z2" attribute of this DefaultValues.
 */
int
DefaultValues::setLinearGradient_z2(const RelAbsVector& linearGradient_z2)
{
  mLinearGradient_z2 = linearGradient_z2;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "radialGradient_cx" attribute of this DefaultValues.
 */
int
DefaultValues::setRadialGradient_cx(const RelAbsVector& radialGradient_cx)
{
  mRadialGradient_cx = radialGradient_cx;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "radialGradient_cy" attribute of this DefaultValues.
 */
int
DefaultValues::setRadialGradient_cy(const RelAbsVector& radialGradient_cy)
{
  mRadialGradient_cy = radialGradient_cy;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "radialGradient_cz" attribute of this DefaultValues.
 */
int
DefaultValues::setRadialGradient_cz(const RelAbsVector& radialGradient_cz)
{
  mRadialGradient_cz = radialGradient_cz;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "radialGradient_r" attribute of this DefaultValues.
 */
int
DefaultValues::setRadialGradient_r(const RelAbsVector& radialGradient_r)
{
  mRadialGradient_r = radialGradient_r;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "radialGradient_fx" attribute of this DefaultValues.
 */
int
DefaultValues::setRadialGradient_fx(const RelAbsVector& radialGradient_fx)
{
  mRadialGradient_fx = radialGradient_fx;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "radialGradient_fy" attribute of this DefaultValues.
 */
int
DefaultValues::setRadialGradient_fy(const RelAbsVector& radialGradient_fy)
{
  mRadialGradient_fy = radialGradient_fy;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "radialGradient_fz" attribute of this DefaultValues.
 */
int
DefaultValues::setRadialGradient_fz(const RelAbsVector& radialGradient_fz)
{
  mRadialGradient_fz = radialGradient_fz;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "fill" attribute of this DefaultValues.
 */
int
DefaultValues::setFill(const std::string& fill)
{
  mFill = fill;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "fill-rule" attribute of this DefaultValues.
 */
int
DefaultValues::setFillRule(const GraphicalPrimitive2D::FILL_RULE fillRule)
{
    mFillRule = fillRule;
    return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "fill-rule" attribute of this DefaultValues.
 */
int
DefaultValues::setFillRule(const FillRule_t fillRule)
{
  if (FillRule_isValid(fillRule) == 0)
  {
    mFillRule = FILL_RULE_INVALID;
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }
  else
  {
    mFillRule = fillRule;
    return LIBSBML_OPERATION_SUCCESS;
  }
}


/*
 * Sets the value of the "fill-rule" attribute of this DefaultValues.
 */
int
DefaultValues::setFillRule(const std::string& fillRule)
{
  mFillRule = FillRule_fromString(fillRule.c_str());
  if (mFillRule == GraphicalPrimitive2D::INVALID)
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;

  return LIBSBML_OPERATION_SUCCESS;  
}


/*
 * Sets the value of the "default_z" attribute of this DefaultValues.
 */
int
DefaultValues::setDefault_z(const RelAbsVector& default_z)
{
  mDefault_z = default_z;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "stroke" attribute of this DefaultValues.
 */
int
DefaultValues::setStroke(const std::string& stroke)
{
  mStroke = stroke;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "stroke-width" attribute of this DefaultValues.
 */
int
DefaultValues::setStrokeWidth(double strokeWidth)
{
  mStrokeWidth = strokeWidth;
  mIsSetStrokeWidth = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "font-family" attribute of this DefaultValues.
 */
int
DefaultValues::setFontFamily(const std::string& fontFamily)
{
  mFontFamily = fontFamily;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "font-size" attribute of this DefaultValues.
 */
int
DefaultValues::setFontSize(const RelAbsVector& fontSize)
{
  mFontSize = fontSize;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "font-weight" attribute of this DefaultValues.
 */
int
DefaultValues::setFontWeight(const Text::FONT_WEIGHT fontWeight)
{
  mFontWeight = fontWeight;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "font-weight" attribute of this DefaultValues.
 */
int
DefaultValues::setFontWeight(const FontWeight_t fontWeight)
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


/*
 * Sets the value of the "font-weight" attribute of this DefaultValues.
 */
int
DefaultValues::setFontWeight(const std::string& fontWeight)
{
  mFontWeight = FontWeight_fromString(fontWeight.c_str());
  if (mFontWeight == Text::WEIGHT_INVALID)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }

  return LIBSBML_OPERATION_SUCCESS;  
}


/*
 * Sets the value of the "font-style" attribute of this DefaultValues.
 */
int
DefaultValues::setFontStyle(const Text::FONT_STYLE fontStyle)
{
    mFontStyle = fontStyle;
    return LIBSBML_OPERATION_SUCCESS; 
}


/*
 * Sets the value of the "font-style" attribute of this DefaultValues.
 */
int
DefaultValues::setFontStyle(const FontStyle_t fontStyle)
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

/*
 * Sets the value of the "font-style" attribute of this DefaultValues.
 */
int
DefaultValues::setFontStyle(const std::string& fontStyle)
{
  mFontStyle = FontStyle_fromString(fontStyle.c_str());
  if (mFontStyle == Text::STYLE_INVALID)
  {
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;
  }

  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "text-anchor" attribute of this DefaultValues.
 */
int
DefaultValues::setTextAnchor(const Text::TEXT_ANCHOR textAnchor)
{
  mTextAnchor = textAnchor;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "text-anchor" attribute of this DefaultValues.
 */
int
DefaultValues::setTextAnchor(const HTextAnchor_t textAnchor)
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


/*
 * Sets the value of the "text-anchor" attribute of this DefaultValues.
 */
int
DefaultValues::setTextAnchor(const std::string& textAnchor)
{
  mTextAnchor = TextAnchor_fromString(textAnchor.c_str());
  if (mTextAnchor == Text::ANCHOR_INVALID)
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;

  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "vtext-anchor" attribute of this DefaultValues.
 */
int
DefaultValues::setVTextAnchor(const Text::TEXT_ANCHOR vtextAnchor)
{
  mVTextAnchor = vtextAnchor;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "vtext-anchor" attribute of this DefaultValues.
 */
int
DefaultValues::setVTextAnchor(const VTextAnchor_t vtextAnchor)
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


/*
 * Sets the value of the "vtext-anchor" attribute of this DefaultValues.
 */
int
DefaultValues::setVTextAnchor(const std::string& vtextAnchor)
{
  mVTextAnchor = TextAnchor_fromString(vtextAnchor.c_str());
  if (mVTextAnchor == Text::ANCHOR_INVALID)
    return LIBSBML_INVALID_ATTRIBUTE_VALUE;

  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Sets the value of the "startHead" attribute of this DefaultValues.
 */
int
DefaultValues::setStartHead(const std::string& startHead)
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
 * Sets the value of the "endHead" attribute of this DefaultValues.
 */
int
DefaultValues::setEndHead(const std::string& endHead)
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
 * Sets the value of the "enableRotationalMapping" attribute of this
 * DefaultValues.
 */
int
DefaultValues::setEnableRotationalMapping(bool enableRotationalMapping)
{
  mEnableRotationalMapping = enableRotationalMapping;
  mIsSetEnableRotationalMapping = true;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "backgroundColor" attribute of this DefaultValues.
 */
int
DefaultValues::unsetBackgroundColor()
{
  mBackgroundColor.erase();

  if (mBackgroundColor.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "spreadMethod" attribute of this DefaultValues.
 */
int
DefaultValues::unsetSpreadMethod()
{
  mSpreadMethod = GradientBase::INVALID;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "linearGradient_x1" attribute of this DefaultValues.
 */
int
DefaultValues::unsetLinearGradient_x1()
{
  mLinearGradient_x1.erase();

  if (mLinearGradient_x1.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "linearGradient_y1" attribute of this DefaultValues.
 */
int
DefaultValues::unsetLinearGradient_y1()
{
  mLinearGradient_y1.erase();

  if (mLinearGradient_y1.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "linearGradient_z1" attribute of this DefaultValues.
 */
int
DefaultValues::unsetLinearGradient_z1()
{
  mLinearGradient_z1.erase();

  if (mLinearGradient_z1.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "linearGradient_x2" attribute of this DefaultValues.
 */
int
DefaultValues::unsetLinearGradient_x2()
{
  mLinearGradient_x2.erase();

  if (mLinearGradient_x2.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "linearGradient_y2" attribute of this DefaultValues.
 */
int
DefaultValues::unsetLinearGradient_y2()
{
  mLinearGradient_y2.erase();

  if (mLinearGradient_y2.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "linearGradient_z2" attribute of this DefaultValues.
 */
int
DefaultValues::unsetLinearGradient_z2()
{
  mLinearGradient_z2.erase();

  if (mLinearGradient_z2.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "radialGradient_cx" attribute of this DefaultValues.
 */
int
DefaultValues::unsetRadialGradient_cx()
{
  mRadialGradient_cx.erase();

  if (mRadialGradient_cx.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "radialGradient_cy" attribute of this DefaultValues.
 */
int
DefaultValues::unsetRadialGradient_cy()
{
  mRadialGradient_cy.erase();

  if (mRadialGradient_cy.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "radialGradient_cz" attribute of this DefaultValues.
 */
int
DefaultValues::unsetRadialGradient_cz()
{
  mRadialGradient_cz.erase();

  if (mRadialGradient_cz.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "radialGradient_r" attribute of this DefaultValues.
 */
int
DefaultValues::unsetRadialGradient_r()
{
  mRadialGradient_r.erase();

  if (mRadialGradient_r.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "radialGradient_fx" attribute of this DefaultValues.
 */
int
DefaultValues::unsetRadialGradient_fx()
{
  mRadialGradient_fx.erase();

  if (mRadialGradient_fx.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "radialGradient_fy" attribute of this DefaultValues.
 */
int
DefaultValues::unsetRadialGradient_fy()
{
  mRadialGradient_fy.erase();

  if (mRadialGradient_fy.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "radialGradient_fz" attribute of this DefaultValues.
 */
int
DefaultValues::unsetRadialGradient_fz()
{
  mRadialGradient_fz.erase();

  if (mRadialGradient_fz.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "fill" attribute of this DefaultValues.
 */
int
DefaultValues::unsetFill()
{
  mFill.erase();

  if (mFill.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "fill-rule" attribute of this DefaultValues.
 */
int
DefaultValues::unsetFillRule()
{
  mFillRule = GraphicalPrimitive2D::INVALID;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "default_z" attribute of this DefaultValues.
 */
int
DefaultValues::unsetDefault_z()
{
  mDefault_z.erase();

  if (mDefault_z.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "stroke" attribute of this DefaultValues.
 */
int
DefaultValues::unsetStroke()
{
  mStroke.erase();

  if (mStroke.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "stroke-width" attribute of this DefaultValues.
 */
int
DefaultValues::unsetStrokeWidth()
{
  mStrokeWidth = util_NaN();
  mIsSetStrokeWidth = false;

  if (isSetStrokeWidth() == false)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "font-family" attribute of this DefaultValues.
 */
int
DefaultValues::unsetFontFamily()
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
 * Unsets the value of the "font-size" attribute of this DefaultValues.
 */
int
DefaultValues::unsetFontSize()
{
  mFontSize.erase();

  if (mFontSize.empty() == true)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * Unsets the value of the "font-weight" attribute of this DefaultValues.
 */
int
DefaultValues::unsetFontWeight()
{
  mFontWeight = Text::WEIGHT_INVALID;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "font-style" attribute of this DefaultValues.
 */
int
DefaultValues::unsetFontStyle()
{
  mFontStyle = Text::STYLE_INVALID;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "text-anchor" attribute of this DefaultValues.
 */
int
DefaultValues::unsetTextAnchor()
{
  mTextAnchor = Text::ANCHOR_INVALID;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "vtext-anchor" attribute of this DefaultValues.
 */
int
DefaultValues::unsetVTextAnchor()
{
  mVTextAnchor = Text::ANCHOR_INVALID;
  return LIBSBML_OPERATION_SUCCESS;
}


/*
 * Unsets the value of the "startHead" attribute of this DefaultValues.
 */
int
DefaultValues::unsetStartHead()
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
 * Unsets the value of the "endHead" attribute of this DefaultValues.
 */
int
DefaultValues::unsetEndHead()
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
 * Unsets the value of the "enableRotationalMapping" attribute of this
 * DefaultValues.
 */
int
DefaultValues::unsetEnableRotationalMapping()
{
  mEnableRotationalMapping = false;
  mIsSetEnableRotationalMapping = false;

  if (isSetEnableRotationalMapping() == false)
  {
    return LIBSBML_OPERATION_SUCCESS;
  }
  else
  {
    return LIBSBML_OPERATION_FAILED;
  }
}


/*
 * @copydoc doc_renamesidref_common
 */
void
DefaultValues::renameSIdRefs(const std::string& oldid,
                             const std::string& newid)
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
 * Returns the XML element name of this DefaultValues object.
 */
const std::string&
DefaultValues::getElementName() const
{
  static const string name = "defaultValues";
  return name;
}


/*
 * Returns the libSBML type code for this DefaultValues object.
 */
int
DefaultValues::getTypeCode() const
{
  return SBML_RENDER_DEFAULTS;
}


/*
 * Predicate returning @c true if all the required attributes for this
 * DefaultValues object have been set.
 */
bool
DefaultValues::hasRequiredAttributes() const
{
  bool allPresent = true;

  return allPresent;
}



/** @cond doxygenLibsbmlInternal */

/*
 * Write any contained elements
 */
void
DefaultValues::writeElements(XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

  SBase::writeExtensionElements(stream);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Accepts the given SBMLVisitor
 */
bool
DefaultValues::accept(SBMLVisitor& v) const
{
  return v.visit(*this);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the parent SBMLDocument
 */
void
DefaultValues::setSBMLDocument(SBMLDocument* d)
{
  SBase::setSBMLDocument(d);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Enables/disables the given package with this element
 */
void
DefaultValues::enablePackageInternal(const std::string& pkgURI,
                                     const std::string& pkgPrefix,
                                     bool flag)
{
  SBase::enablePackageInternal(pkgURI, pkgPrefix, flag);
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this DefaultValues.
 */
int
DefaultValues::getAttribute(const std::string& attributeName,
                            bool& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "enableRotationalMapping")
  {
    value = getEnableRotationalMapping();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this DefaultValues.
 */
int
DefaultValues::getAttribute(const std::string& attributeName,
                            int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this DefaultValues.
 */
int
DefaultValues::getAttribute(const std::string& attributeName,
                            double& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  if (attributeName == "stroke-width")
  {
    value = getStrokeWidth();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this DefaultValues.
 */
int
DefaultValues::getAttribute(const std::string& attributeName,
                            unsigned int& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Returns the value of the "attributeName" attribute of this DefaultValues.
 */
int
DefaultValues::getAttribute(const std::string& attributeName,
                            std::string& value) const
{
  int return_value = SBase::getAttribute(attributeName, value);

  if (return_value == LIBSBML_OPERATION_SUCCESS)
  {
    return return_value;
  }

  if (attributeName == "backgroundColor")
  {
    value = getBackgroundColor();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "spreadMethod")
  {
    value = getSpreadMethodAsString();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "linearGradient_x1")
  {
    value = getLinearGradient_x1().toString();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "linearGradient_y1")
  {
    value = getLinearGradient_y1().toString();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "linearGradient_z1")
  {
    value = getLinearGradient_z1().toString();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "linearGradient_x2")
  {
    value = getLinearGradient_x2().toString();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "linearGradient_y2")
  {
    value = getLinearGradient_y2().toString();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "linearGradient_z2")
  {
    value = getLinearGradient_z2().toString();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "radialGradient_cx")
  {
    value = getRadialGradient_cx().toString();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "radialGradient_cy")
  {
    value = getRadialGradient_cy().toString();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "radialGradient_cz")
  {
    value = getRadialGradient_cz().toString();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "radialGradient_r")
  {
    value = getRadialGradient_r().toString();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "radialGradient_fx")
  {
    value = getRadialGradient_fx().toString();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "radialGradient_fy")
  {
    value = getRadialGradient_fy().toString();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "radialGradient_fz")
  {
    value = getRadialGradient_fz().toString();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "fill")
  {
    value = getFill();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "fill-rule")
  {
    value = getFillRuleAsString();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "default_z")
  {
    value = getDefault_z().toString();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "stroke")
  {
    value = getStroke();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "font-family")
  {
    value = getFontFamily();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "font-size")
  {
    value = getFontSize().toString();
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
  else if (attributeName == "startHead")
  {
    value = getStartHead();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }
  else if (attributeName == "endHead")
  {
    value = getEndHead();
    return_value = LIBSBML_OPERATION_SUCCESS;
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Predicate returning @c true if this DefaultValues's attribute
 * "attributeName" is set.
 */
bool
DefaultValues::isSetAttribute(const std::string& attributeName) const
{
  bool value = SBase::isSetAttribute(attributeName);

  if (attributeName == "backgroundColor")
  {
    value = isSetBackgroundColor();
  }
  else if (attributeName == "spreadMethod")
  {
    value = isSetSpreadMethod();
  }
  else if (attributeName == "linearGradient_x1")
  {
    value = isSetLinearGradient_x1();
  }
  else if (attributeName == "linearGradient_y1")
  {
    value = isSetLinearGradient_y1();
  }
  else if (attributeName == "linearGradient_z1")
  {
    value = isSetLinearGradient_z1();
  }
  else if (attributeName == "linearGradient_x2")
  {
    value = isSetLinearGradient_x2();
  }
  else if (attributeName == "linearGradient_y2")
  {
    value = isSetLinearGradient_y2();
  }
  else if (attributeName == "linearGradient_z2")
  {
    value = isSetLinearGradient_z2();
  }
  else if (attributeName == "radialGradient_cx")
  {
    value = isSetRadialGradient_cx();
  }
  else if (attributeName == "radialGradient_cy")
  {
    value = isSetRadialGradient_cy();
  }
  else if (attributeName == "radialGradient_cz")
  {
    value = isSetRadialGradient_cz();
  }
  else if (attributeName == "radialGradient_r")
  {
    value = isSetRadialGradient_r();
  }
  else if (attributeName == "radialGradient_fx")
  {
    value = isSetRadialGradient_fx();
  }
  else if (attributeName == "radialGradient_fy")
  {
    value = isSetRadialGradient_fy();
  }
  else if (attributeName == "radialGradient_fz")
  {
    value = isSetRadialGradient_fz();
  }
  else if (attributeName == "fill")
  {
    value = isSetFill();
  }
  else if (attributeName == "fill-rule")
  {
    value = isSetFillRule();
  }
  else if (attributeName == "default_z")
  {
    value = isSetDefault_z();
  }
  else if (attributeName == "stroke")
  {
    value = isSetStroke();
  }
  else if (attributeName == "stroke-width")
  {
    value = isSetStrokeWidth();
  }
  else if (attributeName == "font-family")
  {
    value = isSetFontFamily();
  }
  else if (attributeName == "font-size")
  {
    value = isSetFontSize();
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
  else if (attributeName == "startHead")
  {
    value = isSetStartHead();
  }
  else if (attributeName == "endHead")
  {
    value = isSetEndHead();
  }
  else if (attributeName == "enableRotationalMapping")
  {
    value = isSetEnableRotationalMapping();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this DefaultValues.
 */
int
DefaultValues::setAttribute(const std::string& attributeName, bool value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  if (attributeName == "enableRotationalMapping")
  {
    return_value = setEnableRotationalMapping(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this DefaultValues.
 */
int
DefaultValues::setAttribute(const std::string& attributeName, int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this DefaultValues.
 */
int
DefaultValues::setAttribute(const std::string& attributeName, double value)
{
  int return_value = SBase::setAttribute(attributeName, value);
  
  if (attributeName == "stroke-width")
  {
    return_value = setStrokeWidth(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this DefaultValues.
 */
int
DefaultValues::setAttribute(const std::string& attributeName,
                            unsigned int value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Sets the value of the "attributeName" attribute of this DefaultValues.
 */
int
DefaultValues::setAttribute(const std::string& attributeName,
                            const std::string& value)
{
  int return_value = SBase::setAttribute(attributeName, value);

  if (attributeName == "backgroundColor")
  {
    return_value = setBackgroundColor(value);
  }
  else if (attributeName == "spreadMethod")
  {
    return_value = setSpreadMethod(value);
  }
  else if (attributeName == "linearGradient_x1")
  {
    return_value = setLinearGradient_x1(value);
  }
  else if (attributeName == "linearGradient_y1")
  {
    return_value = setLinearGradient_y1(value);
  }
  else if (attributeName == "linearGradient_z1")
  {
    return_value = setLinearGradient_z1(value);
  }
  else if (attributeName == "linearGradient_x2")
  {
    return_value = setLinearGradient_x2(value);
  }
  else if (attributeName == "linearGradient_y2")
  {
    return_value = setLinearGradient_y2(value);
  }
  else if (attributeName == "linearGradient_z2")
  {
    return_value = setLinearGradient_z2(value);
  }
  else if (attributeName == "radialGradient_cx")
  {
    return_value = setRadialGradient_cx(value);
  }
  else if (attributeName == "radialGradient_cy")
  {
    return_value = setRadialGradient_cy(value);
  }
  else if (attributeName == "radialGradient_cz")
  {
    return_value = setRadialGradient_cz(value);
  }
  else if (attributeName == "radialGradient_r")
  {
    return_value = setRadialGradient_r(value);
  }
  else if (attributeName == "radialGradient_fx")
  {
    return_value = setRadialGradient_fx(value);
  }
  else if (attributeName == "radialGradient_fy")
  {
    return_value = setRadialGradient_fy(value);
  }
  else if (attributeName == "radialGradient_fz")
  {
    return_value = setRadialGradient_fz(value);
  }
  else if (attributeName == "fill")
  {
    return_value = setFill(value);
  }
  else if (attributeName == "fill-rule")
  {
    return_value = setFillRule(value);
  }
  else if (attributeName == "default_z")
  {
    return_value = setDefault_z(value);
  }
  else if (attributeName == "stroke")
  {
    return_value = setStroke(value);
  }
  else if (attributeName == "font-family")
  {
    return_value = setFontFamily(value);
  }
  else if (attributeName == "font-size")
  {
    return_value = setFontSize(value);
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
  else if (attributeName == "startHead")
  {
    return_value = setStartHead(value);
  }
  else if (attributeName == "endHead")
  {
    return_value = setEndHead(value);
  }

  return return_value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */


/** @cond doxygenLibsbmlInternal */

/*
 * Unsets the value of the "attributeName" attribute of this DefaultValues.
 */
int
DefaultValues::unsetAttribute(const std::string& attributeName)
{
  int value = SBase::unsetAttribute(attributeName);

  if (attributeName == "backgroundColor")
  {
    value = unsetBackgroundColor();
  }
  else if (attributeName == "spreadMethod")
  {
    value = unsetSpreadMethod();
  }
  else if (attributeName == "linearGradient_x1")
  {
    value = unsetLinearGradient_x1();
  }
  else if (attributeName == "linearGradient_y1")
  {
    value = unsetLinearGradient_y1();
  }
  else if (attributeName == "linearGradient_z1")
  {
    value = unsetLinearGradient_z1();
  }
  else if (attributeName == "linearGradient_x2")
  {
    value = unsetLinearGradient_x2();
  }
  else if (attributeName == "linearGradient_y2")
  {
    value = unsetLinearGradient_y2();
  }
  else if (attributeName == "linearGradient_z2")
  {
    value = unsetLinearGradient_z2();
  }
  else if (attributeName == "radialGradient_cx")
  {
    value = unsetRadialGradient_cx();
  }
  else if (attributeName == "radialGradient_cy")
  {
    value = unsetRadialGradient_cy();
  }
  else if (attributeName == "radialGradient_cz")
  {
    value = unsetRadialGradient_cz();
  }
  else if (attributeName == "radialGradient_r")
  {
    value = unsetRadialGradient_r();
  }
  else if (attributeName == "radialGradient_fx")
  {
    value = unsetRadialGradient_fx();
  }
  else if (attributeName == "radialGradient_fy")
  {
    value = unsetRadialGradient_fy();
  }
  else if (attributeName == "radialGradient_fz")
  {
    value = unsetRadialGradient_fz();
  }
  else if (attributeName == "fill")
  {
    value = unsetFill();
  }
  else if (attributeName == "fill-rule")
  {
    value = unsetFillRule();
  }
  else if (attributeName == "default_z")
  {
    value = unsetDefault_z();
  }
  else if (attributeName == "stroke")
  {
    value = unsetStroke();
  }
  else if (attributeName == "stroke-width")
  {
    value = unsetStrokeWidth();
  }
  else if (attributeName == "font-family")
  {
    value = unsetFontFamily();
  }
  else if (attributeName == "font-size")
  {
    value = unsetFontSize();
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
  else if (attributeName == "startHead")
  {
    value = unsetStartHead();
  }
  else if (attributeName == "endHead")
  {
    value = unsetEndHead();
  }
  else if (attributeName == "enableRotationalMapping")
  {
    value = unsetEnableRotationalMapping();
  }

  return value;
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Adds the expected attributes for this element
 */
void
DefaultValues::addExpectedAttributes(ExpectedAttributes& attributes)
{
  SBase::addExpectedAttributes(attributes);

  attributes.add("backgroundColor");

  attributes.add("spreadMethod");

  attributes.add("linearGradient_x1");

  attributes.add("linearGradient_y1");

  attributes.add("linearGradient_z1");

  attributes.add("linearGradient_x2");

  attributes.add("linearGradient_y2");

  attributes.add("linearGradient_z2");

  attributes.add("radialGradient_cx");

  attributes.add("radialGradient_cy");

  attributes.add("radialGradient_cz");

  attributes.add("radialGradient_r");

  attributes.add("radialGradient_fx");

  attributes.add("radialGradient_fy");

  attributes.add("radialGradient_fz");

  attributes.add("fill");

  attributes.add("fill-rule");

  attributes.add("default_z");

  attributes.add("stroke");

  attributes.add("stroke-width");

  attributes.add("font-family");

  attributes.add("font-size");

  attributes.add("font-weight");

  attributes.add("font-style");

  attributes.add("text-anchor");

  attributes.add("vtext-anchor");

  attributes.add("startHead");

  attributes.add("endHead");

  attributes.add("enableRotationalMapping");
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Reads the expected attributes into the member data variables
 */
void
DefaultValues::readAttributes(const XMLAttributes& attributes,
                              const ExpectedAttributes& expectedAttributes)
{
  unsigned int level = getLevel();
  unsigned int version = getVersion();
  //unsigned int pkgVersion = getPackageVersion();
  unsigned int numErrs;
  bool assigned = false;
  SBMLErrorLog* log = getErrorLog();

  SBase::readAttributes(attributes, expectedAttributes);
  numErrs = log->getNumErrors();

  // 
  // backgroundColor string (use = "optional" )
  // 

  assigned = attributes.readInto("backgroundColor", mBackgroundColor);

  if (assigned == true)
  {
    if (log && mBackgroundColor.empty() == true)
    {
      logEmptyString(mBackgroundColor, level, version, "<DefaultValues>");
    }
  }

  // 
  // spreadMethod enum (use = "optional" )
  // 

  std::string spreadmethod;
  assigned = attributes.readInto("spreadMethod", spreadmethod);

  if (assigned == true)
  {
    if (spreadmethod.empty() == true)
    {
      logEmptyString(spreadmethod, level, version, "<DefaultValues>");
    }
    else
    {
      mSpreadMethod = SpreadMethod_fromString(spreadmethod.c_str());

      if (mSpreadMethod == GradientBase::INVALID)
      {
        std::string msg = "The spreadMethod on the <DefaultValues> ";

        if (isSetId())
        {
          msg += "with id '" + getId() + "'";
        }

        msg += "is '" + spreadmethod + "', which is not a valid option.";

      }
    }
  }

  // 
  // linearGradient_x1 string (use = "optional" )
  // 
  std::string temp;
  assigned = attributes.readInto("linearGradient_x1", temp);
  mLinearGradient_x1 = RelAbsVector(temp);
  if (assigned == true)
  {
    if (mLinearGradient_x1.empty() == true)
    {
      logEmptyString(temp, level, version, "<DefaultValues>");
    }
  }

  // 
  // linearGradient_y1 string (use = "optional" )
  // 

  assigned = attributes.readInto("linearGradient_y1", temp);
  mLinearGradient_y1 = RelAbsVector(temp);

  if (assigned == true)
  {
    if (mLinearGradient_y1.empty() == true)
    {
      logEmptyString(temp, level, version, "<DefaultValues>");
    }
  }

  // 
  // linearGradient_z1 string (use = "optional" )
  // 

  assigned = attributes.readInto("linearGradient_z1", temp);
  mLinearGradient_z1 = RelAbsVector(temp);

  if (assigned == true)
  {
    if (mLinearGradient_z1.empty() == true)
    {
      logEmptyString(temp, level, version, "<DefaultValues>");
    }
  }

  // 
  // linearGradient_x2 string (use = "optional" )
  // 

  assigned = attributes.readInto("linearGradient_x2", temp);
  mLinearGradient_x2 = RelAbsVector(temp);

  if (assigned == true)
  {
    if (mLinearGradient_x2.empty() == true)
    {
      logEmptyString(temp, level, version, "<DefaultValues>");
    }
  }

  // 
  // linearGradient_y2 string (use = "optional" )
  // 

  assigned = attributes.readInto("linearGradient_y2", temp);
  mLinearGradient_y2 = RelAbsVector(temp);
  
  if (assigned == true)
  {
    if (mLinearGradient_y2.empty() == true)
    {
      logEmptyString(temp, level, version, "<DefaultValues>");
    }
  }

  // 
  // linearGradient_z2 string (use = "optional" )
  // 

  assigned = attributes.readInto("linearGradient_z2", temp);
  mLinearGradient_z2 = RelAbsVector(temp);

  if (assigned == true)
  {
    if (mLinearGradient_z2.empty() == true)
    {
      logEmptyString(temp, level, version, "<DefaultValues>");
    }
  }

  // 
  // radialGradient_cx string (use = "optional" )
  // 

  assigned = attributes.readInto("radialGradient_cx", temp);
  mRadialGradient_cx = RelAbsVector(temp);

  if (assigned == true)
  {
    if (mRadialGradient_cx.empty() == true)
    {
      logEmptyString(temp, level, version, "<DefaultValues>");
    }
  }

  // 
  // radialGradient_cy string (use = "optional" )
  // 

  assigned = attributes.readInto("radialGradient_cy", temp);
  mRadialGradient_cy = RelAbsVector(temp);

  if (assigned == true)
  {
    if (mRadialGradient_cy.empty() == true)
    {
      logEmptyString(temp, level, version, "<DefaultValues>");
    }
  }

  // 
  // radialGradient_cz string (use = "optional" )
  // 

  assigned = attributes.readInto("radialGradient_cz", temp);
  mRadialGradient_cz = RelAbsVector(temp);

  if (assigned == true)
  {
    if (mRadialGradient_cz.empty() == true)
    {
      logEmptyString(temp, level, version, "<DefaultValues>");
    }
  }

  // 
  // radialGradient_r string (use = "optional" )
  // 

  assigned = attributes.readInto("radialGradient_r", temp);
  mRadialGradient_r = RelAbsVector(temp);

  if (assigned == true)
  {
    if (mRadialGradient_r.empty() == true)
    {
      logEmptyString(temp, level, version, "<DefaultValues>");
    }
  }

  // 
  // radialGradient_fx string (use = "optional" )
  // 

  assigned = attributes.readInto("radialGradient_fx", temp);
  mRadialGradient_fx = RelAbsVector(temp);

  if (assigned == true)
  {
    if (mRadialGradient_fx.empty() == true)
    {
      logEmptyString(temp, level, version, "<DefaultValues>");
    }
  }

  // 
  // radialGradient_fy string (use = "optional" )
  // 

  assigned = attributes.readInto("radialGradient_fy", temp);
  mRadialGradient_fy = RelAbsVector(temp);

  if (assigned == true)
  {
    if (mRadialGradient_fy.empty() == true)
    {
      logEmptyString(temp, level, version, "<DefaultValues>");
    }
  }

  // 
  // radialGradient_fz string (use = "optional" )
  // 

  assigned = attributes.readInto("radialGradient_fz", temp);
  mRadialGradient_fz = RelAbsVector(temp);

  if (assigned == true)
  {
    if (mRadialGradient_fz.empty() == true)
    {
      logEmptyString(temp, level, version, "<DefaultValues>");
    }
  }

  // 
  // fill string (use = "optional" )
  // 

  assigned = attributes.readInto("fill", mFill);

  if (assigned == true)
  {
    if (mFill.empty() == true)
    {
      logEmptyString(mFill, level, version, "<DefaultValues>");
    }
  }

  // 
  // fill-rule enum (use = "optional" )
  // 

  std::string fillRule;
  assigned = attributes.readInto("fill-rule", fillRule);

  if (assigned == true)
  {
    if (fillRule.empty() == true)
    {
      logEmptyString(fillRule, level, version, "<DefaultValues>");
    }
    else
    {
      mFillRule = FillRule_fromString(fillRule.c_str());

      if (mFillRule == GraphicalPrimitive2D::INVALID)
      {
        std::string msg = "The fill-rule on the <DefaultValues> ";

        if (isSetId())
        {
          msg += "with id '" + getId() + "'";
        }

        msg += "is '" + fillRule + "', which is not a valid option.";

      }
    }
  }

  // 
  // default_z string (use = "optional" )
  // 

  assigned = attributes.readInto("default_z", temp);
  mDefault_z = RelAbsVector(temp);

  if (assigned == true)
  {
    if (mDefault_z.empty() == true)
    {
      logEmptyString(temp, level, version, "<DefaultValues>");
    }
  }

  // 
  // stroke string (use = "optional" )
  // 

  assigned = attributes.readInto("stroke", mStroke);

  if (assigned == true)
  {
    if (mStroke.empty() == true)
    {
      logEmptyString(mStroke, level, version, "<DefaultValues>");
    }
  }

  // 
  // stroke-width string (use = "optional" )
  // 

  mIsSetStrokeWidth = attributes.readInto("stroke-width", mStrokeWidth);
  
  // 
  // font-family string (use = "optional" )
  // 

  assigned = attributes.readInto("font-family", mFontFamily);

  if (assigned == true)
  {
    if (mFontFamily.empty() == true)
    {
      logEmptyString(mFontFamily, level, version, "<DefaultValues>");
    }
  }

  // 
  // font-size string (use = "optional" )
  // 

  assigned = attributes.readInto("font-size", temp);
  mFontSize = RelAbsVector(temp);

  if (assigned == true)
  {
    if (mFontSize.empty() == true)
    {
      logEmptyString(temp, level, version, "<DefaultValues>");
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
      logEmptyString(fontWeight, level, version, "<DefaultValues>");
    }
    else
    {
      mFontWeight = FontWeight_fromString(fontWeight.c_str());

      if (mFontWeight == Text::WEIGHT_INVALID)
      {
        std::string msg = "The font-weight on the <DefaultValues> ";

        if (isSetId())
        {
          msg += "with id '" + getId() + "'";
        }

        msg += "is '" + fontWeight + "', which is not a valid option.";

      }
    }
  }

  // 
  // font-style enum (use = "optional" )
  // 

  std::string fontStyle;
  assigned = attributes.readInto("font-style", fontStyle);

  if (assigned == true)
  {
    if (fontStyle.empty() == true)
    {
      logEmptyString(fontStyle, level, version, "<DefaultValues>");
    }
    else
    {
      mFontStyle = FontStyle_fromString(fontStyle.c_str());

      if (mFontStyle == Text::STYLE_INVALID)
      {
        std::string msg = "The font-style on the <DefaultValues> ";

        if (isSetId())
        {
          msg += "with id '" + getId() + "'";
        }

        msg += "is '" + fontStyle + "', which is not a valid option.";
      }
    }
  }

  // 
  // text-anchor enum (use = "optional" )
  // 

  std::string textAnchor;
  assigned = attributes.readInto("text-anchor", textAnchor);

  if (assigned == true)
  {
    if (textAnchor.empty() == true)
    {
      logEmptyString(textAnchor, level, version, "<DefaultValues>");
    }
    else
    {
      mTextAnchor = TextAnchor_fromString(textAnchor.c_str());

      if (mTextAnchor == Text::ANCHOR_INVALID)
      {
        std::string msg = "The text-anchor on the <DefaultValues> ";

        if (isSetId())
        {
          msg += "with id '" + getId() + "'";
        }

        msg += "is '" + textAnchor + "', which is not a valid option.";

      }
    }
  }

  // 
  // vtext-anchor enum (use = "optional" )
  // 

  std::string vtextAnchor;
  assigned = attributes.readInto("vtext-anchor", vtextAnchor);

  if (assigned == true)
  {
    if (vtextAnchor.empty() == true)
    {
      logEmptyString(vtextAnchor, level, version, "<DefaultValues>");
    }
    else
    {
      mVTextAnchor = TextAnchor_fromString(vtextAnchor.c_str());

      if (mVTextAnchor == Text::ANCHOR_INVALID)
      {
        std::string msg = "The vtext-anchor on the <DefaultValues> ";

        if (isSetId())
        {
          msg += "with id '" + getId() + "'";
        }

        msg += "is '" + vtextAnchor + "', which is not a valid option.";

      }
    }
  }

  // 
  // startHead SIdRef (use = "optional" )
  // 

  assigned = attributes.readInto("startHead", mStartHead);

  if (assigned == true)
  {
    if (mStartHead.empty() == true)
    {
      logEmptyString(mStartHead, level, version, "<DefaultValues>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mStartHead) == false)
    {
      std::string msg = "The startHead attribute on the <" + getElementName() +
        ">";
      if (isSetId())
      {
        msg += " with id '" + getId() + "'";
      }

      msg += " is '" + mStartHead + "', which does not conform to the syntax.";
    }
  }

  // 
  // endHead SIdRef (use = "optional" )
  // 

  assigned = attributes.readInto("endHead", mEndHead);

  if (assigned == true)
  {
    if (mEndHead.empty() == true)
    {
      logEmptyString(mEndHead, level, version, "<DefaultValues>");
    }
    else if (SyntaxChecker::isValidSBMLSId(mEndHead) == false)
    {
      std::string msg = "The endHead attribute on the <" + getElementName() +
        ">";
      if (isSetId())
      {
        msg += " with id '" + getId() + "'";
      }

      msg += " is '" + mEndHead + "', which does not conform to the syntax.";
    }
  }

  // 
  // enableRotationalMapping bool (use = "optional" )
  // 

  numErrs = log->getNumErrors();
  mIsSetEnableRotationalMapping =
    attributes.readInto("enableRotationalMapping", mEnableRotationalMapping);

  if (mIsSetEnableRotationalMapping == false)
  {
    if (log->getNumErrors() == numErrs + 1 &&
      log->contains(XMLAttributeTypeMismatch))
    {
      log->remove(XMLAttributeTypeMismatch);
    }
  }
}

/** @endcond */



/** @cond doxygenLibsbmlInternal */

/*
 * Writes the attributes to the stream
 */
void
DefaultValues::writeAttributes(XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  if (isSetBackgroundColor() == true)
  {
    stream.writeAttribute("backgroundColor", getPrefix(), mBackgroundColor);
  }

  if (isSetSpreadMethod() == true)
  {
    stream.writeAttribute("spreadMethod", getPrefix(),
      getSpreadMethodAsString());
  }

  if (isSetLinearGradient_x1() == true)
  {
    stream.writeAttribute("linearGradient_x1", getPrefix(),
      mLinearGradient_x1.toString());
  }

  if (isSetLinearGradient_y1() == true)
  {
    stream.writeAttribute("linearGradient_y1", getPrefix(),
      mLinearGradient_y1.toString());
  }

  if (isSetLinearGradient_z1() == true)
  {
    stream.writeAttribute("linearGradient_z1", getPrefix(),
      mLinearGradient_z1.toString());
  }

  if (isSetLinearGradient_x2() == true)
  {
    stream.writeAttribute("linearGradient_x2", getPrefix(),
      mLinearGradient_x2.toString());
  }

  if (isSetLinearGradient_y2() == true)
  {
    stream.writeAttribute("linearGradient_y2", getPrefix(),
      mLinearGradient_y2.toString());
  }

  if (isSetLinearGradient_z2() == true)
  {
    stream.writeAttribute("linearGradient_z2", getPrefix(),
      mLinearGradient_z2.toString());
  }

  if (isSetRadialGradient_cx() == true)
  {
    stream.writeAttribute("radialGradient_cx", getPrefix(),
      mRadialGradient_cx.toString());
  }

  if (isSetRadialGradient_cy() == true)
  {
    stream.writeAttribute("radialGradient_cy", getPrefix(),
      mRadialGradient_cy.toString());
  }

  if (isSetRadialGradient_cz() == true)
  {
    stream.writeAttribute("radialGradient_cz", getPrefix(),
      mRadialGradient_cz.toString());
  }

  if (isSetRadialGradient_r() == true)
  {
    stream.writeAttribute("radialGradient_r", getPrefix(), mRadialGradient_r.toString());
  }

  if (isSetRadialGradient_fx() == true)
  {
    stream.writeAttribute("radialGradient_fx", getPrefix(),
      mRadialGradient_fx.toString());
  }

  if (isSetRadialGradient_fy() == true)
  {
    stream.writeAttribute("radialGradient_fy", getPrefix(),
      mRadialGradient_fy.toString());
  }

  if (isSetRadialGradient_fz() == true)
  {
    stream.writeAttribute("radialGradient_fz", getPrefix(),
      mRadialGradient_fz.toString());
  }

  if (isSetFill() == true)
  {
    stream.writeAttribute("fill", getPrefix(), mFill);
  }

  if (isSetFillRule() == true)
  {
    stream.writeAttribute("fill-rule", getPrefix(),
      getFillRuleAsString());
  }

  if (isSetDefault_z() == true)
  {
    stream.writeAttribute("default_z", getPrefix(), mDefault_z.toString());
  }

  if (isSetStroke() == true)
  {
    stream.writeAttribute("stroke", getPrefix(), mStroke);
  }

  if (isSetStrokeWidth() == true)
  {
    stream.writeAttribute("stroke-width", getPrefix(), mStrokeWidth);
  }

  if (isSetFontFamily() == true)
  {
    stream.writeAttribute("font-family", getPrefix(), mFontFamily);
  }

  if (isSetFontSize() == true)
  {
    stream.writeAttribute("font-size", getPrefix(), mFontSize.toString());
  }

  if (isSetFontWeight() == true)
  {
    stream.writeAttribute("font-weight", getPrefix(),
      getFontWeightAsString());
  }

  if (isSetFontStyle() == true)
  {
    stream.writeAttribute("font-style", getPrefix(),
      getFontStyleAsString());
  }

  if (isSetTextAnchor() == true)
  {
    stream.writeAttribute("text-anchor", getPrefix(),
      getTextAnchorAsString());
  }

  if (isSetVTextAnchor() == true)
  {
    stream.writeAttribute("vtext-anchor", getPrefix(),
      getVTextAnchorAsString());
  }

  if (isSetStartHead() == true)
  {
    stream.writeAttribute("startHead", getPrefix(), mStartHead);
  }

  if (isSetEndHead() == true)
  {
    stream.writeAttribute("endHead", getPrefix(), mEndHead);
  }

  if (isSetEnableRotationalMapping() == true)
  {
    stream.writeAttribute("enableRotationalMapping", getPrefix(),
      mEnableRotationalMapping);
  }

  SBase::writeExtensionAttributes(stream);
}

/** @endcond */




#endif /* __cplusplus */


/*
 * Creates a new DefaultValues_t using the given SBML Level, Version and
 * &ldquo;render&rdquo; package version.
 */
LIBSBML_EXTERN
DefaultValues_t *
DefaultValues_create(unsigned int level,
                     unsigned int version,
                     unsigned int pkgVersion)
{
  return new DefaultValues(level, version, pkgVersion);
}


/*
 * Creates and returns a deep copy of this DefaultValues_t object.
 */
LIBSBML_EXTERN
DefaultValues_t*
DefaultValues_clone(const DefaultValues_t* dv)
{
  if (dv != NULL)
  {
    return static_cast<DefaultValues_t*>(dv->clone());
  }
  else
  {
    return NULL;
  }
}


/*
 * Frees this DefaultValues_t object.
 */
LIBSBML_EXTERN
void
DefaultValues_free(DefaultValues_t* dv)
{
  if (dv != NULL)
  {
    delete dv;
  }
}


/*
 * Returns the value of the "backgroundColor" attribute of this
 * DefaultValues_t.
 */
LIBSBML_EXTERN
char *
DefaultValues_getBackgroundColor(const DefaultValues_t * dv)
{
  if (dv == NULL)
  {
    return NULL;
  }

  return dv->getBackgroundColor().empty() ? NULL :
    safe_strdup(dv->getBackgroundColor().c_str());
}


/*
 * Returns the value of the "spreadMethod" attribute of this DefaultValues_t.
 */
LIBSBML_EXTERN
GradientSpreadMethod_t
DefaultValues_getSpreadMethod(const DefaultValues_t * dv)
{
  if (dv == NULL)
  {
    return GRADIENT_SPREAD_METHOD_INVALID;
  }

  return dv->getSpreadMethod();
}


/*
 * Returns the value of the "spreadMethod" attribute of this DefaultValues_t.
 */
LIBSBML_EXTERN
char *
DefaultValues_getSpreadMethodAsString(const DefaultValues_t * dv)
{
  return (char*)(GradientSpreadMethod_toString(dv->getSpreadMethod()));
}


/*
 * Returns the value of the "fill" attribute of this DefaultValues_t.
 */
LIBSBML_EXTERN
char *
DefaultValues_getFill(const DefaultValues_t * dv)
{
  if (dv == NULL)
  {
    return NULL;
  }

  return dv->getFill().empty() ? NULL : safe_strdup(dv->getFill().c_str());
}


/*
 * Returns the value of the "fill-rule" attribute of this DefaultValues_t.
 */
LIBSBML_EXTERN
FillRule_t
DefaultValues_getFillRule(const DefaultValues_t * dv)
{
  if (dv == NULL)
  {
    return FILL_RULE_INVALID;
  }

  return (FillRule_t)(dv->getFillRule());
}


/*
 * Returns the value of the "fill-rule" attribute of this DefaultValues_t.
 */
LIBSBML_EXTERN
char *
DefaultValues_getFillRuleAsString(const DefaultValues_t * dv)
{
  return (char *)(dv->getFillRuleAsString()).c_str();
}


/*
 * Returns the value of the "stroke" attribute of this DefaultValues_t.
 */
LIBSBML_EXTERN
char *
DefaultValues_getStroke(const DefaultValues_t * dv)
{
  if (dv == NULL)
  {
    return NULL;
  }

  return dv->getStroke().empty() ? NULL : safe_strdup(dv->getStroke().c_str());
}


/*
 * Returns the value of the "stroke-width" attribute of this DefaultValues_t.
 */
LIBSBML_EXTERN
double
DefaultValues_getStrokeWidth(const DefaultValues_t * dv)
{
  return (dv != NULL) ? dv->getStrokeWidth() : util_NaN();
}


/*
 * Returns the value of the "font-family" attribute of this DefaultValues_t.
 */
LIBSBML_EXTERN
char *
DefaultValues_getFontFamily(const DefaultValues_t * dv)
{
  if (dv == NULL)
  {
    return NULL;
  }

  return dv->getFontFamily().empty() ? NULL :
    safe_strdup(dv->getFontFamily().c_str());
}


/*
 * Returns the value of the "font-weight" attribute of this DefaultValues_t.
 */
LIBSBML_EXTERN
FontWeight_t
DefaultValues_getFontWeight(const DefaultValues_t * dv)
{
  if (dv == NULL)
  {
    return FONT_WEIGHT_INVALID;
  }

  return dv->getFontWeight();
}


/*
 * Returns the value of the "font-weight" attribute of this DefaultValues_t.
 */
LIBSBML_EXTERN
char *
DefaultValues_getFontWeightAsString(const DefaultValues_t * dv)
{
  return (char*)(FontWeight_toString(dv->getFontWeight()));
}


/*
 * Returns the value of the "font-style" attribute of this DefaultValues_t.
 */
LIBSBML_EXTERN
FontStyle_t
DefaultValues_getFontStyle(const DefaultValues_t * dv)
{
  if (dv == NULL)
  {
    return FONT_STYLE_INVALID;
  }

  return dv->getFontStyle();
}


/*
 * Returns the value of the "font-style" attribute of this DefaultValues_t.
 */
LIBSBML_EXTERN
char *
DefaultValues_getFontStyleAsString(const DefaultValues_t * dv)
{
  return (char*)(FontStyle_toString(dv->getFontStyle()));
}


/*
 * Returns the value of the "text-anchor" attribute of this DefaultValues_t.
 */
LIBSBML_EXTERN
HTextAnchor_t
DefaultValues_getTextAnchor(const DefaultValues_t * dv)
{
  if (dv == NULL)
  {
    return H_TEXTANCHOR_INVALID;
  }

  return dv->getTextAnchor();
}


/*
 * Returns the value of the "text-anchor" attribute of this DefaultValues_t.
 */
LIBSBML_EXTERN
char *
DefaultValues_getTextAnchorAsString(const DefaultValues_t * dv)
{
  return (char*)(HTextAnchor_toString(dv->getTextAnchor()));
}


/*
 * Returns the value of the "vtext-anchor" attribute of this DefaultValues_t.
 */
LIBSBML_EXTERN
VTextAnchor_t
DefaultValues_getVtextAnchor(const DefaultValues_t * dv)
{
  if (dv == NULL)
  {
    return V_TEXTANCHOR_INVALID;
  }

  return dv->getVTextAnchor();
}


/*
 * Returns the value of the "vtext-anchor" attribute of this DefaultValues_t.
 */
LIBSBML_EXTERN
char *
DefaultValues_getVtextAnchorAsString(const DefaultValues_t * dv)
{
  return (char*)(VTextAnchor_toString(dv->getVTextAnchor()));
}


/*
 * Returns the value of the "startHead" attribute of this DefaultValues_t.
 */
LIBSBML_EXTERN
char *
DefaultValues_getStartHead(const DefaultValues_t * dv)
{
  if (dv == NULL)
  {
    return NULL;
  }

  return dv->getStartHead().empty() ? NULL :
    safe_strdup(dv->getStartHead().c_str());
}


/*
 * Returns the value of the "endHead" attribute of this DefaultValues_t.
 */
LIBSBML_EXTERN
char *
DefaultValues_getEndHead(const DefaultValues_t * dv)
{
  if (dv == NULL)
  {
    return NULL;
  }

  return dv->getEndHead().empty() ? NULL :
    safe_strdup(dv->getEndHead().c_str());
}


/*
 * Returns the value of the "enableRotationalMapping" attribute of this
 * DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_getEnableRotationalMapping(const DefaultValues_t * dv)
{
  return (dv != NULL) ? static_cast<int>(dv->getEnableRotationalMapping()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DefaultValues_t's "backgroundColor"
 * attribute is set.
 */
LIBSBML_EXTERN
int
DefaultValues_isSetBackgroundColor(const DefaultValues_t * dv)
{
  return (dv != NULL) ? static_cast<int>(dv->isSetBackgroundColor()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DefaultValues_t's "spreadMethod"
 * attribute is set.
 */
LIBSBML_EXTERN
int
DefaultValues_isSetSpreadMethod(const DefaultValues_t * dv)
{
  return (dv != NULL) ? static_cast<int>(dv->isSetSpreadMethod()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DefaultValues_t's "fill" attribute
 * is set.
 */
LIBSBML_EXTERN
int
DefaultValues_isSetFill(const DefaultValues_t * dv)
{
  return (dv != NULL) ? static_cast<int>(dv->isSetFill()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DefaultValues_t's "fill-rule"
 * attribute is set.
 */
LIBSBML_EXTERN
int
DefaultValues_isSetFillRule(const DefaultValues_t * dv)
{
  return (dv != NULL) ? static_cast<int>(dv->isSetFillRule()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DefaultValues_t's "stroke" attribute
 * is set.
 */
LIBSBML_EXTERN
int
DefaultValues_isSetStroke(const DefaultValues_t * dv)
{
  return (dv != NULL) ? static_cast<int>(dv->isSetStroke()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DefaultValues_t's "stroke-width"
 * attribute is set.
 */
LIBSBML_EXTERN
int
DefaultValues_isSetStrokeWidth(const DefaultValues_t * dv)
{
  return (dv != NULL) ? static_cast<int>(dv->isSetStrokeWidth()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DefaultValues_t's "font-family"
 * attribute is set.
 */
LIBSBML_EXTERN
int
DefaultValues_isSetFontFamily(const DefaultValues_t * dv)
{
  return (dv != NULL) ? static_cast<int>(dv->isSetFontFamily()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DefaultValues_t's "font-weight"
 * attribute is set.
 */
LIBSBML_EXTERN
int
DefaultValues_isSetFontWeight(const DefaultValues_t * dv)
{
  return (dv != NULL) ? static_cast<int>(dv->isSetFontWeight()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DefaultValues_t's "font-style"
 * attribute is set.
 */
LIBSBML_EXTERN
int
DefaultValues_isSetFontStyle(const DefaultValues_t * dv)
{
  return (dv != NULL) ? static_cast<int>(dv->isSetFontStyle()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DefaultValues_t's "text-anchor"
 * attribute is set.
 */
LIBSBML_EXTERN
int
DefaultValues_isSetTextAnchor(const DefaultValues_t * dv)
{
  return (dv != NULL) ? static_cast<int>(dv->isSetTextAnchor()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DefaultValues_t's "vtext-anchor"
 * attribute is set.
 */
LIBSBML_EXTERN
int
DefaultValues_isSetVtextAnchor(const DefaultValues_t * dv)
{
  return (dv != NULL) ? static_cast<int>(dv->isSetVTextAnchor()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DefaultValues_t's "startHead"
 * attribute is set.
 */
LIBSBML_EXTERN
int
DefaultValues_isSetStartHead(const DefaultValues_t * dv)
{
  return (dv != NULL) ? static_cast<int>(dv->isSetStartHead()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DefaultValues_t's "endHead"
 * attribute is set.
 */
LIBSBML_EXTERN
int
DefaultValues_isSetEndHead(const DefaultValues_t * dv)
{
  return (dv != NULL) ? static_cast<int>(dv->isSetEndHead()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DefaultValues_t's
 * "enableRotationalMapping" attribute is set.
 */
LIBSBML_EXTERN
int
DefaultValues_isSetEnableRotationalMapping(const DefaultValues_t * dv)
{
  return (dv != NULL) ? static_cast<int>(dv->isSetEnableRotationalMapping()) :
    0;
}


/*
 * Sets the value of the "backgroundColor" attribute of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_setBackgroundColor(DefaultValues_t * dv,
                                 const char * backgroundColor)
{
  return (dv != NULL) ? dv->setBackgroundColor(backgroundColor) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "spreadMethod" attribute of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_setSpreadMethod(DefaultValues_t * dv,
                              GradientSpreadMethod_t spreadMethod)
{
  return (dv != NULL) ? dv->setSpreadMethod(spreadMethod) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "spreadMethod" attribute of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_setSpreadMethodAsString(DefaultValues_t * dv,
                                      const char * spreadMethod)
{
  return (dv != NULL) ? dv->setSpreadMethod(spreadMethod):
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "fill" attribute of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_setFill(DefaultValues_t * dv, const char * fill)
{
  return (dv != NULL) ? dv->setFill(fill) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "fill-rule" attribute of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_setFillRule(DefaultValues_t * dv, FillRule_t fillRule)
{
  return (dv != NULL) ? dv->setFillRule(fillRule) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "fill-rule" attribute of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_setFillRuleAsString(DefaultValues_t * dv, const char * fillRule)
{
  return (dv != NULL) ? dv->setFillRule(fillRule): LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "stroke" attribute of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_setStroke(DefaultValues_t * dv, const char * stroke)
{
  return (dv != NULL) ? dv->setStroke(stroke) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "stroke-width" attribute of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_setStrokeWidth(DefaultValues_t * dv, double strokeWidth)
{
  return (dv != NULL) ? dv->setStrokeWidth(strokeWidth) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "font-family" attribute of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_setFontFamily(DefaultValues_t * dv, const char * fontFamily)
{
  return (dv != NULL) ? dv->setFontFamily(fontFamily) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "font-weight" attribute of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_setFontWeight(DefaultValues_t * dv, FontWeight_t fontWeight)
{
  return (dv != NULL) ? dv->setFontWeight(fontWeight) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "font-weight" attribute of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_setFontWeightAsString(DefaultValues_t * dv,
                                    const char * fontWeight)
{
  return (dv != NULL) ? dv->setFontWeight(fontWeight): LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "font-style" attribute of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_setFontStyle(DefaultValues_t * dv, FontStyle_t fontStyle)
{
  return (dv != NULL) ? dv->setFontStyle(fontStyle) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "font-style" attribute of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_setFontStyleAsString(DefaultValues_t * dv,
                                   const char * fontStyle)
{
  return (dv != NULL) ? dv->setFontStyle(fontStyle): LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "text-anchor" attribute of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_setTextAnchor(DefaultValues_t * dv, HTextAnchor_t textAnchor)
{
  return (dv != NULL) ? dv->setTextAnchor(textAnchor) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "text-anchor" attribute of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_setTextAnchorAsString(DefaultValues_t * dv,
                                    const char * textAnchor)
{
  return (dv != NULL) ? dv->setTextAnchor(textAnchor): LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "vtext-anchor" attribute of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_setVtextAnchor(DefaultValues_t * dv, VTextAnchor_t vtextAnchor)
{
  return (dv != NULL) ? dv->setVTextAnchor(vtextAnchor) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "vtext-anchor" attribute of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_setVtextAnchorAsString(DefaultValues_t * dv,
                                     const char * vtextAnchor)
{
  return (dv != NULL) ? dv->setVTextAnchor(vtextAnchor):
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "startHead" attribute of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_setStartHead(DefaultValues_t * dv, const char * startHead)
{
  return (dv != NULL) ? dv->setStartHead(startHead) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "endHead" attribute of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_setEndHead(DefaultValues_t * dv, const char * endHead)
{
  return (dv != NULL) ? dv->setEndHead(endHead) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "enableRotationalMapping" attribute of this
 * DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_setEnableRotationalMapping(DefaultValues_t * dv,
                                         int enableRotationalMapping)
{
  return (dv != NULL) ? dv->setEnableRotationalMapping(enableRotationalMapping)
    : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "backgroundColor" attribute of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_unsetBackgroundColor(DefaultValues_t * dv)
{
  return (dv != NULL) ? dv->unsetBackgroundColor() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "spreadMethod" attribute of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_unsetSpreadMethod(DefaultValues_t * dv)
{
  return (dv != NULL) ? dv->unsetSpreadMethod() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "fill" attribute of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_unsetFill(DefaultValues_t * dv)
{
  return (dv != NULL) ? dv->unsetFill() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "fill-rule" attribute of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_unsetFillRule(DefaultValues_t * dv)
{
  return (dv != NULL) ? dv->unsetFillRule() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "stroke" attribute of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_unsetStroke(DefaultValues_t * dv)
{
  return (dv != NULL) ? dv->unsetStroke() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "stroke-width" attribute of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_unsetStrokeWidth(DefaultValues_t * dv)
{
  return (dv != NULL) ? dv->unsetStrokeWidth() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "font-family" attribute of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_unsetFontFamily(DefaultValues_t * dv)
{
  return (dv != NULL) ? dv->unsetFontFamily() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "font-weight" attribute of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_unsetFontWeight(DefaultValues_t * dv)
{
  return (dv != NULL) ? dv->unsetFontWeight() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "font-style" attribute of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_unsetFontStyle(DefaultValues_t * dv)
{
  return (dv != NULL) ? dv->unsetFontStyle() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "text-anchor" attribute of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_unsetTextAnchor(DefaultValues_t * dv)
{
  return (dv != NULL) ? dv->unsetTextAnchor() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "vtext-anchor" attribute of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_unsetVtextAnchor(DefaultValues_t * dv)
{
  return (dv != NULL) ? dv->unsetVTextAnchor() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "startHead" attribute of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_unsetStartHead(DefaultValues_t * dv)
{
  return (dv != NULL) ? dv->unsetStartHead() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "endHead" attribute of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_unsetEndHead(DefaultValues_t * dv)
{
  return (dv != NULL) ? dv->unsetEndHead() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "enableRotationalMapping" attribute of this
 * DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_unsetEnableRotationalMapping(DefaultValues_t * dv)
{
  return (dv != NULL) ? dv->unsetEnableRotationalMapping() :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Returns the value of the "linearGradient_x1" element of this
 * DefaultValues_t.
 */
LIBSBML_EXTERN
const RelAbsVector_t*
DefaultValues_getLinearGradient_x1(const DefaultValues_t * dv)
{
  if (dv == NULL)
  {
    return NULL;
  }

  return (RelAbsVector_t*)(&(dv->getLinearGradient_x1()));
}


/*
 * Returns the value of the "linearGradient_y1" element of this
 * DefaultValues_t.
 */
LIBSBML_EXTERN
const RelAbsVector_t*
DefaultValues_getLinearGradient_y1(const DefaultValues_t * dv)
{
  if (dv == NULL)
  {
    return NULL;
  }

  return (RelAbsVector_t*)(&(dv->getLinearGradient_y1()));
}


/*
 * Returns the value of the "linearGradient_z1" element of this
 * DefaultValues_t.
 */
LIBSBML_EXTERN
const RelAbsVector_t*
DefaultValues_getLinearGradient_z1(const DefaultValues_t * dv)
{
  if (dv == NULL)
  {
    return NULL;
  }

  return (RelAbsVector_t*)(&(dv->getLinearGradient_z1()));
}


/*
 * Returns the value of the "linearGradient_x2" element of this
 * DefaultValues_t.
 */
LIBSBML_EXTERN
const RelAbsVector_t*
DefaultValues_getLinearGradient_x2(const DefaultValues_t * dv)
{
  if (dv == NULL)
  {
    return NULL;
  }

  return (RelAbsVector_t*)(&(dv->getLinearGradient_x2()));
}


/*
 * Returns the value of the "linearGradient_y2" element of this
 * DefaultValues_t.
 */
LIBSBML_EXTERN
const RelAbsVector_t*
DefaultValues_getLinearGradient_y2(const DefaultValues_t * dv)
{
  if (dv == NULL)
  {
    return NULL;
  }

  return (RelAbsVector_t*)(&(dv->getLinearGradient_y2()));
}


/*
 * Returns the value of the "linearGradient_z2" element of this
 * DefaultValues_t.
 */
LIBSBML_EXTERN
const RelAbsVector_t*
DefaultValues_getLinearGradient_z2(const DefaultValues_t * dv)
{
  if (dv == NULL)
  {
    return NULL;
  }

  return (RelAbsVector_t*)(&(dv->getLinearGradient_z2()));
}


/*
 * Returns the value of the "radialGradient_cx" element of this
 * DefaultValues_t.
 */
LIBSBML_EXTERN
const RelAbsVector_t*
DefaultValues_getRadialGradient_cx(const DefaultValues_t * dv)
{
  if (dv == NULL)
  {
    return NULL;
  }

  return (RelAbsVector_t*)(&(dv->getRadialGradient_cx()));
}


/*
 * Returns the value of the "radialGradient_cy" element of this
 * DefaultValues_t.
 */
LIBSBML_EXTERN
const RelAbsVector_t*
DefaultValues_getRadialGradient_cy(const DefaultValues_t * dv)
{
  if (dv == NULL)
  {
    return NULL;
  }

  return (RelAbsVector_t*)(&(dv->getRadialGradient_cy()));
}


/*
 * Returns the value of the "radialGradient_cz" element of this
 * DefaultValues_t.
 */
LIBSBML_EXTERN
const RelAbsVector_t*
DefaultValues_getRadialGradient_cz(const DefaultValues_t * dv)
{
  if (dv == NULL)
  {
    return NULL;
  }

  return (RelAbsVector_t*)(&(dv->getRadialGradient_cz()));
}


/*
 * Returns the value of the "radialGradient_r" element of this DefaultValues_t.
 */
LIBSBML_EXTERN
const RelAbsVector_t*
DefaultValues_getRadialGradient_r(const DefaultValues_t * dv)
{
  if (dv == NULL)
  {
    return NULL;
  }

  return (RelAbsVector_t*)(&(dv->getRadialGradient_r()));
}


/*
 * Returns the value of the "radialGradient_fx" element of this
 * DefaultValues_t.
 */
LIBSBML_EXTERN
const RelAbsVector_t*
DefaultValues_getRadialGradient_fx(const DefaultValues_t * dv)
{
  if (dv == NULL)
  {
    return NULL;
  }

  return (RelAbsVector_t*)(&(dv->getRadialGradient_fx()));
}


/*
 * Returns the value of the "radialGradient_fy" element of this
 * DefaultValues_t.
 */
LIBSBML_EXTERN
const RelAbsVector_t*
DefaultValues_getRadialGradient_fy(const DefaultValues_t * dv)
{
  if (dv == NULL)
  {
    return NULL;
  }

  return (RelAbsVector_t*)(&(dv->getRadialGradient_fy()));
}


/*
 * Returns the value of the "radialGradient_fz" element of this
 * DefaultValues_t.
 */
LIBSBML_EXTERN
const RelAbsVector_t*
DefaultValues_getRadialGradient_fz(const DefaultValues_t * dv)
{
  if (dv == NULL)
  {
    return NULL;
  }

  return (RelAbsVector_t*)(&(dv->getRadialGradient_fz()));
}


/*
 * Returns the value of the "default_z" element of this DefaultValues_t.
 */
LIBSBML_EXTERN
const RelAbsVector_t*
DefaultValues_getDefault_z(const DefaultValues_t * dv)
{
  if (dv == NULL)
  {
    return NULL;
  }

  return (RelAbsVector_t*)(&(dv->getDefault_z()));
}


/*
 * Returns the value of the "font-size" element of this DefaultValues_t.
 */
LIBSBML_EXTERN
const RelAbsVector_t*
DefaultValues_getFontSize(const DefaultValues_t * dv)
{
  if (dv == NULL)
  {
    return NULL;
  }

  return (RelAbsVector_t*)(&(dv->getFontSize()));
}


/*
 * Predicate returning @c 1 (true) if this DefaultValues_t's
 * "linearGradient_x1" element is set.
 */
LIBSBML_EXTERN
int
DefaultValues_isSetLinearGradient_x1(const DefaultValues_t * dv)
{
  return (dv != NULL) ? static_cast<int>(dv->isSetLinearGradient_x1()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DefaultValues_t's
 * "linearGradient_y1" element is set.
 */
LIBSBML_EXTERN
int
DefaultValues_isSetLinearGradient_y1(const DefaultValues_t * dv)
{
  return (dv != NULL) ? static_cast<int>(dv->isSetLinearGradient_y1()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DefaultValues_t's
 * "linearGradient_z1" element is set.
 */
LIBSBML_EXTERN
int
DefaultValues_isSetLinearGradient_z1(const DefaultValues_t * dv)
{
  return (dv != NULL) ? static_cast<int>(dv->isSetLinearGradient_z1()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DefaultValues_t's
 * "linearGradient_x2" element is set.
 */
LIBSBML_EXTERN
int
DefaultValues_isSetLinearGradient_x2(const DefaultValues_t * dv)
{
  return (dv != NULL) ? static_cast<int>(dv->isSetLinearGradient_x2()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DefaultValues_t's
 * "linearGradient_y2" element is set.
 */
LIBSBML_EXTERN
int
DefaultValues_isSetLinearGradient_y2(const DefaultValues_t * dv)
{
  return (dv != NULL) ? static_cast<int>(dv->isSetLinearGradient_y2()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DefaultValues_t's
 * "linearGradient_z2" element is set.
 */
LIBSBML_EXTERN
int
DefaultValues_isSetLinearGradient_z2(const DefaultValues_t * dv)
{
  return (dv != NULL) ? static_cast<int>(dv->isSetLinearGradient_z2()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DefaultValues_t's
 * "radialGradient_cx" element is set.
 */
LIBSBML_EXTERN
int
DefaultValues_isSetRadialGradient_cx(const DefaultValues_t * dv)
{
  return (dv != NULL) ? static_cast<int>(dv->isSetRadialGradient_cx()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DefaultValues_t's
 * "radialGradient_cy" element is set.
 */
LIBSBML_EXTERN
int
DefaultValues_isSetRadialGradient_cy(const DefaultValues_t * dv)
{
  return (dv != NULL) ? static_cast<int>(dv->isSetRadialGradient_cy()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DefaultValues_t's
 * "radialGradient_cz" element is set.
 */
LIBSBML_EXTERN
int
DefaultValues_isSetRadialGradient_cz(const DefaultValues_t * dv)
{
  return (dv != NULL) ? static_cast<int>(dv->isSetRadialGradient_cz()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DefaultValues_t's "radialGradient_r"
 * element is set.
 */
LIBSBML_EXTERN
int
DefaultValues_isSetRadialGradient_r(const DefaultValues_t * dv)
{
  return (dv != NULL) ? static_cast<int>(dv->isSetRadialGradient_r()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DefaultValues_t's
 * "radialGradient_fx" element is set.
 */
LIBSBML_EXTERN
int
DefaultValues_isSetRadialGradient_fx(const DefaultValues_t * dv)
{
  return (dv != NULL) ? static_cast<int>(dv->isSetRadialGradient_fx()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DefaultValues_t's
 * "radialGradient_fy" element is set.
 */
LIBSBML_EXTERN
int
DefaultValues_isSetRadialGradient_fy(const DefaultValues_t * dv)
{
  return (dv != NULL) ? static_cast<int>(dv->isSetRadialGradient_fy()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DefaultValues_t's
 * "radialGradient_fz" element is set.
 */
LIBSBML_EXTERN
int
DefaultValues_isSetRadialGradient_fz(const DefaultValues_t * dv)
{
  return (dv != NULL) ? static_cast<int>(dv->isSetRadialGradient_fz()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DefaultValues_t's "default_z"
 * element is set.
 */
LIBSBML_EXTERN
int
DefaultValues_isSetDefault_z(const DefaultValues_t * dv)
{
  return (dv != NULL) ? static_cast<int>(dv->isSetDefault_z()) : 0;
}


/*
 * Predicate returning @c 1 (true) if this DefaultValues_t's "font-size"
 * element is set.
 */
LIBSBML_EXTERN
int
DefaultValues_isSetFontSize(const DefaultValues_t * dv)
{
  return (dv != NULL) ? static_cast<int>(dv->isSetFontSize()) : 0;
}


/*
 * Sets the value of the "linearGradient_x1" element of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_setLinearGradient_x1(DefaultValues_t * dv,
                                   const RelAbsVector_t* linearGradient_x1)
{
  return (dv != NULL) ? dv->setLinearGradient_x1(*linearGradient_x1) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "linearGradient_y1" element of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_setLinearGradient_y1(DefaultValues_t * dv,
                                   const RelAbsVector_t* linearGradient_y1)
{
  return (dv != NULL) ? dv->setLinearGradient_y1(*linearGradient_y1) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "linearGradient_z1" element of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_setLinearGradient_z1(DefaultValues_t * dv,
                                   const RelAbsVector_t* linearGradient_z1)
{
  return (dv != NULL) ? dv->setLinearGradient_z1(*linearGradient_z1) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "linearGradient_x2" element of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_setLinearGradient_x2(DefaultValues_t * dv,
                                   const RelAbsVector_t* linearGradient_x2)
{
  return (dv != NULL) ? dv->setLinearGradient_x2(*linearGradient_x2) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "linearGradient_y2" element of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_setLinearGradient_y2(DefaultValues_t * dv,
                                   const RelAbsVector_t* linearGradient_y2)
{
  return (dv != NULL) ? dv->setLinearGradient_y2(*linearGradient_y2) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "linearGradient_z2" element of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_setLinearGradient_z2(DefaultValues_t * dv,
                                   const RelAbsVector_t* linearGradient_z2)
{
  return (dv != NULL) ? dv->setLinearGradient_z2(*linearGradient_z2) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "radialGradient_cx" element of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_setRadialGradient_cx(DefaultValues_t * dv,
                                   const RelAbsVector_t* radialGradient_cx)
{
  return (dv != NULL) ? dv->setRadialGradient_cx(*radialGradient_cx) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "radialGradient_cy" element of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_setRadialGradient_cy(DefaultValues_t * dv,
                                   const RelAbsVector_t* radialGradient_cy)
{
  return (dv != NULL) ? dv->setRadialGradient_cy(*radialGradient_cy) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "radialGradient_cz" element of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_setRadialGradient_cz(DefaultValues_t * dv,
                                   const RelAbsVector_t* radialGradient_cz)
{
  return (dv != NULL) ? dv->setRadialGradient_cz(*radialGradient_cz) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "radialGradient_r" element of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_setRadialGradient_r(DefaultValues_t * dv,
                                  const RelAbsVector_t* radialGradient_r)
{
  return (dv != NULL) ? dv->setRadialGradient_r(*radialGradient_r) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "radialGradient_fx" element of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_setRadialGradient_fx(DefaultValues_t * dv,
                                   const RelAbsVector_t* radialGradient_fx)
{
  return (dv != NULL) ? dv->setRadialGradient_fx(*radialGradient_fx) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "radialGradient_fy" element of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_setRadialGradient_fy(DefaultValues_t * dv,
                                   const RelAbsVector_t* radialGradient_fy)
{
  return (dv != NULL) ? dv->setRadialGradient_fy(*radialGradient_fy) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "radialGradient_fz" element of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_setRadialGradient_fz(DefaultValues_t * dv,
                                   const RelAbsVector_t* radialGradient_fz)
{
  return (dv != NULL) ? dv->setRadialGradient_fz(*radialGradient_fz) :
    LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "default_z" element of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_setDefault_z(DefaultValues_t * dv,
                           const RelAbsVector_t* default_z)
{
  return (dv != NULL) ? dv->setDefault_z(*default_z) : LIBSBML_INVALID_OBJECT;
}


/*
 * Sets the value of the "font-size" element of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_setFontSize(DefaultValues_t * dv,
                          const RelAbsVector_t* fontSize)
{
  return (dv != NULL) ? dv->setFontSize(*fontSize) : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "linearGradient_x1" element of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_unsetLinearGradient_x1(DefaultValues_t * dv)
{
  return (dv != NULL) ? dv->unsetLinearGradient_x1() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "linearGradient_y1" element of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_unsetLinearGradient_y1(DefaultValues_t * dv)
{
  return (dv != NULL) ? dv->unsetLinearGradient_y1() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "linearGradient_z1" element of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_unsetLinearGradient_z1(DefaultValues_t * dv)
{
  return (dv != NULL) ? dv->unsetLinearGradient_z1() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "linearGradient_x2" element of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_unsetLinearGradient_x2(DefaultValues_t * dv)
{
  return (dv != NULL) ? dv->unsetLinearGradient_x2() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "linearGradient_y2" element of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_unsetLinearGradient_y2(DefaultValues_t * dv)
{
  return (dv != NULL) ? dv->unsetLinearGradient_y2() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "linearGradient_z2" element of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_unsetLinearGradient_z2(DefaultValues_t * dv)
{
  return (dv != NULL) ? dv->unsetLinearGradient_z2() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "radialGradient_cx" element of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_unsetRadialGradient_cx(DefaultValues_t * dv)
{
  return (dv != NULL) ? dv->unsetRadialGradient_cx() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "radialGradient_cy" element of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_unsetRadialGradient_cy(DefaultValues_t * dv)
{
  return (dv != NULL) ? dv->unsetRadialGradient_cy() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "radialGradient_cz" element of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_unsetRadialGradient_cz(DefaultValues_t * dv)
{
  return (dv != NULL) ? dv->unsetRadialGradient_cz() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "radialGradient_r" element of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_unsetRadialGradient_r(DefaultValues_t * dv)
{
  return (dv != NULL) ? dv->unsetRadialGradient_r() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "radialGradient_fx" element of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_unsetRadialGradient_fx(DefaultValues_t * dv)
{
  return (dv != NULL) ? dv->unsetRadialGradient_fx() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "radialGradient_fy" element of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_unsetRadialGradient_fy(DefaultValues_t * dv)
{
  return (dv != NULL) ? dv->unsetRadialGradient_fy() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "radialGradient_fz" element of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_unsetRadialGradient_fz(DefaultValues_t * dv)
{
  return (dv != NULL) ? dv->unsetRadialGradient_fz() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "default_z" element of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_unsetDefault_z(DefaultValues_t * dv)
{
  return (dv != NULL) ? dv->unsetDefault_z() : LIBSBML_INVALID_OBJECT;
}


/*
 * Unsets the value of the "font-size" element of this DefaultValues_t.
 */
LIBSBML_EXTERN
int
DefaultValues_unsetFontSize(DefaultValues_t * dv)
{
  return (dv != NULL) ? dv->unsetFontSize() : LIBSBML_INVALID_OBJECT;
}


/*
 * Predicate returning @c 1 (true) if all the required attributes for this
 * DefaultValues_t object have been set.
 */
LIBSBML_EXTERN
int
DefaultValues_hasRequiredAttributes(const DefaultValues_t * dv)
{
  return (dv != NULL) ? static_cast<int>(dv->hasRequiredAttributes()) : 0;
}




LIBSBML_CPP_NAMESPACE_END


