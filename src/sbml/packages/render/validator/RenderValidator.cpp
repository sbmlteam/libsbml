/**
 * @file RenderValidator.cpp
 * @brief Definition of RenderValidator.
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
 * Copyright (C) 2013-2017 jointly by the following organizations:
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

#include <sbml/validator/VConstraint.h>

#include <sbml/packages/render/common/RenderExtensionTypes.h>
#include <sbml/packages/render/validator/RenderValidator.h>

/** @cond doxygenLibsbmlInternal */

using namespace std;

/** @endcond */



LIBSBML_CPP_NAMESPACE_BEGIN




#ifdef __cplusplus


// -------------------------------------------
// Apply<T>
// -------------------------------------------
/**
 * Applies a Constraint<T> to an SBML object of type T.
 */

template <typename T>
struct Apply
{
  Apply(const Model& m, const T& o)
    : model(m)
    , object(o)
  {
  }


  void
  operator()(TConstraint<T>* constraint)
  {
    constraint->check(model, object);
  }


  const Model& model;
  const T& object;
};


// -------------------------------------------
// ConstraintSet<T>
// -------------------------------------------
template <typename T>
class ConstraintSet
{
public:

  ConstraintSet() { }
  ~ConstraintSet() { }


  /*
   * Adds a Constraint to this ConstraintSet
   */
  void
  add(TConstraint<T>* c)
  {
    constraints.push_back(c);
  }


  /*
   * Applies all Constraints in this ConstraintSet to the given SBML object of
   * type T. Constraint violations are logged to Validator.
   */
  void
  applyTo(const Model& model, const T& object)
  {
    for_each(constraints.begin(), constraints.end(), Apply<T>(model, object));
  }


  /*
   * Returns true if the ConstraintSet is empty, false otherwise
   */
  bool
  empty() const
  {
    return constraints.empty();
  }


protected:

  std::list< TConstraint<T>* > constraints;
};


// -------------------------------------------
// ValidatorConstraints
// -------------------------------------------
struct RenderValidatorConstraints
{

  ConstraintSet<SBMLDocument>                   mSBMLDocument;
  ConstraintSet<Model>                          mModel;
  ConstraintSet<ColorDefinition>                mColorDefinition;
  ConstraintSet<Ellipse>                        mEllipse;
  ConstraintSet<GlobalRenderInformation>        mGlobalRenderInformation;
  ConstraintSet<GlobalStyle>                    mGlobalStyle;
  ConstraintSet<GradientBase>                   mGradientBase;
  ConstraintSet<GradientStop>                   mGradientStop;
  ConstraintSet<RenderGroup>                    mRenderGroup;
  ConstraintSet<Image>                          mImage;
  ConstraintSet<LineEnding>                     mLineEnding;
  ConstraintSet<LinearGradient>                 mLinearGradient;
  ConstraintSet<LocalRenderInformation>         mLocalRenderInformation;
  ConstraintSet<LocalStyle>                     mLocalStyle;
  ConstraintSet<Polygon>                        mPolygon;
  ConstraintSet<RadialGradient>                 mRadialGradient;
  ConstraintSet<Rectangle>                      mRectangle;
//  ConstraintSet<RelAbsVector>                   mRelAbsVector;
  ConstraintSet<RenderCubicBezier>              mRenderCubicBezier;
  ConstraintSet<RenderCurve>                    mRenderCurve;
  ConstraintSet<RenderPoint>                    mRenderPoint;
  ConstraintSet<Text>                           mText;
  ConstraintSet<Transformation2D>               mTransformation2D;
  ConstraintSet<Transformation>                 mTransformation;
  ConstraintSet<GraphicalPrimitive1D>           mGraphicalPrimitive1D;
  ConstraintSet<GraphicalPrimitive2D>           mGraphicalPrimitive2D;
  ConstraintSet<Style>                          mStyle;
  ConstraintSet<RenderInformationBase>          mRenderInformationBase;
  ConstraintSet<DefaultValues>                  mDefaultValues;

  map<VConstraint*, bool> ptrMap;

  ~RenderValidatorConstraints();

  void add(VConstraint* c);

};


/*
 * Destroys this RenderValidatorConstraints object.
 */
RenderValidatorConstraints::~RenderValidatorConstraints()
{
  map<VConstraint*, bool>::iterator it = ptrMap.begin();

  while (it != ptrMap.end())
  {
    if (it->second)
    {
      delete it->first;
    }

    ++it;
  }
}


/*
 * Adds the given Constraint to the appropriate ConstraintSet
 */
void
RenderValidatorConstraints::add(VConstraint* c)
{
  if (c == NULL) return;

  ptrMap.insert(pair<VConstraint*, bool>(c, true));

  if (dynamic_cast< TConstraint<SBMLDocument>* >(c) != NULL)
  {
    mSBMLDocument.add(static_cast< TConstraint<SBMLDocument>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<Model>* >(c) != NULL)
  {
    mModel.add(static_cast< TConstraint<Model>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<ColorDefinition>* >(c) != NULL)
  {
    mColorDefinition.add(static_cast< TConstraint<ColorDefinition>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<Ellipse>* >(c) != NULL)
  {
    mEllipse.add(static_cast< TConstraint<Ellipse>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<GlobalRenderInformation>* >(c) != NULL)
  {
    mGlobalRenderInformation.add(static_cast<
      TConstraint<GlobalRenderInformation>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<GlobalStyle>* >(c) != NULL)
  {
    mGlobalStyle.add(static_cast< TConstraint<GlobalStyle>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<GradientBase>* >(c) != NULL)
  {
    mGradientBase.add(static_cast< TConstraint<GradientBase>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<GradientStop>* >(c) != NULL)
  {
    mGradientStop.add(static_cast< TConstraint<GradientStop>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<RenderGroup>* >(c) != NULL)
  {
    mRenderGroup.add(static_cast< TConstraint<RenderGroup>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<Image>* >(c) != NULL)
  {
    mImage.add(static_cast< TConstraint<Image>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<LineEnding>* >(c) != NULL)
  {
    mLineEnding.add(static_cast< TConstraint<LineEnding>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<LinearGradient>* >(c) != NULL)
  {
    mLinearGradient.add(static_cast< TConstraint<LinearGradient>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<LocalRenderInformation>* >(c) != NULL)
  {
    mLocalRenderInformation.add(static_cast<
      TConstraint<LocalRenderInformation>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<LocalStyle>* >(c) != NULL)
  {
    mLocalStyle.add(static_cast< TConstraint<LocalStyle>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<Polygon>* >(c) != NULL)
  {
    mPolygon.add(static_cast< TConstraint<Polygon>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<RadialGradient>* >(c) != NULL)
  {
    mRadialGradient.add(static_cast< TConstraint<RadialGradient>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<Rectangle>* >(c) != NULL)
  {
    mRectangle.add(static_cast< TConstraint<Rectangle>* >(c) );
    return;
  }

  //if (dynamic_cast< TConstraint<RelAbsVector>* >(c) != NULL)
  //{
  //  mRelAbsVector.add(static_cast< TConstraint<RelAbsVector>* >(c) );
  //  return;
  //}

  if (dynamic_cast< TConstraint<RenderCubicBezier>* >(c) != NULL)
  {
    mRenderCubicBezier.add(static_cast< TConstraint<RenderCubicBezier>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<RenderCurve>* >(c) != NULL)
  {
    mRenderCurve.add(static_cast< TConstraint<RenderCurve>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<RenderPoint>* >(c) != NULL)
  {
    mRenderPoint.add(static_cast< TConstraint<RenderPoint>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<Text>* >(c) != NULL)
  {
    mText.add(static_cast< TConstraint<Text>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<Transformation2D>* >(c) != NULL)
  {
    mTransformation2D.add(static_cast< TConstraint<Transformation2D>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<Transformation>* >(c) != NULL)
  {
    mTransformation.add(static_cast< TConstraint<Transformation>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<GraphicalPrimitive1D>* >(c) != NULL)
  {
    mGraphicalPrimitive1D.add(static_cast< TConstraint<GraphicalPrimitive1D>*
      >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<GraphicalPrimitive2D>* >(c) != NULL)
  {
    mGraphicalPrimitive2D.add(static_cast< TConstraint<GraphicalPrimitive2D>*
      >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<Style>* >(c) != NULL)
  {
    mStyle.add(static_cast< TConstraint<Style>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<RenderInformationBase>* >(c) != NULL)
  {
    mRenderInformationBase.add(static_cast< TConstraint<RenderInformationBase>*
      >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<DefaultValues>* >(c) != NULL)
  {
    mDefaultValues.add(static_cast< TConstraint<DefaultValues>* >(c) );
    return;
  }
}


// -------------------------------------------
// ValidatingVisitor
// -------------------------------------------

class RenderValidatingVisitor: public SBMLVisitor
{
public:

  RenderValidatingVisitor(RenderValidator& v, const Model& m)
    : v(v)
    , m(m)
  {
  }


  using SBMLVisitor::visit;

  bool
  visit(const ColorDefinition& x)
  {
    v.mRenderConstraints->mColorDefinition.applyTo(m, x);
    return !v.mRenderConstraints->mColorDefinition.empty();
  }


  bool
  visit(const Ellipse& x)
  {
    v.mRenderConstraints->mEllipse.applyTo(m, x);
    return !v.mRenderConstraints->mEllipse.empty();
  }


  bool
  visit(const GlobalRenderInformation& x)
  {
    v.mRenderConstraints->mGlobalRenderInformation.applyTo(m, x);
    return !v.mRenderConstraints->mGlobalRenderInformation.empty();
  }


  bool
  visit(const GlobalStyle& x)
  {
    v.mRenderConstraints->mGlobalStyle.applyTo(m, x);
    return !v.mRenderConstraints->mGlobalStyle.empty();
  }


  bool
  visit(const GradientBase& x)
  {
    v.mRenderConstraints->mGradientBase.applyTo(m, x);
    return !v.mRenderConstraints->mGradientBase.empty();
  }


  bool
  visit(const GradientStop& x)
  {
    v.mRenderConstraints->mGradientStop.applyTo(m, x);
    return !v.mRenderConstraints->mGradientStop.empty();
  }


  bool
  visit(const RenderGroup& x)
  {
    v.mRenderConstraints->mRenderGroup.applyTo(m, x);
    return !v.mRenderConstraints->mRenderGroup.empty();
  }


  bool
  visit(const Image& x)
  {
    v.mRenderConstraints->mImage.applyTo(m, x);
    return !v.mRenderConstraints->mImage.empty();
  }


  bool
  visit(const LineEnding& x)
  {
    v.mRenderConstraints->mLineEnding.applyTo(m, x);
    return !v.mRenderConstraints->mLineEnding.empty();
  }


  bool
  visit(const LinearGradient& x)
  {
    v.mRenderConstraints->mLinearGradient.applyTo(m, x);
    return !v.mRenderConstraints->mLinearGradient.empty();
  }


  bool
  visit(const LocalRenderInformation& x)
  {
    v.mRenderConstraints->mLocalRenderInformation.applyTo(m, x);
    return !v.mRenderConstraints->mLocalRenderInformation.empty();
  }


  bool
  visit(const LocalStyle& x)
  {
    v.mRenderConstraints->mLocalStyle.applyTo(m, x);
    return !v.mRenderConstraints->mLocalStyle.empty();
  }


  bool
  visit(const Polygon& x)
  {
    v.mRenderConstraints->mPolygon.applyTo(m, x);
    return !v.mRenderConstraints->mPolygon.empty();
  }


  bool
  visit(const RadialGradient& x)
  {
    v.mRenderConstraints->mRadialGradient.applyTo(m, x);
    return !v.mRenderConstraints->mRadialGradient.empty();
  }


  bool
  visit(const Rectangle& x)
  {
    v.mRenderConstraints->mRectangle.applyTo(m, x);
    return !v.mRenderConstraints->mRectangle.empty();
  }


  //bool
  //visit(const RelAbsVector& x)
  //{
  //  v.mRenderConstraints->mRelAbsVector.applyTo(m, x);
  //  return !v.mRenderConstraints->mRelAbsVector.empty();
  //}


  bool
  visit(const RenderCubicBezier& x)
  {
    v.mRenderConstraints->mRenderCubicBezier.applyTo(m, x);
    return !v.mRenderConstraints->mRenderCubicBezier.empty();
  }


  bool
  visit(const RenderCurve& x)
  {
    v.mRenderConstraints->mRenderCurve.applyTo(m, x);
    return !v.mRenderConstraints->mRenderCurve.empty();
  }


  bool
  visit(const RenderPoint& x)
  {
    v.mRenderConstraints->mRenderPoint.applyTo(m, x);
    return !v.mRenderConstraints->mRenderPoint.empty();
  }


  bool
  visit(const Text& x)
  {
    v.mRenderConstraints->mText.applyTo(m, x);
    return !v.mRenderConstraints->mText.empty();
  }


  bool
  visit(const Transformation2D& x)
  {
    v.mRenderConstraints->mTransformation2D.applyTo(m, x);
    return !v.mRenderConstraints->mTransformation2D.empty();
  }


  bool
  visit(const Transformation& x)
  {
    v.mRenderConstraints->mTransformation.applyTo(m, x);
    return !v.mRenderConstraints->mTransformation.empty();
  }


  bool
  visit(const GraphicalPrimitive1D& x)
  {
    v.mRenderConstraints->mGraphicalPrimitive1D.applyTo(m, x);
    return !v.mRenderConstraints->mGraphicalPrimitive1D.empty();
  }


  bool
  visit(const GraphicalPrimitive2D& x)
  {
    v.mRenderConstraints->mGraphicalPrimitive2D.applyTo(m, x);
    return !v.mRenderConstraints->mGraphicalPrimitive2D.empty();
  }


  bool
  visit(const Style& x)
  {
    v.mRenderConstraints->mStyle.applyTo(m, x);
    return !v.mRenderConstraints->mStyle.empty();
  }


  bool
  visit(const RenderInformationBase& x)
  {
    v.mRenderConstraints->mRenderInformationBase.applyTo(m, x);
    return !v.mRenderConstraints->mRenderInformationBase.empty();
  }


  bool
  visit(const DefaultValues& x)
  {
    v.mRenderConstraints->mDefaultValues.applyTo(m, x);
    return !v.mRenderConstraints->mDefaultValues.empty();
  }


  virtual bool
  visit(const SBase& x)
  {
    if (x.getPackageName() != "render")
    {
      return SBMLVisitor::visit(x);
    }

    int code = x.getTypeCode();

    const ListOf* list = dynamic_cast<const ListOf*>(&x);

    if (list != NULL)
    {
      return SBMLVisitor::visit(x);
    }
    else
    {
      if (code == SBML_RENDER_COLORDEFINITION)
      {
        return visit((const ColorDefinition&)x);
      }
      else if (code == SBML_RENDER_ELLIPSE)
      {
        return visit((const Ellipse&)x);
      }
      else if (code == SBML_RENDER_GLOBALRENDERINFORMATION)
      {
        return visit((const GlobalRenderInformation&)x);
      }
      else if (code == SBML_RENDER_GLOBALSTYLE)
      {
        return visit((const GlobalStyle&)x);
      }
      else if (code == SBML_RENDER_GRADIENTDEFINITION)
      {
        return visit((const GradientBase&)x);
      }
      else if (code == SBML_RENDER_GRADIENT_STOP)
      {
        return visit((const GradientStop&)x);
      }
      else if (code == SBML_RENDER_GROUP)
      {
        return visit((const RenderGroup&)x);
      }
      else if (code == SBML_RENDER_IMAGE)
      {
        return visit((const Image&)x);
      }
      else if (code == SBML_RENDER_LINEENDING)
      {
        return visit((const LineEnding&)x);
      }
      else if (code == SBML_RENDER_LINEARGRADIENT)
      {
        return visit((const LinearGradient&)x);
      }
      else if (code == SBML_RENDER_LOCALRENDERINFORMATION)
      {
        return visit((const LocalRenderInformation&)x);
      }
      else if (code == SBML_RENDER_LOCALSTYLE)
      {
        return visit((const LocalStyle&)x);
      }
      else if (code == SBML_RENDER_POLYGON)
      {
        return visit((const Polygon&)x);
      }
      else if (code == SBML_RENDER_RADIALGRADIENT)
      {
        return visit((const RadialGradient&)x);
      }
      else if (code == SBML_RENDER_RECTANGLE)
      {
        return visit((const Rectangle&)x);
      }
      //else if (code == SBML_RENDER_RELABSVECTOR)
      //{
      //  return visit((const RelAbsVector&)x);
      //}
      else if (code == SBML_RENDER_CUBICBEZIER)
      {
        return visit((const RenderCubicBezier&)x);
      }
      else if (code == SBML_RENDER_CURVE)
      {
        return visit((const RenderCurve&)x);
      }
      else if (code == SBML_RENDER_POINT)
      {
        return visit((const RenderPoint&)x);
      }
      else if (code == SBML_RENDER_TEXT)
      {
        return visit((const Text&)x);
      }
      else if (code == SBML_RENDER_TRANSFORMATION2D)
      {
        return visit((const Transformation2D&)x);
      }
      else if (code == SBML_RENDER_TRANSFORMATION)
      {
        return visit((const Transformation&)x);
      }
      else if (code == SBML_RENDER_GRAPHICALPRIMITIVE1D)
      {
        return visit((const GraphicalPrimitive1D&)x);
      }
      else if (code == SBML_RENDER_GRAPHICALPRIMITIVE2D)
      {
        return visit((const GraphicalPrimitive2D&)x);
      }
      else if (code == SBML_RENDER_STYLE_BASE)
      {
        return visit((const Style&)x);
      }
      else if (code == SBML_RENDER_RENDERINFORMATION_BASE)
      {
        return visit((const RenderInformationBase&)x);
      }
      else if (code == SBML_RENDER_DEFAULTS)
      {
        return visit((const DefaultValues&)x);
      }
      else
      {
        return SBMLVisitor::visit(x);
      }
    }
  }


protected:

  RenderValidator& v;
  const Model& m;
};


// -------------------------------------------
// RenderValidator
// -------------------------------------------

/*
 * Creates a new RenderValidator object for the given category of validation.
 */
RenderValidator::RenderValidator(SBMLErrorCategory_t category)
  : Validator(category)
{
  mRenderConstraints = new RenderValidatorConstraints();
}


/*
 * Destroys this RenderValidator object.
 */
RenderValidator::~RenderValidator()
{
  delete mRenderConstraints;
}


/*
 * Adds the given VConstraint object to this RenderValidator.
 */
void
RenderValidator::addConstraint(VConstraint* c)
{
  mRenderConstraints->add(c);
}


/*
 * Validates the given SBMLDocument
 */
unsigned int
RenderValidator::validate(const SBMLDocument& d)
{
  const Model* m = d.getModel();

  if (m != NULL)
  {
    RenderValidatingVisitor vv(*this, *m);
    const RenderSBMLDocumentPlugin* plugin = static_cast<const
      RenderSBMLDocumentPlugin*>(d.getPlugin("render"));
    if (plugin != NULL)
    {
      plugin->accept(vv);
    }
  }

  // ADD ANY OTHER OBJECTS THAT HAS PLUGINS

  return (unsigned int)(mFailures.size());
}


/*
 * Validates the SBMLDocument located at the given filename
 */
unsigned int
RenderValidator::validate(const std::string& filename)
{
  SBMLReader reader;
  SBMLDocument* d = reader.readSBML(filename);


  unsigned int numErrors = d->getNumErrors();


  for (unsigned int n=0; n < numErrors; ++n)
  {
    logFailure(*d->getError(n));
  }

  numErrors = validate(*d);
  delete d;
  return numErrors;
}




#endif /* __cplusplus */




LIBSBML_CPP_NAMESPACE_END


