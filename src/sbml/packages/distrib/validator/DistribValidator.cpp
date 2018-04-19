/**
 * @file DistribValidator.cpp
 * @brief Definition of DistribValidator.
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
 */

#include <sbml/validator/VConstraint.h>

#include <sbml/packages/distrib/common/DistribExtensionTypes.h>
#include <sbml/packages/distrib/validator/DistribValidator.h>

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
struct Apply : public unary_function<TConstraint<T>*, void>
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
struct DistribValidatorConstraints
{

  ConstraintSet<SBMLDocument>                   mSBMLDocument;
  ConstraintSet<Model>                          mModel;
  ConstraintSet<DistribDrawFromDistribution>    mDistribDrawFromDistribution;
  ConstraintSet<DistribInput>                   mDistribInput;
  ConstraintSet<DistribDistribution>            mDistribDistribution;
  ConstraintSet<DistribUnivariateDistribution>  mDistribUnivariateDistribution;
  ConstraintSet<DistribMultivariateDistribution> mDistribMultivariateDistribution;
  ConstraintSet<DistribContinuousUnivariateDistribution> mDistribContinuousUnivariateDistribution;
  ConstraintSet<DistribDiscreteUnivariateDistribution> mDistribDiscreteUnivariateDistribution;
  ConstraintSet<DistribCategoricalUnivariateDistribution> mDistribCategoricalUnivariateDistribution;
  ConstraintSet<DistribUncertValue>             mDistribUncertValue;
  ConstraintSet<DistribUncertBound>             mDistribUncertBound;
  ConstraintSet<DistribExternalDistribution>    mDistribExternalDistribution;
  ConstraintSet<DistribExternalParameter>       mDistribExternalParameter;
  ConstraintSet<DistribNormalDistribution>      mDistribNormalDistribution;
  ConstraintSet<DistribUniformDistribution>     mDistribUniformDistribution;
  ConstraintSet<DistribCategoricalDistribution> mDistribCategoricalDistribution;
  ConstraintSet<DistribCategory>                mDistribCategory;
  ConstraintSet<DistribBernoulliDistribution>   mDistribBernoulliDistribution;
  ConstraintSet<DistribBetaDistribution>        mDistribBetaDistribution;
  ConstraintSet<DistribBinomialDistribution>    mDistribBinomialDistribution;
  ConstraintSet<DistribCauchyDistribution>      mDistribCauchyDistribution;
  ConstraintSet<DistribChiSquareDistribution>   mDistribChiSquareDistribution;
  ConstraintSet<DistribExponentialDistribution> mDistribExponentialDistribution;
  ConstraintSet<DistribFDistribution>           mDistribFDistribution;
  ConstraintSet<DistribGammaDistribution>       mDistribGammaDistribution;
  ConstraintSet<DistribGeometricDistribution>   mDistribGeometricDistribution;
  ConstraintSet<DistribHypergeometricDistribution> mDistribHypergeometricDistribution;
  ConstraintSet<DistribInverseGammaDistribution> mDistribInverseGammaDistribution;
  ConstraintSet<DistribLaPlaceDistribution>     mDistribLaPlaceDistribution;
  ConstraintSet<DistribLogNormalDistribution>   mDistribLogNormalDistribution;
  ConstraintSet<DistribLogisticDistribution>    mDistribLogisticDistribution;
  ConstraintSet<DistribNegativeBinomialDistribution> mDistribNegativeBinomialDistribution;
  ConstraintSet<DistribParetoDistribution>      mDistribParetoDistribution;
  ConstraintSet<DistribPoissonDistribution>     mDistribPoissonDistribution;
  ConstraintSet<DistribRayleighDistribution>    mDistribRayleighDistribution;
  ConstraintSet<DistribStudentTDistribution>    mDistribStudentTDistribution;
  ConstraintSet<DistribWeibullDistribution>     mDistribWeibullDistribution;
  ConstraintSet<DistribUncertainty>             mDistribUncertainty;
  ConstraintSet<DistribUncertStatistics>        mDistribUncertStatistics;
  ConstraintSet<DistribUncertStatisticSpan>     mDistribUncertStatisticSpan;

  map<VConstraint*, bool> ptrMap;

  ~DistribValidatorConstraints();

  void add(VConstraint* c);

};


/*
 * Destroys this DistribValidatorConstraints object.
 */
DistribValidatorConstraints::~DistribValidatorConstraints()
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
DistribValidatorConstraints::add(VConstraint* c)
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

  if (dynamic_cast< TConstraint<DistribDrawFromDistribution>* >(c) != NULL)
  {
    mDistribDrawFromDistribution.add(static_cast<
      TConstraint<DistribDrawFromDistribution>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<DistribInput>* >(c) != NULL)
  {
    mDistribInput.add(static_cast< TConstraint<DistribInput>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<DistribDistribution>* >(c) != NULL)
  {
    mDistribDistribution.add(static_cast< TConstraint<DistribDistribution>*
      >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<DistribUnivariateDistribution>* >(c) != NULL)
  {
    mDistribUnivariateDistribution.add(static_cast<
      TConstraint<DistribUnivariateDistribution>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<DistribMultivariateDistribution>* >(c) != NULL)
  {
    mDistribMultivariateDistribution.add(static_cast<
      TConstraint<DistribMultivariateDistribution>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<DistribContinuousUnivariateDistribution>* >(c)
    != NULL)
  {
    mDistribContinuousUnivariateDistribution.add(static_cast<
      TConstraint<DistribContinuousUnivariateDistribution>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<DistribDiscreteUnivariateDistribution>* >(c) !=
    NULL)
  {
    mDistribDiscreteUnivariateDistribution.add(static_cast<
      TConstraint<DistribDiscreteUnivariateDistribution>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<DistribCategoricalUnivariateDistribution>* >(c)
    != NULL)
  {
    mDistribCategoricalUnivariateDistribution.add(static_cast<
      TConstraint<DistribCategoricalUnivariateDistribution>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<DistribUncertValue>* >(c) != NULL)
  {
    mDistribUncertValue.add(static_cast< TConstraint<DistribUncertValue>* >(c)
      );
    return;
  }

  if (dynamic_cast< TConstraint<DistribUncertBound>* >(c) != NULL)
  {
    mDistribUncertBound.add(static_cast< TConstraint<DistribUncertBound>* >(c)
      );
    return;
  }

  if (dynamic_cast< TConstraint<DistribExternalDistribution>* >(c) != NULL)
  {
    mDistribExternalDistribution.add(static_cast<
      TConstraint<DistribExternalDistribution>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<DistribExternalParameter>* >(c) != NULL)
  {
    mDistribExternalParameter.add(static_cast<
      TConstraint<DistribExternalParameter>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<DistribNormalDistribution>* >(c) != NULL)
  {
    mDistribNormalDistribution.add(static_cast<
      TConstraint<DistribNormalDistribution>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<DistribUniformDistribution>* >(c) != NULL)
  {
    mDistribUniformDistribution.add(static_cast<
      TConstraint<DistribUniformDistribution>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<DistribCategoricalDistribution>* >(c) != NULL)
  {
    mDistribCategoricalDistribution.add(static_cast<
      TConstraint<DistribCategoricalDistribution>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<DistribCategory>* >(c) != NULL)
  {
    mDistribCategory.add(static_cast< TConstraint<DistribCategory>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<DistribBernoulliDistribution>* >(c) != NULL)
  {
    mDistribBernoulliDistribution.add(static_cast<
      TConstraint<DistribBernoulliDistribution>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<DistribBetaDistribution>* >(c) != NULL)
  {
    mDistribBetaDistribution.add(static_cast<
      TConstraint<DistribBetaDistribution>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<DistribBinomialDistribution>* >(c) != NULL)
  {
    mDistribBinomialDistribution.add(static_cast<
      TConstraint<DistribBinomialDistribution>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<DistribCauchyDistribution>* >(c) != NULL)
  {
    mDistribCauchyDistribution.add(static_cast<
      TConstraint<DistribCauchyDistribution>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<DistribChiSquareDistribution>* >(c) != NULL)
  {
    mDistribChiSquareDistribution.add(static_cast<
      TConstraint<DistribChiSquareDistribution>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<DistribExponentialDistribution>* >(c) != NULL)
  {
    mDistribExponentialDistribution.add(static_cast<
      TConstraint<DistribExponentialDistribution>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<DistribFDistribution>* >(c) != NULL)
  {
    mDistribFDistribution.add(static_cast< TConstraint<DistribFDistribution>*
      >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<DistribGammaDistribution>* >(c) != NULL)
  {
    mDistribGammaDistribution.add(static_cast<
      TConstraint<DistribGammaDistribution>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<DistribGeometricDistribution>* >(c) != NULL)
  {
    mDistribGeometricDistribution.add(static_cast<
      TConstraint<DistribGeometricDistribution>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<DistribHypergeometricDistribution>* >(c) !=
    NULL)
  {
    mDistribHypergeometricDistribution.add(static_cast<
      TConstraint<DistribHypergeometricDistribution>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<DistribInverseGammaDistribution>* >(c) != NULL)
  {
    mDistribInverseGammaDistribution.add(static_cast<
      TConstraint<DistribInverseGammaDistribution>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<DistribLaPlaceDistribution>* >(c) != NULL)
  {
    mDistribLaPlaceDistribution.add(static_cast<
      TConstraint<DistribLaPlaceDistribution>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<DistribLogNormalDistribution>* >(c) != NULL)
  {
    mDistribLogNormalDistribution.add(static_cast<
      TConstraint<DistribLogNormalDistribution>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<DistribLogisticDistribution>* >(c) != NULL)
  {
    mDistribLogisticDistribution.add(static_cast<
      TConstraint<DistribLogisticDistribution>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<DistribNegativeBinomialDistribution>* >(c) !=
    NULL)
  {
    mDistribNegativeBinomialDistribution.add(static_cast<
      TConstraint<DistribNegativeBinomialDistribution>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<DistribParetoDistribution>* >(c) != NULL)
  {
    mDistribParetoDistribution.add(static_cast<
      TConstraint<DistribParetoDistribution>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<DistribPoissonDistribution>* >(c) != NULL)
  {
    mDistribPoissonDistribution.add(static_cast<
      TConstraint<DistribPoissonDistribution>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<DistribRayleighDistribution>* >(c) != NULL)
  {
    mDistribRayleighDistribution.add(static_cast<
      TConstraint<DistribRayleighDistribution>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<DistribStudentTDistribution>* >(c) != NULL)
  {
    mDistribStudentTDistribution.add(static_cast<
      TConstraint<DistribStudentTDistribution>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<DistribWeibullDistribution>* >(c) != NULL)
  {
    mDistribWeibullDistribution.add(static_cast<
      TConstraint<DistribWeibullDistribution>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<DistribUncertainty>* >(c) != NULL)
  {
    mDistribUncertainty.add(static_cast< TConstraint<DistribUncertainty>* >(c)
      );
    return;
  }

  if (dynamic_cast< TConstraint<DistribUncertStatistics>* >(c) != NULL)
  {
    mDistribUncertStatistics.add(static_cast<
      TConstraint<DistribUncertStatistics>* >(c) );
    return;
  }

  if (dynamic_cast< TConstraint<DistribUncertStatisticSpan>* >(c) != NULL)
  {
    mDistribUncertStatisticSpan.add(static_cast<
      TConstraint<DistribUncertStatisticSpan>* >(c) );
    return;
  }
}


// -------------------------------------------
// ValidatingVisitor
// -------------------------------------------

class DistribValidatingVisitor: public SBMLVisitor
{
public:

  DistribValidatingVisitor(DistribValidator& v, const Model& m)
    : v(v)
    , m(m)
  {
  }


  using SBMLVisitor::visit;

  bool
  visit(const DistribDrawFromDistribution& x)
  {
    v.mDistribConstraints->mDistribDrawFromDistribution.applyTo(m, x);
    return !v.mDistribConstraints->mDistribDrawFromDistribution.empty();
  }


  bool
  visit(const DistribInput& x)
  {
    v.mDistribConstraints->mDistribInput.applyTo(m, x);
    return !v.mDistribConstraints->mDistribInput.empty();
  }


  bool
  visit(const DistribDistribution& x)
  {
    v.mDistribConstraints->mDistribDistribution.applyTo(m, x);
    return !v.mDistribConstraints->mDistribDistribution.empty();
  }


  bool
  visit(const DistribUnivariateDistribution& x)
  {
    v.mDistribConstraints->mDistribUnivariateDistribution.applyTo(m, x);
    return !v.mDistribConstraints->mDistribUnivariateDistribution.empty();
  }


  bool
  visit(const DistribMultivariateDistribution& x)
  {
    v.mDistribConstraints->mDistribMultivariateDistribution.applyTo(m, x);
    return !v.mDistribConstraints->mDistribMultivariateDistribution.empty();
  }


  bool
  visit(const DistribContinuousUnivariateDistribution& x)
  {
    v.mDistribConstraints->mDistribContinuousUnivariateDistribution.applyTo(m,
      x);
    return
      !v.mDistribConstraints->mDistribContinuousUnivariateDistribution.empty();
  }


  bool
  visit(const DistribDiscreteUnivariateDistribution& x)
  {
    v.mDistribConstraints->mDistribDiscreteUnivariateDistribution.applyTo(m,
      x);
    return
      !v.mDistribConstraints->mDistribDiscreteUnivariateDistribution.empty();
  }


  bool
  visit(const DistribCategoricalUnivariateDistribution& x)
  {
    v.mDistribConstraints->mDistribCategoricalUnivariateDistribution.applyTo(m,
      x);
    return
      !v.mDistribConstraints->mDistribCategoricalUnivariateDistribution.empty();
  }


  bool
  visit(const DistribUncertValue& x)
  {
    v.mDistribConstraints->mDistribUncertValue.applyTo(m, x);
    return !v.mDistribConstraints->mDistribUncertValue.empty();
  }


  bool
  visit(const DistribUncertBound& x)
  {
    v.mDistribConstraints->mDistribUncertBound.applyTo(m, x);
    return !v.mDistribConstraints->mDistribUncertBound.empty();
  }


  bool
  visit(const DistribExternalDistribution& x)
  {
    v.mDistribConstraints->mDistribExternalDistribution.applyTo(m, x);
    return !v.mDistribConstraints->mDistribExternalDistribution.empty();
  }


  bool
  visit(const DistribExternalParameter& x)
  {
    v.mDistribConstraints->mDistribExternalParameter.applyTo(m, x);
    return !v.mDistribConstraints->mDistribExternalParameter.empty();
  }


  bool
  visit(const DistribNormalDistribution& x)
  {
    v.mDistribConstraints->mDistribNormalDistribution.applyTo(m, x);
    return !v.mDistribConstraints->mDistribNormalDistribution.empty();
  }


  bool
  visit(const DistribUniformDistribution& x)
  {
    v.mDistribConstraints->mDistribUniformDistribution.applyTo(m, x);
    return !v.mDistribConstraints->mDistribUniformDistribution.empty();
  }


  bool
  visit(const DistribCategoricalDistribution& x)
  {
    v.mDistribConstraints->mDistribCategoricalDistribution.applyTo(m, x);
    return !v.mDistribConstraints->mDistribCategoricalDistribution.empty();
  }


  bool
  visit(const DistribCategory& x)
  {
    v.mDistribConstraints->mDistribCategory.applyTo(m, x);
    return !v.mDistribConstraints->mDistribCategory.empty();
  }


  bool
  visit(const DistribBernoulliDistribution& x)
  {
    v.mDistribConstraints->mDistribBernoulliDistribution.applyTo(m, x);
    return !v.mDistribConstraints->mDistribBernoulliDistribution.empty();
  }


  bool
  visit(const DistribBetaDistribution& x)
  {
    v.mDistribConstraints->mDistribBetaDistribution.applyTo(m, x);
    return !v.mDistribConstraints->mDistribBetaDistribution.empty();
  }


  bool
  visit(const DistribBinomialDistribution& x)
  {
    v.mDistribConstraints->mDistribBinomialDistribution.applyTo(m, x);
    return !v.mDistribConstraints->mDistribBinomialDistribution.empty();
  }


  bool
  visit(const DistribCauchyDistribution& x)
  {
    v.mDistribConstraints->mDistribCauchyDistribution.applyTo(m, x);
    return !v.mDistribConstraints->mDistribCauchyDistribution.empty();
  }


  bool
  visit(const DistribChiSquareDistribution& x)
  {
    v.mDistribConstraints->mDistribChiSquareDistribution.applyTo(m, x);
    return !v.mDistribConstraints->mDistribChiSquareDistribution.empty();
  }


  bool
  visit(const DistribExponentialDistribution& x)
  {
    v.mDistribConstraints->mDistribExponentialDistribution.applyTo(m, x);
    return !v.mDistribConstraints->mDistribExponentialDistribution.empty();
  }


  bool
  visit(const DistribFDistribution& x)
  {
    v.mDistribConstraints->mDistribFDistribution.applyTo(m, x);
    return !v.mDistribConstraints->mDistribFDistribution.empty();
  }


  bool
  visit(const DistribGammaDistribution& x)
  {
    v.mDistribConstraints->mDistribGammaDistribution.applyTo(m, x);
    return !v.mDistribConstraints->mDistribGammaDistribution.empty();
  }


  bool
  visit(const DistribGeometricDistribution& x)
  {
    v.mDistribConstraints->mDistribGeometricDistribution.applyTo(m, x);
    return !v.mDistribConstraints->mDistribGeometricDistribution.empty();
  }


  bool
  visit(const DistribHypergeometricDistribution& x)
  {
    v.mDistribConstraints->mDistribHypergeometricDistribution.applyTo(m, x);
    return !v.mDistribConstraints->mDistribHypergeometricDistribution.empty();
  }


  bool
  visit(const DistribInverseGammaDistribution& x)
  {
    v.mDistribConstraints->mDistribInverseGammaDistribution.applyTo(m, x);
    return !v.mDistribConstraints->mDistribInverseGammaDistribution.empty();
  }


  bool
  visit(const DistribLaPlaceDistribution& x)
  {
    v.mDistribConstraints->mDistribLaPlaceDistribution.applyTo(m, x);
    return !v.mDistribConstraints->mDistribLaPlaceDistribution.empty();
  }


  bool
  visit(const DistribLogNormalDistribution& x)
  {
    v.mDistribConstraints->mDistribLogNormalDistribution.applyTo(m, x);
    return !v.mDistribConstraints->mDistribLogNormalDistribution.empty();
  }


  bool
  visit(const DistribLogisticDistribution& x)
  {
    v.mDistribConstraints->mDistribLogisticDistribution.applyTo(m, x);
    return !v.mDistribConstraints->mDistribLogisticDistribution.empty();
  }


  bool
  visit(const DistribNegativeBinomialDistribution& x)
  {
    v.mDistribConstraints->mDistribNegativeBinomialDistribution.applyTo(m, x);
    return
      !v.mDistribConstraints->mDistribNegativeBinomialDistribution.empty();
  }


  bool
  visit(const DistribParetoDistribution& x)
  {
    v.mDistribConstraints->mDistribParetoDistribution.applyTo(m, x);
    return !v.mDistribConstraints->mDistribParetoDistribution.empty();
  }


  bool
  visit(const DistribPoissonDistribution& x)
  {
    v.mDistribConstraints->mDistribPoissonDistribution.applyTo(m, x);
    return !v.mDistribConstraints->mDistribPoissonDistribution.empty();
  }


  bool
  visit(const DistribRayleighDistribution& x)
  {
    v.mDistribConstraints->mDistribRayleighDistribution.applyTo(m, x);
    return !v.mDistribConstraints->mDistribRayleighDistribution.empty();
  }


  bool
  visit(const DistribStudentTDistribution& x)
  {
    v.mDistribConstraints->mDistribStudentTDistribution.applyTo(m, x);
    return !v.mDistribConstraints->mDistribStudentTDistribution.empty();
  }


  bool
  visit(const DistribWeibullDistribution& x)
  {
    v.mDistribConstraints->mDistribWeibullDistribution.applyTo(m, x);
    return !v.mDistribConstraints->mDistribWeibullDistribution.empty();
  }


  bool
  visit(const DistribUncertainty& x)
  {
    v.mDistribConstraints->mDistribUncertainty.applyTo(m, x);
    return !v.mDistribConstraints->mDistribUncertainty.empty();
  }


  bool
  visit(const DistribUncertStatistics& x)
  {
    v.mDistribConstraints->mDistribUncertStatistics.applyTo(m, x);
    return !v.mDistribConstraints->mDistribUncertStatistics.empty();
  }


  bool
  visit(const DistribUncertStatisticSpan& x)
  {
    v.mDistribConstraints->mDistribUncertStatisticSpan.applyTo(m, x);
    return !v.mDistribConstraints->mDistribUncertStatisticSpan.empty();
  }


  virtual bool
  visit(const SBase& x)
  {
    if (x.getPackageName() != "distrib")
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
      if (code == SBML_DISTRIB_DRAWFROMDISTRIBUTION)
      {
        return visit((const DistribDrawFromDistribution&)x);
      }
      else if (code == SBML_DISTRIB_DISTRIBINPUT)
      {
        return visit((const DistribInput&)x);
      }
      else if (code == SBML_DISTRIB_DISTRIBUTION)
      {
        return visit((const DistribDistribution&)x);
      }
      else if (code == SBML_DISTRIB_UNIVARIATEDISTRIBUTION)
      {
        return visit((const DistribUnivariateDistribution&)x);
      }
      else if (code == SBML_DISTRIB_MULTIVARIATEDISTRIBUTION)
      {
        return visit((const DistribMultivariateDistribution&)x);
      }
      else if (code == SBML_DISTRIB_CONTINUOUSUNIVARIATEDISTRIBUTION)
      {
        return visit((const DistribContinuousUnivariateDistribution&)x);
      }
      else if (code == SBML_DISTRIB_DISCRETEUNIVARIATEDISTRIBUTION)
      {
        return visit((const DistribDiscreteUnivariateDistribution&)x);
      }
      else if (code == SBML_DISTRIB_CATEGORICALUNIVARIATEDISTRIBUTION)
      {
        return visit((const DistribCategoricalUnivariateDistribution&)x);
      }
      else if (code == SBML_DISTRIB_UNCERTVALUE)
      {
        return visit((const DistribUncertValue&)x);
      }
      else if (code == SBML_DISTRIB_UNCERTBOUND)
      {
        return visit((const DistribUncertBound&)x);
      }
      else if (code == SBML_DISTRIB_EXTERNALDISTRIBUTION)
      {
        return visit((const DistribExternalDistribution&)x);
      }
      else if (code == SBML_DISTRIB_EXTERNALPARAMETER)
      {
        return visit((const DistribExternalParameter&)x);
      }
      else if (code == SBML_DISTRIB_NORMALDISTRIBUTION)
      {
        return visit((const DistribNormalDistribution&)x);
      }
      else if (code == SBML_DISTRIB_UNIFORMDISTRIBUTION)
      {
        return visit((const DistribUniformDistribution&)x);
      }
      else if (code == SBML_DISTRIB_CATEGORICALDISTRIBUTION)
      {
        return visit((const DistribCategoricalDistribution&)x);
      }
      else if (code == SBML_DISTRIB_CATEGORY)
      {
        return visit((const DistribCategory&)x);
      }
      else if (code == SBML_DISTRIB_BERNOULLIDISTRIBUTION)
      {
        return visit((const DistribBernoulliDistribution&)x);
      }
      else if (code == SBML_DISTRIB_BETADISTRIBUTION)
      {
        return visit((const DistribBetaDistribution&)x);
      }
      else if (code == SBML_DISTRIB_BINOMIALDISTRIBUTION)
      {
        return visit((const DistribBinomialDistribution&)x);
      }
      else if (code == SBML_DISTRIB_CAUCHYDISTRIBUTION)
      {
        return visit((const DistribCauchyDistribution&)x);
      }
      else if (code == SBML_DISTRIB_CHISQUAREDISTRIBUTION)
      {
        return visit((const DistribChiSquareDistribution&)x);
      }
      else if (code == SBML_DISTRIB_EXPONENTIALDISTRIBUTION)
      {
        return visit((const DistribExponentialDistribution&)x);
      }
      else if (code == SBML_DISTRIB_FDISTRIBUTION)
      {
        return visit((const DistribFDistribution&)x);
      }
      else if (code == SBML_DISTRIB_GAMMADISTRIBUTION)
      {
        return visit((const DistribGammaDistribution&)x);
      }
      else if (code == SBML_DISTRIB_GEOMETRICLDISTRIBUTION)
      {
        return visit((const DistribGeometricDistribution&)x);
      }
      else if (code == SBML_DISTRIB_HYPERGEOMETRICDISTRIBUTION)
      {
        return visit((const DistribHypergeometricDistribution&)x);
      }
      else if (code == SBML_DISTRIB_INVERSEGAMMADISTRIBUTION)
      {
        return visit((const DistribInverseGammaDistribution&)x);
      }
      else if (code == SBML_DISTRIB_LAPLACEDISTRIBUTION)
      {
        return visit((const DistribLaPlaceDistribution&)x);
      }
      else if (code == SBML_DISTRIB_LOGNORMALDISTRIBUTION)
      {
        return visit((const DistribLogNormalDistribution&)x);
      }
      else if (code == SBML_DISTRIB_LOGISTICDISTRIBUTION)
      {
        return visit((const DistribLogisticDistribution&)x);
      }
      else if (code == SBML_DISTRIB_NEGATIVEBINOMIALDISTRIBUTION)
      {
        return visit((const DistribNegativeBinomialDistribution&)x);
      }
      else if (code == SBML_DISTRIB_PARETODISTRIBUTION)
      {
        return visit((const DistribParetoDistribution&)x);
      }
      else if (code == SBML_DISTRIB_POISSONDISTRIBUTION)
      {
        return visit((const DistribPoissonDistribution&)x);
      }
      else if (code == SBML_DISTRIB_RAYLEIGHDISTRIBUTION)
      {
        return visit((const DistribRayleighDistribution&)x);
      }
      else if (code == SBML_DISTRIB_STUDENTTDISTRIBUTION)
      {
        return visit((const DistribStudentTDistribution&)x);
      }
      else if (code == SBML_DISTRIB_WEIBULLDISTRIBUTION)
      {
        return visit((const DistribWeibullDistribution&)x);
      }
      else if (code == SBML_DISTRIB_UNCERTAINTY)
      {
        return visit((const DistribUncertainty&)x);
      }
      else if (code == SBML_DISTRIB_UNCERTSTATISTICS)
      {
        return visit((const DistribUncertStatistics&)x);
      }
      else if (code == SBML_DISTRIB_UNCERTSTATISTICSPAN)
      {
        return visit((const DistribUncertStatisticSpan&)x);
      }
      else
      {
        return SBMLVisitor::visit(x);
      }
    }
  }


protected:

  DistribValidator& v;
  const Model& m;
};


// -------------------------------------------
// DistribValidator
// -------------------------------------------

/*
 * Creates a new DistribValidator object for the given category of validation.
 */
DistribValidator::DistribValidator(SBMLErrorCategory_t category)
  : Validator(category)
{
  mDistribConstraints = new DistribValidatorConstraints();
}


/*
 * Destroys this DistribValidator object.
 */
DistribValidator::~DistribValidator()
{
  delete mDistribConstraints;
}


/*
 * Adds the given VConstraint object to this DistribValidator.
 */
void
DistribValidator::addConstraint(VConstraint* c)
{
  mDistribConstraints->add(c);
}


/*
 * Validates the given SBMLDocument
 */
unsigned int
DistribValidator::validate(const SBMLDocument& d)
{
  const Model* m = d.getModel();

  if (m != NULL)
  {
    DistribValidatingVisitor vv(*this, *m);
    const DistribSBMLDocumentPlugin* plugin = static_cast<const
      DistribSBMLDocumentPlugin*>(d.getPlugin("distrib"));
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
DistribValidator::validate(const std::string& filename)
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


