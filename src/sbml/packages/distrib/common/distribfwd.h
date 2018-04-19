/**
 * @file distribfwd.h
 * @brief Definition of distribfwd.
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


#ifndef distribfwd_H__
#define distribfwd_H__


/**
 * Forward declaration of all opaque C types.
 *
 * Declaring all types up-front avoids "redefinition of type Foo" compile
 * errors and allows our combined C/C++ headers to depend minimally upon each
 * other. Put another way, the type definitions below serve the same purpose as
 * "class Foo;" forward declarations in C++ code.
 */

#ifdef __cplusplus
# define CLASS_OR_STRUCT class
#else
# define CLASS_OR_STRUCT struct
#endif /* __cplusplus */


LIBSBML_CPP_NAMESPACE_BEGIN


typedef CLASS_OR_STRUCT DistribDrawFromDistribution              DistribDrawFromDistribution_t;
typedef CLASS_OR_STRUCT DistribInput                             DistribInput_t;
typedef CLASS_OR_STRUCT DistribDistribution                      DistribDistribution_t;
typedef CLASS_OR_STRUCT DistribUnivariateDistribution            DistribUnivariateDistribution_t;
typedef CLASS_OR_STRUCT DistribMultivariateDistribution          DistribMultivariateDistribution_t;
typedef CLASS_OR_STRUCT DistribContinuousUnivariateDistribution  DistribContinuousUnivariateDistribution_t;
typedef CLASS_OR_STRUCT DistribDiscreteUnivariateDistribution    DistribDiscreteUnivariateDistribution_t;
typedef CLASS_OR_STRUCT DistribCategoricalUnivariateDistribution DistribCategoricalUnivariateDistribution_t;
typedef CLASS_OR_STRUCT DistribUncertValue                       DistribUncertValue_t;
typedef CLASS_OR_STRUCT DistribUncertBound                       DistribUncertBound_t;
typedef CLASS_OR_STRUCT DistribExternalDistribution              DistribExternalDistribution_t;
typedef CLASS_OR_STRUCT DistribExternalParameter                 DistribExternalParameter_t;
typedef CLASS_OR_STRUCT DistribNormalDistribution                DistribNormalDistribution_t;
typedef CLASS_OR_STRUCT DistribUniformDistribution               DistribUniformDistribution_t;
typedef CLASS_OR_STRUCT DistribCategoricalDistribution           DistribCategoricalDistribution_t;
typedef CLASS_OR_STRUCT DistribCategory                          DistribCategory_t;
typedef CLASS_OR_STRUCT DistribBernoulliDistribution             DistribBernoulliDistribution_t;
typedef CLASS_OR_STRUCT DistribBetaDistribution                  DistribBetaDistribution_t;
typedef CLASS_OR_STRUCT DistribBinomialDistribution              DistribBinomialDistribution_t;
typedef CLASS_OR_STRUCT DistribCauchyDistribution                DistribCauchyDistribution_t;
typedef CLASS_OR_STRUCT DistribChiSquareDistribution             DistribChiSquareDistribution_t;
typedef CLASS_OR_STRUCT DistribExponentialDistribution           DistribExponentialDistribution_t;
typedef CLASS_OR_STRUCT DistribFDistribution                     DistribFDistribution_t;
typedef CLASS_OR_STRUCT DistribGammaDistribution                 DistribGammaDistribution_t;
typedef CLASS_OR_STRUCT DistribGeometricDistribution             DistribGeometricDistribution_t;
typedef CLASS_OR_STRUCT DistribHypergeometricDistribution        DistribHypergeometricDistribution_t;
typedef CLASS_OR_STRUCT DistribInverseGammaDistribution          DistribInverseGammaDistribution_t;
typedef CLASS_OR_STRUCT DistribLaPlaceDistribution               DistribLaPlaceDistribution_t;
typedef CLASS_OR_STRUCT DistribLogNormalDistribution             DistribLogNormalDistribution_t;
typedef CLASS_OR_STRUCT DistribLogisticDistribution              DistribLogisticDistribution_t;
typedef CLASS_OR_STRUCT DistribNegativeBinomialDistribution      DistribNegativeBinomialDistribution_t;
typedef CLASS_OR_STRUCT DistribParetoDistribution                DistribParetoDistribution_t;
typedef CLASS_OR_STRUCT DistribPoissonDistribution               DistribPoissonDistribution_t;
typedef CLASS_OR_STRUCT DistribRayleighDistribution              DistribRayleighDistribution_t;
typedef CLASS_OR_STRUCT DistribStudentTDistribution              DistribStudentTDistribution_t;
typedef CLASS_OR_STRUCT DistribWeibullDistribution               DistribWeibullDistribution_t;
typedef CLASS_OR_STRUCT DistribUncertainty                       DistribUncertainty_t;
typedef CLASS_OR_STRUCT DistribUncertStatistics                  DistribUncertStatistics_t;
typedef CLASS_OR_STRUCT DistribUncertStatisticSpan               DistribUncertStatisticSpan_t;
typedef CLASS_OR_STRUCT DistribFunctionDefinitionPlugin          DistribFunctionDefinitionPlugin_t;
typedef CLASS_OR_STRUCT DistribSBasePlugin                       DistribSBasePlugin_t;


LIBSBML_CPP_NAMESPACE_END


#undef CLASS_OR_STRUCT


#endif /* !distribfwd_H__ */


