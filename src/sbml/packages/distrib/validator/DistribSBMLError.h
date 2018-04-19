/**
 * @file DistribSBMLError.h
 * @brief Definition of the DistribSBMLError class.
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


#ifndef DistribSBMLError_H__
#define DistribSBMLError_H__




LIBSBML_CPP_NAMESPACE_BEGIN




BEGIN_C_DECLS


/**
 * @enum DistribSBMLErrorCode_t
 * Codes for all SBML-level errors and warnings from the 'distrib' package.
 *
 * These are distinguished from other SBML error codes by having a number
 * between 1500000 and 1599999.
 *
 * @copydetails doc_sbml_error_code_ranges
 */
typedef enum
{
  DistribUnknown                                              = 1510100
, DistribNSUndeclared                                         = 1510101
, DistribElementNotInNs                                       = 1510102
, DistribDuplicateComponentId                                 = 1510301
, DistribIdSyntaxRule                                         = 1510302
, DistribAttributeRequiredMissing                             = 1520101
, DistribAttributeRequiredMustBeBoolean                       = 1520102
, DistribAttributeRequiredMustHaveValue                       = 1520103
, DistribFunctionDefinitionAllowedElements                    = 1520201
, DistribSBaseAllowedElements                                 = 1520301
, DistribDistribDrawFromDistributionAllowedCoreAttributes     = 1520401
, DistribDistribDrawFromDistributionAllowedCoreElements       = 1520402
, DistribDistribDrawFromDistributionAllowedAttributes         = 1520403
, DistribDistribDrawFromDistributionAllowedElements           = 1520404
, DistribDistribDrawFromDistributionNameMustBeString          = 1520405
, DistribDistribDrawFromDistributionLODistribInputsAllowedCoreElements= 1520407
, DistribDistribDrawFromDistributionLODistribInputsAllowedCoreAttributes= 1520409
, DistribDistribInputAllowedCoreAttributes                    = 1520501
, DistribDistribInputAllowedCoreElements                      = 1520502
, DistribDistribInputAllowedAttributes                        = 1520503
, DistribDistribInputNameMustBeString                         = 1520504
, DistribDistribInputIndexMustBeNonNegativeInteger            = 1520505
, DistribDistribDistributionAllowedCoreAttributes             = 1520601
, DistribDistribDistributionAllowedCoreElements               = 1520602
, DistribDistribUnivariateDistributionAllowedCoreAttributes   = 1520701
, DistribDistribUnivariateDistributionAllowedCoreElements     = 1520702
, DistribDistribMultivariateDistributionAllowedCoreAttributes = 1520801
, DistribDistribMultivariateDistributionAllowedCoreElements   = 1520802
, DistribDistribContinuousUnivariateDistributionAllowedCoreAttributes= 1520901
, DistribDistribContinuousUnivariateDistributionAllowedCoreElements= 1520902
, DistribDistribContinuousUnivariateDistributionAllowedAttributes= 1520903
, DistribDistribContinuousUnivariateDistributionAllowedElements= 1520904
, DistribDistribContinuousUnivariateDistributionNameMustBeString= 1520905
, DistribDistribDiscreteUnivariateDistributionAllowedCoreAttributes= 1521001
, DistribDistribDiscreteUnivariateDistributionAllowedCoreElements= 1521002
, DistribDistribDiscreteUnivariateDistributionAllowedAttributes= 1521003
, DistribDistribDiscreteUnivariateDistributionAllowedElements = 1521004
, DistribDistribDiscreteUnivariateDistributionNameMustBeString= 1521005
, DistribDistribCategoricalUnivariateDistributionAllowedCoreAttributes= 1521101
, DistribDistribCategoricalUnivariateDistributionAllowedCoreElements= 1521102
, DistribDistribUncertValueAllowedCoreAttributes              = 1521201
, DistribDistribUncertValueAllowedCoreElements                = 1521202
, DistribDistribUncertValueAllowedAttributes                  = 1521203
, DistribDistribUncertValueNameMustBeString                   = 1521204
, DistribDistribUncertValueValueMustBeDouble                  = 1521205
, DistribDistribUncertValueVarMustBeSBase                     = 1521206
, DistribDistribUncertValueUnitsMustBeUnitSId                 = 1521207
, DistribDistribUncertBoundAllowedCoreAttributes              = 1521301
, DistribDistribUncertBoundAllowedCoreElements                = 1521302
, DistribDistribUncertBoundAllowedAttributes                  = 1521303
, DistribDistribUncertBoundInclusiveMustBeBoolean             = 1521304
, DistribDistribUncertBoundNameMustBeString                   = 1521306
, DistribDistribExternalDistributionAllowedCoreAttributes     = 1521401
, DistribDistribExternalDistributionAllowedCoreElements       = 1521402
, DistribDistribExternalDistributionAllowedAttributes         = 1521403
, DistribDistribExternalDistributionAllowedElements           = 1521404
, DistribDistribExternalDistributionDefinitionURLMustBeString = 1521405
, DistribDistribExternalDistributionNameMustBeString          = 1521407
, DistribDistribExternalDistributionLODistribExternalParametersAllowedCoreElements= 1521409
, DistribDistribExternalDistributionLODistribExternalParametersAllowedCoreAttributes= 1521411
, DistribDistribExternalParameterAllowedCoreAttributes        = 1521501
, DistribDistribExternalParameterAllowedCoreElements          = 1521502
, DistribDistribExternalParameterAllowedAttributes            = 1521503
, DistribDistribExternalParameterAllowedElements              = 1521504
, DistribDistribExternalParameterDefinitionURLMustBeString    = 1521505
, DistribDistribExternalParameterNameMustBeString             = 1521507
, DistribDistribExternalParameterLODistribExternalParametersAllowedCoreElements= 1521509
, DistribDistribExternalParameterLODistribExternalParametersAllowedCoreAttributes= 1521511
, DistribDistribNormalDistributionAllowedCoreAttributes       = 1521601
, DistribDistribNormalDistributionAllowedCoreElements         = 1521602
, DistribDistribNormalDistributionAllowedAttributes           = 1521603
, DistribDistribNormalDistributionAllowedElements             = 1521604
, DistribDistribNormalDistributionNameMustBeString            = 1521605
, DistribDistribUniformDistributionAllowedCoreAttributes      = 1521701
, DistribDistribUniformDistributionAllowedCoreElements        = 1521702
, DistribDistribUniformDistributionAllowedAttributes          = 1521703
, DistribDistribUniformDistributionAllowedElements            = 1521704
, DistribDistribUniformDistributionNameMustBeString           = 1521705
, DistribDistribCategoricalDistributionAllowedCoreAttributes  = 1521801
, DistribDistribCategoricalDistributionAllowedCoreElements    = 1521802
, DistribDistribCategoricalDistributionAllowedAttributes      = 1521803
, DistribDistribCategoricalDistributionAllowedElements        = 1521804
, DistribDistribCategoricalDistributionNameMustBeString       = 1521805
, DistribDistribCategoricalDistributionEmptyReqdLOElements    = 1521807
, DistribDistribCategoricalDistributionLODistribCategoriesAllowedCoreElements= 1521808
, DistribDistribCategoricalDistributionLODistribCategoriesAllowedCoreAttributes= 1521810
, DistribDistribCategoryAllowedCoreAttributes                 = 1521901
, DistribDistribCategoryAllowedCoreElements                   = 1521902
, DistribDistribCategoryAllowedAttributes                     = 1521903
, DistribDistribCategoryAllowedElements                       = 1521904
, DistribDistribCategoryNameMustBeString                      = 1521905
, DistribDistribCategoryRankMustBeNonNegativeInteger          = 1521906
, DistribDistribBernoulliDistributionAllowedCoreAttributes    = 1522001
, DistribDistribBernoulliDistributionAllowedCoreElements      = 1522002
, DistribDistribBernoulliDistributionAllowedAttributes        = 1522003
, DistribDistribBernoulliDistributionAllowedElements          = 1522004
, DistribDistribBernoulliDistributionNameMustBeString         = 1522005
, DistribDistribBetaDistributionAllowedCoreAttributes         = 1522101
, DistribDistribBetaDistributionAllowedCoreElements           = 1522102
, DistribDistribBetaDistributionAllowedAttributes             = 1522103
, DistribDistribBetaDistributionAllowedElements               = 1522104
, DistribDistribBetaDistributionNameMustBeString              = 1522105
, DistribDistribBinomialDistributionAllowedCoreAttributes     = 1522201
, DistribDistribBinomialDistributionAllowedCoreElements       = 1522202
, DistribDistribBinomialDistributionAllowedAttributes         = 1522203
, DistribDistribBinomialDistributionAllowedElements           = 1522204
, DistribDistribBinomialDistributionNameMustBeString          = 1522205
, DistribDistribCauchyDistributionAllowedCoreAttributes       = 1522301
, DistribDistribCauchyDistributionAllowedCoreElements         = 1522302
, DistribDistribCauchyDistributionAllowedAttributes           = 1522303
, DistribDistribCauchyDistributionAllowedElements             = 1522304
, DistribDistribCauchyDistributionNameMustBeString            = 1522305
, DistribDistribChiSquareDistributionAllowedCoreAttributes    = 1522401
, DistribDistribChiSquareDistributionAllowedCoreElements      = 1522402
, DistribDistribChiSquareDistributionAllowedAttributes        = 1522403
, DistribDistribChiSquareDistributionAllowedElements          = 1522404
, DistribDistribChiSquareDistributionNameMustBeString         = 1522405
, DistribDistribExponentialDistributionAllowedCoreAttributes  = 1522501
, DistribDistribExponentialDistributionAllowedCoreElements    = 1522502
, DistribDistribExponentialDistributionAllowedAttributes      = 1522503
, DistribDistribExponentialDistributionAllowedElements        = 1522504
, DistribDistribExponentialDistributionNameMustBeString       = 1522505
, DistribDistribFDistributionAllowedCoreAttributes            = 1522601
, DistribDistribFDistributionAllowedCoreElements              = 1522602
, DistribDistribFDistributionAllowedAttributes                = 1522603
, DistribDistribFDistributionAllowedElements                  = 1522604
, DistribDistribFDistributionNameMustBeString                 = 1522605
, DistribDistribGammaDistributionAllowedCoreAttributes        = 1522701
, DistribDistribGammaDistributionAllowedCoreElements          = 1522702
, DistribDistribGammaDistributionAllowedAttributes            = 1522703
, DistribDistribGammaDistributionAllowedElements              = 1522704
, DistribDistribGammaDistributionNameMustBeString             = 1522705
, DistribDistribGeometricDistributionAllowedCoreAttributes    = 1522801
, DistribDistribGeometricDistributionAllowedCoreElements      = 1522802
, DistribDistribGeometricDistributionAllowedAttributes        = 1522803
, DistribDistribGeometricDistributionAllowedElements          = 1522804
, DistribDistribGeometricDistributionNameMustBeString         = 1522805
, DistribDistribHypergeometricDistributionAllowedCoreAttributes= 1522901
, DistribDistribHypergeometricDistributionAllowedCoreElements = 1522902
, DistribDistribHypergeometricDistributionAllowedAttributes   = 1522903
, DistribDistribHypergeometricDistributionAllowedElements     = 1522904
, DistribDistribHypergeometricDistributionNameMustBeString    = 1522905
, DistribDistribInverseGammaDistributionAllowedCoreAttributes = 1523001
, DistribDistribInverseGammaDistributionAllowedCoreElements   = 1523002
, DistribDistribInverseGammaDistributionAllowedAttributes     = 1523003
, DistribDistribInverseGammaDistributionAllowedElements       = 1523004
, DistribDistribInverseGammaDistributionNameMustBeString      = 1523005
, DistribDistribLaPlaceDistributionAllowedCoreAttributes      = 1523101
, DistribDistribLaPlaceDistributionAllowedCoreElements        = 1523102
, DistribDistribLaPlaceDistributionAllowedAttributes          = 1523103
, DistribDistribLaPlaceDistributionAllowedElements            = 1523104
, DistribDistribLaPlaceDistributionNameMustBeString           = 1523105
, DistribDistribLogNormalDistributionAllowedCoreAttributes    = 1523201
, DistribDistribLogNormalDistributionAllowedCoreElements      = 1523202
, DistribDistribLogNormalDistributionAllowedAttributes        = 1523203
, DistribDistribLogNormalDistributionAllowedElements          = 1523204
, DistribDistribLogNormalDistributionNameMustBeString         = 1523205
, DistribDistribLogisticDistributionAllowedCoreAttributes     = 1523301
, DistribDistribLogisticDistributionAllowedCoreElements       = 1523302
, DistribDistribLogisticDistributionAllowedAttributes         = 1523303
, DistribDistribLogisticDistributionAllowedElements           = 1523304
, DistribDistribLogisticDistributionNameMustBeString          = 1523305
, DistribDistribNegativeBinomialDistributionAllowedCoreAttributes= 1523401
, DistribDistribNegativeBinomialDistributionAllowedCoreElements= 1523402
, DistribDistribNegativeBinomialDistributionAllowedAttributes = 1523403
, DistribDistribNegativeBinomialDistributionAllowedElements   = 1523404
, DistribDistribNegativeBinomialDistributionNameMustBeString  = 1523405
, DistribDistribParetoDistributionAllowedCoreAttributes       = 1523501
, DistribDistribParetoDistributionAllowedCoreElements         = 1523502
, DistribDistribParetoDistributionAllowedAttributes           = 1523503
, DistribDistribParetoDistributionAllowedElements             = 1523504
, DistribDistribParetoDistributionNameMustBeString            = 1523505
, DistribDistribPoissonDistributionAllowedCoreAttributes      = 1523601
, DistribDistribPoissonDistributionAllowedCoreElements        = 1523602
, DistribDistribPoissonDistributionAllowedAttributes          = 1523603
, DistribDistribPoissonDistributionAllowedElements            = 1523604
, DistribDistribPoissonDistributionNameMustBeString           = 1523605
, DistribDistribRayleighDistributionAllowedCoreAttributes     = 1523701
, DistribDistribRayleighDistributionAllowedCoreElements       = 1523702
, DistribDistribRayleighDistributionAllowedAttributes         = 1523703
, DistribDistribRayleighDistributionAllowedElements           = 1523704
, DistribDistribRayleighDistributionNameMustBeString          = 1523705
, DistribDistribStudentTDistributionAllowedCoreAttributes     = 1523801
, DistribDistribStudentTDistributionAllowedCoreElements       = 1523802
, DistribDistribStudentTDistributionAllowedAttributes         = 1523803
, DistribDistribStudentTDistributionAllowedElements           = 1523804
, DistribDistribStudentTDistributionNameMustBeString          = 1523805
, DistribDistribWeibullDistributionAllowedCoreAttributes      = 1523901
, DistribDistribWeibullDistributionAllowedCoreElements        = 1523902
, DistribDistribWeibullDistributionAllowedAttributes          = 1523903
, DistribDistribWeibullDistributionAllowedElements            = 1523904
, DistribDistribWeibullDistributionNameMustBeString           = 1523905
, DistribDistribUncertaintyAllowedCoreAttributes              = 1524001
, DistribDistribUncertaintyAllowedCoreElements                = 1524002
, DistribDistribUncertaintyAllowedAttributes                  = 1524003
, DistribDistribUncertaintyAllowedElements                    = 1524004
, DistribDistribUncertaintyNameMustBeString                   = 1524005
, DistribDistribUncertStatisticsAllowedCoreAttributes         = 1524101
, DistribDistribUncertStatisticsAllowedCoreElements           = 1524102
, DistribDistribUncertStatisticsAllowedAttributes             = 1524103
, DistribDistribUncertStatisticsAllowedElements               = 1524104
, DistribDistribUncertStatisticsNameMustBeString              = 1524105
, DistribDistribUncertStatisticsLODistribExternalParametersAllowedCoreElements= 1524107
, DistribDistribUncertStatisticsLODistribExternalParametersAllowedCoreAttributes= 1524109
, DistribDistribUncertStatisticSpanAllowedCoreAttributes      = 1524201
, DistribDistribUncertStatisticSpanAllowedCoreElements        = 1524202
, DistribDistribUncertStatisticSpanAllowedAttributes          = 1524203
, DistribDistribUncertStatisticSpanNameMustBeString           = 1524204
, DistribDistribUncertStatisticSpanVarLowerMustBeSBase        = 1524205
, DistribDistribUncertStatisticSpanValueLowerMustBeDouble     = 1524206
, DistribDistribUncertStatisticSpanVarUpperMustBeSBase        = 1524207
, DistribDistribUncertStatisticSpanValueUpperMustBeDouble     = 1524208
} DistribSBMLErrorCode_t;


END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !DistribSBMLError_H__ */


