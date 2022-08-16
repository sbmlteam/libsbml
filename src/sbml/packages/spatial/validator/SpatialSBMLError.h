/**
 * @file SpatialSBMLError.h
 * @brief Definition of the SpatialSBMLError class.
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
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. University of Heidelberg, Heidelberg, Germany
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


#ifndef SpatialSBMLError_H__
#define SpatialSBMLError_H__




LIBSBML_CPP_NAMESPACE_BEGIN




BEGIN_C_DECLS


/**
 * @enum SpatialSBMLErrorCode_t
 * Codes for all SBML-level errors and warnings from the 'spatial' package.
 *
 * These are distinguished from other SBML error codes by having a number
 * between 1200000 and 1299999.
 *
 * @copydetails doc_sbml_error_code_ranges
 */
typedef enum
{
  SpatialUnknown                                              = 1210100
, SpatialNSUndeclared                                         = 1210101
, SpatialElementNotInNs                                       = 1210102
, SpatialDuplicateComponentId                                 = 1210301
, SpatialIdSyntaxRule                                         = 1210302
, SpatialAttributeRequiredMissing                             = 1220101
, SpatialAttributeRequiredMustBeBoolean                       = 1220102
, SpatialAttributeRequiredMustHaveValue                       = 1220103
, SpatialModelAllowedElements                                 = 1220201
, SpatialCompartmentAllowedElements                           = 1220301
, SpatialSpeciesAllowedAttributes                             = 1220401
, SpatialSpeciesIsSpatialMustBeBoolean                        = 1220402
, SpatialCompartmentsMustHaveCompartmentMapping               = 1220450
, SpatialParameterAllowedElements                             = 1220501
, SpatialReactionAllowedAttributes                            = 1220601
, SpatialReactionIsLocalMustBeBoolean                         = 1220602
, SpatialLocalReactionMustDefineCompartment                   = 1220650
, SpatialLocalReactionUnits                                   = 1220651
, SpatialDomainTypeAllowedCoreAttributes                      = 1220701
, SpatialDomainTypeAllowedCoreElements                        = 1220702
, SpatialDomainTypeAllowedAttributes                          = 1220703
, SpatialDomainTypeSpatialDimensionsMustBeInteger             = 1220704
, SpatialDomainTypeNameMustBeString                           = 1220705
, SpatialDomainTypeDimensionsMustMatch3DGeometry              = 1220750
, SpatialDomainTypeDimensionsMustMatch2DGeometry              = 1220751
, SpatialDomainTypeDimensionsMustMatch1DGeometry              = 1220752
, SpatialDomainTypeNoAssignment                               = 1220753
, SpatialDomainAllowedCoreAttributes                          = 1220801
, SpatialDomainAllowedCoreElements                            = 1220802
, SpatialDomainAllowedAttributes                              = 1220803
, SpatialDomainAllowedElements                                = 1220804
, SpatialDomainDomainTypeMustBeDomainType                     = 1220805
, SpatialDomainNameMustBeString                               = 1220806
, SpatialDomainLOInteriorPointsAllowedCoreElements            = 1220807
, SpatialDomainLOInteriorPointsAllowedCoreAttributes          = 1220808
, SpatialDomainNoAssignment                                   = 1220850
, SpatialInteriorPointAllowedCoreAttributes                   = 1220901
, SpatialInteriorPointAllowedCoreElements                     = 1220902
, SpatialInteriorPointAllowedAttributes                       = 1220903
, SpatialInteriorPointCoord1MustBeDouble                      = 1220904
, SpatialInteriorPointCoord2MustBeDouble                      = 1220905
, SpatialInteriorPointCoord3MustBeDouble                      = 1220906
, SpatialInteriorPointOneCoordIn1DGeometry                    = 1220950
, SpatialInteriorPointTwoCoordsIn2DGeometry                   = 1220951
, SpatialInteriorPointThreeCoordsIn3DGeometry                 = 1220952
, SpatialBoundaryAllowedCoreAttributes                        = 1221001
, SpatialBoundaryAllowedCoreElements                          = 1221002
, SpatialBoundaryAllowedAttributes                            = 1221003
, SpatialBoundaryValueMustBeDouble                            = 1221004
, SpatialBoundaryNameMustBeString                             = 1221005
, SpatialBoundaryMinLessThanMax                               = 1221050
, SpatialBoundaryMustBeConstant                               = 1221051
, SpatialBoundaryUnitsShouldMatchCoordinateComponent          = 1221052
, SpatialAdjacentDomainsAllowedCoreAttributes                 = 1221101
, SpatialAdjacentDomainsAllowedCoreElements                   = 1221102
, SpatialAdjacentDomainsAllowedAttributes                     = 1221103
, SpatialAdjacentDomainsDomain1MustBeDomain                   = 1221104
, SpatialAdjacentDomainsDomain2MustBeDomain                   = 1221105
, SpatialAdjacentDomainsNameMustBeString                      = 1221106
, SpatialAdjacentDomainsMustBeAdjacent                        = 1221150
, SpatialGeometryDefinitionAllowedCoreAttributes              = 1221201
, SpatialGeometryDefinitionAllowedCoreElements                = 1221202
, SpatialGeometryDefinitionAllowedAttributes                  = 1221203
, SpatialGeometryDefinitionIsActiveMustBeBoolean              = 1221204
, SpatialGeometryDefinitionNameMustBeString                   = 1221205
, SpatialOneGeometryDefinitionMustBeActive                    = 1221250
, SpatialCompartmentMappingAllowedCoreAttributes              = 1221301
, SpatialCompartmentMappingAllowedCoreElements                = 1221302
, SpatialCompartmentMappingAllowedAttributes                  = 1221303
, SpatialCompartmentMappingDomainTypeMustBeDomainType         = 1221304
, SpatialCompartmentMappingUnitSizeMustBeDouble               = 1221305
, SpatialCompartmentMappingNameMustBeString                   = 1221306
, SpatialCompartmentMappingUnitSizeMustBeFraction             = 1221350
, SpatialCompartmentMappingUnitSizesSum                       = 1221351
, SpatialCompartmentMappingUnits                              = 1221352
, SpatialCoordinateComponentAllowedCoreAttributes             = 1221401
, SpatialCoordinateComponentAllowedCoreElements               = 1221402
, SpatialCoordinateComponentAllowedAttributes                 = 1221403
, SpatialCoordinateComponentAllowedElements                   = 1221404
, SpatialCoordinateComponentTypeMustBeCoordinateKindEnum      = 1221405
, SpatialCoordinateComponentNameMustBeString                  = 1221406
, SpatialCoordinateComponentUnitMustBeUnitSId                 = 1221407
, SpatialSampledFieldGeometryAllowedCoreAttributes            = 1221501
, SpatialSampledFieldGeometryAllowedCoreElements              = 1221502
, SpatialSampledFieldGeometryAllowedAttributes                = 1221503
, SpatialSampledFieldGeometryAllowedElements                  = 1221504
, SpatialSampledFieldGeometrySampledFieldMustBeSampledField   = 1221505
, SpatialSampledFieldGeometryLOSampledVolumesAllowedCoreElements= 1221506
, SpatialSampledFieldGeometryLOSampledVolumesAllowedCoreAttributes= 1221507
, SpatialSampledFieldAllowedCoreAttributes                    = 1221601
, SpatialSampledFieldAllowedCoreElements                      = 1221602
, SpatialSampledFieldAllowedAttributes                        = 1221603
, SpatialSampledFieldDataTypeMustBeDataKindEnum               = 1221604
, SpatialSampledFieldNumSamples1MustBeInteger                 = 1221605
, SpatialSampledFieldInterpolationTypeMustBeInterpolationKindEnum= 1221606
, SpatialSampledFieldCompressionMustBeCompressionKindEnum     = 1221607
, SpatialSampledFieldSamplesMustBeNumeric                     = 1221608
, SpatialSampledFieldSamplesLengthMustBeInteger               = 1221609
, SpatialSampledFieldNameMustBeString                         = 1221610
, SpatialSampledFieldNumSamples2MustBeInteger                 = 1221611
, SpatialSampledFieldNumSamples3MustBeInteger                 = 1221612
, SpatialSampledFieldOneSampleIn1DGeometry                    = 1221650
, SpatialSampledFieldTwoSamplesIn2DGeometry                   = 1221651
, SpatialSampledFieldThreeSamplesIn3DGeometry                 = 1221652
, SpatialSampledFieldSamplesLengthMustMatchUncompressed       = 1221653
, SpatialSampledFieldSamplesLengthMustMatchCompressed         = 1221654
, SpatialSampledFieldFloatArrayDataMustMatch                  = 1221655
, SpatialSampledFieldUIntArrayDataNotNegative                 = 1221656
, SpatialSampledFieldIntArrayDataIntegers                     = 1221657
, SpatialSampledFieldCompressedSamplesMustBeInts              = 1221658
, SpatialSampledVolumeAllowedCoreAttributes                   = 1221701
, SpatialSampledVolumeAllowedCoreElements                     = 1221702
, SpatialSampledVolumeAllowedAttributes                       = 1221703
, SpatialSampledVolumeDomainTypeMustBeDomainType              = 1221704
, SpatialSampledVolumeNameMustBeString                        = 1221705
, SpatialSampledVolumeSampledValueMustBeDouble                = 1221706
, SpatialSampledVolumeMinValueMustBeDouble                    = 1221707
, SpatialSampledVolumeMaxValueMustBeDouble                    = 1221708
, SpatialSampledVolumeSampledValueMinMax                      = 1221750
, SpatialSampledVolumeMinLessThanMax                          = 1221751
, SpatialSampledVolumeValuesMustDiffer                        = 1221752
, SpatialSampledVolumeValuesNotInOtherRange                   = 1221753
, SpatialSampledVolumeRangesCantOverlap                       = 1221754
, SpatialAnalyticGeometryAllowedCoreAttributes                = 1221801
, SpatialAnalyticGeometryAllowedCoreElements                  = 1221802
, SpatialAnalyticGeometryAllowedElements                      = 1221803
, SpatialAnalyticGeometryLOAnalyticVolumesAllowedCoreElements = 1221804
, SpatialAnalyticGeometryLOAnalyticVolumesAllowedCoreAttributes= 1221805
, SpatialAnalyticVolumeAllowedCoreAttributes                  = 1221901
, SpatialAnalyticVolumeAllowedCoreElements                    = 1221902
, SpatialAnalyticVolumeAllowedAttributes                      = 1221903
, SpatialAnalyticVolumeAllowedElements                        = 1221904
, SpatialAnalyticVolumeFunctionTypeMustBeFunctionKindEnum     = 1221905
, SpatialAnalyticVolumeDomainTypeMustBeDomainType             = 1221906
, SpatialAnalyticVolumeNameMustBeString                       = 1221907
, SpatialAnalyticVolumeOrdinalMustBeInteger                   = 1221908
, SpatialAnalyticVolumeOrdinalShouldBeUnique                  = 1221950
, SpatialParametricGeometryAllowedCoreAttributes              = 1222001
, SpatialParametricGeometryAllowedCoreElements                = 1222002
, SpatialParametricGeometryAllowedElements                    = 1222003
, SpatialParametricGeometryLOParametricObjectsAllowedCoreElements= 1222004
, SpatialParametricGeometryLOParametricObjectsAllowedCoreAttributes= 1222005
, SpatialParametricGeometryNotIn1D                            = 1222050
, SpatialParametricObjectAllowedCoreAttributes                = 1222101
, SpatialParametricObjectAllowedCoreElements                  = 1222102
, SpatialParametricObjectAllowedAttributes                    = 1222103
, SpatialParametricObjectPolygonTypeMustBePolygonKindEnum     = 1222104
, SpatialParametricObjectDomainTypeMustBeDomainType           = 1222105
, SpatialParametricObjectPointIndexMustBeString               = 1222106
, SpatialParametricObjectPointIndexLengthMustBeInteger        = 1222107
, SpatialParametricObjectCompressionMustBeCompressionKindEnum = 1222108
, SpatialParametricObjectNameMustBeString                     = 1222109
, SpatialParametricObjectDataTypeMustBeDataKindEnum           = 1222110
, SpatialParametricObjectPointIndexLengthMustMatchUncompressed = 1222150
, SpatialParametricObjectPointIndexLengthMustMatchCompressed  = 1222151
, SpatialParametricObjectThreePointsForTriangles              = 1222152
, SpatialParametricObjectIndexesMustBePositiveIntegers        = 1222154
, SpatialParametricObjectIndexesMustBePoints                  = 1222155
, SpatialParametricObjectFacesSameChirality                   = 1222156
, SpatialParametricObjectMaxTwoPointBorders                   = 1222157
, SpatialCSGeometryAllowedCoreAttributes                      = 1222201
, SpatialCSGeometryAllowedCoreElements                        = 1222202
, SpatialCSGeometryAllowedElements                            = 1222203
, SpatialCSGeometryLOCSGObjectsAllowedCoreElements            = 1222204
, SpatialCSGeometryLOCSGObjectsAllowedCoreAttributes          = 1222205
, SpatialCSGObjectAllowedCoreAttributes                       = 1222301
, SpatialCSGObjectAllowedCoreElements                         = 1222302
, SpatialCSGObjectAllowedAttributes                           = 1222303
, SpatialCSGObjectAllowedElements                             = 1222304
, SpatialCSGObjectDomainTypeMustBeDomainType                  = 1222305
, SpatialCSGObjectNameMustBeString                            = 1222306
, SpatialCSGObjectOrdinalMustBeInteger                        = 1222307
, SpatialCSGObjectOrdinalShouldBeUnique                       = 1222350
, SpatialCSGObjectMustEncompassInteriorPoints                 = 1222351
, SpatialCSGNodeAllowedCoreAttributes                         = 1222401
, SpatialCSGNodeAllowedCoreElements                           = 1222402
, SpatialCSGNodeAllowedAttributes                             = 1222403
, SpatialCSGNodeNameMustBeString                              = 1222404
, SpatialCSGTranslationAllowedCoreAttributes                  = 1222601
, SpatialCSGTranslationAllowedCoreElements                    = 1222602
, SpatialCSGTranslationAllowedAttributes                      = 1222603
, SpatialCSGTranslationTranslateXMustBeDouble                 = 1222604
, SpatialCSGTranslationTranslateYMustBeDouble                 = 1222605
, SpatialCSGTranslationTranslateZMustBeDouble                 = 1222606
, SpatialCSGTranslationAllowedElements                        = 1222650
, SpatialCSGTranslationTranslateYRequiredIn2D                 = 1222651
, SpatialCSGTranslationTranslateZRequiredIn3D                 = 1222652
, SpatialCSGTranslationNoTranslateYIn1D                       = 1222653
, SpatialCSGTranslationNoTranslateZIn2D                       = 1222654
, SpatialCSGRotationAllowedCoreAttributes                     = 1222701
, SpatialCSGRotationAllowedCoreElements                       = 1222702
, SpatialCSGRotationAllowedAttributes                         = 1222703
, SpatialCSGRotationRotateXMustBeDouble                       = 1222704
, SpatialCSGRotationRotateAngleInRadiansMustBeDouble          = 1222705
, SpatialCSGRotationRotateYMustBeDouble                       = 1222706
, SpatialCSGRotationRotateZMustBeDouble                       = 1222707
, SpatialCSGRotationAllowedElements                           = 1222750
, SpatialCSGRotationRotateYRequiredIn2D                       = 1222751
, SpatialCSGRotationRotateZRequiredIn3D                       = 1222752
, SpatialCSGRotationNoRotateYIn1D                             = 1222753
, SpatialCSGRotationNoRotateZIn2D                             = 1222754
, SpatialCSGRotationNoOriginIn3D                              = 1222755
, SpatialCSGScaleAllowedCoreAttributes                        = 1222801
, SpatialCSGScaleAllowedCoreElements                          = 1222802
, SpatialCSGScaleAllowedAttributes                            = 1222803
, SpatialCSGScaleScaleXMustBeDouble                           = 1222804
, SpatialCSGScaleScaleYMustBeDouble                           = 1222805
, SpatialCSGScaleScaleZMustBeDouble                           = 1222806
, SpatialCSGScaleAllowedElements                              = 1222850
, SpatialCSGScaleScaleYRequiredIn2D                           = 1222851
, SpatialCSGScaleScaleZRequiredIn3D                           = 1222852
, SpatialCSGScaleNoScaleYIn1D                                 = 1222853
, SpatialCSGScaleNoScaleZIn2D                                 = 1222854
, SpatialCSGHomogeneousTransformationAllowedCoreAttributes    = 1222901
, SpatialCSGHomogeneousTransformationAllowedCoreElements      = 1222902
, SpatialCSGHomogeneousTransformationAllowedElements          = 1222903
, SpatialTransformationComponentAllowedCoreAttributes         = 1223001
, SpatialTransformationComponentAllowedCoreElements           = 1223002
, SpatialTransformationComponentAllowedAttributes             = 1223003
, SpatialTransformationComponentComponentsMustBeDoubleArray   = 1223004
, SpatialTransformationComponentComponentsLengthMustBeInteger = 1223005
, SpatialTransformationComponentComponentsLengthMustBe16      = 1223050
, SpatialTransformationComponentArrayLengthMustBe16           = 1223051
, SpatialCSGPrimitiveAllowedCoreAttributes                    = 1223101
, SpatialCSGPrimitiveAllowedCoreElements                      = 1223102
, SpatialCSGPrimitiveAllowedAttributes                        = 1223103
, SpatialCSGPrimitivePrimitiveTypeMustBePrimitiveKindEnum     = 1223104
, SpatialCSGPrimitive3DShapes                                 = 1223150
, SpatialCSGPrimitive2DShapes                                 = 1223151
, SpatialCSGSetOperatorAllowedCoreAttributes                  = 1223201
, SpatialCSGSetOperatorAllowedCoreElements                    = 1223202
, SpatialCSGSetOperatorAllowedAttributes                      = 1223203
, SpatialCSGSetOperatorAllowedElements                        = 1223204
, SpatialCSGSetOperatorOperationTypeMustBeSetOperationEnum    = 1223205
, SpatialCSGSetOperatorLOCSGNodesAllowedCoreElements          = 1223208
, SpatialCSGSetOperatorLOCSGNodesAllowedCoreAttributes        = 1223209
, SpatialCSGSetOperatorTwoComplementsForDifference             = 1223250
, SpatialCSGSetOperatorNoComplementsUnionIntersection          = 1223251
, SpatialCSGSetOperatorDifferenceMustHaveTwoChildren          = 1223252
, SpatialCSGSetOperatorComplementsMustReferenceChildren       = 1223253
, SpatialCSGSetOperatorShouldHaveTwoPlusChildren              = 1223254
, SpatialSpatialSymbolReferenceAllowedCoreAttributes          = 1223301
, SpatialSpatialSymbolReferenceAllowedCoreElements            = 1223302
, SpatialSpatialSymbolReferenceAllowedAttributes              = 1223303
, SpatialSpatialSymbolReferenceSpatialRefMustReferenceMath    = 1223304
, SpatialSpatialSymbolReferenceUniqueRef                      = 1223350
, SpatialDiffusionCoefficientAllowedCoreAttributes            = 1223401
, SpatialDiffusionCoefficientAllowedCoreElements              = 1223402
, SpatialDiffusionCoefficientAllowedAttributes                = 1223403
, SpatialDiffusionCoefficientVariableMustBeSpeciesOrParam     = 1223404
, SpatialDiffusionCoefficientTypeMustBeDiffusionKindEnum      = 1223405
, SpatialDiffusionCoefficientCoordinateReference1MustBeCoordinateKindEnum= 1223406
, SpatialDiffusionCoefficientCoordinateReference2MustBeCoordinateKindEnum= 1223407
, SpatialDiffusionCoefficientNoCoordinateReferencesForIsotropic = 1223450
, SpatialDiffusionCoefficientTwoCoordinateReferencesForTensor = 1223451
, SpatialDiffusionCoefficientOneCoordinateReferencesForAnisotropic = 1223452
, SpatialDiffusionCoefficientUnits                            = 1223453
, SpatialDiffusionCoefficientCoordinateReferenceDifference    = 1223454
, SpatialDiffusionCoefficientCoordinateReferenceNoYIn1D       = 1223455
, SpatialDiffusionCoefficientCoordinateReferenceNoZIn2D       = 1223456
, SpatialNoDiffusionCoefficientOverlap                        = 1223457
, SpatialDiffusionCoefficientVariableMustNotBeSelf            = 1223458
, SpatialAdvectionCoefficientAllowedCoreAttributes            = 1223501
, SpatialAdvectionCoefficientAllowedCoreElements              = 1223502
, SpatialAdvectionCoefficientAllowedAttributes                = 1223503
, SpatialAdvectionCoefficientVariableMustBeSpeciesOrParam     = 1223504
, SpatialAdvectionCoefficientCoordinateMustBeCoordinateKindEnum= 1223505
, SpatialAdvectionCoefficientUnits                            = 1223550
, SpatialAdvectionCoefficientsMustBeUnique                    = 1223551
, SpatialAdvectionCoefficientVariableMustNotBeSelf            = 1223552
, SpatialBoundaryConditionAllowedCoreAttributes               = 1223601
, SpatialBoundaryConditionAllowedCoreElements                 = 1223602
, SpatialBoundaryConditionAllowedAttributes                   = 1223603
, SpatialBoundaryConditionVariableMustBeSpecies               = 1223604
, SpatialBoundaryConditionTypeMustBeBoundaryKindEnum          = 1223605
, SpatialBoundaryConditionCoordinateBoundaryMustBeBoundary    = 1223606
, SpatialBoundaryConditionBoundaryDomainTypeMustBeDomainType  = 1223607
, SpatialBoundaryConditionBoundaryDomainTypeOrCoordinateBoundary = 1223650
, SpatialBoundaryConditionsMustBeUnique                       = 1223651
, SpatialDirichletUnits                                       = 1223652
, SpatialNeumannUnits                                         = 1223653
, SpatialGeometryAllowedCoreAttributes                        = 1223701
, SpatialGeometryAllowedCoreElements                          = 1223702
, SpatialGeometryAllowedAttributes                            = 1223703
, SpatialGeometryAllowedElements                              = 1223704
, SpatialGeometryCoordinateSystemMustBeGeometryKindEnum       = 1223705
, SpatialGeometryLOCoordinateComponentsAllowedCoreElements    = 1223706
, SpatialGeometryLODomainTypesAllowedCoreElements             = 1223707
, SpatialGeometryLODomainsAllowedCoreElements                 = 1223708
, SpatialGeometryLOAdjacentDomainsAllowedCoreElements         = 1223709
, SpatialGeometryLOGeometryDefinitionsAllowedCoreElements     = 1223710
, SpatialGeometryLOSampledFieldsAllowedCoreElements           = 1223711
, SpatialGeometryLOCoordinateComponentsAllowedCoreAttributes  = 1223712
, SpatialGeometryLODomainTypesAllowedCoreAttributes           = 1223713
, SpatialGeometryLODomainsAllowedCoreAttributes               = 1223714
, SpatialGeometryLOAdjacentDomainsAllowedCoreAttributes       = 1223715
, SpatialGeometryLOGeometryDefinitionsAllowedCoreAttributes   = 1223716
, SpatialGeometryLOSampledFieldsAllowedCoreAttributes         = 1223717
, SpatialGeometryLOCoordinateComponentsOneToThreeChildren     = 1223751
, SpatialGeometryCoordinateComponent1DisX                     = 1223752
, SpatialGeometryCoordinateComponent2DisXY                    = 1223753
, SpatialGeometryCoordinateComponent3DisXYZ                   = 1223754
, SpatialMixedGeometryAllowedCoreAttributes                   = 1223801
, SpatialMixedGeometryAllowedCoreElements                     = 1223802
, SpatialMixedGeometryAllowedElements                         = 1223803
, SpatialMixedGeometryLOGeometryDefinitionsAllowedCoreElements= 1223804
, SpatialMixedGeometryLOOrdinalMappingsAllowedCoreElements    = 1223805
, SpatialMixedGeometryLOGeometryDefinitionsAllowedCoreAttributes= 1223806
, SpatialMixedGeometryLOOrdinalMappingsAllowedCoreAttributes  = 1223807
, SpatialMixedGeometryChildrenNotActive                       = 1223850
, SpatialOrdinalMappingAllowedCoreAttributes                  = 1223901
, SpatialOrdinalMappingAllowedCoreElements                    = 1223902
, SpatialOrdinalMappingAllowedAttributes                      = 1223903
, SpatialOrdinalMappingGeometryDefinitionMustBeGeometryDefinition= 1223904
, SpatialOrdinalMappingOrdinalMustBeInteger                   = 1223905
, SpatialOrdinalMappingOrdinalShouldBeUnique                  = 1223950
, SpatialSpatialPointsAllowedCoreAttributes                   = 1224001
, SpatialSpatialPointsAllowedCoreElements                     = 1224002
, SpatialSpatialPointsAllowedAttributes                       = 1224003
, SpatialSpatialPointsCompressionMustBeCompressionKindEnum    = 1224004
, SpatialSpatialPointsUncompressedArrayDataMustBeDouble       = 1224005
, SpatialSpatialPointsArrayDataLengthMustBeInteger            = 1224006
, SpatialSpatialPointsNameMustBeString                        = 1224007
, SpatialSpatialPointsDataTypeMustBeDataKindEnum              = 1224008
, SpatialSpatialPointsDataLengthMustMatchUncompressed         = 1224050
, SpatialSpatialPointsDataLengthMustMatchCompressed           = 1224051
, SpatialSpatialPointsArrayDataMultipleOfDimensions           = 1224052
, SpatialSpatialPointsFloatArrayDataMustMatch                 = 1224053
, SpatialSpatialPointsUIntArrayDataNotNegative                = 1224054
, SpatialSpatialPointsIntArrayDataIntegers                    = 1224055
, SpatialSpatialPointsCompressedArrayDataMustBeInts           = 1224056
} SpatialSBMLErrorCode_t;


END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SpatialSBMLError_H__ */


