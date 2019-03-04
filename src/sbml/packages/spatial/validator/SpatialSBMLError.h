/**
 * @file SpatialSBMLError.h
 * @brief Definition of the SpatialSBMLError class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
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
, SpatialParameterAllowedElements                             = 1220501
, SpatialReactionAllowedAttributes                            = 1220601
, SpatialReactionIsLocalMustBeBoolean                         = 1220602
, SpatialDomainTypeAllowedCoreAttributes                      = 1220701
, SpatialDomainTypeAllowedCoreElements                        = 1220702
, SpatialDomainTypeAllowedAttributes                          = 1220703
, SpatialDomainTypeSpatialDimensionsMustBeInteger             = 1220704
, SpatialDomainTypeNameMustBeString                           = 1220705
, SpatialDomainAllowedCoreAttributes                          = 1220801
, SpatialDomainAllowedCoreElements                            = 1220802
, SpatialDomainAllowedAttributes                              = 1220803
, SpatialDomainAllowedElements                                = 1220804
, SpatialDomainDomainTypeMustBeDomainType                     = 1220805
, SpatialDomainNameMustBeString                               = 1220806
, SpatialDomainLOInteriorPointsAllowedCoreElements            = 1220807
, SpatialDomainLOInteriorPointsAllowedCoreAttributes          = 1220808
, SpatialInteriorPointAllowedCoreAttributes                   = 1220901
, SpatialInteriorPointAllowedCoreElements                     = 1220902
, SpatialInteriorPointAllowedAttributes                       = 1220903
, SpatialInteriorPointCoord1MustBeDouble                      = 1220904
, SpatialInteriorPointCoord2MustBeDouble                      = 1220905
, SpatialInteriorPointCoord3MustBeDouble                      = 1220906
, SpatialBoundaryAllowedCoreAttributes                        = 1221001
, SpatialBoundaryAllowedCoreElements                          = 1221002
, SpatialBoundaryAllowedAttributes                            = 1221003
, SpatialBoundaryValueMustBeDouble                            = 1221004
, SpatialBoundaryNameMustBeString                             = 1221005
, SpatialAdjacentDomainsAllowedCoreAttributes                 = 1221101
, SpatialAdjacentDomainsAllowedCoreElements                   = 1221102
, SpatialAdjacentDomainsAllowedAttributes                     = 1221103
, SpatialAdjacentDomainsDomain1MustBeDomain                   = 1221104
, SpatialAdjacentDomainsDomain2MustBeDomain                   = 1221105
, SpatialAdjacentDomainsNameMustBeString                      = 1221106
, SpatialGeometryDefinitionAllowedCoreAttributes              = 1221201
, SpatialGeometryDefinitionAllowedCoreElements                = 1221202
, SpatialGeometryDefinitionAllowedAttributes                  = 1221203
, SpatialGeometryDefinitionIsActiveMustBeBoolean              = 1221204
, SpatialGeometryDefinitionNameMustBeString                   = 1221205
, SpatialCompartmentMappingAllowedCoreAttributes              = 1221301
, SpatialCompartmentMappingAllowedCoreElements                = 1221302
, SpatialCompartmentMappingAllowedAttributes                  = 1221303
, SpatialCompartmentMappingDomainTypeMustBeDomainType         = 1221304
, SpatialCompartmentMappingUnitSizeMustBeDouble               = 1221305
, SpatialCompartmentMappingNameMustBeString                   = 1221306
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
, SpatialSampledFieldSamplesMustBeString                      = 1221608
, SpatialSampledFieldSamplesLengthMustBeInteger               = 1221609
, SpatialSampledFieldNameMustBeString                         = 1221610
, SpatialSampledFieldNumSamples2MustBeInteger                 = 1221611
, SpatialSampledFieldNumSamples3MustBeInteger                 = 1221612
, SpatialSampledVolumeAllowedCoreAttributes                   = 1221701
, SpatialSampledVolumeAllowedCoreElements                     = 1221702
, SpatialSampledVolumeAllowedAttributes                       = 1221703
, SpatialSampledVolumeDomainTypeMustBeDomainType              = 1221704
, SpatialSampledVolumeNameMustBeString                        = 1221705
, SpatialSampledVolumeSampledValueMustBeDouble                = 1221706
, SpatialSampledVolumeMinValueMustBeDouble                    = 1221707
, SpatialSampledVolumeMaxValueMustBeDouble                    = 1221708
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
, SpatialParametricGeometryAllowedCoreAttributes              = 1222001
, SpatialParametricGeometryAllowedCoreElements                = 1222002
, SpatialParametricGeometryAllowedElements                    = 1222003
, SpatialParametricGeometryLOParametricObjectsAllowedCoreElements= 1222004
, SpatialParametricGeometryLOParametricObjectsAllowedCoreAttributes= 1222005
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
, SpatialCSGNodeAllowedCoreAttributes                         = 1222401
, SpatialCSGNodeAllowedCoreElements                           = 1222402
, SpatialCSGNodeAllowedAttributes                             = 1222403
, SpatialCSGNodeNameMustBeString                              = 1222404
, SpatialCSGTransformationAllowedCoreAttributes               = 1222501
, SpatialCSGTransformationAllowedCoreElements                 = 1222502
, SpatialCSGTransformationAllowedElements                     = 1222503
, SpatialCSGTranslationAllowedCoreAttributes                  = 1222601
, SpatialCSGTranslationAllowedCoreElements                    = 1222602
, SpatialCSGTranslationAllowedAttributes                      = 1222603
, SpatialCSGTranslationTranslateXMustBeDouble                 = 1222604
, SpatialCSGTranslationTranslateYMustBeDouble                 = 1222605
, SpatialCSGTranslationTranslateZMustBeDouble                 = 1222606
, SpatialCSGRotationAllowedCoreAttributes                     = 1222701
, SpatialCSGRotationAllowedCoreElements                       = 1222702
, SpatialCSGRotationAllowedAttributes                         = 1222703
, SpatialCSGRotationRotateXMustBeDouble                       = 1222704
, SpatialCSGRotationRotateAngleInRadiansMustBeDouble          = 1222705
, SpatialCSGRotationRotateYMustBeDouble                       = 1222706
, SpatialCSGRotationRotateZMustBeDouble                       = 1222707
, SpatialCSGScaleAllowedCoreAttributes                        = 1222801
, SpatialCSGScaleAllowedCoreElements                          = 1222802
, SpatialCSGScaleAllowedAttributes                            = 1222803
, SpatialCSGScaleScaleXMustBeDouble                           = 1222804
, SpatialCSGScaleScaleYMustBeDouble                           = 1222805
, SpatialCSGScaleScaleZMustBeDouble                           = 1222806
, SpatialCSGHomogeneousTransformationAllowedCoreAttributes    = 1222901
, SpatialCSGHomogeneousTransformationAllowedCoreElements      = 1222902
, SpatialCSGHomogeneousTransformationAllowedElements          = 1222903
, SpatialTransformationComponentAllowedCoreAttributes         = 1223001
, SpatialTransformationComponentAllowedCoreElements           = 1223002
, SpatialTransformationComponentAllowedAttributes             = 1223003
, SpatialTransformationComponentComponentsMustBeString        = 1223004
, SpatialTransformationComponentComponentsLengthMustBeInteger = 1223005
, SpatialCSGPrimitiveAllowedCoreAttributes                    = 1223101
, SpatialCSGPrimitiveAllowedCoreElements                      = 1223102
, SpatialCSGPrimitiveAllowedAttributes                        = 1223103
, SpatialCSGPrimitivePrimitiveTypeMustBePrimitiveKindEnum     = 1223104
, SpatialCSGSetOperatorAllowedCoreAttributes                  = 1223201
, SpatialCSGSetOperatorAllowedCoreElements                    = 1223202
, SpatialCSGSetOperatorAllowedAttributes                      = 1223203
, SpatialCSGSetOperatorAllowedElements                        = 1223204
, SpatialCSGSetOperatorOperationTypeMustBeSetOperationEnum    = 1223205
, SpatialCSGSetOperatorComplementAMustBeCSGNode               = 1223206
, SpatialCSGSetOperatorComplementBMustBeCSGNode               = 1223207
, SpatialCSGSetOperatorLOCSGNodesAllowedCoreElements          = 1223208
, SpatialCSGSetOperatorLOCSGNodesAllowedCoreAttributes        = 1223209
, SpatialSpatialSymbolReferenceAllowedCoreAttributes          = 1223301
, SpatialSpatialSymbolReferenceAllowedCoreElements            = 1223302
, SpatialSpatialSymbolReferenceAllowedAttributes              = 1223303
, SpatialSpatialSymbolReferenceSpatialRefMustBeGeometry       = 1223304
, SpatialDiffusionCoefficientAllowedCoreAttributes            = 1223401
, SpatialDiffusionCoefficientAllowedCoreElements              = 1223402
, SpatialDiffusionCoefficientAllowedAttributes                = 1223403
, SpatialDiffusionCoefficientVariableMustBeSpecies            = 1223404
, SpatialDiffusionCoefficientTypeMustBeDiffusionKindEnum      = 1223405
, SpatialDiffusionCoefficientCoordinateReference1MustBeCoordinateKindEnum= 1223406
, SpatialDiffusionCoefficientCoordinateReference2MustBeCoordinateKindEnum= 1223407
, SpatialAdvectionCoefficientAllowedCoreAttributes            = 1223501
, SpatialAdvectionCoefficientAllowedCoreElements              = 1223502
, SpatialAdvectionCoefficientAllowedAttributes                = 1223503
, SpatialAdvectionCoefficientVariableMustBeSpecies            = 1223504
, SpatialAdvectionCoefficientCoordinateMustBeCoordinateKindEnum= 1223505
, SpatialBoundaryConditionAllowedCoreAttributes               = 1223601
, SpatialBoundaryConditionAllowedCoreElements                 = 1223602
, SpatialBoundaryConditionAllowedAttributes                   = 1223603
, SpatialBoundaryConditionVariableMustBeSpecies               = 1223604
, SpatialBoundaryConditionTypeMustBeBoundaryKindEnum          = 1223605
, SpatialBoundaryConditionCoordinateBoundaryMustBeBoundary    = 1223606
, SpatialBoundaryConditionBoundaryDomainTypeMustBeDomainType  = 1223607
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
, SpatialMixedGeometryAllowedCoreAttributes                   = 1223801
, SpatialMixedGeometryAllowedCoreElements                     = 1223802
, SpatialMixedGeometryAllowedElements                         = 1223803
, SpatialMixedGeometryLOGeometryDefinitionsAllowedCoreElements= 1223804
, SpatialMixedGeometryLOOrdinalMappingsAllowedCoreElements    = 1223805
, SpatialMixedGeometryLOGeometryDefinitionsAllowedCoreAttributes= 1223806
, SpatialMixedGeometryLOOrdinalMappingsAllowedCoreAttributes  = 1223807
, SpatialOrdinalMappingAllowedCoreAttributes                  = 1223901
, SpatialOrdinalMappingAllowedCoreElements                    = 1223902
, SpatialOrdinalMappingAllowedAttributes                      = 1223903
, SpatialOrdinalMappingGeometryDefinitionMustBeGeometryDefinition= 1223904
, SpatialOrdinalMappingOrdinalMustBeInteger                   = 1223905
, SpatialSpatialPointsAllowedCoreAttributes                   = 1224001
, SpatialSpatialPointsAllowedCoreElements                     = 1224002
, SpatialSpatialPointsAllowedAttributes                       = 1224003
, SpatialSpatialPointsCompressionMustBeCompressionKindEnum    = 1224004
, SpatialSpatialPointsArrayDataMustBeString                   = 1224005
, SpatialSpatialPointsArrayDataLengthMustBeInteger            = 1224006
, SpatialSpatialPointsNameMustBeString                        = 1224007
, SpatialSpatialPointsDataTypeMustBeDataKindEnum              = 1224008
} SpatialSBMLErrorCode_t;


END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !SpatialSBMLError_H__ */


