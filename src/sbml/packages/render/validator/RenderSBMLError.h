/**
 * @file RenderSBMLError.h
 * @brief Definition of the RenderSBMLError class.
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


#ifndef RenderSBMLError_H__
#define RenderSBMLError_H__




LIBSBML_CPP_NAMESPACE_BEGIN




BEGIN_C_DECLS


/**
 * @enum RenderSBMLErrorCode_t
 * Codes for all SBML-level errors and warnings from the 'render' package.
 *
 * These are distinguished from other SBML error codes by having a number
 * between 1300000 and 1399999.
 *
 * @copydetails doc_sbml_error_code_ranges
 */
typedef enum
{
  RenderUnknown                                               = 1310100
, RenderNSUndeclared                                          = 1310101
, RenderElementNotInNs                                        = 1310102
, RenderDuplicateComponentId                                  = 1310301
, RenderIdSyntaxRule                                          = 1310302
, RenderAttributeRequiredMissing                              = 1320101
, RenderAttributeRequiredMustBeBoolean                        = 1320102
, RenderAttributeRequiredMustHaveValue                        = 1320103
, RenderGraphicalObjectAllowedAttributes                      = 1320201
, RenderGraphicalObjectObjectRoleMustBeString                 = 1320202
, RenderLayoutAllowedElements                                 = 1320301
, RenderLayoutEmptyLOElements                                 = 1320302
, RenderLayoutLOLocalRenderInformationAllowedCoreElements     = 1320303
, RenderLayoutLOLocalRenderInformationAllowedCoreAttributes   = 1320304
, RenderLayoutLOLocalRenderInformationAllowedAttributes       = 1320305
, RenderLayoutVersionMajorMustBeNonNegativeInteger            = 1320306
, RenderLayoutVersionMinorMustBeNonNegativeInteger            = 1320307
, RenderLayoutDefaultValuesMustBeString                       = 1320308
, RenderListOfLayoutsAllowedElements                          = 1320401
, RenderListOfLayoutsEmptyLOElements                          = 1320402
, RenderListOfLayoutsLOGlobalRenderInformationAllowedCoreElements= 1320403
, RenderListOfLayoutsLOGlobalRenderInformationAllowedCoreAttributes= 1320404
, RenderListOfLayoutsLOGlobalRenderInformationAllowedAttributes= 1320405
, RenderListOfLayoutsVersionMajorMustBeNonNegativeInteger     = 1320406
, RenderListOfLayoutsVersionMinorMustBeNonNegativeInteger     = 1320407
, RenderListOfLayoutsDefaultValuesMustBeString                = 1320408
, RenderColorDefinitionAllowedCoreAttributes                  = 1320501
, RenderColorDefinitionAllowedCoreElements                    = 1320502
, RenderColorDefinitionAllowedAttributes                      = 1320503
, RenderColorDefinitionValueMustBeString                      = 1320504
, RenderColorDefinitionNameMustBeString                       = 1320505
, RenderEllipseAllowedCoreAttributes                          = 1320601
, RenderEllipseAllowedCoreElements                            = 1320602
, RenderEllipseAllowedAttributes                              = 1320603
, RenderEllipseCxMustBeRelAbsVector                                 = 1320604
, RenderEllipseCyMustBeRelAbsVector                                 = 1320605
, RenderEllipseRxMustBeRelAbsVector                                 = 1320606
, RenderEllipseRatioMustBeDouble                              = 1320607
, RenderEllipseCzMustBeRelAbsVector                                 = 1320608
, RenderEllipseRyMustBeRelAbsVector                                 = 1320609
, RenderGlobalRenderInformationAllowedCoreAttributes          = 1320701
, RenderGlobalRenderInformationAllowedCoreElements            = 1320702
, RenderGlobalRenderInformationAllowedElements                = 1320703
, RenderGlobalRenderInformationEmptyLOElements                = 1320704
, RenderGlobalRenderInformationLOGlobalStylesAllowedCoreElements= 1320705
, RenderGlobalRenderInformationLOGlobalStylesAllowedCoreAttributes= 1320706
, RenderGlobalStyleAllowedCoreAttributes                      = 1320801
, RenderGlobalStyleAllowedCoreElements                        = 1320802
, RenderGradientBaseAllowedCoreAttributes                     = 1320901
, RenderGradientBaseAllowedCoreElements                       = 1320902
, RenderGradientBaseAllowedAttributes                         = 1320903
, RenderGradientBaseAllowedElements                           = 1320904
, RenderGradientBaseNameMustBeString                          = 1320905
, RenderGradientBaseSpreadMethodMustBeGradientSpreadMethodEnum= 1320906
, RenderGradientStopAllowedCoreAttributes                     = 1321001
, RenderGradientStopAllowedCoreElements                       = 1321002
, RenderGradientStopAllowedAttributes                         = 1321003
, RenderGradientStopStopColorMustBeString                     = 1321004
, RenderGradientStopOffsetMustBeRelAbsVector                        = 1321005
, RenderRenderGroupAllowedCoreAttributes                      = 1321101
, RenderRenderGroupAllowedCoreElements                        = 1321102
, RenderRenderGroupAllowedAttributes                          = 1321103
, RenderRenderGroupAllowedElements                            = 1321104
, RenderRenderGroupStartHeadMustBeLineEnding                  = 1321105
, RenderRenderGroupEndHeadMustBeLineEnding                    = 1321106
, RenderRenderGroupFontFamilyMustBeString                     = 1321107
, RenderRenderGroupFontWeightMustBeFontWeightEnum             = 1321108
, RenderRenderGroupFontStyleMustBeFontStyleEnum               = 1321109
, RenderRenderGroupTextAnchorMustBeHTextAnchorEnum            = 1321110
, RenderRenderGroupVtextAnchorMustBeVTextAnchorEnum           = 1321111
, RenderRenderGroupFontSizeMustBeRelAbsVector                       = 1321112
, RenderImageAllowedCoreAttributes                            = 1321201
, RenderImageAllowedCoreElements                              = 1321202
, RenderImageAllowedAttributes                                = 1321203
, RenderImageHrefMustBeString                                 = 1321204
, RenderImageXMustBeRelAbsVector                                    = 1321205
, RenderImageYMustBeRelAbsVector                                    = 1321206
, RenderImageWidthMustBeRelAbsVector                                = 1321207
, RenderImageHeightMustBeRelAbsVector                               = 1321208
, RenderImageHrefMustBeImageFile                              = 1321209
, RenderImageZMustBeRelAbsVector                                    = 1321210
, RenderLineEndingAllowedCoreAttributes                       = 1321301
, RenderLineEndingAllowedCoreElements                         = 1321302
, RenderLineEndingAllowedAttributes                           = 1321303
, RenderLineEndingAllowedElements                             = 1321304
, RenderLineEndingEnableRotationalMappingMustBeBoolean        = 1321305
, RenderLinearGradientAllowedCoreAttributes                   = 1321401
, RenderLinearGradientAllowedCoreElements                     = 1321402
, RenderLinearGradientAllowedAttributes                       = 1321403
, RenderLinearGradientX1MustBeRelAbsVector                          = 1321404
, RenderLinearGradientY1MustBeRelAbsVector                          = 1321405
, RenderLinearGradientZ1MustBeRelAbsVector                          = 1321406
, RenderLinearGradientX2MustBeRelAbsVector                          = 1321407
, RenderLinearGradientY2MustBeRelAbsVector                          = 1321408
, RenderLinearGradientZ2MustBeRelAbsVector                          = 1321409
, RenderLocalRenderInformationAllowedCoreAttributes           = 1321501
, RenderLocalRenderInformationAllowedCoreElements             = 1321502
, RenderLocalRenderInformationAllowedElements                 = 1321503
, RenderLocalRenderInformationEmptyLOElements                 = 1321504
, RenderLocalRenderInformationLOLocalStylesAllowedCoreElements= 1321505
, RenderLocalRenderInformationLOLocalStylesAllowedCoreAttributes= 1321506
, RenderLocalStyleAllowedCoreAttributes                       = 1321601
, RenderLocalStyleAllowedCoreElements                         = 1321602
, RenderLocalStyleAllowedAttributes                           = 1321603
, RenderLocalStyleIdListMustBeString                          = 1321604
, RenderPolygonAllowedCoreAttributes                          = 1321701
, RenderPolygonAllowedCoreElements                            = 1321702
, RenderPolygonAllowedElements                                = 1321703
, RenderPolygonEmptyLOElements                                = 1321704
, RenderPolygonLORenderPointsAllowedCoreElements              = 1321705
, RenderPolygonLORenderPointsAllowedCoreAttributes            = 1321706
, RenderRadialGradientAllowedCoreAttributes                   = 1321801
, RenderRadialGradientAllowedCoreElements                     = 1321802
, RenderRadialGradientAllowedAttributes                       = 1321803
, RenderRadialGradientCxMustBeRelAbsVector                          = 1321804
, RenderRadialGradientCyMustBeRelAbsVector                          = 1321805
, RenderRadialGradientCzMustBeRelAbsVector                          = 1321806
, RenderRadialGradientRMustBeRelAbsVector                           = 1321807
, RenderRadialGradientFxMustBeRelAbsVector                          = 1321808
, RenderRadialGradientFyMustBeRelAbsVector                          = 1321809
, RenderRadialGradientFzMustBeRelAbsVector                          = 1321810
, RenderRectangleAllowedCoreAttributes                        = 1321901
, RenderRectangleAllowedCoreElements                          = 1321902
, RenderRectangleAllowedAttributes                            = 1321903
, RenderRectangleXMustBeRelAbsVector                                = 1321904
, RenderRectangleYMustBeRelAbsVector                                = 1321905
, RenderRectangleWidthMustBeRelAbsVector                            = 1321906
, RenderRectangleHeightMustBeRelAbsVector                           = 1321907
, RenderRectangleRatioMustBeDouble                            = 1321908
, RenderRectangleZMustBeRelAbsVector                                = 1321909
, RenderRectangleRXMustBeRelAbsVector                               = 1321910
, RenderRectangleRYMustBeRelAbsVector                               = 1321911
, RenderRenderCubicBezierAllowedCoreAttributes                = 1322001
, RenderRenderCubicBezierAllowedCoreElements                  = 1322002
, RenderRenderCubicBezierAllowedAttributes                    = 1322003
, RenderRenderCubicBezierBasePoint1_xMustBeRelAbsVector             = 1322004
, RenderRenderCubicBezierBasePoint1_yMustBeRelAbsVector             = 1322005
, RenderRenderCubicBezierBasePoint2_xMustBeRelAbsVector             = 1322006
, RenderRenderCubicBezierBasePoint2_yMustBeRelAbsVector             = 1322007
, RenderRenderCubicBezierBasePoint1_zMustBeRelAbsVector             = 1322008
, RenderRenderCubicBezierBasePoint2_zMustBeRelAbsVector             = 1322009
, RenderRenderCurveAllowedCoreAttributes                      = 1322101
, RenderRenderCurveAllowedCoreElements                        = 1322102
, RenderRenderCurveAllowedAttributes                          = 1322103
, RenderRenderCurveAllowedElements                            = 1322104
, RenderRenderCurveStartHeadMustBeLineEnding                  = 1322105
, RenderRenderCurveEndHeadMustBeLineEnding                    = 1322106
, RenderRenderCurveEmptyLOElements                            = 1322107
, RenderRenderCurveLORenderPointsAllowedCoreElements          = 1322108
, RenderRenderCurveLORenderPointsAllowedCoreAttributes        = 1322109
, RenderRenderPointAllowedCoreAttributes                      = 1322201
, RenderRenderPointAllowedCoreElements                        = 1322202
, RenderRenderPointAllowedAttributes                          = 1322203
, RenderRenderPointXMustBeRelAbsVector                              = 1322204
, RenderRenderPointYMustBeRelAbsVector                              = 1322205
, RenderRenderPointZMustBeRelAbsVector                              = 1322206
, RenderTextAllowedCoreAttributes                             = 1322301
, RenderTextAllowedCoreElements                               = 1322302
, RenderTextAllowedAttributes                                 = 1322303
, RenderTextXMustBeRelAbsVector                                     = 1322304
, RenderTextYMustBeRelAbsVector                                     = 1322305
, RenderTextFontFamilyMustBeString                            = 1322306
, RenderTextFontWeightMustBeFontWeightEnum                    = 1322307
, RenderTextFontStyleMustBeFontStyleEnum                      = 1322308
, RenderTextTextAnchorMustBeHTextAnchorEnum                   = 1322309
, RenderTextVtextAnchorMustBeVTextAnchorEnum                  = 1322310
, RenderTextZMustBeRelAbsVector                                     = 1322311
, RenderTextFontSizeMustBeRelAbsVector                              = 1322312
, RenderTransformation2DAllowedCoreAttributes                 = 1322401
, RenderTransformation2DAllowedCoreElements                   = 1322402
, RenderTransformationAllowedCoreAttributes                   = 1322501
, RenderTransformationAllowedCoreElements                     = 1322502
, RenderTransformationAllowedAttributes                       = 1322503
, RenderTransformationTransformMustBeString                   = 1322504
, RenderTransformationNameMustBeString                        = 1322505
, RenderGraphicalPrimitive1DAllowedCoreAttributes             = 1322601
, RenderGraphicalPrimitive1DAllowedCoreElements               = 1322602
, RenderGraphicalPrimitive1DAllowedAttributes                 = 1322603
, RenderGraphicalPrimitive1DStrokeMustBeString                = 1322604
, RenderGraphicalPrimitive1DStrokeWidthMustBeDouble           = 1322605
, RenderGraphicalPrimitive1DStrokeDashArrayMustBeString       = 1322606
, RenderGraphicalPrimitive2DAllowedCoreAttributes             = 1322701
, RenderGraphicalPrimitive2DAllowedCoreElements               = 1322702
, RenderGraphicalPrimitive2DAllowedAttributes                 = 1322703
, RenderGraphicalPrimitive2DFillMustBeString                  = 1322704
, RenderGraphicalPrimitive2DFillRuleMustBeFillRuleEnum        = 1322705
, RenderStyleAllowedCoreAttributes                            = 1322801
, RenderStyleAllowedCoreElements                              = 1322802
, RenderStyleAllowedAttributes                                = 1322803
, RenderStyleAllowedElements                                  = 1322804
, RenderStyleNameMustBeString                                 = 1322805
, RenderStyleRoleListMustBeString                             = 1322806
, RenderStyleTypeListMustBeString                             = 1322807
, RenderRenderInformationBaseAllowedCoreAttributes            = 1322901
, RenderRenderInformationBaseAllowedCoreElements              = 1322902
, RenderRenderInformationBaseAllowedAttributes                = 1322903
, RenderRenderInformationBaseAllowedElements                  = 1322904
, RenderRenderInformationBaseNameMustBeString                 = 1322905
, RenderRenderInformationBaseProgramNameMustBeString          = 1322906
, RenderRenderInformationBaseProgramVersionMustBeString       = 1322907
, RenderRenderInformationBaseReferenceRenderInformationMustBeRenderInformationBase= 1322908
, RenderRenderInformationBaseBackgroundColorMustBeString      = 1322909
, RenderRenderInformationBaseLOElementChildren                = 1322910
, RenderRenderInformationBaseLOColorDefinitionsAllowedCoreElements= 1322911
, RenderRenderInformationBaseLOGradientBasesAllowedCoreElements= 1322912
, RenderRenderInformationBaseLOLineEndingsAllowedCoreElements = 1322913
, RenderRenderInformationBaseLOColorDefinitionsAllowedCoreAttributes= 1322914
, RenderRenderInformationBaseLOGradientBasesAllowedCoreAttributes= 1322915
, RenderRenderInformationBaseLOLineEndingsAllowedCoreAttributes= 1322916
, RenderDefaultValuesAllowedCoreAttributes                    = 1323001
, RenderDefaultValuesAllowedCoreElements                      = 1323002
, RenderDefaultValuesAllowedAttributes                        = 1323003
, RenderDefaultValuesBackgroundColorMustBeString              = 1323004
, RenderDefaultValuesSpreadMethodMustBeGradientSpreadMethodEnum= 1323005
, RenderDefaultValuesFillMustBeString                         = 1323006
, RenderDefaultValuesFillRuleMustBeFillRuleEnum               = 1323007
, RenderDefaultValuesStrokeMustBeString                       = 1323008
, RenderDefaultValuesStrokeWidthMustBeDouble                  = 1323009
, RenderDefaultValuesFontFamilyMustBeString                   = 1323010
, RenderDefaultValuesFontWeightMustBeFontWeightEnum           = 1323011
, RenderDefaultValuesFontStyleMustBeFontStyleEnum             = 1323012
, RenderDefaultValuesTextAnchorMustBeHTextAnchorEnum          = 1323013
, RenderDefaultValuesVtextAnchorMustBeVTextAnchorEnum         = 1323014
, RenderDefaultValuesStartHeadMustBeLineEnding                = 1323015
, RenderDefaultValuesEndHeadMustBeLineEnding                  = 1323016
, RenderDefaultValuesEnableRotationalMappingMustBeBoolean     = 1323017
, RenderDefaultValuesLinearGradient_x1MustBeString            = 1323018
, RenderDefaultValuesLinearGradient_y1MustBeString            = 1323019
, RenderDefaultValuesLinearGradient_z1MustBeString            = 1323020
, RenderDefaultValuesLinearGradient_x2MustBeString            = 1323021
, RenderDefaultValuesLinearGradient_y2MustBeString            = 1323022
, RenderDefaultValuesLinearGradient_z2MustBeString            = 1323023
, RenderDefaultValuesRadialGradient_cxMustBeString            = 1323024
, RenderDefaultValuesRadialGradient_cyMustBeString            = 1323025
, RenderDefaultValuesRadialGradient_czMustBeString            = 1323026
, RenderDefaultValuesRadialGradient_rMustBeString             = 1323027
, RenderDefaultValuesRadialGradient_fxMustBeString            = 1323028
, RenderDefaultValuesRadialGradient_fyMustBeString            = 1323029
, RenderDefaultValuesRadialGradient_fzMustBeString            = 1323030
, RenderDefaultValuesDefault_zMustBeString                    = 1323031
, RenderDefaultValuesFontSizeMustBeString                     = 1323032
} RenderSBMLErrorCode_t;


END_C_DECLS




LIBSBML_CPP_NAMESPACE_END




#endif /* !RenderSBMLError_H__ */


