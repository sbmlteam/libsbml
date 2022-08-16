
/** @cond doxygenLibsbmlInternal */

/**
 * @file SpatialConsistencyConstraintsDeclared.cxx
 * @brief Definition of SpatialConsistencyConstraintsDeclared.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
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

#include "sbml/packages/spatial/validator/SpatialCompartmentMappingUnitSizesCheck.h"
#include "sbml/packages/spatial/validator/SpatialSpatialSymbolReferenceUniqueRefCheck.h"
#include "sbml/packages/spatial/validator/SpatialUniqueDiffusionCoefficientsCheck.h"
#include "sbml/packages/spatial/validator/SpatialUniqueAdvectionCoefficientsCheck.h"
#include "sbml/packages/spatial/validator/SpatialUniqueAnalyticVolumeOrdinalsCheck.h"
#include "sbml/packages/spatial/validator/SpatialUniqueBoundaryConditionsCheck.h"
#include "sbml/packages/spatial/validator/SpatialUniqueSampledVolumeValueCheck.h"
#include "sbml/packages/spatial/validator/SpatialUniqueOrdinalMappingOrdinalsCheck.h"
#include "sbml/packages/spatial/validator/SpatialOrdinalMappingGeometryDefinitionCheck.h"
#include "sbml/packages/spatial/validator/SpatialSampledVolumeValueNotInRangeCheck.h"
#include "sbml/packages/spatial/validator/SpatialSampledVolumeRangeOverlapCheck.h"
#include "sbml/packages/spatial/validator/SpatialUniqueCSGObjectOrdinalsCheck.h"

//Constraints declared in SpatialConsistencyConstraints.cpp
addConstraint(new VConstraintDomainSpatialDomainDomainTypeMustBeDomainType(*this));
addConstraint(new VConstraintAdjacentDomainsSpatialAdjacentDomainsDomain1MustBeDomain(*this));
addConstraint(new VConstraintAdjacentDomainsSpatialAdjacentDomainsDomain2MustBeDomain(*this));
addConstraint(new VConstraintCompartmentMappingSpatialCompartmentMappingDomainTypeMustBeDomainType(*this));
addConstraint(new VConstraintCoordinateComponentSpatialCoordinateComponentAllowedElements(*this));
addConstraint(new VConstraintCoordinateComponentSpatialCoordinateComponentUnitMustBeUnitSId(*this));
addConstraint(new VConstraintSampledFieldGeometrySpatialSampledFieldGeometrySampledFieldMustBeSampledField(*this));
addConstraint(new VConstraintReactionSpatialLocalReactionMustDefineCompartment(*this));
addConstraint(new VConstraintDomainTypeSpatialDomainTypeDimensionsMustMatch3DGeometry(*this));
addConstraint(new VConstraintDomainTypeSpatialDomainTypeDimensionsMustMatch2DGeometry(*this));
addConstraint(new VConstraintDomainTypeSpatialDomainTypeDimensionsMustMatch1DGeometry(*this));
addConstraint(new VConstraintSampledFieldSpatialSampledFieldOneSampleIn1DGeometry(*this));
addConstraint(new VConstraintSampledFieldSpatialSampledFieldTwoSamplesIn2DGeometry(*this));
addConstraint(new VConstraintSampledFieldSpatialSampledFieldThreeSamplesIn3DGeometry(*this));
addConstraint(new VConstraintSampledVolumeSpatialSampledVolumeSampledValueMinMax(*this));
addConstraint(new VConstraintCompartmentMappingSpatialCompartmentMappingUnitSizeMustBeFraction(*this));
addConstraint(new VConstraintSpeciesSpatialCompartmentsMustHaveCompartmentMapping(*this));
addConstraint(new VConstraintSpatialSymbolReferenceSpatialSpatialSymbolReferenceSpatialRefMustReferenceMath(*this));
addConstraint(new VConstraintDiffusionCoefficientSpatialDiffusionCoefficientNoCoordinateReferencesForIsotropic(*this));
addConstraint(new VConstraintDiffusionCoefficientSpatialDiffusionCoefficientTwoCoordinateReferencesForTensor(*this));
addConstraint(new VConstraintDiffusionCoefficientSpatialDiffusionCoefficientOneCoordinateReferencesForAnisotropic(*this));
addConstraint(new VConstraintDiffusionCoefficientSpatialDiffusionCoefficientCoordinateReferenceDifference(*this));
addConstraint(new VConstraintDiffusionCoefficientSpatialDiffusionCoefficientCoordinateReferenceNoYIn1D(*this));
addConstraint(new VConstraintDiffusionCoefficientSpatialDiffusionCoefficientCoordinateReferenceNoZIn2D(*this));
addConstraint(new VConstraintAdvectionCoefficientSpatialAdvectionCoefficientVariableMustBeSpeciesOrParam(*this));
addConstraint(new VConstraintAdvectionCoefficientSpatialAdvectionCoefficientVariableMustNotBeSelf(*this));
addConstraint(new VConstraintBoundaryConditionSpatialBoundaryConditionVariableMustBeSpecies(*this));
addConstraint(new VConstraintCoordinateComponentSpatialBoundaryMinLessThanMax(*this));
addConstraint(new VConstraintParameterSpatialBoundaryMustBeConstant(*this));
addConstraint(new VConstraintParameterSpatialDomainTypeNoAssignment(*this));
addConstraint(new VConstraintParameterSpatialDomainNoAssignment(*this));
addConstraint(new VConstraintInteriorPointSpatialInteriorPointOneCoordIn1DGeometry(*this));
addConstraint(new VConstraintInteriorPointSpatialInteriorPointTwoCoordsIn2DGeometry(*this));
addConstraint(new VConstraintInteriorPointSpatialInteriorPointThreeCoordsIn3DGeometry(*this));
addConstraint(new VConstraintAdjacentDomainsSpatialAdjacentDomainsMustBeAdjacent(*this));
addConstraint(new VConstraintSampledVolumeSpatialSampledVolumeMinLessThanMax(*this));
addConstraint(new VConstraintCSGPrimitiveSpatialCSGPrimitive3DShapes(*this));
addConstraint(new VConstraintCSGPrimitiveSpatialCSGPrimitive2DShapes(*this));
addConstraint(new VConstraintCSGSetOperatorSpatialCSGSetOperatorTwoComplementsForDifference(*this));
addConstraint(new VConstraintCSGSetOperatorSpatialCSGSetOperatorNoComplementsUnionIntersection(*this));
addConstraint(new VConstraintCSGSetOperatorSpatialCSGSetOperatorDifferenceMustHaveTwoChildren(*this));
addConstraint(new VConstraintCSGSetOperatorSpatialCSGSetOperatorComplementsMustReferenceChildren(*this));
addConstraint(new VConstraintCSGSetOperatorSpatialCSGSetOperatorShouldHaveTwoPlusChildren(*this));
addConstraint(new VConstraintCSGTranslationSpatialCSGTranslationTranslateYRequiredIn2D(*this));
addConstraint(new VConstraintCSGTranslationSpatialCSGTranslationTranslateZRequiredIn3D(*this));
addConstraint(new VConstraintCSGTranslationSpatialCSGTranslationNoTranslateYIn1D(*this));
addConstraint(new VConstraintCSGTranslationSpatialCSGTranslationNoTranslateZIn2D(*this));
addConstraint(new VConstraintCSGRotationSpatialCSGRotationRotateYRequiredIn2D(*this));
addConstraint(new VConstraintCSGRotationSpatialCSGRotationRotateZRequiredIn3D(*this));
addConstraint(new VConstraintCSGRotationSpatialCSGRotationNoRotateYIn1D(*this));
addConstraint(new VConstraintCSGRotationSpatialCSGRotationNoRotateZIn2D(*this));
addConstraint(new VConstraintCSGRotationSpatialCSGRotationNoOriginIn3D(*this));
addConstraint(new VConstraintCSGScaleSpatialCSGScaleScaleYRequiredIn2D(*this));
addConstraint(new VConstraintCSGScaleSpatialCSGScaleScaleZRequiredIn3D(*this));
addConstraint(new VConstraintCSGScaleSpatialCSGScaleNoScaleYIn1D(*this));
addConstraint(new VConstraintCSGScaleSpatialCSGScaleNoScaleZIn2D(*this));
addConstraint(new VConstraintTransformationComponentSpatialTransformationComponentComponentsLengthMustBe16(*this));
addConstraint(new VConstraintTransformationComponentSpatialTransformationComponentArrayLengthMustBe16(*this));
addConstraint(new VConstraintParametricGeometrySpatialParametricGeometryNotIn1D(*this));
addConstraint(new VConstraintSpatialPointsSpatialSpatialPointsDataLengthMustMatchUncompressed(*this));
addConstraint(new VConstraintSpatialPointsSpatialSpatialPointsDataLengthMustMatchCompressed(*this));
addConstraint(new VConstraintSpatialPointsSpatialSpatialPointsArrayDataMultipleOfDimensions(*this));
addConstraint(new VConstraintSpatialPointsSpatialSpatialPointsFloatArrayDataMustMatch(*this));
addConstraint(new VConstraintSpatialPointsSpatialSpatialPointsUIntArrayDataNotNegative(*this));
addConstraint(new VConstraintSpatialPointsSpatialSpatialPointsIntArrayDataIntegers(*this));
addConstraint(new VConstraintParametricObjectSpatialParametricObjectPointIndexLengthMustMatchUncompressed(*this));
addConstraint(new VConstraintParametricObjectSpatialParametricObjectPointIndexLengthMustMatchCompressed(*this));
addConstraint(new VConstraintParametricObjectSpatialParametricObjectThreePointsForTriangles(*this));
//addConstraint(new VConstraintParametricObjectSpatialParametricObjectFourPointsForQuadrilaterals(*this));
addConstraint(new VConstraintParametricObjectSpatialParametricObjectIndexesMustBePoints(*this));
addConstraint(new VConstraintParametricObjectSpatialParametricObjectFacesSameChirality(*this));
addConstraint(new VConstraintParametricObjectSpatialParametricObjectMaxTwoPointBorders(*this));
addConstraint(new VConstraintSampledFieldSpatialSampledFieldSamplesLengthMustMatchUncompressed(*this));
addConstraint(new VConstraintSampledFieldSpatialSampledFieldSamplesLengthMustMatchCompressed(*this));
addConstraint(new VConstraintSampledFieldSpatialSampledFieldFloatArrayDataMustMatch(*this));
addConstraint(new VConstraintSampledFieldSpatialSampledFieldUIntArrayDataNotNegative(*this));
addConstraint(new VConstraintSampledFieldSpatialSampledFieldIntArrayDataIntegers(*this));
addConstraint(new VConstraintSampledVolumeSpatialSampledVolumeDomainTypeMustBeDomainType(*this));
addConstraint(new VConstraintAnalyticVolumeSpatialAnalyticVolumeDomainTypeMustBeDomainType(*this));
addConstraint(new VConstraintParametricObjectSpatialParametricObjectDomainTypeMustBeDomainType(*this));
addConstraint(new VConstraintCSGObjectSpatialCSGObjectDomainTypeMustBeDomainType(*this));
addConstraint(new VConstraintBoundaryConditionSpatialBoundaryConditionBoundaryDomainTypeMustBeDomainType(*this));
addConstraint(new VConstraintDiffusionCoefficientSpatialDiffusionCoefficientVariableMustBeSpeciesOrParam(*this));
addConstraint(new VConstraintDiffusionCoefficientSpatialDiffusionCoefficientVariableMustNotBeSelf(*this));
addConstraint(new VConstraintBoundaryConditionSpatialBoundaryConditionCoordinateBoundaryMustBeBoundary(*this));
addConstraint(new VConstraintGeometrySpatialOneGeometryDefinitionMustBeActive(*this));
addConstraint(new VConstraintBoundaryConditionSpatialBoundaryConditionBoundaryDomainTypeOrCoordinateBoundary(*this));
addConstraint(new VConstraintMixedGeometrySpatialMixedGeometryChildrenNotActive(*this));
addConstraint(new VConstraintGeometrySpatialGeometryLOCoordinateComponentsOneToThreeChildren(*this));
addConstraint(new VConstraintGeometrySpatialGeometryCoordinateComponent1DisX(*this));
addConstraint(new VConstraintGeometrySpatialGeometryCoordinateComponent2DisXY(*this));
addConstraint(new VConstraintGeometrySpatialGeometryCoordinateComponent3DisXYZ(*this));

//Constraints defined in their own class ('global constraints')
addConstraint(new SpatialCompartmentMappingUnitSizesCheck(SpatialCompartmentMappingUnitSizesSum, *this));
addConstraint(new SpatialSpatialSymbolReferenceUniqueRefCheck(SpatialSpatialSymbolReferenceUniqueRef, *this));
addConstraint(new SpatialUniqueDiffusionCoefficientsCheck(SpatialNoDiffusionCoefficientOverlap, *this));
addConstraint(new SpatialUniqueAdvectionCoefficientsCheck(SpatialAdvectionCoefficientsMustBeUnique, *this));
addConstraint(new SpatialUniqueBoundaryConditionsCheck(SpatialBoundaryConditionsMustBeUnique, *this));
addConstraint(new SpatialUniqueSampledVolumeValueCheck(SpatialSampledVolumeValuesMustDiffer, *this));
addConstraint(new SpatialSampledVolumeValueNotInRangeCheck(SpatialSampledVolumeValuesNotInOtherRange, *this));
addConstraint(new SpatialSampledVolumeRangeOverlapCheck(SpatialSampledVolumeRangesCantOverlap, *this));
addConstraint(new SpatialUniqueCSGObjectOrdinalsCheck(SpatialCSGObjectOrdinalShouldBeUnique, *this));
addConstraint(new SpatialUniqueOrdinalMappingOrdinalsCheck(SpatialOrdinalMappingOrdinalShouldBeUnique, *this));
addConstraint(new SpatialOrdinalMappingGeometryDefinitionCheck(SpatialOrdinalMappingGeometryDefinitionMustBeGeometryDefinition, *this));
addConstraint(new SpatialUniqueAnalyticVolumeOrdinalsCheck(SpatialAnalyticVolumeOrdinalShouldBeUnique, *this));

/** @endcond */

