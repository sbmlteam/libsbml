
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
#include "sbml/packages/spatial/validator/SpatialUniqueBoundaryConditionsCheck.h"

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
addConstraint(new VConstraintSampledVolumeSpatialSampledVolumeNeedsMaxWithMin(*this));
addConstraint(new VConstraintSampledVolumeSpatialSampledVolumeNeedsMinWithMax(*this));
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
addConstraint(new VConstraintAdvectionCoefficientSpatialAdvectionCoefficientVariableMustBeSpecies(*this));
addConstraint(new VConstraintBoundaryConditionSpatialBoundaryConditionVariableMustBeSpecies(*this));
addConstraint(new VConstraintCoordinateComponentSpatialBoundaryMinLessThanMax(*this));
addConstraint(new VConstraintParameterSpatialBoundaryMustBeConstant(*this));
addConstraint(new VConstraintParameterSpatialDomainTypeNoAssignment(*this));

//Constraints defined in their own class ('global constraints')
addConstraint(new SpatialCompartmentMappingUnitSizesCheck(1221351, *this));
addConstraint(new SpatialSpatialSymbolReferenceUniqueRefCheck(SpatialSpatialSymbolReferenceUniqueRef, *this));
addConstraint(new SpatialUniqueDiffusionCoefficientsCheck(SpatialNoDiffusionCoefficientOverlap, *this));
addConstraint(new SpatialUniqueAdvectionCoefficientsCheck(SpatialAdvectionCoefficientsMustBeUnique, *this));
addConstraint(new SpatialUniqueBoundaryConditionsCheck(SpatialBoundaryConditionsMustBeUnique, *this));

/** @endcond */

