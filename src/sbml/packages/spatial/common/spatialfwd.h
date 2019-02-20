/**
 * @file spatialfwd.h
 * @brief Definition of spatialfwd.
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


#ifndef spatialfwd_H__
#define spatialfwd_H__


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


typedef CLASS_OR_STRUCT DomainType                   DomainType_t;
typedef CLASS_OR_STRUCT Domain                       Domain_t;
typedef CLASS_OR_STRUCT InteriorPoint                InteriorPoint_t;
typedef CLASS_OR_STRUCT Boundary                     Boundary_t;
typedef CLASS_OR_STRUCT AdjacentDomains              AdjacentDomains_t;
typedef CLASS_OR_STRUCT GeometryDefinition           GeometryDefinition_t;
typedef CLASS_OR_STRUCT CompartmentMapping           CompartmentMapping_t;
typedef CLASS_OR_STRUCT CoordinateComponent          CoordinateComponent_t;
typedef CLASS_OR_STRUCT SampledFieldGeometry         SampledFieldGeometry_t;
typedef CLASS_OR_STRUCT SampledField                 SampledField_t;
typedef CLASS_OR_STRUCT SampledVolume                SampledVolume_t;
typedef CLASS_OR_STRUCT AnalyticGeometry             AnalyticGeometry_t;
typedef CLASS_OR_STRUCT AnalyticVolume               AnalyticVolume_t;
typedef CLASS_OR_STRUCT ParametricGeometry           ParametricGeometry_t;
typedef CLASS_OR_STRUCT ParametricObject             ParametricObject_t;
typedef CLASS_OR_STRUCT CSGeometry                   CSGeometry_t;
typedef CLASS_OR_STRUCT CSGObject                    CSGObject_t;
typedef CLASS_OR_STRUCT CSGNode                      CSGNode_t;
typedef CLASS_OR_STRUCT CSGTransformation            CSGTransformation_t;
typedef CLASS_OR_STRUCT CSGTranslation               CSGTranslation_t;
typedef CLASS_OR_STRUCT CSGRotation                  CSGRotation_t;
typedef CLASS_OR_STRUCT CSGScale                     CSGScale_t;
typedef CLASS_OR_STRUCT CSGHomogeneousTransformation CSGHomogeneousTransformation_t;
typedef CLASS_OR_STRUCT TransformationComponent      TransformationComponent_t;
typedef CLASS_OR_STRUCT CSGPrimitive                 CSGPrimitive_t;
typedef CLASS_OR_STRUCT CSGSetOperator               CSGSetOperator_t;
typedef CLASS_OR_STRUCT SpatialSymbolReference       SpatialSymbolReference_t;
typedef CLASS_OR_STRUCT DiffusionCoefficient         DiffusionCoefficient_t;
typedef CLASS_OR_STRUCT AdvectionCoefficient         AdvectionCoefficient_t;
typedef CLASS_OR_STRUCT BoundaryCondition            BoundaryCondition_t;
typedef CLASS_OR_STRUCT Geometry                     Geometry_t;
typedef CLASS_OR_STRUCT MixedGeometry                MixedGeometry_t;
typedef CLASS_OR_STRUCT OrdinalMapping               OrdinalMapping_t;
typedef CLASS_OR_STRUCT SpatialPoints                SpatialPoints_t;
typedef CLASS_OR_STRUCT SpatialModelPlugin           SpatialModelPlugin_t;
typedef CLASS_OR_STRUCT SpatialCompartmentPlugin     SpatialCompartmentPlugin_t;
typedef CLASS_OR_STRUCT SpatialSpeciesPlugin         SpatialSpeciesPlugin_t;
typedef CLASS_OR_STRUCT SpatialParameterPlugin       SpatialParameterPlugin_t;
typedef CLASS_OR_STRUCT SpatialReactionPlugin        SpatialReactionPlugin_t;


LIBSBML_CPP_NAMESPACE_END


#undef CLASS_OR_STRUCT


#endif /* !spatialfwd_H__ */


