/**
 * @file    spatialfwd.h
 * @brief   SBML Spatial C structure declarations
 * @author  
 *
 * $Id: spatialfwd.h 10667 2010-01-16 10:20:44Z ajouraku $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-5/src/packages/spatial/common/spatialfwd.h $
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2009 California Institute of Technology.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *------------------------------------------------------------------------- -->
 */


#ifndef spatialfwd_h__
#define spatialfwd_h__


/**
 * Forward declaration of all opaque C types.
 *
 * Declaring all types up-front avoids "redefinition of type 'Foo'" compile
 * errors and allows our combined C/C++ headers to depend minimally upon
 * each other.  Put another way, the type definitions below serve the same
 * purpose as "class Foo;" forward declarations in C++ code.
 */

#ifdef __cplusplus
#  define CLASS_OR_STRUCT class
#else
#  define CLASS_OR_STRUCT struct
#endif  /* __cplusplus */

LIBSBML_CPP_NAMESPACE_BEGIN

typedef CLASS_OR_STRUCT Geometry  Geometry_t;
typedef CLASS_OR_STRUCT DomainType  DomainType_t;
typedef CLASS_OR_STRUCT Domain  Domain_t;
typedef CLASS_OR_STRUCT InteriorPoint  InteriorPoint_t;
typedef CLASS_OR_STRUCT CoordinateComponent  CoordinateComponent_t;
typedef CLASS_OR_STRUCT Boundary  Boundary_t;
typedef CLASS_OR_STRUCT CompartmentMapping  CompartmentMapping_t;
typedef CLASS_OR_STRUCT AdjacentDomains  AdjacentDomains_t;
typedef CLASS_OR_STRUCT AnalyticVolume  AnalyticVolume_t;
typedef CLASS_OR_STRUCT AnalyticGeometry  AnalyticGeometry_t;
typedef CLASS_OR_STRUCT GeometryDefinition  GeometryDefinition_t;
typedef CLASS_OR_STRUCT SpatialPoint  SpatialPoint_t;
typedef CLASS_OR_STRUCT SampledVolume  SampledVolume_t;
typedef CLASS_OR_STRUCT SampledField  SampledField_t;
typedef CLASS_OR_STRUCT ImageData  ImageData_t;
typedef CLASS_OR_STRUCT SampledFieldGeometry  SampledFieldGeometry_t;
typedef CLASS_OR_STRUCT ParametricObject  ParametricObject_t;
typedef CLASS_OR_STRUCT PolygonObject  PolygonObject_t;
typedef CLASS_OR_STRUCT ParametricGeometry  ParametricGeometry_t;
typedef CLASS_OR_STRUCT CSGObject  CSGObject_t;
typedef CLASS_OR_STRUCT CSGNode  CSGNode_t;
typedef CLASS_OR_STRUCT CSGSetOperator  CSGSetOperator_t;
typedef CLASS_OR_STRUCT CSGTransformation  CSGTransformation_t;
typedef CLASS_OR_STRUCT CSGRotation  CSGRotation_t;
typedef CLASS_OR_STRUCT CSGScale  CSGScale_t;
typedef CLASS_OR_STRUCT CSGTranslation  CSGTranslation_t;
typedef CLASS_OR_STRUCT CSGHomogeneousTransformation  CSGHomogeneousTransformation_t;
typedef CLASS_OR_STRUCT TransformationComponents  TransformationComponents_t;
typedef CLASS_OR_STRUCT CSGPrimitive  CSGPrimitive_t;
typedef CLASS_OR_STRUCT CSGPseudoPrimitive  CSGPseudoPrimitive_t;
typedef CLASS_OR_STRUCT CSGeometry  CSGeometry_t;
typedef CLASS_OR_STRUCT SpatialSymbolReference  SpatialSymbolReference_t;
typedef CLASS_OR_STRUCT DiffusionCoefficient  DiffusionCoefficient_t;
typedef CLASS_OR_STRUCT AdvectionCoefficient  AdvectionCoefficient_t;
typedef CLASS_OR_STRUCT BoundaryCondition  BoundaryCondition_t;

LIBSBML_CPP_NAMESPACE_END

#undef CLASS_OR_STRUCT


#endif  /* spatialfwd_h__ */
