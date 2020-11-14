/**
 * @file local-downcast-packages-spatial.cpp
 * @brief Casting to most specific packages object for ruby
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


#ifdef USE_SPATIAL
else if (pkgName == "spatial")
{
  switch ( sb->getTypeCode() )
  {
    case SBML_LIST_OF:
      name = sb->getElementName();
      if (name == "listOfDomainTypes")
      {
        return SWIGTYPE_p_ListOfDomainTypes;
      }
      else if (name == "listOfDomains")
      {
        return SWIGTYPE_p_ListOfDomains;
      }
      else if (name == "listOfInteriorPoints")
      {
        return SWIGTYPE_p_ListOfInteriorPoints;
      }
      else if (name == "listOfAdjacentDomains")
      {
        return SWIGTYPE_p_ListOfAdjacentDomains;
      }
      else if (name == "listOfGeometryDefinitions")
      {
        return SWIGTYPE_p_ListOfGeometryDefinitions;
      }
      else if (name == "listOfCoordinateComponents")
      {
        return SWIGTYPE_p_ListOfCoordinateComponents;
      }
      else if (name == "listOfSampledFields")
      {
        return SWIGTYPE_p_ListOfSampledFields;
      }
      else if (name == "listOfSampledVolumes")
      {
        return SWIGTYPE_p_ListOfSampledVolumes;
      }
      else if (name == "listOfAnalyticVolumes")
      {
        return SWIGTYPE_p_ListOfAnalyticVolumes;
      }
      else if (name == "listOfParametricObjects")
      {
        return SWIGTYPE_p_ListOfParametricObjects;
      }
      else if (name == "listOfCSGObjects")
      {
        return SWIGTYPE_p_ListOfCSGObjects;
      }
      else if (name == "listOfCSGNodes")
      {
        return SWIGTYPE_p_ListOfCSGNodes;
      }
      else if (name == "listOfOrdinalMappings")
      {
        return SWIGTYPE_p_ListOfOrdinalMappings;
      }

      return SWIGTYPE_p_ListOf;

    case SBML_SPATIAL_DOMAINTYPE:
      return SWIGTYPE_p_DomainType;

    case SBML_SPATIAL_DOMAIN:
      return SWIGTYPE_p_Domain;

    case SBML_SPATIAL_INTERIORPOINT:
      return SWIGTYPE_p_InteriorPoint;

    case SBML_SPATIAL_BOUNDARY:
      return SWIGTYPE_p_Boundary;

    case SBML_SPATIAL_ADJACENTDOMAINS:
      return SWIGTYPE_p_AdjacentDomains;

    case SBML_SPATIAL_GEOMETRYDEFINITION:
      return SWIGTYPE_p_GeometryDefinition;

    case SBML_SPATIAL_COMPARTMENTMAPPING:
      return SWIGTYPE_p_CompartmentMapping;

    case SBML_SPATIAL_COORDINATECOMPONENT:
      return SWIGTYPE_p_CoordinateComponent;

    case SBML_SPATIAL_SAMPLEDFIELDGEOMETRY:
      return SWIGTYPE_p_SampledFieldGeometry;

    case SBML_SPATIAL_SAMPLEDFIELD:
      return SWIGTYPE_p_SampledField;

    case SBML_SPATIAL_SAMPLEDVOLUME:
      return SWIGTYPE_p_SampledVolume;

    case SBML_SPATIAL_ANALYTICGEOMETRY:
      return SWIGTYPE_p_AnalyticGeometry;

    case SBML_SPATIAL_ANALYTICVOLUME:
      return SWIGTYPE_p_AnalyticVolume;

    case SBML_SPATIAL_PARAMETRICGEOMETRY:
      return SWIGTYPE_p_ParametricGeometry;

    case SBML_SPATIAL_PARAMETRICOBJECT:
      return SWIGTYPE_p_ParametricObject;

    case SBML_SPATIAL_CSGEOMETRY:
      return SWIGTYPE_p_CSGeometry;

    case SBML_SPATIAL_CSGOBJECT:
      return SWIGTYPE_p_CSGObject;

    case SBML_SPATIAL_CSGNODE:
      return SWIGTYPE_p_CSGNode;

    case SBML_SPATIAL_CSGTRANSFORMATION:
      return SWIGTYPE_p_CSGTransformation;

    case SBML_SPATIAL_CSGTRANSLATION:
      return SWIGTYPE_p_CSGTranslation;

    case SBML_SPATIAL_CSGROTATION:
      return SWIGTYPE_p_CSGRotation;

    case SBML_SPATIAL_CSGSCALE:
      return SWIGTYPE_p_CSGScale;

    case SBML_SPATIAL_CSGHOMOGENEOUSTRANSFORMATION:
      return SWIGTYPE_p_CSGHomogeneousTransformation;

    case SBML_SPATIAL_TRANSFORMATIONCOMPONENT:
      return SWIGTYPE_p_TransformationComponent;

    case SBML_SPATIAL_CSGPRIMITIVE:
      return SWIGTYPE_p_CSGPrimitive;

    case SBML_SPATIAL_CSGSETOPERATOR:
      return SWIGTYPE_p_CSGSetOperator;

    case SBML_SPATIAL_SPATIALSYMBOLREFERENCE:
      return SWIGTYPE_p_SpatialSymbolReference;

    case SBML_SPATIAL_DIFFUSIONCOEFFICIENT:
      return SWIGTYPE_p_DiffusionCoefficient;

    case SBML_SPATIAL_ADVECTIONCOEFFICIENT:
      return SWIGTYPE_p_AdvectionCoefficient;

    case SBML_SPATIAL_BOUNDARYCONDITION:
      return SWIGTYPE_p_BoundaryCondition;

    case SBML_SPATIAL_GEOMETRY:
      return SWIGTYPE_p_Geometry;

    case SBML_SPATIAL_MIXEDGEOMETRY:
      return SWIGTYPE_p_MixedGeometry;

    case SBML_SPATIAL_ORDINALMAPPING:
      return SWIGTYPE_p_OrdinalMapping;

    case SBML_SPATIAL_SPATIALPOINTS:
      return SWIGTYPE_p_SpatialPoints;

    default:
      return SWIGTYPE_p_SBase;
    }
  }

  #endif // USE_SPATIAL

