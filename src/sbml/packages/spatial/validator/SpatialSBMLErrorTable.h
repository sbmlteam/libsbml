/**
 * @file SpatialSBMLErrorTable.h
 * @brief Definition of the SpatialSBMLErrorTable class.
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


#ifndef SpatialSBMLErrorTable_H__
#define SpatialSBMLErrorTable_H__


#include <sbml/packages/spatial/validator/SpatialSBMLError.h>


LIBSBML_CPP_NAMESPACE_BEGIN



/** @cond doxygenLibsbmlInternal */

static const packageErrorTableEntry spatialErrorTable[] =
{
  // 1210100
  { SpatialUnknown,
    "Unknown error from Spatial",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Unknown error from Spatial",
    { ""
    }
  },

  // 1210101
  { SpatialNSUndeclared,
    "The Spatial namespace is not correctly declared.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "To conform to the Spatial Processes Package specification for SBML Level 3 "
    "Version 1, an SBML document must declare "
    "'http://www.sbml.org/sbml/level3/version1/spatial/version1' as the "
    "XMLNamespace to use for elements of this package.",
    { "L3V1 Spatial V1 Section 3.1"
    }
  },

  // 1210102
  { SpatialElementNotInNs,
    "Element not in Spatial namespace",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Wherever they appear in an SBML document, elements and attributes from the "
    "Spatial Processes Package must use the "
    "'http://www.sbml.org/sbml/level3/version1/spatial/version1' namespace, "
    "declaring so either explicitly or implicitly.",
    { "L3V1 Spatial V1 Section 3.1"
    }
  },

  // 1210301
  { SpatialDuplicateComponentId,
    "Duplicate 'id' attribute value",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "(Extends validation rule #10301 in the SBML Level 3 Core specification. TO "
    "DO list scope of ids)",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1210302
  { SpatialIdSyntaxRule,
    "Invalid SId syntax",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of a 'spatial:id' must conform to the syntax of the <sbml> data "
    "type 'SId'",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1220101
  { SpatialAttributeRequiredMissing,
    "Required spatial:required attribute on <sbml>",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "In all SBML documents using the Spatial Processes Package, the <sbml> "
    "object must have the 'spatial:required' attribute.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1220102
  { SpatialAttributeRequiredMustBeBoolean,
    "The spatial:required attribute must be Boolean",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of attribute 'spatial:required' on the <sbml> object must be of "
    "data type 'boolean'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1220103
  { SpatialAttributeRequiredMustHaveValue,
    "The spatial:required attribute must be 'true'",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of attribute 'spatial:required' on the <sbml> object must be set "
    "to 'true'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1220201
  { SpatialModelAllowedElements,
    "Elements allowed on <model>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <model> object may contain one and only one instance of the <geometry> "
    "element. No other elements from the SBML Level 3 Spatial Processes "
    "namespaces are permitted on a <model> object. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1220301
  { SpatialCompartmentAllowedElements,
    "Elements allowed on <compartment>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <compartment> object may contain one and only one instance of the "
    "<compartmentMapping> element. No other elements from the SBML Level 3 "
    "Spatial Processes namespaces are permitted on a <compartment> object. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1220401
  { SpatialSpeciesAllowedAttributes,
    "Attributes allowed on <species>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <species> object may have the optional attribute 'spatial:isSpatial'. No "
    "other attributes from the SBML Level 3 Spatial Processes namespaces are "
    "permitted on a <species> object. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1220402
  { SpatialSpeciesIsSpatialMustBeBoolean,
    "IsSpatial attribute must be Boolean.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:isSpatial' on a <species> must have a value of data "
    "type 'boolean'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1220450
  { SpatialCompartmentsMustHaveCompartmentMapping,
    "Spatial species compartments must have compartment mapping.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The <compartment> of any <species> that has a value of 'true' for the 'spatial:isSpatial' attribute must have a child <compartmentMapping>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1220501
  { SpatialParameterAllowedElements,
    "Elements allowed on <parameter>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <parameter> object may contain up to one child element from the SBML "
    "Level 3 Spatial Processes namespace:  a <spatialSymbolReference>, an "
    "<advectionCoefficient>, a <boundaryCondition>, or a <diffusionCoefficient>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1220601
  { SpatialReactionAllowedAttributes,
    "Attributes allowed on <reaction>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <reaction> object must have the required attribute 'spatial:isLocal'. No "
    "other attributes from the SBML Level 3 Spatial Processes namespaces are "
    "permitted on a <reaction> object. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1220602
  { SpatialReactionIsLocalMustBeBoolean,
    "IsLocal attribute must be Boolean.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:isLocal' on a <reaction> must have a value of data "
    "type 'boolean'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1220650
  { SpatialLocalReactionMustDefineCompartment,
    "Local spatial reactions must define compartment.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "If the attribute 'spatial:isLocal' of a <reaction> has the value 'true', the <reaction> must also define a value for the 'compartment' attribute.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1220651
  { SpatialLocalReactionUnits,
    "Local spatial reactions should have units of concentration/time.",
    LIBSBML_CAT_UNITS_CONSISTENCY,
    LIBSBML_SEV_WARNING,
    "The units of a <kineticLaw> of a <reaction> with a 'spatial:isLocal' attribute value of 'true' should be concentration/time instead of substance/time.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1220701
  { SpatialDomainTypeAllowedCoreAttributes,
    "Core attributes allowed on <domainType>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <domainType> object may have the optional SBML Level 3 Core attributes "
    "'metaid' and 'sboTerm'. No other attributes from the SBML Level 3 Core "
    "namespaces are permitted on a <domainType>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1220702
  { SpatialDomainTypeAllowedCoreElements,
    "Core elements allowed on <domainType>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <domainType> object may have the optional SBML Level 3 Core subobjects "
    "for notes and annotations. No other elements from the SBML Level 3 Core "
    "namespaces are permitted on a <domainType>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1220703
  { SpatialDomainTypeAllowedAttributes,
    "Attributes allowed on <domainType>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <domainType> object must have the required attributes 'spatial:id' and "
    "'spatial:spatialDimensions', and may have the optional attribute "
    "'spatial:name'. No other attributes from the SBML Level 3 Spatial "
    "Processes namespaces are permitted on a <domainType> object. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1220704
  { SpatialDomainTypeSpatialDimensionsMustBeInteger,
    "The 'spatialDimensions' attribute must be Integer.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:spatialDimensions' on a <domainType> must have a "
    "value of data type 'integer'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1220705
  { SpatialDomainTypeNameMustBeString,
    "The 'name' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:name' on a <domainType> must have a value of data "
    "type 'string'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1220750
  { SpatialDomainTypeDimensionsMustMatch3DGeometry,
    "Dimensions of domainType must match 3D geometry.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "If the <geometry> of the <model> has exactly three <coordinateComponent> children, the attribute 'spatial:spatialDimensions' of a <domainType> may only have a value of '2' or '3'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1220751
  { SpatialDomainTypeDimensionsMustMatch2DGeometry,
    "Dimensions of domainType must match 2D geometry.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "If the <geometry> of the <model> has exactly two <coordinateComponent> children, the attribute 'spatial:spatialDimensions' of a <domainType> may only have a value of '1' or '2'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1220752
  { SpatialDomainTypeDimensionsMustMatch1DGeometry,
    "Dimensions of domainType must match 1D geometry.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "If the <geometry> of the <model> has exactly one <coordinateComponent> childr, the attribute 'spatial:spatialDimensions' of a <domainType> may only have a value of '0' or '1'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1220753
  { SpatialDomainTypeNoAssignment,
    "DomainTypes can't be assigned values.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "If a <parameter> has a child <spatialSymbolReference> that points to a <domainType>, that <parameter> may not be the target of an <initialAssignment>, <eventAssignment>, <rateRule>, or <assignmentRule>, may not be determined by an <algebraicRule>, and may not have the 'value' attribute.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1220801
  { SpatialDomainAllowedCoreAttributes,
    "Core attributes allowed on <domain>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <domain> object may have the optional SBML Level 3 Core attributes "
    "'metaid' and 'sboTerm'. No other attributes from the SBML Level 3 Core "
    "namespaces are permitted on a <domain>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1220802
  { SpatialDomainAllowedCoreElements,
    "Core elements allowed on <domain>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <domain> object may have the optional SBML Level 3 Core subobjects for "
    "notes and annotations. No other elements from the SBML Level 3 Core "
    "namespaces are permitted on a <domain>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1220803
  { SpatialDomainAllowedAttributes,
    "Attributes allowed on <domain>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <domain> object must have the required attributes 'spatial:id' and "
    "'spatial:domainType', and may have the optional attribute 'spatial:name'. "
    "No other attributes from the SBML Level 3 Spatial Processes namespaces are "
    "permitted on a <domain> object. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1220804
  { SpatialDomainAllowedElements,
    "Elements allowed on <domain>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <domain> object may contain one and only one instance of the "
    "<listOfInteriorPoints> element. No other elements from the SBML Level 3 "
    "Spatial Processes namespaces are permitted on a <domain> object. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1220805
  { SpatialDomainDomainTypeMustBeDomainType,
    "The attribute 'domainType' must point to DomainType object.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'spatial:domainType' of a <domain> object must "
    "be the identifier of an existing <domainType> object defined in the "
    "enclosing <model> object.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1220806
  { SpatialDomainNameMustBeString,
    "The 'name' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:name' on a <domain> must have a value of data type "
    "'string'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1220807
  { SpatialDomainLOInteriorPointsAllowedCoreElements,
    "Core elements allowed on <listOfInteriorPoints>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Apart from the general notes and annotations subobjects permitted on all "
    "SBML objects, a <listOfInteriorPoints> container object may only contain "
    "<interiorPoint> objects.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1220808
  { SpatialDomainLOInteriorPointsAllowedCoreAttributes,
    "Core attributes allowed on <listOfInteriorPoints>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <listOfInteriorPoints> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <listOfInteriorPoints> object.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1220753
  { SpatialDomainNoAssignment,
    "Domains can't be assigned values.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "If a <parameter> has a child <spatialSymbolReference> that points to a <domain>, that <parameter> may not be the target of an <initialAssignment>, <eventAssignment>, <rateRule>, or <assignmentRule>, may not be determined by an <algebraicRule>, and may not have the 'value' attribute.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1220901
  { SpatialInteriorPointAllowedCoreAttributes,
    "Core attributes allowed on <interiorPoint>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An <interiorPoint> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on an <interiorPoint>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1220902
  { SpatialInteriorPointAllowedCoreElements,
    "Core elements allowed on <interiorPoint>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An <interiorPoint> object may have the optional SBML Level 3 Core "
    "subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on an <interiorPoint>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1220903
  { SpatialInteriorPointAllowedAttributes,
    "Attributes allowed on <interiorPoint>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An <interiorPoint> object must have the required attribute "
    "'spatial:coord1', and may have the optional attributes "
    "'spatial:coord2' and 'spatial:coord3'. No other attributes from the "
    "SBML Level 3 Spatial Processes namespaces are permitted on an "
    "<interiorPoint> object. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1220904
  { SpatialInteriorPointCoord1MustBeDouble,
    "The 'coord1' attribute must be Double.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:coord1' on an <interiorPoint> must have a value "
    "of data type 'double'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1220905
  { SpatialInteriorPointCoord2MustBeDouble,
    "The 'coord2' attribute must be Double.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:coord2' on an <interiorPoint> must have a value "
    "of data type 'double'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1220906
  { SpatialInteriorPointCoord3MustBeDouble,
    "The 'coord3' attribute must be Double.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:coord3' on an <interiorPoint> must have a value "
    "of data type 'double'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1220950
  { SpatialInteriorPointOneCoordIn1DGeometry,
    "Only one 'coord' attribute in 1D geometry.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "If the <geometry> of the <model> has exactly one <coordinateComponent> child, an <interiorPoint> object must define the attribute 'coord1', and must not define the attributes 'coord2' and 'coord3'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1220951
  { SpatialInteriorPointTwoCoordsIn2DGeometry,
    "Exactly two 'coord' attributes in 2D geometry.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "If the <geometry> of the <model> has exactly two <coordinateComponent> children, an <interiorPoint> object must define the attributes 'coord1' and 'coord2', and must not define the attribute 'coord3'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1220952
  { SpatialInteriorPointThreeCoordsIn3DGeometry,
    "Exactly three 'coord' attributes in 3D geometry.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "If the <geometry> of the <model> has exactly three <coordinateComponent> children, an <interiorPoint> object must define the attributes 'coord1', 'coord2', and 'coord3'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221001
  { SpatialBoundaryAllowedCoreAttributes,
    "Core attributes allowed on <boundary>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <boundary> object may have the optional SBML Level 3 Core attributes "
    "'metaid' and 'sboTerm'. No other attributes from the SBML Level 3 Core "
    "namespaces are permitted on a <boundary>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221002
  { SpatialBoundaryAllowedCoreElements,
    "Core elements allowed on <boundary>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <boundary> object may have the optional SBML Level 3 Core subobjects for "
    "notes and annotations. No other elements from the SBML Level 3 Core "
    "namespaces are permitted on a <boundary>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221003
  { SpatialBoundaryAllowedAttributes,
    "Attributes allowed on <boundary>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <boundary> object must have the required attributes 'spatial:id' and "
    "'spatial:value', and may have the optional attribute 'spatial:name'. No "
    "other attributes from the SBML Level 3 Spatial Processes namespaces are "
    "permitted on a <boundary> object. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221004
  { SpatialBoundaryValueMustBeDouble,
    "The 'value' attribute must be Double.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:value' on a <boundary> must have a value of data "
    "type 'double'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221005
  { SpatialBoundaryNameMustBeString,
    "The 'name' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:name' on a <boundary> must have a value of data "
    "type 'string'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221050
  { SpatialBoundaryMinLessThanMax,
    "The 'boundaryMin' must be less than or equal to 'boundaryMax'.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "If set, the 'value' attribute of a <boundaryMin> must be less than or equal to the 'value' attribute of the <boundaryMax> of the same CoordinateComponent.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221051
  { SpatialBoundaryMustBeConstant,
    "Any Boundary must be constant.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "If a <parameter> has a child <spatialSymbolReference> that points to a Boundary, the 'constant' attribute of that <parameter> must have a value of 'true'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221052
  { SpatialBoundaryUnitsShouldMatchCoordinateComponent,
    "Boundary units should match their CoordinateComponent.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "If a <parameter> has a child <spatialSymbolReference> that points to a Boundary, the units of that <parameter> should be equal to the units of the Boundary's parent <coordinateComponent>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221101
  { SpatialAdjacentDomainsAllowedCoreAttributes,
    "Core attributes allowed on <adjacentDomains>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An <adjacentDomains> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on an <adjacentDomains>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221102
  { SpatialAdjacentDomainsAllowedCoreElements,
    "Core elements allowed on <adjacentDomains>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An <adjacentDomains> object may have the optional SBML Level 3 Core "
    "subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on an <adjacentDomains>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221103
  { SpatialAdjacentDomainsAllowedAttributes,
    "Attributes allowed on <adjacentDomains>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An <adjacentDomains> object must have the required attributes "
    "'spatial:id', 'spatial:domain1' and 'spatial:domain2', and may have "
    "the optional attribute 'spatial:name'. No other attributes from the SBML "
    "Level 3 Spatial Processes namespaces are permitted on an <adjacentDomains> "
    "object. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221104
  { SpatialAdjacentDomainsDomain1MustBeDomain,
    "The attribute 'domain1' must point to Domain object.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'spatial:domain1' of an <adjacentDomains> "
    "object must be the identifier of an existing <domain> object defined in "
    "the enclosing <model> object.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221105
  { SpatialAdjacentDomainsDomain2MustBeDomain,
    "The attribute 'domain2' must point to Domain object.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'spatial:domain2' of an <adjacentDomains> "
    "object must be the identifier of an existing <domain> object defined in "
    "the enclosing <model> object.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221106
  { SpatialAdjacentDomainsNameMustBeString,
    "The 'name' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:name' on an <adjacentDomains> must have a value of "
    "data type 'string'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221150
  { SpatialAdjacentDomainsMustBeAdjacent,
    "The domains in AdjacentDomains must be adjacent.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attributes 'domain1' and 'domain2' of an <adjacentDomains> must reference two different domains that are spatially adjacent.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221201
  { SpatialGeometryDefinitionAllowedCoreAttributes,
    "Core attributes allowed on <geometryDefinition>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <geometryDefinition> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <geometryDefinition>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221202
  { SpatialGeometryDefinitionAllowedCoreElements,
    "Core elements allowed on <geometryDefinition>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <geometryDefinition> object may have the optional SBML Level 3 Core "
    "subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a <geometryDefinition>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221203
  { SpatialGeometryDefinitionAllowedAttributes,
    "Attributes allowed on <geometryDefinition>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <geometryDefinition> object must have the required attributes "
    "'spatial:id' and 'spatial:isActive', and may have the optional attribute "
    "'spatial:name'. No other attributes from the SBML Level 3 Spatial "
    "Processes namespaces are permitted on a <geometryDefinition> object. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221204
  { SpatialGeometryDefinitionIsActiveMustBeBoolean,
    "The 'isActive' attribute must be Boolean.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:isActive' on a <geometryDefinition> must have a "
    "value of data type 'boolean'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221205
  { SpatialGeometryDefinitionNameMustBeString,
    "The 'name' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:name' on a <geometryDefinition> must have a value "
    "of data type 'string'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221250
  { SpatialOneGeometryDefinitionMustBeActive,
    "At least one <geometryDefinition> must be active.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "At least one <geometryDefinition> child of a <geometry> must have a 'spatial:isActive' attribute value of 'true'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221301
  { SpatialCompartmentMappingAllowedCoreAttributes,
    "Core attributes allowed on <compartmentMapping>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <compartmentMapping> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <compartmentMapping>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221302
  { SpatialCompartmentMappingAllowedCoreElements,
    "Core elements allowed on <compartmentMapping>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <compartmentMapping> object may have the optional SBML Level 3 Core "
    "subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a <compartmentMapping>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221303
  { SpatialCompartmentMappingAllowedAttributes,
    "Attributes allowed on <compartmentMapping>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <compartmentMapping> object must have the required attributes "
    "'spatial:id', 'spatial:domainType' and 'spatial:unitSize', and may have "
    "the optional attribute 'spatial:name'. No other attributes from the SBML "
    "Level 3 Spatial Processes namespaces are permitted on a "
    "<compartmentMapping> object. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221304
  { SpatialCompartmentMappingDomainTypeMustBeDomainType,
    "The attribute 'domainType' must point to DomainType object.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'spatial:domainType' of a <compartmentMapping> "
    "object must be the identifier of an existing <domainType> object defined "
    "in the enclosing <model> object.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221305
  { SpatialCompartmentMappingUnitSizeMustBeDouble,
    "The 'unitSize' attribute must be Double.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:unitSize' on a <compartmentMapping> must have a "
    "value of data type 'double'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221306
  { SpatialCompartmentMappingNameMustBeString,
    "The 'name' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:name' on a <compartmentMapping> must have a value "
    "of data type 'string'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221350
  { SpatialCompartmentMappingUnitSizeMustBeFraction,
    "The 'unitSize' attribute must be between 0 and 1.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_WARNING,
    "The attribute 'spatial:unitSize' on a <compartmentMapping> should have a value "
    "between 0 and 1, inclusive, when the dimensions of the referenced compartments are the same.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221351
  { SpatialCompartmentMappingUnitSizesSum,
    "The 'unitSize' attributes should sum to 1.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_WARNING,
    "The values of the 'spatial:unitSize' attributes of every <compartmentMapping> with the same 'spatial:domainType' should sum to 1, when the dimensions of the referenced compartments are the same.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221352
  { SpatialCompartmentMappingUnits,
    "The units of a compartmentMapping should be compartment units over domain units.",
    LIBSBML_CAT_UNITS_CONSISTENCY,
    LIBSBML_SEV_WARNING,
    "If a <parameter> has a child <spatialSymbolReference> that points to a <compartmentMapping>, the units of that <parameter> should be equivalent to the units of the parent <compartment> of the <compartmentMapping>, divided by the units of the <domainType> referenced by the <compartmentMapping>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221401
  { SpatialCoordinateComponentAllowedCoreAttributes,
    "Core attributes allowed on <coordinateComponent>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <coordinateComponent> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <coordinateComponent>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221402
  { SpatialCoordinateComponentAllowedCoreElements,
    "Core elements allowed on <coordinateComponent>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <coordinateComponent> object may have the optional SBML Level 3 Core "
    "subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a <coordinateComponent>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221403
  { SpatialCoordinateComponentAllowedAttributes,
    "Attributes allowed on <coordinateComponent>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <coordinateComponent> object must have the required attributes "
    "'spatial:id' and 'spatial:type', and may have the optional attributes "
    "'spatial:name' and 'spatial:unit'. No other attributes from the SBML Level "
    "3 Spatial Processes namespaces are permitted on a <coordinateComponent> "
    "object. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221404
  { SpatialCoordinateComponentAllowedElements,
    "Elements allowed on <coordinateComponent>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <coordinateComponent> object must contain one and only one instance of "
    "each of the Boundary and <boundary> elements. No other elements from the "
    "SBML Level 3 Spatial Processes namespaces are permitted on a "
    "<coordinateComponent> object. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221405
  { SpatialCoordinateComponentTypeMustBeCoordinateKindEnum,
    "The 'type' attribute must be CoordinateKindEnum.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'spatial:type' of a <coordinateComponent> "
    "object must conform to the syntax of SBML data type 'CoordinateKind' and "
    "may only take on the allowed values of 'CoordinateKind' defined in SBML; "
    "that is, the value must be one of the following: 'cartesianX', "
    "'cartesianY' or 'cartesianZ'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221406
  { SpatialCoordinateComponentNameMustBeString,
    "The 'name' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:name' on a <coordinateComponent> must have a value "
    "of data type 'string'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221407
  { SpatialCoordinateComponentUnitMustBeUnitSId,
    "The 'unit' attribute must be UnitSId.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'spatial:unit' on a <coordinateComponent> must "
    "have a taken from the following: the identifier of a <unitDefinition> "
    "object in the enclosing <model>, or one of the base units in SBML.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221501
  { SpatialSampledFieldGeometryAllowedCoreAttributes,
    "Core attributes allowed on <sampledFieldGeometry>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <sampledFieldGeometry> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <sampledFieldGeometry>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221502
  { SpatialSampledFieldGeometryAllowedCoreElements,
    "Core elements allowed on <sampledFieldGeometry>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <sampledFieldGeometry> object may have the optional SBML Level 3 Core "
    "subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a <sampledFieldGeometry>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221503
  { SpatialSampledFieldGeometryAllowedAttributes,
    "Attributes allowed on <sampledFieldGeometry>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <sampledFieldGeometry> object must have the required attribute "
    "'spatial:sampledField'. No other attributes from the SBML Level 3 Spatial "
    "Processes namespaces are permitted on a <sampledFieldGeometry> object. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221504
  { SpatialSampledFieldGeometryAllowedElements,
    "Elements allowed on <sampledFieldGeometry>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <sampledFieldGeometry> object may contain one and only one instance of "
    "the <listOfSampledVolumes> element. No other elements from the SBML Level "
    "3 Spatial Processes namespaces are permitted on a <sampledFieldGeometry> "
    "object. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221505
  { SpatialSampledFieldGeometrySampledFieldMustBeSampledField,
    "The attribute 'sampledField' must point to <sampledField> object.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'spatial:sampledField' of a "
    "<sampledFieldGeometry> object must be the identifier of an existing "
    "<sampledField> object defined in the enclosing <model> object.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221506
  { SpatialSampledFieldGeometryLOSampledVolumesAllowedCoreElements,
    "Core elements allowed on <listOfSampledVolumes>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Apart from the general notes and annotations subobjects permitted on all "
    "SBML objects, a <listOfSampledVolumes> container object may only contain "
    "<sampledVolume> objects.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221507
  { SpatialSampledFieldGeometryLOSampledVolumesAllowedCoreAttributes,
    "Core attributes allowed on <listOfSampledVolumes>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <listOfSampledVolumes> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <listOfSampledVolumes> object.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221601
  { SpatialSampledFieldAllowedCoreAttributes,
    "Core attributes allowed on <sampledField>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <sampledField> object may have the optional SBML Level 3 Core attributes "
    "'metaid' and 'sboTerm'. No other attributes from the SBML Level 3 Core "
    "namespaces are permitted on a <sampledField>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221602
  { SpatialSampledFieldAllowedCoreElements,
    "Core elements allowed on <sampledField>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <sampledField> object may have the optional SBML Level 3 Core subobjects "
    "for notes and annotations. No other elements from the SBML Level 3 Core "
    "namespaces are permitted on a <sampledField>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221603
  { SpatialSampledFieldAllowedAttributes,
    "Attributes allowed on <sampledField>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <sampledField> object must have the required attributes 'spatial:id', "
    "'spatial:dataType', 'spatial:numSamples1', 'spatial:interpolationType', "
    "'spatial:compression', 'spatial:samples' and 'spatial:samplesLength', and "
    "may have the optional attributes 'spatial:name', 'spatial:numSamples2' "
    "and 'spatial:numSamples3'. No other attributes from the SBML Level 3 "
    "Spatial Processes namespaces are permitted on a <sampledField> object. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221604
  { SpatialSampledFieldDataTypeMustBeDataKindEnum,
    "The 'dataType' attribute must be DataKindEnum.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'spatial:dataType' of a <sampledField> object "
    "must conform to the syntax of SBML data type 'DataKind' and may only take "
    "on the allowed values of 'DataKind' defined in SBML; that is, the value "
    "must be one of the following: 'uint', 'int', 'double', 'float', 'uint8', 'uint16' or "
    "'uint32'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221605
  { SpatialSampledFieldNumSamples1MustBeInteger,
    "The 'numSamples1' attribute must be Integer.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:numSamples1' on a <sampledField> must have a "
    "value of data type 'integer'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221606
  { SpatialSampledFieldInterpolationTypeMustBeInterpolationKindEnum,
    "The 'interpolationType' attribute must be InterpolationKindEnum.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'spatial:interpolationType' of a <sampledField> "
    "object must conform to the syntax of SBML data type 'InterpolationKind' "
    "and may only take on the allowed values of 'InterpolationKind' defined in "
    "SBML; that is, the value must be one of the following: 'nearestNeighbor' "
    "or 'linear'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221607
  { SpatialSampledFieldCompressionMustBeCompressionKindEnum,
    "The 'compression' attribute must be CompressionKindEnum.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'spatial:compression' of a <sampledField> "
    "object must conform to the syntax of SBML data type 'CompressionKind' and "
    "may only take on the allowed values of 'CompressionKind' defined in SBML; "
    "that is, the value must be one of the following: 'uncompressed' or "
    "'deflated'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221608
  { SpatialSampledFieldSamplesMustBeNumeric,
    "The entries of a <sampledField> must be numeric.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the children of a <sampledField> object must be an array of numeric values.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221609
  { SpatialSampledFieldSamplesLengthMustBeInteger,
    "The 'samplesLength' attribute must be Integer.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:samplesLength' on a <sampledField> must have a "
    "value of data type 'integer'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221610
  { SpatialSampledFieldNameMustBeString,
    "The 'name' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:name' on a <sampledField> must have a value of data "
    "type 'string'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221611
  { SpatialSampledFieldNumSamples2MustBeInteger,
    "The 'numSamples2' attribute must be Integer.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:numSamples2' on a <sampledField> must have a "
    "value of data type 'integer'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221612
  { SpatialSampledFieldNumSamples3MustBeInteger,
    "The 'numSamples3' attribute must be Integer.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:numSamples3' on a <sampledField> must have a "
    "value of data type 'integer'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221650
  { SpatialSampledFieldOneSampleIn1DGeometry,
    "Only one 'numSamples' attribute in 1D geometry.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "If the <geometry> of the <model> has exactly one <coordinateComponent> child, a <sampledField> object must define the attribute 'spatial:numSamples1', and must not define the attributes 'spatial:numSamples2' nor 'spatial:numSamples3'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221651
  { SpatialSampledFieldTwoSamplesIn2DGeometry,
    "Two 'numSamples' attributes in 2D geometry.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "If the <geometry> of the <model> has exactly two <coordinateComponent> children, a <sampledField> object must define the attributes 'spatial:numSamples1' and 'spatial:numSamples2', and must not define the attribute 'spatial:numSamples3'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221652
  { SpatialSampledFieldThreeSamplesIn3DGeometry,
    "Three 'numSamples' attributes in 3D geometry.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "If the <geometry> of the <model> has exactly three <coordinateComponent> children, a <sampledField> object must define the attributes 'spatial:numSamples1', 'spatial:numSamples2', and 'spatial:numSamples3'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221653
  { SpatialSampledFieldSamplesLengthMustMatchUncompressed,
    "The 'samplesLength' attribute must match the entries when uncompressed.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "If the 'spatial:compression' attribute of a <sampledField> has the value 'uncompressed', the 'spatial:samplesLength' attribute of the same <sampledField> must equal the number of entries in the ArrayData child of the <sampledField>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221654
  { SpatialSampledFieldSamplesLengthMustMatchCompressed,
    "The 'samplesLength' attribute must match the length of the ArrayData when compressed.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "If the 'spatial:compression' attribute of a <sampledField> has the value 'deflated', the 'spatial:samplesLength' attribute of the same <sampledField> must equal the number of entries of the ArrayData child of the <sampledField>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221655
  { SpatialSampledFieldFloatArrayDataMustMatch,
    "The <sampledField> entries in 'float' must be single precision.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "If the 'spatial:dataType' attribute of a <sampledField> has the value 'float', none of the uncompressed entries in the ArrayData child of the <sampledField> may have a value outside of the range of an IEEE 754-1985 single-precision floating point value (approximately +/-3.4028235e38, and +/-1.17549e-38).",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221656
  { SpatialSampledFieldUIntArrayDataNotNegative,
    "The <sampledField> entries in 'uint' must not be negative.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "If the 'spatial:dataType' attribute of a <sampledField> has the value 'uint', none of the uncompressed entries in the ArrayData child of the <sampledField> may be negative.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221657
  { SpatialSampledFieldIntArrayDataIntegers,
    "The <sampledField> entries in 'uint' or 'int' must be integers.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "If the 'spatial:dataType' attribute of a <sampledField> has the value 'uint' or 'int', all of the uncompressed entries in the ArrayData child of the <sampledField> must be integers.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221658
  { SpatialSampledFieldCompressedSamplesMustBeInts,
    "The compressed entries of a <sampledField> must be integers.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the children of a <sampledField> object "
    "must be an array of values of type 'integer' if the 'spatial:compression' attribute has the value 'deflated'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221701
  { SpatialSampledVolumeAllowedCoreAttributes,
    "Core attributes allowed on <sampledVolume>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <sampledVolume> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <sampledVolume>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221702
  { SpatialSampledVolumeAllowedCoreElements,
    "Core elements allowed on <sampledVolume>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <sampledVolume> object may have the optional SBML Level 3 Core "
    "subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a <sampledVolume>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221703
  { SpatialSampledVolumeAllowedAttributes,
    "Attributes allowed on <sampledVolume>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <sampledVolume> object must have the required attributes 'spatial:id' "
    "and 'spatial:domainType', and may have the optional attributes "
    "'spatial:name', 'spatial:sampledValue', 'spatial:minValue' and "
    "'spatial:maxValue'. No other attributes from the SBML Level 3 Spatial "
    "Processes namespaces are permitted on a <sampledVolume> object. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221704
  { SpatialSampledVolumeDomainTypeMustBeDomainType,
    "The attribute 'domainType' must point to DomainType object.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'spatial:domainType' of a <sampledVolume> "
    "object must be the identifier of an existing <domainType> object defined "
    "in the enclosing <model> object.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221705
  { SpatialSampledVolumeNameMustBeString,
    "The 'name' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:name' on a <sampledVolume> must have a value of "
    "data type 'string'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221706
  { SpatialSampledVolumeSampledValueMustBeDouble,
    "The 'sampledValue' attribute must be Double.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:sampledValue' on a <sampledVolume> must have a "
    "value of data type 'double'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221707
  { SpatialSampledVolumeMinValueMustBeDouble,
    "The 'minValue' attribute must be Double.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:minValue' on a <sampledVolume> must have a value of "
    "data type 'double'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221708
  { SpatialSampledVolumeMaxValueMustBeDouble,
    "The 'maxValue' attribute must be Double.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:maxValue' on a <sampledVolume> must have a value of "
    "data type 'double'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221750
  { SpatialSampledVolumeSampledValueMinMax,
    "Every <sampleVolume> must have a 'sampledValue' attribute or the 'min' and 'max' attributes.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <sampledVolume> must either define the attribute 'spatial:sampledValue' or must define both the 'spatial:minValue' and 'spatial:maxValue' attributes, but may not define any other combination of those three attributes.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221751
  { SpatialSampledVolumeMinLessThanMax,
    "A <sampleVolume> 'minValue' must be less than or equal to the 'maxValue'.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'minValue' of a <sampledVolume> must have a value less than or equal to the value of the 'maxValue' attribute of that <sampledVolume>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221752
  { SpatialSampledVolumeValuesMustDiffer,
    "Every <sampleVolume> 'sampledValue' must be unique.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A 'sampledValue' from one <sampledVolume> may not be the same as a different 'sampledValue' from another <sampledVolume> from the same <sampledFieldGeometry>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221753
  { SpatialSampledVolumeValuesNotInOtherRange,
    "Every <sampleVolume> 'sampledValue' must not fall in another's range.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A 'sampledValue' from one <sampledVolume> may not be less than the value of the attribute 'maxValue' and greater than or equal to the value of the attribute 'minValue' of a different <sampledVolume> from the same <sampledFieldGeometry>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221754
  { SpatialSampledVolumeRangesCantOverlap,
    "No two <sampleVolume> ranges may overlap.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The 'minValue' and 'maxValue' attribute values from one <sampledVolume> may not define a range that overlaps the 'minValue' and 'maxValue' attribute values of a different <sampledVolume> from the same <sampledFieldGeometry>, with the exception that the 'maxValue' of one <sampledVolume> may equal the 'minValue' of another <sampledVolume> from the same <sampledFieldGeometry>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221801
  { SpatialAnalyticGeometryAllowedCoreAttributes,
    "Core attributes allowed on <analyticGeometry>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An <analyticGeometry> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on an <analyticGeometry>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221802
  { SpatialAnalyticGeometryAllowedCoreElements,
    "Core elements allowed on <analyticGeometry>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An <analyticGeometry> object may have the optional SBML Level 3 Core "
    "subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on an <analyticGeometry>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221803
  { SpatialAnalyticGeometryAllowedElements,
    "Elements allowed on <analyticGeometry>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An <analyticGeometry> object may contain one and only one instance of the "
    "<listOfAnalyticVolumes> element. No other elements from the SBML Level 3 "
    "Spatial Processes namespaces are permitted on an <analyticGeometry> "
    "object. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221804
  { SpatialAnalyticGeometryLOAnalyticVolumesAllowedCoreElements,
    "Core elements allowed on <listOfAnalyticVolumes>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Apart from the general notes and annotations subobjects permitted on all "
    "SBML objects, a <listOfAnalyticVolumes> container object may only contain "
    "<analyticVolume> objects.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221805
  { SpatialAnalyticGeometryLOAnalyticVolumesAllowedCoreAttributes,
    "Core attributes allowed on <listOfAnalyticVolumes>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <listOfAnalyticVolumes> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <listOfAnalyticVolumes> object.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221901
  { SpatialAnalyticVolumeAllowedCoreAttributes,
    "Core attributes allowed on <analyticVolume>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An <analyticVolume> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on an <analyticVolume>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221902
  { SpatialAnalyticVolumeAllowedCoreElements,
    "Core elements allowed on <analyticVolume>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An <analyticVolume> object may have the optional SBML Level 3 Core "
    "subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on an <analyticVolume>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221903
  { SpatialAnalyticVolumeAllowedAttributes,
    "Attributes allowed on <analyticVolume>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An <analyticVolume> object must have the required attributes 'spatial:id', "
    "'spatial:functionType' and 'spatial:domainType', and may have the optional "
    "attributes 'spatial:name' and 'spatial:ordinal'. No other attributes from "
    "the SBML Level 3 Spatial Processes namespaces are permitted on an "
    "<analyticVolume> object. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221904
  { SpatialAnalyticVolumeAllowedElements,
    "Elements allowed on <analyticVolume>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An <analyticVolume> object may contain one and only one instance of the "
    "ASTNode element. No other elements from the SBML Level 3 Spatial Processes "
    "namespaces are permitted on an <analyticVolume> object. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221905
  { SpatialAnalyticVolumeFunctionTypeMustBeFunctionKindEnum,
    "The 'functionType' attribute must be FunctionKindEnum.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'spatial:functionType' of an <analyticVolume> "
    "object must conform to the syntax of SBML data type 'FunctionKind' and may "
    "only take on the allowed value of 'FunctionKind' defined in SBML; that "
    "is, the value must be 'layered'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221906
  { SpatialAnalyticVolumeDomainTypeMustBeDomainType,
    "The attribute 'domainType' must point to DomainType object.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'spatial:domainType' of an <analyticVolume> "
    "object must be the identifier of an existing <domainType> object defined "
    "in the enclosing <model> object.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221907
  { SpatialAnalyticVolumeNameMustBeString,
    "The 'name' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:name' on an <analyticVolume> must have a value of "
    "data type 'string'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221908
  { SpatialAnalyticVolumeOrdinalMustBeInteger,
    "The 'ordinal' attribute must be Integer.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:ordinal' on an <analyticVolume> must have a value "
    "of data type 'integer'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221950
  { SpatialAnalyticVolumeOrdinalShouldBeUnique,
    "The 'ordinal' attribute should be unique.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_WARNING,
    "No <analyticVolume> should have a 'spatial:ordinal' attribute with the same value as a different <analyticVolume>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222001
  { SpatialParametricGeometryAllowedCoreAttributes,
    "Core attributes allowed on <parametricGeometry>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <parametricGeometry> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <parametricGeometry>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222002
  { SpatialParametricGeometryAllowedCoreElements,
    "Core elements allowed on <parametricGeometry>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <parametricGeometry> object may have the optional SBML Level 3 Core "
    "subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a <parametricGeometry>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222003
  { SpatialParametricGeometryAllowedElements,
    "Elements allowed on <parametricGeometry>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <parametricGeometry> object may contain one and only one instance of "
    "each of the <spatialPoints> and <listOfParametricObjects> elements. No other "
    "elements from the SBML Level 3 Spatial Processes namespaces are permitted "
    "on a <parametricGeometry> object. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222004
  { SpatialParametricGeometryLOParametricObjectsAllowedCoreElements,
    "Core elements allowed on <listOfParametricObjects>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Apart from the general notes and annotations subobjects permitted on all "
    "SBML objects, a <listOfParametricObjects> container object may only "
    "contain <parametricObject> objects.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222005
  { SpatialParametricGeometryLOParametricObjectsAllowedCoreAttributes,
    "Core attributes allowed on <listOfParametricObjects>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <listOfParametricObjects> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <listOfParametricObjects> object.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222050
  { SpatialParametricGeometryNotIn1D,
    "No <parametricGeometry> allowed in 1D geometries.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A model may not have a <parametricGeometry> and a <geometry> with exactly one <coordinateComponent> child.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222101
  { SpatialParametricObjectAllowedCoreAttributes,
    "Core attributes allowed on <parametricObject>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <parametricObject> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <parametricObject>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222102
  { SpatialParametricObjectAllowedCoreElements,
    "Core elements allowed on <parametricObject>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <parametricObject> object may have the optional SBML Level 3 Core "
    "subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a <parametricObject>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222103
  { SpatialParametricObjectAllowedAttributes,
    "Attributes allowed on <parametricObject>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <parametricObject> object must have the required attributes "
    "'spatial:id', 'spatial:polygonType', 'spatial:domainType', "
    "'spatial:pointIndex', 'spatial:pointIndexLength' and "
    "'spatial:compression', and may have the optional attributes 'spatial:name' "
    "and 'spatial:dataType'. No other attributes from the SBML Level 3 Spatial "
    "Processes namespaces are permitted on a <parametricObject> object. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222104
  { SpatialParametricObjectPolygonTypeMustBePolygonKindEnum,
    "The 'polygonType' attribute must be PolygonKindEnum.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'spatial:polygonType' of a <parametricObject> "
    "object must conform to the syntax of SBML data type 'PolygonKind' and may "
    "only take on the allowed values of 'PolygonKind' defined in SBML; that is, "
    "the value must be 'triangle'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222105
  { SpatialParametricObjectDomainTypeMustBeDomainType,
    "The attribute 'domainType' must point to DomainType object.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'spatial:domainType' of a <parametricObject> "
    "object must be the identifier of an existing <domainType> object defined "
    "in the enclosing <model> object.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222106
  { SpatialParametricObjectPointIndexMustBeString,
    "The 'pointIndex' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'spatial:pointIndex' of a <parametricObject> "
    "object must be an array of values of type 'integer'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222107
  { SpatialParametricObjectPointIndexLengthMustBeInteger,
    "The 'pointIndexLength' attribute must be Integer.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:pointIndexLength' on a <parametricObject> must have "
    "a value of data type 'integer'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222108
  { SpatialParametricObjectCompressionMustBeCompressionKindEnum,
    "The 'compression' attribute must be CompressionKindEnum.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'spatial:compression' of a <parametricObject> "
    "object must conform to the syntax of SBML data type 'CompressionKind' and "
    "may only take on the allowed values of 'CompressionKind' defined in SBML; "
    "that is, the value must be one of the following: 'uncompressed' or "
    "'deflated'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222109
  { SpatialParametricObjectNameMustBeString,
    "The 'name' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:name' on a <parametricObject> must have a value of "
    "data type 'string'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222110
  { SpatialParametricObjectDataTypeMustBeDataKindEnum,
    "The 'dataType' attribute must be DataKindEnum.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'spatial:dataType' of a <parametricObject> "
    "object must conform to the syntax of SBML data type 'DataKind' and may "
    "only take on a subset of the allowed values of 'DataKind' defined in SBML; that is, "
    "the value must be one of the following: 'uint', 'uint8', "
    "'uint16' or 'uint32'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222150
  { SpatialParametricObjectPointIndexLengthMustMatchUncompressed,
    "The 'pointIndexLength' attribute must match the entries when uncompressed.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "If the 'spatial:compression' attribute of a <parametricObject> has the value 'uncompressed', the 'spatial:pointIndexLength' attribute of the same <parametricObject> must equal the number of entries in the ArrayData child of the <parametricObject>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222151
  { SpatialParametricObjectPointIndexLengthMustMatchCompressed,
    "The 'pointIndexLength' attribute must match the compressed data.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "If the 'spatial:compression' attribute of a <parametricObject> has the value 'deflated', the 'spatial:pointIndexLength' attribute of the same <parametricObject> must equal the number of entries in the ArrayData child of the <parametricObject>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222152
  { SpatialParametricObjectThreePointsForTriangles,
    "The number of point indexes must be divisible by three if polygonType is 'triangle'.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "If the attribute 'spatial:polygonType' of a <parametricObject> has the value 'triangle', the number of uncompressed entries in its ArrayData child must be evenly divisible by three.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222154
  { SpatialParametricObjectIndexesMustBePositiveIntegers,
    "Every index in a <parametricObject> must be a non-negative integer.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Every element of an uncompressed ArrayData child of a <parametricObject> must be a non-negative integer.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222155
  { SpatialParametricObjectIndexesMustBePoints,
    "Every index in a <parametricObject> must be a defined point.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Every element of an uncompressed ArrayData child of a <parametricObject> must be a value less than the number of points described by the <spatialPoints> object in the same <parametricGeometry>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222156
  { SpatialParametricObjectFacesSameChirality,
    "Every face in a <parametricObject> should be the same chirality.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_WARNING,
    "Every face defined in the ArrayData child of a <parametricObject> should be consistently defined in a clockwise or counter-clockwise fashion across all faces in the same <parametricObject>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222157
  { SpatialParametricObjectMaxTwoPointBorders,
    "No two faces in a <parametricObject> may share more than one border.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "No two faces defined in the ArrayData child of a <parametricObject> may share more than two coordinates.",
    { "L3V1 Spatial V1 Section"
    }
  },

  //// 1222158
  //{ SpatialParametricObjectMaxOnePointJunctions,
  //  "No three faces in a <parametricObject> may share more than one junction.",
  //  LIBSBML_CAT_GENERAL_CONSISTENCY,
  //  LIBSBML_SEV_ERROR,
  //  "No three faces defined in the ArrayData child of a <parametricObject> may share more than one coordinate.",
  //  { "L3V1 Spatial V1 Section"
  //  }
  //},

  // 1222201
  { SpatialCSGeometryAllowedCoreAttributes,
    "Core attributes allowed on <csGeometry>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <csGeometry> object may have the optional SBML Level 3 Core attributes "
    "'metaid' and 'sboTerm'. No other attributes from the SBML Level 3 Core "
    "namespaces are permitted on a <csGeometry>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222202
  { SpatialCSGeometryAllowedCoreElements,
    "Core elements allowed on <csGeometry>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <csGeometry> object may have the optional SBML Level 3 Core subobjects "
    "for notes and annotations. No other elements from the SBML Level 3 Core "
    "namespaces are permitted on a <csGeometry>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222203
  { SpatialCSGeometryAllowedElements,
    "Elements allowed on <csGeometry>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <csGeometry> object may contain one and only one instance of the "
    "<listOfCSGObjects> element. No other elements from the SBML Level 3 "
    "Spatial Processes namespaces are permitted on a <csGeometry> object. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222204
  { SpatialCSGeometryLOCSGObjectsAllowedCoreElements,
    "Core elements allowed on <listOfCSGObjects>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Apart from the general notes and annotations subobjects permitted on all "
    "SBML objects, a <listOfCSGObjects> container object may only contain "
    "<csgObject> objects.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222205
  { SpatialCSGeometryLOCSGObjectsAllowedCoreAttributes,
    "Core attributes allowed on <listOfCSGObjects>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <listOfCSGObjects> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <listOfCSGObjects> object.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222301
  { SpatialCSGObjectAllowedCoreAttributes,
    "Core attributes allowed on <csgObject>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <csgObject> object may have the optional SBML Level 3 Core attributes "
    "'metaid' and 'sboTerm'. No other attributes from the SBML Level 3 Core "
    "namespaces are permitted on a <csgObject>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222302
  { SpatialCSGObjectAllowedCoreElements,
    "Core elements allowed on <csgObject>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <csgObject> object may have the optional SBML Level 3 Core subobjects "
    "for notes and annotations. No other elements from the SBML Level 3 Core "
    "namespaces are permitted on a <csgObject>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222303
  { SpatialCSGObjectAllowedAttributes,
    "Attributes allowed on <csgObject>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <csgObject> object must have the required attributes 'spatial:id' and "
    "'spatial:domainType', and may have the optional attributes 'spatial:name' "
    "and 'spatial:ordinal'. No other attributes from the SBML Level 3 Spatial "
    "Processes namespaces are permitted on a <csgObject> object. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222304
  { SpatialCSGObjectAllowedElements,
    "Elements allowed on <csgObject>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <csgObject> object must contain one and only one instance of the CSGNode "
    "element. No other elements from the SBML Level 3 Spatial Processes "
    "namespaces are permitted on a <csgObject> object. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222305
  { SpatialCSGObjectDomainTypeMustBeDomainType,
    "The attribute 'domainType' must point to DomainType object.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'spatial:domainType' of a <csgObject> object "
    "must be the identifier of an existing <domainType> object defined in the "
    "enclosing <model> object.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222306
  { SpatialCSGObjectNameMustBeString,
    "The 'name' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:name' on a <csgObject> must have a value of data "
    "type 'string'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222307
  { SpatialCSGObjectOrdinalMustBeInteger,
    "The 'ordinal' attribute must be Integer.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:ordinal' on a <csgObject> must have a value of data "
    "type 'integer'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222350
  { SpatialCSGObjectOrdinalShouldBeUnique,
    "The 'ordinal' attribute should be unique.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_WARNING,
    "No <csgObject> should have a 'spatial:ordinal' attribute with the same value as a different <csgObject>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222351
  { SpatialCSGObjectMustEncompassInteriorPoints,
    "CSGObjects must encompass their domainType's interior points.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_WARNING,
    "Any <interiorPoint> of the <domainType> referenced by a <csgObject> must be inside the geometry the <csgObject> describes.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222401
  { SpatialCSGNodeAllowedCoreAttributes,
    "Core attributes allowed on <csgNode>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <csgNode> object may have the optional SBML Level 3 Core attributes "
    "'metaid' and 'sboTerm'. No other attributes from the SBML Level 3 Core "
    "namespaces are permitted on a <csgNode>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222402
  { SpatialCSGNodeAllowedCoreElements,
    "Core elements allowed on <csgNode>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <csgNode> object may have the optional SBML Level 3 Core subobjects for "
    "notes and annotations. No other elements from the SBML Level 3 Core "
    "namespaces are permitted on a <csgNode>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222403
  { SpatialCSGNodeAllowedAttributes,
    "Attributes allowed on <csgNode>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <csgNode> object may have the optional attributes 'spatial:id' and "
    "'spatial:name'. No other attributes from the SBML Level 3 Spatial "
    "Processes namespaces are permitted on a <csgNode> object. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222404
  { SpatialCSGNodeNameMustBeString,
    "The 'name' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:name' on a <csgNode> must have a value of data type "
    "'string'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222601
  { SpatialCSGTranslationAllowedCoreAttributes,
    "Core attributes allowed on <csgTranslation>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <csgTranslation> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <csgTranslation>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222602
  { SpatialCSGTranslationAllowedCoreElements,
    "Core elements allowed on <csgTranslation>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <csgTranslation> object may have the optional SBML Level 3 Core "
    "subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a <csgTranslation>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222603
  { SpatialCSGTranslationAllowedAttributes,
    "Attributes allowed on <csgTranslation>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <csgTranslation> object must have the required attribute "
    "'spatial:translateX', and may have the optional attributes "
    "'spatial:translateY' and 'spatial:translateZ'. No other attributes from "
    "the SBML Level 3 Spatial Processes namespaces are permitted on a "
    "<csgTranslation> object. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222604
  { SpatialCSGTranslationTranslateXMustBeDouble,
    "The 'translateX' attribute must be Double.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:translateX' on a <csgTranslation> must have a value "
    "of data type 'double'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222605
  { SpatialCSGTranslationTranslateYMustBeDouble,
    "The 'translateY' attribute must be Double.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:translateY' on a <csgTranslation> must have a value "
    "of data type 'double'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222606
  { SpatialCSGTranslationTranslateZMustBeDouble,
    "The 'translateZ' attribute must be Double.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:translateZ' on a <csgTranslation> must have a value "
    "of data type 'double'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222650
  { SpatialCSGTranslationAllowedElements,
    "Elements allowed on <csgTranslation>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <csgTranslation> object may contain one and only one child "
    "CSGNode element. No other elements from the SBML Level 3 Spatial Processes "
    "namespaces are permitted on a <csgTransformation> object. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222651
  { SpatialCSGTranslationTranslateYRequiredIn2D,
    "A <csgTranslation> must have 'translateY' in 2D or 3D geometries.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:translateY' on a <csgTranslation> is required if the <geometry> of the model has a <coordinateComponent> child of type 'cartesianY'. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222652
  { SpatialCSGTranslationTranslateZRequiredIn3D,
    "A <csgTranslation> must have 'translateZ' in 3D geometries.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:translateZ' on a <csgTranslation> is required if the <geometry> of the model has a <coordinateComponent> child of type 'cartesianZ'. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222653
  { SpatialCSGTranslationNoTranslateYIn1D,
    "A <csgTranslation> must not have 'translateY' in 1D geometries.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:translateY' on a <csgTranslation> must not be defined if the <geometry> of the model has no <coordinateComponent> child of type 'cartesianY'. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222654
  { SpatialCSGTranslationNoTranslateZIn2D,
    "A <csgTranslation> must not have 'translateZ' in 1D or 2D geometries.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:translateZ' on a <csgTranslation> must not be defined if the <geometry> of the model has no <coordinateComponent> child of type 'cartesianZ'. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222701
  { SpatialCSGRotationAllowedCoreAttributes,
    "Core attributes allowed on <csgRotation>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <csgRotation> object may have the optional SBML Level 3 Core attributes "
    "'metaid' and 'sboTerm'. No other attributes from the SBML Level 3 Core "
    "namespaces are permitted on a <csgRotation>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222702
  { SpatialCSGRotationAllowedCoreElements,
    "Core elements allowed on <csgRotation>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <csgRotation> object may have the optional SBML Level 3 Core subobjects "
    "for notes and annotations. No other elements from the SBML Level 3 Core "
    "namespaces are permitted on a <csgRotation>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222703
  { SpatialCSGRotationAllowedAttributes,
    "Attributes allowed on <csgRotation>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <csgRotation> object must have the required attributes 'spatial:rotateX' "
    "and 'spatial:rotateAngleInRadians', and may have the optional attributes "
    "'spatial:rotateY' and 'spatial:rotateZ'. No other attributes from the SBML "
    "Level 3 Spatial Processes namespaces are permitted on a <csgRotation> "
    "object. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222704
  { SpatialCSGRotationRotateXMustBeDouble,
    "The 'rotateX' attribute must be Double.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:rotateX' on a <csgRotation> must have a value of "
    "data type 'double'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222705
  { SpatialCSGRotationRotateAngleInRadiansMustBeDouble,
    "The 'rotateAngleInRadians' attribute must be Double.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:rotateAngleInRadians' on a <csgRotation> must have "
    "a value of data type 'double'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222706
  { SpatialCSGRotationRotateYMustBeDouble,
    "The 'rotateY' attribute must be Double.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:rotateY' on a <csgRotation> must have a value of "
    "data type 'double'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222707
  { SpatialCSGRotationRotateZMustBeDouble,
    "The 'rotateZ' attribute must be Double.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:rotateZ' on a <csgRotation> must have a value of "
    "data type 'double'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222750
  { SpatialCSGRotationAllowedElements,
    "Elements allowed on <csgRotation>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <csgRotation> object may contain one and only one child "
    "CSGNode element. No other elements from the SBML Level 3 Spatial Processes "
    "namespaces are permitted on a <csgRotation> object.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222751
  { SpatialCSGRotationRotateYRequiredIn2D,
    "A <csgRotation> must have 'rotateY' in 2D or 3D geometries.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:rotateY' on a <csgRotation> is required if the <geometry> of the model has a <coordinateComponent> child of type 'cartesianY'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222752
  { SpatialCSGRotationRotateZRequiredIn3D,
    "A <csgRotation> must have 'rotateZ' in 3D geometries.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:rotateZ' on a <csgRotation> is required if the <geometry> of the model has a <coordinateComponent> child of type 'cartesianZ'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222753
  { SpatialCSGRotationNoRotateYIn1D,
    "A <csgRotation> must not have 'rotateY' in 1D geometries.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:rotateY' on a <csgRotation> must not be defined if the <geometry> of the model has no <coordinateComponent> child of type 'cartesianY'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222754
  { SpatialCSGRotationNoRotateZIn2D,
    "A <csgRotation> must not have 'rotateZ' in 2D geometries.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:rotateZ' on a <csgRotation> must not be defined if the <geometry> of the model has no <coordinateComponent> child of type 'cartesianZ'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222755
  { SpatialCSGRotationNoOriginIn3D,
    "A <csgRotation> must have a non-zero 'rotate' attribute in 3D geometries.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "If the <geometry> of the model has exactly three <coordinateComponent> children, at least one of the attributes 'spatial:rotateX', 'spatial:rotateY', or 'spatial:rotateZ' of a <csgRotation> must not be equal to zero.",
    { "L3V1 Spatial V1 Section"
    }
  },

    // 1222801
  { SpatialCSGScaleAllowedCoreAttributes,
    "Core attributes allowed on <csgScale>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <csgScale> object may have the optional SBML Level 3 Core attributes "
    "'metaid' and 'sboTerm'. No other attributes from the SBML Level 3 Core "
    "namespaces are permitted on a <csgScale>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222802
  { SpatialCSGScaleAllowedCoreElements,
    "Core elements allowed on <csgScale>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <csgScale> object may have the optional SBML Level 3 Core subobjects for "
    "notes and annotations. No other elements from the SBML Level 3 Core "
    "namespaces are permitted on a <csgScale>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222803
  { SpatialCSGScaleAllowedAttributes,
    "Attributes allowed on <csgScale>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <csgScale> object must have the required attribute 'spatial:scaleX', and "
    "may have the optional attributes 'spatial:scaleY' and 'spatial:scaleZ'. No "
    "other attributes from the SBML Level 3 Spatial Processes namespaces are "
    "permitted on a <csgScale> object. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222804
  { SpatialCSGScaleScaleXMustBeDouble,
    "The 'scaleX' attribute must be Double.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:scaleX' on a <csgScale> must have a value of data "
    "type 'double'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222805
  { SpatialCSGScaleScaleYMustBeDouble,
    "The 'scaleY' attribute must be Double.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:scaleY' on a <csgScale> must have a value of data "
    "type 'double'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222806
  { SpatialCSGScaleScaleZMustBeDouble,
    "The 'scaleZ' attribute must be Double.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:scaleZ' on a <csgScale> must have a value of data "
    "type 'double'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222850
  { SpatialCSGScaleAllowedElements,
    "Elements allowed on <csgScale>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <csgScale> object may contain one and only one child "
    "CSGNode element. No other elements from the SBML Level 3 Spatial Processes "
    "namespaces are permitted on a <csgScale> object. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222751
  { SpatialCSGScaleScaleYRequiredIn2D,
    "A <csgScale> must have 'scaleY' in 2D or 3D geometries.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:scaleY' on a <csgScale> is required if the <geometry> of the model has a <coordinateComponent> child of type 'cartesianY'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222752
  { SpatialCSGScaleScaleZRequiredIn3D,
    "A <csgScale> must have 'scaleZ' in 3D geometries.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:scaleZ' on a <csgScale> is required if the <geometry> of the model has a <coordinateComponent> child of type 'cartesianZ'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222753
  { SpatialCSGScaleNoScaleYIn1D,
    "A <csgScale> must not have 'scaleY' in 1D geometries.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:scaleY' on a <csgScale> must not be defined if the <geometry> of the model has no <coordinateComponent> child of type 'cartesianY'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222754
  { SpatialCSGScaleNoScaleZIn2D,
    "A <csgScale> must not have 'scaleZ' in 1D or 2D geometries.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:scaleZ' on a <csgScale> must not be defined if the <geometry> of the model has no <coordinateComponent> child of type 'cartesian'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222901
  { SpatialCSGHomogeneousTransformationAllowedCoreAttributes,
    "Core attributes allowed on <csgHomogeneousTransformation>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <csgHomogeneousTransformation> object may have the optional SBML Level 3 "
    "Core attributes 'metaid' and 'sboTerm'. No other attributes from the SBML "
    "Level 3 Core namespaces are permitted on a "
    "<csgHomogeneousTransformation>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222902
  { SpatialCSGHomogeneousTransformationAllowedCoreElements,
    "Core elements allowed on <csgHomogeneousTransformation>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <csgHomogeneousTransformation> object may have the optional SBML Level 3 "
    "Core subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a "
    "<csgHomogeneousTransformation>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222903
  { SpatialCSGHomogeneousTransformationAllowedElements,
    "Elements allowed on <csgHomogeneousTransformation>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <csgHomogeneousTransformation> object must contain one and only one "
    "child <forwardTransformation> element of type 'TransformationComponent', and may additionally contain one and only one child CSGNode element. No other elements from "
    "the SBML Level 3 Spatial Processes namespaces are permitted on a "
    "<csgHomogeneousTransformation> object. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223001
  { SpatialTransformationComponentAllowedCoreAttributes,
    "Core attributes allowed on <transformationComponent>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <transformationComponent> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <transformationComponent>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223002
  { SpatialTransformationComponentAllowedCoreElements,
    "Core elements allowed on <transformationComponent>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <transformationComponent> object may have the optional SBML Level 3 Core "
    "subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a <transformationComponent>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223003
  { SpatialTransformationComponentAllowedAttributes,
    "Attributes allowed on <transformationComponent>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <transformationComponent> object must have the required attributes "
    "'spatial:components' and 'spatial:componentsLength'. No other attributes "
    "from the SBML Level 3 Spatial Processes namespaces are permitted on a "
    "<transformationComponent> object. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223004
  { SpatialTransformationComponentComponentsMustBeDoubleArray,
    "The 'components' attribute must be an array of doubles.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'spatial:components' of a "
    "<transformationComponent> object must be an array of values of type "
    "'double'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223005
  { SpatialTransformationComponentComponentsLengthMustBeInteger,
    "The 'componentsLength' attribute must be Integer.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:componentsLength' on a <transformationComponent> "
    "must have a value of data type 'integer'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223050
  { SpatialTransformationComponentComponentsLengthMustBe16,
    "The 'componentsLength' attribute must be '16'.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:componentsLength' on a <transformationComponent> "
    "must have a value of '16'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223051
  { SpatialTransformationComponentArrayLengthMustBe16,
    "The length of the 'components' attribute must be 16.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The length of the array of the attribute 'spatial:components' on a TransformationComponent must be 16.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223101
  { SpatialCSGPrimitiveAllowedCoreAttributes,
    "Core attributes allowed on <csgPrimitive>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <csgPrimitive> object may have the optional SBML Level 3 Core attributes "
    "'metaid' and 'sboTerm'. No other attributes from the SBML Level 3 Core "
    "namespaces are permitted on a <csgPrimitive>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223102
  { SpatialCSGPrimitiveAllowedCoreElements,
    "Core elements allowed on <csgPrimitive>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <csgPrimitive> object may have the optional SBML Level 3 Core subobjects "
    "for notes and annotations. No other elements from the SBML Level 3 Core "
    "namespaces are permitted on a <csgPrimitive>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223103
  { SpatialCSGPrimitiveAllowedAttributes,
    "Attributes allowed on <csgPrimitive>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <csgPrimitive> object must have the required attribute "
    "'spatial:primitiveType'. No other attributes from the SBML Level 3 Spatial "
    "Processes namespaces are permitted on a <csgPrimitive> object. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223104
  { SpatialCSGPrimitivePrimitiveTypeMustBePrimitiveKindEnum,
    "The 'primitiveType' attribute must be PrimitiveKindEnum.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'spatial:primitiveType' of a <csgPrimitive> "
    "object must conform to the syntax of SBML data type 'PrimitiveKind' and "
    "may only take on the allowed values of 'PrimitiveKind' defined in SBML; "
    "that is, the value must be one of the following: 'sphere', 'cube', "
    "'cylinder', 'cone', 'circle' or 'square'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223150
  { SpatialCSGPrimitive3DShapes,
    "Any <csgPrimitive> with a 3D shape must be in 3D geometry.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The 'spatial:primitiveType' attribute of a <csgPrimitive> may have the values 'sphere', 'cube', 'cylinder', or 'cone' only if the <geometry> of the model has exactly three <coordinateComponent> children.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223150
  { SpatialCSGPrimitive2DShapes,
    "Any <csgPrimitive> with a 2D shape must be in 2D or 3D geometry.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The 'spatial:primitiveType' attribute of a <csgPrimitive> may have the values 'circle' or 'square' only if the <geometry> of the model has exactly two or three <coordinateComponent> children.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223201
  { SpatialCSGSetOperatorAllowedCoreAttributes,
    "Core attributes allowed on <csgSetOperator>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <csgSetOperator> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <csgSetOperator>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223202
  { SpatialCSGSetOperatorAllowedCoreElements,
    "Core elements allowed on <csgSetOperator>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <csgSetOperator> object may have the optional SBML Level 3 Core "
    "subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a <csgSetOperator>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223203
  { SpatialCSGSetOperatorAllowedAttributes,
    "Attributes allowed on <csgSetOperator>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <csgSetOperator> object must have the required attribute "
    "'spatial:operationType', and may have the optional attributes "
    "'spatial:complementA' and 'spatial:complementB'. No other attributes from "
    "the SBML Level 3 Spatial Processes namespaces are permitted on a "
    "<csgSetOperator> object. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223204
  { SpatialCSGSetOperatorAllowedElements,
    "Elements allowed on <csgSetOperator>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <csgSetOperator> object may contain one and only one instance of the "
    "<listOfCSGNodes> element. No other elements from the SBML Level 3 Spatial "
    "Processes namespaces are permitted on a <csgSetOperator> object. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223205
  { SpatialCSGSetOperatorOperationTypeMustBeSetOperationEnum,
    "The 'operationType' attribute must be SetOperationEnum.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'spatial:operationType' of a <csgSetOperator> "
    "object must conform to the syntax of SBML data type 'SetOperation' and may "
    "only take on the allowed values of 'SetOperation' defined in SBML; that "
    "is, the value must be one of the following: 'union', 'intersection' or "
    "'difference'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223208
  { SpatialCSGSetOperatorLOCSGNodesAllowedCoreElements,
    "Core elements allowed on <listOfCSGNodes>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Apart from the general notes and annotations subobjects permitted on all "
    "SBML objects, a <listOfCSGNodes> container object may only contain "
    "<csgNode> objects.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223209
  { SpatialCSGSetOperatorLOCSGNodesAllowedCoreAttributes,
    "Core attributes allowed on <listOfCSGNodes>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <listOfCSGNodes> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <listOfCSGNodes> object.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223250
  { SpatialCSGSetOperatorTwoComplementsForDifference,
    "Need two components for 'difference' type.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "If the attribute 'spatial:operationType' of a <csgSetOperator> has the value 'difference', it must also define values for the attributes 'spatial:complementA' and 'spatial:complementB'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223251
  { SpatialCSGSetOperatorNoComplementsUnionIntersection,
    "No components for 'union' or 'intersection' types.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "If the attribute 'spatial:operationType' of a <csgSetOperator> has the value 'union' or 'intersection', it must not define values for the attributes 'spatial:complementA' nor 'spatial:complementB'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223252
  { SpatialCSGSetOperatorDifferenceMustHaveTwoChildren,
    "A <csgSetOperator> difference must have two children.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "If the 'operationType' of a <csgSetOperator> is 'difference', it must have exactly two <csgNode> children.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223253
  { SpatialCSGSetOperatorComplementsMustReferenceChildren,
    "The 'complement' attributes of a <csgSetOperator> must reference its children.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attributes 'complementA' and 'complementB' of a <csgSetOperator> must reference its two <csgNode> children.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223254
  { SpatialCSGSetOperatorShouldHaveTwoPlusChildren,
    "The <csgSetOperator> should have two or more children.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_INFO,
    "A <csgSetOperator> should usually have at least two children, as operations involving zero or one child can be accomplished more efficiently in other ways.",
    { "L3V1 Spatial V1 Section"
    }
  },

  { SpatialSpatialSymbolReferenceAllowedCoreAttributes,
    "Core attributes allowed on <spatialSymbolReference>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <spatialSymbolReference> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <spatialSymbolReference>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223302
  { SpatialSpatialSymbolReferenceAllowedCoreElements,
    "Core elements allowed on <spatialSymbolReference>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <spatialSymbolReference> object may have the optional SBML Level 3 Core "
    "subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a <spatialSymbolReference>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223303
  { SpatialSpatialSymbolReferenceAllowedAttributes,
    "Attributes allowed on <spatialSymbolReference>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <spatialSymbolReference> object must have the required attribute "
    "'spatial:spatialRef'. No other attributes from the SBML Level 3 Spatial "
    "Processes namespaces are permitted on a <spatialSymbolReference> object. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223304
  { SpatialSpatialSymbolReferenceSpatialRefMustReferenceMath,
    "The attribute 'spatialRef' must point to a spatial object with mathematical meaning.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'spatial:spatialRef' of a "
    "<spatialSymbolReference> object must be the identifier of an existing "
    "<compartmentMapping>, <coordinateComponent>, <boundary>, <domainType>, <domain>, or <sampledField> object defined in the enclosing <model> object.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223350
  { SpatialSpatialSymbolReferenceUniqueRef,
    "Every <spatialSymbolReference> must point to unique object.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Every <spatialSymbolReference> in a <model> must reference a unique spatial element.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223401
  { SpatialDiffusionCoefficientAllowedCoreAttributes,
    "Core attributes allowed on <diffusionCoefficient>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <diffusionCoefficient> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <diffusionCoefficient>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223402
  { SpatialDiffusionCoefficientAllowedCoreElements,
    "Core elements allowed on <diffusionCoefficient>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <diffusionCoefficient> object may have the optional SBML Level 3 Core "
    "subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a <diffusionCoefficient>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223403
  { SpatialDiffusionCoefficientAllowedAttributes,
    "Attributes allowed on <diffusionCoefficient>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <diffusionCoefficient> object must have the required attributes "
    "'spatial:variable' and 'spatial:type', and may have the optional "
    "attributes 'spatial:coordinateReference1' and "
    "'spatial:coordinateReference2'. No other attributes from the SBML Level "
    "3 Spatial Processes namespaces are permitted on a <diffusionCoefficient> "
    "object. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223404
  { SpatialDiffusionCoefficientVariableMustBeSpeciesOrParam,
    "The attribute 'variable' must point to Species or Parameter object.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'spatial:variable' of a <diffusionCoefficient> "
    "object must be the identifier of an existing <species> or <parameter> object defined in "
    "the enclosing <model> object.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223405
  { SpatialDiffusionCoefficientTypeMustBeDiffusionKindEnum,
    "The 'type' attribute must be DiffusionKindEnum.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'spatial:type' of a <diffusionCoefficient> "
    "object must conform to the syntax of SBML data type 'DiffusionKind' and "
    "may only take on the allowed values of 'DiffusionKind' defined in SBML; "
    "that is, the value must be one of the following: 'isotropic', "
    "'anisotropic' or 'tensor'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223406
  { SpatialDiffusionCoefficientCoordinateReference1MustBeCoordinateKindEnum,
    "The 'coordinateReference1' attribute must be CoordinateKindEnum.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'spatial:coordinateReference1' of a "
    "<diffusionCoefficient> object must conform to the syntax of SBML data type "
    "'CoordinateKind' and may only take on the allowed values of "
    "'CoordinateKind' defined in SBML; that is, the value must be one of the "
    "following: 'cartesianX', 'cartesianY' or 'cartesianZ'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223407
  { SpatialDiffusionCoefficientCoordinateReference2MustBeCoordinateKindEnum,
    "The 'coordinateReference2' attribute must be CoordinateKindEnum.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'spatial:coordinateReference2' of a "
    "<diffusionCoefficient> object must conform to the syntax of SBML data type "
    "'CoordinateKind' and may only take on the allowed values of "
    "'CoordinateKind' defined in SBML; that is, the value must be one of the "
    "following: 'cartesianX', 'cartesianY' or 'cartesianZ'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223450
  { SpatialDiffusionCoefficientNoCoordinateReferencesForIsotropic,
    "No coordinate references for 'isotropic' type.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <diffusionCoefficient> with an attribute 'type' value of 'isotropic' may not define the attributes 'spatial:coordinateReference1' nor 'spatial:coordinateReference2'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223451
  { SpatialDiffusionCoefficientTwoCoordinateReferencesForTensor,
    "Two coordinate references for 'tensor' type.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <diffusionCoefficient> with an attribute 'type' value of 'tensor' must define the attributes 'spatial:coordinateReference1' and 'spatial:coordinateReference2'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223452
  { SpatialDiffusionCoefficientOneCoordinateReferencesForAnisotropic,
    "One coordinate references for 'anisotropic' type.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <diffusionCoefficient> with an attribute 'type' value of 'anisotropic' must define the attribute 'spatial:coordinateReference1', and must not define the attribute 'spatial:coordinateReference2'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223453
  { SpatialDiffusionCoefficientUnits,
    "Diffusion coefficient units should be length^2/time.",
    LIBSBML_CAT_UNITS_CONSISTENCY,
    LIBSBML_SEV_WARNING,
    "The units of a <parameter> with a <diffusionCoefficient> child should be length^2/time.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223454
  { SpatialDiffusionCoefficientCoordinateReferenceDifference,
    "Coordinate references must be different from each other.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The <diffusionCoefficient> attribute 'spatial:coordinateReference2', if defined, must have a value different from that of the attribute 'spatial:coordinateReference1'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223455
  { SpatialDiffusionCoefficientCoordinateReferenceNoYIn1D,
    "Coordinate references must not be 'cartesianY' in 1D geometries.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The <diffusionCoefficient> attributes 'spatial:coordinateReference1' and 'spatial:coordinateReference2' may not have a value of 'cartesianY' if the <geometry> has exactly one <coordinateComponent> child.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223456
  { SpatialDiffusionCoefficientCoordinateReferenceNoZIn2D,
    "Coordinate references must not be 'cartesianZ' in 1- or 2-D geometries.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The <diffusionCoefficient> attributes 'spatial:coordinateReference1' and 'spatial:coordinateReference2' may not have a value of 'cartesianZ' if the <geometry> has exactly one or two <coordinateComponent> children.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223457
  { SpatialNoDiffusionCoefficientOverlap,
    "No overlapping diffusion coefficients for the same species or parameter.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Any <species> or <parameter> may only have a single <diffusionCoefficient> that applies to any given cardinal axis or plane.  A <diffusionCoefficient> of type 'anisotropic' applies to the axis it references, and any plane in the <geometry> that contains that axis.  A <diffusionCoefficient> of type 'tensor' applies to the plane defined by the two axes it references.  A <diffusionCoefficient> of type 'isotropic' applies to all axes and planes in the <geometry>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223458
  { SpatialDiffusionCoefficientVariableMustNotBeSelf,
    "The attribute 'variable' must not point to itself.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'spatial:variable' of a <diffusionCoefficient> "
    "object must not be the identifier of its parent <parameter>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  { SpatialAdvectionCoefficientAllowedCoreAttributes,
    "Core attributes allowed on <advectionCoefficient>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An <advectionCoefficient> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on an <advectionCoefficient>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223502
  { SpatialAdvectionCoefficientAllowedCoreElements,
    "Core elements allowed on <advectionCoefficient>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An <advectionCoefficient> object may have the optional SBML Level 3 Core "
    "subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on an <advectionCoefficient>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223503
  { SpatialAdvectionCoefficientAllowedAttributes,
    "Attributes allowed on <advectionCoefficient>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An <advectionCoefficient> object must have the required attributes "
    "'spatial:variable' and 'spatial:coordinate'. No other attributes from the "
    "SBML Level 3 Spatial Processes namespaces are permitted on an "
    "<advectionCoefficient> object. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223504
  { SpatialAdvectionCoefficientVariableMustBeSpeciesOrParam,
    "The attribute 'variable' must point to Species or Parameter object.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'spatial:variable' of an <advectionCoefficient> "
    "object must be the identifier of an existing <species> or <parameter> object defined in "
    "the enclosing <model> object.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223505
  { SpatialAdvectionCoefficientCoordinateMustBeCoordinateKindEnum,
    "The 'coordinate' attribute must be CoordinateKindEnum.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'spatial:coordinate' of an "
    "<advectionCoefficient> object must conform to the syntax of SBML data type "
    "'CoordinateKind' and may only take on the allowed values of "
    "'CoordinateKind' defined in SBML; that is, the value must be one of the "
    "following: 'cartesianX', 'cartesianY' or 'cartesianZ'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223550
  { SpatialAdvectionCoefficientUnits,
    "The units of an advection coefficient should be length/time.",
    LIBSBML_CAT_UNITS_CONSISTENCY,
    LIBSBML_SEV_WARNING,
    "The units of a <parameter> with an <advectionCoefficient> child should be length/time.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223551
  { SpatialAdvectionCoefficientsMustBeUnique,
    "The 'coordinate' and 'variable' attributes must be unique.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "No two <advectionCoefficient> elements in the same <model> may have the same values for the attributes 'spatial:variable' and 'spatial:coordinate'.  Only one advection coefficient may be defined per species (or parameter) per axis.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223552
  { SpatialAdvectionCoefficientVariableMustNotBeSelf,
    "The attribute 'variable' must not point to itself.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'spatial:variable' of an <advectionCoefficient> "
    "object must not be the identifier of its parent <parameter> object.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223601
  { SpatialBoundaryConditionAllowedCoreAttributes,
    "Core attributes allowed on <boundaryCondition>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <boundaryCondition> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <boundaryCondition>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223602
  { SpatialBoundaryConditionAllowedCoreElements,
    "Core elements allowed on <boundaryCondition>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <boundaryCondition> object may have the optional SBML Level 3 Core "
    "subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a <boundaryCondition>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223603
  { SpatialBoundaryConditionAllowedAttributes,
    "Attributes allowed on <boundaryCondition>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <boundaryCondition> object must have the required attributes "
    "'spatial:variable' and 'spatial:type', and may have the optional "
    "attributes 'spatial:coordinateBoundary' and 'spatial:boundaryDomainType'. "
    "No other attributes from the SBML Level 3 Spatial Processes namespaces are "
    "permitted on a <boundaryCondition> object. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223604
  { SpatialBoundaryConditionVariableMustBeSpecies,
    "The attribute 'variable' must point to Species object.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'spatial:variable' of a <boundaryCondition> "
    "object must be the identifier of an existing <species> object defined in "
    "the enclosing <model> object.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223605
  { SpatialBoundaryConditionTypeMustBeBoundaryKindEnum,
    "The 'type' attribute must be BoundaryKindEnum.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'spatial:type' of a <boundaryCondition> object "
    "must conform to the syntax of SBML data type 'BoundaryKind' and may only "
    "take on the allowed values of 'BoundaryKind' defined in SBML; that is, the "
    "value must be one of the following: 'Neumann' or "
    "'Dirichlet'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223606
  { SpatialBoundaryConditionCoordinateBoundaryMustBeBoundary,
    "The attribute 'coordinateBoundary' must point to Boundary object.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'spatial:coordinateBoundary' of a "
    "<boundaryCondition> object must be the identifier of an existing "
    "<boundary> object defined in the enclosing <model> object.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223607
  { SpatialBoundaryConditionBoundaryDomainTypeMustBeDomainType,
    "The attribute 'boundaryDomainType' must point to DomainType object.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'spatial:boundaryDomainType' of a "
    "<boundaryCondition> object must be the identifier of an existing "
    "<domainType> object defined in the enclosing <model> object.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223650
  { SpatialBoundaryConditionBoundaryDomainTypeOrCoordinateBoundary,
    "A BoundaryCondition must have 'boundaryDomainType' or 'coordinateBoundary' attribute.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <boundaryCondition> must define a value for either the attribute 'spatial:coordinateBoundary' or 'spatial:boundaryDomainType', but not both.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223651
  { SpatialBoundaryConditionsMustBeUnique,
    "Each BoundaryCondition must define only one boundary.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "For every combination of species and boundary, there must be at most exactly one <boundaryCondition> of type 'Neumann', or exactly one <boundaryCondition> of type 'Dirichlet'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223652
  { SpatialDirichletUnits,
    "A 'Dirichlet' BoundaryCondition's units should be concentration.",
    LIBSBML_CAT_UNITS_CONSISTENCY,
    LIBSBML_SEV_WARNING,
    "The units of a <parameter> with a <boundaryCondition> child of type 'Dirichlet' should be the units of concentration of the referenced <species>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223653
  { SpatialNeumannUnits,
    "A 'Neumann' BoundaryCondition's units should be concentration*length/time.",
    LIBSBML_CAT_UNITS_CONSISTENCY,
    LIBSBML_SEV_WARNING,
    "The units of a <parameter> with a <boundaryCondition> child of type 'Neumann' should be the units of concentration of the referenced <species>, times length/time.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223701
  { SpatialGeometryAllowedCoreAttributes,
    "Core attributes allowed on <geometry>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <geometry> object may have the optional SBML Level 3 Core attributes "
    "'metaid' and 'sboTerm'. No other attributes from the SBML Level 3 Core "
    "namespaces are permitted on a <geometry>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223702
  { SpatialGeometryAllowedCoreElements,
    "Core elements allowed on <geometry>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <geometry> object may have the optional SBML Level 3 Core subobjects for "
    "notes and annotations. No other elements from the SBML Level 3 Core "
    "namespaces are permitted on a <geometry>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223703
  { SpatialGeometryAllowedAttributes,
    "Attributes allowed on <geometry>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <geometry> object must have the required attribute "
    "'spatial:coordinateSystem', and may have the optional attribute "
    "'spatial:id'. No other attributes from the SBML Level 3 Spatial Processes "
    "namespaces are permitted on a <geometry> object. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223704
  { SpatialGeometryAllowedElements,
    "Elements allowed on <geometry>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <geometry> object may contain one and only one instance of each of the "
    "<listOfCoordinateComponents,> <listOfDomainTypes,> <listOfDomains,> "
    "<listOfAdjacentDomains,> <listOfGeometryDefinitions> and "
    "<listOfSampledFields> elements. No other elements from the SBML Level 3 "
    "Spatial Processes namespaces are permitted on a <geometry> object. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223705
  { SpatialGeometryCoordinateSystemMustBeGeometryKindEnum,
    "The 'coordinateSystem' attribute must be GeometryKindEnum.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'spatial:coordinateSystem' of a <geometry> "
    "object must conform to the syntax of SBML data type 'GeometryKind' and may "
    "only take on the allowed value of 'GeometryKind' defined in SBML; that "
    "is, the value must be 'cartesian'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223706
  { SpatialGeometryLOCoordinateComponentsAllowedCoreElements,
    "Core elements allowed on <listOfCoordinateComponents>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Apart from the general notes and annotations subobjects permitted on all "
    "SBML objects, a <listOfCoordinateComponents> container object may only "
    "contain <coordinateComponent> objects.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223707
  { SpatialGeometryLODomainTypesAllowedCoreElements,
    "Core elements allowed on <listOfDomainTypes>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Apart from the general notes and annotations subobjects permitted on all "
    "SBML objects, a <listOfDomainTypes> container object may only contain "
    "<domainType> objects.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223708
  { SpatialGeometryLODomainsAllowedCoreElements,
    "Core elements allowed on <listOfDomains>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Apart from the general notes and annotations subobjects permitted on all "
    "SBML objects, a <listOfDomains> container object may only contain <domain> "
    "objects.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223709
  { SpatialGeometryLOAdjacentDomainsAllowedCoreElements,
    "Core elements allowed on <listOfAdjacentDomains>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Apart from the general notes and annotations subobjects permitted on all "
    "SBML objects, a <listOfAdjacentDomains> container object may only contain "
    "<adjacentDomains> objects.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223710
  { SpatialGeometryLOGeometryDefinitionsAllowedCoreElements,
    "Core elements allowed on <listOfGeometryDefinitions>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Apart from the general notes and annotations subobjects permitted on all "
    "SBML objects, a <listOfGeometryDefinitions> container object may only "
    "contain <geometryDefinition> objects.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223711
  { SpatialGeometryLOSampledFieldsAllowedCoreElements,
    "Core elements allowed on <listOfSampledFields>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Apart from the general notes and annotations subobjects permitted on all "
    "SBML objects, a <listOfSampledFields> container object may only contain "
    "<sampledField> objects.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223712
  { SpatialGeometryLOCoordinateComponentsAllowedCoreAttributes,
    "Core attributes allowed on <listOfCoordinateComponents>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <listOfCoordinateComponents> object may have the optional SBML Level 3 "
    "Core attributes 'metaid' and 'sboTerm'. No other attributes from the SBML "
    "Level 3 Core namespaces are permitted on a <listOfCoordinateComponents> "
    "object.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223713
  { SpatialGeometryLODomainTypesAllowedCoreAttributes,
    "Core attributes allowed on <listOfDomainTypes>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <listOfDomainTypes> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <listOfDomainTypes> object.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223714
  { SpatialGeometryLODomainsAllowedCoreAttributes,
    "Core attributes allowed on <listOfDomains>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <listOfDomains> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <listOfDomains> object.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223715
  { SpatialGeometryLOAdjacentDomainsAllowedCoreAttributes,
    "Core attributes allowed on <listOfAdjacentDomains>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <listOfAdjacentDomains> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <listOfAdjacentDomains> object.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223716
  { SpatialGeometryLOGeometryDefinitionsAllowedCoreAttributes,
    "Core attributes allowed on <listOfGeometryDefinitions>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <listOfGeometryDefinitions> object may have the optional SBML Level 3 "
    "Core attributes 'metaid' and 'sboTerm'. No other attributes from the SBML "
    "Level 3 Core namespaces are permitted on a <listOfGeometryDefinitions> "
    "object.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223717
  { SpatialGeometryLOSampledFieldsAllowedCoreAttributes,
    "Core attributes allowed on <listOfSampledFields>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <listOfSampledFields> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <listOfSampledFields> object.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223751
  { SpatialGeometryLOCoordinateComponentsOneToThreeChildren,
    "A <listOfCoordinateComponents> must have one to three children.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <listOfCoordinateComponents> must have exactly one, two, or three children.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223752
  { SpatialGeometryCoordinateComponent1DisX,
    "Solitary <coordinateComponents> must be 'cartesianX'.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "If a <listOfCoordinateComponents> object has exactly one <coordinateComponents> child, that child must have a 'spatial:type' attribute with the value 'cartesianX'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223753
  { SpatialGeometryCoordinateComponent2DisXY,
    "Paired <coordinateComponents> must be 'cartesianX' and 'cartesianY'.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "If a <listOfCoordinateComponents> object has exactly two <coordinateComponents> children, one of them must have a 'spatial:type' attribute with the value 'cartesianX', and the other must have a 'spatial:type' attribute with the value 'cartesianY'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223754
  { SpatialGeometryCoordinateComponent3DisXYZ,
    "Three <coordinateComponents> must be cartesian X, Y, and Z.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "If a <listOfCoordinateComponents> object has exactly three <coordinateComponents> children, one of them must have a 'spatial:type' attribute with the value 'cartesianX', one must have a 'spatial:type' attribute with the value 'cartesianY', and one must have a 'spatial:type' attribute with the value 'cartesianZ'",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223801
  { SpatialMixedGeometryAllowedCoreAttributes,
    "Core attributes allowed on <mixedGeometry>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <mixedGeometry> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <mixedGeometry>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223802
  { SpatialMixedGeometryAllowedCoreElements,
    "Core elements allowed on <mixedGeometry>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <mixedGeometry> object may have the optional SBML Level 3 Core "
    "subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a <mixedGeometry>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223803
  { SpatialMixedGeometryAllowedElements,
    "Elements allowed on <mixedGeometry>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <mixedGeometry> object may contain one and only one instance of each of "
    "the <listOfGeometryDefinitions> and <listOfOrdinalMappings> elements. No "
    "other elements from the SBML Level 3 Spatial Processes namespaces are "
    "permitted on a <mixedGeometry> object. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223804
  { SpatialMixedGeometryLOGeometryDefinitionsAllowedCoreElements,
    "Core elements allowed on <listOfGeometryDefinitions>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Apart from the general notes and annotations subobjects permitted on all "
    "SBML objects, a <listOfGeometryDefinitions> container object may only "
    "contain <geometryDefinition> objects.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223805
  { SpatialMixedGeometryLOOrdinalMappingsAllowedCoreElements,
    "Core elements allowed on <listOfOrdinalMappings>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Apart from the general notes and annotations subobjects permitted on all "
    "SBML objects, a <listOfOrdinalMappings> container object may only contain "
    "<ordinalMapping> objects.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223806
  { SpatialMixedGeometryLOGeometryDefinitionsAllowedCoreAttributes,
    "Core attributes allowed on <listOfGeometryDefinitions>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <listOfGeometryDefinitions> object may have the optional SBML Level 3 "
    "Core attributes 'metaid' and 'sboTerm'. No other attributes from the SBML "
    "Level 3 Core namespaces are permitted on a <listOfGeometryDefinitions> "
    "object.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223807
  { SpatialMixedGeometryLOOrdinalMappingsAllowedCoreAttributes,
    "Core attributes allowed on <listOfOrdinalMappings>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <listOfOrdinalMappings> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <listOfOrdinalMappings> object.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223850
  { SpatialMixedGeometryChildrenNotActive,
    "Mixed geometry children are not active.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Every <geometryDefinition> child of a <mixedGeometry> must have a 'spatial:isActive' attribute value of 'false'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223901
  { SpatialOrdinalMappingAllowedCoreAttributes,
    "Core attributes allowed on <ordinalMapping>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An <ordinalMapping> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on an <ordinalMapping>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223902
  { SpatialOrdinalMappingAllowedCoreElements,
    "Core elements allowed on <ordinalMapping>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An <ordinalMapping> object may have the optional SBML Level 3 Core "
    "subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on an <ordinalMapping>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223903
  { SpatialOrdinalMappingAllowedAttributes,
    "Attributes allowed on <ordinalMapping>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An <ordinalMapping> object must have the required attributes "
    "'spatial:geometryDefinition' and 'spatial:ordinal'. No other attributes "
    "from the SBML Level 3 Spatial Processes namespaces are permitted on an "
    "<ordinalMapping> object. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223904
  { SpatialOrdinalMappingGeometryDefinitionMustBeGeometryDefinition,
    "The attribute 'geometryDefinition' must point to GeometryDefinition "
      "object.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'spatial:geometryDefinition' of an "
    "<ordinalMapping> object must be the identifier of an existing "
    "<geometryDefinition> object defined in parent <mixedGeometry> object.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223905
  { SpatialOrdinalMappingOrdinalMustBeInteger,
    "The 'ordinal' attribute must be Integer.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:ordinal' on an <ordinalMapping> must have a value "
    "of data type 'integer'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223950
  { SpatialOrdinalMappingOrdinalShouldBeUnique,
    "The 'ordinal' attribute should be unique.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_WARNING,
    "No <ordinalMapping> should have a 'spatial:ordinal' attribute with the same value as a different <ordinalMapping> child of the same <mixedGeometry>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1224001
  { SpatialSpatialPointsAllowedCoreAttributes,
    "Core attributes allowed on <spatialPoints>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <spatialPoints> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <spatialPoints>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1224002
  { SpatialSpatialPointsAllowedCoreElements,
    "Core elements allowed on <spatialPoints>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <spatialPoints> object may have the optional SBML Level 3 Core "
    "subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a <spatialPoints>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1224003
  { SpatialSpatialPointsAllowedAttributes,
    "Attributes allowed on <spatialPoints>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <spatialPoints> object must have the required attributes "
    "'spatial:compression', 'spatial:arrayData' and 'spatial:arrayDataLength', "
    "and may have the optional attributes 'spatial:id', 'spatial:name' and "
    "'spatial:dataType'. No other attributes from the SBML Level 3 Spatial "
    "Processes namespaces are permitted on a <spatialPoints> object. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1224004
  { SpatialSpatialPointsCompressionMustBeCompressionKindEnum,
    "The 'compression' attribute must be CompressionKindEnum.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'spatial:compression' of a <spatialPoints> "
    "object must conform to the syntax of SBML data type 'CompressionKind' and "
    "may only take on the allowed values of 'CompressionKind' defined in SBML; "
    "that is, the value must be one of the following: 'uncompressed' or "
    "'deflated'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1224005
  { SpatialSpatialPointsUncompressedArrayDataMustBeDouble,
    "The uncompressed array data of a <spatialPoints> must be doubles.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the children of a <spatialPoints> object "
    "must be an array of values of type 'double'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1224006
  { SpatialSpatialPointsArrayDataLengthMustBeInteger,
    "The 'arrayDataLength' attribute must be Integer.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:arrayDataLength' on a <spatialPoints> must have a "
    "value of data type 'integer'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1224007
  { SpatialSpatialPointsNameMustBeString,
    "The 'name' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:name' on a <spatialPoints> must have a value of "
    "data type 'string'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1224008
  { SpatialSpatialPointsDataTypeMustBeDataKindEnum,
    "The 'dataType' attribute must be DataKindEnum.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'spatial:dataType' of a <spatialPoints> object "
    "must conform to the syntax of SBML data type 'DataKind' and may only take "
    "on the allowed values of 'DataKind' defined in SBML; that is, the value "
    "must be one of the following: 'uint', 'int', 'double', 'float', 'uint8', 'uint16' or "
    "'uint32'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1224050
  { SpatialSpatialPointsDataLengthMustMatchUncompressed,
    "The 'arrayDataLength' attribute must match the entries when uncompressed.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "If the 'spatial:compression' attribute of a <spatialPoints> has the value 'uncompressed', the 'spatial:arrayDataLength' attribute of the same <spatialPoints> must equal the number of entries in the ArrayData child of the <spatialPoints>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1224051
  { SpatialSpatialPointsDataLengthMustMatchCompressed,
    "The 'arrayDataLength' attribute must match the compressed string.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "If the 'spatial:compression' attribute of a SpatialPoints has the value 'deflated', the 'spatial:arrayDataLength' attribute of the same SpatialPoints must equal the number of entries of the ArrayData child of the SpatialPoints.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1224052
  { SpatialSpatialPointsArrayDataMultipleOfDimensions,
    "The <spatialPoints> entries must match the dimensionality of the <geometry>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "When uncompressed, the number of entries in the ArrayData child of a <spatialPoints> must be evenly divisible by the number of <coordinateComponent> children of the <geometry> of the model (two or three).",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1224053
  { SpatialSpatialPointsFloatArrayDataMustMatch,
    "The <spatialPoints> entries in 'float' must be single precision.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "If the 'spatial:dataType' attribute of a <spatialPoints> has the value 'float', none of the uncompressed entries in the ArrayData child of the <spatialPoints> may have a value outside of the range of an IEEE 754-1985 single-precision floating point value (approximately +/-3.4028235e38, and +/-1.17549e-38).",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1224054
  { SpatialSpatialPointsUIntArrayDataNotNegative,
    "The <spatialPoints> entries in 'uint' must be positive.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "If the 'spatial:dataType' attribute of a <spatialPoints> has the value 'uint', none of the uncompressed entries in the ArrayData child of the <spatialPoints> may be negative.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1224055
  { SpatialSpatialPointsIntArrayDataIntegers,
    "The <spatialPoints> entries in 'uint' and 'int' must be integers.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "If the 'spatial:dataType' attribute of a <spatialPoints> has the value 'uint' or 'int', all of the uncompressed entries in the ArrayData child of the <spatialPoints> must be integers.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1224056
  { SpatialSpatialPointsCompressedArrayDataMustBeInts,
    "The compressed array data of a <spatialPoints> must be integers.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the children of a <spatialPoints> object "
    "must be an array of values of type 'integer' if the 'spatial:compression' attribute has the value 'deflated'.",
    { "L3V1 Spatial V1 Section"
    }
  },

};

/** @endcond */



LIBSBML_CPP_NAMESPACE_END




#endif /* !SpatialSBMLErrorTable_H__ */


