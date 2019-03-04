/**
 * @file SpatialSBMLErrorTable.h
 * @brief Definition of the SpatialSBMLErrorTable class.
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

  // 1220501
  { SpatialParameterAllowedElements,
    "Elements allowed on <parameter>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <parameter> object may contain one and only one instance of each of the "
    "<spatialSymbolReference,> <advectionCoefficient,> <boundaryCondition> and "
    "<diffusionCoefficient> elements. No other elements from the SBML Level 3 "
    "Spatial Processes namespaces are permitted on a <parameter> object. ",
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
    "'spatial:coordOne', and may have the optional attributes "
    "'spatial:coordTwo' and 'spatial:coordThree'. No other attributes from the "
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
    "The attribute 'spatial:coordOne' on an <interiorPoint> must have a value "
    "of data type 'double'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1220905
  { SpatialInteriorPointCoord2MustBeDouble,
    "The 'coord2' attribute must be Double.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:coordTwo' on an <interiorPoint> must have a value "
    "of data type 'double'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1220906
  { SpatialInteriorPointCoord3MustBeDouble,
    "The 'coord3' attribute must be Double.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:coordThree' on an <interiorPoint> must have a value "
    "of data type 'double'.",
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
    "'spatial:id', 'spatial:domainOne' and 'spatial:domainTwo', and may have "
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
    "The value of the attribute 'spatial:domainOne' of an <adjacentDomains> "
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
    "The value of the attribute 'spatial:domainTwo' of an <adjacentDomains> "
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
    "object in the enclosing <model,> or one of the base units in SBML.",
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
    "The attribute 'sampledField' must point to SampledField object.",
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
    "'spatial:dataType', 'spatial:numSamplesOne', 'spatial:interpolationType', "
    "'spatial:compression', 'spatial:samples' and 'spatial:samplesLength', and "
    "may have the optional attributes 'spatial:name', 'spatial:numSamplesTwo' "
    "and 'spatial:numSamplesThree'. No other attributes from the SBML Level 3 "
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
    "must be one of the following: 'double', 'float', 'uint8', 'uint16' or "
    "'uint32'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221605
  { SpatialSampledFieldNumSamples1MustBeInteger,
    "The 'numSamples1' attribute must be Integer.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:numSamplesOne' on a <sampledField> must have a "
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
  { SpatialSampledFieldSamplesMustBeString,
    "The 'samples' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'spatial:samples' of a <sampledField> object "
    "must be an array of values of type 'int'.",
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
    "The attribute 'spatial:numSamplesTwo' on a <sampledField> must have a "
    "value of data type 'integer'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1221612
  { SpatialSampledFieldNumSamples3MustBeInteger,
    "The 'numSamples3' attribute must be Integer.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:numSamplesThree' on a <sampledField> must have a "
    "value of data type 'integer'.",
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
    "only take on the allowed values of 'FunctionKind' defined in SBML; that "
    "is, the value must be one of the following: 'layered'.",
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
    "each of the SpatialPoints and <listOfParametricObjects> elements. No other "
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
    "the value must be one of the following: 'triangle' or 'quadrilateral'.",
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
    "object must be an array of values of type 'int'.",
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
    "only take on the allowed values of 'DataKind' defined in SBML; that is, "
    "the value must be one of the following: 'double', 'float', 'uint8', "
    "'uint16' or 'uint32'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222201
  { SpatialCSGeometryAllowedCoreAttributes,
    "Core attributes allowed on <csGeometry>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <cSGeometry> object may have the optional SBML Level 3 Core attributes "
    "'metaid' and 'sboTerm'. No other attributes from the SBML Level 3 Core "
    "namespaces are permitted on a <cSGeometry>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222202
  { SpatialCSGeometryAllowedCoreElements,
    "Core elements allowed on <csGeometry>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <cSGeometry> object may have the optional SBML Level 3 Core subobjects "
    "for notes and annotations. No other elements from the SBML Level 3 Core "
    "namespaces are permitted on a <cSGeometry>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222203
  { SpatialCSGeometryAllowedElements,
    "Elements allowed on <csGeometry>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <cSGeometry> object may contain one and only one instance of the "
    "<listOfCSGObjects> element. No other elements from the SBML Level 3 "
    "Spatial Processes namespaces are permitted on a <cSGeometry> object. ",
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
    "<cSGObject> objects.",
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
    "A <cSGObject> object may have the optional SBML Level 3 Core attributes "
    "'metaid' and 'sboTerm'. No other attributes from the SBML Level 3 Core "
    "namespaces are permitted on a <cSGObject>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222302
  { SpatialCSGObjectAllowedCoreElements,
    "Core elements allowed on <csgObject>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <cSGObject> object may have the optional SBML Level 3 Core subobjects "
    "for notes and annotations. No other elements from the SBML Level 3 Core "
    "namespaces are permitted on a <cSGObject>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222303
  { SpatialCSGObjectAllowedAttributes,
    "Attributes allowed on <csgObject>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <cSGObject> object must have the required attributes 'spatial:id' and "
    "'spatial:domainType', and may have the optional attributes 'spatial:name' "
    "and 'spatial:ordinal'. No other attributes from the SBML Level 3 Spatial "
    "Processes namespaces are permitted on a <cSGObject> object. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222304
  { SpatialCSGObjectAllowedElements,
    "Elements allowed on <csgObject>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <cSGObject> object must contain one and only one instance of the CSGNode "
    "element. No other elements from the SBML Level 3 Spatial Processes "
    "namespaces are permitted on a <cSGObject> object. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222305
  { SpatialCSGObjectDomainTypeMustBeDomainType,
    "The attribute 'domainType' must point to DomainType object.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'spatial:domainType' of a <cSGObject> object "
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
    "The attribute 'spatial:name' on a <cSGObject> must have a value of data "
    "type 'string'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222307
  { SpatialCSGObjectOrdinalMustBeInteger,
    "The 'ordinal' attribute must be Integer.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:ordinal' on a <cSGObject> must have a value of data "
    "type 'integer'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222401
  { SpatialCSGNodeAllowedCoreAttributes,
    "Core attributes allowed on <csgNode>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <cSGNode> object may have the optional SBML Level 3 Core attributes "
    "'metaid' and 'sboTerm'. No other attributes from the SBML Level 3 Core "
    "namespaces are permitted on a <cSGNode>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222402
  { SpatialCSGNodeAllowedCoreElements,
    "Core elements allowed on <csgNode>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <cSGNode> object may have the optional SBML Level 3 Core subobjects for "
    "notes and annotations. No other elements from the SBML Level 3 Core "
    "namespaces are permitted on a <cSGNode>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222403
  { SpatialCSGNodeAllowedAttributes,
    "Attributes allowed on <csgNode>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <cSGNode> object may have the optional attributes 'spatial:id' and "
    "'spatial:name'. No other attributes from the SBML Level 3 Spatial "
    "Processes namespaces are permitted on a <cSGNode> object. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222404
  { SpatialCSGNodeNameMustBeString,
    "The 'name' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:name' on a <cSGNode> must have a value of data type "
    "'string'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222501
  { SpatialCSGTransformationAllowedCoreAttributes,
    "Core attributes allowed on <csgTransformation>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <cSGTransformation> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <cSGTransformation>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222502
  { SpatialCSGTransformationAllowedCoreElements,
    "Core elements allowed on <csgTransformation>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <cSGTransformation> object may have the optional SBML Level 3 Core "
    "subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a <cSGTransformation>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222503
  { SpatialCSGTransformationAllowedElements,
    "Elements allowed on <csgTransformation>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <cSGTransformation> object may contain one and only one instance of the "
    "CSGNode element. No other elements from the SBML Level 3 Spatial Processes "
    "namespaces are permitted on a <cSGTransformation> object. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222601
  { SpatialCSGTranslationAllowedCoreAttributes,
    "Core attributes allowed on <csgTranslation>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <cSGTranslation> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <cSGTranslation>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222602
  { SpatialCSGTranslationAllowedCoreElements,
    "Core elements allowed on <csgTranslation>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <cSGTranslation> object may have the optional SBML Level 3 Core "
    "subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a <cSGTranslation>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222603
  { SpatialCSGTranslationAllowedAttributes,
    "Attributes allowed on <csgTranslation>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <cSGTranslation> object must have the required attribute "
    "'spatial:translateX', and may have the optional attributes "
    "'spatial:translateY' and 'spatial:translateZ'. No other attributes from "
    "the SBML Level 3 Spatial Processes namespaces are permitted on a "
    "<cSGTranslation> object. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222604
  { SpatialCSGTranslationTranslateXMustBeDouble,
    "The 'translateX' attribute must be Double.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:translateX' on a <cSGTranslation> must have a value "
    "of data type 'double'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222605
  { SpatialCSGTranslationTranslateYMustBeDouble,
    "The 'translateY' attribute must be Double.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:translateY' on a <cSGTranslation> must have a value "
    "of data type 'double'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222606
  { SpatialCSGTranslationTranslateZMustBeDouble,
    "The 'translateZ' attribute must be Double.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:translateZ' on a <cSGTranslation> must have a value "
    "of data type 'double'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222701
  { SpatialCSGRotationAllowedCoreAttributes,
    "Core attributes allowed on <csgRotation>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <cSGRotation> object may have the optional SBML Level 3 Core attributes "
    "'metaid' and 'sboTerm'. No other attributes from the SBML Level 3 Core "
    "namespaces are permitted on a <cSGRotation>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222702
  { SpatialCSGRotationAllowedCoreElements,
    "Core elements allowed on <csgRotation>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <cSGRotation> object may have the optional SBML Level 3 Core subobjects "
    "for notes and annotations. No other elements from the SBML Level 3 Core "
    "namespaces are permitted on a <cSGRotation>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222703
  { SpatialCSGRotationAllowedAttributes,
    "Attributes allowed on <csgRotation>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <cSGRotation> object must have the required attributes 'spatial:rotateX' "
    "and 'spatial:rotateAngleInRadians', and may have the optional attributes "
    "'spatial:rotateY' and 'spatial:rotateZ'. No other attributes from the SBML "
    "Level 3 Spatial Processes namespaces are permitted on a <cSGRotation> "
    "object. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222704
  { SpatialCSGRotationRotateXMustBeDouble,
    "The 'rotateX' attribute must be Double.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:rotateX' on a <cSGRotation> must have a value of "
    "data type 'double'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222705
  { SpatialCSGRotationRotateAngleInRadiansMustBeDouble,
    "The 'rotateAngleInRadians' attribute must be Double.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:rotateAngleInRadians' on a <cSGRotation> must have "
    "a value of data type 'double'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222706
  { SpatialCSGRotationRotateYMustBeDouble,
    "The 'rotateY' attribute must be Double.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:rotateY' on a <cSGRotation> must have a value of "
    "data type 'double'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222707
  { SpatialCSGRotationRotateZMustBeDouble,
    "The 'rotateZ' attribute must be Double.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:rotateZ' on a <cSGRotation> must have a value of "
    "data type 'double'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222801
  { SpatialCSGScaleAllowedCoreAttributes,
    "Core attributes allowed on <csgScale>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <cSGScale> object may have the optional SBML Level 3 Core attributes "
    "'metaid' and 'sboTerm'. No other attributes from the SBML Level 3 Core "
    "namespaces are permitted on a <cSGScale>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222802
  { SpatialCSGScaleAllowedCoreElements,
    "Core elements allowed on <csgScale>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <cSGScale> object may have the optional SBML Level 3 Core subobjects for "
    "notes and annotations. No other elements from the SBML Level 3 Core "
    "namespaces are permitted on a <cSGScale>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222803
  { SpatialCSGScaleAllowedAttributes,
    "Attributes allowed on <csgScale>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <cSGScale> object must have the required attribute 'spatial:scaleX', and "
    "may have the optional attributes 'spatial:scaleY' and 'spatial:scaleZ'. No "
    "other attributes from the SBML Level 3 Spatial Processes namespaces are "
    "permitted on a <cSGScale> object. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222804
  { SpatialCSGScaleScaleXMustBeDouble,
    "The 'scaleX' attribute must be Double.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:scaleX' on a <cSGScale> must have a value of data "
    "type 'double'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222805
  { SpatialCSGScaleScaleYMustBeDouble,
    "The 'scaleY' attribute must be Double.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:scaleY' on a <cSGScale> must have a value of data "
    "type 'double'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222806
  { SpatialCSGScaleScaleZMustBeDouble,
    "The 'scaleZ' attribute must be Double.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'spatial:scaleZ' on a <cSGScale> must have a value of data "
    "type 'double'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222901
  { SpatialCSGHomogeneousTransformationAllowedCoreAttributes,
    "Core attributes allowed on <csgHomogeneousTransformation>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <cSGHomogeneousTransformation> object may have the optional SBML Level 3 "
    "Core attributes 'metaid' and 'sboTerm'. No other attributes from the SBML "
    "Level 3 Core namespaces are permitted on a "
    "<cSGHomogeneousTransformation>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222902
  { SpatialCSGHomogeneousTransformationAllowedCoreElements,
    "Core elements allowed on <csgHomogeneousTransformation>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <cSGHomogeneousTransformation> object may have the optional SBML Level 3 "
    "Core subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a "
    "<cSGHomogeneousTransformation>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1222903
  { SpatialCSGHomogeneousTransformationAllowedElements,
    "Elements allowed on <csgHomogeneousTransformation>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <cSGHomogeneousTransformation> object must contain one and only one "
    "instance of the TransformationComponent element. No other elements from "
    "the SBML Level 3 Spatial Processes namespaces are permitted on a "
    "<cSGHomogeneousTransformation> object. ",
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
  { SpatialTransformationComponentComponentsMustBeString,
    "The 'components' attribute must be String.",
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

  // 1223101
  { SpatialCSGPrimitiveAllowedCoreAttributes,
    "Core attributes allowed on <csgPrimitive>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <cSGPrimitive> object may have the optional SBML Level 3 Core attributes "
    "'metaid' and 'sboTerm'. No other attributes from the SBML Level 3 Core "
    "namespaces are permitted on a <cSGPrimitive>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223102
  { SpatialCSGPrimitiveAllowedCoreElements,
    "Core elements allowed on <csgPrimitive>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <cSGPrimitive> object may have the optional SBML Level 3 Core subobjects "
    "for notes and annotations. No other elements from the SBML Level 3 Core "
    "namespaces are permitted on a <cSGPrimitive>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223103
  { SpatialCSGPrimitiveAllowedAttributes,
    "Attributes allowed on <csgPrimitive>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <cSGPrimitive> object must have the required attribute "
    "'spatial:primitiveType'. No other attributes from the SBML Level 3 Spatial "
    "Processes namespaces are permitted on a <cSGPrimitive> object. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223104
  { SpatialCSGPrimitivePrimitiveTypeMustBePrimitiveKindEnum,
    "The 'primitiveType' attribute must be PrimitiveKindEnum.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'spatial:primitiveType' of a <cSGPrimitive> "
    "object must conform to the syntax of SBML data type 'PrimitiveKind' and "
    "may only take on the allowed values of 'PrimitiveKind' defined in SBML; "
    "that is, the value must be one of the following: 'sphere', 'cube', "
    "'cylinder', 'cone', 'circle' or 'square'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223201
  { SpatialCSGSetOperatorAllowedCoreAttributes,
    "Core attributes allowed on <csgSetOperator>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <cSGSetOperator> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <cSGSetOperator>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223202
  { SpatialCSGSetOperatorAllowedCoreElements,
    "Core elements allowed on <csgSetOperator>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <cSGSetOperator> object may have the optional SBML Level 3 Core "
    "subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a <cSGSetOperator>.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223203
  { SpatialCSGSetOperatorAllowedAttributes,
    "Attributes allowed on <csgSetOperator>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <cSGSetOperator> object must have the required attribute "
    "'spatial:operationType', and may have the optional attributes "
    "'spatial:complementA' and 'spatial:complementB'. No other attributes from "
    "the SBML Level 3 Spatial Processes namespaces are permitted on a "
    "<cSGSetOperator> object. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223204
  { SpatialCSGSetOperatorAllowedElements,
    "Elements allowed on <csgSetOperator>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <cSGSetOperator> object may contain one and only one instance of the "
    "<listOfCSGNodes> element. No other elements from the SBML Level 3 Spatial "
    "Processes namespaces are permitted on a <cSGSetOperator> object. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223205
  { SpatialCSGSetOperatorOperationTypeMustBeSetOperationEnum,
    "The 'operationType' attribute must be SetOperationEnum.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'spatial:operationType' of a <cSGSetOperator> "
    "object must conform to the syntax of SBML data type 'SetOperation' and may "
    "only take on the allowed values of 'SetOperation' defined in SBML; that "
    "is, the value must be one of the following: 'union', 'intersection' or "
    "'difference'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223206
  { SpatialCSGSetOperatorComplementAMustBeCSGNode,
    "The attribute 'complementA' must point to CSGNode object.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'spatial:complementA' of a <cSGSetOperator> "
    "object must be the identifier of an existing <cSGNode> object defined in "
    "the enclosing <model> object.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223207
  { SpatialCSGSetOperatorComplementBMustBeCSGNode,
    "The attribute 'complementB' must point to CSGNode object.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'spatial:complementB' of a <cSGSetOperator> "
    "object must be the identifier of an existing <cSGNode> object defined in "
    "the enclosing <model> object.",
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
    "<cSGNode> objects.",
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

  // 1223301
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
  { SpatialSpatialSymbolReferenceSpatialRefMustBeGeometry,
    "The attribute 'spatialRef' must point to Geometry object.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'spatial:spatialRef' of a "
    "<spatialSymbolReference> object must be the identifier of an existing "
    "<geometry> object defined in the enclosing <model> object.",
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
    "attributes 'spatial:coordinateReferenceOne' and "
    "'spatial:coordinateReferenceTwo'. No other attributes from the SBML Level "
    "3 Spatial Processes namespaces are permitted on a <diffusionCoefficient> "
    "object. ",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223404
  { SpatialDiffusionCoefficientVariableMustBeSpecies,
    "The attribute 'variable' must point to Species object.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'spatial:variable' of a <diffusionCoefficient> "
    "object must be the identifier of an existing <species> object defined in "
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
    "The value of the attribute 'spatial:coordinateReferenceOne' of a "
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
    "The value of the attribute 'spatial:coordinateReferenceTwo' of a "
    "<diffusionCoefficient> object must conform to the syntax of SBML data type "
    "'CoordinateKind' and may only take on the allowed values of "
    "'CoordinateKind' defined in SBML; that is, the value must be one of the "
    "following: 'cartesianX', 'cartesianY' or 'cartesianZ'.",
    { "L3V1 Spatial V1 Section"
    }
  },

  // 1223501
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
  { SpatialAdvectionCoefficientVariableMustBeSpecies,
    "The attribute 'variable' must point to Species object.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'spatial:variable' of an <advectionCoefficient> "
    "object must be the identifier of an existing <species> object defined in "
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
    "value must be one of the following: 'Robin_valueCoefficient', "
    "'Robin_inwardNormalGradientCoefficient', 'Robin_sum', 'Neumann' or "
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
    "only take on the allowed values of 'GeometryKind' defined in SBML; that "
    "is, the value must be one of the following: 'cartesian'.",
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
    "<geometryDefinition> object defined in the enclosing <model> object.",
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
  { SpatialSpatialPointsArrayDataMustBeString,
    "The 'arrayData' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'spatial:arrayData' of a <spatialPoints> object "
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
    "must be one of the following: 'double', 'float', 'uint8', 'uint16' or "
    "'uint32'.",
    { "L3V1 Spatial V1 Section"
    }
  },

};

/** @endcond */



LIBSBML_CPP_NAMESPACE_END




#endif /* !SpatialSBMLErrorTable_H__ */


