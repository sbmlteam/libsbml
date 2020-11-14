/**
 * @file RenderSBMLErrorTable.h
 * @brief Definition of the RenderSBMLErrorTable class.
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


#ifndef RenderSBMLErrorTable_H__
#define RenderSBMLErrorTable_H__


#include <sbml/packages/render/validator/RenderSBMLError.h>


LIBSBML_CPP_NAMESPACE_BEGIN



/** @cond doxygenLibsbmlInternal */

static const packageErrorTableEntry renderErrorTable[] =
{
  // 1310100
  { RenderUnknown,
    "Unknown error from Render",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Unknown error from Render",
    { ""
    }
  },

  // 1310101
  { RenderNSUndeclared,
    "The Render namespace is not correctly declared.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "To conform to the Render Package specification for SBML Level 3 Version 1, "
    "an SBML document must declare "
    "'http://www.sbml.org/sbml/level3/version1/render/version1' as the "
    "XMLNamespace to use for elements of this package.",
    { "L3V1 Render V1 Section 3.1"
    }
  },

  // 1310102
  { RenderElementNotInNs,
    "Element not in Render namespace",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Wherever they appear in an SBML document, elements and attributes from the "
    "Render Package must use the "
    "'http://www.sbml.org/sbml/level3/version1/render/version1' namespace, "
    "declaring so either explicitly or implicitly.",
    { "L3V1 Render V1 Section 3.1"
    }
  },

  // 1310301
  { RenderDuplicateComponentId,
    "Duplicate 'id' attribute value",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "(Extends validation rule #10301 in the SBML Level 3 Core specification. TO "
    "DO list scope of ids)",
    { "L3V1 Render V1 Section"
    }
  },

  // 1310302
  { RenderIdSyntaxRule,
    "Invalid SId syntax",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of a 'render:id' must conform to the syntax of the <sbml> data "
    "type 'SId'",
    { "L3V1 Render V1 Section"
    }
  },

  // 1320101
  { RenderAttributeRequiredMissing,
    "Required render:required attribute on <sbml>",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "In all SBML documents using the Render Package, the <sbml> object must "
    "have the 'render:required' attribute.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1320102
  { RenderAttributeRequiredMustBeBoolean,
    "The render:required attribute must be Boolean",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of attribute 'render:required' on the <sbml> object must be of "
    "data type 'boolean'.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1320103
  { RenderAttributeRequiredMustHaveValue,
    "The render:required attribute must be 'false'",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of attribute 'render:required' on the <sbml> object must be set "
    "to 'false'.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1320201
  { RenderGraphicalObjectAllowedAttributes,
    "Attributes allowed on <graphicalObject>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <graphicalObject> object may have the optional attribute "
    "'render:objectRole'. No other attributes from the SBML Level 3 Render "
    "namespaces are permitted on a <graphicalObject> object. ",
    { "L3V1 Render V1 Section"
    }
  },

  // 1320202
  { RenderGraphicalObjectObjectRoleMustBeString,
    "ObjectRole attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'render:objectRole' on a <graphicalObject> must have a value "
    "of data type 'string'.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1320301
  { RenderLayoutAllowedElements,
    "Elements allowed on <layout>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <layout> object may contain one and only one instance of the "
    "<listOfLocalRenderInformation> element. No other elements from the SBML "
    "Level 3 Render namespaces are permitted on a <layout> object. ",
    { "L3V1 Render V1 Section"
    }
  },

  // 1320302
  { RenderLayoutEmptyLOElements,
    "No Empty ListOf elements allowed on <Layout>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The <listOfLocalRenderInformation> subobject on a <layout> object is "
    "optional, but if present, this container object must not be empty.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1320303
  { RenderLayoutLOLocalRenderInformationAllowedCoreElements,
    "Core elements allowed on <layout>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Apart from the general notes and annotations subobjects permitted on all "
    "SBML objects, a <listOfLocalRenderInformation> container object may only "
    "contain <localRenderInformation> objects.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1320304
  { RenderLayoutLOLocalRenderInformationAllowedCoreAttributes,
    "Core attributes allowed on <layout>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <listOfLocalRenderInformation> object may have the optional SBML Level 3 "
    "Core attributes 'metaid' and 'sboTerm'. No other attributes from the SBML "
    "Level 3 Core namespaces are permitted on a <listOfLocalRenderInformation> "
    "object.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1320305
  { RenderLayoutLOLocalRenderInformationAllowedAttributes,
    "Attributes allowed on <listOfLocalRenderInformation>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <listOfLocalRenderInformation> object may have the optional attributes "
    "'render:versionMajor', 'render:versionMinor' and 'render:defaultValues'. "
    "No other attributes from the SBML Level 3 Render namespaces are permitted "
    "on a <listOfLocalRenderInformation> object. ",
    { "L3V1 Render V1 Section"
    }
  },

  // 1320306
  { RenderLayoutVersionMajorMustBeNonNegativeInteger,
    "VersionMajor attribute must be NonNegativeInteger.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'render:versionMajor' on a <listOfLocalRenderInformation> must have a value of "
    "data type 'integer', and must be non negative.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1320307
  { RenderLayoutVersionMinorMustBeNonNegativeInteger,
    "VersionMinor attribute must be NonNegativeInteger.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'render:versionMinor' on a <listOfLocalRenderInformation> must have a value of "
    "data type 'integer', and must be non negative.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1320308
  { RenderLayoutDefaultValuesMustBeString,
    "DefaultValues attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "FIX ME: Encountered an unknown attribute type element in "
    "ValidationRulesForClass:write_attribute_type_rule",
    { "L3V1 Render V1 Section"
    }
  },

  // 1320401
  { RenderListOfLayoutsAllowedElements,
    "Elements allowed on <listOfLayouts>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <listOfLayouts> object may contain one and only one instance of the "
    "<listOfGlobalRenderInformation> element. No other elements from the SBML "
    "Level 3 Render namespaces are permitted on a <listOfLayouts> object. ",
    { "L3V1 Render V1 Section"
    }
  },

  // 1320402
  { RenderListOfLayoutsEmptyLOElements,
    "No Empty ListOf elements allowed on <ListOfLayouts>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The <listOfGlobalRenderInformation> subobject on a <listOfLayouts> object "
    "is optional, but if present, this container object must not be empty.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1320403
  { RenderListOfLayoutsLOGlobalRenderInformationAllowedCoreElements,
    "Core elements allowed on <listOfLayouts>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Apart from the general notes and annotations subobjects permitted on all "
    "SBML objects, a <listOfGlobalRenderInformation> container object may only "
    "contain <globalRenderInformation> objects.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1320404
  { RenderListOfLayoutsLOGlobalRenderInformationAllowedCoreAttributes,
    "Core attributes allowed on <listOfLayouts>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <listOfGlobalRenderInformation> object may have the optional SBML Level "
    "3 Core attributes 'metaid' and 'sboTerm'. No other attributes from the "
    "SBML Level 3 Core namespaces are permitted on a "
    "<listOfGlobalRenderInformation> object.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1320405
  { RenderListOfLayoutsLOGlobalRenderInformationAllowedAttributes,
    "Attributes allowed on <listOfGlobalRenderInformation>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <listOfGlobalRenderInformation> object may have the optional attributes "
    "'render:versionMajor', and 'render:versionMinor'. "
    "No other attributes from the SBML Level 3 Render namespaces are permitted "
    "on a <listOfGlobalRenderInformation> object. ",
    { "L3V1 Render V1 Section"
    }
  },

  // 1320406
  { RenderListOfLayoutsVersionMajorMustBeNonNegativeInteger,
    "VersionMajor attribute must be NonNegativeInteger.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'render:versionMajor' on a <listOfLayouts> must have a value "
    "of data type 'integer', and must be non negative.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1320407
  { RenderListOfLayoutsVersionMinorMustBeNonNegativeInteger,
    "VersionMinor attribute must be NonNegativeInteger.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'render:versionMinor' on a <listOfLayouts> must have a value "
    "of data type 'integer', and must be non negative.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1320408
  { RenderListOfLayoutsDefaultValuesMustBeString,
    "DefaultValues attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "FIX ME: Encountered an unknown attribute type element in "
    "ValidationRulesForClass:write_attribute_type_rule",
    { "L3V1 Render V1 Section"
    }
  },

  // 1320501
  { RenderColorDefinitionAllowedCoreAttributes,
    "Core attributes allowed on <colorDefinition>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <colorDefinition> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <colorDefinition>.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1320502
  { RenderColorDefinitionAllowedCoreElements,
    "Core elements allowed on <colorDefinition>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <colorDefinition> object may have the optional SBML Level 3 Core "
    "subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a <colorDefinition>.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1320503
  { RenderColorDefinitionAllowedAttributes,
    "Attributes allowed on <colorDefinition>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <colorDefinition> object must have the required attributes 'render:id' "
    "and 'render:value', and may have the optional attribute 'render:name'. No "
    "other attributes from the SBML Level 3 Render namespaces are permitted on "
    "a <colorDefinition> object. ",
    { "L3V1 Render V1 Section"
    }
  },

  // 1320504
  { RenderColorDefinitionValueMustBeString,
    "The 'value' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'render:value' on a <colorDefinition> must have a value of "
    "data type 'string'.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1320505
  { RenderColorDefinitionNameMustBeString,
    "The 'name' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'render:name' on a <colorDefinition> must have a value of "
    "data type 'string'.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1320601
  { RenderEllipseAllowedCoreAttributes,
    "Core attributes allowed on <ellipse>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An <ellipse> object may have the optional SBML Level 3 Core attributes "
    "'metaid' and 'sboTerm'. No other attributes from the SBML Level 3 Core "
    "namespaces are permitted on an <ellipse>.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1320602
  { RenderEllipseAllowedCoreElements,
    "Core elements allowed on <ellipse>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An <ellipse> object may have the optional SBML Level 3 Core subobjects for "
    "notes and annotations. No other elements from the SBML Level 3 Core "
    "namespaces are permitted on an <ellipse>.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1320603
  { RenderEllipseAllowedAttributes,
    "Attributes allowed on <ellipse>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An <ellipse> object must have the required attributes 'render:cx', "
    "'render:cy' and 'render:rx', and may have the optional attributes "
    "'render:ratio', 'render:cz' and 'render:ry'. No other attributes from the "
    "SBML Level 3 Render namespaces are permitted on an <ellipse> object. ",
    { "L3V1 Render V1 Section"
    }
  },

  // 1320604
  { RenderEllipseCxMustBeRelAbsVector,
    "The 'cx' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:cx' of an <ellipse> object must conform "
    "to the syntax of SBML data type <relAbsVector,> i.e., a string encoding "
    "optionally an absolute number followed by an optional relative number "
    "followed by a <% sign>. Adding spaces between the coordinates is "
    "encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1320605
  { RenderEllipseCyMustBeRelAbsVector,
    "The 'cy' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:cy' of an <ellipse> object must conform "
    "to the syntax of SBML data type <relAbsVector,> i.e., a string encoding "
    "optionally an absolute number followed by an optional relative number "
    "followed by a <% sign>. Adding spaces between the coordinates is "
    "encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1320606
  { RenderEllipseRxMustBeRelAbsVector,
    "The 'rx' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:rx' of an <ellipse> object must conform "
    "to the syntax of SBML data type <relAbsVector,> i.e., a string encoding "
    "optionally an absolute number followed by an optional relative number "
    "followed by a <% sign>. Adding spaces between the coordinates is "
    "encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1320607
  { RenderEllipseRatioMustBeDouble,
    "The 'ratio' attribute must be Double.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'render:ratio' on an <ellipse> must have a value of data "
    "type 'double'.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1320608
  { RenderEllipseCzMustBeRelAbsVector,
    "The 'cz' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:cz' of an <ellipse> object must conform "
    "to the syntax of SBML data type <relAbsVector,> i.e., a string encoding "
    "optionally an absolute number followed by an optional relative number "
    "followed by a <% sign>. Adding spaces between the coordinates is "
    "encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1320609
  { RenderEllipseRyMustBeRelAbsVector,
    "The 'ry' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:ry' of an <ellipse> object must conform "
    "to the syntax of SBML data type <relAbsVector,> i.e., a string encoding "
    "optionally an absolute number followed by an optional relative number "
    "followed by a <% sign>. Adding spaces between the coordinates is "
    "encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1320701
  { RenderGlobalRenderInformationAllowedCoreAttributes,
    "Core attributes allowed on <globalRenderInformation>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <globalRenderInformation> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <globalRenderInformation>.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1320702
  { RenderGlobalRenderInformationAllowedCoreElements,
    "Core elements allowed on <globalRenderInformation>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <globalRenderInformation> object may have the optional SBML Level 3 Core "
    "subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a <globalRenderInformation>.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1320703
  { RenderGlobalRenderInformationAllowedElements,
    "Elements allowed on <globalRenderInformation>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <globalRenderInformation> object may contain one and only one instance "
    "of the <listOfGlobalStyles> element. No other elements from the SBML Level "
    "3 Render namespaces are permitted on a <globalRenderInformation> object. ",
    { "L3V1 Render V1 Section"
    }
  },

  // 1320704
  { RenderGlobalRenderInformationEmptyLOElements,
    "No Empty ListOf elements allowed on <globalRenderInformation>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The <listOfGlobalStyles> subobject on a <globalRenderInformation> object "
    "is optional, but if present, this container object must not be empty.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1320705
  { RenderGlobalRenderInformationLOGlobalStylesAllowedCoreElements,
    "Core elements allowed on <listOfGlobalStyles>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Apart from the general notes and annotations subobjects permitted on all "
    "SBML objects, a <listOfGlobalStyles> container object may only contain "
    "<globalStyle> objects.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1320706
  { RenderGlobalRenderInformationLOGlobalStylesAllowedCoreAttributes,
    "Core attributes allowed on <listOfGlobalStyles>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <listOfGlobalStyles> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <listOfGlobalStyles> object.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1320801
  { RenderGlobalStyleAllowedCoreAttributes,
    "Core attributes allowed on <globalStyle>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <globalStyle> object may have the optional SBML Level 3 Core attributes "
    "'metaid' and 'sboTerm'. No other attributes from the SBML Level 3 Core "
    "namespaces are permitted on a <globalStyle>.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1320802
  { RenderGlobalStyleAllowedCoreElements,
    "Core elements allowed on <globalStyle>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <globalStyle> object may have the optional SBML Level 3 Core subobjects "
    "for notes and annotations. No other elements from the SBML Level 3 Core "
    "namespaces are permitted on a <globalStyle>.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1320901
  { RenderGradientBaseAllowedCoreAttributes,
    "Core attributes allowed on <gradientBase>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <gradientBase> object may have the optional SBML Level 3 Core attributes "
    "'metaid' and 'sboTerm'. No other attributes from the SBML Level 3 Core "
    "namespaces are permitted on a <gradientBase>.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1320902
  { RenderGradientBaseAllowedCoreElements,
    "Core elements allowed on <gradientBase>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <gradientBase> object may have the optional SBML Level 3 Core subobjects "
    "for notes and annotations. No other elements from the SBML Level 3 Core "
    "namespaces are permitted on a <gradientBase>.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1320903
  { RenderGradientBaseAllowedAttributes,
    "Attributes allowed on <gradientBase>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <gradientBase> object must have the required attribute 'render:id', and "
    "may have the optional attributes 'render:name' and 'render:spreadMethod'. "
    "No other attributes from the SBML Level 3 Render namespaces are permitted "
    "on a <gradientBase> object. ",
    { "L3V1 Render V1 Section"
    }
  },

  // 1320904
  { RenderGradientBaseAllowedElements,
    "Elements allowed on <gradientBase>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <gradientBase> object may contain one and only one instance of the "
    "<listOfGradientStops> element. No other elements from the SBML Level 3 "
    "Render namespaces are permitted on a <gradientBase> object. ",
    { "L3V1 Render V1 Section"
    }
  },

  // 1320905
  { RenderGradientBaseNameMustBeString,
    "The 'name' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'render:name' on a <gradientBase> must have a value of data "
    "type 'string'.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1320906
  { RenderGradientBaseSpreadMethodMustBeGradientSpreadMethodEnum,
    "The 'spreadMethod' attribute must be GradientSpreadMethod.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:spreadMethod' of a <gradientBase> "
    "object must conform to the syntax of SBML data type 'GradientSpreadMethod' "
    "and may only take on the allowed values of 'GradientSpreadMethod' defined "
    "in SBML; that is, the value must be one of the following: 'pad', 'reflect' "
    "or 'repeat'.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321001
  { RenderGradientStopAllowedCoreAttributes,
    "Core attributes allowed on <gradientStop>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <gradientStop> object may have the optional SBML Level 3 Core attributes "
    "'metaid' and 'sboTerm'. No other attributes from the SBML Level 3 Core "
    "namespaces are permitted on a <gradientStop>.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321002
  { RenderGradientStopAllowedCoreElements,
    "Core elements allowed on <gradientStop>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <gradientStop> object may have the optional SBML Level 3 Core subobjects "
    "for notes and annotations. No other elements from the SBML Level 3 Core "
    "namespaces are permitted on a <gradientStop>.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321003
  { RenderGradientStopAllowedAttributes,
    "Attributes allowed on <gradientStop>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <gradientStop> object must have the required attributes "
    "'render:stop-color' and 'render:offset'. No other attributes from the SBML "
    "Level 3 Render namespaces are permitted on a <gradientStop> object. ",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321004
  { RenderGradientStopStopColorMustBeString,
    "The 'stopColor' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'render:stop-color' on a <gradientStop> must have a value of "
    "data type 'string'.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321005
  { RenderGradientStopOffsetMustBeRelAbsVector,
    "The 'offset' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:offset' of a <gradientStop> object must "
    "conform to the syntax of SBML data type <relAbsVector,> i.e., a string "
    "encoding optionally an absolute number followed by an optional relative "
    "number followed by a <% sign>. Adding spaces between the coordinates is "
    "encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321101
  { RenderRenderGroupAllowedCoreAttributes,
    "Core attributes allowed on <renderGroup>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <renderGroup> object may have the optional SBML Level 3 Core attributes "
    "'metaid' and 'sboTerm'. No other attributes from the SBML Level 3 Core "
    "namespaces are permitted on a <renderGroup>.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321102
  { RenderRenderGroupAllowedCoreElements,
    "Core elements allowed on <renderGroup>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <renderGroup> object may have the optional SBML Level 3 Core subobjects "
    "for notes and annotations. No other elements from the SBML Level 3 Core "
    "namespaces are permitted on a <renderGroup>.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321103
  { RenderRenderGroupAllowedAttributes,
    "Attributes allowed on <renderGroup>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <renderGroup> object may have the optional attributes "
    "'render:startHead', 'render:endHead', 'render:font-family', "
    "'render:font-weight', 'render:font-style', 'render:text-anchor', "
    "'render:vtext-anchor' and 'render:font-size'. No other attributes from the "
    "SBML Level 3 Render namespaces are permitted on a <renderGroup> object. ",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321104
  { RenderRenderGroupAllowedElements,
    "Elements allowed on <renderGroup>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <renderGroup> object may contain one and only one instance of the "
    "<listOfElements> element. No other elements from the SBML Level 3 Render "
    "namespaces are permitted on a <renderGroup> object. ",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321105
  { RenderRenderGroupStartHeadMustBeLineEnding,
    "The attribute 'startHead' must point to LineEnding object.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:startHead' of a <renderGroup> object "
    "must be the identifier of an existing <lineEnding> object defined in the "
    "enclosing <model> object.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321106
  { RenderRenderGroupEndHeadMustBeLineEnding,
    "The attribute 'endHead' must point to LineEnding object.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:endHead' of a <renderGroup> object must "
    "be the identifier of an existing <lineEnding> object defined in the "
    "enclosing <model> object.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321107
  { RenderRenderGroupFontFamilyMustBeString,
    "The 'font-family' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'render:font-family' on a <renderGroup> must have a value of "
    "data type 'string'.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321108
  { RenderRenderGroupFontWeightMustBeFontWeightEnum,
    "The 'font-weight' attribute must be FontWeight.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:font-weight' of a <renderGroup> object "
    "must conform to the syntax of SBML data type 'FontWeight' and may only "
    "take on the allowed values of 'FontWeight' defined in SBML; that is, the "
    "value must be one of the following: 'bold' or 'normal'.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321109
  { RenderRenderGroupFontStyleMustBeFontStyleEnum,
    "The 'font-style' attribute must be FontStyle.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:font-style' of a <renderGroup> object "
    "must conform to the syntax of SBML data type 'FontStyle' and may only take "
    "on the allowed values of 'FontStyle' defined in SBML; that is, the value "
    "must be one of the following: 'italic' or 'normal'.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321110
  { RenderRenderGroupTextAnchorMustBeHTextAnchorEnum,
    "The 'text-anchor' attribute must be HTextAnchor.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:text-anchor' of a <renderGroup> object "
    "must conform to the syntax of SBML data type 'HTextAnchor' and may only "
    "take on the allowed values of 'HTextAnchor' defined in SBML; that is, the "
    "value must be one of the following: 'start', 'middle' or 'end'.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321111
  { RenderRenderGroupVtextAnchorMustBeVTextAnchorEnum,
    "The 'vtext-anchor' attribute must be VTextAnchor.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:vtext-anchor' of a <renderGroup> object "
    "must conform to the syntax of SBML data type 'VTextAnchor' and may only "
    "take on the allowed values of 'VTextAnchor' defined in SBML; that is, the "
    "value must be one of the following: 'top', 'middle', 'bottom' or "
    "'baseline'.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321112
  { RenderRenderGroupFontSizeMustBeRelAbsVector,
    "The 'font-size' attribute must be RelAbsVector.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:font-size' of a <renderGroup> object "
    "must conform to the syntax of SBML data type <relAbsVector,> i.e., a "
    "string encoding optionally an absolute number followed by an optional "
    "relative number followed by a <% sign>. Adding spaces between the "
    "coordinates is encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321201
  { RenderImageAllowedCoreAttributes,
    "Core attributes allowed on <image>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An <image> object may have the optional SBML Level 3 Core attributes "
    "'metaid' and 'sboTerm'. No other attributes from the SBML Level 3 Core "
    "namespaces are permitted on an <image>.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321202
  { RenderImageAllowedCoreElements,
    "Core elements allowed on <image>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An <image> object may have the optional SBML Level 3 Core subobjects for "
    "notes and annotations. No other elements from the SBML Level 3 Core "
    "namespaces are permitted on an <image>.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321203
  { RenderImageAllowedAttributes,
    "Attributes allowed on <image>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An <image> object must have the required attributes 'render:href', "
    "'render:x', 'render:y', 'render:width' and 'render:height', and may have "
    "the optional attributes 'render:id' and 'render:z'. No other attributes "
    "from the SBML Level 3 Render namespaces are permitted on an <image> "
    "object. ",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321204
  { RenderImageHrefMustBeString,
    "The 'href' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'render:href' on an <image> must have a value of data type "
    "'string'.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321205
  { RenderImageXMustBeRelAbsVector,
    "The 'x' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:x' of an <image> object must conform to "
    "the syntax of SBML data type <relAbsVector,> i.e., a string encoding "
    "optionally an absolute number followed by an optional relative number "
    "followed by a <% sign>. Adding spaces between the coordinates is "
    "encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321206
  { RenderImageYMustBeRelAbsVector,
    "The 'y' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:y' of an <image> object must conform to "
    "the syntax of SBML data type <relAbsVector,> i.e., a string encoding "
    "optionally an absolute number followed by an optional relative number "
    "followed by a <% sign>. Adding spaces between the coordinates is "
    "encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321207
  { RenderImageWidthMustBeRelAbsVector,
    "The 'width' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:width' of an <image> object must "
    "conform to the syntax of SBML data type <relAbsVector,> i.e., a string "
    "encoding optionally an absolute number followed by an optional relative "
    "number followed by a <% sign>. Adding spaces between the coordinates is "
    "encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321208
  { RenderImageHeightMustBeRelAbsVector,
    "The 'height' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:height' of an <image> object must "
    "conform to the syntax of SBML data type <relAbsVector,> i.e., a string "
    "encoding optionally an absolute number followed by an optional relative "
    "number followed by a <% sign>. Adding spaces between the coordinates is "
    "encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321209
  { RenderImageZMustBeRelAbsVector,
    "The 'z' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:z' of an <image> object must conform to "
    "the syntax of SBML data type <relAbsVector,> i.e., a string encoding "
    "optionally an absolute number followed by an optional relative number "
    "followed by a <% sign>. Adding spaces between the coordinates is "
    "encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321301
  { RenderLineEndingAllowedCoreAttributes,
    "Core attributes allowed on <lineEnding>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <lineEnding> object may have the optional SBML Level 3 Core attributes "
    "'metaid' and 'sboTerm'. No other attributes from the SBML Level 3 Core "
    "namespaces are permitted on a <lineEnding>.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321302
  { RenderLineEndingAllowedCoreElements,
    "Core elements allowed on <lineEnding>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <lineEnding> object may have the optional SBML Level 3 Core subobjects "
    "for notes and annotations. No other elements from the SBML Level 3 Core "
    "namespaces are permitted on a <lineEnding>.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321303
  { RenderLineEndingAllowedAttributes,
    "Attributes allowed on <lineEnding>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <lineEnding> object must have the required attribute 'render:id', and "
    "may have the optional attribute 'render:enableRotationalMapping'. No other "
    "attributes from the SBML Level 3 Render namespaces are permitted on a "
    "<lineEnding> object. ",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321304
  { RenderLineEndingAllowedElements,
    "Elements allowed on <lineEnding>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <lineEnding> object may contain one and only one instance of each of the "
    "RenderGroup and BoundingBox elements. No other elements from the SBML "
    "Level 3 Render namespaces are permitted on a <lineEnding> object. ",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321305
  { RenderLineEndingEnableRotationalMappingMustBeBoolean,
    "The 'enableRotationalMapping' attribute must be Boolean.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'render:enableRotationalMapping' on a <lineEnding> must have "
    "a value of data type 'boolean'.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321401
  { RenderLinearGradientAllowedCoreAttributes,
    "Core attributes allowed on <linearGradient>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <linearGradient> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <linearGradient>.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321402
  { RenderLinearGradientAllowedCoreElements,
    "Core elements allowed on <linearGradient>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <linearGradient> object may have the optional SBML Level 3 Core "
    "subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a <linearGradient>.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321403
  { RenderLinearGradientAllowedAttributes,
    "Attributes allowed on <linearGradient>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <linearGradient> object may have the optional attributes 'render:x1', "
    "'render:y1', 'render:z1', 'render:x2', 'render:y2' and 'render:z2'. No "
    "other attributes from the SBML Level 3 Render namespaces are permitted on "
    "a <linearGradient> object. ",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321404
  { RenderLinearGradientX1MustBeRelAbsVector,
    "The 'x1' attribute must be RelAbsVector.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:x1' of a <linearGradient> object must "
    "conform to the syntax of SBML data type <relAbsVector,> i.e., a string "
    "encoding optionally an absolute number followed by an optional relative "
    "number followed by a <% sign>. Adding spaces between the coordinates is "
    "encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321405
  { RenderLinearGradientY1MustBeRelAbsVector,
    "The 'y1' attribute must be RelAbsVector.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:y1' of a <linearGradient> object must "
    "conform to the syntax of SBML data type <relAbsVector,> i.e., a string "
    "encoding optionally an absolute number followed by an optional relative "
    "number followed by a <% sign>. Adding spaces between the coordinates is "
    "encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321406
  { RenderLinearGradientZ1MustBeRelAbsVector,
    "The 'z1' attribute must be RelAbsVector.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:z1' of a <linearGradient> object must "
    "conform to the syntax of SBML data type <relAbsVector,> i.e., a string "
    "encoding optionally an absolute number followed by an optional relative "
    "number followed by a <% sign>. Adding spaces between the coordinates is "
    "encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321407
  { RenderLinearGradientX2MustBeRelAbsVector,
    "The 'x2' attribute must be RelAbsVector.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:x2' of a <linearGradient> object must "
    "conform to the syntax of SBML data type <relAbsVector,> i.e., a string "
    "encoding optionally an absolute number followed by an optional relative "
    "number followed by a <% sign>. Adding spaces between the coordinates is "
    "encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321408
  { RenderLinearGradientY2MustBeRelAbsVector,
    "The 'y2' attribute must be RelAbsVector.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:y2' of a <linearGradient> object must "
    "conform to the syntax of SBML data type <relAbsVector,> i.e., a string "
    "encoding optionally an absolute number followed by an optional relative "
    "number followed by a <% sign>. Adding spaces between the coordinates is "
    "encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321409
  { RenderLinearGradientZ2MustBeRelAbsVector,
    "The 'z2' attribute must be RelAbsVector.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:z2' of a <linearGradient> object must "
    "conform to the syntax of SBML data type <relAbsVector,> i.e., a string "
    "encoding optionally an absolute number followed by an optional relative "
    "number followed by a <% sign>. Adding spaces between the coordinates is "
    "encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321501
  { RenderLocalRenderInformationAllowedCoreAttributes,
    "Core attributes allowed on <localRenderInformation>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <localRenderInformation> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <localRenderInformation>.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321502
  { RenderLocalRenderInformationAllowedCoreElements,
    "Core elements allowed on <localRenderInformation>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <localRenderInformation> object may have the optional SBML Level 3 Core "
    "subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a <localRenderInformation>.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321503
  { RenderLocalRenderInformationAllowedElements,
    "Elements allowed on <localRenderInformation>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <localRenderInformation> object may contain one and only one instance of "
    "the <listOfLocalStyles> element. No other elements from the SBML Level 3 "
    "Render namespaces are permitted on a <localRenderInformation> object. ",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321504
  { RenderLocalRenderInformationEmptyLOElements,
    "No Empty ListOf elements allowed on <localRenderInformation>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The <listOfLocalStyles> subobject on a <localRenderInformation> object is "
    "optional, but if present, this container object must not be empty.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321505
  { RenderLocalRenderInformationLOLocalStylesAllowedCoreElements,
    "Core elements allowed on <listOfLocalStyles>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Apart from the general notes and annotations subobjects permitted on all "
    "SBML objects, a <listOfLocalStyles> container object may only contain "
    "<localStyle> objects.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321506
  { RenderLocalRenderInformationLOLocalStylesAllowedCoreAttributes,
    "Core attributes allowed on <listOfLocalStyles>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <listOfLocalStyles> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <listOfLocalStyles> object.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321601
  { RenderLocalStyleAllowedCoreAttributes,
    "Core attributes allowed on <localStyle>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <localStyle> object may have the optional SBML Level 3 Core attributes "
    "'metaid' and 'sboTerm'. No other attributes from the SBML Level 3 Core "
    "namespaces are permitted on a <localStyle>.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321602
  { RenderLocalStyleAllowedCoreElements,
    "Core elements allowed on <localStyle>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <localStyle> object may have the optional SBML Level 3 Core subobjects "
    "for notes and annotations. No other elements from the SBML Level 3 Core "
    "namespaces are permitted on a <localStyle>.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321603
  { RenderLocalStyleAllowedAttributes,
    "Attributes allowed on <localStyle>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <localStyle> object may have the optional attribute 'render:idList'. No "
    "other attributes from the SBML Level 3 Render namespaces are permitted on "
    "a <localStyle> object. ",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321604
  { RenderLocalStyleIdListMustBeString,
    "The 'idList' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'render:idList' on a <localStyle> must have a value of data "
    "type 'string'.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321701
  { RenderPolygonAllowedCoreAttributes,
    "Core attributes allowed on <polygon>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <polygon> object may have the optional SBML Level 3 Core attributes "
    "'metaid' and 'sboTerm'. No other attributes from the SBML Level 3 Core "
    "namespaces are permitted on a <polygon>.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321702
  { RenderPolygonAllowedCoreElements,
    "Core elements allowed on <polygon>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <polygon> object may have the optional SBML Level 3 Core subobjects for "
    "notes and annotations. No other elements from the SBML Level 3 Core "
    "namespaces are permitted on a <polygon>.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321703
  { RenderPolygonAllowedElements,
    "Elements allowed on <polygon>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <polygon> object may contain one and only one instance of the "
    "<listOfRenderPoints> element. No other elements from the SBML Level 3 "
    "Render namespaces are permitted on a <polygon> object. ",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321704
  { RenderPolygonEmptyLOElements,
    "No Empty ListOf elements allowed on <polygon>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The <listOfRenderPoints> subobject on a <polygon> object is optional, but "
    "if present, this container object must not be empty.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321705
  { RenderPolygonLORenderPointsAllowedCoreElements,
    "Core elements allowed on <listOfRenderPoints>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Apart from the general notes and annotations subobjects permitted on all "
    "SBML objects, a <listOfRenderPoints> container object may only contain "
    "<renderPoint> objects.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321706
  { RenderPolygonLORenderPointsAllowedCoreAttributes,
    "Core attributes allowed on <listOfRenderPoints>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <listOfRenderPoints> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <listOfRenderPoints> object.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321801
  { RenderRadialGradientAllowedCoreAttributes,
    "Core attributes allowed on <radialGradient>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <radialGradient> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <radialGradient>.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321802
  { RenderRadialGradientAllowedCoreElements,
    "Core elements allowed on <radialGradient>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <radialGradient> object may have the optional SBML Level 3 Core "
    "subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a <radialGradient>.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321803
  { RenderRadialGradientAllowedAttributes,
    "Attributes allowed on <radialGradient>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <radialGradient> object may have the optional attributes 'render:cx', "
    "'render:cy', 'render:cz', 'render:r', 'render:fx', 'render:fy' and "
    "'render:fz'. No other attributes from the SBML Level 3 Render namespaces "
    "are permitted on a <radialGradient> object. ",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321804
  { RenderRadialGradientCxMustBeRelAbsVector,
    "The 'cx' attribute must be RelAbsVector.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:cx' of a <radialGradient> object must "
    "conform to the syntax of SBML data type <relAbsVector,> i.e., a string "
    "encoding optionally an absolute number followed by an optional relative "
    "number followed by a <% sign>. Adding spaces between the coordinates is "
    "encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321805
  { RenderRadialGradientCyMustBeRelAbsVector,
    "The 'cy' attribute must be RelAbsVector.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:cy' of a <radialGradient> object must "
    "conform to the syntax of SBML data type <relAbsVector,> i.e., a string "
    "encoding optionally an absolute number followed by an optional relative "
    "number followed by a <% sign>. Adding spaces between the coordinates is "
    "encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321806
  { RenderRadialGradientCzMustBeRelAbsVector,
    "The 'cz' attribute must be RelAbsVector.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:cz' of a <radialGradient> object must "
    "conform to the syntax of SBML data type <relAbsVector,> i.e., a string "
    "encoding optionally an absolute number followed by an optional relative "
    "number followed by a <% sign>. Adding spaces between the coordinates is "
    "encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321807
  { RenderRadialGradientRMustBeRelAbsVector,
    "The 'r' attribute must be RelAbsVector.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:r' of a <radialGradient> object must "
    "conform to the syntax of SBML data type <relAbsVector,> i.e., a string "
    "encoding optionally an absolute number followed by an optional relative "
    "number followed by a <% sign>. Adding spaces between the coordinates is "
    "encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321808
  { RenderRadialGradientFxMustBeRelAbsVector,
    "The 'fx' attribute must be RelAbsVector.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:fx' of a <radialGradient> object must "
    "conform to the syntax of SBML data type <relAbsVector,> i.e., a string "
    "encoding optionally an absolute number followed by an optional relative "
    "number followed by a <% sign>. Adding spaces between the coordinates is "
    "encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321809
  { RenderRadialGradientFyMustBeRelAbsVector,
    "The 'fy' attribute must be RelAbsVector.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:fy' of a <radialGradient> object must "
    "conform to the syntax of SBML data type <relAbsVector,> i.e., a string "
    "encoding optionally an absolute number followed by an optional relative "
    "number followed by a <% sign>. Adding spaces between the coordinates is "
    "encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321810
  { RenderRadialGradientFzMustBeRelAbsVector,
    "The 'fz' attribute must be RelAbsVector.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:fz' of a <radialGradient> object must "
    "conform to the syntax of SBML data type <relAbsVector,> i.e., a string "
    "encoding optionally an absolute number followed by an optional relative "
    "number followed by a <% sign>. Adding spaces between the coordinates is "
    "encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321901
  { RenderRectangleAllowedCoreAttributes,
    "Core attributes allowed on <rectangle>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <rectangle> object may have the optional SBML Level 3 Core attributes "
    "'metaid' and 'sboTerm'. No other attributes from the SBML Level 3 Core "
    "namespaces are permitted on a <rectangle>.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321902
  { RenderRectangleAllowedCoreElements,
    "Core elements allowed on <rectangle>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <rectangle> object may have the optional SBML Level 3 Core subobjects "
    "for notes and annotations. No other elements from the SBML Level 3 Core "
    "namespaces are permitted on a <rectangle>.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321903
  { RenderRectangleAllowedAttributes,
    "Attributes allowed on <rectangle>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <rectangle> object must have the required attributes 'render:x', "
    "'render:y', 'render:width' and 'render:height', and may have the optional "
    "attributes 'render:ratio', 'render:z', 'render:rX' and 'render:rY'. No "
    "other attributes from the SBML Level 3 Render namespaces are permitted on "
    "a <rectangle> object. ",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321904
  { RenderRectangleXMustBeRelAbsVector,
    "The 'x' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:x' of a <rectangle> object must conform "
    "to the syntax of SBML data type <relAbsVector,> i.e., a string encoding "
    "optionally an absolute number followed by an optional relative number "
    "followed by a <% sign>. Adding spaces between the coordinates is "
    "encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321905
  { RenderRectangleYMustBeRelAbsVector,
    "The 'y' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:y' of a <rectangle> object must conform "
    "to the syntax of SBML data type <relAbsVector,> i.e., a string encoding "
    "optionally an absolute number followed by an optional relative number "
    "followed by a <% sign>. Adding spaces between the coordinates is "
    "encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321906
  { RenderRectangleWidthMustBeRelAbsVector,
    "The 'width' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:width' of a <rectangle> object must "
    "conform to the syntax of SBML data type <relAbsVector,> i.e., a string "
    "encoding optionally an absolute number followed by an optional relative "
    "number followed by a <% sign>. Adding spaces between the coordinates is "
    "encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321907
  { RenderRectangleHeightMustBeRelAbsVector,
    "The 'height' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:height' of a <rectangle> object must "
    "conform to the syntax of SBML data type <relAbsVector,> i.e., a string "
    "encoding optionally an absolute number followed by an optional relative "
    "number followed by a <% sign>. Adding spaces between the coordinates is "
    "encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321908
  { RenderRectangleRatioMustBeDouble,
    "The 'ratio' attribute must be Double.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'render:ratio' on a <rectangle> must have a value of data "
    "type 'double'.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321909
  { RenderRectangleZMustBeRelAbsVector,
    "The 'z' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:z' of a <rectangle> object must conform "
    "to the syntax of SBML data type <relAbsVector,> i.e., a string encoding "
    "optionally an absolute number followed by an optional relative number "
    "followed by a <% sign>. Adding spaces between the coordinates is "
    "encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321910
  { RenderRectangleRXMustBeRelAbsVector,
    "The 'rX' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:rX' of a <rectangle> object must "
    "conform to the syntax of SBML data type <relAbsVector,> i.e., a string "
    "encoding optionally an absolute number followed by an optional relative "
    "number followed by a <% sign>. Adding spaces between the coordinates is "
    "encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1321911
  { RenderRectangleRYMustBeRelAbsVector,
    "The 'rY' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:rY' of a <rectangle> object must "
    "conform to the syntax of SBML data type <relAbsVector,> i.e., a string "
    "encoding optionally an absolute number followed by an optional relative "
    "number followed by a <% sign>. Adding spaces between the coordinates is "
    "encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322001
  { RenderRenderCubicBezierAllowedCoreAttributes,
    "Core attributes allowed on <renderCubicBezier>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <renderCubicBezier> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <renderCubicBezier>.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322002
  { RenderRenderCubicBezierAllowedCoreElements,
    "Core elements allowed on <renderCubicBezier>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <renderCubicBezier> object may have the optional SBML Level 3 Core "
    "subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a <renderCubicBezier>.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322003
  { RenderRenderCubicBezierAllowedAttributes,
    "Attributes allowed on <renderCubicBezier>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <renderCubicBezier> object must have the required attributes "
    "'render:basePoint1_x', 'render:basePoint1_y', 'render:basePoint2_x' and "
    "'render:basePoint2_y', and may have the optional attributes "
    "'render:basePoint1_z' and 'render:basePoint2_z'. No other attributes from "
    "the SBML Level 3 Render namespaces are permitted on a <renderCubicBezier> "
    "object. ",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322004
  { RenderRenderCubicBezierBasePoint1_xMustBeRelAbsVector,
    "The 'basePoint1_x' attribute must be RelAbsVector.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:basePoint1_x' of a <renderCubicBezier> "
    "object must conform to the syntax of SBML data type <relAbsVector,> i.e., "
    "a string encoding optionally an absolute number followed by an optional "
    "relative number followed by a <% sign>. Adding spaces between the "
    "coordinates is encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322005
  { RenderRenderCubicBezierBasePoint1_yMustBeRelAbsVector,
    "The 'basePoint1_y' attribute must be RelAbsVector.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:basePoint1_y' of a <renderCubicBezier> "
    "object must conform to the syntax of SBML data type <relAbsVector,> i.e., "
    "a string encoding optionally an absolute number followed by an optional "
    "relative number followed by a <% sign>. Adding spaces between the "
    "coordinates is encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322006
  { RenderRenderCubicBezierBasePoint2_xMustBeRelAbsVector,
    "The 'basePoint2_x' attribute must be RelAbsVector.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:basePoint2_x' of a <renderCubicBezier> "
    "object must conform to the syntax of SBML data type <relAbsVector,> i.e., "
    "a string encoding optionally an absolute number followed by an optional "
    "relative number followed by a <% sign>. Adding spaces between the "
    "coordinates is encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322007
  { RenderRenderCubicBezierBasePoint2_yMustBeRelAbsVector,
    "The 'basePoint2_y' attribute must be RelAbsVector.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:basePoint2_y' of a <renderCubicBezier> "
    "object must conform to the syntax of SBML data type <relAbsVector,> i.e., "
    "a string encoding optionally an absolute number followed by an optional "
    "relative number followed by a <% sign>. Adding spaces between the "
    "coordinates is encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322008
  { RenderRenderCubicBezierBasePoint1_zMustBeRelAbsVector,
    "The 'basePoint1_z' attribute must be RelAbsVector.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:basePoint1_z' of a <renderCubicBezier> "
    "object must conform to the syntax of SBML data type <relAbsVector,> i.e., "
    "a string encoding optionally an absolute number followed by an optional "
    "relative number followed by a <% sign>. Adding spaces between the "
    "coordinates is encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322009
  { RenderRenderCubicBezierBasePoint2_zMustBeRelAbsVector,
    "The 'basePoint2_z' attribute must be RelAbsVector.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:basePoint2_z' of a <renderCubicBezier> "
    "object must conform to the syntax of SBML data type <relAbsVector,> i.e., "
    "a string encoding optionally an absolute number followed by an optional "
    "relative number followed by a <% sign>. Adding spaces between the "
    "coordinates is encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322101
  { RenderRenderCurveAllowedCoreAttributes,
    "Core attributes allowed on <renderCurve>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <renderCurve> object may have the optional SBML Level 3 Core attributes "
    "'metaid' and 'sboTerm'. No other attributes from the SBML Level 3 Core "
    "namespaces are permitted on a <renderCurve>.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322102
  { RenderRenderCurveAllowedCoreElements,
    "Core elements allowed on <renderCurve>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <renderCurve> object may have the optional SBML Level 3 Core subobjects "
    "for notes and annotations. No other elements from the SBML Level 3 Core "
    "namespaces are permitted on a <renderCurve>.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322103
  { RenderRenderCurveAllowedAttributes,
    "Attributes allowed on <renderCurve>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <renderCurve> object may have the optional attributes 'render:startHead' "
    "and 'render:endHead'. No other attributes from the SBML Level 3 Render "
    "namespaces are permitted on a <renderCurve> object. ",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322104
  { RenderRenderCurveAllowedElements,
    "Elements allowed on <renderCurve>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <renderCurve> object may contain one and only one instance of the "
    "<listOfRenderPoints> element. No other elements from the SBML Level 3 "
    "Render namespaces are permitted on a <renderCurve> object. ",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322105
  { RenderRenderCurveStartHeadMustBeLineEnding,
    "The attribute 'startHead' must point to LineEnding object.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:startHead' of a <renderCurve> object "
    "must be the identifier of an existing <lineEnding> object defined in the "
    "enclosing <model> object.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322106
  { RenderRenderCurveEndHeadMustBeLineEnding,
    "The attribute 'endHead' must point to LineEnding object.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:endHead' of a <renderCurve> object must "
    "be the identifier of an existing <lineEnding> object defined in the "
    "enclosing <model> object.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322107
  { RenderRenderCurveEmptyLOElements,
    "No Empty ListOf elements allowed on <renderCurve>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The <listOfRenderPoints> subobject on a <renderCurve> object is optional, "
    "but if present, this container object must not be empty.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322108
  { RenderRenderCurveLORenderPointsAllowedCoreElements,
    "Core elements allowed on <listOfRenderPoints>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Apart from the general notes and annotations subobjects permitted on all "
    "SBML objects, a <listOfRenderPoints> container object may only contain "
    "<renderPoint> objects.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322109
  { RenderRenderCurveLORenderPointsAllowedCoreAttributes,
    "Core attributes allowed on <listOfRenderPoints>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <listOfRenderPoints> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <listOfRenderPoints> object.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322201
  { RenderRenderPointAllowedCoreAttributes,
    "Core attributes allowed on <renderPoint>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <renderPoint> object may have the optional SBML Level 3 Core attributes "
    "'metaid' and 'sboTerm'. No other attributes from the SBML Level 3 Core "
    "namespaces are permitted on a <renderPoint>.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322202
  { RenderRenderPointAllowedCoreElements,
    "Core elements allowed on <renderPoint>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <renderPoint> object may have the optional SBML Level 3 Core subobjects "
    "for notes and annotations. No other elements from the SBML Level 3 Core "
    "namespaces are permitted on a <renderPoint>.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322203
  { RenderRenderPointAllowedAttributes,
    "Attributes allowed on <renderPoint>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <renderPoint> object must have the required attributes 'render:x' and "
    "'render:y', and may have the optional attribute 'render:z'. No other "
    "attributes from the SBML Level 3 Render namespaces are permitted on a "
    "<renderPoint> object. ",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322204
  { RenderRenderPointXMustBeRelAbsVector,
    "The 'x' attribute must be RelAbsVector.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:x' of a <renderPoint> object must "
    "conform to the syntax of SBML data type <relAbsVector,> i.e., a string "
    "encoding optionally an absolute number followed by an optional relative "
    "number followed by a <% sign>. Adding spaces between the coordinates is "
    "encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322205
  { RenderRenderPointYMustBeRelAbsVector,
    "The 'y' attribute must be RelAbsVector.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:y' of a <renderPoint> object must "
    "conform to the syntax of SBML data type <relAbsVector,> i.e., a string "
    "encoding optionally an absolute number followed by an optional relative "
    "number followed by a <% sign>. Adding spaces between the coordinates is "
    "encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322206
  { RenderRenderPointZMustBeRelAbsVector,
    "The 'z' attribute must be RelAbsVector.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:z' of a <renderPoint> object must "
    "conform to the syntax of SBML data type <relAbsVector,> i.e., a string "
    "encoding optionally an absolute number followed by an optional relative "
    "number followed by a <% sign>. Adding spaces between the coordinates is "
    "encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322301
  { RenderTextAllowedCoreAttributes,
    "Core attributes allowed on <text>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <text> object may have the optional SBML Level 3 Core attributes "
    "'metaid' and 'sboTerm'. No other attributes from the SBML Level 3 Core "
    "namespaces are permitted on a <text>.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322302
  { RenderTextAllowedCoreElements,
    "Core elements allowed on <text>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <text> object may have the optional SBML Level 3 Core subobjects for "
    "notes and annotations. No other elements from the SBML Level 3 Core "
    "namespaces are permitted on a <text>.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322303
  { RenderTextAllowedAttributes,
    "Attributes allowed on <text>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <text> object must have the required attributes 'render:x' and "
    "'render:y', and may have the optional attributes 'render:font-family', "
    "'render:font-weight', 'render:font-style', 'render:text-anchor', "
    "'render:vtext-anchor', 'render:z' and 'render:font-size'. No other "
    "attributes from the SBML Level 3 Render namespaces are permitted on a "
    "<text> object. ",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322304
  { RenderTextXMustBeRelAbsVector,
    "The 'x' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:x' of a <text> object must conform to "
    "the syntax of SBML data type <relAbsVector,> i.e., a string encoding "
    "optionally an absolute number followed by an optional relative number "
    "followed by a <% sign>. Adding spaces between the coordinates is "
    "encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322305
  { RenderTextYMustBeRelAbsVector,
    "The 'y' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:y' of a <text> object must conform to "
    "the syntax of SBML data type <relAbsVector,> i.e., a string encoding "
    "optionally an absolute number followed by an optional relative number "
    "followed by a <% sign>. Adding spaces between the coordinates is "
    "encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322306
  { RenderTextFontFamilyMustBeString,
    "The 'font-family' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'render:font-family' on a <text> must have a value of data "
    "type 'string'.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322307
  { RenderTextFontWeightMustBeFontWeightEnum,
    "The 'font-weight' attribute must be FontWeight.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:font-weight' of a <text> object must "
    "conform to the syntax of SBML data type 'FontWeight' and may only take on "
    "the allowed values of 'FontWeight' defined in SBML; that is, the value "
    "must be one of the following: 'bold' or 'normal'.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322308
  { RenderTextFontStyleMustBeFontStyleEnum,
    "The 'font-style' attribute must be FontStyle.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:font-style' of a <text> object must "
    "conform to the syntax of SBML data type 'FontStyle' and may only take on "
    "the allowed values of 'FontStyle' defined in SBML; that is, the value must "
    "be one of the following: 'italic' or 'normal'.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322309
  { RenderTextTextAnchorMustBeHTextAnchorEnum,
    "The 'text-anchor' attribute must be HTextAnchor.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:text-anchor' of a <text> object must "
    "conform to the syntax of SBML data type 'HTextAnchor' and may only take on "
    "the allowed values of 'HTextAnchor' defined in SBML; that is, the value "
    "must be one of the following: 'start', 'middle' or 'end'.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322310
  { RenderTextVtextAnchorMustBeVTextAnchorEnum,
    "The 'vtext-anchor' attribute must be VTextAnchor.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:vtext-anchor' of a <text> object must "
    "conform to the syntax of SBML data type 'VTextAnchor' and may only take on "
    "the allowed values of 'VTextAnchor' defined in SBML; that is, the value "
    "must be one of the following: 'top', 'middle', 'bottom' or 'baseline'.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322311
  { RenderTextZMustBeRelAbsVector,
    "The 'z' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:z' of a <text> object must conform to "
    "the syntax of SBML data type <relAbsVector,> i.e., a string encoding "
    "optionally an absolute number followed by an optional relative number "
    "followed by a <% sign>. Adding spaces between the coordinates is "
    "encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322312
  { RenderTextFontSizeMustBeRelAbsVector,
    "The 'font-size' attribute must be RelAbsVector.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:font-size' of a <text> object must "
    "conform to the syntax of SBML data type <relAbsVector,> i.e., a string "
    "encoding optionally an absolute number followed by an optional relative "
    "number followed by a <% sign>. Adding spaces between the coordinates is "
    "encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322401
  { RenderTransformation2DAllowedCoreAttributes,
    "Core attributes allowed on <transformation2D>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <transformation2D> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <transformation2D>.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322402
  { RenderTransformation2DAllowedCoreElements,
    "Core elements allowed on <transformation2D>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <transformation2D> object may have the optional SBML Level 3 Core "
    "subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a <transformation2D>.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322501
  { RenderTransformationAllowedCoreAttributes,
    "Core attributes allowed on <transformation>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <transformation> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <transformation>.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322502
  { RenderTransformationAllowedCoreElements,
    "Core elements allowed on <transformation>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <transformation> object may have the optional SBML Level 3 Core "
    "subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a <transformation>.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322503
  { RenderTransformationAllowedAttributes,
    "Attributes allowed on <transformation>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <transformation> object may have the optional attributes "
    "'render:transform' and 'render:name'. No other attributes from the SBML "
    "Level 3 Render namespaces are permitted on a <transformation> object. ",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322504
  { RenderTransformationTransformMustBeString,
    "The 'transform' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:transform' of a <transformation> object "
    "must be an array of values of type 'double'.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322505
  { RenderTransformationNameMustBeString,
    "The 'name' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'render:name' on a <transformation> must have a value of "
    "data type 'string'.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322601
  { RenderGraphicalPrimitive1DAllowedCoreAttributes,
    "Core attributes allowed on <graphicalPrimitive1D>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <graphicalPrimitive1D> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <graphicalPrimitive1D>.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322602
  { RenderGraphicalPrimitive1DAllowedCoreElements,
    "Core elements allowed on <graphicalPrimitive1D>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <graphicalPrimitive1D> object may have the optional SBML Level 3 Core "
    "subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a <graphicalPrimitive1D>.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322603
  { RenderGraphicalPrimitive1DAllowedAttributes,
    "Attributes allowed on <graphicalPrimitive1D>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <graphicalPrimitive1D> object may have the optional attributes "
    "'render:id', 'render:stroke', 'render:stroke-width' and "
    "'render:stroke-dasharray'. No other attributes from the SBML Level 3 "
    "Render namespaces are permitted on a <graphicalPrimitive1D> object. ",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322604
  { RenderGraphicalPrimitive1DStrokeMustBeString,
    "The 'stroke' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'render:stroke' on a <graphicalPrimitive1D> must have a "
    "value of data type 'string'.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322605
  { RenderGraphicalPrimitive1DStrokeWidthMustBeDouble,
    "The 'stroke-width' attribute must be Double.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'render:stroke-width' on a <graphicalPrimitive1D> must have "
    "a value of data type 'double'.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322606
  { RenderGraphicalPrimitive1DStrokeDashArrayMustBeString,
    "The 'stroke-dasharray' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:stroke-dasharray' of a "
    "<graphicalPrimitive1D> object must be an array of values of type "
    "'unsignedint'.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322701
  { RenderGraphicalPrimitive2DAllowedCoreAttributes,
    "Core attributes allowed on <graphicalPrimitive2D>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <graphicalPrimitive2D> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <graphicalPrimitive2D>.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322702
  { RenderGraphicalPrimitive2DAllowedCoreElements,
    "Core elements allowed on <graphicalPrimitive2D>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <graphicalPrimitive2D> object may have the optional SBML Level 3 Core "
    "subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a <graphicalPrimitive2D>.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322703
  { RenderGraphicalPrimitive2DAllowedAttributes,
    "Attributes allowed on <graphicalPrimitive2D>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <graphicalPrimitive2D> object may have the optional attributes "
    "'render:fill' and 'render:fill-rule'. No other attributes from the SBML "
    "Level 3 Render namespaces are permitted on a <graphicalPrimitive2D> "
    "object. ",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322704
  { RenderGraphicalPrimitive2DFillMustBeString,
    "The 'fill' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'render:fill' on a <graphicalPrimitive2D> must have a value "
    "of data type 'string'.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322705
  { RenderGraphicalPrimitive2DFillRuleMustBeFillRuleEnum,
    "The 'fill-rule' attribute must be FillRule.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:fill-rule' of a <graphicalPrimitive2D> "
    "object must conform to the syntax of SBML data type 'FillRule' and may "
    "only take on the allowed values of 'FillRule' defined in SBML; that is, "
    "the value must be one of the following: 'nonzero' or 'evenodd'.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322801
  { RenderStyleAllowedCoreAttributes,
    "Core attributes allowed on <style>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <style> object may have the optional SBML Level 3 Core attributes "
    "'metaid' and 'sboTerm'. No other attributes from the SBML Level 3 Core "
    "namespaces are permitted on a <style>.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322802
  { RenderStyleAllowedCoreElements,
    "Core elements allowed on <style>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <style> object may have the optional SBML Level 3 Core subobjects for "
    "notes and annotations. No other elements from the SBML Level 3 Core "
    "namespaces are permitted on a <style>.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322803
  { RenderStyleAllowedAttributes,
    "Attributes allowed on <style>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <style> object may have the optional attributes 'render:id', "
    "'render:name', 'render:roleList' and 'render:typeList'. No other "
    "attributes from the SBML Level 3 Render namespaces are permitted on a "
    "<style> object. ",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322804
  { RenderStyleAllowedElements,
    "Elements allowed on <style>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <style> object may contain one and only one instance of the RenderGroup "
    "element. No other elements from the SBML Level 3 Render namespaces are "
    "permitted on a <style> object. ",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322805
  { RenderStyleNameMustBeString,
    "The 'name' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'render:name' on a <style> must have a value of data type "
    "'string'.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322806
  { RenderStyleRoleListMustBeString,
    "The 'roleList' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'render:roleList' on a <style> must have a value of data "
    "type 'string'.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322807
  { RenderStyleTypeListMustBeString,
    "The 'typeList' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'render:typeList' on a <style> must have a value of data "
    "type 'string'.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322901
  { RenderRenderInformationBaseAllowedCoreAttributes,
    "Core attributes allowed on <renderInformationBase>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <renderInformationBase> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <renderInformationBase>.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322902
  { RenderRenderInformationBaseAllowedCoreElements,
    "Core elements allowed on <renderInformationBase>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <renderInformationBase> object may have the optional SBML Level 3 Core "
    "subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a <renderInformationBase>.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322903
  { RenderRenderInformationBaseAllowedAttributes,
    "Attributes allowed on <renderInformationBase>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <renderInformationBase> object must have the required attribute "
    "'render:id', and may have the optional attributes 'render:name', "
    "'render:programName', 'render:programVersion', "
    "'render:referenceRenderInformation' and 'render:backgroundColor'. No other "
    "attributes from the SBML Level 3 Render namespaces are permitted on a "
    "<renderInformationBase> object. ",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322904
  { RenderRenderInformationBaseAllowedElements,
    "Elements allowed on <renderInformationBase>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <renderInformationBase> object may contain one and only one instance of "
    "each of the <listOfColorDefinitions,> <listOfGradientBases> and "
    "<listOfLineEndings> elements. No other elements from the SBML Level 3 "
    "Render namespaces are permitted on a <renderInformationBase> object. ",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322905
  { RenderRenderInformationBaseNameMustBeString,
    "The 'name' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'render:name' on a <renderInformationBase> must have a value "
    "of data type 'string'.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322906
  { RenderRenderInformationBaseProgramNameMustBeString,
    "The 'programName' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'render:programName' on a <renderInformationBase> must have "
    "a value of data type 'string'.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322907
  { RenderRenderInformationBaseProgramVersionMustBeString,
    "The 'programVersion' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'render:programVersion' on a <renderInformationBase> must "
    "have a value of data type 'string'.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322908
  {
    RenderRenderInformationBaseReferenceRenderInformationMustBeRenderInformationBase,
    "The attribute 'referenceRenderInformation' must point to "
      "RenderInformationBase object.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:referenceRenderInformation' of a "
    "<renderInformationBase> object must be the identifier of an existing "
    "<renderInformationBase> object defined in the enclosing <model> object.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322909
  { RenderRenderInformationBaseBackgroundColorMustBeString,
    "The 'backgroundColor' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'render:backgroundColor' on a <renderInformationBase> must "
    "have a value of data type 'string'.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322910
  { RenderRenderInformationBaseLOElementChildren,
    "Number of children in ListOf elements allowed on "
      "<renderInformationBase>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The <listOfColorDefinitions,> <listOfGradientBases> and "
    "<listOfLineEndings> subobjects on a <renderInformationBase> object are "
    "optional, but if present, these container objects must not be empty.The "
    "<listOfColorDefinitions> must contain at least zero instances of the "
    "<colorDefinition> object.The <listOfGradientBases> must contain at least "
    "one instances of the <gradientBase> object.The <listOfLineEndings> must "
    "contain at least one instances of the <lineEnding> object.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322911
  { RenderRenderInformationBaseLOColorDefinitionsAllowedCoreElements,
    "Core elements allowed on <listOfColorDefinitions>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Apart from the general notes and annotations subobjects permitted on all "
    "SBML objects, a <listOfColorDefinitions> container object may only contain "
    "<colorDefinition> objects.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322912
  { RenderRenderInformationBaseLOGradientBasesAllowedCoreElements,
    "Core elements allowed on <listOfGradientBases>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Apart from the general notes and annotations subobjects permitted on all "
    "SBML objects, a <listOfGradientBases> container object may only contain "
    "<gradientBase> objects.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322913
  { RenderRenderInformationBaseLOLineEndingsAllowedCoreElements,
    "Core elements allowed on <listOfLineEndings>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Apart from the general notes and annotations subobjects permitted on all "
    "SBML objects, a <listOfLineEndings> container object may only contain "
    "<lineEnding> objects.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322914
  { RenderRenderInformationBaseLOColorDefinitionsAllowedCoreAttributes,
    "Core attributes allowed on <listOfColorDefinitions>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <listOfColorDefinitions> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <listOfColorDefinitions> object.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322915
  { RenderRenderInformationBaseLOGradientBasesAllowedCoreAttributes,
    "Core attributes allowed on <listOfGradientBases>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <listOfGradientBases> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <listOfGradientBases> object.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1322916
  { RenderRenderInformationBaseLOLineEndingsAllowedCoreAttributes,
    "Core attributes allowed on <listOfLineEndings>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <listOfLineEndings> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <listOfLineEndings> object.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1323001
  { RenderDefaultValuesAllowedCoreAttributes,
    "Core attributes allowed on <defaultValues>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <defaultValues> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <defaultValues>.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1323002
  { RenderDefaultValuesAllowedCoreElements,
    "Core elements allowed on <defaultValues>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <defaultValues> object may have the optional SBML Level 3 Core "
    "subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a <defaultValues>.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1323003
  { RenderDefaultValuesAllowedAttributes,
    "Attributes allowed on <defaultValues>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <defaultValues> object may have the optional attributes "
    "'render:backgroundColor', 'render:spreadMethod', 'render:fill', "
    "'render:fill-rule', 'render:stroke', 'render:stroke-width', "
    "'render:font-family', 'render:font-weight', 'render:font-style', "
    "'render:text-anchor', 'render:vtext-anchor', 'render:startHead', "
    "'render:endHead', 'render:enableRotationalMapping', "
    "'render:linearGradient_x1', 'render:linearGradient_y1', "
    "'render:linearGradient_z1', 'render:linearGradient_x2', "
    "'render:linearGradient_y2', 'render:linearGradient_z2', "
    "'render:radialGradient_cx', 'render:radialGradient_cy', "
    "'render:radialGradient_cz', 'render:radialGradient_r', "
    "'render:radialGradient_fx', 'render:radialGradient_fy', "
    "'render:radialGradient_fz', 'render:default_z' and 'render:font-size'. No "
    "other attributes from the SBML Level 3 Render namespaces are permitted on "
    "a <defaultValues> object. ",
    { "L3V1 Render V1 Section"
    }
  },

  // 1323004
  { RenderDefaultValuesBackgroundColorMustBeString,
    "The 'backgroundColor' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'render:backgroundColor' on a <defaultValues> must have a "
    "value of data type 'string'.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1323005
  { RenderDefaultValuesSpreadMethodMustBeGradientSpreadMethodEnum,
    "The 'spreadMethod' attribute must be GradientSpreadMethod.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:spreadMethod' of a <defaultValues> "
    "object must conform to the syntax of SBML data type 'GradientSpreadMethod' "
    "and may only take on the allowed values of 'GradientSpreadMethod' defined "
    "in SBML; that is, the value must be one of the following: 'pad', 'reflect' "
    "or 'repeat'.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1323006
  { RenderDefaultValuesFillMustBeString,
    "The 'fill' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'render:fill' on a <defaultValues> must have a value of data "
    "type 'string'.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1323007
  { RenderDefaultValuesFillRuleMustBeFillRuleEnum,
    "The 'fill-rule' attribute must be FillRule.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:fill-rule' of a <defaultValues> object "
    "must conform to the syntax of SBML data type 'FillRule' and may only take "
    "on the allowed values of 'FillRule' defined in SBML; that is, the value "
    "must be one of the following: 'nonzero' or 'evenodd'.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1323008
  { RenderDefaultValuesStrokeMustBeString,
    "The 'stroke' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'render:stroke' on a <defaultValues> must have a value of "
    "data type 'string'.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1323009
  { RenderDefaultValuesStrokeWidthMustBeDouble,
    "The 'stroke-width' attribute must be Double.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'render:stroke-width' on a <defaultValues> must have a value "
    "of data type 'double'.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1323010
  { RenderDefaultValuesFontFamilyMustBeString,
    "The 'font-family' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'render:font-family' on a <defaultValues> must have a value "
    "of data type 'string'.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1323011
  { RenderDefaultValuesFontWeightMustBeFontWeightEnum,
    "The 'font-weight' attribute must be FontWeight.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:font-weight' of a <defaultValues> "
    "object must conform to the syntax of SBML data type 'FontWeight' and may "
    "only take on the allowed values of 'FontWeight' defined in SBML; that is, "
    "the value must be one of the following: 'bold' or 'normal'.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1323012
  { RenderDefaultValuesFontStyleMustBeFontStyleEnum,
    "The 'font-style' attribute must be FontStyle.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:font-style' of a <defaultValues> object "
    "must conform to the syntax of SBML data type 'FontStyle' and may only take "
    "on the allowed values of 'FontStyle' defined in SBML; that is, the value "
    "must be one of the following: 'italic' or 'normal'.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1323013
  { RenderDefaultValuesTextAnchorMustBeHTextAnchorEnum,
    "The 'text-anchor' attribute must be HTextAnchor.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:text-anchor' of a <defaultValues> "
    "object must conform to the syntax of SBML data type 'HTextAnchor' and may "
    "only take on the allowed values of 'HTextAnchor' defined in SBML; that is, "
    "the value must be one of the following: 'start', 'middle' or 'end'.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1323014
  { RenderDefaultValuesVtextAnchorMustBeVTextAnchorEnum,
    "The 'vtext-anchor' attribute must be VTextAnchor.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:vtext-anchor' of a <defaultValues> "
    "object must conform to the syntax of SBML data type 'VTextAnchor' and may "
    "only take on the allowed values of 'VTextAnchor' defined in SBML; that is, "
    "the value must be one of the following: 'top', 'middle', 'bottom' or "
    "'baseline'.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1323015
  { RenderDefaultValuesStartHeadMustBeLineEnding,
    "The attribute 'startHead' must point to LineEnding object.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:startHead' of a <defaultValues> object "
    "must be the identifier of an existing <lineEnding> object defined in the "
    "enclosing <model> object.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1323016
  { RenderDefaultValuesEndHeadMustBeLineEnding,
    "The attribute 'endHead' must point to LineEnding object.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:endHead' of a <defaultValues> object "
    "must be the identifier of an existing <lineEnding> object defined in the "
    "enclosing <model> object.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1323017
  { RenderDefaultValuesEnableRotationalMappingMustBeBoolean,
    "The 'enableRotationalMapping' attribute must be Boolean.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'render:enableRotationalMapping' on a <defaultValues> must "
    "have a value of data type 'boolean'.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1323018
  { RenderDefaultValuesLinearGradient_x1MustBeString,
    "The 'linearGradient_x1' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:linearGradient_x1' of a <defaultValues> "
    "object must conform to the syntax of SBML data type <relAbsVector,> i.e., "
    "a string encoding optionally an absolute number followed by an optional "
    "relative number followed by a <% sign>. Adding spaces between the "
    "coordinates is encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1323019
  { RenderDefaultValuesLinearGradient_y1MustBeString,
    "The 'linearGradient_y1' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:linearGradient_y1' of a <defaultValues> "
    "object must conform to the syntax of SBML data type <relAbsVector,> i.e., "
    "a string encoding optionally an absolute number followed by an optional "
    "relative number followed by a <% sign>. Adding spaces between the "
    "coordinates is encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1323020
  { RenderDefaultValuesLinearGradient_z1MustBeString,
    "The 'linearGradient_z1' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:linearGradient_z1' of a <defaultValues> "
    "object must conform to the syntax of SBML data type <relAbsVector,> i.e., "
    "a string encoding optionally an absolute number followed by an optional "
    "relative number followed by a <% sign>. Adding spaces between the "
    "coordinates is encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1323021
  { RenderDefaultValuesLinearGradient_x2MustBeString,
    "The 'linearGradient_x2' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:linearGradient_x2' of a <defaultValues> "
    "object must conform to the syntax of SBML data type <relAbsVector,> i.e., "
    "a string encoding optionally an absolute number followed by an optional "
    "relative number followed by a <% sign>. Adding spaces between the "
    "coordinates is encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1323022
  { RenderDefaultValuesLinearGradient_y2MustBeString,
    "The 'linearGradient_y2' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:linearGradient_y2' of a <defaultValues> "
    "object must conform to the syntax of SBML data type <relAbsVector,> i.e., "
    "a string encoding optionally an absolute number followed by an optional "
    "relative number followed by a <% sign>. Adding spaces between the "
    "coordinates is encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1323023
  { RenderDefaultValuesLinearGradient_z2MustBeString,
    "The 'linearGradient_z2' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:linearGradient_z2' of a <defaultValues> "
    "object must conform to the syntax of SBML data type <relAbsVector,> i.e., "
    "a string encoding optionally an absolute number followed by an optional "
    "relative number followed by a <% sign>. Adding spaces between the "
    "coordinates is encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1323024
  { RenderDefaultValuesRadialGradient_cxMustBeString,
    "The 'radialGradient_cx' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:radialGradient_cx' of a <defaultValues> "
    "object must conform to the syntax of SBML data type <relAbsVector,> i.e., "
    "a string encoding optionally an absolute number followed by an optional "
    "relative number followed by a <% sign>. Adding spaces between the "
    "coordinates is encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1323025
  { RenderDefaultValuesRadialGradient_cyMustBeString,
    "The 'radialGradient_cy' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:radialGradient_cy' of a <defaultValues> "
    "object must conform to the syntax of SBML data type <relAbsVector,> i.e., "
    "a string encoding optionally an absolute number followed by an optional "
    "relative number followed by a <% sign>. Adding spaces between the "
    "coordinates is encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1323026
  { RenderDefaultValuesRadialGradient_czMustBeString,
    "The 'radialGradient_cz' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:radialGradient_cz' of a <defaultValues> "
    "object must conform to the syntax of SBML data type <relAbsVector,> i.e., "
    "a string encoding optionally an absolute number followed by an optional "
    "relative number followed by a <% sign>. Adding spaces between the "
    "coordinates is encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1323027
  { RenderDefaultValuesRadialGradient_rMustBeString,
    "The 'radialGradient_r' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:radialGradient_r' of a <defaultValues> "
    "object must conform to the syntax of SBML data type <relAbsVector,> i.e., "
    "a string encoding optionally an absolute number followed by an optional "
    "relative number followed by a <% sign>. Adding spaces between the "
    "coordinates is encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1323028
  { RenderDefaultValuesRadialGradient_fxMustBeString,
    "The 'radialGradient_fx' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:radialGradient_fx' of a <defaultValues> "
    "object must conform to the syntax of SBML data type <relAbsVector,> i.e., "
    "a string encoding optionally an absolute number followed by an optional "
    "relative number followed by a <% sign>. Adding spaces between the "
    "coordinates is encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1323029
  { RenderDefaultValuesRadialGradient_fyMustBeString,
    "The 'radialGradient_fy' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:radialGradient_fy' of a <defaultValues> "
    "object must conform to the syntax of SBML data type <relAbsVector,> i.e., "
    "a string encoding optionally an absolute number followed by an optional "
    "relative number followed by a <% sign>. Adding spaces between the "
    "coordinates is encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1323030
  { RenderDefaultValuesRadialGradient_fzMustBeString,
    "The 'radialGradient_fz' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:radialGradient_fz' of a <defaultValues> "
    "object must conform to the syntax of SBML data type <relAbsVector,> i.e., "
    "a string encoding optionally an absolute number followed by an optional "
    "relative number followed by a <% sign>. Adding spaces between the "
    "coordinates is encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1323031
  { RenderDefaultValuesDefault_zMustBeString,
    "The 'default_z' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:default_z' of a <defaultValues> object "
    "must conform to the syntax of SBML data type <relAbsVector,> i.e., a "
    "string encoding optionally an absolute number followed by an optional "
    "relative number followed by a <% sign>. Adding spaces between the "
    "coordinates is encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

  // 1323032
  { RenderDefaultValuesFontSizeMustBeString,
    "The 'font-size' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'render:font-size' of a <defaultValues> object "
    "must conform to the syntax of SBML data type <relAbsVector,> i.e., a "
    "string encoding optionally an absolute number followed by an optional "
    "relative number followed by a <% sign>. Adding spaces between the "
    "coordinates is encouraged, but not required.",
    { "L3V1 Render V1 Section"
    }
  },

};

/** @endcond */



LIBSBML_CPP_NAMESPACE_END




#endif /* !RenderSBMLErrorTable_H__ */


