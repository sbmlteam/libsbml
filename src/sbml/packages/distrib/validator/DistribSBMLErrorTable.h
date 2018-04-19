/**
 * @file DistribSBMLErrorTable.h
 * @brief Definition of the DistribSBMLErrorTable class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
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


#ifndef DistribSBMLErrorTable_H__
#define DistribSBMLErrorTable_H__


#include <sbml/packages/distrib/validator/DistribSBMLError.h>


LIBSBML_CPP_NAMESPACE_BEGIN



/** @cond doxygenLibsbmlInternal */

static const packageErrorTableEntry distribErrorTable[] =
{
  // 1510100
  { DistribUnknown,
    "Unknown error from Distrib",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Unknown error from Distrib",
    { ""
    }
  },

  // 1510101
  { DistribNSUndeclared,
    "The Distrib namespace is not correctly declared.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "To conform to the Distributions Package specification for SBML Level 3 "
    "Version 2, an SBML document must declare "
    "'http://www.sbml.org/sbml/level3/version2/distrib/version1' as the "
    "XMLNamespace to use for elements of this package.",
    { "L3V1 Distrib V1 Section 3.1"
    }
  },

  // 1510102
  { DistribElementNotInNs,
    "Element not in Distrib namespace",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Wherever they appear in an SBML document, elements and attributes from the "
    "Distributions Package must use the "
    "'http://www.sbml.org/sbml/level3/version2/distrib/version1' namespace, "
    "declaring so either explicitly or implicitly.",
    { "L3V1 Distrib V1 Section 3.1"
    }
  },

  // 1510301
  { DistribDuplicateComponentId,
    "Duplicate 'id' attribute value",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "(Extends validation rule #10301 in the SBML Level 3 Core specification. TO "
    "DO list scope of ids)",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1510302
  { DistribIdSyntaxRule,
    "Invalid SId syntax",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of a 'distrib:id' must conform to the syntax of the <sbml> data "
    "type 'SId'",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520101
  { DistribAttributeRequiredMissing,
    "Required distrib:required attribute on <sbml>",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "In all SBML documents using the Distributions Package, the <sbml> object "
    "must have the 'distrib:required' attribute.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520102
  { DistribAttributeRequiredMustBeBoolean,
    "The distrib:required attribute must be Boolean",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of attribute 'distrib:required' on the <sbml> object must be of "
    "data type 'boolean'.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520103
  { DistribAttributeRequiredMustHaveValue,
    "The distrib:required attribute must be 'true'",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of attribute 'distrib:required' on the <sbml> object must be set "
    "to 'true'.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520201
  { DistribFunctionDefinitionAllowedElements,
    "Elements allowed on <functionDefinition>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <functionDefinition> object may contain one and only one instance of the "
    "<distribDrawFromDistribution> element. No other elements from the SBML "
    "Level 3 Distributions namespaces are permitted on a <functionDefinition> "
    "object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520301
  { DistribSBaseAllowedElements,
    "Elements allowed on <sBase>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A 'SBase' object may contain one and only one instance of the "
    "<distribUncertainty> element. No other elements from the SBML Level 3 "
    "Distributions namespaces are permitted on a 'SBase' object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520401
  { DistribDistribDrawFromDistributionAllowedCoreAttributes,
    "Core attributes allowed on <distribDrawFromDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribDrawFromDistribution> object may have the optional SBML Level 3 "
    "Core attributes 'metaid' and 'sboTerm'. No other attributes from the SBML "
    "Level 3 Core namespaces are permitted on a "
    "<distribDrawFromDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520402
  { DistribDistribDrawFromDistributionAllowedCoreElements,
    "Core elements allowed on <distribDrawFromDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribDrawFromDistribution> object may have the optional SBML Level 3 "
    "Core subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a "
    "<distribDrawFromDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520403
  { DistribDistribDrawFromDistributionAllowedAttributes,
    "Attributes allowed on <distribDrawFromDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribDrawFromDistribution> object may have the optional attributes "
    "'distrib:id', 'distrib:name', 'distrib:id' and 'distrib:name'. No other "
    "attributes from the SBML Level 3 Distributions namespaces are permitted on "
    "a <distribDrawFromDistribution> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520404
  { DistribDistribDrawFromDistributionAllowedElements,
    "Elements allowed on <distribDrawFromDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribDrawFromDistribution> object may contain one and only one "
    "instance of each of the <listOfDistribInputs,> DistribDistribution, "
    "<listOfDistribInputs> and DistribDistribution elements. No other elements "
    "from the SBML Level 3 Distributions namespaces are permitted on a "
    "<distribDrawFromDistribution> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520405
  { DistribDistribDrawFromDistributionNameMustBeString,
    "The 'name' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'distrib:name' on a <distribDrawFromDistribution> must have "
    "a value of data type 'string'.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520407
  { DistribDistribDrawFromDistributionLODistribInputsAllowedCoreElements,
    "Core elements allowed on <listOfDistribInputs>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Apart from the general notes and annotations subobjects permitted on all "
    "SBML objects, a <listOfDistribInputs> container object may only contain "
    "<distribInput> objects.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520409
  { DistribDistribDrawFromDistributionLODistribInputsAllowedCoreAttributes,
    "Core attributes allowed on <listOfDistribInputs>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <listOfDistribInputs> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <listOfDistribInputs> object.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520501
  { DistribDistribInputAllowedCoreAttributes,
    "Core attributes allowed on <distribInput>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribInput> object may have the optional SBML Level 3 Core attributes "
    "'metaid' and 'sboTerm'. No other attributes from the SBML Level 3 Core "
    "namespaces are permitted on a <distribInput>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520502
  { DistribDistribInputAllowedCoreElements,
    "Core elements allowed on <distribInput>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribInput> object may have the optional SBML Level 3 Core subobjects "
    "for notes and annotations. No other elements from the SBML Level 3 Core "
    "namespaces are permitted on a <distribInput>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520503
  { DistribDistribInputAllowedAttributes,
    "Attributes allowed on <distribInput>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribInput> object may have the optional attributes 'distrib:id', "
    "'distrib:name', 'distrib:index', 'distrib:id', 'distrib:name' and "
    "'distrib:index'. No other attributes from the SBML Level 3 Distributions "
    "namespaces are permitted on a <distribInput> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520504
  { DistribDistribInputNameMustBeString,
    "The 'name' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'distrib:name' on a <distribInput> must have a value of data "
    "type 'string'.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520505
  { DistribDistribInputIndexMustBeNonNegativeInteger,
    "The 'index' attribute must be NonNegativeInteger.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'distrib:index' on a <distribInput> must have a value of "
    "data type 'integer', and must be non negative.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520601
  { DistribDistribDistributionAllowedCoreAttributes,
    "Core attributes allowed on <distribDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribDistribution> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <distribDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520602
  { DistribDistribDistributionAllowedCoreElements,
    "Core elements allowed on <distribDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribDistribution> object may have the optional SBML Level 3 Core "
    "subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a <distribDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520701
  { DistribDistribUnivariateDistributionAllowedCoreAttributes,
    "Core attributes allowed on <distribUnivariateDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribUnivariateDistribution> object may have the optional SBML Level "
    "3 Core attributes 'metaid' and 'sboTerm'. No other attributes from the "
    "SBML Level 3 Core namespaces are permitted on a "
    "<distribUnivariateDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520702
  { DistribDistribUnivariateDistributionAllowedCoreElements,
    "Core elements allowed on <distribUnivariateDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribUnivariateDistribution> object may have the optional SBML Level "
    "3 Core subobjects for notes and annotations. No other elements from the "
    "SBML Level 3 Core namespaces are permitted on a "
    "<distribUnivariateDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520801
  { DistribDistribMultivariateDistributionAllowedCoreAttributes,
    "Core attributes allowed on <distribMultivariateDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribMultivariateDistribution> object may have the optional SBML "
    "Level 3 Core attributes 'metaid' and 'sboTerm'. No other attributes from "
    "the SBML Level 3 Core namespaces are permitted on a "
    "<distribMultivariateDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520802
  { DistribDistribMultivariateDistributionAllowedCoreElements,
    "Core elements allowed on <distribMultivariateDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribMultivariateDistribution> object may have the optional SBML "
    "Level 3 Core subobjects for notes and annotations. No other elements from "
    "the SBML Level 3 Core namespaces are permitted on a "
    "<distribMultivariateDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520901
  { DistribDistribContinuousUnivariateDistributionAllowedCoreAttributes,
    "Core attributes allowed on <distribContinuousUnivariateDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribContinuousUnivariateDistribution> object may have the optional "
    "SBML Level 3 Core attributes 'metaid' and 'sboTerm'. No other attributes "
    "from the SBML Level 3 Core namespaces are permitted on a "
    "<distribContinuousUnivariateDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520902
  { DistribDistribContinuousUnivariateDistributionAllowedCoreElements,
    "Core elements allowed on <distribContinuousUnivariateDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribContinuousUnivariateDistribution> object may have the optional "
    "SBML Level 3 Core subobjects for notes and annotations. No other elements "
    "from the SBML Level 3 Core namespaces are permitted on a "
    "<distribContinuousUnivariateDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520903
  { DistribDistribContinuousUnivariateDistributionAllowedAttributes,
    "Attributes allowed on <distribContinuousUnivariateDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribContinuousUnivariateDistribution> object may have the optional "
    "attributes 'distrib:id', 'distrib:name', 'distrib:id' and 'distrib:name'. "
    "No other attributes from the SBML Level 3 Distributions namespaces are "
    "permitted on a <distribContinuousUnivariateDistribution> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520904
  { DistribDistribContinuousUnivariateDistributionAllowedElements,
    "Elements allowed on <distribContinuousUnivariateDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribContinuousUnivariateDistribution> object may contain one and "
    "only one instance of each of the DistribUncertBound, DistribUncertBound, "
    "DistribUncertBound and DistribUncertBound elements. No other elements from "
    "the SBML Level 3 Distributions namespaces are permitted on a "
    "<distribContinuousUnivariateDistribution> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520905
  { DistribDistribContinuousUnivariateDistributionNameMustBeString,
    "The 'name' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'distrib:name' on a "
    "<distribContinuousUnivariateDistribution> must have a value of data type "
    "'string'.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1521001
  { DistribDistribDiscreteUnivariateDistributionAllowedCoreAttributes,
    "Core attributes allowed on <distribDiscreteUnivariateDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribDiscreteUnivariateDistribution> object may have the optional "
    "SBML Level 3 Core attributes 'metaid' and 'sboTerm'. No other attributes "
    "from the SBML Level 3 Core namespaces are permitted on a "
    "<distribDiscreteUnivariateDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1521002
  { DistribDistribDiscreteUnivariateDistributionAllowedCoreElements,
    "Core elements allowed on <distribDiscreteUnivariateDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribDiscreteUnivariateDistribution> object may have the optional "
    "SBML Level 3 Core subobjects for notes and annotations. No other elements "
    "from the SBML Level 3 Core namespaces are permitted on a "
    "<distribDiscreteUnivariateDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1521003
  { DistribDistribDiscreteUnivariateDistributionAllowedAttributes,
    "Attributes allowed on <distribDiscreteUnivariateDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribDiscreteUnivariateDistribution> object may have the optional "
    "attributes 'distrib:id', 'distrib:name', 'distrib:id' and 'distrib:name'. "
    "No other attributes from the SBML Level 3 Distributions namespaces are "
    "permitted on a <distribDiscreteUnivariateDistribution> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1521004
  { DistribDistribDiscreteUnivariateDistributionAllowedElements,
    "Elements allowed on <distribDiscreteUnivariateDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribDiscreteUnivariateDistribution> object may contain one and only "
    "one instance of each of the DistribUncertBound, DistribUncertBound, "
    "DistribUncertBound and DistribUncertBound elements. No other elements from "
    "the SBML Level 3 Distributions namespaces are permitted on a "
    "<distribDiscreteUnivariateDistribution> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1521005
  { DistribDistribDiscreteUnivariateDistributionNameMustBeString,
    "The 'name' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'distrib:name' on a <distribDiscreteUnivariateDistribution> "
    "must have a value of data type 'string'.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1521101
  { DistribDistribCategoricalUnivariateDistributionAllowedCoreAttributes,
    "Core attributes allowed on <distribCategoricalUnivariateDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribCategoricalUnivariateDistribution> object may have the optional "
    "SBML Level 3 Core attributes 'metaid' and 'sboTerm'. No other attributes "
    "from the SBML Level 3 Core namespaces are permitted on a "
    "<distribCategoricalUnivariateDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1521102
  { DistribDistribCategoricalUnivariateDistributionAllowedCoreElements,
    "Core elements allowed on <distribCategoricalUnivariateDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribCategoricalUnivariateDistribution> object may have the optional "
    "SBML Level 3 Core subobjects for notes and annotations. No other elements "
    "from the SBML Level 3 Core namespaces are permitted on a "
    "<distribCategoricalUnivariateDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1521201
  { DistribDistribUncertValueAllowedCoreAttributes,
    "Core attributes allowed on <distribUncertValue>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribUncertValue> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <distribUncertValue>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1521202
  { DistribDistribUncertValueAllowedCoreElements,
    "Core elements allowed on <distribUncertValue>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribUncertValue> object may have the optional SBML Level 3 Core "
    "subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a <distribUncertValue>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1521203
  { DistribDistribUncertValueAllowedAttributes,
    "Attributes allowed on <distribUncertValue>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribUncertValue> object may have the optional attributes "
    "'distrib:id', 'distrib:name', 'distrib:value', 'distrib:var', "
    "'distrib:units', 'distrib:id', 'distrib:name', 'distrib:value', "
    "'distrib:var' and 'distrib:units'. No other attributes from the SBML Level "
    "3 Distributions namespaces are permitted on a <distribUncertValue> object. "
    "",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1521204
  { DistribDistribUncertValueNameMustBeString,
    "The 'name' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'distrib:name' on a <distribUncertValue> must have a value "
    "of data type 'string'.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1521205
  { DistribDistribUncertValueValueMustBeDouble,
    "The 'value' attribute must be Double.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'distrib:value' on a <distribUncertValue> must have a value "
    "of data type 'double'.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1521206
  { DistribDistribUncertValueVarMustBeSBase,
    "The attribute 'var' must point to SBase object.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'distrib:var' of a <distribUncertValue> object "
    "must be the identifier of an existing object derived from the 'SBase' "
    "class and defined in the enclosing <model> object.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1521207
  { DistribDistribUncertValueUnitsMustBeUnitSId,
    "The 'units' attribute must be UnitSId.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'distrib:units' on a <distribUncertValue> must "
    "have a taken from the following: the identifier of a <unitDefinition> "
    "object in the enclosing <model,> or one of the base units in SBML.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1521301
  { DistribDistribUncertBoundAllowedCoreAttributes,
    "Core attributes allowed on <distribUncertBound>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribUncertBound> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <distribUncertBound>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1521302
  { DistribDistribUncertBoundAllowedCoreElements,
    "Core elements allowed on <distribUncertBound>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribUncertBound> object may have the optional SBML Level 3 Core "
    "subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a <distribUncertBound>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1521303
  { DistribDistribUncertBoundAllowedAttributes,
    "Attributes allowed on <distribUncertBound>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribUncertBound> object must have the required attributes "
    "'distrib:inclusive' and 'distrib:inclusive', and may have the optional "
    "attributes 'distrib:id', 'distrib:name', 'distrib:id' and 'distrib:name'. "
    "No other attributes from the SBML Level 3 Distributions namespaces are "
    "permitted on a <distribUncertBound> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1521304
  { DistribDistribUncertBoundInclusiveMustBeBoolean,
    "The 'inclusive' attribute must be Boolean.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'distrib:inclusive' on a <distribUncertBound> must have a "
    "value of data type 'boolean'.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1521306
  { DistribDistribUncertBoundNameMustBeString,
    "The 'name' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'distrib:name' on a <distribUncertBound> must have a value "
    "of data type 'string'.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1521401
  { DistribDistribExternalDistributionAllowedCoreAttributes,
    "Core attributes allowed on <distribExternalDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribExternalDistribution> object may have the optional SBML Level 3 "
    "Core attributes 'metaid' and 'sboTerm'. No other attributes from the SBML "
    "Level 3 Core namespaces are permitted on a "
    "<distribExternalDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1521402
  { DistribDistribExternalDistributionAllowedCoreElements,
    "Core elements allowed on <distribExternalDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribExternalDistribution> object may have the optional SBML Level 3 "
    "Core subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a "
    "<distribExternalDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1521403
  { DistribDistribExternalDistributionAllowedAttributes,
    "Attributes allowed on <distribExternalDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribExternalDistribution> object must have the required attributes "
    "'distrib:definitionURL' and 'distrib:definitionURL', and may have the "
    "optional attributes 'distrib:id', 'distrib:name', 'distrib:id' and "
    "'distrib:name'. No other attributes from the SBML Level 3 Distributions "
    "namespaces are permitted on a <distribExternalDistribution> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1521404
  { DistribDistribExternalDistributionAllowedElements,
    "Elements allowed on <distribExternalDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribExternalDistribution> object may contain one and only one "
    "instance of each of the <listOfDistribExternalParameters> and "
    "<listOfDistribExternalParameters> elements. No other elements from the "
    "SBML Level 3 Distributions namespaces are permitted on a "
    "<distribExternalDistribution> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1521405
  { DistribDistribExternalDistributionDefinitionURLMustBeString,
    "The 'definitionURL' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'distrib:definitionURL' on a <distribExternalDistribution> "
    "must have a value of data type 'string'.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1521407
  { DistribDistribExternalDistributionNameMustBeString,
    "The 'name' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'distrib:name' on a <distribExternalDistribution> must have "
    "a value of data type 'string'.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1521409
  {
    DistribDistribExternalDistributionLODistribExternalParametersAllowedCoreElements,
    "Core elements allowed on <listOfDistribExternalParameters>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Apart from the general notes and annotations subobjects permitted on all "
    "SBML objects, a <listOfDistribExternalParameters> container object may "
    "only contain <distribExternalParameter> objects.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1521411
  {
    DistribDistribExternalDistributionLODistribExternalParametersAllowedCoreAttributes,
    "Core attributes allowed on <listOfDistribExternalParameters>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <listOfDistribExternalParameters> object may have the optional SBML "
    "Level 3 Core attributes 'metaid' and 'sboTerm'. No other attributes from "
    "the SBML Level 3 Core namespaces are permitted on a "
    "<listOfDistribExternalParameters> object.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1521501
  { DistribDistribExternalParameterAllowedCoreAttributes,
    "Core attributes allowed on <distribExternalParameter>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribExternalParameter> object may have the optional SBML Level 3 "
    "Core attributes 'metaid' and 'sboTerm'. No other attributes from the SBML "
    "Level 3 Core namespaces are permitted on a <distribExternalParameter>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1521502
  { DistribDistribExternalParameterAllowedCoreElements,
    "Core elements allowed on <distribExternalParameter>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribExternalParameter> object may have the optional SBML Level 3 "
    "Core subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a <distribExternalParameter>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1521503
  { DistribDistribExternalParameterAllowedAttributes,
    "Attributes allowed on <distribExternalParameter>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribExternalParameter> object must have the required attributes "
    "'distrib:definitionURL' and 'distrib:definitionURL', and may have the "
    "optional attributes 'distrib:id', 'distrib:name', 'distrib:id' and "
    "'distrib:name'. No other attributes from the SBML Level 3 Distributions "
    "namespaces are permitted on a <distribExternalParameter> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1521504
  { DistribDistribExternalParameterAllowedElements,
    "Elements allowed on <distribExternalParameter>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribExternalParameter> object may contain one and only one instance "
    "of each of the <listOfDistribExternalParameters> and "
    "<listOfDistribExternalParameters> elements. No other elements from the "
    "SBML Level 3 Distributions namespaces are permitted on a "
    "<distribExternalParameter> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1521505
  { DistribDistribExternalParameterDefinitionURLMustBeString,
    "The 'definitionURL' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'distrib:definitionURL' on a <distribExternalParameter> must "
    "have a value of data type 'string'.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1521507
  { DistribDistribExternalParameterNameMustBeString,
    "The 'name' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'distrib:name' on a <distribExternalParameter> must have a "
    "value of data type 'string'.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1521509
  {
    DistribDistribExternalParameterLODistribExternalParametersAllowedCoreElements,
    "Core elements allowed on <listOfDistribExternalParameters>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Apart from the general notes and annotations subobjects permitted on all "
    "SBML objects, a <listOfDistribExternalParameters> container object may "
    "only contain <distribExternalParameter> objects.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1521511
  {
    DistribDistribExternalParameterLODistribExternalParametersAllowedCoreAttributes,
    "Core attributes allowed on <listOfDistribExternalParameters>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <listOfDistribExternalParameters> object may have the optional SBML "
    "Level 3 Core attributes 'metaid' and 'sboTerm'. No other attributes from "
    "the SBML Level 3 Core namespaces are permitted on a "
    "<listOfDistribExternalParameters> object.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1521601
  { DistribDistribNormalDistributionAllowedCoreAttributes,
    "Core attributes allowed on <distribNormalDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribNormalDistribution> object may have the optional SBML Level 3 "
    "Core attributes 'metaid' and 'sboTerm'. No other attributes from the SBML "
    "Level 3 Core namespaces are permitted on a <distribNormalDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1521602
  { DistribDistribNormalDistributionAllowedCoreElements,
    "Core elements allowed on <distribNormalDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribNormalDistribution> object may have the optional SBML Level 3 "
    "Core subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a <distribNormalDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1521603
  { DistribDistribNormalDistributionAllowedAttributes,
    "Attributes allowed on <distribNormalDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribNormalDistribution> object may have the optional attributes "
    "'distrib:id', 'distrib:name', 'distrib:id' and 'distrib:name'. No other "
    "attributes from the SBML Level 3 Distributions namespaces are permitted on "
    "a <distribNormalDistribution> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1521604
  { DistribDistribNormalDistributionAllowedElements,
    "Elements allowed on <distribNormalDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribNormalDistribution> object must contain one and only one "
    "instance of each of the DistribUncertValue and <distribUncertValue> "
    "elements, and may contain one and only one instance of each of the "
    "DistribUncertValue, DistribUncertValue, DistribUncertValue and "
    "DistribUncertValue elements. No other elements from the SBML Level 3 "
    "Distributions namespaces are permitted on a <distribNormalDistribution> "
    "object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1521605
  { DistribDistribNormalDistributionNameMustBeString,
    "The 'name' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'distrib:name' on a <distribNormalDistribution> must have a "
    "value of data type 'string'.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1521701
  { DistribDistribUniformDistributionAllowedCoreAttributes,
    "Core attributes allowed on <distribUniformDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribUniformDistribution> object may have the optional SBML Level 3 "
    "Core attributes 'metaid' and 'sboTerm'. No other attributes from the SBML "
    "Level 3 Core namespaces are permitted on a <distribUniformDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1521702
  { DistribDistribUniformDistributionAllowedCoreElements,
    "Core elements allowed on <distribUniformDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribUniformDistribution> object may have the optional SBML Level 3 "
    "Core subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a <distribUniformDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1521703
  { DistribDistribUniformDistributionAllowedAttributes,
    "Attributes allowed on <distribUniformDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribUniformDistribution> object may have the optional attributes "
    "'distrib:id', 'distrib:name', 'distrib:id' and 'distrib:name'. No other "
    "attributes from the SBML Level 3 Distributions namespaces are permitted on "
    "a <distribUniformDistribution> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1521704
  { DistribDistribUniformDistributionAllowedElements,
    "Elements allowed on <distribUniformDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribUniformDistribution> object must contain one and only one "
    "instance of each of the DistribUncertValue, DistribUncertValue, "
    "DistribUncertValue and <distribUncertValue> elements, and may contain one "
    "and only one instance of each of the DistribUncertValue and "
    "DistribUncertValue elements. No other elements from the SBML Level 3 "
    "Distributions namespaces are permitted on a <distribUniformDistribution> "
    "object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1521705
  { DistribDistribUniformDistributionNameMustBeString,
    "The 'name' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'distrib:name' on a <distribUniformDistribution> must have a "
    "value of data type 'string'.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1521801
  { DistribDistribCategoricalDistributionAllowedCoreAttributes,
    "Core attributes allowed on <distribCategoricalDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribCategoricalDistribution> object may have the optional SBML Level "
    "3 Core attributes 'metaid' and 'sboTerm'. No other attributes from the "
    "SBML Level 3 Core namespaces are permitted on a "
    "<distribCategoricalDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1521802
  { DistribDistribCategoricalDistributionAllowedCoreElements,
    "Core elements allowed on <distribCategoricalDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribCategoricalDistribution> object may have the optional SBML Level "
    "3 Core subobjects for notes and annotations. No other elements from the "
    "SBML Level 3 Core namespaces are permitted on a "
    "<distribCategoricalDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1521803
  { DistribDistribCategoricalDistributionAllowedAttributes,
    "Attributes allowed on <distribCategoricalDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribCategoricalDistribution> object may have the optional attributes "
    "'distrib:id', 'distrib:name', 'distrib:id' and 'distrib:name'. No other "
    "attributes from the SBML Level 3 Distributions namespaces are permitted on "
    "a <distribCategoricalDistribution> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1521804
  { DistribDistribCategoricalDistributionAllowedElements,
    "Elements allowed on <distribCategoricalDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribCategoricalDistribution> object must contain one and only one "
    "instance of each of the <listOfDistribCategories> and "
    "<ListOfDistribCategories> elements. No other elements from the SBML Level "
    "3 Distributions namespaces are permitted on a "
    "<distribCategoricalDistribution> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1521805
  { DistribDistribCategoricalDistributionNameMustBeString,
    "The 'name' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'distrib:name' on a <distribCategoricalDistribution> must "
    "have a value of data type 'string'.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1521807
  { DistribDistribCategoricalDistributionEmptyReqdLOElements,
    "No Empty ListOf elements allowed on <distribCategoricalDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The <listOfDistribCategories> and <listOfDistribCategories> subobjects on "
    "a <distribCategoricalDistribution> object must not be empty.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1521808
  {
    DistribDistribCategoricalDistributionLODistribCategoriesAllowedCoreElements,
    "Core elements allowed on <listOfDistribCategories>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Apart from the general notes and annotations subobjects permitted on all "
    "SBML objects, a <listOfDistribCategories> container object may only "
    "contain <distribCategory> objects.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1521810
  {
    DistribDistribCategoricalDistributionLODistribCategoriesAllowedCoreAttributes,
    "Core attributes allowed on <listOfDistribCategories>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <listOfDistribCategories> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <listOfDistribCategories> object.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1521901
  { DistribDistribCategoryAllowedCoreAttributes,
    "Core attributes allowed on <distribCategory>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribCategory> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <distribCategory>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1521902
  { DistribDistribCategoryAllowedCoreElements,
    "Core elements allowed on <distribCategory>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribCategory> object may have the optional SBML Level 3 Core "
    "subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a <distribCategory>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1521903
  { DistribDistribCategoryAllowedAttributes,
    "Attributes allowed on <distribCategory>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribCategory> object may have the optional attributes 'distrib:id', "
    "'distrib:name', 'distrib:rank', 'distrib:id', 'distrib:name' and "
    "'distrib:rank'. No other attributes from the SBML Level 3 Distributions "
    "namespaces are permitted on a <distribCategory> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1521904
  { DistribDistribCategoryAllowedElements,
    "Elements allowed on <distribCategory>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribCategory> object must contain one and only one instance of each "
    "of the DistribUncertValue and <distribUncertValue> elements, and may "
    "contain one and only one instance of each of the DistribUncertValue and "
    "DistribUncertValue elements. No other elements from the SBML Level 3 "
    "Distributions namespaces are permitted on a <distribCategory> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1521905
  { DistribDistribCategoryNameMustBeString,
    "The 'name' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'distrib:name' on a <distribCategory> must have a value of "
    "data type 'string'.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1521906
  { DistribDistribCategoryRankMustBeNonNegativeInteger,
    "The 'rank' attribute must be NonNegativeInteger.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'distrib:rank' on a <distribCategory> must have a value of "
    "data type 'integer', and must be non negative.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1522001
  { DistribDistribBernoulliDistributionAllowedCoreAttributes,
    "Core attributes allowed on <distribBernoulliDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribBernoulliDistribution> object may have the optional SBML Level 3 "
    "Core attributes 'metaid' and 'sboTerm'. No other attributes from the SBML "
    "Level 3 Core namespaces are permitted on a "
    "<distribBernoulliDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1522002
  { DistribDistribBernoulliDistributionAllowedCoreElements,
    "Core elements allowed on <distribBernoulliDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribBernoulliDistribution> object may have the optional SBML Level 3 "
    "Core subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a "
    "<distribBernoulliDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1522003
  { DistribDistribBernoulliDistributionAllowedAttributes,
    "Attributes allowed on <distribBernoulliDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribBernoulliDistribution> object may have the optional attributes "
    "'distrib:id', 'distrib:name', 'distrib:id' and 'distrib:name'. No other "
    "attributes from the SBML Level 3 Distributions namespaces are permitted on "
    "a <distribBernoulliDistribution> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1522004
  { DistribDistribBernoulliDistributionAllowedElements,
    "Elements allowed on <distribBernoulliDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribBernoulliDistribution> object must contain one and only one "
    "instance of each of the DistribUncertValue and <distribUncertValue> "
    "elements. No other elements from the SBML Level 3 Distributions namespaces "
    "are permitted on a <distribBernoulliDistribution> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1522005
  { DistribDistribBernoulliDistributionNameMustBeString,
    "The 'name' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'distrib:name' on a <distribBernoulliDistribution> must have "
    "a value of data type 'string'.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1522101
  { DistribDistribBetaDistributionAllowedCoreAttributes,
    "Core attributes allowed on <distribBetaDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribBetaDistribution> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <distribBetaDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1522102
  { DistribDistribBetaDistributionAllowedCoreElements,
    "Core elements allowed on <distribBetaDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribBetaDistribution> object may have the optional SBML Level 3 Core "
    "subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a <distribBetaDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1522103
  { DistribDistribBetaDistributionAllowedAttributes,
    "Attributes allowed on <distribBetaDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribBetaDistribution> object may have the optional attributes "
    "'distrib:id', 'distrib:name', 'distrib:id' and 'distrib:name'. No other "
    "attributes from the SBML Level 3 Distributions namespaces are permitted on "
    "a <distribBetaDistribution> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1522104
  { DistribDistribBetaDistributionAllowedElements,
    "Elements allowed on <distribBetaDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribBetaDistribution> object must contain one and only one instance "
    "of each of the DistribUncertValue, DistribUncertValue, DistribUncertValue "
    "and <distribUncertValue> elements. No other elements from the SBML Level 3 "
    "Distributions namespaces are permitted on a <distribBetaDistribution> "
    "object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1522105
  { DistribDistribBetaDistributionNameMustBeString,
    "The 'name' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'distrib:name' on a <distribBetaDistribution> must have a "
    "value of data type 'string'.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1522201
  { DistribDistribBinomialDistributionAllowedCoreAttributes,
    "Core attributes allowed on <distribBinomialDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribBinomialDistribution> object may have the optional SBML Level 3 "
    "Core attributes 'metaid' and 'sboTerm'. No other attributes from the SBML "
    "Level 3 Core namespaces are permitted on a "
    "<distribBinomialDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1522202
  { DistribDistribBinomialDistributionAllowedCoreElements,
    "Core elements allowed on <distribBinomialDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribBinomialDistribution> object may have the optional SBML Level 3 "
    "Core subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a "
    "<distribBinomialDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1522203
  { DistribDistribBinomialDistributionAllowedAttributes,
    "Attributes allowed on <distribBinomialDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribBinomialDistribution> object may have the optional attributes "
    "'distrib:id', 'distrib:name', 'distrib:id' and 'distrib:name'. No other "
    "attributes from the SBML Level 3 Distributions namespaces are permitted on "
    "a <distribBinomialDistribution> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1522204
  { DistribDistribBinomialDistributionAllowedElements,
    "Elements allowed on <distribBinomialDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribBinomialDistribution> object must contain one and only one "
    "instance of each of the DistribUncertValue, DistribUncertValue, "
    "DistribUncertValue and <distribUncertValue> elements. No other elements "
    "from the SBML Level 3 Distributions namespaces are permitted on a "
    "<distribBinomialDistribution> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1522205
  { DistribDistribBinomialDistributionNameMustBeString,
    "The 'name' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'distrib:name' on a <distribBinomialDistribution> must have "
    "a value of data type 'string'.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1522301
  { DistribDistribCauchyDistributionAllowedCoreAttributes,
    "Core attributes allowed on <distribCauchyDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribCauchyDistribution> object may have the optional SBML Level 3 "
    "Core attributes 'metaid' and 'sboTerm'. No other attributes from the SBML "
    "Level 3 Core namespaces are permitted on a <distribCauchyDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1522302
  { DistribDistribCauchyDistributionAllowedCoreElements,
    "Core elements allowed on <distribCauchyDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribCauchyDistribution> object may have the optional SBML Level 3 "
    "Core subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a <distribCauchyDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1522303
  { DistribDistribCauchyDistributionAllowedAttributes,
    "Attributes allowed on <distribCauchyDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribCauchyDistribution> object may have the optional attributes "
    "'distrib:id', 'distrib:name', 'distrib:id' and 'distrib:name'. No other "
    "attributes from the SBML Level 3 Distributions namespaces are permitted on "
    "a <distribCauchyDistribution> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1522304
  { DistribDistribCauchyDistributionAllowedElements,
    "Elements allowed on <distribCauchyDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribCauchyDistribution> object must contain one and only one "
    "instance of each of the DistribUncertValue, DistribUncertValue, "
    "DistribUncertValue and <distribUncertValue> elements. No other elements "
    "from the SBML Level 3 Distributions namespaces are permitted on a "
    "<distribCauchyDistribution> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1522305
  { DistribDistribCauchyDistributionNameMustBeString,
    "The 'name' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'distrib:name' on a <distribCauchyDistribution> must have a "
    "value of data type 'string'.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1522401
  { DistribDistribChiSquareDistributionAllowedCoreAttributes,
    "Core attributes allowed on <distribChiSquareDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribChiSquareDistribution> object may have the optional SBML Level 3 "
    "Core attributes 'metaid' and 'sboTerm'. No other attributes from the SBML "
    "Level 3 Core namespaces are permitted on a "
    "<distribChiSquareDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1522402
  { DistribDistribChiSquareDistributionAllowedCoreElements,
    "Core elements allowed on <distribChiSquareDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribChiSquareDistribution> object may have the optional SBML Level 3 "
    "Core subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a "
    "<distribChiSquareDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1522403
  { DistribDistribChiSquareDistributionAllowedAttributes,
    "Attributes allowed on <distribChiSquareDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribChiSquareDistribution> object may have the optional attributes "
    "'distrib:id', 'distrib:name', 'distrib:id' and 'distrib:name'. No other "
    "attributes from the SBML Level 3 Distributions namespaces are permitted on "
    "a <distribChiSquareDistribution> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1522404
  { DistribDistribChiSquareDistributionAllowedElements,
    "Elements allowed on <distribChiSquareDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribChiSquareDistribution> object must contain one and only one "
    "instance of each of the DistribUncertValue and <distribUncertValue> "
    "elements. No other elements from the SBML Level 3 Distributions namespaces "
    "are permitted on a <distribChiSquareDistribution> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1522405
  { DistribDistribChiSquareDistributionNameMustBeString,
    "The 'name' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'distrib:name' on a <distribChiSquareDistribution> must have "
    "a value of data type 'string'.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1522501
  { DistribDistribExponentialDistributionAllowedCoreAttributes,
    "Core attributes allowed on <distribExponentialDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribExponentialDistribution> object may have the optional SBML Level "
    "3 Core attributes 'metaid' and 'sboTerm'. No other attributes from the "
    "SBML Level 3 Core namespaces are permitted on a "
    "<distribExponentialDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1522502
  { DistribDistribExponentialDistributionAllowedCoreElements,
    "Core elements allowed on <distribExponentialDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribExponentialDistribution> object may have the optional SBML Level "
    "3 Core subobjects for notes and annotations. No other elements from the "
    "SBML Level 3 Core namespaces are permitted on a "
    "<distribExponentialDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1522503
  { DistribDistribExponentialDistributionAllowedAttributes,
    "Attributes allowed on <distribExponentialDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribExponentialDistribution> object may have the optional attributes "
    "'distrib:id', 'distrib:name', 'distrib:id' and 'distrib:name'. No other "
    "attributes from the SBML Level 3 Distributions namespaces are permitted on "
    "a <distribExponentialDistribution> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1522504
  { DistribDistribExponentialDistributionAllowedElements,
    "Elements allowed on <distribExponentialDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribExponentialDistribution> object must contain one and only one "
    "instance of each of the DistribUncertValue and <distribUncertValue> "
    "elements. No other elements from the SBML Level 3 Distributions namespaces "
    "are permitted on a <distribExponentialDistribution> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1522505
  { DistribDistribExponentialDistributionNameMustBeString,
    "The 'name' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'distrib:name' on a <distribExponentialDistribution> must "
    "have a value of data type 'string'.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1522601
  { DistribDistribFDistributionAllowedCoreAttributes,
    "Core attributes allowed on <distribFDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribFDistribution> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <distribFDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1522602
  { DistribDistribFDistributionAllowedCoreElements,
    "Core elements allowed on <distribFDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribFDistribution> object may have the optional SBML Level 3 Core "
    "subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a <distribFDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1522603
  { DistribDistribFDistributionAllowedAttributes,
    "Attributes allowed on <distribFDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribFDistribution> object may have the optional attributes "
    "'distrib:id', 'distrib:name', 'distrib:id' and 'distrib:name'. No other "
    "attributes from the SBML Level 3 Distributions namespaces are permitted on "
    "a <distribFDistribution> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1522604
  { DistribDistribFDistributionAllowedElements,
    "Elements allowed on <distribFDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribFDistribution> object must contain one and only one instance of "
    "each of the DistribUncertValue, DistribUncertValue, DistribUncertValue and "
    "<distribUncertValue> elements. No other elements from the SBML Level 3 "
    "Distributions namespaces are permitted on a <distribFDistribution> object. "
    "",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1522605
  { DistribDistribFDistributionNameMustBeString,
    "The 'name' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'distrib:name' on a <distribFDistribution> must have a value "
    "of data type 'string'.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1522701
  { DistribDistribGammaDistributionAllowedCoreAttributes,
    "Core attributes allowed on <distribGammaDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribGammaDistribution> object may have the optional SBML Level 3 "
    "Core attributes 'metaid' and 'sboTerm'. No other attributes from the SBML "
    "Level 3 Core namespaces are permitted on a <distribGammaDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1522702
  { DistribDistribGammaDistributionAllowedCoreElements,
    "Core elements allowed on <distribGammaDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribGammaDistribution> object may have the optional SBML Level 3 "
    "Core subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a <distribGammaDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1522703
  { DistribDistribGammaDistributionAllowedAttributes,
    "Attributes allowed on <distribGammaDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribGammaDistribution> object may have the optional attributes "
    "'distrib:id', 'distrib:name', 'distrib:id' and 'distrib:name'. No other "
    "attributes from the SBML Level 3 Distributions namespaces are permitted on "
    "a <distribGammaDistribution> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1522704
  { DistribDistribGammaDistributionAllowedElements,
    "Elements allowed on <distribGammaDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribGammaDistribution> object must contain one and only one instance "
    "of each of the DistribUncertValue, DistribUncertValue, DistribUncertValue "
    "and <distribUncertValue> elements. No other elements from the SBML Level 3 "
    "Distributions namespaces are permitted on a <distribGammaDistribution> "
    "object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1522705
  { DistribDistribGammaDistributionNameMustBeString,
    "The 'name' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'distrib:name' on a <distribGammaDistribution> must have a "
    "value of data type 'string'.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1522801
  { DistribDistribGeometricDistributionAllowedCoreAttributes,
    "Core attributes allowed on <distribGeometricDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribGeometricDistribution> object may have the optional SBML Level 3 "
    "Core attributes 'metaid' and 'sboTerm'. No other attributes from the SBML "
    "Level 3 Core namespaces are permitted on a "
    "<distribGeometricDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1522802
  { DistribDistribGeometricDistributionAllowedCoreElements,
    "Core elements allowed on <distribGeometricDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribGeometricDistribution> object may have the optional SBML Level 3 "
    "Core subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a "
    "<distribGeometricDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1522803
  { DistribDistribGeometricDistributionAllowedAttributes,
    "Attributes allowed on <distribGeometricDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribGeometricDistribution> object may have the optional attributes "
    "'distrib:id', 'distrib:name', 'distrib:id' and 'distrib:name'. No other "
    "attributes from the SBML Level 3 Distributions namespaces are permitted on "
    "a <distribGeometricDistribution> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1522804
  { DistribDistribGeometricDistributionAllowedElements,
    "Elements allowed on <distribGeometricDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribGeometricDistribution> object must contain one and only one "
    "instance of each of the DistribUncertValue and <distribUncertValue> "
    "elements. No other elements from the SBML Level 3 Distributions namespaces "
    "are permitted on a <distribGeometricDistribution> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1522805
  { DistribDistribGeometricDistributionNameMustBeString,
    "The 'name' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'distrib:name' on a <distribGeometricDistribution> must have "
    "a value of data type 'string'.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1522901
  { DistribDistribHypergeometricDistributionAllowedCoreAttributes,
    "Core attributes allowed on <distribHypergeometricDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribHypergeometricDistribution> object may have the optional SBML "
    "Level 3 Core attributes 'metaid' and 'sboTerm'. No other attributes from "
    "the SBML Level 3 Core namespaces are permitted on a "
    "<distribHypergeometricDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1522902
  { DistribDistribHypergeometricDistributionAllowedCoreElements,
    "Core elements allowed on <distribHypergeometricDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribHypergeometricDistribution> object may have the optional SBML "
    "Level 3 Core subobjects for notes and annotations. No other elements from "
    "the SBML Level 3 Core namespaces are permitted on a "
    "<distribHypergeometricDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1522903
  { DistribDistribHypergeometricDistributionAllowedAttributes,
    "Attributes allowed on <distribHypergeometricDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribHypergeometricDistribution> object may have the optional "
    "attributes 'distrib:id', 'distrib:name', 'distrib:id' and 'distrib:name'. "
    "No other attributes from the SBML Level 3 Distributions namespaces are "
    "permitted on a <distribHypergeometricDistribution> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1522904
  { DistribDistribHypergeometricDistributionAllowedElements,
    "Elements allowed on <distribHypergeometricDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribHypergeometricDistribution> object must contain one and only one "
    "instance of each of the DistribUncertValue, DistribUncertValue, "
    "DistribUncertValue, DistribUncertValue, DistribUncertValue and "
    "<distribUncertValue> elements. No other elements from the SBML Level 3 "
    "Distributions namespaces are permitted on a "
    "<distribHypergeometricDistribution> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1522905
  { DistribDistribHypergeometricDistributionNameMustBeString,
    "The 'name' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'distrib:name' on a <distribHypergeometricDistribution> must "
    "have a value of data type 'string'.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1523001
  { DistribDistribInverseGammaDistributionAllowedCoreAttributes,
    "Core attributes allowed on <distribInverseGammaDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribInverseGammaDistribution> object may have the optional SBML "
    "Level 3 Core attributes 'metaid' and 'sboTerm'. No other attributes from "
    "the SBML Level 3 Core namespaces are permitted on a "
    "<distribInverseGammaDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1523002
  { DistribDistribInverseGammaDistributionAllowedCoreElements,
    "Core elements allowed on <distribInverseGammaDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribInverseGammaDistribution> object may have the optional SBML "
    "Level 3 Core subobjects for notes and annotations. No other elements from "
    "the SBML Level 3 Core namespaces are permitted on a "
    "<distribInverseGammaDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1523003
  { DistribDistribInverseGammaDistributionAllowedAttributes,
    "Attributes allowed on <distribInverseGammaDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribInverseGammaDistribution> object may have the optional "
    "attributes 'distrib:id', 'distrib:name', 'distrib:id' and 'distrib:name'. "
    "No other attributes from the SBML Level 3 Distributions namespaces are "
    "permitted on a <distribInverseGammaDistribution> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1523004
  { DistribDistribInverseGammaDistributionAllowedElements,
    "Elements allowed on <distribInverseGammaDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribInverseGammaDistribution> object must contain one and only one "
    "instance of each of the DistribUncertValue, DistribUncertValue, "
    "DistribUncertValue and <distribUncertValue> elements. No other elements "
    "from the SBML Level 3 Distributions namespaces are permitted on a "
    "<distribInverseGammaDistribution> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1523005
  { DistribDistribInverseGammaDistributionNameMustBeString,
    "The 'name' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'distrib:name' on a <distribInverseGammaDistribution> must "
    "have a value of data type 'string'.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1523101
  { DistribDistribLaPlaceDistributionAllowedCoreAttributes,
    "Core attributes allowed on <distribLaPlaceDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribLaPlaceDistribution> object may have the optional SBML Level 3 "
    "Core attributes 'metaid' and 'sboTerm'. No other attributes from the SBML "
    "Level 3 Core namespaces are permitted on a <distribLaPlaceDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1523102
  { DistribDistribLaPlaceDistributionAllowedCoreElements,
    "Core elements allowed on <distribLaPlaceDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribLaPlaceDistribution> object may have the optional SBML Level 3 "
    "Core subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a <distribLaPlaceDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1523103
  { DistribDistribLaPlaceDistributionAllowedAttributes,
    "Attributes allowed on <distribLaPlaceDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribLaPlaceDistribution> object may have the optional attributes "
    "'distrib:id', 'distrib:name', 'distrib:id' and 'distrib:name'. No other "
    "attributes from the SBML Level 3 Distributions namespaces are permitted on "
    "a <distribLaPlaceDistribution> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1523104
  { DistribDistribLaPlaceDistributionAllowedElements,
    "Elements allowed on <distribLaPlaceDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribLaPlaceDistribution> object must contain one and only one "
    "instance of each of the DistribUncertValue, DistribUncertValue, "
    "DistribUncertValue and <distribUncertValue> elements. No other elements "
    "from the SBML Level 3 Distributions namespaces are permitted on a "
    "<distribLaPlaceDistribution> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1523105
  { DistribDistribLaPlaceDistributionNameMustBeString,
    "The 'name' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'distrib:name' on a <distribLaPlaceDistribution> must have a "
    "value of data type 'string'.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1523201
  { DistribDistribLogNormalDistributionAllowedCoreAttributes,
    "Core attributes allowed on <distribLogNormalDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribLogNormalDistribution> object may have the optional SBML Level 3 "
    "Core attributes 'metaid' and 'sboTerm'. No other attributes from the SBML "
    "Level 3 Core namespaces are permitted on a "
    "<distribLogNormalDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1523202
  { DistribDistribLogNormalDistributionAllowedCoreElements,
    "Core elements allowed on <distribLogNormalDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribLogNormalDistribution> object may have the optional SBML Level 3 "
    "Core subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a "
    "<distribLogNormalDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1523203
  { DistribDistribLogNormalDistributionAllowedAttributes,
    "Attributes allowed on <distribLogNormalDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribLogNormalDistribution> object may have the optional attributes "
    "'distrib:id', 'distrib:name', 'distrib:id' and 'distrib:name'. No other "
    "attributes from the SBML Level 3 Distributions namespaces are permitted on "
    "a <distribLogNormalDistribution> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1523204
  { DistribDistribLogNormalDistributionAllowedElements,
    "Elements allowed on <distribLogNormalDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribLogNormalDistribution> object must contain one and only one "
    "instance of each of the DistribUncertValue, DistribUncertValue, "
    "DistribUncertValue and <distribUncertValue> elements. No other elements "
    "from the SBML Level 3 Distributions namespaces are permitted on a "
    "<distribLogNormalDistribution> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1523205
  { DistribDistribLogNormalDistributionNameMustBeString,
    "The 'name' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'distrib:name' on a <distribLogNormalDistribution> must have "
    "a value of data type 'string'.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1523301
  { DistribDistribLogisticDistributionAllowedCoreAttributes,
    "Core attributes allowed on <distribLogisticDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribLogisticDistribution> object may have the optional SBML Level 3 "
    "Core attributes 'metaid' and 'sboTerm'. No other attributes from the SBML "
    "Level 3 Core namespaces are permitted on a "
    "<distribLogisticDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1523302
  { DistribDistribLogisticDistributionAllowedCoreElements,
    "Core elements allowed on <distribLogisticDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribLogisticDistribution> object may have the optional SBML Level 3 "
    "Core subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a "
    "<distribLogisticDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1523303
  { DistribDistribLogisticDistributionAllowedAttributes,
    "Attributes allowed on <distribLogisticDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribLogisticDistribution> object may have the optional attributes "
    "'distrib:id', 'distrib:name', 'distrib:id' and 'distrib:name'. No other "
    "attributes from the SBML Level 3 Distributions namespaces are permitted on "
    "a <distribLogisticDistribution> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1523304
  { DistribDistribLogisticDistributionAllowedElements,
    "Elements allowed on <distribLogisticDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribLogisticDistribution> object must contain one and only one "
    "instance of each of the DistribUncertValue, DistribUncertValue, "
    "DistribUncertValue and <distribUncertValue> elements. No other elements "
    "from the SBML Level 3 Distributions namespaces are permitted on a "
    "<distribLogisticDistribution> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1523305
  { DistribDistribLogisticDistributionNameMustBeString,
    "The 'name' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'distrib:name' on a <distribLogisticDistribution> must have "
    "a value of data type 'string'.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1523401
  { DistribDistribNegativeBinomialDistributionAllowedCoreAttributes,
    "Core attributes allowed on <distribNegativeBinomialDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribNegativeBinomialDistribution> object may have the optional SBML "
    "Level 3 Core attributes 'metaid' and 'sboTerm'. No other attributes from "
    "the SBML Level 3 Core namespaces are permitted on a "
    "<distribNegativeBinomialDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1523402
  { DistribDistribNegativeBinomialDistributionAllowedCoreElements,
    "Core elements allowed on <distribNegativeBinomialDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribNegativeBinomialDistribution> object may have the optional SBML "
    "Level 3 Core subobjects for notes and annotations. No other elements from "
    "the SBML Level 3 Core namespaces are permitted on a "
    "<distribNegativeBinomialDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1523403
  { DistribDistribNegativeBinomialDistributionAllowedAttributes,
    "Attributes allowed on <distribNegativeBinomialDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribNegativeBinomialDistribution> object may have the optional "
    "attributes 'distrib:id', 'distrib:name', 'distrib:id' and 'distrib:name'. "
    "No other attributes from the SBML Level 3 Distributions namespaces are "
    "permitted on a <distribNegativeBinomialDistribution> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1523404
  { DistribDistribNegativeBinomialDistributionAllowedElements,
    "Elements allowed on <distribNegativeBinomialDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribNegativeBinomialDistribution> object must contain one and only "
    "one instance of each of the DistribUncertValue, DistribUncertValue, "
    "DistribUncertValue and <distribUncertValue> elements. No other elements "
    "from the SBML Level 3 Distributions namespaces are permitted on a "
    "<distribNegativeBinomialDistribution> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1523405
  { DistribDistribNegativeBinomialDistributionNameMustBeString,
    "The 'name' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'distrib:name' on a <distribNegativeBinomialDistribution> "
    "must have a value of data type 'string'.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1523501
  { DistribDistribParetoDistributionAllowedCoreAttributes,
    "Core attributes allowed on <distribParetoDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribParetoDistribution> object may have the optional SBML Level 3 "
    "Core attributes 'metaid' and 'sboTerm'. No other attributes from the SBML "
    "Level 3 Core namespaces are permitted on a <distribParetoDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1523502
  { DistribDistribParetoDistributionAllowedCoreElements,
    "Core elements allowed on <distribParetoDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribParetoDistribution> object may have the optional SBML Level 3 "
    "Core subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a <distribParetoDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1523503
  { DistribDistribParetoDistributionAllowedAttributes,
    "Attributes allowed on <distribParetoDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribParetoDistribution> object may have the optional attributes "
    "'distrib:id', 'distrib:name', 'distrib:id' and 'distrib:name'. No other "
    "attributes from the SBML Level 3 Distributions namespaces are permitted on "
    "a <distribParetoDistribution> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1523504
  { DistribDistribParetoDistributionAllowedElements,
    "Elements allowed on <distribParetoDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribParetoDistribution> object must contain one and only one "
    "instance of each of the DistribUncertValue, DistribUncertValue, "
    "DistribUncertValue and <distribUncertValue> elements. No other elements "
    "from the SBML Level 3 Distributions namespaces are permitted on a "
    "<distribParetoDistribution> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1523505
  { DistribDistribParetoDistributionNameMustBeString,
    "The 'name' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'distrib:name' on a <distribParetoDistribution> must have a "
    "value of data type 'string'.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1523601
  { DistribDistribPoissonDistributionAllowedCoreAttributes,
    "Core attributes allowed on <distribPoissonDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribPoissonDistribution> object may have the optional SBML Level 3 "
    "Core attributes 'metaid' and 'sboTerm'. No other attributes from the SBML "
    "Level 3 Core namespaces are permitted on a <distribPoissonDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1523602
  { DistribDistribPoissonDistributionAllowedCoreElements,
    "Core elements allowed on <distribPoissonDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribPoissonDistribution> object may have the optional SBML Level 3 "
    "Core subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a <distribPoissonDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1523603
  { DistribDistribPoissonDistributionAllowedAttributes,
    "Attributes allowed on <distribPoissonDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribPoissonDistribution> object may have the optional attributes "
    "'distrib:id', 'distrib:name', 'distrib:id' and 'distrib:name'. No other "
    "attributes from the SBML Level 3 Distributions namespaces are permitted on "
    "a <distribPoissonDistribution> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1523604
  { DistribDistribPoissonDistributionAllowedElements,
    "Elements allowed on <distribPoissonDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribPoissonDistribution> object must contain one and only one "
    "instance of each of the DistribUncertValue and <distribUncertValue> "
    "elements. No other elements from the SBML Level 3 Distributions namespaces "
    "are permitted on a <distribPoissonDistribution> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1523605
  { DistribDistribPoissonDistributionNameMustBeString,
    "The 'name' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'distrib:name' on a <distribPoissonDistribution> must have a "
    "value of data type 'string'.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1523701
  { DistribDistribRayleighDistributionAllowedCoreAttributes,
    "Core attributes allowed on <distribRayleighDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribRayleighDistribution> object may have the optional SBML Level 3 "
    "Core attributes 'metaid' and 'sboTerm'. No other attributes from the SBML "
    "Level 3 Core namespaces are permitted on a "
    "<distribRayleighDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1523702
  { DistribDistribRayleighDistributionAllowedCoreElements,
    "Core elements allowed on <distribRayleighDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribRayleighDistribution> object may have the optional SBML Level 3 "
    "Core subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a "
    "<distribRayleighDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1523703
  { DistribDistribRayleighDistributionAllowedAttributes,
    "Attributes allowed on <distribRayleighDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribRayleighDistribution> object may have the optional attributes "
    "'distrib:id', 'distrib:name', 'distrib:id' and 'distrib:name'. No other "
    "attributes from the SBML Level 3 Distributions namespaces are permitted on "
    "a <distribRayleighDistribution> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1523704
  { DistribDistribRayleighDistributionAllowedElements,
    "Elements allowed on <distribRayleighDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribRayleighDistribution> object must contain one and only one "
    "instance of each of the DistribUncertValue and <distribUncertValue> "
    "elements. No other elements from the SBML Level 3 Distributions namespaces "
    "are permitted on a <distribRayleighDistribution> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1523705
  { DistribDistribRayleighDistributionNameMustBeString,
    "The 'name' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'distrib:name' on a <distribRayleighDistribution> must have "
    "a value of data type 'string'.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1523801
  { DistribDistribStudentTDistributionAllowedCoreAttributes,
    "Core attributes allowed on <distribStudentTDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribStudentTDistribution> object may have the optional SBML Level 3 "
    "Core attributes 'metaid' and 'sboTerm'. No other attributes from the SBML "
    "Level 3 Core namespaces are permitted on a "
    "<distribStudentTDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1523802
  { DistribDistribStudentTDistributionAllowedCoreElements,
    "Core elements allowed on <distribStudentTDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribStudentTDistribution> object may have the optional SBML Level 3 "
    "Core subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a "
    "<distribStudentTDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1523803
  { DistribDistribStudentTDistributionAllowedAttributes,
    "Attributes allowed on <distribStudentTDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribStudentTDistribution> object may have the optional attributes "
    "'distrib:id', 'distrib:name', 'distrib:id' and 'distrib:name'. No other "
    "attributes from the SBML Level 3 Distributions namespaces are permitted on "
    "a <distribStudentTDistribution> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1523804
  { DistribDistribStudentTDistributionAllowedElements,
    "Elements allowed on <distribStudentTDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribStudentTDistribution> object must contain one and only one "
    "instance of each of the DistribUncertValue, DistribUncertValue, "
    "DistribUncertValue, DistribUncertValue, DistribUncertValue and "
    "<distribUncertValue> elements. No other elements from the SBML Level 3 "
    "Distributions namespaces are permitted on a <distribStudentTDistribution> "
    "object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1523805
  { DistribDistribStudentTDistributionNameMustBeString,
    "The 'name' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'distrib:name' on a <distribStudentTDistribution> must have "
    "a value of data type 'string'.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1523901
  { DistribDistribWeibullDistributionAllowedCoreAttributes,
    "Core attributes allowed on <distribWeibullDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribWeibullDistribution> object may have the optional SBML Level 3 "
    "Core attributes 'metaid' and 'sboTerm'. No other attributes from the SBML "
    "Level 3 Core namespaces are permitted on a <distribWeibullDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1523902
  { DistribDistribWeibullDistributionAllowedCoreElements,
    "Core elements allowed on <distribWeibullDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribWeibullDistribution> object may have the optional SBML Level 3 "
    "Core subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a <distribWeibullDistribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1523903
  { DistribDistribWeibullDistributionAllowedAttributes,
    "Attributes allowed on <distribWeibullDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribWeibullDistribution> object may have the optional attributes "
    "'distrib:id', 'distrib:name', 'distrib:id' and 'distrib:name'. No other "
    "attributes from the SBML Level 3 Distributions namespaces are permitted on "
    "a <distribWeibullDistribution> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1523904
  { DistribDistribWeibullDistributionAllowedElements,
    "Elements allowed on <distribWeibullDistribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribWeibullDistribution> object must contain one and only one "
    "instance of each of the DistribUncertValue, DistribUncertValue, "
    "DistribUncertValue and <distribUncertValue> elements. No other elements "
    "from the SBML Level 3 Distributions namespaces are permitted on a "
    "<distribWeibullDistribution> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1523905
  { DistribDistribWeibullDistributionNameMustBeString,
    "The 'name' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'distrib:name' on a <distribWeibullDistribution> must have a "
    "value of data type 'string'.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1524001
  { DistribDistribUncertaintyAllowedCoreAttributes,
    "Core attributes allowed on <distribUncertainty>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribUncertainty> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <distribUncertainty>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1524002
  { DistribDistribUncertaintyAllowedCoreElements,
    "Core elements allowed on <distribUncertainty>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribUncertainty> object may have the optional SBML Level 3 Core "
    "subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a <distribUncertainty>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1524003
  { DistribDistribUncertaintyAllowedAttributes,
    "Attributes allowed on <distribUncertainty>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribUncertainty> object may have the optional attributes "
    "'distrib:id', 'distrib:name', 'distrib:id' and 'distrib:name'. No other "
    "attributes from the SBML Level 3 Distributions namespaces are permitted on "
    "a <distribUncertainty> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1524004
  { DistribDistribUncertaintyAllowedElements,
    "Elements allowed on <distribUncertainty>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribUncertainty> object may contain one and only one instance of "
    "each of the DistribUncertStatistics, DistribDistribution, "
    "DistribUncertStatistics and DistribDistribution elements. No other "
    "elements from the SBML Level 3 Distributions namespaces are permitted on a "
    "<distribUncertainty> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1524005
  { DistribDistribUncertaintyNameMustBeString,
    "The 'name' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'distrib:name' on a <distribUncertainty> must have a value "
    "of data type 'string'.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1524101
  { DistribDistribUncertStatisticsAllowedCoreAttributes,
    "Core attributes allowed on <distribUncertStatistics>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribUncertStatistics> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <distribUncertStatistics>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1524102
  { DistribDistribUncertStatisticsAllowedCoreElements,
    "Core elements allowed on <distribUncertStatistics>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribUncertStatistics> object may have the optional SBML Level 3 Core "
    "subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a <distribUncertStatistics>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1524103
  { DistribDistribUncertStatisticsAllowedAttributes,
    "Attributes allowed on <distribUncertStatistics>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribUncertStatistics> object may have the optional attributes "
    "'distrib:id', 'distrib:name', 'distrib:id' and 'distrib:name'. No other "
    "attributes from the SBML Level 3 Distributions namespaces are permitted on "
    "a <distribUncertStatistics> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1524104
  { DistribDistribUncertStatisticsAllowedElements,
    "Elements allowed on <distribUncertStatistics>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribUncertStatistics> object may contain one and only one instance "
    "of each of the DistribUncertValue, DistribUncertValue, DistribUncertValue, "
    "DistribUncertValue, DistribUncertValue, DistribUncertValue, "
    "DistribUncertValue, DistribUncertValue, DistribUncertStatisticSpan, "
    "DistribUncertStatisticSpan, DistribUncertStatisticSpan, "
    "DistribUncertStatisticSpan, <listOfDistribExternalParameters,> "
    "DistribUncertValue, DistribUncertValue, DistribUncertValue, "
    "DistribUncertValue, DistribUncertValue, DistribUncertValue, "
    "DistribUncertValue, DistribUncertValue, DistribUncertStatisticSpan, "
    "DistribUncertStatisticSpan, DistribUncertStatisticSpan, "
    "DistribUncertStatisticSpan and <listOfDistribExternalParameters> elements. "
    "No other elements from the SBML Level 3 Distributions namespaces are "
    "permitted on a <distribUncertStatistics> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1524105
  { DistribDistribUncertStatisticsNameMustBeString,
    "The 'name' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'distrib:name' on a <distribUncertStatistics> must have a "
    "value of data type 'string'.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1524107
  {
    DistribDistribUncertStatisticsLODistribExternalParametersAllowedCoreElements,
    "Core elements allowed on <listOfDistribExternalParameters>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Apart from the general notes and annotations subobjects permitted on all "
    "SBML objects, a <listOfDistribExternalParameters> container object may "
    "only contain <distribExternalParameter> objects.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1524109
  {
    DistribDistribUncertStatisticsLODistribExternalParametersAllowedCoreAttributes,
    "Core attributes allowed on <listOfDistribExternalParameters>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <listOfDistribExternalParameters> object may have the optional SBML "
    "Level 3 Core attributes 'metaid' and 'sboTerm'. No other attributes from "
    "the SBML Level 3 Core namespaces are permitted on a "
    "<listOfDistribExternalParameters> object.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1524201
  { DistribDistribUncertStatisticSpanAllowedCoreAttributes,
    "Core attributes allowed on <distribUncertStatisticSpan>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribUncertStatisticSpan> object may have the optional SBML Level 3 "
    "Core attributes 'metaid' and 'sboTerm'. No other attributes from the SBML "
    "Level 3 Core namespaces are permitted on a <distribUncertStatisticSpan>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1524202
  { DistribDistribUncertStatisticSpanAllowedCoreElements,
    "Core elements allowed on <distribUncertStatisticSpan>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribUncertStatisticSpan> object may have the optional SBML Level 3 "
    "Core subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on a <distribUncertStatisticSpan>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1524203
  { DistribDistribUncertStatisticSpanAllowedAttributes,
    "Attributes allowed on <distribUncertStatisticSpan>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribUncertStatisticSpan> object may have the optional attributes "
    "'distrib:id', 'distrib:name', 'distrib:varLower', 'distrib:valueLower', "
    "'distrib:varUpper', 'distrib:valueUpper', 'distrib:id', 'distrib:name', "
    "'distrib:varLower', 'distrib:valueLower', 'distrib:varUpper' and "
    "'distrib:valueUpper'. No other attributes from the SBML Level 3 "
    "Distributions namespaces are permitted on a <distribUncertStatisticSpan> "
    "object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1524204
  { DistribDistribUncertStatisticSpanNameMustBeString,
    "The 'name' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'distrib:name' on a <distribUncertStatisticSpan> must have a "
    "value of data type 'string'.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1524205
  { DistribDistribUncertStatisticSpanVarLowerMustBeSBase,
    "The attribute 'varLower' must point to SBase object.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'distrib:varLower' of a "
    "<distribUncertStatisticSpan> object must be the identifier of an existing "
    "object derived from the 'SBase' class and defined in the enclosing <model> "
    "object.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1524206
  { DistribDistribUncertStatisticSpanValueLowerMustBeDouble,
    "The 'valueLower' attribute must be Double.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'distrib:valueLower' on a <distribUncertStatisticSpan> must "
    "have a value of data type 'double'.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1524207
  { DistribDistribUncertStatisticSpanVarUpperMustBeSBase,
    "The attribute 'varUpper' must point to SBase object.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'distrib:varUpper' of a "
    "<distribUncertStatisticSpan> object must be the identifier of an existing "
    "object derived from the 'SBase' class and defined in the enclosing <model> "
    "object.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1524208
  { DistribDistribUncertStatisticSpanValueUpperMustBeDouble,
    "The 'valueUpper' attribute must be Double.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'distrib:valueUpper' on a <distribUncertStatisticSpan> must "
    "have a value of data type 'double'.",
    { "L3V1 Distrib V1 Section"
    }
  },

};

/** @endcond */



LIBSBML_CPP_NAMESPACE_END




#endif /* !DistribSBMLErrorTable_H__ */


