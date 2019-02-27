/**
 * @file DistribSBMLErrorTable.h
 * @brief Definition of the DistribSBMLErrorTable class.
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
    "Version 1, an SBML document must declare "
    "'http://www.sbml.org/sbml/level3/version1/distrib/version1' as the "
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
    "'http://www.sbml.org/sbml/level3/version1/distrib/version1' namespace, "
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
  { DistribSBaseAllowedElements,
    "Elements allowed on <sBase>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A 'SBase' object may contain one and only one instance of the "
    "<uncertainty> element. No other elements from the SBML Level 3 "
    "Distributions namespaces are permitted on a 'SBase' object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520301
  { DistribDistributionAllowedCoreAttributes,
    "Core attributes allowed on <distribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribution> object may have the optional SBML Level 3 Core attributes "
    "'metaid' and 'sboTerm'. No other attributes from the SBML Level 3 Core "
    "namespaces are permitted on a <distribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520302
  { DistribDistributionAllowedCoreElements,
    "Core elements allowed on <distribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribution> object may have the optional SBML Level 3 Core subobjects "
    "for notes and annotations. No other elements from the SBML Level 3 Core "
    "namespaces are permitted on a <distribution>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520303
  { DistribDistributionAllowedElements,
    "Elements allowed on <distribution>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribution> object may contain one and only one instance of the "
    "ASTNode element. No other elements from the SBML Level 3 Distributions "
    "namespaces are permitted on a <distribution> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520401
  { DistribUncertValueAllowedCoreAttributes,
    "Core attributes allowed on <uncertValue>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An <uncertValue> object may have the optional SBML Level 3 Core attributes "
    "'metaid' and 'sboTerm'. No other attributes from the SBML Level 3 Core "
    "namespaces are permitted on an <uncertValue>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520402
  { DistribUncertValueAllowedCoreElements,
    "Core elements allowed on <uncertValue>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An <uncertValue> object may have the optional SBML Level 3 Core subobjects "
    "for notes and annotations. No other elements from the SBML Level 3 Core "
    "namespaces are permitted on an <uncertValue>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520403
  { DistribUncertValueAllowedAttributes,
    "Attributes allowed on <uncertValue>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An <uncertValue> object may have the optional attributes 'distrib:value', "
    "'distrib:var' and 'distrib:units'. No other attributes from the SBML Level "
    "3 Distributions namespaces are permitted on an <uncertValue> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520404
  { DistribUncertValueValueMustBeDouble,
    "The 'value' attribute must be Double.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'distrib:value' on an <uncertValue> must have a value of "
    "data type 'double'.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520405
  { DistribUncertValueVarMustBeSBase,
    "The attribute 'var' must point to SBase object.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'distrib:var' of an <uncertValue> object must "
    "be the identifier of an existing object derived from the 'SBase' class and "
    "defined in the enclosing <model> object.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520406
  { DistribUncertValueUnitsMustBeUnitSId,
    "The 'units' attribute must be UnitSId.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'distrib:units' on an <uncertValue> must have a "
    "taken from the following: the identifier of a <unitDefinition> object in "
    "the enclosing <model,> or one of the base units in SBML.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520501
  { DistribExternalParameterAllowedCoreAttributes,
    "Core attributes allowed on <externalParameter>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An <externalParameter> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on an <externalParameter>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520502
  { DistribExternalParameterAllowedCoreElements,
    "Core elements allowed on <externalParameter>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An <externalParameter> object may have the optional SBML Level 3 Core "
    "subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on an <externalParameter>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520503
  { DistribExternalParameterAllowedAttributes,
    "Attributes allowed on <externalParameter>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An <externalParameter> object must have the required attribute "
    "'distrib:definitionURL'. No other attributes from the SBML Level 3 "
    "Distributions namespaces are permitted on an <externalParameter> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520504
  { DistribExternalParameterAllowedElements,
    "Elements allowed on <externalParameter>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An <externalParameter> object may contain one and only one instance of the "
    "<listOfExternalParameters> element. No other elements from the SBML Level "
    "3 Distributions namespaces are permitted on an <externalParameter> object. "
    "",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520505
  { DistribExternalParameterDefinitionURLMustBeString,
    "The 'definitionURL' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'distrib:definitionURL' on an <externalParameter> must have "
    "a value of data type 'string'.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520506
  { DistribExternalParameterLOExternalParametersAllowedCoreElements,
    "Core elements allowed on <listOfExternalParameters>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Apart from the general notes and annotations subobjects permitted on all "
    "SBML objects, a <listOfExternalParameters> container object may only "
    "contain <externalParameter> objects.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520507
  { DistribExternalParameterLOExternalParametersAllowedCoreAttributes,
    "Core attributes allowed on <listOfExternalParameters>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <listOfExternalParameters> object may have the optional SBML Level 3 "
    "Core attributes 'metaid' and 'sboTerm'. No other attributes from the SBML "
    "Level 3 Core namespaces are permitted on a <listOfExternalParameters> "
    "object.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520508
  { DistribExternalParameterLOExternalParametersAllowedAttributes,
    "Attributes allowed on <listOfExternalParameters>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An <listOfExternalParameters> object may have the optional attributes "
    "'distrib:id' and 'distrib:name'. No other attributes from the SBML Level 3 "
    "Distributions namespaces are permitted on an <listOfExternalParameters> "
    "object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520509
  { DistribLOExternalParametersNameMustBeString,
    "The 'name' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'distrib:name' on an <listOfExternalParameters> must have a "
    "value of data type 'string'.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520601
  { DistribUncertaintyAllowedCoreAttributes,
    "Core attributes allowed on <uncertainty>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An <uncertainty> object may have the optional SBML Level 3 Core attributes "
    "'metaid' and 'sboTerm'. No other attributes from the SBML Level 3 Core "
    "namespaces are permitted on an <uncertainty>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520602
  { DistribUncertaintyAllowedCoreElements,
    "Core elements allowed on <uncertainty>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An <uncertainty> object may have the optional SBML Level 3 Core subobjects "
    "for notes and annotations. No other elements from the SBML Level 3 Core "
    "namespaces are permitted on an <uncertainty>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520603
  { DistribUncertaintyAllowedElements,
    "Elements allowed on <uncertainty>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An <uncertainty> object may contain one and only one instance of each of "
    "the UncertValue, UncertValue, UncertValue, UncertValue, UncertValue, "
    "UncertValue, UncertValue, UncertValue, UncertValue, UncertValue, "
    "UncertStatisticSpan, UncertStatisticSpan, UncertStatisticSpan, "
    "UncertStatisticSpan, <listOfExternalParameters> and Distribution elements. "
    "No other elements from the SBML Level 3 Distributions namespaces are "
    "permitted on an <uncertainty> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520604
  { DistribUncertaintyLOExternalParametersAllowedCoreElements,
    "Core elements allowed on <listOfExternalParameters>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Apart from the general notes and annotations subobjects permitted on all "
    "SBML objects, a <listOfExternalParameters> container object may only "
    "contain <externalParameter> objects.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520605
  { DistribUncertaintyLOExternalParametersAllowedCoreAttributes,
    "Core attributes allowed on <listOfExternalParameters>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <listOfExternalParameters> object may have the optional SBML Level 3 "
    "Core attributes 'metaid' and 'sboTerm'. No other attributes from the SBML "
    "Level 3 Core namespaces are permitted on a <listOfExternalParameters> "
    "object.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520606
  { DistribUncertaintyLOExternalParametersAllowedAttributes,
    "Attributes allowed on <listOfExternalParameters>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An <listOfExternalParameters> object may have the optional attributes "
    "'distrib:id' and 'distrib:name'. No other attributes from the SBML Level 3 "
    "Distributions namespaces are permitted on an <listOfExternalParameters> "
    "object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520701
  { DistribUncertStatisticSpanAllowedCoreAttributes,
    "Core attributes allowed on <uncertStatisticSpan>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An <uncertStatisticSpan> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on an <uncertStatisticSpan>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520702
  { DistribUncertStatisticSpanAllowedCoreElements,
    "Core elements allowed on <uncertStatisticSpan>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An <uncertStatisticSpan> object may have the optional SBML Level 3 Core "
    "subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on an <uncertStatisticSpan>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520703
  { DistribUncertStatisticSpanAllowedAttributes,
    "Attributes allowed on <uncertStatisticSpan>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An <uncertStatisticSpan> object may have the optional attributes "
    "'distrib:varLower', 'distrib:valueLower', 'distrib:varUpper' and "
    "'distrib:valueUpper'. No other attributes from the SBML Level 3 "
    "Distributions namespaces are permitted on an <uncertStatisticSpan> object. "
    "",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520704
  { DistribUncertStatisticSpanVarLowerMustBeSBase,
    "The attribute 'varLower' must point to SBase object.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'distrib:varLower' of an <uncertStatisticSpan> "
    "object must be the identifier of an existing object derived from the "
    "'SBase' class and defined in the enclosing <model> object.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520705
  { DistribUncertStatisticSpanValueLowerMustBeDouble,
    "The 'valueLower' attribute must be Double.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'distrib:valueLower' on an <uncertStatisticSpan> must have a "
    "value of data type 'double'.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520706
  { DistribUncertStatisticSpanVarUpperMustBeSBase,
    "The attribute 'varUpper' must point to SBase object.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'distrib:varUpper' of an <uncertStatisticSpan> "
    "object must be the identifier of an existing object derived from the "
    "'SBase' class and defined in the enclosing <model> object.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520707
  { DistribUncertStatisticSpanValueUpperMustBeDouble,
    "The 'valueUpper' attribute must be Double.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'distrib:valueUpper' on an <uncertStatisticSpan> must have a "
    "value of data type 'double'.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520801
  { DistribDistribBaseAllowedCoreAttributes,
    "Core attributes allowed on <distribBase>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribBase> object may have the optional SBML Level 3 Core attributes "
    "'metaid' and 'sboTerm'. No other attributes from the SBML Level 3 Core "
    "namespaces are permitted on a <distribBase>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520802
  { DistribDistribBaseAllowedCoreElements,
    "Core elements allowed on <distribBase>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribBase> object may have the optional SBML Level 3 Core subobjects "
    "for notes and annotations. No other elements from the SBML Level 3 Core "
    "namespaces are permitted on a <distribBase>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520803
  { DistribDistribBaseAllowedAttributes,
    "Attributes allowed on <distribBase>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <distribBase> object may have the optional attributes 'distrib:id' and "
    "'distrib:name'. No other attributes from the SBML Level 3 Distributions "
    "namespaces are permitted on a <distribBase> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520804
  { DistribDistribBaseNameMustBeString,
    "The 'name' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'distrib:name' on a <distribBase> must have a value of data "
    "type 'string'.",
    { "L3V1 Distrib V1 Section"
    }
  },

};

/** @endcond */



LIBSBML_CPP_NAMESPACE_END




#endif /* !DistribSBMLErrorTable_H__ */


