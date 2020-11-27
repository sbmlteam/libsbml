/**
 * @file DistribSBMLErrorTable.h
 * @brief Definition of the DistribSBMLErrorTable class.
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
    "<listOfUncertainties> element. No other elements from the SBML Level 3 "
    "Distributions namespaces are permitted on a 'SBase' object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520202
  { DistribSBaseLOUncertaintiesAllowedCoreElements,
    "Core elements allowed on <sBase>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Apart from the general notes and annotations subobjects permitted on all "
    "SBML objects, a <listOfUncertainties> container object may only contain "
    "<uncertainty> objects.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520203
  { DistribSBaseLOUncertaintiesAllowedCoreAttributes,
    "Core attributes allowed on <sBase>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <listOfUncertainties> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <listOfUncertainties> object.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520301
  { DistribUncertParameterAllowedCoreAttributes,
    "Core attributes allowed on <uncertParameter>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An <uncertParameter> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on an <uncertParameter>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520302
  { DistribUncertParameterAllowedCoreElements,
    "Core elements allowed on <uncertParameter>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An <uncertParameter> object may have the optional SBML Level 3 Core "
    "subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespaces are permitted on an <uncertParameter>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520303
  { DistribUncertParameterAllowedAttributes,
    "Attributes allowed on <uncertParameter>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An <uncertParameter> object may have the optional attributes "
    "'distrib:value', 'distrib:var', 'distrib:units', 'distrib:type' and "
    "'distrib:definitionURL'. No other attributes from the SBML Level 3 "
    "Distributions namespaces are permitted on an <uncertParameter> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520304
  { DistribUncertParameterAllowedElements,
    "Elements allowed on <uncertParameter>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An <uncertParameter> object may contain one and only one instance of the "
    "<listOfUncertParameters> element. No other elements from the SBML Level 3 "
    "Distributions namespaces are permitted on an <uncertParameter> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520305
  { DistribUncertParameterValueMustBeDouble,
    "The 'value' attribute must be Double.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'distrib:value' on an <uncertParameter> must have a value of "
    "data type 'double'.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520306
  { DistribUncertParameterVarMustBeSBase,
    "The attribute 'var' must point to SBase object.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'distrib:var' of an <uncertParameter> object "
    "must be the identifier of an existing object derived from the 'SBase' "
    "class and defined in the enclosing <model> object.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520307
  { DistribUncertParameterUnitsMustBeUnitSId,
    "The 'units' attribute must be UnitSId.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'distrib:units' on an <uncertParameter> must "
    "have a taken from the following: the identifier of a <unitDefinition> "
    "object in the enclosing <model,> or one of the base units in SBML.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520308
  { DistribUncertParameterTypeMustBeUncertTypeEnum,
    "The 'type' attribute must be UncertTypeEnum.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'distrib:type' of an <uncertParameter> object "
    "must conform to the syntax of SBML data type 'UncertType' and may only "
    "take on the allowed values of 'UncertType' defined in SBML; that is, the "
    "value must be one of the following: 'distribution', 'externalParameter', "
    "'coeffientOfVariation', 'kurtosis', 'mean', 'median', 'mode', "
    "'sampleSize', 'skewness', 'standardDeviation', 'standardError', "
    "'variance', 'confidenceInterval', 'credibleInterval', 'interquartileRange' "
    "or 'range'.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520309
  { DistribUncertParameterDefinitionURLMustBeString,
    "The 'definitionURL' attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'distrib:definitionURL' on an <uncertParameter> must have a "
    "value of data type 'string'.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520310
  { DistribUncertParameterLOUncertParametersAllowedCoreElements,
    "Core elements allowed on <listOfUncertParameters>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Apart from the general notes and annotations subobjects permitted on all "
    "SBML objects, a <listOfUncertParameters> container object may only contain "
    "<uncertParameter> objects.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520311
  { DistribUncertParameterLOUncertParametersAllowedCoreAttributes,
    "Core attributes allowed on <listOfUncertParameters>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <listOfUncertParameters> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <listOfUncertParameters> object.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520401
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

  // 1520402
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

  // 1520403
  { DistribUncertaintyAllowedElements,
    "Elements allowed on <uncertainty>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An <uncertainty> object may contain one and only one instance of the "
    "<listOfUncertParameters> element. No other elements from the SBML Level 3 "
    "Distributions namespaces are permitted on an <uncertainty> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520501
  { DistribUncertSpanAllowedCoreAttributes,
    "Core attributes allowed on <uncertSpan>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An <uncertSpan> object may have the optional SBML Level 3 Core attributes "
    "'metaid' and 'sboTerm'. No other attributes from the SBML Level 3 Core "
    "namespaces are permitted on an <uncertSpan>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520502
  { DistribUncertSpanAllowedCoreElements,
    "Core elements allowed on <uncertSpan>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An <uncertSpan> object may have the optional SBML Level 3 Core subobjects "
    "for notes and annotations. No other elements from the SBML Level 3 Core "
    "namespaces are permitted on an <uncertSpan>.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520503
  { DistribUncertSpanAllowedAttributes,
    "Attributes allowed on <uncertSpan>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An <uncertSpan> object may have the optional attributes "
    "'distrib:varLower', 'distrib:valueLower', 'distrib:varUpper' and "
    "'distrib:valueUpper'. No other attributes from the SBML Level 3 "
    "Distributions namespaces are permitted on an <uncertSpan> object. ",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520504
  { DistribUncertSpanVarLowerMustBeSBase,
    "The attribute 'varLower' must point to SBase object.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'distrib:varLower' of an <uncertSpan> object "
    "must be the identifier of an existing object derived from the 'SBase' "
    "class and defined in the enclosing <model> object.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520505
  { DistribUncertSpanValueLowerMustBeDouble,
    "The 'valueLower' attribute must be Double.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'distrib:valueLower' on an <uncertSpan> must have a value of "
    "data type 'double'.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520506
  { DistribUncertSpanVarUpperMustBeSBase,
    "The attribute 'varUpper' must point to SBase object.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'distrib:varUpper' of an <uncertSpan> object "
    "must be the identifier of an existing object derived from the 'SBase' "
    "class and defined in the enclosing <model> object.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520507
  { DistribUncertSpanValueUpperMustBeDouble,
    "The 'valueUpper' attribute must be Double.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'distrib:valueUpper' on an <uncertSpan> must have a value of "
    "data type 'double'.",
    { "L3V1 Distrib V1 Section"
    }
  },

  // 1520601
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

  // 1520602
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

  // 1520603
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

  // 1520604
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


