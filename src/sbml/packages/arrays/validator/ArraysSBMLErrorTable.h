/**
 * @file ArraysSBMLErrorTable.h
 * @brief Definition of the ArraysSBMLErrorTable class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2017 jointly by the following organizations:
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


#ifndef ArraysSBMLErrorTable_H__
#define ArraysSBMLErrorTable_H__


#include <sbml/packages/arrays/validator/ArraysSBMLError.h>


LIBSBML_CPP_NAMESPACE_BEGIN



/** @cond doxygenLibsbmlInternal */

static const packageErrorTableEntry arraysErrorTable[] =
{
  // 8010100 != does not exist
  { ArraysUnknown,
    "Unknown error from Arrays",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Unknown error from Arrays",
    { ""
    }
  },

  // 8010101 == 8010101
  { ArraysNSUndeclared,
    "The Arrays namespace is not correctly declared.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "To conform to the arrays Package specification for SBML Level 3 Version 1, "
    "an SBML document must declare "
    "'http://www.sbml.org/sbml/level3/version1/arrays/version1' as the "
    "XMLNamespace to use for elements of this package.",
    { "L3V1 Arrays V1 Section 3.1"
    }
  },

  // 8010102 == 8010102
  { ArraysElementNotInNs,
    "Element not in Arrays namespace",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Wherever they appear in an SBML document, elements and attributes from the "
    "arrays Package must use the "
    "'http://www.sbml.org/sbml/level3/version1/arrays/version1' namespace, "
    "declaring so either explicitly or implicitly.",
    { "L3V1 Arrays V1 Section 3.1"
    }
  },

  // 8010301 libsbml only
  { ArraysDuplicateComponentId,
    "Duplicate 'id' attribute value",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "(Extends validation rule #10301 in the SBML Level 3 Core specification. TO "
    "DO list scope of ids)",
    { "L3V1 Arrays V1 Section"
    }
  },

  // 8010302 libsbml only
  { ArraysIdSyntaxRule,
    "Invalid SId syntax",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of a 'arrays:id' must conform to the syntax of the <sbml> data "
    "type 'SId'",
    { "L3V1 Arrays V1 Section"
    }
  },

  // 8020101 -> 8010201
  { ArraysAttributeRequiredMissing,
    "Required arrays:required attribute on <sbml>",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "In all SBML documents using the arrays Package, the <sbml> object must "
    "have the 'arrays:required' attribute.",
    { "L3V1 Arrays V1 Section"
    }
  },

  // 8020102 -> 8010202
  { ArraysAttributeRequiredMustBeBoolean,
    "The arrays:required attribute must be Boolean",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of attribute 'arrays:required' on the <sbml> object must be of "
    "data type 'boolean'.",
    { "L3V1 Arrays V1 Section"
    }
  },

  // 8020103 -> 8010203
  { ArraysAttributeRequiredMustHaveValue,
    "The arrays:required attribute must be 'true'",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of attribute 'arrays:required' on the <sbml> object must be set "
    "to 'true'.",
    { "L3V1 Arrays V1 Section"
    }
  },

  // 8020201 -> 8020206 libsbml only
  // spec splits this into 8020101 (LoD) + 8020109 (LoI) 
  { ArraysSBaseAllowedElements,
    "Elements allowed on <sBase>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <sBase> object may contain one and only one instance of each of the "
    "<listOfIndices> and <listOfDimensions> elements. No other elements from "
    "the SBML Level 3 arrays namespaces are permitted on a <sBase> object. ",
    { "L3V1 Arrays V1 Section"
    }
  },

  // 8020202 -> 8020110
  { ArraysSBaseLOIndicesAllowedCoreElements,
    "Core elements allowed on <sBase>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Apart from the general notes and annotations subobjects permitted on all "
    "SBML objects, a <listOfIndices> container object may only contain <index> "
    "objects.",
    { "L3V1 Arrays V1 Section"
    }
  },

  // 8020203 -> 8020102
  { ArraysSBaseLODimensionsAllowedCoreElements,
    "Core elements allowed on <sBase>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Apart from the general notes and annotations subobjects permitted on all "
    "SBML objects, a <listOfDimensions> container object may only contain "
    "<dimension> objects.",
    { "L3V1 Arrays V1 Section"
    }
  },

  // 8020204 -> 8020113
  { ArraysSBaseLOIndicesAllowedCoreAttributes,
    "Core attributes allowed on <sBase>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <listOfIndices> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <listOfIndices> object.",
    { "L3V1 Arrays V1 Section"
    }
  },

  // 8020205 -> 8020105
  { ArraysSBaseLODimensionsAllowedCoreAttributes,
    "Core attributes allowed on <sBase>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <listOfDimensions> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML Level "
    "3 Core namespaces are permitted on a <listOfDimensions> object.",
    { "L3V1 Arrays V1 Section"
    }
  },

  // 8020301 == 8020301
  { ArraysIndexAllowedCoreAttributes,
    "Core attributes allowed on <index>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An <index> object may have the optional SBML Level 3 Core attributes "
    "'metaid' and 'sboTerm'. No other attributes from the SBML Level 3 Core "
    "namespaces are permitted on an <index>.",
    { "L3V1 Arrays V1 Section"
    }
  },

  // 8020302 -> 8020309 libsbml only
  { ArraysIndexAllowedCoreElements,
    "Core elements allowed on <index>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An <index> object may have the optional SBML Level 3 Core subobjects for "
    "notes and annotations. No other elements from the SBML Level 3 Core "
    "namespaces are permitted on an <index>.",
    { "L3V1 Arrays V1 Section"
    }
  },

  // 8020303 -> 8020302
  { ArraysIndexAllowedAttributes,
    "Attributes allowed on <index>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An <index> object must have the required attributes "
    "'arrays:referencedAttribute' and 'arrays:arrayDimension'. No other "
    "attributes from the SBML Level 3 arrays namespaces are permitted on an "
    "<index> object. ",
    { "L3V1 Arrays V1 Section"
    }
  },

  // 8020304 -> 8020306
  { ArraysIndexAllowedElements,
    "Elements allowed on <index>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An <index> object must contain one and only one instance of the ASTNode "
    "element. No other elements from the SBML Level 3 arrays namespaces are "
    "permitted on an <index> object. ",
    { "L3V1 Arrays V1 Section"
    }
  },

  // 8020305 -> 8020303
  { ArraysIndexReferencedAttributeMustBeString,
    "ReferencedAttribute attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'arrays:referencedAttribute' on an <index> must have a value "
    "of data type 'string'.",
    { "L3V1 Arrays V1 Section"
    }
  },

  // 8020306 -> 8020304
  { ArraysIndexArrayDimensionMustBeUnInteger,
    "ArrayDimension attribute must be UnInteger.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'arrays:arrayDimension' on an <index> must have a value of "
    "data type 'integer'.",
    { "L3V1 Arrays V1 Section"
    }
  },

  // 8020401 -> 8020201
  { ArraysDimensionAllowedCoreAttributes,
    "Core attributes allowed on <dimension>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <dimension> object may have the optional SBML Level 3 Core attributes "
    "'metaid' and 'sboTerm'. No other attributes from the SBML Level 3 Core "
    "namespaces are permitted on a <dimension>.",
    { "L3V1 Arrays V1 Section"
    }
  },

  // 8020402 libsbml only
  { ArraysDimensionAllowedCoreElements,
    "Core elements allowed on <dimension>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <dimension> object may have the optional SBML Level 3 Core subobjects "
    "for notes and annotations. No other elements from the SBML Level 3 Core "
    "namespaces are permitted on a <dimension>.",
    { "L3V1 Arrays V1 Section"
    }
  },

  // 8020403 -> 8020202
  { ArraysDimensionAllowedAttributes,
    "Attributes allowed on <dimension>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A <dimension> object must have the required attributes 'arrays:size' and "
    "'arrays:arrayDimension', and may have the optional attributes 'arrays:id' "
    "and 'arrays:name'. No other attributes from the SBML Level 3 arrays "
    "namespaces are permitted on a <dimension> object. ",
    { "L3V1 Arrays V1 Section"
    }
  },

  // 8020404 != equivalent to 8020204 except that states it must be a Parameter SIdRef
  { ArraysDimensionSizeMustBeSBase,
    "Size attribute must be SBase.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the attribute 'arrays:size' of a <dimension> object must be "
    "the identifier of an existing <sBase> object defined in the enclosing "
    "<model> object.",
    { "L3V1 Arrays V1 Section"
    }
  },

  // 8020405 -> 8020203
  { ArraysDimensionArrayDimensionMustBeUnInteger,
    "ArrayDimension attribute must be UnInteger.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'arrays:arrayDimension' on a <dimension> must have a value "
    "of data type 'integer'.",
    { "L3V1 Arrays V1 Section"
    }
  },

  // 8020406 libsbml only
  { ArraysDimensionNameMustBeString,
    "Name attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'arrays:name' on a <dimension> must have a value of data "
    "type 'string'.",
    { "L3V1 Arrays V1 Section"
    }
  },

};

/** @endcond */



LIBSBML_CPP_NAMESPACE_END




#endif /* !ArraysSBMLErrorTable_H__ */


