/**
 * @file ArraysSBMLErrorTable.h
 * @brief Definition of the ArraysSBMLErrorTable class.
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

  // 8020403 libsbml only
  { ArraysDimensionNameMustBeString,
    "Name attribute must be String.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The attribute 'arrays:name' on a <dimension> must have a value of data "
    "type 'string'.",
    { "L3V1 Arrays V1 Section"
    }
  },

  // New
  // 8010204
  { arrays_8010204,
    "MathML elements must be in MathML namespace",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Wherever MathML content appears in an SBML document, the MathML "
    "content must be placed within a math element, and that math element must "
    "be either explicitly or implicitly declared to be in the XML namespace "
    "http://www.w3.org/1998/Math/MathML",
    { "L3V1 Arrays V1 Section 3.4"
    }
  },

  // 8010205
  { arrays_8010205,
    "Only additional MathML elements permitted in arrays are vector and selector.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The following is a list of the additional MathML 2.0 elements permitted in "
    "the Arrays package: vector and selector.",
    { "L3V1 Arrays V1 Section 3.5"
    }
  },

  // 8010206
  { arrays_8010206,
    "Short description",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The first argument of aMathML selector must be aMathML vector object or a "
    "valid identifier to an SBase object extended with a list of Dimension objects.",
    { "L3V1 Arrays V1 Section 3.5"
    }
  },

  // 8010207
  { arrays_8010207,
    "Short description",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The first argument of a MathML selector must have a number of dimensions equal to "
    "the number of arguments to the selector minus 1.",
    { "L3V1 Arrays V1 Section 3.5"
    }
  },

  // 8010208
  { arrays_8010208,
    "Short description",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The arguments of a MathML selector other than the first argument must be "
    "statically computable. In other words, any identifier that appears in an "
    "argument, other than a Dimension id for the corresponding object, must be a "
    "constant.",
    { "L3V1 Arrays V1 Section 3.5"
    }
  },

  // 8010209
  { arrays_8010209,
    "Short description",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The arguments of a MathML selector other than the first argument must be "
    " evaluated to a scalar value.",
    { "L3V1 Arrays V1 Section 3.5"
    }
  },

  // 8010210
  { arrays_8010210,
    "Short description",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "For each possible value of each Dimension id (i.e., 0 to size-1 of the "
    "Dimension referred to) that appears in the second and later arguments of "
    "the selector, there should be no array out - of - bounds problems.Namely, "
    "it must evaluate to a non - negative integer that is less than the size of "
    "the corresponding Dimension for the object being indexed where the last "
    "argument refers to dimension 0, next to last to dimension 1",
    { "L3V1 Arrays V1 Section 3.5"
        }
  },

  // 8010211
  { arrays_8010211,
    "Short description",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "All mathematical operations must be performed on scalar values rather "
    "than vectors.",
    { "L3V1 Arrays V1 Section 3.5"
    }
  },

  // 8010212
  { arrays_8010212,
    "Short description",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "For MathML operations with two or more operands involving MathML "
    "vectors or SBase objects with a list of Dimension objects, the number "
    "of dimensions and their sizemust agree for all operands unless the "
    "operand is a scalar type(i.e., it does not have a list of Dimension "
    "objects).",
    { "L3V1 Arrays V1 Section 3.5"
    }
  },

  // 8010213
  { arrays_8010213,
    "Short description",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "No SBase is allowed to have value of type vector.",
    { "L3V1 Arrays V1 Section 3.5"
    }
  },

  //8020101
  { arrays_8020101,
    "Elements allowed on <sBase>.",
	LIBSBML_CAT_GENERAL_CONSISTENCY,
	LIBSBML_SEV_ERROR,
	"Any object derived from the extended SBase class (defined in the Arrays "
	"package) may contain	at most one instance of a ListOfDimensions.",
	{ "L3V1 Arrays V1 Section 3.3"
	}
  },

  // 8020103
  { arrays_8020103,
    "Short description",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The ListOfDimensions associated with an SBase object must have a "
    "Dimension object with arrays:arrayDimension attribute set to 0 ... n-1 "
    "before adding a Dimension object with arrays : arrayDimension attribute "
    "set to n.",
    { "L3V1 Arrays V1 Section 3.3"
    }
  },

  // 8020104
  { arrays_8020104,
    "Short description",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The ListOfDimensions associated with an SBase object must not have "
    "multiple Dimension objects with the same arrays:arrayDimension "
    "attribute.",
    { "L3V1 Arrays V1 Section 3.3"
    }
  },

  // 8020106
  { arrays_8020106,
    "Short description",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "ListOf objects are not permitted to have a ListOfDimensions.",
    { "L3V1 Arrays V1 Section 3.3"
    }
  },

  // 8020107
  { arrays_8020107,
    "Short description",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "InSBML Level 3 Core, Models, FunctionDefinitions, Units, "
    "UnitDefinitions, KineticLaws, LocalParameters, Triggers, Priorities, "
    "and Delays are not permitted to have a ListOfDimensions. All other "
    "SBML Level 3 Core objects are permitted to have a ListOfDimensions "
    "including: Compartments, Species, Parameters, Initial assignments, "
    "Rules, Constraints, Reactions, Species references, Events, and Event "
    "assignments.All SBML objects defined by packages that inherit from "
    "SBase are permitted to have a ListOfDimensions unless it is explicitly "
    "disallowed in the corresponding package specification.",
    { "L3V1 Arrays V1 Section 3.3"
    }
  },

  // 8020108
  { arrays_8020108,
    "Short description",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The Dimension and Index objects are not permitted to have a "
    "ListOfDimensions.",
    { "L3V1 Arrays V1 Section 3.3"
    }
  },

  //8020109
  { arrays_8020109,
    "Elements allowed on <sBase>.",
	LIBSBML_CAT_GENERAL_CONSISTENCY,
	LIBSBML_SEV_ERROR,
	"Any object derived from the extended SBase class (defined in the Arrays "
	"package) may contain at most one instance of a ListOfIndices.",
	{ "L3V1 Arrays V1 Section 3.4"
	}
  },

  // 8020111
  { arrays_8020111,
    "Short description",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The ListOfIndices associated with an SBase object must have a Index "
    "object with arrays:arrayDimension attribute set to 0, 1, . ,n-1 before "
    "adding a Index object with arrays : arrayDimension attribute set to n "
    "for every arrays : referencedAttribute that are being indexed.",
    { "L3V1 Arrays V1 Section 3.3"
    }
  },

  // 8020112
  { arrays_8020112,
    "Short description",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The ListOfIndices in an SBase object must not have multiple Index "
    "objects with the same pair of values arrays:arrayDimension and "
    "arrays:referencedAttribute attributes.",
    { "L3V1 Arrays V1 Section 3.4"
    }
  },

  // 8020114
  { arrays_8020114,
    "Short description",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Only SBML objects that include defined attributes of type SIdRef "
    "are permitted to have a ListOfIndices. For SBML Level 3 Core, this "
    "includes Model to reference a conversionFactor	element, Species to "
    "reference a compartment or a conversionFactor element, Reactions "
    "to reference a compartment, Initial assignments to reference a symbol, "
    "Rules to reference a variable, Species references to reference a "
    "species, and Events assignments to reference a variable. In addition "
    "to these, any SBML object in a package with a defined attribute of "
    "type SIdRef may also have a ListOfIndices.",
    { "L3V1 Arrays V1 Section 3.4"
    }
  },

  // 8020115
  { arrays_8020115,
    "Short description",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "SBase objects containg SIdRef must have a ListOfIndices if the "
    "referenced SBase is an array. ",
    { "L3V1 Arrays V1 Section 3.4"
    }
  },

  // 8020116
  { arrays_8020116,
    "Short description",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "SBase objects containg SIdRef with a ListOfDimensions should have a "
    "ListOfIndices containing as many Index objects for this particular "
    "arrays:referencedAttribute as the number of Dimension objects the "
    "referenced object contains.",
    { "L3V1 Arrays V1 Section 3.4"
    }
  },

      // 8020117
  { ArraysSBaseAllowedElements,
      "Short description",
      LIBSBML_CAT_GENERAL_CONSISTENCY,
      LIBSBML_SEV_ERROR,
      "",
      { "L3V1 Arrays V1"
      }
  },

      // 8020205
  { arrays_8020205,
    "Short description",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the Parameter referenced by the arrays:size attribute "
    "must be a non-negative scalar constant integer.",
    { "L3V1 Arrays V1 Section 3.3"
    }
  },

  // 8020305
  { arrays_8020305,
    "Short description",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The object referenced by the SIdRef indicated by the "
    "arrays:referencedAttribute attribute must include an "
    "arrays:arrayDimension matching the arrays:arrayDimension for the Index",
    { "L3V1 Arrays V1 Section 3.4"
    }
  },

  // 8020307
  { arrays_8020307,
    "Short description",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The MathML math element in an Index object must be statically "
    "computable. In other words, any identifier that appears in the "
    "math element, other than a Dimension id for the object with this "
    "Index, must be a constant.",
    { "L3V1 Arrays V1 Section 3.4"
    }
  },

  // 8020308
  { arrays_8020308,
    "Short description",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "For each possible value of each Dimension id (i.e., 0 to size-1 of "
    "the Dimension referred to) that appears in theMathML math element, "
    "there should be no array out of bounds problems. Namely, it must "
    "evaluate to a non-negative integer that is less than the size of "
    "the corresponding Dimension for the object being indexed.",
    { "L3V1 Arrays V1"
    }
  },

};

/** @endcond */



LIBSBML_CPP_NAMESPACE_END




#endif /* !ArraysSBMLErrorTable_H__ */


