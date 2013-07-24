/**
 * @file:   LayoutSBMLErrorTable.h
 * @brief:  Implementation of the LayoutSBMLErrorTable class
 * @author: Generated by autocreate code
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2009-2013 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EBML-EBI), Hinxton, UK
 *
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 *     Pasadena, CA, USA 
 *
 * Copyright (C) 2002-2005 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. Japan Science and Technology Agency, Japan
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 */


#ifndef LayoutSBMLErrorTable_H__
#define LayoutSBMLErrorTable_H__


#include <sbml/packages/layout/validator/LayoutSBMLError.h>

LIBSBML_CPP_NAMESPACE_BEGIN

	/** @cond doxygen-libsbml-internal */

static const packageErrorTableEntry layoutErrorTable[] = 
{
	//6010100
	{	LayoutUnknownError,
		"Unknown error from layout",
		LIBSBML_CAT_GENERAL_CONSISTENCY,
		LIBSBML_SEV_ERROR,
		"Unknown error from layout",
		{ " "
		}
	},

  // 6010101
  { LayoutNSUndeclared, 
    "The layout ns is not correctly declared",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "To conform to Version 1 of the Layout "
    "package specification for SBML Level 3, an "
    "SBML document must declare the use of the following XML Namespace: "
    "'http://www.sbml.org/sbml/level3/version1/layout/version1'",
    { "L3V1 Layout V1 Section 3.1"
    }
  },

  // 6010102
  { LayoutElementNotInNs, 
    "Element not in layout namespace",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "Wherever they appear in an SBML document, "
    "elements and attributes from the Layout "
    "package must be declared either implicitly or explicitly to be in the "
    "XML namespace 'http://www.sbml.org/sbml/level3/version1/layout/version1'",
    { "L3V1 Layout V1 Section 3.1"
    }
  },

    // 6010301
  { LayoutDuplicateComponentId, 
    "Duplicate 'id' attribute value",
    LIBSBML_CAT_IDENTIFIER_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "(Extends validation rule #10301 in the SBML Level 3 Version 1 Core "
	  "specification.) Within a <model> object"
	  "the values of the attributes id and layout:id on every instance of the "
	  "following classes of objects must be unique across the set of all id "
	  "and layout:id attribute values of all such objects in a model: the "
	  "model itself, plus all contained <functionDefinition>, <compartment>,"
	  "<species>, <reaction>, <speciesReference>, <modifierSpeciesReference>, "
    "<event>, and <parameter> objects, plus the <boundingBox>, "
    "<compartmentGlyph>, <generalGlyph>, <graphicalObject>, <layout>, "
    "<speciesGlyph>, <speciesReferenceGlyph>, <reactionGlyph>, "
    "<referenceGlyph> and <textGlyph> "
	  "objects defined by the Layout package.", 
    { "L3V1 Layout V1 Section 3.3"
    }
  },

    // 6010302
  { LayoutSIdSyntax, 
    "'id' attribute incorrect syntax",
    LIBSBML_CAT_IDENTIFIER_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "The value of the 'layout:id' attribute must conform to the SBML "
    "data type SId", 
    { "L3V1 Layout V1 Section 3.3"
    }
  },

  // 6020101
  { LayoutAttributeRequiredMissing, 
    "Required layout:required attribute on <sbml>",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "In all SBML documents using the Layout "
    "package, the SBML object must include a value for the attribute "
    "'layout:required.",
    { "L3V1 Core Section 4.1.2"
    }
  },

  // 6020102
  { LayoutAttributeRequiredMustBeBoolean, 
    "The layout:required attribute must be Boolean",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "The value of attribute 'layout:required' on the SBML object must "
    "be of the data type Boolean.",
    { "L3V1 Core Section 4.1.2"
    }
  },

  // 6020103
  { LayoutRequiredFalse, 
    "The layout:required attribute must be 'false'",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "The value of attribute 'layout:required' on the SBML object must "
    "be set to 'false'.",
    { "L3V1 Layout V1 Section 3.1"
    }
  },

  // 6020201
  { LayoutOnlyOneLOLayouts, 
    "Only one listOfLayouts on <model>",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "There may be at most one instance of <listOfLayouts> element within "
    "a <model> object using Layout. No other elements from the Layout "
    "package are allowed.",
    { "L3V1 Layout V1 Section 3.5"
    }
  },

  // 6020202
  { LayoutLOLayoutsNotEmpty, 
    "ListOf elements cannot be empty",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "The <listOfLayouts> within a <model> object is optional, "
    "but if present, this object must not be empty.",
    { "L3V1 Layout V1 Section 3.5"
    }
  },

  // 6020203
  { LayoutLOLayoutsAllowedElements, 
    "Allowed elements on ListOfLayouts",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "Apart from the general notes and annotation subobjects permitted on "
    "all SBML objects, a <listOfLayouts> container object may only "
    "contain <layout> objects.",
    { "L3V1 Layout V1 Section 3.5"
    }
  },

  // 6020204
  { LayoutLOLayoutsAllowedAttributes, 
    "Allowed attributes on ListOfLayouts",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "A <listOfLayouts> object may have the optional attributes 'metaid' "
    "and 'sboTerm' defined by SBML Level~3 Core. No other attributes from "
    "the SBML Level 3 Core namespace or the Layout "
    "namespace are permitted on a <listOfLayouts> object. ",
    { "L3V1 Layout V1 Section 3.5"
    }
  },

  // 6020301
  { LayoutLayoutAllowedElements, 
    "Allowed elements on Layout",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "A <layout> object may have the optional SBML Level 3 Core "
    "subobjects for notes and annotations. No other elements from "
    "the SBML Level 3 Core namespace are permitted on a <layout>.",
    { "L3V1 Core Section 3.2"
    }
  },

  // 6020302
  { LayoutLayoutAllowedCoreAttributes, 
    "Allowed core attributes on Layout",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "A <layout> object may have the optional SBML Level~3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from "
    "the SBML Level 3 Core namespace are permitted on a <layout> object. ",
    { "L3V1 Core Section 3.2"
    }
  },

  // 6020303
  { LayoutOnlyOneEachListOf, 
    "Only one each listOf on <layout>",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "There may be at most one instance of each of the following kinds "
    "of objects within a <layout> object: <listOfCompartmentGlyphs>, "
    "<listOfSpeciesGlyphs>, <listOfReactionGlyphs>, "
    "<listOfTextGlyphs>, <listOfAdditionalGraphicalObjects>.",
    { "L3V1 Layout V1 Section 3.6"
    }
  },

  // 6020304
  { LayoutNoEmptyListOfs, 
    "ListOf elements cannot be empty",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "The various ListOf subobjects within a <layout> object are "
    "optional, but if present, these container object must not be "
    "empty.  Specifically, if any of the following classes of objects "
    "are present on the <layout>, it must not be empty: "
    "<listOfCompartmentGlyphs>, <listOfSpeciesGlyphs>, <listOfReactionGlyphs>,"
    " <listOfTextGlyphs>, <listOfAdditionalGraphicalObjects>.",
    { "L3V1 Layout V1 Section 3.6"
    }
  },

  // 6020305
  { LayoutLayoutAllowedAttributes, 
    "<layout> must have 'id' and may have 'name'",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "A <layout> object must have the required attribute 'layout:id' and may "
    "have the optional attribute 'layout:name'. No other attributes from the "
    "SBML Level~3 Layout namespace are permitted on a <layout> object.",
    { "L3V1 Layout V1 Section 3.6"
    }
  },
  
  // 6020306
  { LayoutLayoutNameMustBeString, 
    "'name' must be string",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "The attribute 'layout:name' of a <layout> must be of the data "
    "type 'string'.",
    { "L3V1 Layout V1 Section 3.6"
    }
  },
  
  // 6020307
  { LayoutLOCompGlyphAllowedAttributes, 
    "Attributes allowed on <listOfCompartmentGlyphs>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "A <listOfCompartmentGlyphs> object may have the optional "
    "attributes 'metaid' "
    "and 'sboTerm' defined by SBML Level~3 Core.  No other attributes "
    "from the SBML Level~3 Core namespace or the Layout namespace are "
    "permitted on a <listOfCompartmentGlyphs> object. ",
    { "L3V1 Layout V1 Section 3.6"
    }
  },
  
  // 6020308
  { LayoutLOCompGlyphAllowedElements, 
    "Elements allowed on <listOfCompartmentGlyphs>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "Apart from the general notes and annotation subobjects permitted "
    "on all SBML objects, a <listOfCompartmentGlyphs> container object "
    "may only contain <compartmentGlyph> objects. ",
    { "L3V1 Layout V1 Section 3.6"
    }
  },
  
  // 6020309
  { LayoutLOSpeciesGlyphAllowedAttributes, 
    "Attributes allowed on <listOfSpeciesGlyphs>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "A <listOfSpeciesGlyphs> object may have the optional "
    "attributes 'metaid' "
    "and 'sboTerm' defined by SBML Level~3 Core.  No other attributes "
    "from the SBML Level~3 Core namespace or the Layout namespace are "
    "permitted on a <listOfSpeciesGlyphs> object. ",
    { "L3V1 Layout V1 Section 3.6"
    }
  },
  
  // 6020310
  { LayoutLOSpeciesGlyphAllowedElements, 
    "Elements allowed on <listOfSpeciesGlyphs>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "Apart from the general notes and annotation subobjects permitted "
    "on all SBML objects, a <listOfSpeciesGlyphs> container object "
    "may only contain <compartmentGlyph> objects. ",
    { "L3V1 Layout V1 Section 3.6"
    }
  },
  
  // 6020311
  { LayoutLORnGlyphAllowedAttributes, 
    "Attributes allowed on <listOfReactionGlyphs>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "A <listOfReactionGlyphs> object may have the optional "
    "attributes 'metaid' "
    "and 'sboTerm' defined by SBML Level~3 Core.  No other attributes "
    "from the SBML Level~3 Core namespace or the Layout namespace are "
    "permitted on a <listOfReactionGlyphs> object. ",
    { "L3V1 Layout V1 Section 3.6"
    }
  },
  
  // 6020312
  { LayoutLORnGlyphAllowedElements, 
    "Elements allowed on <listOfReactionGlyphs>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "Apart from the general notes and annotation subobjects permitted "
    "on all SBML objects, a <listOfReactionGlyphs> container object "
    "may only contain <compartmentGlyph> objects. ",
    { "L3V1 Layout V1 Section 3.6"
    }
  },
  
  // 6020313
  { LayoutLOAddGOAllowedAttribut, 
    "Attributes allowed on <listOfAdditionalGraphicalObjectGlyphs>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "A <listOfAdditionalGraphicalObjectGlyphs> object may have the "
    "optional attributes 'metaid' "
    "and 'sboTerm' defined by SBML Level~3 Core.  No other attributes "
    "from the SBML Level~3 Core namespace or the Layout namespace are "
    "permitted on a <listOfAdditionalGraphicalObjectGlyphs> object. ",
    { "L3V1 Layout V1 Section 3.6"
    }
  },
  
  // 6020314
  { LayoutLOAddGOAllowedElements, 
    "Elements allowed on <listOfAdditionalGraphicalObjectGlyphs>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "Apart from the general notes and annotation subobjects permitted "
    "on all SBML objects, a <listOfAdditionalGraphicalObjectGlyphs> container object "
    "may only contain <compartmentGlyph> objects. ",
    { "L3V1 Layout V1 Section 3.6"
    }
  },
  
  // 6020315
  { LayoutLayoutMustHaveDimensions, 
    "Layout must have <dimensions>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "A <layout> object must contain exactly one <dimensions> object.",
    { "L3V1 Layout V1 Section 3.6"
    }
  },
 
  // 6020316
  { LayoutLOTextGlyphAllowedAttributes, 
    "Attributes allowed on <listOfTextGlyphs>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "A <listOfTextGlyphs> object may have the optional "
    "attributes 'metaid' "
    "and 'sboTerm' defined by SBML Level~3 Core.  No other attributes "
    "from the SBML Level~3 Core namespace or the Layout namespace are "
    "permitted on a <listOfTextGlyphs> object. ",
    { "L3V1 Layout V1 Section 3.6"
    }
  },
  
  // 6020317
  { LayoutLOTextGlyphAllowedElements, 
    "Elements allowed on <listOfTextGlyphs>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "Apart from the general notes and annotation subobjects permitted "
    "on all SBML objects, a <listOfTextGlyphs> container object "
    "may only contain <compartmentGlyph> objects. ",
    { "L3V1 Layout V1 Section 3.6"
    }
  },
  
  // 6020401
  { LayoutGOAllowedCoreElements, 
    "Core elements allowed on <graphicalObject>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "A <graphicalObject> object may have the optional SBML Level 3 Core "
    "subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespace are permitted on a <graphicalObject>. ",
    { "L3V1 Core Section 3.2"
    }
  },
  
  // 6020402
  { LayoutGOAllowedCoreAttributes, 
    "Core attributes allowed on <graphicalObject>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "A <graphicalObject> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML "
    "Level 3 Core namespace are permitted on a <graphicalObject>. ",
    { "L3V1 Core Section 3.2"
    }
  },
  
  // 6020403
  { LayoutGOAllowedElements, 
    "Layout elements allowed on <graphicalObject>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "There may be at most one instance of a <boundingBox> object on a "
    "<graphicalObject>.  No other elements from the Layout namespace "
    "are permitted on a <graphicalObject>. ",
    { "L3V1 Layout V1 Section 3.7"
    }
  },
  
  // 6020404
  { LayoutGOAllowedAttributes, 
    "Layout attributes allowed on <graphicalObject>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "A <graphicalObject> object must have the required attribute "
    "'layout:id and may have the optional attribute 'layout:metaidRef'.  "
    "No other attributes from the Layout namespace "
    "are permitted on a <graphicalObject>. ",
    { "L3V1 Layout V1 Section 3.7"
    }
  },

  // 6020405
  { LayoutGOMetaIdRefMustBeIDREF, 
    "Layout 'metIdRef' must be IDREF.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "The attribute 'layout:metaidRef' of a <graphicalObject> "
    "must be of the data type 'IDREF'.",
    { "L3V1 Layout V1 Section 3.7"
    }
  },

  // 6020406
  { LayoutGOMetaIdRefMustReferenceObject, 
    "Layout 'metIdRef' must reference existing object.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "The value of a 'layout:metaidRef' attribute of a <graphicalObject> "
    "must be of the 'metaid' of an existing element in the <model>.",
    { "L3V1 Layout V1 Section 3.7"
    }
  },

  // 6020407
  { LayoutGOMustContainBoundingBox, 
    "A <graphicalObject> must contain a <boundingBox>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "A <graphicalObject> must contain exactly one <boundingBox> object.",
    { "L3V1 Layout V1 Section 3.7"
    }
  },

  
  // 6020501
  { LayoutCGAllowedCoreElements, 
    "Core elements allowed on <compartmentGlyph>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "A <compartmentGlyph> object may have the optional SBML Level 3 Core "
    "subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespace are permitted on a <compartmentGlyph>. ",
    { "L3V1 Core Section 3.2"
    }
  },
  
  // 6020502
  { LayoutCGAllowedCoreAttributes, 
    "Core attributes allowed on <compartmentGlyph>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "A <compartmentGlyph> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML "
    "Level 3 Core namespace are permitted on a <compartmentGlyph>. ",
    { "L3V1 Core Section 3.2"
    }
  },
  
  // 6020503
  { LayoutCGAllowedElements, 
    "Layout elements allowed on <compartmentGlyph>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "There may be at most one instance of a <boundingBox> object on a "
    "<compartmentGlyph>.  No other elements from the Layout namespace "
    "are permitted on a <compartmentGlyph>. ",
    { "L3V1 Layout V1 Section 3.8"
    }
  },
  
  // 6020504
  { LayoutCGAllowedAttributes, 
    "Layout attributes allowed on <compartmentGlyph>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "A <compartmentGlyph> object must have the required attribute "
    "'layout:id and may have the optional attributes 'layout:metaidRef' "
    "or 'layout:compartment'.  "
    "No other attributes from the Layout namespace "
    "are permitted on a <compartmentGlyph>. ",
    { "L3V1 Layout V1 Section 3.8"
    }
  },

  // 6020505
  { LayoutCGMetaIdRefMustBeIDREF, 
    "Layout 'metIdRef' must be IDREF.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "The attribute 'layout:metaidRef' of a <compartmentGlyph> "
    "must be of the data type 'IDREF'.",
    { "L3V1 Layout V1 Section 3.7"
    }
  },

  // 6020506
  { LayoutCGMetaIdRefMustReferenceObject, 
    "Layout 'metIdRef' must reference existing object.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "The value of a 'layout:metaidRef' attribute of a <compartmentGlyph> "
    "must be of the 'metaid' of an existing element in the <model>.",
    { "L3V1 Layout V1 Section 3.7"
    }
  },

  // 6020507
  { LayoutCGCompartmentSyntax, 
    "CompartmentGlyph 'compartment' must have SIdRef syntax.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "The attribute 'layout:compartment' of a <compartmentGlyph> "
    "must be the data type 'SIdRef'.",
    { "L3V1 Layout V1 Section 3.8"
    }
  },

  // 6020508
  { LayoutCGCompartmentMustRefComp, 
    "CompartmentGlyph compartment must reference existing compartment.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "The value of the 'layout:compartment' attribute of a <compartmentGlyph> "
    "must be of the 'id' of an existing <compartment> in the <model>.",
    { "L3V1 Layout V1 Section 3.8"
    }
  },

  // 6020509
  { LayoutCGNoDuplicateReferences, 
    "CompartmentGlyph cannot reference two objects.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "If both attributes 'layout:compartment' and 'layout:metaidRef' "
    "are specified on a <compartmentGlyph> they have to reference "
    "the same <compartment> in the <model>.",
    { "L3V1 Layout V1 Section 3.8"
    }
  },

  // 6020510
  { LayoutCGOrderMustBeDouble, 
    "CompartmentGlyph order must be double.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "The attribute 'layout:order' of a <compartmentGlyph> "
    "must be the data type 'double'.",
    { "L3V1 Layout V1 Section 3.8"
    }
  },

  // 6020601
  { LayoutSGAllowedCoreElements, 
    "Core elements allowed on <speciesGlyph>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "A <speciesGlyph> object may have the optional SBML Level 3 Core "
    "subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespace are permitted on a <speciesGlyph>. ",
    { "L3V1 Core Section 3.2"
    }
  },
  
  // 6020602
  { LayoutSGAllowedCoreAttributes, 
    "Core attributes allowed on <speciesGlyph>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "A <speciesGlyph> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML "
    "Level 3 Core namespace are permitted on a <speciesGlyph>. ",
    { "L3V1 Core Section 3.2"
    }
  },
  
  // 6020603
  { LayoutSGAllowedElements, 
    "Layout elements allowed on <speciesGlyph>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "There may be at most one instance of a <boundingBox> object on a "
    "<speciesGlyph>.  No other elements from the Layout namespace "
    "are permitted on a <speciesGlyph>. ",
    { "L3V1 Layout V1 Section 3.9"
    }
  },
  
  // 6020604
  { LayoutSGAllowedAttributes, 
    "Layout attributes allowed on <speciesGlyph>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "A <speciesGlyph> object must have the required attribute "
    "'layout:id and may have the optional attributes 'layout:metaidRef' "
    "or 'layout:species'.  "
    "No other attributes from the Layout namespace "
    "are permitted on a <speciesGlyph>. ",
    { "L3V1 Layout V1 Section 3.9"
    }
  },

  // 6020605
  { LayoutSGMetaIdRefMustBeIDREF, 
    "Layout 'metIdRef' must be IDREF.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "The attribute 'layout:metaidRef' of a <speciesGlyph> "
    "must be of the data type 'IDREF'.",
    { "L3V1 Layout V1 Section 3.7"
    }
  },

  // 6020606
  { LayoutSGMetaIdRefMustReferenceObject, 
    "Layout 'metIdRef' must reference existing object.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "The value of a 'layout:metaidRef' attribute of a <speciesGlyph> "
    "must be of the 'metaid' of an existing element in the <model>.",
    { "L3V1 Layout V1 Section 3.7"
    }
  },

  // 6020607
  { LayoutSGSpeciesSyntax, 
    "SpeciesGlyph 'species' must have SIdRef syntax.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "The attribute 'layout:species' of a <speciesGlyph> "
    "must be the data type 'SIdRef'.",
    { "L3V1 Layout V1 Section 3.9"
    }
  },

  // 6020608
  { LayoutSGSpeciesMustRefSpecies, 
    "SpeciesGlyph species must reference existing species.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "The value of the 'layout:species' attribute of a <speciesGlyph> "
    "must be of the 'id' of an existing <species> in the <model>.",
    { "L3V1 Layout V1 Section 3.8"
    }
  },

  // 6020609
  { LayoutSGNoDuplicateReferences, 
    "SpeciesGlyph cannot reference two objects.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "If both attributes 'layout:species' and 'layout:metaidRef' "
    "are specified on a <speciesGlyph> they have to reference "
    "the same <species> in the <model>.",
    { "L3V1 Layout V1 Section 3.9"
    }
  },

  // 6020701
  { LayoutRGAllowedCoreElements, 
    "Core elements allowed on <reactionGlyph>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "A <reactionGlyph> object may have the optional SBML Level 3 Core "
    "subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespace are permitted on a <reactionGlyph>. ",
    { "L3V1 Core Section 3.2"
    }
  },
  
  // 6020702
  { LayoutRGAllowedCoreAttributes, 
    "Core attributes allowed on <reactionGlyph>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "A <reactionGlyph> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML "
    "Level 3 Core namespace are permitted on a <reactionGlyph>. ",
    { "L3V1 Core Section 3.2"
    }
  },
  
  // 6020703
  { LayoutRGAllowedElements, 
    "Layout elements allowed on <reactionGlyph>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "There may be at most one instance of each of the following kinds "
    "of objects within a <reactionGlyph> object: <boundingBox>, "
    "<curve>, <listOfSpeciesReferenceGlyphs>. No other elements from "
    "the Layout namespace are permitted on a <reactionGlyph>. ",
    { "L3V1 Layout V1 Section 3.10"
    }
  },
  
  // 6020704
  { LayoutRGAllowedAttributes, 
    "Layout attributes allowed on <reactionGlyph>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "A <reactionGlyph> object must have the required attribute "
    "'layout:id and may have the optional attributes 'layout:metaidRef' "
    "or 'layout:reaction'.  "
    "No other attributes from the Layout namespace "
    "are permitted on a <reactionGlyph>. ",
    { "L3V1 Layout V1 Section 3.10"
    }
  },

  // 6020705
  { LayoutRGMetaIdRefMustBeIDREF, 
    "Layout 'metIdRef' must be IDREF.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "The attribute 'layout:metaidRef' of a <reactionGlyph> "
    "must be of the data type 'IDREF'.",
    { "L3V1 Layout V1 Section 3.7"
    }
  },

  // 6020706
  { LayoutRGMetaIdRefMustReferenceObject, 
    "Layout 'metIdRef' must reference existing object.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "The value of a 'layout:metaidRef' attribute of a <reactionGlyph> "
    "must be of the 'metaid' of an existing element in the <model>.",
    { "L3V1 Layout V1 Section 3.7"
    }
  },

  // 6020707
  { LayoutRGReactionSyntax, 
    "ReactionGlyph 'reaction' must have SIdRef syntax.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "The attribute 'layout:reaction' of a <reactionGlyph> "
    "must be the data type 'SIdRef'.",
    { "L3V1 Layout V1 Section 3.10"
    }
  },

  // 6020708
  { LayoutRGReactionMustRefReaction, 
    "ReactionGlyph reaction must reference existing reaction.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "The value of the 'layout:reaction' attribute of a <reactionGlyph> "
    "must be of the 'id' of an existing <reaction> in the <model>.",
    { "L3V1 Layout V1 Section 3.10"
    }
  },

  // 6020709
  { LayoutRGNoDuplicateReferences, 
    "ReactionGlyph cannot reference two objects.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "If both attributes 'layout:reaction' and 'layout:metaidRef' "
    "are specified on a <reactionGlyph> they have to reference "
    "the same <reaction> in the <model>.",
    { "L3V1 Layout V1 Section 3.10"
    }
  },

  // 6020710
  { LayoutLOSpeciesRefGlyphAllowedElements, 
    "Allowed elements on ListOfSpeciesReferenceGlyphs",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "Apart from the general notes and annotation subobjects permitted on "
    "all SBML objects, a <listOfSpeciesReferenceGlyphs> container object "
    "may only contain <speciesReferenceGlyph> objects.",
    { "L3V1 Layout V1 Section 3.10"
    }
  },

  // 6020711
  { LayoutLOSpeciesRefGlyphAllowedAttribs, 
    "Allowed attributes on ListOfSpeciesReferenceGlyphs",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "A <listOfSpeciesReferenceGlyphs> object may have the optional "
    "attributes 'metaid' "
    "and 'sboTerm' defined by SBML Level~3 Core. No other attributes from "
    "the SBML Level 3 Core namespace or the Layout "
    "namespace are permitted on a <listOfSpeciesReferenceGlyphs> object. ",
    { "L3V1 Layout V1 Section 3.10"
    }
  },

  // 6020801
  { LayoutGGAllowedCoreElements, 
    "Core elements allowed on <generalGlyph>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "A <generalGlyph> object may have the optional SBML Level 3 Core "
    "subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespace are permitted on a <generalGlyph>. ",
    { "L3V1 Core Section 3.2"
    }
  },
  
  // 6020802
  { LayoutGGAllowedCoreAttributes, 
    "Core attributes allowed on <generalGlyph>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "A <generalGlyph> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML "
    "Level 3 Core namespace are permitted on a <generalGlyph>. ",
    { "L3V1 Core Section 3.2"
    }
  },
  
  // 6020803
  { LayoutGGAllowedElements, 
    "Layout elements allowed on <generalGlyph>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "There may be at most one instance of each of the following kinds "
    "of objects within a <generalGlyph> object: <boundingBox>, "
    "<curve>, <listOfReferenceGlyphs> and <listOfSubGlyphs>. "
    "No other elements from "
    "the Layout namespace are permitted on a <generalGlyph>. ",
    { "L3V1 Layout V1 Section 3.11"
    }
  },
  
  // 6020804
  { LayoutGGAllowedAttributes, 
    "Layout attributes allowed on <generalGlyph>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "A <generalGlyph> object must have the required attribute "
    "'layout:id and may have the optional attributes 'layout:metaidRef' "
    "or 'layout:reference'.  "
    "No other attributes from the Layout namespace "
    "are permitted on a <generalGlyph>. ",
    { "L3V1 Layout V1 Section 3.11"
    }
  },

  // 6020805
  { LayoutGGMetaIdRefMustBeIDREF, 
    "Layout 'metIdRef' must be IDREF.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "The attribute 'layout:metaidRef' of a <generalGlyph> "
    "must be of the data type 'IDREF'.",
    { "L3V1 Layout V1 Section 3.7"
    }
  },

  // 6020806
  { LayoutGGMetaIdRefMustReferenceObject, 
    "Layout 'metIdRef' must reference existing object.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "The value of a 'layout:metaidRef' attribute of a <generalGlyph> "
    "must be of the 'metaid' of an existing element in the <model>.",
    { "L3V1 Layout V1 Section 3.7"
    }
  },

  // 6020807
  { LayoutGGReferenceSyntax, 
    "GeneralGlyph 'reference' must have SIdRef syntax.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "The attribute 'layout:reference' of a <generalGlyph> "
    "must be the data type 'SIdRef'.",
    { "L3V1 Layout V1 Section 3.11"
    }
  },

  // 6020808
  { LayoutGGReferenceMustRefObject, 
    "GeneralGlyph 'reference' must reference existing element.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "The value of the 'layout:reference' attribute of a <generalGlyph> "
    "must be of the 'id' of an existing element in the <model>.",
    { "L3V1 Layout V1 Section 3.11"
    }
  },

  // 6020809
  { LayoutGGNoDuplicateReferences, 
    "GeneralGlyph cannot reference two objects.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "If both attributes 'layout:reference' and 'layout:metaidRef' "
    "are specified on a <generalGlyph> they have to reference "
    "the same element in the <model>.",
    { "L3V1 Layout V1 Section 3.11"
    }
  },

  // 6020810
  { LayoutLOReferenceGlyphAllowedElements, 
    "Allowed elements on ListOfReferenceGlyphs",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "Apart from the general notes and annotation subobjects permitted on "
    "all SBML objects, a <listOfReferenceGlyphs> container object "
    "may only contain <referenceGlyph> objects.",
    { "L3V1 Layout V1 Section 3.11"
    }
  },

  // 6020811
  { LayoutLOReferenceGlyphAllowedAttribs, 
    "Allowed attributes on ListOfReferenceGlyphs",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "A <listOfReferenceGlyphs> object may have the optional "
    "attributes 'metaid' "
    "and 'sboTerm' defined by SBML Level~3 Core. No other attributes from "
    "the SBML Level 3 Core namespace or the Layout "
    "namespace are permitted on a <listOfReferenceGlyphs> object. ",
    { "L3V1 Layout V1 Section 3.11"
    }
  },

  // 6020812
  { LayoutLOSpeciesRefGlyphAllowedElements, 
    "Allowed elements on ListOfSubGlyphs",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "Apart from the general notes and annotation subobjects permitted on "
    "all SBML objects, a <listOfSubGlyphs> container object "
    "may only contain <compartmentGlyph>, <speciesGlyph>, <reactionGlyph>, "
    "<generalGlyph>, <graphicalObject>, <textGlyph>, <speciesReferenceGlyph> "
    "and <referenceGlyph> objects.",
    { "L3V1 Layout V1 Section 3.11"
    }
  },

  // 6020813
  { LayoutLOSubGlyphAllowedAttribs, 
    "Allowed attributes on ListOfSubGlyphs",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "A <listOfSubGlyphs> object may have the optional attributes 'metaid' "
    "and 'sboTerm' defined by SBML Level~3 Core. No other attributes from "
    "the SBML Level 3 Core namespace or the Layout "
    "namespace are permitted on a <listOfSubGlyphs> object. ",
    { "L3V1 Layout V1 Section 3.11"
    }
  },

  // 6020901
  { LayoutTGAllowedCoreElements, 
    "Core elements allowed on <textGlyph>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "A <textGlyph> object may have the optional SBML Level 3 Core "
    "subobjects for notes and annotations. No other elements from the SBML "
    "Level 3 Core namespace are permitted on a <textGlyph>. ",
    { "L3V1 Core Section 3.2"
    }
  },
  
  // 6020902
  { LayoutTGAllowedCoreAttributes, 
    "Core attributes allowed on <textGlyph>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "A <textGlyph> object may have the optional SBML Level 3 Core "
    "attributes 'metaid' and 'sboTerm'. No other attributes from the SBML "
    "Level 3 Core namespace are permitted on a <textGlyph>. ",
    { "L3V1 Core Section 3.2"
    }
  },
  
  // 6020903
  { LayoutTGAllowedElements, 
    "Layout elements allowed on <textGlyph>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "A ><textGlyph> must contain exactly one <boundingBox> object. "
    "No other elements from "
    "the Layout namespace are permitted on a <textGlyph>. ",
    { "L3V1 Layout V1 Section 3.12"
    }
  },
  
  // 6020904
  { LayoutTGAllowedAttributes, 
    "Layout attributes allowed on <textGlyph>.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "A <textGlyph> object must have the required attribute "
    "'layout:id and may have the optional attributes 'layout:metaidRef', "
    "'layout:graphicalObject', 'layout:text' and 'layout:originOfText'.  "
    "No other attributes from the Layout namespace "
    "are permitted on a <textGlyph>. ",
    { "L3V1 Layout V1 Section 3.12"
    }
  },

  // 6020905
  { LayoutTGMetaIdRefMustBeIDREF, 
    "Layout 'metIdRef' must be IDREF.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "The attribute 'layout:metaidRef' of a <textGlyph> "
    "must be of the data type 'IDREF'.",
    { "L3V1 Layout V1 Section 3.7"
    }
  },

  // 6020906
  { LayoutTGMetaIdRefMustReferenceObject, 
    "Layout 'metIdRef' must reference existing object.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "The value of a 'layout:metaidRef' attribute of a <textGlyph> "
    "must be of the 'metaid' of an existing element in the <model>.",
    { "L3V1 Layout V1 Section 3.7"
    }
  },

  // 6020907
  { LayoutTGOriginOfTextSyntax, 
    "TextGlyph 'originOfText' must have SIdRef syntax.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "The attribute 'layout:originOfText' of a <textGlyph> "
    "must be the data type 'SIdRef'.",
    { "L3V1 Layout V1 Section 3.12"
    }
  },

  // 6020908
  { LayoutTGOriginOfTextMustRefObject, 
    "TextGlyph 'originOfText' must reference existing element.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "The value of the 'layout:originOfText' attribute of a <textGlyph> "
    "must be of the 'id' of an existing element in the <model>.",
    { "L3V1 Layout V1 Section 3.12"
    }
  },

  // 6020909
  { LayoutTGNoDuplicateReferences, 
    "TextGlyph cannot reference two objects.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "If both attributes 'layout:originOfText' and 'layout:metaidRef' "
    "are specified on a <textGlyph> they have to reference "
    "the same element in the <model>.",
    { "L3V1 Layout V1 Section 3.12"
    }
  },

  // 6020910
  { LayoutTGGraphicalObjectSyntax, 
    "TextGlyph 'graphicalObject' must have SIdRef syntax.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "The attribute 'layout:graphicalObject' of a <textGlyph> "
    "must be the data type 'SIdRef'.",
    { "L3V1 Layout V1 Section 3.12"
    }
  },

  // 6020911
  { LayoutTGGraphicalObjectMustRefObject, 
    "TextGlyph 'graphicalObject' must reference existing element.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "The value of the 'layout:graphicalObject' attribute of a <textGlyph> "
    "must be of the 'id' of an existing <graphicalObject> (or derived) "
    "element in the <layout>.",
    { "L3V1 Layout V1 Section 3.12"
    }
  },

  // 6020912
  { LayoutTGTextMustBeString, 
    "TextGlyph 'text' must be string.",
    LIBSBML_CAT_GENERAL_CONSISTENCY, 
    LIBSBML_SEV_ERROR,
    "The attribute 'layout:text' of a <textGlyph> "
    "must be the data type 'string'.",
    { "L3V1 Layout V1 Section 3.12"
    }
  },





};


LIBSBML_CPP_NAMESPACE_END

	/** @endcond doxygen-libsbml-internal */


#endif  /*  LayoutSBMLErrorTable_h__  */

