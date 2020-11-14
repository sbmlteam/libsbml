/**
 * @file:   MultiSBMLErrorTable.h
 * @brief:  Implementation of the MultiSBMLErrorTable class
 * @author: Fengkai Zhang
 * @author: SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
 *
 * Copyright (C) 2009-2013 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
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


#ifndef MultiSBMLErrorTable_H__
#define MultiSBMLErrorTable_H__


#include <sbml/packages/multi/validator/MultiSBMLError.h>

LIBSBML_CPP_NAMESPACE_BEGIN

  /** @cond doxygenLibsbmlInternal */

static const packageErrorTableEntry multiErrorTable[] = 
{
  //7010100
  {  MultiUnknownError,
    "Unknown error from multi",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Unknown error from multi",
    { "L3V1 Multi V1.1"
    }
  },

  // 7010101
  { MultiNSUndeclared,
    "The Multi ns is not correctly declared",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "To conform to Version 1 of the Multi "
    "package specification for SBML Level 3, an "
    "SBML document must declare the use of the following XML Namespace: "
    "'http://www.sbml.org/sbml/level3/version1/multi/version1'",
    { "L3V1 Multi V1.1 Section 3.1"
    }
  },

  // 7010102
  { MultiElementNotInNs,
    "Element not in Multi namespace",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Wherever they appear in an SBML document, "
    "elements and attributes from the Multi "
    "package must be declared either implicitly or explicitly to be in the "
    "XML namespace 'http://www.sbml.org/sbml/level3/version1/comp/version1'",
    { "L3V1 Multi V1.1 Section 3.1"
    }
  },

  // 7010201
  { MultiMathCi_AllowedMultiAtts,
  "Math ci element: Allowed Multi attributes ",
  LIBSBML_CAT_GENERAL_CONSISTENCY,
  LIBSBML_SEV_ERROR,
  "A 'ci' element in a Math object may have the optional attributes 'multi:speciesReference' and 'multi:representationType'. "
  "No other attributes from the Multi namespace are permitted on a 'ci' element. ",
    { "L3V1 Multi V1.1 Section 3.26"
    }
  },

  // 7010202
  { MultiMathCi_SpeRefAtt_Ref,
  "Math ci element: 'speciesReference' must be the 'id' of a speciesReference ",
  LIBSBML_CAT_GENERAL_CONSISTENCY,
  LIBSBML_SEV_ERROR,
  "The value of the 'multi:speciesReference' attribute on a given 'ci' element must be the identifier of a SpeciesReference "
  "object within the same reaction. ",
  { "L3V1 Multi V1.1 Section 3.26.1"
  }
  },

  // 7010203
  { MultiMathCi_RepTypAtt_Ref,
  "Math ci element: 'representationType' must be a value of the Multi data type 'RepresentationType' ",
  LIBSBML_CAT_GENERAL_CONSISTENCY,
  LIBSBML_SEV_ERROR,
  "The value of the 'multi:representationType' attribute on a given 'ci' element must conform to the syntax of the Multi data "
  "type 'RepresentationType'. ",
  { "L3V1 Multi V1.1 Section 3.26.2"
  }
  },


  // SK moved
  //// 7010103
  //{ MultiSBML_RequiredAttMissing,
  //  "The 'multi:required' attribute is required on <code>&lt;sbml&gt;</code>",
  //  LIBSBML_CAT_GENERAL_CONSISTENCY,
  //  LIBSBML_SEV_ERROR,
  //  "In all SBML documents using the Multi "
  //  "package, the SBML object must include a value for the "
  //  "'multi:required' attribute.",
  //  { "L3V1 Core Section 3.1"
  //  }
  //},

  //// 7010104
  //{ MultiSBML_RequiredAttMustBeBoolean,
  //  "The multi:required attribute must be Boolean",
  //  LIBSBML_CAT_GENERAL_CONSISTENCY,
  //  LIBSBML_SEV_ERROR,
  //  "The value of attribute 'multi:required' on the SBML object must "
  //  "be of the data type Boolean.",
  //  { "L3V1 Core Section 3.1"
  //  }
  //},

  //// 7010105
  //{ MultiSBML_RequiredAttMustBeTrue,
  //  "The multi:required attribute must be 'true'",
  //  LIBSBML_CAT_GENERAL_CONSISTENCY,
  //  LIBSBML_SEV_ERROR,
  //  "The value of attribute 'multi:required' on the SBML object must "
  //  "be set to 'true'.",
  //  { "L3V1 Multi V1 Section 3.1"
  //  }
  //},


  // 7010301
  { MultiDupClaId,
    "Duplicate 'id' attribute value",
    LIBSBML_CAT_IDENTIFIER_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "(Extends validation rule #10301 in the SBML Level 3 Version 1 Core "
    "specification.) Within a <model> object, "
    "the values of the attributes id and multi:id on every instance of the "
    "following classes of objects must be unique across the set of all id "
    "and multi:id attribute values of all such objects in a model: the "
    "model itself, plus all contained <functionDefinition>, <compartment>,"
    "<species>, <reaction>, <speciesReference>, <modifierSpeciesReference>, "
    "<event>, and <parameter> objects, plus the <SpeciesType> and "
    "<PossibleSpeciesFeatureValue> objects defined by the Multi package, and "
    "any objects defined by any other package with 'package:id' "
    "attributes defined as falling in the 'SId' namespace.",
    { "L3V1 Multi V1.1 Section 3.27"
    }
  },

    // 7010302
  { MultiInvSIdSyn,
    "Invalid SId syntax",
    LIBSBML_CAT_IDENTIFIER_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of a multi:id attribute must always conform to the "
    "syntax of the SBML data type SId.",
    { "L3V1 Core Section 3.1.7"
    }
  },

  // 7010303
  { MultiInvNamSyn,
    "Invalid name syntax",
    LIBSBML_CAT_IDENTIFIER_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of a 'multi:name' attribute "
    "must always conform to the "
    "syntax of type string.",
    { "L3V1 Core Section 3.1.1"
    }
  },

  // 7010304 - SK renumbered from 7010401
  { MultiUnqId_SptIns,
    "SpeciesTypeInstance must have unique ids within parent speciesType",
    LIBSBML_CAT_IDENTIFIER_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of a multi:id attribute on SpeciesTypeInstance objects must be unique across "
    "the set of all multi:id attribute values of all the SpeciesTypeInstance objects under the "
    "direct parent SpeciesType object in which it is located. ",
    { "L3V1 Multi V1.1 Section 3.11.1 and Section 3.27"
    }
  },

  // 7010305 - SK renumbered from 7010402
  { MultiUnqId_SptCpoInd,
    "SpeciesTypeComponentIndex must have unique ids within the parent speciesType",
    LIBSBML_CAT_IDENTIFIER_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of a multi:id attribute on SpeciesTypeComponentIndex objects must be unique across "
    "the set of all multi:id attribute values of all the SpeciesTypeComponentIndex objects under the "
    "direct parent SpeciesType object in which it is located. ",
    { "L3V1 Multi V1.1 Section 3.12.1 and Section 3.27"
    }
  },

  // 7010306 - SK renumbered from 7010403
  { MultiUnqId_InSptBnd,
    "InSpeciesTypeBond must have unique ids within the parent speciesType",
    LIBSBML_CAT_IDENTIFIER_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of a multi:id attribute on InSpeciesTypeBond objects must be unique across the "
    "set of all multi:id attribute values of all the InSpeciesTypeBond objects under the direct "
    "parent SpeciesType object in which it is it is located. ",
    { "L3V1 Multi V1.1 Section 3.13.1 and Section 3.27"
    }
  },

  // 7010307 - SK renumbered from 7010404
  { MultiUnqId_Sft,
    "SpeciesFeatureType must have unique ids within the parent speciesType",
    LIBSBML_CAT_IDENTIFIER_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of a multi:id attribute on SpeciesFeatureType objects must be unique "
    "across the set of all multi:id attribute values of all the SpeciesFeature objects under "
    "the direct parent SpeciesType object in which it is located. ",
    { "L3V1 Multi V1.1 Section 3.9.1 and Section 3.27"
    }
  },

  // 7010308  - SK renumbered from 7010405 // add at v1.0.6
  { MultiUnqId_SubListOfSfs,
    "SubListOfSpeciesFeatures must have unique ids within a species",
    LIBSBML_CAT_IDENTIFIER_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of a multi:id attribute on SubListOfSpeciesFeatures objects must be unique across the set of"
    "all id and multi:id attribute values of all objects in the Species object in which it is located.",
    { "L3V1 Multi V1.1 Section 3.17.1 and Section 3.27"
    }
  },

  // 7010309 - SK renumbered from 7010406
  { MultiUnqId_SpeFtr,
    "SpeciesFeature must have unique ids within a species",
    LIBSBML_CAT_IDENTIFIER_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of a multi:id attribute on SpeciesFeature objects must be unique across the set of"
    "all id and multi:id attribute values of all objects in the Species object in which it is located.",
    { "L3V1 Multi V1.1 Section 3.18.1 and Section 3.27"
    }
  },

  // 7010310 - SK renumbered from 7010408
  { MultiUnqId_CpaRef,
    "CompartmentReference must have unique ids within a compartment",
    LIBSBML_CAT_IDENTIFIER_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of a multi:id attribute on CompartmentReference objects must be unique across "
    "the set of all id and multi:id attribute values of all objects in the Compartment object in "
    "which it is located.",
    { "L3V1 Multi V1.1 Section 3.6.1 and Section 3.27"
    }
  },

  // 7010311 - SK renumbered from 7010501
  { MultiInvSIdRefSyn_Spt_CpaAtt,
    "Invalid SIdRef syntax: compartment attribute of SpeciesType",
    LIBSBML_CAT_IDENTIFIER_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of a multi:compartment attribute on SpeciesType objects must conform to the "
    "syntax of the SBML data type SIdRef. ",
    { "L3V1 Multi V1.1 Section 3.8.2"
    }
  },

  // 7010312 - SK renumbered from 7010502
  { MultiInvSIdRefSyn_PslSpeFtrVal_NumAtt,
    "Invalid SIdRef syntax: 'numericValue' attribute of PossibleSpeciesFeatureValue",
    LIBSBML_CAT_IDENTIFIER_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of a multi:numericValue attribute on PossibleSpeciesFeatureValue objects must conform to the "
    "syntax of the SBML data type SIdRef. ",
    { "L3V1 Multi V1.1 Section 3.10.2"
    }
  },

  // 7010313 - SK renumbered from 7010503
  { MultiInvSIdRefSyn_SptIns_SptAtt,
    "Invalid SIdRef syntax: 'speciesType' attribute of SpeciesTypeInstance",
    LIBSBML_CAT_IDENTIFIER_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of a multi:speciesType attribute on SpeciesTypeInstance objects must conform to the "
    "syntax of the SBML data type SIdRef. ",
    { "L3V1 Multi V1.1 Section 3.11.2"
    }
  },

  // 7010314 - SK renumbered from 7010504
  { MultiInvSIdRefSyn_SptIns_CpaRefAtt,
    "Invalid SIdRef syntax: 'compartmentReference' attribute of SpeciesTypeInstance",
    LIBSBML_CAT_IDENTIFIER_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of a multi:compartmentReference attribute on SpeciesTypeInstance objects must conform to the "
    "syntax of the SBML data type SIdRef. ",
    { "L3V1 Multi V1.1 Section 3.11.3"
    }
  },

  // 7010315 - SK renumbered from 7010505
  { MultiInvSIdRefSyn_SptCpoInd_CpoAtt,
    "Invalid SIdRef syntax: 'component' attribute of SpeciesTypeComponentIndex",
    LIBSBML_CAT_IDENTIFIER_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of a multi:component attribute on SpeciesTypeComponentIndex objects must conform to the "
    "syntax of the SBML data type SIdRef. ",
    { "L3V1 Multi V1.1 Section 3.12.2"
    }
  },

  // 7010316 - SK renumbered from 7010506
  { MultiInvSIdRefSyn_SptCpoInd_ParAtt,
    "Invalid SIdRef syntax: 'identifyingParent' attribute of SpeciesTypeComponentIndex",
    LIBSBML_CAT_IDENTIFIER_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of a multi:identifyingParent attribute on SpeciesTypeComponentIndex objects must conform to the "
    "syntax of the SBML data type SIdRef. ",
    { "L3V1 Multi V1.1 Section 3.12.3"
    }
  },

  // 7010317 - SK renumbered from 7010508
  { MultiInvSIdRefSyn_InSptBnd_Bst1Att,
    "Invalid SIdRef syntax: 'bindingSite1' attribute of InSpeciesTypeBond",
    LIBSBML_CAT_IDENTIFIER_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of a multi:bindingSite1 attribute on InSpeciesTypeBond objects must conform to the "
    "syntax of the SBML data type SIdRef. ",
    { "L3V1 Multi V1.1 Section 3.13.2"
    }
  },

  // 7010318 - SK renumbered from 7010509
  { MultiInvSIdRefSyn_InSptBnd_Bst2Att,
    "Invalid SIdRef syntax: 'bindingSite2' attribute of InSpeciesTypeBond",
    LIBSBML_CAT_IDENTIFIER_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of a multi:bindingSite2 attribute on InSpeciesTypeBond objects must conform to the "
    "syntax of the SBML data type SIdRef. ",
    { "L3V1 Multi V1.1 Section 3.13.2"
    }
  },

  // 7010319 - SK renumbered from 7010601
  { MultiInvSIdRefSyn_Spe_SptAtt,
    "Invalid SIdRef syntax: 'speciesType' attribute of extended Species",
    LIBSBML_CAT_IDENTIFIER_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of a multi:speciesType attribute on extended Species objects must conform to the "
    "syntax of the SBML data type SIdRef. ",
    { "L3V1 Multi V1.1 Section 3.15.1"
    }
  },

  // 7010320 - SK renumbered from 7010602
  { MultiInvSIdRefSyn_OutBst_CpoAtt,
    "Invalid SIdRef syntax: 'component' attribute of OutwardBindingSite",
    LIBSBML_CAT_IDENTIFIER_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of a multi:component attribute on OutwardBindingSite objects must conform to the "
    "syntax of the SBML data type SIdRef. ",
    { "L3V1 Multi V1.1 Section 3.16.3"
    }
  },

  // 7010321 - SK renumbered from 7010603
  { MultiInvSIdRefSyn_SpeFtr_SpeFtrTypAtt,
    "Invalid SIdRef syntax: 'speciesFeatureType' attribute of SpeciesFeature",
    LIBSBML_CAT_IDENTIFIER_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of a multi:speciesFeatureType attribute on SpeciesFeature objects must conform to the "
    "syntax of the SBML data type SIdRef. ",
    { "L3V1 Multi V1.1 Section 3.18.2"
    }
  },

  // 7010322 - SK renumbered from 7010604
  { MultiInvSIdRefSyn_SpeFtr_CpoAtt,
    "Invalid SIdRef syntax: 'component' attribute of SpeciesFeature",
    LIBSBML_CAT_IDENTIFIER_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of a multi:component attribute on SpeciesFeature objects must conform to the "
    "syntax of the SBML data type SIdRef. ",
    { "L3V1 Multi V1.1 Section 3.18.4"
    }
  },

  // 7010323 - SK renumbered from 7010605
  { MultiInvSIdRefSyn_SpeFtrVal_ValAtt,
    "Invalid SIdRef syntax: 'value' attribute of SpeciesFeatureValue",
    LIBSBML_CAT_IDENTIFIER_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of a multi:value attribute on SpeciesFeatureValue objects must conform to the "
    "syntax of the SBML data type SIdRef. ",
    { "L3V1 Multi V1.1 Section 3.18.6"
    }
  },

  // 7010324 - SK renumbered from 7010701
  { MultiInvSIdRefSyn_SplSpeRef_CompRefAtt,
    "Invalid SIdRef syntax: 'compartmentReference' attribute of extended SimpleSpeciesReference",
    LIBSBML_CAT_IDENTIFIER_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of a multi:compartmentReference attribute on extended SimpleSpeciesReference objects must conform to the "
    "syntax of the SBML data type SIdRef. ",
    { "L3V1 Multi V1.1 Section 3.22"
    }
  },

  // 7010325 - SK renumbered from 7010702
  { MultiInvSIdRefSyn_StpCpoMapInPro_RctAtt,
    "Invalid SIdRef syntax: 'reactant' attribute of SpeciesTypeComponentMapInProduct",
    LIBSBML_CAT_IDENTIFIER_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of a multi:reactant attribute on SpeciesTypeComponentMapInProduct objects must conform to the "
    "syntax of the SBML data type SIdRef. ",
    { "L3V1 Multi V1.1 Section 3.24.2"
    }
  },

  // 7010326 - SK renumbered from 7010703
  { MultiInvSIdRefSyn_StpCpoMapInPro_RctCpoAtt,
    "Invalid SIdRef syntax: 'reactantComponent' attribute of SpeciesTypeComponentMapInProduct",
    LIBSBML_CAT_IDENTIFIER_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of a multi:reactantComponent attribute on SpeciesTypeComponentMapInProduct objects must conform to the "
    "syntax of the SBML data type SIdRef. ",
    { "L3V1 Multi V1.1 Section 3.24.3"
    }
  },

  // 7010327 - SK renumbered from 7010704
  { MultiInvSIdRefSyn_StpCpoMapInPro_ProCpoAtt,
    "Invalid SIdRef syntax: 'productComponent' attribute of SpeciesTypeComponentMapInProduct",
    LIBSBML_CAT_IDENTIFIER_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of a multi:productComponent attribute on SpeciesTypeComponentMapInProduct objects must conform to the "
    "syntax of the SBML data type SIdRef. ",
    { "L3V1 Multi V1.1 Section 3.24.4"
    }
  },

  // 7010328 - SK renumbered from 7010801
  { MultiInvSIdRefSyn_Cpa_CpaTypAtt,
    "Invalid SIdRef syntax: 'compartmentType' attribute of extended Compartment",
    LIBSBML_CAT_IDENTIFIER_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of a multi:compartmentType attribute on extended Compartment objects must conform to the "
    "syntax of the SBML data type SIdRef. ",
    { "L3V1 Multi V1.1 Section 3.5.2"
    }
  },

  // 7010329 - SK renumbered from 7010802
  { MultiInvSIdRefSyn_CpaRef_CpaAtt,
    "Invalid SIdRef syntax: 'compartment' attribute of CompartmentReference",
    LIBSBML_CAT_IDENTIFIER_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of a multi:compartment attribute on CompartmentReference objects must conform to the "
    "syntax of the SBML data type SIdRef. ",
    { "L3V1 Multi V1.1 Section 3.6.2"
    }
  },


    // 7020101 - SK renumbered from 7010103
  { MultiSBML_RequiredAttMissing,
    "The 'multi:required' attribute is required on <code>&lt;sbml&gt;</code>",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "In all SBML documents using the Multi "
    "package, the SBML object must include a value for the "
    "'multi:required' attribute.",
    { "L3V1 Core Section 3.1"
    }
  },

    // 7020102 - SK renumbered from 7010104
  { MultiSBML_RequiredAttMustBeBoolean,
    "The multi:required attribute must be Boolean",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of attribute 'multi:required' on the SBML object must "
    "be of the data type Boolean.",
    { "L3V1 Core Section 3.1"
    }
  },

    // 7020103 - SK renumbered from 7010105
  { MultiSBML_RequiredAttMustBeTrue,
    "The multi:required attribute must be 'true'",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of attribute 'multi:required' on the SBML object must "
    "be set to 'true'.",
    { "L3V1 Multi V1.1 Section 3.1"
    }
  },


  // 7020201 SK renumbered from 7020101
  { MultiLofStps_OnlyOne,
    "ListOfSpeciesTypes: Only one object allowed in a model",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "There may be at most one instance of ListOfSpeciesTypes "
    "within an extended Model object that uses the SBML Level 3 Multi package.",
    { "L3V1 Multi V1.1 Section 3.4"
    }
  },

  // 7020202 SK renumbered from 7020102
  { MultiLofStps_NoEmpty,
    "ListOfSpeciesTypes: Must not be empty",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A ListOfSpeciesTypes object within an ExModel object is optional, but if present, must not be empty.",
    { "L3V1 Multi V1.1 Section 3.4"
    }
  },

  // 7020203 SK renumbered from 7020103
  { MultiLofStps_AllowedAtts,
    "ListOfSpeciesTypes: Allowed attributes",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A ListOfSpeciesTypes object may have the optional SBML core attributes 'metaid' and 'sboTerm'.  "
    "No other attributes from the SBML Level~3 Core namespace or the Multi namespace are permitted "
    "on a ListOfSpeciesTypes object.",
    { "L3V1 Multi V1.1 Section 3.4.1"
    }
  },

  // 7020204 SK renumbered from 7020104
  { MultiLofStps_AllowedElts,
    "ListOfSpeciesTypes: Allowed elements",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Apart from the general 'notes' and 'annotation' subobjects permitted on all SBML objects, "
    "a ListOfSpeciesTypes container object may only contain SpeciesType objects.",
    { "L3V1 Multi V1.1 Section 3.4.1"
    }
  },


  // 7020301 SK renumbered from 7020201
  { MultiExCpa_AllowedMultiAtts,
    "Extended Compartment: Allowed Multi attributes ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An extended Compartment object must have the required attribute 'multi:isType', and may also "
    "have the optional attribute 'multi:compartmentType'. No other attributes from the Multi "
    "namespace are permitted on an extended Compartment object.",
    { "L3V1 Multi V1.1 Section 3.5"
    }
  },

  // 7020302 SK renumbered from 7020202
  { MultiExCpa_IsTypeAtt_Invalid,
    "Extended Compartment: Invalid boolean syntax of 'isType' attribute ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of a 'multi:isType' attribute on an extended Compartment object must always confirm "
    "to the syntax of the SBML data type 'boolean'.",
    { "L3V1 Multi V1.1 Section 3.5.1"
    }
  },

  // 7020303 SK renumbered from 7020203
  { MultiExCpa_IsTypeAtt_Required,
    "Extended Compartment: 'isType' attribute is required ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Extended Compartment: 'isType' attribute is required. ",
    { "L3V1 Multi V1.1 Section 3.5.1"
    }
  },

  // 7020304 SK renumbered from 7020204
  { MultiExCpa_IsTypeAtt_SameAsParent,
    "Extended Compartment: 'isType' attribute, if referenced, must be same as that of the containing compartment ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the 'multi:isType' attribute of the Compartment object referenced by a CompartmentReference "
    "object must be the same as that of the 'multi:isType' attribute of the parent Compartment object of the "
    "ListOfCompartmentReferences object which contains the CompartmentReference object.",
    { "L3V1 Multi V1.1 Section 3.7"
    }
  },

  // 7020305 SK renumbered from 7020205
  { MultiExCpa_CpaTypAtt_Restrict,
    "Extended Compartment: Compartment type can not reference another compartment type ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The 'multi:compartmentType' attribute on a Compartment object must not be defined if the value of "
    "the 'multi:isType' is 'true'.",
    { "L3V1 Multi V1.1 Section 3.5.2"
    }
  },

  // 7020306 SK renumbered from 7020206
  { MultiLofCpaRefs_OnlyOne,
    "ListOfCompartmentReferences: Only one object allowed in a compartment ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "There may be at most one ListOfCompartmentReferences container object within a Compartment object.",
    { "L3V1 Multi V1.1 Section 3.5.3"
    }
  },

  // 7020307 SK renumbered from 7020207
  { MultiLofCpaRefs_NoEmpty,
    "ListOfCompartmentReferences: Must not be empty ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A ListOfCompartmentReferences object within a Compartment object is optional, but if present, must not be empty.",
    { "L3V1 Multi V1.1 Section 3.5.3"
    }
  },

  // 7020308 SK renumbered from 7020208
  { MultiLofCpaRefs_AllowedAtts,
    "ListOfCompartmentReferences: Allowed attributes ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A ListOfCompartmentReferences object may have the optional SBML core attributes 'metaid' and 'sboTerm'.  "
    "No other attributes from the SBML Level~3 Core namespace or the Multi namespace are permitted on a "
    "ListOfCompartmentReferences object. ",
    { "L3V1 Multi V1.1 Section 3.5.3"
    }
  },

  // 7020309 SK renumbered from 7020209
  { MultiLofCpaRefs_AllowedElts,
    "ListOfCompartmentReferences: Allowed elements ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Apart from the general 'notes' and 'annotation' subobjects permitted on all SBML objects, a "
    "ListOfCompartmentReferences container object may only contain CompartmentReference objects.",
    { "L3V1 Multi V1.1 Section 3.5.3"
    }
  },

    // SK moved block to end to preserve numbers after this
  //  // 7020301
  //{ MultiCpaRef_AllowedCoreAtts,
  //  "CompartmentReference: Allowed SBML core attributes ",
  //  LIBSBML_CAT_GENERAL_CONSISTENCY,
  //  LIBSBML_SEV_ERROR,
  //  "A CompartmentReference object may have the optional SBML Level~3 Core attributes 'metaid' and "
  //  "'sboTerm'.  No other attributes from the SBML Level~3 Core namespace are permitted on a "
  //  "'CompartmentReference object.",
  //  { "L3V1 Multi V1.1 Section 3.6"
  //  }
  //},

  //// 7020302
  //{ MultiCpaRef_AllowedCoreElts,
  //  "CompartmentReference: Allowed SBML core elements ",
  //  LIBSBML_CAT_GENERAL_CONSISTENCY,
  //  LIBSBML_SEV_ERROR,
  //  "A CompartmentReference object may have the optional SBML Level~3 Core subobjects for 'notes' and "
  //  "'annotation'.  No other elements from the SBML Level~3 Core namespace are permitted on a "
  //  "CompartmentReference object.",
  //  { "L3V1 Multi V1.1 Section 3.6"
  //  }
  //},

  //// 7020303
  //{ MultiCpaRef_AllowedMultiAtts,
  //  "CompartmentReference: Allowed Multi attributes ",
  //  LIBSBML_CAT_GENERAL_CONSISTENCY,
  //  LIBSBML_SEV_ERROR,
  //  "A CompartmentReference object must have the required attribute 'multi:compartment', and may have "
  //  "the optional attributes 'multi:id' and 'multi:name'. No other attributes from the Multi namespace "
  //  "are permitted on a CompartmentReference object.",
  //  { "L3V1 Multi V1.1 Section 3.6"
  //  }
  //},

  //// 7020304
  //{ MultiCpaRef_CompartmentAtt_Ref,
  //  "CompartmentReference: 'compartment' must be the 'id' of a compartment ",
  //  LIBSBML_CAT_GENERAL_CONSISTENCY,
  //  LIBSBML_SEV_ERROR,
  //  "The value of the 'multi:compartment' attribute must be the value of an 'id' attribute on an existing "
  //  "Compartment object in the 'SId' namespace of the parent model.",
  //  { "L3V1 Multi V1.1 Section 3.6"
  //  }
  //},

  //// 7020305
  //{ MultiCpaRef_IdRequiredOrOptional,
  //  "CompartmentReference: 'multi:id' is required when referencing the same compartment ",
  //  LIBSBML_CAT_GENERAL_CONSISTENCY,
  //  LIBSBML_SEV_ERROR,
  //  "If some or all compartmentReferences within a ListOfCompartmentReferences object reference the same "
  //  "compartment, those compartmentReferences are required to have its 'multi:id' attribute defined to "
  //  "distinguish different compartmentReferences.",
  //  { "L3V1 Multi V1.1 Section 3.6.1"
  //  }
  //},

  //// 7020306
  //{ MultiCpaRef_NoReferenceToAnyParent,
  //  "CompartmentReference: A compartmentReference cannot reference any parent compartment ",
  //  LIBSBML_CAT_GENERAL_CONSISTENCY,
  //  LIBSBML_SEV_ERROR,
  //  "A <compartmentReference> cannot reference a <compartment> that directly or "
  //  "indirectly contains teh <compartmentReference>.",
  //  { "L3V1 Multi V1.1 Section 3.6.1"
  //  }
  //},

      // 7020401
  { MultiSpt_AllowedCoreAtts,
    "SpeciesType: Allowed SBML core attributes ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A SpeciesType object may have the optional SBML Level~3 Core attributes 'metaid' and 'sboTerm'.  No "
    "other attributes from the SBML Level~3 Core namespace are permitted on a SpeciesType object. ",
    { "L3V1 Multi V1.1 Section 3.8"
    }
  },

  // 7020402
  { MultiSpt_AllowedCoreElts,
    "SpeciesType: Allowed SBML core children ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A SpeciesType object may have the optional SBML Level~3 Core subobjects for 'notes' and 'annotation'. "
    "No other elements from the SBML Level~3 Core namespace are permitted on a SpeciesType object.",
    { "L3V1 Multi V1.1 Section 3.8"
    }
  },

  // 7020403
  { MultiSpt_AllowedMultiAtts,
    "SpeciesType: Allowed Multi attributes ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A SpeciesType object must have the required attribute 'multi:id', and may have the optional attributes "
    "'multi:name' and 'multi:compartment'. No other attributes from the Multi namespace are permitted on a "
    "SpeciesType object.",
    { "L3V1 Multi V1.1 Section 3.8"
    }
  },

  // 7020404
  { MultiSpt_CompartmentAtt_Ref,
    "SpeciesType: 'compartment' must be the 'id' of a compartment ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the 'multi:compartment' attribute, if set on a given SpeciesType object, must be the value "
    "of an 'id' attribute on an existing Compartment object in the 'SId' namespace of the parent Model object.",
    { "L3V1 Multi V1.1 Section 3.8.2"
    }
  },

  // 7020405
  { MultiSpt_ListOfDefs_NoEmpty,
    "SpeciesType: ListOf- subobjects must not be empty ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The various 'ListOf' subobjects within a SpeciesType object are optional, but if present, these container "
    "objects must not be empty. Specifically, if any of the following classes of objects are present on a "
    "SpeciesType object, it must not be empty: ListOfSpeciesFeatureTypes, ListOfSpeciesTypeInstances, "
    "ListOfSpeciesTypeComponentIndexes and ListOfInSpeciesTypeBonds.",
    { "L3V1 Multi V1.1 Section 3.8"
    }
  },

  // 7020406
  { MultiLofSpeFtrTyps_onlyOne,
    "ListOfSpeciesFeatureTypes: Only one <listOfSpeciesFeatureTypes> is allowed in a <speciesType> ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "There may be at most one ListOfSpeciesFeatureTypes container object within a SpeciesType object.",
    { "L3V1 Multi V1.1 Section 3.8"
    }
  },

  // 7020407
  { MultiLofSpeFtrTyps_Elts,
    "ListOfSpeciesFeatureTypes: Only allow speciesFeatureType elements ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Apart from the general 'notes' and 'annotation' subobjects permitted on all SBML objects, a "
    "ListOfSpeciesFeatureTypes container object may only contain SpeciesFeatureType objects.",
    { "L3V1 Multi V1.1 Section 3.8.3"
    }
  },

  // 7020408
  { MultiLofSpeFtrTyps_AllowedAtts,
    "ListOfSpeciesFeatureTypes: Allowed attributes ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A ListOfSpeciesFeatureTypes object may have the optional SBML core attributes 'metaid' and 'sboTerm'.  "
    "No other attributes from the SBML Level~3 Core namespace or the Multi namespace are permitted on a "
    "ListOfSpeciesFeatureTypes object.",
    { "L3V1 Multi V1.1 Section 3.8.3"
    }
  },

  // 7020409
  { MultiLofSptInss_onlyOne,
    "ListOfSpeciesTypeInstances: Only one <listOfSpeciesTypeInstances> is allowed in a <speciesType> ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "There may be at most one ListOfSpeciesTypeInstances container object within a SpeciesType object.",
    { "L3V1 Multi V1.1 Section 3.8"
    }
  },

  // 7020410
  { MultiLofSptInss_Elts,
    "ListOfSpeciesTypeInstances: Only allow speciesTypeInstance elements ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Apart from the general 'notes' and 'annotation' subobjects permitted on all SBML objects, a "
    "ListOfSpeciesTypeInstances container object may only contain SpeciesTypeInstance objects.",
    { "L3V1 Multi V1.1 Section 3.8.4"
    }
  },

  // 7020411
  { MultiLofSptInss_AllowedAtts,
    "ListOfSpeciesTypeInstances: Allowed attributes ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A ListOfSpeciesTypeInstances object may have the optional SBML core attributes 'metaid' and 'sboTerm'.  "
    "No other attributes from the SBML Level~3 Core namespace or the Multi namespace are permitted on a "
    "ListOfSpeciesTypeInstances.",
    { "L3V1 Multi V1.1 Section 3.8.4"
    }
  },

  // 7020412
  { MultiLofSptCpoInds_onlyOne,
    "ListOfSpeciesTypeComponentIndexes: Only one <listOfSpeciesTypeComponentIndexes> is allowed in a <speciesType> ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "There may be at most one ListOfSpeciesTypeComponentIndexes container object within a SpeciesType object.",
    { "L3V1 Multi V1.1 Section 3.8"
    }
  },

  // 7020413
  { MultiLofSptCpoInds_Elts,
    "ListOfSpeciesTypeComponentIndexes: Only allow speciesTypeComponentIndex elements ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Apart from the general 'notes' and 'annotation' subobjects permitted on all SBML objects, a "
    "ListOfSpeciesTypeComponentIndexes container object may only contain SpeciesTypeComponentIndex objects.",
    { "L3V1 Multi V1.1 Section 3.8.6"
    }
  },

  // 7020414
  { MultiLofSptCpoInds_AllowedAtts,
    "ListOfSpeciesTypeComponentIndexes: Allowed attributes ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A ListOfSpeciesTypeComponentIndexes object may have the optional SBML core attributes 'metaid' and 'sboTerm'. "
    "No other attributes from the SBML Level~3 Core namespace or the Multi namespace are permitted on a "
    "ListOfSpeciesTypeComponentIndexes object.",
    { "L3V1 Multi V1.1 Section 3.8.6"
    }
  },

  // 7020415
  { MultiLofInSptBnds_onlyOne,
    "ListOfInSpeciesTypeBonds: Only one <listOfInSpeciesTypeBonds> is allowed in a <speciesType> ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "There may be at most one ListOfInSpeciesTypeBonds container object within a SpeciesType object.",
    { "L3V1 Multi V1.1 Section 3.8"
    }
  },

  // 7020416
  { MultiLofInSptBnds_Elts,
    "ListOfInSpeciesTypeBonds: Only allow inSpeciesTypeBond elements ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Apart from the general 'notes' and 'annotation' subobjects permitted on all SBML objects, a "
    "ListOfInSpeciesTypeBonds container object may only contain InSpeciesTypeBond objects.",
    { "L3V1 Multi V1.1 Section 3.8.5"
    }
  },

  // 7020417
  { MultiLofInSptBnds_AllowedAtts,
    "ListOfInSpeciesTypeBonds: Allowed attributes ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A ListOfInSpeciesTypeBonds object may have the optional SBML core attributes 'metaid' and 'sboTerm'. "
    "No other attributes from the SBML Level~3 Core namespace or the Multi namespace are permitted on a "
    "ListOfInSpeciesTypeBonds object.",
    { "L3V1 Multi V1.1 Section 3.8.5"
    }
  },

  // 7020501
  { MultiBstSpt_Restrict,
    "BindingSiteSpeciesType: Not permitted to have listOfSpeciesTypeInstances ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A BindingSiteSpeciesType object is not permitted to have any ListOfSpeciesTypeInstances subobject.",
    { "L3V1 Multi V1.1 Section 3.8.7"
    }
  },

  // 7020601
  { MultiSpeFtrTyp_AllowedCoreAtts,
    "SpeciesFeatureType: Allowed SBML core attributes ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A SpeciesFeatureType object may have the optional SBML Level~3 Core attributes 'metaid' and 'sboTerm'. "
    "No other attributes from the SBML Level~3 Core namespace are permitted on a SpeciesFeatureType object.",
    { "L3V1 Multi V1.1 Section 3.9"
    }
  },

  // 7020602
  { MultiSpeFtrTyp_AllowedCoreElts,
    "SpeciesFeatureType: Allowed SBML core elements ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A SpeciesFeatureType object may have the optional SBML Level~3 Core subobjects for 'notes' and 'annotation'. "
    "No other elements from the SBML Level~3 Core namespace are permitted on a SpeciesFeatureType object.",
    { "L3V1 Multi V1.1 Section 3.9"
    }
  },

  // 7020603
  { MultiSpeFtrTyp_AllowedMultiAtts,
    "SpeciesFeatureType: Allowed Multi attributes ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A SpeciesFeatureType object must have the required attributes 'multi:id' and 'multi:occur', and may have "
    "the optional attribute 'multi:name'. No other attributes from the Multi namespace are permitted on a "
    "SpeciesFeatureType object.",
    { "L3V1 Multi V1.1 Section 3.9"
    }
  },

  // 7020604
  { MultiSpeFtrTyp_OccAtt_Ref,
    "SpeciesFeatureType: 'occur' must be a positiveInteger ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the 'multi:occur' attribute on a given SpeciesFeatureType object must conform to the syntax "
    "of the SBML data type 'positiveInteger'.",
    { "L3V1 Multi V1.1 Section 3.9.2"
    }
  },

  // 7020605
  { MultiSpeFtrTyp_RestrictElt,
    "SpeciesFeatureType: Required to have one listOfPossibleSpeciesFeatureValues ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "One ListOfPossibleSpeciesFeatureValues subobject in a SpeciesFeatureType object is required. ",
    { "L3V1 Multi V1.1 Section 3.9.3"
    }
  },

  // 7020606
  { MultiLofPsbSpeFtrVals_AllowedAtts,
    "ListOfPossibleSpeciesFeatureValues: Allowed attributes ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A ListOfPossibleSpeciesFeatureValues object may have the optional SBML core attributes 'metaid' and 'sboTerm'. "
    "No other attributes from the SBML Level~3 Core namespace or the Multi namespace are permitted on a "
    "ListOfPossibleSpeciesFeatureValues object. ",
    { "L3V1 Multi V1.1 Section 3.9.3"
    }
  },

  // 7020607
  { MultiLofPsbSpeFtrVals_Elts,
    "ListOfPossibleSpeciesFeatureValues: Only allow possibleSpeciesFeatureValue elements ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Apart from the general 'notes' and 'annotation' subobjects permitted on all SBML objects, a "
    "ListOfPossibleSpeciesFeatureValues container object may only contain PossibleSpeciesFeatureValue objects. ",
    { "L3V1 Multi V1.1 Section 3.9.3"
    }
  },

  // 7020608
  { MultiLofPsbSpeFtrVals_NoEmpty,
    "ListOfPossibleSpeciesFeatureValues: Must not be empty ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A ListOfPossibleSpeciesFeatureValues object must not be empty. ",
    { "L3V1 Multi V1.1 Section 3.9.3"
    }
  },

  // 7020701
  { MultiPsbSpeFtrVal_AllowedCoreAtts,
    "PossibleSpeciesFeatureValue: Allowed SBML core attributes ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A PossibleSpeciesFeatureValue object may have the optional SBML Level~3 Core attributes 'metaid' and 'sboTerm'. "
    "No other attributes from the SBML Level~3 Core namespace are permitted on a PossibleSpeciesFeatureValue object. ",
    { "L3V1 Multi V1.1 Section 3.10"
    }
  },

  // 7020702
  { MultiPsbSpeFtrVal_AllowedCoreElts,
    "PossibleSpeciesFeatureValue: Allowed SBML core elements ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A PossibleSpeciesFeatureValue object may have the optional SBML Level~3 Core subobjects for 'notes' and 'annotation'. "
    "No other elements from the SBML Level~3 Core namespace are permitted on a PossibleSpeciesFeatureValue object. ",
    { "L3V1 Multi V1.1 Section 3.10"
    }
  },

  // 7020703
  { MultiPsbSpeFtrVal_AllowedMultiAtts,
    "PossibleSpeciesFeatureValue: Allowed Multi attributes ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A PossibleSpeciesFeatureValue object must have the required attribute 'multi:id', and may have the optional attributes "
    "'multi:name' and 'multi:numericValue'. No other attributes from the Multi namespace are permitted on a "
    "PossibleSpeciesFeatureValue object. ",
    { "L3V1 Multi V1.1 Section 3.10"
    }
  },

  // 7020704
  { MultiPsbSpeFtrVal_NumAtt_Ref,
    "PossibleSpeciesFeatureValue: 'numbericValue' must be the 'id' of a parameter ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the 'multi:numericValue' attribute on a given PossibleSpeciesFeatureValue object must be the identifier of "
    "a Parameter object defined in the same Model object. ",
    { "L3V1 Multi V1.1 Section 3.10.2"
    }
  },

  // 7020801
  { MultiSptIns_AllowedCoreAtts,
    "SpeciesTypeInstance: Allowed SBML core attributes ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A SpeciesTypeInstance object may have the optional SBML Level~3 Core attributes 'metaid' and 'sboTerm'.  "
    "No other attributes from the SBML Level~3 Core namespace are permitted on a SpeciesTypeInstance object. ",
    { "L3V1 Multi V1.1 Section 3.11"
    }
  },

  // 7020802
  { MultiSptIns_AllowedCoreElts,
    "SpeciesTypeInstance: Allowed SBML core elements ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A SpeciesTypeInstance object may have the optional SBML Level~3 Core subobjects for 'notes' and 'annotation'. "
    "No other elements from the SBML Level~3 Core namespace are permitted on a SpeciesTypeInstance object. ",
    { "L3V1 Multi V1.1 Section 3.11"
    }
  },

  // 7020803
  { MultiSptIns_AllowedMultiAtts,
    "SpeciesTypeInstance: Allowed Multi attributes ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A SpeciesTypeInstance object must have the required attributes 'multi:id', 'multi:speciesType' and 'multi:occur', "
    "and may have the optional attributes 'multi:name' and 'mulit:compartmentReference'. No other attributes from "
    "the Multi namespace are permitted on a SpeciesTypeInstance object. ",
    { "L3V1 Multi V1.1 Section 3.11"
    }
  },

  // 7020805
  { MultiSptIns_SptAtt_Ref,
    "SpeciesTypeInstance: 'speciesType' must be the 'id' of a speciesType ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the 'multi:speciesType' attribute on a given SpeciesTypeInstance object must be the identifier "
    "of a SpeciesType object defined in the same Model object. ",
    { "L3V1 Multi V1.1 Section 3.11.2"
    }
  },

  // 7020806
  { MultiSptIns_CpaRefAtt_Ref,
    "SpeciesTypeInstance: 'compartmentReference' must be the 'id' of a compartmentReference ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the 'multi:compartmentReference' attribute, if present on a given SpeciesTypeInstance object, "
    "must be the identifier of a CompartmentReference object defined in the same Model object. ",
    { "L3V1 Multi V1.1 Section 3.11.3"
    }
  },

  // 7020901
  { MultiSptCpoInd_AllowedCoreAtts,
    "SpeciesTypeComponentIndex: Allowed SBML core attributes ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A SpeciesTypeComponentIndex object may have the optional SBML Level~3 Core attributes 'metaid' and 'sboTerm'. "
    "No other attributes from the SBML Level~3 Core namespace are permitted on a SpeciesTypeComponentIndex object.",
    { "L3V1 Multi V1.1 Section 3.12"
    }
  },

  // 7020902
  { MultiSptCpoInd_AllowedCoreElts,
    "SpeciesTypeComponentIndex: Allowed SBML core elements ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A SpeciesTypeComponentIndex object may have the optional SBML Level~3 Core subobjects for 'notes' and 'annotation'. "
    "No other elements from the SBML Level~3 Core namespace are permitted on a SpeciesTypeComponentIndex object.",
    { "L3V1 Multi V1.1 Section 3.12"
    }
  },

  // 7020903
  { MultiSptCpoInd_AllowedMultiAtts,
    "SpeciesTypeComponentIndex: Allowed Multi attributes ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A SpeciesTypeComponentIndex object must have the required attributes 'multi:id' and 'multi:compartment' "
    ", and may have the optional attribute 'mulit:identifyingParent'. No other attributes from the "
    "Multi namespace are permitted on a SpeciesTypeComponentIndex object.",
    { "L3V1 Multi V1.1 Section 3.12"
    }
  },

  // 7020904
  { MultiSptCpoInd_CpoAtt_Ref,
    "SpeciesTypeComponentIndex: 'component' must be the 'id' of a component ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the 'mulit:component' attribute on a given SpeciesTypeComponentIndex object must be the identifier "
    "of a SpeciesTypeInstance object, or a SpeciesTypeComponentIndex object under the SpeciesType object that this "
    "SpeciesTypeComponentIndex object belongs to, or the SpeciesType object itself.",
    { "L3V1 Multi V1.1 Section 3.12.2"
    }
  },

  // 7020907
  { MultiSptCpoInd_IdParAtt_Ref,
    "SpeciesTypeComponentIndex: 'identifyingParent' must be the 'id' of a component ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the 'multi:identifyingParent' attribute on a given SpeciesTypeComponentIndex object must be the identifier "
    "of a component object under the SpeciesType object that this SpeciesTypeComponentIndex object belongs to. A component "
    "object can be an object of SpeciesTypeInstance, SpeciesTypeComponentIndex or SpeciesType.",
    { "L3V1 Multi V1.1 Section 3.12.3"
    }
  },

  // 7021101
  { MultiInSptBnd_AllowedCoreAtts,
    "InSpeciesTypeBond: Allowed SBML core attributes ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An InSpeciesTypeBond object may have the optional SBML Level~3 Core attributes 'metaid' and 'sboTerm'. "
    "No other attributes from the SBML Level~3 Core namespace are permitted on an InSpeciesTypeBond object.",
    { "L3V1 Multi V1.1 Section 3.13"
    }
  },

  // 7021102
  { MultiInSptBnd_AllowedCoreElts,
    "InSpeciesTypeBond: Allowed SBML core elements ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An InSpeciesTypeBond object may have the optional SBML Level~3 Core subobjects for 'notes' and 'annotation'. "
    "No other elements from the SBML Level~3 Core namespace are permitted on an InSpeciesTypeBond object.",
    { "L3V1 Multi V1.1 Section 3.13"
    }
  },

  // 7021103
  { MultiInSptBnd_AllowedMultiAtts,
    "InSpeciesTypeBond: Allowed Multi attributes ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An InSpeciesTypeBond object must have the required attributes, 'multi:bindingSite1' and 'multi:BindingSite2', "
    "and may have the optional attributes, 'multi:id' and 'multi:name'. No other attributes from the Multi "
    "namespace are permitted on an InSpeciesTypeBond object.",
    { "L3V1 Multi V1.1 Section 3.13"
    }
  },

  // 7021104
  { MultiInSptBnd_Bst1Att_Ref,
    "InSpeciesTypeBond: 'bindingSite1' must be the 'id' of a speciesTypeInstance or speciesTypeComponentIndex ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the 'multi:bindingSite1' attribute on a given InSpeciesTypeBond object must be the identifier "
    "of a SpeciesTypeInstance object or SpeciesTypeComponentIndex which ultimately reference a object of "
    "BindingSiteSpeciesType.",
    { "L3V1 Multi V1.1 Section 3.13.2"
    }
  },

  // 7021105
  { MultiInSptBnd_Bst2Att_Ref,
    "InSpeciesTypeBond: 'bindingSite2' must be the 'id' of a speciesTypeInstance or speciesTypeComponentIndex ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the 'multi:bindingSite2' attribute on a given InSpeciesTypeBond object must be the identifier "
    "of a SpeciesTypeInstance object or SpeciesTypeComponentIndex which ultimately reference a object of "
    "BindingSiteSpeciesType.",
    { "L3V1 Multi V1.1 Section 3.13.2"
    }
  },

  // 7021106
  { MultiInSptBnd_TwoBstAtts_NotSame,
    "InSpeciesTypeBond: 'bindingSite1' and 'bindingSite2' can not reference the same binding site ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The 'multi:bindingSite1' and 'multi:bindingSite2' attributes must not reference the same BindingSiteSpeciesType object.",
    { "L3V1 Multi V1.1 Section 3.13.2"
    }
  },

  // 7021201
  { MultiExSpe_AllowedMultiAtts,
    "Extended Species: Allowed Multi attributes ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A Species object may have the optional attribute, 'multi:speciesType'. No other attributes from the Multi "
    "namespace are permitted on a Species object.",
    { "L3V1 Multi V1.1 Section 3.15"
    }
  },

  // 7021202
  { MultiExSpe_RestrictSpeciesTypeAtt,
    "Extended Species: SpeciesType attribute must have value of the id of a speciesType ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of a 'multi:speciesTypeAtt' attribute, if present on a Species object, must be the identifier of a "
    "SpeciesType object.",
    { "L3V1 Multi V1.1 Section 3.15.1"
    }
  },

  // 7021203
  { MultiExSpe_NoEmptyListOfDefs,
    "Extended Species: ListOf- subobjects must not be empty ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Two 'ListOf' subobjects with a Species object are optional, but if present, these container object must not be empty. "
    "Specifically, if any of the following two classes of objects are present on the Species object, it must not be empty: "
    "ListOfOutwardBindingSites and ListOfSpeciesFeatures.",
    { "L3V1 Multi V1.1 Section 3.15"
    }
  },

  // 7021204
  { MultiLofOutBsts_AllowedAtts,
    "ListOfOutwardBindingSites: Allowed attributes ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A ListOfOutwardBindingSites object may have the optional SBML core attributes 'metaid' and 'sboTerm'. "
    "No other attributes from the SBML Level~3 Core namespace or the Multi namespace are permitted on a "
    "ListOfOutwardBindingSites object.",
    { "L3V1 Multi V1.1 Section 3.15.2"
    }
  },

  // 7021205
  { MultiLofOutBsts_AllowedElts,
    "ListOfOutwardBindingSites: Allowed elements ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Apart from the general 'notes' and 'annotation' subobjects permitted on all SBML objects, a ListOfOutwardBindingSites "
    "container object may only contain OutwardBindingSite objects.",
    { "L3V1 Multi V1.1 Section 3.15.2"
    }
  },

  // 7021206
  { MultiLofSpeFtrs_AllowedAtts,
    "ListOfSpeciesFeatures: Allowed SBML attributes ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A ListOfSpeciesFeatures object may have the optional SBML core attributes 'metaid' and 'sboTerm'. "
    "No other attributes from the SBML Level~3 Core namespace or the Multi namespace are permitted on a ListOfSpeciesFeatures object.",
    { "L3V1 Multi V1.1 Section 3.15.3"
    }
  },

  // 7021207
  { MultiSubLofSpeFtrs_AllowedMultiAtts,
    "SubListOfSpeciesFeatures: Allowed Multi attributes ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A SubListOfSpeciesFeatures object may have the optional attributes multi:id, multi:relation "
    "and multi:component. No other attributes from the Multi namespace are permitted on a "
    "SubListOfSpeciesFeatures object. ",
    { "L3V1 Multi V1.1 Section 3.17"
    }
  },

  // 7021208
  { MultiSubLofSpeFtrs_RelationAtt_Ref,
    "SubListOfSpeciesFeatures: 'relation' must be a value of 'Relation' ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the 'multi:relation' attribute, if presented on a SubListOfSpeciesFeatures object, must conform "
    "to the syntax of the Multi data type 'Relation'.",
    { "L3V1 Multi V1.1 Section 3.17.2"
    }
  },

  // 7021209
  { MultiLofSpeFtrs_AllowedElts,
    "ListOfSpeciesFeatures: Allowed elements ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Apart from the general 'notes' and 'annotation' subobjects permitted on all SBML objects, a "
    "ListOfSpeciesFeatures container object may only contain SpeciesFeature and/or SubListOfSpeciesFeatures "
    "objects.",
    { "L3V1 Multi V1.1 Section 3.15.3"
    }
  },

  // 7021210
  { MultiSubLofSpeFtrs_AllowedCoreAtts,
    "SubListOfSpeciesFeatures: Allowed SBML Core attributes ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A SubListOfSpeciesFeatures object may have the optional SBML core attributes 'metaid' and 'sboTerm'.  "
    "No other attributes from the SBML Level~3 Core namespace are permitted on a SubListOfSpeciesFeatures object.  ",
    { "L3V1 Multi V1.1 Section 3.17"
    }
  },

  // 7021211
  { MultiSubLofSpeFtrs_AllowedElts,
    "SubListOfSpeciesFeatures: Allowed elements ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Apart from the general 'notes' and 'annotation' subobjects permitted on all SBML objects, a "
    "SubListOfSpeciesFeatures container object may only contain SpeciesFeature objects.",
    { "L3V1 Multi V1.1 Section 3.17"
    }
  },

  // 7021212
  { MultiSubLofSpeFtrs_CpoAtt_Ref,
    "SubListOfSpeciesFeatures: 'component' must be the 'id' of a 'SpeciesType' component ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the 'multi:component' attribute on a given SubListOfSpeciesFeatures object must be the identifier of an "
    "object of SpeciesTypeInstance, SpeciesTypeComponentIndex or SpeciesType which contains the SpeciesFeature objects in "
    "this subListOfSpeciesFeatures.",
    { "L3V1 Multi V1.1 Section 3.17.3"
    }
  },

  // 7021213
  { MultiExSpe_ReqSpt_LofOutBsts,
    "Extended Species: 'speciesType' is required when it has a 'listOfOutwardBindingSites' ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A species must have its 'speciesType' attribute defined when it has a 'listOfOutwardBindingSites.' ",
    { "L3V1 Multi V1.1 Section 3.15"
    }
  },

  // 7021214
  { MultiExSpe_ReqSpt_LofSpeFtrs,
    "Extended Species: 'speciesType' is required when it has a 'listOfSpeciesFeatures' ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A species must have its 'speciesType' attribute defined when it has a 'listOfSpeciesFeatures.' ",
    { "L3V1 Multi V1.1 Section 3.15"
    }
  },

  // 7021215
  { MultiSubLofSpeFtrs_RelationAndOcc,
    "SubListOfSpeciesFeatures: 'relation' can only be 'and' when referencing a speciesFeatureType with occur > 1 ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The 'relation' attribute of a subListOfSpeciesFeatures can only have the value 'and' if any speciesFeature involved "
    "references a speciesFeatureType with occur > 1 ",
    { "L3V1 Multi V1.1 Section 3.17.2"
    }
  },

  // 7021216
  { MultiSubLofSpeFtrs_TwoSpeFtrs,
    "SubListOfSpeciesFeatures: must have at least two 'speciesFeatures' ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A SubListOfSpeciesFeatures object must have at least two speciesFeatures.",
    { "L3V1 Multi V1.1 Section 3.17"
    }
  },

  // 7021301
  { MultiOutBst_AllowedCoreAtts,
    "OutwardBindingSite: Allowed SBML core attributes ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An OutwardBindingSite object may have the optional SBML Level~3 Core attributes 'metaid' and 'sboTerm.  "
    "No other attributes from the SBML Level~3 Core namespace are permitted on an OutwardBindingSite object.",
    { "L3V1 Multi V1.1 Section 3.16"
    }
  },

  // 7021302
  { MultiOutBst_AllowedCoreElts,
    "OutwardBindingSite: Allowed SBML core elements ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An OutwardBindingSite object may have the optional SBML Level~3 Core subobjects for 'notes' and 'annotation.  "
    "No other elements from the SBML Level~3 Core namespace are permitted on an OutwardBindingSite object.",
    { "L3V1 Multi V1.1 Section 3.16"
    }
  },

  // 7021303
  { MultiOutBst_AllowedMultiAtts,
    "OutwardBindingSite: Allowed Multi attributes ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An OutwardBindingSite object must have the required attributes, 'multi:bindingStatus' and 'mulit:component'. "
    "No other attributes from the Multi namespace are permitted on an OutwardBindingSite object.",
    { "L3V1 Multi V1.1 Section 3.16"
    }
  },

  // 7021304
  { MultiOutBst_BdgStaAtt_Ref,
    "OutwardBindingSite: 'bindingStatus' must have a value of 'BindingStatus' ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the 'multi:bindingStatus' attribute on a given OutwardBindingSite object must confirm to the "
    "syntax of the Multi data type 'BindingStatus'.",
    { "L3V1 Multi V1.1 Section 3.16.2"
    }
  },

  // 7021305
  { MultiOutBst_CpoAtt_Ref,
    "OutwardBindingSite: 'component' must be the 'id' of a 'BindingSiteSpeciesType' component ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the 'multi:component' attribute on a given OutwardBindingSite object must be the identifier of an "
    "object of SpeciesTypeInstance, SpeciesTypeComponentIndex or SpeciesType which ultimately reference an object of "
    "BindingSiteSpeciesType.",
    { "L3V1 Multi V1.1 Section 3.16.3"
    }
  },

  // 7021306
  { MultiOutBst_NotInBond,
    "OutwardBindingSite: An outwardBindingSite can not be in a bond of the species ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An outwardBindingSite can not be a binding site referenced by any inSpeciesTypeBond in the species.",
    { "L3V1 Multi V1.1 Section 3.16.3"
    }
  },

  // 7021401
  { MultiSpeFtr_AllowedCoreAtts,
    "SpeciesFeature: Allowed SBML core attributes ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A SpeciesFeature object may have the optional SBML Level~3 Core attributes 'metaid' and 'sboTerm'. "
    "No other attributes from the SBML Level~3 Core namespace are permitted on a SpeciesFeature object.",
    { "L3V1 Multi V1.1 Section 3.16"
    }
  },

  // 7021402
  { MultiSpeFtr_AllowedCoreElts,
    "SpeciesFeature: Allowed SBML core elements ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A SpeciesFeature object may have the optional SBML Level~3 Core subobjects for 'notes' and 'annotation'. "
    "No other elements from the SBML Level~3 Core namespace are permitted on a SpeciesFeature object.",
    { "L3V1 Multi V1.1 Section 3.18"
    }
  },

  // 7021403
  { MultiSpeFtr_AllowedMultiAtts,
    "SpeciesFeature: Allowed Multi attributes ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A SpeciesFeature object must have the required attributes, 'multi:speciesFeatureType' and 'mulit:occur', "
    "and may have the optional attribute, 'multi:id' and 'multi:component'. No other attributes from the Multi "
    "namespace are permitted on a SpeciesFeature object.",
    { "L3V1 Multi V1.1 Section 3.18"
    }
  },

  // 7021404
  { MultiSpeFtr_SpeFtrTypAtt_Ref,
    "SpeciesFeature: 'speciesFeatureType' must be the 'id' of a speciesFeatureType ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the 'multi:speciesFeatureType' attribute on a given SpeciesFeature object must be the identifier "
    "of a SpeciesFeatureType object which is in the SpeciesType object referenced by the Species object containing "
    "this SpeciesFeature object.",
    { "L3V1 Multi V1.1 Section 3.18.2"
    }
  },

  // 7021405
  { MultiSpeFtr_OccAtt_Ref,
    "SpeciesFeature: 'occur' must be a positiveInteger with restriction ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the 'mulit:occur' attribute on a given SpeciesFeature object must conform to the syntax of the SBML "
    "data type 'positiveInteger'. The value of the 'multi:occur' attribute must not be larger than that of the 'multi:occur' "
    "attribute of the SpeciesFeatureType object referenced by this SpeciesFeature object.",
    { "L3V1 Multi V1.1 Section 3.18.3"
    }
  },

  // 7021406
  { MultiSpeFtr_CpoAtt_Ref,
    "SpeciesFeature: 'component' must be the 'id' of a component ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the 'multi:component' attribute on a given SpeciesFeature object must be the identifier of an object of "
    "SpeciesTypeInstance, SpeciesTypeComponentIndex or SpeciesType which contains this SpeciesFeature object.",
    { "L3V1 Multi V1.1 Section 3.18.4"
    }
  },

  // 7021407
  { MultiSpeFtr_RestrictElts,
    "SpeciesFeature: Required one listOfSpeciesFeatureValues ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "One ListOfSpeciesFeatureValues subobject within a SpeciesFeature object is required.",
    { "L3V1 Multi V1.1 Section 3.18.5"
    }
  },

  // 7021408
  { MultiLofSpeFtrVals_NoEmpty,
    "ListOfSpeciesFeatureValues: Must not be empty ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A ListOfSpeciesFeatureValues object must not be empty.",
    { "L3V1 Multi V1.1 Section 3.18.5"
    }
  },

  // 7021409
  { MultiLofSpeFtrVals_AllowedAtts,
    "ListOfSpeciesFeatureValues: Allowed attributes ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A ListOfSpeciesFeatureValues object may have the optional SBML core attributes 'metaid' and 'sboTerm'.  "
    "No other attributes from the SBML Level~3 Core namespace or the Multi namespace are permitted on a "
    "ListOfSpeciesFeatureValues object.",
    { "L3V1 Multi V1.1 Section 3.18.5"
    }
  },

  // 7021410
  { MultiLofSpeFtrVals_AllowedElts,
    "ListOfSpeciesFeatureValues: Allowed elements ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Apart from the general 'notes' and 'annotation' subobjects permitted on all SBML objects, a "
    "ListOfSpeciesFeatureValues container object may only contain SpeciesFeatureValue objects.",
    { "L3V1 Multi V1.1 Section 3.18.5"
    }
  },

  // 7021501
  { MultiSpeFtrVal_AllowedCoreAtts,
    "SpeciesFeatureValue: Allowed SBML core attributes ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A SpeciesFeatureValue object may have the optional SBML Level~3 Core attributes 'metaid' and 'sboTerm'. "
    "No other attributes from the SBML Level~3 Core namespace are permitted on a SpeciesFeatureValue object.",
    { "L3V1 Multi V1.1 Section 3.18.6"
    }
  },

  // 7021502
  { MultiSpeFtrVal_AllowedCoreElts,
    "SpeciesFeatureValue: Allowed SBML core elements ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A SpeciesFeatureValue object may have the optional SBML Level~3 Core subobjects for 'notes' and 'annotation'. "
    "No other elements from the SBML Level~3 Core namespace are permitted on a SpeciesFeatureValue object.",
    { "L3V1 Multi V1.1 Section 3.18.6"
    }
  },

  // 7021503
  { MultiSpeFtrVal_AllowedMultiAtts,
    "SpeciesFeatureValue: Allowed Multi attributes ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A SpeciesFeatureValue object must have the required attribute 'multi:value'. No other attributes from the "
    "Multi namespace are permitted on a SpeciesFeatureValue object.",
    { "L3V1 Multi V1.1 Section 3.18.6"
    }
  },

  // 7021504
  { MultiSpeFtrVal_ValAtt_Ref,
    "SpeciesFeatureValue: 'value' must be the 'id' of a possibleSpeciesFeatureValue ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the 'multi:value' attribute on a given SpeciesFeatureValue object must be the identifier of a "
    "PossibleSpeciesFeatureValue object defined in the SpeciesFeatureType object referenced by the SpeciesFeature "
    "object containing this SpeciesFeatureValue object.",
    { "L3V1 Multi V1.1 Section 3.18.6"
    }
  },

  // 7021601
  { MultiIntSpeRec_AllowedAtts,
    "IntraSpeciesReaction: Allowed attributes ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An IntraSpeciesReaction object may have the optional SBML Level~3 Core attributes 'metaid' and 'sboTerm'. "
    "No other attributes from the SBML Level~3 Core namespace and the Multi namespace are permitted on an "
    "IntraSpeciesReaction object.",
    { "L3V1 Multi V1.1 Section 3.21"
    }
  },

  // 7021602
  { MultiIntSpeRec_AllowedCoreElts,
    "IntraSpeciesReaction: Allowed SBML core elements ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An IntraSpeciesReaction object may have the optional SBML Level~3 Core subobjects for 'notes' and 'annotation'. "
    "No other elements from the SBML Level~3 Core namespace are permitted on an IntraSpeciesReaction object.",
    { "L3V1 Multi V1.1 Section 3.21"
    }
  },

  // 7021701
  { MultiExSplSpeRef_AllowedMultiAtts,
    "Extended SimpleSpeciesReference: Allowed Multi attributes ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An extended SimpleSpeciesReference object may have the optional attribute, 'multi:compartmentReference'. "
    "No other attributes from the Multi namespace are permitted on a SimpleSpeciesReference object.",
    { "L3V1 Multi V1.1 Section 3.22"
    }
  },

  // 7021702
  { MultiExSplSpeRef_CpaRefAtt_Ref,
    "Extended SimpleSpeciesReference: 'compartmentReference' must be the 'id' of a compartmentReference ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "An extended SimpleSpeciesReference object may have the optional attribute, 'multi:compartmentReference'. "
    "No other attributes from the Multi namespace are permitted on a SimpleSpeciesReference object.",
    { "L3V1 Multi V1.1 Section 3.22"
    }
  },

  // 7021801
  { MultiLofSptCpoMapsInPro_NoEmpty,
    "ListOfSpeciesTypeComponentMapsInProduct: Must not be empty ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A ListOfSpeciesTypeComponentMapsInProduct object within an extended SpeciesReference object is optional, "
    "but if present, must not be empty.",
    { "L3V1 Multi V1.1 Section 3.23.1"
    }
  },

  // 7021802
  { MultiLofSptCpoMapsInPro_AllowedAtts,
    "ListOfSpeciesTypeComponentMapsInProduct: Allowed attributes ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A ListOfSpeciesTypeComponentMapsInProduct object may have the optional SBML core attributes 'metaid' and 'sboTerm'. "
    "No other attributes from the SBML Level~3 Core namespace or the Multi namespace are permitted on a "
    "ListOfSpeciesTypeComponentMapsInProduct object.",
    { "L3V1 Multi V1.1 Section 3.23.1"
    }
  },

  // 7021803
  { MultiLofSptCpoMapsInPro_AllowedElts,
    "ListOfSpeciesTypeComponentMapsInProduct: Allowed elements ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Apart from the general 'notes' and 'annotation' subobjects permitted on all SBML objects, a "
    "ListOfSpeciesTypeComponentMapsInProduct container object may only contain SpeciesTypeComponentMapInProduct objects.",
    { "L3V1 Multi V1.1 Section 3.23.1"
    }
  },

  // 7021901
  { MultiSptCpoMapInPro_AllowedCoreAtts,
    "SpeciesTypeComponentMapInProduct: Allowed SBML core attributes ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A SpeciesTypeComponentMapInProduct object may have the optional SBML Level~3 Core attributes 'metaid' and 'sboTerm'.  "
    "No other attributes from the SBML Level~3 Core namespace are permitted on a SpeciesTypeComponentMapInProduct object.",
    { "L3V1 Multi V1.1 Section 3.24"
    }
  },

  // 7021902
  { MultiSptCpoMapInPro_AllowedCoreElts,
    "SpeciesTypeComponentMapInProduct: Allowed SBML core elements ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A SpeciesTypeComponentMapInProduct object may have the optional SBML Level~3 Core subobjects for 'notes' and 'annotation'. "
    "No other elements from the SBML Level~3 Core namespace are permitted on a SpeciesTypeComponentMapInProduct object.",
    { "L3V1 Multi V1.1 Section 3.24"
    }
  },

  // 7021903
  { MultiSptCpoMapInPro_AllowedMultiAtts,
    "SpeciesTypeComponentMapInProduct: Allowed Multi attributes ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "A SpeciesTypeComponentMapInProduct object must have the required attributes 'multi:reactant', 'multi:reactantComponent', "
    "and 'multi:productComponent'. No other attributes from the Multi namespace are permitted on a "
    "SpeciesTypeComponentMapInProduct object.",
    { "L3V1 Multi V1.1 Section 3.24"
    }
  },

  // 7021904
  { MultiSptCpoMapInPro_RctAtt_Ref,
    "SpeciesTypeComponentMapInProduct: 'reactant' must be the 'id' of a reactant speciesReference ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the 'multi:reactant' attribute on a given SpeciesTypeComponentMapInProduct object must be the identifier "
    "of a reactant SpeciesReference object within a reaction.",
    { "L3V1 Multi V1.1 Section 3.24.2"
    }
  },

  // 7021905
  { MultiSptCpoMapInPro_RctCpoAtt_Ref,
    "SpeciesTypeComponentMapInProduct: 'reactantComponent' must be the 'id' of a reactant component ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the 'multi:reactantComponent' attribute on a given SpeciesTypeComponentMapInProduct object must be the identifier "
    "of an object of SpeciesTypeInstance, SpeciesTypeComponentIndex or SpeciesType.",
    { "L3V1 Multi V1.1 Section 3.24.3"
    }
  },

  // 7021906
  { MultiSptCpoMapInPro_ProCpoAtt_Ref,
    "SpeciesTypeComponentMapInProduct: 'productComponent' must be the 'id' of a product component ",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of the 'multi:productComponent' attribute on a given SpeciesTypeComponentMapInProduct object must be the identifier "
    "of an object of SpeciesTypeInstance, SpeciesTypeComponentIndex or SpeciesType.",
    { "L3V1 Multi V1.1 Section 3.24.4"
    }
  },


 // 7022001 SK renumbered from 7020301
  { MultiCpaRef_AllowedCoreAtts,
      "CompartmentReference: Allowed SBML core attributes ",
      LIBSBML_CAT_GENERAL_CONSISTENCY,
      LIBSBML_SEV_ERROR,
      "A CompartmentReference object may have the optional SBML Level~3 Core attributes 'metaid' and "
      "'sboTerm'.  No other attributes from the SBML Level~3 Core namespace are permitted on a "
      "'CompartmentReference object.",
      { "L3V1 Multi V1.1 Section 3.6"
      }
  },

      // 7022002 SK renumbered from 7020302
  { MultiCpaRef_AllowedCoreElts,
      "CompartmentReference: Allowed SBML core elements ",
      LIBSBML_CAT_GENERAL_CONSISTENCY,
      LIBSBML_SEV_ERROR,
      "A CompartmentReference object may have the optional SBML Level~3 Core subobjects for 'notes' and "
      "'annotation'.  No other elements from the SBML Level~3 Core namespace are permitted on a "
      "CompartmentReference object.",
      { "L3V1 Multi V1.1 Section 3.6"
      }
  },

      // 7022003 SK renumbered from 7020303
  { MultiCpaRef_AllowedMultiAtts,
      "CompartmentReference: Allowed Multi attributes ",
      LIBSBML_CAT_GENERAL_CONSISTENCY,
      LIBSBML_SEV_ERROR,
      "A CompartmentReference object must have the required attribute 'multi:compartment', and may have "
      "the optional attributes 'multi:id' and 'multi:name'. No other attributes from the Multi namespace "
      "are permitted on a CompartmentReference object.",
      { "L3V1 Multi V1.1 Section 3.6"
      }
  },

      // 7022004 SK renumbered from 7020304
  { MultiCpaRef_CompartmentAtt_Ref,
      "CompartmentReference: 'compartment' must be the 'id' of a compartment ",
      LIBSBML_CAT_GENERAL_CONSISTENCY,
      LIBSBML_SEV_ERROR,
      "The value of the 'multi:compartment' attribute must be the value of an 'id' attribute on an existing "
      "Compartment object in the 'SId' namespace of the parent model.",
      { "L3V1 Multi V1.1 Section 3.6"
      }
  },

      // 7022005 SK renumbered from 7020305
  { MultiCpaRef_IdRequiredOrOptional,
      "CompartmentReference: 'multi:id' is required when referencing the same compartment ",
      LIBSBML_CAT_GENERAL_CONSISTENCY,
      LIBSBML_SEV_ERROR,
      "If some or all compartmentReferences within a ListOfCompartmentReferences object reference the same "
      "compartment, those compartmentReferences are required to have its 'multi:id' attribute defined to "
      "distinguish different compartmentReferences.",
      { "L3V1 Multi V1.1 Section 3.6.1"
      }
  },

      // 7022006 SK renumbered from 7020306
  { MultiCpaRef_NoReferenceToAnyParent,
      "CompartmentReference: A compartmentReference cannot reference any parent compartment ",
      LIBSBML_CAT_GENERAL_CONSISTENCY,
      LIBSBML_SEV_ERROR,
      "A <compartmentReference> cannot reference a <compartment> that directly or "
      "indirectly contains teh <compartmentReference>.",
      { "L3V1 Multi V1.1 Section 3.6.1"
      }
  },



// SK moved block to numbers 7010201
  //// 7022101
  //{ MultiMathCi_AllowedMultiAtts,
  //  "Math ci element: Allowed Multi attributes ",
  //  LIBSBML_CAT_GENERAL_CONSISTENCY,
  //  LIBSBML_SEV_ERROR,
  //  "A 'ci' element in a Math object may have the optional attributes 'multi:speciesReference' and 'multi:representationType'. "
  //  "No other attributes from the Multi namespace are permitted on a 'ci' element. ",
  //  { "L3V1 Multi V1.1 Section 3.26"
  //  }
  //},

  //// 7022102
  //{ MultiMathCi_SpeRefAtt_Ref,
  //  "Math ci element: 'speciesReference' must be the 'id' of a speciesReference ",
  //  LIBSBML_CAT_GENERAL_CONSISTENCY,
  //  LIBSBML_SEV_ERROR,
  //  "The value of the 'multi:speciesReference' attribute on a given 'ci' element must be the identifier of a SpeciesReference "
  //  "object within the same reaction. ",
  //  { "L3V1 Multi V1.1 Section 3.26.1"
  //  }
  //},

  //// 7022103
  //{ MultiMathCi_RepTypAtt_Ref,
  //  "Math ci element: 'representationType' must be a value of the Multi data type 'RepresentationType' ",
  //  LIBSBML_CAT_GENERAL_CONSISTENCY,
  //  LIBSBML_SEV_ERROR,
  //  "The value of the 'multi:representationType' attribute on a given 'ci' element must conform to the syntax of the Multi data "
  //  "type 'RepresentationType'. ",
  //  { "L3V1 Multi V1.1 Section 3.26.2"
  //  }
  //},



};


LIBSBML_CPP_NAMESPACE_END

  /** @endcond */


#endif  /*  MultiSBMLErrorTable_h__  */

