/**
 * @file L3v2extendedmathSBMLErrorTable.h
 * @brief Definition of the L3v2extendedmathSBMLErrorTable class.
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
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


#ifndef L3v2extendedmathSBMLErrorTable_H__
#define L3v2extendedmathSBMLErrorTable_H__


#include <sbml/packages/l3v2extendedmath/validator/L3v2extendedmathSBMLError.h>


LIBSBML_CPP_NAMESPACE_BEGIN



/** @cond doxygenLibsbmlInternal */

static const packageErrorTableEntry l3v2extendedmathErrorTable[] =
{
  // 1410100
  { L3v2extendedmathUnknown,
    "Unknown error from L3v2extendedmath",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Unknown error from L3v2extendedmath",
    { ""
    }
  },

  // 1410101
  { L3v2extendedmathNSUndeclared,
    "The L3v2extendedmath namespace is not correctly declared.",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "To conform to the Extended Math for L3V2 Package specification for SBML "
    "Level 3 Version 1, an SBML document must declare "
    "'http://www.sbml.org/sbml/level3/version1/l3v2extendedmath/version1' as "
    "the XMLNamespace to use for elements of this package.",
    { "L3V1 L3v2extendedmath V1 Section 3.1"
    }
  },

  // 1410102
  { L3v2extendedmathElementNotInNs,
    "Element not in L3v2extendedmath namespace",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "Wherever they appear in an SBML document, elements and attributes from the "
    "Extended Math for L3V2 Package must use the "
    "'http://www.sbml.org/sbml/level3/version2/l3v2extendedmath/version1' "
    "namespace, declaring so either explicitly or implicitly.",
    { "L3V1 L3v2extendedmath V1 Section 3.1"
    }
  },


  //1410218
  {
    L3v2EMOpsNeedCorrectNumberOfArgs,
    "Incorrect number of arguments given to MathML operator",
  LIBSBML_CAT_MATHML_CONSISTENCY,
  LIBSBML_SEV_ERROR,
  "A MathML operator must be supplied the number of arguments "
  "appropriate for that operator.",
  { 
  "L3V2 Section 3.4.1" }
  },


  // 1410301
  { L3v2extendedmathDuplicateComponentId,
    "Duplicate 'id' attribute value",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "(Extends validation rule #10301 in the SBML Level 3 Core specification. TO "
    "DO list scope of ids)",
    { "L3V1 L3v2extendedmath V1 Section"
    }
  },

  // 1410302
  { L3v2extendedmathIdSyntaxRule,
    "Invalid SId syntax",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of a 'l3v2extendedmath:id' must conform to the syntax of the "
    "<sbml> data type 'SId'",
    { "L3V1 L3v2extendedmath V1 Section"
    }
  },

  //1410501
  {
    L3v2EMInconsistentArgUnits,
    "The units of the function call's arguments are not consistent with its definition",
  LIBSBML_CAT_UNITS_CONSISTENCY,
  LIBSBML_SEV_WARNING,
  "The units of the expressions used as arguments to a function call are expected to "
  "match the units expected for the arguments of that function. ",
  {"L3V2 Section 3.4" }
  },


  // 1420101
  { L3v2extendedmathAttributeRequiredMissing,
    "Required l3v2extendedmath:required attribute on <sbml>",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "In all SBML documents using the Extended Math for L3V2 Package, the <sbml> "
    "object must have the 'l3v2extendedmath:required' attribute.",
    { "L3V1 L3v2extendedmath V1 Section"
    }
  },

  // 1420102
  { L3v2extendedmathAttributeRequiredMustBeBoolean,
    "The l3v2extendedmath:required attribute must be Boolean",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of attribute 'l3v2extendedmath:required' on the <sbml> object "
    "must be of data type 'boolean'.",
    { "L3V1 L3v2extendedmath V1 Section"
    }
  },

  // 1420103
  { L3v2extendedmathAttributeRequiredMustHaveValue,
    "The l3v2extendedmath:required attribute must be 'true'",
    LIBSBML_CAT_GENERAL_CONSISTENCY,
    LIBSBML_SEV_ERROR,
    "The value of attribute 'l3v2extendedmath:required' on the <sbml> object "
    "must be set to 'true'.",
    { "L3V1 L3v2extendedmath V1 Section"
    }
  },


};

/** @endcond */



LIBSBML_CPP_NAMESPACE_END




#endif /* !L3v2extendedmathSBMLErrorTable_H__ */


