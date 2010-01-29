/**
 * @file    SBMLTypeCodes.h
 * @brief   Enumeration to identify SBML objects at runtime
 * @author  Ben Bornstein
 *
 * $Id$
 * $HeadURL$
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2010 California Institute of Technology.
 * Copyright 2002-2005 California Institute of Technology and
 *                     Japan Science and Technology Corporation.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *----------------------------------------------------------------------- -->*/

#ifndef SBMLTypeCodes_h
#define SBMLTypeCodes_h


#include <sbml/common/libsbml-config.h>
#include <sbml/common/extern.h>

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS


/**
 * An enumeration of SBML types to help identify SBML objects at runtime.
 * Abstract types do not have a typecode since they cannot be instantiated.
 */
typedef enum
{
    SBML_UNKNOWN

  , SBML_COMPARTMENT
  , SBML_COMPARTMENT_TYPE
  , SBML_CONSTRAINT
  , SBML_DOCUMENT
  , SBML_EVENT
  , SBML_EVENT_ASSIGNMENT
  , SBML_FUNCTION_DEFINITION
  , SBML_INITIAL_ASSIGNMENT
  , SBML_KINETIC_LAW
  , SBML_LIST_OF
  , SBML_MODEL
  , SBML_PARAMETER
  , SBML_REACTION
  , SBML_RULE
  , SBML_SPECIES
  , SBML_SPECIES_REFERENCE
  , SBML_SPECIES_TYPE
  , SBML_MODIFIER_SPECIES_REFERENCE
  , SBML_UNIT_DEFINITION
  , SBML_UNIT
  , SBML_ALGEBRAIC_RULE
  , SBML_ASSIGNMENT_RULE
  , SBML_RATE_RULE
  , SBML_SPECIES_CONCENTRATION_RULE
  , SBML_COMPARTMENT_VOLUME_RULE
  , SBML_PARAMETER_RULE
  , SBML_TRIGGER
  , SBML_DELAY
  , SBML_STOICHIOMETRY_MATH
  , SBML_LOCAL_PARAMETER

#ifdef USE_LAYOUT
  , SBML_LAYOUT_BOUNDINGBOX
  , SBML_LAYOUT_COMPARTMENTGLYPH
  , SBML_LAYOUT_CUBICBEZIER
  , SBML_LAYOUT_CURVE
  , SBML_LAYOUT_DIMENSIONS
  , SBML_LAYOUT_GRAPHICALOBJECT
  , SBML_LAYOUT_LAYOUT
  , SBML_LAYOUT_LINESEGMENT
  , SBML_LAYOUT_POINT
  , SBML_LAYOUT_REACTIONGLYPH
  , SBML_LAYOUT_SPECIESGLYPH
  , SBML_LAYOUT_SPECIESREFERENCEGLYPH
  , SBML_LAYOUT_TEXTGLYPH
#endif  /* USE_LAYOUT */

  , SBML_FORMULA_UNITS_DATA
  , SBML_LIST_FORMULA_UNITS_DATA
  , SBML_LISTOF_COMPARTMENTS
  , SBML_LISTOF_COMPARTMENT_TYPES
  , SBML_LISTOF_CONSTRAINTS
  , SBML_LISTOF_EVENTS
  , SBML_LISTOF_EVENT_ASSIGNMENTS
  , SBML_LISTOF_FUNCTION_DEFINITIONS
  , SBML_LISTOF_INITIAL_ASSIGNMENTS
  , SBML_LISTOF_PARAMETERS
  , SBML_LISTOF_REACTIONS
  , SBML_LISTOF_RULES
  , SBML_LISTOF_SPECIES
  , SBML_LISTOF_SPECIES_REFERENCES
  , SBML_LISTOF_SPECIES_TYPES
  , SBML_LISTOF_UNIT_DEFINITIONS
  , SBML_LISTOF_UNITS

} SBMLTypeCode_t;


/**
 * This method takes an SBML type code and returns a string representing
 * the code.
 *
 * @if clike LibSBML attaches an identifying code to every
 * kind of SBML object.  These are known as <em>SBML type codes</em>.
 * The set of possible type codes is defined in the enumeration
 * #SBMLTypeCode_t.  The names of the type codes all begin with the
 * characters @c SBML_. @endif@if java LibSBML attaches an
 * identifying code to every kind of SBML object.  These are known as
 * <em>SBML type codes</em>.  In other languages, the set of type codes
 * is stored in an enumeration; in the Java language interface for
 * libSBML, the type codes are defined as static integer constants in
 * interface class {@link libsbmlConstants}.  The names of the type codes
 * all begin with the characters @c SBML_. @endif
 * This method takes a type code as argument, and returns a string name
 * corresponding to that code.  For example, passing it the type code
 * <code>SBML_COMPARTMENT</code> will return the string
 * "<code>Compartment</code>". 
 *
 * @return a human readable name for the given SBMLTypeCode_t.
 *
 * @note The caller does not own the returned string and is therefore not
 * allowed to modify it.
 */
LIBSBML_EXTERN
const char *
SBMLTypeCode_toString (SBMLTypeCode_t tc);


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END

#endif  /* SBMLTypeCodes_h */
