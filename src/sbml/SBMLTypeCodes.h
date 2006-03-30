/**
 * \file    SBMLTypeCodes.h
 * \brief   Enumeration to identify SBML objects at runtime
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2002 California Institute of Technology and
 * Japan Science and Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 2.1 of the License, or
 * any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY, WITHOUT EVEN THE IMPLIED WARRANTY OF
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  The software and
 * documentation provided hereunder is on an "as is" basis, and the
 * California Institute of Technology and Japan Science and Technology
 * Corporation have no obligations to provide maintenance, support,
 * updates, enhancements or modifications.  In no event shall the
 * California Institute of Technology or the Japan Science and Technology
 * Corporation be liable to any party for direct, indirect, special,
 * incidental or consequential damages, including lost profits, arising
 * out of the use of this software and its documentation, even if the
 * California Institute of Technology and/or Japan Science and Technology
 * Corporation have been advised of the possibility of such damage.  See
 * the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 *
 * The original code contained here was initially developed by:
 *
 *     Ben Bornstein
 *     The Systems Biology Markup Language Development Group
 *     ERATO Kitano Symbiotic Systems Project
 *     Control and Dynamical Systems, MC 107-81
 *     California Institute of Technology
 *     Pasadena, CA, 91125, USA
 *
 *     http://www.cds.caltech.edu/erato
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s):
 */


#ifndef SBMLTypeCodes_h
#define SBMLTypeCodes_h


#include "common/libsbml-config.h"
#include "common/extern.h"


BEGIN_C_DECLS


/**
 * An enumeration of SBML types to help identify SBML objects at runtime.
 * Abstract types do not have a typecode since they cannot be instantiated.
 */
typedef enum
{
    SBML_UNKNOWN

  , SBML_COMPARTMENT
  , SBML_DOCUMENT
  , SBML_EVENT
  , SBML_EVENT_ASSIGNMENT
  , SBML_FUNCTION_DEFINITION
  , SBML_KINETIC_LAW
  , SBML_LIST_OF
  , SBML_MODEL
  , SBML_PARAMETER
  , SBML_REACTION
  , SBML_RULE
  , SBML_SPECIES
  , SBML_SPECIES_REFERENCE
  , SBML_MODIFIER_SPECIES_REFERENCE
  , SBML_UNIT_DEFINITION
  , SBML_UNIT
  , SBML_ALGEBRAIC_RULE
  , SBML_ASSIGNMENT_RULE
  , SBML_RATE_RULE
  , SBML_SPECIES_CONCENTRATION_RULE
  , SBML_COMPARTMENT_VOLUME_RULE
  , SBML_PARAMETER_RULE

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

} SBMLTypeCode_t;


/**
 * @return a human readable name for the given SBMLTypeCode_t.  The caller
 * does not own the returned string and is therefore not allowed to modify
 * it.
 */
LIBSBML_EXTERN
const char *
SBMLTypeCode_toString (SBMLTypeCode_t tc);


END_C_DECLS


#endif  /* SBMLTypeCodes_h */
