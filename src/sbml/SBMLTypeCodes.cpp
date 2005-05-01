/**
 * \file    SBMLTypeCodes.cpp
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


#include "common/common.h"
#include "SBMLTypeCodes.h"


static
const char* SBML_TYPE_CODE_STRINGS[] =
{
    "(Unknown SBML Type)"
  , "Compartment"
  , "Document"
  , "Event"
  , "EventAssignment"
  , "FunctionDefinition"
  , "KineticLaw"
  , "ListOf"
  , "Model"
  , "Parameter"
  , "Reaction"
  , "Species"
  , "SpeciesReference"
  , "ModifierSpeciesReference"
  , "UnitDefinition"
  , "Unit"
  , "AlgebraicRule"
  , "AssignmentRule"
  , "RateRule"
  , "SpeciesConcentrationRule"
  , "CompartmentVolumeRule"
  , "ParameterRule"

#ifdef USE_LAYOUT
  , "BoundingBox"
  , "CompartmentGlyph"
  , "CubicBezier"
  , "Curve"
  , "Dimensions"
  , "GraphicalObject"
  , "Layout"
  , "LineSegment"
  , "Point"
  , "ReactionGlyph"
  , "SpeciesGlyph"
  , "SpeciesReferenceGlyph"
  , "TextGlyph"
#endif  /* USE_LAYOUT */

};


/**
 * @return a human readable name for the given SBMLTypeCode_t.  The caller
 * does not own the returned string and is therefore not allowed to modify
 * it.
 */
const char *
SBMLTypeCode_toString (SBMLTypeCode_t tc)
{
  SBMLTypeCode_t max = SBML_PARAMETER_RULE;

#ifdef USE_LAYOUT
  max = SBML_LAYOUT_TEXTGLYPH;
#endif  // USE_LAYOUT


  if (tc < SBML_COMPARTMENT || tc > max)
  {
    tc = SBML_UNKNOWN;
  }

  return SBML_TYPE_CODE_STRINGS[tc];
}
