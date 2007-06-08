/**
 * @file    SBMLTypeCodes.cpp
 * @brief   Enumeration to identify SBML objects at runtime
 * @author  Ben Bornstein
 *
 * $Id$
 * $Source$
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2007 California Institute of Technology.
 * Copyright 2002-2005 California Institute of Technology and
 *                     Japan Science and Technology Corporation.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *----------------------------------------------------------------------- -->*/

#include <sbml/common/common.h>
#include <sbml/SBMLTypeCodes.h>


static
const char* SBML_TYPE_CODE_STRINGS[] =
{
    "(Unknown SBML Type)"
  , "Compartment"
  , "CompartmentType"
  , "Constraint"
  , "Document"
  , "Event"
  , "EventAssignment"
  , "FunctionDefinition"
  , "InitialAssignment"
  , "KineticLaw"
  , "ListOf"
  , "Model"
  , "Parameter"
  , "Reaction"
  , "Rule"
  , "Species"
  , "SpeciesReference"
  , "SpeciesType"
  , "ModifierSpeciesReference"
  , "UnitDefinition"
  , "Unit"
  , "AlgebraicRule"
  , "AssignmentRule"
  , "RateRule"
  , "SpeciesConcentrationRule"
  , "CompartmentVolumeRule"
  , "ParameterRule"
  , "Trigger"
  , "Delay"
  , "StoichiometryMath"

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
LIBSBML_EXTERN
const char *
SBMLTypeCode_toString (SBMLTypeCode_t tc)
{
  SBMLTypeCode_t max = SBML_STOICHIOMETRY_MATH;

#ifdef USE_LAYOUT
  max = SBML_LAYOUT_TEXTGLYPH;
#endif  // USE_LAYOUT


  if (tc < SBML_COMPARTMENT || tc > max)
  {
    tc = SBML_UNKNOWN;
  }

  return SBML_TYPE_CODE_STRINGS[tc];
}
