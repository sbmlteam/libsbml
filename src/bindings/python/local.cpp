/**
 * @file    local.cpp
 * @brief   Python-specific SWIG support code for wrapping libSBML API
 * @author  Ben Bornstein
 * @author  Ben Kovitz
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

#include "sbml/SBase.h"


/**
 * @return the most specific Swig type for the given SBase object.
 */
struct swig_type_info*
GetDowncastSwigType (SBase* sb)
{
  if (sb == 0) return SWIGTYPE_p_SBase;

  std::string name;
  switch (sb->getTypeCode())
  {
    case SBML_COMPARTMENT:
      return SWIGTYPE_p_Compartment;

    case SBML_COMPARTMENT_TYPE:
      return SWIGTYPE_p_CompartmentType;

    case SBML_CONSTRAINT:
      return SWIGTYPE_p_Constraint;

    case SBML_DOCUMENT:
      return SWIGTYPE_p_SBMLDocument;

    case SBML_EVENT:
      return SWIGTYPE_p_Event;

    case SBML_EVENT_ASSIGNMENT:
      return SWIGTYPE_p_EventAssignment;

    case SBML_FUNCTION_DEFINITION:
      return SWIGTYPE_p_FunctionDefinition;

    case SBML_INITIAL_ASSIGNMENT:
      return SWIGTYPE_p_InitialAssignment;

    case SBML_KINETIC_LAW:
      return SWIGTYPE_p_KineticLaw;

    case SBML_LIST_OF:
      name = sb->getElementName();
      if(name == "listOf"){
        return SWIGTYPE_p_ListOf;
      }
      else if(name == "listOfCompartments"){
        return SWIGTYPE_p_ListOfCompartments;
      }
      else if(name == "listOfCompartmentTypes"){
        return SWIGTYPE_p_ListOfCompartmentTypes;
      }
      else if(name == "listOfConstraints"){
        return SWIGTYPE_p_ListOfConstraints;
      }
      else if(name == "listOfEvents"){
        return SWIGTYPE_p_ListOfEvents;
      }
      else if(name == "listOfEventAssignments"){
        return SWIGTYPE_p_ListOfEventAssignments;
      }
      else if(name == "listOfFunctionDefinitions"){
        return SWIGTYPE_p_ListOfFunctionDefinitions;
      }
      else if(name == "listOfInitialAssignments"){
        return SWIGTYPE_p_ListOfInitialAssignments;
      }
      else if(name == "listOfParameters"){
        return SWIGTYPE_p_ListOfParameters;
      }
      else if(name == "listOfReactions"){
        return SWIGTYPE_p_ListOfReactions;
      }
      else if(name == "listOfRules"){
        return SWIGTYPE_p_ListOfRules;
      }
      else if(name == "listOfSpecies"){
        return SWIGTYPE_p_ListOfSpecies;
      }
      else if(name == "listOfUnknowns"){
        return SWIGTYPE_p_ListOfSpeciesReferences;
      }
      else if(name == "listOfReactants"){
        return SWIGTYPE_p_ListOfSpeciesReferences;
      }
      else if(name == "listOfProducts"){
        return SWIGTYPE_p_ListOfSpeciesReferences;
      }
      else if(name == "listOfModifiers"){
        return SWIGTYPE_p_ListOfSpeciesReferences;
      }
      else if(name == "listOfSpeciesTypes"){
        return SWIGTYPE_p_ListOfSpeciesTypes;
      }
      else if(name == "listOfUnits"){
        return SWIGTYPE_p_ListOfUnits;
      }
      else if(name == "listOfUnitDefinitions"){
        return SWIGTYPE_p_ListOfUnitDefinitions;
      }
#ifdef USE_LAYOUT
      else if(name == "listOfCompartmentGlyphs"){
        return SWIGTYPE_p_ListOfCompartmentGlyphs;
      }
      else if(name == "listOfAdditionalGraphicalObjects"){
        return SWIGTYPE_p_ListOfGraphicalObjects;
      }
      else if(name == "listOfLayouts"){
        return SWIGTYPE_p_ListOfLayouts;
      }
      else if(name == "listOfCurveSegments"){
        return SWIGTYPE_p_ListOfLineSegments;
      }
      else if(name == "listOfSpeciesGlyphs"){
        return SWIGTYPE_p_ListOfSpeciesGlyphs;
      }
      else if(name == "listOfSpeciesReferenceGlyphs"){
        return SWIGTYPE_p_ListOfSpeciesReferenceGlyphs;
      }
      else if(name == "listOfReactionGlyphs"){
        return SWIGTYPE_p_ListOfReactionGlyphs;
      }
      else if(name == "listOfTextGlyphs"){
        return SWIGTYPE_p_ListOfTextGlyphs;
      }
#endif // USE_LAYOUT
      return SWIGTYPE_p_ListOf;

    case SBML_MODEL:
      return SWIGTYPE_p_Model;

    case SBML_PARAMETER:
      return SWIGTYPE_p_Parameter;

    case SBML_REACTION:
      return SWIGTYPE_p_Reaction;

    case SBML_SPECIES:
      return SWIGTYPE_p_Species;

    case SBML_SPECIES_REFERENCE:
      return SWIGTYPE_p_SpeciesReference;

    case SBML_MODIFIER_SPECIES_REFERENCE:
      return SWIGTYPE_p_ModifierSpeciesReference;

    case SBML_SPECIES_TYPE:
      return SWIGTYPE_p_SpeciesType;

    case SBML_UNIT_DEFINITION:
      return SWIGTYPE_p_UnitDefinition;

    case SBML_UNIT:
      return SWIGTYPE_p_Unit;

    case SBML_ALGEBRAIC_RULE:
      return SWIGTYPE_p_AlgebraicRule;

    case SBML_ASSIGNMENT_RULE:
      return SWIGTYPE_p_AssignmentRule;

    case SBML_RATE_RULE:
      return SWIGTYPE_p_RateRule;

    case SBML_DELAY:
      return SWIGTYPE_p_Delay;

    case SBML_TRIGGER:
      return SWIGTYPE_p_Trigger;

    case SBML_STOICHIOMETRY_MATH:
     return SWIGTYPE_p_StoichiometryMath;
      
#ifdef USE_LAYOUT
    case SBML_LAYOUT_BOUNDINGBOX:
        return SWIGTYPE_p_BoundingBox;
        
    case SBML_LAYOUT_COMPARTMENTGLYPH:
        return SWIGTYPE_p_CompartmentGlyph;
        
    case SBML_LAYOUT_CUBICBEZIER:
        return SWIGTYPE_p_CubicBezier;
        
    case SBML_LAYOUT_CURVE:
        return SWIGTYPE_p_Curve;
        
    case SBML_LAYOUT_DIMENSIONS:
        return SWIGTYPE_p_Dimensions;
        
    case SBML_LAYOUT_GRAPHICALOBJECT:
        return SWIGTYPE_p_GraphicalObject;
        
    case SBML_LAYOUT_LAYOUT:
        return SWIGTYPE_p_Layout;
        
    case SBML_LAYOUT_LINESEGMENT:
        return SWIGTYPE_p_LineSegment;
        
    case SBML_LAYOUT_POINT:
        return SWIGTYPE_p_Point;
        
    case SBML_LAYOUT_REACTIONGLYPH:
        return SWIGTYPE_p_ReactionGlyph;
        
    case SBML_LAYOUT_SPECIESGLYPH:
        return SWIGTYPE_p_SpeciesGlyph;
        
    case SBML_LAYOUT_SPECIESREFERENCEGLYPH:
        return SWIGTYPE_p_SpeciesReferenceGlyph;
        
    case SBML_LAYOUT_TEXTGLYPH:
        return SWIGTYPE_p_TextGlyph;
        
#endif // USE_LAYOUT

    default:
      return SWIGTYPE_p_SBase;
  }
}
