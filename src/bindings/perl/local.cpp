/**
 * \file    local.cpp
 * \brief   Perl-specific SWIG support code for wrapping libSBML API
 * \author  Ben Bornstein and Ben Kovitz
 *
 * $Id$
 * $Source$
 */
/* Copyright 2004 California Institute of Technology and
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
 *     Ben Bornstein and Ben Kovitz
 *
 *     The SBML Team
 *     Control and Dynamical Systems, MC 107-81
 *     California Institute of Technology
 *     Pasadena, CA, 91125, USA
 *
 *     http://sbml.org
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s):
 */

#include "sbml/SBase.h"


/**
 * @return the most specific Swig type for the given SBase object.
 */
struct swig_type_info*
GetDowncastSwigType (SBase* sb)
{
  if (sb == NULL) return SWIGTYPE_p_SBase;


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

    case SBML_FORMULA_UNITS_DATA:
      return SWIGTYPE_p_FormulaUnitsData;

    case SBML_STOICHIOMETRY_MATH:
     return SWIGTYPE_p_StoichiometryMath;
      
    default:
      return SWIGTYPE_p_SBase;
  }
}
