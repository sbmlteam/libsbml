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


  switch (sb->getTypeCode())
  {
    case SBML_COMPARTMENT:
      return SWIGTYPE_p_Compartment;

    case SBML_DOCUMENT:
      return SWIGTYPE_p_SBMLDocument;

    case SBML_EVENT:
      return SWIGTYPE_p_Event;

    case SBML_EVENT_ASSIGNMENT:
      return SWIGTYPE_p_EventAssignment;

    case SBML_FUNCTION_DEFINITION:
      return SWIGTYPE_p_FunctionDefinition;

    case SBML_KINETIC_LAW:
      return SWIGTYPE_p_KineticLaw;

    case SBML_LIST_OF:
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

    case SBML_SPECIES_CONCENTRATION_RULE:
      return SWIGTYPE_p_SpeciesConcentrationRule;

    case SBML_COMPARTMENT_VOLUME_RULE:
      return SWIGTYPE_p_CompartmentVolumeRule;

    case SBML_PARAMETER_RULE:
      return SWIGTYPE_p_ParameterRule;

    default:
      return SWIGTYPE_p_SBase;
  }
}
