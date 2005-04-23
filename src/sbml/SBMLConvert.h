/**
 * \file    SBMLConvert.h
 * \brief   Top-level container for all things SBML
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2003 California Institute of Technology and
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


#ifndef SBMLConvert_h
#define SBMLConvert_h


#include "SBase.h"
#include "Model.h"
#include "Reaction.h"


BEGIN_C_DECLS


/**
 * Converts the given SBase object and any of its subordinate objects from
 * SBML L1 to L2.  In some cases, the larger Model is needed as context for
 * conversion.  This function delegates, based on SBMLTypeCode, to
 * SBML_convertNameToId() and others.
 */
void
LIBSBML_EXTERN
SBML_convertToL2 (Model_t *m, SBase_t *sb);

/**
 * Moves the name field of the given SBase object to its Id field if and
 * only if the name field is not set.  SBase may be any L1 object that has
 * a name: Model, UnitDefinition, Species, Parameter or Reaction.
 */
void
LIBSBML_EXTERN
SBML_convertNameToId (SBase_t *sb);

/**
 * Converts the list of Reactions in this Model from SBML L1 to L2.
 *
 * Conversion involves:
 *
 *   - Converting Reaction name to Reaction id (via SBML_convertNameToId())
 *
 *   - Converting the subordinate KineticLaw (and its Parameters) to L2
 *     (via SBML_convertToL2()), and
 *
 *   - Adding modifiers (ModifierSpeciesReference) to this Reaction as
 *     appropriate (via SBML_addModifiersToReaction()).
 */
void
SBML_convertReactionsInModelToL2 (Model_t *m);

/**
 * Adds modifiers (ModifierSpeciesReferences) to the given Reaction.
 *
 * A Model is needed for context to determine the set of allowable Species
 * (see criterion 1 below).
 *
 * For each symbol in the Reaction's KineticLaw, that symbol is a modifier
 * iff:
 *
 *   1. It is defined as a Species in the Model
 *   2. It is not a Reactant or Product in this Reaction.
 */
LIBSBML_EXTERN
void
SBML_addModifiersToReaction (Reaction_t *r, const Model_t *m);

/**
 * Ensures that the contant attribute is set to false any rules that refer
 * to Compartments, Species, or Parameters
 */
LIBSBML_EXTERN
void
SBML_convertRuleToL2 (Model_t *m, Rule_t *r);
/**
 * Converts the given Model from SBML L2 to L1. 
 * Deals with the species concentrations and amounts seperately
 */
void
LIBSBML_EXTERN
SBML_convertModelToL1 (Model_t *m, SBase_t *sb);

/**
 * Converts the given SBase object and any of its subordinate objects from
 * SBML L2 to L1.  In some cases, the larger Model is needed as context for
 * conversion.  This function delegates, based on SBMLTypeCode, to
 * SBML_convertIdToName() and others.
 */
void
LIBSBML_EXTERN
SBML_convertToL1 (Model_t *m, SBase_t *sb);

/**
 * Moves the id field of the given SBase object to its name field 
 * losing any name fields that are already set. SBase may be any L2 object that has
 * an id: Model, UnitDefinition, Species, Parameter, Reaction or Compartment.
 */
void
LIBSBML_EXTERN
SBML_convertIdToName (SBase_t *sb);

/**
 * Level 2 allows a model to be specified without a Compartment.
 * However this is not valid in Level 1. Thus if a L2 model has no
 * Compartment one must be included.
 */
void
LIBSBML_EXTERN
SBML_includeCompartment (Model_t *m);

/**
 * Converts all the species in a model from 
 * SBML L2 to L1.  This is necessary before any other conversion 
 * happens because of the potential conflicts of concentration 
 * versus amount.
 */
void
LIBSBML_EXTERN
SBML_convertAllSpeciesToL1 (Model_t *m);

/**
 * Ensures that the species has a compartment field (required in level 1)
 * the initialAmount is set 
 */
LIBSBML_EXTERN
void
SBML_convertSpeciesToL1 (Model_t *m, Species_t *s);

/**
 * Converts the list of Reactions in this Model from SBML L2 to L1.
 *
 * Conversion involves:
 *
 *   - Converting Reaction id to Reaction name (via SBML_convertIdToName())
 *
 *   - Converting the subordinate KineticLaw (and its Parameters) to L1
 *     (via SBML_convertToL1()), and
 *
 */
void
SBML_convertReactionsInModelToL1 (Model_t *m);

/**
 * Converts the stoichiometry attribute of a SpeciesReference
 * from SBML L2 to L1. Since in L2 stoichiometry can be of type 
 * double but in L1 the attributes are stoichiometry and denominator
 * both of type integer
 *
 */
void
SBML_convertStoichiometryToL1 (SpeciesReference_t *sr);

END_C_DECLS


#endif  /** SBMLConvert_h **/
