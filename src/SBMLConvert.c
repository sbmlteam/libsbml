/**
 * \file    SBMLConvert.c
 * \brief   Converts SBML L1 objects to SBML L2 objects.
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


#include "common.h"

#include "List.h"
#include "SBMLTypes.h"
#include "SBMLTypeCodes.h"

#include "SBMLConvert.h"


/**
 * Converts the given SBase object and any of its subordinate objects from
 * SBML L1 to L2.  In some cases, the larger Model is needed as context for
 * conversion.  This function delegates, based on SBMLTypeCode, to
 * SBML_convertNameToId() and others.
 */
void
LIBSBML_EXTERN
SBML_convertToL2 (Model_t *m, SBase_t *sb)
{
  SBMLDocument_t *d;
  KineticLaw_t   *kl;
  ListOf_t       *lo;

  unsigned int n;
  unsigned int size;


  if (sb == NULL) return;

  switch (SBase_getTypeCode(sb))
  {
    case SBML_DOCUMENT:
      d = (SBMLDocument_t *) sb;
      SBMLDocument_setLevel(d, 2);
      SBML_convertToL2( m, (SBase_t *) SBMLDocument_getModel(d) );
      break;

    case SBML_LIST_OF:
      lo   = (ListOf_t *) sb;
      size = ListOf_getNumItems(lo);

      for (n = 0; n < size; n++)
      {
        SBML_convertToL2( m, (SBase_t *) ListOf_get(lo, n) );
      }
      break;

    case SBML_MODEL:
      /* m = (Model_t *) sb; */
      SBML_convertNameToId(sb);
      SBML_convertToL2( m, (SBase_t *) Model_getListOfUnitDefinitions(m) );
      SBML_convertToL2( m, (SBase_t *) Model_getListOfCompartments   (m) );
      SBML_convertToL2( m, (SBase_t *) Model_getListOfSpecies        (m) );
      SBML_convertToL2( m, (SBase_t *) Model_getListOfParameters     (m) );
      SBML_convertToL2( m, (SBase_t *) Model_getListOfRules          (m) );
      SBML_convertReactionsInModelToL2(m);
      break;

    case SBML_UNIT_DEFINITION:
    case SBML_COMPARTMENT:
    case SBML_SPECIES:
    case SBML_PARAMETER:
      SBML_convertNameToId(sb);
      break;

    case SBML_SPECIES_CONCENTRATION_RULE:
    case SBML_COMPARTMENT_VOLUME_RULE:
    case SBML_PARAMETER_RULE:
      SBML_convertRuleToL2(m, (Rule_t *) sb);
      break;

    case SBML_KINETIC_LAW:
      kl = (KineticLaw_t *) sb;
      SBML_convertToL2( m, (SBase_t *) KineticLaw_getListOfParameters(kl) );
      break;

    default:
      break;
  }
}


/**
 * Moves the name field of the given SBase object to its Id field if and
 * only if the name field is not set.  SBase may be any L1 object that has
 * a name: Model, UnitDefinition, Species, Parameter or Reaction.
 */
void
LIBSBML_EXTERN
SBML_convertNameToId (SBase_t *sb)
{
  if (sb == NULL) return;

  switch (SBase_getTypeCode(sb))
  {
    case SBML_MODEL:
      Model_moveNameToId((Model_t *) sb);
      break;

    case SBML_UNIT_DEFINITION:
      UnitDefinition_moveNameToId((UnitDefinition_t *) sb);
      break;

    case SBML_COMPARTMENT:
      Compartment_moveNameToId((Compartment_t *) sb);
      break;

    case SBML_SPECIES:
      Species_moveNameToId((Species_t *) sb);
      break;

    case SBML_PARAMETER:
      Parameter_moveNameToId((Parameter_t *) sb);
      break;

    case SBML_REACTION:
      Reaction_moveNameToId((Reaction_t *) sb);
      break;

    default:
      break;
  }
}


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
SBML_convertReactionsInModelToL2 (Model_t *m)
{
  unsigned int  numReactions = Model_getNumReactions(m);
  ListOf_t     *reactions    = Model_getListOfReactions(m);

  unsigned int  n;
  Reaction_t   *r;


  for (n = 0; n < numReactions; n++)
  {
    r = (Reaction_t *) ListOf_get(reactions, n);

    SBML_convertNameToId( (SBase_t *) r );
    SBML_convertToL2    ( m, (SBase_t *) Reaction_getKineticLaw(r) );

    SBML_addModifiersToReaction(r, m);
  }
}


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
SBML_addModifiersToReaction (Reaction_t *r, const Model_t *m)
{
  const char *id;

  unsigned int size;
  unsigned int n;

  const ASTNode_t *node;
  List_t          *names;

  KineticLaw_t *kl = Reaction_getKineticLaw(r);


  /**
   * If the Reaction does not have a KineticLaw or the KineticLaw does not
   * have a formula, there is nothing to be done.
   */
  if ( kl == NULL ) return;
  if ( !KineticLaw_isSetMath(kl) && !KineticLaw_isSetFormula(kl) ) return;

  /**
   * Ensure the KineticLaw has an AST math expression by deriving it from
   * the infix formula string if nescessary.
   */
  if ( !KineticLaw_isSetMath(kl) )
  {
    KineticLaw_setMathFromFormula(kl);
  }

  /**
   * Get a list of AST_NAMEs (symbols) used in the KineticLaw.
   */
  node  = KineticLaw_getMath(kl);
  names = ASTNode_getListOfNodes( (ASTNode_t*) node,
                                  (ASTNodePredicate) ASTNode_isName) ;
  size  = List_size(names);

  /**
   * NOTE: The C 'continue' keyword immediately aborts the current loop
   * iteration and continues with the next one (n++).  It is used below as
   * an alternative to several levels of nested if statements.
   *
   * For each symbol, add it as a Reaction modifier iff:
   */
  for (n = 0; n < size; n++)
  {
    node = (ASTNode_t *) List_get(names, n);
    id   = ASTNode_getName(node);

    /** 1. It is an AST_NAME (not AST_NAME_TIME), and **/
    if (ASTNode_getType(node) != AST_NAME) continue;

    /** 2. It refers to a Species in this Model, and **/
    if (id == NULL || Model_getSpeciesById(m, id) == NULL) continue;

    /** 3. It is not a Reactant, Product, or (already) a Modifier **/
    if (Reaction_getReactantById(r, id) != NULL) continue;
    if (Reaction_getProductById (r, id) != NULL) continue;
    if (Reaction_getModifierById(r, id) != NULL) continue;

    Reaction_addModifier(r, ModifierSpeciesReference_createWith(id));
  }

  List_free(names);
}


/**
 * Ensures that the contant attribute is set to false any rules that refer
 * to Compartments, Species, or Parameters
 */
LIBSBML_EXTERN
void
SBML_convertRuleToL2 (Model_t *m, Rule_t *r)
{
  Compartment_t  *c;
  Species_t      *s;
  Parameter_t    *p;

  const char *id;


  switch ( SBase_getTypeCode((SBase_t*) r) )
  {
    case SBML_SPECIES_CONCENTRATION_RULE:
      id = SpeciesConcentrationRule_getSpecies((SpeciesConcentrationRule_t*) r);
      s  = Model_getSpeciesById(m, id);
      if (s != NULL) Species_setConstant(s, 0);
      break;

    case SBML_COMPARTMENT_VOLUME_RULE:
      id = CompartmentVolumeRule_getCompartment((CompartmentVolumeRule_t*) r);
      c  = Model_getCompartmentById(m, id);
      if (c != NULL) Compartment_setConstant(c, 0);
      break;

    case SBML_PARAMETER_RULE:
      id = ParameterRule_getName((ParameterRule_t*) r);
      p  = Model_getParameterById(m, id);
      if (p != NULL) Parameter_setConstant(p, 0);
      break;

    default:
      break;
  }
}
