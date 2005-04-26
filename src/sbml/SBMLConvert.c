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


#include "common/common.h"
#include "util/List.h"

#include "SBMLTypes.h"
#include "SBMLTypeCodes.h"
#include "SBMLConvert.h"


/* #include <tchar.h> */

#define ASSIGNEDCOMPARTMENT "AssignedName"


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


/**
 * Converts the given Model from SBML L2 to L1. 
 * Deals with the species concentrations and amounts seperately
 */
void
LIBSBML_EXTERN
SBML_convertModelToL1 (Model_t *m, SBase_t *sb)

{
  SBML_convertAllRulesToL1(m);
  SBML_convertAllSpeciesToL1(m);
  SBML_convertToL1(m, sb);
  SBML_applyFunctionDefinitions(m);
}


/**
 * Converts the given SBase object and any of its subordinate objects from
 * SBML L2 to L1.  In some cases, the larger Model is needed as context for
 * conversion.  This function delegates, based on SBMLTypeCode, to
 * SBML_convertIdToName() and others.
 */
void
LIBSBML_EXTERN
SBML_convertToL1 (Model_t *m, SBase_t *sb)
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
      SBMLDocument_setLevel(d, 1);
      SBML_convertToL1( m, (SBase_t *) SBMLDocument_getModel(d) );
      break;

    case SBML_LIST_OF:
      lo   = (ListOf_t *) sb;
      size = ListOf_getNumItems(lo);

      for (n = 0; n < size; n++)
      {
        SBML_convertToL1( m, (SBase_t *) ListOf_get(lo, n) );
      }
      break;

    case SBML_MODEL:
      /* m = (Model_t *) sb; */
      SBML_convertIdToName(sb);
      SBML_convertToL1( m, (SBase_t *) Model_getListOfUnitDefinitions(m) );
      SBML_convertToL1( m, (SBase_t *) Model_getListOfCompartments   (m) );
      SBML_convertToL1( m, (SBase_t *) Model_getListOfSpecies        (m) );
      SBML_convertToL1( m, (SBase_t *) Model_getListOfParameters     (m) );
      SBML_convertToL1( m, (SBase_t *) Model_getListOfRules          (m) );
      
      if (Model_getNumCompartments(m) == 0)
        SBML_includeCompartment(m);

      SBML_convertReactionsInModelToL1(m);
      break;

    case SBML_UNIT_DEFINITION:
    case SBML_COMPARTMENT:
    case SBML_PARAMETER:
    case SBML_SPECIES:
      SBML_convertIdToName(sb);
      break;

    case SBML_KINETIC_LAW:
      kl = (KineticLaw_t *) sb;
      SBML_convertToL1( m, (SBase_t *) KineticLaw_getListOfParameters(kl) );
      break;

    default:
      break;
  }

}


/**
 * Moves the id field of the given SBase object to its name field losing
 * any name fields that are already set. SBase may be any L2 object that
 * has an id: Model, UnitDefinition, Species, Parameter, Reaction or
 * Compartment.
 */
void
LIBSBML_EXTERN
SBML_convertIdToName (SBase_t *sb)
{
  if (sb == NULL) return;

  switch (SBase_getTypeCode(sb))
  {
    case SBML_MODEL:
      Model_unsetName((Model_t *) sb);
      Model_moveIdToName((Model_t *) sb);
      break;

    case SBML_UNIT_DEFINITION:
      UnitDefinition_unsetName((UnitDefinition_t *) sb);
      UnitDefinition_moveIdToName((UnitDefinition_t *) sb);
      break;

    case SBML_COMPARTMENT:
      Compartment_unsetName((Compartment_t *) sb);
      Compartment_moveIdToName((Compartment_t *) sb);
      break;

    case SBML_SPECIES:
      Species_unsetName((Species_t *) sb);
      Species_moveIdToName((Species_t *) sb);
      break;

    case SBML_PARAMETER:
      Parameter_unsetName((Parameter_t *) sb);
      Parameter_moveIdToName((Parameter_t *) sb);
      break;

    case SBML_REACTION:
      Reaction_unsetName((Reaction_t *) sb);
      Reaction_moveIdToName((Reaction_t *) sb);
      break;

    default:
      break;
  }
}


/**
 * Level 2 allows a model to be specified without a Compartment.  However
 * this is not valid in Level 1. Thus if a L2 model has no Compartment one
 * must be included.
 */
void
LIBSBML_EXTERN
SBML_includeCompartment (Model_t *m)
{
  Compartment_t * c = Compartment_create();
  Compartment_setName(c, ASSIGNEDCOMPARTMENT);
  Model_addCompartment(m, c);
}


/**
 * Ensures that the species has an initialAmount set 
 */
LIBSBML_EXTERN
void
SBML_convertSpeciesToL1 (Model_t *m, Species_t *s)
{
  double amount;
  Compartment_t *c;

  /* need to deal with possible mismatch of units */

  if (Species_isSetInitialConcentration(s) == 1)
  {
    c = Model_getCompartmentById(m, Species_getCompartment(s));
    amount = (Species_getInitialConcentration(s))*(Compartment_getSize(c));
  }
  else if (Species_isSetInitialConcentration(s) == 0)
  {
    amount = 0.0;
  }
  Species_setInitialAmount(s, amount);

}


/**
 * Converts all the species in a model from 
 * SBML L2 to L1.  This is necessary before any other conversion 
 * happens because of the potential conflicts of concentration 
 * versus amount.
 */
void
LIBSBML_EXTERN
SBML_convertAllSpeciesToL1 (Model_t *m)
{
  unsigned int n;
  unsigned int size;

  size = Model_getNumSpecies(m);

  if (size == 0) return;

  for (n = 0; n < size; n++)
  {
    SBML_convertSpeciesToL1(m, Model_getSpecies(m, n));
  }
}


/**
 * Converts the list of Reactions in this Model from SBML L2 to L1.
 *
 * Conversion involves:
 *
 *   - Converting Reaction id to Reaction name (via SBML_convertIdToName())
 *
 *   - Converting the subordinate KineticLaw (and its Parameters) to L1
 *     (via SBML_convertToL1()), and
 */
void
SBML_convertReactionsInModelToL1 (Model_t *m)
{
  unsigned int  numReactions = Model_getNumReactions(m);
  ListOf_t     *reactions    = Model_getListOfReactions(m);

  unsigned int  n;
  Reaction_t   *r;
/*
  unsigned int numReactants, numProducts;
  ListOf_t *reactants;
  ListOf_t *products;
  SpeciesReference_t *sr;
*/

  for (n = 0; n < numReactions; n++)
  {
    r = (Reaction_t *) ListOf_get(reactions, n);

    SBML_convertIdToName( (SBase_t *) r );
    
    SBML_convertToL1    ( m, (SBase_t *) Reaction_getKineticLaw(r) );
  
    /*
     * code left in as will be necessary when allow
     * the option of converting a rational stoichiometry 
     * to an integal stoichiometry and denominator
     *
    reactants = Reaction_getListOfReactants(r);
    products  = Reaction_getListOfProducts(r);

    numReactants = Reaction_getNumReactants(r);
    numProducts  = Reaction_getNumProducts( r);

    for (noSR = 0; noSR < numReactants; noSR++)
    {
      sr = (SpeciesReference_t *) ListOf_get(reactants, noSR);
      SBML_convertStoichiometryToL1(sr);
    }

    for (noSR = 0; noSR < numProducts; noSR++)
    {
      sr = (SpeciesReference_t *) ListOf_get(products, noSR);
      SBML_convertStoichiometryToL1(sr);
    }

    */


  }
}


/**
 * Converts the stoichiometry attribute of a SpeciesReference from SBML L2
 * to L1. Since in L2 stoichiometry can be of type double but in L1 the
 * attributes are stoichiometry and denominator both of type integer
 */
void
SBML_convertStoichiometryToL1 (SpeciesReference_t *sr)
{
  double stoichiometry;

  stoichiometry = SpeciesReference_getStoichiometry(sr);  
}


/**
 * Converts all the rules in a model from 
 * SBML L2 to L1.  This is necessary before any other conversion 
 * happens because of the need to track ids
 */
void
LIBSBML_EXTERN
SBML_convertAllRulesToL1 (Model_t *m)
{
  unsigned int  numRules = Model_getNumRules(m);
  ListOf_t     *rules    = Model_getListOfRules(m);
  unsigned int n;
  Compartment_t  *c;
  Species_t      *s;
  Parameter_t    *p;
  SpeciesConcentrationRule_t * scr;
  CompartmentVolumeRule_t * cvr;
  ParameterRule_t * pr;
  const char *id;


  Rule_t * r;

  if (numRules == 0) return;

  for (n = 0; n < numRules; n++)
  {
    r = (Rule_t *) ListOf_get(rules, n);

    switch ( SBase_getTypeCode((SBase_t*) r) )
      {
        case SBML_ASSIGNMENT_RULE:
          id = AssignmentRule_getVariable((AssignmentRule_t*) r);
      
          if ((s  = Model_getSpeciesById(m, id)) != NULL)
          {
            scr = Model_createSpeciesConcentrationRule(m);
            SpeciesConcentrationRule_setSpecies (scr, id);
            Rule_setFormula((Rule_t*) scr, Rule_getFormula((Rule_t*) r));
            ListOf_remove(rules, n);
          }
          else if ((c = Model_getCompartmentById(m, id)) != NULL)
          {
            cvr = Model_createCompartmentVolumeRule(m);
            CompartmentVolumeRule_setCompartment (cvr, id);
            Rule_setFormula((Rule_t*) cvr, Rule_getFormula((Rule_t*) r));
            ListOf_remove(rules, n);
          }
          else if ((p = Model_getParameterById(m, id)) != NULL)
          {
            pr = Model_createParameterRule(m);
            ParameterRule_setName (pr, id);
            Rule_setFormula((Rule_t*) pr, Rule_getFormula((Rule_t*) r));
            ListOf_remove(rules, n);
          }
          break;

        case SBML_RATE_RULE:
          id = RateRule_getVariable((RateRule_t*) r);
      
          if ((s  = Model_getSpeciesById(m, id)) != NULL)
          {
            scr = Model_createSpeciesConcentrationRule(m);
            SpeciesConcentrationRule_setSpecies (scr, id);
            Rule_setFormula((Rule_t*) scr, Rule_getFormula((Rule_t*) r));
            AssignmentRule_setType((AssignmentRule_t*) scr, RULE_TYPE_RATE);
            ListOf_remove(rules, n);
          }
          else if ((c = Model_getCompartmentById(m, id)) != NULL)
          {
            cvr = Model_createCompartmentVolumeRule(m);
            CompartmentVolumeRule_setCompartment (cvr, id);
            Rule_setFormula((Rule_t*) cvr, Rule_getFormula((Rule_t*) r));
            AssignmentRule_setType((AssignmentRule_t*) cvr, RULE_TYPE_RATE);
            ListOf_remove(rules, n);
          }
          else if ((p = Model_getParameterById(m, id)) != NULL)
          {
            pr = Model_createParameterRule(m);
            ParameterRule_setName (pr, id);
            Rule_setFormula((Rule_t*) pr, Rule_getFormula((Rule_t*) r));
            AssignmentRule_setType((AssignmentRule_t*) pr, RULE_TYPE_RATE);
            ListOf_remove(rules, n);
          }
            break;

        case SBML_ALGEBRAIC_RULE:
          break;

        default:
          break;
      }
  }
}
/**
 * Applies the function definitions in a L2 model 
 * directly to any formula strings
 */
void
LIBSBML_EXTERN
SBML_applyFunctionDefinitions (Model_t *m)
{
  /*
  unsigned int  numFunctions = Model_getNumFunctionDefinitions(m);
  ListOf_t     *functions    = Model_getListOfFunctionDefinitions(m);
  unsigned int  numRules = Model_getNumRules(m);
  ListOf_t     *rules    = Model_getListOfRules(m);
  unsigned int n, nr, i;

  FunctionDefinition_t *fd;
  const char *id;
  char * idWithBracket, * argument = NULL;

  char* match;
  char* closeBracket, *a;
  Rule_t *r;

  char * formula, * function, *functionArg;

  if (numFunctions == 0) return;

  for (n = 0; n < numFunctions; n++)
  {
    fd = Model_getFunctionDefinition(m, n);
    id = FunctionDefinition_getId(fd);

    idWithBracket = strcat(id, "(");
    for (nr = 0; nr < numRules; nr++)
    {
      r = Model_getRule(m, nr);

      formula = Rule_getFormula(r);
      if ((match = strstr(formula, idWithBracket)) != NULL)
      {
        closeBracket = strstr(formula, ")");
        //  cant do this a it messes up the actual formula
        _tcsnset(closeBracket, NULL, strlen(closeBracket));
        argument = _tcsninc(match, strlen(idWithBracket));
        
        functionArg = SBML_formulaToString(ASTNode_getChild(FunctionDefinition_getMath(fd),0));
        function = SBML_formulaToString(ASTNode_getChild(FunctionDefinition_getMath(fd),1));

        a = strstr(function, functionArg);
        strncpy(function + strlen(function) - strlen(a), argument, strlen(argument));
       
        formula = Rule_getFormula(r);
        match = strstr(formula, idWithBracket);
        closeBracket = strstr(formula, ")");

        _tcsnset(closeBracket+1, NULL, strlen(closeBracket)-1);

        formula = Rule_getFormula(r);
        strncpy(formula, match, function);

     }
    }
  }
  */
}
