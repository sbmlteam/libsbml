/**
 * Filename    : Model.h
 * Description : SBML Model
 * Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2002-10-18
 * Revision    : $Id$
 * Source      : $Source$
 *
 * Copyright 2002 California Institute of Technology and
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
 *     mailto:sysbio-team@caltech.edu
 *
 * Contributor(s):
 */


#ifndef Model_h
#define Model_h


#include "extern.h"

#include "ListOf.h"
#include "SBase.h"
#include "FunctionDefinition.h"
#include "UnitDefinition.h"
#include "Compartment.h"
#include "Species.h"
#include "Parameter.h"
#include "Rule.h"
#include "AssignmentRule.h"
#include "RateRule.h"
#include "AlgebraicRule.h"
#include "CompartmentVolumeRule.h"
#include "ParameterRule.h"
#include "SpeciesConcentrationRule.h"
#include "Reaction.h"
#include "Event.h"

BEGIN_C_DECLS


typedef struct
{
  SBASE_FIELDS;
  char     *id;
  char     *name;
  ListOf_t *functionDefinition;
  ListOf_t *unitDefinition;
  ListOf_t *compartment;
  ListOf_t *species;
  ListOf_t *parameter;
  ListOf_t *rule;
  ListOf_t *reaction;
  ListOf_t *event;
} Model_t;


/**
 * Creates a new Model and returns a pointer to it.
 */
LIBSBML_EXTERN
Model_t *
Model_create (void);

/**
 * Creates a new Model with the given id and returns a pointer to it.
 * This convenience function is functionally equivalent to:
 *
 *   Model_setId(Model_create(), sid);
 */
LIBSBML_EXTERN
Model_t *
Model_createWith (const char *sid);

/**
 * Creates a new Model with the given name and returns a pointer to it.
 * This convenience function is functionally equivalent to:
 *
 *   Model_setName(Model_create(), string);
 */
LIBSBML_EXTERN
Model_t *
Model_createWithName (const char *string);

/**
 * Frees the given Model.
 */
LIBSBML_EXTERN
void
Model_free (Model_t *m);


/**
 * @return the id of this Model.
 */
LIBSBML_EXTERN
const char *
Model_getId (const Model_t *m);

/**
 * @return the name of this Model.
 */
LIBSBML_EXTERN
const char *
Model_getName (const Model_t *m);


/**
 * @return 1 if the id of this Model has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Model_isSetId (const Model_t *m);

/**
 * @return 1 if the name of this Model has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Model_isSetName (const Model_t *m);


/**
 * Sets the id of this Model to a copy of sid.
 */
LIBSBML_EXTERN
void
Model_setId (Model_t *m, const char *sid);

/**
 * Sets the name of this Model to a copy of string (SName in L1).
 */
LIBSBML_EXTERN
void
Model_setName (Model_t *m, const char *string);


/**
 * Unsets the id of this Model.  This is equivalent to:
 * safe_free(m->id); m->id = NULL;
 */
LIBSBML_EXTERN
void
Model_unsetId (Model_t *m);

/**
 * Unsets the name of this Model.  This is equivalent to:
 * safe_free(m->name); m->name = NULL;
 */
LIBSBML_EXTERN
void
Model_unsetName (Model_t *m);


/**
 * Creates a new FunctionDefinition inside this Model and returns a pointer
 * to it.  This covenience function is functionally equivalent to:
 *
 *   Model_addFunctionDefinition(m, FunctionDefinition_create());
 */
LIBSBML_EXTERN
FunctionDefinition_t *
Model_createFunctionDefinition (Model_t *m);

/**
 * Creates a new UnitDefinition inside this Model and returns a pointer to
 * it.  This covenience function is functionally equivalent to:
 *
 *   Model_addUnitDefinition(m, UnitDefinition_create());
 */
LIBSBML_EXTERN
UnitDefinition_t *
Model_createUnitDefinition (Model_t *m);

/**
 * Creates a new Unit inside this Model and returns a pointer to it.  The
 * Unit is added to the last UnitDefinition created.
 *
 * If a UnitDefinitions does not exist for this model, a new Unit is not
 * created and NULL is returned.
 */
LIBSBML_EXTERN
Unit_t *
Model_createUnit (Model_t *m);

/**
 * Creates a new Compartment inside this Model and returns a pointer to it.
 * This covenience function is functionally equivalent to:
 *
 *   Model_addCompartment(m, Compartment_create());
 */
LIBSBML_EXTERN
Compartment_t *
Model_createCompartment (Model_t *m);

/**
 * Creates a new Species inside this Model and returns a pointer to it.
 * This covenience function is functionally equivalent to:
 *
 *   Model_addSpecies(m, Species_create());
 */
LIBSBML_EXTERN
Species_t *
Model_createSpecies (Model_t *m);

/**
 * Creates a new Parameter inside this Model and returns a pointer to it.
 * This covenience function is functionally equivalent to:
 *
 *   Model_addParameter(m, Parameter_create());
 */
LIBSBML_EXTERN
Parameter_t *
Model_createParameter (Model_t *m);

/**
 * Creates a new AssignmentRule inside this Model and returns a pointer to
 * it.  This covenience function is functionally equivalent to:
 *
 *   Model_addRule(m, AssignmentRule_create());
 *
 * (L2 only)
 */
LIBSBML_EXTERN
AssignmentRule_t *
Model_createAssignmentRule (Model_t *m);

/**
 * Creates a new RateRule inside this Model and returns a pointer to it.
 * This covenience function is functionally equivalent to:
 *
 *   Model_addRule(m, RateRule_create());
 *
 * (L2 only)
 */
LIBSBML_EXTERN
RateRule_t *
Model_createRateRule (Model_t *m);

/**
 * Creates a new AlgebraicRule inside this Model and returns a pointer to
 * it.  This covenience function is functionally equivalent to:
 *
 *   Model_addRule(m, AlgebraicRule_create());
 */
LIBSBML_EXTERN
AlgebraicRule_t *
Model_createAlgebraicRule (Model_t *m);

/**
 * Creates a new CompartmentVolumeRule inside this Model and returns a
 * pointer to it.  This covenience function is functionally equivalent to:
 *
 *   Model_addRule(m, CompartmentVolumeRule_create());
 */
LIBSBML_EXTERN
CompartmentVolumeRule_t *
Model_createCompartmentVolumeRule (Model_t *m);

/**
 * Creates a new ParameterRule inside this Model and returns a pointer to
 * it.  This covenience function is functionally equivalent to:
 *
 *   Model_addRule(m, ParameterRule_create());
 */
LIBSBML_EXTERN
ParameterRule_t *
Model_createParameterRule (Model_t *m);

/**
 * Creates a new SpeciesConcentrationRule inside this Model and returns a
 * pointer to it.  This covenience function is functionally equivalent to:
 *
 *   Model_addRule(m, SpeciesConcentrationRule_create());
 */
LIBSBML_EXTERN
SpeciesConcentrationRule_t *
Model_createSpeciesConcentrationRule (Model_t *m);

/**
 * Creates a new Reaction inside this Model and returns a pointer to it.
 * This covenience function is functionally equivalent to:
 *
 *   Model_addRule(m, Reaction_create());
 */
LIBSBML_EXTERN
Reaction_t *
Model_createReaction (Model_t *m);

/**
 * Creates a new Reactant (i.e. SpeciesReference) inside this Model and
 * returns a pointer to it.  The SpeciesReference is added to the reactants
 * of the last Reaction created.
 *
 * If a Reaction does not exist for this model, a new SpeciesReference is
 * not created and NULL is returned.
 */
LIBSBML_EXTERN
SpeciesReference_t *
Model_createReactant (Model_t *m);

/**
 * Creates a new Product (i.e. SpeciesReference) inside this Model and
 * returns a pointer to it.  The SpeciesReference is added to the products
 * of the last Reaction created.
 *
 * If a Reaction does not exist for this model, a new SpeciesReference is
 * not created and NULL is returned.
 */
LIBSBML_EXTERN
SpeciesReference_t *
Model_createProduct (Model_t *m);

/**
 * Creates a new Modifer (i.e. ModifierSpeciesReference) inside this Model
 * and returns a pointer to it.  The ModifierSpeciesReference is added to
 * the modifiers of the last Reaction created.
 *
 * If a Reaction does not exist for this model, a new
 * ModifierSpeciesReference is not created and NULL is returned.
 */
LIBSBML_EXTERN
ModifierSpeciesReference_t *
Model_createModifier (Model_t *m);

/**
 * Creates a new KineticLaw inside this Model and returns a pointer to it.
 * The KineticLaw is associated with the last Reaction created.
 *
 * If a Reaction does not exist for this model, or a Reaction does exist,
 * but already has a KineticLaw, a new KineticLaw is not created and NULL
 * is returned.
 */
LIBSBML_EXTERN
KineticLaw_t *
Model_createKineticLaw (Model_t *m);

/**
 * Creates a new Parameter (of a KineticLaw) inside this Model and returns
 * a pointer to it.  The Parameter is associated with the KineticLaw of the
 * last Reaction created.
 *
 * If a Reaction does not exist for this model, or a KineticLaw for the
 * Reaction, a new Parameter is not created and NULL is returned.
 */
LIBSBML_EXTERN
Parameter_t *
Model_createKineticLawParameter (Model_t *m);

/**
 * Creates a new Event inside this Model and returns a pointer to it.
 * This covenience function is functionally equivalent to:
 *
 *   Model_addEvent(m, Event_create());
 */
LIBSBML_EXTERN
Event_t *
Model_createEvent (Model_t *m);

/**
 * Creates a new EventAssignment inside this Model and returns a pointer to
 * it.  The EventAssignment is added to the the last Event created.
 *
 * If an Event does not exist for this model, a new EventAssignment is not
 * created and NULL is returned.
 */
LIBSBML_EXTERN
EventAssignment_t *
Model_createEventAssignment (Model_t *m);


/**
 * Adds the given FunctionDefinition to this Model.
 */
LIBSBML_EXTERN
void
Model_addFunctionDefinition (Model_t *m, FunctionDefinition_t *fd);

/**
 * Adds the given UnitDefinition to this Model.
 */
LIBSBML_EXTERN
void
Model_addUnitDefinition (Model_t *m, UnitDefinition_t *ud);

/**
 * Adds the given Compartment to this Model.
 */
LIBSBML_EXTERN
void
Model_addCompartment (Model_t *m, Compartment_t *c);

/**
 * Adds the given Species to this Model.
 */
LIBSBML_EXTERN
void
Model_addSpecies (Model_t *m, Species_t *s);

/**
 * Adds the given Parameter to this Model.
 */
LIBSBML_EXTERN
void
Model_addParameter (Model_t *m, Parameter_t *p);

/**
 * Adds the given Rule to this Model.
 */
LIBSBML_EXTERN
void
Model_addRule (Model_t *m, Rule_t *r);

/**
 * Adds the given Reaction to this Model.
 */
LIBSBML_EXTERN
void
Model_addReaction (Model_t *m, Reaction_t *r);

/**
 * Adds the given Event to this Model.
 */
LIBSBML_EXTERN
void
Model_addEvent (Model_t *m, Event_t *e);


/**
 * @return the list of FunctionDefinitions for this Model.
 */
LIBSBML_EXTERN
ListOf_t *
Model_getListOfFunctionDefinitions (const Model_t *m);

/**
 * @return the list of UnitDefinitions for this Model.
 */
LIBSBML_EXTERN
ListOf_t *
Model_getListOfUnitDefinitions (const Model_t *m);

/**
 * @return the list of Compartments for this Model.
 */
LIBSBML_EXTERN
ListOf_t *
Model_getListOfCompartments (const Model_t *m);

/**
 * @return the list of Species for this Model.
 */
LIBSBML_EXTERN
ListOf_t *
Model_getListOfSpecies (const Model_t *m);

/**
 * @return the list of Parameters for this Model.
 */
LIBSBML_EXTERN
ListOf_t *
Model_getListOfParameters (const Model_t *m);

/**
 * @return the list of Rules for this Model.
 */
LIBSBML_EXTERN
ListOf_t *
Model_getListOfRules (const Model_t *m);

/**
 * @return the list of Rules for this Model.
 */
LIBSBML_EXTERN
ListOf_t *
Model_getListOfReactions (const Model_t *m);

/**
 * @return the list of Rules for this Model.
 */
LIBSBML_EXTERN
ListOf_t *
Model_getListOfEvents (const Model_t *m);

/**
 * @return the list of items of the given type for this Model.  If the
 * given SBMLTypeCode does not correspond to a ListOf contained in SBML
 * Models, NULL is returned.
 */
ListOf_t *
Model_getListOfByTypecode (const Model_t *m, SBMLTypeCode_t type);

/**
 * @return the nth FunctionDefinition of this Model.
 */
LIBSBML_EXTERN
FunctionDefinition_t *
Model_getFunctionDefinition (const Model_t *m, unsigned int n);

/**
 * @return the nth UnitDefinition of this Model.
 */
LIBSBML_EXTERN
UnitDefinition_t *
Model_getUnitDefinition (const Model_t *m, unsigned int n);

/**
 * @return the UnitDefinition in this Model with the given id or NULL if
 * no such UnitDefinition exists.
 */
LIBSBML_EXTERN
UnitDefinition_t *
Model_getUnitDefinitionById (const Model_t *m, const char *sid);

/**
 * @return the nth Compartment of this Model.
 */
LIBSBML_EXTERN
Compartment_t *
Model_getCompartment (const Model_t *m, unsigned int n);

/**
 * @return the nth Species of this Model.
 */
LIBSBML_EXTERN
Species_t *
Model_getSpecies (const Model_t *m, unsigned int n);

/**
 * @return the Species in this Model with the given id or NULL if no such
 * Species exists.
 */
LIBSBML_EXTERN
Species_t *
Model_getSpeciesById (const Model_t *m, const char *sid);

/**
 * @return the nth Parameter of this Model.
 */
LIBSBML_EXTERN
Parameter_t *
Model_getParameter (const Model_t *m, unsigned int n);

/**
 * @return the nth Rule of this Model.
 */
LIBSBML_EXTERN
Rule_t *
Model_getRule (const Model_t *m, unsigned int n);

/**
 * @return the nth Reaction of this Model.
 */
LIBSBML_EXTERN
Reaction_t *
Model_getReaction (const Model_t *m, unsigned int n);

/**
 * @return the nth Event of this Model.
 */
LIBSBML_EXTERN
Event_t *
Model_getEvent (const Model_t *m, unsigned int n);


/**
 * @return the number of FunctionDefinitions in this Model.
 */
LIBSBML_EXTERN
unsigned int
Model_getNumFunctionDefinitions (const Model_t *m);

/**
 * @return the number of UnitDefinitions in this Model.
 */
LIBSBML_EXTERN
unsigned int
Model_getNumUnitDefinitions (const Model_t *m);

/**
 * @return the number of Compartments in this Model.
 */
LIBSBML_EXTERN
unsigned int
Model_getNumCompartments (const Model_t *m);

/**
 * @return the number of Species in this Model.
 */
LIBSBML_EXTERN
unsigned int
Model_getNumSpecies (const Model_t *m);

/**
 * @return the number of Species in this Model with boundaryCondition set
 * to true.
 */
LIBSBML_EXTERN
unsigned int
Model_getNumSpeciesWithBoundaryCondition (const Model_t *m);

/**
 * @return the number of Parameters in this Model.  Parameters defined in
 * KineticLaws are not included.
 */
LIBSBML_EXTERN
unsigned int
Model_getNumParameters (const Model_t *m);

/**
 * @return the number of Rules in this Model.
 */
LIBSBML_EXTERN
unsigned int
Model_getNumRules (const Model_t *m);

/**
 * @return the number of Reactions in this Model.
 */
LIBSBML_EXTERN
unsigned int
Model_getNumReactions (const Model_t *m);

/**
 * @return the number of Events in this Model.
 */
LIBSBML_EXTERN
unsigned int
Model_getNumEvents (const Model_t *m);


END_C_DECLS


#endif  /** Model_h **/
