/**
 * Filename    : Model.c
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


#include "sbml/common.h"
#include "sbml/Model.h"


/**
 * Creates a new Model and returns a pointer to it.
 */
LIBSBML_EXTERN
Model_t *
Model_create (void)
{
  Model_t *m;


  m = (Model_t *) safe_calloc(1, sizeof(Model_t));

  SBase_init((SBase_t *) m, SBML_MODEL);

  m->functionDefinition = (ListOf_t *) ListOf_create();
  m->unitDefinition     = (ListOf_t *) ListOf_create();
  m->compartment        = (ListOf_t *) ListOf_create();
  m->species            = (ListOf_t *) ListOf_create();
  m->parameter          = (ListOf_t *) ListOf_create();
  m->rule               = (ListOf_t *) ListOf_create();
  m->reaction           = (ListOf_t *) ListOf_create();
  m->event              = (ListOf_t *) ListOf_create();

  return m;
}


/**
 * Creates a new Model with the given id and returns a pointer to it.
 * This convenience function is functionally equivalent to:
 *
 *   Model_setId(Model_create(), sid);
 */
LIBSBML_EXTERN
Model_t *
Model_createWith (const char *sid)
{
  Model_t *m = Model_create();


  Model_setId(m, sid);

  return m;
}


/**
 * Creates a new Model with the given name and returns a pointer to it.
 * This convenience function is functionally equivalent to:
 *
 *   Model_setName(Model_create(), string);
 */
LIBSBML_EXTERN
Model_t *
Model_createWithName (const char *string)
{
  Model_t *m = Model_create();


  Model_setName(m, string);

  return m;
}


/**
 * Frees the given Model.
 */
LIBSBML_EXTERN
void
Model_free (Model_t *m)
{
  if (m == NULL) return;


  ListOf_free( m->functionDefinition );
  ListOf_free( m->unitDefinition     );
  ListOf_free( m->compartment        );
  ListOf_free( m->species            );
  ListOf_free( m->parameter          );
  ListOf_free( m->reaction           );
  ListOf_free( m->rule               );
  ListOf_free( m->event              );

  SBase_clear((SBase_t *) m);

  safe_free(m->id);
  safe_free(m->name);
  safe_free(m);
}


/**
 * @return the id of this Model.
 */
LIBSBML_EXTERN
const char *
Model_getId (const Model_t *m)
{
  return m->id;
}


/**
 * @return the name of this Model.
 */
LIBSBML_EXTERN
const char *
Model_getName (const Model_t *m)
{
  return m->name;
}


/**
 * @return 1 if the id of this Model has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Model_isSetId (const Model_t *m)
{
  return (m->id != NULL);
}


/**
 * @return 1 if the name of this Model has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Model_isSetName (const Model_t *m)
{
  return (m->name != NULL);
}


/**
 * Sets the id of this Model to a copy of sid.
 */
LIBSBML_EXTERN
void
Model_setId (Model_t *m, const char *sid)
{
  if (m->id == sid) return;


  if (m->id != NULL)
  {
    safe_free(m->id);
  }

  m->id = (sid == NULL) ? NULL : safe_strdup(sid);
}


/**
 * Sets the name of this Model to a copy of string (SName in L1).
 */
LIBSBML_EXTERN
void
Model_setName (Model_t *m, const char *string)
{
  if (m->name == string) return;


  if (m->name != NULL)
  {
    safe_free(m->name);
  }

  m->name = (string == NULL) ? NULL : safe_strdup(string);
}


/**
 * Unsets the id of this Model.  This is equivalent to:
 * safe_free(m->id); m->id = NULL;
 */
LIBSBML_EXTERN
void
Model_unsetId (Model_t *m)
{
  safe_free(m->id);
  m->id = NULL;
}


/**
 * Unsets the name of this Model.  This is equivalent to:
 * safe_free(m->name); m->name = NULL;
 */
LIBSBML_EXTERN
void
Model_unsetName (Model_t *m)
{
  safe_free(m->name);
  m->name = NULL;
}


/**
 * Creates a new FunctionDefinition inside this Model and returns a pointer
 * to it.  This covenience function is functionally equivalent to:
 *
 *   Model_addFunctionDefinition(m, FunctionDefinition_create());
 */
LIBSBML_EXTERN
FunctionDefinition_t *
Model_createFunctionDefinition (Model_t *m)
{
  FunctionDefinition_t *fd = FunctionDefinition_create();


  Model_addFunctionDefinition(m, fd);
  return fd;
}


/**
 * Creates a new UnitDefinition inside this Model and returns a pointer to
 * it.  This covenience function is functionally equivalent to:
 *
 *   Model_addUnitDefinition(m, UnitDefinition_create());
 */
LIBSBML_EXTERN
UnitDefinition_t *
Model_createUnitDefinition (Model_t *m)
{
  UnitDefinition_t *ud = UnitDefinition_create();


  Model_addUnitDefinition(m, ud);
  return ud;
}


/**
 * Creates a new Unit inside this Model and returns a pointer to it.  The
 * Unit is added to the last UnitDefinition created.
 *
 * If a UnitDefinitions does not exist for this model, a new Unit is not
 * created and NULL is returned.
 */
LIBSBML_EXTERN
Unit_t *
Model_createUnit (Model_t *m)
{
  Unit_t           *u    = NULL;
  UnitDefinition_t *ud   = NULL;
  unsigned int      size = Model_getNumUnitDefinitions(m);


  if (size > 0)
  {
    u  = Unit_create();
    ud = Model_getUnitDefinition(m, size - 1);

    UnitDefinition_addUnit(ud, u);
  }

  return u;
}


/**
 * Creates a new Compartment inside this Model and returns a pointer to it.
 * This covenience function is functionally equivalent to:
 *
 *   Model_addCompartment(m, Compartment_create());
 */
LIBSBML_EXTERN
Compartment_t *
Model_createCompartment (Model_t *m)
{
  Compartment_t *c = Compartment_create();


  Model_addCompartment(m, c);
  return c;
}


/**
 * Creates a new Species inside this Model and returns a pointer to it.
 * This covenience function is functionally equivalent to:
 *
 *   Model_addSpecies(m, Species_create());
 */
LIBSBML_EXTERN
Species_t *
Model_createSpecies (Model_t *m)
{
  Species_t *s = Species_create();


  Model_addSpecies(m, s);
  return s;
}


/**
 * Creates a new Parameter inside this Model and returns a pointer to it.
 * This covenience function is functionally equivalent to:
 *
 *   Model_addParameter(m, Parameter_create());
 */
LIBSBML_EXTERN
Parameter_t *
Model_createParameter (Model_t *m)
{
  Parameter_t *p = Parameter_create();


  Model_addParameter(m, p);
  return p;
}


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
Model_createAssignmentRule (Model_t *m)
{
  AssignmentRule_t *ar = AssignmentRule_create();


  Model_addRule(m, (Rule_t *) ar);
  return ar;
}


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
Model_createRateRule (Model_t *m)
{
  RateRule_t *rr = RateRule_create();


  Model_addRule(m, (Rule_t *) rr);
  return rr;
}


/**
 * Creates a new AlgebraicRule inside this Model and returns a pointer to
 * it.  This covenience function is functionally equivalent to:
 *
 *   Model_addRule(m, AlgebraicRule_create());
 */
LIBSBML_EXTERN
AlgebraicRule_t *
Model_createAlgebraicRule (Model_t *m)
{
  AlgebraicRule_t *ar = AlgebraicRule_create();


  Model_addRule(m, (Rule_t *) ar);
  return ar;
}


/**
 * Creates a new CompartmentVolumeRule inside this Model and returns a
 * pointer to it.  This covenience function is functionally equivalent to:
 *
 *   Model_addRule(m, CompartmentVolumeRule_create());
 */
LIBSBML_EXTERN
CompartmentVolumeRule_t *
Model_createCompartmentVolumeRule (Model_t *m)
{
  CompartmentVolumeRule_t *cvr = CompartmentVolumeRule_create();


  Model_addRule(m, (Rule_t *) cvr);
  return cvr;
}


/**
 * Creates a new ParameterRule inside this Model and returns a pointer to
 * it.  This covenience function is functionally equivalent to:
 *
 *   Model_addRule(m, ParameterRule_create());
 */
LIBSBML_EXTERN
ParameterRule_t *
Model_createParameterRule (Model_t *m)
{
  ParameterRule_t *pr = ParameterRule_create();


  Model_addRule(m, (Rule_t *) pr);
  return pr;
}


/**
 * Creates a new SpeciesConcentrationRule inside this Model and returns a
 * pointer to it.  This covenience function is functionally equivalent to:
 *
 *   Model_addRule(m, SpeciesConcentrationRule_create());
 */
LIBSBML_EXTERN
SpeciesConcentrationRule_t *
Model_createSpeciesConcentrationRule (Model_t *m)
{
  SpeciesConcentrationRule_t *scr = SpeciesConcentrationRule_create();


  Model_addRule(m, (Rule_t *) scr);
  return scr;
}


/**
 * Creates a new Reaction inside this Model and returns a pointer to it.
 * This covenience function is functionally equivalent to:
 *
 *   Model_addReaction(m, Reaction_create());
 */
LIBSBML_EXTERN
Reaction_t *
Model_createReaction (Model_t *m)
{
  Reaction_t *r = Reaction_create();


  Model_addReaction(m, r);
  return r;
}


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
Model_createReactant (Model_t *m)
{
  SpeciesReference_t *sr   = NULL;
  Reaction_t         *r    = NULL;
  unsigned int        size = Model_getNumReactions(m);


  if (size > 0)
  {
    sr = SpeciesReference_create();
    r  = Model_getReaction(m, size - 1);

    Reaction_addReactant(r, sr);
  }

  return sr;
}


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
Model_createProduct (Model_t *m)
{
  SpeciesReference_t *sr   = NULL;
  Reaction_t         *r    = NULL;
  unsigned int        size = Model_getNumReactions(m);


  if (size > 0)
  {
    sr = SpeciesReference_create();
    r  = Model_getReaction(m, size - 1);

    Reaction_addProduct(r, sr);
  }

  return sr;
}


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
Model_createModifier (Model_t *m)
{
  ModifierSpeciesReference_t *msr   = NULL;
  Reaction_t                 *r     = NULL;
  unsigned int                size  = Model_getNumReactions(m);


  if (size > 0)
  {
    msr = ModifierSpeciesReference_create();
    r   = Model_getReaction(m, size - 1);

    Reaction_addModifier(r, msr);
  }

  return msr;
}


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
Model_createKineticLaw (Model_t *m)
{
  KineticLaw_t *kl   = NULL;
  Reaction_t   *r    = NULL;
  unsigned int  size = Model_getNumReactions(m);


  if (size > 0)
  {
    r  = Model_getReaction(m, size - 1);
    kl = r->kineticLaw;

    if (kl == NULL)
    {
      kl            = KineticLaw_create();
      r->kineticLaw = kl;
    }
    else
    {
      kl = NULL;
    }
  }

  return kl;
}


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
Model_createKineticLawParameter (Model_t *m)
{
  Parameter_t  *p    = NULL;
  KineticLaw_t *kl   = NULL;
  Reaction_t   *r    = NULL;
  unsigned int  size = Model_getNumReactions(m);


  if (size > 0)
  {
    r  = Model_getReaction(m, size - 1);
    kl = r->kineticLaw;

    if (kl != NULL)
    {
      p = Parameter_create();
      KineticLaw_addParameter(kl, p);
    }
  }

  return p;
}


/**
 * Creates a new Event inside this Model and returns a pointer to it.
 * This covenience function is functionally equivalent to:
 *
 *   Model_addEvent(m, Event_create());
 */
LIBSBML_EXTERN
Event_t *
Model_createEvent (Model_t *m)
{
  Event_t *e = Event_create();


  Model_addEvent(m, e);
  return e;
}


/**
 * Creates a new EventAssignment inside this Model and returns a pointer to
 * it.  The EventAssignment is added to the the last Event created.
 *
 * If an Event does not exist for this model, a new EventAssignment is not
 * created and NULL is returned.
 */
LIBSBML_EXTERN
EventAssignment_t *
Model_createEventAssignment (Model_t *m)
{
  EventAssignment_t *ea   = NULL;
  Event_t           *e    = NULL;
  unsigned int      size  = Model_getNumEvents(m);


  if (size > 0)
  {
    ea = EventAssignment_create();
    e  = Model_getEvent(m, size - 1);

    Event_addEventAssignment(e, ea);
  }

  return ea;
}


/**
 * Adds the given FunctionDefinition to this Model.
 */
LIBSBML_EXTERN
void
Model_addFunctionDefinition (Model_t *m, FunctionDefinition_t *fd)
{
  ListOf_append(m->functionDefinition, fd);
}


/**
 * Adds the given UnitDefinition to this Model.
 */
LIBSBML_EXTERN
void
Model_addUnitDefinition (Model_t *m, UnitDefinition_t *ud)
{
  ListOf_append(m->unitDefinition, ud);
}


/**
 * Adds the given Compartment to this Model.
 */
LIBSBML_EXTERN
void
Model_addCompartment (Model_t *m, Compartment_t *c)
{
  ListOf_append(m->compartment, c);
}


/**
 * Adds the given Species to this Model.
 */
LIBSBML_EXTERN
void
Model_addSpecies (Model_t *m, Species_t *s)
{
  ListOf_append(m->species, s);
}


/**
 * Adds the given Parameter to this Model.
 */
LIBSBML_EXTERN
void
Model_addParameter (Model_t *m, Parameter_t *p)
{
  ListOf_append(m->parameter, p);
}


/**
 * Adds the given Rule to this Model.
 */
LIBSBML_EXTERN
void
Model_addRule (Model_t *m, Rule_t *r)
{
  ListOf_append(m->rule, r);
}


/**
 * Adds the given Reaction to this Model.
 */
LIBSBML_EXTERN
void
Model_addReaction (Model_t *m, Reaction_t *r)
{
  ListOf_append(m->reaction, r);
}


/**
 * Adds the given Event to this Model.
 */
LIBSBML_EXTERN
void
Model_addEvent (Model_t *m, Event_t *e)
{
  ListOf_append(m->event, e);
}


/**
 * @return the list of FunctionDefinitions for this Model.
 */
LIBSBML_EXTERN
ListOf_t *
Model_getListOfFunctionDefinitions (const Model_t *m)
{
  return m->functionDefinition;
}


/**
 * @return the list of UnitDefinitions for this Model.
 */
LIBSBML_EXTERN
ListOf_t *
Model_getListOfUnitDefinitions (const Model_t *m)
{
  return m->unitDefinition;
}


/**
 * @return the list of Compartments for this Model.
 */
LIBSBML_EXTERN
ListOf_t *
Model_getListOfCompartments (const Model_t *m)
{
  return m->compartment;
}


/**
 * @return the list of Species for this Model.
 */
LIBSBML_EXTERN
ListOf_t *
Model_getListOfSpecies (const Model_t *m)
{
  return m->species;
}


/**
 * @return the list of Parameters for this Model.
 */
LIBSBML_EXTERN
ListOf_t *
Model_getListOfParameters (const Model_t *m)
{
  return m->parameter;
}


/**
 * @return the list of Rules for this Model.
 */
LIBSBML_EXTERN
ListOf_t *
Model_getListOfRules (const Model_t *m)
{
  return m->rule;
}


/**
 * @return the list of Rules for this Model.
 */
LIBSBML_EXTERN
ListOf_t *
Model_getListOfReactions (const Model_t *m)
{
  return m->reaction;
}


/**
 * @return the list of Rules for this Model.
 */
LIBSBML_EXTERN
ListOf_t *
Model_getListOfEvents (const Model_t *m)
{
  return m->event;
}


/**
 * @return the list of items of the given type for this Model.  If the
 * given SBMLTypeCode does not correspond to a ListOf contained in SBML
 * Models, NULL is returned.
 */
ListOf_t *
Model_getListOfByTypecode (const Model_t *m, SBMLTypeCode_t type)
{
  ListOf_t *result;


  switch (type)
  {
    case SBML_FUNCTION_DEFINITION:
      result = m->functionDefinition;
      break;

    case SBML_UNIT_DEFINITION:
      result = m->unitDefinition;
      break;

    case SBML_COMPARTMENT:
      result = m->compartment;
      break;

    case SBML_SPECIES:
      result = m->species;
      break;

    case SBML_PARAMETER:
      result = m->parameter;
      break;

    case SBML_REACTION:
      result = m->reaction;
      break;

    case SBML_EVENT:
      result = m->event;
      break;

    case SBML_ALGEBRAIC_RULE:
    case SBML_ASSIGNMENT_RULE:
    case SBML_RATE_RULE:
    case SBML_SPECIES_CONCENTRATION_RULE:
    case SBML_COMPARTMENT_VOLUME_RULE:
    case SBML_PARAMETER_RULE:
      result = m->rule;
      break;

    default:
      result = NULL;
      break;
  }

  return result;
}


/**
 * @return the nth FunctionDefinition of this Model.
 */
LIBSBML_EXTERN
FunctionDefinition_t *
Model_getFunctionDefinition (const Model_t *m, unsigned int n)
{
  return (FunctionDefinition_t *) ListOf_get(m->functionDefinition, n);
}


/**
 * @return the nth UnitDefinition of this Model.
 */
LIBSBML_EXTERN
UnitDefinition_t *
Model_getUnitDefinition (const Model_t *m, unsigned int n)
{
  return (UnitDefinition_t *) ListOf_get(m->unitDefinition, n);
}


/**
 * @return the UnitDefinition in this Model with the given id or NULL if no
 * such UnitDefinition exists.
 */
LIBSBML_EXTERN
Species_t *
Model_getUnitDefinitionById (const Model_t *m, const char *sid)
{
  return (UnitDefinition_t *)
         ListOf_find(
            m->unitDefinition, sid, (ListItemComparator) UnitDefinitionIdCmp
         );
}


/**
 * @return the nth Compartment of this Model.
 */
LIBSBML_EXTERN
Compartment_t *
Model_getCompartment (const Model_t *m, unsigned int n)
{
  return (Compartment_t *) ListOf_get(m->compartment, n);
}


/**
 * @return the nth Species of this Model.
 */
LIBSBML_EXTERN
Species_t *
Model_getSpecies (const Model_t *m, unsigned int n)
{
  return (Species_t *) ListOf_get(m->species, n);
}


/**
 * @return the Species in this Model with the given id or NULL if no such
 * Species exists.
 */
LIBSBML_EXTERN
Species_t *
Model_getSpeciesById (const Model_t *m, const char *sid)
{
  return (Species_t *)
         ListOf_find(m->species, sid, (ListItemComparator) SpeciesIdCmp);
}


/**
 * @return the nth Parameter of this Model.
 */
LIBSBML_EXTERN
Parameter_t *
Model_getParameter (const Model_t *m, unsigned int n)
{
  return (Parameter_t *) ListOf_get(m->parameter, n);
}


/**
 * @return the nth Rule of this Model.
 */
LIBSBML_EXTERN
Rule_t *
Model_getRule (const Model_t *m, unsigned int n)
{
  return (Rule_t *) ListOf_get(m->rule, n);
}


/**
 * @return the nth Reaction of this Model.
 */
LIBSBML_EXTERN
Reaction_t *
Model_getReaction (const Model_t *m, unsigned int n)
{
  return (Reaction_t *) ListOf_get(m->reaction, n);
}


/**
 * @return the nth Event of this Model.
 */
LIBSBML_EXTERN
Event_t *
Model_getEvent (const Model_t *m, unsigned int n)
{
  return (Event_t *) ListOf_get(m->event, n);
}


/**
 * @return the number of FunctionDefinitions in this Model.
 */
LIBSBML_EXTERN
unsigned int
Model_getNumFunctionDefinitions (const Model_t *m)
{
  return ListOf_getNumItems(m->functionDefinition);
}


/**
 * @return the number of UnitDefinitions in this Model.
 */
LIBSBML_EXTERN
unsigned int
Model_getNumUnitDefinitions (const Model_t *m)
{
  return ListOf_getNumItems(m->unitDefinition);
}


/**
 * @return the number of Compartments in this Model.
 */
LIBSBML_EXTERN
unsigned int
Model_getNumCompartments (const Model_t *m)
{
  return ListOf_getNumItems(m->compartment);
}


/**
 * @return the number of Species in this Model.
 */
LIBSBML_EXTERN
unsigned int
Model_getNumSpecies (const Model_t *m)
{
  return ListOf_getNumItems(m->species);
}


/**
 * @return the number of Species in this Model with boundaryCondition set
 * to true.
 */
LIBSBML_EXTERN
unsigned int
Model_getNumSpeciesWithBoundaryCondition (const Model_t *m)
{
  return ListOf_countIf( m->species,
                         (ListItemPredicate) Species_getBoundaryCondition );
}


/**
 * @return the number of Parameters in this Model.  Parameters defined in
 * KineticLaws are not included.
 */
LIBSBML_EXTERN
unsigned int
Model_getNumParameters (const Model_t *m)
{
  return ListOf_getNumItems(m->parameter);
}


/**
 * @return the number of Rules in this Model.
 */
LIBSBML_EXTERN
unsigned int
Model_getNumRules (const Model_t *m)
{
  return ListOf_getNumItems(m->rule);
}


/**
 * @return the number of Reactions in this Model.
 */
LIBSBML_EXTERN
unsigned int
Model_getNumReactions (const Model_t *m)
{
  return ListOf_getNumItems(m->reaction);
}


/**
 * @return the number of Events in this Model.
 */
LIBSBML_EXTERN
unsigned int
Model_getNumEvents (const Model_t *m)
{
  return ListOf_getNumItems(m->event);
}
