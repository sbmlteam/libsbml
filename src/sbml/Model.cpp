/**
 * \file    Model.cpp
 * \brief   SBML Model
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


#include "SBMLVisitor.h"
#include "FunctionDefinition.h"
#include "UnitDefinition.h"
#include "Unit.h"
#include "Compartment.h"
#include "Species.h"
#include "Parameter.h"
#include "AssignmentRule.h"
#include "RateRule.h"
#include "AlgebraicRule.h"
#include "CompartmentVolumeRule.h"
#include "ParameterRule.h"
#include "SpeciesConcentrationRule.h"
#include "Reaction.h"
#include "SpeciesReference.h"
#include "ModifierSpeciesReference.h"
#include "KineticLaw.h"
#include "Event.h"
#include "EventAssignment.h"

#include "math/ASTNode.h"


#ifdef USE_LAYOUT
#  include "layout/Layout.h"
#endif  /* USE_LAYOUT */

#include "Model.h"


/**
 * Creates a new Model, optionally with its id and name attributes set.
 */
LIBSBML_EXTERN
Model::Model (const std::string& id, const std::string& name) :
    SBase ()
  , id    ( id   )
  , name  ( name )
{
  init(SBML_MODEL);
}


/**
 * Destroys this Model.
 */
LIBSBML_EXTERN
Model::~Model ()
{
}


/**
 * Accepts the given SBMLVisitor.
 */
LIBSBML_EXTERN
void
Model::accept (SBMLVisitor& v) const
{
  unsigned int n;
  bool next;


  v.visit(*this);

  //
  // FunctionDefinition
  //

  getListOfFunctionDefinitions().accept(v, SBML_FUNCTION_DEFINITION);

  for (n = 0, next = true; n < getNumFunctionDefinitions() && next; n++)
  {
    next = getFunctionDefinition(n)->accept(v);
  }

  v.leave(getListOfFunctionDefinitions(), SBML_FUNCTION_DEFINITION);


  //
  // UnitDefinition
  //

  getListOfUnitDefinitions().accept(v, SBML_UNIT_DEFINITION);

  for (n = 0, next = true; n < getNumUnitDefinitions()  && next; n++)
  {
    next = getUnitDefinition(n)->accept(v);
  }

  v.leave(getListOfUnitDefinitions(), SBML_UNIT_DEFINITION);


  //
  // Compartment
  //

  getListOfCompartments().accept(v, SBML_COMPARTMENT);

  for (n = 0, next = true; n < getNumCompartments() && next; n++)
  {
    next = getCompartment(n)->accept(v);
  }

  v.leave(getListOfCompartments(), SBML_COMPARTMENT);


  //
  // Species
  //

  getListOfSpecies().accept(v, SBML_SPECIES);

  for (n = 0, next = true; n < getNumSpecies() && next; n++)
  {
    next = getSpecies(n)->accept(v);
  }

  v.leave(getListOfSpecies(), SBML_SPECIES);


  //
  // Parameter
  //

  getListOfParameters().accept(v, SBML_PARAMETER);

  for (n = 0, next = true; n < getNumParameters() && next; n++)
  {
    next = getParameter(n)->accept(v);
  }

  v.leave(getListOfParameters(), SBML_PARAMETER);


  //
  // Rule
  //

  getListOfRules().accept(v, SBML_ALGEBRAIC_RULE);

  for (n = 0, next = true; n < getNumRules() && next; n++)
  {
    next = getRule(n)->accept(v);
  }

  v.leave(getListOfRules(), SBML_ALGEBRAIC_RULE);


  //
  // Reaction
  //

  getListOfReactions().accept(v, SBML_REACTION);

  for (n = 0, next = true; n < getNumReactions() && next; n++)
  {
    next = getReaction(n)->accept(v);
  }

  v.leave(getListOfReactions(), SBML_REACTION);


  //
  // Event
  //

  getListOfEvents().accept(v, SBML_EVENT);

  for (n = 0, next = true; n < getNumEvents() && next; n++)
  {
    next = getEvent(n)->accept(v);
  }

  v.leave(getListOfEvents(), SBML_EVENT);

  v.leave(*this);
}


/**
 * @return the id of this Model.
 */
LIBSBML_EXTERN
const std::string&
Model::getId () const
{
  return id;
}


/**
 * @return the name of this Model.
 */
LIBSBML_EXTERN
const std::string&
Model::getName () const
{
  return name;
}


/**
 * @return true if the id of this Model has been set, false otherwise.
 */
LIBSBML_EXTERN
bool
Model::isSetId () const
{
  return ! id.empty();
}


/**
 * @return true if the name of this Model has been set, false otherwise.
 */
LIBSBML_EXTERN
bool
Model::isSetName () const
{
  return ! name.empty();
}


/**
 * Moves the id field to the name field for this Model and all of its
 * contituent UnitDefinitions, Compartments, Species, Parameters, and
 * Reactions.  This method is used for converting from L2 to L1.
 *
 * NOTE: Any object with its name field already set will be skipped.
 *
 * @see moveIdToName
 */
LIBSBML_EXTERN
void
Model::moveAllIdsToNames ()
{
  int n;
  int size;


  moveIdToName();

  size = getNumUnitDefinitions();
  for (n = 0; n < size; n++) getUnitDefinition(n)->moveIdToName();

  size = getNumCompartments();
  for (n = 0; n < size; n++) getCompartment(n)->moveIdToName();

  size = getNumSpecies();
  for (n = 0; n < size; n++) getSpecies(n)->moveIdToName();

  size = getNumParameters();
  for (n = 0; n < size; n++) getParameter(n)->moveIdToName();

  size = getNumReactions();
  for (n = 0; n < size; n++) getReaction(n)->moveIdToName();
}


/**
 * Moves the name field to the id field for this Model and all of its
 * contituent UnitDefinitions, Compartments, Species, Parameters, and
 * Reactions.  This method is used for converting from L1 to L2.
 *
 * NOTE: Any object with its id field already set will be skipped.
 *
 * @see moveNameToId
 */
LIBSBML_EXTERN
void
Model::moveAllNamesToIds ()
{
  int n;
  int size;


  moveNameToId();

  size = getNumUnitDefinitions();
  for (n = 0; n < size; n++) getUnitDefinition(n)->moveNameToId();

  size = getNumCompartments();
  for (n = 0; n < size; n++) getCompartment(n)->moveNameToId();

  size = getNumSpecies();
  for (n = 0; n < size; n++) getSpecies(n)->moveNameToId();

  size = getNumParameters();
  for (n = 0; n < size; n++) getParameter(n)->moveNameToId();

  size = getNumReactions();
  for (n = 0; n < size; n++) getReaction(n)->moveNameToId();
}


/**
 * Moves the id field of this Model to its name field (iff name is not
 * already set).  This method is used for converting from L2 to L1.
 */
LIBSBML_EXTERN
void
Model::moveIdToName ()
{
  if ( isSetName() ) return;

  setName( getId() );
  setId  ( "" );
}


/**
 * Moves the name field of this Model to its id field (iff id is not
 * already set).  This method is used for converting from L1 to L2.
 */
LIBSBML_EXTERN
void
Model::moveNameToId ()
{
  if ( isSetId() ) return;

  setId  ( getName() );
  setName( "" );
}


/**
 * Sets the id of this Model to a copy of sid.
 */
LIBSBML_EXTERN
void
Model::setId (const std::string& sid)
{
  id = sid;
}


/**
 * Sets the name of this Model to a copy of string (SName in L1).
 */
LIBSBML_EXTERN
void
Model::setName (const std::string& string)
{
  name = string;
}


/**
 * Unsets the id of this Model.
 */
LIBSBML_EXTERN
void
Model::unsetId ()
{
  id.erase();
}


/**
 * Unsets the name of this Model.
 */
LIBSBML_EXTERN
void
Model::unsetName ()
{
  name.erase();
}


/**
 * Creates a new FunctionDefinition inside this Model and returns it.  This
 * covenience method is equivalent to:
 *
 *   addFunctionDefinition( FunctionDefinition() );
 */
LIBSBML_EXTERN
FunctionDefinition&
Model::createFunctionDefinition ()
{
  FunctionDefinition* fd = new FunctionDefinition;


  addFunctionDefinition(*fd);
  return *fd;
}


/**
 * Creates a new UnitDefinition inside this Model and returns it.  This
 * covenience method is equivalent to:
 *
 *   addUnitDefinition( UnitDefinition() );
 */
LIBSBML_EXTERN
UnitDefinition&
Model::createUnitDefinition ()
{
  UnitDefinition* ud = new UnitDefinition;


  addUnitDefinition(*ud);
  return *ud;
}


/**
 * Creates a new Unit inside this Model and returns a pointer to it.  The
 * Unit is added to the last UnitDefinition created.
 *
 * If a UnitDefinitions does not exist for this model, a new Unit is not
 * created and NULL is returned.
 */
LIBSBML_EXTERN
Unit*
Model::createUnit ()
{
  Unit*           u    = NULL;
  UnitDefinition* ud   = NULL;
  unsigned int    size = getNumUnitDefinitions();


  if (size > 0)
  {
    u  = new Unit;
    ud = getUnitDefinition(size - 1);

    ud->addUnit(*u);
  }

  return u;
}


/**
 * Creates a new Compartment inside this Model and returns it.  This
 * covenience method is equivalent to:
 *
 *   addCompartment( Compartment() );
 */
LIBSBML_EXTERN
Compartment&
Model::createCompartment ()
{
  Compartment* c = new Compartment;


  addCompartment(*c);
  return *c;
}


/**
 * Creates a new Species inside this Model and returns .  This covenience
 * method is equivalent to:
 *
 *   addSpecies( Species() );
 */
LIBSBML_EXTERN
Species&
Model::createSpecies ()
{
  Species* s = new Species;


  addSpecies(*s);
  return *s;
}


/**
 * Creates a new Parameter inside this Model and returns.  This covenience
 * method is equivalent to:
 *
 *   addParameter( Parameter() );
 */
LIBSBML_EXTERN
Parameter&
Model::createParameter ()
{
  Parameter* p = new Parameter;


  addParameter(*p);
  return *p;
}


/**
 * Creates a new AssignmentRule inside this Model and returns .  This
 * covenience method is equivalent to:
 *
 *   addRule( AssignmentRule() );
 *
 * (L2 only)
 */
LIBSBML_EXTERN
AssignmentRule&
Model::createAssignmentRule ()
{
  AssignmentRule* ar = new AssignmentRule;


  addRule(*ar);
  return *ar;
}


/**
 * Creates a new RateRule inside this Model and returns it.  This
 * covenience method is equivalent to:
 *
 *   addRule( RateRule() );
 *
 * (L2 only)
 */
LIBSBML_EXTERN
RateRule&
Model::createRateRule ()
{
  RateRule* rr = new RateRule();


  addRule(*rr);
  return *rr;
}


/**
 * Creates a new AlgebraicRule inside this Model and returns it.  This
 * covenience method is equivalent to:
 *
 *   addRule( AlgebraicRule() );
 */
LIBSBML_EXTERN
AlgebraicRule&
Model::createAlgebraicRule ()
{
  AlgebraicRule* ar = new AlgebraicRule;


  addRule(*ar);
  return *ar;
}


/**
 * Creates a new CompartmentVolumeRule inside this Model and returns.  This
 * covenience method is equivalent to:
 *
 *   addRule( CompartmentVolumeRule() );
 */
LIBSBML_EXTERN
CompartmentVolumeRule&
Model::createCompartmentVolumeRule ()
{
  CompartmentVolumeRule* cvr = new CompartmentVolumeRule;


  addRule(*cvr);
  return *cvr;
}


/**
 * Creates a new ParameterRule inside this Model and returns it.  This
 * covenience method is equivalent to:
 *
 *   addRule( ParameterRule() );
 */
LIBSBML_EXTERN
ParameterRule&
Model::createParameterRule ()
{
  ParameterRule* pr = new ParameterRule;


  addRule(*pr);
  return *pr;
}


/**
 * Creates a new SpeciesConcentrationRule inside this Model and returns it.
 * This covenience method is equivalent to:
 *
 *   addRule( SpeciesConcentrationRule() );
 */
LIBSBML_EXTERN
SpeciesConcentrationRule&
Model::createSpeciesConcentrationRule ()
{
  SpeciesConcentrationRule* scr = new SpeciesConcentrationRule;


  addRule(*scr);
  return *scr;
}


/**
 * Creates a new Reaction inside this Model and returns.  This covenience
 * method is equivalent to:
 *
 *   addReaction( Reaction() );
 */
LIBSBML_EXTERN
Reaction&
Model::createReaction ()
{
  Reaction* r = new Reaction;


  addReaction(*r);
  return *r;
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
SpeciesReference*
Model::createReactant ()
{
  SpeciesReference* sr   = NULL;
  Reaction*         r    = NULL;
  unsigned int      size = getNumReactions();


  if (size > 0)
  {
    sr = new SpeciesReference;
    r  = getReaction(size - 1);

    r->addReactant(*sr);
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
SpeciesReference*
Model::createProduct ()
{
  SpeciesReference* sr   = NULL;
  Reaction*         r    = NULL;
  unsigned int      size = getNumReactions();


  if (size > 0)
  {
    sr = new SpeciesReference;
    r  = getReaction(size - 1);

    r->addProduct(*sr);
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
ModifierSpeciesReference*
Model::createModifier ()
{
  ModifierSpeciesReference* msr   = NULL;
  Reaction*                 r     = NULL;
  unsigned int              size  = getNumReactions();


  if (size > 0)
  {
    msr = new ModifierSpeciesReference;
    r   = getReaction(size - 1);

    r->addModifier(*msr);
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
KineticLaw*
Model::createKineticLaw ()
{
  KineticLaw*  kl   = NULL;
  Reaction*    r    = NULL;
  unsigned int size = getNumReactions();


  if (size > 0)
  {
    r  = getReaction(size - 1);
    kl = r->getKineticLaw();

    if (kl == NULL)
    {
      kl = new KineticLaw;
      r->setKineticLaw(*kl);
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
Parameter*
Model::createKineticLawParameter ()
{
  Parameter*   p    = NULL;
  KineticLaw*  kl   = NULL;
  Reaction*    r    = NULL;
  unsigned int size = getNumReactions();


  if (size > 0)
  {
    r  = getReaction(size - 1);
    kl = r->getKineticLaw();

    if (kl != NULL)
    {
      p = new Parameter;
      kl->addParameter(*p);
    }
  }

  return p;
}


/**
 * Creates a new Event inside this Model and returns.  This covenience
 * function is functionally equivalent to:
 *
 *   addEvent( Event() );
 */
LIBSBML_EXTERN
Event&
Model::createEvent ()
{
  Event* e = new Event;


  addEvent(*e);
  return *e;
}


/**
 * Creates a new EventAssignment inside this Model and returns a pointer to
 * it.  The EventAssignment is added to the the last Event created.
 *
 * If an Event does not exist for this model, a new EventAssignment is not
 * created and NULL is returned.
 */
LIBSBML_EXTERN
EventAssignment*
Model::createEventAssignment ()
{
  EventAssignment* ea   = NULL;
  Event*           e    = NULL;
  unsigned int     size = getNumEvents();


  if (size > 0)
  {
    ea = new EventAssignment;
    e  = getEvent(size - 1);

    e->addEventAssignment(*ea);
  }

  return ea;
}


/**
 * Adds the given FunctionDefinition to this Model.
 */
LIBSBML_EXTERN
void
Model::addFunctionDefinition (FunctionDefinition& fd)
{
  functionDefinition.append(&fd);
}


/**
 * Adds the given UnitDefinition to this Model.
 */
LIBSBML_EXTERN
void
Model::addUnitDefinition (UnitDefinition& ud)
{
  unitDefinition.append(&ud);
}


/**
 * Adds the given Compartment to this Model.
 */
LIBSBML_EXTERN
void
Model::addCompartment (Compartment& c)
{
  compartment.append(&c);
}


/**
 * Adds the given Species to this Model.
 */
LIBSBML_EXTERN
void
Model::addSpecies (Species& s)
{
  species.append(&s);
}


/**
 * Adds the given Parameter to this Model.
 */
LIBSBML_EXTERN
void
Model::addParameter (Parameter& p)
{
  parameter.append(&p);
}


/**
 * Adds the given Rule to this Model.
 */
LIBSBML_EXTERN
void
Model::addRule (Rule& r)
{
  rule.append(&r);
}


/**
 * Adds the given Reaction to this Model.
 */
LIBSBML_EXTERN
void
Model::addReaction (Reaction& r)
{
  reaction.append(&r);
}


/**
 * Adds the given Event to this Model.
 */
LIBSBML_EXTERN
void
Model::addEvent (Event& e)
{
  event.append(&e);
}


/**
 * @return the list of FunctionDefinitions for this Model.
 */
LIBSBML_EXTERN
ListOf&
Model::getListOfFunctionDefinitions ()
{
  return functionDefinition;
}


/**
 * @return the list of FunctionDefinitions for this Model.
 */
LIBSBML_EXTERN
const ListOf&
Model::getListOfFunctionDefinitions () const
{
  return functionDefinition;
}


/**
 * @return the list of UnitDefinitions for this Model.
 */
LIBSBML_EXTERN
ListOf&
Model::getListOfUnitDefinitions ()
{
  return unitDefinition;
}


/**
 * @return the list of UnitDefinitions for this Model.
 */
LIBSBML_EXTERN
const ListOf&
Model::getListOfUnitDefinitions () const
{
  return unitDefinition;
}


/**
 * @return the list of Compartments for this Model.
 */
LIBSBML_EXTERN
ListOf&
Model::getListOfCompartments ()
{
  return compartment;
}


/**
 * @return the list of Compartments for this Model.
 */
LIBSBML_EXTERN
const ListOf&
Model::getListOfCompartments () const
{
  return compartment;
}


/**
 * @return the list of Species for this Model.
 */
LIBSBML_EXTERN
ListOf&
Model::getListOfSpecies ()
{
  return species;
}


/**
 * @return the list of Species for this Model.
 */
LIBSBML_EXTERN
const ListOf&
Model::getListOfSpecies () const
{
  return species;
}


/**
 * @return the list of Parameters for this Model.
 */
LIBSBML_EXTERN
ListOf&
Model::getListOfParameters ()
{
  return parameter;
}


/**
 * @return the list of Parameters for this Model.
 */
LIBSBML_EXTERN
const ListOf&
Model::getListOfParameters () const
{
  return parameter;
}


/**
 * @return the list of Rules for this Model.
 */
LIBSBML_EXTERN
ListOf&
Model::getListOfRules ()
{
  return rule;
}


/**
 * @return the list of Rules for this Model.
 */
LIBSBML_EXTERN
const ListOf&
Model::getListOfRules () const
{
  return rule;
}


/**
 * @return the list of Rules for this Model.
 */
LIBSBML_EXTERN
ListOf&
Model::getListOfReactions ()
{
  return reaction;
}


/**
 * @return the list of Rules for this Model.
 */
LIBSBML_EXTERN
const ListOf&
Model::getListOfReactions () const
{
  return reaction;
}


/**
 * @return the list of Rules for this Model.
 */
LIBSBML_EXTERN
ListOf&
Model::getListOfEvents ()
{
  return event;
}


/**
 * @return the list of Rules for this Model.
 */
LIBSBML_EXTERN
const ListOf&
Model::getListOfEvents () const
{
  return event;
}


/**
 * @return the list of items of the given type for this Model.  If the
 * given SBMLTypeCode does not correspond to a ListOf contained in SBML
 * Models, NULL is returned.
 */
ListOf*
Model::getListOfByTypecode (SBMLTypeCode_t type)
{
  ListOf* result;


  switch (type)
  {
    case SBML_FUNCTION_DEFINITION:
      result = &functionDefinition;
      break;

    case SBML_UNIT_DEFINITION:
      result = &unitDefinition;
      break;

    case SBML_COMPARTMENT:
      result = &compartment;
      break;

    case SBML_SPECIES:
      result = &species;
      break;

    case SBML_PARAMETER:
      result = &parameter;
      break;

    case SBML_REACTION:
      result = &reaction;
      break;

    case SBML_EVENT:
      result = &event;
      break;

    case SBML_ALGEBRAIC_RULE:
    case SBML_ASSIGNMENT_RULE:
    case SBML_RATE_RULE:
    case SBML_SPECIES_CONCENTRATION_RULE:
    case SBML_COMPARTMENT_VOLUME_RULE:
    case SBML_PARAMETER_RULE:
      result = &rule;
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
FunctionDefinition*
Model::getFunctionDefinition (unsigned int n) const
{
  return static_cast<FunctionDefinition*>( functionDefinition.get(n) );
}


/**
 * @return the FunctionDefinition in this Model with the given id or NULL
 * if no such FunctionDefinition exists.
 */
LIBSBML_EXTERN
FunctionDefinition*
Model::getFunctionDefinition (const std::string& sid) const
{
  void* x =
    functionDefinition.find( sid.c_str(),
                             (ListItemComparator) FunctionDefinitionIdCmp );


  return static_cast<FunctionDefinition*>(x);
}


/**
 * @return the nth UnitDefinition of this Model.
 */
LIBSBML_EXTERN
UnitDefinition*
Model::getUnitDefinition (unsigned int n) const
{
  return static_cast<UnitDefinition*>( unitDefinition.get(n) );
}


/**
 * @return the UnitDefinition in this Model with the given id or NULL if no
 * such UnitDefinition exists.
 */
LIBSBML_EXTERN
UnitDefinition*
Model::getUnitDefinition (const std::string& sid) const
{
  void* x =
    unitDefinition.find( sid.c_str(),
                         (ListItemComparator) UnitDefinitionIdCmp );


  return static_cast<UnitDefinition*>(x);
}


/**
 * @return the nth Compartment of this Model.
 */
LIBSBML_EXTERN
Compartment*
Model::getCompartment (unsigned int n) const
{
  return static_cast<Compartment*>( compartment.get(n) );
}


/**
 * @return the Compartment in this Model with the given id or NULL if no
 * such Compartment exists.
 */
LIBSBML_EXTERN
Compartment*
Model::getCompartment (const std::string& sid) const
{
  void* x =
    compartment.find( sid.c_str(), (ListItemComparator) CompartmentIdCmp );


  return static_cast<Compartment*>(x);
}


/**
 * @return the nth Species of this Model.
 */
LIBSBML_EXTERN
Species*
Model::getSpecies (unsigned int n) const
{
  return static_cast<Species*>( species.get(n) );
}


/**
 * @return the Species in this Model with the given id or NULL if no such
 * Species exists.
 */
LIBSBML_EXTERN
Species*
Model::getSpecies (const std::string& sid) const
{
  void* x = species.find( sid.c_str(), (ListItemComparator) SpeciesIdCmp );


  return static_cast<Species*>(x);
}


/**
 * @return the nth Parameter of this Model.
 */
LIBSBML_EXTERN
Parameter*
Model::getParameter (unsigned int n) const
{
  return static_cast<Parameter*>( parameter.get(n) );
}


/**
 * @return the Parameter in this Model with the given id or NULL if no such
 * Parameter exists.
 */
LIBSBML_EXTERN
Parameter*
Model::getParameter (const std::string& sid) const
{
  void* x = parameter.find( sid.c_str(), (ListItemComparator) ParameterIdCmp );


  return static_cast<Parameter*>(x);
}


/**
 * @return the nth Rule of this Model.
 */
LIBSBML_EXTERN
Rule*
Model::getRule (unsigned int n) const
{
  return static_cast<Rule*>( rule.get(n) );
}


/**
 * @return the nth Reaction of this Model.
 */
LIBSBML_EXTERN
Reaction*
Model::getReaction (unsigned int n) const
{
  return static_cast<Reaction*>( reaction.get(n) );
}


/**
 * @return the Reaction in this Model with the given id or NULL if no such
 * Reaction exists.
 */
LIBSBML_EXTERN
Reaction*
Model::getReaction (const std::string& sid) const
{
  void* x = reaction.find( sid.c_str(), (ListItemComparator) ReactionIdCmp );


  return static_cast<Reaction*>(x);
}


/**
 * @return the nth Event of this Model.
 */
LIBSBML_EXTERN
Event*
Model::getEvent (unsigned int n) const
{
  return static_cast<Event*>( event.get(n) );
}


/**
 * @return the Event in this Model with the given id or NULL if no such
 * Event exists.
 */
LIBSBML_EXTERN
Event*
Model::getEvent (const std::string& sid) const
{
  void* x = event.find( sid.c_str(), (ListItemComparator) EventIdCmp );


  return static_cast<Event*>(x);
}


/**
 * @return the number of FunctionDefinitions in this Model.
 */
LIBSBML_EXTERN
unsigned int
Model::getNumFunctionDefinitions () const
{
  return functionDefinition.getNumItems();
}


/**
 * @return the number of UnitDefinitions in this Model.
 */
LIBSBML_EXTERN
unsigned int
Model::getNumUnitDefinitions () const
{
  return unitDefinition.getNumItems();
}


/**
 * @return the number of Compartments in this Model.
 */
LIBSBML_EXTERN
unsigned int
Model::getNumCompartments () const
{
  return compartment.getNumItems();
}


/**
 * @return the number of Species in this Model.
 */
LIBSBML_EXTERN
unsigned int
Model::getNumSpecies () const
{
  return species.getNumItems();
}


/**
 * @return the number of Species in this Model with boundaryCondition set
 * to true.
 */
LIBSBML_EXTERN
unsigned int
Model::getNumSpeciesWithBoundaryCondition () const
{
  return species.countIf( (ListItemPredicate) Species_getBoundaryCondition );
}


/**
 * @return the number of Parameters in this Model.  Parameters defined in
 * KineticLaws are not included.
 */
LIBSBML_EXTERN
unsigned int
Model::getNumParameters () const
{
  return parameter.getNumItems();
}


/**
 * @return the number of Rules in this Model.
 */
LIBSBML_EXTERN
unsigned int
Model::getNumRules () const
{
  return rule.getNumItems();
}


/**
 * @return the number of Reactions in this Model.
 */
LIBSBML_EXTERN
unsigned int
Model::getNumReactions () const
{
  return reaction.getNumItems();
}


/**
 * @return the number of Events in this Model.
 */
LIBSBML_EXTERN
unsigned int
Model::getNumEvents () const
{
  return event.getNumItems();
}


/**
 * @return true if the given ASTNode is a boolean.  Often times, this
 * question can be answered with the ASTNode's own isBoolean() method,
 * but if the AST is an expression that calls a function defined in the
 * Model's ListOf FunctionDefinitions, the model is needed for lookup
 * context.
 */
LIBSBML_EXTERN
bool
Model::isBoolean (const ASTNode* node) const
{
  if ( !node )
  {
    return false;
  }

  else if ( node->isBoolean() )
  {
    return true;
  }

  else if (node->getType() == AST_FUNCTION)
  {
    FunctionDefinition* fd = getFunctionDefinition( node->getName() );

    if (fd && fd->isSetMath())
    {
      return isBoolean( fd->getMath()->getRightChild() );
    }
    else
    {
      return false;
    }
  }

  else if (node->getType() == AST_FUNCTION_PIECEWISE)
  {
    for (unsigned int c = 0; c < node->getNumChildren(); c += 2)
    {
      if ( !isBoolean( node->getChild(c) ) ) return false;
    }

    return true;
  }

  return false;
}



#ifdef USE_LAYOUT


/**
 * Returns a reference to the ListOf object that holds the layouts.
 */
LIBSBML_EXTERN
ListOf&
Model::getListOfLayouts ()
{
  return layouts;
}

     
/**
 * Returns a reference to the ListOf object that holds the layouts.
 */
LIBSBML_EXTERN
const ListOf&
Model::getListOfLayouts () const
{
  return layouts;
}


/**
 * Returns the layout object that belongs to the given index. If the index
 * is invalid, NULL is returned.
 */
LIBSBML_EXTERN
Layout*
Model::getLayout (unsigned int index) const
{
  return static_cast<Layout*>( layouts.get(index) );
}


/**
 * Adds a copy of the layout object to the list of layouts.
 */ 
LIBSBML_EXTERN
void
Model::addLayout (Layout& layout)
{
  layouts.append(&layout);
}


/**
 * Creates a new layout object and adds it to the list of layout objects.
 * A reference to the newly created object is returned.
 */
LIBSBML_EXTERN
Layout&
Model::createLayout ()
{
  Layout* l = new Layout();


  addLayout(*l);
  return *l;
}


#endif  /* USE_LAYOUT */




/**
 * Creates a new Model and returns a pointer to it.
 */
LIBSBML_EXTERN
Model_t *
Model_create (void)
{
  return new(std::nothrow) Model;
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
  return new(std::nothrow) Model(sid ? sid : "");
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
  return new(std::nothrow) Model("", string ? string : "");
}


/**
 * Frees the given Model.
 */
LIBSBML_EXTERN
void
Model_free (Model_t *m)
{
  delete static_cast<Model*>(m);
}


/**
 * @return the id of this Model.
 */
LIBSBML_EXTERN
const char *
Model_getId (const Model_t *m)
{
  const Model* x = static_cast<const Model*>(m);


  return x->isSetId() ? x->getId().c_str() : NULL;
}


/**
 * @return the name of this Model.
 */
LIBSBML_EXTERN
const char *
Model_getName (const Model_t *m)
{
  const Model* x = static_cast<const Model*>(m);


  return x->isSetName() ? x->getName().c_str() : NULL;
}


/**
 * @return 1 if the id of this Model has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Model_isSetId (const Model_t *m)
{
  return (int) static_cast<const Model*>(m)->isSetId();
}


/**
 * @return 1 if the name of this Model has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Model_isSetName (const Model_t *m)
{
  return (int) static_cast<const Model*>(m)->isSetName();
}


/**
 * Moves the id field to the name field for this Model and all of its
 * contituent UnitDefinitions, Compartments, Species, Parameters, and
 * Reactions.  This method is used for converting from L2 to L1.
 *
 * NOTE: Any object with its name field already set will be skipped.
 *
 * @see moveIdToName
 */
LIBSBML_EXTERN
void
Model_moveAllIdsToNames (Model_t *m)
{
  static_cast<Model*>(m)->moveAllIdsToNames();
}


/**
 * Moves the name field to the id field for this Model and all of its
 * contituent UnitDefinitions, Compartments, Species, Parameters, and
 * Reactions.  This method is used for converting from L1 to L2.
 *
 * NOTE: Any object with its id field already set will be skipped.
 *
 * @see moveNameToId
 */
LIBSBML_EXTERN
void
Model_moveAllNamesToIds (Model_t *m)
{
  static_cast<Model*>(m)->moveAllNamesToIds();
}


/**
 * Moves the id field of this Model to its name field (iff name is not
 * already set).  This method is used for converting from L2 to L1.
 */
LIBSBML_EXTERN
void
Model_moveIdToName (Model_t *m)
{
  static_cast<Model*>(m)->moveIdToName();
}


/**
 * Moves the name field of this Model to its id field (iff id is not
 * already set).  This method is used for converting from L1 to L2.
 */
LIBSBML_EXTERN
void
Model_moveNameToId (Model_t *m)
{
  static_cast<Model*>(m)->moveNameToId();
}


/**
 * Sets the id of this Model to a copy of sid.
 */
LIBSBML_EXTERN
void
Model_setId (Model_t *m, const char *sid)
{
  if (sid == NULL)
  {
    static_cast<Model*>(m)->unsetId();
  }
  else
  {
    static_cast<Model*>(m)->setId(sid);
  }
}


/**
 * Sets the name of this Model to a copy of string (SName in L1).
 */
LIBSBML_EXTERN
void
Model_setName (Model_t *m, const char *string)
{
  if (string == NULL)
  {
    static_cast<Model*>(m)->unsetName();
  }
  else
  {
    static_cast<Model*>(m)->setName(string);
  }
}


/**
 * Unsets the id of this Model.  This is equivalent to:
 * safe_free(m->id); m->id = NULL;
 */
LIBSBML_EXTERN
void
Model_unsetId (Model_t *m)
{
  static_cast<Model*>(m)->unsetId();
}


/**
 * Unsets the name of this Model.  This is equivalent to:
 * safe_free(m->name); m->name = NULL;
 */
LIBSBML_EXTERN
void
Model_unsetName (Model_t *m)
{
  static_cast<Model*>(m)->unsetName();
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
  return (FunctionDefinition_t *) &
         static_cast<Model*>(m)->createFunctionDefinition();
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
  return (UnitDefinition_t *) &
         static_cast<Model*>(m)->createUnitDefinition();
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
  return (Unit_t *) static_cast<Model*>(m)->createUnit();
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
  return (Compartment_t *) & static_cast<Model*>(m)->createCompartment();
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
  return (Species_t *) & static_cast<Model*>(m)->createSpecies();
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
  return (Parameter_t *) & static_cast<Model*>(m)->createParameter();
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
  return (AssignmentRule_t *) &
         static_cast<Model*>(m)->createAssignmentRule();
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
  return (RateRule_t *) & static_cast<Model*>(m)->createRateRule();
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
  return (AlgebraicRule_t *) &
         static_cast<Model*>(m)->createAlgebraicRule();
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
  return (CompartmentVolumeRule_t *) &
         static_cast<Model*>(m)->createCompartmentVolumeRule();
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
  return (ParameterRule_t *) &
         static_cast<Model*>(m)->createParameterRule();
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
  return (SpeciesConcentrationRule_t *) &
         static_cast<Model*>(m)->createSpeciesConcentrationRule();
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
  return (Reaction_t *) & static_cast<Model*>(m)->createReaction();
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
  return (SpeciesReference_t *) static_cast<Model*>(m)->createReactant();
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
  return (SpeciesReference_t *) static_cast<Model*>(m)->createProduct();
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
  return (ModifierSpeciesReference_t *)
         static_cast<Model*>(m)->createModifier();
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
  return (KineticLaw_t *) static_cast<Model*>(m)->createKineticLaw();
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
  return (Parameter_t *)
         static_cast<Model*>(m)->createKineticLawParameter();
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
  return (Event_t *) & static_cast<Model*>(m)->createEvent();
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
  return (EventAssignment_t *)
         static_cast<Model*>(m)->createEventAssignment();
}


/**
 * Adds the given FunctionDefinition to this Model.
 */
LIBSBML_EXTERN
void
Model_addFunctionDefinition (Model_t *m, FunctionDefinition_t *fd)
{
  if (fd != NULL)
  {
    FunctionDefinition* x = static_cast<FunctionDefinition*>(fd);
    static_cast<Model*>(m)->addFunctionDefinition(*x);
  }
}


/**
 * Adds the given UnitDefinition to this Model.
 */
LIBSBML_EXTERN
void
Model_addUnitDefinition (Model_t *m, UnitDefinition_t *ud)
{
  if (ud != NULL)
  {
    UnitDefinition* x = static_cast<UnitDefinition*>(ud);
    static_cast<Model*>(m)->addUnitDefinition(*x);
  }
}


/**
 * Adds the given Compartment to this Model.
 */
LIBSBML_EXTERN
void
Model_addCompartment (Model_t *m, Compartment_t *c)
{
  if (c != NULL)
  {
    Compartment* x = static_cast<Compartment*>(c);
    static_cast<Model*>(m)->addCompartment(*x);
  }
}


/**
 * Adds the given Species to this Model.
 */
LIBSBML_EXTERN
void
Model_addSpecies (Model_t *m, Species_t *s)
{
  if (s != NULL)
  {
    Species* x = static_cast<Species*>(s);
    static_cast<Model*>(m)->addSpecies(*x);
  }
}


/**
 * Adds the given Parameter to this Model.
 */
LIBSBML_EXTERN
void
Model_addParameter (Model_t *m, Parameter_t *p)
{
  if (p != NULL)
  {
    Parameter* x = static_cast<Parameter*>(p);
    static_cast<Model*>(m)->addParameter(*x);
  }
}


/**
 * Adds the given Rule to this Model.
 */
LIBSBML_EXTERN
void
Model_addRule (Model_t *m, Rule_t *r)
{
  if (r != NULL)
  {
    Rule* x = static_cast<Rule*>(r);
    static_cast<Model*>(m)->addRule(*x);
  }
}


/**
 * Adds the given Reaction to this Model.
 */
LIBSBML_EXTERN
void
Model_addReaction (Model_t *m, Reaction_t *r)
{
  if (r != NULL)
  {
    Reaction* x = static_cast<Reaction*>(r);
    static_cast<Model*>(m)->addReaction(*x);
  }
}


/**
 * Adds the given Event to this Model.
 */
LIBSBML_EXTERN
void
Model_addEvent (Model_t *m, Event_t *e)
{
  if (e != NULL)
  {
    Event* x = static_cast<Event*>(e);
    static_cast<Model*>(m)->addEvent(*x);
  }
}


/**
 * @return the list of FunctionDefinitions for this Model.
 */
LIBSBML_EXTERN
ListOf_t *
Model_getListOfFunctionDefinitions (Model_t *m)
{
  return (ListOf_t *) &
         static_cast<Model*>(m)->getListOfFunctionDefinitions();
}


/**
 * @return the list of UnitDefinitions for this Model.
 */
LIBSBML_EXTERN
ListOf_t *
Model_getListOfUnitDefinitions (Model_t *m)
{
  return (ListOf_t *) &
         static_cast<Model*>(m)->getListOfUnitDefinitions();
}


/**
 * @return the list of Compartments for this Model.
 */
LIBSBML_EXTERN
ListOf_t *
Model_getListOfCompartments (Model_t *m)
{
  return (ListOf_t *) &
         static_cast<Model*>(m)->getListOfCompartments();
}


/**
 * @return the list of Species for this Model.
 */
LIBSBML_EXTERN
ListOf_t *
Model_getListOfSpecies (Model_t *m)
{
  return (ListOf_t *) &
         static_cast<Model*>(m)->getListOfSpecies();
}


/**
 * @return the list of Parameters for this Model.
 */
LIBSBML_EXTERN
ListOf_t *
Model_getListOfParameters (Model_t *m)
{
  return (ListOf_t *) &
         static_cast<Model*>(m)->getListOfParameters();
}


/**
 * @return the list of Rules for this Model.
 */
LIBSBML_EXTERN
ListOf_t *
Model_getListOfRules (Model_t *m)
{
  return (ListOf_t *) &
         static_cast<Model*>(m)->getListOfRules();
}


/**
 * @return the list of Rules for this Model.
 */
LIBSBML_EXTERN
ListOf_t *
Model_getListOfReactions (Model_t *m)
{
  return (ListOf_t *) &
         static_cast<Model*>(m)->getListOfReactions();
}


/**
 * @return the list of Rules for this Model.
 */
LIBSBML_EXTERN
ListOf_t *
Model_getListOfEvents (Model_t *m)
{
  return (ListOf_t *) &
         static_cast<Model*>(m)->getListOfEvents();
}


/**
 * @return the list of items of the given type for this Model.  If the
 * given SBMLTypeCode does not correspond to a ListOf contained in SBML
 * Models, NULL is returned.
 */
ListOf_t *
Model_getListOfByTypecode (Model_t *m, SBMLTypeCode_t type)
{
  return (ListOf_t *)
         static_cast<Model*>(m)->getListOfByTypecode(type);
}


/**
 * @return the nth FunctionDefinition of this Model.
 */
LIBSBML_EXTERN
FunctionDefinition_t *
Model_getFunctionDefinition (const Model_t *m, unsigned int n)
{
  return static_cast<const Model*>(m)->getFunctionDefinition(n);
}


/**
 * @return the FunctionDefinition in this Model with the given id or NULL
 * if no such FunctionDefinition exists.
 */
LIBSBML_EXTERN
FunctionDefinition_t *
Model_getFunctionDefinitionById (const Model_t *m, const char *sid)
{
  return static_cast<const Model*>(m)->getFunctionDefinition(sid ? sid : "");
}


/**
 * @return the nth UnitDefinition of this Model.
 */
LIBSBML_EXTERN
UnitDefinition_t *
Model_getUnitDefinition (const Model_t *m, unsigned int n)
{
  return static_cast<const Model*>(m)->getUnitDefinition(n);
}


/**
 * @return the UnitDefinition in this Model with the given id or NULL if no
 * such UnitDefinition exists.
 */
LIBSBML_EXTERN
UnitDefinition_t *
Model_getUnitDefinitionById (const Model_t *m, const char *sid)
{
  return static_cast<const Model*>(m)->getUnitDefinition(sid ? sid : "");
}


/**
 * @return the nth Compartment of this Model.
 */
LIBSBML_EXTERN
Compartment_t *
Model_getCompartment (const Model_t *m, unsigned int n)
{
  return static_cast<const Model*>(m)->getCompartment(n);
}


/**
 * @return the Compartment in this Model with the given id or NULL if no
 * such Compartment exists.
 */
LIBSBML_EXTERN
Compartment_t *
Model_getCompartmentById (const Model_t *m, const char *sid)
{
  return static_cast<const Model*>(m)->getCompartment(sid ? sid : "");
}


/**
 * @return the nth Species of this Model.
 */
LIBSBML_EXTERN
Species_t *
Model_getSpecies (const Model_t *m, unsigned int n)
{
  return static_cast<const Model*>(m)->getSpecies(n);
}


/**
 * @return the Species in this Model with the given id or NULL if no such
 * Species exists.
 */
LIBSBML_EXTERN
Species_t *
Model_getSpeciesById (const Model_t *m, const char *sid)
{
  return static_cast<const Model*>(m)->getSpecies(sid ? sid : "");
}


/**
 * @return the nth Parameter of this Model.
 */
LIBSBML_EXTERN
Parameter_t *
Model_getParameter (const Model_t *m, unsigned int n)
{
  return static_cast<const Model*>(m)->getParameter(n);
}


/**
 * @return the Parameter in this Model with the given id or NULL if no such
 * Parameter exists.
 */
LIBSBML_EXTERN
Parameter_t *
Model_getParameterById (const Model_t *m, const char *sid)
{
  return static_cast<const Model*>(m)->getParameter(sid ? sid : "");
}


/**
 * @return the nth Rule of this Model.
 */
LIBSBML_EXTERN
Rule_t *
Model_getRule (const Model_t *m, unsigned int n)
{
  return static_cast<const Model*>(m)->getRule(n);
}


/**
 * @return the nth Reaction of this Model.
 */
LIBSBML_EXTERN
Reaction_t *
Model_getReaction (const Model_t *m, unsigned int n)
{
  return static_cast<const Model*>(m)->getReaction(n);
}


/**
 * @return the Reaction in this Model with the given id or NULL if no such
 * Reaction exists.
 */
LIBSBML_EXTERN
Reaction_t *
Model_getReactionById (const Model_t *m, const char *sid)
{
  return static_cast<const Model*>(m)->getReaction(sid ? sid : "");
}


/**
 * @return the nth Event of this Model.
 */
LIBSBML_EXTERN
Event_t *
Model_getEvent (const Model_t *m, unsigned int n)
{
  return static_cast<const Model*>(m)->getEvent(n);
}


/**
 * @return the Event in this Model with the given id or NULL if no such
 * Event exists.
 */
LIBSBML_EXTERN
Event_t *
Model_getEventById (const Model_t *m, const char *sid)
{
  return static_cast<const Model*>(m)->getEvent(sid ? sid : "");
}


/**
 * @return the number of FunctionDefinitions in this Model.
 */
LIBSBML_EXTERN
unsigned int
Model_getNumFunctionDefinitions (const Model_t *m)
{
  return static_cast<const Model*>(m)->getNumFunctionDefinitions();
}


/**
 * @return the number of UnitDefinitions in this Model.
 */
LIBSBML_EXTERN
unsigned int
Model_getNumUnitDefinitions (const Model_t *m)
{
  return static_cast<const Model*>(m)->getNumUnitDefinitions();
}


/**
 * @return the number of Compartments in this Model.
 */
LIBSBML_EXTERN
unsigned int
Model_getNumCompartments (const Model_t *m)
{
  return static_cast<const Model*>(m)->getNumCompartments();
}


/**
 * @return the number of Species in this Model.
 */
LIBSBML_EXTERN
unsigned int
Model_getNumSpecies (const Model_t *m)
{
  return static_cast<const Model*>(m)->getNumSpecies();
}


/**
 * @return the number of Species in this Model with boundaryCondition set
 * to true.
 */
LIBSBML_EXTERN
unsigned int
Model_getNumSpeciesWithBoundaryCondition (const Model_t *m)
{
  return static_cast<const Model*>(m)->getNumSpeciesWithBoundaryCondition();
}


/**
 * @return the number of Parameters in this Model.  Parameters defined in
 * KineticLaws are not included.
 */
LIBSBML_EXTERN
unsigned int
Model_getNumParameters (const Model_t *m)
{
  return static_cast<const Model*>(m)->getNumParameters();
}


/**
 * @return the number of Rules in this Model.
 */
LIBSBML_EXTERN
unsigned int
Model_getNumRules (const Model_t *m)
{
  return static_cast<const Model*>(m)->getNumRules();
}


/**
 * @return the number of Reactions in this Model.
 */
LIBSBML_EXTERN
unsigned int
Model_getNumReactions (const Model_t *m)
{
  return static_cast<const Model*>(m)->getNumReactions();
}


/**
 * @return the number of Events in this Model.
 */
LIBSBML_EXTERN
unsigned int
Model_getNumEvents (const Model_t *m)
{
  return static_cast<const Model*>(m)->getNumEvents();
}



#ifdef USE_LAYOUT  


/**
 * Returns a reference to the ListOf object that holds the layouts.
 */
LIBSBML_EXTERN
ListOf_t *
Model_getListOfLayouts (Model_t *m)
{
  return & m->getListOfLayouts();
}


/**
 * Returns the layout object that belongs to the given index. If the index
 * is invalid, NULL is returned.
 */
LIBSBML_EXTERN
Layout_t *
Model_getLayout (Model_t *m, unsigned int index)
{
  return m->getLayout(index);
}


/**
 * Adds a copy of the layout object to the list of layouts.
 */ 
LIBSBML_EXTERN
void 
Model_addLayout (Model_t *m, Layout_t *layout)
{
  m->addLayout(*layout);
}


/**
 * Creates a new layout object and adds it to the list of layout objects.
 * A pointer to the newly created object is returned.
 */
LIBSBML_EXTERN
Layout_t *
Model_createLayout (Model_t *m)
{
	return & m->createLayout();
}


#endif  /* USE_LAYOUT */
