/**
 * \file    Model.cpp
 * \brief   SBML Model
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2002 California Institute of Technology and Japan Science and
 * Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is
 * provided in the file named "LICENSE.txt" included with this software
 * distribution.  It is also available online at
 * http://sbml.org/software/libsbml/license.html
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 */


#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>

#include <sbml/math/ASTNode.h>

#include <sbml/SBMLDocument.h>

#include "SBML.h"
#include "SBMLVisitor.h"


#include "KineticLaw.h"
#include "Model.h"


using namespace std;


/**
 * Creates a new Model, optionally with its id and name attributes set.
 */
Model::Model (const string& id, const string& name) :
   SBase   ( id, name )
 , mSBOTerm( -1       )
{
}


/**
 * Destroys this Model.
 */
Model::~Model ()
{
}


/**
 * Accepts the given SBMLVisitor.
 */
bool
Model::accept (SBMLVisitor& v) const
{
  v.visit(*this);

  mFunctionDefinitions.accept(v);
  mUnitDefinitions    .accept(v);
  mCompartmentTypes   .accept(v);
  mSpeciesTypes       .accept(v);
  mCompartments       .accept(v);
  mSpecies            .accept(v);
  mParameters         .accept(v);
  mInitialAssignments .accept(v);
  mRules              .accept(v);
  mConstraints        .accept(v);
  mReactions          .accept(v);
  mEvents             .accept(v);

  v.leave(*this);

  return true;
}


/**
 * @return a (deep) copy of this Model.
 */
SBase*
Model::clone () const
{
  return new Model(*this);
}


/**
 * @return the sboTerm of this KineticLaw as an integer.  If not set,
 * sboTerm will be -1.  Use SBML::sboTermToString() to convert the
 * sboTerm to a zero-padded, seven digit string.
 */
int
Model::getSBOTerm () const
{
  return mSBOTerm;
}


/**
 * @return true if the sboTerm of this KineticLaw has been set, false
 * otherwise.
 */
bool
Model::isSetSBOTerm () const
{
  return (mSBOTerm != -1);
}


/**
 * Sets the sboTerm field of this KineticLaw to value.
 */
void
Model::setSBOTerm (int sboTerm)
{
  mSBOTerm = sboTerm;
}


/**
 * Unsets the sboTerm of this KineticLaw.
 */
void
Model::unsetSBOTerm ()
{
  mSBOTerm = -1;
}


/**
 * Adds a copy of the given FunctionDefinition to this Model.
 */
void
Model::addFunctionDefinition (const FunctionDefinition* fd)
{
  mFunctionDefinitions.append(fd);
}


/**
 * Adds a copy of the given UnitDefinition to this Model.
 */
void
Model::addUnitDefinition (const UnitDefinition* ud)
{
  mUnitDefinitions.append(ud);
}


/**
 * Adds a copy of the given CompartmentType to this Model.
 */
void
Model::addCompartmentType (const CompartmentType* ct)
{
  mCompartmentTypes.append(ct);
}


/**
 * Adds a copy of the given SpeciesType to this Model.
 */
void
Model::addSpeciesType (const SpeciesType* st)
{
  mSpeciesTypes.append(st);
}


/**
 * Adds a copy of the given Compartment to this Model.
 */
void
Model::addCompartment (const Compartment* c)
{
  mCompartments.append(c);
}


/**
 * Adds a copy of the given Species to this Model.
 */
void
Model::addSpecies (const Species* s)
{
  mSpecies.append(s);
}


/**
 * Adds a copy of the given Parameter to this Model.
 */
void
Model::addParameter (const Parameter* p)
{
  mParameters.append(p);
}


/**
 * Adds a copy of the given InitialAssignment to this Model.
 */
void
Model::addInitialAssignment (const InitialAssignment* ia)
{
  mInitialAssignments.append(ia);
}


/**
 * Adds a copy of the given Rule to this Model.
 */
void
Model::addRule (const Rule* r)
{
  mRules.append(r);
}


/**
 * Adds a copy of the given Constraint to this Model.
 */
void
Model::addConstraint (const Constraint* c)
{
  mConstraints.append(c);
}


/**
 * Adds a copy of the given Reaction to this Model.
 */
void
Model::addReaction (const Reaction* r)
{
  mReactions.append(r);
}


/**
 * Adds a copy of the given Event to this Model.
 */
void
Model::addEvent (const Event* e)
{
  mEvents.append(e);
}


/**
 * Creates a new FunctionDefinition inside this Model and returns it.
 */
FunctionDefinition*
Model::createFunctionDefinition ()
{
  FunctionDefinition* fd = new FunctionDefinition;
  mFunctionDefinitions.appendAndOwn(fd);

  return fd;
}


/**
 * Creates a new UnitDefinition inside this Model and returns it.
 */
UnitDefinition*
Model::createUnitDefinition ()
{
  UnitDefinition* ud = new UnitDefinition;
  mUnitDefinitions.appendAndOwn(ud);

  return ud;
}


/**
 * Creates a new Unit inside this Model and returns a pointer to it.  The
 * Unit is added to the last UnitDefinition created.
 *
 * If a UnitDefinitions does not exist for this model, a new Unit is not
 * created and NULL is returned.
 */
Unit*
Model::createUnit ()
{
  unsigned int size = getNumUnitDefinitions();
  return (size > 0) ? getUnitDefinition(size - 1)->createUnit() : 0;
}


/**
 * Creates a new CompartmentType inside this Model and returns it.
 */
CompartmentType*
Model::createCompartmentType ()
{
  CompartmentType* ct = new CompartmentType;
  mCompartmentTypes.appendAndOwn(ct);

  return ct;
}


/**
 * Creates a new SpeciesType inside this Model and returns it.
 */
SpeciesType*
Model::createSpeciesType ()
{
  SpeciesType* st = new SpeciesType;
  mSpeciesTypes.appendAndOwn(st);

  return st;
}


/**
 * Creates a new Compartment inside this Model and returns it.
 */
Compartment*
Model::createCompartment ()
{
  Compartment* c = new Compartment;
  mCompartments.appendAndOwn(c);

  return c;
}


/**
 * Creates a new Species inside this Model and returns it.
 */
Species*
Model::createSpecies ()
{
  Species* s = new Species;
  mSpecies.appendAndOwn(s);

  return s;
}


/**
 * Creates a new Parameter inside this Model and returns.
 */
Parameter*
Model::createParameter ()
{
  Parameter* p = new Parameter;
  mParameters.appendAndOwn(p);

  return p;
}


/**
 * Creates a new InitialAssignment inside this Model and returns it.
 */
InitialAssignment*
Model::createInitialAssignment ()
{
  InitialAssignment* ia = new InitialAssignment;
  mInitialAssignments.appendAndOwn(ia);

  return ia;
}


/**
 * Creates a new AlgebraicRule inside this Model and returns it.
 */
AlgebraicRule*
Model::createAlgebraicRule ()
{
  AlgebraicRule* ar = new AlgebraicRule;
  mRules.appendAndOwn(ar);

  return ar;
}


/**
 * Creates a new AssignmentRule inside this Model and returns it.
 */
AssignmentRule*
Model::createAssignmentRule ()
{
  AssignmentRule* ar = new AssignmentRule;
  mRules.appendAndOwn(ar);

  return ar;
}


/**
 * Creates a new RateRule inside this Model and returns it.
 */
RateRule*
Model::createRateRule ()
{
  RateRule* rr = new RateRule();
  mRules.appendAndOwn(rr);

  return rr;
}


/**
 * Creates a new Constraint inside this Model and returns it.
 */
Constraint*
Model::createConstraint ()
{
  Constraint* c = new Constraint;
  mConstraints.appendAndOwn(c);

  return c;
}


/**
 * Creates a new Reaction inside this Model and returns it.
 */
Reaction*
Model::createReaction ()
{
  Reaction* r = new Reaction;
  mReactions.appendAndOwn(r);

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
SpeciesReference*
Model::createReactant ()
{
  unsigned int size = getNumReactions();
  return (size > 0) ? getReaction(size - 1)->createReactant() : 0;
}


/**
 * Creates a new Product (i.e. SpeciesReference) inside this Model and
 * returns a pointer to it.  The SpeciesReference is added to the products
 * of the last Reaction created.
 *
 * If a Reaction does not exist for this model, a new SpeciesReference is
 * not created and NULL is returned.
 */
SpeciesReference*
Model::createProduct ()
{
  unsigned int size = getNumReactions();
  return (size > 0) ? getReaction(size - 1)->createProduct() : 0;
}


/**
 * Creates a new Modifer (i.e. ModifierSpeciesReference) inside this Model
 * and returns a pointer to it.  The ModifierSpeciesReference is added to
 * the modifiers of the last Reaction created.
 *
 * If a Reaction does not exist for this model, a new
 * ModifierSpeciesReference is not created and NULL is returned.
 */
ModifierSpeciesReference*
Model::createModifier ()
{
  unsigned int size = getNumReactions();
  return (size > 0) ? getReaction(size - 1)->createModifier() : 0;
}


/**
 * Creates a new KineticLaw inside this Model and returns a pointer to it.
 * The KineticLaw is associated with the last Reaction created.
 *
 * If a Reaction does not exist for this model, or a Reaction does exist,
 * but already has a KineticLaw, a new KineticLaw is not created and NULL
 * is returned.
 */
KineticLaw*
Model::createKineticLaw ()
{
  unsigned int size = getNumReactions();
  return (size > 0) ? getReaction(size - 1)->createKineticLaw() : 0;
}


/**
 * Creates a new Parameter (of a KineticLaw) inside this Model and returns
 * a pointer to it.  The Parameter is associated with the KineticLaw of the
 * last Reaction created.
 *
 * If a Reaction does not exist for this model, or a KineticLaw for the
 * Reaction, a new Parameter is not created and NULL is returned.
 */
Parameter*
Model::createKineticLawParameter ()
{
  unsigned int size = getNumReactions();

  if (size > 0)
  {
    KineticLaw* kl = getReaction(size - 1)->getKineticLaw();
    if (kl) return kl->createParameter();
  }

  return 0;
}


/**
 * Creates a new Event inside this Model and returns it.
 */
Event*
Model::createEvent ()
{
  Event* e = new Event;
  mEvents.appendAndOwn(e);

  return e;
}


/**
 * Creates a new EventAssignment inside this Model and returns a pointer to
 * it.  The EventAssignment is added to the the last Event created.
 *
 * If an Event does not exist for this model, a new EventAssignment is not
 * created and NULL is returned.
 */
EventAssignment*
Model::createEventAssignment ()
{
  unsigned int size = getNumEvents();
  return (size > 0) ? getEvent(size - 1)->createEventAssignment() : 0;
}


/**
 * @return the list of FunctionDefinitions for this Model.
 */
const ListOfFunctionDefinitions*
Model::getListOfFunctionDefinitions () const
{
  return &mFunctionDefinitions;
}


/**
 * @return the list of FunctionDefinitions for this Model.
 */
ListOfFunctionDefinitions*
Model::getListOfFunctionDefinitions ()
{
  return &mFunctionDefinitions;
}


/**
 * @return the list of UnitDefinitions for this Model.
 */
const ListOfUnitDefinitions*
Model::getListOfUnitDefinitions () const
{
  return &mUnitDefinitions;
}


/**
 * @return the list of UnitDefinitions for this Model.
 */
ListOfUnitDefinitions*
Model::getListOfUnitDefinitions ()
{
  return &mUnitDefinitions;
}


/**
 * @return the list of CompartmentTypes for this Model.
 */
const ListOfCompartmentTypes*
Model::getListOfCompartmentTypes () const
{
  return &mCompartmentTypes;
}


/**
 * @return the list of CompartmentTypes for this Model.
 */
ListOfCompartmentTypes*
Model::getListOfCompartmentTypes ()
{
  return &mCompartmentTypes;
}


/**
 * @return the list of SpeciesTypes for this Model.
 */
const ListOfSpeciesTypes*
Model::getListOfSpeciesTypes () const
{
  return &mSpeciesTypes;
}

 
/**
 * @return the list of SpeciesTypes for this Model.
 */
ListOfSpeciesTypes*
Model::getListOfSpeciesTypes ()
{
  return &mSpeciesTypes;
}


/**
 * @return the list of Compartments for this Model.
 */
const ListOfCompartments*
Model::getListOfCompartments () const
{
  return &mCompartments;
}


/**
 * @return the list of Compartments for this Model.
 */
ListOfCompartments*
Model::getListOfCompartments ()
{
  return &mCompartments;
}


/**
 * @return the list of Species for this Model.
 */
const ListOfSpecies*
Model::getListOfSpecies () const
{
  return &mSpecies;
}


/**
 * @return the list of Species for this Model.
 */
ListOfSpecies*
Model::getListOfSpecies ()
{
  return &mSpecies;
}


/**
 * @return the list of Parameters for this Model.
 */
const ListOfParameters*
Model::getListOfParameters () const
{
  return &mParameters;
}


/**
 * @return the list of Parameters for this Model.
 */
ListOfParameters*
Model::getListOfParameters ()
{
  return &mParameters;
}


/**
 * @return the list of InitialAssignments for this Model.
 */
const ListOfInitialAssignments*
Model::getListOfInitialAssignments () const
{
  return &mInitialAssignments;
}


/**
 * @return the list of InitialAssignment for this Model.
 */
ListOfInitialAssignments*
Model::getListOfInitialAssignments ()
{
  return &mInitialAssignments;
}


/**
 * @return the list of Rules for this Model.
 */
const ListOfRules*
Model::getListOfRules () const
{
  return &mRules;
}


/**
 * @return the list of Rules for this Model.
 */
ListOfRules*
Model::getListOfRules ()
{
  return &mRules;
}


/**
 * @return the list of Constraints for this Model.
 */
const ListOfConstraints*
Model::getListOfConstraints () const
{
  return &mConstraints;
}

 
/**
 * @return the list of Constraints for this Model.
 */
ListOfConstraints*
Model::getListOfConstraints ()
{
  return &mConstraints;
}


/**
 * @return the list of Reactions for this Model.
 */
const ListOfReactions*
Model::getListOfReactions () const
{
  return &mReactions;
}


/**
 * @return the list of Reactions for this Model.
 */
ListOfReactions*
Model::getListOfReactions ()
{
  return &mReactions;
}


/**
 * @return the list of Events for this Model.
 */
const ListOfEvents*
Model::getListOfEvents () const
{
  return &mEvents;
}


/**
 * @return the list of Events for this Model.
 */
ListOfEvents*
Model::getListOfEvents ()
{
  return &mEvents;
}


/**
 * @return the nth FunctionDefinition of this Model.
 */
const FunctionDefinition*
Model::getFunctionDefinition (unsigned int n) const
{
  return static_cast<const FunctionDefinition*>( mFunctionDefinitions.get(n) );
}


/**
 * @return the nth FunctionDefinition of this Model.
 */
FunctionDefinition*
Model::getFunctionDefinition (unsigned int n)
{
  return static_cast<FunctionDefinition*>( mFunctionDefinitions.get(n) );
}


/**
 * @return the FunctionDefinition in this Model with the given id or NULL
 * if no such FunctionDefinition exists.
 */
const FunctionDefinition*
Model::getFunctionDefinition (const string& sid) const
{
  return static_cast<const FunctionDefinition*>(mFunctionDefinitions.get(sid));
}


/**
 * @return the FunctionDefinition in this Model with the given id or NULL
 * if no such FunctionDefinition exists.
 */
FunctionDefinition*
Model::getFunctionDefinition (const string& sid)
{
  return static_cast<FunctionDefinition*>( mFunctionDefinitions.get(sid) );
}


/**
 * @return the nth UnitDefinition of this Model.
 */
const UnitDefinition*
Model::getUnitDefinition (unsigned int n) const
{
  return static_cast<const UnitDefinition*>( mUnitDefinitions.get(n) );
}


/**
 * @return the nth UnitDefinition of this Model.
 */
UnitDefinition*
Model::getUnitDefinition (unsigned int n)
{
  return static_cast<UnitDefinition*>( mUnitDefinitions.get(n) );
}


/**
 * @return the UnitDefinition in this Model with the given id or NULL if no
 * such UnitDefinition exists.
 */
const UnitDefinition*
Model::getUnitDefinition (const string& sid) const
{
  return static_cast<const UnitDefinition*>( mUnitDefinitions.get(sid) );
}


/**
 * @return the UnitDefinition in this Model with the given id or NULL if no
 * such UnitDefinition exists.
 */
UnitDefinition*
Model::getUnitDefinition (const string& sid)
{
  return static_cast<UnitDefinition*>( mUnitDefinitions.get(sid) );
}


/**
 * @return the nth CompartmentType of this Model.
 */
const CompartmentType*
Model::getCompartmentType (unsigned int n) const
{
  return static_cast<const CompartmentType*>( mCompartmentTypes.get(n) );
}


/**
 * @return the nth CompartmentType of this Model.
 */
CompartmentType*
Model::getCompartmentType (unsigned int n)
{
  return static_cast<CompartmentType*>( mCompartmentTypes.get(n) );
}


/**
 * @return the CompartmentType in this Model with the given id or NULL if
 * no such CompartmentType exists.
 */
const CompartmentType*
Model::getCompartmentType (const string& sid) const
{
  return static_cast<const CompartmentType*>( mCompartmentTypes.get(sid) );
}


/**
 * @return the CompartmentType in this Model with the given id or NULL if
 * no such CompartmentType exists.
 */
CompartmentType*
Model::getCompartmentType (const string& sid)
{
  return static_cast<CompartmentType*>( mCompartmentTypes.get(sid) );
}


/**
 * @return the nth SpeciesType of this Model.
 */
const SpeciesType*
Model::getSpeciesType (unsigned int n) const
{
  return static_cast<const SpeciesType*>( mSpeciesTypes.get(n) );
}


/**
 * @return the nth SpeciesType of this Model.
 */
SpeciesType*
Model::getSpeciesType (unsigned int n)
{
  return static_cast<SpeciesType*>( mSpeciesTypes.get(n) );
}


/**
 * @return the SpeciesType in this Model with the given id or NULL if
 * no such SpeciesType exists.
 */
const SpeciesType*
Model::getSpeciesType (const string& sid) const
{
  return static_cast<const SpeciesType*>( mSpeciesTypes.get(sid) );
}


/**
 * @return the SpeciesType in this Model with the given id or NULL if
 * no such SpeciesType exists.
 */
SpeciesType*
Model::getSpeciesType (const string& sid)
{
  return static_cast<SpeciesType*>( mSpeciesTypes.get(sid) );
}


/**
 * @return the nth Compartment of this Model.
 */
const Compartment*
Model::getCompartment (unsigned int n) const
{
  return static_cast<const Compartment*>( mCompartments.get(n) );
}


/**
 * @return the nth Compartment of this Model.
 */
Compartment*
Model::getCompartment (unsigned int n)
{
  return static_cast<Compartment*>( mCompartments.get(n) );
}


/**
 * @return the Compartment in this Model with the given id or NULL if no
 * such Compartment exists.
 */
const Compartment*
Model::getCompartment (const string& sid) const
{
  return static_cast<const Compartment*>( mCompartments.get(sid) );
}


/**
 * @return the Compartment in this Model with the given id or NULL if no
 * such Compartment exists.
 */
Compartment*
Model::getCompartment (const string& sid)
{
  return static_cast<Compartment*>( mCompartments.get(sid) );
}


/**
 * @return the nth Species of this Model.
 */
const Species*
Model::getSpecies (unsigned int n) const
{
  return static_cast<const Species*>( mSpecies.get(n) );
}


/**
 * @return the nth Species of this Model.
 */
Species*
Model::getSpecies (unsigned int n)
{
  return static_cast<Species*>( mSpecies.get(n) );
}


/**
 * @return the Species in this Model with the given id or NULL if no such
 * Species exists.
 */
const Species*
Model::getSpecies (const string& sid) const
{
  return static_cast<const Species*>( mSpecies.get(sid) );
}


/**
 * @return the Species in this Model with the given id or NULL if no such
 * Species exists.
 */
Species*
Model::getSpecies (const string& sid)
{
  return static_cast<Species*>( mSpecies.get(sid) );
}


/**
 * @return the nth Parameter of this Model.
 */
const Parameter*
Model::getParameter (unsigned int n) const
{
  return static_cast<const Parameter*>( mParameters.get(n) );
}


/**
 * @return the nth Parameter of this Model.
 */
Parameter*
Model::getParameter (unsigned int n)
{
  return static_cast<Parameter*>( mParameters.get(n) );
}


/**
 * @return the Parameter in this Model with the given id or NULL if no such
 * Parameter exists.
 */
const Parameter*
Model::getParameter (const string& sid) const
{
  return static_cast<const Parameter*>( mParameters.get(sid) );
}


/**
 * @return the Parameter in this Model with the given id or NULL if no such
 * Parameter exists.
 */
Parameter*
Model::getParameter (const string& sid)
{
  return static_cast<Parameter*>( mParameters.get(sid) );
}


/**
 * @return the nth InitialAssignment of this Model.
 */
const InitialAssignment*
Model::getInitialAssignment (unsigned int n) const
{
  return static_cast<const InitialAssignment*>( mInitialAssignments.get(n) );
}


/**
 * @return the nth InitialAssignment of this Model.
 */
InitialAssignment*
Model::getInitialAssignment (unsigned int n)
{
  return static_cast<InitialAssignment*>( mInitialAssignments.get(n) );
}


/**
 * @return the InitialAssignment in this Model with the given symbol or
 * NULL if no such InitialAssignment exists.
 */
const InitialAssignment*
Model::getInitialAssignment (const string& symbol) const
{
  return static_cast<const InitialAssignment*>
  (
    mInitialAssignments.get(symbol)
  );
}

 
/**
 * @return the InitialAssignment in this Model with the given symbol or
 * NULL if no such InitialAssignment exists.
 */
InitialAssignment*
Model::getInitialAssignment (const string& symbol)
{
  return static_cast<InitialAssignment*>( mInitialAssignments.get(symbol) );
}


/**
 * @return the nth Rule of this Model.
 */
const Rule*
Model::getRule (unsigned int n) const
{
  return static_cast<const Rule*>( mRules.get(n) );
}


/**
 * @return the nth Rule of this Model.
 */
Rule*
Model::getRule (unsigned int n)
{
  return static_cast<Rule*>( mRules.get(n) );
}


/**
 * @return the Rule in this Model with the given variable or NULL if no
 * such Rule exists.
 */
const Rule*
Model::getRule (const string& variable) const
{
  return static_cast<const Rule*>( mRules.get(variable) );
}

 
/**
 * @return the Rule in this Model with the given symbol or NULL if no
 * such Rule exists.
 */
Rule*
Model::getRule (const string& variable)
{
  return static_cast<Rule*>( mRules.get(variable) );
}


/**
 * @return the nth Constraint of this Model.
 */
const Constraint*
Model::getConstraint (unsigned int n) const
{
  return static_cast<const Constraint*>( mConstraints.get(n) );
}


/**
 * @return the nth Constraint of this Model.
 */
Constraint*
Model::getConstraint (unsigned int n)
{
  return static_cast<Constraint*>( mConstraints.get(n) );
}


/**
 * @return the nth Reaction of this Model.
 */
const Reaction*
Model::getReaction (unsigned int n) const
{
  return static_cast<const Reaction*>( mReactions.get(n) );
}


/**
 * @return the nth Reaction of this Model.
 */
Reaction*
Model::getReaction (unsigned int n)
{
  return static_cast<Reaction*>( mReactions.get(n) );
}


/**
 * @return the Reaction in this Model with the given id or NULL if no such
 * Reaction exists.
 */
const Reaction*
Model::getReaction (const string& sid) const
{
  return static_cast<const Reaction*>( mReactions.get(sid) );
}


/**
 * @return the Reaction in this Model with the given id or NULL if no such
 * Reaction exists.
 */
Reaction*
Model::getReaction (const string& sid)
{
  return static_cast<Reaction*>( mReactions.get(sid) );
}


/**
 * @return the nth Event of this Model.
 */
const Event*
Model::getEvent (unsigned int n) const
{
  return static_cast<const Event*>( mEvents.get(n) );
}


/**
 * @return the nth Event of this Model.
 */
Event*
Model::getEvent (unsigned int n)
{
  return static_cast<Event*>( mEvents.get(n) );
}


/**
 * @return the Event in this Model with the given id or NULL if no such
 * Event exists.
 */
const Event*
Model::getEvent (const string& sid) const
{
  return static_cast<const Event*>( mEvents.get(sid) );
}


/**
 * @return the Event in this Model with the given id or NULL if no such
 * Event exists.
 */
Event*
Model::getEvent (const string& sid)
{
  return static_cast<Event*>( mEvents.get(sid) );
}


/**
 * @return the number of FunctionDefinitions in this Model.
 */
unsigned int
Model::getNumFunctionDefinitions () const
{
  return mFunctionDefinitions.size();
}


/**
 * @return the number of UnitDefinitions in this Model.
 */
unsigned int
Model::getNumUnitDefinitions () const
{
  return mUnitDefinitions.size();
}


/**
 * @return the number of CompartmentTypes in this Model.
 */
unsigned int
Model::getNumCompartmentTypes () const
{
  return mCompartmentTypes.size();
}


/**
 * @return the number of SpeciesTypes in this Model.
 */
unsigned int
Model::getNumSpeciesTypes () const
{
  return mSpeciesTypes.size();
}


/**
 * @return the number of Compartments in this Model.
 */
unsigned int
Model::getNumCompartments () const
{
  return mCompartments.size();
}


/**
 * @return the number of Species in this Model.
 */
unsigned int
Model::getNumSpecies () const
{
  return mSpecies.size();
}


/**
 * @return the number of Species in this Model with boundaryCondition set
 * to true.
 */
unsigned int
Model::getNumSpeciesWithBoundaryCondition () const // FIXME
{
  return 0;
  //return species.countIf( (ListItemPredicate) Species_getBoundaryCondition );
}


/**
 * @return the number of Parameters in this Model.  Parameters defined in
 * KineticLaws are not included.
 */
unsigned int
Model::getNumParameters () const
{
  return mParameters.size();
}


/**
 * @return the number of InitialAssignments in this Model.
 */
unsigned int
Model::getNumInitialAssignments () const
{
  return mInitialAssignments.size();
}


/**
 * @return the number of Rules in this Model.
 */
unsigned int
Model::getNumRules () const
{
  return mRules.size();
}


/**
 * @return the number of Constraints in this Model.
 */
unsigned int
Model::getNumConstraints () const
{
  return mConstraints.size();
}


/**
 * @return the number of Reactions in this Model.
 */
unsigned int
Model::getNumReactions () const
{
  return mReactions.size();
}


/**
 * @return the number of Events in this Model.
 */
unsigned int
Model::getNumEvents () const
{
  return mEvents.size();
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
    const FunctionDefinition* fd = getFunctionDefinition( node->getName() );

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


/**
 * Sets the parent SBMLDocument of this SBML object.
 */
void
Model::setSBMLDocument (SBMLDocument* d)
{
  mSBML = d;

  mFunctionDefinitions.setSBMLDocument(d);
  mUnitDefinitions    .setSBMLDocument(d);
  mCompartmentTypes   .setSBMLDocument(d);
  mSpeciesTypes       .setSBMLDocument(d);
  mCompartments       .setSBMLDocument(d);
  mSpecies            .setSBMLDocument(d);
  mParameters         .setSBMLDocument(d);
  mInitialAssignments .setSBMLDocument(d);
  mRules              .setSBMLDocument(d);
  mConstraints        .setSBMLDocument(d);
  mReactions          .setSBMLDocument(d);
  mEvents             .setSBMLDocument(d);
}


/**
 * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
 * (default).
 *
 * @see getElementName()
 */
SBMLTypeCode_t
Model::getTypeCode () const
{
  return SBML_MODEL;
}


/**
 * Subclasses should override this method to return XML element name of
 * this SBML object.
 */
const string&
Model::getElementName () const
{
  static const string name = "model";
  return name;
}


/**
 * @return the SBML object corresponding to next XMLToken in the
 * XMLInputStream or NULL if the token was not recognized.
 */
SBase*
Model::createObject (XMLInputStream& stream)
{
  const string& name   = stream.peek().getName();
  SBase*        object = 0;

  if (name == "listOfFunctionDefinitions")
  {
    object = &mFunctionDefinitions;
  }

  else if ( name == "listOfUnitDefinitions"    ) object = &mUnitDefinitions;
  else if ( name == "listOfCompartmentTypes"   ) object = &mCompartmentTypes;
  else if ( name == "listOfSpeciesTypes"       ) object = &mSpeciesTypes;
  else if ( name == "listOfCompartments"       ) object = &mCompartments;
  else if ( name == "listOfSpecies"            ) object = &mSpecies;
  else if ( name == "listOfParameters"         ) object = &mParameters;
  else if ( name == "listOfInitialAssignments" ) object = &mInitialAssignments;
  else if ( name == "listOfRules"              ) object = &mRules;
  else if ( name == "listOfConstraints"        ) object = &mConstraints;
  else if ( name == "listOfReactions"          ) object = &mReactions;
  else if ( name == "listOfEvents"             ) object = &mEvents;

  else if ( getLevel() == 1 && getVersion() == 1 )
  {
    if (name == "listOfSpecie") object = &mSpecies;
  }
#ifdef USE_LAYOUT
  else if ( name == "listOfLayouts"             ) object = &mLayouts;
#endif /* USE_LAYOUT */  

  return object;
}


/**
 * Subclasses should override this method to read values from the given
 * XMLAttributes set into their specific fields.  Be sure to call your
 * parents implementation of this method as well.
 */
void
Model::readAttributes (const XMLAttributes& attributes)
{
  SBase::readAttributes(attributes);

  const unsigned int level   = getLevel  ();
  const unsigned int version = getVersion();

  //
  // name: SName  { use="optional" }  (L1v1, L1v2)
  //   id: SId    { use="optional" }  (L2v1, L2v2)
  //
  const string id = (level == 1) ? "name" : "id";
  attributes.readInto(id, mId);

  //
  // name: string  { use="optional" }  (L2v1, L2v2)
  //
  if (level == 2) attributes.readInto("name", mName);

  //
  // sboTerm: SBOTerm { use="optional" }  (L2v2)
  //
  if (level == 2 && version == 2) mSBOTerm = SBML::readSBOTerm(attributes, this->getErrorLog());

}


/**
 * Subclasses should override this method to write their XML attributes
 * to the XMLOutputStream.  Be sure to call your parents implementation
 * of this method as well.
 */
void
Model::writeAttributes (XMLOutputStream& stream) const
{
  SBase::writeAttributes(stream);

  const unsigned int level   = getLevel  ();
  const unsigned int version = getVersion();

  //
  // name: SName   { use="required" }  (L1v1, L1v2)
  //   id: SId     { use="required" }  (L2v1, L2v2)
  //
  const string id = (level == 1) ? "name" : "id";
  stream.writeAttribute(id, mId);

  //
  // name: string  { use="optional" }  (L2v1, L2v2)
  //
  if (level == 2) stream.writeAttribute("name", mName);

  //
  // sboTerm: SBOTerm { use="optional" }  (L2v2)
  //
  if (level == 2 && version == 2) SBML::writeSBOTerm(stream, mSBOTerm);
}


/**
 * Subclasses should override this method to write out their contained
 * SBML objects as XML elements.  Be sure to call your parents
 * implementation of this method as well.
 */
void
Model::writeElements (XMLOutputStream& stream) const
{
  SBase::writeElements(stream);

  const unsigned int level   = getLevel  ();
  const unsigned int version = getVersion();

  if (level == 2 && getNumFunctionDefinitions() > 0)
  {
    mFunctionDefinitions.write(stream);
  }

  if ( getNumUnitDefinitions() > 0 ) mUnitDefinitions.write(stream);

  if (level == 2 && version == 2)
  {
    if ( getNumCompartmentTypes() > 0 ) mCompartmentTypes.write(stream);
    if ( getNumSpeciesTypes    () > 0 ) mSpeciesTypes    .write(stream);
  }

  if ( getNumCompartments() > 0 ) mCompartments.write(stream);
  if ( getNumSpecies     () > 0 ) mSpecies     .write(stream);
  if ( getNumParameters  () > 0 ) mParameters  .write(stream);

  if (level == 2 && version == 2)
  {
    if ( getNumInitialAssignments() > 0 ) mInitialAssignments.write(stream);
  }

  if ( getNumRules() > 0 ) mRules.write(stream);

  if (level == 2 && version == 2)
  {
    if ( getNumConstraints() > 0 ) mConstraints.write(stream);
  }

  if ( getNumReactions() > 0 ) mReactions.write(stream);

  if (level == 2 && getNumEvents () > 0 )
  {
    mEvents.write(stream);
  }
}


/**
 * returns expected position of compartment in a model
 */
int
Model::getElementPosition()
{
  return -1;
}


#ifdef USE_LAYOUT


/**
 * Returns the ListOf Layouts for this Model.
 */
const ListOfLayouts*
Model::getListOfLayouts () const
{
  return &this->mLayouts;
}


/**
 * Returns the ListOf Layouts for this Model.
 */
ListOfLayouts*
Model::getListOfLayouts ()
{
  return &this->mLayouts;
}


/**
 * Returns the layout object that belongs to the given index. If the index
 * is invalid, NULL is returned.
 */
const Layout*
Model::getLayout (unsigned int index) const
{
  return static_cast<const Layout*>( mLayouts.get(index) );
}


/**
 * Returns the layout object that belongs to the given index. If the index
 * is invalid, NULL is returned.
 */
Layout*
Model::getLayout (unsigned int index)
{
  return static_cast<Layout*>( mLayouts.get(index) );
}


/**
 * Adds a copy of the layout object to the list of layouts.
 */ 
void
Model::addLayout (const Layout* layout)
{
  mLayouts.append(layout);
}


/**
 * Creates a new layout object and adds it to the list of layout objects.
 * A reference to the newly created object is returned.
 */
Layout*
Model::createLayout ()
{
  Layout* l = new Layout();
  mLayouts.appendAndOwn(l);

  return l;
}


#endif  /* USE_LAYOUT */




/**
 * Creates a new Model and returns a pointer to it.
 */
LIBSBML_EXTERN
Model_t *
Model_create ()
{
  return new(nothrow) Model;
}


/**
 * Creates a new Model with the given id and returns a pointer to it.
 */
LIBSBML_EXTERN
Model_t *
Model_createWith (const char *sid)
{
  return new(nothrow) Model(sid ? sid : "");
}


/**
 * @return a (deep) copy of this Model.
 */
LIBSBML_EXTERN
Model_t *
Model_clone (const Model_t *m)
{
  return static_cast<Model*>( m->clone() );
}


/**
 * Frees the given Model.
 */
LIBSBML_EXTERN
void
Model_free (Model_t *m)
{
  delete m;
}


/**
 * @return the id of this Model.
 */
LIBSBML_EXTERN
const char *
Model_getId (const Model_t *m)
{
  return m->isSetId() ? m->getId().c_str() : NULL;
}


/**
 * @return the name of this Model.
 */
LIBSBML_EXTERN
const char *
Model_getName (const Model_t *m)
{
  return m->isSetName() ? m->getName().c_str() : NULL;
}


/**
 * @return the sboTerm of this Reaction as an integer.  If not set,
 * sboTerm will be -1.  Use SBML_sboTermToString() to convert the
 * sboTerm to a zero-padded, seven digit string.
 */
LIBSBML_EXTERN
int
Model_getSBOTerm (const Model_t *m)
{
  return m->getSBOTerm();
}


/**
 * @return true (non-zero) if the id of this Model has been set, false (0)
 * otherwise.
 */
LIBSBML_EXTERN
int
Model_isSetId (const Model_t *m)
{
  return static_cast<int>( m->isSetId() );
}


/**
 * @return true (non-zero) if the name of this Model has been set, false
 * (0) otherwise.
 */
LIBSBML_EXTERN
int
Model_isSetName (const Model_t *m)
{
  return static_cast<int>( m->isSetName() );
}


/**
 * @return true (non-zero) if the sboTerm of this Model has been set, false
 * (0) otherwise.
 */
LIBSBML_EXTERN
int
Model_isSetSBOTerm (const Model_t *m)
{
  return static_cast<int>( m->isSetSBOTerm() );
}


/**
 * Sets the id of this Model to a copy of sid.
 */
LIBSBML_EXTERN
void
Model_setId (Model_t *m, const char *sid)
{
  (sid == NULL) ? m->unsetId() : m->setId(sid);
}


/**
 * Sets the name of this Model to a copy of name.
 */
LIBSBML_EXTERN
void
Model_setName (Model_t *m, const char *name)
{
  (name == NULL) ? m->unsetName() : m->setName(name);
}


/**
 * Sets the sboTerm field of this Model to value.
 */
LIBSBML_EXTERN
void
Model_setSBOTerm (Model_t *m, int sboTerm)
{
  m->setSBOTerm(sboTerm);
}


/**
 * Unsets the id of this Model.
 */
LIBSBML_EXTERN
void
Model_unsetId (Model_t *m)
{
  m->unsetId();
}


/**
 * Unsets the name of this Model.
 */
LIBSBML_EXTERN
void
Model_unsetName (Model_t *m)
{
  m->unsetName();
}


/**
 * Unsets the sboTerm of this Model.
 */
LIBSBML_EXTERN
void
Model_unsetSBOTerm (Model_t *m)
{
  m->unsetSBOTerm();
}


/**
 * Adds a copy of the given FunctionDefinition to this Model.
 */
LIBSBML_EXTERN
void
Model_addFunctionDefinition (Model_t *m, const FunctionDefinition_t *fd)
{
  if (fd != NULL) m->addFunctionDefinition(fd);
}


/**
 * Adds a copy of the given UnitDefinition to this Model.
 */
LIBSBML_EXTERN
void
Model_addUnitDefinition (Model_t *m, const UnitDefinition_t *ud)
{
  if (ud != NULL) m->addUnitDefinition(ud);
}


/**
 * Adds a copy of the given CompartmentType to this Model.
 */
LIBSBML_EXTERN
void
Model_addCompartmentType (Model_t *m, const CompartmentType_t *ct)
{
  if (ct != NULL) m->addCompartmentType(ct);
}


/**
 * Adds a copy of the given SpeciesType to this Model.
 */
LIBSBML_EXTERN
void
Model_addSpeciesType (Model_t *m, const SpeciesType_t *st)
{
  if (st != NULL) m->addSpeciesType(st);
}


/**
 * Adds a copy of the given Compartment to this Model.
 */
LIBSBML_EXTERN
void
Model_addCompartment (Model_t *m, const Compartment_t *c)
{
  if (c != NULL) m->addCompartment(c);
}


/**
 * Adds a copy of the given Species to this Model.
 */
LIBSBML_EXTERN
void
Model_addSpecies (Model_t *m, const Species_t *s)
{
  if (s != NULL) m->addSpecies(s);
}


/**
 * Adds a copy of the given Parameter to this Model.
 */
LIBSBML_EXTERN
void
Model_addParameter (Model_t *m, const Parameter_t *p)
{
  if (p != NULL) m->addParameter(p);
}


/**
 * Adds a copy of the given InitialAssignment to this Model.
 */
LIBSBML_EXTERN
void
Model_addInitialAssignment (Model_t *m, const InitialAssignment_t *ia)
{
  if (ia != NULL) m->addInitialAssignment(ia);
}


/**
 * Adds a copy of the given Rule to this Model.
 */
LIBSBML_EXTERN
void
Model_addRule (Model_t *m, const Rule_t *r)
{
  if (r != NULL) m->addRule(r);
}


/**
 * Adds a copy of the given Constraint to this Model.
 */
LIBSBML_EXTERN
void
Model_addConstraint (Model_t *m, const Constraint_t *c)
{
  if (c != NULL) m->addConstraint(c);
}


/**
 * Adds a copy of the given Reaction to this Model.
 */
LIBSBML_EXTERN
void
Model_addReaction (Model_t *m, const Reaction_t *r)
{
  if (r != NULL) m->addReaction(r);
}


/**
 * Adds a copy of the given Event to this Model.
 */
LIBSBML_EXTERN
void
Model_addEvent (Model_t *m, const Event_t *e)
{
  if (e != NULL) m->addEvent(e);
}


/**
 * Creates a new FunctionDefinition inside this Model and returns a pointer
 * to it.
 */
LIBSBML_EXTERN
FunctionDefinition_t *
Model_createFunctionDefinition (Model_t *m)
{
  return m->createFunctionDefinition();
}


/**
 * Creates a new UnitDefinition inside this Model and returns a pointer to
 * it.
 */
LIBSBML_EXTERN
UnitDefinition_t *
Model_createUnitDefinition (Model_t *m)
{
  return m->createUnitDefinition();
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
  return m->createUnit();
}


/**
 * Creates a new CompartmentType inside this Model and returns a pointer to
 * it.
 */
LIBSBML_EXTERN
CompartmentType_t *
Model_createCompartmentType (Model_t *m)
{
  return m->createCompartmentType();
}


/**
 * Creates a new SpeciesType inside this Model and returns a pointer to
 * it.
 */
LIBSBML_EXTERN
SpeciesType_t *
Model_createSpeciesType (Model_t *m)
{
  return m->createSpeciesType();
}


/**
 * Creates a new Compartment inside this Model and returns a pointer to it.
 */
LIBSBML_EXTERN
Compartment_t *
Model_createCompartment (Model_t *m)
{
  return m->createCompartment();
}


/**
 * Creates a new Species inside this Model and returns a pointer to it.
 */
LIBSBML_EXTERN
Species_t *
Model_createSpecies (Model_t *m)
{
  return m->createSpecies();
}


/**
 * Creates a new Parameter inside this Model and returns a pointer to it.
 */
LIBSBML_EXTERN
Parameter_t *
Model_createParameter (Model_t *m)
{
  return m->createParameter();
}


/**
 * Creates a new InitialAssignment inside this Model and returns it.
 */
LIBSBML_EXTERN
InitialAssignment_t *
Model_createInitialAssignment (Model_t *m)
{
  return m->createInitialAssignment();
}


/**
 * Creates a new AlgebraicRule inside this Model and returns a pointer to
 * it.
 */
LIBSBML_EXTERN
Rule_t *
Model_createAlgebraicRule (Model_t *m)
{
  return m->createAlgebraicRule();
}


/**
 * Creates a new AssignmentRule inside this Model and returns a pointer to
 * it.
 */
LIBSBML_EXTERN
Rule_t *
Model_createAssignmentRule (Model_t *m)
{
  return m->createAssignmentRule();
}


/**
 * Creates a new RateRule inside this Model and returns a pointer to it.
 */
LIBSBML_EXTERN
Rule_t *
Model_createRateRule (Model_t *m)
{
  return m->createRateRule();
}


/**
 * Creates a new Constraint inside this Model and returns it.
 */
LIBSBML_EXTERN
Constraint_t *
Model_createConstraint (Model_t *m)
{
  return m->createConstraint();
}

/**
 * Creates a new Reaction inside this Model and returns a pointer to it.
 */
LIBSBML_EXTERN
Reaction_t *
Model_createReaction (Model_t *m)
{
  return m->createReaction();
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
  return m->createReactant();
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
  return m->createProduct();
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
SpeciesReference_t *
Model_createModifier (Model_t *m)
{
  return static_cast<SpeciesReference_t*>( m->createModifier() );
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
  return m->createKineticLaw();
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
  return m->createKineticLawParameter();
}


/**
 * Creates a new Event inside this Model and returns a pointer to it.
 */
LIBSBML_EXTERN
Event_t *
Model_createEvent (Model_t *m)
{
  return m->createEvent();
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
  return m->createEventAssignment();
}




/**
 * @return the list of FunctionDefinitions for this Model.
 */
LIBSBML_EXTERN
ListOf_t *
Model_getListOfFunctionDefinitions (Model_t *m)
{
  return m->getListOfFunctionDefinitions();
}


/**
 * @return the list of UnitDefinitions for this Model.
 */
LIBSBML_EXTERN
ListOf_t *
Model_getListOfUnitDefinitions (Model_t *m)
{
  return m->getListOfUnitDefinitions();
}


/**
 * @return the list of CompartmentTypes for this Model.
 */
LIBSBML_EXTERN
ListOf_t *
Model_getListOfCompartmentTypes (Model_t *m)
{
  return m->getListOfCompartmentTypes();
}


/**
 * @return the list of SpeciesTypes for this Model.
 */
LIBSBML_EXTERN
ListOf_t *
Model_getListOfSpeciesTypes (Model_t *m)
{
  return m->getListOfSpeciesTypes();
}


/**
 * @return the list of Compartments for this Model.
 */
LIBSBML_EXTERN
ListOf_t *
Model_getListOfCompartments (Model_t *m)
{
  return m->getListOfCompartments();
}


/**
 * @return the list of Species for this Model.
 */
LIBSBML_EXTERN
ListOf_t *
Model_getListOfSpecies (Model_t *m)
{
  return m->getListOfSpecies();
}


/**
 * @return the list of Parameters for this Model.
 */
LIBSBML_EXTERN
ListOf_t *
Model_getListOfParameters (Model_t *m)
{
  return m->getListOfParameters();
}


/**
 * @return the list of InitialAssignments for this Model.
 */
LIBSBML_EXTERN
ListOf_t *
Model_getListOfInitialAssignments (Model_t* m)
{
  return m->getListOfInitialAssignments();
}


/**
 * @return the list of Rules for this Model.
 */
LIBSBML_EXTERN
ListOf_t *
Model_getListOfRules (Model_t *m)
{
  return m->getListOfRules();
}


/**
 * @return the list of Constraints for this Model.
 */
LIBSBML_EXTERN
ListOf_t *
Model_getListOfConstraints (Model_t* m)
{
  return m->getListOfConstraints();
}


/**
 * @return the list of Rules for this Model.
 */
LIBSBML_EXTERN
ListOf_t *
Model_getListOfReactions (Model_t *m)
{
  return m->getListOfReactions();
}


/**
 * @return the list of Rules for this Model.
 */
LIBSBML_EXTERN
ListOf_t *
Model_getListOfEvents (Model_t *m)
{
  return m->getListOfEvents();
}


/**
 * @return the nth FunctionDefinition of this Model.
 */
LIBSBML_EXTERN
FunctionDefinition_t *
Model_getFunctionDefinition (Model_t *m, unsigned int n)
{
  return m->getFunctionDefinition(n);
}


/**
 * @return the FunctionDefinition in this Model with the given id or NULL
 * if no such FunctionDefinition exists.
 */
LIBSBML_EXTERN
FunctionDefinition_t *
Model_getFunctionDefinitionById (Model_t *m, const char *sid)
{
  return (sid != NULL) ? m->getFunctionDefinition(sid) : NULL;
}


/**
 * @return the nth UnitDefinition of this Model.
 */
LIBSBML_EXTERN
UnitDefinition_t *
Model_getUnitDefinition (Model_t *m, unsigned int n)
{
  return m->getUnitDefinition(n);
}


/**
 * @return the UnitDefinition in this Model with the given id or NULL if no
 * such UnitDefinition exists.
 */
LIBSBML_EXTERN
UnitDefinition_t *
Model_getUnitDefinitionById (Model_t *m, const char *sid)
{
  return (sid != NULL) ? m->getUnitDefinition(sid) : NULL;
}


/**
 * @return the nth CompartmentType of this Model.
 */
LIBSBML_EXTERN
CompartmentType_t *
Model_getCompartmentType (Model_t *m, unsigned int n)
{
  return m->getCompartmentType(n);
}


/**
 * @return the CompartmentType in this Model with the given id or NULL if no
 * such CompartmentType exists.
 */
LIBSBML_EXTERN
CompartmentType_t *
Model_getCompartmentTypeById (Model_t *m, const char *sid)
{
  return (sid != NULL) ? m->getCompartmentType(sid) : NULL;
}


/**
 * @return the nth SpeciesType of this Model.
 */
LIBSBML_EXTERN
SpeciesType_t *
Model_getSpeciesType (Model_t *m, unsigned int n)
{
  return m->getSpeciesType(n);
}


/**
 * @return the SpeciesType in this Model with the given id or NULL if no
 * such SpeciesType exists.
 */
LIBSBML_EXTERN
SpeciesType_t *
Model_getSpeciesTypeById (Model_t *m, const char *sid)
{
  return (sid != NULL) ? m->getSpeciesType(sid) : NULL;
}


/**
 * @return the nth Compartment of this Model.
 */
LIBSBML_EXTERN
Compartment_t *
Model_getCompartment (Model_t *m, unsigned int n)
{
  return m->getCompartment(n);
}


/**
 * @return the Compartment in this Model with the given id or NULL if no
 * such Compartment exists.
 */
LIBSBML_EXTERN
Compartment_t *
Model_getCompartmentById (Model_t *m, const char *sid)
{
  return (sid != NULL) ? m->getCompartment(sid) : NULL;
}


/**
 * @return the nth Species of this Model.
 */
LIBSBML_EXTERN
Species_t *
Model_getSpecies (Model_t *m, unsigned int n)
{
  return m->getSpecies(n);
}


/**
 * @return the Species in this Model with the given id or NULL if no such
 * Species exists.
 */
LIBSBML_EXTERN
Species_t *
Model_getSpeciesById (Model_t *m, const char *sid)
{
  return (sid != NULL) ? m->getSpecies(sid) : NULL;
}


/**
 * @return the nth Parameter of this Model.
 */
LIBSBML_EXTERN
Parameter_t *
Model_getParameter (Model_t *m, unsigned int n)
{
  return m->getParameter(n);
}


/**
 * @return the Parameter in this Model with the given id or NULL if no such
 * Parameter exists.
 */
LIBSBML_EXTERN
Parameter_t *
Model_getParameterById (Model_t *m, const char *sid)
{
  return (sid != NULL) ? m->getParameter(sid) : NULL;
}


/**
 * @return the nth InitialAssignment of this Model.
 */
LIBSBML_EXTERN
InitialAssignment_t *
Model_getInitialAssignment (Model_t *m, unsigned int n)
{
  return m->getInitialAssignment(n);
}


/**
 * @return the InitialAssignment in this Model with the given symbol or
 * NULL if no such InitialAssignment exists.
 */
LIBSBML_EXTERN
InitialAssignment_t *
Model_getInitialAssignmentBySym (Model_t *m, const char *symbol)
{
  return (symbol != NULL) ? m->getInitialAssignment(symbol) : NULL;
}


/**
 * @return the nth Rule of this Model.
 */
LIBSBML_EXTERN
Rule_t *
Model_getRule (Model_t *m, unsigned int n)
{
  return m->getRule(n);
}


/**
 * @return the Rule in this Model with the given symbol or NULL if no
 * such Rule exists.
 */
LIBSBML_EXTERN
Rule_t *
Model_getRuleByVar (Model_t *m, const char *variable)
{
  return (variable != NULL) ? m->getRule(variable) : NULL;
}


/**
 * @return the nth Constraint of this Model.
 */
LIBSBML_EXTERN
Constraint_t *
Model_getConstraint (Model_t *m, unsigned int n)
{
  return m->getConstraint(n);
}


/**
 * @return the nth Reaction of this Model.
 */
LIBSBML_EXTERN
Reaction_t *
Model_getReaction (Model_t *m, unsigned int n)
{
  return m->getReaction(n);
}


/**
 * @return the Reaction in this Model with the given id or NULL if no such
 * Reaction exists.
 */
LIBSBML_EXTERN
Reaction_t *
Model_getReactionById (Model_t *m, const char *sid)
{
  return (sid != NULL) ? m->getReaction(sid) : NULL;
}


/**
 * @return the nth Event of this Model.
 */
LIBSBML_EXTERN
Event_t *
Model_getEvent (Model_t *m, unsigned int n)
{
  return m->getEvent(n);
}


/**
 * @return the Event in this Model with the given id or NULL if no such
 * Event exists.
 */
LIBSBML_EXTERN
Event_t *
Model_getEventById (Model_t *m, const char *sid)
{
  return (sid != NULL) ? m->getEvent(sid) : NULL;
}


/**
 * @return the number of FunctionDefinitions in this Model.
 */
LIBSBML_EXTERN
unsigned int
Model_getNumFunctionDefinitions (const Model_t *m)
{
  return m->getNumFunctionDefinitions();
}


/**
 * @return the number of UnitDefinitions in this Model.
 */
LIBSBML_EXTERN
unsigned int
Model_getNumUnitDefinitions (const Model_t *m)
{
  return m->getNumUnitDefinitions();
}


/**
 * @return the number of CompartmentTypes in this Model.
 */
LIBSBML_EXTERN
unsigned int
Model_getNumCompartmentTypes (const Model_t *m)
{
  return m->getNumCompartmentTypes();
}


/**
 * @return the number of SpeciesTypes in this Model.
 */
LIBSBML_EXTERN
unsigned int
Model_getNumSpeciesTypes (const Model_t *m)
{
  return m->getNumSpeciesTypes();
}


/**
 * @return the number of Compartments in this Model.
 */
LIBSBML_EXTERN
unsigned int
Model_getNumCompartments (const Model_t *m)
{
  return m->getNumCompartments();
}


/**
 * @return the number of Species in this Model.
 */
LIBSBML_EXTERN
unsigned int
Model_getNumSpecies (const Model_t *m)
{
  return m->getNumSpecies();
}


/**
 * @return the number of Species in this Model with boundaryCondition set
 * to true.
 */
LIBSBML_EXTERN
unsigned int
Model_getNumSpeciesWithBoundaryCondition (const Model_t *m)
{
  return m->getNumSpeciesWithBoundaryCondition();
}


/**
 * @return the number of Parameters in this Model.  Parameters defined in
 * KineticLaws are not included.
 */
LIBSBML_EXTERN
unsigned int
Model_getNumParameters (const Model_t *m)
{
  return m->getNumParameters();
}


/**
 * @return the number of InitialAssignments in this Model.
 */
LIBSBML_EXTERN
unsigned int
Model_getNumInitialAssignments (const Model_t *m)
{
  return m->getNumInitialAssignments();
}


/**
 * @return the number of Rules in this Model.
 */
LIBSBML_EXTERN
unsigned int
Model_getNumRules (const Model_t *m)
{
  return m->getNumRules();
}


/**
 * @return the number of Constraints in this Model.
 */
LIBSBML_EXTERN
unsigned int
Model_getNumConstraints (const Model_t *m)
{
  return m->getNumConstraints();
}

/**
 * @return the number of Reactions in this Model.
 */
LIBSBML_EXTERN
unsigned int
Model_getNumReactions (const Model_t *m)
{
  return m->getNumReactions();
}


/**
 * @return the number of Events in this Model.
 */
LIBSBML_EXTERN
unsigned int
Model_getNumEvents (const Model_t *m)
{
  return m->getNumEvents();
}



#ifdef USE_LAYOUT  


/**
 * Returns a reference to the ListOf object that holds the layouts.
 */
LIBSBML_EXTERN
ListOf_t *
Model_getListOfLayouts (Model_t *m)
{
  return m->getListOfLayouts();
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
Model_addLayout (Model_t *m, const Layout_t *layout)
{
  m->addLayout(layout);
}


/**
 * Creates a new layout object and adds it to the list of layout objects.
 * A pointer to the newly created object is returned.
 */
LIBSBML_EXTERN
Layout_t *
Model_createLayout (Model_t *m)
{
  return m->createLayout();
}


#endif  /* USE_LAYOUT */
