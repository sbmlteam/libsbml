/**
 * @file    Model.cpp
 * @brief   Implementation of Model.
 * @author  Ben Bornstein
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

#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>

#include <sbml/math/ASTNode.h>

#include <sbml/SBMLDocument.h>
#include <sbml/SBO.h>
#include <sbml/SBMLVisitor.h>
#include <sbml/SBMLError.h>
#include <sbml/KineticLaw.h>
#include <sbml/Model.h>

#ifdef USE_LAYOUT
#include <sbml/annotation/LayoutAnnotation.h>
#endif // USE_LAYOUT

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */


/**
 * Creates a new Model, optionally with its id and name attributes set.
 */
Model::Model (const std::string& id, const std::string& name) :
   SBase   ( id, name, -1 )
 , mHistory (0)
{
    //mFormulaUnitsData = new ListFormulaUnitsData;
}


/**
 * Destroys this Model.
 */
Model::~Model ()
{
  delete mHistory;
  //int size = getNumFormulaUnitsData();


  //while (size--) delete static_cast<FormulaUnitsData*>( mFormulaUnitsData->remove(0) );
  //delete static_cast<ListFormulaUnitsData *> (mFormulaUnitsData);

}


/**
 * Copy constructor.
 */
Model::Model(const Model& orig) :
       SBase   ( orig )
     , mFunctionDefinitions (orig.mFunctionDefinitions)
     , mUnitDefinitions     (orig.mUnitDefinitions)
     , mCompartmentTypes    (orig.mCompartmentTypes)
     , mSpeciesTypes        (orig.mSpeciesTypes)
     , mCompartments        (orig.mCompartments)
     , mSpecies             (orig.mSpecies)
     , mParameters          (orig.mParameters)
     , mInitialAssignments  (orig.mInitialAssignments)
     , mRules               (orig.mRules)
     , mConstraints         (orig.mConstraints)
     , mReactions           (orig.mReactions)
     , mEvents              (orig.mEvents)
     , mFormulaUnitsData    (orig.mFormulaUnitsData)
#ifdef USE_LAYOUT
     , mLayouts             (orig.mLayouts)
#endif
{
  if (orig.mHistory)
  {
    this->mHistory = orig.mHistory->clone();
  }
  else
  {
    this->mHistory = 0;
  }
}


/**
 * Assignment operator
 */
Model& Model::operator=(const Model& rhs)
{
  this->SBase::operator = (rhs);
  mFunctionDefinitions  = rhs.mFunctionDefinitions;
  mUnitDefinitions      = rhs.mUnitDefinitions;
  mCompartmentTypes     = rhs.mCompartmentTypes;
  mSpeciesTypes         = rhs.mSpeciesTypes;
  mCompartments         = rhs.mCompartments;
  mSpecies              = rhs.mSpecies;
  mParameters           = rhs.mParameters;
  mInitialAssignments   = rhs.mInitialAssignments;
  mRules                = rhs.mRules;
  mConstraints          = rhs.mConstraints;
  mReactions            = rhs.mReactions;
  mEvents               = rhs.mEvents;
#ifdef USE_LAYOUT
  mLayouts              = rhs.mLayouts;
#endif
  //  /* TO DO NEED TO DEAL WITH CLONING THIS */
  //mFormulaUnitsData = new ListFormulaUnitsData();
  mFormulaUnitsData     = rhs.mFormulaUnitsData;
  if (rhs.mHistory)
  {
    this->mHistory = rhs.mHistory->clone();
  }
  else
  {
    this->mHistory = 0;
  }

  return *this;
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

ModelHistory* 
Model::getModelHistory() const
{
  return mHistory;
}

ModelHistory* 
Model::getModelHistory()
{
  return mHistory;
}

bool
Model::isSetModelHistory()
{
  return (mHistory != 0);
}

void
Model::setModelHistory(ModelHistory * history)
{
  if (mHistory == history) return;

  delete mHistory;
  mHistory = (history != 0) ? static_cast<ModelHistory*>( history->clone() ) : 0;
}


void 
Model::unsetModelHistory()
{
  delete mHistory;
  mHistory = 0;
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
 * Creates a new Reactant (ie SpeciesReference) inside this Model and
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
 * Creates a new Product (ie SpeciesReference) inside this Model and
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
 * Creates a new Modifer (ie ModifierSpeciesReference) inside this Model
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
Model::getFunctionDefinition (const std::string& sid) const
{
  return static_cast<const FunctionDefinition*>(mFunctionDefinitions.get(sid));
}


/**
 * @return the FunctionDefinition in this Model with the given id or NULL
 * if no such FunctionDefinition exists.
 */
FunctionDefinition*
Model::getFunctionDefinition (const std::string& sid)
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
Model::getUnitDefinition (const std::string& sid) const
{
  return static_cast<const UnitDefinition*>( mUnitDefinitions.get(sid) );
}


/**
 * @return the UnitDefinition in this Model with the given id or NULL if no
 * such UnitDefinition exists.
 */
UnitDefinition*
Model::getUnitDefinition (const std::string& sid)
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
Model::getCompartmentType (const std::string& sid) const
{
  return static_cast<const CompartmentType*>( mCompartmentTypes.get(sid) );
}


/**
 * @return the CompartmentType in this Model with the given id or NULL if
 * no such CompartmentType exists.
 */
CompartmentType*
Model::getCompartmentType (const std::string& sid)
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
Model::getSpeciesType (const std::string& sid) const
{
  return static_cast<const SpeciesType*>( mSpeciesTypes.get(sid) );
}


/**
 * @return the SpeciesType in this Model with the given id or NULL if
 * no such SpeciesType exists.
 */
SpeciesType*
Model::getSpeciesType (const std::string& sid)
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
Model::getCompartment (const std::string& sid) const
{
  return static_cast<const Compartment*>( mCompartments.get(sid) );
}


/**
 * @return the Compartment in this Model with the given id or NULL if no
 * such Compartment exists.
 */
Compartment*
Model::getCompartment (const std::string& sid)
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
Model::getSpecies (const std::string& sid) const
{
  return static_cast<const Species*>( mSpecies.get(sid) );
}


/**
 * @return the Species in this Model with the given id or NULL if no such
 * Species exists.
 */
Species*
Model::getSpecies (const std::string& sid)
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
Model::getParameter (const std::string& sid) const
{
  return static_cast<const Parameter*>( mParameters.get(sid) );
}


/**
 * @return the Parameter in this Model with the given id or NULL if no such
 * Parameter exists.
 */
Parameter*
Model::getParameter (const std::string& sid)
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
Model::getInitialAssignment (const std::string& symbol) const
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
Model::getInitialAssignment (const std::string& symbol)
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
Model::getRule (const std::string& variable) const
{
  return static_cast<const Rule*>( mRules.get(variable) );
}

 
/**
 * @return the Rule in this Model with the given symbol or NULL if no
 * such Rule exists.
 */
Rule*
Model::getRule (const std::string& variable)
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
Model::getReaction (const std::string& sid) const
{
  return static_cast<const Reaction*>( mReactions.get(sid) );
}


/**
 * @return the Reaction in this Model with the given id or NULL if no such
 * Reaction exists.
 */
Reaction*
Model::getReaction (const std::string& sid)
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
Model::getEvent (const std::string& sid) const
{
  return static_cast<const Event*>( mEvents.get(sid) );
}


/**
 * @return the Event in this Model with the given id or NULL if no such
 * Event exists.
 */
Event*
Model::getEvent (const std::string& sid)
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
Model::getNumSpeciesWithBoundaryCondition () const 
{
  unsigned int count = 0;

  for(unsigned int i = 0; i < mSpecies.size(); i++)
  {
    if (getSpecies(i)->getBoundaryCondition())
      count++;
  }
  return count;
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
 * @return the name of this element ie "model".
 
 */
const string&
Model::getElementName () const
{
  static const string name = "model";
  return name;
}


/** @cond doxygen-libsbml-internal */
/**
 * Subclasses should override this method to read (and store) XHTML,
 * MathML, etc. directly from the XMLInputStream.
 *
 * @return true if the subclass read from the stream, false otherwise.
 */
bool
Model::readOtherXML (XMLInputStream& stream)
{
  bool          read = false;
  const string& name = stream.peek().getName();

  // This has to do additional work for reading annotations, so the code
  // here is copied and expanded from SBase::readNotes().

  if (name == "annotation")
  {
    XMLNode* new_annotation = NULL;
    /* if annotation already exists then it is an error 
     */
    if (mAnnotation)
    {
      logError(SBMLError::NotSchemaConformant,
	       "Multiple annotation elements not permitted");
    }

    delete mAnnotation;
    mAnnotation = new XMLNode(stream);
    checkAnnotation();
    if (mCVTerms)
    {
      unsigned int size = mCVTerms->getSize();
      while (size--) delete static_cast<CVTerm*>( mCVTerms->remove(0) );
      delete mCVTerms;
    }
    mCVTerms = new List();
    delete mHistory;
    mHistory = RDFAnnotationParser::parseRDFAnnotation(mAnnotation);
    RDFAnnotationParser::parseRDFAnnotation(mAnnotation, mCVTerms);
    new_annotation = RDFAnnotationParser::deleteRDFAnnotation(mAnnotation);
    delete mAnnotation;
    mAnnotation = new_annotation;
#ifdef USE_LAYOUT
    parseLayoutAnnotation(mAnnotation,mLayouts);
    new_annotation=deleteLayoutAnnotation(mAnnotation);
    delete mAnnotation;
    mAnnotation = new_annotation;
#endif // USE_LAYOUT
	
    read = true;
  }

  return read;
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
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
    if (mFunctionDefinitions.size() != 0)
    {
      logError(SBMLError::NotSchemaConformant);
    }
    object = &mFunctionDefinitions;
  }

  else if ( name == "listOfUnitDefinitions"    ) 
  {
    if (mUnitDefinitions.size() != 0)
    {
      logError(SBMLError::NotSchemaConformant);
    }
    object = &mUnitDefinitions;
  }

  else if ( name == "listOfCompartmentTypes"   ) 
  {
    if (mCompartmentTypes.size() != 0)
    {
      logError(SBMLError::NotSchemaConformant);
    }
    object = &mCompartmentTypes;
  }

  else if ( name == "listOfSpeciesTypes"       ) 
  {
    if (mSpeciesTypes.size() != 0)
    {
      logError(SBMLError::NotSchemaConformant);
    }
    object = &mSpeciesTypes;
  }

  else if ( name == "listOfCompartments"       ) 
  {
    if (mCompartments.size() != 0)
    {
      logError(SBMLError::NotSchemaConformant);
    }
    object = &mCompartments;
  }
  
  else if ( name == "listOfSpecies"            ) 
  {
    if (mSpecies.size() != 0)
    {
      logError(SBMLError::NotSchemaConformant);
    }
    object = &mSpecies;
  }

  else if ( name == "listOfParameters"         ) 
  {
    if (mParameters.size() != 0)
    {
      logError(SBMLError::NotSchemaConformant);
    }
    object = &mParameters;
  }

  else if ( name == "listOfInitialAssignments" ) 
  {
    if (mInitialAssignments.size() != 0)
    {
      logError(SBMLError::NotSchemaConformant);
    }
    object = &mInitialAssignments;
  }

  else if ( name == "listOfRules"              ) 
  {
    if (mRules.size() != 0)
    {
      logError(SBMLError::NotSchemaConformant);
    }
    object = &mRules;
  }

  else if ( name == "listOfConstraints"        ) 
  {
    if (mConstraints.size() != 0)
    {
      logError(SBMLError::NotSchemaConformant);
    }
    object = &mConstraints;
  }

  else if ( name == "listOfReactions"          ) 
  {
    if (mReactions.size() != 0)
    {
      logError(SBMLError::NotSchemaConformant);
    }
    object = &mReactions;
  }

  else if ( name == "listOfEvents"             ) 
  {
    if (mEvents.size() != 0)
    {
      logError(SBMLError::NotSchemaConformant);
    }
    object = &mEvents;
  }

  else if ( getLevel() == 1 && getVersion() == 1 )
  {
    if (name == "listOfSpecie") 
    {
      if (mSpecies.size() != 0)
      {
        logError(SBMLError::NotSchemaConformant);
      }
      object = &mSpecies;
    }
  }
#ifdef USE_LAYOUT
  else if ( name == "listOfLayouts"             ) object = &mLayouts;
#endif /* USE_LAYOUT */  

  return object;
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
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
  SBase::checkIdSyntax();

  //
  // name: string  { use="optional" }  (L2v1, L2v2)
  //
  if (level == 2) attributes.readInto("name", mName);

  //
  // sboTerm: SBOTerm { use="optional" }  (L2v2)
  //
  if (level == 2 && (version == 2 || version == 3)) 
    mSBOTerm = SBO::readTerm(attributes, this->getErrorLog());

}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
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
  if (level == 2 && (version == 2 || version == 3)) 
    SBO::writeTerm(stream, mSBOTerm);
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/**
 * Subclasses should override this method to write out their contained
 * SBML objects as XML elements.  Be sure to call your parents
 * implementation of this method as well.
 */
void
Model::writeElements (XMLOutputStream& stream) const
{
  /* add/append model history to annotation */
  if (getModelHistory())
  {
    Model * m = const_cast <Model *> (this);
    XMLNode * history = RDFAnnotationParser::parseModelHistory(this);
    if(!mAnnotation)

    {
      if (history) static_cast <SBase *> (m)->setAnnotation(history);
    }
    else
    {
      if (history) static_cast <SBase *> (m)->appendAnnotation(history);
    }
  }
#ifdef USE_LAYOUT
    if (this->getListOfLayouts()->size()!=0)
    {
      Model * m = const_cast <Model *> (this);
      XMLNode * layouts = parseLayouts(this);
      if(!mAnnotation)
      {
        if (layouts) static_cast <SBase *> (m)->setAnnotation(layouts);
      }
      else
      {
        if (layouts) static_cast <SBase *> (m)->appendAnnotation(layouts);
      }
    }
#endif // USE_LAYOUT    

  SBase::writeElements(stream);

  const unsigned int level   = getLevel  ();
  const unsigned int version = getVersion();

  if (level == 2 && getNumFunctionDefinitions() > 0)
  {
    mFunctionDefinitions.write(stream);
  }

  if ( getNumUnitDefinitions() > 0 ) mUnitDefinitions.write(stream);

  if (level == 2 && (version == 2 || version == 3))
  {
    if ( getNumCompartmentTypes() > 0 ) mCompartmentTypes.write(stream);
    if ( getNumSpeciesTypes    () > 0 ) mSpeciesTypes    .write(stream);
  }

  if ( getNumCompartments() > 0 ) mCompartments.write(stream);
  if ( getNumSpecies     () > 0 ) mSpecies     .write(stream);
  if ( getNumParameters  () > 0 ) mParameters  .write(stream);

  if (level == 2 && (version == 2 || version == 3))
  {
    if ( getNumInitialAssignments() > 0 ) mInitialAssignments.write(stream);
  }

  if ( getNumRules() > 0 ) mRules.write(stream);

  if (level == 2 && (version == 2 || version == 3))
  {
    if ( getNumConstraints() > 0 ) mConstraints.write(stream);
  }

  if ( getNumReactions() > 0 ) mReactions.write(stream);

  if (level == 2 && getNumEvents () > 0 )
  {
    mEvents.write(stream);
  }
}
/** @endcond doxygen-libsbml-internal */


/** @cond doxygen-libsbml-internal */
/**
 * @return the ordinal position of the element with respect to its siblings
 * or -1 (default) to indicate the position is not significant.
 */
int
Model::getElementPosition () const
{
  return 2;
}
/** @endcond doxygen-libsbml-internal */



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



/** @cond doxygen-c-only */


/**
 * Creates a new Model_t structure and returns a pointer to it.
 *
 * @return the freshly-created Model_t structure.
 */
LIBSBML_EXTERN
Model_t *
Model_create ()
{
  return new(nothrow) Model;
}


/**
 * Creates a new Model_t structure with the given identifier and returns a
 * pointer to it.
 *
 * @param sid a string, the identifier to assign to this Model_t structure
 * @param name a string, the name to give this Model_t structure
 *
 * @return the Model_t structure created
 */
LIBSBML_EXTERN
Model_t *
Model_createWith (const char *sid, const char * name)
{
  return new(nothrow) Model(sid ? sid : "", name ? name : "");
}


/**
 * Creates and returns a deep copy of a given Model_t structure.
 *
 * @param m the Model_t structure to copy
 * 
 * @return a (deep) copy of this Model_t structure.
 */
LIBSBML_EXTERN
Model_t *
Model_clone (const Model_t *m)
{
  return static_cast<Model*>( m->clone() );
}


/**
 * Frees the given Model_t structure.
 *
 * @param m the Model_structure to free
 */
LIBSBML_EXTERN
void
Model_free (Model_t *m)
{
  delete m;
}


/**
 * Get the identifier of the given Model_t structure.
 *
 * @param m the Model_t structure
 * 
 * @return the id of this Model_t structure.
 */
LIBSBML_EXTERN
const char *
Model_getId (const Model_t *m)
{
  return m->isSetId() ? m->getId().c_str() : NULL;
}


/**
 * Get the name of the given Model_t structure.
 * 
 * @param m the Model_t structure
 * 
 * @return the name of this Model_t structure.
 */
LIBSBML_EXTERN
const char *
Model_getName (const Model_t *m)
{
  return m->isSetName() ? m->getName().c_str() : NULL;
}


/**
 * Predicate for testing whether the identifier of a given Model_t
 * structure has been assigned.
 * 
 * @param m the Model_t structure
 * 
 * @return nonzero if the "id" attribute of this Model_t structure has been
 * set, zero (0) otherwise.
 */
LIBSBML_EXTERN
int
Model_isSetId (const Model_t *m)
{
  return static_cast<int>( m->isSetId() );
}


/**
 * Predicate for testing whether the name of a given Model_t structure has
 * been assigned.
 * 
 * @param m the Model_t structure
 * 
 * @return nonzero if the "name" attribute of this Model_t structure has
 * been set, zero (0) otherwise.
 */
LIBSBML_EXTERN
int
Model_isSetName (const Model_t *m)
{
  return static_cast<int>( m->isSetName() );
}


/**
 * Set the identifier of a given Model_t structure.
 *
 * This copies the string in @p sid.
 * 
 * @param m the Model_t structure
 * @param sid the identifier string
 */
LIBSBML_EXTERN
void
Model_setId (Model_t *m, const char *sid)
{
  (sid == NULL) ? m->unsetId() : m->setId(sid);
}


/**
 * Set the identifier of the given Model_t structure.
 * 
 * This copies the string in @p name.
 *
 * @param m the Model_t structure
 * @param name the name string
 */
LIBSBML_EXTERN
void
Model_setName (Model_t *m, const char *name)
{
  (name == NULL) ? m->unsetName() : m->setName(name);
}


/**
 * Unsets the "id" attribute of the given Model_t structure.
 *
 * @param m the Model_t structure
 */
LIBSBML_EXTERN
void
Model_unsetId (Model_t *m)
{
  m->unsetId();
}


/**
 * Unsets the "name" attribute of the given Model_t structure.
 *
 * @param m the Model_t structure
 */
LIBSBML_EXTERN
void
Model_unsetName (Model_t *m)
{
  m->unsetName();
}


/**
 * Returns the ModelHistory of the given Model_t structure.
 *
 * @return the ModelHistory of the given Model_t structure.
 * 
 * @param m the Model_t structure
 */
LIBSBML_EXTERN
ModelHistory_t * 
Model_getModelHistory(Model_t *m)
{
  return m->getModelHistory();
}

/**
 * Predicate for testing whether the ModelHistory of a given Model_t structure has
 * been assigned.
 * 
 * @param m the Model_t structure
 * 
 * @return nonzero if the ModelHistory of this Model_t structure has
 * been set, zero (0) otherwise.
 */LIBSBML_EXTERN
int 
Model_isSetModelHistory(Model_t *m)
{
  return static_cast<int>( m->isSetModelHistory() );
}


/**
 * Set the ModelHistory of the given Model_t structure.
 * 
 * @param m the Model_t structure
 * @param history the ModelHistory_t structure
 */
LIBSBML_EXTERN
void 
Model_setModelHistory(Model_t *m, ModelHistory_t *history)
{
  m->setModelHistory(history);
}

/**
 * Unsets the ModelHistory of the given Model_t structure.
 * 
 * @param m the Model_t structure
 */
LIBSBML_EXTERN
void 
Model_unsetModelHistory(Model_t *m)
{
  m->unsetModelHistory();
}


/**
 * Adds a copy of a FunctionDefinition_t structure to a given Model_t
 * structure.
 *
 * @param m the Model_t structure
 * @param fd the FunctionDefinition_t structure to copy and add
 */
LIBSBML_EXTERN
void
Model_addFunctionDefinition (Model_t *m, const FunctionDefinition_t *fd)
{
  if (fd != NULL) m->addFunctionDefinition(fd);
}


/**
 * Adds a copy of a UnitDefinition_t structure to a given Model_t structure.
 *
 * @param m the Model_t structure
 * @param ud the UnitDefinition_t structure to copy and add
 */
LIBSBML_EXTERN
void
Model_addUnitDefinition (Model_t *m, const UnitDefinition_t *ud)
{
  if (ud != NULL) m->addUnitDefinition(ud);
}


/**
 * Adds a copy of a CompartmentType_t structure to a given Model_t structure.
 *
 * @param m the Model_t structure
 * @param ct the CompartmentType_t structure to copy and add
 */
LIBSBML_EXTERN
void
Model_addCompartmentType (Model_t *m, const CompartmentType_t *ct)
{
  if (ct != NULL) m->addCompartmentType(ct);
}


/**
 * Adds a copy of a SpeciesType_t structure to a given Model_t structure.
 *
 * @param m the Model_t structure
 * @param st the SpeciesType_t structure to copy and add
 */
LIBSBML_EXTERN
void
Model_addSpeciesType (Model_t *m, const SpeciesType_t *st)
{
  if (st != NULL) m->addSpeciesType(st);
}


/**
 * Adds a copy of a Compartment_t structure to a given Model_t structure.
 *
 * @param m the Model_t structure
 * @param c the Compartment_t structure to copy and add
 */
LIBSBML_EXTERN
void
Model_addCompartment (Model_t *m, const Compartment_t *c)
{
  if (c != NULL) m->addCompartment(c);
}


/**
 * Adds a copy of a Species_t structure to a given Model_t structure.
 *
 * @param m the Model_t structure
 * @param s the Species_t structure to copy and add
 */
LIBSBML_EXTERN
void
Model_addSpecies (Model_t *m, const Species_t *s)
{
  if (s != NULL) m->addSpecies(s);
}


/**
 * Adds a copy of a Parameter_t structure to a given Model_t structure.
 *
 * @param m the Model_t structure
 * @param p the Parameter_t structure to copy and add
 */
LIBSBML_EXTERN
void
Model_addParameter (Model_t *m, const Parameter_t *p)
{
  if (p != NULL) m->addParameter(p);
}


/**
 * Adds a copy of a InitialAssignment_t structure to a given Model_t structure.
 *
 * @param m the Model_t structure
 * @param ia the InitialAssignment_t structure to copy and add
 */
LIBSBML_EXTERN
void
Model_addInitialAssignment (Model_t *m, const InitialAssignment_t *ia)
{
  if (ia != NULL) m->addInitialAssignment(ia);
}


/**
 * Adds a copy of a Rule_t structure to a given Model_t structure.
 *
 * @param m the Model_t structure
 * @param r the Rule_t structure to copy and add
 */
LIBSBML_EXTERN
void
Model_addRule (Model_t *m, const Rule_t *r)
{
  if (r != NULL) m->addRule(r);
}


/**
 * Adds a copy of a Constraint_t structure to a given Model_t structure.
 *
 * @param m the Model_t structure
 * @param c the Constraint_t structure to copy and add
 */
LIBSBML_EXTERN
void
Model_addConstraint (Model_t *m, const Constraint_t *c)
{
  if (c != NULL) m->addConstraint(c);
}


/**
 * Adds a copy of a Reaction_t structure to a given Model_t structure.
 *
 * @param m the Model_t structure
 * @param r the Reaction_t structure to copy and add
 */
LIBSBML_EXTERN
void
Model_addReaction (Model_t *m, const Reaction_t *r)
{
  if (r != NULL) m->addReaction(r);
}


/**
 * Adds a copy of a Event_t structure to a given Model_t structure.
 *
 * @param m the Model_t structure
 * @param e the Event_t structure to copy and add
 */
LIBSBML_EXTERN
void
Model_addEvent (Model_t *m, const Event_t *e)
{
  if (e != NULL) m->addEvent(e);
}


/**
 * Creates a new FunctionDefinition_t structure inside the given Model_t
 * and returns a pointer to it.
 *
 * @param m the Model_t structure
 *
 * @return the new FunctionDefinition_t structure
 */
LIBSBML_EXTERN
FunctionDefinition_t *
Model_createFunctionDefinition (Model_t *m)
{
  return m->createFunctionDefinition();
}


/**
 * Creates a new UnitDefinition_t structure inside the given Model_t and
 * returns a pointer to it.
 *
 * @param m the Model_t structure
 *
 * @return the new UnitDefinition_t structure
 */
LIBSBML_EXTERN
UnitDefinition_t *
Model_createUnitDefinition (Model_t *m)
{
  return m->createUnitDefinition();
}


/**
 * Creates a new Unit_t structure inside the last UnitDefinition_t
 * structure created in this model and returns a pointer to it.
 *
 * The mechanism by which the UnitDefinition_t structure was created is not
 * significant.  If a UnitDefinition_t does not exist in this model, a new
 * Unit_t structure is @em not created and NULL is returned instead.
 *
 * @param m the Model_t structure
 *
 * @return the Unit_t structure created, or NULL.
 */
LIBSBML_EXTERN
Unit_t *
Model_createUnit (Model_t *m)
{
  return m->createUnit();
}


/**
 * Creates a new CompartmentType_t structure inside the given Model_t and
 * returns a pointer to it.
 *
 * @param m the Model_t structure
 *
 * @return the CompartmentType_t structure created
 */
LIBSBML_EXTERN
CompartmentType_t *
Model_createCompartmentType (Model_t *m)
{
  return m->createCompartmentType();
}


/**
 * Creates a new SpeciesType_t structure inside the given Model_t and
 * returns a pointer to it.
 *
 * @param m the Model_t structure
 *
 * @return the SpeciesType_t structure created
 */
LIBSBML_EXTERN
SpeciesType_t *
Model_createSpeciesType (Model_t *m)
{
  return m->createSpeciesType();
}


/**
 * Creates a new Compartment_t structure inside the given Model_t and
 * returns a pointer to it.
 *
 * @param m the Model_t structure
 *
 * @return the Compartment_t structure created
 */
LIBSBML_EXTERN
Compartment_t *
Model_createCompartment (Model_t *m)
{
  return m->createCompartment();
}


/**
 * Creates a new Species_t structure inside the given Model_t and returns a
 * pointer to it.
 *
 * @param m the Model_t structure
 *
 * @return the Species_t structure created
 */
LIBSBML_EXTERN
Species_t *
Model_createSpecies (Model_t *m)
{
  return m->createSpecies();
}


/**
 * Creates a new Parameter_t structure inside the given Model_t and returns
 * a pointer to it.
 *
 * @param m the Model_t structure
 *
 * @return the Parameter_t structure created
 */
LIBSBML_EXTERN
Parameter_t *
Model_createParameter (Model_t *m)
{
  return m->createParameter();
}


/**
 * Creates a new InitialAssignment_t structure inside the given Model_t
 * structure and returns it.
 *
 * @param m the Model_t structure
 *
 * @return the InitialAssignment_t structure created
 */
LIBSBML_EXTERN
InitialAssignment_t *
Model_createInitialAssignment (Model_t *m)
{
  return m->createInitialAssignment();
}


/**
 * Creates a new AlgebraicRule_t structure inside the given Model_t
 * structure and returns a pointer to it.
 *
 * @param m the Model_t structure
 *
 * @return the AlgebraicRule_t structure created.
 */
LIBSBML_EXTERN
Rule_t *
Model_createAlgebraicRule (Model_t *m)
{
  return m->createAlgebraicRule();
}


/**
 * Creates a new AssignmentRule_t structure inside the given Model_t
 * structure and returns a pointer to it.
 *
 * @param m the Model_t structure
 *
 * @return the AssignmentRule_t structure created
 */
LIBSBML_EXTERN
Rule_t *
Model_createAssignmentRule (Model_t *m)
{
  return m->createAssignmentRule();
}


/**
 * Creates a new RateRule_t structure inside the given Model_t structure
 * and returns a pointer to it.
 *
 * @param m the Model_t structure
 *
 * @return the RateRule_t structure created.
 */
LIBSBML_EXTERN
Rule_t *
Model_createRateRule (Model_t *m)
{
  return m->createRateRule();
}


/**
 * Creates a new Constraint_t structure inside the given Model_t structure
 * and returns it.
 *
 * @param m the Model_t structure
 *
 * @return the Constraint_t structure created.
 */
LIBSBML_EXTERN
Constraint_t *
Model_createConstraint (Model_t *m)
{
  return m->createConstraint();
}

/**
 * Creates a new Reaction_t structure inside the given Model_t structure
 * and returns a pointer to it.
 *
 * @param m the Model_t structure
 *
 * @return the Reaction_t structure created.
 */
LIBSBML_EXTERN
Reaction_t *
Model_createReaction (Model_t *m)
{
  return m->createReaction();
}


/**
 * Creates a new SpeciesReference_t structure for a reactant inside the
 * last Reaction_t structure in the given Model_t structure, and returns a
 * pointer to it.
 *
 * The mechanism by which the last Reaction_t structure was created and
 * added to @p m is not significant.  It could have been created in a
 * variety of ways, for example using Model_createReaction().  If a
 * Reaction_t structure does not exist, a new SpeciesReference_t structure
 * is @em not created and NULL is returned instead.
 *
 * @param m the Model_t structure
 * 
 * @return the SpeciesReference object created
 */
LIBSBML_EXTERN
SpeciesReference_t *
Model_createReactant (Model_t *m)
{
  return m->createReactant();
}


/**
 * Creates a new SpeciesReference_t structure for a product inside the
 * last Reaction_t structure in the given Model_t structure, and returns a
 * pointer to it.
 *
 * The mechanism by which the last Reaction_t structure was created and
 * added to @p m is not significant.  It could have been created in a
 * variety of ways, for example using Model_createReaction().  If a
 * Reaction_t structure does not exist, a new SpeciesReference_t structure
 * is @em not created and NULL is returned instead.
 *
 * @param m the Model_t structure
 * 
 * @return the SpeciesReference object created
 */
LIBSBML_EXTERN
SpeciesReference_t *
Model_createProduct (Model_t *m)
{
  return m->createProduct();
}


/**
 * Creates a new ModifierSpeciesReference_t structure for a reactant inside
 * the last Reaction_t structure in the given Model_t structure, and
 * returns a pointer to it.
 *
 * The mechanism by which the last Reaction_t structure was created and
 * added to @p m is not significant.  It could have been created in a
 * variety of ways, for example using Model_createReaction().  If a
 * Reaction_t structure does not exist, a new ModifierSpeciesReference_t
 * structure is @em not created and NULL is returned instead.
 *
 * @param m the Model_t structure
 * 
 * @return the ModifierSpeciesReference object created
 */
LIBSBML_EXTERN
SpeciesReference_t *
Model_createModifier (Model_t *m)
{
  return static_cast<SpeciesReference_t*>( m->createModifier() );
}


/**
 * Creates a new KineticLaw_t structure inside the last Reaction_t
 * structure in the given Model_t structure, and returns a pointer to it.
 *
 * The mechanism by which the last Reaction_t structure was created and
 * added to @p m is not significant.  It could have been created in a
 * variety of ways, for example using Model_createReaction().  If a
 * Reaction_t structure does not exist for the model, or a Reaction_t
 * structure exists but already contains a KineticLaw_t structure, a new
 * KineticLaw_t is @em not created and NULL is returned instead.
 *
 * @param m the Model_t structure
 * 
 * @return the KineticLaw object created
 */
LIBSBML_EXTERN
KineticLaw_t *
Model_createKineticLaw (Model_t *m)
{
  return m->createKineticLaw();
}


/**
 * Creates a new local Parameter_t structure inside the KineticLaw_t
 * structure of the last Reaction_t structure created inside the given
 * model, and returns a pointer to it.
 *
 * The last KineticLaw_t structure could have been created in a variety of
 * ways.  For example, it could have been added using
 * Model_createKineticLaw(), or it could be the result of using
 * Reaction_createKineticLaw() on the Reaction_t structure created by a
 * Model_createReaction().  If a Reaction_t structure does not exist for
 * this model, or the last Reaction_t structure does not contain a
 * KineticLaw_t structure, a new Parameter_t is @em not created and NULL is
 * returned instead.
 *
 * @param m the Model_t structure
 *
 * @return the Parameter object created
 */
LIBSBML_EXTERN
Parameter_t *
Model_createKineticLawParameter (Model_t *m)
{
  return m->createKineticLawParameter();
}


/**
 * Creates a new Event inside the given Model_t structure and returns a
 * pointer to it.
 *
 * @param m the Model_t structure
 *
 * @return a new Event_t structure
 */
LIBSBML_EXTERN
Event_t *
Model_createEvent (Model_t *m)
{
  return m->createEvent();
}


/**
 * Creates a new EventAssignment_t structure inside the last Event_t
 * structure created in the given Model_t structure, and returns a pointer
 * to it.
 *
 * The mechanism by which the last Event_t structure was created is not
 * significant.  It could have been created in a variety of ways, for
 * example by using Model_createEvent().  If an Event_t structure does not
 * exist, a new EventAssignment_t structure is @em not created and NULL is
 * returned instead.
 *
 * @param m the Model_t structure
 *
 * @return the new EventAssignment_t structure
 */
LIBSBML_EXTERN
EventAssignment_t *
Model_createEventAssignment (Model_t *m)
{
  return m->createEventAssignment();
}


/**
 * Get the list of FunctionDefinition_t structures contained in the given
 * Model_t structure.
 *
 * @param m the Model_t structure
 *
 * @return the list of FunctionDefinition_t structures
 */
LIBSBML_EXTERN
ListOf_t *
Model_getListOfFunctionDefinitions (Model_t *m)
{
  return m->getListOfFunctionDefinitions();
}


/**
 * Get the list of UnitDefinition_t structures contained in the given
 * Model_t structure.
 *
 * @param m the Model_t structure
 *
 * @return the list of UnitDefinition_t structures.
 */
LIBSBML_EXTERN
ListOf_t *
Model_getListOfUnitDefinitions (Model_t *m)
{
  return m->getListOfUnitDefinitions();
}


/**
 * Get the list of CompartmentType_t structures contained in the given
 * Model_t structure.
 *
 * @param m the Model_t structure
 *
 * @return the list of CompartmentType_t structures.
 */
LIBSBML_EXTERN
ListOf_t *
Model_getListOfCompartmentTypes (Model_t *m)
{
  return m->getListOfCompartmentTypes();
}


/**
 * Get the list of SpeciesType_t structures contained in the given Model_t
 * structure.
 *
 * @param m the Model_t structure
 *
 * @return the list of SpeciesType_t structures.
 */
LIBSBML_EXTERN
ListOf_t *
Model_getListOfSpeciesTypes (Model_t *m)
{
  return m->getListOfSpeciesTypes();
}


/**
 * Get the list of Compartment_t structures contained in the given Model_t
 * structure.
 *
 * @param m the Model_t structure
 *
 * @return the list of Compartment_t structures.
 */
LIBSBML_EXTERN
ListOf_t *
Model_getListOfCompartments (Model_t *m)
{
  return m->getListOfCompartments();
}


/**
 * Get the list of Species_t structures contained in the given Model_t
 * structure.
 *
 * @param m the Model_t structure
 *
 * @return the list of Specie_t structures.
 */
LIBSBML_EXTERN
ListOf_t *
Model_getListOfSpecies (Model_t *m)
{
  return m->getListOfSpecies();
}


/**
 * Get the list of Parameter_t structures contained in the given Model_t
 * structure.
 *
 * @param m the Model_t structure
 *
 * @return the list of Parameter_t structures.
 */
LIBSBML_EXTERN
ListOf_t *
Model_getListOfParameters (Model_t *m)
{
  return m->getListOfParameters();
}


/**
 * Get the list of InitialAssignment_t structures contained in the given
 * Model_t structure.
 *
 * @param m the Model_t structure
 *
 * @return the list of InitialAssignment_t structures.
 */
LIBSBML_EXTERN
ListOf_t *
Model_getListOfInitialAssignments (Model_t* m)
{
  return m->getListOfInitialAssignments();
}


/**
 * Get the list of Rule_t structures contained in the given Model_t
 * structure.
 *
 * @param m the Model_t structure
 *
 * @return the list of Rule_t structures.
 */
LIBSBML_EXTERN
ListOf_t *
Model_getListOfRules (Model_t *m)
{
  return m->getListOfRules();
}


/**
 * Get the list of Constraint_t structures contained in the given Model_t
 * structure.
 *
 * @param m the Model_t structure
 *
 * @return the list of Constraint_t structures.
 */
LIBSBML_EXTERN
ListOf_t *
Model_getListOfConstraints (Model_t* m)
{
  return m->getListOfConstraints();
}


/**
 * Get the list of Reaction_t structures contained in the given Model_t
 * structure.
 *
 * @param m the Model_t structure
 *
 * @return the list of Reaction_t structures.
 */
LIBSBML_EXTERN
ListOf_t *
Model_getListOfReactions (Model_t *m)
{
  return m->getListOfReactions();
}


/**
 * Get the list of Event_t structures contained in the given Model_t
 * structure.
 *
 * @param m the Model_t structure
 *
 * @return the list of Event_t structures.
 */
LIBSBML_EXTERN
ListOf_t *
Model_getListOfEvents (Model_t *m)
{
  return m->getListOfEvents();
}


/**
 * Get the nth FunctionDefinition_t structure contained in the given
 * Model_t structure.
 *
 * @param m the Model_t structure
 * @param n the integer index of the FunctionDefinition_t sought
 *
 * @return the FunctionDefinition_t if found, or NULL if not found
 */
LIBSBML_EXTERN
FunctionDefinition_t *
Model_getFunctionDefinition (Model_t *m, unsigned int n)
{
  return m->getFunctionDefinition(n);
}


/**
 * Get the FunctionDefinition_t structure whose identifier is @p sid in the
 * given Model_t structure.
 *
 * @param m the Model_t structure
 * @param sid the identifier of the FunctionDefinition_t structure sought
 *
 * @return the FunctionDefinition_t if found, or NULL if not found
 */
LIBSBML_EXTERN
FunctionDefinition_t *
Model_getFunctionDefinitionById (Model_t *m, const char *sid)
{
  return (sid != NULL) ? m->getFunctionDefinition(sid) : NULL;
}


/**
 * Get the nth UnitDefinition_t structure contained in the given
 * Model_t structure.
 *
 * @param m the Model_t structure
 * @param n the integer index of the UnitDefinition_t sought
 *
 * @return the UnitDefinition_t if found, or NULL if not found
 */
LIBSBML_EXTERN
UnitDefinition_t *
Model_getUnitDefinition (Model_t *m, unsigned int n)
{
  return m->getUnitDefinition(n);
}


/**
 * Get the UnitDefinition_t structure whose identifier is @p sid in the
 * given Model_t structure.
 *
 * @param m the Model_t structure
 * @param sid the identifier of the UnitDefinition_t structure sought
 *
 * @return the UnitDefinition_t if found, or NULL if not found
 */
LIBSBML_EXTERN
UnitDefinition_t *
Model_getUnitDefinitionById (Model_t *m, const char *sid)
{
  return (sid != NULL) ? m->getUnitDefinition(sid) : NULL;
}


/**
 * Get the nth CompartmentType_t structure contained in the given
 * Model_t structure.
 *
 * @param m the Model_t structure
 * @param n the integer index of the CompartmentType_t sought
 *
 * @return the CompartmentType_t if found, or NULL if not found
 */
LIBSBML_EXTERN
CompartmentType_t *
Model_getCompartmentType (Model_t *m, unsigned int n)
{
  return m->getCompartmentType(n);
}


/**
 * Get the CompartmentType_t structure whose identifier is @p sid in the
 * given Model_t structure.
 *
 * @param m the Model_t structure
 * @param sid the identifier of the CompartmentType_t structure sought
 *
 * @return the CompartmentType_t if found, or NULL if not found
 */
LIBSBML_EXTERN
CompartmentType_t *
Model_getCompartmentTypeById (Model_t *m, const char *sid)
{
  return (sid != NULL) ? m->getCompartmentType(sid) : NULL;
}


/**
 * Get the nth SpeciesType_t structure contained in the given
 * Model_t structure.
 *
 * @param m the Model_t structure
 * @param n the integer index of the SpeciesType_t sought
 *
 * @return the SpeciesType_t if found, or NULL if not found
 */
LIBSBML_EXTERN
SpeciesType_t *
Model_getSpeciesType (Model_t *m, unsigned int n)
{
  return m->getSpeciesType(n);
}


/**
 * Get the SpeciesType_t structure whose identifier is @p sid in the
 * given Model_t structure.
 *
 * @param m the Model_t structure
 * @param sid the identifier of the SpeciesType_t structure sought
 *
 * @return the SpeciesType_t if found, or NULL if not found
 */
LIBSBML_EXTERN
SpeciesType_t *
Model_getSpeciesTypeById (Model_t *m, const char *sid)
{
  return (sid != NULL) ? m->getSpeciesType(sid) : NULL;
}


/**
 * Get the nth Compartment_t structure contained in the given
 * Model_t structure.
 *
 * @param m the Model_t structure
 * @param n the integer index of the Compartment_t sought
 *
 * @return the Compartment_t if found, or NULL if not found
 */
LIBSBML_EXTERN
Compartment_t *
Model_getCompartment (Model_t *m, unsigned int n)
{
  return m->getCompartment(n);
}


/**
 * Get the Compartment_t structure whose identifier is @p sid in the
 * given Model_t structure.
 *
 * @param m the Model_t structure
 * @param sid the identifier of the Compartment_t structure sought
 *
 * @return the Compartment_t if found, or NULL if not found
 */
LIBSBML_EXTERN
Compartment_t *
Model_getCompartmentById (Model_t *m, const char *sid)
{
  return (sid != NULL) ? m->getCompartment(sid) : NULL;
}


/**
 * Get the nth Species_t structure contained in the given
 * Model_t structure.
 *
 * @param m the Model_t structure
 * @param n the integer index of the Species_t sought
 *
 * @return the Species_t if found, or NULL if not found
 */
LIBSBML_EXTERN
Species_t *
Model_getSpecies (Model_t *m, unsigned int n)
{
  return m->getSpecies(n);
}


/**
 * Get the Species_t structure whose identifier is @p sid in the
 * given Model_t structure.
 *
 * @param m the Model_t structure
 * @param sid the identifier of the Species_t structure sought
 *
 * @return the Species_t if found, or NULL if not found
 */
LIBSBML_EXTERN
Species_t *
Model_getSpeciesById (Model_t *m, const char *sid)
{
  return (sid != NULL) ? m->getSpecies(sid) : NULL;
}


/**
 * Get the nth Parameter_t structure contained in the given
 * Model_t structure.
 *
 * @param m the Model_t structure
 * @param n the integer index of the Parameter_t sought
 *
 * @return the Parameter_t if found, or NULL if not found
 */
LIBSBML_EXTERN
Parameter_t *
Model_getParameter (Model_t *m, unsigned int n)
{
  return m->getParameter(n);
}


/**
 * Get the Parameter_t structure whose identifier is @p sid in the
 * given Model_t structure.
 *
 * @param m the Model_t structure
 * @param sid the identifier of the Parameter_t structure sought
 *
 * @return the Parameter_t if found, or NULL if not found
 */
LIBSBML_EXTERN
Parameter_t *
Model_getParameterById (Model_t *m, const char *sid)
{
  return (sid != NULL) ? m->getParameter(sid) : NULL;
}


/**
 * Get the nth InitialAssignment_t structure contained in the given
 * Model_t structure.
 *
 * @param m the Model_t structure
 * @param n the integer index of the InitialAssignment_t sought
 *
 * @return the InitialAssignment_t if found, or NULL if not found
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
 *
 * @param m the Model_t structure
 */
LIBSBML_EXTERN
InitialAssignment_t *
Model_getInitialAssignmentBySym (Model_t *m, const char *symbol)
{
  return (symbol != NULL) ? m->getInitialAssignment(symbol) : NULL;
}


/**
 * Get the nth Rule_t structure contained in the given
 * Model_t structure.
 *
 * @param m the Model_t structure
 * @param n the integer index of the Rule_t sought
 *
 * @return the Rule_t if found, or NULL if not found
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
 *
 * @param m the Model_t structure
 */
LIBSBML_EXTERN
Rule_t *
Model_getRuleByVar (Model_t *m, const char *variable)
{
  return (variable != NULL) ? m->getRule(variable) : NULL;
}


/**
 * Get the nth Constraint_t structure contained in the given
 * Model_t structure.
 *
 * @param m the Model_t structure
 * @param n the integer index of the Constraint_t sought
 *
 * @return the Constraint_t if found, or NULL if not found
 */
LIBSBML_EXTERN
Constraint_t *
Model_getConstraint (Model_t *m, unsigned int n)
{
  return m->getConstraint(n);
}


/**
 * Get the nth Reaction_t structure contained in the given
 * Model_t structure.
 *
 * @param m the Model_t structure
 * @param n the integer index of the Reaction_t sought
 *
 * @return the Reaction_t if found, or NULL if not found
 */
LIBSBML_EXTERN
Reaction_t *
Model_getReaction (Model_t *m, unsigned int n)
{
  return m->getReaction(n);
}


/**
 * Get the Reaction_t structure whose identifier is @p sid in the
 * given Model_t structure.
 *
 * @param m the Model_t structure
 * @param sid the identifier of the Reaction_t structure sought
 *
 * @return the Reaction_t if found, or NULL if not found
 */
LIBSBML_EXTERN
Reaction_t *
Model_getReactionById (Model_t *m, const char *sid)
{
  return (sid != NULL) ? m->getReaction(sid) : NULL;
}


/**
 * Get the nth Event_t structure contained in the given
 * Model_t structure.
 *
 * @param m the Model_t structure
 * @param n the integer index of the Event_t sought
 *
 * @return the Event_t if found, or NULL if not found
 */
LIBSBML_EXTERN
Event_t *
Model_getEvent (Model_t *m, unsigned int n)
{
  return m->getEvent(n);
}


/**
 * Get the Event_t structure whose identifier is @p sid in the
 * given Model_t structure.
 *
 * @param m the Model_t structure
 * @param sid the identifier of the Event_t structure sought
 *
 * @return the Event_t if found, or NULL if not found
 */
LIBSBML_EXTERN
Event_t *
Model_getEventById (Model_t *m, const char *sid)
{
  return (sid != NULL) ? m->getEvent(sid) : NULL;
}


/**
 * Get the number of FunctionDefinition_t structures in the given
 * Model_t structure
 *
 * @param m the Model_t structure
 *
 * @return an unsigned integer as the count of FunctionDefinition_t
 * structures in @p m
 */
LIBSBML_EXTERN
unsigned int
Model_getNumFunctionDefinitions (const Model_t *m)
{
  return m->getNumFunctionDefinitions();
}


/**
 * Get the number of UnitDefinition_t structures in the given
 * Model_t structure
 *
 * @param m the Model_t structure
 *
 * @return an unsigned integer as the count of UnitDefinition_t
 * structures in @p m
 */
LIBSBML_EXTERN
unsigned int
Model_getNumUnitDefinitions (const Model_t *m)
{
  return m->getNumUnitDefinitions();
}


/**
 * Get the number of CompartmentType_t structures in the given
 * Model_t structure
 *
 * @param m the Model_t structure
 *
 * @return an unsigned integer as the count of CompartmentType_t
 * structures in @p m
 */
LIBSBML_EXTERN
unsigned int
Model_getNumCompartmentTypes (const Model_t *m)
{
  return m->getNumCompartmentTypes();
}


/**
 * Get the number of SpeciesType_t structures in the given
 * Model_t structure
 *
 * @param m the Model_t structure
 *
 * @return an unsigned integer as the count of SpeciesType_t
 * structures in @p m
 */
LIBSBML_EXTERN
unsigned int
Model_getNumSpeciesTypes (const Model_t *m)
{
  return m->getNumSpeciesTypes();
}


/**
 * Get the number of Compartment_t structures in the given
 * Model_t structure
 *
 * @param m the Model_t structure
 *
 * @return an unsigned integer as the count of Compartment_t
 * structures in @p m
 */
LIBSBML_EXTERN
unsigned int
Model_getNumCompartments (const Model_t *m)
{
  return m->getNumCompartments();
}


/**
 * Get the number of Specie_t structures in the given
 * Model_t structure
 *
 * @param m the Model_t structure
 *
 * @return an unsigned integer as the count of Specie_t
 * structures in @p m
 */
LIBSBML_EXTERN
unsigned int
Model_getNumSpecies (const Model_t *m)
{
  return m->getNumSpecies();
}


/**
 * Get the number of Species_t structure in this Model_t structure having
 * nonzero values for their "boundaryCondition" attribute.
 *
 * @param m the Model_t structure
 *
 * @return an unsigned integer as the count of Species_t structures
 */
LIBSBML_EXTERN
unsigned int
Model_getNumSpeciesWithBoundaryCondition (const Model_t *m)
{
  return m->getNumSpeciesWithBoundaryCondition();
}


/**
 * Get the number of Parameter_t structures in the given
 * Model_t structure
 *
 * @param m the Model_t structure
 *
 * @return an unsigned integer as the count of Parameter_t
 * structures in @p m
 */
LIBSBML_EXTERN
unsigned int
Model_getNumParameters (const Model_t *m)
{
  return m->getNumParameters();
}


/**
 * Get the number of InitialAssignment_t structures in the given
 * Model_t structure
 *
 * @param m the Model_t structure
 *
 * @return an unsigned integer as the count of InitialAssignment_t
 * structures in @p m
 */
LIBSBML_EXTERN
unsigned int
Model_getNumInitialAssignments (const Model_t *m)
{
  return m->getNumInitialAssignments();
}


/**
 * Get the number of Rule_t structures in the given
 * Model_t structure
 *
 * @param m the Model_t structure
 *
 * @return an unsigned integer as the count of Rule_t
 * structures in @p m
 */
LIBSBML_EXTERN
unsigned int
Model_getNumRules (const Model_t *m)
{
  return m->getNumRules();
}


/**
 * Get the number of Constraint_t structures in the given
 * Model_t structure
 *
 * @param m the Model_t structure
 *
 * @return an unsigned integer as the count of Constraint_t
 * structures in @p m
 */
LIBSBML_EXTERN
unsigned int
Model_getNumConstraints (const Model_t *m)
{
  return m->getNumConstraints();
}

/**
 * Get the number of Reaction_t structures in the given
 * Model_t structure
 *
 * @param m the Model_t structure
 *
 * @return an unsigned integer as the count of Reaction_t
 * structures in @p m
 */
LIBSBML_EXTERN
unsigned int
Model_getNumReactions (const Model_t *m)
{
  return m->getNumReactions();
}


/**
 * Get the number of Event_t structures in the given
 * Model_t structure
 *
 * @param m the Model_t structure
 *
 * @return an unsigned integer as the count of Event_t
 * structures in @p m
 */
LIBSBML_EXTERN
unsigned int
Model_getNumEvents (const Model_t *m)
{
  return m->getNumEvents();
}


#ifdef USE_LAYOUT  


/**
 * Get the list of Layout_t structures contained in the given Model_t
 * structure.
 *
 * @param m the Model_t structure
 *
 * @return the list of Layout_t structures.
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
 *
 * @param m the Model_t structure
 */
LIBSBML_EXTERN
Layout_t *
Model_getLayout (Model_t *m, unsigned int index)
{
  return m->getLayout(index);
}


/**
 * Adds a copy of a Layout_t structure to a given Model_t structure.
 *
 * @param m the Model_t structure
 * @param layout the Layout_t structure to copy and add
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
 *
 * @param m the Model_t structure
 */
LIBSBML_EXTERN
Layout_t *
Model_createLayout (Model_t *m)
{
  return m->createLayout();
}


#endif  /* USE_LAYOUT */


/** @endcond doxygen-c-only */
