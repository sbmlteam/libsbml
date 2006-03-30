/**
 * \file    Model.h
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


#ifndef Model_h
#define Model_h


#include "common/libsbml-config.h"
#include "common/extern.h"

#include "SBMLTypeCodes.h"


#ifdef __cplusplus


#include <string>

#include "FunctionDefinition.h"
#include "UnitDefinition.h"
#include "Compartment.h"
#include "Species.h"
#include "Parameter.h"
#include "Rule.h"
#include "Reaction.h"
#include "Event.h"


class SBMLVisitor;


#ifdef USE_LAYOUT
  class Layout;
#endif  /* USE_LAYOUT */


class LIBSBML_EXTERN Model : public SBase
{
public:

  /**
   * Creates a new Model, optionally with its id and name attributes set.
   */
  Model (const std::string& id = "", const std::string& name = "");

  /**
   * Destroys this Model.
   */
  virtual ~Model ();


  /**
   * Accepts the given SBMLVisitor.
   */
  virtual bool accept (SBMLVisitor& v) const;

  /**
   * @return a (deep) copy of this Model.
   */
  virtual SBase* clone () const;


  /**
   * @return the sboTerm of this KineticLaw as an integer.  If not set,
   * sboTerm will be -1.  Use SBML::sboTermToString() to convert the
   * sboTerm to a zero-padded, seven digit string.
   */
  int getSBOTerm () const;

  /**
   * @return true if the sboTerm of this KineticLaw has been set, false
   * otherwise.
   */
  bool isSetSBOTerm () const;

  /**
   * Sets the sboTerm field of this KineticLaw to value.
   */
  void setSBOTerm (int sboTerm);

  /**
   * Unsets the sboTerm of this KineticLaw.
   */
  void unsetSBOTerm ();


  /**
   * Adds a copy of the given FunctionDefinition to this Model.
   */
  void addFunctionDefinition (const FunctionDefinition* fd);

  /**
   * Adds a copy of the given UnitDefinition to this Model.
   */
  void addUnitDefinition (const UnitDefinition* ud);

  /**
   * Adds a copy of the given Compartment to this Model.
   */
  void addCompartment (const Compartment* c);

  /**
   * Adds a copy of the given Species to this Model.
   */
  void addSpecies (const Species* s);

  /**
   * Adds a copy of the given Parameter to this Model.
   */
  void addParameter (const Parameter* p);

  /**
   * Adds a copy of the given Rule to this Model.
   */
  void addRule (const Rule* r);

  /**
   * Adds a copy of the given Reaction to this Model.
   */
  void addReaction (const Reaction* r);

  /**
   * Adds a copy of the given Event to this Model.
   */
  void addEvent (const Event* e);


  /**
   * Creates a new FunctionDefinition inside this Model and returns it.
   */
  FunctionDefinition* createFunctionDefinition ();

  /**
   * Creates a new UnitDefinition inside this Model and returns it.
   */
  UnitDefinition* createUnitDefinition ();

  /**
   * Creates a new Unit inside this Model and returns a pointer to it.  The
   * Unit is added to the last UnitDefinition created.
   *
   * If a UnitDefinitions does not exist for this model, a new Unit is not
   * created and NULL is returned.
   */
  Unit* createUnit ();

  /**
   * Creates a new Compartment inside this Model and returns it.
   */
  Compartment* createCompartment ();

  /**
   * Creates a new Species inside this Model and returns it.
   */
  Species* createSpecies ();

  /**
   * Creates a new Parameter inside this Model and returns it.
   */
  Parameter* createParameter ();

  /**
   * Creates a new AlgebraicRule inside this Model and returns it.
   */
  AlgebraicRule* createAlgebraicRule ();

  /**
   * Creates a new AssignmentRule inside this Model and returns it.
   */
  AssignmentRule* createAssignmentRule ();

  /**
   * Creates a new RateRule inside this Model and returns it.
   */
  RateRule* createRateRule ();

  /**
   * Creates a new Reaction inside this Model and returns it.
   */
  Reaction* createReaction ();

  /**
   * Creates a new Reactant (i.e. SpeciesReference) inside this Model and
   * returns a pointer to it.  The SpeciesReference is added to the
   * reactants of the last Reaction created.
   *
   * If a Reaction does not exist for this model, a new SpeciesReference is
   * not created and NULL is returned.
   */
  SpeciesReference* createReactant ();

  /**
   * Creates a new Product (i.e. SpeciesReference) inside this Model and
   * returns a pointer to it.  The SpeciesReference is added to the
   * products of the last Reaction created.
   *
   * If a Reaction does not exist for this model, a new SpeciesReference is
   * not created and NULL is returned.
   */
  SpeciesReference* createProduct ();

  /**
   * Creates a new Modifer (i.e. ModifierSpeciesReference) inside this
   * Model and returns a pointer to it.  The ModifierSpeciesReference is
   * added to the modifiers of the last Reaction created.
   *
   * If a Reaction does not exist for this model, a new
   * ModifierSpeciesReference is not created and NULL is returned.
   */
  ModifierSpeciesReference* createModifier ();

  /**
   * Creates a new KineticLaw inside this Model and returns a pointer to
   * it.  The KineticLaw is associated with the last Reaction created.
   *
   * If a Reaction does not exist for this model, or a Reaction does exist,
   * but already has a KineticLaw, a new KineticLaw is not created and NULL
   * is returned.
   */
  KineticLaw* createKineticLaw ();

  /**
   * Creates a new Parameter (of a KineticLaw) inside this Model and
   * returns a pointer to it.  The Parameter is associated with the
   * KineticLaw of the last Reaction created.
   *
   * If a Reaction does not exist for this model, or a KineticLaw for the
   * Reaction, a new Parameter is not created and NULL is returned.
   */
  Parameter* createKineticLawParameter ();

  /**
   * Creates a new Event inside this Model and returns it.
   */
  Event* createEvent ();

  /**
   * Creates a new EventAssignment inside this Model and returns a pointer
   * to it.  The EventAssignment is added to the the last Event created.
   *
   * If an Event does not exist for this model, a new EventAssignment is
   * not created and NULL is returned.
   */
  EventAssignment* createEventAssignment ();


  /**
   * @return the list of FunctionDefinitions for this Model.
   */
  const ListOf* getListOfFunctionDefinitions () const;

  /**
   * @return the list of FunctionDefinitions for this Model.
   */
  ListOf* getListOfFunctionDefinitions ();

  /**
   * @return the list of UnitDefinitions for this Model.
   */
  const ListOf* getListOfUnitDefinitions () const;

  /**
   * @return the list of UnitDefinitions for this Model.
   */
  ListOf* getListOfUnitDefinitions ();

  /**
   * @return the list of Compartments for this Model.
   */
  const ListOf* getListOfCompartments () const;

  /**
   * @return the list of Compartments for this Model.
   */
  ListOf* getListOfCompartments ();

  /**
   * @return the list of Species for this Model.
   */
  const ListOf* getListOfSpecies () const;

  /**
   * @return the list of Species for this Model.
   */
  ListOf* getListOfSpecies ();

  /**
   * @return the list of Parameters for this Model.
   */
  const ListOf* getListOfParameters () const;

  /**
   * @return the list of Parameters for this Model.
   */
  ListOf* getListOfParameters ();

  /**
   * @return the list of Rules for this Model.
   */
  const ListOf* getListOfRules () const;

  /**
   * @return the list of Rules for this Model.
   */
  ListOf* getListOfRules ();

  /**
   * @return the list of Reactions for this Model.
   */
  const ListOf* getListOfReactions () const;

  /**
   * @return the list of Reactions for this Model.
   */
  ListOf* getListOfReactions ();

  /**
   * @return the list of Events for this Model.
   */
  const ListOf* getListOfEvents () const;

  /**
   * @return the list of Events for this Model.
   */
  ListOf* getListOfEvents ();


  /**
   * @return the nth FunctionDefinition of this Model.
   */
  const FunctionDefinition* getFunctionDefinition (unsigned int n) const;

  /**
   * @return the nth FunctionDefinition of this Model.
   */
  FunctionDefinition* getFunctionDefinition (unsigned int n);

  /**
   * @return the FunctionDefinition in this Model with the given id or NULL
   * if no such FunctionDefinition exists.
   */
  const FunctionDefinition*
  getFunctionDefinition (const std::string& sid) const;

  /**
   * @return the FunctionDefinition in this Model with the given id or NULL
   * if no such FunctionDefinition exists.
   */
  FunctionDefinition* getFunctionDefinition (const std::string& sid);


  /**
   * @return the nth UnitDefinition of this Model.
   */
  const UnitDefinition* getUnitDefinition (unsigned int n) const;

  /**
   * @return the nth UnitDefinition of this Model.
   */
  UnitDefinition* getUnitDefinition (unsigned int n);

  /**
   * @return the UnitDefinition in this Model with the given id or NULL if
   * no such UnitDefinition exists.
   */
  const UnitDefinition* getUnitDefinition (const std::string& sid) const;

  /**
   * @return the UnitDefinition in this Model with the given id or NULL if
   * no such UnitDefinition exists.
   */
  UnitDefinition* getUnitDefinition (const std::string& sid);


  /**
   * @return the nth Compartment of this Model.
   */
  const Compartment* getCompartment (unsigned int n) const;

  /**
   * @return the nth Compartment of this Model.
   */
  Compartment* getCompartment (unsigned int n);

  /**
   * @return the Compartment in this Model with the given id or NULL if no
   * such Compartment exists.
   */
  const Compartment* getCompartment (const std::string& sid) const;

  /**
   * @return the Compartment in this Model with the given id or NULL if no
   * such Compartment exists.
   */
  Compartment* getCompartment (const std::string& sid);


  /**
   * @return the nth Species of this Model.
   */
  const Species* getSpecies (unsigned int n) const;

  /**
   * @return the nth Species of this Model.
   */
  Species* getSpecies (unsigned int n);

  /**
   * @return the Species in this Model with the given id or NULL if no such
   * Species exists.
   */
  const Species* getSpecies (const std::string& sid) const;

  /**
   * @return the Species in this Model with the given id or NULL if no such
   * Species exists.
   */
  Species* getSpecies (const std::string& sid);


  /**
   * @return the nth Parameter of this Model.
   */
  const Parameter* getParameter (unsigned int n) const;

  /**
   * @return the nth Parameter of this Model.
   */
  Parameter* getParameter (unsigned int n);

  /**
   * @return the Parameter in this Model with the given id or NULL if no
   * such Parameter exists.
   */
  const Parameter* getParameter (const std::string& sid) const;

  /**
   * @return the Parameter in this Model with the given id or NULL if no
   * such Parameter exists.
   */
  Parameter* getParameter (const std::string& sid);


  /**
   * @return the nth Rule of this Model.
   */
  const Rule* getRule (unsigned int n) const;

  /**
   * @return the nth Rule of this Model.
   */
  Rule* getRule (unsigned int n);


  /**
   * @return the nth Reaction of this Model.
   */
  const Reaction* getReaction (unsigned int n) const;

  /**
   * @return the nth Reaction of this Model.
   */
  Reaction* getReaction (unsigned int n);

  /**
   * @return the Reaction in this Model with the given id or NULL if no
   * such Reaction exists.
   */
  const Reaction* getReaction (const std::string& sid) const;

  /**
   * @return the Reaction in this Model with the given id or NULL if no
   * such Reaction exists.
   */
  Reaction* getReaction (const std::string& sid);


  /**
   * @return the nth Event of this Model.
   */
  const Event* getEvent (unsigned int n) const;

  /**
   * @return the nth Event of this Model.
   */
  Event* getEvent (unsigned int n);

  /**
   * @return the Event in this Model with the given id or NULL if no such
   * Event exists.
   */
  const Event* getEvent (const std::string& sid) const;

  /**
   * @return the Event in this Model with the given id or NULL if no such
   * Event exists.
   */
  Event* getEvent (const std::string& sid);


  /**
   * @return the number of FunctionDefinitions in this Model.
   */
  unsigned int getNumFunctionDefinitions () const;

  /**
   * @return the number of UnitDefinitions in this Model.
   */
  unsigned int getNumUnitDefinitions () const;

  /**
   * @return the number of Compartments in this Model.
   */
  unsigned int getNumCompartments () const;

  /**
   * @return the number of Species in this Model.
   */
  unsigned int getNumSpecies () const;

  /**
   * @return the number of Species in this Model with boundaryCondition set
   * to true.
   */
  unsigned int getNumSpeciesWithBoundaryCondition () const;

  /**
   * @return the number of Parameters in this Model.  Parameters defined in
   * KineticLaws are not included.
   */
  unsigned int getNumParameters () const;

  /**
   * @return the number of Rules in this Model.
   */
  unsigned int getNumRules () const;

  /**
   * @return the number of Reactions in this Model.
   */
  unsigned int getNumReactions () const;

  /**
   * @return the number of Events in this Model.
   */
  unsigned int getNumEvents () const;


  /**
   * @return true if the given ASTNode is a boolean.  Often times, this
   * question can be answered with the ASTNode's own isBoolean() method,
   * but if the AST is an expression that calls a function defined in the
   * Model's ListOf FunctionDefinitions, the model is needed for lookup
   * context.
   */
  bool isBoolean (const ASTNode* node) const;

  /**
   * @return the SBMLTypeCode_t of this SBML object or SBML_UNKNOWN
   * (default).
   *
   * @see getElementName()
   */
  virtual SBMLTypeCode_t getTypeCode () const;

  /**
   * Subclasses should override this method to return XML element name of
   * this SBML object.
   */
  virtual const std::string& getElementName () const;

  /**
   * Subclasses should override this method to write out their contained
   * SBML objects as XML elements.  Be sure to call your parents
   * implementation of this method as well.
   */
  virtual void writeElements (XMLOutputStream& stream);


#ifdef USE_LAYOUT

  /**
   * Returns the ListOf Layouts for this Model.
   */
  const ListOf* getListOfLayouts () const;

  /**
   * Returns the ListOf Layouts for this Model.
   */
  ListOf* getListOfLayouts ();

  /**
   * Returns the layout object that belongs to the given index. If the
   * index is invalid, NULL is returned.
   */
  const Layout* getLayout (unsigned int index) const;

  /**
   * Returns the layout object that belongs to the given index. If the
   * index is invalid, NULL is returned.
   */
  Layout* getLayout (unsigned int index);

  /**
   * Adds a copy of the layout object to the list of layouts.
   */ 
  void addLayout (const Layout* layout);

  /**
   * Creates a new layout object and adds it to the list of layout objects
   * and returns it.
   */
  Layout* createLayout();

#endif  /* USE_LAYOUT */  


protected:

  /**
   * @return the SBML object corresponding to next XMLToken in the
   * XMLInputStream or NULL if the token was not recognized.
   */
  virtual SBase* createObject (XMLInputStream& stream);

  /**
   * Subclasses should override this method to read values from the given
   * XMLAttributes set into their specific fields.  Be sure to call your
   * parents implementation of this method as well.
   */
  virtual void readAttributes (const XMLAttributes& attributes);

  /**
   * Subclasses should override this method to write their XML attributes
   * to the XMLOutputStream.  Be sure to call your parents implementation
   * of this method as well.
   */
  virtual void writeAttributes (XMLOutputStream& stream);


  int mSBOTerm;

  ListOfFunctionDefinitions  mFunctionDefinitions;
  ListOfUnitDefinitions      mUnitDefinitions;
  ListOfCompartments         mCompartments;
  ListOfSpecies              mSpecies;
  ListOfParameters           mParameters;
  ListOfRules                mRules;
  ListOfReactions            mReactions;
  ListOfEvents               mEvents;

#ifdef USE_LAYOUT
  ListOf mLayouts;
#endif  /* USE_LAYOUT */
};


#endif  /* __cplusplus */


#ifndef SWIG


BEGIN_C_DECLS


#include "common/sbmlfwd.h"


/**
 * Creates a new Model and returns a pointer to it.
 */
LIBSBML_EXTERN
Model_t *
Model_create (void);

/**
 * Creates a new Model with the given id and returns a pointer to it.
 */
LIBSBML_EXTERN
Model_t *
Model_createWith (const char *sid);

/**
 * Frees the given Model.
 */
LIBSBML_EXTERN
void
Model_free (Model_t *m);

/**
 * @return a (deep) copy of this Model.
 */
LIBSBML_EXTERN
Model_t *
Model_clone (const Model_t *m);


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
 * @return the sboTerm of this Reaction as an integer.  If not set,
 * sboTerm will be -1.  Use SBML_sboTermToString() to convert the
 * sboTerm to a zero-padded, seven digit string.
 */
LIBSBML_EXTERN
int
Model_getSBOTerm (const Model_t *m);


/**
 * @return true (non-zero) if the id of this Model has been set, false (0)
 * otherwise.
 */
LIBSBML_EXTERN
int
Model_isSetId (const Model_t *m);

/**
 * @return true (non-zero) if the name of this Model has been set, false
 * (0) otherwise.
 */
LIBSBML_EXTERN
int
Model_isSetName (const Model_t *m);

/**
 * @return true (non-zero) if the sboTerm of this Model has been set, false
 * (0) otherwise.
 */
LIBSBML_EXTERN
int
Model_isSetSBOTerm (const Model_t *m);


/**
 * Sets the id of this Model to a copy of sid.
 */
LIBSBML_EXTERN
void
Model_setId (Model_t *m, const char *sid);

/**
 * Sets the name of this Model to a copy of name.
 */
LIBSBML_EXTERN
void
Model_setName (Model_t *m, const char *name);

/**
 * Sets the sboTerm field of this Model to value.
 */
LIBSBML_EXTERN
void
Model_setSBOTerm (Model_t *m, int sboTerm);


/**
 * Unsets the id of this Model.
 */
LIBSBML_EXTERN
void
Model_unsetId (Model_t *m);

/**
 * Unsets the name of this Model.
 */
LIBSBML_EXTERN
void
Model_unsetName (Model_t *m);

/**
 * Unsets the sboTerm of this Model.
 */
LIBSBML_EXTERN
void
Model_unsetSBOTerm (Model_t *m);


/**
 * Adds a copy of the given FunctionDefinition to this Model.
 */
LIBSBML_EXTERN
void
Model_addFunctionDefinition (Model_t *m, const FunctionDefinition_t *fd);

/**
 * Adds a copy of the given UnitDefinition to this Model.
 */
LIBSBML_EXTERN
void
Model_addUnitDefinition (Model_t *m, const UnitDefinition_t *ud);

/**
 * Adds a copy of the given Compartment to this Model.
 */
LIBSBML_EXTERN
void
Model_addCompartment (Model_t *m, const Compartment_t *c);

/**
 * Adds a copy of the given Species to this Model.
 */
LIBSBML_EXTERN
void
Model_addSpecies (Model_t *m, const Species_t *s);

/**
 * Adds a copy of the given Parameter to this Model.
 */
LIBSBML_EXTERN
void
Model_addParameter (Model_t *m, const Parameter_t *p);

/**
 * Adds a copy of the given Rule to this Model.
 */
LIBSBML_EXTERN
void
Model_addRule (Model_t *m, const Rule_t *r);

/**
 * Adds a copy of the given Reaction to this Model.
 */
LIBSBML_EXTERN
void
Model_addReaction (Model_t *m, const Reaction_t *r);

/**
 * Adds a copy of the given Event to this Model.
 */
LIBSBML_EXTERN
void
Model_addEvent (Model_t *m, const Event_t *e);


/**
 * Creates a new FunctionDefinition inside this Model and returns a pointer
 * to it.
 */
LIBSBML_EXTERN
FunctionDefinition_t *
Model_createFunctionDefinition (Model_t *m);

/**
 * Creates a new UnitDefinition inside this Model and returns a pointer to
 * it.
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
 */
LIBSBML_EXTERN
Compartment_t *
Model_createCompartment (Model_t *m);

/**
 * Creates a new Species inside this Model and returns a pointer to it.
 */
LIBSBML_EXTERN
Species_t *
Model_createSpecies (Model_t *m);

/**
 * Creates a new Parameter inside this Model and returns a pointer to it.
 */
LIBSBML_EXTERN
Parameter_t *
Model_createParameter (Model_t *m);

/**
 * Creates a new AlgebraicRule inside this Model and returns a pointer to
 * it.
 */
LIBSBML_EXTERN
AlgebraicRule_t *
Model_createAlgebraicRule (Model_t *m);

/**
 * Creates a new AssignmentRule inside this Model and returns a pointer to
 * it.
 */
LIBSBML_EXTERN
AssignmentRule_t *
Model_createAssignmentRule (Model_t *m);

/**
 * Creates a new RateRule inside this Model and returns a pointer to it.
 */
LIBSBML_EXTERN
RateRule_t *
Model_createRateRule (Model_t *m);

/**
 * Creates a new Reaction inside this Model and returns a pointer to it.
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
SpeciesReference_t *
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
 * @return the list of FunctionDefinitions for this Model.
 */
LIBSBML_EXTERN
ListOf_t *
Model_getListOfFunctionDefinitions (Model_t *m);

/**
 * @return the list of UnitDefinitions for this Model.
 */
LIBSBML_EXTERN
ListOf_t *
Model_getListOfUnitDefinitions (Model_t *m);

/**
 * @return the list of Compartments for this Model.
 */
LIBSBML_EXTERN
ListOf_t *
Model_getListOfCompartments (Model_t *m);

/**
 * @return the list of Species for this Model.
 */
LIBSBML_EXTERN
ListOf_t *
Model_getListOfSpecies (Model_t *m);

/**
 * @return the list of Parameters for this Model.
 */
LIBSBML_EXTERN
ListOf_t *
Model_getListOfParameters (Model_t *m);

/**
 * @return the list of Rules for this Model.
 */
LIBSBML_EXTERN
ListOf_t *
Model_getListOfRules (Model_t *m);

/**
 * @return the list of Rules for this Model.
 */
LIBSBML_EXTERN
ListOf_t *
Model_getListOfReactions (Model_t *m);

/**
 * @return the list of Rules for this Model.
 */
LIBSBML_EXTERN
ListOf_t *
Model_getListOfEvents (Model_t *m);


/**
 * @return the nth FunctionDefinition of this Model.
 */
LIBSBML_EXTERN
FunctionDefinition_t *
Model_getFunctionDefinition (Model_t *m, unsigned int n);

/**
 * @return the FunctionDefinition in this Model with the given id or NULL
 * if no such FunctionDefinition exists.
 */
LIBSBML_EXTERN
FunctionDefinition_t *
Model_getFunctionDefinitionById (Model_t *m, const char *sid);

/**
 * @return the nth UnitDefinition of this Model.
 */
LIBSBML_EXTERN
UnitDefinition_t *
Model_getUnitDefinition (Model_t *m, unsigned int n);

/**
 * @return the UnitDefinition in this Model with the given id or NULL if
 * no such UnitDefinition exists.
 */
LIBSBML_EXTERN
UnitDefinition_t *
Model_getUnitDefinitionById (Model_t *m, const char *sid);

/**
 * @return the nth Compartment of this Model.
 */
LIBSBML_EXTERN
Compartment_t *
Model_getCompartment (Model_t *m, unsigned int n);

/**
 * @return the Compartment in this Model with the given id or NULL if no
 * such Compartment exists.
 */
LIBSBML_EXTERN
Compartment_t *
Model_getCompartmentById (Model_t *m, const char *sid);

/**
 * @return the nth Species of this Model.
 */
LIBSBML_EXTERN
Species_t *
Model_getSpecies (Model_t *m, unsigned int n);

/**
 * @return the Species in this Model with the given id or NULL if no such
 * Species exists.
 */
LIBSBML_EXTERN
Species_t *
Model_getSpeciesById (Model_t *m, const char *sid);

/**
 * @return the nth Parameter of this Model.
 */
LIBSBML_EXTERN
Parameter_t *
Model_getParameter (Model_t *m, unsigned int n);

/**
 * @return the Parameter in this Model with the given id or NULL if no such
 * Parameter exists.
 */
LIBSBML_EXTERN
Parameter_t *
Model_getParameterById (Model_t *m, const char *sid);

/**
 * @return the nth Rule of this Model.
 */
LIBSBML_EXTERN
Rule_t *
Model_getRule (Model_t *m, unsigned int n);

/**
 * @return the nth Reaction of this Model.
 */
LIBSBML_EXTERN
Reaction_t *
Model_getReaction (Model_t *m, unsigned int n);

/**
 * @return the Reaction in this Model with the given id or NULL if no such
 * Reaction exists.
 */
LIBSBML_EXTERN
Reaction_t *
Model_getReactionById (Model_t *m, const char *sid);

/**
 * @return the nth Event of this Model.
 */
LIBSBML_EXTERN
Event_t *
Model_getEvent (Model_t *m, unsigned int n);

/**
 * @return the Event in this Model with the given id or NULL if no such
 * Event exists.
 */
LIBSBML_EXTERN
Event_t *
Model_getEventById (Model_t *m, const char *sid);


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



#ifdef USE_LAYOUT


/**
 * Returns a reference to the ListOf object that holds the layouts.
 */
LIBSBML_EXTERN
ListOf_t *
Model_getListOfLayouts (Model_t *m);

/**
 * Returns the layout object that belongs to the given index. If the index
 * is invalid, NULL is returned.
 */
LIBSBML_EXTERN
Layout_t *
Model_getLayout (Model_t *m, unsigned int index);

/**
 * Adds a copy of the layout object to the list of layouts.
 */ 
LIBSBML_EXTERN
void 
Model_addLayout (Model_t *m, Layout_t *layout);

/**
 * Creates a new layout object and adds it to the list of layout objects.
 * A reference to the newly created object is returned.
 */
LIBSBML_EXTERN
Layout_t *
Model_createLayout (Model_t *m);


#endif /* USE_LAYOUT */


END_C_DECLS


#endif  /* !SWIG   */
#endif  /* Model_h */
