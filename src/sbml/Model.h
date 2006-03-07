/**
 * \file    Model.h
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


#ifndef Model_h
#define Model_h


#include "common/libsbml-config.h"
#include "common/extern.h"

#include "SBMLTypeCodes.h"


#ifdef __cplusplus


#include <string>

#include "SBase.h"
#include "ListOf.h"


class FunctionDefinition;
class UnitDefinition;
class Unit;
class Compartment;
class Species;
class Parameter;
class Rule;
class AssignmentRule;
class RateRule;
class AlgebraicRule;
class CompartmentVolumeRule;
class ParameterRule;
class SpeciesConcentrationRule;
class Reaction;
class SpeciesReference;
class ModifierSpeciesReference;
class KineticLaw;
class Event;
class EventAssignment;
class SBMLVisitor;

#ifdef USE_LAYOUT
  class Layout;
#endif  /* USE_LAYOUT */


class Model : public SBase
{
public:

  /**
   * Creates a new Model, optionally with its id and name attributes set.
   */
  LIBSBML_EXTERN
  Model (const std::string& id = "", const std::string& name = "");

  /**
   * Destroys this Model.
   */
  LIBSBML_EXTERN
  virtual ~Model ();


  /**
   * Accepts the given SBMLVisitor.
   */
  LIBSBML_EXTERN
  void accept (SBMLVisitor& v) const;

  /**
   * @return the id of this Model.
   */
  LIBSBML_EXTERN
  const std::string& getId () const;

  /**
   * @return the name of this Model.
   */
  LIBSBML_EXTERN
  const std::string& getName () const;

  /**
   * @return the SBML Level of this Model.
   */
  LIBSBML_EXTERN
  unsigned int getLevel () const;

  /**
   * @return the SBML Level of this Model.
   */
  LIBSBML_EXTERN
  unsigned int getVersion () const;


  /**
   * @return true if the id of this Model has been set, false otherwise.
   */
  LIBSBML_EXTERN
  bool isSetId () const;

  /**
   * @return true if the name of this Model has been set, false otherwise.
   */
  LIBSBML_EXTERN
  bool isSetName () const;

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
  void moveAllIdsToNames ();

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
  void moveAllNamesToIds ();

  /**
   * Moves the id field of this Model to its name field (iff name is not
   * already set).  This method is used for converting from L2 to L1.
   */
  LIBSBML_EXTERN
  void moveIdToName ();

  /**
   * Moves the name field of this Model to its id field (iff id is not
   * already set).  This method is used for converting from L1 to L2.
   */
  LIBSBML_EXTERN
  void moveNameToId ();

  /**
   * Sets the id of this Model to a copy of sid.
   */
  LIBSBML_EXTERN
  void setId (const std::string& sid);

  /**
   * Sets the name of this Model to a copy of string (SName in L1).
   */
  LIBSBML_EXTERN
  void setName (const std::string& str);

  /**
   * Unsets the id of this Model.
   */
  LIBSBML_EXTERN
  void unsetId ();

  /**
   * Unsets the name of this Model.
   */
  LIBSBML_EXTERN
  void unsetName ();

  /**
   * Creates a new FunctionDefinition inside this Model and returns it.
   * This covenience method is equivalent to:
   *
   *   addFunctionDefinition( FunctionDefinition() );
   */
  LIBSBML_EXTERN
  FunctionDefinition& createFunctionDefinition ();

  /**
   * Creates a new UnitDefinition inside this Model and returns it.  This
   * covenience method is equivalent to:
   *
   *   addUnitDefinition( UnitDefinition() );
   */
  LIBSBML_EXTERN
  UnitDefinition& createUnitDefinition ();

  /**
   * Creates a new Unit inside this Model and returns a pointer to it.  The
   * Unit is added to the last UnitDefinition created.
   *
   * If a UnitDefinitions does not exist for this model, a new Unit is not
   * created and NULL is returned.
   */
  LIBSBML_EXTERN
  Unit* createUnit ();

  /**
   * Creates a new Compartment inside this Model and returns it.  This
   * covenience method is equivalent to:
   *
   *   addCompartment( Compartment() );
   */
  LIBSBML_EXTERN
  Compartment& createCompartment ();

  /**
   * Creates a new Species inside this Model and returns .  This covenience
   * method is equivalent to:
   *
   *   addSpecies( Species() );
   */
  LIBSBML_EXTERN
  Species& createSpecies ();

  /**
   * Creates a new Parameter inside this Model and returns.  This
   * covenience method is equivalent to:
   *
   *   addParameter( Parameter() );
   */
  LIBSBML_EXTERN
  Parameter& createParameter ();

  /**
   * Creates a new AssignmentRule inside this Model and returns .  This
   * covenience method is equivalent to:
   *
   *   addRule( AssignmentRule() );
   *
   * (L2 only)
   */
  LIBSBML_EXTERN
  AssignmentRule& createAssignmentRule ();

  /**
   * Creates a new RateRule inside this Model and returns it.  This
   * covenience method is equivalent to:
   *
   *   addRule( RateRule() );
   *
   * (L2 only)
   */
  LIBSBML_EXTERN
  RateRule& createRateRule ();

  /**
   * Creates a new AlgebraicRule inside this Model and returns it.  This
   * covenience method is equivalent to:
   *
   *   addRule( AlgebraicRule() );
   */
  LIBSBML_EXTERN
  AlgebraicRule& createAlgebraicRule ();

  /**
   * Creates a new CompartmentVolumeRule inside this Model and returns.
   * This covenience method is equivalent to:
   *
   *   addRule( CompartmentVolumeRule() );
   */
  LIBSBML_EXTERN
  CompartmentVolumeRule& createCompartmentVolumeRule ();

  /**
   * Creates a new ParameterRule inside this Model and returns it.  This
   * covenience method is equivalent to:
   *
   *   addRule( ParameterRule() );
   */
  LIBSBML_EXTERN
  ParameterRule& createParameterRule ();

  /**
   * Creates a new SpeciesConcentrationRule inside this Model and returns
   * it.  This covenience method is equivalent to:
   *
   *   addRule( SpeciesConcentrationRule() );
   */
  LIBSBML_EXTERN
  SpeciesConcentrationRule& createSpeciesConcentrationRule ();

  /**
   * Creates a new Reaction inside this Model and returns.  This covenience
   * method is equivalent to:
   *
   *   addReaction( Reaction() );
   */
  LIBSBML_EXTERN
  Reaction& createReaction ();

  /**
   * Creates a new Reactant (i.e. SpeciesReference) inside this Model and
   * returns a pointer to it.  The SpeciesReference is added to the
   * reactants of the last Reaction created.
   *
   * If a Reaction does not exist for this model, a new SpeciesReference is
   * not created and NULL is returned.
   */
  LIBSBML_EXTERN
  SpeciesReference* createReactant ();

  /**
   * Creates a new Product (i.e. SpeciesReference) inside this Model and
   * returns a pointer to it.  The SpeciesReference is added to the
   * products of the last Reaction created.
   *
   * If a Reaction does not exist for this model, a new SpeciesReference is
   * not created and NULL is returned.
   */
  LIBSBML_EXTERN
  SpeciesReference* createProduct ();

  /**
   * Creates a new Modifer (i.e. ModifierSpeciesReference) inside this
   * Model and returns a pointer to it.  The ModifierSpeciesReference is
   * added to the modifiers of the last Reaction created.
   *
   * If a Reaction does not exist for this model, a new
   * ModifierSpeciesReference is not created and NULL is returned.
   */
  LIBSBML_EXTERN
  ModifierSpeciesReference* createModifier ();

  /**
   * Creates a new KineticLaw inside this Model and returns a pointer to
   * it.  The KineticLaw is associated with the last Reaction created.
   *
   * If a Reaction does not exist for this model, or a Reaction does exist,
   * but already has a KineticLaw, a new KineticLaw is not created and NULL
   * is returned.
   */
  LIBSBML_EXTERN
  KineticLaw* createKineticLaw ();

  /**
   * Creates a new Parameter (of a KineticLaw) inside this Model and
   * returns a pointer to it.  The Parameter is associated with the
   * KineticLaw of the last Reaction created.
   *
   * If a Reaction does not exist for this model, or a KineticLaw for the
   * Reaction, a new Parameter is not created and NULL is returned.
   */
  LIBSBML_EXTERN
  Parameter* createKineticLawParameter ();

  /**
   * Creates a new Event inside this Model and returns.  This covenience
   * function is functionally equivalent to:
   *
   *   addEvent( Event() );
   */
  LIBSBML_EXTERN
  Event& createEvent ();

  /**
   * Creates a new EventAssignment inside this Model and returns a pointer
   * to it.  The EventAssignment is added to the the last Event created.
   *
   * If an Event does not exist for this model, a new EventAssignment is
   * not created and NULL is returned.
   */
  LIBSBML_EXTERN
  EventAssignment* createEventAssignment ();

  /**
   * Adds the given FunctionDefinition to this Model.
   */
  LIBSBML_EXTERN
  void addFunctionDefinition (FunctionDefinition& fd);

  /**
   * Adds the given UnitDefinition to this Model.
   */
  LIBSBML_EXTERN
  void addUnitDefinition (UnitDefinition& ud);

  /**
   * Adds the given Compartment to this Model.
   */
  LIBSBML_EXTERN
  void addCompartment (Compartment& c);

  /**
   * Adds the given Species to this Model.
   */
  LIBSBML_EXTERN
  void addSpecies (Species& s);

  /**
   * Adds the given Parameter to this Model.
   */
  LIBSBML_EXTERN
  void addParameter (Parameter& p);

  /**
   * Adds the given Rule to this Model.
   */
  LIBSBML_EXTERN
  void addRule (Rule& r);

  /**
   * Adds the given Reaction to this Model.
   */
  LIBSBML_EXTERN
  void addReaction (Reaction& r);

  /**
   * Adds the given Event to this Model.
   */
  LIBSBML_EXTERN
  void addEvent (Event& e);

  /**
   * @return the list of FunctionDefinitions for this Model.
   */
  LIBSBML_EXTERN
  ListOf& getListOfFunctionDefinitions ();

  /**
   * @return the list of FunctionDefinitions for this Model.
   */
  LIBSBML_EXTERN
  const ListOf& getListOfFunctionDefinitions () const;

  /**
   * @return the list of UnitDefinitions for this Model.
   */
  LIBSBML_EXTERN
  ListOf& getListOfUnitDefinitions ();

  /**
   * @return the list of UnitDefinitions for this Model.
   */
  LIBSBML_EXTERN
  const ListOf& getListOfUnitDefinitions () const;

  /**
   * @return the list of Compartments for this Model.
   */
  LIBSBML_EXTERN
  ListOf& getListOfCompartments ();

  /**
   * @return the list of Compartments for this Model.
   */
  LIBSBML_EXTERN
  const ListOf& getListOfCompartments () const;

  /**
   * @return the list of Species for this Model.
   */
  LIBSBML_EXTERN
  ListOf& getListOfSpecies ();

  /**
   * @return the list of Species for this Model.
   */
  LIBSBML_EXTERN
  const ListOf& getListOfSpecies () const;

  /**
   * @return the list of Parameters for this Model.
   */
  LIBSBML_EXTERN
  ListOf& getListOfParameters ();

  /**
   * @return the list of Parameters for this Model.
   */
  LIBSBML_EXTERN
  const ListOf& getListOfParameters () const;

  /**
   * @return the list of Rules for this Model.
   */
  LIBSBML_EXTERN
  ListOf& getListOfRules ();

  /**
   * @return the list of Rules for this Model.
   */
  LIBSBML_EXTERN
  const ListOf& getListOfRules () const;

  /**
   * @return the list of Rules for this Model.
   */
  LIBSBML_EXTERN
  ListOf& getListOfReactions ();

  /**
   * @return the list of Rules for this Model.
   */
  LIBSBML_EXTERN
  const ListOf& getListOfReactions () const;

  /**
   * @return the list of Rules for this Model.
   */
  LIBSBML_EXTERN
  ListOf& getListOfEvents ();

  /**
   * @return the list of Rules for this Model.
   */
  LIBSBML_EXTERN
  const ListOf& getListOfEvents () const;

  /**
   * @return the list of items of the given type for this Model.  If the
   * given SBMLTypeCode does not correspond to a ListOf contained in SBML
   * Models, NULL is returned.
   */
  ListOf* getListOfByTypecode (SBMLTypeCode_t type);

  /**
   * @return the nth FunctionDefinition of this Model.
   */
  LIBSBML_EXTERN
  FunctionDefinition* getFunctionDefinition (unsigned int n) const;

  /**
   * @return the FunctionDefinition in this Model with the given id or NULL
   * if no such FunctionDefinition exists.
   */
  LIBSBML_EXTERN
  FunctionDefinition* getFunctionDefinition (const std::string& sid) const;

  /**
   * @return the nth UnitDefinition of this Model.
   */
  LIBSBML_EXTERN
  UnitDefinition* getUnitDefinition (unsigned int n) const;

  /**
   * @return the UnitDefinition in this Model with the given id or NULL if
   * no such UnitDefinition exists.
   */
  LIBSBML_EXTERN
  UnitDefinition* getUnitDefinition (const std::string& sid) const;

  /**
   * @return the nth Compartment of this Model.
   */
  LIBSBML_EXTERN
  Compartment* getCompartment (unsigned int n) const;

  /**
   * @return the Compartment in this Model with the given id or NULL if no
   * such Compartment exists.
   */
  LIBSBML_EXTERN
  Compartment* getCompartment (const std::string& sid) const;

  /**
   * @return the nth Species of this Model.
   */
  LIBSBML_EXTERN
  Species* getSpecies (unsigned int n) const;

  /**
   * @return the Species in this Model with the given id or NULL if no such
   * Species exists.
   */
  LIBSBML_EXTERN
  Species* getSpecies (const std::string& sid) const;

  /**
   * @return the nth Parameter of this Model.
   */
  LIBSBML_EXTERN
  Parameter* getParameter (unsigned int n) const;

  /**
   * @return the Parameter in this Model with the given id or NULL if no
   * such Parameter exists.
   */
  LIBSBML_EXTERN
  Parameter* getParameter (const std::string& sid) const;

  /**
   * @return the nth Rule of this Model.
   */
  LIBSBML_EXTERN
  Rule* getRule (unsigned int n) const;

  /**
   * @return the nth Reaction of this Model.
   */
  LIBSBML_EXTERN
  Reaction* getReaction (unsigned int n) const;

  /**
   * @return the Reaction in this Model with the given id or NULL if no
   * such Reaction exists.
   */
  LIBSBML_EXTERN
  Reaction* getReaction (const std::string& sid) const;

  /**
   * @return the nth Event of this Model.
   */
  LIBSBML_EXTERN
  Event* getEvent (unsigned int n) const;

  /**
   * @return the Event in this Model with the given id or NULL if no such
   * Event exists.
   */
  LIBSBML_EXTERN
  Event* getEvent (const std::string& sid) const;

  /**
   * @return the number of FunctionDefinitions in this Model.
   */
  LIBSBML_EXTERN
  unsigned int getNumFunctionDefinitions () const;

  /**
   * @return the number of UnitDefinitions in this Model.
   */
  LIBSBML_EXTERN
  unsigned int getNumUnitDefinitions () const;

  /**
   * @return the number of Compartments in this Model.
   */
  LIBSBML_EXTERN
  unsigned int getNumCompartments () const;

  /**
   * @return the number of Species in this Model.
   */
  LIBSBML_EXTERN
  unsigned int getNumSpecies () const;

  /**
   * @return the number of Species in this Model with boundaryCondition set
   * to true.
   */
  LIBSBML_EXTERN
  unsigned int getNumSpeciesWithBoundaryCondition () const;

  /**
   * @return the number of Parameters in this Model.  Parameters defined in
   * KineticLaws are not included.
   */
  LIBSBML_EXTERN
  unsigned int getNumParameters () const;

  /**
   * @return the number of Rules in this Model.
   */
  LIBSBML_EXTERN
  unsigned int getNumRules () const;

  /**
   * @return the number of Reactions in this Model.
   */
  LIBSBML_EXTERN
  unsigned int getNumReactions () const;

  /**
   * @return the number of Events in this Model.
   */
  LIBSBML_EXTERN
  unsigned int getNumEvents () const;

  /**
   * @return true if the given ASTNode is a boolean.  Often times, this
   * question can be answered with the ASTNode's own isBoolean() method,
   * but if the AST is an expression that calls a function defined in the
   * Model's ListOf FunctionDefinitions, the model is needed for lookup
   * context.
   */
  LIBSBML_EXTERN
  bool isBoolean (const ASTNode* node) const;


#ifdef USE_LAYOUT

  /**
   * Returns a reference to the ListOf object that holds the layouts.
   */
  LIBSBML_EXTERN
  ListOf& getListOfLayouts ();

  /**
   * Returns a reference to the ListOf object that holds the layouts.
   */
  LIBSBML_EXTERN
  const ListOf& getListOfLayouts () const;

  /**
   * Returns the layout object that belongs to the given index. If the
   * index is invalid, NULL is returned.
   */
  LIBSBML_EXTERN
  Layout* getLayout (unsigned int index) const;

  /**
   * Adds the layout object to the list of layouts.
   */ 
  LIBSBML_EXTERN
  void addLayout (Layout& layout);

  /**
   * Creates a new layout object and adds it to the list of layout objects.
   * A reference to the newly created object is returned.
   */
  LIBSBML_EXTERN
  Layout& createLayout();

#endif  /* USE_LAYOUT */  


protected:

  std::string  id;
  std::string  name;
  ListOf       functionDefinition;
  ListOf       unitDefinition;
  ListOf       compartment;
  ListOf       species;
  ListOf       parameter;
  ListOf       rule;
  ListOf       reaction;
  ListOf       event;

#ifdef USE_LAYOUT
  ListOf layouts;
#endif  /* USE_LAYOUT */


  unsigned int mLevel;
  unsigned int mVersion;

  friend class SBMLDocument;
  friend class SBMLFormatter;
  friend class SBMLHandler;
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
 * @return the SBML Level of this Model.
 */
LIBSBML_EXTERN
unsigned int
Model_getLevel (const Model_t *m);

/**
 * @return the SBML Level of this Model.
 */
LIBSBML_EXTERN
unsigned int
Model_getVersion (const Model_t *m);


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
Model_moveAllIdsToNames (Model_t *m);

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
Model_moveAllNamesToIds (Model_t *m);

/**
 * Moves the id field of this Model to its name field (iff name is not
 * already set).  This method is used for converting from L2 to L1.
 */
LIBSBML_EXTERN
void
Model_moveIdToName (Model_t *m);

/**
 * Moves the name field of this Model to its id field (iff id is not
 * already set).  This method is used for converting from L1 to L2.
 */
LIBSBML_EXTERN
void
Model_moveNameToId (Model_t *m);


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
 * @return the list of items of the given type for this Model.  If the
 * given SBMLTypeCode does not correspond to a ListOf contained in SBML
 * Models, NULL is returned.
 */
ListOf_t *
Model_getListOfByTypecode (Model_t *m, SBMLTypeCode_t type);

/**
 * @return the nth FunctionDefinition of this Model.
 */
LIBSBML_EXTERN
FunctionDefinition_t *
Model_getFunctionDefinition (const Model_t *m, unsigned int n);

/**
 * @return the FunctionDefinition in this Model with the given id or NULL
 * if no such FunctionDefinition exists.
 */
LIBSBML_EXTERN
FunctionDefinition_t *
Model_getFunctionDefinitionById (const Model_t *m, const char *sid);

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
 * @return the Compartment in this Model with the given id or NULL if no
 * such Compartment exists.
 */
LIBSBML_EXTERN
Compartment_t *
Model_getCompartmentById (const Model_t *m, const char *sid);

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
 * @return the Parameter in this Model with the given id or NULL if no such
 * Parameter exists.
 */
LIBSBML_EXTERN
Parameter_t *
Model_getParameterById (const Model_t *m, const char *sid);

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
 * @return the Reaction in this Model with the given id or NULL if no such
 * Reaction exists.
 */
LIBSBML_EXTERN
Reaction_t *
Model_getReactionById (const Model_t *m, const char *sid);

/**
 * @return the nth Event of this Model.
 */
LIBSBML_EXTERN
Event_t *
Model_getEvent (const Model_t *m, unsigned int n);

/**
 * @return the Event in this Model with the given id or NULL if no such
 * Event exists.
 */
LIBSBML_EXTERN
Event_t *
Model_getEventById (const Model_t *m, const char *sid);


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
