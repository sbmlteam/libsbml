/**
 * Filename    : Model.hpp
 * Description : SBML Model
 * Author(s)   : SBML Development Group <sbml-team@caltech.edu>
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
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s):
 */


#ifndef Model_hpp
#define Model_hpp


#include <string>

#include "extern.h"

#include "SBase.hpp"
#include "ListOf.hpp"
#include "FunctionDefinition.hpp"
#include "UnitDefinition.hpp"
#include "Compartment.hpp"
#include "Species.hpp"
#include "Parameter.hpp"
#include "Rule.hpp"
#include "AssignmentRule.hpp"
#include "RateRule.hpp"
#include "AlgebraicRule.hpp"
#include "CompartmentVolumeRule.hpp"
#include "ParameterRule.hpp"
#include "SpeciesConcentrationRule.hpp"
#include "Reaction.hpp"
#include "Event.hpp"


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
   * @return the list of UnitDefinitions for this Model.
   */
  LIBSBML_EXTERN
  ListOf& getListOfUnitDefinitions ();

  /**
   * @return the list of Compartments for this Model.
   */
  LIBSBML_EXTERN
  ListOf& getListOfCompartments ();

  /**
   * @return the list of Species for this Model.
   */
  LIBSBML_EXTERN
  ListOf& getListOfSpecies ();

  /**
   * @return the list of Parameters for this Model.
   */
  LIBSBML_EXTERN
  ListOf& getListOfParameters ();

  /**
   * @return the list of Rules for this Model.
   */
  LIBSBML_EXTERN
  ListOf& getListOfRules ();

  /**
   * @return the list of Rules for this Model.
   */
  LIBSBML_EXTERN
  ListOf& getListOfReactions ();

  /**
   * @return the list of Rules for this Model.
   */
  LIBSBML_EXTERN
  ListOf& getListOfEvents ();

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


  friend class SBMLFormatter;
  friend class SBMLHandler;
};


#endif  // Model_hpp
