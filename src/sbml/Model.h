/**
 * Filename    : Model.h
 * Description : SBML Model
 * Author(s)   : SBW Development Group <sysbio-team@caltech.edu>
 * Organization: Caltech ERATO Kitano Systems Biology Project
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
 *     The Systems Biology Workbench Development Group
 *     ERATO Kitano Systems Biology Project
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


#include "List.h"
#include "SBase.h"
#include "UnitDefinition.h"
#include "Compartment.h"
#include "Species.h"
#include "Parameter.h"
#include "Rule.h"
#include "AlgebraicRule.h"
#include "Reaction.h"
#include "CompartmentVolumeRule.h"
#include "ParameterRule.h"
#include "SpeciesConcentrationRule.h"


#ifdef __cplusplus
extern "C" {
#endif


typedef struct
{
  SBASE_FIELDS;
  char   *name;
  List_t *unitDefinition;
  List_t *compartment;
  List_t *species;
  List_t *parameter;
  List_t *rule;
  List_t *reaction;
} Model_t;


/**
 * Creates a new Model and returns a pointer to it.
 */
Model_t *
Model_create (void);

/**
 * Creates a new Model with the given name and returns a pointer to it.
 * This convenience function is functionally equivalent to:
 *
 *   Model_setName(Model_create(), sname);
 */
Model_t *
Model_createWith (const char *sname);

/**
 * Frees the given Model.
 */
void
Model_free (Model_t *m);

/**
 * Sets the name field of this Model to a copy of sname.
 */
void
Model_setName(Model_t *m, const char *sname);

/**
 * Creates a new UnitDefinition inside this Model and returns a pointer to
 * it.  This covenience function is functionally equivalent to:
 *
 *   Model_addUnitDefinition(m, UnitDefinition_create());
 */
UnitDefinition_t *
Model_createUnitDefinition (Model_t *m);

/**
 * Creates a new Unit inside this Model and returns a pointer to it.  The
 * Unit is added to the last UnitDefinition created.
 *
 * If a UnitDefinitions does not exist for this model, a new Unit is not
 * created and NULL is returned.
 */
Unit_t *
Model_createUnit (Model_t *m);

/**
 * Creates a new Compartment inside this Model and returns a pointer to it.
 * This covenience function is functionally equivalent to:
 *
 *   Model_addCompartment(m, Compartment_create());
 */
Compartment_t *
Model_createCompartment (Model_t *m);

/**
 * Creates a new Species inside this Model and returns a pointer to it.
 * This covenience function is functionally equivalent to:
 *
 *   Model_addSpecies(m, Species_create());
 */
Species_t *
Model_createSpecies (Model_t *m);

/**
 * Creates a new Parameter inside this Model and returns a pointer to it.
 * This covenience function is functionally equivalent to:
 *
 *   Model_addParameter(m, Parameter_create());
 */
Parameter_t *
Model_createParameter (Model_t *m);

/**
 * Creates a new AlgebraicRule inside this Model and returns a pointer to
 * it.  This covenience function is functionally equivalent to:
 *
 *   Model_addRule(m, AlgebraicRule_create());
 */
AlgebraicRule_t *
Model_createAlgebraicRule (Model_t *m);

/**
 * Creates a new CompartmentVolumeRule inside this Model and returns a
 * pointer to it.  This covenience function is functionally equivalent to:
 *
 *   Model_addRule(m, CompartmentVolumeRule_create());
 */
CompartmentVolumeRule_t *
Model_createCompartmentVolumeRule (Model_t *m);

/**
 * Creates a new ParameterRule inside this Model and returns a pointer to
 * it.  This covenience function is functionally equivalent to:
 *
 *   Model_addRule(m, ParameterRule_create());
 */
ParameterRule_t *
Model_createParameterRule (Model_t *m);

/**
 * Creates a new SpeciesConcentrationRule inside this Model and returns a
 * pointer to it.  This covenience function is functionally equivalent to:
 *
 *   Model_addRule(m, SpeciesConcentrationRule_create());
 */
SpeciesConcentrationRule_t *
Model_createSpeciesConcentrationRule (Model_t *m);

/**
 * Creates a new Reaction inside this Model and returns a pointer to it.
 * This covenience function is functionally equivalent to:
 *
 *   Model_addRule(m, Reaction_create());
 */
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
SpeciesReference_t *
Model_createProduct (Model_t *m);

/**
 * Creates a new KineticLaw inside this Model and returns a pointer to it.
 * The KineticLaw is associated with the last Reaction created.
 *
 * If a Reaction does not exist for this model, or a Reaction does exist,
 * but already has a KineticLaw, a new KineticLaw is not created and NULL
 * is returned.
 */
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
Parameter_t *
Model_createKineticLawParameter (Model_t *m);

/**
 * Adds the given UnitDefinition to this Model.
 */
void
Model_addUnitDefinition(Model_t *m, UnitDefinition_t *ud);

/**
 * Adds the given Compartment to this Model.
 */
void
Model_addCompartment(Model_t *m, Compartment_t *c);

/**
 * Adds the given Species to this Model.
 */
void
Model_addSpecies(Model_t *m, Species_t *s);

/**
 * Adds the given Parameter to this Model.
 */
void
Model_addParameter(Model_t *m, Parameter_t *p);

/**
 * Adds the given Rule to this Model.
 */
void
Model_addRule(Model_t *m, Rule_t *r);

/**
 * Adds the given Reaction to this Model.
 */
void
Model_addReaction(Model_t *m, Reaction_t *r);

/**
 * @return the nth UnitDefinition of this Model.
 */
UnitDefinition_t *
Model_getUnitDefinition(Model_t *m, unsigned int n);

/**
 * @return the nth Compartment of this Model.
 */
Compartment_t *
Model_getCompartment(Model_t *m, unsigned int n);

/**
 * @return the nth Species of this Model.
 */
Species_t *
Model_getSpecies(Model_t *m, unsigned int n);

/**
 * @return the nth Parameter of this Model.
 */
Parameter_t *
Model_getParameter(Model_t *m, unsigned int n);

/**
 * @return the nth Rule of this Model.
 */
Rule_t *
Model_getRule(Model_t *m, unsigned int n);

/**
 * @return the nth Reaction of this Model.
 */
Reaction_t *
Model_getReaction(Model_t *m, unsigned int n);

/**
 * @return the number of UnitDefinitions in this Model.
 */
unsigned int
Model_getNumUnitDefinitions(Model_t *m);

/**
 * @return the number of Compartments in this Model.
 */
unsigned int
Model_getNumCompartments(Model_t *m);

/**
 * @return the number of Species in this Model.
 */
unsigned int
Model_getNumSpecies(Model_t *m);

/**
 * @return the number of Parameters in this Model.  Parameters defined in
 * KineticLaws are not included.
 */
unsigned int
Model_getNumParameters(Model_t *m);

/**
 * @return the number of Rules in this Model.
 */
unsigned int
Model_getNumRules(Model_t *m);

/**
 * @return the number of Reactions in this Model.
 */
unsigned int
Model_getNumReactions(Model_t *m);


#ifdef __cplusplus
}
#endif


#endif  /** Model_h **/
