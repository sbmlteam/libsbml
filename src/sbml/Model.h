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


#include "common.h"
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


BEGIN_C_DECLS


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
LIBSBML_EXTERN
Model_t *
Model_create (void);

/**
 * Creates a new Model with the given name and returns a pointer to it.
 * This convenience function is functionally equivalent to:
 *
 *   Model_setName(Model_create(), sname);
 */
LIBSBML_EXTERN
Model_t *
Model_createWith (const char *sname);

/**
 * Frees the given Model.
 */
LIBSBML_EXTERN
void
Model_free (Model_t *m);


/**
 * @return the name of this Model.
 */
LIBSBML_EXTERN
const char *
Model_getName (const Model_t *m);

/**
 * @return 1 if the name of Model has been set, 0 otherwise.
 */
LIBSBML_EXTERN
int
Model_isSetName (const Model_t *m);

/**
 * Sets the name of this Model to a copy of sname.
 */
LIBSBML_EXTERN
void
Model_setName (Model_t *m, const char *sname);

/**
 * Unsets the name of this Model.  This is equivalent to:
 * safe_free(m->name); m->name = NULL;
 */
LIBSBML_EXTERN
void
Model_unsetName (Model_t *m);


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
 * Adds the given UnitDefinition to this Model.
 */
LIBSBML_EXTERN
void
Model_addUnitDefinition(Model_t *m, UnitDefinition_t *ud);

/**
 * Adds the given Compartment to this Model.
 */
LIBSBML_EXTERN
void
Model_addCompartment(Model_t *m, Compartment_t *c);

/**
 * Adds the given Species to this Model.
 */
LIBSBML_EXTERN
void
Model_addSpecies(Model_t *m, Species_t *s);

/**
 * Adds the given Parameter to this Model.
 */
LIBSBML_EXTERN
void
Model_addParameter(Model_t *m, Parameter_t *p);

/**
 * Adds the given Rule to this Model.
 */
LIBSBML_EXTERN
void
Model_addRule(Model_t *m, Rule_t *r);

/**
 * Adds the given Reaction to this Model.
 */
LIBSBML_EXTERN
void
Model_addReaction(Model_t *m, Reaction_t *r);

/**
 * @return the nth UnitDefinition of this Model.
 */
LIBSBML_EXTERN
UnitDefinition_t *
Model_getUnitDefinition(Model_t *m, unsigned int n);

/**
 * @return the nth Compartment of this Model.
 */
LIBSBML_EXTERN
Compartment_t *
Model_getCompartment(Model_t *m, unsigned int n);

/**
 * @return the nth Species of this Model.
 */
LIBSBML_EXTERN
Species_t *
Model_getSpecies(Model_t *m, unsigned int n);

/**
 * @return the nth Parameter of this Model.
 */
LIBSBML_EXTERN
Parameter_t *
Model_getParameter(Model_t *m, unsigned int n);

/**
 * @return the nth Rule of this Model.
 */
LIBSBML_EXTERN
Rule_t *
Model_getRule(Model_t *m, unsigned int n);

/**
 * @return the nth Reaction of this Model.
 */
LIBSBML_EXTERN
Reaction_t *
Model_getReaction(Model_t *m, unsigned int n);

/**
 * @return the number of UnitDefinitions in this Model.
 */
LIBSBML_EXTERN
unsigned int
Model_getNumUnitDefinitions(const Model_t *m);

/**
 * @return the number of Compartments in this Model.
 */
LIBSBML_EXTERN
unsigned int
Model_getNumCompartments(const Model_t *m);

/**
 * @return the number of Species in this Model.
 */
LIBSBML_EXTERN
unsigned int
Model_getNumSpecies(const Model_t *m);

/**
 * @return the number of Parameters in this Model.  Parameters defined in
 * KineticLaws are not included.
 */
LIBSBML_EXTERN
unsigned int
Model_getNumParameters(const Model_t *m);

/**
 * @return the number of Rules in this Model.
 */
LIBSBML_EXTERN
unsigned int
Model_getNumRules(const Model_t *m);

/**
 * @return the number of Reactions in this Model.
 */
LIBSBML_EXTERN
unsigned int
Model_getNumReactions(const Model_t *m);


END_C_DECLS


#endif  /** Model_h **/
