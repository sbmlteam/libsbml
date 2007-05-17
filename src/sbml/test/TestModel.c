/**
 * \file    TestModel.c
 * \brief   SBML Model unit tests
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


#include "common/common.h"
#include "SBMLTypes.h"

#include <check.h>


static Model_t *M;


void
ModelTest_setup (void)
{
  M = Model_create();

  if (M == NULL)
  {
    fail("Model_create() returned a NULL pointer.");
  }
}


void
ModelTest_teardown (void)
{
  Model_free(M);
}


START_TEST (test_Model_create)
{
  fail_unless( SBase_getTypeCode  ((SBase_t *) M) == SBML_MODEL );
  fail_unless( SBase_getMetaId    ((SBase_t *) M) == NULL );
  fail_unless( SBase_getNotes     ((SBase_t *) M) == NULL );
  fail_unless( SBase_getAnnotation((SBase_t *) M) == NULL );

  fail_unless( Model_getId  (M) == NULL );
  fail_unless( Model_getName(M) == NULL );

  fail_unless( !Model_isSetId(M)   );
  fail_unless( !Model_isSetName(M) );

  fail_unless( Model_getNumUnitDefinitions(M) == 0 );
  fail_unless( Model_getNumCompartments   (M) == 0 );
  fail_unless( Model_getNumSpecies        (M) == 0 );
  fail_unless( Model_getNumParameters     (M) == 0 );
  fail_unless( Model_getNumReactions      (M) == 0 );
}
END_TEST


START_TEST (test_Model_free_NULL)
{
  Model_free(NULL);
}
END_TEST


START_TEST (test_Model_createWith)
{
  Model_t *m = Model_createWith("repressilator", "");


  fail_unless( SBase_getTypeCode  ((SBase_t *) m) == SBML_MODEL );  
  fail_unless( SBase_getMetaId    ((SBase_t *) m) == NULL );
  fail_unless( SBase_getNotes     ((SBase_t *) m) == NULL );
  fail_unless( SBase_getAnnotation((SBase_t *) m) == NULL );

  fail_unless( Model_getName(m) == NULL );

  fail_unless( !strcmp(Model_getId(m), "repressilator") );
  fail_unless( Model_isSetId(m) );

  fail_unless( Model_getNumUnitDefinitions(m) == 0 );
  fail_unless( Model_getNumFunctionDefinitions(m) == 0 );
  fail_unless( Model_getNumCompartments   (m) == 0 );
  fail_unless( Model_getNumSpecies        (m) == 0 );
  fail_unless( Model_getNumParameters     (m) == 0 );
  fail_unless( Model_getNumReactions      (m) == 0 );
  fail_unless( Model_getNumRules          (m) == 0 );
  fail_unless( Model_getNumConstraints    (m) == 0 );
  fail_unless( Model_getNumEvents         (m) == 0 );
  fail_unless( Model_getNumCompartmentTypes(m) == 0 );
  fail_unless( Model_getNumSpeciesTypes    (m) == 0 );
  fail_unless( Model_getNumInitialAssignments (m) == 0 );

  Model_free(m);
}
END_TEST


START_TEST (test_Model_setId)
{
  char *id = "Branch";


  Model_setId(M, id);

  fail_unless( !strcmp(Model_getId(M), id) );
  fail_unless( Model_isSetId(M)   );

  if (Model_getId(M) == id)
  {
    fail("Model_setId(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  Model_setId(M, Model_getId(M));
  fail_unless( !strcmp(Model_getId(M), id) );

  Model_setId(M, NULL);
  fail_unless( !Model_isSetId(M) );

  if (Model_getId(M) != NULL)
  {
    fail("Model_setId(M, NULL) did not clear string.");
  }

  Model_setId(M, id);
  Model_unsetId(M);
  fail_unless( !Model_isSetId(M) );

}
END_TEST


START_TEST (test_Model_setName)
{
  char *name = "My Branch Model";


  Model_setName(M, name);

  fail_unless( !strcmp(Model_getName(M), name) );
  fail_unless( Model_isSetName(M) );

  if (Model_getName(M) == name)
  {
    fail("Model_setName(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  Model_setName(M, Model_getName(M));
  fail_unless( !strcmp(Model_getName(M), name) );

  Model_setName(M, NULL);
  fail_unless( !Model_isSetName(M) );

  if (Model_getName(M) != NULL)
  {
    fail("Model_setName(M, NULL) did not clear string.");
  }
}
END_TEST

START_TEST(test_Model_setgetModelHistory)
{
  ModelHistory_t * history = ModelHistory_create();
  ModelCreator_t * mc = ModelCreator_create();

  ModelCreator_setFamilyName(mc, "Keating");
  ModelCreator_setGivenName(mc, "Sarah");
  ModelCreator_setEmail(mc, "sbml-team@caltech.edu");
  ModelCreator_setOrganisation(mc, "UH");

  ModelHistory_addCreator(history, mc);

  fail_unless(Model_isSetModelHistory(M) == 0);

  Model_setModelHistory(M, history);

  fail_unless(Model_isSetModelHistory(M) == 1);

  ModelCreator_t *newMC = List_get(ModelHistory_getCreator(history), 0);

  fail_unless(newMC != NULL);

  fail_unless(!strcmp(ModelCreator_getFamilyName(newMC), "Keating"));
  fail_unless(!strcmp(ModelCreator_getGivenName(newMC), "Sarah"));
  fail_unless(!strcmp(ModelCreator_getEmail(newMC), "sbml-team@caltech.edu"));
  fail_unless(!strcmp(ModelCreator_getOrganisation(newMC), "UH"));

  Model_unsetModelHistory(M);
  fail_unless(Model_isSetModelHistory(M) == 0);


  ModelHistory_free(history);
  ModelCreator_free(mc);

}
END_TEST


START_TEST (test_Model_createFunctionDefinition)
{
  FunctionDefinition_t *fd = Model_createFunctionDefinition(M);


  fail_unless( fd != NULL );
  fail_unless( Model_getNumFunctionDefinitions(M) == 1  );
  fail_unless( Model_getFunctionDefinition(M, 0)  == fd );
}
END_TEST


START_TEST (test_Model_createUnitDefinition)
{
  UnitDefinition_t *ud = Model_createUnitDefinition(M);


  fail_unless( ud != NULL );
  fail_unless( Model_getNumUnitDefinitions(M) == 1  );
  fail_unless( Model_getUnitDefinition(M, 0)  == ud );
}
END_TEST


START_TEST (test_Model_createUnit)
{
  UnitDefinition_t *ud;
  Unit_t           *u;


  Model_createUnitDefinition(M);
  Model_createUnitDefinition(M);

  u = Model_createUnit(M);

  fail_unless( u != NULL );
  fail_unless( Model_getNumUnitDefinitions(M) == 2 );

  ud = Model_getUnitDefinition(M, 1);

  fail_unless( UnitDefinition_getNumUnits(ud) == 1 );
  fail_unless( UnitDefinition_getUnit(ud, 0)  == u );
}
END_TEST


START_TEST (test_Model_createUnit_noUnitDefinition)
{
  fail_unless( Model_getNumUnitDefinitions(M) == 0 );
  fail_unless( Model_createUnit(M) == NULL );
}
END_TEST


START_TEST (test_Model_createCompartment)
{
  Compartment_t *c = Model_createCompartment(M);


  fail_unless( c != NULL );
  fail_unless( Model_getNumCompartments(M) == 1 );
  fail_unless( Model_getCompartment(M, 0)  == c );
}
END_TEST


START_TEST (test_Model_createCompartmentType)
{
  CompartmentType_t *c = Model_createCompartmentType(M);


  fail_unless( c != NULL );
  fail_unless( Model_getNumCompartmentTypes(M) == 1 );
  fail_unless( Model_getCompartmentType(M, 0)  == c );
}
END_TEST


START_TEST (test_Model_createSpeciesType)
{
  SpeciesType_t *c = Model_createSpeciesType(M);


  fail_unless( c != NULL );
  fail_unless( Model_getNumSpeciesTypes(M) == 1 );
  fail_unless( Model_getSpeciesType(M, 0)  == c );
}
END_TEST


START_TEST (test_Model_createInitialAssignment)
{
  InitialAssignment_t *c = Model_createInitialAssignment(M);


  fail_unless( c != NULL );
  fail_unless( Model_getNumInitialAssignments(M) == 1 );
  fail_unless( Model_getInitialAssignment(M, 0)  == c );
}
END_TEST


START_TEST (test_Model_createConstraint)
{
  Constraint_t *c = Model_createConstraint(M);


  fail_unless( c != NULL );
  fail_unless( Model_getNumConstraints(M) == 1 );
  fail_unless( Model_getConstraint(M, 0)  == c );
}
END_TEST


START_TEST (test_Model_createSpecies)
{
  Species_t *s = Model_createSpecies(M);


  fail_unless( s != NULL );
  fail_unless( Model_getNumSpecies(M) == 1 );
  fail_unless( Model_getSpecies(M, 0) == s );
}
END_TEST


START_TEST (test_Model_createParameter)
{
  Parameter_t *p = Model_createParameter(M);


  fail_unless( p != NULL );
  fail_unless( Model_getNumParameters(M) == 1 );
  fail_unless( Model_getParameter(M, 0)  == p );
}
END_TEST

/*
START_TEST (test_Model_createAssignmentRule)
{
  AssignmentRule_t *ar = Model_createAssignmentRule(M);


  fail_unless( ar != NULL );
  fail_unless( Model_getNumRules(M) == 1 );
  fail_unless( Model_getRule(M, 0)  == (Rule_t *) ar );
}
END_TEST


START_TEST (test_Model_createRateRule)
{
  RateRule_t *rr = Model_createRateRule(M);


  fail_unless( rr != NULL );
  fail_unless( Model_getNumRules(M) == 1 );
  fail_unless( Model_getRule(M, 0)  == (Rule_t *) rr );
}
END_TEST


START_TEST (test_Model_createAlgebraicRule)
{
  AlgebraicRule_t *ar = Model_createAlgebraicRule(M);


  fail_unless( ar != NULL );
  fail_unless( Model_getNumRules(M) == 1 );
  fail_unless( Model_getRule(M, 0)  == (Rule_t *) ar );
}
END_TEST


START_TEST (test_Model_createCompartmentVolumeRule)
{
  CompartmentVolumeRule_t *cvr = Model_createCompartmentVolumeRule(M);


  fail_unless( cvr != NULL );
  fail_unless( Model_getNumRules(M) == 1 );
  fail_unless( Model_getRule(M, 0)  == (Rule_t *) cvr );
}
END_TEST


START_TEST (test_Model_createParameterRule)
{
  ParameterRule_t *pr = Model_createParameterRule(M);


  fail_unless( pr != NULL );
  fail_unless( Model_getNumRules(M) == 1 );
  fail_unless( Model_getRule(M, 0)  == (Rule_t *) pr );
}
END_TEST


START_TEST (test_Model_createSpeciesConcentrationRule)
{
  SpeciesConcentrationRule_t *scr = Model_createSpeciesConcentrationRule(M);


  fail_unless( scr != NULL );
  fail_unless( Model_getNumRules(M) == 1 );
  fail_unless( Model_getRule(M, 0)  == (Rule_t *) scr );
}
END_TEST
*/

START_TEST (test_Model_createReaction)
{
  Reaction_t *r = Model_createReaction(M);


  fail_unless( r != NULL );
  fail_unless( Model_getNumReactions(M) == 1 );
  fail_unless( Model_getReaction(M, 0)  == r );
}
END_TEST


START_TEST (test_Model_createReactant)
{
  Reaction_t         *r;
  SpeciesReference_t *sr;


  Model_createReaction(M);
  Model_createReaction(M);

  sr = Model_createReactant(M);

  fail_unless( sr != NULL );
  fail_unless( Model_getNumReactions(M) == 2 );

  r = Model_getReaction(M, 1);

  fail_unless( Reaction_getNumReactants(r) == 1  );
  fail_unless( Reaction_getReactant(r, 0)  == sr );
}
END_TEST


START_TEST (test_Model_createReactant_noReaction)
{
  fail_unless( Model_getNumReactions(M) == 0    );
  fail_unless( Model_createReactant(M)  == NULL );
}
END_TEST


START_TEST (test_Model_createProduct)
{
  Reaction_t         *r;
  SpeciesReference_t *sr;


  Model_createReaction(M);
  Model_createReaction(M);

  sr = Model_createProduct(M);

  fail_unless( sr != NULL );
  fail_unless( Model_getNumReactions(M) == 2 );

  r = Model_getReaction(M, 1);

  fail_unless( Reaction_getNumProducts(r) == 1  );
  fail_unless( Reaction_getProduct(r, 0)  == sr );
}
END_TEST


START_TEST (test_Model_createProduct_noReaction)
{
  fail_unless( Model_getNumReactions(M) == 0    );
  fail_unless( Model_createProduct(M)   == NULL );
}
END_TEST

/*
START_TEST (test_Model_createModifier)
{
  Reaction_t                 *r;
  ModifierSpeciesReference_t *msr;


  Model_createReaction(M);
  Model_createReaction(M);

  msr = Model_createModifier(M);

  fail_unless( msr != NULL );
  fail_unless( Model_getNumReactions(M) == 2 );

  r = Model_getReaction(M, 1);

  fail_unless( Reaction_getNumModifiers(r) == 1   );
  fail_unless( Reaction_getModifier(r, 0)  == msr );
}
END_TEST


START_TEST (test_Model_createModifier_noReaction)
{
  fail_unless( Model_getNumReactions(M) == 0    );
  fail_unless( Model_createModifier(M)  == NULL );
}
END_TEST
*/

START_TEST (test_Model_createKineticLaw)
{
  Reaction_t   *r;
  KineticLaw_t *kl;


  Model_createReaction(M);
  Model_createReaction(M);

  kl = Model_createKineticLaw(M);

  fail_unless( kl != NULL );
  fail_unless( Model_getNumReactions(M) == 2 );

  r = Model_getReaction(M, 0);
  fail_unless( Reaction_getKineticLaw(r) == NULL );

  r = Model_getReaction(M, 1);
  fail_unless( Reaction_getKineticLaw(r) == kl );
}
END_TEST


START_TEST (test_Model_createKineticLaw_alreadyExists)
{
  Reaction_t   *r;
  KineticLaw_t *kl;


  r  = Model_createReaction(M);
  kl = Model_createKineticLaw(M);

  fail_unless( Model_createKineticLaw(M) == kl );
  fail_unless( Reaction_getKineticLaw(r) == kl );
}
END_TEST


START_TEST (test_Model_createKineticLaw_noReaction)
{
  fail_unless( Model_getNumReactions(M)  == 0    );
  fail_unless( Model_createKineticLaw(M) == NULL );
}
END_TEST


START_TEST (test_Model_createKineticLawParameter)
{
  Reaction_t   *r;
  KineticLaw_t *kl;
  Parameter_t  *p;


  Model_createReaction(M);
  Model_createReaction(M);
  Model_createKineticLaw(M);

  p = Model_createKineticLawParameter(M);

  fail_unless( Model_getNumReactions(M) == 2 );

  r = Model_getReaction(M, 0);
  fail_unless( Reaction_getKineticLaw(r) == NULL );

  r = Model_getReaction(M, 1);
  fail_unless( Reaction_getKineticLaw(r) != NULL );

  kl = Reaction_getKineticLaw(r);
  fail_unless( KineticLaw_getNumParameters(kl) == 1 );
  fail_unless( KineticLaw_getParameter(kl, 0)  == p );
}
END_TEST


START_TEST (test_Model_createKineticLawParameter_noReaction)
{
  fail_unless( Model_getNumReactions(M)           == 0    );
  fail_unless( Model_createKineticLawParameter(M) == NULL );
}
END_TEST


START_TEST (test_Model_createKineticLawParameter_noKineticLaw)
{
  Reaction_t *r;


  r = Model_createReaction(M);

  fail_unless( Reaction_getKineticLaw(r) == NULL );
  fail_unless( Model_createKineticLawParameter(M) == NULL );
}
END_TEST


START_TEST (test_Model_createEvent)
{
  //Event_t *e = Model_createEvent(M);


  //fail_unless( e != NULL );
  //fail_unless( Model_getNumEvents(M) == 1 );
  //fail_unless( Model_getEvent(M, 0)  == e );
}
END_TEST


START_TEST (test_Model_createEventAssignment)
{
  //Event_t           *e;
  //EventAssignment_t *ea;


  //Model_createEvent(M);
  //Model_createEvent(M);

  //ea = Model_createEventAssignment(M);

  //fail_unless( ea != NULL );
  //fail_unless( Model_getNumEvents(M) == 2 );

  //e = Model_getEvent(M, 1);

  //fail_unless( Event_getNumEventAssignments(e) == 1  );
  //fail_unless( Event_getEventAssignment(e, 0)  == ea );
}
END_TEST


START_TEST (test_Model_createEventAssignment_noEvent)
{
  fail_unless( Model_getNumEvents(M)          == 0    );
  fail_unless( Model_createEventAssignment(M) == NULL );
}
END_TEST


/**
 * If I had time to do it over again, this is how I would write and
 * combine the get / add tests for collection (see below).
 */
START_TEST (test_Model_add_get_FunctionDefinitions)
{
  FunctionDefinition_t *fd1 = FunctionDefinition_create();
  FunctionDefinition_t *fd2 = FunctionDefinition_create();


  Model_addFunctionDefinition(M, fd1);
  Model_addFunctionDefinition(M, fd2);

  fail_unless( Model_getNumFunctionDefinitions(M) == 2    );
  fail_unless( Model_getFunctionDefinition(M, 0)  != fd1  );
  fail_unless( Model_getFunctionDefinition(M, 1)  != fd2  );
  fail_unless( Model_getFunctionDefinition(M, 2)  == NULL );
  fail_unless( Model_getFunctionDefinition(M, -2) == NULL );
}
END_TEST


START_TEST (test_Model_add_get_UnitDefinitions)
{
  UnitDefinition_t *ud1 = UnitDefinition_create();
  UnitDefinition_t *ud2 = UnitDefinition_create();


  Model_addUnitDefinition(M, ud1);
  Model_addUnitDefinition(M, ud2);

  fail_unless( Model_getNumUnitDefinitions(M) == 2    );
  fail_unless( Model_getUnitDefinition(M, 0)  != ud1  );
  fail_unless( Model_getUnitDefinition(M, 1)  != ud2  );
  fail_unless( Model_getUnitDefinition(M, 2)  == NULL );
  fail_unless( Model_getUnitDefinition(M, -2) == NULL );
}
END_TEST


START_TEST (test_Model_addCompartment)
{
  Model_addCompartment(M, Compartment_create());

  fail_unless( Model_getNumCompartments(M) == 1 );
}
END_TEST


START_TEST (test_Model_addSpecies)
{
  Model_addSpecies(M, Species_create());

  fail_unless( Model_getNumSpecies(M) == 1 );
}
END_TEST


START_TEST (test_Model_addParameter)
{
  Model_addParameter(M, Parameter_create());

  fail_unless( Model_getNumParameters(M) == 1 );
}
END_TEST


START_TEST (test_Model_addRules)
{
  Model_addRule( M, (Rule_t *) Rule_createAlgebraic()            );
  Model_addRule( M, (Rule_t *) Rule_createAssignment()            );
  Model_addRule( M, (Rule_t *) Rule_createRate()            );

  fail_unless( Model_getNumRules(M) == 3 );
}
END_TEST


START_TEST (test_Model_addReaction)
{
  Model_addReaction(M, Reaction_create());

  fail_unless( Model_getNumReactions(M) == 1 );
}
END_TEST


START_TEST (test_Model_add_get_Event)
{
  //Event_t *e1 = Event_create();
  //Event_t *e2 = Event_create();


  //Model_addEvent(M, e1);
  //Model_addEvent(M, e2);

  //fail_unless( Model_getNumEvents(M) == 2    );
  //fail_unless( Model_getEvent(M, 0)  == e1   );
  //fail_unless( Model_getEvent(M, 1)  == e2   );
  //fail_unless( Model_getEvent(M, 2)  == NULL );
  //fail_unless( Model_getEvent(M, -2) == NULL );
}
END_TEST


START_TEST (test_Model_getFunctionDefinitionById)
{
  FunctionDefinition_t *fd1 = FunctionDefinition_create();
  FunctionDefinition_t *fd2 = FunctionDefinition_create();

  FunctionDefinition_setId( fd1, "sin" );
  FunctionDefinition_setId( fd2, "cos" );

  Model_addFunctionDefinition(M, fd1);
  Model_addFunctionDefinition(M, fd2);

  fail_unless( Model_getNumFunctionDefinitions(M) == 2 );

  fail_unless( Model_getFunctionDefinitionById(M, "sin" ) != fd1  );
  fail_unless( Model_getFunctionDefinitionById(M, "cos" ) != fd2  );
  fail_unless( Model_getFunctionDefinitionById(M, "tan" ) == NULL );
}
END_TEST


START_TEST (test_Model_getUnitDefinition)
{
  UnitDefinition_t *ud1 = UnitDefinition_create();
  UnitDefinition_t *ud2 = UnitDefinition_create();

  UnitDefinition_setName( ud1, "mmls"   );
  UnitDefinition_setName( ud2, "volume" );

  Model_addUnitDefinition(M, ud1);
  Model_addUnitDefinition(M, ud2);

  fail_unless( Model_getNumUnitDefinitions(M) == 2 );

  ud1 = Model_getUnitDefinition(M, 0);
  ud2 = Model_getUnitDefinition(M, 1);

  fail_unless( !strcmp( UnitDefinition_getName(ud1), "mmls"   ) );
  fail_unless( !strcmp( UnitDefinition_getName(ud2), "volume" ) );
}
END_TEST


START_TEST (test_Model_getUnitDefinitionById)
{
  UnitDefinition_t *ud1 = UnitDefinition_create();
  UnitDefinition_t *ud2 = UnitDefinition_create();

  UnitDefinition_setId( ud1, "mmls"   );
  UnitDefinition_setId( ud2, "volume" );

  Model_addUnitDefinition(M, ud1);
  Model_addUnitDefinition(M, ud2);

  fail_unless( Model_getNumUnitDefinitions(M) == 2 );

  fail_unless( Model_getUnitDefinitionById(M, "mmls"       ) != ud1  );
  fail_unless( Model_getUnitDefinitionById(M, "volume"     ) != ud2  );
  fail_unless( Model_getUnitDefinitionById(M, "rototillers") == NULL );
}
END_TEST


START_TEST (test_Model_getCompartment)
{
  Compartment_t *c1 = Compartment_create();
  Compartment_t *c2 = Compartment_create();

  Compartment_setName(c1, "A");
  Compartment_setName(c2, "B");

  Model_addCompartment(M, c1);
  Model_addCompartment(M, c2);

  fail_unless( Model_getNumCompartments(M) == 2 );

  c1 = Model_getCompartment(M, 0);
  c2 = Model_getCompartment(M, 1);

  fail_unless( !strcmp(Compartment_getName(c1), "A") );
  fail_unless( !strcmp(Compartment_getName(c2), "B") );

}
END_TEST


START_TEST (test_Model_getCompartmentById)
{
  Compartment_t *c1 = Compartment_create();
  Compartment_t *c2 = Compartment_create();

  Compartment_setId( c1, "A" );
  Compartment_setId( c2, "B" );

  Model_addCompartment(M, c1);
  Model_addCompartment(M, c2);

  fail_unless( Model_getNumCompartments(M) == 2 );

  fail_unless( Model_getCompartmentById(M, "A" ) != c1   );
  fail_unless( Model_getCompartmentById(M, "B" ) != c2   );
  fail_unless( Model_getCompartmentById(M, "C" ) == NULL );
}
END_TEST


START_TEST (test_Model_getSpecies)
{
  Species_t *s1 = Species_create();
  Species_t *s2 = Species_create();

  Species_setName( s1, "Glucose"     );
  Species_setName( s2, "Glucose_6_P" );

  Model_addSpecies(M, s1);
  Model_addSpecies(M, s2);

  fail_unless( Model_getNumSpecies(M) == 2 );

  s1 = Model_getSpecies(M, 0);
  s2 = Model_getSpecies(M, 1);

  fail_unless( !strcmp( Species_getName(s1), "Glucose"     ) );
  fail_unless( !strcmp( Species_getName(s2), "Glucose_6_P" ) );
}
END_TEST


START_TEST (test_Model_getSpeciesById)
{
  Species_t *s1 = Species_create();
  Species_t *s2 = Species_create();

  Species_setId( s1, "Glucose"     );
  Species_setId( s2, "Glucose_6_P" );

  Model_addSpecies(M, s1);
  Model_addSpecies(M, s2);

  fail_unless( Model_getNumSpecies(M) == 2 );

  fail_unless( Model_getSpeciesById(M, "Glucose"    ) != s1   );
  fail_unless( Model_getSpeciesById(M, "Glucose_6_P") != s2   );
  fail_unless( Model_getSpeciesById(M, "Glucose2"   ) == NULL );
}
END_TEST


START_TEST (test_Model_getParameter)
{
  Parameter_t *p1 = Parameter_create();
  Parameter_t *p2 = Parameter_create();

  Parameter_setName(p1, "Km1");
  Parameter_setName(p2, "Km2");

  Model_addParameter(M, p1);
  Model_addParameter(M, p2);

  fail_unless( Model_getNumParameters(M) == 2 );

  p1 = Model_getParameter(M, 0);
  p2 = Model_getParameter(M, 1);

  fail_unless( !strcmp(Parameter_getName(p1), "Km1") );
  fail_unless( !strcmp(Parameter_getName(p2), "Km2") );
}
END_TEST


START_TEST (test_Model_getParameterById)
{
  Parameter_t *p1 = Parameter_create();
  Parameter_t *p2 = Parameter_create();

  Parameter_setId( p1, "Km1" );
  Parameter_setId( p2, "Km2" );

  Model_addParameter(M, p1);
  Model_addParameter(M, p2);

  fail_unless( Model_getNumParameters(M) == 2 );

  fail_unless( Model_getParameterById(M, "Km1" ) != p1   );
  fail_unless( Model_getParameterById(M, "Km2" ) != p2   );
  fail_unless( Model_getParameterById(M, "Km3" ) == NULL );
}
END_TEST

/*
START_TEST (test_Model_getRules)
{
  AlgebraicRule_t            *ar  = AlgebraicRule_create();
  SpeciesConcentrationRule_t *scr = SpeciesConcentrationRule_create();
  CompartmentVolumeRule_t    *cvr = CompartmentVolumeRule_create();
  ParameterRule_t            *pr  = ParameterRule_create();


  Rule_setFormula( (Rule_t *) ar , "x + 1"         );
  Rule_setFormula( (Rule_t *) scr, "k * t/(1 + k)" );
  Rule_setFormula( (Rule_t *) cvr, "0.10 * t"      );
  Rule_setFormula( (Rule_t *) pr , "k3/k2"         );

  Model_addRule( M, (Rule_t *) ar  );
  Model_addRule( M, (Rule_t *) scr );
  Model_addRule( M, (Rule_t *) cvr );
  Model_addRule( M, (Rule_t *) pr  );

  fail_unless( Model_getNumRules(M) == 4 );

  ar  = (AlgebraicRule_t *)            Model_getRule(M, 0);
  scr = (SpeciesConcentrationRule_t *) Model_getRule(M, 1);
  cvr = (CompartmentVolumeRule_t *)    Model_getRule(M, 2);
  pr  = (ParameterRule_t *)            Model_getRule(M, 3);

  fail_unless( !strcmp(Rule_getFormula((Rule_t *) ar) , "x + 1"        ) );
  fail_unless( !strcmp(Rule_getFormula((Rule_t *) scr), "k * t/(1 + k)") );
  fail_unless( !strcmp(Rule_getFormula((Rule_t *) cvr), "0.10 * t"     ) );
  fail_unless( !strcmp(Rule_getFormula((Rule_t *) pr) , "k3/k2"        ) );
}
END_TEST
*/

START_TEST (test_Model_getReaction)
{
  Reaction_t *r1 = Reaction_create();
  Reaction_t *r2 = Reaction_create();

  Reaction_setName(r1, "reaction_1");
  Reaction_setName(r2, "reaction_2");

  Model_addReaction(M, r1);
  Model_addReaction(M, r2);

  fail_unless( Model_getNumReactions(M) == 2 );

  r1 = Model_getReaction(M, 0);
  r2 = Model_getReaction(M, 1);

  fail_unless( !strcmp(Reaction_getName(r1), "reaction_1") );
  fail_unless( !strcmp(Reaction_getName(r2), "reaction_2") );
}
END_TEST


START_TEST (test_Model_getReactionById)
{
  Reaction_t *r1 = Reaction_create();
  Reaction_t *r2 = Reaction_create();

  Reaction_setId( r1, "reaction_1" );
  Reaction_setId( r2, "reaction_2" );

  Model_addReaction(M, r1);
  Model_addReaction(M, r2);

  fail_unless( Model_getNumReactions(M) == 2 );

  fail_unless( Model_getReactionById(M, "reaction_1" ) != r1   );
  fail_unless( Model_getReactionById(M, "reaction_2" ) != r2   );
  fail_unless( Model_getReactionById(M, "reaction_3" ) == NULL );
}
END_TEST

/* THIS IS NOT LOGICAL BUT NEEDS A WHOLE MODEL TO TEST */
START_TEST (test_KineticLaw_getParameterById)
{
  Parameter_t *k1 = Parameter_create();
  Parameter_t *k2 = Parameter_create();

  Parameter_setId(k1, "k1");
  Parameter_setId(k2, "k2");

  Parameter_setValue(k1, 3.14);
  Parameter_setValue(k2, 2.72);

  Model_addParameter(M, k1);
  Model_addParameter(M, k2);

  Reaction_t *r1 = Reaction_create();

  Reaction_setId( r1, "reaction_1" );

  KineticLaw_t *kl = KineticLaw_createWithFormula("k1 * X0");
  
  Parameter_t *k3 = Parameter_create();
  Parameter_t *k4 = Parameter_create();

  Parameter_setId(k3, "k1");
  Parameter_setId(k4, "k2");

  Parameter_setValue(k3, 2.72);
  Parameter_setValue(k4, 3.14);

  KineticLaw_addParameter(kl, k3);
  KineticLaw_addParameter(kl, k4);

  Reaction_setKineticLaw(r1, kl);
  Model_addReaction(M, r1);

  KineticLaw_t * kl1 = Reaction_getKineticLaw(Model_getReaction(M,0));

  fail_unless( KineticLaw_getParameterById(kl1, "k1" ) != k3   );
  fail_unless( KineticLaw_getParameterById(kl1, "k1" ) != k1   );
  fail_unless( KineticLaw_getParameterById(kl1, "k2" ) != k4   );
  fail_unless( KineticLaw_getParameterById(kl1, "k3" ) == NULL );
}
END_TEST


START_TEST (test_Model_getEventById)
{
  //Event_t *e1 = Event_create();
  //Event_t *e2 = Event_create();

  //Event_setId( e1, "e1" );
  //Event_setId( e2, "e2" );

  //Model_addEvent(M, e1);
  //Model_addEvent(M, e2);

  //fail_unless( Model_getNumEvents(M) == 2 );

  //fail_unless( Model_getEventById(M, "e1" ) == e1   );
  //fail_unless( Model_getEventById(M, "e2" ) == e2   );
  //fail_unless( Model_getEventById(M, "e3" ) == NULL );
}
END_TEST


START_TEST (test_Model_getNumSpeciesWithBoundaryCondition)
{
  Species_t *s1 = Species_createWith("s1", "c");
  Species_t *s2 = Species_createWith("s2", "c");
  Species_t *s3 = Species_createWith("s3", "c");

  Species_setBoundaryCondition(s1, 1);
  Species_setBoundaryCondition(s2, 0);
  Species_setBoundaryCondition(s3, 1);


  //fail_unless( Model_getNumSpecies(M) == 0 );
  //fail_unless( Model_getNumSpeciesWithBoundaryCondition(M) == 0 );

  //Model_addSpecies(M, s1);

  //fail_unless( Model_getNumSpecies(M) == 1 );
  //fail_unless( Model_getNumSpeciesWithBoundaryCondition(M) == 1 );

  //Model_addSpecies(M, s2);

  //fail_unless( Model_getNumSpecies(M) == 2 );
  //fail_unless( Model_getNumSpeciesWithBoundaryCondition(M) == 1 );

  //Model_addSpecies(M, s3);

  //fail_unless( Model_getNumSpecies(M) == 3 );
  //fail_unless( Model_getNumSpeciesWithBoundaryCondition(M) == 2 );
}
END_TEST


Suite *
create_suite_Model (void)
{
  Suite *s = suite_create("Model");
  TCase *t = tcase_create("Model");


  tcase_add_checked_fixture(t, ModelTest_setup, ModelTest_teardown);

  tcase_add_test( t, test_Model_create         );
  tcase_add_test( t, test_Model_free_NULL      );
  tcase_add_test( t, test_Model_createWith     );
  tcase_add_test( t, test_Model_setId          );
  tcase_add_test( t, test_Model_setName        );


  tcase_add_test( t, test_Model_setgetModelHistory        );
  /**
   * Model_createXXX() methods
   */
  tcase_add_test( t, test_Model_createFunctionDefinition               );
  tcase_add_test( t, test_Model_createUnitDefinition                   );
  tcase_add_test( t, test_Model_createUnit                             );
  tcase_add_test( t, test_Model_createUnit_noUnitDefinition            );
  tcase_add_test( t, test_Model_createCompartment                      );
  tcase_add_test( t, test_Model_createCompartmentType                      );
  tcase_add_test( t, test_Model_createConstraint                      );
  tcase_add_test( t, test_Model_createSpeciesType                      );
  tcase_add_test( t, test_Model_createInitialAssignment                      );
  tcase_add_test( t, test_Model_createSpecies                          );
  tcase_add_test( t, test_Model_createParameter                        );
 // tcase_add_test( t, test_Model_createAssignmentRule                   );
 // tcase_add_test( t, test_Model_createRateRule                         );
 // tcase_add_test( t, test_Model_createAlgebraicRule                    );
 // tcase_add_test( t, test_Model_createCompartmentVolumeRule            );
 // tcase_add_test( t, test_Model_createParameterRule                    );
 // tcase_add_test( t, test_Model_createSpeciesConcentrationRule         );
  tcase_add_test( t, test_Model_createReaction                         );
  tcase_add_test( t, test_Model_createReactant                         );
  tcase_add_test( t, test_Model_createReactant_noReaction              );
  tcase_add_test( t, test_Model_createProduct                          );
  tcase_add_test( t, test_Model_createProduct_noReaction               );
 // tcase_add_test( t, test_Model_createModifier                         );
 // tcase_add_test( t, test_Model_createModifier_noReaction              );
  tcase_add_test( t, test_Model_createKineticLaw                       );
  tcase_add_test( t, test_Model_createKineticLaw_alreadyExists         );
  tcase_add_test( t, test_Model_createKineticLaw_noReaction            );
  tcase_add_test( t, test_Model_createKineticLawParameter              );
  tcase_add_test( t, test_Model_createKineticLawParameter_noReaction   );
  tcase_add_test( t, test_Model_createKineticLawParameter_noKineticLaw );
  tcase_add_test( t, test_Model_createEvent                            );
  tcase_add_test( t, test_Model_createEventAssignment                  );
  tcase_add_test( t, test_Model_createEventAssignment_noEvent          );

  /**
   * Model_addXXX() methods
   */
  tcase_add_test( t, test_Model_add_get_FunctionDefinitions );
  tcase_add_test( t, test_Model_add_get_UnitDefinitions     );
  tcase_add_test( t, test_Model_addCompartment              );
  tcase_add_test( t, test_Model_addSpecies                  );
  tcase_add_test( t, test_Model_addParameter                );
  tcase_add_test( t, test_Model_addRules                    );
  tcase_add_test( t, test_Model_addReaction                 );
  tcase_add_test( t, test_Model_add_get_Event               );

  /**
   * Model_getXXX() methods
   */
  tcase_add_test( t, test_Model_getFunctionDefinitionById );
  tcase_add_test( t, test_Model_getUnitDefinition         );
  tcase_add_test( t, test_Model_getUnitDefinitionById     );
  tcase_add_test( t, test_Model_getCompartment            );
  tcase_add_test( t, test_Model_getCompartmentById        );
  tcase_add_test( t, test_Model_getSpecies                );
  tcase_add_test( t, test_Model_getSpeciesById            );
  tcase_add_test( t, test_Model_getParameter              );
  tcase_add_test( t, test_Model_getParameterById          );
//  tcase_add_test( t, test_Model_getRules                  );
  tcase_add_test( t, test_Model_getReaction               );
  tcase_add_test( t, test_Model_getReactionById           );
  tcase_add_test( t, test_Model_getEventById              );
 
  tcase_add_test( t, test_KineticLaw_getParameterById              );

  tcase_add_test( t, test_Model_getNumSpeciesWithBoundaryCondition );

  suite_add_tcase(s, t);

  return s;
}
