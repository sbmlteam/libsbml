/**
 * Filename    : TestModel.c
 * Description : SBML Model unit tests
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


#include "sbml/common.h"
#include "sbml/Model.h"
#include "sbml/SBMLTypes.h"


Model_t *M;


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
  fail_unless( M->typecode   == SBML_MODEL, NULL );
  fail_unless( M->metaid     == NULL, NULL );
  fail_unless( M->notes      == NULL, NULL );
  fail_unless( M->annotation == NULL, NULL );

  fail_unless( M->id   == NULL, NULL );
  fail_unless( M->name == NULL, NULL );

  fail_unless( !Model_isSetId(M)  , NULL );
  fail_unless( !Model_isSetName(M), NULL );

  fail_unless( Model_getNumUnitDefinitions(M) == 0, NULL );
  fail_unless( Model_getNumCompartments   (M) == 0, NULL );
  fail_unless( Model_getNumSpecies        (M) == 0, NULL );
  fail_unless( Model_getNumParameters     (M) == 0, NULL );
  fail_unless( Model_getNumReactions      (M) == 0, NULL );
}
END_TEST


START_TEST (test_Model_free_NULL)
{
  Model_free(NULL);
}
END_TEST


START_TEST (test_Model_createWith)
{
  Model_t *m = Model_createWith("repressilator");


  fail_unless( m->typecode   == SBML_MODEL, NULL );  
  fail_unless( m->metaid     == NULL, NULL );
  fail_unless( m->notes      == NULL, NULL );
  fail_unless( m->annotation == NULL, NULL );
  fail_unless( m->name       == NULL, NULL );

  fail_unless( !strcmp(m->id, "repressilator"), NULL );
  fail_unless( Model_isSetId(m), NULL );

  fail_unless( Model_getNumUnitDefinitions(m) == 0, NULL );
  fail_unless( Model_getNumCompartments   (m) == 0, NULL );
  fail_unless( Model_getNumSpecies        (m) == 0, NULL );
  fail_unless( Model_getNumParameters     (m) == 0, NULL );
  fail_unless( Model_getNumReactions      (m) == 0, NULL );

  Model_free(m);
}
END_TEST


START_TEST (test_Model_createWithName)
{
  Model_t *m = Model_createWithName("The Repressilator Model");


  fail_unless( m->typecode   == SBML_MODEL, NULL );  
  fail_unless( m->metaid     == NULL, NULL );
  fail_unless( m->notes      == NULL, NULL );
  fail_unless( m->annotation == NULL, NULL );
  fail_unless( m->id         == NULL, NULL );

  fail_unless( !strcmp(m->name, "The Repressilator Model"), NULL );
  fail_unless( Model_isSetName(m), NULL );

  fail_unless( Model_getNumUnitDefinitions(m) == 0, NULL );
  fail_unless( Model_getNumCompartments   (m) == 0, NULL );
  fail_unless( Model_getNumSpecies        (m) == 0, NULL );
  fail_unless( Model_getNumParameters     (m) == 0, NULL );
  fail_unless( Model_getNumReactions      (m) == 0, NULL );

  Model_free(m);
}
END_TEST


START_TEST (test_Model_setId)
{
  char *id = "Branch";


  Model_setId(M, id);

  fail_unless( !strcmp(M->id, id), NULL );
  fail_unless( Model_isSetId(M)  , NULL );

  if (M->id == id)
  {
    fail("Model_setId(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  Model_setId(M, M->id);
  fail_unless( !strcmp(M->id, id), NULL );

  Model_setId(M, NULL);
  fail_unless( !Model_isSetId(M), NULL );

  if (M->id != NULL)
  {
    fail("Model_setId(M, NULL) did not clear string.");
  }
}
END_TEST


START_TEST (test_Model_setName)
{
  char *name = "My Branch Model";


  Model_setName(M, name);

  fail_unless( !strcmp(M->name, name), NULL );
  fail_unless( Model_isSetName(M)    , NULL );

  if (M->name == name)
  {
    fail("Model_setName(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  Model_setName(M, M->name);
  fail_unless( !strcmp(M->name, name), NULL );

  Model_setName(M, NULL);
  fail_unless( !Model_isSetName(M), NULL );

  if (M->name != NULL)
  {
    fail("Model_setName(M, NULL) did not clear string.");
  }
}
END_TEST


START_TEST (test_Model_createUnitDefinition)
{
  UnitDefinition_t *ud = Model_createUnitDefinition(M);


  fail_unless( ud != NULL, NULL );
  fail_unless( Model_getNumUnitDefinitions(M) == 1 , NULL );
  fail_unless( Model_getUnitDefinition(M, 0)  == ud, NULL );
}
END_TEST


START_TEST (test_Model_createUnit)
{
  UnitDefinition_t *ud;
  Unit_t           *u;


  Model_createUnitDefinition(M);
  Model_createUnitDefinition(M);

  u = Model_createUnit(M);

  fail_unless( u != NULL, NULL );
  fail_unless( Model_getNumUnitDefinitions(M) == 2, NULL );

  ud = Model_getUnitDefinition(M, 1);

  fail_unless( UnitDefinition_getNumUnits(ud) == 1, NULL );
  fail_unless( UnitDefinition_getUnit(ud, 0)  == u, NULL );
}
END_TEST


START_TEST (test_Model_createUnit_noUnitDefinition)
{
  fail_unless( Model_getNumUnitDefinitions(M) == 0, NULL );
  fail_unless( Model_createUnit(M) == NULL, NULL );
}
END_TEST


START_TEST (test_Model_createCompartment)
{
  Compartment_t *c = Model_createCompartment(M);


  fail_unless( c != NULL, NULL );
  fail_unless( Model_getNumCompartments(M) == 1, NULL );
  fail_unless( Model_getCompartment(M, 0)  == c, NULL );
}
END_TEST


START_TEST (test_Model_createSpecies)
{
  Species_t *s = Model_createSpecies(M);


  fail_unless( s != NULL, NULL );
  fail_unless( Model_getNumSpecies(M) == 1, NULL );
  fail_unless( Model_getSpecies(M, 0) == s, NULL );
}
END_TEST


START_TEST (test_Model_createParameter)
{
  Parameter_t *p = Model_createParameter(M);


  fail_unless( p != NULL, NULL );
  fail_unless( Model_getNumParameters(M) == 1, NULL );
  fail_unless( Model_getParameter(M, 0)  == p, NULL );
}
END_TEST


START_TEST (test_Model_createAlgebraicRule)
{
  AlgebraicRule_t *ar = Model_createAlgebraicRule(M);


  fail_unless( ar != NULL, NULL );
  fail_unless( Model_getNumRules(M) == 1, NULL );
  fail_unless( Model_getRule(M, 0)  == (Rule_t *) ar, NULL );
}
END_TEST


START_TEST (test_Model_createCompartmentVolumeRule)
{
  CompartmentVolumeRule_t *cvr = Model_createCompartmentVolumeRule(M);


  fail_unless( cvr != NULL, NULL );
  fail_unless( Model_getNumRules(M) == 1, NULL );
  fail_unless( Model_getRule(M, 0)  == (Rule_t *) cvr, NULL );
}
END_TEST


START_TEST (test_Model_createParameterRule)
{
  ParameterRule_t *pr = Model_createParameterRule(M);


  fail_unless( pr != NULL, NULL );
  fail_unless( Model_getNumRules(M) == 1, NULL );
  fail_unless( Model_getRule(M, 0)  == (Rule_t *) pr, NULL );
}
END_TEST


START_TEST (test_Model_createSpeciesConcentrationRule)
{
  SpeciesConcentrationRule_t *scr = Model_createSpeciesConcentrationRule(M);


  fail_unless( scr != NULL, NULL );
  fail_unless( Model_getNumRules(M) == 1, NULL );
  fail_unless( Model_getRule(M, 0)  == (Rule_t *) scr, NULL );
}
END_TEST


START_TEST (test_Model_createReaction)
{
  Reaction_t *r = Model_createReaction(M);


  fail_unless( r != NULL, NULL );
  fail_unless( Model_getNumReactions(M) == 1, NULL );
  fail_unless( Model_getReaction(M, 0)  == r, NULL );
}
END_TEST


START_TEST (test_Model_createReactant)
{
  Reaction_t         *r;
  SpeciesReference_t *sr;


  Model_createReaction(M);
  Model_createReaction(M);

  sr = Model_createReactant(M);

  fail_unless( sr != NULL, NULL );
  fail_unless( Model_getNumReactions(M) == 2, NULL );

  r = Model_getReaction(M, 1);

  fail_unless( Reaction_getNumReactants(r) == 1,  NULL );
  fail_unless( Reaction_getReactant(r, 0)  == sr, NULL );
}
END_TEST


START_TEST (test_Model_createReactant_noReaction)
{
  fail_unless( Model_getNumReactions(M) == 0,    NULL );
  fail_unless( Model_createReactant(M)  == NULL, NULL );
}
END_TEST


START_TEST (test_Model_createProduct)
{
  Reaction_t         *r;
  SpeciesReference_t *sr;


  Model_createReaction(M);
  Model_createReaction(M);

  sr = Model_createProduct(M);

  fail_unless( sr != NULL, NULL );
  fail_unless( Model_getNumReactions(M) == 2, NULL );

  r = Model_getReaction(M, 1);

  fail_unless( Reaction_getNumProducts(r) == 1,  NULL );
  fail_unless( Reaction_getProduct(r, 0)  == sr, NULL );
}
END_TEST


START_TEST (test_Model_createProduct_noReaction)
{
  fail_unless( Model_getNumReactions(M) == 0,    NULL );
  fail_unless( Model_createProduct(M)   == NULL, NULL );
}
END_TEST


START_TEST (test_Model_createKineticLaw)
{
  Reaction_t   *r;
  KineticLaw_t *kl;


  Model_createReaction(M);
  Model_createReaction(M);

  kl = Model_createKineticLaw(M);

  fail_unless( kl != NULL, NULL );
  fail_unless( Model_getNumReactions(M) == 2, NULL );

  r = Model_getReaction(M, 0);
  fail_unless( r->kineticLaw == NULL, NULL );

  r = Model_getReaction(M, 1);
  fail_unless( r->kineticLaw == kl, NULL );
}
END_TEST


START_TEST (test_Model_createKineticLaw_alreadyExists)
{
  Reaction_t   *r;
  KineticLaw_t *kl;


  r  = Model_createReaction(M);
  kl = Model_createKineticLaw(M);

  fail_unless( Model_createKineticLaw(M) == NULL, NULL );
  fail_unless( r->kineticLaw == kl, NULL );
}
END_TEST


START_TEST (test_Model_createKineticLaw_noReaction)
{
  fail_unless( Model_getNumReactions(M)  == 0,    NULL );
  fail_unless( Model_createKineticLaw(M) == NULL, NULL );
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

  fail_unless( Model_getNumReactions(M) == 2, NULL );

  r = Model_getReaction(M, 0);
  fail_unless( r->kineticLaw == NULL, NULL );

  r = Model_getReaction(M, 1);
  fail_unless( r->kineticLaw != NULL, NULL );

  kl = r->kineticLaw;
  fail_unless( KineticLaw_getNumParameters(kl) == 1, NULL );
  fail_unless( KineticLaw_getParameter(kl, 0)  == p, NULL );
}
END_TEST


START_TEST (test_Model_createKineticLawParameter_noReaction)
{
  fail_unless( Model_getNumReactions(M)           == 0,    NULL );
  fail_unless( Model_createKineticLawParameter(M) == NULL, NULL );
}
END_TEST


START_TEST (test_Model_createKineticLawParameter_noKineticLaw)
{
  Reaction_t *r;


  r = Model_createReaction(M);

  fail_unless( r->kineticLaw == NULL, NULL );
  fail_unless( Model_createKineticLawParameter(M) == NULL, NULL );
}
END_TEST


/**
 * If I had time to do it over again, this is how I would write and
 * combine the get / add tests for collection (see below).
 */
START_TEST (test_Model_add_get_UnitDefinitions)
{
  UnitDefinition_t *ud1 = UnitDefinition_create();
  UnitDefinition_t *ud2 = UnitDefinition_create();


  Model_addUnitDefinition(M, ud1);
  Model_addUnitDefinition(M, ud2);

  fail_unless( Model_getNumUnitDefinitions(M) == 2,    NULL );
  fail_unless( Model_getUnitDefinition(M, 0)  == ud1,  NULL );
  fail_unless( Model_getUnitDefinition(M, 1)  == ud2,  NULL );
  fail_unless( Model_getUnitDefinition(M, 2)  == NULL, NULL );
  fail_unless( Model_getUnitDefinition(M, -2) == NULL, NULL );
}
END_TEST


START_TEST (test_Model_addCompartment)
{
  Model_addCompartment(M, Compartment_create());

  fail_unless( Model_getNumCompartments(M) == 1, NULL );
}
END_TEST


START_TEST (test_Model_addSpecies)
{
  Model_addSpecies(M, Species_create());

  fail_unless( Model_getNumSpecies(M) == 1, NULL );
}
END_TEST


START_TEST (test_Model_addParameter)
{
  Model_addParameter(M, Parameter_create());

  fail_unless( Model_getNumParameters(M) == 1, NULL );
}
END_TEST


START_TEST (test_Model_addRules)
{
  Model_addRule( M, (Rule_t *) AlgebraicRule_create()            );
  Model_addRule( M, (Rule_t *) SpeciesConcentrationRule_create() );
  Model_addRule( M, (Rule_t *) CompartmentVolumeRule_create()    );
  Model_addRule( M, (Rule_t *) ParameterRule_create()            );

  fail_unless( Model_getNumRules(M) == 4, NULL );
}
END_TEST


START_TEST (test_Model_addReaction)
{
  Model_addReaction(M, Reaction_create());

  fail_unless( Model_getNumReactions(M) == 1, NULL );
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

  fail_unless( Model_getNumUnitDefinitions(M) == 2, NULL );

  ud1 = Model_getUnitDefinition(M, 0);
  ud2 = Model_getUnitDefinition(M, 1);

  fail_unless( !strcmp( ud1->name, "mmls"   ), NULL );
  fail_unless( !strcmp( ud2->name, "volume" ), NULL );
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

  fail_unless( Model_getNumCompartments(M) == 2, NULL );

  c1 = Model_getCompartment(M, 0);
  c2 = Model_getCompartment(M, 1);

  fail_unless( !strcmp(c1->name, "A"), NULL );
  fail_unless( !strcmp(c2->name, "B"), NULL );

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

  fail_unless( Model_getNumSpecies(M) == 2, NULL );

  s1 = Model_getSpecies(M, 0);
  s2 = Model_getSpecies(M, 1);

  fail_unless( !strcmp( s1->name, "Glucose"     ), NULL );
  fail_unless( !strcmp( s2->name, "Glucose_6_P" ), NULL );

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

  fail_unless( Model_getNumParameters(M) == 2, NULL );

  p1 = Model_getParameter(M, 0);
  p2 = Model_getParameter(M, 1);

  fail_unless( !strcmp(p1->name, "Km1"), NULL );
  fail_unless( !strcmp(p2->name, "Km2"), NULL );

}
END_TEST


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

  fail_unless( Model_getNumRules(M) == 4, NULL );

  ar  = (AlgebraicRule_t *)            Model_getRule(M, 0);
  scr = (SpeciesConcentrationRule_t *) Model_getRule(M, 1);
  cvr = (CompartmentVolumeRule_t *)    Model_getRule(M, 2);
  pr  = (ParameterRule_t *)            Model_getRule(M, 3);

  fail_unless( !strcmp(ar->formula , "x + 1"        ), NULL );
  fail_unless( !strcmp(scr->formula, "k * t/(1 + k)"), NULL );
  fail_unless( !strcmp(cvr->formula, "0.10 * t"     ), NULL );
  fail_unless( !strcmp(pr->formula , "k3/k2"        ), NULL );
}
END_TEST


START_TEST (test_Model_getReaction)
{
  Reaction_t *r1 = Reaction_create();
  Reaction_t *r2 = Reaction_create();

  Reaction_setName(r1, "reaction_1");
  Reaction_setName(r2, "reaction_2");

  Model_addReaction(M, r1);
  Model_addReaction(M, r2);

  fail_unless( Model_getNumReactions(M) == 2, NULL );

  r1 = Model_getReaction(M, 0);
  r2 = Model_getReaction(M, 1);

  fail_unless( !strcmp(r1->name, "reaction_1"), NULL );
  fail_unless( !strcmp(r2->name, "reaction_2"), NULL );
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
  tcase_add_test( t, test_Model_createWithName );
  tcase_add_test( t, test_Model_setId          );
  tcase_add_test( t, test_Model_setName        );

  /**
   * Model_createXXX() methods
   */
  tcase_add_test( t, test_Model_createUnitDefinition                   );
  tcase_add_test( t, test_Model_createUnit                             );
  tcase_add_test( t, test_Model_createUnit_noUnitDefinition            );
  tcase_add_test( t, test_Model_createCompartment                      );
  tcase_add_test( t, test_Model_createSpecies                          );
  tcase_add_test( t, test_Model_createParameter                        );
  tcase_add_test( t, test_Model_createAlgebraicRule                    );
  tcase_add_test( t, test_Model_createCompartmentVolumeRule            );
  tcase_add_test( t, test_Model_createParameterRule                    );
  tcase_add_test( t, test_Model_createSpeciesConcentrationRule         );
  tcase_add_test( t, test_Model_createReaction                         );
  tcase_add_test( t, test_Model_createReactant                         );
  tcase_add_test( t, test_Model_createReactant_noReaction              );
  tcase_add_test( t, test_Model_createProduct                          );
  tcase_add_test( t, test_Model_createProduct_noReaction               );
  tcase_add_test( t, test_Model_createKineticLaw                       );
  tcase_add_test( t, test_Model_createKineticLaw_alreadyExists         );
  tcase_add_test( t, test_Model_createKineticLaw_noReaction            );
  tcase_add_test( t, test_Model_createKineticLawParameter              );
  tcase_add_test( t, test_Model_createKineticLawParameter_noReaction   );
  tcase_add_test( t, test_Model_createKineticLawParameter_noKineticLaw );

  /**
   * Model_addXXX() methods
   */
  tcase_add_test( t, test_Model_add_get_UnitDefinitions );
  tcase_add_test( t, test_Model_addCompartment          );
  tcase_add_test( t, test_Model_addSpecies              );
  tcase_add_test( t, test_Model_addParameter            );
  tcase_add_test( t, test_Model_addRules                );
  tcase_add_test( t, test_Model_addReaction             );

  /**
   * Model_getXXX() methods
   */
  tcase_add_test( t, test_Model_getUnitDefinition );
  tcase_add_test( t, test_Model_getCompartment    );
  tcase_add_test( t, test_Model_getSpecies        );
  tcase_add_test( t, test_Model_getParameter      );
  tcase_add_test( t, test_Model_getRules          );
  tcase_add_test( t, test_Model_getReaction       );

  suite_add_tcase(s, t);

  return s;
}
