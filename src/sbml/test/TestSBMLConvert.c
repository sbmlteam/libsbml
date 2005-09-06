/**
 * \file    TestSBMLConvert.c
 * \brief   SBMLConvert unit tests
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2003 California Institute of Technology and
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


#include <check.h>

#include "common/common.h"
#include "SBMLConvert.h"
#include "SBMLTypes.h"


START_TEST (test_SBMLConvert_convertToL2_SBMLDocument)
{
  SBMLDocument_t *d = SBMLDocument_createWith(1, 2);
  Model_t        *m = SBMLDocument_createModel(d);


  SBML_convertToL2( m, (SBase_t *) d);

  fail_unless( SBMLDocument_getLevel  (d) == 2, NULL );
  fail_unless( SBMLDocument_getVersion(d) == 1, NULL );

  SBMLDocument_free(d);
}
END_TEST


START_TEST (test_SBMLConvert_convertNameToId_Model)
{
  const char *id = "Model";
  Model_t    *m  = Model_create();


  fail_unless( !Model_isSetId  (m), NULL );
  fail_unless( !Model_isSetName(m), NULL );

  Model_setName(m, id);

  fail_unless( !Model_isSetId  (m), NULL );
  fail_unless(  Model_isSetName(m), NULL );

  SBML_convertNameToId( (SBase_t *) m);

  fail_unless(  Model_isSetId  (m), NULL );
  fail_unless( !Model_isSetName(m), NULL );

  fail_unless( !strcmp(Model_getId(m), id), NULL );

  Model_free(m);
}
END_TEST


START_TEST (test_SBMLConvert_convertNameToId_UnitDefinition)
{
  const char       *id = "bps";
  UnitDefinition_t *ud = UnitDefinition_create();


  fail_unless( !UnitDefinition_isSetId  (ud), NULL );
  fail_unless( !UnitDefinition_isSetName(ud), NULL );

  UnitDefinition_setName(ud, id);

  fail_unless( !UnitDefinition_isSetId  (ud), NULL );
  fail_unless(  UnitDefinition_isSetName(ud), NULL );

  SBML_convertNameToId( (SBase_t *) ud);

  fail_unless(  UnitDefinition_isSetId  (ud), NULL );
  fail_unless( !UnitDefinition_isSetName(ud), NULL );

  fail_unless( !strcmp(UnitDefinition_getId(ud), id), NULL );

  UnitDefinition_free(ud);
}
END_TEST


START_TEST (test_SBMLConvert_convertNameToId_Compartment)
{
  const char    *id = "cell2";
  Compartment_t *c  = Compartment_create();


  fail_unless( !Compartment_isSetId  (c), NULL );
  fail_unless( !Compartment_isSetName(c), NULL );

  Compartment_setName(c, id);

  fail_unless( !Compartment_isSetId  (c), NULL );
  fail_unless(  Compartment_isSetName(c), NULL );

  SBML_convertNameToId( (SBase_t *) c);

  fail_unless(  Compartment_isSetId  (c), NULL );
  fail_unless( !Compartment_isSetName(c), NULL );

  fail_unless( !strcmp(Compartment_getId(c), id), NULL );

  Compartment_free(c);
}
END_TEST


START_TEST (test_SBMLConvert_convertNameToId_Species)
{
  const char *id = "X1";
  Species_t  *s  = Species_create();


  fail_unless( !Species_isSetId  (s), NULL );
  fail_unless( !Species_isSetName(s), NULL );

  Species_setName(s, id);

  fail_unless( !Species_isSetId  (s), NULL );
  fail_unless(  Species_isSetName(s), NULL );

  SBML_convertNameToId( (SBase_t *) s);

  fail_unless(  Species_isSetId  (s), NULL );
  fail_unless( !Species_isSetName(s), NULL );

  fail_unless( !strcmp(Species_getId(s), id), NULL );

  Species_free(s);
}
END_TEST


START_TEST (test_SBMLConvert_convertNameToId_Parameter)
{
  const char  *id = "x";
  Parameter_t *p  = Parameter_create();


  fail_unless( !Parameter_isSetId  (p), NULL );
  fail_unless( !Parameter_isSetName(p), NULL );

  Parameter_setName(p, id);

  fail_unless( !Parameter_isSetId  (p), NULL );
  fail_unless(  Parameter_isSetName(p), NULL );

  SBML_convertNameToId( (SBase_t *) p);

  fail_unless(  Parameter_isSetId  (p), NULL );
  fail_unless( !Parameter_isSetName(p), NULL );

  fail_unless( !strcmp(Parameter_getId(p), id), NULL );

  Parameter_free(p);
}
END_TEST


START_TEST (test_SBMLConvert_convertNameToId_Reaction)
{
  const char *id = "r1";
  Reaction_t *r  = Reaction_create();


  fail_unless( !Reaction_isSetId  (r), NULL );
  fail_unless( !Reaction_isSetName(r), NULL );

  Reaction_setName(r, id);

  fail_unless( !Reaction_isSetId  (r), NULL );
  fail_unless(  Reaction_isSetName(r), NULL );

  SBML_convertNameToId( (SBase_t *) r);

  fail_unless(  Reaction_isSetId  (r), NULL );
  fail_unless( !Reaction_isSetName(r), NULL );

  fail_unless( !strcmp(Reaction_getId(r), id), NULL );

  Reaction_free(r);
}
END_TEST


START_TEST (test_SBMLConvert_addModifiersToReaction)
{
  Model_t      *m  = Model_create();
  KineticLaw_t *kl = KineticLaw_createWith("k1*S1*S2*S3*S4*S5", NULL, NULL);
  Reaction_t   *r  = Reaction_createWith("R", kl, 1, 0);

  SimpleSpeciesReference_t *ssr1;
  SimpleSpeciesReference_t *ssr2;


  Model_addSpecies( m, Species_createWith("S1", "cell", 2.0, "amount", 0, 0) );
  Model_addSpecies( m, Species_createWith("S2", "cell", 2.0, "amount", 0, 0) );
  Model_addSpecies( m, Species_createWith("S3", "cell", 1.0, "amount", 0, 0) );
  Model_addSpecies( m, Species_createWith("S4", "cell", 1.0, "amount", 0, 0) );
  Model_addSpecies( m, Species_createWith("S5", "cell", 0.0, "amount", 0, 0) );

  Reaction_addReactant( r, SpeciesReference_createWith("S1", 1, 1) );
  Reaction_addReactant( r, SpeciesReference_createWith("S2", 1, 1) );
  Reaction_addProduct ( r, SpeciesReference_createWith("S5", 1, 1) );

  Model_addReaction(m, r);

  fail_unless( Reaction_getNumModifiers(r) == 0, NULL );

  SBML_addModifiersToReaction(r, m);

  fail_unless( Reaction_getNumModifiers(r) == 2, NULL );

  ssr1 = (SimpleSpeciesReference_t *) Reaction_getModifier(r, 0);
  ssr2 = (SimpleSpeciesReference_t *) Reaction_getModifier(r, 1);

  fail_unless( !strcmp(SimpleSpeciesReference_getSpecies(ssr1), "S3"), NULL );
  fail_unless( !strcmp(SimpleSpeciesReference_getSpecies(ssr2), "S4"), NULL );

  Model_free(m);
}
END_TEST


START_TEST (test_SBMLConvert_convertRuleToL2_SpeciesConcentrationRule)
{
  SpeciesConcentrationRule_t *scr;

  Model_t      *m  = Model_create();
  Species_t    *s1 = Model_createSpecies(m);
  Species_t    *s2 = Model_createSpecies(m);
  Species_t    *s3 = Model_createSpecies(m);

  Species_setId(s1, "s1");
  Species_setId(s2, "s2");
  Species_setId(s3, "s3");

  Species_setConstant(s1, 1);
  Species_setConstant(s2, 1);
  Species_setConstant(s3, 1);

  scr = Model_createSpeciesConcentrationRule(m);
  SpeciesConcentrationRule_setSpecies(scr, "s2");


  fail_unless( Species_getConstant(s1) != 0 );
  fail_unless( Species_getConstant(s2) != 0 );
  fail_unless( Species_getConstant(s3) != 0 );

  SBML_convertRuleToL2(m, (Rule_t *) scr);

  fail_unless( Species_getConstant(s1) != 0 );
  fail_unless( Species_getConstant(s2) == 0 );
  fail_unless( Species_getConstant(s3) != 0 );

  Model_free(m);
}
END_TEST


START_TEST (test_SBMLConvert_convertRuleToL2_CompartmentVolumeRule)
{
  CompartmentVolumeRule_t *cvr;

  Model_t       *m  = Model_create();
  Compartment_t *c1 = Model_createCompartment(m);
  Compartment_t *c2 = Model_createCompartment(m);
  Compartment_t *c3 = Model_createCompartment(m);

  Compartment_setId(c1, "c1");
  Compartment_setId(c2, "c2");
  Compartment_setId(c3, "c3");

  cvr = Model_createCompartmentVolumeRule(m);
  CompartmentVolumeRule_setCompartment(cvr, "c2");


  fail_unless( Compartment_getConstant(c1) != 0 );
  fail_unless( Compartment_getConstant(c2) != 0 );
  fail_unless( Compartment_getConstant(c3) != 0 );

  SBML_convertRuleToL2(m, (Rule_t *) cvr);

  fail_unless( Compartment_getConstant(c1) != 0 );
  fail_unless( Compartment_getConstant(c2) == 0 );
  fail_unless( Compartment_getConstant(c3) != 0 );

  Model_free(m);
}
END_TEST


START_TEST (test_SBMLConvert_convertRuleToL2_ParameterRule)
{
  ParameterRule_t *pr;

  Model_t     *m  = Model_create();
  Parameter_t *p1 = Model_createParameter(m);
  Parameter_t *p2 = Model_createParameter(m);
  Parameter_t *p3 = Model_createParameter(m);

  Parameter_setId(p1, "p1");
  Parameter_setId(p2, "p2");
  Parameter_setId(p3, "p3");

  pr = Model_createParameterRule(m);
  ParameterRule_setName(pr, "p2");


  fail_unless( Parameter_getConstant(p1) != 0 );
  fail_unless( Parameter_getConstant(p2) != 0 );
  fail_unless( Parameter_getConstant(p3) != 0 );

  SBML_convertRuleToL2(m, (Rule_t *) pr);

  fail_unless( Parameter_getConstant(p1) != 0 );
  fail_unless( Parameter_getConstant(p2) == 0 );
  fail_unless( Parameter_getConstant(p3) != 0 );

  Model_free(m);
}
END_TEST

START_TEST (test_SBMLConvert_convertToL1_SBMLDocument)
{
  SBMLDocument_t *d = SBMLDocument_createWith(2, 1);
  Model_t        *m = SBMLDocument_createModel(d);


  SBML_convertToL1( m, (SBase_t *) d);

  fail_unless( SBMLDocument_getLevel  (d) == 1, NULL );
  fail_unless( SBMLDocument_getVersion(d) == 2, NULL );

  SBMLDocument_free(d);
}
END_TEST

START_TEST (test_SBMLConvert_convertToL1_Species_Amount)
{
  SBMLDocument_t *d = SBMLDocument_createWith(2, 1);
  Model_t        *m = SBMLDocument_createModel(d);
  const char   *sid = "C";
  Compartment_t  *c = Compartment_create();
  Species_t      *s = Species_create();

  Compartment_setId    (c, sid); 
  Model_addCompartment ( m, c);

  Species_setCompartment  ( s, sid); 
  Species_setInitialAmount( s, 2.34);
  Model_addSpecies        ( m, s);
  
  SBML_convertToL1( m, (SBase_t *) d);

  fail_unless( Species_getInitialAmount (s) == 2.34, NULL );

  SBMLDocument_free(d);
}
END_TEST

START_TEST (test_SBMLConvert_convertToL1_Species_Concentration)
{
  SBMLDocument_t *d = SBMLDocument_createWith(2, 1);
  Model_t        *m = SBMLDocument_createModel(d);
  const char   *sid = "C";
  Compartment_t  *c = Compartment_create();
  Species_t      *s = Species_create();

  Compartment_setId    (c, sid);
  Compartment_setSize  (c, 1.2); 
  Model_addCompartment ( m, c);

  Species_setCompartment         ( s, sid); 
  Species_setInitialConcentration( s, 2.34);
  Model_addSpecies               ( m, s);
  
  SBML_convertToL1( m, (SBase_t *) d);

  fail_unless( Species_getInitialAmount (s) == 2.808, NULL );

  SBMLDocument_free(d);
}
END_TEST

START_TEST (test_SBMLConvert_convertRuleToL1_SpeciesConcentrationRule)
{
  AssignmentRule_t *ar;
  RateRule_t *rr;
  SpeciesConcentrationRule_t *scr, *scr1;
  const char *s, *ss;

  Model_t      *m  = Model_create();
  Species_t    *s1 = Model_createSpecies(m);
  Species_t    *s2 = Model_createSpecies(m);

  Species_setId(s1, "s1");
  Species_setId(s2, "s2");

  ar = Model_createAssignmentRule(m);
  AssignmentRule_setVariable(ar, "s1");

  rr = Model_createRateRule(m);
  RateRule_setVariable(rr, "s2");

  SBML_convertAllRulesToL1(m);

  scr = (SpeciesConcentrationRule_t *)Model_getRule(m,0);
  scr1 = (SpeciesConcentrationRule_t *)Model_getRule(m,1);
  s = SpeciesConcentrationRule_getSpecies(scr);
  ss = SpeciesConcentrationRule_getSpecies(scr1);

  fail_unless( !strcmp(s, "s1") );
  fail_unless( AssignmentRule_getType((AssignmentRule_t *) scr) ==
               RULE_TYPE_SCALAR );
  fail_unless( !strcmp(ss, "s2") );
  fail_unless( AssignmentRule_getType((AssignmentRule_t *) scr1) ==
               RULE_TYPE_RATE );

  Model_free(m);
}
END_TEST

START_TEST (test_SBMLConvert_convertRuleToL1_CompartmentVolumeRule)
{
  AssignmentRule_t *ar;
  RateRule_t *rr;
  CompartmentVolumeRule_t *cvr, *cvr1;
  const char *s, *ss;

  Model_t      *m  = Model_create();
  Compartment_t    *s1 = Model_createCompartment(m);
  Compartment_t    *s2 = Model_createCompartment(m);

  Compartment_setId(s1, "s1");
  Compartment_setId(s2, "s2");


  ar = Model_createAssignmentRule(m);
  AssignmentRule_setVariable(ar, "s1");

  rr = Model_createRateRule(m);
  RateRule_setVariable(rr, "s2");

  SBML_convertAllRulesToL1(m);

  cvr = (CompartmentVolumeRule_t *)Model_getRule(m,0);
  cvr1 = (CompartmentVolumeRule_t *)Model_getRule(m,1);
  s = CompartmentVolumeRule_getCompartment(cvr);
  ss = CompartmentVolumeRule_getCompartment(cvr1);

  fail_unless( !strcmp(s, "s1") );
  fail_unless( AssignmentRule_getType((AssignmentRule_t *) cvr) ==
               RULE_TYPE_SCALAR );
  fail_unless( !strcmp(ss, "s2") );
  fail_unless( AssignmentRule_getType((AssignmentRule_t *) cvr1) ==
               RULE_TYPE_RATE );

  Model_free(m);
}
END_TEST

START_TEST (test_SBMLConvert_convertRuleToL1_ParameterRule)
{
  AssignmentRule_t *ar;
  RateRule_t *rr;
  ParameterRule_t *pr, *pr1;
  const char *s, *ss;

  Model_t      *m  = Model_create();
  Parameter_t    *s1 = Model_createParameter(m);
  Parameter_t    *s2 = Model_createParameter(m);

  Parameter_setId(s1, "s1");
  Parameter_setId(s2, "s2");


  ar = Model_createAssignmentRule(m);
  AssignmentRule_setVariable(ar, "s1");

  rr = Model_createRateRule(m);
  RateRule_setVariable(rr, "s2");

  SBML_convertAllRulesToL1(m);

  pr = (ParameterRule_t *)Model_getRule(m,0);
  pr1 = (ParameterRule_t *)Model_getRule(m,1);
  s = ParameterRule_getName(pr);
  ss = ParameterRule_getName(pr1);

  fail_unless( !strcmp(s, "s1") );
  fail_unless( AssignmentRule_getType((AssignmentRule_t *) pr) ==
               RULE_TYPE_SCALAR );
  fail_unless( !strcmp(ss, "s2") );
  fail_unless( AssignmentRule_getType((AssignmentRule_t *) pr1) ==
               RULE_TYPE_RATE );

  Model_free(m);
}
END_TEST

Suite *
create_suite_SBMLConvert (void) 
{ 
  Suite *suite = suite_create("SBMLConvert");
  TCase *tcase = tcase_create("SBMLConvert");


  tcase_add_test( tcase, test_SBMLConvert_convertToL2_SBMLDocument       );
  tcase_add_test( tcase, test_SBMLConvert_convertNameToId_Model          );
  tcase_add_test( tcase, test_SBMLConvert_convertNameToId_UnitDefinition );
  tcase_add_test( tcase, test_SBMLConvert_convertNameToId_Compartment    );
  tcase_add_test( tcase, test_SBMLConvert_convertNameToId_Species        );
  tcase_add_test( tcase, test_SBMLConvert_convertNameToId_Parameter      );
  tcase_add_test( tcase, test_SBMLConvert_convertNameToId_Reaction       );
  tcase_add_test( tcase, test_SBMLConvert_addModifiersToReaction         );

  tcase_add_test( tcase,
                  test_SBMLConvert_convertRuleToL2_SpeciesConcentrationRule );

  tcase_add_test( tcase,
                  test_SBMLConvert_convertRuleToL2_CompartmentVolumeRule );

  tcase_add_test( tcase, test_SBMLConvert_convertRuleToL2_ParameterRule  );
  tcase_add_test( tcase, test_SBMLConvert_convertToL1_SBMLDocument       );
  tcase_add_test( tcase, test_SBMLConvert_convertToL1_Species_Amount     );
  tcase_add_test( tcase, 
                  test_SBMLConvert_convertToL1_Species_Concentration       );
  tcase_add_test( tcase, 
                  test_SBMLConvert_convertRuleToL1_SpeciesConcentrationRule);
  tcase_add_test( tcase, 
                  test_SBMLConvert_convertRuleToL1_CompartmentVolumeRule);
  tcase_add_test( tcase, 
                  test_SBMLConvert_convertRuleToL1_ParameterRule);


  suite_add_tcase(suite, tcase);

  return suite;
}
