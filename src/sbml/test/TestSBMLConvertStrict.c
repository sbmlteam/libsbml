/**
 * \file    TestSBMLConvertStrict.c
 * \brief   SBMLConvert unit tests for strict conversion
 * \author  Sarah Keating
 *
 * $Id: TestSBMLConvert.c 9087 2009-02-17 01:41:43Z sarahkeating $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/branches/libsbml-4/src/sbml/test/TestSBMLConvert.c $
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


#include <sbml/common/common.h>
#include <sbml/SBMLDocument.h>
#include <sbml/SBMLTypes.h>

#include <check.h>


START_TEST (test_SBMLConvertStrict_convertNonStrictUnits)
{
  SBMLDocument_t *d = SBMLDocument_createWithLevelAndVersion(2, 4);
  Model_t * m = SBMLDocument_createModel(d);
  
  /* create a compartment */
  Compartment_t * c = Model_createCompartment(m);
  Compartment_setId(c, "c");
  Compartment_setConstant(c, 0);

  /* create  a parameter with units mole */
  Parameter_t * p = Model_createParameter(m);
  Parameter_setId(p, "p");
  Parameter_setUnits(p, "mole");

  /* create a math element */
  ASTNode_t *math = SBML_parseFormula("p");

  /* create an assignment rule */
  Rule_t *ar = Model_createAssignmentRule(m);
  Rule_setVariable(ar, "c");
  Rule_setMath(ar, math);

  /* these should all fail since the model has bad units */

  unsigned int success = SBMLDocument_setLevelAndVersionStrict(d, 2, 1);

  fail_unless( success == 0 );
  fail_unless( SBMLDocument_getLevel  (d) == 2, NULL );
  fail_unless( SBMLDocument_getVersion(d) == 4, NULL );

  success = SBMLDocument_setLevelAndVersionStrict(d, 2, 2);
  
  fail_unless( success == 0 );
  fail_unless( SBMLDocument_getLevel  (d) == 2, NULL );
  fail_unless( SBMLDocument_getVersion(d) == 4, NULL );

  success = SBMLDocument_setLevelAndVersionStrict(d, 2, 3);
  
  fail_unless( success == 0 );
  fail_unless( SBMLDocument_getLevel  (d) == 2, NULL );
  fail_unless( SBMLDocument_getVersion(d) == 4, NULL );

  success = SBMLDocument_setLevelAndVersionStrict(d, 1, 2);
  
  fail_unless( success == 0 );
  fail_unless( SBMLDocument_getLevel  (d) == 2, NULL );
  fail_unless( SBMLDocument_getVersion(d) == 4, NULL );

  SBMLDocument_free(d);
}
END_TEST


START_TEST (test_SBMLConvertStrict_convertNonStrictSBO)
{
  SBMLDocument_t *d = SBMLDocument_createWithLevelAndVersion(2, 4);
  Model_t * m = SBMLDocument_createModel(d);
  
  /* create a compartment with SBO */
  Compartment_t * c = Model_createCompartment(m);
  Compartment_setId(c, "c");
  Compartment_setConstant(c, 0);
  SBase_setSBOTerm((SBase_t *) (c), 64);

  /* conversion to L2V3 and L2V2 should fail due to bad SBO
   * but to L2V1 and L1 should pass as sbo terms not applicable
   */
  unsigned int success = SBMLDocument_setLevelAndVersionStrict(d, 2, 3);

  fail_unless( success == 0 );
  fail_unless( SBMLDocument_getLevel  (d) == 2, NULL );
  fail_unless( SBMLDocument_getVersion(d) == 4, NULL );
  
  success = SBMLDocument_setLevelAndVersionStrict(d, 2, 2);
  
  fail_unless( success == 0 );
  fail_unless( SBMLDocument_getLevel  (d) == 2, NULL );
  fail_unless( SBMLDocument_getVersion(d) == 4, NULL );

  success = SBMLDocument_setLevelAndVersionStrict(d, 2, 1);
  
  fail_unless( success == 1 );
  fail_unless( SBMLDocument_getLevel  (d) == 2, NULL );
  fail_unless( SBMLDocument_getVersion(d) == 1, NULL );

  /* check that sbo term has been removed */
  Compartment_t *c1 = Model_getCompartment(SBMLDocument_getModel(d), 0);

  fail_unless (SBase_getSBOTerm((SBase_t *) (c1)) == -1, NULL );

  success = SBMLDocument_setLevelAndVersionStrict(d, 1, 2);
  
  fail_unless( success == 1 );
  fail_unless( SBMLDocument_getLevel  (d) == 1, NULL );
  fail_unless( SBMLDocument_getVersion(d) == 2, NULL );
  
  /* check that sbo term has been removed */
  Compartment_t *c2 = Model_getCompartment(SBMLDocument_getModel(d), 0);

  fail_unless (SBase_getSBOTerm((SBase_t *) (c2)) == -1, NULL );

  SBMLDocument_free(d);
}
END_TEST


START_TEST (test_SBMLConvertStrict_convertToL1)
{
  SBMLDocument_t *d = SBMLDocument_createWithLevelAndVersion(2, 4);
  
  /* create model with metaid */
  Model_t * m = SBMLDocument_createModel(d);
  SBase_setMetaId((SBase_t *) (m), "_m");
  
  /* create a compartment with sbo*/
  Compartment_t * c = Model_createCompartment(m);
  Compartment_setId(c, "c");
  SBase_setSBOTerm((SBase_t *) (c), 240);

  /* create a species with hasOnlySubstanceUnits = true*/
  Species_t *s = Model_createSpecies(m);
  Species_setId(s, "s");
  Species_setCompartment(s, "c");
  Species_setHasOnlySubstanceUnits(s, 1);

  unsigned int success = SBMLDocument_setLevelAndVersionStrict(d, 1, 2);

  fail_unless( success == 1 );
  fail_unless( SBMLDocument_getLevel  (d) == 1, NULL );
  fail_unless( SBMLDocument_getVersion(d) == 2, NULL );

  ///* check that attributes that are no longer valid have been removed */
  Model_t * m1 = SBMLDocument_getModel(d);

  fail_unless (SBase_getMetaId((SBase_t *) (m1)) == NULL);

  Compartment_t *c1 = Model_getCompartment(m1, 0);

  fail_unless (SBase_getSBOTerm((SBase_t *) (c1)) == -1, NULL );

  Species_t *s1 = Model_getSpecies(m1, 0);

  fail_unless (Species_getHasOnlySubstanceUnits(s1) == 0);

  SBMLDocument_free(d);
}
END_TEST


START_TEST (test_SBMLConvertStrict_convertSBO)
{
  SBMLDocument_t *d = SBMLDocument_createWithLevelAndVersion(2, 4);
  Model_t * m = SBMLDocument_createModel(d);
  
  /* create a compartment with SBO */
  Compartment_t * c = Model_createCompartment(m);
  Compartment_setId(c, "c");
  SBase_setSBOTerm((SBase_t *) (c), 240);

  unsigned int success = SBMLDocument_setLevelAndVersionStrict(d, 2, 3);

  fail_unless( success == 1 );
  fail_unless( SBMLDocument_getLevel  (d) == 2, NULL );
  fail_unless( SBMLDocument_getVersion(d) == 3, NULL );
  
  success = SBMLDocument_setLevelAndVersionStrict(d, 2, 2);
  
  fail_unless( success == 1 );
  fail_unless( SBMLDocument_getLevel  (d) == 2, NULL );
  fail_unless( SBMLDocument_getVersion(d) == 2, NULL );

  /* check that sbo term has been removed */
  Compartment_t *c1 = Model_getCompartment(SBMLDocument_getModel(d), 0);

  fail_unless (SBase_getSBOTerm((SBase_t *) (c1)) == -1, NULL );

  SBMLDocument_free(d);
}
END_TEST


START_TEST (test_SBMLConvertStrict_convertL1ParamRule)
{
  SBMLDocument_t *d = SBMLDocument_createWithLevelAndVersion(1, 2);
  Model_t * m = SBMLDocument_createModel(d);
  
  /* create a compartment */
  Compartment_t * c = Model_createCompartment(m);
  Compartment_setId(c, "c");

  /* create  a parameter */
  Parameter_t * p = Model_createParameter(m);
  Parameter_setId(p, "p");
  Parameter_t * p1 = Model_createParameter(m);
  Parameter_setId(p1, "p1");

  /* create a math element */
  ASTNode_t *math = SBML_parseFormula("p");

  /* create an assignment rule */
  Rule_t *ar = Model_createAssignmentRule(m);
  Rule_setVariable(ar, "p1");
  Rule_setMath(ar, math);
  Rule_setUnits(ar, "mole");

  unsigned int success = SBMLDocument_setLevelAndVersionStrict(d, 2, 1);

  fail_unless( success == 1 );
  fail_unless( SBMLDocument_getLevel  (d) == 2, NULL );
  fail_unless( SBMLDocument_getVersion(d) == 1, NULL );

  Rule_t * r1 = Model_getRule(SBMLDocument_getModel(d), 0);

  fail_unless (Rule_getUnits(r1) == NULL );

  SBMLDocument_free(d);
}
END_TEST


Suite *
create_suite_SBMLConvertStrict (void) 
{ 
  Suite *suite = suite_create("SBMLConvertStrict");
  TCase *tcase = tcase_create("SBMLConvertStrict");


  tcase_add_test( tcase, test_SBMLConvertStrict_convertNonStrictUnits       );
  tcase_add_test( tcase, test_SBMLConvertStrict_convertNonStrictSBO         );
  tcase_add_test( tcase, test_SBMLConvertStrict_convertToL1                 );
  tcase_add_test( tcase, test_SBMLConvertStrict_convertSBO                  );
  tcase_add_test( tcase, test_SBMLConvertStrict_convertL1ParamRule          );

  suite_add_tcase(suite, tcase);

  return suite;
}
