/**
 * \file    TestSpeciesReference.c
 * \brief   SpeciesReference unit tests
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


#include "SBase.h"
#include "SpeciesReference.h"

#include <check.h>


static SpeciesReference_t *SR;


void
SpeciesReferenceTest_setup (void)
{
  SR = SpeciesReference_create();

  if (SR == NULL)
  {
    fail("SpeciesReference_create() returned a NULL pointer.");
  }
}


void
SpeciesReferenceTest_teardown (void)
{
  SpeciesReference_free(SR);
}


START_TEST (test_SpeciesReference_create)
{
  fail_unless( SBase_getTypeCode  ((SBase_t *) SR) == SBML_SPECIES_REFERENCE );
  fail_unless( SBase_getMetaId    ((SBase_t *) SR) == NULL );
  //fail_unless( SBase_getNotes     ((SBase_t *) SR) == NULL );
  //fail_unless( SBase_getAnnotation((SBase_t *) SR) == NULL );

  fail_unless( SpeciesReference_getSpecies          (SR) == NULL );
  fail_unless( SpeciesReference_getStoichiometry    (SR) == 1    );
  fail_unless( SpeciesReference_getStoichiometryMath(SR) == NULL );
  fail_unless( SpeciesReference_getDenominator      (SR) == 1    );

  fail_unless( !SpeciesReference_isSetSpecies(SR) );
  fail_unless( !SpeciesReference_isSetStoichiometryMath(SR) );
}
END_TEST


START_TEST (test_SpeciesReference_createWith)
{
  SpeciesReference_t *sr = SpeciesReference_createWith("s3", 4, 2);


  fail_unless( SBase_getTypeCode  ((SBase_t *) sr) == SBML_SPECIES_REFERENCE );
  fail_unless( SBase_getMetaId    ((SBase_t *) sr) == NULL );
  //fail_unless( SBase_getNotes     ((SBase_t *) sr) == NULL );
  //fail_unless( SBase_getAnnotation((SBase_t *) sr) == NULL );

  fail_unless( !strcmp(SpeciesReference_getSpecies(sr), "s3") );

  fail_unless( SpeciesReference_getStoichiometry(sr) == 4    );
  fail_unless( SpeciesReference_getDenominator  (sr) == 2    );

  fail_unless( SpeciesReference_isSetSpecies(sr) );

  SpeciesReference_free(sr);
}
END_TEST


START_TEST (test_SpeciesReference_createModifier)
{
  SpeciesReference_t *sr = SpeciesReference_createModifier();


  fail_unless( SBase_getTypeCode  ((SBase_t *) sr) == SBML_MODIFIER_SPECIES_REFERENCE );
  fail_unless( SBase_getMetaId    ((SBase_t *) sr) == NULL );
  //fail_unless( SBase_getNotes     ((SBase_t *) sr) == NULL );
  //fail_unless( SBase_getAnnotation((SBase_t *) sr) == NULL );

  fail_unless( SpeciesReference_isModifier(sr));
  SpeciesReference_free(sr);
}
END_TEST


START_TEST (test_SpeciesReference_free_NULL)
{
  SpeciesReference_free(NULL);
}
END_TEST


START_TEST (test_SpeciesReference_setSpecies)
{
  char *species = "X0";


  SpeciesReference_setSpecies(SR, species);

  fail_unless( !strcmp(SpeciesReference_getSpecies(SR), species) );
  fail_unless( SpeciesReference_isSetSpecies(SR) );

  if (SpeciesReference_getSpecies(SR) == species)
  {
    fail("SpeciesReference_setSpecies(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  SpeciesReference_setSpecies(SR, SpeciesReference_getSpecies(SR));
  fail_unless( !strcmp(SpeciesReference_getSpecies(SR), species) );

  SpeciesReference_setSpecies(SR, NULL);
  fail_unless( !SpeciesReference_isSetSpecies(SR) );

  if (SpeciesReference_getSpecies(SR) != NULL)
  {
    fail("SpeciesReference_setSpecies(SR, NULL) did not clear string.");
  }
}
END_TEST


START_TEST (test_SpeciesReference_setId)
{
  char *species = "X0";


  SpeciesReference_setId(SR, species);

  fail_unless( !strcmp(SpeciesReference_getId(SR), species) );
  fail_unless( SpeciesReference_isSetId(SR) );

  if (SpeciesReference_getId(SR) == species)
  {
    fail("SpeciesReference_setId(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  SpeciesReference_setId(SR, SpeciesReference_getId(SR));
  fail_unless( !strcmp(SpeciesReference_getId(SR), species) );

  SpeciesReference_setId(SR, NULL);
  fail_unless( !SpeciesReference_isSetId(SR) );

  if (SpeciesReference_getId(SR) != NULL)
  {
    fail("SpeciesReference_setId(SR, NULL) did not clear string.");
  }
}
END_TEST


START_TEST (test_SpeciesReference_setStoichiometryMath)
{
  ASTNode_t *math = SBML_parseFormula("k3 / k2");

  ASTNode_t * math1;
  char * formula;


  SpeciesReference_setStoichiometryMath(SR, math);

  math1 = SpeciesReference_getStoichiometryMath(SR);
  fail_unless( math1 != NULL );

  formula = SBML_formulaToString(math1);
  fail_unless( formula != NULL );
  fail_unless( !strcmp(formula, "k3 / k2") );

  fail_unless( SpeciesReference_isSetStoichiometryMath(SR) );

  safe_free(formula);
  ASTNode_free(math);

}
END_TEST


Suite *
create_suite_SpeciesReference (void)
{
  Suite *suite = suite_create("SpeciesReference");
  TCase *tcase = tcase_create("SpeciesReference");


  tcase_add_checked_fixture( tcase,
                             SpeciesReferenceTest_setup,
                             SpeciesReferenceTest_teardown );

  tcase_add_test( tcase, test_SpeciesReference_create               );
  tcase_add_test( tcase, test_SpeciesReference_createWith           );
  tcase_add_test( tcase, test_SpeciesReference_createModifier           );
  tcase_add_test( tcase, test_SpeciesReference_free_NULL            );
  tcase_add_test( tcase, test_SpeciesReference_setSpecies           );
  tcase_add_test( tcase, test_SpeciesReference_setId           );
  tcase_add_test( tcase, test_SpeciesReference_setStoichiometryMath );

  suite_add_tcase(suite, tcase);

  return suite;
}
