/**
 * Filename    : TestSpeciesConcentrationRule.c
 * Description : SpeciesConcentrationRule unit tests
 * Author(s)   : SBML Development Group <sbml-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2002-11-26
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


#include <check.h>

#include "sbml/common.h"
#include "sbml/SpeciesConcentrationRule.h"


static SpeciesConcentrationRule_t *SCR;


void
SpeciesConcentrationRuleTest_setup (void)
{
  SCR = SpeciesConcentrationRule_create();

  if (SCR == NULL)
  {
    fail("SpeciesConcentrationRule_create() returned a NULL pointer.");
  }
}


void
SpeciesConcentrationRuleTest_teardown (void)
{
  SpeciesConcentrationRule_free(SCR);
}


START_TEST (test_SpeciesConcentrationRule_create)
{
  fail_unless( SBase_getTypeCode(SCR) == SBML_SPECIES_CONCENTRATION_RULE,
               NULL );

  fail_unless( SBase_getNotes     (SCR) == NULL, NULL );
  fail_unless( SBase_getAnnotation(SCR) == NULL, NULL );

  fail_unless( Rule_getFormula(SCR) == NULL, NULL );

  fail_unless( AssignmentRule_getType(SCR) == RULE_TYPE_SCALAR, NULL );

  fail_unless( SpeciesConcentrationRule_getSpecies(SCR) == NULL, NULL );

  fail_unless( !SpeciesConcentrationRule_isSetSpecies(SCR), NULL );
}
END_TEST


START_TEST (test_SpeciesConcentrationRule_createWith)
{
  SpeciesConcentrationRule_t *scr;


  scr = SpeciesConcentrationRule_createWith("t - s2", RULE_TYPE_RATE, "s1");


  fail_unless( SBase_getTypeCode(scr) == SBML_SPECIES_CONCENTRATION_RULE,
               NULL );

  fail_unless( SBase_getNotes     (scr) == NULL, NULL );
  fail_unless( SBase_getAnnotation(scr) == NULL, NULL );

  fail_unless( !strcmp(Rule_getFormula(scr), "t - s2"), NULL );
  fail_unless( !strcmp(SpeciesConcentrationRule_getSpecies(scr), "s1"), NULL );

  fail_unless( AssignmentRule_getType(scr) == RULE_TYPE_RATE, NULL );

  fail_unless( SpeciesConcentrationRule_isSetSpecies(scr), NULL );

  SpeciesConcentrationRule_free(scr);
}
END_TEST


START_TEST (test_SpeciesConcentrationRule_free_NULL)
{
  SpeciesConcentrationRule_free(NULL);
}
END_TEST


START_TEST (test_SpeciesConcentrationRule_setSpecies)
{
  char       *species = "s2";
  const char *s;


  SpeciesConcentrationRule_setSpecies(SCR, species);

  s = SpeciesConcentrationRule_getSpecies(SCR);
  fail_unless( !strcmp(s, species), NULL );

  fail_unless( SpeciesConcentrationRule_isSetSpecies(SCR), NULL );

  if (SpeciesConcentrationRule_getSpecies(SCR) == species)
  {
    fail( "SpeciesConcentrationRule_setSpecies(...)"
          " did not make a copy of string." );
  }

  /* Reflexive case (pathological) */
  s = SpeciesConcentrationRule_getSpecies(SCR);
  SpeciesConcentrationRule_setSpecies(SCR, s);
  fail_unless(!strcmp(s, species), NULL);

  SpeciesConcentrationRule_setSpecies(SCR, NULL);
  fail_unless( !SpeciesConcentrationRule_isSetSpecies(SCR), NULL );

  if (SpeciesConcentrationRule_getSpecies(SCR) != NULL)
  {
    fail( "SpeciesConcentrationRule_setSpecies(SCR, NULL)"
          " did not clear string." );
  }
}
END_TEST


Suite *
create_suite_SpeciesConcentrationRule (void)
{
  Suite *suite = suite_create("SpeciesConcentrationRule");
  TCase *tcase = tcase_create("SpeciesConcentrationRule");


  tcase_add_checked_fixture( tcase,
                             SpeciesConcentrationRuleTest_setup,
                             SpeciesConcentrationRuleTest_teardown );

  tcase_add_test( tcase, test_SpeciesConcentrationRule_create     );
  tcase_add_test( tcase, test_SpeciesConcentrationRule_createWith );
  tcase_add_test( tcase, test_SpeciesConcentrationRule_free_NULL  );
  tcase_add_test( tcase, test_SpeciesConcentrationRule_setSpecies );

  suite_add_tcase(suite, tcase);

  return suite;
}
