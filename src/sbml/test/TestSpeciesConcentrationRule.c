/**
 * \file    TestSpeciesConcentrationRule.c
 * \brief   SpeciesConcentrationRule unit tests
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


#include <sbml/common/common.h>

#include <sbml/SBase.h>
#include <sbml/Rule.h>

#include <check.h>

static Rule_t *SCR;


void
SpeciesConcentrationRuleTest_setup (void)
{
  SCR = Rule_createAssignment();
  Rule_setL1TypeCode(SCR, SBML_SPECIES_CONCENTRATION_RULE);

  if (SCR == NULL)
  {
    fail("SpeciesConcentrationRule_create() returned a NULL pointer.");
  }
}


void
SpeciesConcentrationRuleTest_teardown (void)
{
  Rule_free(SCR);
}


START_TEST (test_SpeciesConcentrationRule_create)
{
  fail_unless( SBase_getTypeCode((SBase_t *) SCR) ==
               SBML_ASSIGNMENT_RULE );
  fail_unless( Rule_getL1TypeCode((Rule_t *) SCR) ==
               SBML_SPECIES_CONCENTRATION_RULE );

  fail_unless( SBase_getNotes     ((SBase_t *) SCR) == NULL );
  fail_unless( SBase_getAnnotation((SBase_t *) SCR) == NULL );

  fail_unless( Rule_getFormula((Rule_t *) SCR) == NULL );
  fail_unless( Rule_getType((Rule_t *) SCR) ==  RULE_TYPE_SCALAR );

  fail_unless( Rule_getVariable(SCR) == NULL );
  fail_unless( !Rule_isSetVariable(SCR) );
}
END_TEST


START_TEST (test_SpeciesConcentrationRule_createWith)
{
  Rule_t *scr;


  scr = Rule_createRateWithVariableAndFormula("c", "v + 1");
  Rule_setL1TypeCode(scr, SBML_SPECIES_CONCENTRATION_RULE);

  fail_unless( SBase_getTypeCode((SBase_t *) scr) ==
               SBML_RATE_RULE );
  fail_unless( Rule_getL1TypeCode((Rule_t *) scr) ==
               SBML_SPECIES_CONCENTRATION_RULE );

  fail_unless( SBase_getNotes     ((SBase_t *) scr) == NULL );
  fail_unless( SBase_getAnnotation((SBase_t *) scr) == NULL );

  fail_unless( !strcmp(Rule_getFormula( scr), "v + 1") );
  fail_unless( !strcmp(Rule_getVariable(scr), "c") );

  fail_unless( Rule_getType( scr) ==  RULE_TYPE_RATE );

  fail_unless( Rule_isSetVariable(scr) );

  Rule_free(scr);
}
END_TEST


START_TEST (test_SpeciesConcentrationRule_free_NULL)
{
  Rule_free(NULL);
}
END_TEST


START_TEST (test_SpeciesConcentrationRule_setSpecies)
{
  char       *species = "s2";
  const char *s;


  Rule_setVariable(SCR, species);

  fail_unless( !strcmp(Rule_getVariable(SCR), species),
               NULL );
  fail_unless( Rule_isSetVariable(SCR) );

  if (Rule_getVariable(SCR) == species)
  {
    fail( "SpeciesConcentrationRule_setSpecies(...)"
          " did not make a copy of string." );
  }

  /* Reflexive case (pathological) */
  s = Rule_getVariable(SCR);
  Rule_setVariable(SCR, s);
  fail_unless( !strcmp(Rule_getVariable(SCR), species),
               NULL );

  Rule_setVariable(SCR, NULL);
  fail_unless( !Rule_isSetVariable(SCR) );

  if (Rule_getVariable(SCR) != NULL)
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
