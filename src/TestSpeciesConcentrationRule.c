/**
 * Filename    : TestSpeciesConcentrationRule.c
 * Description : SpeciesConcentrationRule unit tests
 * Author(s)   : SBW Development Group <sysbio-team@caltech.edu>
 * Organization: Caltech ERATO Kitano Systems Biology Project
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
#include "sbml/SpeciesConcentrationRule.h"


SpeciesConcentrationRule_t *SCR;


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
  fail_unless( SCR->typecode   == SBML_SPECIES_CONCENTRATION_RULE, NULL );
  fail_unless( SCR->notes      == NULL, NULL );
  fail_unless( SCR->annotation == NULL, NULL );

  fail_unless( SCR->formula == NULL, NULL );
  fail_unless( SCR->type    == RULE_TYPE_SCALAR, NULL );
  fail_unless( SCR->species == NULL, NULL );

  fail_unless( !SpeciesConcentrationRule_isSetSpecies(SCR), NULL );
}
END_TEST


START_TEST (test_SpeciesConcentrationRule_createWith)
{
  SpeciesConcentrationRule_t *scr;

  scr = SpeciesConcentrationRule_createWith("t - s2", RULE_TYPE_RATE, "s1");


  fail_unless( scr->typecode   == SBML_SPECIES_CONCENTRATION_RULE, NULL );
  fail_unless( scr->notes      == NULL, NULL );
  fail_unless( scr->annotation == NULL, NULL );

  fail_unless( !strcmp(scr->formula, "t - s2"), NULL );
  fail_unless( !strcmp(scr->species, "s1")    , NULL );

  fail_unless( scr->type == RULE_TYPE_RATE, NULL );

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
  char *species = "s2";


  SpeciesConcentrationRule_setSpecies(SCR, species);

  fail_unless( !strcmp(SCR->species, species), NULL );
  fail_unless( SpeciesConcentrationRule_isSetSpecies(SCR), NULL );

  if (SCR->species == species)
  {
    fail( "SpeciesConcentrationRule_setSpecies(...)"
          " did not make a copy of string." );
  }

  SpeciesConcentrationRule_setSpecies(SCR, NULL);
  fail_unless( !SpeciesConcentrationRule_isSetSpecies(SCR), NULL );

  if (SCR->species != NULL)
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
