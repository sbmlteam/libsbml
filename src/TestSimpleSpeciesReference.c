/**
 * Filename    : TestSimpleSpeciesReference.c
 * Description : SimpleSpeciesReference unit tests
 * Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2003-04-29
 * Revision    : $Id$
 * Source      : $Source$
 *
 * Copyright 2003 California Institute of Technology and
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
 *     mailto:sysbio-team@caltech.edu
 *
 * Contributor(s):
 */


#include <check.h>

#include "sbml/common.h"
#include "sbml/FormulaParser.h"
#include "sbml/SimpleSpeciesReference.h"


static SimpleSpeciesReference_t *SSR;


void
SimpleSpeciesReferenceTest_setup (void)
{
  SSR = (SimpleSpeciesReference_t *)
        safe_calloc(1, sizeof(SimpleSpeciesReference_t));

  if (SSR == NULL)
  {
    fail( "safe_calloc(1, sizeof(SimpleSpeciesReference_t)) "
          "returned a NULL pointer." );
  }

  /**
   * SimpleSpeciesReference_init() requires an SBMLTypeCode_t as its second
   * argument.  Which one doesn't really matter for the purposes of these
   * tests, so one was picked at random.
   */
  SimpleSpeciesReference_init(SSR, SBML_SPECIES_REFERENCE);
}


void
SimpleSpeciesReferenceTest_teardown (void)
{
  SimpleSpeciesReference_clear(SSR);
  safe_free(SSR);
}


START_TEST (test_SimpleSpeciesReference_init)
{
  fail_unless( SSR->typecode   == SBML_SPECIES_REFERENCE, NULL );
  fail_unless( SSR->metaid     == NULL, NULL );
  fail_unless( SSR->notes      == NULL, NULL );
  fail_unless( SSR->annotation == NULL, NULL );
  fail_unless( SSR->species    == NULL, NULL );
}
END_TEST


START_TEST (test_SimpleSpeciesReference_clear_NULL)
{
  SimpleSpeciesReference_clear(NULL);
}
END_TEST


START_TEST (test_SimpleSpeciesReference_setSpecies)
{
  char *species = "s1";


  SimpleSpeciesReference_setSpecies(SSR, species);

  fail_unless( !strcmp(SSR->species, species), NULL );
  fail_unless( SimpleSpeciesReference_isSetSpecies(SSR), NULL );

  if (SSR->species == species)
  {
    fail( "SimpleSpeciesReference_setSpecies(...) "
          "did not make a copy of string." );
  }

  /* Reflexive case (pathological) */
  SimpleSpeciesReference_setSpecies(SSR, SSR->species);
  fail_unless( !strcmp(SSR->species, species), NULL );

  SimpleSpeciesReference_setSpecies(SSR, NULL);
  fail_unless( !SimpleSpeciesReference_isSetSpecies(SSR), NULL );

  if (SSR->species != NULL)
  {
    fail("SimpleSpeciesReference_setSpecies(SSR, NULL) did not clear string.");
  }
}
END_TEST


Suite *
create_suite_SimpleSpeciesReference (void)
{
  Suite *suite = suite_create("SimpleSpeciesReference");
  TCase *tcase = tcase_create("SimpleSpeciesReference");


  tcase_add_checked_fixture( tcase,
                             SimpleSpeciesReferenceTest_setup,
                             SimpleSpeciesReferenceTest_teardown );

  tcase_add_test( tcase, test_SimpleSpeciesReference_init       );
  tcase_add_test( tcase, test_SimpleSpeciesReference_clear_NULL );
  tcase_add_test( tcase, test_SimpleSpeciesReference_setSpecies );

  suite_add_tcase(suite, tcase);

  return suite;
}
