/**
 * Filename    : TestSpeciesReference.c
 * Description : SpeciesReference unit tests
 * Author(s)   : SBW Development Group <sysbio-team@caltech.edu>
 * Organization: Caltech ERATO Kitano Systems Biology Project
 * Created     : 2002-11-25
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
#include "sbml/SpeciesReference.h"


SpeciesReference_t *SR;


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
  fail_unless( SR->typecode   == SBML_SPECIES_REFERENCE, NULL );
  fail_unless( SR->notes      == NULL, NULL );
  fail_unless( SR->annotation == NULL, NULL );

  fail_unless( SR->species       == NULL, NULL );
  fail_unless( SR->stoichiometry == 1   , NULL );
  fail_unless( SR->denominator   == 1   , NULL );
}
END_TEST


START_TEST (test_SpeciesReference_createWith)
{
  SpeciesReference_t *sr = SpeciesReference_createWith("s3", 4, 2);


  fail_unless( sr->typecode   == SBML_SPECIES_REFERENCE, NULL );
  fail_unless( sr->notes      == NULL, NULL );
  fail_unless( sr->annotation == NULL, NULL );

  fail_unless( !strcmp(sr->species, "s3"), NULL );

  fail_unless( sr->stoichiometry == 4   , NULL );
  fail_unless( sr->denominator   == 2   , NULL );

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

  fail_unless( !strcmp(SR->species, species), NULL );

  if (SR->species == species)
  {
    fail("SpeciesReference_setSpecies(...) did not make a copy of string.");
  }

  SpeciesReference_setSpecies(SR, NULL);

  if (SR->species != NULL)
  {
    fail("SpeciesReference_setSpecies(SR, NULL) did not clear string.");
  }
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

  tcase_add_test( tcase, test_SpeciesReference_create     );
  tcase_add_test( tcase, test_SpeciesReference_createWith );
  tcase_add_test( tcase, test_SpeciesReference_free_NULL  );
  tcase_add_test( tcase, test_SpeciesReference_setSpecies );

  suite_add_tcase(suite, tcase);

  return suite;
}
