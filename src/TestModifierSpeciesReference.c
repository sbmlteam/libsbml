/**
 * Filename    : TestModifierSpeciesReference.c
 * Description : ModifierSpeciesReference unit tests
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
#include "sbml/ModifierSpeciesReference.h"


static ModifierSpeciesReference_t *MSR;


void
ModifierSpeciesReferenceTest_setup (void)
{
  MSR = ModifierSpeciesReference_create();

  if (MSR == NULL)
  {
    fail( "ModifierSpeciesReference_create() returned a NULL pointer." );
  }
}


void
ModifierSpeciesReferenceTest_teardown (void)
{
  ModifierSpeciesReference_free(MSR);
}


START_TEST (test_ModifierSpeciesReference_create)
{
  fail_unless(SBase_getTypeCode(MSR) == SBML_MODIFIER_SPECIES_REFERENCE, NULL);

  fail_unless( SBase_getMetaId    (MSR) == NULL, NULL );
  fail_unless( SBase_getNotes     (MSR) == NULL, NULL );
  fail_unless( SBase_getAnnotation(MSR) == NULL, NULL );

  fail_unless( ModifierSpeciesReference_getSpecies(MSR) == NULL, NULL );

  fail_unless( !ModifierSpeciesReference_isSetSpecies(MSR), NULL );
}
END_TEST


START_TEST (test_ModifierSpeciesReference_createWith)
{
  ModifierSpeciesReference_t *msr;


  msr = ModifierSpeciesReference_createWith("s5");

  fail_unless(SBase_getTypeCode(msr) == SBML_MODIFIER_SPECIES_REFERENCE, NULL);

  fail_unless( SBase_getMetaId    (msr) == NULL, NULL );
  fail_unless( SBase_getNotes     (msr) == NULL, NULL );
  fail_unless( SBase_getAnnotation(msr) == NULL, NULL );

  fail_unless( !strcmp(ModifierSpeciesReference_getSpecies(msr), "s5"), NULL );
  fail_unless( ModifierSpeciesReference_isSetSpecies(msr), NULL );

  ModifierSpeciesReference_free(msr);
}
END_TEST


START_TEST (test_ModifierSpeciesReference_free_NULL)
{
  ModifierSpeciesReference_free(NULL);
}
END_TEST


START_TEST (test_ModifierSpeciesReference_setSpecies)
{
  const char *s;
  char *species = "s1";



  ModifierSpeciesReference_setSpecies(MSR, species);

  s = ModifierSpeciesReference_getSpecies(MSR);
  fail_unless( !strcmp(s, species), NULL );
  fail_unless(ModifierSpeciesReference_isSetSpecies(MSR), NULL);

  if (ModifierSpeciesReference_getSpecies(MSR) == species)
  {
    fail( "ModifierSpeciesReference_setSpecies(...) "
          "did not make a copy of string." );
  }

  /* Reflexive case (pathological) */
  s = ModifierSpeciesReference_getSpecies(MSR);
  ModifierSpeciesReference_setSpecies(MSR, s);

  s = ModifierSpeciesReference_getSpecies(MSR);
  fail_unless( !strcmp(s, species), NULL );

  ModifierSpeciesReference_setSpecies(MSR, NULL);
  fail_unless(!ModifierSpeciesReference_isSetSpecies(MSR), NULL);

  if (ModifierSpeciesReference_getSpecies(MSR) != NULL)
  {
    fail( "ModifierSpeciesReference_setSpecies(MSR, NULL) "
          "did not clear string." );
  }
}
END_TEST


Suite *
create_suite_ModifierSpeciesReference (void)
{
  Suite *suite = suite_create("ModifierSpeciesReference");
  TCase *tcase = tcase_create("ModifierSpeciesReference");


  tcase_add_checked_fixture( tcase,
                             ModifierSpeciesReferenceTest_setup,
                             ModifierSpeciesReferenceTest_teardown );

  tcase_add_test( tcase, test_ModifierSpeciesReference_create     );
  tcase_add_test( tcase, test_ModifierSpeciesReference_createWith );
  tcase_add_test( tcase, test_ModifierSpeciesReference_free_NULL  );
  tcase_add_test( tcase, test_ModifierSpeciesReference_setSpecies );

  suite_add_tcase(suite, tcase);

  return suite;
}
