/**
 * \file    TestSimpleSpeciesReference.c
 * \brief   SimpleSpeciesReference unit tests
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


#include "common/common.h"

#include "SBase.h"
#include "SimpleSpeciesReference.h"
#include "SpeciesReference.h"

#include <check.h>


static SimpleSpeciesReference_t *SSR;


void
SimpleSpeciesReferenceTest_setup (void)
{
  SSR = (SimpleSpeciesReference_t *) SpeciesReference_create();


  if (SSR == NULL)
  {
    fail("SpeciesReference_create() returned a NULL pointer.");
  }
}


void
SimpleSpeciesReferenceTest_teardown (void)
{
  SpeciesReference_free((SpeciesReference_t *) SSR);
}


START_TEST (test_SimpleSpeciesReference_init)
{
  fail_unless( SBase_getTypeCode  ((SBase_t *) SSR) == SBML_SPECIES_REFERENCE );
  fail_unless( SBase_getMetaId    ((SBase_t *) SSR) == NULL );
  fail_unless( SBase_getNotes     ((SBase_t *) SSR) == NULL );
  fail_unless( SBase_getAnnotation((SBase_t *) SSR) == NULL );

  fail_unless( SimpleSpeciesReference_getSpecies(SSR) == NULL );
}
END_TEST


START_TEST (test_SimpleSpeciesReference_setSpecies)
{
  const char *s;
  char *species = "s1";


  SimpleSpeciesReference_setSpecies(SSR, species);

  fail_unless(!strcmp(SimpleSpeciesReference_getSpecies(SSR), species));
  fail_unless( SimpleSpeciesReference_isSetSpecies(SSR) );

  if (SimpleSpeciesReference_getSpecies(SSR) == species)
  {
    fail( "SimpleSpeciesReference_setSpecies(...) "
          "did not make a copy of string." );
  }

  /* Reflexive case (pathological) */
  s = SimpleSpeciesReference_getSpecies(SSR);
  SimpleSpeciesReference_setSpecies(SSR, s);

  s = SimpleSpeciesReference_getSpecies(SSR);
  fail_unless( !strcmp(s, species) );

  SimpleSpeciesReference_setSpecies(SSR, NULL);
  fail_unless( !SimpleSpeciesReference_isSetSpecies(SSR) );

  if (SimpleSpeciesReference_getSpecies(SSR) != NULL)
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
  tcase_add_test( tcase, test_SimpleSpeciesReference_setSpecies );

  suite_add_tcase(suite, tcase);

  return suite;
}
