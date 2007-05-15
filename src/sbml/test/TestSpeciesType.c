/**
 * \file    TestSpeciesType.c
 * \brief   SpeciesType unit tests
 * \author  Sarah Keating
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
#include "SpeciesType.h"

#include <check.h>


static SpeciesType_t *CT;


void
SpeciesTypeTest_setup (void)
{
  CT = SpeciesType_create();

  if (CT == NULL)
  {
    fail("SpeciesType_create() returned a NULL pointer.");
  }
}


void
SpeciesTypeTest_teardown (void)
{
  SpeciesType_free(CT);
}


START_TEST (test_SpeciesType_create)
{
  fail_unless( SBase_getTypeCode  ((SBase_t *) CT) == SBML_SPECIES_TYPE );
  fail_unless( SBase_getMetaId    ((SBase_t *) CT) == NULL );
  fail_unless( SBase_getNotes     ((SBase_t *) CT) == NULL );
  fail_unless( SBase_getAnnotation((SBase_t *) CT) == NULL );

  fail_unless( SpeciesType_getId     (CT) == NULL );
  fail_unless( SpeciesType_getName   (CT) == NULL );

  fail_unless( !SpeciesType_isSetId     (CT) );
  fail_unless( !SpeciesType_isSetName   (CT) );
}
END_TEST


START_TEST (test_SpeciesType_createWith)
{
  SpeciesType_t *c = SpeciesType_createWith("A", "");


  fail_unless( SBase_getTypeCode  ((SBase_t *) c) == SBML_SPECIES_TYPE );
  fail_unless( SBase_getMetaId    ((SBase_t *) c) == NULL );
  fail_unless( SBase_getNotes     ((SBase_t *) c) == NULL );
  fail_unless( SBase_getAnnotation((SBase_t *) c) == NULL );

  fail_unless( SpeciesType_getName(c)              == NULL );

  fail_unless( !strcmp( SpeciesType_getId     (c), "A"     ) );

  fail_unless( SpeciesType_isSetId     (c) );
  fail_unless( !SpeciesType_isSetName  (c) );

  SpeciesType_free(c);
}
END_TEST


START_TEST (test_SpeciesType_free_NULL)
{
  SpeciesType_free(NULL);
}
END_TEST


START_TEST (test_SpeciesType_setId)
{
  char *id = "mitochondria";


  SpeciesType_setId(CT, id);

  fail_unless( !strcmp(SpeciesType_getId(CT), id) );
  fail_unless( SpeciesType_isSetId(CT) );

  if (SpeciesType_getId(CT) == id)
  {
    fail("SpeciesType_setId(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  SpeciesType_setId(CT, SpeciesType_getId(CT));
  fail_unless( !strcmp(SpeciesType_getId(CT), id) );

  SpeciesType_setId(CT, NULL);
  fail_unless( !SpeciesType_isSetId(CT) );

  if (SpeciesType_getId(CT) != NULL)
  {
    fail("SpeciesType_setId(CT, NULL) did not clear string.");
  }
}
END_TEST


START_TEST (test_SpeciesType_setName)
{
  char *name = "My Favorite Factory";


  SpeciesType_setName(CT, name);

  fail_unless( !strcmp(SpeciesType_getName(CT), name) );
  fail_unless( SpeciesType_isSetName(CT) );

  if (SpeciesType_getName(CT) == name)
  {
    fail("SpeciesType_setName(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  SpeciesType_setName(CT, SpeciesType_getName(CT));
  fail_unless( !strcmp(SpeciesType_getName(CT), name) );

  SpeciesType_setName(CT, NULL);
  fail_unless( !SpeciesType_isSetName(CT) );

  if (SpeciesType_getName(CT) != NULL)
  {
    fail("SpeciesType_setName(CT, NULL) did not clear string.");
  }
}
END_TEST


START_TEST (test_SpeciesType_unsetName)
{
  SpeciesType_setName(CT, "name");

  fail_unless( !strcmp( SpeciesType_getName(CT), "name"     ));
  fail_unless( SpeciesType_isSetName(CT) );

  SpeciesType_unsetName(CT);

  fail_unless( !SpeciesType_isSetName(CT) );
}
END_TEST


Suite *
create_suite_SpeciesType (void)
{
  Suite *suite = suite_create("SpeciesType");
  TCase *tcase = tcase_create("SpeciesType");


  tcase_add_checked_fixture( tcase,
                             SpeciesTypeTest_setup,
                             SpeciesTypeTest_teardown );

  tcase_add_test( tcase, test_SpeciesType_create      );
  tcase_add_test( tcase, test_SpeciesType_createWith  );
  tcase_add_test( tcase, test_SpeciesType_free_NULL   );
  tcase_add_test( tcase, test_SpeciesType_setId       );
  tcase_add_test( tcase, test_SpeciesType_setName     );
  tcase_add_test( tcase, test_SpeciesType_unsetName   );

  suite_add_tcase(suite, tcase);

  return suite;
}
