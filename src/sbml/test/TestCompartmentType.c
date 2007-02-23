/**
 * \file    TestCompartmentTypeType.c
 * \brief   CompartmentTypeType unit tests
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
#include "CompartmentType.h"

#include <check.h>


static CompartmentType_t *CT;


void
CompartmentTypeTest_setup (void)
{
  CT = CompartmentType_create();

  if (CT == NULL)
  {
    fail("CompartmentType_create() returned a NULL pointer.");
  }
}


void
CompartmentTypeTest_teardown (void)
{
  CompartmentType_free(CT);
}


START_TEST (test_CompartmentType_create)
{
  fail_unless( SBase_getTypeCode  ((SBase_t *) CT) == SBML_COMPARTMENT_TYPE );
  fail_unless( SBase_getMetaId    ((SBase_t *) CT) == NULL );
/*  fail_unless( SBase_getNotes     ((SBase_t *) CT) == NULL );
  fail_unless( SBase_getAnnotation((SBase_t *) CT) == NULL );
*/
  fail_unless( CompartmentType_getId     (CT) == NULL );
  fail_unless( CompartmentType_getName   (CT) == NULL );

  fail_unless( !CompartmentType_isSetId     (CT) );
  fail_unless( !CompartmentType_isSetName   (CT) );
}
END_TEST


START_TEST (test_CompartmentType_createWith)
{
  CompartmentType_t *c = CompartmentType_createWith("A", "");


  fail_unless( SBase_getTypeCode  ((SBase_t *) c) == SBML_COMPARTMENT_TYPE );
  fail_unless( SBase_getMetaId    ((SBase_t *) c) == NULL );
/*  fail_unless( SBase_getNotes     ((SBase_t *) c) == NULL );
  fail_unless( SBase_getAnnotation((SBase_t *) c) == NULL );
*/
  fail_unless( CompartmentType_getName(c)              == NULL );

  fail_unless( !strcmp( CompartmentType_getId     (c), "A"     ) );

  fail_unless( CompartmentType_isSetId     (c) );
  fail_unless( !CompartmentType_isSetName  (c) );

  CompartmentType_free(c);
}
END_TEST


START_TEST (test_CompartmentType_free_NULL)
{
  CompartmentType_free(NULL);
}
END_TEST


START_TEST (test_CompartmentType_setId)
{
  char *id = "mitochondria";


  CompartmentType_setId(CT, id);

  fail_unless( !strcmp(CompartmentType_getId(CT), id) );
  fail_unless( CompartmentType_isSetId(CT) );

  if (CompartmentType_getId(CT) == id)
  {
    fail("CompartmentType_setId(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  CompartmentType_setId(CT, CompartmentType_getId(CT));
  fail_unless( !strcmp(CompartmentType_getId(CT), id) );

  CompartmentType_setId(CT, NULL);
  fail_unless( !CompartmentType_isSetId(CT) );

  if (CompartmentType_getId(CT) != NULL)
  {
    fail("CompartmentType_setId(CT, NULL) did not clear string.");
  }
}
END_TEST


START_TEST (test_CompartmentType_setName)
{
  char *name = "My Favorite Factory";


  CompartmentType_setName(CT, name);

  fail_unless( !strcmp(CompartmentType_getName(CT), name) );
  fail_unless( CompartmentType_isSetName(CT) );

  if (CompartmentType_getName(CT) == name)
  {
    fail("CompartmentType_setName(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  CompartmentType_setName(CT, CompartmentType_getName(CT));
  fail_unless( !strcmp(CompartmentType_getName(CT), name) );

  CompartmentType_setName(CT, NULL);
  fail_unless( !CompartmentType_isSetName(CT) );

  if (CompartmentType_getName(CT) != NULL)
  {
    fail("CompartmentType_setName(CT, NULL) did not clear string.");
  }
}
END_TEST


START_TEST (test_CompartmentType_unsetName)
{
  CompartmentType_setName(CT, "name");

  fail_unless( !strcmp( CompartmentType_getName(CT), "name"     ));
  fail_unless( CompartmentType_isSetName(CT) );

  CompartmentType_unsetName(CT);

  fail_unless( !CompartmentType_isSetName(CT) );
}
END_TEST


Suite *
create_suite_CompartmentType (void)
{
  Suite *suite = suite_create("CompartmentType");
  TCase *tcase = tcase_create("CompartmentType");


  tcase_add_checked_fixture( tcase,
                             CompartmentTypeTest_setup,
                             CompartmentTypeTest_teardown );

  tcase_add_test( tcase, test_CompartmentType_create      );
  tcase_add_test( tcase, test_CompartmentType_createWith  );
  tcase_add_test( tcase, test_CompartmentType_free_NULL   );
  tcase_add_test( tcase, test_CompartmentType_setId       );
  tcase_add_test( tcase, test_CompartmentType_setName     );
  tcase_add_test( tcase, test_CompartmentType_unsetName   );

  suite_add_tcase(suite, tcase);

  return suite;
}
