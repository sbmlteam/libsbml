/**
 * \file    TestCompartment.c
 * \brief   Compartment unit tests
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
#include "Compartment.h"

#include <check.h>


static Compartment_t *C;


void
CompartmentTest_setup (void)
{
  C = Compartment_create();

  if (C == NULL)
  {
    fail("Compartment_create() returned a NULL pointer.");
  }
}


void
CompartmentTest_teardown (void)
{
  Compartment_free(C);
}


START_TEST (test_Compartment_create)
{
  fail_unless( SBase_getTypeCode  ((SBase_t *) C) == SBML_COMPARTMENT );
  fail_unless( SBase_getMetaId    ((SBase_t *) C) == NULL );
/*  fail_unless( SBase_getNotes     ((SBase_t *) C) == NULL );
  fail_unless( SBase_getAnnotation((SBase_t *) C) == NULL );
*/
  fail_unless( Compartment_getId     (C) == NULL );
  fail_unless( Compartment_getName   (C) == NULL );
  fail_unless( Compartment_getUnits  (C) == NULL );
  fail_unless( Compartment_getOutside(C) == NULL );

  fail_unless( Compartment_getSpatialDimensions(C) == 3   );
  fail_unless( Compartment_getVolume           (C) == 1.0 );
  fail_unless( Compartment_getConstant         (C) == 1   );

  fail_unless( !Compartment_isSetId     (C) );
  fail_unless( !Compartment_isSetName   (C) );
  fail_unless( !Compartment_isSetSize   (C) );
  fail_unless( !Compartment_isSetVolume (C) );
  fail_unless( !Compartment_isSetUnits  (C) );
  fail_unless( !Compartment_isSetOutside(C) );
}
END_TEST


START_TEST (test_Compartment_createWith)
{
  Compartment_t *c = Compartment_createWith("A", "");


  fail_unless( SBase_getTypeCode  ((SBase_t *) c) == SBML_COMPARTMENT );
  fail_unless( SBase_getMetaId    ((SBase_t *) c) == NULL );
/*  fail_unless( SBase_getNotes     ((SBase_t *) c) == NULL );
  fail_unless( SBase_getAnnotation((SBase_t *) c) == NULL );
*/
  fail_unless( Compartment_getName(c)              == NULL );
  fail_unless( Compartment_getSpatialDimensions(c) == 3    );

  fail_unless( !strcmp( Compartment_getId     (c), "A"     ) );

  fail_unless( Compartment_getConstant(c) == 1   );

  fail_unless( Compartment_isSetId     (c) );
  fail_unless( !Compartment_isSetName  (c) );

  Compartment_free(c);
}
END_TEST


START_TEST (test_Compartment_free_NULL)
{
  Compartment_free(NULL);
}
END_TEST


START_TEST (test_Compartment_setId)
{
  char *id = "mitochondria";


  Compartment_setId(C, id);

  fail_unless( !strcmp(Compartment_getId(C), id) );
  fail_unless( Compartment_isSetId(C) );

  if (Compartment_getId(C) == id)
  {
    fail("Compartment_setId(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  Compartment_setId(C, Compartment_getId(C));
  fail_unless( !strcmp(Compartment_getId(C), id) );

  Compartment_setId(C, NULL);
  fail_unless( !Compartment_isSetId(C) );

  if (Compartment_getId(C) != NULL)
  {
    fail("Compartment_setId(C, NULL) did not clear string.");
  }
}
END_TEST


START_TEST (test_Compartment_setName)
{
  char *name = "My Favorite Factory";


  Compartment_setName(C, name);

  fail_unless( !strcmp(Compartment_getName(C), name) );
  fail_unless( Compartment_isSetName(C) );

  if (Compartment_getName(C) == name)
  {
    fail("Compartment_setName(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  Compartment_setName(C, Compartment_getName(C));
  fail_unless( !strcmp(Compartment_getName(C), name) );

  Compartment_setName(C, NULL);
  fail_unless( !Compartment_isSetName(C) );

  if (Compartment_getName(C) != NULL)
  {
    fail("Compartment_setName(C, NULL) did not clear string.");
  }
}
END_TEST


START_TEST (test_Compartment_setUnits)
{
  char *units = "volume";


  Compartment_setUnits(C, units);

  fail_unless( !strcmp(Compartment_getUnits(C), units) );
  fail_unless( Compartment_isSetUnits(C) );

  if (Compartment_getUnits(C) == units)
  {
    fail("Compartment_setUnits(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  Compartment_setUnits(C, Compartment_getUnits(C));
  fail_unless( !strcmp(Compartment_getUnits(C), units) );

  Compartment_setUnits(C, NULL);
  fail_unless( !Compartment_isSetUnits(C) );

  if (Compartment_getUnits(C) != NULL)
  {
    fail("Compartment_setUnits(C, NULL) did not clear string.");
  }
}
END_TEST


START_TEST (test_Compartment_setOutside)
{
  char *outside = "cell";


  Compartment_setOutside(C, outside);

  fail_unless( !strcmp(Compartment_getOutside(C), outside) );
  fail_unless( Compartment_isSetOutside(C)  );

  if (Compartment_getOutside(C) == outside)
  {
    fail("Compartment_setOutside(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  Compartment_setOutside(C, Compartment_getOutside(C));
  fail_unless( !strcmp(Compartment_getOutside(C), outside) );

  Compartment_setOutside(C, NULL);
  fail_unless( !Compartment_isSetOutside(C) );

  if (Compartment_getOutside(C) != NULL)
  {
    fail("Compartment_setOutside(C, NULL) did not clear string.");
  }
}
END_TEST


START_TEST (test_Compartment_unsetSize)
{
  Compartment_setSize(C, 0.2);

  fail_unless( Compartment_getSize(C) == 0.2 );
  fail_unless( Compartment_isSetSize(C) );

  Compartment_unsetSize(C);

  fail_unless( !Compartment_isSetSize(C) );
}
END_TEST


START_TEST (test_Compartment_unsetVolume)
{
  Compartment_setVolume(C, 1.0);

  fail_unless( Compartment_getVolume(C) == 1.0 );
  fail_unless( Compartment_isSetVolume(C) );

  Compartment_unsetVolume(C);

  fail_unless( !Compartment_isSetVolume(C) );
}
END_TEST


Suite *
create_suite_Compartment (void)
{
  Suite *suite = suite_create("Compartment");
  TCase *tcase = tcase_create("Compartment");


  tcase_add_checked_fixture( tcase,
                             CompartmentTest_setup,
                             CompartmentTest_teardown );

  tcase_add_test( tcase, test_Compartment_create      );
  tcase_add_test( tcase, test_Compartment_createWith  );
  tcase_add_test( tcase, test_Compartment_free_NULL   );
  tcase_add_test( tcase, test_Compartment_setId       );
  tcase_add_test( tcase, test_Compartment_setName     );
  tcase_add_test( tcase, test_Compartment_setUnits    );
  tcase_add_test( tcase, test_Compartment_setOutside  );
  tcase_add_test( tcase, test_Compartment_unsetSize   );
  tcase_add_test( tcase, test_Compartment_unsetVolume );

  suite_add_tcase(suite, tcase);

  return suite;
}
