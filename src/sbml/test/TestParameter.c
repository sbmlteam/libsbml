/**
 * \file    TestParameter.c
 * \brief   Parameter unit tests
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


#include <check.h>

#include "common.h"

#include "SBase.h"
#include "Parameter.h"


static Parameter_t *P;


void
ParameterTest_setup (void)
{
  P = Parameter_create();

  if (P == NULL)
  {
    fail("Parameter_create() returned a NULL pointer.");
  }
}


void
ParameterTest_teardown (void)
{
  Parameter_free(P);
}

START_TEST (test_Parameter_create)
{
  fail_unless( SBase_getTypeCode  ((SBase_t *) P) == SBML_PARAMETER );
  fail_unless( SBase_getMetaId    ((SBase_t *) P) == NULL );
  fail_unless( SBase_getNotes     ((SBase_t *) P) == NULL );
  fail_unless( SBase_getAnnotation((SBase_t *) P) == NULL );

  fail_unless( Parameter_getId      (P) == NULL );
  fail_unless( Parameter_getName    (P) == NULL );
  fail_unless( Parameter_getUnits   (P) == NULL );
  fail_unless( Parameter_getConstant(P) == 1    );

  fail_unless( !Parameter_isSetId   (P) );
  fail_unless( !Parameter_isSetName (P) );
  fail_unless( !Parameter_isSetValue(P) );
  fail_unless( !Parameter_isSetUnits(P) );
}
END_TEST


START_TEST (test_Parameter_createWith)
{
  Parameter_t *p = Parameter_createWith("delay", 6.2, "second");


  fail_unless( SBase_getTypeCode  ((SBase_t *) p) == SBML_PARAMETER );
  fail_unless( SBase_getMetaId    ((SBase_t *) p) == NULL );
  fail_unless( SBase_getNotes     ((SBase_t *) p) == NULL );
  fail_unless( SBase_getAnnotation((SBase_t *) p) == NULL );

  fail_unless( !strcmp(Parameter_getId   (p), "delay" ) );
  fail_unless( !strcmp(Parameter_getUnits(p), "second") );

  fail_unless( Parameter_getName    (p) == NULL );
  fail_unless( Parameter_getValue   (p) == 6.2 );
  fail_unless( Parameter_getConstant(p) == 1   );

  fail_unless(   Parameter_isSetId   (p) );
  fail_unless( ! Parameter_isSetName (p) );
  fail_unless(   Parameter_isSetValue(p) );
  fail_unless(   Parameter_isSetUnits(p) );

  Parameter_free(p);
}
END_TEST


START_TEST (test_Parameter_free_NULL)
{
  Parameter_free(NULL);
}
END_TEST


START_TEST (test_Parameter_setId)
{
  char *id = "Km1";


  Parameter_setId(P, id);

  fail_unless( !strcmp(Parameter_getId(P), id) );
  fail_unless( Parameter_isSetId(P) );

  if (Parameter_getId(P) == id)
  {
    fail("Parameter_setId(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  Parameter_setId(P, Parameter_getId(P));
  fail_unless( !strcmp(Parameter_getId(P), id) );

  Parameter_setId(P, NULL);
  fail_unless( !Parameter_isSetId(P) );

  if (Parameter_getId(P) != NULL)
  {
    fail("Parameter_setId(P, NULL) did not clear string.");
  }
}
END_TEST


START_TEST (test_Parameter_setName)
{
  char *name = "Forward Michaelis-Menten Constant";


  Parameter_setName(P, name);

  fail_unless( !strcmp(Parameter_getName(P), name) );
  fail_unless( Parameter_isSetName(P) );

  if (Parameter_getName(P) == name)
  {
    fail("Parameter_setName(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  Parameter_setName(P, Parameter_getName(P));
  fail_unless( !strcmp(Parameter_getName(P), name) );

  Parameter_setName(P, NULL);
  fail_unless( !Parameter_isSetName(P) );

  if (Parameter_getName(P) != NULL)
  {
    fail("Parameter_setName(P, NULL) did not clear string.");
  }
}
END_TEST


START_TEST (test_Parameter_setUnits)
{
  char *units = "second";


  Parameter_setUnits(P, units);

  fail_unless( !strcmp(Parameter_getUnits(P), units) );
  fail_unless( Parameter_isSetUnits(P)  );

  if (Parameter_getUnits(P) == units)
  {
    fail("Parameter_setUnits(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  Parameter_setUnits(P, Parameter_getUnits(P));
  fail_unless( !strcmp(Parameter_getUnits(P), units) );

  Parameter_setUnits(P, NULL);
  fail_unless( !Parameter_isSetUnits(P) );

  if (Parameter_getUnits(P) != NULL)
  {
    fail("Parameter_setUnits(P, NULL) did not clear string.");
  }
}
END_TEST


Suite *
create_suite_Parameter (void)
{
  Suite *suite = suite_create("Parameter");
  TCase *tcase = tcase_create("Parameter");


  tcase_add_checked_fixture( tcase,
                             ParameterTest_setup,
                             ParameterTest_teardown );

  tcase_add_test( tcase, test_Parameter_create     );
  tcase_add_test( tcase, test_Parameter_createWith );
  tcase_add_test( tcase, test_Parameter_free_NULL  );
  tcase_add_test( tcase, test_Parameter_setId      );
  tcase_add_test( tcase, test_Parameter_setName    );
  tcase_add_test( tcase, test_Parameter_setUnits   );

  suite_add_tcase(suite, tcase);

  return suite;
}
