/**
 * Filename    : TestParameter.c
 * Description : Parameter unit tests
 * Author(s)   : SBW Development Group <sysbio-team@caltech.edu>
 * Organization: Caltech ERATO Kitano Systems Biology Project
 * Created     : 2002-10-18
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
#include "sbml/Parameter.h"


Parameter_t *P;


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
  fail_unless( P->typecode   == SBML_PARAMETER, NULL );
  fail_unless( P->notes      == NULL, NULL );
  fail_unless( P->annotation == NULL, NULL );

  fail_unless( P->name  == NULL, NULL );
  fail_unless( P->units == NULL, NULL );

  fail_unless( !Parameter_isSetName (P), NULL );
  fail_unless( !Parameter_isSetValue(P), NULL );
  fail_unless( !Parameter_isSetUnits(P), NULL );
}
END_TEST


START_TEST (test_Parameter_createWith)
{
  Parameter_t *p = Parameter_createWith("delay", 6.2, "second");


  fail_unless( p->typecode   == SBML_PARAMETER, NULL );
  fail_unless( p->notes      == NULL, NULL );
  fail_unless( p->annotation == NULL, NULL );

  fail_unless( !strcmp(p->name , "delay" ), NULL );
  fail_unless( !strcmp(p->units, "second"), NULL );

  fail_unless( p->value == 6.2, NULL );

  fail_unless( Parameter_isSetName (p), NULL );
  fail_unless( Parameter_isSetValue(p), NULL );
  fail_unless( Parameter_isSetUnits(p), NULL );

  Parameter_free(p);
}
END_TEST


START_TEST (test_Parameter_free_NULL)
{
  Parameter_free(NULL);
}
END_TEST


START_TEST (test_Parameter_setName)
{
  char *name = "Km1";


  Parameter_setName(P, name);

  fail_unless( !strcmp(P->name, name), NULL );
  fail_unless( Parameter_isSetName(P), NULL );

  if (P->name == name)
  {
    fail("Parameter_setName(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  Parameter_setName(P, P->name);
  fail_unless( !strcmp(P->name, name), NULL );

  Parameter_setName(P, NULL);
  fail_unless( !Parameter_isSetName(P), NULL );

  if (P->name != NULL)
  {
    fail("Parameter_setName(P, NULL) did not clear string.");
  }
}
END_TEST


START_TEST (test_Parameter_setUnits)
{
  char *units = "second";


  Parameter_setUnits(P, units);

  fail_unless( !strcmp(P->units, units), NULL );
  fail_unless( Parameter_isSetUnits(P) , NULL );

  if (P->units == units)
  {
    fail("Parameter_setUnits(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  Parameter_setUnits(P, P->units);
  fail_unless( !strcmp(P->units, units), NULL );

  Parameter_setUnits(P, NULL);
  fail_unless( !Parameter_isSetUnits(P), NULL );

  if (P->units != NULL)
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
  tcase_add_test( tcase, test_Parameter_setName    );
  tcase_add_test( tcase, test_Parameter_setUnits   );

  suite_add_tcase(suite, tcase);

  return suite;
}
