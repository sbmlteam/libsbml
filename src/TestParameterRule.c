/**
 * Filename    : TestParameterRule.c
 * Description : ParameterRule unit tests
 * Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
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
#include "sbml/ParameterRule.h"


static ParameterRule_t *PR;


void
ParameterRuleTest_setup (void)
{
  PR = ParameterRule_create();

  if (PR == NULL)
  {
    fail("ParameterRule_create() returned a NULL pointer.");
  }
}


void
ParameterRuleTest_teardown (void)
{
  ParameterRule_free(PR);
}


START_TEST (test_ParameterRule_create)
{
  fail_unless( SBase_getTypeCode  (PR) == SBML_PARAMETER_RULE, NULL );
  fail_unless( SBase_getNotes     (PR) == NULL, NULL );
  fail_unless( SBase_getAnnotation(PR) == NULL, NULL );

  fail_unless( Rule_getFormula(PR) == NULL, NULL );

  fail_unless( ParameterRule_getUnits(PR) == NULL, NULL );
  fail_unless( ParameterRule_getName (PR) == NULL, NULL );

  fail_unless( AssignmentRule_getType(PR) == RULE_TYPE_SCALAR, NULL );

  fail_unless( !ParameterRule_isSetName (PR), NULL );
  fail_unless( !ParameterRule_isSetUnits(PR), NULL );
}
END_TEST


START_TEST (test_ParameterRule_createWith)
{
  ParameterRule_t *pr;


  pr = ParameterRule_createWith("x + 1", RULE_TYPE_SCALAR, "y");

  fail_unless( SBase_getTypeCode  (pr) == SBML_PARAMETER_RULE, NULL );
  fail_unless( SBase_getNotes     (pr) == NULL, NULL );
  fail_unless( SBase_getAnnotation(pr) == NULL, NULL );

  fail_unless( ParameterRule_getUnits(pr) == NULL, NULL );

  fail_unless( !strcmp(Rule_getFormula(pr)      , "x + 1"), NULL );
  fail_unless( !strcmp(ParameterRule_getName(pr), "y")    , NULL );

  fail_unless( AssignmentRule_getType(pr) == RULE_TYPE_SCALAR, NULL );

  fail_unless( ParameterRule_isSetName  (pr), NULL );
  fail_unless( !ParameterRule_isSetUnits(pr), NULL );

  ParameterRule_free(pr);
}
END_TEST


START_TEST (test_ParameterRule_free_NULL)
{
  ParameterRule_free(NULL);
}
END_TEST


START_TEST (test_ParameterRule_setName)
{
  char *name = "cell";


  ParameterRule_setName(PR, name);

  fail_unless( !strcmp(ParameterRule_getName(PR), name), NULL );
  fail_unless( ParameterRule_isSetName(PR), NULL );

  if (ParameterRule_getName(PR) == name)
  {
    fail( "ParameterRule_setName(...) did not make a copy of string." );
          
  }

  /* Reflexive case (pathological) */
  ParameterRule_setName(PR, ParameterRule_getName(PR));
  fail_unless( !strcmp(ParameterRule_getName(PR), name), NULL );

  ParameterRule_setName(PR, NULL);
  fail_unless( !ParameterRule_isSetName(PR), NULL );

  if (ParameterRule_getName(PR) != NULL)
  {
    fail( "ParameterRule_setName(PR, NULL) did not clear string." );          
  }
}
END_TEST


START_TEST (test_ParameterRule_setUnits)
{
  char *units = "cell";


  ParameterRule_setUnits(PR, units);

  fail_unless( !strcmp(ParameterRule_getUnits(PR), units)   , NULL );
  fail_unless( ParameterRule_isSetUnits(PR), NULL );

  if (ParameterRule_getUnits(PR) == units)
  {
    fail( "ParameterRule_setUnits(...) did not make a copy of string." );
  }

  /* Reflexive case (pathological) */
  ParameterRule_setUnits(PR, ParameterRule_getUnits(PR));
  fail_unless( !strcmp(ParameterRule_getUnits(PR), units), NULL );

  ParameterRule_setUnits(PR, NULL);
  fail_unless( !ParameterRule_isSetUnits(PR), NULL );

  if (ParameterRule_getUnits(PR) != NULL)
  {
    fail( "ParameterRule_setUnits(PR, NULL) did not clear string." );
  }
}
END_TEST


Suite *
create_suite_ParameterRule (void)
{
  Suite *suite = suite_create("ParameterRule");
  TCase *tcase = tcase_create("ParameterRule");


  tcase_add_checked_fixture( tcase,
                             ParameterRuleTest_setup,
                             ParameterRuleTest_teardown );

  tcase_add_test( tcase, test_ParameterRule_create     );
  tcase_add_test( tcase, test_ParameterRule_createWith );
  tcase_add_test( tcase, test_ParameterRule_free_NULL  );
  tcase_add_test( tcase, test_ParameterRule_setName    );
  tcase_add_test( tcase, test_ParameterRule_setUnits   );

  suite_add_tcase(suite, tcase);

  return suite;
}
