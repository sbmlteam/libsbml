/**
 * \file    TestParameterRule.c
 * \brief   ParameterRule unit tests
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
#include "Rule.h"

#include <check.h>


static Rule_t *PR;


void
ParameterRuleTest_setup (void)
{
  PR = Rule_createAssignment();
  Rule_setL1TypeCode(PR, SBML_PARAMETER_RULE);

  if (PR == NULL)
  {
    fail("ParameterRule_create() returned a NULL pointer.");
  }
}


void
ParameterRuleTest_teardown (void)
{
  Rule_free(PR);
}


START_TEST (test_ParameterRule_create)
{
  fail_unless( SBase_getTypeCode((SBase_t *) PR) ==
               SBML_ASSIGNMENT_RULE );
  fail_unless( Rule_getL1TypeCode((Rule_t *) PR) ==
               SBML_PARAMETER_RULE );
  fail_unless( SBase_getNotes     ((SBase_t *) PR) == NULL );
  fail_unless( SBase_getAnnotation((SBase_t *) PR) == NULL );

  fail_unless( Rule_getFormula((Rule_t *) PR) == NULL );

  fail_unless( Rule_getUnits(PR) == NULL );
  fail_unless( Rule_getVariable (PR) == NULL );

  fail_unless( Rule_getType( PR) ==  RULE_TYPE_SCALAR );

  fail_unless( !Rule_isSetVariable (PR) );
  fail_unless( !Rule_isSetUnits(PR) );
}
END_TEST


START_TEST (test_ParameterRule_createWith)
{
  Rule_t *pr;


  pr = Rule_createRateWithVariableAndFormula("c", "v + 1");
  Rule_setL1TypeCode(pr, SBML_PARAMETER_RULE);

  fail_unless( SBase_getTypeCode((SBase_t *) pr) ==
               SBML_RATE_RULE );
  fail_unless( Rule_getL1TypeCode((Rule_t *) pr) ==
               SBML_PARAMETER_RULE );
  fail_unless( SBase_getNotes     ((SBase_t *) pr) == NULL );
  fail_unless( SBase_getAnnotation((SBase_t *) pr) == NULL );

  fail_unless( Rule_getUnits(pr) == NULL );

  fail_unless( !strcmp(Rule_getFormula(pr), "v + 1") );
  fail_unless( !strcmp(Rule_getVariable(pr), "c") );

  fail_unless( Rule_getType( pr) ==  RULE_TYPE_RATE );

  fail_unless( Rule_isSetVariable(pr) );
  fail_unless( !Rule_isSetUnits(pr) );

  Rule_free(pr);
}
END_TEST


START_TEST (test_ParameterRule_free_NULL)
{
  Rule_free(NULL);
}
END_TEST


START_TEST (test_ParameterRule_setName)
{
  char *name = "cell";
  const char *c;


  Rule_setVariable(PR, name);

  fail_unless( !strcmp(Rule_getVariable(PR), name));
  fail_unless( Rule_isSetVariable(PR) );

  if (Rule_getVariable(PR) == name)
  {
    fail( "ParameterRule_setName(...) did not make a copy of string." );
          
  }

  /* Reflexive case (pathological) */
  c = Rule_getVariable(PR);
  Rule_setVariable(PR, c);
  fail_unless( !strcmp(Rule_getVariable(PR), name),
               NULL );

  Rule_setVariable(PR, NULL);
  fail_unless( !Rule_isSetVariable(PR) );

  if (Rule_getVariable(PR) != NULL)
  {
    fail( "Rule_setVariable(PR, NULL)"
          " did not clear string." );
  }
}
END_TEST


START_TEST (test_ParameterRule_setUnits)
{
  char *units = "cell";


  Rule_setUnits(PR, units);

  fail_unless( !strcmp(Rule_getUnits(PR), units)    );
  fail_unless( Rule_isSetUnits(PR) );

  if (Rule_getUnits(PR) == units)
  {
    fail( "Rule_setUnits(...) did not make a copy of string." );
  }

  /* Reflexive case (pathological) */
  Rule_setUnits(PR, Rule_getUnits(PR));
  fail_unless( !strcmp(Rule_getUnits(PR), units) );

  Rule_setUnits(PR, NULL);
  fail_unless( !Rule_isSetUnits(PR) );

  if (Rule_getUnits(PR) != NULL)
  {
    fail( "Rule_setUnits(PR, NULL) did not clear string." );
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
