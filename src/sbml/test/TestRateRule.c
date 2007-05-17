/**
 * \file    TestRateRule.c
 * \brief   RateRule unit tests
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
#include "math/FormulaParser.h"
#include "math/FormulaFormatter.h"

#include "SBase.h"
#include "Rule.h"

#include <check.h>


static Rule_t *RR;


void
RateRuleTest_setup (void)
{
  RR = Rule_createRate();

  if (RR == NULL)
  {
    fail("RateRule_create() returned a NULL pointer.");
  }
}


void
RateRuleTest_teardown (void)
{
  Rule_free(RR);
}

START_TEST (test_RateRule_create)
{
  fail_unless( SBase_getTypeCode  ((SBase_t *) RR) == SBML_RATE_RULE );
  fail_unless( SBase_getMetaId    ((SBase_t *) RR) == NULL );
  fail_unless( SBase_getNotes     ((SBase_t *) RR) == NULL );
  fail_unless( SBase_getAnnotation((SBase_t *) RR) == NULL );

  fail_unless( Rule_getFormula     ((Rule_t *) RR) == NULL );
  fail_unless( Rule_getMath        ((Rule_t *) RR) == NULL );
  fail_unless( Rule_getVariable(RR) == NULL );
  fail_unless( Rule_getType    (RR) == RULE_TYPE_RATE );
}
END_TEST


START_TEST (test_RateRule_free_NULL)
{
  Rule_free(NULL);
}
END_TEST


START_TEST (test_RateRule_setVariable)
{
  char *variable = "x";


  Rule_setVariable(RR, variable);

  fail_unless( !strcmp(Rule_getVariable(RR), variable) );
  fail_unless( Rule_isSetVariable(RR) );

  if (Rule_getVariable(RR) == variable)
  {
    fail("Rule_setVariable(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  Rule_setVariable(RR, Rule_getVariable(RR));
  fail_unless( !strcmp(Rule_getVariable(RR), variable) );

  Rule_setVariable(RR, NULL);
  fail_unless( !Rule_isSetVariable(RR) );

  if (Rule_getVariable(RR) != NULL)
  {
    fail("Rule_setVariable(RR, NULL) did not clear string.");
  }
}
END_TEST


START_TEST (test_RateRule_createWithFormula)
{
  const ASTNode_t *math;
  char *formula;

  Rule_t *ar = Rule_createRateWithVariableAndFormula("s", "1 + 1");


  fail_unless( SBase_getTypeCode  ((SBase_t *) ar) == SBML_RATE_RULE );
  fail_unless( SBase_getMetaId    ((SBase_t *) ar) == NULL );
  fail_unless( !strcmp(Rule_getVariable(ar), "s") );

  math = Rule_getMath((Rule_t *) ar);
  fail_unless(math != NULL);

  formula = SBML_formulaToString(math);
  fail_unless( formula != NULL );
  fail_unless( !strcmp(formula, "1 + 1") );

  fail_unless( !strcmp(Rule_getFormula((Rule_t *) ar), formula) );

  Rule_free(ar);
  safe_free(formula);
}
END_TEST


START_TEST (test_RateRule_createWithMath)
{
  ASTNode_t       *math = SBML_parseFormula("1 + 1");

  Rule_t *ar = Rule_createRateWithVariableAndMath("s", math);


  fail_unless( SBase_getTypeCode  ((SBase_t *) ar) == SBML_RATE_RULE );
  fail_unless( SBase_getMetaId    ((SBase_t *) ar) == NULL );
  fail_unless( !strcmp(Rule_getVariable(ar), "s") );
  fail_unless( !strcmp(Rule_getFormula((Rule_t *) ar), "1 + 1") );
  fail_unless( Rule_getMath((Rule_t *) ar) != math );

  Rule_free(ar);
}
END_TEST


Suite *
create_suite_RateRule (void)
{
  Suite *suite = suite_create("RateRule");
  TCase *tcase = tcase_create("RateRule");


  tcase_add_checked_fixture( tcase,
                             RateRuleTest_setup,
                             RateRuleTest_teardown );

  tcase_add_test( tcase, test_RateRule_create      );
  tcase_add_test( tcase, test_RateRule_free_NULL   );
  tcase_add_test( tcase, test_RateRule_setVariable );
  tcase_add_test( tcase, test_RateRule_createWithFormula   );
  tcase_add_test( tcase, test_RateRule_createWithMath   );

  suite_add_tcase(suite, tcase);

  return suite;
}
