/**
 * Filename    : TestRule.c
 * Description : Rule unit tests
 * Author(s)   : SBW Development Group <sysbio-team@caltech.edu>
 * Organization: Caltech ERATO Kitano Systems Biology Project
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
#include "sbml/FormulaParser.h"
#include "sbml/Rule.h"


static Rule_t *R;


void
RuleTest_setup (void)
{
  R = (Rule_t *) safe_calloc(1, sizeof(Rule_t));

  if (R == NULL)
  {
    fail("safe_calloc(1, sizeof(Rule_t)) returned a NULL pointer.");
  }

  /**
   * Rule_init() requires an SBMLTypeCode_t as its second argument.  Which
   * one doesn't really matter for the purposes of these tests, so one was
   * picked at random.
   */
  Rule_init(R, SBML_ALGEBRAIC_RULE);
}


void
RuleTest_teardown (void)
{
  Rule_clear(R);
  safe_free(R);
}


START_TEST (test_Rule_init)
{
  fail_unless( R->typecode   == SBML_ALGEBRAIC_RULE, NULL );
  fail_unless( R->metaid     == NULL, NULL );
  fail_unless( R->notes      == NULL, NULL );
  fail_unless( R->annotation == NULL, NULL );
  fail_unless( R->formula    == NULL, NULL );
  fail_unless( R->math       == NULL, NULL );
}
END_TEST


START_TEST (test_Rule_clear_NULL)
{
  Rule_clear(NULL);
}
END_TEST


START_TEST (test_Rule_setFormula)
{
  char *formula = "k1*X0";


  Rule_setFormula(R, formula);

  fail_unless( !strcmp(R->formula, formula), NULL );
  fail_unless( Rule_isSetFormula(R), NULL );

  if (R->formula == formula)
  {
    fail("Rule_setFormula(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  Rule_setFormula(R, R->formula);
  fail_unless( !strcmp(R->formula, formula), NULL );

  Rule_setFormula(R, NULL);
  fail_unless( !Rule_isSetFormula(R), NULL );

  if (R->formula != NULL)
  {
    fail("Rule_setFormula(R, NULL) did not clear string.");
  }
}
END_TEST


START_TEST (test_Rule_setMath)
{
  ASTNode_t *math = SBML_parseFormula("1 + 1");


  Rule_setMath(R, math);

  fail_unless( R->math == math, NULL );
  fail_unless( Rule_isSetMath(R), NULL );

  /* Reflexive case (pathological) */
  Rule_setMath(R, R->math);
  fail_unless( R->math == math, NULL );

  Rule_setMath(R, NULL);
  fail_unless( !Rule_isSetMath(R), NULL );

  if (R->math != NULL)
  {
    fail("Rule_setMath(R, NULL) did not clear ASTNode.");
  }
}
END_TEST


Suite *
create_suite_Rule (void)
{
  Suite *suite = suite_create("Rule");
  TCase *tcase = tcase_create("Rule");


  tcase_add_checked_fixture( tcase, RuleTest_setup, RuleTest_teardown );

  tcase_add_test( tcase, test_Rule_init       );
  tcase_add_test( tcase, test_Rule_clear_NULL );
  tcase_add_test( tcase, test_Rule_setFormula );
  tcase_add_test( tcase, test_Rule_setMath    );

  suite_add_tcase(suite, tcase);

  return suite;
}
