/**
 * \file    TestRule.c
 * \brief   Rule unit tests
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

#include "math/FormulaFormatter.h"
#include "math/FormulaParser.h"

#include "SBase.h"
#include "Rule.h"

#include <check.h>


static Rule_t *R;


void
RuleTest_setup (void)
{
  R = Rule_createAlgebraic();

  if (R == NULL)
  {
    fail("Rule_createAlgebraic() returned a NULL pointer.");
  }
}


void
RuleTest_teardown (void)
{
  Rule_free(R);
}


START_TEST (test_Rule_init)
{
  fail_unless( SBase_getTypeCode  ((SBase_t *) R) == SBML_ALGEBRAIC_RULE );
  fail_unless( SBase_getMetaId    ((SBase_t *) R) == NULL );
  //fail_unless( SBase_getNotes     ((SBase_t *) R) == NULL );
  //fail_unless( SBase_getAnnotation((SBase_t *) R) == NULL );

  fail_unless( Rule_getFormula(R) == NULL );
  fail_unless( Rule_getMath   (R) == NULL );
}
END_TEST


START_TEST (test_Rule_setFormula)
{
  char *formula = "k1*X0";


  Rule_setFormula(R, formula);

  fail_unless( !strcmp(Rule_getFormula(R), formula) );
  fail_unless( Rule_isSetFormula(R) );

  if (Rule_getFormula(R) == formula)
  {
    fail("Rule_setFormula(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  Rule_setFormula(R, Rule_getFormula(R));
  fail_unless( !strcmp(Rule_getFormula(R), formula) );

  Rule_setFormula(R, NULL);
  fail_unless( !Rule_isSetFormula(R) );

  if (Rule_getFormula(R) != NULL)
  {
    fail("Rule_setFormula(R, NULL) did not clear string.");
  }
}
END_TEST


START_TEST (test_Rule_setMath)
{
  ASTNode_t *math = SBML_parseFormula("1 + 1");


  Rule_setMath(R, math);

  fail_unless( Rule_getMath(R) != math );
  fail_unless( Rule_isSetMath(R) );

  /* Reflexive case (pathological) */
  Rule_setMath(R, (ASTNode_t *) Rule_getMath(R));
  fail_unless( Rule_getMath(R) != math );

  Rule_setMath(R, NULL);
  fail_unless( !Rule_isSetMath(R) );

  if (Rule_getMath(R) != NULL)
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

  tcase_add_test( tcase, test_Rule_init               );
  tcase_add_test( tcase, test_Rule_setFormula         );
  tcase_add_test( tcase, test_Rule_setMath            );


  suite_add_tcase(suite, tcase);

  return suite;
}
