/**
 * \file    TestDelay.c
 * \brief   SBML Delay unit tests
 * \author  Sarah Keating
 *
 * $Id$
 * $Source$
 */
/* Copyright 2003 California Institute of Technology and
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


#include <sbml/common/common.h>
#include <sbml/math/FormulaParser.h>
#include <sbml/math/FormulaFormatter.h>

#include <sbml/SBase.h>
#include <sbml/Delay.h>

#include <check.h>


static Delay_t *D;


void
DelayTest_setup (void)
{
  D = Delay_create();

  if (D == NULL)
  {
    fail("Delay_create() returned a NULL pointer.");
  }
}


void
DelayTest_teardown (void)
{
  Delay_free(D);
}


START_TEST (test_Delay_create)
{
  fail_unless( SBase_getTypeCode((SBase_t *) D) == SBML_DELAY );
  fail_unless( SBase_getMetaId    ((SBase_t *) D) == NULL );
  fail_unless( SBase_getNotes     ((SBase_t *) D) == NULL );
  fail_unless( SBase_getAnnotation((SBase_t *) D) == NULL );

  fail_unless( Delay_getMath(D) == NULL );
}
END_TEST


START_TEST (test_Delay_createWithMath)
{
  ASTNode_t            *math = SBML_parseFormula("x^3");
  Delay_t *fd   = Delay_createWithMath(math);

  const ASTNode_t * math1;
  char * formula;

  fail_unless( SBase_getTypeCode((SBase_t *) fd) == SBML_DELAY );
  fail_unless( SBase_getMetaId    ((SBase_t *) fd) == NULL );
  fail_unless( SBase_getNotes     ((SBase_t *) fd) == NULL );
  fail_unless( SBase_getAnnotation((SBase_t *) fd) == NULL );


  math1 = Delay_getMath(fd);
  fail_unless( math1 != NULL );

  formula = SBML_formulaToString(math1);
  fail_unless( formula != NULL );
  fail_unless( !strcmp(formula, "x^3") );
  fail_unless( Delay_getMath(fd) != math );
  fail_unless( Delay_isSetMath(fd) );


  Delay_free(fd);
}
END_TEST


START_TEST (test_Delay_free_NULL)
{
  Delay_free(NULL);
}
END_TEST


START_TEST (test_Delay_setMath)
{
  ASTNode_t *math = SBML_parseFormula("lambda(x, x^3)");

  const ASTNode_t * math1;
  char * formula;

  Delay_setMath(D, math);

  math1 = Delay_getMath(D);
  fail_unless( math1 != NULL );

  formula = SBML_formulaToString(math1);
  fail_unless( formula != NULL );
  fail_unless( !strcmp(formula, "lambda(x, x^3)") );
  fail_unless( Delay_getMath(D) != math );
  fail_unless( Delay_isSetMath(D) );

  /* Reflexive case (pathological) */
  Delay_setMath(D, (ASTNode_t *) Delay_getMath(D));
  math1 = Delay_getMath(D);
  fail_unless( math1 != NULL );

  formula = SBML_formulaToString(math1);
  fail_unless( formula != NULL );
  fail_unless( !strcmp(formula, "lambda(x, x^3)") );

  Delay_setMath(D, NULL);
  fail_unless( !Delay_isSetMath(D) );

  if (Delay_getMath(D) != NULL)
  {
    fail("Delay_setMath(D, NULL) did not clear ASTNode.");
  }
}
END_TEST


Suite *
create_suite_Delay (void)
{
  Suite *suite = suite_create("Delay");
  TCase *tcase = tcase_create("Delay");


  tcase_add_checked_fixture( tcase,
                             DelayTest_setup,
                             DelayTest_teardown );

  tcase_add_test( tcase, test_Delay_create       );
  tcase_add_test( tcase, test_Delay_createWithMath   );
  tcase_add_test( tcase, test_Delay_setMath      );
  tcase_add_test( tcase, test_Delay_free_NULL );

  suite_add_tcase(suite, tcase);

  return suite;
}
