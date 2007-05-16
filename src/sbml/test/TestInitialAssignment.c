/**
 * \file    TestInitialAssignment.c
 * \brief   SBML InitialAssignment unit tests
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


#include "common/common.h"
#include "math/FormulaParser.h"
#include "math/FormulaFormatter.h"

#include "SBase.h"
#include "InitialAssignment.h"

#include <check.h>


static InitialAssignment_t *IA;


void
InitialAssignmentTest_setup (void)
{
  IA = InitialAssignment_create();

  if (IA == NULL)
  {
    fail("InitialAssignment_create() returned a NULL pointer.");
  }
}


void
InitialAssignmentTest_teardown (void)
{
  InitialAssignment_free(IA);
}


START_TEST (test_InitialAssignment_create)
{
  fail_unless( SBase_getTypeCode  ((SBase_t *) IA) == SBML_INITIAL_ASSIGNMENT );
  fail_unless( SBase_getMetaId    ((SBase_t *) IA) == NULL );
  fail_unless( SBase_getNotes     ((SBase_t *) IA) == NULL );
  fail_unless( SBase_getAnnotation((SBase_t *) IA) == NULL );

  fail_unless( InitialAssignment_getSymbol(IA) == NULL );
  fail_unless( InitialAssignment_getMath    (IA) == NULL );
}
END_TEST


START_TEST (test_InitialAssignment_createWith)
{
  InitialAssignment_t *ia   = InitialAssignment_createWithSymbol("k");

  fail_unless( SBase_getTypeCode  ((SBase_t *) ia) == SBML_INITIAL_ASSIGNMENT );
  fail_unless( SBase_getMetaId    ((SBase_t *) ia) == NULL );
  fail_unless( SBase_getNotes     ((SBase_t *) ia) == NULL );
  fail_unless( SBase_getAnnotation((SBase_t *) ia) == NULL );

  fail_unless( !InitialAssignment_isSetMath(ia) );

  fail_unless( !strcmp(InitialAssignment_getSymbol(ia), "k") );
  fail_unless( InitialAssignment_isSetSymbol(ia) );

  InitialAssignment_free(ia);
}
END_TEST


START_TEST (test_InitialAssignment_free_NULL)
{
  InitialAssignment_free(NULL);
}
END_TEST


START_TEST (test_InitialAssignment_setSymbol)
{
  char *Symbol = "k2";


  InitialAssignment_setSymbol(IA, Symbol);

  fail_unless( !strcmp(InitialAssignment_getSymbol(IA), Symbol) );
  fail_unless( InitialAssignment_isSetSymbol(IA) );

  if (InitialAssignment_getSymbol(IA) == Symbol)
  {
    fail("InitialAssignment_setSymbol(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  InitialAssignment_setSymbol(IA, InitialAssignment_getSymbol(IA));
  fail_unless( !strcmp(InitialAssignment_getSymbol(IA), Symbol) );

  InitialAssignment_setSymbol(IA, NULL);
  fail_unless( !InitialAssignment_isSetSymbol(IA) );

  if (InitialAssignment_getSymbol(IA) != NULL)
  {
    fail("InitialAssignment_setSymbol(IA, NULL) did not clear string.");
  }
}
END_TEST


START_TEST (test_InitialAssignment_setMath)
{
  ASTNode_t *math = SBML_parseFormula("2 * k");
  char *formula;
  const ASTNode_t *math1;

  InitialAssignment_setMath(IA, math);

  math1 = InitialAssignment_getMath(IA);
  fail_unless( math1 != NULL );

  formula = SBML_formulaToString(math1);
  fail_unless( formula != NULL );
  fail_unless( !strcmp(formula, "2 * k") );
  fail_unless( InitialAssignment_getMath(IA) != math );
  fail_unless( InitialAssignment_isSetMath(IA) );

  /* Reflexive case (pathological) */
  InitialAssignment_setMath(IA, (ASTNode_t *) InitialAssignment_getMath(IA));

  math1 = InitialAssignment_getMath(IA);
  fail_unless( math1 != NULL );

  formula = SBML_formulaToString(math1);
  fail_unless( formula != NULL );
  fail_unless( !strcmp(formula, "2 * k") );
  fail_unless( InitialAssignment_getMath(IA) != math );

  InitialAssignment_setMath(IA, NULL);
  fail_unless( !InitialAssignment_isSetMath(IA) );

  if (InitialAssignment_getMath(IA) != NULL)
  {
    fail("InitialAssignment_setMath(IA, NULL) did not clear ASTNode.");
  }

  ASTNode_free(math);
}
END_TEST


Suite *
create_suite_InitialAssignment (void)
{
  Suite *suite = suite_create("InitialAssignment");
  TCase *tcase = tcase_create("InitialAssignment");


  tcase_add_checked_fixture( tcase,
                             InitialAssignmentTest_setup,
                             InitialAssignmentTest_teardown );

  tcase_add_test( tcase, test_InitialAssignment_create      );
  tcase_add_test( tcase, test_InitialAssignment_createWith  );
  tcase_add_test( tcase, test_InitialAssignment_free_NULL   );
  tcase_add_test( tcase, test_InitialAssignment_setSymbol );
  tcase_add_test( tcase, test_InitialAssignment_setMath     );

  suite_add_tcase(suite, tcase);

  return suite;
}
