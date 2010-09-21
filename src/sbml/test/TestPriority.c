/**
 * \file    TestPriority.c
 * \brief   SBML Priority unit tests
 * \author  Sarah Keating
 *
 * $Id:  $
 * $HeadURL:  $
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
#include <sbml/Priority.h>
#include <sbml/xml/XMLNamespaces.h>
#include <sbml/SBMLDocument.h>

#include <check.h>


static Priority_t *P;


void
PriorityTest_setup (void)
{
  P = Priority_create(3, 1);

  if (P == NULL)
  {
    fail("Priority_create() returned a NULL pointer.");
  }
}


void
PriorityTest_teardown (void)
{
  Priority_free(P);
}


START_TEST (test_Priority_create)
{
  fail_unless( SBase_getTypeCode((SBase_t *) P) == SBML_PRIORITY );
  fail_unless( SBase_getMetaId    ((SBase_t *) P) == NULL );
  fail_unless( SBase_getNotes     ((SBase_t *) P) == NULL );
  fail_unless( SBase_getAnnotation((SBase_t *) P) == NULL );

  fail_unless( Priority_getMath(P) == NULL );
}
END_TEST


START_TEST (test_Priority_free_NULL)
{
  Priority_free(NULL);
}
END_TEST


START_TEST (test_Priority_setMath)
{
  ASTNode_t *math = SBML_parseFormula("lambda(x, x^3)");

  const ASTNode_t * math1;
  char * formula;

  Priority_setMath(P, math);

  math1 = Priority_getMath(P);
  fail_unless( math1 != NULL );

  formula = SBML_formulaToString(math1);
  fail_unless( formula != NULL );
  fail_unless( !strcmp(formula, "lambda(x, x^3)") );
  fail_unless( Priority_getMath(P) != math );
  fail_unless( Priority_isSetMath(P) );

  /* Reflexive case (pathological) */
  Priority_setMath(P, (ASTNode_t *) Priority_getMath(P));
  math1 = Priority_getMath(P);
  fail_unless( math1 != NULL );

  formula = SBML_formulaToString(math1);
  fail_unless( formula != NULL );
  fail_unless( !strcmp(formula, "lambda(x, x^3)") );

  Priority_setMath(P, NULL);
  fail_unless( !Priority_isSetMath(P) );

  if (Priority_getMath(P) != NULL)
  {
    fail("Priority_setMath(P, NULL) did not clear ASTNode.");
  }
}
END_TEST


START_TEST (test_Priority_setMath1)
{
  ASTNode_t *math = SBML_parseFormula("2 * k");

  int i = Priority_setMath(P, math);

  fail_unless( i == LIBSBML_OPERATION_SUCCESS);
  fail_unless( Priority_getMath(P) != math );
  fail_unless( Priority_isSetMath(P) );

  i = Priority_setMath(P, NULL);
  
  fail_unless( i == LIBSBML_OPERATION_SUCCESS);
  fail_unless( Priority_getMath(P) == NULL );
  fail_unless( !Priority_isSetMath(P) );

  ASTNode_free(math);
}
END_TEST


START_TEST (test_Priority_setMath2)
{
  ASTNode_t *math = ASTNode_createWithType(AST_TIMES);

  int i = Priority_setMath(P, math);

  fail_unless( i == LIBSBML_INVALID_OBJECT);
  fail_unless( !Priority_isSetMath(P) );

  ASTNode_free(math);
}
END_TEST


START_TEST (test_Priority_createWithNS )
{
  XMLNamespaces_t *xmlns = XMLNamespaces_create();
  XMLNamespaces_add(xmlns, "http://www.sbml.org", "testsbml");
  SBMLNamespaces_t *sbmlns = SBMLNamespaces_create(2,1);
  SBMLNamespaces_addNamespaces(sbmlns,xmlns);

  Priority_t *object = 
    Priority_createWithNS (sbmlns);


  fail_unless( SBase_getTypeCode  ((SBase_t *) object) == SBML_PRIORITY );
  fail_unless( SBase_getMetaId    ((SBase_t *) object) == NULL );
  fail_unless( SBase_getNotes     ((SBase_t *) object) == NULL );
  fail_unless( SBase_getAnnotation((SBase_t *) object) == NULL );

  fail_unless( SBase_getLevel       ((SBase_t *) object) == 2 );
  fail_unless( SBase_getVersion     ((SBase_t *) object) == 1 );

  fail_unless( Priority_getNamespaces     (object) != NULL );
  fail_unless( XMLNamespaces_getLength(Priority_getNamespaces(object)) == 2 );

  Priority_free(object);
}
END_TEST


Suite *
create_suite_Priority (void)
{
  Suite *suite = suite_create("Priority");
  TCase *tcase = tcase_create("Priority");


  tcase_add_checked_fixture( tcase,
                             PriorityTest_setup,
                             PriorityTest_teardown );

  tcase_add_test( tcase, test_Priority_create       );
  tcase_add_test( tcase, test_Priority_setMath      );
  tcase_add_test( tcase, test_Priority_setMath1     );
  tcase_add_test( tcase, test_Priority_setMath2     );
  tcase_add_test( tcase, test_Priority_free_NULL );
  tcase_add_test( tcase, test_Priority_createWithNS         );

  suite_add_tcase(suite, tcase);

  return suite;
}
