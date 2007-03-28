/**
 * \file    TestFunctionDefinition.c
 * \brief   SBML FunctionDefinition unit tests
 * \author  Ben Bornstein
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
#include "FunctionDefinition.h"

#include <check.h>


static FunctionDefinition_t *FD;


void
FunctionDefinitionTest_setup (void)
{
  FD = FunctionDefinition_create();

  if (FD == NULL)
  {
    fail("FunctionDefinition_create() returned a NULL pointer.");
  }
}


void
FunctionDefinitionTest_teardown (void)
{
  FunctionDefinition_free(FD);
}


START_TEST (test_FunctionDefinition_create)
{
  fail_unless( SBase_getTypeCode((SBase_t *) FD) == SBML_FUNCTION_DEFINITION );
  fail_unless( SBase_getMetaId    ((SBase_t *) FD) == NULL );
/*  fail_unless( SBase_getNotes     ((SBase_t *) FD) == NULL );
  fail_unless( SBase_getAnnotation((SBase_t *) FD) == NULL );
*/
  fail_unless( FunctionDefinition_getId  (FD) == NULL );
  fail_unless( FunctionDefinition_getName(FD) == NULL );
  fail_unless( FunctionDefinition_getMath(FD) == NULL );
}
END_TEST


START_TEST (test_FunctionDefinition_createWith)
{
  ASTNode_t            *math = SBML_parseFormula("lambda(x, x^3)");
  FunctionDefinition_t *fd   = FunctionDefinition_createWith("pow3", math);

  const ASTNode_t * math1;
  char * formula;

  fail_unless( SBase_getTypeCode((SBase_t *) fd) == SBML_FUNCTION_DEFINITION );
  fail_unless( SBase_getMetaId    ((SBase_t *) fd) == NULL );
/*  fail_unless( SBase_getNotes     ((SBase_t *) fd) == NULL );
  fail_unless( SBase_getAnnotation((SBase_t *) fd) == NULL );
*/
  fail_unless( FunctionDefinition_getName(fd) == NULL );

  math1 = FunctionDefinition_getMath(fd);
  fail_unless( math1 != NULL );

  formula = SBML_formulaToString(math1);
  fail_unless( formula != NULL );
  fail_unless( !strcmp(formula, "lambda(x, x^3)") );
  fail_unless( FunctionDefinition_getMath(fd) != math );
  fail_unless( FunctionDefinition_isSetMath(fd) );

  fail_unless( !strcmp(FunctionDefinition_getId(fd), "pow3") );
  fail_unless( FunctionDefinition_isSetId(fd) );

  ASTNode_free(math);
  FunctionDefinition_free(fd);
}
END_TEST


START_TEST (test_FunctionDefinition_free_NULL)
{
  FunctionDefinition_free(NULL);
}
END_TEST


START_TEST (test_FunctionDefinition_getArguments)
{
  const ASTNode_t *math;


  FunctionDefinition_setMath(FD, SBML_parseFormula("lambda(x, y, x^y)") );

  fail_unless( FunctionDefinition_getNumArguments(FD) == 2 );


  math = FunctionDefinition_getArgument(FD, 0);

  fail_unless( math != NULL                        );
  fail_unless( ASTNode_isName(math)                );
  fail_unless( !strcmp(ASTNode_getName(math), "x") );
  fail_unless( ASTNode_getNumChildren(math) == 0   );

  math = FunctionDefinition_getArgument(FD, 1);

  fail_unless( math != NULL                        );
  fail_unless( ASTNode_isName(math)                );
  fail_unless( !strcmp(ASTNode_getName(math), "y") );
  fail_unless( ASTNode_getNumChildren(math) == 0   );

  fail_unless( FunctionDefinition_getArgument(FD, 0) ==
               FunctionDefinition_getArgumentByName(FD, "x") );

  fail_unless( FunctionDefinition_getArgument(FD, 1) ==
               FunctionDefinition_getArgumentByName(FD, "y") );
}
END_TEST


START_TEST (test_FunctionDefinition_getBody)
{
  const ASTNode_t *math;

  ASTNode_t * math1 = SBML_parseFormula("lambda(x, x)");

  FunctionDefinition_setMath(FD, math1 );
  math = FunctionDefinition_getBody(FD);

  fail_unless( math != NULL                        );
  fail_unless( ASTNode_isName(math)                );
  fail_unless( !strcmp(ASTNode_getName(math), "x") );
  fail_unless( ASTNode_getNumChildren(math) == 0   );

  ASTNode_free(math1);
}
END_TEST


START_TEST (test_FunctionDefinition_setId)
{
  char *id = "pow3";


  FunctionDefinition_setId(FD, id);

  fail_unless( !strcmp(FunctionDefinition_getId(FD), id) );
  fail_unless( FunctionDefinition_isSetId(FD) );

  if (FunctionDefinition_getId(FD) == id)
  {
    fail("FunctionDefinition_setId(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  FunctionDefinition_setId(FD, FunctionDefinition_getId(FD));
  fail_unless( !strcmp(FunctionDefinition_getId(FD), id) );

  FunctionDefinition_setId(FD, NULL);
  fail_unless( !FunctionDefinition_isSetId(FD) );

  if (FunctionDefinition_getId(FD) != NULL)
  {
    fail("FunctionDefinition_setId(FD, NULL) did not clear string.");
  }
}
END_TEST


START_TEST (test_FunctionDefinition_setName)
{
  char *name = "Cube Me";


  FunctionDefinition_setName(FD, name);

  fail_unless( !strcmp(FunctionDefinition_getName(FD), name) );
  fail_unless( FunctionDefinition_isSetName(FD) );

  if (FunctionDefinition_getName(FD) == name)
  {
    fail("FunctionDefinition_setName(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  FunctionDefinition_setName(FD, FunctionDefinition_getName(FD));
  fail_unless( !strcmp(FunctionDefinition_getName(FD), name) );

  FunctionDefinition_setName(FD, NULL);
  fail_unless( !FunctionDefinition_isSetName(FD) );

  if (FunctionDefinition_getName(FD) != NULL)
  {
    fail("FunctionDefinition_setName(FD, NULL) did not clear string.");
  }
}
END_TEST


START_TEST (test_FunctionDefinition_setMath)
{
  ASTNode_t *math = SBML_parseFormula("lambda(x, x^3)");

  const ASTNode_t * math1;
  char * formula;

  FunctionDefinition_setMath(FD, math);

  math1 = FunctionDefinition_getMath(FD);
  fail_unless( math1 != NULL );

  formula = SBML_formulaToString(math1);
  fail_unless( formula != NULL );
  fail_unless( !strcmp(formula, "lambda(x, x^3)") );
  fail_unless( FunctionDefinition_getMath(FD) != math );
  fail_unless( FunctionDefinition_isSetMath(FD) );

  /* Reflexive case (pathological) */
  FunctionDefinition_setMath(FD, (ASTNode_t *) FunctionDefinition_getMath(FD));
  math1 = FunctionDefinition_getMath(FD);
  fail_unless( math1 != NULL );

  formula = SBML_formulaToString(math1);
  fail_unless( formula != NULL );
  fail_unless( !strcmp(formula, "lambda(x, x^3)") );
  fail_unless( FunctionDefinition_getMath(FD) != math );

  FunctionDefinition_setMath(FD, NULL);
  fail_unless( !FunctionDefinition_isSetMath(FD) );

  if (FunctionDefinition_getMath(FD) != NULL)
  {
    fail("FunctionDefinition_setMath(FD, NULL) did not clear ASTNode.");
  }
}
END_TEST


Suite *
create_suite_FunctionDefinition (void)
{
  Suite *suite = suite_create("FunctionDefinition");
  TCase *tcase = tcase_create("FunctionDefinition");


  tcase_add_checked_fixture( tcase,
                             FunctionDefinitionTest_setup,
                             FunctionDefinitionTest_teardown );

  tcase_add_test( tcase, test_FunctionDefinition_create       );
  tcase_add_test( tcase, test_FunctionDefinition_createWith   );
  tcase_add_test( tcase, test_FunctionDefinition_free_NULL    );
  tcase_add_test( tcase, test_FunctionDefinition_getArguments );
  tcase_add_test( tcase, test_FunctionDefinition_getBody      );
  tcase_add_test( tcase, test_FunctionDefinition_setId        );
  tcase_add_test( tcase, test_FunctionDefinition_setName      );
  tcase_add_test( tcase, test_FunctionDefinition_setMath      );

  suite_add_tcase(suite, tcase);

  return suite;
}
