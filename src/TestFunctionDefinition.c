/**
 * Filename    : TestFunctionDefinition.c
 * Description : SBML FunctionDefinition unit tests
 * Author(s)   : SBML Development Group <sbml-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2003-05-03
 * Revision    : $Id$
 * Source      : $Source$
 *
 * Copyright 2003 California Institute of Technology and
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


#include <check.h>

#include "sbml/common.h"
#include "sbml/FormulaParser.h"
#include "sbml/FunctionDefinition.h"


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
  fail_unless( SBase_getTypeCode  (FD) == SBML_FUNCTION_DEFINITION, NULL );
  fail_unless( SBase_getMetaId    (FD) == NULL, NULL );
  fail_unless( SBase_getNotes     (FD) == NULL, NULL );
  fail_unless( SBase_getAnnotation(FD) == NULL, NULL );

  fail_unless( FunctionDefinition_getId  (FD) == NULL, NULL );
  fail_unless( FunctionDefinition_getName(FD) == NULL, NULL );
  fail_unless( FunctionDefinition_getMath(FD) == NULL, NULL );
}
END_TEST


START_TEST (test_FunctionDefinition_createWith)
{
  ASTNode_t            *math = SBML_parseFormula("lambda(x, x^3)");
  FunctionDefinition_t *fd   = FunctionDefinition_createWith("pow3", math);


  fail_unless( SBase_getTypeCode  (fd) == SBML_FUNCTION_DEFINITION, NULL );
  fail_unless( SBase_getMetaId    (fd) == NULL, NULL );
  fail_unless( SBase_getNotes     (fd) == NULL, NULL );
  fail_unless( SBase_getAnnotation(fd) == NULL, NULL );

  fail_unless( FunctionDefinition_getName(fd) == NULL, NULL );

  fail_unless( FunctionDefinition_getMath(fd) == math, NULL );
  fail_unless( FunctionDefinition_isSetMath(fd), NULL );

  fail_unless( !strcmp(FunctionDefinition_getId(fd), "pow3"), NULL );
  fail_unless( FunctionDefinition_isSetId(fd), NULL );

  FunctionDefinition_free(fd);
}
END_TEST


START_TEST (test_FunctionDefinition_free_NULL)
{
  FunctionDefinition_free(NULL);
}
END_TEST


START_TEST (test_FunctionDefinition_setId)
{
  char *id = "pow3";


  FunctionDefinition_setId(FD, id);

  fail_unless( !strcmp(FunctionDefinition_getId(FD), id), NULL );
  fail_unless( FunctionDefinition_isSetId(FD), NULL );

  if (FunctionDefinition_getId(FD) == id)
  {
    fail("FunctionDefinition_setId(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  FunctionDefinition_setId(FD, FunctionDefinition_getId(FD));
  fail_unless( !strcmp(FunctionDefinition_getId(FD), id), NULL );

  FunctionDefinition_setId(FD, NULL);
  fail_unless( !FunctionDefinition_isSetId(FD), NULL );

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

  fail_unless( !strcmp(FunctionDefinition_getName(FD), name), NULL );
  fail_unless( FunctionDefinition_isSetName(FD), NULL );

  if (FunctionDefinition_getName(FD) == name)
  {
    fail("FunctionDefinition_setName(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  FunctionDefinition_setName(FD, FunctionDefinition_getName(FD));
  fail_unless( !strcmp(FunctionDefinition_getName(FD), name), NULL );

  FunctionDefinition_setName(FD, NULL);
  fail_unless( !FunctionDefinition_isSetName(FD), NULL );

  if (FunctionDefinition_getName(FD) != NULL)
  {
    fail("FunctionDefinition_setName(FD, NULL) did not clear string.");
  }
}
END_TEST


START_TEST (test_FunctionDefinition_setMath)
{
  ASTNode_t *math = SBML_parseFormula("lambda(x, x^3)");


  FunctionDefinition_setMath(FD, math);

  fail_unless( FunctionDefinition_getMath(FD) == math, NULL );
  fail_unless( FunctionDefinition_isSetMath(FD), NULL );

  /* Reflexive case (pathological) */
  FunctionDefinition_setMath(FD, (ASTNode_t *) FunctionDefinition_getMath(FD));
  fail_unless( FunctionDefinition_getMath(FD) == math, NULL );

  FunctionDefinition_setMath(FD, NULL);
  fail_unless( !FunctionDefinition_isSetMath(FD), NULL );

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

  tcase_add_test( tcase, test_FunctionDefinition_create     );
  tcase_add_test( tcase, test_FunctionDefinition_createWith );
  tcase_add_test( tcase, test_FunctionDefinition_free_NULL  );
  tcase_add_test( tcase, test_FunctionDefinition_setId      );
  tcase_add_test( tcase, test_FunctionDefinition_setName    );
  tcase_add_test( tcase, test_FunctionDefinition_setMath    );

  suite_add_tcase(suite, tcase);

  return suite;
}
