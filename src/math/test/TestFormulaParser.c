/**
 * \file    TestFormulaParser.c
 * \brief   FormulaParser unit tests
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


#include <check.h>

#include "common/common.h"
#include "FormulaParser.h"


/**
 * The following are private and used only within FormulaParser.c; however,
 * I don't know how else to make them "public" only for testing than to put
 * them here
 */
#define START_STATE   0
#define ACCEPT_STATE  0
#define ERROR_STATE  27
#define NUM_STATES   27


/**
 * The Action[] table contains 144 entries.  To test them all would be
 * laborious and silly.  Instead, for a few token type, test the first,
 * last and middle entry in each token "run".  Also, test some error
 * states.
 */
START_TEST (test_FormulaParser_getAction)
{
  int i;
  Token_t *t = Token_create();


  t->type = TT_NAME;
  fail_unless( FormulaParser_getAction( 0, t) == 6, NULL );
  fail_unless( FormulaParser_getAction(10, t) == 6, NULL );
  fail_unless( FormulaParser_getAction(25, t) == 6, NULL );
  fail_unless( FormulaParser_getAction( 1, t) == ERROR_STATE, NULL );

  t->type = TT_INTEGER;
  fail_unless( FormulaParser_getAction( 0, t) == 1, NULL );
  fail_unless( FormulaParser_getAction(10, t) == 1, NULL );
  fail_unless( FormulaParser_getAction(25, t) == 1, NULL );
  fail_unless( FormulaParser_getAction( 1, t) == ERROR_STATE, NULL );

  t->type = TT_PLUS;
  fail_unless( FormulaParser_getAction( 1, t) ==  -9, NULL );
  fail_unless( FormulaParser_getAction(16, t) ==  -2, NULL );
  fail_unless( FormulaParser_getAction(24, t) == -11, NULL );
  fail_unless( FormulaParser_getAction( 2, t) == ERROR_STATE, NULL );

  t->type = TT_MINUS;
  fail_unless( FormulaParser_getAction( 0, t) ==  5, NULL );
  fail_unless( FormulaParser_getAction(16, t) == -2, NULL );
  fail_unless( FormulaParser_getAction(25, t) ==  5, NULL );
  fail_unless( FormulaParser_getAction( 2, t) == ERROR_STATE, NULL );

  t->type = TT_END;
  fail_unless( FormulaParser_getAction( 1, t) ==  -9, NULL );
  fail_unless( FormulaParser_getAction(17, t) ==  -5, NULL );
  fail_unless( FormulaParser_getAction(24, t) == -11, NULL );
  fail_unless( FormulaParser_getAction( 3, t) == ERROR_STATE, NULL );


  /**
   * TT_UNKNOWN should always yield an error state.
   */
  t->type = TT_UNKNOWN;
  for (i = 0; i < NUM_STATES; i++)
  {
    fail_unless( FormulaParser_getAction(i, t) == ERROR_STATE, NULL );
  }

  Token_free(t);
}
END_TEST


START_TEST (test_FormulaParser_getGoto)
{
  int r;


  fail_unless( FormulaParser_getGoto( 0,  1) ==  2, NULL );
  fail_unless( FormulaParser_getGoto(14, 12) == 21, NULL );
  fail_unless( FormulaParser_getGoto(14, 13) == 21, NULL );
  fail_unless( FormulaParser_getGoto(14, 14) == 22, NULL );
  fail_unless( FormulaParser_getGoto(14, 15) == 22, NULL );

  for (r = 2; r < 12; r++)
  {
    fail_unless( FormulaParser_getGoto( 0, r) ==  4, NULL );
    fail_unless( FormulaParser_getGoto( 3, r) ==  7, NULL );
    fail_unless( FormulaParser_getGoto( 5, r) == 13, NULL );
    fail_unless( FormulaParser_getGoto( 8, r) == 16, NULL );
    fail_unless( FormulaParser_getGoto( 9, r) == 17, NULL );
    fail_unless( FormulaParser_getGoto(10, r) == 18, NULL );
    fail_unless( FormulaParser_getGoto(11, r) == 19, NULL );
    fail_unless( FormulaParser_getGoto(12, r) == 20, NULL );
    fail_unless( FormulaParser_getGoto(14, r) == 23, NULL );
    fail_unless( FormulaParser_getGoto(25, r) == 26, NULL );
  }
}
END_TEST


START_TEST (test_SBML_parseFormula_1)
{
  ASTNode_t *r = SBML_parseFormula("1");


  fail_unless( ASTNode_getType       (r) == AST_INTEGER, NULL );
  fail_unless( ASTNode_getInteger    (r) == 1, NULL );
  fail_unless( ASTNode_getNumChildren(r) == 0, NULL );

  ASTNode_free(r);
}
END_TEST


START_TEST (test_SBML_parseFormula_2)
{
  ASTNode_t *r = SBML_parseFormula("2.1");


  fail_unless( ASTNode_getType       (r) == AST_REAL, NULL );
  fail_unless( ASTNode_getReal       (r) == 2.1, NULL );
  fail_unless( ASTNode_getNumChildren(r) ==   0, NULL );

  ASTNode_free(r);
}
END_TEST


START_TEST (test_SBML_parseFormula_3)
{
  ASTNode_t *r = SBML_parseFormula("2.1e5");


  fail_unless( ASTNode_getType       (r) == AST_REAL_E, NULL );
  fail_unless( ASTNode_getMantissa   (r) == 2.1, NULL );
  fail_unless( ASTNode_getExponent   (r) ==   5, NULL );
  fail_unless( ASTNode_getNumChildren(r) ==   0, NULL );

  ASTNode_free(r);
}
END_TEST


START_TEST (test_SBML_parseFormula_4)
{
  ASTNode_t *r = SBML_parseFormula("foo");


  fail_unless( ASTNode_getType(r) == AST_NAME     , NULL );
  fail_unless( !strcmp(ASTNode_getName(r), "foo") , NULL );
  fail_unless( ASTNode_getNumChildren(r) == 0     , NULL );

  ASTNode_free(r);
}
END_TEST


START_TEST (test_SBML_parseFormula_5)
{
  ASTNode_t *r = SBML_parseFormula("1 + foo");
  ASTNode_t *c;



  fail_unless( ASTNode_getType       (r) == AST_PLUS, NULL );
  fail_unless( ASTNode_getCharacter  (r) == '+', NULL );
  fail_unless( ASTNode_getNumChildren(r) == 2  , NULL );

  c = ASTNode_getLeftChild(r);

  fail_unless( ASTNode_getType       (c) == AST_INTEGER, NULL );
  fail_unless( ASTNode_getInteger    (c) == 1, NULL );
  fail_unless( ASTNode_getNumChildren(c) == 0, NULL );

  c = ASTNode_getRightChild(r);

  fail_unless( ASTNode_getType(c) == AST_NAME     , NULL );
  fail_unless( !strcmp(ASTNode_getName(c), "foo") , NULL );
  fail_unless( ASTNode_getNumChildren(c) == 0     , NULL );

  ASTNode_free(r);
}
END_TEST


START_TEST (test_SBML_parseFormula_6)
{
  ASTNode_t *r = SBML_parseFormula("1 + 2");
  ASTNode_t *c;



  fail_unless( ASTNode_getType       (r) == AST_PLUS, NULL );
  fail_unless( ASTNode_getCharacter  (r) == '+', NULL );
  fail_unless( ASTNode_getNumChildren(r) == 2  , NULL );

  c = ASTNode_getLeftChild(r);

  fail_unless( ASTNode_getType       (c) == AST_INTEGER, NULL );
  fail_unless( ASTNode_getInteger    (c) == 1, NULL );
  fail_unless( ASTNode_getNumChildren(c) == 0, NULL );

  c = ASTNode_getRightChild(r);

  fail_unless( ASTNode_getType       (c) == AST_INTEGER, NULL );
  fail_unless( ASTNode_getInteger    (c) == 2, NULL );
  fail_unless( ASTNode_getNumChildren(c) == 0, NULL );

  ASTNode_free(r);
}
END_TEST


START_TEST (test_SBML_parseFormula_7)
{
  ASTNode_t *r = SBML_parseFormula("1 + 2 * 3");
  ASTNode_t *c;



  fail_unless( ASTNode_getType       (r) == AST_PLUS, NULL );
  fail_unless( ASTNode_getCharacter  (r) == '+', NULL );
  fail_unless( ASTNode_getNumChildren(r) == 2  , NULL );

  c = ASTNode_getLeftChild(r);

  fail_unless( ASTNode_getType       (c) == AST_INTEGER, NULL );
  fail_unless( ASTNode_getInteger    (c) == 1, NULL );
  fail_unless( ASTNode_getNumChildren(c) == 0, NULL );

  c = ASTNode_getRightChild(r);

  fail_unless( ASTNode_getType       (c) == AST_TIMES, NULL );
  fail_unless( ASTNode_getCharacter  (c) == '*', NULL );
  fail_unless( ASTNode_getNumChildren(c) == 2  , NULL );

  c = ASTNode_getLeftChild(c);

  fail_unless( ASTNode_getType       (c) == AST_INTEGER, NULL );
  fail_unless( ASTNode_getInteger    (c) == 2, NULL );
  fail_unless( ASTNode_getNumChildren(c) == 0, NULL );

  c = ASTNode_getRightChild( ASTNode_getRightChild(r) );

  fail_unless( ASTNode_getType       (c) == AST_INTEGER, NULL );
  fail_unless( ASTNode_getInteger    (c) == 3, NULL );
  fail_unless( ASTNode_getNumChildren(c) == 0, NULL );

  ASTNode_free(r);
}
END_TEST


START_TEST (test_SBML_parseFormula_8)
{
  ASTNode_t *r = SBML_parseFormula("(1 - 2) * 3");
  ASTNode_t *c;


  fail_unless( ASTNode_getType       (r) == AST_TIMES, NULL );
  fail_unless( ASTNode_getCharacter  (r) == '*', NULL );
  fail_unless( ASTNode_getNumChildren(r) == 2  , NULL );

  c = ASTNode_getLeftChild(r);

  fail_unless( ASTNode_getType       (c) == AST_MINUS, NULL );
  fail_unless( ASTNode_getCharacter  (c) == '-', NULL );
  fail_unless( ASTNode_getNumChildren(c) == 2, NULL );

  c = ASTNode_getRightChild(r);

  fail_unless( ASTNode_getType       (c) == AST_INTEGER, NULL );
  fail_unless( ASTNode_getInteger    (c) == 3, NULL );
  fail_unless( ASTNode_getNumChildren(c) == 0, NULL );

  c = ASTNode_getLeftChild( ASTNode_getLeftChild(r) );

  fail_unless( ASTNode_getType       (c) == AST_INTEGER, NULL );
  fail_unless( ASTNode_getInteger    (c) == 1, NULL );
  fail_unless( ASTNode_getNumChildren(c) == 0, NULL );

  c = ASTNode_getRightChild( ASTNode_getLeftChild(r) );

  fail_unless( ASTNode_getType       (c) == AST_INTEGER, NULL );
  fail_unless( ASTNode_getInteger   (c)  == 2, NULL );
  fail_unless( ASTNode_getNumChildren(c) == 0, NULL );

  ASTNode_free(r);
}
END_TEST


START_TEST (test_SBML_parseFormula_9)
{
  ASTNode_t *r = SBML_parseFormula("1 + -2 / 3");
  ASTNode_t *c;


  fail_unless( ASTNode_getType       (r) == AST_PLUS, NULL );
  fail_unless( ASTNode_getCharacter  (r) == '+', NULL );
  fail_unless( ASTNode_getNumChildren(r) == 2  , NULL );

  c = ASTNode_getLeftChild(r);

  fail_unless( ASTNode_getType       (c) == AST_INTEGER, NULL );
  fail_unless( ASTNode_getInteger    (c) == 1, NULL );
  fail_unless( ASTNode_getNumChildren(c) == 0, NULL );

  c = ASTNode_getRightChild(r);

  fail_unless( ASTNode_getType       (c) == AST_DIVIDE, NULL );
  fail_unless( ASTNode_getCharacter  (c) == '/', NULL );
  fail_unless( ASTNode_getNumChildren(c) == 2  , NULL );

  c = ASTNode_getLeftChild(c);

  fail_unless( ASTNode_getType       (c) == AST_INTEGER, NULL );
  fail_unless( ASTNode_getInteger    (c) == -2, NULL );
  fail_unless( ASTNode_getNumChildren(c) ==  0, NULL );

  c = ASTNode_getRightChild( ASTNode_getRightChild(r) );

  fail_unless( ASTNode_getType       (c) == AST_INTEGER, NULL );
  fail_unless( ASTNode_getInteger    (c) == 3, NULL );
  fail_unless( ASTNode_getNumChildren(c) == 0, NULL );

  ASTNode_free(r);
}
END_TEST


START_TEST (test_SBML_parseFormula_10)
{
  ASTNode_t *r = SBML_parseFormula("1 + -2e100 / 3");
  ASTNode_t *c;


  fail_unless( ASTNode_getType       (r) == AST_PLUS, NULL );
  fail_unless( ASTNode_getCharacter  (r) == '+', NULL );
  fail_unless( ASTNode_getNumChildren(r) == 2  , NULL );

  c = ASTNode_getLeftChild(r);

  fail_unless( ASTNode_getType       (c) == AST_INTEGER, NULL );
  fail_unless( ASTNode_getInteger    (c) == 1, NULL );
  fail_unless( ASTNode_getNumChildren(c) == 0, NULL );

  c = ASTNode_getRightChild(r);

  fail_unless( ASTNode_getType       (c) == AST_DIVIDE, NULL );
  fail_unless( ASTNode_getCharacter  (c) == '/', NULL );
  fail_unless( ASTNode_getNumChildren(c) == 2  , NULL );

  c = ASTNode_getLeftChild(c);

  fail_unless( ASTNode_getType       (c) == AST_REAL_E, NULL );
  fail_unless( ASTNode_getMantissa   (c) ==  -2, NULL );
  fail_unless( ASTNode_getExponent   (c) == 100, NULL );
  fail_unless( ASTNode_getNumChildren(c) ==   0, NULL );

  c = ASTNode_getRightChild( ASTNode_getRightChild(r) );

  fail_unless( ASTNode_getType       (c) == AST_INTEGER, NULL );
  fail_unless( ASTNode_getInteger    (c) == 3, NULL );
  fail_unless( ASTNode_getNumChildren(c) == 0, NULL );

  ASTNode_free(r);
}
END_TEST


START_TEST (test_SBML_parseFormula_11)
{
  ASTNode_t *r = SBML_parseFormula("1 - -foo / 3");
  ASTNode_t *c;



  fail_unless( ASTNode_getType       (r) == AST_MINUS, NULL );
  fail_unless( ASTNode_getCharacter  (r) == '-', NULL );
  fail_unless( ASTNode_getNumChildren(r) == 2  , NULL );

  c = ASTNode_getLeftChild(r);

  fail_unless( ASTNode_getType       (c) == AST_INTEGER, NULL );
  fail_unless( ASTNode_getInteger    (c) == 1, NULL );
  fail_unless( ASTNode_getNumChildren(c) == 0, NULL );

  c = ASTNode_getRightChild(r);

  fail_unless( ASTNode_getType       (c) == AST_DIVIDE, NULL );
  fail_unless( ASTNode_getCharacter  (c) == '/', NULL );
  fail_unless( ASTNode_getNumChildren(c) == 2, NULL );

  c = ASTNode_getLeftChild( ASTNode_getRightChild(r) );

  fail_unless( ASTNode_getType       (c) == AST_MINUS, NULL );
  fail_unless( ASTNode_getCharacter  (c) == '-', NULL );
  fail_unless( ASTNode_getNumChildren(c) == 1  , NULL );

  c = ASTNode_getLeftChild( ASTNode_getLeftChild( ASTNode_getRightChild(r) ) );

  fail_unless( ASTNode_getType(c) == AST_NAME     , NULL );
  fail_unless( !strcmp(ASTNode_getName(c), "foo") , NULL );
  fail_unless( ASTNode_getNumChildren(c) == 0     , NULL );

  c = ASTNode_getRightChild( ASTNode_getRightChild(r) );

  fail_unless( ASTNode_getType       (c) == AST_INTEGER, NULL );
  fail_unless( ASTNode_getInteger    (c) == 3, NULL );
  fail_unless( ASTNode_getNumChildren(c) == 0, NULL );

  ASTNode_free(r);
}
END_TEST


START_TEST (test_SBML_parseFormula_12)
{
  ASTNode_t *r = SBML_parseFormula("2 * foo^bar + 3.0");
  ASTNode_t *c;


  fail_unless( ASTNode_getType       (r) == AST_PLUS, NULL );
  fail_unless( ASTNode_getCharacter  (r) == '+', NULL );
  fail_unless( ASTNode_getNumChildren(r) == 2  , NULL );

  c = ASTNode_getLeftChild(r);

  fail_unless( ASTNode_getType       (c) == AST_TIMES, NULL );
  fail_unless( ASTNode_getCharacter  (c) == '*', NULL );
  fail_unless( ASTNode_getNumChildren(c) == 2  , NULL );

  c = ASTNode_getRightChild(r);

  fail_unless( ASTNode_getType       (c) == AST_REAL, NULL );
  fail_unless( ASTNode_getReal       (c) == 3.0, NULL );
  fail_unless( ASTNode_getNumChildren(c) == 0  , NULL );

  c = ASTNode_getLeftChild( ASTNode_getLeftChild(r) );

  fail_unless( ASTNode_getType       (c) == AST_INTEGER, NULL );
  fail_unless( ASTNode_getInteger    (c) == 2, NULL );
  fail_unless( ASTNode_getNumChildren(c) == 0, NULL );

  c = ASTNode_getRightChild( ASTNode_getLeftChild(r) );

  fail_unless( ASTNode_getType       (c) == AST_POWER, NULL );
  fail_unless( ASTNode_getCharacter  (c) == '^', NULL );
  fail_unless( ASTNode_getNumChildren(c) == 2  , NULL );

  c = ASTNode_getLeftChild( ASTNode_getRightChild( ASTNode_getLeftChild(r) ) );

  fail_unless( ASTNode_getType(c) == AST_NAME     , NULL );
  fail_unless( !strcmp(ASTNode_getName(c), "foo") , NULL );
  fail_unless( ASTNode_getNumChildren(c) == 0     , NULL );

  c = ASTNode_getRightChild( ASTNode_getRightChild( ASTNode_getLeftChild(r) ) );

  fail_unless( ASTNode_getType(c) == AST_NAME     , NULL );
  fail_unless( !strcmp(ASTNode_getName(c), "bar") , NULL );
  fail_unless( ASTNode_getNumChildren(c) == 0     , NULL );

  ASTNode_free(r);
}
END_TEST


START_TEST (test_SBML_parseFormula_13)
{
  ASTNode_t *r = SBML_parseFormula("foo()");


  fail_unless( ASTNode_getType(r) == AST_FUNCTION , NULL );
  fail_unless( !strcmp(ASTNode_getName(r), "foo") , NULL );
  fail_unless( ASTNode_getNumChildren(r) == 0     , NULL );

  ASTNode_free(r);
}
END_TEST


START_TEST (test_SBML_parseFormula_14)
{
  ASTNode_t *r = SBML_parseFormula("foo(1)");
  ASTNode_t *c;


  fail_unless( ASTNode_getType(r) == AST_FUNCTION , NULL );
  fail_unless( !strcmp(ASTNode_getName(r), "foo") , NULL );
  fail_unless( ASTNode_getNumChildren(r) == 1     , NULL );

  c = ASTNode_getLeftChild(r);

  fail_unless( ASTNode_getType       (c) == AST_INTEGER, NULL );
  fail_unless( ASTNode_getInteger    (c) == 1, NULL );
  fail_unless( ASTNode_getNumChildren(c) == 0, NULL );

  ASTNode_free(r);
}
END_TEST


START_TEST (test_SBML_parseFormula_15)
{
  ASTNode_t *r = SBML_parseFormula("foo(1, bar)");
  ASTNode_t *c;


  fail_unless( ASTNode_getType(r) == AST_FUNCTION , NULL );
  fail_unless( !strcmp(ASTNode_getName(r), "foo") , NULL );
  fail_unless( ASTNode_getNumChildren(r) == 2     , NULL );

  c = ASTNode_getLeftChild(r);

  fail_unless( ASTNode_getType       (c) == AST_INTEGER, NULL );
  fail_unless( ASTNode_getInteger    (c) == 1, NULL );
  fail_unless( ASTNode_getNumChildren(c) == 0, NULL );

  c = ASTNode_getRightChild(r);

  fail_unless( ASTNode_getType(c) == AST_NAME     , NULL );
  fail_unless( !strcmp(ASTNode_getName(c), "bar") , NULL );
  fail_unless( ASTNode_getNumChildren(c) == 0     , NULL );

  ASTNode_free(r);
}
END_TEST


START_TEST (test_SBML_parseFormula_16)
{
  ASTNode_t *r = SBML_parseFormula("foo(1, bar, 2^-3)");
  ASTNode_t *c;


  fail_unless( ASTNode_getType(r) == AST_FUNCTION , NULL );
  fail_unless( !strcmp(ASTNode_getName(r), "foo") , NULL );
  fail_unless( ASTNode_getNumChildren(r) == 3     , NULL );

  c = ASTNode_getChild(r, 0);

  fail_unless( ASTNode_getType       (c) == AST_INTEGER, NULL );
  fail_unless( ASTNode_getInteger    (c) == 1, NULL );
  fail_unless( ASTNode_getNumChildren(c) == 0, NULL );

  c = ASTNode_getChild(r, 1);

  fail_unless( ASTNode_getType(c) == AST_NAME     , NULL );
  fail_unless( !strcmp(ASTNode_getName(c), "bar") , NULL );
  fail_unless( ASTNode_getNumChildren(c) == 0     , NULL );

  c = ASTNode_getChild(r, 2);

  fail_unless( ASTNode_getType       (c) == AST_POWER, NULL );
  fail_unless( ASTNode_getCharacter  (c) == '^', NULL );
  fail_unless( ASTNode_getNumChildren(c) == 2  , NULL );

  c = ASTNode_getLeftChild( ASTNode_getChild(r, 2) );

  fail_unless( ASTNode_getType       (c) == AST_INTEGER, NULL );
  fail_unless( ASTNode_getInteger    (c) == 2, NULL );
  fail_unless( ASTNode_getNumChildren(c) == 0, NULL );

  c = ASTNode_getRightChild( ASTNode_getChild(r, 2) );

  fail_unless( ASTNode_getType       (c) == AST_INTEGER, NULL );
  fail_unless( ASTNode_getInteger    (c) == -3, NULL );
  fail_unless( ASTNode_getNumChildren(c) == 0 , NULL );

  ASTNode_free(r);
}
END_TEST


START_TEST (test_SBML_parseFormula_17)
{
  ASTNode_t *r = SBML_parseFormula("1//1");


  /* Pathetic error handling */
  fail_unless(r == NULL, NULL);

  ASTNode_free(r);
}
END_TEST


START_TEST (test_SBML_parseFormula_18)
{
  ASTNode_t *r = SBML_parseFormula("1+2*3 4");


  /* Pathetic error handling */
  fail_unless(r == NULL, NULL);

  ASTNode_free(r);
}
END_TEST


START_TEST (test_SBML_parseFormula_negInf)
{
  ASTNode_t *r = SBML_parseFormula("-inf");


  fail_unless( ASTNode_getType(r)             == AST_REAL, NULL );
  fail_unless( util_isInf(ASTNode_getReal(r)) == -1, NULL );
  fail_unless( ASTNode_getNumChildren(r)      ==  0, NULL );

  ASTNode_free(r);
}
END_TEST


START_TEST (test_SBML_parseFormula_negZero)
{
  ASTNode_t *r = SBML_parseFormula("-0.0");


  fail_unless( ASTNode_getType(r)                 == AST_REAL, NULL );
  fail_unless( util_isNegZero(ASTNode_getReal(r)) == 1, NULL );
  fail_unless( ASTNode_getNumChildren(r)          == 0, NULL );

  ASTNode_free(r);
}
END_TEST


Suite *
create_suite_FormulaParser (void) 
{ 
  Suite *suite = suite_create("FormulaParser");
  TCase *tcase = tcase_create("FormulaParser");
 

  tcase_add_test( tcase, test_FormulaParser_getAction );
  tcase_add_test( tcase, test_FormulaParser_getGoto   );

  tcase_add_test( tcase, test_SBML_parseFormula_1       );
  tcase_add_test( tcase, test_SBML_parseFormula_2       );
  tcase_add_test( tcase, test_SBML_parseFormula_3       );
  tcase_add_test( tcase, test_SBML_parseFormula_4       );
  tcase_add_test( tcase, test_SBML_parseFormula_5       );
  tcase_add_test( tcase, test_SBML_parseFormula_6       );
  tcase_add_test( tcase, test_SBML_parseFormula_7       );
  tcase_add_test( tcase, test_SBML_parseFormula_8       );
  tcase_add_test( tcase, test_SBML_parseFormula_9       );
  tcase_add_test( tcase, test_SBML_parseFormula_10      );
  tcase_add_test( tcase, test_SBML_parseFormula_11      );
  tcase_add_test( tcase, test_SBML_parseFormula_12      );
  tcase_add_test( tcase, test_SBML_parseFormula_13      );
  tcase_add_test( tcase, test_SBML_parseFormula_14      );
  tcase_add_test( tcase, test_SBML_parseFormula_15      );
  tcase_add_test( tcase, test_SBML_parseFormula_16      );
  tcase_add_test( tcase, test_SBML_parseFormula_17      );
  tcase_add_test( tcase, test_SBML_parseFormula_18      );
  tcase_add_test( tcase, test_SBML_parseFormula_negInf  );
  tcase_add_test( tcase, test_SBML_parseFormula_negZero );

  suite_add_tcase(suite, tcase);

  return suite;
}
