/**
 * Filename    : TestFormulaParser.c
 * Description : FormulaParser unit tests
 * Author(s)   : SBW Development Group <sysbio-team@caltech.edu>
 * Organization: Caltech ERATO Kitano Systems Biology Project
 * Created     : 2003-05-02
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


#include "sbml/FormulaParser.h"


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

/*
START_TEST (test_FormulaParser_reduceStackByRule)
{
}
END_TEST
*/


START_TEST (test_SBML_parseFormula_1)
{
  ASTNode_t *r = SBML_parseFormula("1");


  fail_unless( r->type                   == AST_INTEGER, NULL );
  fail_unless( r->value.integer          == 1, NULL );
  fail_unless( ASTNode_getNumChildren(r) == 0, NULL );

  ASTNode_free(r);
}
END_TEST


START_TEST (test_SBML_parseFormula_2)
{
  ASTNode_t *r = SBML_parseFormula("foo");


  fail_unless( r->type == AST_NAME           , NULL );
  fail_unless( !strcmp(r->value.name, "foo") , NULL );
  fail_unless( ASTNode_getNumChildren(r) == 0, NULL );

  ASTNode_free(r);
}
END_TEST


START_TEST (test_SBML_parseFormula_3)
{
  ASTNode_t *r = SBML_parseFormula("1 + foo");
  ASTNode_t *c;



  fail_unless( r->type                   == AST_PLUS, NULL );
  fail_unless( r->value.ch               == '+', NULL );
  fail_unless( ASTNode_getNumChildren(r) == 2  , NULL );

  c = ASTNode_getLeftChild(r);

  fail_unless( c->type                   == AST_INTEGER, NULL );
  fail_unless( c->value.integer          == 1, NULL );
  fail_unless( ASTNode_getNumChildren(c) == 0, NULL );

  c = ASTNode_getRightChild(r);

  fail_unless( c->type == AST_NAME           , NULL );
  fail_unless( !strcmp(c->value.name, "foo") , NULL );
  fail_unless( ASTNode_getNumChildren(c) == 0, NULL );

  ASTNode_free(r);
}
END_TEST


START_TEST (test_SBML_parseFormula_4)
{
  ASTNode_t *r = SBML_parseFormula("1 + 2");
  ASTNode_t *c;



  fail_unless( r->type                   == AST_PLUS, NULL );
  fail_unless( r->value.ch               == '+', NULL );
  fail_unless( ASTNode_getNumChildren(r) == 2  , NULL );

  c = ASTNode_getLeftChild(r);

  fail_unless( c->type                   == AST_INTEGER, NULL );
  fail_unless( c->value.integer          == 1, NULL );
  fail_unless( ASTNode_getNumChildren(c) == 0, NULL );

  c = ASTNode_getRightChild(r);

  fail_unless( c->type                   == AST_INTEGER, NULL );
  fail_unless( c->value.integer          == 2, NULL );
  fail_unless( ASTNode_getNumChildren(c) == 0, NULL );

  ASTNode_free(r);
}
END_TEST


START_TEST (test_SBML_parseFormula_5)
{
  ASTNode_t *r = SBML_parseFormula("1 + 2 * 3");
  ASTNode_t *c;



  fail_unless( r->type                   == AST_PLUS, NULL );
  fail_unless( r->value.ch               == '+', NULL );
  fail_unless( ASTNode_getNumChildren(r) == 2  , NULL );

  c = ASTNode_getLeftChild(r);

  fail_unless( c->type                   == AST_INTEGER, NULL );
  fail_unless( c->value.integer          == 1, NULL );
  fail_unless( ASTNode_getNumChildren(c) == 0, NULL );

  c = ASTNode_getRightChild(r);

  fail_unless( c->type                   == AST_TIMES, NULL );
  fail_unless( c->value.ch               == '*', NULL );
  fail_unless( ASTNode_getNumChildren(c) == 2  , NULL );

  c = ASTNode_getLeftChild(c);

  fail_unless( c->type                   == AST_INTEGER, NULL );
  fail_unless( c->value.integer          == 2, NULL );
  fail_unless( ASTNode_getNumChildren(c) == 0, NULL );

  c = ASTNode_getRightChild( ASTNode_getRightChild(r) );

  fail_unless( c->type                   == AST_INTEGER, NULL );
  fail_unless( c->value.integer          == 3, NULL );
  fail_unless( ASTNode_getNumChildren(c) == 0, NULL );

  ASTNode_free(r);
}
END_TEST


START_TEST (test_SBML_parseFormula_6)
{
  ASTNode_t *r = SBML_parseFormula("(1 - 2) * 3");
  ASTNode_t *c;


  fail_unless( r->type                   == AST_TIMES, NULL );
  fail_unless( r->value.ch               == '*', NULL );
  fail_unless( ASTNode_getNumChildren(r) == 2  , NULL );

  c = ASTNode_getLeftChild(r);

  fail_unless( c->type                   == AST_MINUS, NULL );
  fail_unless( c->value.ch               == '-', NULL );
  fail_unless( ASTNode_getNumChildren(c) == 2, NULL );

  c = ASTNode_getRightChild(r);

  fail_unless( c->type                   == AST_INTEGER, NULL );
  fail_unless( c->value.integer          == 3, NULL );
  fail_unless( ASTNode_getNumChildren(c) == 0, NULL );

  c = ASTNode_getLeftChild( ASTNode_getLeftChild(r) );

  fail_unless( c->type                   == AST_INTEGER, NULL );
  fail_unless( c->value.integer          == 1, NULL );
  fail_unless( ASTNode_getNumChildren(c) == 0, NULL );

  c = ASTNode_getRightChild( ASTNode_getLeftChild(r) );

  fail_unless( c->type                   == AST_INTEGER, NULL );
  fail_unless( c->value.integer          == 2, NULL );
  fail_unless( ASTNode_getNumChildren(c) == 0, NULL );

  ASTNode_free(r);
}
END_TEST


START_TEST (test_SBML_parseFormula_7)
{
  ASTNode_t *r = SBML_parseFormula("1 + -2 / 3");
  ASTNode_t *c;



  fail_unless( r->type                   == AST_PLUS, NULL );
  fail_unless( r->value.ch               == '+', NULL );
  fail_unless( ASTNode_getNumChildren(r) == 2  , NULL );

  c = ASTNode_getLeftChild(r);

  fail_unless( c->type                   == AST_INTEGER, NULL );
  fail_unless( c->value.integer          == 1, NULL );
  fail_unless( ASTNode_getNumChildren(c) == 0, NULL );

  c = ASTNode_getRightChild(r);

  fail_unless( c->type                   == AST_DIVIDE, NULL );
  fail_unless( c->value.ch               == '/', NULL );
  fail_unless( ASTNode_getNumChildren(c) == 2  , NULL );

  c = ASTNode_getLeftChild(c);

  fail_unless( c->type                   == AST_INTEGER, NULL );
  fail_unless( c->value.integer          == -2, NULL );
  fail_unless( ASTNode_getNumChildren(c) ==  0, NULL );

  c = ASTNode_getRightChild( ASTNode_getRightChild(r) );

  fail_unless( c->type                   == AST_INTEGER, NULL );
  fail_unless( c->value.integer          == 3, NULL );
  fail_unless( ASTNode_getNumChildren(c) == 0, NULL );

  ASTNode_free(r);
}
END_TEST


START_TEST (test_SBML_parseFormula_8)
{
  ASTNode_t *r = SBML_parseFormula("1 - -foo / 3");
  ASTNode_t *c;



  fail_unless( r->type                   == AST_MINUS, NULL );
  fail_unless( r->value.ch               == '-', NULL );
  fail_unless( ASTNode_getNumChildren(r) == 2  , NULL );

  c = ASTNode_getLeftChild(r);

  fail_unless( c->type                   == AST_INTEGER, NULL );
  fail_unless( c->value.integer          == 1, NULL );
  fail_unless( ASTNode_getNumChildren(c) == 0, NULL );

  c = ASTNode_getRightChild(r);

  fail_unless( c->type                   == AST_DIVIDE, NULL );
  fail_unless( c->value.ch               == '/', NULL );
  fail_unless( ASTNode_getNumChildren(c) == 2, NULL );

  c = ASTNode_getLeftChild( ASTNode_getRightChild(r) );

  fail_unless( c->type                   == AST_MINUS, NULL );
  fail_unless( c->value.ch               == '-', NULL );
  fail_unless( ASTNode_getNumChildren(c) == 1  , NULL );

  c = ASTNode_getLeftChild( ASTNode_getLeftChild( ASTNode_getRightChild(r) ) );

  fail_unless( c->type == AST_NAME           , NULL );
  fail_unless( !strcmp(c->value.name, "foo") , NULL );
  fail_unless( ASTNode_getNumChildren(c) == 0, NULL );

  c = ASTNode_getRightChild( ASTNode_getRightChild(r) );

  fail_unless( c->type                   == AST_INTEGER, NULL );
  fail_unless( c->value.integer          == 3, NULL );
  fail_unless( ASTNode_getNumChildren(c) == 0, NULL );

  ASTNode_free(r);
}
END_TEST


START_TEST (test_SBML_parseFormula_9)
{
  ASTNode_t *r = SBML_parseFormula("2 * foo^bar + 3.0");
  ASTNode_t *c;



  fail_unless( r->type                   == AST_PLUS, NULL );
  fail_unless( r->value.ch               == '+', NULL );
  fail_unless( ASTNode_getNumChildren(r) == 2  , NULL );

  c = ASTNode_getLeftChild(r);

  fail_unless( c->type                   == AST_TIMES, NULL );
  fail_unless( c->value.ch               == '*', NULL );
  fail_unless( ASTNode_getNumChildren(c) == 2  , NULL );

  c = ASTNode_getRightChild(r);

  fail_unless( c->type                   == AST_REAL, NULL );
  fail_unless( c->value.real             == 3.0, NULL );
  fail_unless( ASTNode_getNumChildren(c) == 0  , NULL );

  c = ASTNode_getLeftChild( ASTNode_getLeftChild(r) );

  fail_unless( c->type                   == AST_INTEGER, NULL );
  fail_unless( c->value.integer          == 2, NULL );
  fail_unless( ASTNode_getNumChildren(c) == 0, NULL );

  c = ASTNode_getRightChild( ASTNode_getLeftChild(r) );

  fail_unless( c->type                   == AST_POWER, NULL );
  fail_unless( c->value.ch               == '^', NULL );
  fail_unless( ASTNode_getNumChildren(c) == 2  , NULL );

  c = ASTNode_getLeftChild( ASTNode_getRightChild( ASTNode_getLeftChild(r) ) );

  fail_unless( c->type == AST_NAME           , NULL );
  fail_unless( !strcmp(c->value.name, "foo") , NULL );
  fail_unless( ASTNode_getNumChildren(c) == 0, NULL );

  c = ASTNode_getRightChild( ASTNode_getRightChild( ASTNode_getLeftChild(r) ) );

  fail_unless( c->type == AST_NAME           , NULL );
  fail_unless( !strcmp(c->value.name, "bar") , NULL );
  fail_unless( ASTNode_getNumChildren(c) == 0, NULL );

  ASTNode_free(r);
}
END_TEST


START_TEST (test_SBML_parseFormula_10)
{
  ASTNode_t *r = SBML_parseFormula("foo()");


  fail_unless( r->type == AST_FUNCTION       , NULL );
  fail_unless( !strcmp(r->value.name, "foo") , NULL );
  fail_unless( ASTNode_getNumChildren(r) == 0, NULL );

  ASTNode_free(r);
}
END_TEST


START_TEST (test_SBML_parseFormula_11)
{
  ASTNode_t *r = SBML_parseFormula("foo(1)");
  ASTNode_t *c;


  fail_unless( r->type == AST_FUNCTION       , NULL );
  fail_unless( !strcmp(r->value.name, "foo") , NULL );
  fail_unless( ASTNode_getNumChildren(r) == 1, NULL );

  c = ASTNode_getLeftChild(r);

  fail_unless( c->type                   == AST_INTEGER, NULL );
  fail_unless( c->value.integer          == 1, NULL );
  fail_unless( ASTNode_getNumChildren(c) == 0, NULL );

  ASTNode_free(r);
}
END_TEST


START_TEST (test_SBML_parseFormula_12)
{
  ASTNode_t *r = SBML_parseFormula("foo(1, bar)");
  ASTNode_t *c;


  fail_unless( r->type == AST_FUNCTION       , NULL );
  fail_unless( !strcmp(r->value.name, "foo") , NULL );
  fail_unless( ASTNode_getNumChildren(r) == 2, NULL );

  c = ASTNode_getLeftChild(r);

  fail_unless( c->type                   == AST_INTEGER, NULL );
  fail_unless( c->value.integer          == 1, NULL );
  fail_unless( ASTNode_getNumChildren(c) == 0, NULL );

  c = ASTNode_getRightChild(r);

  fail_unless( c->type == AST_NAME           , NULL );
  fail_unless( !strcmp(c->value.name, "bar") , NULL );
  fail_unless( ASTNode_getNumChildren(c) == 0, NULL );

  ASTNode_free(r);
}
END_TEST


START_TEST (test_SBML_parseFormula_13)
{
  ASTNode_t *r = SBML_parseFormula("foo(1, bar, 2^-3)");
  ASTNode_t *c;


  fail_unless( r->type == AST_FUNCTION       , NULL );
  fail_unless( !strcmp(r->value.name, "foo") , NULL );
  fail_unless( ASTNode_getNumChildren(r) == 3, NULL );

  c = ASTNode_getChild(r, 0);

  fail_unless( c->type                   == AST_INTEGER, NULL );
  fail_unless( c->value.integer          == 1, NULL );
  fail_unless( ASTNode_getNumChildren(c) == 0, NULL );

  c = ASTNode_getChild(r, 1);

  fail_unless( c->type == AST_NAME           , NULL );
  fail_unless( !strcmp(c->value.name, "bar") , NULL );
  fail_unless( ASTNode_getNumChildren(c) == 0, NULL );

  c = ASTNode_getChild(r, 2);

  fail_unless( c->type                   == AST_POWER, NULL );
  fail_unless( c->value.ch               == '^', NULL );
  fail_unless( ASTNode_getNumChildren(c) == 2  , NULL );

  c = ASTNode_getLeftChild( ASTNode_getChild(r, 2) );

  fail_unless( c->type                   == AST_INTEGER, NULL );
  fail_unless( c->value.integer          == 2, NULL );
  fail_unless( ASTNode_getNumChildren(c) == 0, NULL );

  c = ASTNode_getRightChild( ASTNode_getChild(r, 2) );

  fail_unless( c->type                   == AST_INTEGER, NULL );
  fail_unless( c->value.integer          == -3, NULL );
  fail_unless( ASTNode_getNumChildren(c) == 0 , NULL );

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

  tcase_add_test( tcase, test_SBML_parseFormula_1  );
  tcase_add_test( tcase, test_SBML_parseFormula_2  );
  tcase_add_test( tcase, test_SBML_parseFormula_3  );
  tcase_add_test( tcase, test_SBML_parseFormula_4  );
  tcase_add_test( tcase, test_SBML_parseFormula_5  );
  tcase_add_test( tcase, test_SBML_parseFormula_6  );
  tcase_add_test( tcase, test_SBML_parseFormula_7  );
  tcase_add_test( tcase, test_SBML_parseFormula_8  );
  tcase_add_test( tcase, test_SBML_parseFormula_9  );
  tcase_add_test( tcase, test_SBML_parseFormula_10 );
  tcase_add_test( tcase, test_SBML_parseFormula_11 );
  tcase_add_test( tcase, test_SBML_parseFormula_12 );
  tcase_add_test( tcase, test_SBML_parseFormula_13 );

  suite_add_tcase(suite, tcase);

  return suite;
}
