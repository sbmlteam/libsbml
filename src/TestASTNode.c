/**
 * Filename    : TestASTNode.c
 * Description : ASTNode unit tests
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


#include "sbml/ASTNode.h"


START_TEST (test_ASTNode_create)
{
  ASTNode_t *n = ASTNode_create();


  fail_unless( n->type == AST_TRANSIENT, NULL );

  fail_unless( n->value.ch      == '\0', NULL );
  fail_unless( n->value.name    == NULL, NULL );
  fail_unless( n->value.integer == 0   , NULL );
  fail_unless( n->value.real    == 0.0 , NULL );

  fail_unless( ASTNode_getNumChildren(n) == 0, NULL );

  ASTNode_free(n);
}
END_TEST


START_TEST (test_ASTNode_free_NULL)
{
  ASTNode_free(NULL);
}
END_TEST


START_TEST (test_ASTNode_createFromToken)
{
  const char         *formula = "foo 2 4.0 +-*/^@";
  FormulaTokenizer_t *ft      = FormulaTokenizer_create(formula);

  Token_t   *t;
  ASTNode_t *n;


  t = FormulaTokenizer_nextToken(ft);
  n = ASTNode_createFromToken(t);

  fail_unless( n->type == AST_NAME, NULL );
  fail_unless( !strcmp(n->value.name, "foo") , NULL );
  fail_unless( ASTNode_getNumChildren(n) == 0, NULL );

  Token_free(t);
  ASTNode_free(n);


  t = FormulaTokenizer_nextToken(ft);
  n = ASTNode_createFromToken(t);

  fail_unless( n->type          == AST_INTEGER, NULL );
  fail_unless( n->value.integer == 2          , NULL );
  fail_unless( ASTNode_getNumChildren(n) == 0 , NULL );

  Token_free(t);
  ASTNode_free(n);


  t = FormulaTokenizer_nextToken(ft);
  n = ASTNode_createFromToken(t);

  fail_unless( n->type       == AST_REAL     , NULL );
  fail_unless( n->value.real == 4.0          , NULL );
  fail_unless( ASTNode_getNumChildren(n) == 0, NULL );

  Token_free(t);
  ASTNode_free(n);


  t = FormulaTokenizer_nextToken(ft);
  n = ASTNode_createFromToken(t);

  fail_unless( n->type     == AST_PLUS       , NULL );
  fail_unless( n->value.ch == '+'            , NULL );
  fail_unless( ASTNode_getNumChildren(n) == 0, NULL );

  Token_free(t);
  ASTNode_free(n);


  t = FormulaTokenizer_nextToken(ft);
  n = ASTNode_createFromToken(t);

  fail_unless( n->type     == AST_MINUS      , NULL );
  fail_unless( n->value.ch == '-'            , NULL );
  fail_unless( ASTNode_getNumChildren(n) == 0, NULL );

  Token_free(t);
  ASTNode_free(n);


  t = FormulaTokenizer_nextToken(ft);
  n = ASTNode_createFromToken(t);

  fail_unless( n->type     == AST_TIMES      , NULL );
  fail_unless( n->value.ch == '*'            , NULL );
  fail_unless( ASTNode_getNumChildren(n) == 0, NULL );

  Token_free(t);
  ASTNode_free(n);


  t = FormulaTokenizer_nextToken(ft);
  n = ASTNode_createFromToken(t);

  fail_unless( n->type     == AST_DIVIDE     , NULL );
  fail_unless( n->value.ch == '/'            , NULL );
  fail_unless( ASTNode_getNumChildren(n) == 0, NULL );

  Token_free(t);
  ASTNode_free(n);


  t = FormulaTokenizer_nextToken(ft);
  n = ASTNode_createFromToken(t);

  fail_unless( n->type     == AST_POWER      , NULL );
  fail_unless( n->value.ch == '^'            , NULL );
  fail_unless( ASTNode_getNumChildren(n) == 0, NULL );

  Token_free(t);
  ASTNode_free(n);


  t = FormulaTokenizer_nextToken(ft);
  n = ASTNode_createFromToken(t);

  fail_unless( n->type     == AST_TRANSIENT  , NULL );
  fail_unless( n->value.ch == '@'            , NULL );
  fail_unless( ASTNode_getNumChildren(n) == 0, NULL );

  FormulaTokenizer_free(ft);
  Token_free(t);
  ASTNode_free(n);
}
END_TEST


START_TEST (test_ASTNode_setCharacter)
{
  ASTNode_t *node = ASTNode_create();


  /**
   * Ensure "foo" is cleared in subsequent sets.
   */
  ASTNode_setName(node, "foo");
  fail_unless( node->type == AST_NAME, NULL );

  ASTNode_setCharacter(node, '+');
  fail_unless( node->type     == AST_PLUS, NULL );
  fail_unless( node->value.ch == '+'     , NULL );

  ASTNode_setCharacter(node, '-');
  fail_unless( node->type     == AST_MINUS, NULL );
  fail_unless( node->value.ch == '-'      , NULL );

  ASTNode_setCharacter(node, '*');
  fail_unless( node->type     == AST_TIMES, NULL );
  fail_unless( node->value.ch == '*'      , NULL );

  ASTNode_setCharacter(node, '/');
  fail_unless( node->type     == AST_DIVIDE, NULL );
  fail_unless( node->value.ch == '/'       , NULL );

  ASTNode_setCharacter(node, '^');
  fail_unless( node->type     == AST_POWER, NULL );
  fail_unless( node->value.ch == '^'      , NULL );

  ASTNode_setCharacter(node, '$');
  fail_unless( node->type     == AST_TRANSIENT, NULL );
  fail_unless( node->value.ch == '$'          , NULL );

  ASTNode_free(node);
}
END_TEST


START_TEST (test_ASTNode_setName)
{
  const char *name = "foo";
  ASTNode_t  *node = ASTNode_create();


  fail_unless( node->type == AST_TRANSIENT, NULL );

  ASTNode_setName(node, name);

  fail_unless( node->type == AST_NAME, NULL );
  fail_unless( !strcmp(node->value.name, name), NULL );

  if (node->value.name == name)
  {
    fail("ASTNode_setName(...) did not make a copy of name.");
  }

  ASTNode_setName(node, NULL);
  fail_unless( node->type == AST_NAME, NULL );

  if (node->value.name != NULL)
  {
    fail("ASTNode_setName(node, NULL) did not clear string.");
  }

  ASTNode_free(node);
}
END_TEST


START_TEST (test_ASTNode_setInteger)
{
  ASTNode_t *node = ASTNode_create();


  /**
   * Ensure "foo" is cleared in subsequent sets.
   */
  ASTNode_setName(node, "foo");
  fail_unless( node->type == AST_NAME, NULL );

  ASTNode_setInteger(node, 321);
  fail_unless( node->type          == AST_INTEGER, NULL );
  fail_unless( node->value.integer == 321        , NULL );

  ASTNode_free(node);
}
END_TEST


START_TEST (test_ASTNode_setReal)
{
  ASTNode_t *node = ASTNode_create();


  /**
   * Ensure "foo" is cleared in subsequent sets.
   */
  ASTNode_setName(node, "foo");
  fail_unless( node->type == AST_NAME, NULL );

  ASTNode_setReal(node, 32.1);
  fail_unless( node->type       == AST_REAL, NULL );
  fail_unless( node->value.real == 32.1    , NULL );

  ASTNode_free(node);
}
END_TEST



START_TEST (test_ASTNode_setType)
{
  ASTNode_t *node = ASTNode_create();


  /**
   * Ensure "foo" is cleared in subsequent sets.
   */
  ASTNode_setName(node, "foo");
  fail_unless( node->type == AST_NAME, NULL );

  /**
   * node->value.name should not to cleared or changed as we toggle from
   * AST_FUNCTION to and from AST_NAME.
   */
  ASTNode_setType(node, AST_FUNCTION);
  fail_unless( node->type == AST_FUNCTION, NULL );
  fail_unless( !strcmp(node->value.name, "foo"), NULL );

  ASTNode_setType(node, AST_NAME);
  fail_unless( node->type == AST_NAME, NULL );
  fail_unless( !strcmp(node->value.name, "foo"), NULL );

  /**
   * But now it should...
   */
  ASTNode_setType(node, AST_INTEGER);
  fail_unless( node->type == AST_INTEGER, NULL );

  ASTNode_setType(node, AST_REAL);
  fail_unless( node->type == AST_REAL, NULL );

  ASTNode_setType(node, AST_TRANSIENT);
  fail_unless( node->type == AST_TRANSIENT, NULL );

  /**
   * Setting these types should also set node->value.ch
   */
  ASTNode_setType(node, AST_PLUS);
  fail_unless( node->type     == AST_PLUS, NULL );
  fail_unless( node->value.ch == '+'     , NULL );

  ASTNode_setType(node, AST_MINUS);
  fail_unless( node->type     == AST_MINUS, NULL );
  fail_unless( node->value.ch == '-'      , NULL );

  ASTNode_setType(node, AST_TIMES);
  fail_unless( node->type     == AST_TIMES, NULL );
  fail_unless( node->value.ch == '*'      , NULL );

  ASTNode_setType(node, AST_DIVIDE);
  fail_unless( node->type     == AST_DIVIDE, NULL );
  fail_unless( node->value.ch == '/'       , NULL );

  ASTNode_setType(node, AST_POWER);
  fail_unless( node->type     == AST_POWER, NULL );
  fail_unless( node->value.ch == '^'      , NULL );

  ASTNode_free(node);
}
END_TEST


START_TEST (test_ASTNode_no_children)
{
  ASTNode_t *node = ASTNode_create();


  fail_unless( ASTNode_getNumChildren(node) == 0, NULL );

  fail_unless( ASTNode_getLeftChild (node) == NULL, NULL );
  fail_unless( ASTNode_getRightChild(node) == NULL, NULL );

  fail_unless( ASTNode_getChild(node, 0) == NULL, NULL );

  ASTNode_free(node);
}
END_TEST


START_TEST (test_ASTNode_one_child)
{
  ASTNode_t *node  = ASTNode_create();
  ASTNode_t *child = ASTNode_create();


  ASTNode_addChild(node, child);

  fail_unless( ASTNode_getNumChildren(node) == 1, NULL );

  fail_unless( ASTNode_getLeftChild (node) == child, NULL );
  fail_unless( ASTNode_getRightChild(node) == NULL , NULL );

  fail_unless( ASTNode_getChild(node, 0) == child, NULL );
  fail_unless( ASTNode_getChild(node, 1) == NULL , NULL );

  ASTNode_free(node);
}
END_TEST


START_TEST (test_ASTNode_children)
{
  ASTNode_t *parent = ASTNode_create();
  ASTNode_t *left   = ASTNode_create();
  ASTNode_t *right  = ASTNode_create();
  ASTNode_t *right2 = ASTNode_create();


  ASTNode_setType(parent, AST_PLUS);
  ASTNode_setInteger(left  , 1);
  ASTNode_setInteger(right , 2);
  ASTNode_setInteger(right2, 3);

  /**
   * Two Children
   */
  ASTNode_addChild( parent, left  );
  ASTNode_addChild( parent, right );

  fail_unless( ASTNode_getNumChildren(parent) == 2, NULL );
  fail_unless( ASTNode_getNumChildren(left)   == 0, NULL );
  fail_unless( ASTNode_getNumChildren(right)  == 0, NULL );

  fail_unless( ASTNode_getLeftChild (parent) == left , NULL );
  fail_unless( ASTNode_getRightChild(parent) == right, NULL );

  fail_unless( ASTNode_getChild(parent, 0) == left , NULL );
  fail_unless( ASTNode_getChild(parent, 1) == right, NULL );
  fail_unless( ASTNode_getChild(parent, 2) == NULL , NULL );

  /**
   * Three Children
   */
  ASTNode_addChild(parent, right2);

  fail_unless( ASTNode_getNumChildren(parent) == 3, NULL );
  fail_unless( ASTNode_getNumChildren(left)   == 0, NULL );
  fail_unless( ASTNode_getNumChildren(right)  == 0, NULL );
  fail_unless( ASTNode_getNumChildren(right2) == 0, NULL );

  fail_unless( ASTNode_getLeftChild (parent) == left  , NULL );
  fail_unless( ASTNode_getRightChild(parent) == right2, NULL );

  fail_unless( ASTNode_getChild(parent, 0) == left  , NULL );
  fail_unless( ASTNode_getChild(parent, 1) == right , NULL );
  fail_unless( ASTNode_getChild(parent, 2) == right2, NULL );
  fail_unless( ASTNode_getChild(parent, 3) == NULL  , NULL );

  ASTNode_free(parent);
}
END_TEST


Suite *
create_suite_ASTNode (void) 
{ 
  Suite *suite = suite_create("ASTNode");
  TCase *tcase = tcase_create("ASTNode");
 

  tcase_add_test( tcase, test_ASTNode_create          );
  tcase_add_test( tcase, test_ASTNode_free_NULL       );
  tcase_add_test( tcase, test_ASTNode_createFromToken );
  tcase_add_test( tcase, test_ASTNode_setCharacter    );
  tcase_add_test( tcase, test_ASTNode_setName         );
  tcase_add_test( tcase, test_ASTNode_setInteger      );
  tcase_add_test( tcase, test_ASTNode_setReal         );
  tcase_add_test( tcase, test_ASTNode_setType         );
  tcase_add_test( tcase, test_ASTNode_no_children     );
  tcase_add_test( tcase, test_ASTNode_one_child       );
  tcase_add_test( tcase, test_ASTNode_children        );

  suite_add_tcase(suite, tcase);

  return suite;
}
