/**
 * Filename    : TestASTNode.c
 * Description : ASTNode unit tests
 * Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
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
 *     The Systems Biology Markup Language Development Group
 *     ERATO Kitano Symbiotic Systems Project
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
#include "sbml/FormulaParser.h"


START_TEST (test_ASTNode_create)
{
  ASTNode_t *n = ASTNode_create();


  fail_unless( n->type == AST_UNKNOWN, NULL );

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
  const char         *formula = "foo 2 4.0 .272e1 +-*/^@";
  FormulaTokenizer_t *ft      = FormulaTokenizer_create(formula);

  Token_t   *t;
  ASTNode_t *n;


  /** "foo" **/
  t = FormulaTokenizer_nextToken(ft);
  n = ASTNode_createFromToken(t);

  fail_unless( n->type == AST_NAME, NULL );
  fail_unless( !strcmp(n->value.name, "foo") , NULL );
  fail_unless( ASTNode_getNumChildren(n) == 0, NULL );

  Token_free(t);
  ASTNode_free(n);

  /** "2" **/
  t = FormulaTokenizer_nextToken(ft);
  n = ASTNode_createFromToken(t);

  fail_unless( n->type          == AST_INTEGER, NULL );
  fail_unless( n->value.integer == 2          , NULL );
  fail_unless( ASTNode_getNumChildren(n) == 0 , NULL );

  Token_free(t);
  ASTNode_free(n);

  /** "4.0" **/
  t = FormulaTokenizer_nextToken(ft);
  n = ASTNode_createFromToken(t);

  fail_unless( n->type       == AST_REAL     , NULL );
  fail_unless( n->value.real == 4.0          , NULL );
  fail_unless( ASTNode_getNumChildren(n) == 0, NULL );

  Token_free(t);
  ASTNode_free(n);

  /** ".272e1" **/
  t = FormulaTokenizer_nextToken(ft);
  n = ASTNode_createFromToken(t);

  fail_unless( n->type           == AST_REAL_E, NULL );
  fail_unless( n->value.real     == .272      , NULL );
  fail_unless( n->extra.exponent == 1         , NULL );
  fail_unless( ASTNode_getNumChildren(n) == 0 , NULL );

  Token_free(t);
  ASTNode_free(n);

  /** "+" **/
  t = FormulaTokenizer_nextToken(ft);
  n = ASTNode_createFromToken(t);

  fail_unless( n->type     == AST_PLUS       , NULL );
  fail_unless( n->value.ch == '+'            , NULL );
  fail_unless( ASTNode_getNumChildren(n) == 0, NULL );

  Token_free(t);
  ASTNode_free(n);

  /** "-" **/
  t = FormulaTokenizer_nextToken(ft);
  n = ASTNode_createFromToken(t);

  fail_unless( n->type     == AST_MINUS      , NULL );
  fail_unless( n->value.ch == '-'            , NULL );
  fail_unless( ASTNode_getNumChildren(n) == 0, NULL );

  Token_free(t);
  ASTNode_free(n);

  /** "*" **/
  t = FormulaTokenizer_nextToken(ft);
  n = ASTNode_createFromToken(t);

  fail_unless( n->type     == AST_TIMES      , NULL );
  fail_unless( n->value.ch == '*'            , NULL );
  fail_unless( ASTNode_getNumChildren(n) == 0, NULL );

  Token_free(t);
  ASTNode_free(n);

  /** "/" **/
  t = FormulaTokenizer_nextToken(ft);
  n = ASTNode_createFromToken(t);

  fail_unless( n->type     == AST_DIVIDE     , NULL );
  fail_unless( n->value.ch == '/'            , NULL );
  fail_unless( ASTNode_getNumChildren(n) == 0, NULL );

  Token_free(t);
  ASTNode_free(n);

  /** "^" **/
  t = FormulaTokenizer_nextToken(ft);
  n = ASTNode_createFromToken(t);

  fail_unless( n->type     == AST_POWER      , NULL );
  fail_unless( n->value.ch == '^'            , NULL );
  fail_unless( ASTNode_getNumChildren(n) == 0, NULL );

  Token_free(t);
  ASTNode_free(n);

  /** "@" **/
  t = FormulaTokenizer_nextToken(ft);
  n = ASTNode_createFromToken(t);

  fail_unless( n->type     == AST_UNKNOWN    , NULL );
  fail_unless( n->value.ch == '@'            , NULL );
  fail_unless( ASTNode_getNumChildren(n) == 0, NULL );

  Token_free(t);
  ASTNode_free(n);

  FormulaTokenizer_free(ft);
}
END_TEST


START_TEST (test_ASTNode_canonicalizeConstants)
{
  ASTNode_t *n = ASTNode_create();


  /** ExponentialE **/
  ASTNode_setName(n, "ExponentialE");
  fail_unless( ASTNode_isName(n), NULL);

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_CONSTANT_E, NULL );

  ASTNode_setType(n, AST_NAME);


  /** False **/
  ASTNode_setName(n, "False");
  fail_unless( ASTNode_isName(n), NULL);

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_CONSTANT_FALSE, NULL );

  ASTNode_setType(n, AST_NAME);


  /** Pi **/
  ASTNode_setName(n, "Pi");
  fail_unless( ASTNode_isName(n), NULL);

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_CONSTANT_PI, NULL );

  ASTNode_setType(n, AST_NAME);


  /** True **/
  ASTNode_setName(n, "True");
  fail_unless( ASTNode_isName(n), NULL);

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_CONSTANT_TRUE, NULL );

  ASTNode_setType(n, AST_NAME);


  /** Foo **/
  ASTNode_setName(n, "Foo");
  fail_unless( ASTNode_isName(n), NULL);

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_isName(n), NULL);


  ASTNode_free(n);
}
END_TEST


START_TEST (test_ASTNode_canonicalizeFunctions)
{
  ASTNode_t *n = ASTNode_createWithType(AST_FUNCTION);


  /** abs **/
  ASTNode_setName(n, "abs");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION, NULL );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_ABS, NULL );

  ASTNode_setType(n, AST_FUNCTION);


  /** arccos **/
  ASTNode_setName(n, "arccos");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION, NULL );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_ARCCOS, NULL );

  ASTNode_setType(n, AST_FUNCTION);


  /** arccosh **/
  ASTNode_setName(n, "arccosh");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION, NULL );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_ARCCOSH, NULL );

  ASTNode_setType(n, AST_FUNCTION);


  /** arccot **/
  ASTNode_setName(n, "arccot");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION, NULL );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_ARCCOT, NULL );

  ASTNode_setType(n, AST_FUNCTION);


  /** arccoth **/
  ASTNode_setName(n, "arccoth");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION, NULL );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_ARCCOTH, NULL );

  ASTNode_setType(n, AST_FUNCTION);


  /** arccsc **/
  ASTNode_setName(n, "arccsc");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION, NULL );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_ARCCSC, NULL );

  ASTNode_setType(n, AST_FUNCTION);


  /** arccsch **/
  ASTNode_setName(n, "arccsch");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION, NULL );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_ARCCSCH, NULL );

  ASTNode_setType(n, AST_FUNCTION);


  /** arcsec **/
  ASTNode_setName(n, "arcsec");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION, NULL );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_ARCSEC, NULL );

  ASTNode_setType(n, AST_FUNCTION);


  /** arcsech **/
  ASTNode_setName(n, "arcsech");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION, NULL );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_ARCSECH, NULL );

  ASTNode_setType(n, AST_FUNCTION);


  /** arcsin **/
  ASTNode_setName(n, "arcsin");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION, NULL );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_ARCSIN, NULL );

  ASTNode_setType(n, AST_FUNCTION);


  /** arcsinh **/
  ASTNode_setName(n, "arcsinh");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION, NULL );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_ARCSINH, NULL );

  ASTNode_setType(n, AST_FUNCTION);


  /** arctan **/
  ASTNode_setName(n, "arctan");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION, NULL );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_ARCTAN, NULL );

  ASTNode_setType(n, AST_FUNCTION);


  /** arctanh **/
  ASTNode_setName(n, "arctanh");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION, NULL );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_ARCTANH, NULL );

  ASTNode_setType(n, AST_FUNCTION);


  /** ceiling **/
  ASTNode_setName(n, "ceiling");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION, NULL );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_CEILING, NULL );

  ASTNode_setType(n, AST_FUNCTION);


  /** cos **/
  ASTNode_setName(n, "cos");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION, NULL );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_COS, NULL );

  ASTNode_setType(n, AST_FUNCTION);


  /** cosh **/
  ASTNode_setName(n, "cosh");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION, NULL );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_COSH, NULL );

  ASTNode_setType(n, AST_FUNCTION);


  /** cot **/
  ASTNode_setName(n, "cot");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION, NULL );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_COT, NULL );

  ASTNode_setType(n, AST_FUNCTION);


  /** coth **/
  ASTNode_setName(n, "coth");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION, NULL );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_COTH, NULL );

  ASTNode_setType(n, AST_FUNCTION);


  /** csc **/
  ASTNode_setName(n, "csc");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION, NULL );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_CSC, NULL );

  ASTNode_setType(n, AST_FUNCTION);


  /** csch **/
  ASTNode_setName(n, "csch");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION, NULL );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_CSCH, NULL );

  ASTNode_setType(n, AST_FUNCTION);


  /** exp **/
  ASTNode_setName(n, "exp");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION, NULL );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_EXP, NULL );

  ASTNode_setType(n, AST_FUNCTION);


  /** factorial **/
  ASTNode_setName(n, "factorial");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION, NULL );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_FACTORIAL, NULL );

  ASTNode_setType(n, AST_FUNCTION);


  /** floor **/
  ASTNode_setName(n, "floor");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION, NULL );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_FLOOR, NULL );

  ASTNode_setType(n, AST_FUNCTION);


  /** lambda **/
  ASTNode_setName(n, "lambda");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION, NULL );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_LAMBDA, NULL );

  ASTNode_setType(n, AST_FUNCTION);


  /** ln **/
  ASTNode_setName(n, "ln");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION, NULL );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_LN, NULL );

  ASTNode_setType(n, AST_FUNCTION);


  /** log **/
  ASTNode_setName(n, "log");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION, NULL );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_LOG, NULL );

  ASTNode_setType(n, AST_FUNCTION);


  /** piecewise **/
  ASTNode_setName(n, "piecewise");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION, NULL );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_PIECEWISE, NULL );

  ASTNode_setType(n, AST_FUNCTION);


  /** power **/
  ASTNode_setName(n, "power");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION, NULL );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_POWER, NULL );

  ASTNode_setType(n, AST_FUNCTION);


  /** root **/
  ASTNode_setName(n, "root");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION, NULL );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_ROOT, NULL );

  ASTNode_setType(n, AST_FUNCTION);


  /** sec **/
  ASTNode_setName(n, "sec");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION, NULL );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_SEC, NULL );

  ASTNode_setType(n, AST_FUNCTION);


  /** sech **/
  ASTNode_setName(n, "sech");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION, NULL );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_SECH, NULL );

  ASTNode_setType(n, AST_FUNCTION);


  /** sin **/
  ASTNode_setName(n, "sin");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION, NULL );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_SIN, NULL );

  ASTNode_setType(n, AST_FUNCTION);


  /** sinh **/
  ASTNode_setName(n, "sinh");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION, NULL );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_SINH, NULL );

  ASTNode_setType(n, AST_FUNCTION);


  /** tan **/
  ASTNode_setName(n, "tan");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION, NULL );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_TAN, NULL );

  ASTNode_setType(n, AST_FUNCTION);


  /** tanh **/
  ASTNode_setName(n, "tanh");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION, NULL );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_TANH, NULL );

  ASTNode_setType(n, AST_FUNCTION);


  /** Foo **/
  ASTNode_setName(n, "Foo");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION, NULL);

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION, NULL );


  ASTNode_free(n);
}
END_TEST


START_TEST (test_ASTNode_canonicalizeFunctionsL1)
{
  ASTNode_t *n = ASTNode_createWithType(AST_FUNCTION);
  ASTNode_t *c;


  /** acos **/
  ASTNode_setName(n, "acos");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION, NULL );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_ARCCOS, NULL );

  ASTNode_setType(n, AST_FUNCTION);


  /** asin **/
  ASTNode_setName(n, "asin");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION, NULL );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_ARCSIN, NULL );

  ASTNode_setType(n, AST_FUNCTION);


  /** atan **/
  ASTNode_setName(n, "atan");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION, NULL );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_ARCTAN, NULL );

  ASTNode_setType(n, AST_FUNCTION);


  /** ceil **/
  ASTNode_setName(n, "ceil");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION, NULL );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_CEILING, NULL );

  ASTNode_setType(n, AST_FUNCTION);

  /** pow **/
  ASTNode_setName(n, "pow");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION, NULL );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_POWER, NULL );

  ASTNode_free(n);

  /**
   * log(x) and log(x, y)
   *
   * In SBML L1 log(x) (with exactly one argument) canonicalizes to a node
   * of type AST_FUNCTION_LN (see L1 Specification, Appendix C), whereas
   * log(x, y) canonicalizes to a node of type AST_FUNCTION_LOG.
   */
  n = ASTNode_createWithType(AST_FUNCTION);
  ASTNode_setName(n, "log");

  c = ASTNode_create();
  ASTNode_setName (c, "x");
  ASTNode_addChild(n, c);

  fail_unless( ASTNode_getType(n) == AST_FUNCTION, NULL );  
  fail_unless( ASTNode_getNumChildren(n) == 1, NULL );

  ASTNode_canonicalize(n);

  fail_unless( ASTNode_getType(n) == AST_FUNCTION_LN, NULL );  
  fail_unless( ASTNode_getNumChildren(n) == 1, NULL );

  /** log(x, y) (continued) **/
  ASTNode_setType(n, AST_FUNCTION);

  c = ASTNode_create();
  ASTNode_setName (c, "y");
  ASTNode_addChild(n, c);

  fail_unless( ASTNode_getType(n) == AST_FUNCTION, NULL );
  fail_unless( ASTNode_getNumChildren(n) == 2, NULL );

  ASTNode_canonicalize(n);

  fail_unless( ASTNode_getType(n) == AST_FUNCTION_LOG, NULL );

  ASTNode_free(n);


  /** log10(x) -> log(10, x) **/
  n = ASTNode_createWithType(AST_FUNCTION);
  ASTNode_setName(n, "log10");

  c = ASTNode_create();
  ASTNode_setName (c, "x");
  ASTNode_addChild(n, c);

  fail_unless( ASTNode_getType(n) == AST_FUNCTION, NULL );  
  fail_unless( ASTNode_getNumChildren(n) == 1, NULL );

  ASTNode_canonicalize(n);

  fail_unless( ASTNode_getType(n) == AST_FUNCTION_LOG, NULL );  
  fail_unless( ASTNode_getNumChildren(n) == 2, NULL );

  c = ASTNode_getLeftChild(n);
  fail_unless( ASTNode_getType(c)    == AST_INTEGER, NULL );
  fail_unless( ASTNode_getInteger(c) == 10, NULL );

  c = ASTNode_getRightChild(n);
  fail_unless( ASTNode_getType(c) == AST_NAME  , NULL );
  fail_unless( !strcmp(ASTNode_getName(c), "x"), NULL );

  ASTNode_free(n);


  /** sqr(x) -> power(x, 2) **/
  n = ASTNode_createWithType(AST_FUNCTION);
  ASTNode_setName(n, "sqr");

  c = ASTNode_create();
  ASTNode_setName (c, "x");
  ASTNode_addChild(n, c);

  fail_unless( ASTNode_getType(n) == AST_FUNCTION, NULL );  
  fail_unless( ASTNode_getNumChildren(n) == 1, NULL );

  ASTNode_canonicalize(n);

  fail_unless( ASTNode_getType(n) == AST_FUNCTION_POWER, NULL );  
  fail_unless( ASTNode_getNumChildren(n) == 2, NULL );

  c = ASTNode_getLeftChild(n);
  fail_unless( ASTNode_getType(c) == AST_NAME  , NULL );
  fail_unless( !strcmp(ASTNode_getName(c), "x"), NULL );

  c = ASTNode_getRightChild(n);
  fail_unless( ASTNode_getType(c)    == AST_INTEGER, NULL );
  fail_unless( ASTNode_getInteger(c) == 2, NULL );

  ASTNode_free(n);


  /** sqrt(x) -> root(2, x) **/
  n = ASTNode_createWithType(AST_FUNCTION);
  ASTNode_setName(n, "sqrt");

  c = ASTNode_create();
  ASTNode_setName (c, "x");
  ASTNode_addChild(n, c);

  fail_unless( ASTNode_getType(n) == AST_FUNCTION, NULL );  
  fail_unless( ASTNode_getNumChildren(n) == 1, NULL );

  ASTNode_canonicalize(n);

  fail_unless( ASTNode_getType(n) == AST_FUNCTION_ROOT, NULL );  
  fail_unless( ASTNode_getNumChildren(n) == 2, NULL );

  c = ASTNode_getLeftChild(n);
  fail_unless( ASTNode_getType(c)    == AST_INTEGER, NULL );
  fail_unless( ASTNode_getInteger(c) == 2, NULL );

  c = ASTNode_getRightChild(n);
  fail_unless( ASTNode_getType(c) == AST_NAME  , NULL );
  fail_unless( !strcmp(ASTNode_getName(c), "x"), NULL );

  ASTNode_free(n);
}
END_TEST


START_TEST (test_ASTNode_canonicalizeLogical)
{
  ASTNode_t *n = ASTNode_createWithType(AST_FUNCTION);


  /** and **/
  ASTNode_setName(n, "and");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION, NULL );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_LOGICAL_AND, NULL );

  ASTNode_setType(n, AST_FUNCTION);


  /** not **/
  ASTNode_setName(n, "not");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION, NULL );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_LOGICAL_NOT, NULL );

  ASTNode_setType(n, AST_FUNCTION);


  /** or **/
  ASTNode_setName(n, "or");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION, NULL );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_LOGICAL_OR, NULL );

  ASTNode_setType(n, AST_FUNCTION);


  /** xor **/
  ASTNode_setName(n, "xor");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION, NULL );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_LOGICAL_XOR, NULL );

  ASTNode_setType(n, AST_FUNCTION);


  ASTNode_free(n);
}
END_TEST


START_TEST (test_ASTNode_canonicalizeRelational)
{
  ASTNode_t *n = ASTNode_createWithType(AST_FUNCTION);


  /** eq **/
  ASTNode_setName(n, "eq");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION, NULL );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_RELATIONAL_EQ, NULL );

  ASTNode_setType(n, AST_FUNCTION);


  /** geq **/
  ASTNode_setName(n, "geq");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION, NULL );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_RELATIONAL_GEQ, NULL );

  ASTNode_setType(n, AST_FUNCTION);


  /** gt **/
  ASTNode_setName(n, "gt");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION, NULL );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_RELATIONAL_GT, NULL );

  ASTNode_setType(n, AST_FUNCTION);


  /** leq **/
  ASTNode_setName(n, "leq");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION, NULL );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_RELATIONAL_LEQ, NULL );

  ASTNode_setType(n, AST_FUNCTION);


  /** lt **/
  ASTNode_setName(n, "lt");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION, NULL );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_RELATIONAL_LT, NULL );

  ASTNode_setType(n, AST_FUNCTION);


  /** neq **/
  ASTNode_setName(n, "neq");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION, NULL );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_RELATIONAL_NEQ, NULL );

  ASTNode_setType(n, AST_FUNCTION);


  ASTNode_free(n);
}
END_TEST


START_TEST (test_ASTNode_getName)
{
  ASTNode_t *n = ASTNode_create();


  /** AST_NAMEs **/
  ASTNode_setName(n, "foo");
  fail_unless( !strcmp(ASTNode_getName(n), "foo"), NULL );

  ASTNode_setType(n, AST_NAME_DELAY);
  fail_unless( !strcmp(ASTNode_getName(n), "foo"), NULL );

  ASTNode_setType(n, AST_NAME_TIME);
  fail_unless( !strcmp(ASTNode_getName(n), "foo"), NULL );

  ASTNode_setName(n, NULL);
  fail_unless( ASTNode_getName(n) == NULL, NULL );


  /** AST_CONSTANTs **/
  ASTNode_setType(n, AST_CONSTANT_E);
  fail_unless( !strcmp(ASTNode_getName(n), "exponentiale"), NULL );

  ASTNode_setType(n, AST_CONSTANT_FALSE);
  fail_unless( !strcmp(ASTNode_getName(n), "false"), NULL );

  ASTNode_setType(n, AST_CONSTANT_PI);
  fail_unless( !strcmp(ASTNode_getName(n), "pi"), NULL );

  ASTNode_setType(n, AST_CONSTANT_TRUE);
  fail_unless( !strcmp(ASTNode_getName(n), "true"), NULL );


  /** AST_LAMBDA **/
  ASTNode_setType(n, AST_LAMBDA);
  fail_unless( !strcmp(ASTNode_getName(n), "lambda"), NULL );


  /** AST_FUNCTION (user-defined) **/
  ASTNode_setType(n, AST_FUNCTION);
  ASTNode_setName(n, "f");
  fail_unless( !strcmp(ASTNode_getName(n), "f"), NULL );

  ASTNode_setName(n, NULL);
  fail_unless( ASTNode_getName(n) == NULL, NULL );


  /** AST_FUNCTIONs (builtin)  **/
  ASTNode_setType(n, AST_FUNCTION_ABS);
  fail_unless( !strcmp(ASTNode_getName(n), "abs"), NULL );

  ASTNode_setType(n, AST_FUNCTION_ARCCOS);
  fail_unless( !strcmp(ASTNode_getName(n), "arccos"), NULL );

  ASTNode_setType(n, AST_FUNCTION_TAN);
  fail_unless( !strcmp(ASTNode_getName(n), "tan"), NULL );

  ASTNode_setType(n, AST_FUNCTION_TANH);
  fail_unless( !strcmp(ASTNode_getName(n), "tanh"), NULL );


  /** AST_LOGICALs **/
  ASTNode_setType(n, AST_LOGICAL_AND);
  fail_unless( !strcmp(ASTNode_getName(n), "and"), NULL );

  ASTNode_setType(n, AST_LOGICAL_NOT);
  fail_unless( !strcmp(ASTNode_getName(n), "not"), NULL );

  ASTNode_setType(n, AST_LOGICAL_OR);
  fail_unless( !strcmp(ASTNode_getName(n), "or") , NULL );

  ASTNode_setType(n, AST_LOGICAL_XOR);
  fail_unless( !strcmp(ASTNode_getName(n), "xor"), NULL );


  /** AST_RELATIONALs **/
  ASTNode_setType(n, AST_RELATIONAL_EQ);
  fail_unless( !strcmp(ASTNode_getName(n), "eq"), NULL );

  ASTNode_setType(n, AST_RELATIONAL_GEQ);
  fail_unless( !strcmp(ASTNode_getName(n), "geq"), NULL );

  ASTNode_setType(n, AST_RELATIONAL_LT);
  fail_unless( !strcmp(ASTNode_getName(n), "lt"), NULL );

  ASTNode_setType(n, AST_RELATIONAL_NEQ);
  fail_unless( !strcmp(ASTNode_getName(n), "neq"), NULL );

  ASTNode_free(n);
}
END_TEST


START_TEST (test_ASTNode_getReal)
{
  ASTNode_t *n = ASTNode_create();


  /** 2.0 **/
  n->type       = AST_REAL;
  n->value.real = 1.6;

  fail_unless(ASTNode_getReal(n) == 1.6, NULL);

  /** 12.3e3 **/
  n->type           = AST_REAL_E;
  n->value.real     = 12.3;
  n->extra.exponent = 3;

  fail_unless(ASTNode_getReal(n) == 12300.0, NULL);

  /** 1/2 **/
  n->type              = AST_RATIONAL;
  n->value.integer     = 1;
  n->extra.denominator = 2;

  fail_unless(ASTNode_getReal(n) == 0.5, NULL);

  ASTNode_free(n);
}
END_TEST


START_TEST (test_ASTNode_getPrecedence)
{
  ASTNode_t *n = ASTNode_create();


  ASTNode_setType(n, AST_PLUS);
  fail_unless( ASTNode_getPrecedence(n) == 2, NULL );

  ASTNode_setType(n, AST_MINUS);
  fail_unless( ASTNode_getPrecedence(n) == 2, NULL );

  ASTNode_setType(n, AST_TIMES);
  fail_unless( ASTNode_getPrecedence(n) == 3, NULL );

  ASTNode_setType(n, AST_DIVIDE);
  fail_unless( ASTNode_getPrecedence(n) == 3, NULL );

  ASTNode_setType(n, AST_POWER);
  fail_unless( ASTNode_getPrecedence(n) == 4, NULL );

  ASTNode_setType (n, AST_MINUS);
  ASTNode_addChild(n, ASTNode_createWithType(AST_NAME));
  fail_unless( ASTNode_isUMinus(n)      == 1, NULL );
  fail_unless( ASTNode_getPrecedence(n) == 5, NULL );

  ASTNode_setType(n, AST_NAME);
  fail_unless( ASTNode_getPrecedence(n) == 6, NULL );

  ASTNode_setType(n, AST_FUNCTION);
  fail_unless( ASTNode_getPrecedence(n) == 6, NULL );

  ASTNode_free(n);
}
END_TEST


START_TEST (test_ASTNode_isLog10)
{
  ASTNode_t *n = ASTNode_create();
  ASTNode_t *c;


  ASTNode_setType(n, AST_FUNCTION);
  fail_unless( ASTNode_isLog10(n) == 0, NULL );

  /** log() **/
  ASTNode_setType(n, AST_FUNCTION_LOG);
  fail_unless( ASTNode_isLog10(n) == 0, NULL );

  /** log(10) **/
  c = ASTNode_create();
  ASTNode_addChild(n, c);

  ASTNode_setInteger(c, 10);
  fail_unless( ASTNode_isLog10(n) == 0, NULL );

  /** log(10, x) -> ASTNode_isLog10() == 1 **/
  ASTNode_addChild(n, ASTNode_create());
  fail_unless( ASTNode_isLog10(n) == 1, NULL );

  /** log(2, x) **/
  ASTNode_setInteger(c, 2);
  fail_unless( ASTNode_isLog10(n) == 0, NULL );

  ASTNode_free(n);
}
END_TEST


START_TEST (test_ASTNode_isSqrt)
{
  ASTNode_t *n = ASTNode_create();
  ASTNode_t *c;


  ASTNode_setType(n, AST_FUNCTION);
  fail_unless( ASTNode_isSqrt(n) == 0, NULL );

  /** root() **/
  ASTNode_setType(n, AST_FUNCTION_ROOT);
  fail_unless( ASTNode_isSqrt(n) == 0, NULL );

  /** root(2) **/
  c = ASTNode_create();
  ASTNode_addChild(n, c);

  ASTNode_setInteger(c, 2);
  fail_unless( ASTNode_isSqrt(n) == 0, NULL );

  /** root(2, x) -> ASTNode_isSqrt() == 1 **/
  ASTNode_addChild(n, ASTNode_create());
  fail_unless( ASTNode_isSqrt(n) == 1, NULL );

  /** root(3, x) **/
  ASTNode_setInteger(c, 3);
  fail_unless( ASTNode_isSqrt(n) == 0, NULL );

  ASTNode_free(n);
}
END_TEST


START_TEST (test_ASTNode_isUMinus)
{
  ASTNode_t *n = ASTNode_create();


  ASTNode_setType(n, AST_MINUS);
  fail_unless( ASTNode_isUMinus(n) == 0, NULL );

  ASTNode_addChild(n, ASTNode_createWithType(AST_NAME));
  fail_unless( ASTNode_isUMinus(n) == 1, NULL );

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
  fail_unless( node->type     == AST_UNKNOWN, NULL );
  fail_unless( node->value.ch == '$'        , NULL );

  ASTNode_free(node);
}
END_TEST


START_TEST (test_ASTNode_setName)
{
  const char *name = "foo";
  ASTNode_t  *node = ASTNode_create();


  fail_unless( node->type == AST_UNKNOWN, NULL );

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


START_TEST (test_ASTNode_setName_override)
{
  ASTNode_t  *node = ASTNode_createWithType(AST_FUNCTION_SIN);


  fail_unless( !strcmp(ASTNode_getName(node), "sin")    , NULL );
  fail_unless( ASTNode_getType(node) == AST_FUNCTION_SIN, NULL );

  ASTNode_setName(node, "MySinFunc");

  fail_unless( !strcmp(ASTNode_getName(node), "MySinFunc"), NULL );
  fail_unless( ASTNode_getType(node) == AST_FUNCTION_SIN  , NULL );

  ASTNode_setName(node, NULL);

  fail_unless( !strcmp(ASTNode_getName(node), "sin")    , NULL );
  fail_unless( ASTNode_getType(node) == AST_FUNCTION_SIN, NULL );

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

  ASTNode_setType(node, AST_UNKNOWN);
  fail_unless( node->type == AST_UNKNOWN, NULL );

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


START_TEST (test_ASTNode_getListOfNodes)
{
  const char *gaussian =
  (
    "(1 / (sigma * sqrt(2 * pi))) * exp( -(x - mu)^2 / (2 * sigma^2) )"
  );

  ASTNode_t *root, *node;
  List_t    *list;


  root = SBML_parseFormula(gaussian);
  list = ASTNode_getListOfNodes(root, (ASTNodePredicate) ASTNode_isName);

  fail_unless( List_size(list) == 4, NULL );


  node = (ASTNode_t *) List_get(list, 0);

  fail_unless( ASTNode_isName(node), NULL );
  fail_unless( !strcmp(ASTNode_getName(node), "sigma"), NULL );

  node = (ASTNode_t *) List_get(list, 1);

  fail_unless( ASTNode_isName(node), NULL );
  fail_unless( !strcmp(ASTNode_getName(node), "x"), NULL );

  node = (ASTNode_t *) List_get(list, 2);

  fail_unless( ASTNode_isName(node), NULL );
  fail_unless( !strcmp(ASTNode_getName(node), "mu"), NULL );

  node = (ASTNode_t *) List_get(list, 3);

  fail_unless( ASTNode_isName(node), NULL );
  fail_unless( !strcmp(ASTNode_getName(node), "sigma"), NULL );

  List_free(list);
  ASTNode_free(root);
}
END_TEST


Suite *
create_suite_ASTNode (void) 
{ 
  Suite *suite = suite_create("ASTNode");
  TCase *tcase = tcase_create("ASTNode");


  tcase_add_test( tcase, test_ASTNode_create                  );
  tcase_add_test( tcase, test_ASTNode_free_NULL               );
  tcase_add_test( tcase, test_ASTNode_createFromToken         );
  tcase_add_test( tcase, test_ASTNode_canonicalizeConstants   );
  tcase_add_test( tcase, test_ASTNode_canonicalizeFunctions   );
  tcase_add_test( tcase, test_ASTNode_canonicalizeFunctionsL1 );
  tcase_add_test( tcase, test_ASTNode_canonicalizeLogical     );
  tcase_add_test( tcase, test_ASTNode_canonicalizeRelational  );
  tcase_add_test( tcase, test_ASTNode_getName                 );
  tcase_add_test( tcase, test_ASTNode_getReal                 );
  tcase_add_test( tcase, test_ASTNode_getPrecedence           );
  tcase_add_test( tcase, test_ASTNode_isLog10                 );
  tcase_add_test( tcase, test_ASTNode_isSqrt                  );
  tcase_add_test( tcase, test_ASTNode_isUMinus                );
  tcase_add_test( tcase, test_ASTNode_setCharacter            );
  tcase_add_test( tcase, test_ASTNode_setName                 );
  tcase_add_test( tcase, test_ASTNode_setName_override        );
  tcase_add_test( tcase, test_ASTNode_setInteger              );
  tcase_add_test( tcase, test_ASTNode_setReal                 );
  tcase_add_test( tcase, test_ASTNode_setType                 );
  tcase_add_test( tcase, test_ASTNode_no_children             );
  tcase_add_test( tcase, test_ASTNode_one_child               );
  tcase_add_test( tcase, test_ASTNode_children                );
  tcase_add_test( tcase, test_ASTNode_getListOfNodes          );

  suite_add_tcase(suite, tcase);

  return suite;
}
