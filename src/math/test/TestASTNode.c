/**
 * \file    TestASTNode.c
 * \brief   ASTNode unit tests
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

#include <sbml/common/common.h>
#include <sbml/util/List.h>

#include <sbml/math/ASTNode.h>
#include <sbml/math/FormulaParser.h>


START_TEST (test_ASTNode_create)
{
  ASTNode_t *n = ASTNode_create();


  fail_unless( ASTNode_getType(n) == AST_UNKNOWN );

  fail_unless( ASTNode_getCharacter(n) == '\0' );
  fail_unless( ASTNode_getName     (n) == NULL );
  fail_unless( ASTNode_getInteger  (n) == 0    );
  fail_unless( ASTNode_getExponent (n) == 0    );

  fail_unless( ASTNode_getNumChildren(n) == 0 );

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
  FormulaTokenizer_t *ft      = FormulaTokenizer_createFromFormula(formula);

  Token_t   *t;
  ASTNode_t *n;


  /** "foo" **/
  t = FormulaTokenizer_nextToken(ft);
  n = ASTNode_createFromToken(t);

  fail_unless( ASTNode_getType(n) == AST_NAME     );
  fail_unless( !strcmp(ASTNode_getName(n), "foo") );
  fail_unless( ASTNode_getNumChildren(n) == 0     );

  Token_free(t);
  ASTNode_free(n);

  /** "2" **/
  t = FormulaTokenizer_nextToken(ft);
  n = ASTNode_createFromToken(t);

  fail_unless( ASTNode_getType       (n) == AST_INTEGER );
  fail_unless( ASTNode_getInteger    (n) == 2 );
  fail_unless( ASTNode_getNumChildren(n) == 0 );

  Token_free(t);
  ASTNode_free(n);

  /** "4.0" **/
  t = FormulaTokenizer_nextToken(ft);
  n = ASTNode_createFromToken(t);

  fail_unless( ASTNode_getType       (n) == AST_REAL );
  fail_unless( ASTNode_getReal       (n) == 4.0 );
  fail_unless( ASTNode_getNumChildren(n) == 0   );

  Token_free(t);
  ASTNode_free(n);

  /** ".272e1" **/
  t = FormulaTokenizer_nextToken(ft);
  n = ASTNode_createFromToken(t);

  fail_unless( ASTNode_getType       (n) == AST_REAL_E );
  fail_unless( ASTNode_getMantissa   (n) == .272 );
  fail_unless( ASTNode_getExponent   (n) == 1    );
  fail_unless( ASTNode_getNumChildren(n) == 0    );

  Token_free(t);
  ASTNode_free(n);

  /** "+" **/
  t = FormulaTokenizer_nextToken(ft);
  n = ASTNode_createFromToken(t);

  fail_unless( ASTNode_getType       (n) == AST_PLUS );
  fail_unless( ASTNode_getCharacter  (n) == '+' );
  fail_unless( ASTNode_getNumChildren(n) == 0   );

  Token_free(t);
  ASTNode_free(n);

  /** "-" **/
  t = FormulaTokenizer_nextToken(ft);
  n = ASTNode_createFromToken(t);

  fail_unless( ASTNode_getType       (n) == AST_MINUS );
  fail_unless( ASTNode_getCharacter  (n) == '-' );
  fail_unless( ASTNode_getNumChildren(n) == 0   );

  Token_free(t);
  ASTNode_free(n);

  /** "*" **/
  t = FormulaTokenizer_nextToken(ft);
  n = ASTNode_createFromToken(t);

  fail_unless( ASTNode_getType       (n) == AST_TIMES );
  fail_unless( ASTNode_getCharacter  (n) == '*' );
  fail_unless( ASTNode_getNumChildren(n) == 0   );

  Token_free(t);
  ASTNode_free(n);

  /** "/" **/
  t = FormulaTokenizer_nextToken(ft);
  n = ASTNode_createFromToken(t);

  fail_unless( ASTNode_getType       (n) == AST_DIVIDE );
  fail_unless( ASTNode_getCharacter  (n) == '/' );
  fail_unless( ASTNode_getNumChildren(n) == 0   );

  Token_free(t);
  ASTNode_free(n);

  /** "^" **/
  t = FormulaTokenizer_nextToken(ft);
  n = ASTNode_createFromToken(t);

  fail_unless( ASTNode_getType       (n) == AST_POWER );
  fail_unless( ASTNode_getCharacter  (n) == '^' );
  fail_unless( ASTNode_getNumChildren(n) == 0   );

  Token_free(t);
  ASTNode_free(n);

  /** "@" **/
  t = FormulaTokenizer_nextToken(ft);
  n = ASTNode_createFromToken(t);

  fail_unless( ASTNode_getType       (n) == AST_UNKNOWN );
  fail_unless( ASTNode_getCharacter  (n) == '@' );
  fail_unless( ASTNode_getNumChildren(n) == 0   );

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
  fail_unless( ASTNode_isName(n));

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_CONSTANT_E );

  ASTNode_setType(n, AST_NAME);


  /** False **/
  ASTNode_setName(n, "False");
  fail_unless( ASTNode_isName(n));

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_CONSTANT_FALSE );

  ASTNode_setType(n, AST_NAME);


  /** Pi **/
  ASTNode_setName(n, "Pi");
  fail_unless( ASTNode_isName(n));

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_CONSTANT_PI );

  ASTNode_setType(n, AST_NAME);


  /** True **/
  ASTNode_setName(n, "True");
  fail_unless( ASTNode_isName(n));

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_CONSTANT_TRUE );

  ASTNode_setType(n, AST_NAME);


  /** Foo **/
  ASTNode_setName(n, "Foo");
  fail_unless( ASTNode_isName(n));

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_isName(n));


  ASTNode_free(n);
}
END_TEST


START_TEST (test_ASTNode_canonicalizeFunctions)
{
  ASTNode_t *n = ASTNode_createWithType(AST_FUNCTION);


  /** abs **/
  ASTNode_setName(n, "abs");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_ABS );

  ASTNode_setType(n, AST_FUNCTION);


  /** arccos **/
  ASTNode_setName(n, "arccos");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_ARCCOS );

  ASTNode_setType(n, AST_FUNCTION);


  /** arccosh **/
  ASTNode_setName(n, "arccosh");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_ARCCOSH );

  ASTNode_setType(n, AST_FUNCTION);


  /** arccot **/
  ASTNode_setName(n, "arccot");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_ARCCOT );

  ASTNode_setType(n, AST_FUNCTION);


  /** arccoth **/
  ASTNode_setName(n, "arccoth");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_ARCCOTH );

  ASTNode_setType(n, AST_FUNCTION);


  /** arccsc **/
  ASTNode_setName(n, "arccsc");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_ARCCSC );

  ASTNode_setType(n, AST_FUNCTION);


  /** arccsch **/
  ASTNode_setName(n, "arccsch");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_ARCCSCH );

  ASTNode_setType(n, AST_FUNCTION);


  /** arcsec **/
  ASTNode_setName(n, "arcsec");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_ARCSEC );

  ASTNode_setType(n, AST_FUNCTION);


  /** arcsech **/
  ASTNode_setName(n, "arcsech");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_ARCSECH );

  ASTNode_setType(n, AST_FUNCTION);


  /** arcsin **/
  ASTNode_setName(n, "arcsin");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_ARCSIN );

  ASTNode_setType(n, AST_FUNCTION);


  /** arcsinh **/
  ASTNode_setName(n, "arcsinh");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_ARCSINH );

  ASTNode_setType(n, AST_FUNCTION);


  /** arctan **/
  ASTNode_setName(n, "arctan");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_ARCTAN );

  ASTNode_setType(n, AST_FUNCTION);


  /** arctanh **/
  ASTNode_setName(n, "arctanh");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_ARCTANH );

  ASTNode_setType(n, AST_FUNCTION);


  /** ceiling **/
  ASTNode_setName(n, "ceiling");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_CEILING );

  ASTNode_setType(n, AST_FUNCTION);


  /** cos **/
  ASTNode_setName(n, "cos");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_COS );

  ASTNode_setType(n, AST_FUNCTION);


  /** cosh **/
  ASTNode_setName(n, "cosh");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_COSH );

  ASTNode_setType(n, AST_FUNCTION);


  /** cot **/
  ASTNode_setName(n, "cot");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_COT );

  ASTNode_setType(n, AST_FUNCTION);


  /** coth **/
  ASTNode_setName(n, "coth");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_COTH );

  ASTNode_setType(n, AST_FUNCTION);


  /** csc **/
  ASTNode_setName(n, "csc");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_CSC );

  ASTNode_setType(n, AST_FUNCTION);


  /** csch **/
  ASTNode_setName(n, "csch");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_CSCH );

  ASTNode_setType(n, AST_FUNCTION);


  /** exp **/
  ASTNode_setName(n, "exp");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_EXP );

  ASTNode_setType(n, AST_FUNCTION);


  /** factorial **/
  ASTNode_setName(n, "factorial");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_FACTORIAL );

  ASTNode_setType(n, AST_FUNCTION);


  /** floor **/
  ASTNode_setName(n, "floor");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_FLOOR );

  ASTNode_setType(n, AST_FUNCTION);


  /** lambda **/
  ASTNode_setName(n, "lambda");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_LAMBDA );

  ASTNode_setType(n, AST_FUNCTION);


  /** ln **/
  ASTNode_setName(n, "ln");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_LN );

  ASTNode_setType(n, AST_FUNCTION);


  /** log **/
  ASTNode_setName(n, "log");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_LOG );

  ASTNode_setType(n, AST_FUNCTION);


  /** piecewise **/
  ASTNode_setName(n, "piecewise");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_PIECEWISE );

  ASTNode_setType(n, AST_FUNCTION);


  /** power **/
  ASTNode_setName(n, "power");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_POWER );

  ASTNode_setType(n, AST_FUNCTION);


  /** root **/
  ASTNode_setName(n, "root");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_ROOT );

  ASTNode_setType(n, AST_FUNCTION);


  /** sec **/
  ASTNode_setName(n, "sec");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_SEC );

  ASTNode_setType(n, AST_FUNCTION);


  /** sech **/
  ASTNode_setName(n, "sech");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_SECH );

  ASTNode_setType(n, AST_FUNCTION);


  /** sin **/
  ASTNode_setName(n, "sin");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_SIN );

  ASTNode_setType(n, AST_FUNCTION);


  /** sinh **/
  ASTNode_setName(n, "sinh");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_SINH );

  ASTNode_setType(n, AST_FUNCTION);


  /** tan **/
  ASTNode_setName(n, "tan");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_TAN );

  ASTNode_setType(n, AST_FUNCTION);


  /** tanh **/
  ASTNode_setName(n, "tanh");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_TANH );

  ASTNode_setType(n, AST_FUNCTION);


  /** Foo **/
  ASTNode_setName(n, "Foo");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION);

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION );


  ASTNode_free(n);
}
END_TEST


START_TEST (test_ASTNode_canonicalizeFunctionsL1)
{
  ASTNode_t *n = ASTNode_createWithType(AST_FUNCTION);
  ASTNode_t *c;


  /** acos **/
  ASTNode_setName(n, "acos");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_ARCCOS );

  ASTNode_setType(n, AST_FUNCTION);


  /** asin **/
  ASTNode_setName(n, "asin");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_ARCSIN );

  ASTNode_setType(n, AST_FUNCTION);


  /** atan **/
  ASTNode_setName(n, "atan");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_ARCTAN );

  ASTNode_setType(n, AST_FUNCTION);


  /** ceil **/
  ASTNode_setName(n, "ceil");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_CEILING );

  ASTNode_setType(n, AST_FUNCTION);

  /** pow **/
  ASTNode_setName(n, "pow");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_FUNCTION_POWER );

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

  fail_unless( ASTNode_getType(n) == AST_FUNCTION );  
  fail_unless( ASTNode_getNumChildren(n) == 1 );

  ASTNode_canonicalize(n);

  fail_unless( ASTNode_getType(n) == AST_FUNCTION_LN );  
  fail_unless( ASTNode_getNumChildren(n) == 1 );

  /** log(x, y) (continued) **/
  ASTNode_setType(n, AST_FUNCTION);

  c = ASTNode_create();
  ASTNode_setName (c, "y");
  ASTNode_addChild(n, c);

  fail_unless( ASTNode_getType(n) == AST_FUNCTION );
  fail_unless( ASTNode_getNumChildren(n) == 2 );

  ASTNode_canonicalize(n);

  fail_unless( ASTNode_getType(n) == AST_FUNCTION_LOG );

  ASTNode_free(n);


  /** log10(x) -> log(10, x) **/
  n = ASTNode_createWithType(AST_FUNCTION);
  ASTNode_setName(n, "log10");

  c = ASTNode_create();
  ASTNode_setName (c, "x");
  ASTNode_addChild(n, c);

  fail_unless( ASTNode_getType(n) == AST_FUNCTION );  
  fail_unless( ASTNode_getNumChildren(n) == 1 );

  ASTNode_canonicalize(n);

  fail_unless( ASTNode_getType(n) == AST_FUNCTION_LOG );  
  fail_unless( ASTNode_getNumChildren(n) == 2 );

  c = ASTNode_getLeftChild(n);
  fail_unless( ASTNode_getType(c)    == AST_INTEGER );
  fail_unless( ASTNode_getInteger(c) == 10 );

  c = ASTNode_getRightChild(n);
  fail_unless( ASTNode_getType(c) == AST_NAME   );
  fail_unless( !strcmp(ASTNode_getName(c), "x") );

  ASTNode_free(n);


  /** sqr(x) -> power(x, 2) **/
  n = ASTNode_createWithType(AST_FUNCTION);
  ASTNode_setName(n, "sqr");

  c = ASTNode_create();
  ASTNode_setName (c, "x");
  ASTNode_addChild(n, c);

  fail_unless( ASTNode_getType(n) == AST_FUNCTION );  
  fail_unless( ASTNode_getNumChildren(n) == 1 );

  ASTNode_canonicalize(n);

  fail_unless( ASTNode_getType(n) == AST_FUNCTION_POWER );  
  fail_unless( ASTNode_getNumChildren(n) == 2 );

  c = ASTNode_getLeftChild(n);
  fail_unless( ASTNode_getType(c) == AST_NAME   );
  fail_unless( !strcmp(ASTNode_getName(c), "x") );

  c = ASTNode_getRightChild(n);
  fail_unless( ASTNode_getType(c)    == AST_INTEGER );
  fail_unless( ASTNode_getInteger(c) == 2 );

  ASTNode_free(n);


  /** sqrt(x) -> root(2, x) **/
  n = ASTNode_createWithType(AST_FUNCTION);
  ASTNode_setName(n, "sqrt");

  c = ASTNode_create();
  ASTNode_setName (c, "x");
  ASTNode_addChild(n, c);

  fail_unless( ASTNode_getType(n) == AST_FUNCTION );  
  fail_unless( ASTNode_getNumChildren(n) == 1 );

  ASTNode_canonicalize(n);

  fail_unless( ASTNode_getType(n) == AST_FUNCTION_ROOT );  
  fail_unless( ASTNode_getNumChildren(n) == 2 );

  c = ASTNode_getLeftChild(n);
  fail_unless( ASTNode_getType(c)    == AST_INTEGER );
  fail_unless( ASTNode_getInteger(c) == 2 );

  c = ASTNode_getRightChild(n);
  fail_unless( ASTNode_getType(c) == AST_NAME   );
  fail_unless( !strcmp(ASTNode_getName(c), "x") );

  ASTNode_free(n);
}
END_TEST


START_TEST (test_ASTNode_canonicalizeLogical)
{
  ASTNode_t *n = ASTNode_createWithType(AST_FUNCTION);


  /** and **/
  ASTNode_setName(n, "and");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_LOGICAL_AND );

  ASTNode_setType(n, AST_FUNCTION);


  /** not **/
  ASTNode_setName(n, "not");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_LOGICAL_NOT );

  ASTNode_setType(n, AST_FUNCTION);


  /** or **/
  ASTNode_setName(n, "or");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_LOGICAL_OR );

  ASTNode_setType(n, AST_FUNCTION);


  /** xor **/
  ASTNode_setName(n, "xor");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_LOGICAL_XOR );

  ASTNode_setType(n, AST_FUNCTION);


  ASTNode_free(n);
}
END_TEST


START_TEST (test_ASTNode_canonicalizeRelational)
{
  ASTNode_t *n = ASTNode_createWithType(AST_FUNCTION);


  /** eq **/
  ASTNode_setName(n, "eq");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_RELATIONAL_EQ );

  ASTNode_setType(n, AST_FUNCTION);


  /** geq **/
  ASTNode_setName(n, "geq");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_RELATIONAL_GEQ );

  ASTNode_setType(n, AST_FUNCTION);


  /** gt **/
  ASTNode_setName(n, "gt");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_RELATIONAL_GT );

  ASTNode_setType(n, AST_FUNCTION);


  /** leq **/
  ASTNode_setName(n, "leq");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_RELATIONAL_LEQ );

  ASTNode_setType(n, AST_FUNCTION);


  /** lt **/
  ASTNode_setName(n, "lt");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_RELATIONAL_LT );

  ASTNode_setType(n, AST_FUNCTION);


  /** neq **/
  ASTNode_setName(n, "neq");
  fail_unless( ASTNode_getType(n) == AST_FUNCTION );

  ASTNode_canonicalize(n);
  fail_unless( ASTNode_getType(n) == AST_RELATIONAL_NEQ );

  ASTNode_setType(n, AST_FUNCTION);


  ASTNode_free(n);
}
END_TEST


START_TEST (test_ASTNode_deepCopy_1)
{
  ASTNode_t *node = ASTNode_create();
  ASTNode_t *child, *copy;


  /** 1 + 2 **/
  ASTNode_setCharacter(node, '+');
  ASTNode_addChild( node, ASTNode_create() );
  ASTNode_addChild( node, ASTNode_create() );

  ASTNode_setInteger( ASTNode_getLeftChild (node), 1 );
  ASTNode_setInteger( ASTNode_getRightChild(node), 2 );

  fail_unless( ASTNode_getType       (node) == AST_PLUS );
  fail_unless( ASTNode_getCharacter  (node) == '+'      );
  fail_unless( ASTNode_getNumChildren(node) == 2        );

  child = ASTNode_getLeftChild(node);

  fail_unless( ASTNode_getType       (child) == AST_INTEGER );
  fail_unless( ASTNode_getInteger    (child) == 1           );
  fail_unless( ASTNode_getNumChildren(child) == 0           );

  child = ASTNode_getRightChild(node);

  fail_unless( ASTNode_getType       (child) == AST_INTEGER );
  fail_unless( ASTNode_getInteger    (child) == 2           );
  fail_unless( ASTNode_getNumChildren(child) == 0           );

  /** deepCopy() **/
  copy = ASTNode_deepCopy(node);

  fail_unless( copy != node );
  fail_unless( ASTNode_getType       (copy) == AST_PLUS );
  fail_unless( ASTNode_getCharacter  (copy) == '+'      );
  fail_unless( ASTNode_getNumChildren(copy) == 2        );

  child = ASTNode_getLeftChild(copy);

  fail_unless( child != ASTNode_getLeftChild(node) );
  fail_unless( ASTNode_getType       (child) == AST_INTEGER );
  fail_unless( ASTNode_getInteger    (child) == 1           );
  fail_unless( ASTNode_getNumChildren(child) == 0           );

  child = ASTNode_getRightChild(copy);
  fail_unless( child != ASTNode_getRightChild(node) );
  fail_unless( ASTNode_getType       (child) == AST_INTEGER );
  fail_unless( ASTNode_getInteger    (child) == 2           );
  fail_unless( ASTNode_getNumChildren(child) == 0           );

  ASTNode_free(node);
  ASTNode_free(copy);
}
END_TEST


START_TEST (test_ASTNode_deepCopy_2)
{
  ASTNode_t *node = ASTNode_create();
  ASTNode_t *copy;


  ASTNode_setName(node, "Foo");

  fail_unless( ASTNode_getType(node) == AST_NAME     );
  fail_unless( !strcmp(ASTNode_getName(node), "Foo") );
  fail_unless( ASTNode_getNumChildren(node) == 0     );

  /** deepCopy() **/
  copy = ASTNode_deepCopy(node);

  fail_unless( copy != node );
  fail_unless( ASTNode_getType(copy) == AST_NAME     );
  fail_unless( !strcmp(ASTNode_getName(copy), "Foo") );
  fail_unless( ASTNode_getNumChildren(copy) == 0     );

  fail_unless( ASTNode_getName(copy) != ASTNode_getName(node) );

  ASTNode_free(node);
  ASTNode_free(copy);
}
END_TEST


START_TEST (test_ASTNode_deepCopy_3)
{
  ASTNode_t *node = ASTNode_createWithType(AST_FUNCTION);
  ASTNode_t *copy;


  ASTNode_setName(node, "Foo");
  fail_unless( ASTNode_getType(node) == AST_FUNCTION );
  fail_unless( !strcmp(ASTNode_getName(node), "Foo") );
  fail_unless( ASTNode_getNumChildren(node) == 0     );

  /** deepCopy() **/
  copy = ASTNode_deepCopy(node);

  fail_unless( copy != node );
  fail_unless( ASTNode_getType(copy) == AST_FUNCTION );
  fail_unless( !strcmp(ASTNode_getName(copy), "Foo") );
  fail_unless( ASTNode_getNumChildren(copy) == 0     );

  fail_unless( ASTNode_getName(copy) != ASTNode_getName(node) );

  ASTNode_free(node);
  ASTNode_free(copy);
}
END_TEST


START_TEST (test_ASTNode_deepCopy_4)
{
  ASTNode_t *node = ASTNode_createWithType(AST_FUNCTION_ABS);
  ASTNode_t *copy;


  ASTNode_setName(node, "ABS");
  fail_unless( ASTNode_getType(node) == AST_FUNCTION_ABS );
  fail_unless( !strcmp(ASTNode_getName(node), "ABS")     );
  fail_unless( ASTNode_getNumChildren(node) == 0         );

  /** deepCopy() **/
  copy = ASTNode_deepCopy(node);

  fail_unless( copy != node );
  fail_unless( ASTNode_getType(copy) == AST_FUNCTION_ABS );
  fail_unless( !strcmp(ASTNode_getName(copy), "ABS")     );
  fail_unless( ASTNode_getNumChildren(copy) == 0         );

  fail_unless( ASTNode_getName(copy) != ASTNode_getName(node) );

  ASTNode_free(node);
  ASTNode_free(copy);
}
END_TEST


START_TEST (test_ASTNode_getName)
{
  ASTNode_t *n = ASTNode_create();


  /** AST_NAMEs **/
  ASTNode_setName(n, "foo");
  fail_unless( !strcmp(ASTNode_getName(n), "foo") );

  ASTNode_setType(n, AST_NAME_TIME);
  fail_unless( !strcmp(ASTNode_getName(n), "foo") );

  ASTNode_setName(n, NULL);
  fail_unless( ASTNode_getName(n) == NULL );


  /** AST_CONSTANTs **/
  ASTNode_setType(n, AST_CONSTANT_E);
  fail_unless( !strcmp(ASTNode_getName(n), "exponentiale") );

  ASTNode_setType(n, AST_CONSTANT_FALSE);
  fail_unless( !strcmp(ASTNode_getName(n), "false") );

  ASTNode_setType(n, AST_CONSTANT_PI);
  fail_unless( !strcmp(ASTNode_getName(n), "pi") );

  ASTNode_setType(n, AST_CONSTANT_TRUE);
  fail_unless( !strcmp(ASTNode_getName(n), "true") );


  /** AST_LAMBDA **/
  ASTNode_setType(n, AST_LAMBDA);
  fail_unless( !strcmp(ASTNode_getName(n), "lambda") );


  /** AST_FUNCTION (user-defined) **/
  ASTNode_setType(n, AST_FUNCTION);
  ASTNode_setName(n, "f");
  fail_unless( !strcmp(ASTNode_getName(n), "f") );

  ASTNode_setType(n, AST_FUNCTION_DELAY);
  fail_unless( !strcmp(ASTNode_getName(n), "f") );

  ASTNode_setName(n, NULL);
  fail_unless( !strcmp(ASTNode_getName(n), "delay") );

  ASTNode_setType(n, AST_FUNCTION);
  fail_unless( ASTNode_getName(n) == NULL );


  /** AST_FUNCTIONs (builtin)  **/
  ASTNode_setType(n, AST_FUNCTION_ABS);
  fail_unless( !strcmp(ASTNode_getName(n), "abs") );

  ASTNode_setType(n, AST_FUNCTION_ARCCOS);
  fail_unless( !strcmp(ASTNode_getName(n), "arccos") );

  ASTNode_setType(n, AST_FUNCTION_TAN);
  fail_unless( !strcmp(ASTNode_getName(n), "tan") );

  ASTNode_setType(n, AST_FUNCTION_TANH);
  fail_unless( !strcmp(ASTNode_getName(n), "tanh") );


  /** AST_LOGICALs **/
  ASTNode_setType(n, AST_LOGICAL_AND);
  fail_unless( !strcmp(ASTNode_getName(n), "and") );

  ASTNode_setType(n, AST_LOGICAL_NOT);
  fail_unless( !strcmp(ASTNode_getName(n), "not") );

  ASTNode_setType(n, AST_LOGICAL_OR);
  fail_unless( !strcmp(ASTNode_getName(n), "or")  );

  ASTNode_setType(n, AST_LOGICAL_XOR);
  fail_unless( !strcmp(ASTNode_getName(n), "xor") );


  /** AST_RELATIONALs **/
  ASTNode_setType(n, AST_RELATIONAL_EQ);
  fail_unless( !strcmp(ASTNode_getName(n), "eq") );

  ASTNode_setType(n, AST_RELATIONAL_GEQ);
  fail_unless( !strcmp(ASTNode_getName(n), "geq") );

  ASTNode_setType(n, AST_RELATIONAL_LT);
  fail_unless( !strcmp(ASTNode_getName(n), "lt") );

  ASTNode_setType(n, AST_RELATIONAL_NEQ);
  fail_unless( !strcmp(ASTNode_getName(n), "neq") );

  ASTNode_free(n);
}
END_TEST


START_TEST (test_ASTNode_getReal)
{
  ASTNode_t *n = ASTNode_create();


  /** 2.0 **/
  ASTNode_setType(n, AST_REAL);
  ASTNode_setReal(n, 1.6);

  fail_unless(ASTNode_getReal(n) == 1.6);

  /** 12.3e3 **/
  ASTNode_setType(n, AST_REAL_E);
  ASTNode_setRealWithExponent(n, 12.3, 3.0);

  fail_unless(abs(ASTNode_getReal(n) - 12300.0) < DBL_EPSILON);

  /** 1/2 **/
  ASTNode_setType(n, AST_RATIONAL);
  ASTNode_setRational(n, 1, 2);

  fail_unless(ASTNode_getReal(n) == 0.5);

  ASTNode_free(n);
}
END_TEST


START_TEST (test_ASTNode_getPrecedence)
{
  ASTNode_t *n = ASTNode_create();


  ASTNode_setType(n, AST_PLUS);
  fail_unless( ASTNode_getPrecedence(n) == 2 );

  ASTNode_setType(n, AST_MINUS);
  fail_unless( ASTNode_getPrecedence(n) == 2 );

  ASTNode_setType(n, AST_TIMES);
  fail_unless( ASTNode_getPrecedence(n) == 3 );

  ASTNode_setType(n, AST_DIVIDE);
  fail_unless( ASTNode_getPrecedence(n) == 3 );

  ASTNode_setType(n, AST_POWER);
  fail_unless( ASTNode_getPrecedence(n) == 4 );

  ASTNode_setType (n, AST_MINUS);
  ASTNode_addChild(n, ASTNode_createWithType(AST_NAME));
  fail_unless( ASTNode_isUMinus(n)      == 1 );
  fail_unless( ASTNode_getPrecedence(n) == 5 );

  ASTNode_setType(n, AST_NAME);
  fail_unless( ASTNode_getPrecedence(n) == 6 );

  ASTNode_setType(n, AST_FUNCTION);
  fail_unless( ASTNode_getPrecedence(n) == 6 );

  ASTNode_free(n);
}
END_TEST


START_TEST (test_ASTNode_isLog10)
{
  ASTNode_t *n = ASTNode_create();
  ASTNode_t *c;


  ASTNode_setType(n, AST_FUNCTION);
  fail_unless( ASTNode_isLog10(n) == 0 );

  /** log() **/
  ASTNode_setType(n, AST_FUNCTION_LOG);
  fail_unless( ASTNode_isLog10(n) == 0 );

  /** log(10) **/
  c = ASTNode_create();
  ASTNode_addChild(n, c);

  ASTNode_setInteger(c, 10);
  fail_unless( ASTNode_isLog10(n) == 0 );

  /** log(10, x) -> ASTNode_isLog10() == 1 **/
  ASTNode_addChild(n, ASTNode_create());
  fail_unless( ASTNode_isLog10(n) == 1 );

  /** log(2, x) **/
  ASTNode_setInteger(c, 2);
  fail_unless( ASTNode_isLog10(n) == 0 );

  ASTNode_free(n);
}
END_TEST


START_TEST (test_ASTNode_isSqrt)
{
  ASTNode_t *n = ASTNode_create();
  ASTNode_t *c;


  ASTNode_setType(n, AST_FUNCTION);
  fail_unless( ASTNode_isSqrt(n) == 0 );

  /** root() **/
  ASTNode_setType(n, AST_FUNCTION_ROOT);
  fail_unless( ASTNode_isSqrt(n) == 0 );

  /** root(2) **/
  c = ASTNode_create();
  ASTNode_addChild(n, c);

  ASTNode_setInteger(c, 2);
  fail_unless( ASTNode_isSqrt(n) == 0 );

  /** root(2, x) -> ASTNode_isSqrt() == 1 **/
  ASTNode_addChild(n, ASTNode_create());
  fail_unless( ASTNode_isSqrt(n) == 1 );

  /** root(3, x) **/
  ASTNode_setInteger(c, 3);
  fail_unless( ASTNode_isSqrt(n) == 0 );

  ASTNode_free(n);
}
END_TEST


START_TEST (test_ASTNode_isUMinus)
{
  ASTNode_t *n = ASTNode_create();


  ASTNode_setType(n, AST_MINUS);
  fail_unless( ASTNode_isUMinus(n) == 0 );

  ASTNode_addChild(n, ASTNode_createWithType(AST_NAME));
  fail_unless( ASTNode_isUMinus(n) == 1 );

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
  fail_unless( ASTNode_getType(node)      == AST_NAME );
  fail_unless( ASTNode_getCharacter(node) == 0        );
  fail_unless( !strcmp(ASTNode_getName(node), "foo")  );
  fail_unless( ASTNode_getInteger(node)   == 0        );
  fail_unless( ASTNode_getReal(node)      == 0        );
  fail_unless( ASTNode_getExponent(node)  == 0        );
  fail_unless( ASTNode_getDenominator(node) == 1      );

  ASTNode_setCharacter(node, '+');
  fail_unless( ASTNode_getType     (node) == AST_PLUS );
  fail_unless( ASTNode_getCharacter(node) == '+'      );
  fail_unless( ASTNode_getName(node)      == NULL     );
  fail_unless( ASTNode_getInteger(node)   == 0        );
  fail_unless( ASTNode_getReal(node)      == 0        );
  fail_unless( ASTNode_getExponent(node)  == 0        );
  fail_unless( ASTNode_getDenominator(node) == 1      );

  ASTNode_setCharacter(node, '-');
  fail_unless( ASTNode_getType     (node) == AST_MINUS );
  fail_unless( ASTNode_getCharacter(node) == '-'       );
  fail_unless( ASTNode_getName(node)      == NULL     );
  fail_unless( ASTNode_getInteger(node)   == 0        );
  fail_unless( ASTNode_getReal(node)      == 0        );
  fail_unless( ASTNode_getExponent(node)  == 0        );
  fail_unless( ASTNode_getDenominator(node) == 1      );

  ASTNode_setCharacter(node, '*');
  fail_unless( ASTNode_getType     (node) == AST_TIMES );
  fail_unless( ASTNode_getCharacter(node) == '*'       );
  fail_unless( ASTNode_getName(node)      == NULL     );
  fail_unless( ASTNode_getInteger(node)   == 0        );
  fail_unless( ASTNode_getReal(node)      == 0        );
  fail_unless( ASTNode_getExponent(node)  == 0        );
  fail_unless( ASTNode_getDenominator(node) == 1      );

  ASTNode_setCharacter(node, '/');
  fail_unless( ASTNode_getType     (node) == AST_DIVIDE );
  fail_unless( ASTNode_getCharacter(node) == '/'        );
  fail_unless( ASTNode_getName(node)      == NULL     );
  fail_unless( ASTNode_getInteger(node)   == 0        );
  fail_unless( ASTNode_getReal(node)      == 0        );
  fail_unless( ASTNode_getExponent(node)  == 0        );
  fail_unless( ASTNode_getDenominator(node) == 1      );

  ASTNode_setCharacter(node, '^');
  fail_unless( ASTNode_getType     (node) == AST_POWER );
  fail_unless( ASTNode_getCharacter(node) == '^'       );
  fail_unless( ASTNode_getName(node)      == NULL     );
  fail_unless( ASTNode_getInteger(node)   == 0        );
  fail_unless( ASTNode_getReal(node)      == 0        );
  fail_unless( ASTNode_getExponent(node)  == 0        );
  fail_unless( ASTNode_getDenominator(node) == 1      );

  ASTNode_setCharacter(node, '$');
  fail_unless( ASTNode_getType     (node) == AST_UNKNOWN );
  fail_unless( ASTNode_getCharacter(node) == '$'         );
  fail_unless( ASTNode_getName(node)      == NULL     );
  fail_unless( ASTNode_getInteger(node)   == 0        );
  fail_unless( ASTNode_getReal(node)      == 0        );
  fail_unless( ASTNode_getExponent(node)  == 0        );
  fail_unless( ASTNode_getDenominator(node) == 1      );

  ASTNode_free(node);
}
END_TEST


START_TEST (test_ASTNode_setName)
{
  const char *name = "foo";
  ASTNode_t  *node = ASTNode_create();


  fail_unless( ASTNode_getType(node) == AST_UNKNOWN );

  ASTNode_setName(node, name);

  fail_unless( ASTNode_getType(node) == AST_NAME );
  fail_unless( !strcmp(ASTNode_getName(node), name) );
  fail_unless( ASTNode_getCharacter(node) == 0        );
  fail_unless( ASTNode_getInteger(node)   == 0        );
  fail_unless( ASTNode_getReal(node)      == 0        );
  fail_unless( ASTNode_getExponent(node)  == 0        );
  fail_unless( ASTNode_getDenominator(node) == 1      );

  if (ASTNode_getName(node) == name)
  {
    fail("ASTNode_setName(...) did not make a copy of name.");
  }

  ASTNode_setName(node, NULL);
  fail_unless( ASTNode_getType(node) == AST_NAME );

  if (ASTNode_getName(node) != NULL)
  {
    fail("ASTNode_setName(node, NULL) did not clear string.");
  }

  ASTNode_setType(node, AST_FUNCTION_COS);
  fail_unless( ASTNode_getType(node) == AST_FUNCTION_COS );
  fail_unless( !strcmp(ASTNode_getName(node), "cos") );
  fail_unless( ASTNode_getCharacter(node) == 0        );
  fail_unless( ASTNode_getInteger(node)   == 0        );
  fail_unless( ASTNode_getReal(node)      == 0        );
  fail_unless( ASTNode_getExponent(node)  == 0        );
  fail_unless( ASTNode_getDenominator(node) == 1      );

  ASTNode_setType(node, AST_PLUS);
  ASTNode_setName(node, name);
  fail_unless( ASTNode_getType(node) == AST_NAME );
  fail_unless( !strcmp(ASTNode_getName(node), name) );
  fail_unless( ASTNode_getCharacter(node) == '+'        );
  fail_unless( ASTNode_getInteger(node)   == 0        );
  fail_unless( ASTNode_getReal(node)      == 0        );
  fail_unless( ASTNode_getExponent(node)  == 0        );
  fail_unless( ASTNode_getDenominator(node) == 1      );

  ASTNode_free(node);
}
END_TEST


START_TEST (test_ASTNode_setName_override)
{
  ASTNode_t  *node = ASTNode_createWithType(AST_FUNCTION_SIN);


  fail_unless( !strcmp(ASTNode_getName(node), "sin")     );
  fail_unless( ASTNode_getType(node) == AST_FUNCTION_SIN );

  ASTNode_setName(node, "MySinFunc");

  fail_unless( !strcmp(ASTNode_getName(node), "MySinFunc") );
  fail_unless( ASTNode_getType(node) == AST_FUNCTION_SIN   );

  ASTNode_setName(node, NULL);

  fail_unless( !strcmp(ASTNode_getName(node), "sin")     );
  fail_unless( ASTNode_getType(node) == AST_FUNCTION_SIN );

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
  fail_unless( ASTNode_getType(node) == AST_NAME );
  fail_unless( !strcmp(ASTNode_getName(node), "foo") );
  fail_unless( ASTNode_getCharacter(node) == 0        );
  fail_unless( ASTNode_getInteger(node)   == 0        );
  fail_unless( ASTNode_getReal(node)      == 0        );
  fail_unless( ASTNode_getExponent(node)  == 0        );
  fail_unless( ASTNode_getDenominator(node) == 1      );

  ASTNode_setReal(node, 3.2);
  fail_unless( ASTNode_getType   (node) == AST_REAL );
  fail_unless( ASTNode_getInteger(node) == 0         );
  fail_unless( ASTNode_getName(node)== NULL );
  fail_unless( ASTNode_getCharacter(node) == 0        );
  fail_unless( ASTNode_getReal(node)      == 3.2        );
  fail_unless( ASTNode_getExponent(node)  == 0        );
  fail_unless( ASTNode_getDenominator(node) == 1      );

  ASTNode_setInteger(node, 321);
  fail_unless( ASTNode_getType   (node) == AST_INTEGER );
  fail_unless( ASTNode_getInteger(node) == 321         );
  fail_unless( ASTNode_getName(node)== NULL );
  fail_unless( ASTNode_getCharacter(node) == 0        );
  fail_unless( ASTNode_getReal(node)      == 0        );
  fail_unless( ASTNode_getExponent(node)  == 0        );
  fail_unless( ASTNode_getDenominator(node) == 1      );

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
  fail_unless( ASTNode_getType(node) == AST_NAME );

  ASTNode_setReal(node, 32.1);
  fail_unless( ASTNode_getType(node) == AST_REAL );
  fail_unless( ASTNode_getInteger(node) == 0         );
  fail_unless( ASTNode_getName(node)== NULL );
  fail_unless( ASTNode_getCharacter(node) == 0        );
  fail_unless( ASTNode_getReal(node)      == 32.1        );
  fail_unless( ASTNode_getExponent(node)  == 0        );
  fail_unless( ASTNode_getDenominator(node) == 1      );
  fail_unless( ASTNode_getMantissa(node) == 32.1     );

  ASTNode_setRational(node, 45, 90);
  fail_unless( ASTNode_getType(node) == AST_RATIONAL );
  fail_unless( ASTNode_getInteger(node) == 45         );
  fail_unless( ASTNode_getName(node)== NULL );
  fail_unless( ASTNode_getCharacter(node) == 0        );
  fail_unless( ASTNode_getReal(node)      == 0.5        );
  fail_unless( ASTNode_getExponent(node)  == 0        );
  fail_unless( ASTNode_getDenominator(node) == 90      );
  fail_unless( ASTNode_getMantissa(node) == 0     );

  ASTNode_setRealWithExponent(node, 32, 4);
  fail_unless( ASTNode_getType(node) == AST_REAL_E );
  fail_unless( ASTNode_getInteger(node) == 0         );
  fail_unless( ASTNode_getName(node)== NULL );
  fail_unless( ASTNode_getCharacter(node) == 0        );
  fail_unless( ASTNode_getReal(node)      == 320000        );
  fail_unless( ASTNode_getExponent(node)  == 4        );
  fail_unless( ASTNode_getDenominator(node) == 1      );
  fail_unless( ASTNode_getMantissa(node) == 32     );

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
  fail_unless( ASTNode_getType(node) == AST_NAME );

  /**
   * node->value.name should not to cleared or changed as we toggle from
   * AST_FUNCTION to and from AST_NAME.
   */
  ASTNode_setType(node, AST_FUNCTION);
  fail_unless( ASTNode_getType(node) == AST_FUNCTION );
  fail_unless( !strcmp(ASTNode_getName(node), "foo") );

  ASTNode_setType(node, AST_NAME);
  fail_unless( ASTNode_getType(node) == AST_NAME );
  fail_unless( !strcmp(ASTNode_getName(node), "foo") );

  /**
   * But now it should...
   */
  ASTNode_setType(node, AST_INTEGER);
  fail_unless( ASTNode_getType(node) == AST_INTEGER );

  ASTNode_setType(node, AST_REAL);
  fail_unless( ASTNode_getType(node) == AST_REAL );

  ASTNode_setType(node, AST_UNKNOWN);
  fail_unless( ASTNode_getType(node) == AST_UNKNOWN );

  /**
   * Setting these types should also set node->value.ch
   */
  ASTNode_setType(node, AST_PLUS);
  fail_unless( ASTNode_getType     (node) == AST_PLUS );
  fail_unless( ASTNode_getCharacter(node) == '+'      );

  ASTNode_setType(node, AST_MINUS);
  fail_unless( ASTNode_getType     (node) == AST_MINUS );
  fail_unless( ASTNode_getCharacter(node) == '-'       );

  ASTNode_setType(node, AST_TIMES);
  fail_unless( ASTNode_getType     (node) == AST_TIMES );
  fail_unless( ASTNode_getCharacter(node) == '*'       );

  ASTNode_setType(node, AST_DIVIDE);
  fail_unless( ASTNode_getType     (node) == AST_DIVIDE );
  fail_unless( ASTNode_getCharacter(node) == '/'        );

  ASTNode_setType(node, AST_POWER);
  fail_unless( ASTNode_getType     (node) == AST_POWER );
  fail_unless( ASTNode_getCharacter(node) == '^'       );

  ASTNode_free(node);
}
END_TEST


START_TEST (test_ASTNode_no_children)
{
  ASTNode_t *node = ASTNode_create();


  fail_unless( ASTNode_getNumChildren(node) == 0 );

  fail_unless( ASTNode_getLeftChild (node) == NULL );
  fail_unless( ASTNode_getRightChild(node) == NULL );

  fail_unless( ASTNode_getChild(node, 0) == NULL );

  ASTNode_free(node);
}
END_TEST


START_TEST (test_ASTNode_one_child)
{
  ASTNode_t *node  = ASTNode_create();
  ASTNode_t *child = ASTNode_create();


  ASTNode_addChild(node, child);

  fail_unless( ASTNode_getNumChildren(node) == 1 );

  fail_unless( ASTNode_getLeftChild (node) == child );
  fail_unless( ASTNode_getRightChild(node) == NULL  );

  fail_unless( ASTNode_getChild(node, 0) == child );
  fail_unless( ASTNode_getChild(node, 1) == NULL  );

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

  fail_unless( ASTNode_getNumChildren(parent) == 2 );
  fail_unless( ASTNode_getNumChildren(left)   == 0 );
  fail_unless( ASTNode_getNumChildren(right)  == 0 );

  fail_unless( ASTNode_getLeftChild (parent) == left  );
  fail_unless( ASTNode_getRightChild(parent) == right );

  fail_unless( ASTNode_getChild(parent, 0) == left  );
  fail_unless( ASTNode_getChild(parent, 1) == right );
  fail_unless( ASTNode_getChild(parent, 2) == NULL  );

  /**
   * Three Children
   */
  ASTNode_addChild(parent, right2);

  fail_unless( ASTNode_getNumChildren(parent) == 3 );
  fail_unless( ASTNode_getNumChildren(left)   == 0 );
  fail_unless( ASTNode_getNumChildren(right)  == 0 );
  fail_unless( ASTNode_getNumChildren(right2) == 0 );

  fail_unless( ASTNode_getLeftChild (parent) == left   );
  fail_unless( ASTNode_getRightChild(parent) == right2 );

  fail_unless( ASTNode_getChild(parent, 0) == left   );
  fail_unless( ASTNode_getChild(parent, 1) == right  );
  fail_unless( ASTNode_getChild(parent, 2) == right2 );
  fail_unless( ASTNode_getChild(parent, 3) == NULL   );

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

  fail_unless( List_size(list) == 4 );


  node = (ASTNode_t *) List_get(list, 0);

  fail_unless( ASTNode_isName(node) );
  fail_unless( !strcmp(ASTNode_getName(node), "sigma") );

  node = (ASTNode_t *) List_get(list, 1);

  fail_unless( ASTNode_isName(node) );
  fail_unless( !strcmp(ASTNode_getName(node), "x") );

  node = (ASTNode_t *) List_get(list, 2);

  fail_unless( ASTNode_isName(node) );
  fail_unless( !strcmp(ASTNode_getName(node), "mu") );

  node = (ASTNode_t *) List_get(list, 3);

  fail_unless( ASTNode_isName(node) );
  fail_unless( !strcmp(ASTNode_getName(node), "sigma") );

  List_free(list);
  ASTNode_free(root);
}
END_TEST


START_TEST (test_ASTNode_replaceArgument)
{
  ASTNode_t *node = ASTNode_create();
  ASTNode_t *c1 = ASTNode_create();
  ASTNode_t *c2 = ASTNode_create();
  ASTNode_t *arg = ASTNode_create();
  const char* bvar = "foo";

  ASTNode_setType(node, AST_PLUS);
  ASTNode_setName(c1, "foo");
  ASTNode_setName(c2, "foo2");
  ASTNode_addChild(node, c1);
  ASTNode_addChild(node, c2);

  fail_unless( !strcmp(ASTNode_getName(ASTNode_getChild(node, 0)), "foo")); 


  ASTNode_setName(arg, "repl");

  ASTNode_replaceArgument(node, bvar, arg);

  fail_unless( !strcmp(ASTNode_getName(ASTNode_getChild(node, 0)), "repl")); 

  ASTNode_free(node);
  ASTNode_free(arg);
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
  tcase_add_test( tcase, test_ASTNode_deepCopy_1              );
  tcase_add_test( tcase, test_ASTNode_deepCopy_2              );
  tcase_add_test( tcase, test_ASTNode_deepCopy_3              );
  tcase_add_test( tcase, test_ASTNode_deepCopy_4              );
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
  tcase_add_test( tcase, test_ASTNode_replaceArgument         );

  suite_add_tcase(suite, tcase);

  return suite;
}
