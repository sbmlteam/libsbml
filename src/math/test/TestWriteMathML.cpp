/**
 * \file    TestWriteMathML.cpp
 * \brief   Write MathML unit tests
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2003 California Institute of Technology and Japan Science and
 * Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is
 * provided in the file named "LICENSE.txt" included with this software
 * distribution.  It is also available online at
 * http://sbml.org/software/libsbml/license.html
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 */

#include <limits>
#include <iostream>

#include <check.h>

#include <sbml/math/FormulaParser.h>
#include <sbml/math/ASTNode.h>
#include <sbml/math/MathML.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */


/**
 * Wraps the string s in the appropriate XML boilerplate.
 */
#define XML_HEADER    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
#define MATHML_HEADER "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
#define MATHML_FOOTER "</math>"

#define wrapMathML(s)   XML_HEADER MATHML_HEADER s MATHML_FOOTER


static ASTNode* N;
static char*    S;


void
WriteMathML_setup ()
{
  N = 0;
  S = 0;
}


void
WriteMathML_teardown ()
{
  delete N;
  free(S);
}


static bool
equals (const char* expected, const char* actual)
{
  if ( !strcmp(expected, actual) ) return true;

  printf( "\nStrings are not equal:\n"  );
  printf( "Expected:\n[%s]\n", expected );
  printf( "Actual:\n[%s]\n"  , actual   );

  return false;
}


CK_CPPSTART


START_TEST (test_MathMLFormatter_cn_real_1)
{
  const char *expected = wrapMathML("  <cn> 1.2 </cn>\n");

  N = SBML_parseFormula("1.2");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_MathMLFormatter_cn_real_2)
{
  const char* expected = wrapMathML("  <cn> 1234567.8 </cn>\n");

  N = SBML_parseFormula("1234567.8");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_MathMLFormatter_cn_real_3)
{
  const char* expected = wrapMathML("  <cn> -3.14 </cn>\n");

  N = SBML_parseFormula("-3.14");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_MathMLFormatter_cn_real_locale)
{
  const char* expected = wrapMathML("  <cn> 2.72 </cn>\n");


  setlocale(LC_NUMERIC, "de_DE");

  N = SBML_parseFormula("2.72");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );

  setlocale(LC_NUMERIC, "C");
}
END_TEST


START_TEST (test_MathMLFormatter_cn_e_notation_1)
{
  const char* expected = wrapMathML
  (
    "  <cn type=\"e-notation\"> 0 <sep/> 3 </cn>\n"
  );

  N = SBML_parseFormula("0e3");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_MathMLFormatter_cn_e_notation_2)
{
  const char* expected = wrapMathML
  (
    "  <cn type=\"e-notation\"> 2 <sep/> 3 </cn>\n"
  );

  N = SBML_parseFormula("2e3");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_MathMLFormatter_cn_e_notation_3)
{
  const char* expected = wrapMathML
  (
    "  <cn type=\"e-notation\"> 1234567.8 <sep/> 3 </cn>\n"
  );

  N = SBML_parseFormula("1234567.8e3");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_MathMLFormatter_cn_e_notation_4)
{
  const char* expected = wrapMathML
  (
    "  <cn type=\"e-notation\"> 6.0221367 <sep/> 23 </cn>\n"
  );

  N = SBML_parseFormula("6.0221367e+23");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_MathMLFormatter_cn_e_notation_5)
{
  const char* expected = wrapMathML
  (
    "  <cn type=\"e-notation\"> 4 <sep/> -6 </cn>\n"
  );

  N = SBML_parseFormula(".000004");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_MathMLFormatter_cn_e_notation_6)
{
  const char* expected = wrapMathML
  (
    "  <cn type=\"e-notation\"> 4 <sep/> -12 </cn>\n"
  );

  N = SBML_parseFormula(".000004e-6");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_MathMLFormatter_cn_e_notation_7)
{
  const char* expected = wrapMathML
  (
    "  <cn type=\"e-notation\"> -1 <sep/> -6 </cn>\n"
  );

  N = SBML_parseFormula("-1e-6");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_MathMLFormatter_cn_integer)
{
  const char* expected = wrapMathML("  <cn type=\"integer\"> 5 </cn>\n");

  N = SBML_parseFormula("5");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_MathMLFormatter_cn_rational)
{
  const char* expected = wrapMathML
  (
    "  <cn type=\"rational\"> 1 <sep/> 3 </cn>\n"
  );

  N = new ASTNode;
  N->setValue(static_cast<long>(1), 3);

  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_MathMLFormatter_ci)
{
  const char* expected = wrapMathML("  <ci> foo </ci>\n");

  N = SBML_parseFormula("foo");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_MathMLFormatter_csymbol_delay)
{
  const char* expected = wrapMathML
  (
    "  <apply>\n"
    "    <csymbol encoding=\"text\" definitionURL=\"http://www.sbml.org/sbml/"
    "symbols/delay\"> my_delay </csymbol>\n"
    "    <ci> x </ci>\n"
    "    <cn> 0.1 </cn>\n"
    "  </apply>\n"
  );

  N = SBML_parseFormula("delay(x, 0.1)");
  N->setName("my_delay");

  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_MathMLFormatter_csymbol_time)
{
  const char* expected = wrapMathML
  (
    "  <csymbol encoding=\"text\" "
    "definitionURL=\"http://www.sbml.org/sbml/symbols/time\"> t </csymbol>\n"
  );

  N = new ASTNode(AST_NAME_TIME);
  N->setName("t");

  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_MathMLFormatter_constant_true)
{
  const char* expected = wrapMathML("  <true/>\n");

  N = new ASTNode(AST_CONSTANT_TRUE);
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_MathMLFormatter_constant_false)
{
  const char* expected = wrapMathML("  <false/>\n");

  N = new ASTNode(AST_CONSTANT_FALSE);
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_MathMLFormatter_constant_notanumber)
{
  const char* expected = wrapMathML("  <notanumber/>\n");

  N = new ASTNode(AST_REAL);
  N->setValue( numeric_limits<double>::quiet_NaN() );

  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_MathMLFormatter_constant_infinity)
{
  const char* expected = wrapMathML("  <infinity/>\n");

  N = new ASTNode;
  N->setValue( numeric_limits<double>::infinity() );

  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_MathMLFormatter_constant_infinity_neg)
{
  const char* expected = wrapMathML
  (
    "  <apply> <minus/> <infinity/> </apply>\n"
  );

  N = new ASTNode;
  N->setValue( - numeric_limits<double>::infinity() );

  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_MathMLFormatter_constant_exponentiale)
{
  const char* expected = wrapMathML("  <exponentiale/>\n");

  N = new ASTNode(AST_CONSTANT_E);
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_MathMLFormatter_plus_binary)
{
  const char* expected = wrapMathML
  (
    "  <apply>\n"
    "    <plus/>\n"
    "    <cn type=\"integer\"> 1 </cn>\n"
    "    <cn type=\"integer\"> 2 </cn>\n"
    "  </apply>\n"
  );

  N = SBML_parseFormula("1 + 2");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_MathMLFormatter_plus_nary_1)
{
  const char* expected = wrapMathML
  (
    "  <apply>\n"
    "    <plus/>\n"
    "    <cn type=\"integer\"> 1 </cn>\n"
    "    <cn type=\"integer\"> 2 </cn>\n"
    "    <cn type=\"integer\"> 3 </cn>\n"
    "  </apply>\n"
  );

  N = SBML_parseFormula("1 + 2 + 3");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_MathMLFormatter_plus_nary_2)
{
  const char* expected = wrapMathML
  (
    "  <apply>\n"
    "    <plus/>\n"
    "    <cn type=\"integer\"> 1 </cn>\n"
    "    <cn type=\"integer\"> 2 </cn>\n"
    "    <cn type=\"integer\"> 3 </cn>\n"
    "  </apply>\n"
  );

  N = SBML_parseFormula("(1 + 2) + 3");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_MathMLFormatter_plus_nary_3)
{
  const char* expected = wrapMathML
  (
    "  <apply>\n"
    "    <plus/>\n"
    "    <cn type=\"integer\"> 1 </cn>\n"
    "    <cn type=\"integer\"> 2 </cn>\n"
    "    <cn type=\"integer\"> 3 </cn>\n"
    "  </apply>\n"
  );

  N = SBML_parseFormula("1 + (2 + 3)");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_MathMLFormatter_plus_nary_4)
{
  const char* expected = wrapMathML
  (
    "  <apply>\n"
    "    <plus/>\n"
    "    <cn type=\"integer\"> 1 </cn>\n"
    "    <cn type=\"integer\"> 2 </cn>\n"
    "    <apply>\n"
    "      <times/>\n"
    "      <ci> x </ci>\n"
    "      <ci> y </ci>\n"
    "      <ci> z </ci>\n"
    "    </apply>\n"
    "    <cn type=\"integer\"> 3 </cn>\n"
    "  </apply>\n"
  );

  N = SBML_parseFormula("1 + 2 + x * y * z + 3");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_MathMLFormatter_minus)
{
  const char* expected = wrapMathML
  (
    "  <apply>\n"
    "    <minus/>\n"
    "    <cn type=\"integer\"> 1 </cn>\n"
    "    <cn type=\"integer\"> 2 </cn>\n"
    "  </apply>\n"
  );

  N = SBML_parseFormula("1 - 2");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_MathMLFormatter_minus_unary_1)
{
  const char* expected = wrapMathML
  (
    "  <cn type=\"integer\"> -2 </cn>\n"
  );

  N = SBML_parseFormula("-2");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_MathMLFormatter_minus_unary_2)
{
  const char* expected = wrapMathML
  (
    "  <apply>\n"
    "    <minus/>\n"
    "    <ci> a </ci>\n"
    "  </apply>\n"
  );

  N = SBML_parseFormula("-a");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_MathMLFormatter_function_1)
{
  const char* expected = wrapMathML
  (
    "  <apply>\n"
    "    <ci> foo </ci>\n"
    "    <cn type=\"integer\"> 1 </cn>\n"
    "    <cn type=\"integer\"> 2 </cn>\n"
    "    <cn type=\"integer\"> 3 </cn>\n"
    "  </apply>\n"
  );

  N = SBML_parseFormula("foo(1, 2, 3)");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_MathMLFormatter_function_2)
{
  const char* expected = wrapMathML
  (
    "  <apply>\n"
    "    <ci> foo </ci>\n"
    "    <cn type=\"integer\"> 1 </cn>\n"
    "    <cn type=\"integer\"> 2 </cn>\n"
    "    <apply>\n"
    "      <ci> bar </ci>\n"
    "      <ci> z </ci>\n"
    "    </apply>\n"
    "  </apply>\n"
  );

  N = SBML_parseFormula("foo(1, 2, bar(z))");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_MathMLFormatter_sin)
{
  const char* expected = wrapMathML
  (
    "  <apply>\n"
    "    <sin/>\n"
    "    <ci> x </ci>\n"
    "  </apply>\n"
  );

  N = SBML_parseFormula("sin(x)");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_MathMLFormatter_log)
{
  const char* expected = wrapMathML
  (
    "  <apply>\n"
    "    <log/>\n"
    "    <logbase>\n"
    "      <cn type=\"integer\"> 2 </cn>\n"
    "    </logbase>\n"
    "    <ci> N </ci>\n"
    "  </apply>\n"
  );

  N = SBML_parseFormula("log(2, N)");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_MathMLFormatter_root)
{
  const char* expected = wrapMathML
  (
    "  <apply>\n"
    "    <root/>\n"
    "    <degree>\n"
    "      <cn type=\"integer\"> 3 </cn>\n"
    "    </degree>\n"
    "    <ci> x </ci>\n"
    "  </apply>\n"
  );

  N = SBML_parseFormula("root(3, x)");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_MathMLFormatter_lambda)
{
  const char* expected = wrapMathML
  (
    "  <lambda>\n"
    "    <bvar>\n"
    "      <ci> x </ci>\n"
    "    </bvar>\n"
    "    <bvar>\n"
    "      <ci> y </ci>\n"
    "    </bvar>\n"
    "    <apply>\n"
    "      <root/>\n"
    "      <degree>\n"
    "        <cn type=\"integer\"> 2 </cn>\n"
    "      </degree>\n"
    "      <apply>\n"
    "        <plus/>\n"
    "        <apply>\n"
    "          <power/>\n"
    "          <ci> x </ci>\n"
    "          <cn type=\"integer\"> 2 </cn>\n"
    "        </apply>\n"
    "        <apply>\n"
    "          <power/>\n"
    "          <ci> y </ci>\n"
    "          <cn type=\"integer\"> 2 </cn>\n"
    "        </apply>\n"
    "      </apply>\n"
    "    </apply>\n"
    "  </lambda>\n"
  );

  N = SBML_parseFormula("lambda(x, y, root(2, x^2 + y^2))");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_MathMLFormatter_lambda_no_bvars)
{
  const char* expected = wrapMathML
  (
    "  <lambda>\n"
    "    <apply>\n"
    "      <plus/>\n"
    "      <cn type=\"integer\"> 2 </cn>\n"
    "      <cn type=\"integer\"> 2 </cn>\n"
    "    </apply>\n"
    "  </lambda>\n"
  );

  N = SBML_parseFormula("lambda(2 + 2)");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_MathMLFormatter_piecewise)
{
  const char* expected = wrapMathML
  (
    "  <piecewise>\n"
    "    <piece>\n"
    "      <apply>\n"
    "        <minus/>\n"
    "        <ci> x </ci>\n"
    "      </apply>\n"
    "      <apply>\n"
    "        <lt/>\n"
    "        <ci> x </ci>\n"
    "        <cn type=\"integer\"> 0 </cn>\n"
    "      </apply>\n"
    "    </piece>\n"
    "    <piece>\n"
    "      <cn type=\"integer\"> 0 </cn>\n"
    "      <apply>\n"
    "        <eq/>\n"
    "        <ci> x </ci>\n" 
    "        <cn type=\"integer\"> 0 </cn>\n"
    "      </apply>\n"
    "    </piece>\n"
    "    <piece>\n"
    "      <ci> x </ci>\n"
    "      <apply>\n"
    "        <gt/>\n"
    "        <ci> x </ci>\n" 
    "        <cn type=\"integer\"> 0 </cn>\n"
    "      </apply>\n"
    "    </piece>\n"
    "  </piecewise>\n"
  );

  const char *f = "piecewise(-x, lt(x, 0), 0, eq(x, 0), x, gt(x, 0))";

  N = SBML_parseFormula(f);
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


START_TEST (test_MathMLFormatter_piecewise_otherwise)
{
  const char* expected = wrapMathML
  (
    "  <piecewise>\n"
    "    <piece>\n"
    "      <cn type=\"integer\"> 0 </cn>\n"
    "      <apply>\n"
    "        <lt/>\n"
    "        <ci> x </ci>\n"
    "        <cn type=\"integer\"> 0 </cn>\n"
    "      </apply>\n"
    "    </piece>\n"
    "    <otherwise>\n"
    "      <ci> x </ci>\n" 
    "    </otherwise>\n"
    "  </piecewise>\n"
  );

  N = SBML_parseFormula("piecewise(0, lt(x, 0), x)");
  S = writeMathMLToString(N);

  fail_unless( equals(expected, S) );
}
END_TEST


Suite *
create_suite_WriteMathML ()
{
  Suite *suite = suite_create("WriteMathML");
  TCase *tcase = tcase_create("WriteMathML");

  tcase_add_checked_fixture(tcase, WriteMathML_setup, WriteMathML_teardown);

  tcase_add_test( tcase, test_MathMLFormatter_cn_real_1             );
  tcase_add_test( tcase, test_MathMLFormatter_cn_real_2             );
  tcase_add_test( tcase, test_MathMLFormatter_cn_real_3             );
  tcase_add_test( tcase, test_MathMLFormatter_cn_real_locale        );
  tcase_add_test( tcase, test_MathMLFormatter_cn_e_notation_1       );
  tcase_add_test( tcase, test_MathMLFormatter_cn_e_notation_2       );
  tcase_add_test( tcase, test_MathMLFormatter_cn_e_notation_3       );
  tcase_add_test( tcase, test_MathMLFormatter_cn_e_notation_4       );
  tcase_add_test( tcase, test_MathMLFormatter_cn_e_notation_5       );
  tcase_add_test( tcase, test_MathMLFormatter_cn_e_notation_6       );
  tcase_add_test( tcase, test_MathMLFormatter_cn_e_notation_7       );
  tcase_add_test( tcase, test_MathMLFormatter_cn_integer            );
  tcase_add_test( tcase, test_MathMLFormatter_cn_rational           );

  tcase_add_test( tcase, test_MathMLFormatter_ci                    );
  tcase_add_test( tcase, test_MathMLFormatter_csymbol_delay         );
  tcase_add_test( tcase, test_MathMLFormatter_csymbol_time          );
  tcase_add_test( tcase, test_MathMLFormatter_constant_true         );
  tcase_add_test( tcase, test_MathMLFormatter_constant_false        );
  tcase_add_test( tcase, test_MathMLFormatter_constant_notanumber   );
  tcase_add_test( tcase, test_MathMLFormatter_constant_infinity     );
  tcase_add_test( tcase, test_MathMLFormatter_constant_infinity_neg );
  tcase_add_test( tcase, test_MathMLFormatter_constant_exponentiale );
  tcase_add_test( tcase, test_MathMLFormatter_plus_binary           );
  tcase_add_test( tcase, test_MathMLFormatter_plus_nary_1           );
  tcase_add_test( tcase, test_MathMLFormatter_plus_nary_2           );
  tcase_add_test( tcase, test_MathMLFormatter_plus_nary_3           );
  tcase_add_test( tcase, test_MathMLFormatter_plus_nary_4           );
  tcase_add_test( tcase, test_MathMLFormatter_minus                 );
  tcase_add_test( tcase, test_MathMLFormatter_minus_unary_1         );
  tcase_add_test( tcase, test_MathMLFormatter_minus_unary_2         );
  tcase_add_test( tcase, test_MathMLFormatter_function_1            );
  tcase_add_test( tcase, test_MathMLFormatter_function_2            );
  tcase_add_test( tcase, test_MathMLFormatter_sin                   );
  tcase_add_test( tcase, test_MathMLFormatter_log                   );
  tcase_add_test( tcase, test_MathMLFormatter_root                  );
  tcase_add_test( tcase, test_MathMLFormatter_lambda                );
  tcase_add_test( tcase, test_MathMLFormatter_lambda_no_bvars       );
  tcase_add_test( tcase, test_MathMLFormatter_piecewise             );
  tcase_add_test( tcase, test_MathMLFormatter_piecewise_otherwise   );

  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND
