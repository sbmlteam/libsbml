/**
 * \file    TestMathMLFormatter.cpp
 * \brief   MathMLFormatter unit tests
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
 *   Stefan Hoops
 */


#include <iostream>
#include <check.h>

#include "common/common.h"

#include "FormulaParser.h"
#include "MathMLFormatter.h"

#ifdef USE_EXPAT
#  include "xml/ExpatFormatter.h"
#else
   using namespace xercesc;
#endif  // USE_EXPAT


BEGIN_C_DECLS


/**
 * Wraps the string s in the appropriate XML boilerplate.
 */
#define XML_HEADER    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
#define MATHML_HEADER "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">"
#define MATHML_FOOTER "</math>"

#define wrapXML(s)      XML_HEADER s
#define wrapMathML(s)   XML_HEADER MATHML_HEADER s MATHML_FOOTER


static MemBufFormatTarget *target;
static MathMLFormatter    *formatter;


void
TestMathMLFormatter_setup (void)
{
  try
  {
    XML_PLATFORM_UTILS_INIT();
  }
  catch (...)
  {
    fail("XMLPlatformUtils::Initialize() threw an Exception.");
  }

  target    = new MemBufFormatTarget();
  formatter = new MathMLFormatter(target, true);
}


void
TestMathMLFormatter_teardown (void)
{
  delete formatter;
}


START_TEST (test_MathMLFormatter_cn_real_1)
{
  ASTNode_t  *n = SBML_parseFormula("1.2");
  const char *s = wrapXML("<cn> 1.2 </cn>\n");


  *formatter << n;
  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);

  ASTNode_free(n);
}
END_TEST


START_TEST (test_MathMLFormatter_cn_real_2)
{
  ASTNode_t  *n = SBML_parseFormula("1234567.8");
  const char *s = wrapXML("<cn> 1234567.8 </cn>\n");


  *formatter << n;
  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);

  ASTNode_free(n);
}
END_TEST


START_TEST (test_MathMLFormatter_cn_real_3)
{
  ASTNode_t  *n = SBML_parseFormula("-3.14");
  const char *s = wrapXML("<cn> -3.14 </cn>\n");


  *formatter << n;
  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);

  ASTNode_free(n);
}
END_TEST


START_TEST (test_MathMLFormatter_cn_real_locale)
{
  ASTNode_t  *n = SBML_parseFormula("2.72");
  const char *s = wrapXML("<cn> 2.72 </cn>\n");


  setlocale(LC_NUMERIC, "de_DE");

  *formatter << n;
  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);

  setlocale(LC_NUMERIC, "C");

  ASTNode_free(n);
}
END_TEST


START_TEST (test_MathMLFormatter_cn_e_notation_1)
{
  ASTNode_t  *n = SBML_parseFormula("0e3");
  const char *s = wrapXML
  (
    "<cn type=\"e-notation\"> 0 <sep/> 3 </cn>\n"
  );


  *formatter << n;
  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);

  ASTNode_free(n);
}
END_TEST


START_TEST (test_MathMLFormatter_cn_e_notation_2)
{
  ASTNode_t  *n = SBML_parseFormula("2e3");
  const char *s = wrapXML
  (
    "<cn type=\"e-notation\"> 2 <sep/> 3 </cn>\n"
  );


  *formatter << n;
  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);

  ASTNode_free(n);
}
END_TEST


START_TEST (test_MathMLFormatter_cn_e_notation_3)
{
  ASTNode_t  *n = SBML_parseFormula("1234567.8e3");
  const char *s = wrapXML
  (
    "<cn type=\"e-notation\"> 1234567.8 <sep/> 3 </cn>\n"
  );


  *formatter << n;
  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);

  ASTNode_free(n);
}
END_TEST


START_TEST (test_MathMLFormatter_cn_e_notation_4)
{
  ASTNode_t  *n = SBML_parseFormula("6.0221367e+23");
  const char *s = wrapXML
  (
    "<cn type=\"e-notation\"> 6.0221367 <sep/> 23 </cn>\n"
  );


  *formatter << n;
  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);

  ASTNode_free(n);
}
END_TEST


START_TEST (test_MathMLFormatter_cn_e_notation_5)
{
  ASTNode_t  *n = SBML_parseFormula(".000004");
  const char *s = wrapXML("<cn type=\"e-notation\"> 4 <sep/> -6 </cn>\n");


  *formatter << n;
  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);

  ASTNode_free(n);
}
END_TEST


START_TEST (test_MathMLFormatter_cn_e_notation_6)
{
  ASTNode_t  *n = SBML_parseFormula(".000004e-6");
  const char *s = wrapXML("<cn type=\"e-notation\"> 4 <sep/> -12 </cn>\n");


  *formatter << n;
  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);

  ASTNode_free(n);
}
END_TEST


START_TEST (test_MathMLFormatter_cn_e_notation_7)
{
  const char *s = wrapXML("<cn type=\"e-notation\"> -1 <sep/> -6 </cn>\n");


  *formatter << -1e-6;
  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);
}
END_TEST


START_TEST (test_MathMLFormatter_cn_integer)
{
  ASTNode_t  *n = SBML_parseFormula("5");
  const char *s = wrapXML("<cn type=\"integer\"> 5 </cn>\n");


  *formatter << n;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);

  ASTNode_free(n);
}
END_TEST


START_TEST (test_MathMLFormatter_cn_rational)
{
  ASTNode_t  *n = ASTNode_createWithType(AST_RATIONAL);
  const char *s = wrapXML("<cn type=\"rational\"> 1 <sep/> 3 </cn>\n");


  ASTNode_setRational(n, 1, 3);

  *formatter << n;
  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);

  ASTNode_free(n);
}
END_TEST


START_TEST (test_MathMLFormatter_ci)
{
  ASTNode_t  *n = SBML_parseFormula("foo");
  const char *s = wrapXML("<ci> foo </ci>\n");


  *formatter << n;
  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);

  ASTNode_free(n);
}
END_TEST


START_TEST (test_MathMLFormatter_csymbol_delay)
{
  ASTNode_t  *n = SBML_parseFormula("delay(x, 0.1)");
  const char *s = wrapXML
  (
    "<apply>\n"
    "  <csymbol encoding=\"text\" definitionURL=\"http://www.sbml.org/sbml/"
    "symbols/delay\"> my_delay </csymbol>\n"
    "  <ci> x </ci>\n"
    "  <cn> 0.1 </cn>\n"
    "</apply>\n"
  );


  ASTNode_setName(n, "my_delay");

  *formatter << n;
  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);

  ASTNode_free(n);
}
END_TEST


START_TEST (test_MathMLFormatter_csymbol_time)
{
  ASTNode_t  *n = ASTNode_createWithType(AST_NAME_TIME);
  const char *s = wrapXML
  (
    "<csymbol encoding=\"text\" "
    "definitionURL=\"http://www.sbml.org/sbml/symbols/time\"> t </csymbol>\n"
  );


  ASTNode_setName(n, "t");

  *formatter << n;
  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);

  ASTNode_free(n);
}
END_TEST


START_TEST (test_MathMLFormatter_constant_true)
{
  ASTNode_t  *n = ASTNode_createWithType(AST_CONSTANT_TRUE);
  const char *s = wrapXML("<true/>\n");


  *formatter << n;
  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);

  ASTNode_free(n);
}
END_TEST


START_TEST (test_MathMLFormatter_constant_false)
{
  ASTNode_t  *n = ASTNode_createWithType(AST_CONSTANT_FALSE);
  const char *s = wrapXML("<false/>\n");


  *formatter << n;
  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);

  ASTNode_free(n);
}
END_TEST


START_TEST (test_MathMLFormatter_constant_notanumber)
{
  ASTNode_t  *n = ASTNode_createWithType(AST_REAL);
  const char *s = wrapXML("<notanumber/>\n");


  ASTNode_setReal(n, util_NaN());

  *formatter << n;
  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);

  ASTNode_free(n);
}
END_TEST


START_TEST (test_MathMLFormatter_constant_infinity)
{
  ASTNode_t  *n = ASTNode_createWithType(AST_REAL);
  const char *s = wrapXML("<infinity/>\n");


  ASTNode_setReal(n, util_PosInf());

  *formatter << n;
  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);

  ASTNode_free(n);
}
END_TEST


START_TEST (test_MathMLFormatter_constant_infinity_neg)
{
  ASTNode_t  *n = ASTNode_createWithType(AST_REAL);
  const char *s = wrapXML
  (
    "<apply>\n"
    "  <minus/>\n"
    "  <infinity/>\n"
    "</apply>\n"
  );


  ASTNode_setReal(n, util_NegInf());

  *formatter << n;
  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);

  ASTNode_free(n);
}
END_TEST


START_TEST (test_MathMLFormatter_constant_exponentiale)
{
  ASTNode_t  *n = ASTNode_createWithType(AST_CONSTANT_E);
  const char *s = wrapXML("<exponentiale/>\n");


  *formatter << n;
  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);

  ASTNode_free(n);
}
END_TEST


START_TEST (test_MathMLFormatter_plus_binary)
{
  ASTNode_t  *n = SBML_parseFormula("1 + 2");
  const char *s = wrapXML
  (
    "<apply>\n"
    "  <plus/>\n"
    "  <cn type=\"integer\"> 1 </cn>\n"
    "  <cn type=\"integer\"> 2 </cn>\n"
    "</apply>\n"
  );


  *formatter << n;
  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);

  ASTNode_free(n);
}
END_TEST


START_TEST (test_MathMLFormatter_plus_nary_1)
{
  ASTNode_t  *n = SBML_parseFormula("1 + 2 + 3");
  const char *s = wrapXML
  (
    "<apply>\n"
    "  <plus/>\n"
    "  <cn type=\"integer\"> 1 </cn>\n"
    "  <cn type=\"integer\"> 2 </cn>\n"
    "  <cn type=\"integer\"> 3 </cn>\n"
    "</apply>\n"
  );


  *formatter << n;
  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);

  ASTNode_free(n);
}
END_TEST


START_TEST (test_MathMLFormatter_plus_nary_2)
{
  ASTNode_t  *n = SBML_parseFormula("(1 + 2) + 3");
  const char *s = wrapXML
  (
    "<apply>\n"
    "  <plus/>\n"
    "  <cn type=\"integer\"> 1 </cn>\n"
    "  <cn type=\"integer\"> 2 </cn>\n"
    "  <cn type=\"integer\"> 3 </cn>\n"
    "</apply>\n"
  );


  *formatter << n;
  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);

  ASTNode_free(n);
}
END_TEST


START_TEST (test_MathMLFormatter_plus_nary_3)
{
  ASTNode_t  *n = SBML_parseFormula("1 + (2 + 3)");
  const char *s = wrapXML
  (
    "<apply>\n"
    "  <plus/>\n"
    "  <cn type=\"integer\"> 1 </cn>\n"
    "  <cn type=\"integer\"> 2 </cn>\n"
    "  <cn type=\"integer\"> 3 </cn>\n"
    "</apply>\n"
  );


  *formatter << n;
  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);

  ASTNode_free(n);
}
END_TEST


START_TEST (test_MathMLFormatter_plus_nary_4)
{
  ASTNode_t  *n = SBML_parseFormula("1 + 2 + x * y * z + 3");
  const char *s = wrapXML
  (
    "<apply>\n"
    "  <plus/>\n"
    "  <cn type=\"integer\"> 1 </cn>\n"
    "  <cn type=\"integer\"> 2 </cn>\n"
    "  <apply>\n"
    "    <times/>\n"
    "    <ci> x </ci>\n"
    "    <ci> y </ci>\n"
    "    <ci> z </ci>\n"
    "  </apply>\n"
    "  <cn type=\"integer\"> 3 </cn>\n"
    "</apply>\n"
  );


  *formatter << n;
  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);

  ASTNode_free(n);
}
END_TEST


START_TEST (test_MathMLFormatter_minus)
{
  ASTNode_t  *n = SBML_parseFormula("1 - 2");
  const char *s = wrapXML
  (
    "<apply>\n"
    "  <minus/>\n"
    "  <cn type=\"integer\"> 1 </cn>\n"
    "  <cn type=\"integer\"> 2 </cn>\n"
    "</apply>\n"
  );


  *formatter << n;
  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);

  ASTNode_free(n);
}
END_TEST


START_TEST (test_MathMLFormatter_minus_unary_1)
{
  ASTNode_t  *n = SBML_parseFormula("-2");
  const char *s = wrapXML
  (
    "<cn type=\"integer\"> -2 </cn>\n"
  );


  *formatter << n;
  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);

  ASTNode_free(n);
}
END_TEST


START_TEST (test_MathMLFormatter_minus_unary_2)
{
  ASTNode_t  *n = SBML_parseFormula("-a");
  const char *s = wrapXML
  (
    "<apply>\n"
    "  <minus/>\n"
    "  <ci> a </ci>\n"
    "</apply>\n"
  );


  *formatter << n;
  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);

  ASTNode_free(n);
}
END_TEST


START_TEST (test_MathMLFormatter_function_1)
{
  ASTNode_t  *n = SBML_parseFormula("foo(1, 2, 3)");
  const char *s = wrapXML
  (
    "<apply>\n"
    "  <ci> foo </ci>\n"
    "  <cn type=\"integer\"> 1 </cn>\n"
    "  <cn type=\"integer\"> 2 </cn>\n"
    "  <cn type=\"integer\"> 3 </cn>\n"
    "</apply>\n"
  );


  *formatter << n;
  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);

  ASTNode_free(n);
}
END_TEST


START_TEST (test_MathMLFormatter_function_2)
{
  ASTNode_t  *n = SBML_parseFormula("foo(1, 2, bar(z))");
  const char *s = wrapXML
  (
    "<apply>\n"
    "  <ci> foo </ci>\n"
    "  <cn type=\"integer\"> 1 </cn>\n"
    "  <cn type=\"integer\"> 2 </cn>\n"
    "  <apply>\n"
    "    <ci> bar </ci>\n"
    "    <ci> z </ci>\n"
    "  </apply>\n"
    "</apply>\n"
  );


  *formatter << n;
  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);

  ASTNode_free(n);
}
END_TEST


START_TEST (test_MathMLFormatter_sin)
{
  ASTNode_t  *n = SBML_parseFormula("sin(x)");
  const char *s = wrapXML
  (
    "<apply>\n"
    "  <sin/>\n"
    "  <ci> x </ci>\n"
    "</apply>\n"
  );


  *formatter << n;
  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);

  ASTNode_free(n);
}
END_TEST


START_TEST (test_MathMLFormatter_log)
{
  ASTNode_t  *n = SBML_parseFormula("log(2, N)");
  const char *s = wrapXML
  (
    "<apply>\n"
    "  <log/>\n"
    "  <logbase>\n"
    "    <cn type=\"integer\"> 2 </cn>\n"
    "  </logbase>\n"
    "  <ci> N </ci>\n"
    "</apply>\n"
  );


  *formatter << n;
  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);

  ASTNode_free(n);
}
END_TEST


START_TEST (test_MathMLFormatter_root)
{
  ASTNode_t  *n = SBML_parseFormula("root(3, x)");
  const char *s = wrapXML
  (
    "<apply>\n"
    "  <root/>\n"
    "  <degree>\n"
    "    <cn type=\"integer\"> 3 </cn>\n"
    "  </degree>\n"
    "  <ci> x </ci>\n"
    "</apply>\n"
  );


  *formatter << n;
  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);

  ASTNode_free(n);
}
END_TEST


START_TEST (test_MathMLFormatter_lambda)
{
  ASTNode_t  *n = SBML_parseFormula("lambda(x, y, root(2, x^2 + y^2))");
  const char *s = wrapXML
  (
    "<lambda>\n"
    "  <bvar>\n"
    "    <ci> x </ci>\n"
    "  </bvar>\n"
    "  <bvar>\n"
    "    <ci> y </ci>\n"
    "  </bvar>\n"
    "  <apply>\n"
    "    <root/>\n"
    "    <degree>\n"
    "      <cn type=\"integer\"> 2 </cn>\n"
    "    </degree>\n"
    "    <apply>\n"
    "      <plus/>\n"
    "      <apply>\n"
    "        <power/>\n"
    "        <ci> x </ci>\n"
    "        <cn type=\"integer\"> 2 </cn>\n"
    "      </apply>\n"
    "      <apply>\n"
    "        <power/>\n"
    "        <ci> y </ci>\n"
    "        <cn type=\"integer\"> 2 </cn>\n"
    "      </apply>\n"
    "    </apply>\n"
    "  </apply>\n"
    "</lambda>\n"
  );


  *formatter << n;
  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);

  ASTNode_free(n);
}
END_TEST


START_TEST (test_MathMLFormatter_piecewise)
{
  const char *f = "piecewise(-x, lt(x, 0), 0, eq(x, 0), x, gt(x, 0))";
  ASTNode_t  *n = SBML_parseFormula(f);
  const char *s = wrapXML
  (
    "<piecewise>\n"
    "  <piece>\n"
    "    <apply>\n"
    "      <minus/>\n"
    "      <ci> x </ci>\n"
    "    </apply>\n"
    "    <apply>\n"
    "      <lt/>\n"
    "      <ci> x </ci>\n"
    "      <cn type=\"integer\"> 0 </cn>\n"
    "    </apply>\n"
    "  </piece>\n"
    "  <piece>\n"
    "    <cn type=\"integer\"> 0 </cn>\n"
    "    <apply>\n"
    "      <eq/>\n"
    "      <ci> x </ci>\n" 
    "      <cn type=\"integer\"> 0 </cn>\n"
    "    </apply>\n"
    "  </piece>\n"
    "  <piece>\n"
    "    <ci> x </ci>\n"
    "    <apply>\n"
    "      <gt/>\n"
    "      <ci> x </ci>\n" 
    "      <cn type=\"integer\"> 0 </cn>\n"
    "    </apply>\n"
    "  </piece>\n"
    "</piecewise>\n"
  );


  *formatter << n;
  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);

  ASTNode_free(n);
}
END_TEST


START_TEST (test_MathMLFormatter_piecewise_otherwise)
{
  ASTNode_t  *n = SBML_parseFormula("piecewise(0, lt(x, 0), x)");
  const char *s = wrapXML
  (
    "<piecewise>\n"
    "  <piece>\n"
    "    <cn type=\"integer\"> 0 </cn>\n"
    "    <apply>\n"
    "      <lt/>\n"
    "      <ci> x </ci>\n"
    "      <cn type=\"integer\"> 0 </cn>\n"
    "    </apply>\n"
    "  </piece>\n"
    "  <otherwise>\n"
    "    <ci> x </ci>\n" 
    "  </otherwise>\n"
    "</piecewise>\n"
  );


  *formatter << n;
  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);

  ASTNode_free(n);
}
END_TEST


Suite *
create_suite_MathMLFormatter (void)
{
  Suite *suite = suite_create("MathMLFormatter");
  TCase *tcase = tcase_create("MathMLFormatter");


  tcase_add_checked_fixture(tcase,
                            TestMathMLFormatter_setup,
                            TestMathMLFormatter_teardown);
 

  tcase_add_test( tcase, test_MathMLFormatter_cn_real_1             );
  tcase_add_test( tcase, test_MathMLFormatter_cn_real_2             );
  tcase_add_test( tcase, test_MathMLFormatter_cn_real_3             );

  /**
   * This test will fail under Cygwin because of a minimal setlocale()
   * implementation (see setlocale manpage).
   */
#ifndef CYGWIN
  tcase_add_test( tcase, test_MathMLFormatter_cn_real_locale        );
#endif

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
  tcase_add_test( tcase, test_MathMLFormatter_piecewise             );
  tcase_add_test( tcase, test_MathMLFormatter_piecewise_otherwise   );

  suite_add_tcase(suite, tcase);

  return suite;
}


END_C_DECLS
