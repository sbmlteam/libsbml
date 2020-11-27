/**
 * \file    TestReadMathML.cpp
 * \brief   Read MathML unit tests
 * \author  Lucian Smith, from orig by Ben Bornstein
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
 *
 * Copyright (C) 2019 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *  
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 *     Pasadena, CA, USA 
 *  
 * Copyright (C) 2002-2005 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. Japan Science and Technology Agency, Japan
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ---------------------------------------------------------------------- -->*/

#include <iostream>
#include <cstring>
#include <check.h>

#include <sbml/util/util.h>

#include <sbml/math/FormulaFormatter.h>
#include <sbml/math/FormulaParser.h>
#include <sbml/math/L3FormulaFormatter.h>
#include <sbml/math/ASTNode.h>
#include <sbml/math/MathML.h>

#include <sbml/xml/XMLNode.h>

LIBSBML_CPP_NAMESPACE_USE

/**
 * Wraps the string s in the appropriate XML or MathML boilerplate.
 */
#define XML_HEADER     "<?xml version='1.0' encoding='UTF-8'?>\n"
#define MATHML_HEADER  "<math xmlns='http://www.w3.org/1998/Math/MathML'>\n"
#define MATHML_HEADER_UNITS  "<math xmlns='http://www.w3.org/1998/Math/MathML'\n"
#define MATHML_HEADER_UNITS2  " xmlns:sbml='http://www.sbml.org/sbml/level3/version2/core'>\n"
#define MATHML_FOOTER  "</math>"

#define wrapXML(s)     XML_HEADER s
#define wrapMathML(s)  XML_HEADER MATHML_HEADER s MATHML_FOOTER
#define wrapMathMLUnits(s)  XML_HEADER MATHML_HEADER_UNITS MATHML_HEADER_UNITS2 s MATHML_FOOTER


static ASTNode* N;
static char*    F;


static void
ReadMathML_setup ()
{
  N = NULL;
  F = NULL;
}


static void
ReadMathML_teardown ()
{
  delete N;
  free(F);
}


CK_CPPSTART


START_TEST (test_element_csymbol_normal_1)
{
  const char* s = wrapMathML
  (
    "<csymbol encoding='text' "
    "definitionURL='http://www.sbml.org/sbml/symbols/distrib/normal'> normal </csymbol>"
  );


  N = readMathMLFromString(s);

  fail_unless( N != NULL );

  fail_unless( N->getType() == AST_DISTRIB_FUNCTION_NORMAL );
  fail_unless( !strcmp(N->getName(), "normal") );
  fail_unless( N->getNumChildren() == 0       );
}
END_TEST


START_TEST (test_element_csymbol_normal_2)
{
  const char* s = wrapMathML
  (
    "<apply>"
    "  <csymbol encoding='text' definitionURL='http://www.sbml.org/sbml/"
    "symbols/distrib/normal'> my_normal </csymbol>"
    "  <ci> x </ci>"
    "  <cn> 0.1 </cn>"
    "</apply>\n"
  );



  N = readMathMLFromString(s);

  fail_unless( N != NULL );

  F = SBML_formulaToString(N);
  fail_unless( !strcmp(F, "my_normal(x, 0.1)") );
}
END_TEST


START_TEST (test_element_csymbol_normal_3)
{
  const char* s = wrapMathML
  (
    "<apply>"
    "  <power/>"
    "  <apply>"
    "    <csymbol encoding='text' definitionURL='http://www.sbml.org/sbml/"
    "symbols/distrib/normal'> normal </csymbol>"
    "    <ci> P </ci>"
    "    <ci> delta_t </ci>"
    "  </apply>\n"
    "  <ci> q </ci>"
    "</apply>\n"
  );



  N = readMathMLFromString(s);

  fail_unless( N != NULL );

  F = SBML_formulaToString(N);
  fail_unless( !strcmp(F, "pow(normal(P, delta_t), q)") );
}
END_TEST


START_TEST (test_element_csymbol_normal_4)
{
  const char* s = wrapMathML
  (
    "<apply>"
    "  <csymbol encoding='text' definitionURL='http://www.sbml.org/sbml/"
    "symbols/distrib/normal'> my_normal </csymbol>"
    "  <ci> x </ci>"
    "</apply>\n"
  );



  N = readMathMLFromString(s);

  fail_unless( N != NULL );

  F = SBML_formulaToString(N);
  fail_unless( !strcmp(F, "my_normal(x)") );
}
END_TEST


START_TEST (test_element_csymbol_normal_5)
{
  const char* s = wrapMathML
  (
    "<apply>"
    "  <csymbol encoding='text' definitionURL='http://www.sbml.org/sbml/"
    "symbols/distrib/normal'> my_normal </csymbol>"
    "  <ci> x </ci>"
    "  <cn> 0.1 </cn>"
    "</apply>\n"
  );



  N = readMathMLFromString(s);

  fail_unless( N != NULL );

  //The L3 parser holds no truck with this 'let's rename csymbols so we 
  // don't know what they actually are any more' nonsense.  It uses the 
  // canonical name for functions, instead.
  F = SBML_formulaToL3String(N);
  fail_unless( !strcmp(F, "normal(x, 0.1)") );
}
END_TEST


START_TEST (test_element_csymbol_normal)
{
  const char* s = wrapMathML
  (
    "<apply>"
    "  <csymbol encoding='text' definitionURL='http://www.sbml.org/sbml/"
    "symbols/distrib/normal'> fake_name </csymbol>"
    "  <ci> x </ci>"
    "  <cn> 0.1 </cn>"
    "</apply>\n"
  );

  N = readMathMLFromString(s);

  fail_unless( N != NULL );

  fail_unless( N->getType() == AST_DISTRIB_FUNCTION_NORMAL );
  fail_unless( !strcmp(N->getName(), "fake_name") );
  fail_unless( N->getNumChildren() == 2       );

  F = SBML_formulaToL3String(N);
  fail_unless( !strcmp(F, "normal(x, 0.1)") );
}
END_TEST


START_TEST (test_element_csymbol_uniform)
{
  const char* s = wrapMathML
  (
    "<apply>"
    "  <csymbol encoding='text' definitionURL='http://www.sbml.org/sbml/"
    "symbols/distrib/uniform'> fake_name </csymbol>"
    "  <ci> x </ci>"
    "  <cn> 0.1 </cn>"
    "</apply>\n"
  );

  N = readMathMLFromString(s);

  fail_unless( N != NULL );

  fail_unless( N->getType() == AST_DISTRIB_FUNCTION_UNIFORM );
  fail_unless( !strcmp(N->getName(), "fake_name") );
  fail_unless( N->getNumChildren() == 2       );

  F = SBML_formulaToL3String(N);
  fail_unless( !strcmp(F, "uniform(x, 0.1)") );
}
END_TEST


START_TEST (test_element_csymbol_bernoulli)
{
  const char* s = wrapMathML
  (
    "<apply>"
    "  <csymbol encoding='text' definitionURL='http://www.sbml.org/sbml/"
    "symbols/distrib/bernoulli'> fake_name </csymbol>"
    "  <ci> x </ci>"
    "  <cn> 0.1 </cn>"
    "</apply>\n"
  );

  N = readMathMLFromString(s);

  fail_unless( N != NULL );

  fail_unless( N->getType() == AST_DISTRIB_FUNCTION_BERNOULLI );
  fail_unless( !strcmp(N->getName(), "fake_name") );
  fail_unless( N->getNumChildren() == 2       );

  F = SBML_formulaToL3String(N);
  fail_unless( !strcmp(F, "bernoulli(x, 0.1)") );
}
END_TEST


START_TEST (test_element_csymbol_binomial)
{
  const char* s = wrapMathML
  (
    "<apply>"
    "  <csymbol encoding='text' definitionURL='http://www.sbml.org/sbml/"
    "symbols/distrib/binomial'> fake_name </csymbol>"
    "  <ci> x </ci>"
    "  <cn> 0.1 </cn>"
    "</apply>\n"
  );

  N = readMathMLFromString(s);

  fail_unless( N != NULL );

  fail_unless( N->getType() == AST_DISTRIB_FUNCTION_BINOMIAL );
  fail_unless( !strcmp(N->getName(), "fake_name") );
  fail_unless( N->getNumChildren() == 2       );

  F = SBML_formulaToL3String(N);
  fail_unless( !strcmp(F, "binomial(x, 0.1)") );
}
END_TEST


START_TEST (test_element_csymbol_cauchy)
{
  const char* s = wrapMathML
  (
    "<apply>"
    "  <csymbol encoding='text' definitionURL='http://www.sbml.org/sbml/"
    "symbols/distrib/cauchy'> fake_name </csymbol>"
    "  <ci> x </ci>"
    "  <cn> 0.1 </cn>"
    "</apply>\n"
  );

  N = readMathMLFromString(s);

  fail_unless( N != NULL );

  fail_unless( N->getType() == AST_DISTRIB_FUNCTION_CAUCHY );
  fail_unless( !strcmp(N->getName(), "fake_name") );
  fail_unless( N->getNumChildren() == 2       );

  F = SBML_formulaToL3String(N);
  fail_unless( !strcmp(F, "cauchy(x, 0.1)") );
}
END_TEST


START_TEST (test_element_csymbol_chisquare)
{
  const char* s = wrapMathML
  (
    "<apply>"
    "  <csymbol encoding='text' definitionURL='http://www.sbml.org/sbml/"
    "symbols/distrib/chisquare'> fake_name </csymbol>"
    "  <ci> x </ci>"
    "  <cn> 0.1 </cn>"
    "</apply>\n"
  );

  N = readMathMLFromString(s);

  fail_unless( N != NULL );

  fail_unless( N->getType() == AST_DISTRIB_FUNCTION_CHISQUARE );
  fail_unless( !strcmp(N->getName(), "fake_name") );
  fail_unless( N->getNumChildren() == 2       );

  F = SBML_formulaToL3String(N);
  fail_unless( !strcmp(F, "chisquare(x, 0.1)") );
}
END_TEST


START_TEST (test_element_csymbol_exponential)
{
  const char* s = wrapMathML
  (
    "<apply>"
    "  <csymbol encoding='text' definitionURL='http://www.sbml.org/sbml/"
    "symbols/distrib/exponential'> fake_name </csymbol>"
    "  <ci> x </ci>"
    "  <cn> 0.1 </cn>"
    "</apply>\n"
  );

  N = readMathMLFromString(s);

  fail_unless( N != NULL );

  fail_unless( N->getType() == AST_DISTRIB_FUNCTION_EXPONENTIAL );
  fail_unless( !strcmp(N->getName(), "fake_name") );
  fail_unless( N->getNumChildren() == 2       );

  F = SBML_formulaToL3String(N);
  fail_unless( !strcmp(F, "exponential(x, 0.1)") );
}
END_TEST


START_TEST (test_element_csymbol_gamma)
{
  const char* s = wrapMathML
  (
    "<apply>"
    "  <csymbol encoding='text' definitionURL='http://www.sbml.org/sbml/"
    "symbols/distrib/gamma'> fake_name </csymbol>"
    "  <ci> x </ci>"
    "  <cn> 0.1 </cn>"
    "</apply>\n"
  );

  N = readMathMLFromString(s);

  fail_unless( N != NULL );

  fail_unless( N->getType() == AST_DISTRIB_FUNCTION_GAMMA );
  fail_unless( !strcmp(N->getName(), "fake_name") );
  fail_unless( N->getNumChildren() == 2       );

  F = SBML_formulaToL3String(N);
  fail_unless( !strcmp(F, "gamma(x, 0.1)") );
}
END_TEST


START_TEST (test_element_csymbol_laplace)
{
  const char* s = wrapMathML
  (
    "<apply>"
    "  <csymbol encoding='text' definitionURL='http://www.sbml.org/sbml/"
    "symbols/distrib/laplace'> fake_name </csymbol>"
    "  <ci> x </ci>"
    "  <cn> 0.1 </cn>"
    "</apply>\n"
  );

  N = readMathMLFromString(s);

  fail_unless( N != NULL );

  fail_unless( N->getType() == AST_DISTRIB_FUNCTION_LAPLACE );
  fail_unless( !strcmp(N->getName(), "fake_name") );
  fail_unless( N->getNumChildren() == 2       );

  F = SBML_formulaToL3String(N);
  fail_unless( !strcmp(F, "laplace(x, 0.1)") );
}
END_TEST


START_TEST (test_element_csymbol_lognormal)
{
  const char* s = wrapMathML
  (
    "<apply>"
    "  <csymbol encoding='text' definitionURL='http://www.sbml.org/sbml/"
    "symbols/distrib/lognormal'> fake_name </csymbol>"
    "  <ci> x </ci>"
    "  <cn> 0.1 </cn>"
    "</apply>\n"
  );

  N = readMathMLFromString(s);

  fail_unless( N != NULL );

  fail_unless( N->getType() == AST_DISTRIB_FUNCTION_LOGNORMAL );
  fail_unless( !strcmp(N->getName(), "fake_name") );
  fail_unless( N->getNumChildren() == 2       );

  F = SBML_formulaToL3String(N);
  fail_unless( !strcmp(F, "lognormal(x, 0.1)") );
}
END_TEST


START_TEST (test_element_csymbol_poisson)
{
  const char* s = wrapMathML
  (
    "<apply>"
    "  <csymbol encoding='text' definitionURL='http://www.sbml.org/sbml/"
    "symbols/distrib/poisson'> fake_name </csymbol>"
    "  <ci> x </ci>"
    "  <cn> 0.1 </cn>"
    "</apply>\n"
  );

  N = readMathMLFromString(s);

  fail_unless( N != NULL );

  fail_unless( N->getType() == AST_DISTRIB_FUNCTION_POISSON );
  fail_unless( !strcmp(N->getName(), "fake_name") );
  fail_unless( N->getNumChildren() == 2       );

  F = SBML_formulaToL3String(N);
  fail_unless( !strcmp(F, "poisson(x, 0.1)") );
}
END_TEST


START_TEST (test_element_csymbol_rayleigh)
{
  const char* s = wrapMathML
  (
    "<apply>"
    "  <csymbol encoding='text' definitionURL='http://www.sbml.org/sbml/"
    "symbols/distrib/rayleigh'> fake_name </csymbol>"
    "  <ci> x </ci>"
    "  <cn> 0.1 </cn>"
    "</apply>\n"
  );

  N = readMathMLFromString(s);

  fail_unless( N != NULL );

  fail_unless( N->getType() == AST_DISTRIB_FUNCTION_RAYLEIGH );
  fail_unless( !strcmp(N->getName(), "fake_name") );
  fail_unless( N->getNumChildren() == 2       );

  F = SBML_formulaToL3String(N);
  fail_unless( !strcmp(F, "rayleigh(x, 0.1)") );
}
END_TEST


Suite *
create_suite_ReadMathML ()
{
  Suite *suite = suite_create("ReadMathML");
  TCase *tcase = tcase_create("ReadMathML");

  tcase_add_checked_fixture( tcase, ReadMathML_setup, ReadMathML_teardown);

  tcase_add_test( tcase, test_element_csymbol_normal_1           );
  tcase_add_test( tcase, test_element_csymbol_normal_2           );
  tcase_add_test( tcase, test_element_csymbol_normal_3           );
  tcase_add_test( tcase, test_element_csymbol_normal_4           );
  tcase_add_test( tcase, test_element_csymbol_normal_5           );
  tcase_add_test( tcase, test_element_csymbol_normal);
  tcase_add_test( tcase, test_element_csymbol_uniform);
  tcase_add_test( tcase, test_element_csymbol_bernoulli);
  tcase_add_test( tcase, test_element_csymbol_binomial);
  tcase_add_test( tcase, test_element_csymbol_cauchy);
  tcase_add_test( tcase, test_element_csymbol_chisquare);
  tcase_add_test( tcase, test_element_csymbol_exponential);
  tcase_add_test( tcase, test_element_csymbol_gamma);
  tcase_add_test( tcase, test_element_csymbol_laplace);
  tcase_add_test( tcase, test_element_csymbol_lognormal);
  tcase_add_test( tcase, test_element_csymbol_poisson);
  tcase_add_test( tcase, test_element_csymbol_rayleigh);

  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND
