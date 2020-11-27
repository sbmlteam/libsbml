/**
 * @file    TestWriteL3v2EMExtension.cpp
 * @brief   Unit tests of writing L3v2extendedmathExtension 
 * @author  Akiya Jouraku
 *
 * $Id: $
 * $HeadURL: $
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
 * Copyright (C) 2009-2011 jointly by the following organizations: 
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


#include <limits>

#include <iostream>
#include <check.h>
#include <sbml/SBMLTypes.h>
#include <sbml/extension/SBMLExtensionRegistry.h>
#include <sbml/packages/l3v2extendedmath/extension/L3v2extendedmathExtension.h>
#include <sbml/packages/l3v2extendedmath/common/L3v2extendedmathExtensionTypes.h>
#include <string>

/** @cond doxygenIgnored */

using namespace std;
LIBSBML_CPP_NAMESPACE_USE

/** @endcond doxygenIgnored */


CK_CPPSTART

START_TEST (test_L3v2EMExtension_infix_parser_normal)
{
  ASTNode* math = SBML_parseL3Formula("max(5,5, 10, inf, 3-2, -40)");
  fail_unless(math->getType() == AST_FUNCTION_MAX);
  fail_unless(math->getNumChildren() == 6);
  delete math;

  math = SBML_parseL3Formula("min(5,5, 10, max(x, 3-2, -40))");
  fail_unless(math->getType() == AST_FUNCTION_MIN);
  fail_unless(math->getNumChildren() == 4);
  delete math;

  math = SBML_parseL3Formula("max()");
  fail_unless(math->getType() == AST_FUNCTION_MAX);
  fail_unless(math->getNumChildren() == 0);
  delete math;

  math = SBML_parseL3Formula("min()");
  fail_unless(math->getType() == AST_FUNCTION_MIN);
  fail_unless(math->getNumChildren() == 0);
  delete math;

  math = SBML_parseL3Formula("quotient(8, x)");
  fail_unless(math->getType() == AST_FUNCTION_QUOTIENT);
  fail_unless(math->getNumChildren() == 2);
  delete math;

  math = SBML_parseL3Formula("rateOf(x)");
  fail_unless(math->getType() == AST_FUNCTION_RATE_OF);
  fail_unless(math->getNumChildren() == 1);
  delete math;

  math = SBML_parseL3Formula("rem(5,5)");
  fail_unless(math->getType() == AST_FUNCTION_REM);
  fail_unless(math->getNumChildren() == 2);
  delete math;

  math = SBML_parseL3Formula("implies(x,y)");
  fail_unless(math->getType() == AST_LOGICAL_IMPLIES);
  fail_unless(math->getNumChildren() == 2);
  delete math;

}
END_TEST


START_TEST (test_L3v2EMExtension_infix_parser_wrongargs)
{
  ASTNode* math = SBML_parseL3Formula("quotient(8, x, c)");
  fail_unless(math == NULL);
  char* error = SBML_getLastParseL3Error();
  fail_unless( strcmp(error, "Error when parsing input 'quotient(8, x, c)' at position 17:  The function 'quotient' takes exactly two arguments, but 3 were found.") == 0);
  safe_free(error);
  delete math;

  math = SBML_parseL3Formula("quotient()");
  fail_unless(math == NULL);
  error = SBML_getLastParseL3Error();
  fail_unless( strcmp(error, "Error when parsing input 'quotient()' at position 10:  The function 'quotient' takes exactly two arguments, but 0 were found.") == 0);
  safe_free(error);
  delete math;

  math = SBML_parseL3Formula("rateOf()");
  fail_unless(math == NULL);
  error = SBML_getLastParseL3Error();
  fail_unless( strcmp(error, "Error when parsing input 'rateOf()' at position 8:  The function 'rateOf' takes exactly one argument, but 0 were found.") == 0);
  safe_free(error);
  delete math;

  math = SBML_parseL3Formula("rateOf(x, y, 4)");
  fail_unless(math == NULL);
  error = SBML_getLastParseL3Error();
  fail_unless( strcmp(error, "Error when parsing input 'rateOf(x, y, 4)' at position 15:  The function 'rateOf' takes exactly one argument, but 3 were found.") == 0);
  safe_free(error);
  delete math;

  math = SBML_parseL3Formula("rem(8, x, c)");
  fail_unless(math == NULL);
  error = SBML_getLastParseL3Error();
  fail_unless( strcmp(error, "Error when parsing input 'rem(8, x, c)' at position 12:  The function 'rem' takes exactly two arguments, but 3 were found.") == 0);
  safe_free(error);
  delete math;

  math = SBML_parseL3Formula("rem()");
  fail_unless(math == NULL);
  error = SBML_getLastParseL3Error();
  fail_unless( strcmp(error, "Error when parsing input 'rem()' at position 5:  The function 'rem' takes exactly two arguments, but 0 were found.") == 0);
  safe_free(error);
  delete math;

  math = SBML_parseL3Formula("implies(8, x, c)");
  fail_unless(math == NULL);
  error = SBML_getLastParseL3Error();
  fail_unless( strcmp(error, "Error when parsing input 'implies(8, x, c)' at position 16:  The function 'implies' takes exactly two arguments, but 3 were found.") == 0);
  safe_free(error);
  delete math;

  math = SBML_parseL3Formula("implies()");
  fail_unless(math == NULL);
  error = SBML_getLastParseL3Error();
  fail_unless( strcmp(error, "Error when parsing input 'implies()' at position 9:  The function 'implies' takes exactly two arguments, but 0 were found.") == 0);
  safe_free(error);
  delete math;

}
END_TEST


START_TEST (test_L3v2EMExtension_infix_parser_capitalization)
{
  ASTNode* math = SBML_parseL3Formula("Max(5,5, 10, inf, 3-2, -40)");
  fail_unless(math->getType() == AST_FUNCTION_MAX);
  fail_unless(math->getNumChildren() == 6);
  delete math;

  math = SBML_parseL3Formula("MIN(5,5, 10, max(x, 3-2, -40))");
  fail_unless(math->getType() == AST_FUNCTION_MIN);
  fail_unless(math->getNumChildren() == 4);
  delete math;

  math = SBML_parseL3Formula("maX()");
  fail_unless(math->getType() == AST_FUNCTION_MAX);
  fail_unless(math->getNumChildren() == 0);
  delete math;

  math = SBML_parseL3Formula("Min()");
  fail_unless(math->getType() == AST_FUNCTION_MIN);
  fail_unless(math->getNumChildren() == 0);
  delete math;

  math = SBML_parseL3Formula("QuoTIENT(8, x)");
  fail_unless(math->getType() == AST_FUNCTION_QUOTIENT);
  fail_unless(math->getNumChildren() == 2);
  delete math;

  math = SBML_parseL3Formula("rateof(x)");
  fail_unless(math->getType() == AST_FUNCTION_RATE_OF);
  fail_unless(math->getNumChildren() == 1);
  delete math;

  math = SBML_parseL3Formula("REM(5,5)");
  fail_unless(math->getType() == AST_FUNCTION_REM);
  fail_unless(math->getNumChildren() == 2);
  delete math;

  math = SBML_parseL3Formula("imPLIES(x,y)");
  fail_unless(math->getType() == AST_LOGICAL_IMPLIES);
  fail_unless(math->getNumChildren() == 2);
  delete math;

}
END_TEST


START_TEST (test_L3v2EMExtension_infix_parser_no_capitalization)
{
  L3ParserSettings l3ps;
  l3ps.setComparisonCaseSensitivity(true);
  ASTNode* math = SBML_parseL3FormulaWithSettings("Max(5,5, 10, inf, 3-2, -40)", &l3ps);
  fail_unless(math->getType() == AST_FUNCTION);
  fail_unless(math->getNumChildren() == 6);
  delete math;

  math = SBML_parseL3FormulaWithSettings("MIN(5,5, 10, max(x, 3-2, -40))", &l3ps);
  fail_unless(math->getType() == AST_FUNCTION);
  fail_unless(math->getNumChildren() == 4);
  delete math;

  math = SBML_parseL3FormulaWithSettings("maX()", &l3ps);
  fail_unless(math->getType() == AST_FUNCTION);
  fail_unless(math->getNumChildren() == 0);
  delete math;

  math = SBML_parseL3FormulaWithSettings("Min()", &l3ps);
  fail_unless(math->getType() == AST_FUNCTION);
  fail_unless(math->getNumChildren() == 0);
  delete math;

  math = SBML_parseL3FormulaWithSettings("QuoTIENT(8, x)", &l3ps);
  fail_unless(math->getType() == AST_FUNCTION);
  fail_unless(math->getNumChildren() == 2);
  delete math;

  math = SBML_parseL3FormulaWithSettings("rateof(x)", &l3ps);
  fail_unless(math->getType() == AST_FUNCTION);
  fail_unless(math->getNumChildren() == 1);
  delete math;

  math = SBML_parseL3FormulaWithSettings("REM(5,5)", &l3ps);
  fail_unless(math->getType() == AST_FUNCTION);
  fail_unless(math->getNumChildren() == 2);
  delete math;

  math = SBML_parseL3FormulaWithSettings("imPLIES(x,y)", &l3ps);
  fail_unless(math->getType() == AST_FUNCTION);
  fail_unless(math->getNumChildren() == 2);
  delete math;

}
END_TEST


START_TEST (test_L3v2EMExtension_infix_parser_roundtrip)
{
  string infix = "max(5, 5, 10, INF, 3 - 2, -40)";
  ASTNode* math = SBML_parseL3Formula(infix.c_str());
  char* infix_rt = SBML_formulaToL3String(math);
  fail_unless(strcmp(infix.c_str(), infix_rt) == 0);
  safe_free(infix_rt);
  delete math;

  infix= "min(5, 5, 10, max(x, 3 - 2, -40))";
  math = SBML_parseL3Formula(infix.c_str());
  infix_rt = SBML_formulaToL3String(math);
  fail_unless(strcmp(infix.c_str(), infix_rt) == 0);
  safe_free(infix_rt);
  delete math;

  infix= "max()";
  math = SBML_parseL3Formula(infix.c_str());
  infix_rt = SBML_formulaToL3String(math);
  fail_unless(strcmp(infix.c_str(), infix_rt) == 0);
  safe_free(infix_rt);
  delete math;

  infix= "min()";
  math = SBML_parseL3Formula(infix.c_str());
  infix_rt = SBML_formulaToL3String(math);
  fail_unless(strcmp(infix.c_str(), infix_rt) == 0);
  safe_free(infix_rt);
  delete math;

  infix= "quotient(8, x)";
  math = SBML_parseL3Formula(infix.c_str());
  infix_rt = SBML_formulaToL3String(math);
  fail_unless(strcmp(infix.c_str(), infix_rt) == 0);
  safe_free(infix_rt);
  delete math;

  infix= "rateOf(x)";
  math = SBML_parseL3Formula(infix.c_str());
  infix_rt = SBML_formulaToL3String(math);
  fail_unless(strcmp(infix.c_str(), infix_rt) == 0);
  safe_free(infix_rt);
  delete math;

  infix= "rem(5, 5)";
  math = SBML_parseL3Formula(infix.c_str());
  infix_rt = SBML_formulaToL3String(math);
  fail_unless(strcmp(infix.c_str(), infix_rt) == 0);
  safe_free(infix_rt);
  delete math;

  infix= "implies(x, y)";
  math = SBML_parseL3Formula(infix.c_str());
  infix_rt = SBML_formulaToL3String(math);
  fail_unless(strcmp(infix.c_str(), infix_rt) == 0);
  safe_free(infix_rt);
  delete math;

}
END_TEST


START_TEST (test_L3v2EMExtension_infix_parser_symbols)
{
  ASTNode* math = SBML_parseL3Formula("max");
  fail_unless(math->getType() == AST_NAME);
  fail_unless( strcmp(math->getName(), "max") == 0);
  delete math;

  math = SBML_parseL3Formula("min");
  fail_unless(math->getType() == AST_NAME);
  fail_unless( strcmp(math->getName(), "min") == 0);
  delete math;

  math = SBML_parseL3Formula("quotient");
  fail_unless(math->getType() == AST_NAME);
  fail_unless( strcmp(math->getName(), "quotient") == 0);
  delete math;

  math = SBML_parseL3Formula("rateof");
  fail_unless(math->getType() == AST_NAME);
  fail_unless( strcmp(math->getName(), "rateof") == 0);
  delete math;

  math = SBML_parseL3Formula("rem");
  fail_unless(math->getType() == AST_NAME);
  fail_unless( strcmp(math->getName(), "rem") == 0);
  delete math;

  math = SBML_parseL3Formula("implies");
  fail_unless(math->getType() == AST_NAME);
  fail_unless( strcmp(math->getName(), "implies") == 0);
  delete math;

}
END_TEST


Suite *
create_suite_L3v2EMExtensionReadWriteInfix (void)
{
  Suite *suite = suite_create("ReadWriteL3v2Infix");
  TCase *tcase = tcase_create("ReadWriteL3v2Infix");

  tcase_add_test( tcase, test_L3v2EMExtension_infix_parser_normal);
  tcase_add_test( tcase, test_L3v2EMExtension_infix_parser_wrongargs);
  tcase_add_test( tcase, test_L3v2EMExtension_infix_parser_capitalization);
  tcase_add_test( tcase, test_L3v2EMExtension_infix_parser_no_capitalization);
  tcase_add_test( tcase, test_L3v2EMExtension_infix_parser_roundtrip);
  tcase_add_test( tcase, test_L3v2EMExtension_infix_parser_symbols);

  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND
