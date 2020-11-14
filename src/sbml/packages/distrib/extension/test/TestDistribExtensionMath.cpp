/**
 * @file    TestDistribExtensionMath.cpp
 * @brief   TestDistribExtensionMath unit tests
 * @author  Sarah Keating
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
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
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 * 3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 * Pasadena, CA, USA
 *
 * Copyright (C) 2002-2005 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. Japan Science and Technology Agency, Japan
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation. A copy of the license agreement is provided in the
 * file named "LICENSE.txt" included with this software distribution and also
 * available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 */

#include <sbml/common/common.h>
#include <sbml/util/util.h>
#include <sbml/math/L3Parser.h>
#include <sbml/math/L3ParserSettings.h>
#include <sbml/Model.h>

#include <check.h>

#if __cplusplus
LIBSBML_CPP_NAMESPACE_USE
CK_CPPSTART
#endif

START_TEST (test_SBML_parseL3Formula_distrib_functions)
{
  ASTNode_t *r = SBML_parseL3Formula("normal(x,y)");
  fail_unless( ASTNode_getType       (r) == AST_DISTRIB_FUNCTION_NORMAL, NULL );
  fail_unless( ASTNode_getNumChildren(r) == 2  , NULL );
  ASTNode_free(r);

  r = SBML_parseL3Formula("normal(x,y,p,q)");
  fail_unless( ASTNode_getType       (r) == AST_DISTRIB_FUNCTION_NORMAL, NULL );
  fail_unless( ASTNode_getNumChildren(r) == 4  , NULL );
  ASTNode_free(r);

  r = SBML_parseL3Formula("uniform(x,y)");
  fail_unless( ASTNode_getType       (r) == AST_DISTRIB_FUNCTION_UNIFORM, NULL );
  fail_unless( ASTNode_getNumChildren(r) == 2  , NULL );
  ASTNode_free(r);

  r = SBML_parseL3Formula("bernoulli(x)");
  fail_unless( ASTNode_getType       (r) == AST_DISTRIB_FUNCTION_BERNOULLI, NULL );
  fail_unless( ASTNode_getNumChildren(r) == 1  , NULL );
  ASTNode_free(r);

  r = SBML_parseL3Formula("binomial(x,y)");
  fail_unless( ASTNode_getType       (r) == AST_DISTRIB_FUNCTION_BINOMIAL, NULL );
  fail_unless( ASTNode_getNumChildren(r) == 2  , NULL );
  ASTNode_free(r);

  r = SBML_parseL3Formula("binomial(x,y,p,q)");
  fail_unless( ASTNode_getType       (r) == AST_DISTRIB_FUNCTION_BINOMIAL, NULL );
  fail_unless( ASTNode_getNumChildren(r) == 4  , NULL );
  ASTNode_free(r);

  r = SBML_parseL3Formula("cauchy(x,y)");
  fail_unless( ASTNode_getType       (r) == AST_DISTRIB_FUNCTION_CAUCHY, NULL );
  fail_unless( ASTNode_getNumChildren(r) == 2  , NULL );
  ASTNode_free(r);

  r = SBML_parseL3Formula("cauchy(x,y,p,q)");
  fail_unless( ASTNode_getType       (r) == AST_DISTRIB_FUNCTION_CAUCHY, NULL );
  fail_unless( ASTNode_getNumChildren(r) == 4  , NULL );
  ASTNode_free(r);

  r = SBML_parseL3Formula("chisquare(x)");
  fail_unless( ASTNode_getType       (r) == AST_DISTRIB_FUNCTION_CHISQUARE, NULL );
  fail_unless( ASTNode_getNumChildren(r) == 1  , NULL );
  ASTNode_free(r);

  r = SBML_parseL3Formula("chisquare(x,y,p)");
  fail_unless( ASTNode_getType       (r) == AST_DISTRIB_FUNCTION_CHISQUARE, NULL );
  fail_unless( ASTNode_getNumChildren(r) == 3  , NULL );
  ASTNode_free(r);

  r = SBML_parseL3Formula("exponential(x)");
  fail_unless( ASTNode_getType       (r) == AST_DISTRIB_FUNCTION_EXPONENTIAL, NULL );
  fail_unless( ASTNode_getNumChildren(r) == 1  , NULL );
  ASTNode_free(r);

  r = SBML_parseL3Formula("exponential(x,y,p)");
  fail_unless( ASTNode_getType       (r) == AST_DISTRIB_FUNCTION_EXPONENTIAL, NULL );
  fail_unless( ASTNode_getNumChildren(r) == 3  , NULL );
  ASTNode_free(r);

  r = SBML_parseL3Formula("gamma(x,y)");
  fail_unless( ASTNode_getType       (r) == AST_DISTRIB_FUNCTION_GAMMA, NULL );
  fail_unless( ASTNode_getNumChildren(r) == 2  , NULL );
  ASTNode_free(r);

  r = SBML_parseL3Formula("gamma(x,y,p,q)");
  fail_unless( ASTNode_getType       (r) == AST_DISTRIB_FUNCTION_GAMMA, NULL );
  fail_unless( ASTNode_getNumChildren(r) == 4  , NULL );
  ASTNode_free(r);

  r = SBML_parseL3Formula("laplace(x,y)");
  fail_unless( ASTNode_getType       (r) == AST_DISTRIB_FUNCTION_LAPLACE, NULL );
  fail_unless( ASTNode_getNumChildren(r) == 2  , NULL );
  ASTNode_free(r);

  r = SBML_parseL3Formula("laplace(x,y,p,q)");
  fail_unless( ASTNode_getType       (r) == AST_DISTRIB_FUNCTION_LAPLACE, NULL );
  fail_unless( ASTNode_getNumChildren(r) == 4  , NULL );
  ASTNode_free(r);

  r = SBML_parseL3Formula("lognormal(x,y)");
  fail_unless( ASTNode_getType       (r) == AST_DISTRIB_FUNCTION_LOGNORMAL, NULL );
  fail_unless( ASTNode_getNumChildren(r) == 2  , NULL );
  ASTNode_free(r);

  r = SBML_parseL3Formula("lognormal(x,y,p,q)");
  fail_unless( ASTNode_getType       (r) == AST_DISTRIB_FUNCTION_LOGNORMAL, NULL );
  fail_unless( ASTNode_getNumChildren(r) == 4  , NULL );
  ASTNode_free(r);

  r = SBML_parseL3Formula("poisson(y)");
  fail_unless( ASTNode_getType       (r) == AST_DISTRIB_FUNCTION_POISSON, NULL );
  fail_unless( ASTNode_getNumChildren(r) == 1  , NULL );
  ASTNode_free(r);

  r = SBML_parseL3Formula("poisson(y,p,q)");
  fail_unless( ASTNode_getType       (r) == AST_DISTRIB_FUNCTION_POISSON, NULL );
  fail_unless( ASTNode_getNumChildren(r) == 3  , NULL );
  ASTNode_free(r);

  r = SBML_parseL3Formula("rayleigh(x)");
  fail_unless( ASTNode_getType       (r) == AST_DISTRIB_FUNCTION_RAYLEIGH, NULL );
  fail_unless( ASTNode_getNumChildren(r) == 1  , NULL );
  ASTNode_free(r);

  r = SBML_parseL3Formula("rayleigh(x,p,q)");
  fail_unless( ASTNode_getType       (r) == AST_DISTRIB_FUNCTION_RAYLEIGH, NULL );
  fail_unless( ASTNode_getNumChildren(r) == 3  , NULL );
  ASTNode_free(r);

}
END_TEST


START_TEST (test_SBML_parseL3Formula_distrib_functions_generic)
{
  L3ParserSettings l3ps;
  l3ps.setParsePackageMath(EM_DISTRIB, L3P_PARSE_PACKAGE_MATH_AS_GENERIC);

  ASTNode_t *r = SBML_parseL3FormulaWithSettings("normal(x,y)", &l3ps);
  fail_unless( ASTNode_getType       (r) == AST_FUNCTION, NULL );
  fail_unless( ASTNode_getNumChildren(r) == 2  , NULL );
  ASTNode_free(r);

  r = SBML_parseL3FormulaWithSettings("normal(x,y,p,q)", &l3ps);
  fail_unless( ASTNode_getType       (r) == AST_FUNCTION, NULL );
  fail_unless( ASTNode_getNumChildren(r) == 4  , NULL );
  ASTNode_free(r);

  r = SBML_parseL3FormulaWithSettings("uniform(x,y)", &l3ps);
  fail_unless( ASTNode_getType       (r) == AST_FUNCTION, NULL );
  fail_unless( ASTNode_getNumChildren(r) == 2  , NULL );
  ASTNode_free(r);

  r = SBML_parseL3FormulaWithSettings("bernoulli(x)", &l3ps);
  fail_unless( ASTNode_getType       (r) == AST_FUNCTION, NULL );
  fail_unless( ASTNode_getNumChildren(r) == 1  , NULL );
  ASTNode_free(r);

  r = SBML_parseL3FormulaWithSettings("binomial(x,y)", &l3ps);
  fail_unless( ASTNode_getType       (r) == AST_FUNCTION, NULL );
  fail_unless( ASTNode_getNumChildren(r) == 2  , NULL );
  ASTNode_free(r);

  r = SBML_parseL3FormulaWithSettings("binomial(x,y,p,q)", &l3ps);
  fail_unless( ASTNode_getType       (r) == AST_FUNCTION, NULL );
  fail_unless( ASTNode_getNumChildren(r) == 4  , NULL );
  ASTNode_free(r);

  r = SBML_parseL3FormulaWithSettings("cauchy(x)", &l3ps);
  fail_unless(ASTNode_getType(r) == AST_FUNCTION, NULL);
  fail_unless(ASTNode_getNumChildren(r) == 1, NULL);
  ASTNode_free(r);

  r = SBML_parseL3FormulaWithSettings("cauchy(x,y)", &l3ps);
  fail_unless( ASTNode_getType       (r) == AST_FUNCTION, NULL );
  fail_unless( ASTNode_getNumChildren(r) == 2  , NULL );
  ASTNode_free(r);

  r = SBML_parseL3FormulaWithSettings("cauchy(x,y,p,q)", &l3ps);
  fail_unless( ASTNode_getType       (r) == AST_FUNCTION, NULL );
  fail_unless( ASTNode_getNumChildren(r) == 4  , NULL );
  ASTNode_free(r);

  r = SBML_parseL3FormulaWithSettings("chisquare(x)", &l3ps);
  fail_unless( ASTNode_getType       (r) == AST_FUNCTION, NULL );
  fail_unless( ASTNode_getNumChildren(r) == 1  , NULL );
  ASTNode_free(r);

  r = SBML_parseL3FormulaWithSettings("chisquare(x,y,p)", &l3ps);
  fail_unless( ASTNode_getType       (r) == AST_FUNCTION, NULL );
  fail_unless( ASTNode_getNumChildren(r) == 3  , NULL );
  ASTNode_free(r);

  r = SBML_parseL3FormulaWithSettings("exponential(x)", &l3ps);
  fail_unless( ASTNode_getType       (r) == AST_FUNCTION, NULL );
  fail_unless( ASTNode_getNumChildren(r) == 1  , NULL );
  ASTNode_free(r);

  r = SBML_parseL3FormulaWithSettings("exponential(x,y,p)", &l3ps);
  fail_unless( ASTNode_getType       (r) == AST_FUNCTION, NULL );
  fail_unless( ASTNode_getNumChildren(r) == 3  , NULL );
  ASTNode_free(r);

  r = SBML_parseL3FormulaWithSettings("gamma(x,y)", &l3ps);
  fail_unless( ASTNode_getType       (r) == AST_FUNCTION, NULL );
  fail_unless( ASTNode_getNumChildren(r) == 2  , NULL );
  ASTNode_free(r);

  r = SBML_parseL3FormulaWithSettings("gamma(x,y,p,q)", &l3ps);
  fail_unless( ASTNode_getType       (r) == AST_FUNCTION, NULL );
  fail_unless( ASTNode_getNumChildren(r) == 4  , NULL );
  ASTNode_free(r);

  r = SBML_parseL3FormulaWithSettings("laplace(x)", &l3ps);
  fail_unless(ASTNode_getType(r) == AST_FUNCTION, NULL);
  fail_unless(ASTNode_getNumChildren(r) == 1, NULL);
  ASTNode_free(r);

  r = SBML_parseL3FormulaWithSettings("laplace(x,y)", &l3ps);
  fail_unless( ASTNode_getType       (r) == AST_FUNCTION, NULL );
  fail_unless( ASTNode_getNumChildren(r) == 2  , NULL );
  ASTNode_free(r);

  r = SBML_parseL3FormulaWithSettings("laplace(x,y,p,q)", &l3ps);
  fail_unless( ASTNode_getType       (r) == AST_FUNCTION, NULL );
  fail_unless( ASTNode_getNumChildren(r) == 4  , NULL );
  ASTNode_free(r);

  r = SBML_parseL3FormulaWithSettings("lognormal(x,y)", &l3ps);
  fail_unless( ASTNode_getType       (r) == AST_FUNCTION, NULL );
  fail_unless( ASTNode_getNumChildren(r) == 2  , NULL );
  ASTNode_free(r);

  r = SBML_parseL3FormulaWithSettings("lognormal(x,y,p,q)", &l3ps);
  fail_unless( ASTNode_getType       (r) == AST_FUNCTION, NULL );
  fail_unless( ASTNode_getNumChildren(r) == 4  , NULL );
  ASTNode_free(r);

  r = SBML_parseL3FormulaWithSettings("poisson(y)", &l3ps);
  fail_unless( ASTNode_getType       (r) == AST_FUNCTION, NULL );
  fail_unless( ASTNode_getNumChildren(r) == 1  , NULL );
  ASTNode_free(r);

  r = SBML_parseL3FormulaWithSettings("poisson(y,p,q)", &l3ps);
  fail_unless( ASTNode_getType       (r) == AST_FUNCTION, NULL );
  fail_unless( ASTNode_getNumChildren(r) == 3  , NULL );
  ASTNode_free(r);

  r = SBML_parseL3FormulaWithSettings("rayleigh(x)", &l3ps);
  fail_unless( ASTNode_getType       (r) == AST_FUNCTION, NULL );
  fail_unless( ASTNode_getNumChildren(r) == 1  , NULL );
  ASTNode_free(r);

  r = SBML_parseL3FormulaWithSettings("rayleigh(x,p,q)", &l3ps);
  fail_unless( ASTNode_getType       (r) == AST_FUNCTION, NULL );
  fail_unless( ASTNode_getNumChildren(r) == 3  , NULL );
  ASTNode_free(r);

}
END_TEST

START_TEST (test_SBML_parseL3Formula_distrib_functions_errors)
{
  char * error;
  ASTNode_t *r = SBML_parseL3Formula("normal(x)");
  fail_unless(r == NULL, NULL);
  error = SBML_getLastParseL3Error();
  fail_unless( !strcmp(error, "Error when parsing input 'normal(x)' at position 9:  The function 'normal' takes exactly two or four arguments, but 1 were found."), NULL );
  safe_free(error);

  r = SBML_parseL3Formula("normal(x,y,p,q,d)");
  fail_unless(r == NULL, NULL);
  error = SBML_getLastParseL3Error();
  fail_unless( !strcmp(error, "Error when parsing input 'normal(x,y,p,q,d)' at position 17:  The function 'normal' takes exactly two or four arguments, but 5 were found."), NULL );
  safe_free(error);

  r = SBML_parseL3Formula("uniform()");
  fail_unless(r == NULL, NULL);
  error = SBML_getLastParseL3Error();
  fail_unless( !strcmp(error, "Error when parsing input 'uniform()' at position 9:  The function 'uniform' takes exactly two arguments, but 0 were found."), NULL );
  safe_free(error);

  r = SBML_parseL3Formula("bernoulli(x,y)");
  fail_unless(r == NULL, NULL);
  error = SBML_getLastParseL3Error();
  fail_unless( !strcmp(error, "Error when parsing input 'bernoulli(x,y)' at position 14:  The function 'bernoulli' takes exactly one argument, but 2 were found."), NULL );
  safe_free(error);

  r = SBML_parseL3Formula("binomial()");
  fail_unless(r == NULL, NULL);
  error = SBML_getLastParseL3Error();
  fail_unless( !strcmp(error, "Error when parsing input 'binomial()' at position 10:  The function 'binomial' takes exactly two or four arguments, but 0 were found."), NULL );
  safe_free(error);

  r = SBML_parseL3Formula("binomial(x,y,q)");
  fail_unless(r == NULL, NULL);
  error = SBML_getLastParseL3Error();
  fail_unless( !strcmp(error, "Error when parsing input 'binomial(x,y,q)' at position 15:  The function 'binomial' takes exactly two or four arguments, but 3 were found."), NULL );
  safe_free(error);

  r = SBML_parseL3Formula("cauchy(x,y,z)");
  fail_unless(r == NULL, NULL);
  error = SBML_getLastParseL3Error();
  fail_unless( !strcmp(error, "Error when parsing input 'cauchy(x,y,z)' at position 13:  The function 'cauchy' takes exactly two or one or four arguments, but 3 were found."), NULL );
  safe_free(error);

  r = SBML_parseL3Formula("cauchy()");
  fail_unless(r == NULL, NULL);
  error = SBML_getLastParseL3Error();
  fail_unless( !strcmp(error, "Error when parsing input 'cauchy()' at position 8:  The function 'cauchy' takes exactly two or one or four arguments, but 0 were found."), NULL );
  safe_free(error);

  r = SBML_parseL3Formula("chisquare(x,x)");
  fail_unless(r == NULL, NULL);
  error = SBML_getLastParseL3Error();
  fail_unless( !strcmp(error, "Error when parsing input 'chisquare(x,x)' at position 14:  The function 'chisquare' takes exactly one or three arguments, but 2 were found."), NULL );
  safe_free(error);

  r = SBML_parseL3Formula("chisquare(x,y,p,d,h+3)");
  fail_unless(r == NULL, NULL);
  error = SBML_getLastParseL3Error();
  fail_unless( !strcmp(error, "Error when parsing input 'chisquare(x,y,p,d,h+3)' at position 22:  The function 'chisquare' takes exactly one or three arguments, but 5 were found."), NULL );
  safe_free(error);

  r = SBML_parseL3Formula("exponential(x+4,7)");
  fail_unless(r == NULL, NULL);
  error = SBML_getLastParseL3Error();
  fail_unless( !strcmp(error, "Error when parsing input 'exponential(x+4,7)' at position 18:  The function 'exponential' takes exactly one or three arguments, but 2 were found."), NULL );
  safe_free(error);

  r = SBML_parseL3Formula("exponential(x,y,p,3)");
  fail_unless(r == NULL, NULL);
  error = SBML_getLastParseL3Error();
  fail_unless( !strcmp(error, "Error when parsing input 'exponential(x,y,p,3)' at position 20:  The function 'exponential' takes exactly one or three arguments, but 4 were found."), NULL );
  safe_free(error);

  r = SBML_parseL3Formula("gamma()");
  fail_unless(r == NULL, NULL);
  error = SBML_getLastParseL3Error();
  fail_unless( !strcmp(error, "Error when parsing input 'gamma()' at position 7:  The function 'gamma' takes exactly two or four arguments, but 0 were found."), NULL );
  safe_free(error);

  r = SBML_parseL3Formula("gamma(x)");
  fail_unless(r == NULL, NULL);
  error = SBML_getLastParseL3Error();
  fail_unless( !strcmp(error, "Error when parsing input 'gamma(x)' at position 8:  The function 'gamma' takes exactly two or four arguments, but 1 were found."), NULL );
  safe_free(error);

  r = SBML_parseL3Formula("laplace()");
  fail_unless(r == NULL, NULL);
  error = SBML_getLastParseL3Error();
  fail_unless( !strcmp(error, "Error when parsing input 'laplace()' at position 9:  The function 'laplace' takes exactly two or one or four arguments, but 0 were found."), NULL );
  safe_free(error);

  r = SBML_parseL3Formula("laplace(x,y,p)");
  fail_unless(r == NULL, NULL);
  error = SBML_getLastParseL3Error();
  fail_unless(!strcmp(error, "Error when parsing input 'laplace(x,y,p)' at position 14:  The function 'laplace' takes exactly two or one or four arguments, but 3 were found."), NULL);
  safe_free(error);

  r = SBML_parseL3Formula("laplace(x,y,p,q,l+3)");
  fail_unless(r == NULL, NULL);
  error = SBML_getLastParseL3Error();
  fail_unless( !strcmp(error, "Error when parsing input 'laplace(x,y,p,q,l+3)' at position 20:  The function 'laplace' takes exactly two or one or four arguments, but 5 were found."), NULL );
  safe_free(error);

  r = SBML_parseL3Formula("lognormal(x)");
  fail_unless(r == NULL, NULL);
  error = SBML_getLastParseL3Error();
  fail_unless( !strcmp(error, "Error when parsing input 'lognormal(x)' at position 12:  The function 'lognormal' takes exactly two or four arguments, but 1 were found."), NULL );
  safe_free(error);

  r = SBML_parseL3Formula("lognormal(x,y,p,q,z,z,z,z,z)");
  fail_unless(r == NULL, NULL);
  error = SBML_getLastParseL3Error();
  fail_unless( !strcmp(error, "Error when parsing input 'lognormal(x,y,p,q,z,z,z,z,z)' at position 28:  The function 'lognormal' takes exactly two or four arguments, but 9 were found."), NULL );
  safe_free(error);

  r = SBML_parseL3Formula("poisson()");
  fail_unless(r == NULL, NULL);
  error = SBML_getLastParseL3Error();
  fail_unless( !strcmp(error, "Error when parsing input 'poisson()' at position 9:  The function 'poisson' takes exactly one or three arguments, but 0 were found."), NULL );
  safe_free(error);

  r = SBML_parseL3Formula("poisson(y,p)");
  fail_unless(r == NULL, NULL);
  error = SBML_getLastParseL3Error();
  fail_unless( !strcmp(error, "Error when parsing input 'poisson(y,p)' at position 12:  The function 'poisson' takes exactly one or three arguments, but 2 were found."), NULL );
  safe_free(error);

  r = SBML_parseL3Formula("rayleigh()");
  fail_unless(r == NULL, NULL);
  error = SBML_getLastParseL3Error();
  fail_unless( !strcmp(error, "Error when parsing input 'rayleigh()' at position 10:  The function 'rayleigh' takes exactly one or three arguments, but 0 were found."), NULL );
  safe_free(error);

  r = SBML_parseL3Formula("rayleigh(x,p,q,d)");
  fail_unless(r == NULL, NULL);
  error = SBML_getLastParseL3Error();
  fail_unless( !strcmp(error, "Error when parsing input 'rayleigh(x,p,q,d)' at position 17:  The function 'rayleigh' takes exactly one or three arguments, but 4 were found."), NULL );
  safe_free(error);

}
END_TEST


START_TEST(test_SBML_parseL3Formula_distrib_functions_case_insensitive)
{
  ASTNode_t *r = SBML_parseL3Formula("Normal(x,y)");
  fail_unless(ASTNode_getType(r) == AST_DISTRIB_FUNCTION_NORMAL, NULL);
  fail_unless(ASTNode_getNumChildren(r) == 2, NULL);
  ASTNode_free(r);

  r = SBML_parseL3Formula("NORMAL(x,y,p,q)");
  fail_unless(ASTNode_getType(r) == AST_DISTRIB_FUNCTION_NORMAL, NULL);
  fail_unless(ASTNode_getNumChildren(r) == 4, NULL);
  ASTNode_free(r);

  r = SBML_parseL3Formula("uniForM(x,y)");
  fail_unless(ASTNode_getType(r) == AST_DISTRIB_FUNCTION_UNIFORM, NULL);
  fail_unless(ASTNode_getNumChildren(r) == 2, NULL);
  ASTNode_free(r);

  r = SBML_parseL3Formula("BERNoulli(x)");
  fail_unless(ASTNode_getType(r) == AST_DISTRIB_FUNCTION_BERNOULLI, NULL);
  fail_unless(ASTNode_getNumChildren(r) == 1, NULL);
  ASTNode_free(r);

  r = SBML_parseL3Formula("binOMial(x,y)");
  fail_unless(ASTNode_getType(r) == AST_DISTRIB_FUNCTION_BINOMIAL, NULL);
  fail_unless(ASTNode_getNumChildren(r) == 2, NULL);
  ASTNode_free(r);

  r = SBML_parseL3Formula("binomIAL(x,y,p,q)");
  fail_unless(ASTNode_getType(r) == AST_DISTRIB_FUNCTION_BINOMIAL, NULL);
  fail_unless(ASTNode_getNumChildren(r) == 4, NULL);
  ASTNode_free(r);

  r = SBML_parseL3Formula("caUchy(x,y)");
  fail_unless(ASTNode_getType(r) == AST_DISTRIB_FUNCTION_CAUCHY, NULL);
  fail_unless(ASTNode_getNumChildren(r) == 2, NULL);
  ASTNode_free(r);

  r = SBML_parseL3Formula("caucHy(x,y,p,q)");
  fail_unless(ASTNode_getType(r) == AST_DISTRIB_FUNCTION_CAUCHY, NULL);
  fail_unless(ASTNode_getNumChildren(r) == 4, NULL);
  ASTNode_free(r);

  r = SBML_parseL3Formula("chiSquare(x)");
  fail_unless(ASTNode_getType(r) == AST_DISTRIB_FUNCTION_CHISQUARE, NULL);
  fail_unless(ASTNode_getNumChildren(r) == 1, NULL);
  ASTNode_free(r);

  r = SBML_parseL3Formula("CHIsquare(x,y,p)");
  fail_unless(ASTNode_getType(r) == AST_DISTRIB_FUNCTION_CHISQUARE, NULL);
  fail_unless(ASTNode_getNumChildren(r) == 3, NULL);
  ASTNode_free(r);

  r = SBML_parseL3Formula("eXponential(x)");
  fail_unless(ASTNode_getType(r) == AST_DISTRIB_FUNCTION_EXPONENTIAL, NULL);
  fail_unless(ASTNode_getNumChildren(r) == 1, NULL);
  ASTNode_free(r);

  r = SBML_parseL3Formula("exponEntial(x,y,p)");
  fail_unless(ASTNode_getType(r) == AST_DISTRIB_FUNCTION_EXPONENTIAL, NULL);
  fail_unless(ASTNode_getNumChildren(r) == 3, NULL);
  ASTNode_free(r);

  r = SBML_parseL3Formula("Gamma(x,y)");
  fail_unless(ASTNode_getType(r) == AST_DISTRIB_FUNCTION_GAMMA, NULL);
  fail_unless(ASTNode_getNumChildren(r) == 2, NULL);
  ASTNode_free(r);

  r = SBML_parseL3Formula("GAMMa(x,y,p,q)");
  fail_unless(ASTNode_getType(r) == AST_DISTRIB_FUNCTION_GAMMA, NULL);
  fail_unless(ASTNode_getNumChildren(r) == 4, NULL);
  ASTNode_free(r);

  r = SBML_parseL3Formula("laPlace(x,y)");
  fail_unless(ASTNode_getType(r) == AST_DISTRIB_FUNCTION_LAPLACE, NULL);
  fail_unless(ASTNode_getNumChildren(r) == 2, NULL);
  ASTNode_free(r);

  r = SBML_parseL3Formula("LaPlace(x,y,p,q)");
  fail_unless(ASTNode_getType(r) == AST_DISTRIB_FUNCTION_LAPLACE, NULL);
  fail_unless(ASTNode_getNumChildren(r) == 4, NULL);
  ASTNode_free(r);

  r = SBML_parseL3Formula("logNormal(x,y)");
  fail_unless(ASTNode_getType(r) == AST_DISTRIB_FUNCTION_LOGNORMAL, NULL);
  fail_unless(ASTNode_getNumChildren(r) == 2, NULL);
  ASTNode_free(r);

  r = SBML_parseL3Formula("LOGnormal(x,y,p,q)");
  fail_unless(ASTNode_getType(r) == AST_DISTRIB_FUNCTION_LOGNORMAL, NULL);
  fail_unless(ASTNode_getNumChildren(r) == 4, NULL);
  ASTNode_free(r);

  r = SBML_parseL3Formula("poiSson(y)");
  fail_unless(ASTNode_getType(r) == AST_DISTRIB_FUNCTION_POISSON, NULL);
  fail_unless(ASTNode_getNumChildren(r) == 1, NULL);
  ASTNode_free(r);

  r = SBML_parseL3Formula("pOISSON(y,p,q)");
  fail_unless(ASTNode_getType(r) == AST_DISTRIB_FUNCTION_POISSON, NULL);
  fail_unless(ASTNode_getNumChildren(r) == 3, NULL);
  ASTNode_free(r);

  r = SBML_parseL3Formula("raYleigh(x)");
  fail_unless(ASTNode_getType(r) == AST_DISTRIB_FUNCTION_RAYLEIGH, NULL);
  fail_unless(ASTNode_getNumChildren(r) == 1, NULL);
  ASTNode_free(r);

  r = SBML_parseL3Formula("RAYLEIGH(x,p,q)");
  fail_unless(ASTNode_getType(r) == AST_DISTRIB_FUNCTION_RAYLEIGH, NULL);
  fail_unless(ASTNode_getNumChildren(r) == 3, NULL);
  ASTNode_free(r);

}
END_TEST


START_TEST(test_SBML_parseL3Formula_distrib_functions_case_sensitive)
{
  L3ParserSettings l3ps;
  l3ps.setComparisonCaseSensitivity(true);

  ASTNode_t *r = SBML_parseL3FormulaWithSettings("Normal(x,y)", &l3ps);
  fail_unless(ASTNode_getType(r) == AST_FUNCTION, NULL);
  fail_unless(ASTNode_getNumChildren(r) == 2, NULL);
  ASTNode_free(r);

  r = SBML_parseL3FormulaWithSettings("NORMAL(x,y,p,q)", &l3ps);
  fail_unless(ASTNode_getType(r) == AST_FUNCTION, NULL);
  fail_unless(ASTNode_getNumChildren(r) == 4, NULL);
  ASTNode_free(r);

  r = SBML_parseL3FormulaWithSettings("uniForm(x,y)", &l3ps);
  fail_unless(ASTNode_getType(r) == AST_FUNCTION, NULL);
  fail_unless(ASTNode_getNumChildren(r) == 2, NULL);
  ASTNode_free(r);

  r = SBML_parseL3FormulaWithSettings("bernouLLI(x)", &l3ps);
  fail_unless(ASTNode_getType(r) == AST_FUNCTION, NULL);
  fail_unless(ASTNode_getNumChildren(r) == 1, NULL);
  ASTNode_free(r);

  r = SBML_parseL3FormulaWithSettings("BiNomial(x,y)", &l3ps);
  fail_unless(ASTNode_getType(r) == AST_FUNCTION, NULL);
  fail_unless(ASTNode_getNumChildren(r) == 2, NULL);
  ASTNode_free(r);

  r = SBML_parseL3FormulaWithSettings("binOMial(x,y,p,q)", &l3ps);
  fail_unless(ASTNode_getType(r) == AST_FUNCTION, NULL);
  fail_unless(ASTNode_getNumChildren(r) == 4, NULL);
  ASTNode_free(r);

  r = SBML_parseL3FormulaWithSettings("CAUCHY(x,y)", &l3ps);
  fail_unless(ASTNode_getType(r) == AST_FUNCTION, NULL);
  fail_unless(ASTNode_getNumChildren(r) == 2, NULL);
  ASTNode_free(r);

  r = SBML_parseL3FormulaWithSettings("cauchy(x,y,p,q)", &l3ps);
  fail_unless(ASTNode_getType(r) == AST_DISTRIB_FUNCTION_CAUCHY, NULL);
  fail_unless(ASTNode_getNumChildren(r) == 4, NULL);
  ASTNode_free(r);

  r = SBML_parseL3FormulaWithSettings("chiSquare(x)", &l3ps);
  fail_unless(ASTNode_getType(r) == AST_FUNCTION, NULL);
  fail_unless(ASTNode_getNumChildren(r) == 1, NULL);
  ASTNode_free(r);

  r = SBML_parseL3FormulaWithSettings("CHIsquare(x,y,p)", &l3ps);
  fail_unless(ASTNode_getType(r) == AST_FUNCTION, NULL);
  fail_unless(ASTNode_getNumChildren(r) == 3, NULL);
  ASTNode_free(r);

  r = SBML_parseL3FormulaWithSettings("eXponential(x)", &l3ps);
  fail_unless(ASTNode_getType(r) == AST_FUNCTION, NULL);
  fail_unless(ASTNode_getNumChildren(r) == 1, NULL);
  ASTNode_free(r);

  r = SBML_parseL3FormulaWithSettings("exponEntial(x,y,p)", &l3ps);
  fail_unless(ASTNode_getType(r) == AST_FUNCTION, NULL);
  fail_unless(ASTNode_getNumChildren(r) == 3, NULL);
  ASTNode_free(r);

  r = SBML_parseL3FormulaWithSettings("Gamma(x,y)", &l3ps);
  fail_unless(ASTNode_getType(r) == AST_FUNCTION, NULL);
  fail_unless(ASTNode_getNumChildren(r) == 2, NULL);
  ASTNode_free(r);

  r = SBML_parseL3FormulaWithSettings("gammA(x,y,p,q)", &l3ps);
  fail_unless(ASTNode_getType(r) == AST_FUNCTION, NULL);
  fail_unless(ASTNode_getNumChildren(r) == 4, NULL);
  ASTNode_free(r);

  r = SBML_parseL3FormulaWithSettings("laPlace(x,y)", &l3ps);
  fail_unless(ASTNode_getType(r) == AST_FUNCTION, NULL);
  fail_unless(ASTNode_getNumChildren(r) == 2, NULL);
  ASTNode_free(r);

  r = SBML_parseL3FormulaWithSettings("LaPlace(x,y,p,q)", &l3ps);
  fail_unless(ASTNode_getType(r) == AST_FUNCTION, NULL);
  fail_unless(ASTNode_getNumChildren(r) == 4, NULL);
  ASTNode_free(r);

  r = SBML_parseL3FormulaWithSettings("logNormal(x,y)", &l3ps);
  fail_unless(ASTNode_getType(r) == AST_FUNCTION, NULL);
  fail_unless(ASTNode_getNumChildren(r) == 2, NULL);
  ASTNode_free(r);

  r = SBML_parseL3FormulaWithSettings("LOGnormal(x,y,p,q)", &l3ps);
  fail_unless(ASTNode_getType(r) == AST_FUNCTION, NULL);
  fail_unless(ASTNode_getNumChildren(r) == 4, NULL);
  ASTNode_free(r);

  r = SBML_parseL3FormulaWithSettings("Poisson(y)", &l3ps);
  fail_unless(ASTNode_getType(r) == AST_FUNCTION, NULL);
  fail_unless(ASTNode_getNumChildren(r) == 1, NULL);
  ASTNode_free(r);

  r = SBML_parseL3FormulaWithSettings("poiSSon(y,p,q)", &l3ps);
  fail_unless(ASTNode_getType(r) == AST_FUNCTION, NULL);
  fail_unless(ASTNode_getNumChildren(r) == 3, NULL);
  ASTNode_free(r);

  r = SBML_parseL3FormulaWithSettings("rayLeigh(x)", &l3ps);
  fail_unless(ASTNode_getType(r) == AST_FUNCTION, NULL);
  fail_unless(ASTNode_getNumChildren(r) == 1, NULL);
  ASTNode_free(r);

  r = SBML_parseL3FormulaWithSettings("rayleigh(x,p,q)", &l3ps);
  fail_unless(ASTNode_getType(r) == AST_DISTRIB_FUNCTION_RAYLEIGH, NULL);
  fail_unless(ASTNode_getNumChildren(r) == 3, NULL);
  ASTNode_free(r);

}
END_TEST

Suite *
create_suite_DistribExtensionMath (void)
{
  Suite *suite = suite_create("DistribExtension");
  TCase *tcase = tcase_create("DistribExtension");

  tcase_add_test( tcase, test_SBML_parseL3Formula_distrib_functions         );
  tcase_add_test( tcase, test_SBML_parseL3Formula_distrib_functions_generic );
  tcase_add_test( tcase, test_SBML_parseL3Formula_distrib_functions_errors  );
  tcase_add_test(tcase, test_SBML_parseL3Formula_distrib_functions_case_insensitive);
  tcase_add_test(tcase, test_SBML_parseL3Formula_distrib_functions_case_sensitive);

  suite_add_tcase(suite, tcase);

  return suite;
}


#if __cplusplus
CK_CPPEND
#endif
