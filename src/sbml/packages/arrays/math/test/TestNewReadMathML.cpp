/**
 * \file    TestNewReadMathML.cpp
 * \brief   Read MathML unit tests
 * \author  Ben Bornstein
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2014 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2012 jointly by the following organizations: 
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
#include <sbml/math/ASTNode.h>
#include <sbml/math/MathML.h>

#include <sbml/xml/XMLNode.h>
#include <sbml/packages/arrays/extension/ArraysExtension.h>
#include <sbml/packages/arrays/extension/ArraysASTPlugin.h>

LIBSBML_CPP_NAMESPACE_USE

/**
 * Wraps the string s in the appropriate XML or MathML boilerplate.
 */
#define XML_HEADER     "<?xml version='1.0' encoding='UTF-8'?>\n"
#define MATHML_HEADER  "<math xmlns='http://www.w3.org/1998/Math/MathML'>\n"
#define MATHML_HEADER_UNITS  "<math xmlns='http://www.w3.org/1998/Math/MathML'\n"
#define MATHML_HEADER_UNITS2  " xmlns:sbml='http://www.sbml.org/sbml/level3/version1/core'>\n"
#define MATHML_FOOTER  "</math>"

#define wrapXML(s)     XML_HEADER s
#define wrapMathML(s)  XML_HEADER MATHML_HEADER s MATHML_FOOTER
#define wrapMathMLUnits(s)  XML_HEADER MATHML_HEADER_UNITS MATHML_HEADER_UNITS2 s MATHML_FOOTER


static ASTNode* N;
static char*    F;
static XMLNamespaces * NS;


static void
NewReadMathML_setup ()
{
  N = NULL;
  F = NULL;
  NS = new XMLNamespaces();
  NS->add("http://www.sbml.org/sbml/level3/version1/arrays/version1", "arrays");
}


static void
NewReadMathML_teardown ()
{
  delete N;
  delete NS;
  free(F);
}


CK_CPPSTART


START_TEST (test_element_vector)
{
  const char* s = wrapMathML
  (
    "<vector>"
    "  <apply>"
    "    <cos/>"
    "    <cn type=\"integer\"> 5 </cn>"
    "  </apply>"
    "  <ci> y </ci>"
    "</vector>\n"
  );



  N = readMathMLFromStringWithNamespaces(s, NS);

  fail_unless( N != NULL );
  fail_unless( N->getType() == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( N->getExtendedType() == AST_LINEAR_ALGEBRA_VECTOR_CONSTRUCTOR);
  fail_unless( N->getNumChildren() == 2);
  fail_unless( N->getPackageName() == "arrays");

  ASTNode * child = N->getChild(0);

  fail_unless( child != NULL );
  fail_unless( child->getType() == AST_FUNCTION_COS);
  fail_unless( child->getExtendedType() == AST_FUNCTION_COS);
  fail_unless( child->getNumChildren() == 1);
  fail_unless( child->getPackageName() == "core");

  ASTNode *c1 = child->getChild(0);

  fail_unless( c1 != NULL );
  fail_unless( c1->getType() == AST_INTEGER);
  fail_unless( c1->getExtendedType() == AST_INTEGER);
  fail_unless( c1->getNumChildren() == 0);
  fail_unless( c1->getInteger() == 5);

  child = N->getChild(1);

  fail_unless( child != NULL );
  fail_unless( child->getType() == AST_NAME);
  fail_unless( child->getExtendedType() == AST_NAME);
  fail_unless( strcmp(child->getName(), "y") == 0);
  fail_unless( child->getNumChildren() == 0);


  ArraysASTPlugin* plugin = static_cast<ArraysASTPlugin*>(N->getPlugin("arrays"));
  
  fail_unless(plugin != NULL);
  fail_unless(plugin->getASTType() == AST_LINEAR_ALGEBRA_VECTOR_CONSTRUCTOR);


  plugin = static_cast<ArraysASTPlugin*>(child->getPlugin("arrays"));
  
  fail_unless(plugin != NULL);
  fail_unless(plugin->getASTType() == AST_ARRAYS_UNKNOWN);

}
END_TEST



#if (0)
START_TEST (test_element_matrix)
{
  const char* s = wrapMathML
  (
    "<matrix>"
    "<matrixrow>"
    "<apply>"
    "<cos/>"
    "<cn type=\"integer\"> 5 </cn>"
    "</apply>"
    "<ci> y </ci>"
    "</matrixrow>"
    "<matrixrow>"
    "<cn type=\"integer\"> 2 </cn>"
    "<apply>"
    "<cos/>"
    "<cn type=\"integer\"> 5 </cn>"
    "</apply>"
    "</matrixrow>"
    "</matrix>\n"
  );



  N = readMathMLFromStringWithNamespaces(s, NS);

  fail_unless( N != NULL );
  fail_unless( N->getType() == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( N->getExtendedType() == AST_LINEAR_ALGEBRA_MATRIX_CONSTRUCTOR);
  fail_unless( N->getNumChildren() == 2);
  fail_unless( N->getPackageName() == "arrays");

  ASTNode * child = N->getChild(0);

  fail_unless( child != NULL );
  fail_unless( child->getType() == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( child->getExtendedType() == AST_LINEAR_ALGEBRA_MATRIXROW_CONSTRUCTOR);
  fail_unless( child->getNumChildren() == 2);
  fail_unless( child->getPackageName() == "arrays");

  ASTNode *c1 = child->getChild(0);

  fail_unless( c1 != NULL );
  fail_unless( c1->getType() == AST_FUNCTION_COS);
  fail_unless( c1->getExtendedType() == AST_FUNCTION_COS);
  fail_unless( c1->getNumChildren() == 1);
  fail_unless( c1->getPackageName() == "core");

  child = N->getChild(1);

  fail_unless( child != NULL );
  fail_unless( child->getType() == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( child->getExtendedType() == AST_LINEAR_ALGEBRA_MATRIXROW_CONSTRUCTOR);
  fail_unless( child->getNumChildren() == 2);
  fail_unless( child->getPackageName() == "arrays");

  c1 = child->getChild(0);

  fail_unless( c1 != NULL );
  fail_unless( c1->getType() == AST_INTEGER);
  fail_unless( c1->getExtendedType() == AST_INTEGER);
  fail_unless( c1->getNumChildren() == 0);
  fail_unless( c1->getInteger() == 2);
  
  ArraysASTPlugin* plugin = static_cast<ArraysASTPlugin*>(N->getPlugin("arrays"));
  
  fail_unless(plugin != NULL);
  fail_unless(plugin->getASTType() == AST_LINEAR_ALGEBRA_MATRIX_CONSTRUCTOR);

}
END_TEST
#endif


#if (0)
START_TEST (test_element_determinant)
{
  const char* s = wrapMathML
  (
    "  <apply>"
    "    <determinant/>"
    "    <ci> A </ci>"
    "  </apply>"
  );



  N = readMathMLFromStringWithNamespaces(s, NS);

  fail_unless( N != NULL );
  fail_unless( N->getType() == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( N->getExtendedType() == AST_LINEAR_ALGEBRA_DETERMINANT);
  fail_unless( N->getNumChildren() == 1);
  fail_unless( N->getPackageName() == "arrays");

  ASTNode * child = N->getChild(0);

  fail_unless( child != NULL );
  fail_unless( child->getType() == AST_NAME);
  fail_unless( child->getExtendedType() == AST_NAME);
  fail_unless( strcmp(child->getName(), "A") == 0);
  fail_unless( child->getNumChildren() == 0);
  fail_unless( child->getPackageName() == "core");

  ArraysASTPlugin* plugin = static_cast<ArraysASTPlugin*>(N->getPlugin("arrays"));
  
  fail_unless(plugin != NULL);
  fail_unless(plugin->getASTType() == AST_LINEAR_ALGEBRA_DETERMINANT);

}
END_TEST
#endif


#if (0)
START_TEST (test_element_vectorproduct)
{
  const char* s = wrapMathML
  (
    "  <apply>"
    "    <vectorproduct/>"
    "    <ci> A </ci>"
    "    <ci> B </ci>"
    "  </apply>"
  );



  N = readMathMLFromStringWithNamespaces(s, NS);

  fail_unless( N != NULL );
  fail_unless( N->getType() == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( N->getExtendedType() == AST_LINEAR_ALGEBRA_VECTOR_PRODUCT);
  fail_unless( N->getNumChildren() == 2);
  fail_unless( N->getPackageName() == "arrays");

  ASTNode * child = N->getChild(0);

  fail_unless( child != NULL );
  fail_unless( child->getType() == AST_NAME);
  fail_unless( child->getExtendedType() == AST_NAME);
  fail_unless( strcmp(child->getName(), "A") == 0);
  fail_unless( child->getNumChildren() == 0);

  child = N->getChild(1);

  fail_unless( child != NULL );
  fail_unless( child->getType() == AST_NAME);
  fail_unless( child->getExtendedType() == AST_NAME);
  fail_unless( strcmp(child->getName(), "B") == 0);
  fail_unless( child->getNumChildren() == 0);
  fail_unless( child->getPackageName() == "core");

  ArraysASTPlugin* plugin = static_cast<ArraysASTPlugin*>(N->getPlugin("arrays"));
  
  fail_unless(plugin != NULL);
  fail_unless(plugin->getASTType() == AST_LINEAR_ALGEBRA_VECTOR_PRODUCT);

}
END_TEST
#endif



START_TEST (test_element_selector)
{
  const char* s = wrapMathML
  (
    "  <apply>"
    "    <selector/>"
    "    <ci> A </ci>"
    "    <ci> i </ci>"
    "  </apply>"
  );



  N = readMathMLFromStringWithNamespaces(s, NS);

  fail_unless( N != NULL );
  fail_unless( N->getType() == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( N->getExtendedType() == AST_LINEAR_ALGEBRA_SELECTOR);
  fail_unless( N->getNumChildren() == 2);
  fail_unless( N->getPackageName() == "arrays");

  ASTNode * child = N->getChild(0);

  fail_unless( child != NULL );
  fail_unless( child->getType() == AST_NAME);
  fail_unless( child->getExtendedType() == AST_NAME);
  fail_unless( strcmp(child->getName(), "A") == 0);
  fail_unless( child->getNumChildren() == 0);
  fail_unless( child->getPackageName() == "core");

  child = N->getChild(1);

  fail_unless( child != NULL );
  fail_unless( child->getType() == AST_NAME);
  fail_unless( child->getExtendedType() == AST_NAME);
  fail_unless( strcmp(child->getName(), "i") == 0);
  fail_unless( child->getNumChildren() == 0);

  ArraysASTPlugin* plugin = static_cast<ArraysASTPlugin*>(N->getPlugin("arrays"));
  
  fail_unless(plugin != NULL);
  fail_unless(plugin->getASTType() == AST_LINEAR_ALGEBRA_SELECTOR);

}
END_TEST



START_TEST (test_element_sum)
{
  const char* s = wrapMathML
  (
    "  <apply>"
    "    <sum/>"
    "    <bvar><ci> x </ci></bvar>"
    "    <lowlimit><cn type=\"integer\">0</cn></lowlimit>"
    "    <ci> A </ci>"
    "  </apply>"
  );



  N = readMathMLFromStringWithNamespaces(s, NS);

  fail_unless( N != NULL );
  fail_unless( N->getType() == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( N->getExtendedType() == AST_SERIES_SUM);
  fail_unless( N->getNumChildren() == 3);
  fail_unless( N->getPackageName() == "arrays");

  ASTNode * child = N->getChild(0);

  fail_unless( child != NULL );
  fail_unless( child->getType() == AST_QUALIFIER_BVAR);
  fail_unless( child->getExtendedType() == AST_QUALIFIER_BVAR);
  fail_unless( child->getNumChildren() == 1);
  fail_unless( child->getPackageName() == "core");

  ASTNode * child1 = child->getChild(0);

  fail_unless( child1 != NULL );
  fail_unless( child1->getType() == AST_NAME);
  fail_unless( child1->getExtendedType() == AST_NAME);
  fail_unless( strcmp(child1->getName(), "x") == 0);
  fail_unless( child1->getNumChildren() == 0);
  fail_unless( child1->getPackageName() == "core");

  child = N->getChild(1);

  fail_unless( child != NULL );
  fail_unless( child->getType() == AST_ORIGINATES_IN_PACKAGE);
  fail_unless( child->getExtendedType() == AST_QUALIFIER_LOWLIMIT);
  fail_unless( child->getNumChildren() == 1);
  fail_unless( child->getPackageName() == "arrays");

  child1 = child->getChild(0);

  fail_unless( child1 != NULL );
  fail_unless( child1->getType() == AST_INTEGER);
  fail_unless( child1->getExtendedType() == AST_INTEGER);
  fail_unless( child1->getNumChildren() == 0);
  fail_unless( child1->getInteger() == 0);

  child1 = N->getChild(2);

  fail_unless( child1 != NULL );
  fail_unless( child1->getType() == AST_NAME);
  fail_unless( child1->getExtendedType() == AST_NAME);
  fail_unless( strcmp(child1->getName(), "A") == 0);
  fail_unless( child1->getNumChildren() == 0);
  fail_unless( child1->getPackageName() == "core");

  ArraysASTPlugin* plugin = static_cast<ArraysASTPlugin*>(N->getPlugin("arrays"));
  
  fail_unless(plugin != NULL);
  fail_unless(plugin->getASTType() == AST_SERIES_SUM);

}
END_TEST




Suite *
create_suite_NewReadMathML ()
{
  Suite *suite = suite_create("NewReadMathML");
  TCase *tcase = tcase_create("NewReadMathML");

  tcase_add_checked_fixture( tcase, NewReadMathML_setup, NewReadMathML_teardown);

  tcase_add_test( tcase, test_element_vector                      );
#if (0)
  tcase_add_test( tcase, test_element_matrix                      );
  tcase_add_test( tcase, test_element_determinant                      );
  tcase_add_test( tcase, test_element_vectorproduct                      );
#endif
  tcase_add_test( tcase, test_element_selector                      );
  tcase_add_test( tcase, test_element_sum                      );

  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND
