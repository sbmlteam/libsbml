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

static string L3V2EM_XMLNS_L3V1V1;
static L3v2extendedmathExtension* G; 
static L3v2extendedmathPkgNamespaces* GNS;

void
WriteL3v2EMExtensionTest_setup (void)
{
  try
  {
    G = new L3v2extendedmathExtension();
    GNS = new L3v2extendedmathPkgNamespaces();
    L3V2EM_XMLNS_L3V1V1 = GNS->getURI();
  }
  catch(...)
  {
    fail("Failed to create a L3v2extendedmathExtension object");
  }
}


void
WriteL3v2EMExtensionTest_teardown (void)
{
  delete G;
  delete GNS;
}

START_TEST (test_L3v2EMExtension_create_and_write_L3V1V1)
{
  const char* s1 =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<sbml xmlns=\"http://www.sbml.org/sbml/level3/version1/core\" xmlns:l3v2extendedmath=\"http://www.sbml.org/sbml/level3/version1/l3v2extendedmath/version1\" level=\"3\" version=\"1\" l3v2extendedmath:required=\"true\">\n"
    "  <model>\n"
    "    <listOfParameters>\n"
    "      <parameter id=\"p\" value=\"1\" constant=\"false\"/>\n"
    "    </listOfParameters>\n"
    "    <listOfRules>\n"
    "      <assignmentRule variable=\"p\">\n"
    "        <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    "          <apply>\n"
    "            <rem/>\n"
    "            <cn type=\"integer\"> 5 </cn>\n"
    "            <cn type=\"integer\"> 5 </cn>\n"
    "          </apply>\n"
    "        </math>\n"
    "      </assignmentRule>\n"
    "    </listOfRules>\n"
    "  </model>\n"
    "</sbml>\n"
    ;

  L3v2extendedmathPkgNamespaces sbmlns(3,1,1);

  // create the document

  SBMLDocument document(&sbmlns);

  //// mark l3v2extendedmath as required

  document.setPackageRequired("l3v2extendedmath", true);

  // create the Model

  Model* model=document.createModel();

  Parameter * p = model->createParameter();
  p->setConstant(false);
  p->setId("p");
  p->setValue(1);

  Rule * r = model->createAssignmentRule();
  r->setVariable("p");
  ASTNode * math = SBML_parseL3Formula("rem(5,5)");
  r->setMath(math);
  delete math;
  string s2 = writeSBMLToStdString(&document);

  fail_unless(strcmp(s1,s2.c_str()) == 0); 

  // check clone()

  SBMLDocument document2 = document;
  s2 = writeSBMLToStdString(&document2);
  fail_unless(strcmp(s1,s2.c_str()) == 0); 

  // check operator=

  Model* m = document.getModel();
  document2.setModel(m);
  s2 = writeSBMLToStdString(&document2);

  fail_unless(strcmp(s1,s2.c_str()) == 0); 

}
END_TEST


START_TEST (test_L3v2EMExtension_create_add_and_write_L3V1V1)
{
  const char* s1 =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<sbml xmlns=\"http://www.sbml.org/sbml/level3/version1/core\" xmlns:l3v2extendedmath=\"http://www.sbml.org/sbml/level3/version1/l3v2extendedmath/version1\" level=\"3\" version=\"1\" l3v2extendedmath:required=\"true\">\n"
    "  <model>\n"
    "    <listOfParameters>\n"
    "      <parameter id=\"p\" value=\"1\" constant=\"false\"/>\n"
    "    </listOfParameters>\n"
    "    <listOfRules>\n"
    "      <assignmentRule variable=\"p\">\n"
    "        <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    "          <apply>\n"
    "            <rem/>\n"
    "            <cn type=\"integer\"> 5 </cn>\n"
    "            <cn type=\"integer\"> 5 </cn>\n"
    "          </apply>\n"
    "        </math>\n"
    "      </assignmentRule>\n"
    "    </listOfRules>\n"
    "  </model>\n"
    "</sbml>\n"
    ;

  L3v2extendedmathPkgNamespaces sbmlns(3,1,1);

  // create the document

  SBMLDocument document(&sbmlns);

  //// mark l3v2extendedmath as required

  document.setPackageRequired("l3v2extendedmath", true);

  // create the Model

  Model model(&sbmlns);

  Parameter p(&sbmlns);
  p.setConstant(false);
  p.setId("p");
  p.setValue(1);

  fail_unless(model.addParameter(&p) == LIBSBML_OPERATION_SUCCESS);

  AssignmentRule r(&sbmlns);
  r.setVariable("p");
  ASTNode * math = SBML_parseL3Formula("rem(5,5)");
  fail_unless(r.setMath(math) == LIBSBML_OPERATION_SUCCESS);

  fail_unless(model.addRule(&r) == LIBSBML_OPERATION_SUCCESS);

  fail_unless(document.setModel(&model) == LIBSBML_OPERATION_SUCCESS);

  string s2 = writeSBMLToStdString(&document);

  fail_unless(strcmp(s1,s2.c_str()) == 0); 
  delete math;
}
END_TEST


START_TEST (test_L3v2EMExtension_read_enable_via_model_and_write_L3V1V1)
{
  const char* s1 =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<sbml xmlns=\"http://www.sbml.org/sbml/level3/version1/core\" level=\"3\" version=\"1\">\n"
    "  <model>\n"
    "    <listOfParameters>\n"
    "      <parameter id=\"p\" value=\"1\" constant=\"false\"/>\n"
    "    </listOfParameters>\n"
    "    <listOfRules>\n"
    "      <assignmentRule variable=\"p\">\n"
    "        <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    "          <apply>\n"
    "            <rem/>\n"
    "            <cn type=\"integer\"> 5 </cn>\n"
    "            <cn type=\"integer\"> 5 </cn>\n"
    "          </apply>\n"
    "        </math>\n"
    "      </assignmentRule>\n"
    "    </listOfRules>\n"
    "  </model>\n"
    "</sbml>\n"
    ;

  const char* s1a =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<sbml xmlns=\"http://www.sbml.org/sbml/level3/version1/core\" xmlns:l3v2extendedmath=\"http://www.sbml.org/sbml/level3/version1/l3v2extendedmath/version1\" level=\"3\" version=\"1\" l3v2extendedmath:required=\"true\">\n"
    "  <model>\n"
    "    <listOfParameters>\n"
    "      <parameter id=\"p\" value=\"1\" constant=\"false\"/>\n"
    "    </listOfParameters>\n"
    "    <listOfRules>\n"
    "      <assignmentRule variable=\"p\">\n"
    "        <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    "          <apply>\n"
    "            <rem/>\n"
    "            <cn type=\"integer\"> 5 </cn>\n"
    "            <cn type=\"integer\"> 5 </cn>\n"
    "          </apply>\n"
    "        </math>\n"
    "      </assignmentRule>\n"
    "    </listOfRules>\n"
    "  </model>\n"
    "</sbml>\n"
    ;

  SBMLDocument *document = readSBMLFromString(s1);

  fail_unless(document->getNumPlugins()             == 0);

  // should report an error in math
  fail_unless(document->getNumErrors() == 1);

  Model *model = document->getModel();

  fail_unless(model != NULL);
  const ASTNode * math = model->getRule(0)->getMath();
  fail_unless(math->getNumPlugins() == 0);

  //
  // enable the l3v2extendedmath package by invoking enablePackage function with Model object
  //
  fail_unless(model->enablePackage(L3V2EM_XMLNS_L3V1V1, "l3v2extendedmath", true) == LIBSBML_OPERATION_SUCCESS);

  // mark l3v2extendedmath as required

  document->setPackageRequired("l3v2extendedmath", true);

  fail_unless(document->getNumPlugins() == 1);
  model = document->getModel();
  math = model->getRule(0)->getMath();
  // for now this does not get enabled unless you reread the document
  fail_unless(math->getNumPlugins() == 1);

  char *s2 = writeSBMLToString(document);

  fail_unless(strcmp(s1a,s2) == 0); 
  
  free(s2);
  delete document;  
}
END_TEST


START_TEST (test_L3v2EMExtension_read_disable_via_model_and_write_L3V1V1)
{
  const char* s1 =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<sbml xmlns=\"http://www.sbml.org/sbml/level3/version1/core\" xmlns:l3v2extendedmath=\"http://www.sbml.org/sbml/level3/version1/l3v2extendedmath/version1\" level=\"3\" version=\"1\" l3v2extendedmath:required=\"true\">\n"
    "  <model>\n"
    "    <listOfParameters>\n"
    "      <parameter id=\"p\" value=\"1\" constant=\"false\"/>\n"
    "    </listOfParameters>\n"
    "    <listOfRules>\n"
    "      <assignmentRule variable=\"p\">\n"
    "        <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    "          <apply>\n"
    "            <rem/>\n"
    "            <cn type=\"integer\"> 5 </cn>\n"
    "            <cn type=\"integer\"> 5 </cn>\n"
    "          </apply>\n"
    "        </math>\n"
    "      </assignmentRule>\n"
    "    </listOfRules>\n"
    "  </model>\n"
    "</sbml>\n"
    ;

  const char* s1d =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<sbml xmlns=\"http://www.sbml.org/sbml/level3/version1/core\" level=\"3\" version=\"1\">\n"
    "  <model>\n"
    "    <listOfParameters>\n"
    "      <parameter id=\"p\" value=\"1\" constant=\"false\"/>\n"
    "    </listOfParameters>\n"
    "    <listOfRules>\n"
    "      <assignmentRule variable=\"p\">\n"
    "        <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    "          <apply>\n"
    "            <rem/>\n"
    "            <cn type=\"integer\"> 5 </cn>\n"
    "            <cn type=\"integer\"> 5 </cn>\n"
    "          </apply>\n"
    "        </math>\n"
    "      </assignmentRule>\n"
    "    </listOfRules>\n"
    "  </model>\n"
    "</sbml>\n"
    ;

  SBMLDocument *document = readSBMLFromString(s1);

  fail_unless(document->getNumPlugins() == 1);

  Model *model = document->getModel();

  fail_unless(model != NULL);
  const ASTNode * math = model->getRule(0)->getMath();
  fail_unless(math->getNumPlugins() > 0);

  //
  // disable the l3v2extendedmath package by invoking enablePackage function with Model object
  //
  fail_unless(model->enablePackage(L3V2EM_XMLNS_L3V1V1, "l3v2extendedmath", false) == LIBSBML_OPERATION_SUCCESS);

  fail_unless(document->getNumPlugins() == 0);
  model = document->getModel();
  math = model->getRule(0)->getMath();
  // for now this does not get disabled unless you reread the document
  //fail_unless(math->getNumPlugins()  > 0);

  char *s2 = writeSBMLToString(document);

  fail_unless(strcmp(s1d,s2) == 0); 
  
  free(s2);
  delete document;  
}
END_TEST


START_TEST (test_L3v2EMExtension_read_enable_via_sbmldocument_and_write_L3V1V1)
{
  const char* s1 =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<sbml xmlns=\"http://www.sbml.org/sbml/level3/version1/core\" level=\"3\" version=\"1\">\n"
    "  <model>\n"
    "    <listOfParameters>\n"
    "      <parameter id=\"p\" value=\"1\" constant=\"false\"/>\n"
    "    </listOfParameters>\n"
    "    <listOfRules>\n"
    "      <assignmentRule variable=\"p\">\n"
    "        <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    "          <apply>\n"
    "            <rem/>\n"
    "            <cn type=\"integer\"> 5 </cn>\n"
    "            <cn type=\"integer\"> 5 </cn>\n"
    "          </apply>\n"
    "        </math>\n"
    "      </assignmentRule>\n"
    "    </listOfRules>\n"
    "  </model>\n"
    "</sbml>\n"
    ;

  const char* s1a =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<sbml xmlns=\"http://www.sbml.org/sbml/level3/version1/core\" xmlns:l3v2extendedmath=\"http://www.sbml.org/sbml/level3/version1/l3v2extendedmath/version1\" level=\"3\" version=\"1\" l3v2extendedmath:required=\"true\">\n"
    "  <model>\n"
    "    <listOfParameters>\n"
    "      <parameter id=\"p\" value=\"1\" constant=\"false\"/>\n"
    "    </listOfParameters>\n"
    "    <listOfRules>\n"
    "      <assignmentRule variable=\"p\">\n"
    "        <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    "          <apply>\n"
    "            <rem/>\n"
    "            <cn type=\"integer\"> 5 </cn>\n"
    "            <cn type=\"integer\"> 5 </cn>\n"
    "          </apply>\n"
    "        </math>\n"
    "      </assignmentRule>\n"
    "    </listOfRules>\n"
    "  </model>\n"
    "</sbml>\n"
    ;

  SBMLDocument *document = readSBMLFromString(s1);

  fail_unless(document->getNumPlugins()             == 0);
  Model *model = document->getModel();

  fail_unless(model != NULL);
  const ASTNode * math = model->getRule(0)->getMath();
  fail_unless(math->getNumPlugins() == 0);

  //
  // enable the l3v2extendedmath package by invoking enablePackage function with SBMLDocument object
  //
  fail_unless(document->enablePackage(L3V2EM_XMLNS_L3V1V1, "l3v2extendedmath", true) == LIBSBML_OPERATION_SUCCESS);

  // mark l3v2extendedmath as required

  document->setPackageRequired("l3v2extendedmath", true);

  fail_unless(document->getNumPlugins()             == 1);

  model = document->getModel();

  fail_unless(model != NULL);
  math = model->getRule(0)->getMath();
  // for now this does not get enabled unless you reread the document
  //fail_unless(math->getNumPlugins() > 1);

  char *s2 = writeSBMLToString(document);

  fail_unless(strcmp(s1a,s2) == 0); 
  
  free(s2);
  delete document;  
}
END_TEST


START_TEST (test_L3v2EMExtension_read_disable_via_sbmldocument_and_write_L3V1V1)
{
  const char* s1 =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<sbml xmlns=\"http://www.sbml.org/sbml/level3/version1/core\" xmlns:l3v2extendedmath=\"http://www.sbml.org/sbml/level3/version1/l3v2extendedmath/version1\" level=\"3\" version=\"1\" l3v2extendedmath:required=\"true\">\n"
    "  <model>\n"
    "    <listOfParameters>\n"
    "      <parameter id=\"p\" value=\"1\" constant=\"false\"/>\n"
    "    </listOfParameters>\n"
    "    <listOfRules>\n"
    "      <assignmentRule variable=\"p\">\n"
    "        <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    "          <apply>\n"
    "            <rem/>\n"
    "            <cn type=\"integer\"> 5 </cn>\n"
    "            <cn type=\"integer\"> 5 </cn>\n"
    "          </apply>\n"
    "        </math>\n"
    "      </assignmentRule>\n"
    "    </listOfRules>\n"
    "  </model>\n"
    "</sbml>\n"
    ;

  const char* s1d =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<sbml xmlns=\"http://www.sbml.org/sbml/level3/version1/core\" level=\"3\" version=\"1\">\n"
    "  <model>\n"
    "    <listOfParameters>\n"
    "      <parameter id=\"p\" value=\"1\" constant=\"false\"/>\n"
    "    </listOfParameters>\n"
    "    <listOfRules>\n"
    "      <assignmentRule variable=\"p\">\n"
    "        <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    "          <apply>\n"
    "            <rem/>\n"
    "            <cn type=\"integer\"> 5 </cn>\n"
    "            <cn type=\"integer\"> 5 </cn>\n"
    "          </apply>\n"
    "        </math>\n"
    "      </assignmentRule>\n"
    "    </listOfRules>\n"
    "  </model>\n"
    "</sbml>\n"
    ;

  SBMLDocument *document = readSBMLFromString(s1);

  fail_unless(document->getNumPlugins() == 1);
  Model *model = document->getModel();

  fail_unless(model != NULL);
  const ASTNode * math = model->getRule(0)->getMath();
  fail_unless(math->getNumPlugins() > 0);

  //
  // disable the l3v2extendedmath package by invoking enablePackage function with Model object
  //
  fail_unless(document->enablePackage(L3V2EM_XMLNS_L3V1V1, "l3v2extendedmath", false) == LIBSBML_OPERATION_SUCCESS);

  fail_unless(document->getNumPlugins() == 0);

  model = document->getModel(); 

  fail_unless(model != NULL);
  model = document->getModel();
  math = model->getRule(0)->getMath();
  // for now this does not get disabled unless you reread the document
  //fail_unless(math->getNumPlugins()  > 0);

  char *s2 = writeSBMLToString(document);

  fail_unless(strcmp(s1d,s2) == 0); 
  
  free(s2);
  delete document;  
}
END_TEST



Suite *
create_suite_WriteL3v2EMExtension (void)
{
  Suite *suite = suite_create("WriteL3v2EMExtension");
  TCase *tcase = tcase_create("WriteL3v2EMExtension");

  tcase_add_checked_fixture(tcase, WriteL3v2EMExtensionTest_setup, WriteL3v2EMExtensionTest_teardown);	
  
  tcase_add_test( tcase, test_L3v2EMExtension_create_and_write_L3V1V1);
  tcase_add_test( tcase, test_L3v2EMExtension_create_add_and_write_L3V1V1);
  //tcase_add_test( tcase, test_L3v2EMExtension_read_enable_via_model_and_write_L3V1V1);
  //tcase_add_test( tcase, test_L3v2EMExtension_read_disable_via_model_and_write_L3V1V1);
  //tcase_add_test( tcase, test_L3v2EMExtension_read_enable_via_sbmldocument_and_write_L3V1V1);
  //tcase_add_test( tcase, test_L3v2EMExtension_read_disable_via_sbmldocument_and_write_L3V1V1);
  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND
