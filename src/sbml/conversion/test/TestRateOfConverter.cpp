/**
 * @file    TestRateOfConverter.cpp
 * @brief   Tests for rate of converter
 * @author  Sarah Keating
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

#include <sbml/common/common.h>

#include <sbml/SBase.h>
#include <sbml/SBMLTypes.h>

#include <sbml/conversion/SBMLConverter.h>
#include <sbml/conversion/SBMLConverterRegistry.h>
#include <sbml/conversion/SBMLRateOfConverter.h>

#include <sbml/math/FormulaParser.h>

#include <string>

#include <check.h>

using namespace std;

LIBSBML_CPP_NAMESPACE_USE

BEGIN_C_DECLS

#define XML_START   "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
#define SBML_START  "<sbml "
#define NS_L3v2     "xmlns=\"http://www.sbml.org/sbml/level3/version2/core\" "
#define LV_L3v2     "level=\"3\" version=\"2\">\n"
#define SBML_END    "</sbml>\n"

#define wrapSBML_L3v2(s)  XML_START SBML_START NS_L3v2 LV_L3v2 s SBML_END


static bool
equals (const char* expected, const char* actual)
{
  if ( !strcmp(expected, actual) ) return true;

  printf( "\nStrings are not equal:\n"  );
  printf( "Expected:\n[%s]\n", expected );
  printf( "Actual:\n[%s]\n"  , actual   );

  return false;
}

bool
usesRateOf(const ASTNode* math)
{
  if (math->getType() == AST_FUNCTION_RATE_OF) 
  {
    return true;
  }

  return false;
}

static bool
usesCSymbol(const ASTNode * math)
{
  bool used = usesRateOf(math);

  unsigned int i = 0;
  while(!used && i < math->getNumChildren())
  {
    if (usesCSymbol(math->getChild(i)))
    { 
      used = true;
    }
    i++;
  }

  return used;
}



extern char *TestDataDirectory;


START_TEST (test_conversion_rate_of_converter)
{
  const char* expectedFD = 
    "<functionDefinition id=\"rateOf\">\n"
    "  <annotation>\n"
    "    <symbols xmlns=\"http://sbml.org/annotations/symbols\" definition=\"http://en.wikipedia.org/wiki/Derivative\"/>\n"
    "  </annotation>\n"
    "  <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    "    <lambda>\n"
    "      <bvar>\n"
    "        <ci> x </ci>\n"
    "      </bvar>\n"
    "      <notanumber/>\n"
    "    </lambda>\n"
    "  </math>\n"
    "</functionDefinition>";

  const char* expectedRR = 
    "<rateRule variable=\"p2\">\n"
    "  <math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n"
    "    <apply>\n"
    "      <plus/>\n"
    "      <apply>\n"
    "        <ci> rateOf </ci>\n"
    "        <ci> p1 </ci>\n"
    "      </apply>\n"
    "      <ci> p3 </ci>\n"
    "    </apply>\n"
    "  </math>\n"
    "</rateRule>";

  string filename(TestDataDirectory);
  filename += "test_rateOf.xml";

  SBMLDocument* d = readSBMLFromFile(filename.c_str());

  fail_unless(d != NULL);

  ConversionProperties props;
  props.addOption("replaceRateOf", true,
                  "Replace rateOf with functionDefinition");

  SBMLConverter* converter = new SBMLRateOfConverter();
  converter->setProperties(&props);
  converter->setDocument(d);

  fail_unless (converter->convert() == LIBSBML_OPERATION_SUCCESS);

  FunctionDefinition *fd = d->getModel()->getFunctionDefinition(0);

  Rule *rr = d->getModel()->getRule(0);

  char * fdStr = fd->toSBML();
  char * rrStr = rr->toSBML();

  fail_unless( equals(expectedFD, fdStr) );

  fail_unless( equals(expectedRR, rrStr) );

  safe_free(rrStr);
  safe_free(fdStr);
  delete converter;
  delete d;
}
END_TEST


START_TEST (test_conversion_rate_of_converter_ia)
{
  // create test model

  SBMLDocument *doc = new SBMLDocument(3,2); 

  Model* model = doc->createModel();
  model->setId("m");

  Parameter* parameter1 = model->createParameter();
  parameter1->setId("s");
  parameter1->setConstant(false);
  parameter1->setValue(0);

  Parameter* parameter = model->createParameter();
  parameter->setId("p");
  parameter->setConstant(false);
  parameter->setValue(0);

  InitialAssignment* ia = model->createInitialAssignment();
  ia->setSymbol("p");
  ASTNode* node = SBML_parseL3Formula("rateOf(s)");
  ia->setMath(node);
  delete node;
 
  fail_unless(model->getNumFunctionDefinitions() == 0);
  
  const ASTNode* math = model->getInitialAssignment(0)->getMath();

  fail_unless(math->getType() == AST_FUNCTION_RATE_OF);
  fail_unless(math->getNumChildren() == 1);
  fail_unless(math->getChild(0)->getType() == AST_NAME);
  fail_unless(!strcmp(math->getChild(0)->getName(), "s"));
  fail_unless(usesCSymbol(math) == true);

  ConversionProperties props;
  props.addOption("replaceRateOf", true,
                  "Replace rateOf with functionDefinition");

  SBMLConverter* converter = new SBMLRateOfConverter();
  converter->setProperties(&props);
  converter->setDocument(doc);

  fail_unless (converter->convert() == LIBSBML_OPERATION_SUCCESS);

  fail_unless(model->getNumFunctionDefinitions() == 1);
  fail_unless(model->getFunctionDefinition(0)->getId() == "rateOf");
  char * formula = SBML_formulaToL3String(model->getFunctionDefinition(0)->getMath());
  fail_unless(!strcmp(formula, "lambda(x, NaN)"));
  
  math = model->getInitialAssignment(0)->getMath(); 

  fail_unless(math->getType() == AST_FUNCTION);
  fail_unless(!strcmp(math->getName(), "rateOf"));
  fail_unless(math->getNumChildren() == 1);
  fail_unless(math->getChild(0)->getType() == AST_NAME);
  fail_unless(!strcmp(math->getChild(0)->getName(), "s"));
  fail_unless(usesCSymbol(math) == false);

  safe_free(formula);
  delete converter;
  delete doc;
}
END_TEST


START_TEST (test_conversion_rate_of_converter_nested)
{
  string filename(TestDataDirectory);
  filename += "nested_rateOf.xml";

  SBMLDocument* d = readSBMLFromFile(filename.c_str());

  fail_unless(d != NULL);

  Model * model = d->getModel();

  fail_unless(model != NULL);

  fail_unless(model->getNumFunctionDefinitions() == 0);
  
  const ASTNode* math = model->getInitialAssignment(0)->getMath();

  fail_unless(math->getType() == AST_PLUS);
  fail_unless(math->getNumChildren() == 2);

  ASTNode * child = math->getChild(0);

  fail_unless(child->getType() == AST_FUNCTION_RATE_OF);
  fail_unless(child->getNumChildren() == 1);
  fail_unless(child->getChild(0)->getType() == AST_NAME);
  fail_unless(!strcmp(child->getChild(0)->getName(), "p1"));

  child = math->getChild(1);

  fail_unless(child->getType() == AST_FUNCTION_RATE_OF);
  fail_unless(child->getNumChildren() == 1);
  fail_unless(child->getChild(0)->getType() == AST_NAME);
  fail_unless(!strcmp(child->getChild(0)->getName(), "p3"));  
  
  ConversionProperties props;
  props.addOption("replaceRateOf", true,
                  "Replace rateOf with functionDefinition");

  SBMLConverter* converter = new SBMLRateOfConverter();
  converter->setProperties(&props);
  converter->setDocument(d);

  fail_unless (converter->convert() == LIBSBML_OPERATION_SUCCESS);

  fail_unless(model->getNumFunctionDefinitions() == 1);
  fail_unless(model->getFunctionDefinition(0)->getId() == "rateOf");
  char * formula = SBML_formulaToL3String(model->getFunctionDefinition(0)->getMath());
  fail_unless(!strcmp(formula, "lambda(x, NaN)"));
  safe_free(formula);
  
  math = model->getInitialAssignment(0)->getMath(); 

  fail_unless(math->getType() == AST_PLUS);
  fail_unless(math->getNumChildren() == 2);

  child = math->getChild(0);

  fail_unless(child->getType() == AST_FUNCTION);
  fail_unless(!strcmp(child->getName(), "rateOf"));
  fail_unless(child->getNumChildren() == 1);
  fail_unless(child->getChild(0)->getType() == AST_NAME);
  fail_unless(!strcmp(child->getChild(0)->getName(), "p1"));

  child = math->getChild(1);

  fail_unless(child->getType() == AST_FUNCTION);
  fail_unless(!strcmp(child->getName(), "rateOf"));
  fail_unless(child->getNumChildren() == 1);
  fail_unless(child->getChild(0)->getType() == AST_NAME);
  fail_unless(!strcmp(child->getChild(0)->getName(), "p3"));  

  delete converter;
  delete d;
}
END_TEST


START_TEST (test_conversion_rate_of_converter_multiple)
{
  string filename(TestDataDirectory);
  filename += "multiple_rateOf.xml";

  SBMLDocument* d = readSBMLFromFile(filename.c_str());

  fail_unless(d != NULL);

  Model * model = d->getModel();

  fail_unless(model != NULL);

  fail_unless(model->getNumFunctionDefinitions() == 0);
  
  const ASTNode* math = model->getConstraint(0)->getMath();
  fail_unless(usesCSymbol(math) == true);

  math = model->getReaction(0)->getKineticLaw()->getMath();
  fail_unless(usesCSymbol(math) == true);

  math = model->getEvent(0)->getTrigger()->getMath();
  fail_unless(usesCSymbol(math) == true);

  math = model->getEvent(0)->getDelay()->getMath();
  fail_unless(usesCSymbol(math) == true);

  math = model->getEvent(0)->getPriority()->getMath();
  fail_unless(usesCSymbol(math) == true);

  math = model->getEvent(0)->getEventAssignment(0)->getMath();
  fail_unless(usesCSymbol(math) == true);

  ConversionProperties props;
  props.addOption("replaceRateOf", true,
                  "Replace rateOf with functionDefinition");

  SBMLConverter* converter = new SBMLRateOfConverter();
  converter->setProperties(&props);
  converter->setDocument(d);

  fail_unless (converter->convert() == LIBSBML_OPERATION_SUCCESS);

  fail_unless(model->getNumFunctionDefinitions() == 1);
  fail_unless(model->getFunctionDefinition(0)->getId() == "rateOf");
  char * formula = SBML_formulaToL3String(model->getFunctionDefinition(0)->getMath());
  fail_unless(!strcmp(formula, "lambda(x, NaN)"));
  safe_free(formula);
  
  math = model->getConstraint(0)->getMath(); 
  fail_unless(usesCSymbol(math) == false);

  math = model->getReaction(0)->getKineticLaw()->getMath();
  fail_unless(usesCSymbol(math) == false);

  math = model->getEvent(0)->getTrigger()->getMath();
  fail_unless(usesCSymbol(math) == false);

  math = model->getEvent(0)->getDelay()->getMath();
  fail_unless(usesCSymbol(math) == false);

  math = model->getEvent(0)->getPriority()->getMath();
  fail_unless(usesCSymbol(math) == false);

  math = model->getEvent(0)->getEventAssignment(0)->getMath();
  fail_unless(usesCSymbol(math) == false);

  delete converter;
  delete d;
}
END_TEST


START_TEST (test_conversion_rate_of_converter_fromFD)
{
  string filename_in(TestDataDirectory);
  filename_in += "test_rateOf_FD.xml";

  SBMLDocument* d = readSBMLFromFile(filename_in.c_str());

  fail_unless(d != NULL);

  fail_unless(d->getModel()->getNumFunctionDefinitions() == 1);

  const ASTNode* math = d->getModel()->getRule(0)->getMath();
  fail_unless(usesCSymbol(math) == false);

  ConversionProperties props;
  props.addOption("replaceRateOf", true,
                  "Replace rateOf with functionDefinition");
  props.addOption("toFunction", false);

  SBMLConverter* converter = new SBMLRateOfConverter();
  converter->setProperties(&props);
  converter->setDocument(d);

  fail_unless (converter->convert() == LIBSBML_OPERATION_SUCCESS);

  fail_unless(d->getModel()->getNumFunctionDefinitions() == 0);

  math = d->getModel()->getRule(0)->getMath();
  fail_unless(usesCSymbol(math) == true);

  delete converter;
  delete d;
}
END_TEST


START_TEST (test_conversion_rate_of_converter_roundtrip)
{
  string filename_in(TestDataDirectory);
  filename_in += "multiple_rateOf_FD.xml";

  string filename_out(TestDataDirectory);
  filename_out += "multiple_rateOf.xml";

  SBMLDocument* d = readSBMLFromFile(filename_in.c_str());
  SBMLDocument* d1 = readSBMLFromFile(filename_out.c_str());

  fail_unless(d != NULL);

  ConversionProperties props;
  props.addOption("replaceRateOf", true,
                  "Replace rateOf with functionDefinition");
  props.addOption("toFunction", false);

  SBMLConverter* converter = new SBMLRateOfConverter();
  converter->setProperties(&props);
  converter->setDocument(d);

  fail_unless (converter->convert() == LIBSBML_OPERATION_SUCCESS);

  string out = writeSBMLToStdString(d);

  string expected = writeSBMLToStdString(d1);

  fail_unless(equals(expected.c_str(), out.c_str()));

  delete converter;
  delete d;
  delete d1;
}
END_TEST


START_TEST (test_conversion_rate_of_converter_roundtrip_1)
{
  string filename_in(TestDataDirectory);
  filename_in += "nested_rateOf.xml";

  string filename_out(TestDataDirectory);
  filename_out += "nested_rateOf_FD.xml";

  SBMLDocument* d = readSBMLFromFile(filename_in.c_str());
  SBMLDocument* d1 = readSBMLFromFile(filename_out.c_str());

  fail_unless(d != NULL);

  ConversionProperties props;
  props.addOption("replaceRateOf", true,
                  "Replace rateOf with functionDefinition");
  props.addOption("toFunction", true);

  SBMLConverter* converter = new SBMLRateOfConverter();
  converter->setProperties(&props);
  converter->setDocument(d);

  fail_unless (converter->convert() == LIBSBML_OPERATION_SUCCESS);

  string out = writeSBMLToStdString(d);

  string expected = writeSBMLToStdString(d1);

  fail_unless(equals(expected.c_str(), out.c_str()));

  delete converter;
  delete d;
  delete d1;
}
END_TEST


Suite *
create_suite_TestRateOfConverter (void)
{ 
  Suite *suite = suite_create("RateOfConverter");
  TCase *tcase = tcase_create("RateOfConverter");

  tcase_add_test(tcase, test_conversion_rate_of_converter);
  tcase_add_test(tcase, test_conversion_rate_of_converter_ia);
  tcase_add_test(tcase, test_conversion_rate_of_converter_nested);
  tcase_add_test(tcase, test_conversion_rate_of_converter_multiple);

  tcase_add_test(tcase, test_conversion_rate_of_converter_fromFD);

  tcase_add_test(tcase, test_conversion_rate_of_converter_roundtrip);
  tcase_add_test(tcase, test_conversion_rate_of_converter_roundtrip_1);

  suite_add_tcase(suite, tcase);

  return suite;
}


END_C_DECLS

