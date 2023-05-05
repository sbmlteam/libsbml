/**
 * @file    TestSBMLRateRuleConverter.cpp
 * @brief   Tests for raterule to reaction converter
 * @author  Sarah Keating
 * @author  Alessandro Felder
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
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
#include <sbml/conversion/SBMLRateRuleConverter.h>

#include <sbml/math/FormulaParser.h>

#include <string>
#include <iostream>
#include <check.h>

using namespace std;
LIBSBML_CPP_NAMESPACE_USE
BEGIN_C_DECLS

static bool
equals(const char* expected, const char* actual)
{
  if (!strcmp(expected, actual)) return true;

  printf("\nStrings are not equal:\n");
  printf("Expected:\n[%s]\n", expected);
  printf("Actual:\n[%s]\n", actual);

  return false;
}

extern char *TestDataDirectory;



// helper function to set up a parameter with 0 value
Parameter* setupZeroParameter(Model* model, const char* name, bool is_constant)
{
	Parameter* parameter = model->createParameter();
	parameter->setId(name);
	parameter->setConstant(is_constant);
	parameter->setValue(0);
	return parameter;
}



extern char *TestDataDirectory;

START_TEST(test_conversion_raterule_converter_invalid)
{
  ConversionProperties props;
  props.addOption("inferReactions", true);

  SBMLConverter* converter = new SBMLRateRuleConverter();
  converter->setProperties(&props);

  // test NULL document
  SBMLDocument* doc = NULL;
  converter->setDocument(doc);

  fail_unless(converter->convert() == LIBSBML_INVALID_OBJECT);

  // test NULL model
  doc = new SBMLDocument(3, 2);
  converter->setDocument(doc);

  fail_unless(converter->convert() == LIBSBML_INVALID_OBJECT);

  // create model no rules
  Model* model = doc->createModel();
  model->setId("m");

  converter->setDocument(doc);

  fail_unless(converter->convert() == LIBSBML_OPERATION_SUCCESS);

  Parameter* parameter1 = model->createParameter();
  parameter1->setId("s");
  parameter1->setConstant(false);
  parameter1->setValue(0);

  Parameter* parameter = model->createParameter();
  parameter->setId("p");
  parameter->setConstant(false);
  parameter->setValue(0);

  RateRule* rr1 = model->createRateRule();
  rr1->setVariable("p");

  RateRule* rr2 = model->createRateRule();
  rr2->setVariable("p");

  // invalid document
  converter->setDocument(doc);

  fail_unless(converter->convert() == LIBSBML_CONV_INVALID_SRC_DOCUMENT);

  delete converter;
  delete doc;
}
END_TEST


START_TEST(test_conversion_raterule_converter)
{
  ConversionProperties props;
  props.addOption("inferReactions", true);

  SBMLConverter* converter = new SBMLRateRuleConverter();
  converter->setProperties(&props);

  SBMLDocument* doc = new SBMLDocument(3, 2);
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

  parameter = model->createParameter();
  parameter->setId("k");
  parameter->setConstant(true);
  parameter->setValue(0);

  RateRule* rr1 = model->createRateRule();
  rr1->setVariable("s");
  ASTNode *math = SBML_parseL3Formula("-k*s");
  rr1->setMath(math);
  delete math;

  RateRule* rr2 = model->createRateRule();
  rr2->setVariable("p");
  math = SBML_parseL3Formula("k*s");
  rr2->setMath(math);
  delete math;


  fail_unless(doc->getModel()->getNumCompartments() == 0);
  fail_unless(doc->getModel()->getNumSpecies() == 0);
  fail_unless(doc->getModel()->getNumParameters() == 3);
  fail_unless(doc->getModel()->getNumRules() == 2);
  fail_unless(doc->getModel()->getNumReactions() == 0);

  // s = -k*s
  // p = k*s

  converter->setDocument(doc);
  fail_unless(converter->convert() == LIBSBML_OPERATION_SUCCESS);

  fail_unless(doc->getModel()->getNumCompartments() == 1);
  fail_unless(doc->getModel()->getNumSpecies() == 2);
  fail_unless(doc->getModel()->getNumParameters() == 1);
  fail_unless(doc->getModel()->getNumRules() == 0);
  fail_unless(doc->getModel()->getNumReactions() == 1);

  Reaction *r = doc->getModel()->getReaction(0);
  fail_unless(r->getReversible() == false);
  fail_unless(r->getNumReactants() == 1);
  fail_unless(r->getNumProducts() == 1);
  fail_unless(r->getNumModifiers() == 0);
  fail_unless(r->isSetKineticLaw());

  SpeciesReference *sr = r->getReactant(0);
  fail_unless(sr->getSpecies() == "s");
  fail_unless(util_isEqual(sr->getStoichiometry(), 1.0));

  sr = r->getProduct(0);
  fail_unless(sr->getSpecies() == "p");
  fail_unless(util_isEqual(sr->getStoichiometry(), 1.0));

  const char * kl = SBML_formulaToL3String(r->getKineticLaw()->getMath());
  fail_unless(strcmp(kl, "k*s"));
  safe_free((char *)kl);

  delete converter;
  delete doc;
}
END_TEST

START_TEST(test_conversion_raterule_converter_non_standard_stoichiometry)
{
	// example 3.13 in Fages et al, TCS, 2015
	// Simple example of dx/dt = -2*k*x = - dy/dt
	// Unlike typical converters of Mass action reactions that would give x -> y with f=2*k*x,
	// this converter give (the equivalent) 2*x -> 2*y with f=k*x
	
	ConversionProperties props;
	props.addOption("inferReactions", true);

	SBMLConverter* converter = new SBMLRateRuleConverter();
	converter->setProperties(&props);

	SBMLDocument* doc = new SBMLDocument(3, 2);
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

	parameter = model->createParameter();
	parameter->setId("k");
	parameter->setConstant(true);
	parameter->setValue(0);

	RateRule* rr1 = model->createRateRule();
	rr1->setVariable("s");
	ASTNode* math = SBML_parseL3Formula("-2*k*s");
	rr1->setMath(math);
	delete math;

	RateRule* rr2 = model->createRateRule();
	rr2->setVariable("p");
	math = SBML_parseL3Formula("2*k*s");
	rr2->setMath(math);
	delete math;


	fail_unless(doc->getModel()->getNumCompartments() == 0);
	fail_unless(doc->getModel()->getNumSpecies() == 0);
	fail_unless(doc->getModel()->getNumParameters() == 3);
	fail_unless(doc->getModel()->getNumRules() == 2);
	fail_unless(doc->getModel()->getNumReactions() == 0);

	converter->setDocument(doc);
	fail_unless(converter->convert() == LIBSBML_OPERATION_SUCCESS);

	fail_unless(doc->getModel()->getNumCompartments() == 1);
	fail_unless(doc->getModel()->getNumSpecies() == 2);
	fail_unless(doc->getModel()->getNumParameters() == 1);
	fail_unless(doc->getModel()->getNumRules() == 0);
	fail_unless(doc->getModel()->getNumReactions() == 1);

	Reaction* r = doc->getModel()->getReaction(0);
	fail_unless(r->getNumReactants() == 1);
	fail_unless(r->getNumProducts() == 1);
	fail_unless(r->getNumModifiers() == 0);
	fail_unless(r->isSetKineticLaw());

	SpeciesReference* sr = r->getReactant(0);
	fail_unless(sr->getSpecies() == "s");
	fail_unless(util_isEqual(sr->getStoichiometry(), 2.0));

	sr = r->getProduct(0);
	fail_unless(sr->getSpecies() == "p");
	fail_unless(util_isEqual(sr->getStoichiometry(), 2.0));

	const char* kl = SBML_formulaToL3String(r->getKineticLaw()->getMath());
	fail_unless(strcmp(kl, "k*s"));
	safe_free((char*)kl);
}
END_TEST

START_TEST(test_conversion_raterule_converter_hidden_variable)
{
	// example 3.7 in Fages et al, TCS, 2015 (circadian clock)
	// requires implementation of Algo 3.1 (hidden variable inference) to give correct result
	// The additional ODE dMPFi/dt = -k1*MPFi*Cdc25 + k2*MPF*Wee1 should be created by 3.1.
	// @alessandrofelder has visually checked that the expected (incorrect) result is 
	// obtained in the absence of an implementation of 3.1.
	ConversionProperties props;
	props.addOption("inferReactions", true);

	SBMLConverter* converter = new SBMLRateRuleConverter();
	converter->setProperties(&props);

	SBMLDocument* doc = new SBMLDocument(3, 2);
	Model* model = doc->createModel();
	model->setId("m");

	// MPF, Cdc25, Wee1, Clock (variables)
	Parameter* mpf = setupZeroParameter(model, "MPF", false);
	Parameter* cdc25 = setupZeroParameter(model, "Cdc25", false);
	Parameter* wee1 = setupZeroParameter(model, "Wee1", false);
	Parameter* clock = setupZeroParameter(model, "Clock", false);

	// c, k1, k2, k3, k4 (constants)
	Parameter* c = setupZeroParameter(model, "c", true);
	Parameter* k1 = setupZeroParameter(model, "k1", true);
	Parameter* k2 = setupZeroParameter(model, "k2", true);
	Parameter* k3 = setupZeroParameter(model, "k3", true);
	Parameter* k4 = setupZeroParameter(model, "k4", true);

	RateRule* rr1 = model->createRateRule();
	rr1->setVariable("MPF");
	ASTNode* math = SBML_parseL3Formula("k1*(c-MPF)*Cdc25-k2*MPF*Wee1");
	rr1->setMath(math);
	delete math;

	RateRule* rr2 = model->createRateRule();
	rr2->setVariable("Wee1");
	math = SBML_parseL3Formula("k3/(k4+Clock)");
	rr2->setMath(math);
	delete math;

	RateRule* rr3 = model->createRateRule();
	rr3->setVariable("Cdc25");
	math = SBML_parseL3Formula("0");
	rr3->setMath(math);
	delete math;

	RateRule* rr4 = model->createRateRule();
	rr4->setVariable("Clock");
	math = SBML_parseL3Formula("0");
	rr4->setMath(math);
	delete math;

	fail_unless(doc->getModel()->getNumCompartments() == 0);
	fail_unless(doc->getModel()->getNumSpecies() == 0);
	fail_unless(doc->getModel()->getNumParameters() == 9);
	fail_unless(doc->getModel()->getNumRules() == 4);
	fail_unless(doc->getModel()->getNumReactions() == 0);

  converter->setDocument(doc);
	fail_unless(converter->convert() == LIBSBML_OPERATION_SUCCESS);

	fail_unless(doc->getModel()->getNumCompartments() == 1);
	fail_unless(doc->getModel()->getNumSpecies() == 5); // should be first failure while 3.1. is missing.
	fail_unless(doc->getModel()->getNumParameters() == 5);
	fail_unless(doc->getModel()->getNumRules() == 0);
	fail_unless(doc->getModel()->getNumReactions() == 3);

	// references for involved species
	SpeciesReference* srMpfi;
	SpeciesReference* srCdc25;
	SpeciesReference* srMpf;
	SpeciesReference* srWee1;
	ModifierSpeciesReference* srClock;

	// Reaction 0
	// MPFi + Cdc25 -> MPF + Cdc25 with f=k1*MPFi*Cdc25
	Reaction* r0 = doc->getModel()->getReaction(0);
	fail_unless(r0->getNumReactants() == 2);
	fail_unless(r0->getNumProducts() == 2);
	fail_unless(r0->getNumModifiers() == 0);
	fail_unless(r0->isSetKineticLaw());

	// reactants

	srCdc25 = r0->getReactant(0);
	fail_unless(srCdc25->getSpecies() == "Cdc25");
	fail_unless(util_isEqual(srCdc25->getStoichiometry(), 1));

	srMpfi = r0->getReactant(1);
	fail_unless(srMpfi->getSpecies() == "newVar1");
	fail_unless(util_isEqual(srMpfi->getStoichiometry(), 1));

	// products
	srMpf = r0->getProduct(0);
	fail_unless(srMpf->getSpecies() == "MPF");
	fail_unless(util_isEqual(srMpf->getStoichiometry(), 1.0));

	srCdc25 = r0->getProduct(1);
	fail_unless(srCdc25->getSpecies() == "Cdc25");
	fail_unless(util_isEqual(srCdc25->getStoichiometry(), 1.0));

	// kinetic law
	const char* kl = SBML_formulaToL3String(r0->getKineticLaw()->getMath());
	fail_unless(strcmp(kl, "k1*z9*Cdc25"));
	safe_free((char*)kl);

	// Reaction 1
	// MPF + Wee1 -> MPFi + Wee1 with f=k2*MPF*Wee1
	Reaction* r1 = doc->getModel()->getReaction(1);
	fail_unless(r1->getNumReactants() == 2);
	fail_unless(r1->getNumProducts() == 2);
	fail_unless(r1->getNumModifiers() == 0);
	fail_unless(r1->isSetKineticLaw());

	// reactants
	srMpf = r1->getReactant(0);
	fail_unless(srMpf->getSpecies() == "MPF");
	fail_unless(util_isEqual(srMpf->getStoichiometry(), 1));

	srWee1 = r1->getReactant(1);
	fail_unless(srWee1->getSpecies() == "Wee1");
	fail_unless(util_isEqual(srWee1->getStoichiometry(), 1));

	// products
	srWee1 = r1->getProduct(0);
	fail_unless(srWee1->getSpecies() == "Wee1");
	fail_unless(util_isEqual(srWee1->getStoichiometry(), 1.0));

	srMpfi = r1->getProduct(1);
	fail_unless(srMpfi->getSpecies() == "newVar1");
	fail_unless(util_isEqual(srMpfi->getStoichiometry(), 1.0));

	// kinetic law
	kl = SBML_formulaToL3String(r1->getKineticLaw()->getMath());
	fail_unless(strcmp(kl, "k2*MPF*Cdc25"));
	safe_free((char*)kl);


	// Reaction 2
	// 0/Clock -> Wee1 with f=k3/(k4+Clock)
	Reaction* r2 = doc->getModel()->getReaction(2);
	fail_unless(r2->getNumReactants() == 0);
	fail_unless(r2->getNumProducts() == 1);
	fail_unless(r2->getNumModifiers() == 1);
	fail_unless(r2->isSetKineticLaw());

	// modifier
	srClock = r2->getModifier(0);
	fail_unless(srClock->getSpecies() == "Clock");

	// products
	srWee1 = r2->getProduct(0);
	fail_unless(srWee1->getSpecies() == "Wee1");
	fail_unless(util_isEqual(srWee1->getStoichiometry(), 1.0));

	// kinetic law
	kl = SBML_formulaToL3String(r1->getKineticLaw()->getMath());
	fail_unless(strcmp(kl, "k3/(k4+Clock)"));
	safe_free((char*)kl);
}
END_TEST

START_TEST(test_model)
{
  ConversionProperties props;
  props.addOption("inferReactions", true);

  SBMLConverter* converter = new SBMLRateRuleConverter();
  converter->setProperties(&props);

  std::string filename(TestDataDirectory);
  filename += "mraterules.xml";
  std::string filename1(TestDataDirectory);
  filename1 += "mreact.xml";

  SBMLDocument* d = readSBMLFromFile(filename.c_str());

  converter->setDocument(d);
  fail_unless(converter->convert() == LIBSBML_OPERATION_SUCCESS);

  SBMLDocument* d1 = readSBMLFromFile(filename1.c_str());
  std::string out = writeSBMLToStdString(d);
  std::string expected = writeSBMLToStdString(d1);

  fail_unless(equals(expected.c_str(), out.c_str()));

  delete converter;
  delete d;
  delete d1;
}
END_TEST


START_TEST(test_model1)
{
  ConversionProperties props;
  props.addOption("inferReactions", true);

  SBMLConverter* converter = new SBMLRateRuleConverter();
  converter->setProperties(&props);

  std::string filename(TestDataDirectory);
  filename += "mraterules1.xml";
  std::string filename1(TestDataDirectory);
  filename1 += "mreact1.xml";

  SBMLDocument* d = readSBMLFromFile(filename.c_str());

  converter->setDocument(d);
  fail_unless(converter->convert() == LIBSBML_OPERATION_SUCCESS);

  SBMLDocument* d1 = readSBMLFromFile(filename1.c_str());
  std::string out = writeSBMLToStdString(d);
  std::string expected = writeSBMLToStdString(d1);

  fail_unless(equals(expected.c_str(), out.c_str()));

  delete converter;
  delete d;
  delete d1;
}
END_TEST

START_TEST(test_model2)
{
  ConversionProperties props;
  props.addOption("inferReactions", true);

  SBMLConverter* converter = new SBMLRateRuleConverter();
  converter->setProperties(&props);

  std::string filename(TestDataDirectory);
  filename += "mraterules2.xml";
  std::string filename1(TestDataDirectory);
  filename1 += "mreact2.xml";
  SBMLDocument* d = readSBMLFromFile(filename.c_str());

  converter->setDocument(d);
  fail_unless(converter->convert() == LIBSBML_OPERATION_SUCCESS);

  SBMLDocument* d1 = readSBMLFromFile(filename1.c_str());
  std::string out = writeSBMLToStdString(d);
  std::string expected = writeSBMLToStdString(d1);

  fail_unless(equals(expected.c_str(), out.c_str()));

  delete converter;
  delete d;
  delete d1;
}
END_TEST


START_TEST(test_model3)
{
  ConversionProperties props;
  props.addOption("inferReactions", true);

  SBMLConverter* converter = new SBMLRateRuleConverter();
  converter->setProperties(&props);

  std::string filename(TestDataDirectory);
  filename += "mraterules3.xml";
  std::string filename1(TestDataDirectory);
  filename1 += "mreact3.xml";
  SBMLDocument* d = readSBMLFromFile(filename.c_str());

  converter->setDocument(d);
  fail_unless(converter->convert() == LIBSBML_OPERATION_SUCCESS);
  SBMLDocument* d1 = readSBMLFromFile(filename1.c_str());
  std::string out = writeSBMLToStdString(d);
  std::string expected = writeSBMLToStdString(d1);

  fail_unless(equals(expected.c_str(), out.c_str()));

  delete converter;
  delete d;
  delete d1;
}
END_TEST


START_TEST(test_model4)
{
  ConversionProperties props;
  props.addOption("inferReactions", true);

  SBMLConverter* converter = new SBMLRateRuleConverter();
  converter->setProperties(&props);

  std::string filename(TestDataDirectory);
  filename += "mraterules4.xml";
  std::string filename1(TestDataDirectory);
  filename1 += "mreact4.xml";
  SBMLDocument* d = readSBMLFromFile(filename.c_str());

  converter->setDocument(d);
  fail_unless(converter->convert() == LIBSBML_OPERATION_SUCCESS);

  SBMLDocument* d1 = readSBMLFromFile(filename1.c_str());
  std::string out = writeSBMLToStdString(d);
  std::string expected = writeSBMLToStdString(d1);

  fail_unless(equals(expected.c_str(), out.c_str()));

  delete converter;
  delete d;
  delete d1;
}
END_TEST


START_TEST(test_model5)
{
  ConversionProperties props;
  props.addOption("inferReactions", true);

  SBMLConverter* converter = new SBMLRateRuleConverter();
  converter->setProperties(&props);

  std::string filename(TestDataDirectory);
  filename += "mraterules5.xml";
  std::string filename1(TestDataDirectory);
  filename1 += "mreact5.xml";
  SBMLDocument* d = readSBMLFromFile(filename.c_str());

  converter->setDocument(d);
  fail_unless(converter->convert() == LIBSBML_OPERATION_SUCCESS);

  SBMLDocument* d1 = readSBMLFromFile(filename1.c_str());
  std::string out = writeSBMLToStdString(d);
  std::string expected = writeSBMLToStdString(d1);

  fail_unless(equals(expected.c_str(), out.c_str()));

  delete converter;
  delete d;
  delete d1;
}
END_TEST


START_TEST(test_model6)
{
  ConversionProperties props;
  props.addOption("inferReactions", true);

  SBMLConverter* converter = new SBMLRateRuleConverter();
  converter->setProperties(&props);

  std::string filename(TestDataDirectory);
  filename += "mraterules6.xml";
  std::string filename1(TestDataDirectory);
  filename1 += "mreact6.xml";
  SBMLDocument* d = readSBMLFromFile(filename.c_str());

  converter->setDocument(d);
  fail_unless(converter->convert() == LIBSBML_OPERATION_SUCCESS);

  SBMLDocument* d1 = readSBMLFromFile(filename1.c_str());
  std::string out = writeSBMLToStdString(d);
  std::string expected = writeSBMLToStdString(d1);

  fail_unless(equals(expected.c_str(), out.c_str()));

  delete converter;
  delete d;
  delete d1;
}
END_TEST


Suite *
create_suite_TestSBMLRateRuleConverter (void)
{ 
  Suite *suite = suite_create("SBMLRateRuleConverter");
  TCase *tcase = tcase_create("SBMLRateRuleConverter");

  tcase_add_test(tcase, test_conversion_raterule_converter_invalid);
  tcase_add_test(tcase, test_conversion_raterule_converter);
  tcase_add_test(tcase, test_conversion_raterule_converter_non_standard_stoichiometry);
  tcase_add_test(tcase, test_conversion_raterule_converter_hidden_variable);
  tcase_add_test(tcase, test_model);
  tcase_add_test(tcase, test_model1);
  tcase_add_test(tcase, test_model2);
  tcase_add_test(tcase, test_model3);
  tcase_add_test(tcase, test_model4);
  tcase_add_test(tcase, test_model5);
  tcase_add_test(tcase, test_model6);

  suite_add_tcase(suite, tcase);

  return suite;
}
END_C_DECLS

