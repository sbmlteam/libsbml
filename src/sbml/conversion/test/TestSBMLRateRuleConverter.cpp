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
#include <sbml/conversion/ExpressionAnalyser.h>
#include <sbml/conversion/SBMLReactionConverter.h>

#include <sbml/math/FormulaParser.h>

#include <string>
#include <iostream>
#include <check.h>

using namespace std;
LIBSBML_CPP_NAMESPACE_USE
BEGIN_C_DECLS

static ConversionProperties rule_rn_props;
static SBMLRateRuleConverter* rule_rn_converter;
static Model* model;

static bool
equals(const char* expected, const char* actual)
{
  if (!strcmp(expected, actual)) 
  {
	  //printf("\nStrings equal:\n");
	  return true;
  }

  //printf("\nStrings are not equal:\n");
  printf("Expected:\n[%s]\n", expected);
  printf("Actual:\n[%s]\n", actual);

  return false;
}

static bool
formulas_equal(const char* expected, ASTNode* actual)
{
	return equals(expected, SBML_formulaToL3String(actual));
}

extern char *TestDataDirectory;

void
RateRuleConverter_setup(void)
{
	rule_rn_props.addOption("inferReactions", true);

	rule_rn_converter = new SBMLRateRuleConverter();
	rule_rn_converter->setProperties(&rule_rn_props);
}

void
RateRuleConverter_teardown(void)
{
	delete rule_rn_converter;
}

// helper function to set up a parameter with 0 value
static Parameter* setupZeroParameter(Model* model, const char* name, bool is_constant)
{
	Parameter* parameter = model->createParameter();
	parameter->setId(name);
	parameter->setConstant(is_constant);
	parameter->setValue(0);
	return parameter;
}

bool test_rule_to_reaction(const std::string& raterule_file, 
									const std::string& reaction_file,
									bool useStoichiometryfromMath = true)

{
	SBMLDocument* d_rule = readSBMLFromFile(raterule_file.c_str());
	SBMLDocument* d_rn = readSBMLFromFile(reaction_file.c_str());
	SBMLDocument* d = readSBMLFromFile(raterule_file.c_str());

	if (!useStoichiometryfromMath)
	{
		rule_rn_props.addOption("useStoichiometryFromMath", false);
		rule_rn_converter->setProperties(&rule_rn_props);
	}
	else if (!rule_rn_props.getOption("useStoichiometryFromMath"))
	{
		// it was set to false for a previous test - set it back to true
		rule_rn_props.addOption("useStoichiometryFromMath", true);
		rule_rn_converter->setProperties(&rule_rn_props);
	}


	rule_rn_converter->setDocument(d);
	if (rule_rn_converter->convert() != LIBSBML_OPERATION_SUCCESS)
	{
		//cout << "rule_reaction: converter rule->reaction failed" << endl;
		delete d;
		delete d_rn;
		delete d_rule;
		return false;
	}	
	std::string out = writeSBMLToStdString(d);
	std::string expected = writeSBMLToStdString(d_rn);
	bool strings_equal = equals(expected.c_str(), out.c_str());
	if (!strings_equal)
	{
		std::string TEST_file(TestDataDirectory);
		TEST_file += "test_TEMP_out.xml";
		writeSBMLToFile(d, TEST_file.c_str());
		cout << "rule_reaction: rule->reaction failed" << endl;
		delete d;
		delete d_rn;
		delete d_rule;
		return false;
	}

	delete d;
	delete d_rn;
	delete d_rule;
    return true;
}

START_TEST(test_check_derivative_sign)
{
    const ASTNode* deriv = NULL;
	bool derivativeSign = false;
	bool signDetermined = rule_rn_converter->checkDerivativeSign(deriv, derivativeSign);
	fail_unless(signDetermined == false);
    fail_unless(derivativeSign == false);
	delete deriv;

	ASTNode* node = SBML_parseL3Formula("3");
	deriv = node->derivative("S1");
	derivativeSign = false;
	signDetermined = rule_rn_converter->checkDerivativeSign(deriv, derivativeSign);
	fail_unless(signDetermined == true);
	fail_unless(derivativeSign == false);
	delete node;
	delete deriv;
    
	node = SBML_parseL3Formula("3*S1");
    deriv = node->derivative("S1");
    derivativeSign = false;
    signDetermined = rule_rn_converter->checkDerivativeSign(deriv, derivativeSign);
    fail_unless(signDetermined == true);
    fail_unless(derivativeSign == true);
    delete node;
	delete deriv;

	node = SBML_parseL3Formula("-5*S1");
	deriv = node->derivative("S1");
	derivativeSign = false;
	signDetermined = rule_rn_converter->checkDerivativeSign(deriv, derivativeSign);
	fail_unless(signDetermined == true);
	fail_unless(derivativeSign == false);
	delete node;
	delete deriv;
	
	node = SBML_parseL3Formula("-k1*S1");
	deriv = node->derivative("S1");
	signDetermined = rule_rn_converter->checkDerivativeSign(deriv, derivativeSign);
	fail_unless(signDetermined == true);
	fail_unless(derivativeSign == false);
	delete node;
	delete deriv;
	
	node = SBML_parseL3Formula("k1*S1 + k2*S2");
	deriv = node->derivative("S1");
	signDetermined = rule_rn_converter->checkDerivativeSign(deriv, derivativeSign);
	fail_unless(signDetermined == true);
	fail_unless(derivativeSign == true);
	delete node;
	delete deriv;
}
END_TEST

START_TEST(test_conversion_raterule_converter_invalid)
{
  // test NULL document
  SBMLDocument* doc = NULL;
  rule_rn_converter->setDocument(doc);

  fail_unless(rule_rn_converter->convert() == LIBSBML_INVALID_OBJECT);

  // test NULL model
  doc = new SBMLDocument(3, 2);
  rule_rn_converter->setDocument(doc);

  fail_unless(rule_rn_converter->convert() == LIBSBML_INVALID_OBJECT);

  // create model no rules
  Model* model = doc->createModel();
  model->setId("m");

  rule_rn_converter->setDocument(doc);

  fail_unless(rule_rn_converter->convert() == LIBSBML_OPERATION_SUCCESS);

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
  rule_rn_converter->setDocument(doc);

  fail_unless(rule_rn_converter->convert() == LIBSBML_CONV_INVALID_SRC_DOCUMENT);

  delete doc;
}
END_TEST


START_TEST(test_conversion_raterule_converter)
{
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

  rule_rn_converter->setDocument(doc);
  fail_unless(rule_rn_converter->convert() == LIBSBML_OPERATION_SUCCESS);

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

  delete doc;
}
END_TEST


START_TEST(test_crash_converter)
{
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
  ASTNode *math = SBML_parseL3Formula("cos(s)");
  rr1->setMath(math);
  delete math;

  RateRule* rr2 = model->createRateRule();
  rr1->setVariable("s");
  math = SBML_parseL3Formula("sin(s)");
  rr1->setMath(math);
  delete math;

  rule_rn_converter->setDocument(doc);
  fail_unless(rule_rn_converter->convert() == LIBSBML_OPERATION_FAILED);

  delete doc;
}
END_TEST



START_TEST(test_conversion_raterule_converter_non_standard_stoichiometry)
{
	// example 3.13 in Fages et al, TCS, 2015
	// Simple example of dx/dt = -2*k*x = - dy/dt
	// Unlike typical converters of Mass action reactions that would give x -> y with f=2*k*x,
	// this rule_rn_converter give (the equivalent) 2*x -> 2*y with f=k*x
	
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

	rule_rn_converter->setDocument(doc);
	fail_unless(rule_rn_converter->convert() == LIBSBML_OPERATION_SUCCESS);

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

    delete doc;
}
END_TEST

START_TEST(test_conversion_raterule_converter_my_example)
{
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
	parameter->setId("r");
	parameter->setConstant(false);
	parameter->setValue(0);

	parameter = model->createParameter();
	parameter->setId("k");
	parameter->setConstant(true);
	parameter->setValue(0);

	parameter = model->createParameter();
	parameter->setId("k1");
	parameter->setConstant(true);
	parameter->setValue(0);

	RateRule* rr1 = model->createRateRule();
	rr1->setVariable("s");
	ASTNode* math = SBML_parseL3Formula("-1*k*s");
	rr1->setMath(math);
	delete math;

	RateRule* rr2 = model->createRateRule();
	rr2->setVariable("p");
	math = SBML_parseL3Formula("k*s");
	rr2->setMath(math);
	delete math;

	RateRule* rr3 = model->createRateRule();
	rr3->setVariable("r");
	math = SBML_parseL3Formula("k1*r");
	rr3->setMath(math);
	delete math;


	fail_unless(doc->getModel()->getNumCompartments() == 0);
	fail_unless(doc->getModel()->getNumSpecies() == 0);
	fail_unless(doc->getModel()->getNumParameters() == 5);
	fail_unless(doc->getModel()->getNumRules() == 3);
	fail_unless(doc->getModel()->getNumReactions() == 0);

	rule_rn_converter->setDocument(doc);
	fail_unless(rule_rn_converter->convert() == LIBSBML_OPERATION_SUCCESS);

	fail_unless(doc->getModel()->getNumCompartments() == 1);
	fail_unless(doc->getModel()->getNumSpecies() == 3);
	fail_unless(doc->getModel()->getNumParameters() == 2);
	fail_unless(doc->getModel()->getNumRules() == 0);
	fail_unless(doc->getModel()->getNumReactions() == 2);

	Reaction* r = doc->getModel()->getReaction(0);
	fail_unless(r->getNumReactants() == 1);
	fail_unless(r->getNumProducts() == 1);
	fail_unless(r->getNumModifiers() == 0);
	fail_unless(r->isSetKineticLaw());

	const char* kl = SBML_formulaToL3String(r->getKineticLaw()->getMath());
	fail_unless(strcmp(kl, "k*s"));
	safe_free((char*)kl);

	Reaction* r1 = doc->getModel()->getReaction(1);
	fail_unless(r1->getNumReactants() == 1);
	fail_unless(r1->getNumProducts() == 1);
	fail_unless(r1->getNumModifiers() == 0);
	fail_unless(r1->isSetKineticLaw());

	const char* kl1 = SBML_formulaToL3String(r1->getKineticLaw()->getMath());
	fail_unless(strcmp(kl1, "k1*r"));
	safe_free((char*)kl1);

    delete doc;
}
END_TEST


START_TEST(test_conversion_raterule_converter_hidden_variable)
{
	// example 3.7 in Fages et al, TCS, 2015 (circadian clock)
	// requires implementation of Algo 3.1 (hidden variable inference) to give correct result
	// The additional ODE dMPFi/dt = -k1*MPFi*Cdc25 + k2*MPF*Wee1 should be created by 3.1.
	// @alessandrofelder has visually checked that the expected (incorrect) result is 
	// obtained in the absence of an implementation of 3.1.
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

	rule_rn_converter->setDocument(doc);
	fail_unless(rule_rn_converter->convert() == LIBSBML_OPERATION_SUCCESS);

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
	fail_unless(srCdc25->getSpecies() == string("Cdc25"));
	fail_unless(util_isEqual(srCdc25->getStoichiometry(), 1));

	srMpfi = r0->getReactant(1);
	fail_unless(srMpfi->getSpecies() == string("newVar1"));
	fail_unless(util_isEqual(srMpfi->getStoichiometry(), 1));

	// products
	srMpf = r0->getProduct(0);
	fail_unless(srMpf->getSpecies() == string("MPF"));
	fail_unless(util_isEqual(srMpf->getStoichiometry(), 1.0));

	srCdc25 = r0->getProduct(1);
	fail_unless(srCdc25->getSpecies() == string("Cdc25"));
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
	fail_unless(srMpf->getSpecies() == string("MPF"));
	fail_unless(util_isEqual(srMpf->getStoichiometry(), 1));

	srWee1 = r1->getReactant(1);
	fail_unless(srWee1->getSpecies() == string("Wee1"));
	fail_unless(util_isEqual(srWee1->getStoichiometry(), 1));

	// products
	srWee1 = r1->getProduct(0);
	fail_unless(srWee1->getSpecies() == string("Wee1"));
	fail_unless(util_isEqual(srWee1->getStoichiometry(), 1.0));

	srMpfi = r1->getProduct(1);
	fail_unless(srMpfi->getSpecies() == string("newVar1"));
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
	fail_unless(srClock->getSpecies() == string("Clock"));

	// products
	srWee1 = r2->getProduct(0);
	fail_unless(srWee1->getSpecies() == string("Wee1"));
	fail_unless(util_isEqual(srWee1->getStoichiometry(), 1.0));

	// kinetic law
	kl = SBML_formulaToL3String(r1->getKineticLaw()->getMath());
	fail_unless(strcmp(kl, "k3/(k4+Clock)"));
	safe_free((char*)kl);

    delete doc;
}
END_TEST

START_TEST(test_rule_reaction_01)
{
	std::string raterule_file(TestDataDirectory);
	raterule_file += "valid_01_rr.xml";
	std::string reaction_file(TestDataDirectory);
	reaction_file += "valid_01_bio.xml";

	bool result = test_rule_to_reaction(raterule_file, reaction_file);

	fail_unless(result == true);
}
END_TEST

START_TEST(test_rule_reaction_02)
{
	std::string raterule_file(TestDataDirectory);
	raterule_file += "valid_02_rr.xml";
	std::string reaction_file(TestDataDirectory);
	reaction_file += "valid_02_bio.xml";

	bool result = test_rule_to_reaction(raterule_file, reaction_file);

	fail_unless(result == true);
}
END_TEST

START_TEST(test_rule_reaction_03)
{
	std::string raterule_file(TestDataDirectory);
	raterule_file += "valid_03_rr.xml";
	std::string reaction_file(TestDataDirectory);
	reaction_file += "valid_03_bio.xml";

	bool result = test_rule_to_reaction(raterule_file, reaction_file);

	fail_unless(result == true);
}
END_TEST

START_TEST(test_rule_reaction_04)
{
	std::string raterule_file(TestDataDirectory);
	raterule_file += "valid_04_rr.xml";
	std::string reaction_file(TestDataDirectory);
	reaction_file += "valid_04_bio.xml";

	bool result = test_rule_to_reaction(raterule_file, reaction_file);

	fail_unless(result == true);
}
END_TEST

START_TEST(test_rule_reaction_05)
{
	std::string raterule_file(TestDataDirectory);
	raterule_file += "valid_05_rr.xml";
	std::string reaction_file(TestDataDirectory);
	reaction_file += "valid_05_bio.xml";

	bool result = test_rule_to_reaction(raterule_file, reaction_file);

	fail_unless(result == true);
}
END_TEST

START_TEST(test_rule_reaction_06)
{
	std::string raterule_file(TestDataDirectory);
	raterule_file += "valid_06_rr.xml";
	std::string reaction_file(TestDataDirectory);
	reaction_file += "valid_06_bio.xml";

	bool result = test_rule_to_reaction(raterule_file, reaction_file);

	fail_unless(result == true);
}
END_TEST

START_TEST(test_rule_reaction_07)
{
	std::string raterule_file(TestDataDirectory);
	raterule_file += "valid_07_rr.xml";
	std::string reaction_file(TestDataDirectory);
	reaction_file += "valid_07_bio.xml";

	bool result = test_rule_to_reaction(raterule_file, reaction_file, false);

	fail_unless(result == true);
}
END_TEST

START_TEST(test_rule_reaction_08)
{
	std::string raterule_file(TestDataDirectory);
	raterule_file += "valid_08_rr.xml";
	std::string reaction_file(TestDataDirectory);
	reaction_file += "valid_08_bio.xml";

	bool result = test_rule_to_reaction(raterule_file, reaction_file);

	fail_unless(result == true);
}
END_TEST

START_TEST(test_rule_reaction_09)
{
	std::string raterule_file(TestDataDirectory);
	raterule_file += "valid_09_rr.xml";
	std::string reaction_file(TestDataDirectory);
	reaction_file += "valid_09_bio.xml";

	bool result = test_rule_to_reaction(raterule_file, reaction_file);

	fail_unless(result == true);
}
END_TEST

START_TEST(test_rule_reaction_010)
{
	std::string raterule_file(TestDataDirectory);
	raterule_file += "invalid_010_rr.xml";
	std::string reaction_file(TestDataDirectory);
	reaction_file += "invalid_010_bio.xml";

	bool result = test_rule_to_reaction(raterule_file, reaction_file);

	fail_unless(result == false);
}
END_TEST

START_TEST(test_rule_reaction_011)
{
	std::string raterule_file(TestDataDirectory);
	raterule_file += "valid_011_rr.xml";
	std::string reaction_file(TestDataDirectory);
	reaction_file += "valid_011_bio.xml";

	bool result = test_rule_to_reaction(raterule_file, reaction_file);

	fail_unless(result == true);
}
END_TEST

START_TEST(test_rule_reaction_012)
{
	std::string raterule_file(TestDataDirectory);
	raterule_file += "valid_012_rr.xml";
	std::string reaction_file(TestDataDirectory);
	reaction_file += "valid_012_bio.xml";

	bool result = test_rule_to_reaction(raterule_file, reaction_file);

	fail_unless(result == true);
}
END_TEST

START_TEST(test_rule_reaction_013)
{
	std::string raterule_file(TestDataDirectory);
	raterule_file += "valid_013_rr_original.xml";
	std::string reaction_file(TestDataDirectory);
	reaction_file += "valid_013_bio_from_rr_original.xml";

	bool result = test_rule_to_reaction(raterule_file, reaction_file);

	fail_unless(result == true);
}
END_TEST

START_TEST(test_rule_reaction_014)
{
	std::string raterule_file(TestDataDirectory);
	raterule_file += "valid_014_rr.xml";
	std::string reaction_file(TestDataDirectory);
	reaction_file += "valid_014_bio.xml";

	bool result = test_rule_to_reaction(raterule_file, reaction_file);

	fail_unless(result == true);
}
END_TEST

START_TEST(test_rule_reaction_015)
{
	std::string raterule_file(TestDataDirectory);
	raterule_file += "valid_015_rr.xml";
	std::string reaction_file(TestDataDirectory);
	reaction_file += "valid_015_bio.xml";

	bool result = test_rule_to_reaction(raterule_file, reaction_file);

	fail_unless(result == true);
}
END_TEST

START_TEST(test_rule_reaction_016)
{
	std::string raterule_file(TestDataDirectory);
	raterule_file += "valid_016_rr.xml";
	std::string reaction_file(TestDataDirectory);
	reaction_file += "valid_016_bio.xml";

	bool result = test_rule_to_reaction(raterule_file, reaction_file);

	fail_unless(result == true);
}
END_TEST

START_TEST(test_rule_reaction_017)
{
	std::string raterule_file(TestDataDirectory);
	raterule_file += "valid_017_rr.xml";
	std::string reaction_file(TestDataDirectory);
	reaction_file += "valid_017_bio.xml";

	bool result = test_rule_to_reaction(raterule_file, reaction_file);

	fail_unless(result == true);
}
END_TEST

START_TEST(test_rule_reaction_51)
{
	std::string raterule_file(TestDataDirectory);
	raterule_file += "valid_51_rr.xml";
	std::string reaction_file(TestDataDirectory);
	reaction_file += "valid_51_bio.xml";

	bool result = test_rule_to_reaction(raterule_file, reaction_file);

	fail_unless(result == true);
}
END_TEST

START_TEST(test_rule_reaction_52)
{
	std::string raterule_file(TestDataDirectory);
	raterule_file += "valid_52_rr.xml";
	std::string reaction_file(TestDataDirectory);
	reaction_file += "valid_52_bio.xml";

	bool result = test_rule_to_reaction(raterule_file, reaction_file);

	fail_unless(result == true);
}
END_TEST

START_TEST(test_rule_reaction_53)
{
	std::string raterule_file(TestDataDirectory);
	raterule_file += "valid_53_rr.xml";
	std::string reaction_file(TestDataDirectory);
	reaction_file += "valid_53_bio.xml";

	bool result = test_rule_to_reaction(raterule_file, reaction_file);

	fail_unless(result == true);
}
END_TEST

START_TEST(test_rule_reaction_54)
{
	std::string raterule_file(TestDataDirectory);
	raterule_file += "valid_54_rr.xml";
	std::string reaction_file(TestDataDirectory);
	reaction_file += "valid_54_bio.xml";

	bool result = test_rule_to_reaction(raterule_file, reaction_file);

	fail_unless(result == true);
}
END_TEST

START_TEST(test_rule_reaction_55)
{
	std::string raterule_file(TestDataDirectory);
	raterule_file += "valid_55_rr.xml";
	std::string reaction_file(TestDataDirectory);
	reaction_file += "valid_55_bio.xml";

	bool result = test_rule_to_reaction(raterule_file, reaction_file);

	fail_unless(result == true);
}
END_TEST

Suite *
create_suite_TestSBMLRateRuleConverter (void)
{ 
	bool testing = false;
Suite *suite = suite_create("SBMLRateRuleConverter");
  TCase *tcase = tcase_create("SBMLRateRuleConverter");
  tcase_add_checked_fixture(tcase, RateRuleConverter_setup,
	  RateRuleConverter_teardown);

  if (testing)
  {
	  tcase_add_test(tcase, test_rule_reaction_07);
  }
  else
  {
	  tcase_add_test(tcase, test_check_derivative_sign);
	  tcase_add_test(tcase, test_conversion_raterule_converter_invalid); 
	  tcase_add_test(tcase, test_conversion_raterule_converter); 
	  tcase_add_test(tcase, test_conversion_raterule_converter_non_standard_stoichiometry); 
	  tcase_add_test(tcase, test_crash_converter); 
	  //tcase_add_test(tcase, test_conversion_raterule_converter_hidden_variable);
	  tcase_add_test(tcase, test_rule_reaction_01);
	  tcase_add_test(tcase, test_rule_reaction_02);
	  tcase_add_test(tcase, test_rule_reaction_03); 
	  tcase_add_test(tcase, test_rule_reaction_04);
	  tcase_add_test(tcase, test_rule_reaction_05);
  //    tcase_add_test(tcase, test_rule_reaction_06); // need to make parameters local
      tcase_add_test(tcase, test_rule_reaction_07); 
 //     tcase_add_test(tcase, test_rule_reaction_08); // fails it does an extra divide by campartment volume
	// tcase_add_test(tcase, test_rule_reaction_09); // fails
	  tcase_add_test(tcase, test_rule_reaction_010);
 //     tcase_add_test(tcase, test_rule_reaction_011); // fails
   //   tcase_add_test(tcase, test_rule_reaction_012); // fails
   //   tcase_add_test(tcase, test_rule_reaction_013); // fails
   //   tcase_add_test(tcase, test_rule_reaction_014); // fails
   //   tcase_add_test(tcase, test_rule_reaction_015); // fails
   //   tcase_add_test(tcase, test_rule_reaction_016); // fails
   //   tcase_add_test(tcase, test_rule_reaction_017); // fails
   //   tcase_add_test(tcase, test_rule_reaction_51); // fails 51 - 55
	  //tcase_add_test(tcase, test_rule_reaction_52);	 
	  //tcase_add_test(tcase, test_rule_reaction_53); 	
	  //tcase_add_test(tcase, test_rule_reaction_54); 	
	  //tcase_add_test(tcase, test_rule_reaction_55); 	 
  }
  suite_add_tcase(suite, tcase);

  return suite;
}
END_C_DECLS
