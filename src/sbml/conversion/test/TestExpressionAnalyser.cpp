/**
 +* @file    TestExpressionAnalyser.cpp
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

#include <sbml/math/FormulaParser.h>

#include <string>
#include <iostream>
#include <check.h>

using namespace std;
LIBSBML_CPP_NAMESPACE_USE

CK_CPPSTART

static Model* m;
static SBMLDocument* d;
static ConversionProperties props;
static SBMLRateRuleConverter* converter;  


static bool
equals(const char* expected, const char* actual)
{
  if (!strcmp(expected, actual)) return true;

  printf("\nStrings are not equal:\n");
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

static Parameter* setupZeroParameter(Model* model, const char* name, bool is_constant)
{
	Parameter* parameter = model->createParameter();
	parameter->setId(name);
	parameter->setConstant(is_constant);
	parameter->setValue(0);
	return parameter;
}

Species* setupSpecies(Model* model, const char* name, const char* compartment) {
    Species* species = model->createSpecies();
    species->setId(name);
    species->setCompartment(compartment);
    species->setInitialAmount(0);
	species->setConstant(false);
    return species;
}

Model* setupModel(SBMLDocument* doc) {
    Model* model = doc->createModel();
    model->setId("m");

	// create compartment
    Compartment* compartment = model->createCompartment();
    compartment->setId("c");
    compartment->setSpatialDimensions(3.0);
    compartment->setSize(1);
    compartment->setConstant(true);

	// create species
    setupSpecies(model, "x", "c");
    setupSpecies(model, "y", "c");
    setupSpecies(model, "a", "c");
	setupSpecies(model, "b", "c");

	// create parameters
    setupZeroParameter(model, "k", true);
    setupZeroParameter(model, "v", false);
	setupZeroParameter(model, "w", false);


    return model;
}


void
ExpressionAnalyser_setup(void)
{
	props.addOption("inferReactions", true);

	converter = new SBMLRateRuleConverter();
	converter->setProperties(&props);

	d = new SBMLDocument();
	m = setupModel(d);
	converter->setDocument(d);
}

void
ExpressionAnalyser_teardown(void)
{
	delete converter;
	delete d;
}

extern char *TestDataDirectory;
START_TEST(test_analyse)
{
	RateRule* rr = d->getModel()->createRateRule();
	rr->setVariable("a");
	rr->setMath(SBML_parseFormula("k-x-y"));
	converter->populateInitialODEinfo();
	ExpressionAnalyser* analyser = new ExpressionAnalyser(m, converter->getOdePairs());

	fail_unless(analyser->getNumExpressions() == 0);
    
	analyser->analyse();

	fail_unless(analyser->getNumExpressions() == 1);
	SubstitutionValues_t *value = analyser->getExpression(0);
	fail_unless(value->k_value == "k");
	fail_unless(value->x_value == "x");
	fail_unless(value->y_value == "y");
	fail_unless(value->z_value.empty());
	fail_unless(value->type == TYPE_K_MINUS_X_MINUS_Y);
	fail_unless(formulas_equal("k - x - y", value->current));
	fail_unless(formulas_equal( "0", value->dxdt_expression));
	fail_unless(formulas_equal( "0", value->dydt_expression));
	fail_unless(value->v_expression == NULL);
	fail_unless(value->w_expression == NULL);
	fail_unless(value->z_expression == NULL);
	fail_unless(value->odeIndex == 0);
	fail_unless(util_isNaN(value->k_real_value));
}
END_TEST

START_TEST(test_analyse_same_expression)
{
	RateRule* rr = d->getModel()->createRateRule();
	rr->setVariable("a");
	rr->setMath(SBML_parseFormula("k-x-y"));

	RateRule* rr1 = d->getModel()->createRateRule();
	rr1->setVariable("b");
	rr1->setMath(SBML_parseFormula("k-x-y"));


	converter->populateInitialODEinfo();
	ExpressionAnalyser* analyser = new ExpressionAnalyser(m, converter->getOdePairs());

	fail_unless(analyser->getNumExpressions() == 0);

	analyser->analyse();

	fail_unless(analyser->getNumExpressions() == 1);
	SubstitutionValues_t* value = analyser->getExpression(0);
	fail_unless(value->k_value == "k");
	fail_unless(value->x_value == "x");
	fail_unless(value->y_value == "y");
	fail_unless(value->z_value.empty());
	fail_unless(value->type == TYPE_K_MINUS_X_MINUS_Y);
	fail_unless(formulas_equal("k - x - y", value->current));
	fail_unless(formulas_equal("0", value->dxdt_expression));
	fail_unless(formulas_equal("0", value->dydt_expression));
	fail_unless(value->v_expression == NULL);
	fail_unless(value->w_expression == NULL);
	fail_unless(value->z_expression == NULL);
	fail_unless(value->odeIndex == 0);
	fail_unless(util_isNaN(value->k_real_value));
}
END_TEST


START_TEST(test_analyse_different_expression)
{
	// the second expression is the same type but has a different variable
	RateRule* rr = d->getModel()->createRateRule();
	rr->setVariable("a");
	rr->setMath(SBML_parseFormula("k-x-y"));

	RateRule* rr1 = d->getModel()->createRateRule();
	rr1->setVariable("b");
	rr1->setMath(SBML_parseFormula("k-x-a"));


	converter->populateInitialODEinfo();
	ExpressionAnalyser* analyser = new ExpressionAnalyser(m, converter->getOdePairs());

	fail_unless(analyser->getNumExpressions() == 0);

	analyser->analyse();

	fail_unless(analyser->getNumExpressions() == 2);
	SubstitutionValues_t* value = analyser->getExpression(1);
	fail_unless(value->k_value == "k");
	fail_unless(value->x_value == "x");
	fail_unless(value->y_value == "a");
	fail_unless(value->z_value.empty());
	fail_unless(value->type == TYPE_K_MINUS_X_MINUS_Y);
	fail_unless(formulas_equal("k - x - a", value->current));
	fail_unless(formulas_equal("0", value->dxdt_expression));
	fail_unless(formulas_equal("k - x - y", value->dydt_expression));
	fail_unless(value->v_expression == NULL);
	fail_unless(value->w_expression == NULL);
	fail_unless(value->z_expression == NULL);
	fail_unless(value->odeIndex == 1);
	fail_unless(util_isNaN(value->k_real_value));
}
END_TEST

START_TEST(test_analyse_1)
{
	RateRule* rr = d->getModel()->createRateRule();
	rr->setVariable("a");
	rr->setMath(SBML_parseFormula("k + v - x - y"));
	converter->populateInitialODEinfo();
	ExpressionAnalyser* analyser = new ExpressionAnalyser(m, converter->getOdePairs());

	fail_unless(analyser->getNumExpressions() == 0);

	analyser->analyse();

	fail_unless(analyser->getNumExpressions() == 1);
	SubstitutionValues_t* value = analyser->getExpression(0);
	fail_unless(value->k_value == "k");
	fail_unless(value->x_value == "x");
	fail_unless(value->y_value == "y");
	fail_unless(value->z_value.empty());
	fail_unless(value->type == TYPE_K_PLUS_V_MINUS_X_MINUS_Y);
	fail_unless(formulas_equal("k + v - x - y", value->current));
	fail_unless(formulas_equal("0", value->dxdt_expression));
	fail_unless(formulas_equal("0", value->dydt_expression));
	fail_unless(formulas_equal("v", value->v_expression));
	fail_unless(value->w_expression == NULL);
	fail_unless(value->z_expression == NULL);
	fail_unless(value->odeIndex == 0);
	fail_unless(util_isNaN(value->k_real_value));
}
END_TEST

START_TEST(test_analyse_1_same)
{
	RateRule* rr = d->getModel()->createRateRule();
	rr->setVariable("a");
	rr->setMath(SBML_parseFormula("k + v - x - y"));
	RateRule* rr1 = d->getModel()->createRateRule();
	rr1->setVariable("b");
	rr1->setMath(SBML_parseFormula("k + v - x - y"));

	converter->populateInitialODEinfo();
	ExpressionAnalyser* analyser = new ExpressionAnalyser(m, converter->getOdePairs());

	fail_unless(analyser->getNumExpressions() == 0);

	analyser->analyse();

	fail_unless(analyser->getNumExpressions() == 1);
	SubstitutionValues_t* value = analyser->getExpression(0);
	fail_unless(value->k_value == "k");
	fail_unless(value->x_value == "x");
	fail_unless(value->y_value == "y");
	fail_unless(value->z_value.empty());
	fail_unless(value->type == TYPE_K_PLUS_V_MINUS_X_MINUS_Y);
	fail_unless(formulas_equal("k + v - x - y", value->current));
	fail_unless(formulas_equal("0", value->dxdt_expression));
	fail_unless(formulas_equal("0", value->dydt_expression));
	fail_unless(formulas_equal("v", value->v_expression));
	fail_unless(value->w_expression == NULL);
	fail_unless(value->z_expression == NULL);
	fail_unless(value->odeIndex == 0);
	fail_unless(util_isNaN(value->k_real_value));
}
END_TEST



START_TEST(test_analyse_1_two_terms)
{
	RateRule* rr = d->getModel()->createRateRule();
	rr->setVariable("a");
	rr->setMath(SBML_parseFormula("(k + v - x - y) + (k - x)"));

	converter->populateInitialODEinfo();
	ExpressionAnalyser* analyser = new ExpressionAnalyser(m, converter->getOdePairs());

	fail_unless(analyser->getNumExpressions() == 0);

	analyser->analyse();

	fail_unless(analyser->getNumExpressions() == 2);
	SubstitutionValues_t* value = analyser->getExpression(0);
	fail_unless(value->k_value == "k");
	fail_unless(value->x_value == "x");
	fail_unless(value->y_value == "y");
	fail_unless(value->z_value.empty());
	fail_unless(value->type == TYPE_K_PLUS_V_MINUS_X_MINUS_Y);
	fail_unless(formulas_equal("k + v - x - y", value->current));
	fail_unless(formulas_equal("0", value->dxdt_expression));
	fail_unless(formulas_equal("0", value->dydt_expression));
	fail_unless(formulas_equal("v", value->v_expression));
	fail_unless(value->w_expression == NULL);
	fail_unless(value->z_expression == NULL);
	fail_unless(value->odeIndex == 0);
	fail_unless(util_isNaN(value->k_real_value));

	SubstitutionValues_t* value1 = analyser->getExpression(1);
	fail_unless(value1->k_value == "k");
	fail_unless(value1->x_value == "x");
	fail_unless(value1->y_value.empty());
	fail_unless(value1->z_value.empty());
	fail_unless(value1->type == TYPE_K_MINUS_X);
	fail_unless(formulas_equal("k - x", value1->current));
	fail_unless(formulas_equal("0", value1->dxdt_expression));
	fail_unless(value1->dydt_expression == NULL);
	fail_unless(value1->v_expression == NULL);
	fail_unless(value1->w_expression == NULL);
	fail_unless(value1->z_expression == NULL);
	fail_unless(value1->odeIndex == 0);
	fail_unless(util_isNaN(value1->k_real_value));

}
END_TEST


START_TEST(test_analyse_1_different)
{
	RateRule* rr = d->getModel()->createRateRule();
	rr->setVariable("a");
	rr->setMath(SBML_parseFormula("k + v - x - y"));
	RateRule* rr1 = d->getModel()->createRateRule();
	rr1->setVariable("b");
	rr1->setMath(SBML_parseFormula("k + v - x - a"));
	converter->populateInitialODEinfo();
	ExpressionAnalyser* analyser = new ExpressionAnalyser(m, converter->getOdePairs());

	fail_unless(analyser->getNumExpressions() == 0);

	analyser->analyse();

	fail_unless(analyser->getNumExpressions() == 2);
	SubstitutionValues_t* value = analyser->getExpression(1);
	fail_unless(value->k_value == "k");
	fail_unless(value->x_value == "x");
	fail_unless(value->y_value == "a");
	fail_unless(value->z_value.empty());
	fail_unless(value->type == TYPE_K_PLUS_V_MINUS_X_MINUS_Y);
	fail_unless(formulas_equal("k + v - x - a", value->current));
	fail_unless(formulas_equal("0", value->dxdt_expression));
	fail_unless(formulas_equal("k + v - x - y", value->dydt_expression));
	fail_unless(formulas_equal("v", value->v_expression));
	fail_unless(value->w_expression == NULL);
	fail_unless(value->z_expression == NULL);
	fail_unless(value->odeIndex == 1);
	fail_unless(util_isNaN(value->k_real_value));
}
END_TEST

START_TEST(test_analyse_2)
{
	RateRule* rr = d->getModel()->createRateRule();
	rr->setVariable("a");
	rr->setMath(SBML_parseFormula("k - x + w - y"));
	converter->populateInitialODEinfo();
	ExpressionAnalyser* analyser = new ExpressionAnalyser(m, converter->getOdePairs());

	fail_unless(analyser->getNumExpressions() == 0);

	analyser->analyse();

	fail_unless(analyser->getNumExpressions() == 1);
	SubstitutionValues_t* value = analyser->getExpression(0);
	fail_unless(value->k_value == "k");
	fail_unless(value->x_value == "x");
	fail_unless(value->y_value == "y");
	fail_unless(value->z_value.empty());
	fail_unless(value->type == TYPE_K_MINUS_X_PLUS_W_MINUS_Y);
	fail_unless(formulas_equal("w + (k - x) - y", value->current));
	fail_unless(formulas_equal("0", value->dxdt_expression));
	fail_unless(formulas_equal("0", value->dydt_expression));
	fail_unless(formulas_equal("w", value->w_expression));
	fail_unless(value->v_expression == NULL);
	fail_unless(value->z_expression == NULL);
	fail_unless(value->odeIndex == 0);
	fail_unless(util_isNaN(value->k_real_value));
}
END_TEST

START_TEST(test_analyse_3)
{
	RateRule* rr = d->getModel()->createRateRule();
	rr->setVariable("a");
	rr->setMath(SBML_parseFormula("k - x"));
	converter->populateInitialODEinfo();
	ExpressionAnalyser* analyser = new ExpressionAnalyser(m, converter->getOdePairs());

	fail_unless(analyser->getNumExpressions() == 0);

	analyser->analyse();

	fail_unless(analyser->getNumExpressions() == 1);
	SubstitutionValues_t* value = analyser->getExpression(0);
	fail_unless(value->k_value == "k");
	fail_unless(value->x_value == "x");
	fail_unless(value->y_value.empty());
	fail_unless(value->z_value.empty());
	fail_unless(value->type == TYPE_K_MINUS_X);
	fail_unless(formulas_equal("k - x", value->current));
	fail_unless(formulas_equal("0", value->dxdt_expression));
	fail_unless(value->dydt_expression == NULL);
	fail_unless(value->v_expression == NULL);
	fail_unless(value->w_expression == NULL);
	fail_unless(value->z_expression == NULL);
	fail_unless(value->odeIndex == 0);
	fail_unless(util_isNaN(value->k_real_value));
}
END_TEST

START_TEST(test_analyse_4)
{
	RateRule* rr = d->getModel()->createRateRule();
	rr->setVariable("a");
	rr->setMath(SBML_parseFormula("k + v - x"));
	converter->populateInitialODEinfo();
	ExpressionAnalyser* analyser = new ExpressionAnalyser(m, converter->getOdePairs());

	fail_unless(analyser->getNumExpressions() == 0);

	analyser->analyse();

	fail_unless(analyser->getNumExpressions() == 1);
	SubstitutionValues_t* value = analyser->getExpression(0);
	fail_unless(value->k_value == "k");
	fail_unless(value->x_value == "x");
	fail_unless(value->y_value.empty());
	fail_unless(value->z_value.empty());
	fail_unless(value->type == TYPE_K_PLUS_V_MINUS_X);
	fail_unless(formulas_equal("k + v - x", value->current));
	fail_unless(formulas_equal("0", value->dxdt_expression));
	fail_unless(value->dydt_expression == NULL);
	fail_unless(formulas_equal("v", value->v_expression));
	fail_unless(value->w_expression == NULL);
	fail_unless(value->z_expression == NULL);
	fail_unless(value->odeIndex == 0);
	fail_unless(util_isNaN(value->k_real_value));
}
END_TEST

START_TEST(test_order_expressions_1)
{
	RateRule* rr = d->getModel()->createRateRule();
	rr->setVariable("b");
	rr->setMath(SBML_parseFormula("k - x + w - y"));

	RateRule* rrr = d->getModel()->createRateRule();
	rrr->setVariable("a");
	rrr->setMath(SBML_parseFormula("k-x-y"));
	converter->populateInitialODEinfo();
	ExpressionAnalyser* analyser = new ExpressionAnalyser(m, converter->getOdePairs());

	fail_unless(analyser->getNumExpressions() == 0);

	analyser->analyse();

	fail_unless(analyser->getNumExpressions() == 2);
	SubstitutionValues_t* value = analyser->getExpression(0);
	//fail_unless(value->type == TYPE_K_MINUS_X_PLUS_W_MINUS_Y);
	SubstitutionValues_t* value1 = analyser->getExpression(1);
	//fail_unless(value1->type == TYPE_K_MINUS_X_MINUS_Y);

	analyser->orderExpressions();
    fail_unless(analyser->getNumExpressions() == 2);
    SubstitutionValues_t* value2 = analyser->getExpression(0);
    fail_unless(value2->type == TYPE_K_MINUS_X_MINUS_Y);
    SubstitutionValues_t* value3 = analyser->getExpression(1);
    fail_unless(value3->type == TYPE_K_MINUS_X_PLUS_W_MINUS_Y);
}
END_TEST


START_TEST(test_reorder_minusXplusYIteratively_simple)
{
	RateRule* rr = d->getModel()->createRateRule();
	rr->setVariable("a");
	rr->setMath(SBML_parseFormula("-x + y"));
	converter->populateInitialODEinfo();
	ExpressionAnalyser* analyser = new ExpressionAnalyser(m, converter->getOdePairs());

	fail_unless(analyser->getNumExpressions() == 0);

	analyser->analyse();

	// since we decomposed the term we no longer need to identify this type

	fail_unless(analyser->getNumExpressions() == 0);

	//SubstitutionValues_t* value = analyser->getExpression(0);
	//fail_unless(value->k_value.empty());
	//fail_unless(value->x_value == "x");
	//fail_unless(value->y_value == "y");
	//fail_unless(value->z_value.empty());
	//fail_unless(value->type == TYPE_MINUS_X_PLUS_Y);
	//fail_unless(formulas_equal("-x + y", value->current));
	//fail_unless(formulas_equal("0", value->dxdt_expression));
	//fail_unless(formulas_equal("0", value->dydt_expression));
	//fail_unless(value->v_expression == NULL);
	//fail_unless(value->w_expression == NULL);
	//fail_unless(value->z_expression == NULL);
	//fail_unless(value->odeIndex == 0);
	//fail_unless(util_isNaN(value->k_real_value));
}
END_TEST



START_TEST(test_order_of_replacements)
{
	ConversionProperties props;
	props.addOption("inferReactions", true);

	SBMLRateRuleConverter* converter = new SBMLRateRuleConverter();
	converter->setProperties(&props);

	std::string filename(TestDataDirectory);
	filename += "mraterules7.xml";


	SBMLDocument* d = readSBMLFromFile(filename.c_str());
	Model* model = d->getModel();
	fail_unless(model != NULL);
	fail_unless(model->getNumParameters() == 2);

	converter->setDocument(d);
	converter->populateInitialODEinfo();
	converter->populateODEinfo();
	fail_unless(model->getNumParameters() == 3);

	delete converter;
	delete d;
}
END_TEST

START_TEST(test_order_of_replacements1)
{
	ConversionProperties props;
	props.addOption("inferReactions", true);

	SBMLRateRuleConverter* converter = new SBMLRateRuleConverter();
	converter->setProperties(&props);

	std::string filename(TestDataDirectory);
	filename += "mraterules7.xml";


	SBMLDocument* d = readSBMLFromFile(filename.c_str());
	Model* model = d->getModel();
	fail_unless(model != NULL);
	fail_unless(model->getNumParameters() == 2);

	converter->setDocument(d);
	converter->populateInitialODEinfo();
	ASTNode* currentNode = const_cast<ASTNode*>(model->getRule(0)->getMath())  ;

	ExpressionAnalyser* ea = new ExpressionAnalyser(model, converter->getOdePairs());

	SubstitutionValues_t* value = new SubstitutionValues_t;
	value->type = TYPE_UNKNOWN;
	value->k_real_value = util_NaN();
	value->dxdt_expression = NULL;
	value->dydt_expression = NULL;
	value->v_expression = NULL;
	value->w_expression = NULL;

	fail_unless(ea->analyseNode(currentNode, value));

	fail_unless(value->type == TYPE_K_PLUS_V_MINUS_X_MINUS_Y);
	fail_unless(util_isNaN(value->k_real_value));
	fail_unless(value->k_value == "k");
	fail_unless(value->x_value == "a");
    fail_unless(value->y_value == "b");

	fail_unless(formulas_equal("0", value->dxdt_expression));
	fail_unless(formulas_equal("0", value->dydt_expression));
	fail_unless(formulas_equal("v", value->v_expression));
    fail_unless(value->w_expression == NULL);

	fail_unless(formulas_equal("k + v - a - b", value->current));
	fail_unless(value->z_value == "");

	ea->analyse();

	fail_unless(ea->getNumExpressions() == 1);

	SubstitutionValues_t* value1 = new SubstitutionValues_t;
	value1 = ea->getExpression(0);
	fail_unless(ea->areIdenticalSubstitutionValues(value, value1));
	fail_unless(value1->odeIndex == 0);

	delete converter;
	delete d;
}
END_TEST


START_TEST(test_order_of_replacements2)
{
	ConversionProperties props;
	props.addOption("inferReactions", true);

	SBMLRateRuleConverter* converter = new SBMLRateRuleConverter();
	converter->setProperties(&props);

	std::string filename(TestDataDirectory);
	filename += "mraterules5.xml";


	SBMLDocument* d = readSBMLFromFile(filename.c_str());
	Model* model = d->getModel();
	fail_unless(model != NULL);

	converter->setDocument(d);
	converter->populateInitialODEinfo();
	ASTNode* currentNode = const_cast<ASTNode*>(model->getRule(1)->getMath()->getChild(1));
	cout << SBML_formulaToL3String(currentNode);
	ExpressionAnalyser* ea = new ExpressionAnalyser(model, converter->getOdePairs());
	ea->analyse();
	fail_unless(ea->getNumExpressions() == 2);

	SubstitutionValues_t* value1 = new SubstitutionValues_t;
	value1 = ea->getExpression(0);
	fail_unless(value1->type == TYPE_K_PLUS_V_MINUS_X_MINUS_Y);
	fail_unless(util_isNaN(value1->k_real_value));
	fail_unless(value1->k_value == "k");
	fail_unless(value1->x_value == "a");
	fail_unless(value1->y_value == "b");

	fail_unless(formulas_equal("-1 * (k - a - b)", value1->dxdt_expression));
	fail_unless(formulas_equal("c", value1->dydt_expression));
	fail_unless(formulas_equal("v", value1->v_expression ));
	fail_unless(value1->w_expression == NULL);

	fail_unless(formulas_equal("k + v - a - b", value1->current));
	fail_unless(value1->z_value == "");
	fail_unless(value1->odeIndex == 0);

	SubstitutionValues_t* value = new SubstitutionValues_t;
	value = ea->getExpression(1);

	fail_unless(value->type == TYPE_K_MINUS_X_MINUS_Y);
	fail_unless(util_isNaN(value->k_real_value));
	fail_unless(value->k_value == "k");
	fail_unless(value->x_value == "a");
	fail_unless(value->y_value == "b");

	fail_unless(formulas_equal("-1 * (k - a - b)", value->dxdt_expression));
	fail_unless(formulas_equal("c", value->dydt_expression));
	fail_unless(value->v_expression == NULL);
	fail_unless(value->w_expression == NULL);

	fail_unless(formulas_equal("k - a - b", value->current));
	fail_unless(value->z_value == "");

	fail_unless(value->odeIndex == 1);

	ea->orderExpressions();
	SubstitutionValues_t* value2 = new SubstitutionValues_t;
	value2 = ea->getExpression(0);
	fail_unless(ea->areIdenticalSubstitutionValues(value, value2));

	delete converter;
	delete d;
}
END_TEST

START_TEST(test_variations)
{
	ConversionProperties props;
	props.addOption("inferReactions", true);

	SBMLRateRuleConverter* converter = new SBMLRateRuleConverter();
	converter->setProperties(&props);

	SBMLDocument* d = new SBMLDocument(3, 1);
	Model* model = setupModel(d);

	RateRule* rr = model->createRateRule();
    rr->setVariable("a");
    rr->setMath(SBML_parseFormula("k-x-y"));
	RateRule* rr1 = model->createRateRule();
	rr1->setVariable("b");
	rr1->setMath(SBML_parseFormula("-1*(k-x-y)"));
	cout << d->toSBML() << endl;


	converter->setDocument(d);
	//converter->populateInitialODEinfo();
	//ExpressionAnalyser* ea = new ExpressionAnalyser(model, converter->getOde());
	//ea->analyse();


	fail_unless(converter->convert() == LIBSBML_OPERATION_SUCCESS);

	cout << d->toSBML() << endl;
    delete converter;
    delete d;

}
END_TEST

Suite *
create_suite_TestExpressionAnalyser (void)
{ 
	bool testing = true;
Suite *suite = suite_create("ExpressionAnalyser");
  TCase *tcase = tcase_create("ExpressionAnalyser");
  tcase_add_checked_fixture(tcase,
	  ExpressionAnalyser_setup, ExpressionAnalyser_teardown);

  if (testing)
  {
	  tcase_add_test(tcase, test_order_expressions_1);
  }
  else
  {
	  tcase_add_test(tcase, test_analyse); //k-x-y
	  tcase_add_test(tcase, test_analyse_1); //k+v-x-y
	  tcase_add_test(tcase, test_analyse_2); //k-x+w-y
	  tcase_add_test(tcase, test_analyse_3); //k-x
	  tcase_add_test(tcase, test_analyse_4); //k+v-x
	  tcase_add_test(tcase, test_analyse_same_expression); //k-x-y
	  tcase_add_test(tcase, test_analyse_different_expression); //k-x-y
	  tcase_add_test(tcase, test_analyse_1_same); //k+v-x-y
	  tcase_add_test(tcase, test_analyse_1_two_terms); //(k+v-x-y)+(k-x)
	  tcase_add_test(tcase, test_analyse_1_different); //k+v-x-y
	  tcase_add_test(tcase, test_reorder_minusXplusYIteratively_simple);
	  //tcase_add_test(tcase, test_order_of_replacements1);
	  //tcase_add_test(tcase, test_order_of_replacements2);

  }
  suite_add_tcase(suite, tcase);

  return suite;

}
END_C_DECLS

