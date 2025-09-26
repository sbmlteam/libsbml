/**
 * @file    TestExpressionAnalyser.cpp
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
static unsigned int count = 0;


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

    delete analyser;
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

	delete analyser;
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

	delete analyser;
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

	delete analyser;
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

	delete analyser;
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


	delete analyser;
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

	delete analyser;
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

	delete analyser;
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

	delete analyser;
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

	delete analyser;
}
END_TEST

START_TEST(test_order_expressions_1)
{
	RateRule* rr = d->getModel()->createRateRule();
	rr->setVariable("a");
	rr->setMath(SBML_parseFormula("k - x + w - y"));

	RateRule* rrr = d->getModel()->createRateRule();
	rrr->setVariable("b");
	rrr->setMath(SBML_parseFormula("k-x-y"));
	converter->populateInitialODEinfo();
	ExpressionAnalyser* analyser = new ExpressionAnalyser(m, converter->getOdePairs());

	fail_unless(analyser->getNumExpressions() == 0);

	analyser->analyse();

	fail_unless(analyser->getNumExpressions() == 2);
	SubstitutionValues_t* value = analyser->getExpression(0);
	fail_unless(value->type == TYPE_K_MINUS_X_PLUS_W_MINUS_Y);
	SubstitutionValues_t* value1 = analyser->getExpression(1);
	fail_unless(value1->type == TYPE_K_MINUS_X_MINUS_Y);

	analyser->orderExpressions();
    fail_unless(analyser->getNumExpressions() == 2);
    SubstitutionValues_t* value2 = analyser->getExpression(0);
    fail_unless(value2->type == TYPE_K_MINUS_X_MINUS_Y);
    SubstitutionValues_t* value3 = analyser->getExpression(1);
    fail_unless(value3->type == TYPE_K_MINUS_X_PLUS_W_MINUS_Y);

	delete analyser;
}
END_TEST

START_TEST(test_order_expressions_2)
{
	RateRule* rr = d->getModel()->createRateRule();
	rr->setVariable("a");
	rr->setMath(SBML_parseFormula("k + v - x - y"));

	RateRule* rrr = d->getModel()->createRateRule();
	rrr->setVariable("b");
	rrr->setMath(SBML_parseFormula("k-x-y"));
	converter->populateInitialODEinfo();
	ExpressionAnalyser* analyser = new ExpressionAnalyser(m, converter->getOdePairs());

	fail_unless(analyser->getNumExpressions() == 0);

	analyser->analyse();

	fail_unless(analyser->getNumExpressions() == 2);
	SubstitutionValues_t* value = analyser->getExpression(0);
	fail_unless(value->type == TYPE_K_PLUS_V_MINUS_X_MINUS_Y);
	SubstitutionValues_t* value1 = analyser->getExpression(1);
	fail_unless(value1->type == TYPE_K_MINUS_X_MINUS_Y);

	analyser->orderExpressions();

	fail_unless(analyser->getNumExpressions() == 2);
	SubstitutionValues_t* value2 = analyser->getExpression(0);
	fail_unless(value2->type == TYPE_K_MINUS_X_MINUS_Y);
	SubstitutionValues_t* value3 = analyser->getExpression(1);
	fail_unless(value3->type == TYPE_K_PLUS_V_MINUS_X_MINUS_Y);

	delete analyser;
}
END_TEST

START_TEST(test_order_expressions_3)
{
	RateRule* rr = d->getModel()->createRateRule();
	rr->setVariable("a");
	rr->setMath(SBML_parseFormula("k - x - y"));

	RateRule* rrr = d->getModel()->createRateRule();
	rrr->setVariable("b");
	rrr->setMath(SBML_parseFormula("k-x"));
	converter->populateInitialODEinfo();
	ExpressionAnalyser* analyser = new ExpressionAnalyser(m, converter->getOdePairs());

	fail_unless(analyser->getNumExpressions() == 0);

	analyser->analyse();

	fail_unless(analyser->getNumExpressions() == 2);
	SubstitutionValues_t* value = analyser->getExpression(0);
	fail_unless(value->type == TYPE_K_MINUS_X_MINUS_Y);
	SubstitutionValues_t* value1 = analyser->getExpression(1);
	fail_unless(value1->type == TYPE_K_MINUS_X);

	analyser->orderExpressions();
	fail_unless(analyser->getNumExpressions() == 2);
	SubstitutionValues_t* value2 = analyser->getExpression(0);
	fail_unless(value2->type == TYPE_K_MINUS_X_MINUS_Y);
	SubstitutionValues_t* value3 = analyser->getExpression(1);
	fail_unless(value3->type == TYPE_K_MINUS_X);

	delete analyser;
}
END_TEST

START_TEST(test_order_expressions_4)
{
	RateRule* rr = d->getModel()->createRateRule();
	rr->setVariable("a");
	rr->setMath(SBML_parseFormula("k + v - x - y"));

	RateRule* rrr = d->getModel()->createRateRule();
	rrr->setVariable("b");
	rrr->setMath(SBML_parseFormula("k-x"));
	converter->populateInitialODEinfo();
	ExpressionAnalyser* analyser = new ExpressionAnalyser(m, converter->getOdePairs());

	fail_unless(analyser->getNumExpressions() == 0);

	analyser->analyse();

	fail_unless(analyser->getNumExpressions() == 2);
	SubstitutionValues_t* value = analyser->getExpression(0);
	fail_unless(value->type == TYPE_K_PLUS_V_MINUS_X_MINUS_Y);
	SubstitutionValues_t* value1 = analyser->getExpression(1);
	fail_unless(value1->type == TYPE_K_MINUS_X);

	analyser->orderExpressions();
	fail_unless(analyser->getNumExpressions() == 2);
	SubstitutionValues_t* value2 = analyser->getExpression(0);
	fail_unless(value2->type == TYPE_K_PLUS_V_MINUS_X_MINUS_Y);
	SubstitutionValues_t* value3 = analyser->getExpression(1);
	fail_unless(value3->type == TYPE_K_MINUS_X);

	delete analyser;
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

	delete analyser;
}
END_TEST

START_TEST(test_analyse_replace)
{
	RateRule* rr = d->getModel()->createRateRule();
	rr->setVariable("a");
	rr->setMath(SBML_parseFormula("k-x-y"));
	converter->populateInitialODEinfo();
	ExpressionAnalyser* analyser = new ExpressionAnalyser(m, converter->getOdePairs());

	fail_unless(analyser->getNumExpressions() == 0);

	analyser->detectHiddenSpecies(true);

	fail_unless(analyser->getNumExpressions() == 1);
	SubstitutionValues_t* value = analyser->getExpression(0);
	fail_unless(value->k_value == "k");
	fail_unless(value->x_value == "x");
	fail_unless(value->y_value == "y");
	fail_unless(value->z_value == "newVar1");
	fail_unless(value->type == TYPE_K_MINUS_X_MINUS_Y);
	fail_unless(formulas_equal("k - x - y", value->current));
	fail_unless(formulas_equal("0", value->dxdt_expression));
	fail_unless(formulas_equal("0", value->dydt_expression));
	fail_unless(value->v_expression == NULL);
	fail_unless(value->w_expression == NULL);
	fail_unless(formulas_equal("newVar1", value->z_expression));
	fail_unless(value->odeIndex == 0);
	fail_unless(util_isNaN(value->k_real_value));

	fail_unless(analyser->getNumHiddenNodes() == 1);
	delete analyser;
}
END_TEST

START_TEST(test_analyse_same_expression_replace)
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

	analyser->detectHiddenSpecies(true);

	fail_unless(analyser->getNumExpressions() == 1);
	SubstitutionValues_t* value = analyser->getExpression(0);
	fail_unless(value->k_value == "k");
	fail_unless(value->x_value == "x");
	fail_unless(value->y_value == "y");
	fail_unless(value->z_value == "newVar1");
	fail_unless(value->type == TYPE_K_MINUS_X_MINUS_Y);
	fail_unless(formulas_equal("k - x - y", value->current));
	fail_unless(formulas_equal("0", value->dxdt_expression));
	fail_unless(formulas_equal("0", value->dydt_expression));
	fail_unless(value->v_expression == NULL);
	fail_unless(value->w_expression == NULL);
	fail_unless(formulas_equal("newVar1", value->z_expression));
	fail_unless(value->odeIndex == 0);
	fail_unless(util_isNaN(value->k_real_value));
	fail_unless(analyser->getNumHiddenNodes() == 1);

	delete analyser;
}
END_TEST

START_TEST(test_analyse_different_expression_replace)
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

	analyser->detectHiddenSpecies(true);

	fail_unless(analyser->getNumExpressions() == 2);
	SubstitutionValues_t* value = analyser->getExpression(0);
	fail_unless(value->k_value == "k");
	fail_unless(value->x_value == "x");
	fail_unless(value->y_value == "y");
	fail_unless(value->z_value == "newVar1");
	fail_unless(value->type == TYPE_K_MINUS_X_MINUS_Y);
	fail_unless(formulas_equal("k - x - y", value->current));
	fail_unless(formulas_equal("0", value->dxdt_expression));
	fail_unless(formulas_equal("0", value->dydt_expression));
	fail_unless(value->v_expression == NULL);
	fail_unless(value->w_expression == NULL);
	fail_unless(formulas_equal("newVar1", value->z_expression));
	fail_unless(value->odeIndex == 0);
	fail_unless(util_isNaN(value->k_real_value));

	SubstitutionValues_t* value1 = analyser->getExpression(1);
	fail_unless(value1->k_value == "k");
	fail_unless(value1->x_value == "x");
	fail_unless(value1->y_value == "a");
	fail_unless(value1->z_value == "newVar2");
	fail_unless(value1->type == TYPE_K_MINUS_X_MINUS_Y);
	fail_unless(formulas_equal("k - x - a", value1->current));
	fail_unless(formulas_equal("0", value1->dxdt_expression));
	fail_unless(formulas_equal("k - x - y", value1->dydt_expression));
	fail_unless(value1->v_expression == NULL);
	fail_unless(value1->w_expression == NULL);
	fail_unless(formulas_equal("newVar2", value1->z_expression));
	fail_unless(value1->odeIndex == 1);
	fail_unless(util_isNaN(value1->k_real_value));
	fail_unless(analyser->getNumHiddenNodes() == 2);

	delete analyser;
}
END_TEST

START_TEST(test_analyse_1_replace)
{
	RateRule* rr = d->getModel()->createRateRule();
	rr->setVariable("a");
	rr->setMath(SBML_parseFormula("k + v - x - y"));
	converter->populateInitialODEinfo();
	ExpressionAnalyser* analyser = new ExpressionAnalyser(m, converter->getOdePairs());

	fail_unless(analyser->getNumExpressions() == 0);

	analyser->detectHiddenSpecies(true);

	fail_unless(analyser->getNumExpressions() == 1);
	SubstitutionValues_t* value = analyser->getExpression(0);
	fail_unless(value->k_value == "k");
	fail_unless(value->x_value == "x");
	fail_unless(value->y_value == "y");
	fail_unless(value->z_value == "newVar1");
	fail_unless(value->type == TYPE_K_PLUS_V_MINUS_X_MINUS_Y);
	fail_unless(formulas_equal("k + v - x - y", value->current));
	fail_unless(formulas_equal("0", value->dxdt_expression));
	fail_unless(formulas_equal("0", value->dydt_expression));
	fail_unless(formulas_equal("v", value->v_expression));
	fail_unless(value->w_expression == NULL);
	fail_unless(formulas_equal("newVar1 + v", value->z_expression));
	fail_unless(value->odeIndex == 0);
	fail_unless(util_isNaN(value->k_real_value));
	fail_unless(analyser->getNumHiddenNodes() == 1);

	delete analyser;
}
END_TEST

START_TEST(test_analyse_2_replace)
{
	RateRule* rr = d->getModel()->createRateRule();
	rr->setVariable("a");
	rr->setMath(SBML_parseFormula("k - x + w - y"));
	converter->populateInitialODEinfo();
	ExpressionAnalyser* analyser = new ExpressionAnalyser(m, converter->getOdePairs());

	fail_unless(analyser->getNumExpressions() == 0);

	analyser->detectHiddenSpecies(true);

	fail_unless(analyser->getNumExpressions() == 1);
	SubstitutionValues_t* value = analyser->getExpression(0);
	fail_unless(value->k_value == "k");
	fail_unless(value->x_value == "x");
	fail_unless(value->y_value == "y");
	fail_unless(value->z_value == "newVar1");
	fail_unless(value->type == TYPE_K_MINUS_X_PLUS_W_MINUS_Y);
	fail_unless(formulas_equal("w + (k - x) - y", value->current));
	fail_unless(formulas_equal("0", value->dxdt_expression));
	fail_unless(formulas_equal("0", value->dydt_expression));
	fail_unless(formulas_equal("w", value->w_expression));
	fail_unless(value->v_expression == NULL);
	fail_unless(formulas_equal("newVar1 + w", value->z_expression));
	fail_unless(value->odeIndex == 0);
	fail_unless(util_isNaN(value->k_real_value));
	fail_unless(analyser->getNumHiddenNodes() == 1);

	delete analyser;
}
END_TEST

START_TEST(test_analyse_3_replace)
{
	RateRule* rr = d->getModel()->createRateRule();
	rr->setVariable("a");
	rr->setMath(SBML_parseFormula("k - x"));
	converter->populateInitialODEinfo();
	ExpressionAnalyser* analyser = new ExpressionAnalyser(m, converter->getOdePairs());

	fail_unless(analyser->getNumExpressions() == 0);

	analyser->detectHiddenSpecies(true);

	fail_unless(analyser->getNumExpressions() == 1);
	SubstitutionValues_t* value = analyser->getExpression(0);
	fail_unless(value->k_value == "k");
	fail_unless(value->x_value == "x");
	fail_unless(value->y_value.empty());
	fail_unless(value->z_value == "newVar1");
	fail_unless(value->type == TYPE_K_MINUS_X);
	fail_unless(formulas_equal("k - x", value->current));
	fail_unless(formulas_equal("0", value->dxdt_expression));
	fail_unless(value->dydt_expression == NULL);
	fail_unless(value->v_expression == NULL);
	fail_unless(value->w_expression == NULL);
	fail_unless(formulas_equal("newVar1", value->z_expression));
	fail_unless(value->odeIndex == 0);
	fail_unless(util_isNaN(value->k_real_value));
	fail_unless(analyser->getNumHiddenNodes() == 1);

	delete analyser;
}
END_TEST

START_TEST(test_analyse_4_replace)
{
	RateRule* rr = d->getModel()->createRateRule();
	rr->setVariable("a");
	rr->setMath(SBML_parseFormula("k + v - x"));
	converter->populateInitialODEinfo();
	ExpressionAnalyser* analyser = new ExpressionAnalyser(m, converter->getOdePairs());

	fail_unless(analyser->getNumExpressions() == 0);

	analyser->detectHiddenSpecies(true);

	fail_unless(analyser->getNumExpressions() == 1);
	SubstitutionValues_t* value = analyser->getExpression(0);
	fail_unless(value->k_value == "k");
	fail_unless(value->x_value == "x");
	fail_unless(value->y_value.empty());
	fail_unless(value->z_value == "newVar1");
	fail_unless(value->type == TYPE_K_PLUS_V_MINUS_X);
	fail_unless(formulas_equal("k + v - x", value->current));
	fail_unless(formulas_equal("0", value->dxdt_expression));
	fail_unless(value->dydt_expression == NULL);
	fail_unless(formulas_equal("v", value->v_expression));
	fail_unless(value->w_expression == NULL);
	fail_unless(formulas_equal("newVar1 + v", value->z_expression));
	fail_unless(value->odeIndex == 0);
	fail_unless(util_isNaN(value->k_real_value));
	fail_unless(analyser->getNumHiddenNodes() == 1);

	delete analyser;
}
END_TEST

START_TEST(test_analyse_1_different_replace)
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

	analyser->detectHiddenSpecies(true);

	fail_unless(analyser->getNumExpressions() == 2);
	SubstitutionValues_t* value = analyser->getExpression(0);
	fail_unless(value->k_value == "k");
	fail_unless(value->x_value == "x");
	fail_unless(value->y_value == "y");
	fail_unless(value->z_value == "newVar1");
	fail_unless(value->type == TYPE_K_PLUS_V_MINUS_X_MINUS_Y);
	fail_unless(formulas_equal("k + v - x - y", value->current));
	fail_unless(formulas_equal("0", value->dxdt_expression));
	fail_unless(formulas_equal("0", value->dydt_expression));
	fail_unless(formulas_equal("v", value->v_expression));
	fail_unless(value->w_expression == NULL);
	fail_unless(formulas_equal("newVar1 + v", value->z_expression));
	fail_unless(value->odeIndex == 0);
	fail_unless(util_isNaN(value->k_real_value));

	SubstitutionValues_t* value1 = analyser->getExpression(1);
	fail_unless(value1->k_value == "k");
	fail_unless(value1->x_value == "x");
	fail_unless(value1->y_value == "a");
	fail_unless(value1->z_value == "newVar2");
	fail_unless(value1->type == TYPE_K_PLUS_V_MINUS_X_MINUS_Y);
	fail_unless(formulas_equal("k + v - x - a", value1->current));
	fail_unless(formulas_equal("0", value1->dxdt_expression));
	fail_unless(formulas_equal("k + v - x - y", value1->dydt_expression));
	fail_unless(formulas_equal("v", value1->v_expression));
	fail_unless(value1->w_expression == NULL);
	fail_unless(formulas_equal("newVar2 + v", value1->z_expression));
	fail_unless(value1->odeIndex == 1);
	fail_unless(util_isNaN(value1->k_real_value));
	fail_unless(analyser->getNumHiddenNodes() == 2);

	delete analyser;
}
END_TEST

START_TEST(test_analyse_1_different_replace_1)
{
	RateRule* rr = d->getModel()->createRateRule();
	rr->setVariable("a");
	rr->setMath(SBML_parseFormula("k - x - y"));
	RateRule* rr1 = d->getModel()->createRateRule();
	rr1->setVariable("b");
	rr1->setMath(SBML_parseFormula("k + v - x - y"));
	converter->populateInitialODEinfo();
	ExpressionAnalyser* analyser = new ExpressionAnalyser(m, converter->getOdePairs());

	fail_unless(analyser->getNumExpressions() == 0);

	analyser->detectHiddenSpecies(true);

	fail_unless(analyser->getNumExpressions() == 2);
	SubstitutionValues_t* value = analyser->getExpression(0);
	fail_unless(value->k_value == "k");
	fail_unless(value->x_value == "x");
	fail_unless(value->y_value == "y");
	fail_unless(value->z_value == "newVar1");
	fail_unless(value->type == TYPE_K_MINUS_X_MINUS_Y);
	fail_unless(formulas_equal("k - x - y", value->current));
	fail_unless(formulas_equal("0", value->dxdt_expression));
	fail_unless(formulas_equal("0", value->dydt_expression));
	fail_unless(value->v_expression == NULL);
	fail_unless(value->w_expression == NULL);
	fail_unless(formulas_equal("newVar1", value->z_expression));
	fail_unless(value->odeIndex == 0);
	fail_unless(util_isNaN(value->k_real_value));

	SubstitutionValues_t* value1 = analyser->getExpression(1);
	fail_unless(value1->k_value == "k");
	fail_unless(value1->x_value == "x");
	fail_unless(value1->y_value == "y");
	fail_unless(value1->z_value == "newVar1");
	fail_unless(value1->type == TYPE_K_PLUS_V_MINUS_X_MINUS_Y);
	fail_unless(formulas_equal("k + v - x - y", value1->current));
	fail_unless(formulas_equal("0", value1->dxdt_expression));
	fail_unless(formulas_equal("0", value1->dydt_expression));
	fail_unless(formulas_equal("v", value1->v_expression));
	fail_unless(value1->w_expression == NULL);
	fail_unless(formulas_equal("newVar1 + v", value1->z_expression));
	fail_unless(value1->odeIndex == 1);
	fail_unless(util_isNaN(value1->k_real_value));
	fail_unless(analyser->getNumHiddenNodes() == 1);

	delete analyser;
}
END_TEST

START_TEST(test_analyse_1_different_replace_2)
{
	RateRule* rr = d->getModel()->createRateRule();
	rr->setVariable("a");
	rr->setMath(SBML_parseFormula("k - x - y"));
	RateRule* rr1 = d->getModel()->createRateRule();
	rr1->setVariable("b");
	rr1->setMath(SBML_parseFormula("k + v - x - a"));
	converter->populateInitialODEinfo();
	ExpressionAnalyser* analyser = new ExpressionAnalyser(m, converter->getOdePairs());

	fail_unless(analyser->getNumExpressions() == 0);

	analyser->detectHiddenSpecies(true);

	fail_unless(analyser->getNumExpressions() == 2);
	SubstitutionValues_t* value = analyser->getExpression(0);
	fail_unless(value->k_value == "k");
	fail_unless(value->x_value == "x");
	fail_unless(value->y_value == "y");
	fail_unless(value->z_value == "newVar1");
	fail_unless(value->type == TYPE_K_MINUS_X_MINUS_Y);
	fail_unless(formulas_equal("k - x - y", value->current));
	fail_unless(formulas_equal("0", value->dxdt_expression));
	fail_unless(formulas_equal("0", value->dydt_expression));
	fail_unless(value->v_expression == NULL);
	fail_unless(value->w_expression == NULL);
	fail_unless(formulas_equal("newVar1", value->z_expression));
	fail_unless(value->odeIndex == 0);
	fail_unless(util_isNaN(value->k_real_value));

	SubstitutionValues_t* value1 = analyser->getExpression(1);
	fail_unless(value1->k_value == "k");
	fail_unless(value1->x_value == "x");
	fail_unless(value1->y_value == "a");
	fail_unless(value1->z_value == "newVar2");
	fail_unless(value1->type == TYPE_K_PLUS_V_MINUS_X_MINUS_Y);
	fail_unless(formulas_equal("v", value1->v_expression));
	fail_unless(formulas_equal("newVar2 + v", value1->z_expression));
	fail_unless(value1->odeIndex == 1);
	fail_unless(util_isNaN(value1->k_real_value));
	fail_unless(analyser->getNumHiddenNodes() == 2);

	delete analyser;
}
END_TEST

START_TEST(test_analyse_1_different_replace_3)
{
	RateRule* rr = d->getModel()->createRateRule();
	rr->setVariable("a");
	rr->setMath(SBML_parseFormula("k - x - y"));
	RateRule* rr1 = d->getModel()->createRateRule();
	rr1->setVariable("b");
	rr1->setMath(SBML_parseFormula("k - x + w - y"));
	converter->populateInitialODEinfo();
	ExpressionAnalyser* analyser = new ExpressionAnalyser(m, converter->getOdePairs());

	fail_unless(analyser->getNumExpressions() == 0);

	analyser->detectHiddenSpecies(true);

	fail_unless(analyser->getNumExpressions() == 2);
	SubstitutionValues_t* value = analyser->getExpression(0);
	fail_unless(value->k_value == "k");
	fail_unless(value->x_value == "x");
	fail_unless(value->y_value == "y");
	fail_unless(value->z_value == "newVar1");
	fail_unless(value->type == TYPE_K_MINUS_X_MINUS_Y);
	fail_unless(formulas_equal("k - x - y", value->current));
	fail_unless(formulas_equal("0", value->dxdt_expression));
	fail_unless(formulas_equal("0", value->dydt_expression));
	fail_unless(value->v_expression == NULL);
	fail_unless(value->w_expression == NULL);
	fail_unless(formulas_equal("newVar1", value->z_expression));
	fail_unless(value->odeIndex == 0);
	fail_unless(util_isNaN(value->k_real_value));

	SubstitutionValues_t* value1 = analyser->getExpression(1);
	fail_unless(value1->k_value == "k");
	fail_unless(value1->x_value == "x");
	fail_unless(value1->y_value == "y");
	fail_unless(value1->z_value == "newVar1");
	fail_unless(value1->type == TYPE_K_MINUS_X_PLUS_W_MINUS_Y);
	fail_unless(formulas_equal("w + (k - x) - y", value1->current));
	fail_unless(formulas_equal("0", value1->dxdt_expression));
	fail_unless(formulas_equal("0", value1->dydt_expression));
	fail_unless(formulas_equal("w", value1->w_expression));
	fail_unless(formulas_equal("newVar1 + w", value1->z_expression));
	fail_unless(value1->odeIndex == 1);
	fail_unless(util_isNaN(value1->k_real_value));
	fail_unless(analyser->getNumHiddenNodes() == 1);

	delete analyser;
}
END_TEST

START_TEST(test_analyse_1_different_replace_4)
{
	RateRule* rr = d->getModel()->createRateRule();
	rr->setVariable("a");
	rr->setMath(SBML_parseFormula("k - x - y"));
	RateRule* rr1 = d->getModel()->createRateRule();
	rr1->setVariable("b");
	rr1->setMath(SBML_parseFormula("k - x + w - a"));
	converter->populateInitialODEinfo();
	ExpressionAnalyser* analyser = new ExpressionAnalyser(m, converter->getOdePairs());

	fail_unless(analyser->getNumExpressions() == 0);

	analyser->detectHiddenSpecies(true);

	fail_unless(analyser->getNumExpressions() == 2);
	SubstitutionValues_t* value = analyser->getExpression(0);
	fail_unless(value->k_value == "k");
	fail_unless(value->x_value == "x");
	fail_unless(value->y_value == "y");
	fail_unless(value->z_value == "newVar1");
	fail_unless(value->type == TYPE_K_MINUS_X_MINUS_Y);
	fail_unless(formulas_equal("k - x - y", value->current));
	fail_unless(formulas_equal("0", value->dxdt_expression));
	fail_unless(formulas_equal("0", value->dydt_expression));
	fail_unless(value->v_expression == NULL);
	fail_unless(value->w_expression == NULL);
	fail_unless(formulas_equal("newVar1", value->z_expression));
	fail_unless(value->odeIndex == 0);
	fail_unless(util_isNaN(value->k_real_value));

	SubstitutionValues_t* value1 = analyser->getExpression(1);
	fail_unless(value1->k_value == "k");
	fail_unless(value1->x_value == "x");
	fail_unless(value1->y_value == "a");
	fail_unless(value1->z_value == "newVar2");
	fail_unless(value1->type == TYPE_K_MINUS_X_PLUS_W_MINUS_Y);
	fail_unless(formulas_equal("w + (k - x) - a", value1->current));
	fail_unless(formulas_equal("0", value1->dxdt_expression));
	fail_unless(formulas_equal("k - x - y", value1->dydt_expression));
	fail_unless(formulas_equal("w", value1->w_expression));
	fail_unless(formulas_equal("newVar2 + w", value1->z_expression));
	fail_unless(value1->odeIndex == 1);
	fail_unless(util_isNaN(value1->k_real_value));
	fail_unless(analyser->getNumHiddenNodes() == 2);

	delete analyser;
}
END_TEST

START_TEST(test_analyse_1_different_replace_5)
{
	setupSpecies(d->getModel(), "q", "c");
	RateRule* rr = d->getModel()->createRateRule();
	rr->setVariable("a");
	rr->setMath(SBML_parseFormula("k - x - y"));
	RateRule* rr2 = d->getModel()->createRateRule();
	rr2->setVariable("q");
	rr2->setMath(SBML_parseFormula("k - x - a"));
	RateRule* rr1 = d->getModel()->createRateRule();
	rr1->setVariable("b");
	rr1->setMath(SBML_parseFormula("k + v - x - y"));
	converter->populateInitialODEinfo();
	ExpressionAnalyser* analyser = new ExpressionAnalyser(m, converter->getOdePairs());

	fail_unless(analyser->getNumExpressions() == 0);

	analyser->detectHiddenSpecies(true);

	fail_unless(analyser->getNumExpressions() == 3);
	SubstitutionValues_t* value = analyser->getExpression(0);
	fail_unless(value->k_value == "k");
	fail_unless(value->x_value == "x");
	fail_unless(value->y_value == "y");
	fail_unless(value->z_value == "newVar1");
	fail_unless(value->type == TYPE_K_MINUS_X_MINUS_Y);
	fail_unless(formulas_equal("k - x - y", value->current));
	fail_unless(value->v_expression == NULL);
	fail_unless(formulas_equal("newVar1", value->z_expression));
	fail_unless(value->odeIndex == 0);
	fail_unless(util_isNaN(value->k_real_value));

	SubstitutionValues_t* value2 = analyser->getExpression(1);
	fail_unless(value2->k_value == "k");
	fail_unless(value2->x_value == "x");
	fail_unless(value2->y_value == "a");
	fail_unless(value2->z_value == "newVar2");
	fail_unless(value2->type == TYPE_K_MINUS_X_MINUS_Y);
	fail_unless(formulas_equal("k - x - a", value2->current));
	fail_unless(value2->v_expression == NULL);
	fail_unless(formulas_equal("newVar2", value2->z_expression));
	fail_unless(value2->odeIndex == 2);
	fail_unless(util_isNaN(value2->k_real_value));


	SubstitutionValues_t* value1 = analyser->getExpression(2);
	fail_unless(value1->k_value == "k");
	fail_unless(value1->x_value == "x");
	fail_unless(value1->y_value == "y");
	fail_unless(value1->z_value == "newVar1");
	fail_unless(value1->type == TYPE_K_PLUS_V_MINUS_X_MINUS_Y);
	fail_unless(formulas_equal("k + v - x - y", value1->current));
	fail_unless(formulas_equal("v", value1->v_expression));
	fail_unless(formulas_equal("newVar1 + v", value1->z_expression));
	fail_unless(value1->odeIndex == 1);
	fail_unless(util_isNaN(value1->k_real_value));
	fail_unless(analyser->getNumHiddenNodes() == 2);

	delete analyser;
}
END_TEST

START_TEST(test_analyse_1_different_replace_6)
{
	setupSpecies(d->getModel(), "q", "c");
	RateRule* rr = d->getModel()->createRateRule();
	rr->setVariable("a");
	rr->setMath(SBML_parseFormula("k - x - y"));
	RateRule* rr1 = d->getModel()->createRateRule();
	rr1->setVariable("b");
	rr1->setMath(SBML_parseFormula("k + v - x - a"));
	RateRule* rr2 = d->getModel()->createRateRule();
	rr2->setVariable("q");
	rr2->setMath(SBML_parseFormula("k + v - x - y"));
	converter->populateInitialODEinfo();
	ExpressionAnalyser* analyser = new ExpressionAnalyser(m, converter->getOdePairs());

	fail_unless(analyser->getNumExpressions() == 0);

	analyser->detectHiddenSpecies(true);

	fail_unless(analyser->getNumExpressions() == 3);
	SubstitutionValues_t* value = analyser->getExpression(0);
	fail_unless(value->k_value == "k");
	fail_unless(value->x_value == "x");
	fail_unless(value->y_value == "y");
	fail_unless(value->z_value == "newVar1");
	fail_unless(value->type == TYPE_K_MINUS_X_MINUS_Y);
	fail_unless(formulas_equal("k - x - y", value->current));
	fail_unless(value->v_expression == NULL);
	fail_unless(formulas_equal("newVar1", value->z_expression));
	fail_unless(value->odeIndex == 0);
	fail_unless(util_isNaN(value->k_real_value));

	SubstitutionValues_t* value2 = analyser->getExpression(1);
	fail_unless(value2->k_value == "k");
	fail_unless(value2->x_value == "x");
	fail_unless(value2->y_value == "a");
	fail_unless(value2->z_value == "newVar2");
	fail_unless(value2->type == TYPE_K_PLUS_V_MINUS_X_MINUS_Y);
	fail_unless(formulas_equal("k + v - x - a", value2->current));
	fail_unless(formulas_equal("v", value2->v_expression));
	fail_unless(formulas_equal("newVar2 + v", value2->z_expression));
	fail_unless(value2->odeIndex == 1);
	fail_unless(util_isNaN(value2->k_real_value));


	SubstitutionValues_t* value1 = analyser->getExpression(2);
	fail_unless(value1->k_value == "k");
	fail_unless(value1->x_value == "x");
	fail_unless(value1->y_value == "y");
	fail_unless(value1->z_value == "newVar1");
	fail_unless(value1->type == TYPE_K_PLUS_V_MINUS_X_MINUS_Y);
	fail_unless(formulas_equal("k + v - x - y", value1->current));
	fail_unless(formulas_equal("v", value1->v_expression));
	fail_unless(formulas_equal("newVar1 + v", value1->z_expression));
	fail_unless(value1->odeIndex == 2);
	fail_unless(util_isNaN(value1->k_real_value));
	fail_unless(analyser->getNumHiddenNodes() == 2);

	delete analyser;
}
END_TEST



START_TEST(test_analyse_1_two_terms_replace)
{
	RateRule* rr = d->getModel()->createRateRule();
	rr->setVariable("a");
	rr->setMath(SBML_parseFormula("(k + v - x - y) + (k - x)"));

	converter->populateInitialODEinfo();
	ExpressionAnalyser* analyser = new ExpressionAnalyser(m, converter->getOdePairs());

	fail_unless(analyser->getNumExpressions() == 0);

	analyser->detectHiddenSpecies(true);

	fail_unless(analyser->getNumExpressions() == 2);
	SubstitutionValues_t* value = analyser->getExpression(0);
	fail_unless(value->k_value == "k");
	fail_unless(value->x_value == "x");
	fail_unless(value->y_value == "y");
	fail_unless(value->z_value == "newVar1");
	fail_unless(value->type == TYPE_K_PLUS_V_MINUS_X_MINUS_Y);
	fail_unless(formulas_equal("k + v - x - y", value->current));
	fail_unless(formulas_equal("0", value->dxdt_expression));
	fail_unless(formulas_equal("0", value->dydt_expression));
	fail_unless(formulas_equal("v", value->v_expression));
	fail_unless(value->w_expression == NULL);
	fail_unless(formulas_equal("newVar1 + v", value->z_expression));
	fail_unless(value->odeIndex == 0);
	fail_unless(util_isNaN(value->k_real_value));

	SubstitutionValues_t* value1 = analyser->getExpression(1);
	fail_unless(value1->k_value == "k");
	fail_unless(value1->x_value == "x");
	fail_unless(value1->y_value.empty());
	fail_unless(value1->z_value == "newVar2");
	fail_unless(value1->type == TYPE_K_MINUS_X);
	fail_unless(formulas_equal("k - x", value1->current));
	fail_unless(formulas_equal("0", value1->dxdt_expression));
	fail_unless(value1->dydt_expression == NULL);
	fail_unless(value1->v_expression == NULL);
	fail_unless(value1->w_expression == NULL);
	fail_unless(formulas_equal("newVar2", value1->z_expression));	
	fail_unless(value1->odeIndex == 0);
	fail_unless(util_isNaN(value1->k_real_value));

	fail_unless(analyser->getNumHiddenNodes() == 2);

	delete analyser;
}
END_TEST

START_TEST(test_analyse_2_replace_1)
{
	RateRule* rr = d->getModel()->createRateRule();
	rr->setVariable("a");
	rr->setMath(SBML_parseFormula("k - x  + w - y"));
	RateRule* rr1 = d->getModel()->createRateRule();
	rr1->setVariable("b");
	rr1->setMath(SBML_parseFormula("k - x + w - a"));
	converter->populateInitialODEinfo();
	ExpressionAnalyser* analyser = new ExpressionAnalyser(m, converter->getOdePairs());

	fail_unless(analyser->getNumExpressions() == 0);

	analyser->detectHiddenSpecies(true);

	fail_unless(analyser->getNumExpressions() == 2);
	SubstitutionValues_t* value = analyser->getExpression(0);
	fail_unless(value->k_value == "k");
	fail_unless(value->x_value == "x");
	fail_unless(value->y_value == "y");
	fail_unless(value->z_value == "newVar1");
	fail_unless(value->type == TYPE_K_MINUS_X_PLUS_W_MINUS_Y);
	fail_unless(formulas_equal("w + (k - x) - y", value->current));
	fail_unless(formulas_equal("0", value->dxdt_expression));
	fail_unless(formulas_equal("0", value->dydt_expression));
	fail_unless(value->v_expression == NULL);
	fail_unless(formulas_equal("w", value->w_expression));
	fail_unless(formulas_equal("newVar1 + w", value->z_expression));
	fail_unless(value->odeIndex == 0);
	fail_unless(util_isNaN(value->k_real_value));

	SubstitutionValues_t* value1 = analyser->getExpression(1);
	fail_unless(value1->k_value == "k");
	fail_unless(value1->x_value == "x");
	fail_unless(value1->y_value == "a");
	fail_unless(value1->z_value == "newVar2");
	fail_unless(value1->type == TYPE_K_MINUS_X_PLUS_W_MINUS_Y);
	fail_unless(formulas_equal("w + (k - x) - a", value1->current));
	fail_unless(formulas_equal("0", value1->dxdt_expression));
	fail_unless(formulas_equal("w + (k - x) - y", value1->dydt_expression));
	fail_unless(formulas_equal("w", value1->w_expression));
	fail_unless(formulas_equal("newVar2 + w", value1->z_expression));
	fail_unless(value1->odeIndex == 1);
	fail_unless(util_isNaN(value1->k_real_value));
	fail_unless(analyser->getNumHiddenNodes() == 2);

	delete analyser;
}
END_TEST

START_TEST(test_analyse_2_replace_2)
{
	setupSpecies(d->getModel(), "q", "c");
	RateRule* rr = d->getModel()->createRateRule();
	rr->setVariable("a");
	rr->setMath(SBML_parseFormula("k - x  + w - y"));
	RateRule* rr1 = d->getModel()->createRateRule();
	rr1->setVariable("b");
	rr1->setMath(SBML_parseFormula("k - x + w - a"));
	RateRule* rr2 = d->getModel()->createRateRule();
	rr2->setVariable("q");
	rr2->setMath(SBML_parseFormula("k - x - y"));
	converter->populateInitialODEinfo();
	ExpressionAnalyser* analyser = new ExpressionAnalyser(m, converter->getOdePairs());

	fail_unless(analyser->getNumExpressions() == 0);

	analyser->detectHiddenSpecies(true);

	fail_unless(analyser->getNumExpressions() == 3);
	SubstitutionValues_t* value = analyser->getExpression(1);
	fail_unless(value->k_value == "k");
	fail_unless(value->x_value == "x");
	fail_unless(value->y_value == "y");
	fail_unless(value->z_value == "newVar1");
	fail_unless(value->type == TYPE_K_MINUS_X_PLUS_W_MINUS_Y);
	fail_unless(formulas_equal("w + (k - x) - y", value->current));
	fail_unless(formulas_equal("0", value->dxdt_expression));
	fail_unless(formulas_equal("0", value->dydt_expression));
	fail_unless(value->v_expression == NULL);
	fail_unless(formulas_equal("w", value->w_expression));
	fail_unless(formulas_equal("newVar1 + w", value->z_expression));
	fail_unless(value->odeIndex == 0);
	fail_unless(util_isNaN(value->k_real_value));

	SubstitutionValues_t* value1 = analyser->getExpression(2);
	fail_unless(value1->k_value == "k");
	fail_unless(value1->x_value == "x");
	fail_unless(value1->y_value == "a");
	fail_unless(value1->z_value == "newVar2");
	fail_unless(value1->type == TYPE_K_MINUS_X_PLUS_W_MINUS_Y);
	fail_unless(formulas_equal("w + (k - x) - a", value1->current));
	fail_unless(formulas_equal("0", value1->dxdt_expression));
	fail_unless(formulas_equal("w + (k - x) - y", value1->dydt_expression));
	fail_unless(formulas_equal("w", value1->w_expression));
	fail_unless(formulas_equal("newVar2 + w", value1->z_expression));
	fail_unless(value1->odeIndex == 1);
	fail_unless(util_isNaN(value1->k_real_value));

	SubstitutionValues_t* value2 = analyser->getExpression(0);
	fail_unless(value2->k_value == "k");
	fail_unless(value2->x_value == "x");
	fail_unless(value2->y_value == "y");
	fail_unless(value2->z_value == "newVar1");
	fail_unless(value2->type == TYPE_K_MINUS_X_MINUS_Y);
	fail_unless(formulas_equal("k - x - y", value2->current));
	fail_unless(value2->w_expression == NULL);
	fail_unless(formulas_equal("newVar1", value2->z_expression));
	fail_unless(value2->odeIndex == 2);
	fail_unless(util_isNaN(value2->k_real_value));

	fail_unless(analyser->getNumHiddenNodes() == 2);

	delete analyser;
}
END_TEST



START_TEST(test_analyse_3_replace_1)
{
	RateRule* rr = d->getModel()->createRateRule();
	rr->setVariable("a");
	rr->setMath(SBML_parseFormula("k - x"));
	RateRule* rra = d->getModel()->createRateRule();
	rra->setVariable("b");
	rra->setMath(SBML_parseFormula("k - y"));
	converter->populateInitialODEinfo();
	ExpressionAnalyser* analyser = new ExpressionAnalyser(m, converter->getOdePairs());

	fail_unless(analyser->getNumExpressions() == 0);

	analyser->detectHiddenSpecies(true);

	fail_unless(analyser->getNumExpressions() == 2);
	SubstitutionValues_t* value = analyser->getExpression(0);
	fail_unless(value->k_value == "k");
	fail_unless(value->x_value == "x"); 
	fail_unless(value->y_value.empty());
	fail_unless(value->z_value == "newVar1");
	fail_unless(value->type == TYPE_K_MINUS_X);
	fail_unless(formulas_equal("k - x", value->current));
	fail_unless(formulas_equal("newVar1", value->z_expression));
	fail_unless(value->odeIndex == 0);
	fail_unless(util_isNaN(value->k_real_value));

    SubstitutionValues_t* value1 = analyser->getExpression(1);
    fail_unless(value1->k_value == "k");
    fail_unless(value1->y_value.empty());
    fail_unless(value1->x_value == "y");
    fail_unless(value1->z_value == "newVar2");
    fail_unless(value1->type == TYPE_K_MINUS_X);
    fail_unless(formulas_equal("k - y", value1->current));
    fail_unless(formulas_equal("newVar2", value1->z_expression));
    fail_unless(value1->odeIndex == 1);
    fail_unless(util_isNaN(value1->k_real_value));

	fail_unless(analyser->getNumHiddenNodes() == 2);

	delete analyser;
}
END_TEST

START_TEST(test_analyse_4_replace_1)
{
	RateRule* rr = d->getModel()->createRateRule();
	rr->setVariable("a");
	rr->setMath(SBML_parseFormula("k + v - x"));
	RateRule* rrb = d->getModel()->createRateRule();
	rrb->setVariable("b");
	rrb->setMath(SBML_parseFormula("k - x"));
	converter->populateInitialODEinfo();
	ExpressionAnalyser* analyser = new ExpressionAnalyser(m, converter->getOdePairs());

	fail_unless(analyser->getNumExpressions() == 0);

	analyser->detectHiddenSpecies(true);

	fail_unless(analyser->getNumExpressions() == 2);
	SubstitutionValues_t* value = analyser->getExpression(0);
	fail_unless(value->k_value == "k");
	fail_unless(value->x_value == "x");
	fail_unless(value->y_value.empty());
	fail_unless(value->z_value == "newVar1");
	fail_unless(value->type == TYPE_K_MINUS_X);
	fail_unless(formulas_equal("k - x", value->current));
	fail_unless(formulas_equal("newVar1", value->z_expression));
	fail_unless(value->odeIndex == 1);
	fail_unless(util_isNaN(value->k_real_value));

	SubstitutionValues_t* value1 = analyser->getExpression(1);
	fail_unless(value1->k_value == "k");
	fail_unless(value1->x_value == "x");
	fail_unless(value1->y_value.empty());
	fail_unless(value1->z_value == "newVar1");
	fail_unless(value1->type == TYPE_K_PLUS_V_MINUS_X);
	fail_unless(formulas_equal("k + v - x", value1->current));
	fail_unless(formulas_equal("newVar1 + v", value1->z_expression));
	fail_unless(value1->odeIndex == 0);
	fail_unless(util_isNaN(value1->k_real_value));

	fail_unless(analyser->getNumHiddenNodes() == 1);

	delete analyser;
}
END_TEST


START_TEST(test_analyse_4_replace_2)
{
	RateRule* rr = d->getModel()->createRateRule();
	rr->setVariable("a");
	rr->setMath(SBML_parseFormula("k + v - y"));
	RateRule* rrb = d->getModel()->createRateRule();
	rrb->setVariable("b");
	rrb->setMath(SBML_parseFormula("k - x"));
	converter->populateInitialODEinfo();
	ExpressionAnalyser* analyser = new ExpressionAnalyser(m, converter->getOdePairs());

	fail_unless(analyser->getNumExpressions() == 0);

	analyser->detectHiddenSpecies(true);

	fail_unless(analyser->getNumExpressions() == 2);
	SubstitutionValues_t* value = analyser->getExpression(0);
	fail_unless(value->k_value == "k");
	fail_unless(value->x_value == "x");
	fail_unless(value->y_value.empty());
	fail_unless(value->z_value == "newVar1");
	fail_unless(value->type == TYPE_K_MINUS_X);
	fail_unless(formulas_equal("k - x", value->current));
	fail_unless(formulas_equal("newVar1", value->z_expression));
	fail_unless(value->odeIndex == 1);
	fail_unless(util_isNaN(value->k_real_value));

	SubstitutionValues_t* value1 = analyser->getExpression(1);
	fail_unless(value1->k_value == "k");
	fail_unless(value1->x_value == "y");
	fail_unless(value1->y_value.empty());
	fail_unless(value1->z_value == "newVar2");
	fail_unless(value1->type == TYPE_K_PLUS_V_MINUS_X);
	fail_unless(formulas_equal("k + v - y", value1->current));
	fail_unless(formulas_equal("newVar2 + v", value1->z_expression));
	fail_unless(value1->odeIndex == 0);
	fail_unless(util_isNaN(value1->k_real_value));

	fail_unless(analyser->getNumHiddenNodes() == 2);

	delete analyser;
}
END_TEST

START_TEST(test_analyse_4_replace_3)
{
	RateRule* rr = d->getModel()->createRateRule();
	rr->setVariable("a");
	rr->setMath(SBML_parseFormula("k + v - x"));
	RateRule* rrb = d->getModel()->createRateRule();
	rrb->setVariable("b");
	rrb->setMath(SBML_parseFormula("k + v - y"));
	converter->populateInitialODEinfo();
	ExpressionAnalyser* analyser = new ExpressionAnalyser(m, converter->getOdePairs());

	fail_unless(analyser->getNumExpressions() == 0);

	analyser->detectHiddenSpecies(true);

	fail_unless(analyser->getNumExpressions() == 2);
	SubstitutionValues_t* value = analyser->getExpression(0);
	fail_unless(value->k_value == "k");
	fail_unless(value->x_value == "x");
	fail_unless(value->y_value.empty());
	fail_unless(value->z_value == "newVar1");
	fail_unless(value->type == TYPE_K_PLUS_V_MINUS_X);
	fail_unless(formulas_equal("k + v - x", value->current));
	fail_unless(formulas_equal("newVar1 + v", value->z_expression));
	fail_unless(value->odeIndex == 0);
	fail_unless(util_isNaN(value->k_real_value));

	SubstitutionValues_t* value1 = analyser->getExpression(1);
	fail_unless(value1->k_value == "k");
	fail_unless(value1->x_value == "y");
	fail_unless(value1->y_value.empty());
	fail_unless(value1->z_value == "newVar2");
	fail_unless(value1->type == TYPE_K_PLUS_V_MINUS_X);
	fail_unless(formulas_equal("k + v - y", value1->current));
	fail_unless(formulas_equal("newVar2 + v", value1->z_expression));
	fail_unless(value1->odeIndex == 1);
	fail_unless(util_isNaN(value1->k_real_value));

	fail_unless(analyser->getNumHiddenNodes() == 2);

	delete analyser;
}
END_TEST


START_TEST(test_analyse_4_replace_4)
{
	RateRule* rr = d->getModel()->createRateRule();
	rr->setVariable("a");
	rr->setMath(SBML_parseFormula("k - x - y"));
	RateRule* rrb = d->getModel()->createRateRule();
	rrb->setVariable("b");
	rrb->setMath(SBML_parseFormula("k + v - x"));
	converter->populateInitialODEinfo();
	ExpressionAnalyser* analyser = new ExpressionAnalyser(m, converter->getOdePairs());

	fail_unless(analyser->getNumExpressions() == 0);

	analyser->detectHiddenSpecies(true);

	fail_unless(analyser->getNumExpressions() == 2);
	SubstitutionValues_t* value = analyser->getExpression(0);
	fail_unless(value->k_value == "k");
	fail_unless(value->x_value == "x");
	fail_unless(value->y_value == "y");
	fail_unless(value->z_value == "newVar1");
	fail_unless(value->type == TYPE_K_MINUS_X_MINUS_Y);
	fail_unless(formulas_equal("k - x - y", value->current));
	fail_unless(formulas_equal("newVar1", value->z_expression));
	fail_unless(value->odeIndex == 0);
	fail_unless(util_isNaN(value->k_real_value));

	SubstitutionValues_t* value1 = analyser->getExpression(1);
	fail_unless(value1->k_value == "k");
	fail_unless(value1->x_value == "x");
	fail_unless(value1->y_value.empty());
	fail_unless(value1->z_value == "newVar2");
	fail_unless(value1->type == TYPE_K_PLUS_V_MINUS_X);
	fail_unless(formulas_equal("k + v - x", value1->current));
	fail_unless(formulas_equal("newVar2 + v", value1->z_expression));
	fail_unless(value1->odeIndex == 1);
	fail_unless(util_isNaN(value1->k_real_value));

	fail_unless(analyser->getNumHiddenNodes() == 2);

	delete analyser;
}
END_TEST

START_TEST(test_analyse_4_replace_5)
{
	setupSpecies(d->getModel(), "q", "c");
	RateRule* rr = d->getModel()->createRateRule();
	rr->setVariable("a");
	rr->setMath(SBML_parseFormula("k - x"));
	RateRule* rrb = d->getModel()->createRateRule();
	rrb->setVariable("b");
	rrb->setMath(SBML_parseFormula("k + v - x"));
	RateRule* rr2 = d->getModel()->createRateRule();
	rr2->setVariable("q");
	rr2->setMath(SBML_parseFormula("k + v - y"));
	converter->populateInitialODEinfo();
	ExpressionAnalyser* analyser = new ExpressionAnalyser(m, converter->getOdePairs());

	fail_unless(analyser->getNumExpressions() == 0);

	analyser->detectHiddenSpecies(true);

	fail_unless(analyser->getNumExpressions() == 3);
	SubstitutionValues_t* value = analyser->getExpression(1);
	fail_unless(value->k_value == "k");
	fail_unless(value->x_value == "x");
	fail_unless(value->y_value.empty());
	fail_unless(value->z_value == "newVar1");
	fail_unless(value->type == TYPE_K_PLUS_V_MINUS_X);
	fail_unless(formulas_equal("k + v - x", value->current));
	fail_unless(formulas_equal("newVar1 + v", value->z_expression));
	fail_unless(value->odeIndex == 1);
	fail_unless(util_isNaN(value->k_real_value));

	SubstitutionValues_t* value1 = analyser->getExpression(0);
	fail_unless(value1->k_value == "k");
	fail_unless(value1->x_value == "x");
	fail_unless(value1->y_value.empty());
	fail_unless(value1->z_value == "newVar1");
	fail_unless(value1->type == TYPE_K_MINUS_X);
	fail_unless(formulas_equal("k - x", value1->current));
	fail_unless(formulas_equal("newVar1", value1->z_expression));
	fail_unless(value1->odeIndex == 0);
	fail_unless(util_isNaN(value1->k_real_value));
	
	SubstitutionValues_t* value2 = analyser->getExpression(2);
	fail_unless(value2->k_value == "k");
	fail_unless(value2->x_value == "y");
	fail_unless(value1->y_value.empty());
	fail_unless(value2->z_value == "newVar2");
	fail_unless(value2->type == TYPE_K_PLUS_V_MINUS_X);
	fail_unless(formulas_equal("k + v - y", value2->current));
	fail_unless(value2->w_expression == NULL);
	fail_unless(formulas_equal("newVar2 + v", value2->z_expression));
	fail_unless(value2->odeIndex == 2);
	fail_unless(util_isNaN(value2->k_real_value));

	fail_unless(analyser->getNumHiddenNodes() == 2);

	delete analyser;
}
END_TEST

START_TEST(test_analyse_4_replace_6)
{
	setupSpecies(d->getModel(), "q", "c");
	RateRule* rr = d->getModel()->createRateRule();
	rr->setVariable("a");
	rr->setMath(SBML_parseFormula("k - x"));
	RateRule* rrb = d->getModel()->createRateRule();
	rrb->setVariable("b");
	rrb->setMath(SBML_parseFormula("k - y"));
	RateRule* rr2 = d->getModel()->createRateRule();
	rr2->setVariable("q");
	rr2->setMath(SBML_parseFormula("k + v - y"));
	converter->populateInitialODEinfo();
	ExpressionAnalyser* analyser = new ExpressionAnalyser(m, converter->getOdePairs());

	fail_unless(analyser->getNumExpressions() == 0);

	analyser->detectHiddenSpecies(true);

	fail_unless(analyser->getNumExpressions() == 3);
	SubstitutionValues_t* value = analyser->getExpression(1);
	fail_unless(value->k_value == "k");
	fail_unless(value->x_value == "y");
	fail_unless(value->y_value.empty());
	fail_unless(value->z_value == "newVar2");
	fail_unless(value->type == TYPE_K_MINUS_X);
	fail_unless(formulas_equal("k - y", value->current));
	fail_unless(formulas_equal("newVar2", value->z_expression));
	fail_unless(value->odeIndex == 1);
	fail_unless(util_isNaN(value->k_real_value));

	SubstitutionValues_t* value1 = analyser->getExpression(0);
	fail_unless(value1->k_value == "k");
	fail_unless(value1->x_value == "x");
	fail_unless(value1->y_value.empty());
	fail_unless(value1->z_value == "newVar1");
	fail_unless(value1->type == TYPE_K_MINUS_X);
	fail_unless(formulas_equal("k - x", value1->current));
	fail_unless(formulas_equal("newVar1", value1->z_expression));
	fail_unless(value1->odeIndex == 0);
	fail_unless(util_isNaN(value1->k_real_value));

	SubstitutionValues_t* value2 = analyser->getExpression(2);
	fail_unless(value2->k_value == "k");
	fail_unless(value2->x_value == "y");
	fail_unless(value1->y_value.empty());
	fail_unless(value2->z_value == "newVar2");
	fail_unless(value2->type == TYPE_K_PLUS_V_MINUS_X);
	fail_unless(formulas_equal("k + v - y", value2->current));
	fail_unless(value2->w_expression == NULL);
	fail_unless(formulas_equal("newVar2 + v", value2->z_expression));
	fail_unless(value2->odeIndex == 2);
	fail_unless(util_isNaN(value2->k_real_value));

	fail_unless(analyser->getNumHiddenNodes() == 2);

	delete analyser;
}
END_TEST


START_TEST(test_analyse_4_replace_7)
{
	setupSpecies(d->getModel(), "q", "c");
	RateRule* rr = d->getModel()->createRateRule();
	rr->setVariable("a");
	rr->setMath(SBML_parseFormula("k - y"));
	RateRule* rrb = d->getModel()->createRateRule();
	rrb->setVariable("b");
	rrb->setMath(SBML_parseFormula("k - x"));
	RateRule* rr2 = d->getModel()->createRateRule();
	rr2->setVariable("q");
	rr2->setMath(SBML_parseFormula("k + v - y"));
	converter->populateInitialODEinfo();
	ExpressionAnalyser* analyser = new ExpressionAnalyser(m, converter->getOdePairs());

	fail_unless(analyser->getNumExpressions() == 0);

	analyser->detectHiddenSpecies(true);

	fail_unless(analyser->getNumExpressions() == 3);
	SubstitutionValues_t* value = analyser->getExpression(0);
	fail_unless(value->k_value == "k");
	fail_unless(value->x_value == "y");
	fail_unless(value->y_value.empty());
	fail_unless(value->z_value == "newVar1");
	fail_unless(value->type == TYPE_K_MINUS_X);
	fail_unless(formulas_equal("k - y", value->current));
	fail_unless(formulas_equal("newVar1", value->z_expression));
	fail_unless(value->odeIndex == 0);
	fail_unless(util_isNaN(value->k_real_value));

	SubstitutionValues_t* value1 = analyser->getExpression(1);
	fail_unless(value1->k_value == "k");
	fail_unless(value1->x_value == "x");
	fail_unless(value1->y_value.empty());
	fail_unless(value1->z_value == "newVar2");
	fail_unless(value1->type == TYPE_K_MINUS_X);
	fail_unless(formulas_equal("k - x", value1->current));
	fail_unless(formulas_equal("newVar2", value1->z_expression));
	fail_unless(value1->odeIndex == 1);
	fail_unless(util_isNaN(value1->k_real_value));

	SubstitutionValues_t* value2 = analyser->getExpression(2);
	fail_unless(value2->k_value == "k");
	fail_unless(value2->x_value == "y");
	fail_unless(value1->y_value.empty());
	fail_unless(value2->z_value == "newVar1");
	fail_unless(value2->type == TYPE_K_PLUS_V_MINUS_X);
	fail_unless(formulas_equal("k + v - y", value2->current));
	fail_unless(value2->w_expression == NULL);
	fail_unless(formulas_equal("newVar1 + v", value2->z_expression));
	fail_unless(value2->odeIndex == 2);
	fail_unless(util_isNaN(value2->k_real_value));

	fail_unless(analyser->getNumHiddenNodes() == 2);

	delete analyser;
}
END_TEST



Suite *
create_suite_TestExpressionAnalyser (void)
{ 
	bool testing = false;
	Suite *suite = suite_create("ExpressionAnalyser");
	TCase *tcase = tcase_create("ExpressionAnalyser");
	tcase_add_checked_fixture(tcase, ExpressionAnalyser_setup, 
								ExpressionAnalyser_teardown);

  if (testing)
  {
	  tcase_add_test(tcase, test_order_expressions_2); //k-x-y & k+v-x
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
	  tcase_add_test(tcase, test_order_expressions_1);
	  tcase_add_test(tcase, test_order_expressions_2);
	  tcase_add_test(tcase, test_order_expressions_3);
	  tcase_add_test(tcase, test_order_expressions_4);
	  tcase_add_test(tcase, test_analyse_replace); //k-x-y
	  tcase_add_test(tcase, test_analyse_same_expression_replace); //k-x-y
	  tcase_add_test(tcase, test_analyse_different_expression_replace); //k-x-y
	  tcase_add_test(tcase, test_analyse_1_replace); //k+v-x-y
	  tcase_add_test(tcase, test_analyse_2_replace); //k-x+w-y
	  tcase_add_test(tcase, test_analyse_3_replace); //k-x
	  tcase_add_test(tcase, test_analyse_4_replace); //k+v-x
	  tcase_add_test(tcase, test_analyse_1_different_replace); //k+v-x-y
	  tcase_add_test(tcase, test_analyse_1_different_replace_1); //k-x-y & k+v-x-y
	  tcase_add_test(tcase, test_analyse_1_different_replace_2); //k-x-y & k+v-x-a
	  tcase_add_test(tcase, test_analyse_1_different_replace_3); //k-x-y & k-x+w-y
	  tcase_add_test(tcase, test_analyse_1_different_replace_4); //k-x-y & k-x+w-a
	  tcase_add_test(tcase, test_analyse_1_different_replace_5); //k-x-y & k-x-a & k+v-x-y
	  tcase_add_test(tcase, test_analyse_1_different_replace_6); //k-x-y & k+v-x-a & k+v-x-y
	  tcase_add_test(tcase, test_analyse_1_two_terms_replace); //(k+v-x-y)+(k-x)
	  tcase_add_test(tcase, test_analyse_2_replace_1); //k-x+w-y & k-x+w-a
	  tcase_add_test(tcase, test_analyse_2_replace_2); //k-x-y & k-x+w-y & k-x+w-a
	  tcase_add_test(tcase, test_analyse_3_replace_1); //k-x & k-y
	  tcase_add_test(tcase, test_analyse_4_replace_1); //k+v-x & k-x
	  tcase_add_test(tcase, test_analyse_4_replace_2); //k+v-y & k-x
	  tcase_add_test(tcase, test_analyse_4_replace_3); //k+v-x & k+v-y
	  tcase_add_test(tcase, test_analyse_4_replace_4); //k-x-y & k+v-x
	  tcase_add_test(tcase, test_analyse_4_replace_5); //k-x & k+v-x & k+v-y
	  tcase_add_test(tcase, test_analyse_4_replace_6); //k-x & k-y & k+v-x
	  tcase_add_test(tcase, test_analyse_4_replace_7); //k-y & k-x & k+v-x
  }
  suite_add_tcase(suite, tcase);

  return suite;

}
END_C_DECLS

				 