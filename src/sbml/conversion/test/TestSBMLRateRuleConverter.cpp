/**
 * @file    TestSBMLRateRuleConverter.cpp
 * @brief   Tests for raterule to reaction converter
 * @author  Sarah Keating
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

#include <check.h>

LIBSBML_CPP_NAMESPACE_USE

BEGIN_C_DECLS


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

  converter->setDocument(doc);
  fail_unless(converter->convert() == LIBSBML_OPERATION_SUCCESS);

  fail_unless(doc->getModel()->getNumCompartments() == 1);
  fail_unless(doc->getModel()->getNumSpecies() == 2);
  fail_unless(doc->getModel()->getNumParameters() == 1);
  fail_unless(doc->getModel()->getNumRules() == 0);
  fail_unless(doc->getModel()->getNumReactions() == 1);

  Reaction *r = doc->getModel()->getReaction(0);
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


Suite *
create_suite_TestSBMLRateRuleConverter (void)
{ 
  Suite *suite = suite_create("SBMLRateRuleConverter");
  TCase *tcase = tcase_create("SBMLRateRuleConverter");

  tcase_add_test(tcase, test_conversion_raterule_converter_invalid);
  tcase_add_test(tcase, test_conversion_raterule_converter);


  suite_add_tcase(suite, tcase);

  return suite;
}


END_C_DECLS

