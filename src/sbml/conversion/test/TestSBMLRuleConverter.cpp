/**
 * @file    TestSBMLRuleConverter.cpp
 * @brief   Tests for assignment rule sorter
 * @author  Frank Bergmann
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2009-2011 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EBML-EBI), Hinxton, UK
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
#include <sbml/conversion/SBMLConverter.h>
#include <sbml/conversion/SBMLConverterRegistry.h>
#include <sbml/conversion/SBMLRuleConverter.h>


#include <string>

#include <check.h>

LIBSBML_CPP_NAMESPACE_USE

BEGIN_C_DECLS


extern char *TestDataDirectory;


START_TEST (test_conversion_rulecovnerter_create)
{

  // create test model

  SBMLDocument doc; 
  
  Model* model = doc.createModel();
  model->setId("m");

  Parameter* parameter1 = model->createParameter();
  parameter1->setId("s");
  parameter1->setConstant(false);
  parameter1->setValue(0);

  Parameter* parameter = model->createParameter();
  parameter->setId("p");
  parameter->setConstant(false);
  parameter->setValue(0);

  AssignmentRule* rule1 = model->createAssignmentRule();
  rule1->setVariable("s");
  rule1->setFormula("p + 1");
  rule1->setMetaId("m1");

  AssignmentRule* rule2 = model->createAssignmentRule();
  rule2->setVariable("p");
  rule2->setFormula("1");
  rule2->setMetaId("m2");

  doc.checkConsistency();
  doc.printErrors();

  std::string model1 = doc.toSBML();

  ConversionProperties *props = new ConversionProperties();
  props->addOption("sortRules", true, "sort rules");
  
  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(*props);
  
  if (converter == NULL)
  {
    converter = new SBMLRuleConverter();
    converter->setProperties(props);
    converter->setDocument(&doc);
    converter->convert();
  }
  else
  {
    // why oh why
    doc.convert(*props);
  }
  std::string model2 = doc.toSBML();
  
  delete props;
}
END_TEST


//START_TEST (test_conversion_properties_read)
//{
//}
//END_TEST


Suite *
create_suite_TestSBMLRuleConverter (void)
{ 
  Suite *suite = suite_create("SBMLRuleConverter");
  TCase *tcase = tcase_create("SBMLRuleConverter");

  tcase_add_test(tcase, test_conversion_rulecovnerter_create);

  suite_add_tcase(suite, tcase);

  return suite;
}


END_C_DECLS

