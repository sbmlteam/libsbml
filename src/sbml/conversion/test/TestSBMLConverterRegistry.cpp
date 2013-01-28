/**
 * @file    TestSBMLConverterRegistry.cpp
 * @brief   Tests for creating converter registry
 * @author  Frank Bergmann
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2009-2013 jointly by the following organizations: 
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



#include <sbml/conversion/SBMLConverterRegistry.h>
#include <sbml/conversion/SBMLConverterRegister.h>
#include <sbml/conversion/SBMLFunctionDefinitionConverter.h>
#include <sbml/SBMLReader.h>
#include <sbml/SBMLTypes.h>

#include <string>

#include <check.h>

LIBSBML_CPP_NAMESPACE_USE

BEGIN_C_DECLS


extern char *TestDataDirectory;

START_TEST (test_conversion_registry_get)
{
  ConversionProperties* props = new ConversionProperties();
  props->addOption("expandInitialAssignments");
  
  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(*props);
  fail_unless(converter!= NULL);
  delete converter;

  // ensure that nothing bad happened when deleting the converter
  converter = SBMLConverterRegistry::getInstance().getConverterFor(*props);
  fail_unless(converter!= NULL);
  delete converter;

  // ensure that nothing bad happened when deleting the converter
  converter = SBMLConverterRegistry::getInstance().getConverterFor(*props);
  fail_unless(converter!= NULL);
  delete converter;


}
END_TEST

START_TEST (test_conversion_registry_getByIndex)
{

  int numConverters = SBMLConverterRegistry::getInstance().getNumConverters();

  for (int i = 0; i < numConverters; i++)
  {
    SBMLConverter *converter = SBMLConverterRegistry::getInstance().getConverterByIndex(i);

    fail_unless(converter != NULL);
    fail_unless(converter->getDefaultProperties().hasOption("none") == false);
    
    ConversionProperties props = converter->getDefaultProperties();

    
    delete converter;
  }


}
END_TEST

#include <string>
#include <iostream>
using namespace std;

START_TEST (test_conversion_units)
{
  string filename(TestDataDirectory);
  filename += "wrongl3v1-invalid.xml";

  SBMLDocument* doc = readSBMLFromFile(filename.c_str());

  fail_unless(doc != NULL);
  doc->setLevelAndVersion(2, 4, false);

  string sbml = writeSBMLToString(doc);
  
  fail_unless(sbml.find("sbml:units") == string::npos);

}
END_TEST

Suite *
create_suite_TestSBMLConverterRegistry (void)
{ 
  TCase *tcase = tcase_create("SBMLConverterRegistry");
  Suite *suite = suite_create("SBMLConverterRegistry");

  
  tcase_add_test(tcase, test_conversion_registry_get);
  tcase_add_test(tcase, test_conversion_registry_getByIndex);

  tcase_add_test(tcase, test_conversion_units);

  suite_add_tcase(suite, tcase);

  return suite;
}


END_C_DECLS

