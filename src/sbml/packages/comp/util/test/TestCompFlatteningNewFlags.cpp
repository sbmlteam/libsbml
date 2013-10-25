/**
 * \file    TestCompFlatteningNewFlags.cpp
 * \brief   Implementation of the Tests for the Comp flattening converter
 * \author  Sarah Keating
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

#include <sbml/packages/comp/common/CompExtensionTypes.h>

#include <sbml/conversion/SBMLConverterRegistry.h>
#include <sbml/SBMLReader.h>
#include <sbml/SBMLTypes.h>

#include <string>

#include <check.h>

using namespace std;

LIBSBML_CPP_NAMESPACE_USE

BEGIN_C_DECLS


extern char *TestDataDirectory;

START_TEST(test_comp_flatten_abort_reqd_only_1)
{
  ConversionProperties* props = new ConversionProperties();
  
  props->addOption("flatten comp");

  SBMLConverter* converter = 
    SBMLConverterRegistry::getInstance().getConverterFor(*props);
  
  // load document
  string dir(TestDataDirectory);
  string fileName = dir + "unknown2.xml";  
  SBMLDocument* doc = readSBMLFromFile(fileName.c_str());

  // fail if there is no model 
  //(but we have the unrecognised package error)
  fail_unless(doc->getNumErrors() == 1);
  fail_unless(doc->getModel() != NULL);
  fail_unless(doc->getErrorLog()->contains(RequiredPackagePresent) == true);

  converter->setDocument(doc);
  int result = converter->convert();

  fail_unless( result == LIBSBML_OPERATION_FAILED);

  fail_unless(doc->getNumErrors() == 2);
  fail_unless(doc->getErrorLog()->contains(RequiredPackagePresent) == true);
  fail_unless(doc->getErrorLog()->contains(CompFlatteningNotRecognisedReqd) == true);

  delete doc;
  delete converter;
}
END_TEST


START_TEST(test_comp_flatten_abort_reqd_only_2)
{
  ConversionProperties* props = new ConversionProperties();
  
  props->addOption("flatten comp");
  
  // checking the default
  props->addOption("abortIfUnflattenable", "requiredOnly");

  SBMLConverter* converter = 
    SBMLConverterRegistry::getInstance().getConverterFor(*props);
  
  // load document
  string dir(TestDataDirectory);
  string fileName = dir + "unknown2.xml";  
  SBMLDocument* doc = readSBMLFromFile(fileName.c_str());

  // fail if there is no model 
  //(but we have the unrecognised package error)
  fail_unless(doc->getNumErrors() == 1);
  fail_unless(doc->getModel() != NULL);
  fail_unless(doc->getErrorLog()->contains(RequiredPackagePresent) == true);

  converter->setDocument(doc);
  int result = converter->convert();

  fail_unless( result == LIBSBML_OPERATION_FAILED);

  fail_unless(doc->getNumErrors() == 2);
  fail_unless(doc->getErrorLog()->contains(RequiredPackagePresent) == true);
  fail_unless(doc->getErrorLog()->contains(CompFlatteningNotRecognisedReqd) == true);

  delete doc;
  delete converter;
}
END_TEST


START_TEST(test_comp_flatten_abort_reqd_only_3)
{
  ConversionProperties* props = new ConversionProperties();
  
  props->addOption("flatten comp");
  
  // checking the default
  props->addOption("stripUnflattenablePackages", true);

  SBMLConverter* converter = 
    SBMLConverterRegistry::getInstance().getConverterFor(*props);
  
  // load document
  string dir(TestDataDirectory);
  string fileName = dir + "unknown2.xml";  
  SBMLDocument* doc = readSBMLFromFile(fileName.c_str());

  // fail if there is no model 
  //(but we have the unrecognised package error)
  fail_unless(doc->getNumErrors() == 1);
  fail_unless(doc->getModel() != NULL);
  fail_unless(doc->getErrorLog()->contains(RequiredPackagePresent) == true);

  converter->setDocument(doc);
  int result = converter->convert();

  fail_unless( result == LIBSBML_OPERATION_FAILED);

  fail_unless(doc->getNumErrors() == 2);
  fail_unless(doc->getErrorLog()->contains(RequiredPackagePresent) == true);
  fail_unless(doc->getErrorLog()->contains(CompFlatteningNotRecognisedReqd) == true);

  delete doc;
  delete converter;
}
END_TEST


START_TEST(test_comp_flatten_abort_reqd_only_4)
{
  ConversionProperties* props = new ConversionProperties();
  
  props->addOption("flatten comp");
  
  props->addOption("stripUnflattenablePackages", false);

  SBMLConverter* converter = 
    SBMLConverterRegistry::getInstance().getConverterFor(*props);
  
  // load document
  string dir(TestDataDirectory);
  string fileName = dir + "unknown2.xml";  
  SBMLDocument* doc = readSBMLFromFile(fileName.c_str());

  // fail if there is no model 
  //(but we have the unrecognised package error)
  fail_unless(doc->getNumErrors() == 1);
  fail_unless(doc->getModel() != NULL);
  fail_unless(doc->getErrorLog()->contains(RequiredPackagePresent) == true);

  converter->setDocument(doc);
  int result = converter->convert();

  fail_unless( result == LIBSBML_OPERATION_FAILED);

  fail_unless(doc->getNumErrors() == 2);
  fail_unless(doc->getErrorLog()->contains(RequiredPackagePresent) == true);
  fail_unless(doc->getErrorLog()->contains(CompFlatteningNotRecognisedReqd) == true);

  delete doc;
  delete converter;
}
END_TEST


START_TEST(test_comp_flatten_abort_reqd_only_5)
{
  ConversionProperties* props = new ConversionProperties();
  
  props->addOption("flatten comp");

  SBMLConverter* converter = 
    SBMLConverterRegistry::getInstance().getConverterFor(*props);
  
  // load document
  string dir(TestDataDirectory);
  string fileName = dir + "unknown1.xml";  
  SBMLDocument* doc = readSBMLFromFile(fileName.c_str());

  // fail if there is no model 
  //(but we have the unrecognised package error)
  fail_unless(doc->getNumErrors() == 1);
  fail_unless(doc->getModel() != NULL);
  fail_unless(doc->getErrorLog()->contains(UnrequiredPackagePresent) == true);

  converter->setDocument(doc);
  int result = converter->convert();

  fail_unless( result == LIBSBML_OPERATION_SUCCESS);

  string newModel = writeSBMLToString(doc);

  string ffile = dir + "unknown1_flat.xml";
  SBMLDocument* fdoc = readSBMLFromFile(ffile.c_str());
  string flatModel = writeSBMLToString(fdoc);
  fail_unless(flatModel == newModel);

  delete doc;
  delete fdoc;
  delete converter;
}
END_TEST


START_TEST(test_comp_flatten_abort_reqd_only_6)
{
  ConversionProperties* props = new ConversionProperties();
  
  props->addOption("flatten comp");
  
  // checking the default
  props->addOption("abortIfUnflattenable", "all");

  SBMLConverter* converter = 
    SBMLConverterRegistry::getInstance().getConverterFor(*props);
  
  // load document
  string dir(TestDataDirectory);
  string fileName = dir + "unknown2.xml";  
  SBMLDocument* doc = readSBMLFromFile(fileName.c_str());

  // fail if there is no model 
  //(but we have the unrecognised package error)
  fail_unless(doc->getNumErrors() == 1);
  fail_unless(doc->getModel() != NULL);
  fail_unless(doc->getErrorLog()->contains(RequiredPackagePresent) == true);

  converter->setDocument(doc);
  int result = converter->convert();

  fail_unless( result == LIBSBML_OPERATION_FAILED);

  fail_unless(doc->getNumErrors() == 2);
  fail_unless(doc->getErrorLog()->contains(RequiredPackagePresent) == true);
  fail_unless(doc->getErrorLog()->contains(CompFlatteningNotRecognisedReqd) == true);

  delete doc;
  delete converter;
}
END_TEST


START_TEST(test_comp_flatten_abort_reqd_only_7)
{
  ConversionProperties* props = new ConversionProperties();
  
  props->addOption("flatten comp");
  
  // checking the default
  props->addOption("abortIfUnflattenable", "all");

  SBMLConverter* converter = 
    SBMLConverterRegistry::getInstance().getConverterFor(*props);
  
  // load document
  string dir(TestDataDirectory);
  string fileName = dir + "unknown1.xml";  
  SBMLDocument* doc = readSBMLFromFile(fileName.c_str());

  // fail if there is no model 
  //(but we have the unrecognised package error)
  fail_unless(doc->getNumErrors() == 1);
  fail_unless(doc->getModel() != NULL);
  fail_unless(doc->getErrorLog()->contains(UnrequiredPackagePresent) == true);

  converter->setDocument(doc);
  int result = converter->convert();

  fail_unless( result == LIBSBML_OPERATION_FAILED);

  fail_unless(doc->getNumErrors() == 2);
  fail_unless(doc->getErrorLog()->contains(UnrequiredPackagePresent) == true);
  fail_unless(doc->getErrorLog()->contains(CompFlatteningNotRecognisedNotReqd) == true);

  delete doc;
  delete converter;
}
END_TEST


START_TEST(test_comp_flatten_abort_reqd_only_8)
{
  ConversionProperties* props = new ConversionProperties();
  
  props->addOption("flatten comp");
  
  // checking for none
  props->addOption("abortIfUnflattenable", "none");

  SBMLConverter* converter = 
    SBMLConverterRegistry::getInstance().getConverterFor(*props);
  
  // load document
  string dir(TestDataDirectory);
  string fileName = dir + "unknown2.xml";  
  SBMLDocument* doc = readSBMLFromFile(fileName.c_str());

  // fail if there is no model 
  //(but we have the unrecognised package error)
  fail_unless(doc->getNumErrors() == 1);
  fail_unless(doc->getModel() != NULL);
  fail_unless(doc->getErrorLog()->contains(RequiredPackagePresent) == true);

  converter->setDocument(doc);
  int result = converter->convert();

  fail_unless( result == LIBSBML_OPERATION_SUCCESS);

  string newModel = writeSBMLToString(doc);

  string ffile = dir + "unknown2_flat.xml";
  SBMLDocument* fdoc = readSBMLFromFile(ffile.c_str());
  string flatModel = writeSBMLToString(fdoc);
  fail_unless(flatModel == newModel);

  delete doc;
  delete fdoc;
  delete converter;
}
END_TEST


START_TEST(test_comp_flatten_abort_reqd_only_9)
{
  ConversionProperties* props = new ConversionProperties();
  
  props->addOption("flatten comp");
  
  // checking for none
  props->addOption("abortIfUnflattenable", "none");

  SBMLConverter* converter = 
    SBMLConverterRegistry::getInstance().getConverterFor(*props);
  
  // load document
  string dir(TestDataDirectory);
  string fileName = dir + "unknown1.xml";  
  SBMLDocument* doc = readSBMLFromFile(fileName.c_str());

  // fail if there is no model 
  //(but we have the unrecognised package error)
  fail_unless(doc->getNumErrors() == 1);
  fail_unless(doc->getModel() != NULL);
  fail_unless(doc->getErrorLog()->contains(UnrequiredPackagePresent) == true);

  converter->setDocument(doc);
  int result = converter->convert();

  fail_unless( result == LIBSBML_OPERATION_SUCCESS);

  string newModel = writeSBMLToString(doc);

  string ffile = dir + "unknown1_flat.xml";
  SBMLDocument* fdoc = readSBMLFromFile(ffile.c_str());
  string flatModel = writeSBMLToString(fdoc);
  fail_unless(flatModel == newModel);

  delete doc;
  delete fdoc;
  delete converter;
}
END_TEST




Suite *
create_suite_TestCompFlatteningNewFlags (void)
{ 
  TCase *tcase = tcase_create("SBMLCompFlatteningNewFlags");
  Suite *suite = suite_create("SBMLCompFlatteningNewFlags");
  
  tcase_add_test(tcase, test_comp_flatten_abort_reqd_only_1);
  tcase_add_test(tcase, test_comp_flatten_abort_reqd_only_2);
  tcase_add_test(tcase, test_comp_flatten_abort_reqd_only_3);
  tcase_add_test(tcase, test_comp_flatten_abort_reqd_only_4);
  tcase_add_test(tcase, test_comp_flatten_abort_reqd_only_5);
  tcase_add_test(tcase, test_comp_flatten_abort_reqd_only_6);
  tcase_add_test(tcase, test_comp_flatten_abort_reqd_only_7);
  tcase_add_test(tcase, test_comp_flatten_abort_reqd_only_8);
  tcase_add_test(tcase, test_comp_flatten_abort_reqd_only_9);

  suite_add_tcase(suite, tcase);

  return suite;
}


END_C_DECLS

