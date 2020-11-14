/**
 * \file    TestCompFlatteningConverterStripPackage.cpp
 * \brief   Implementation of the Tests for the Comp flattening converter
 * \author  Frank T. Bergmann
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
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
 * Copyright 2011-2012 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
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

void TestPair(const std::string& file1, const std::string& file2, const std::string& pkgToStrip,
              unsigned int numErrors = 0)
{
  std::string filename(TestDataDirectory);
  std::string dir("test_external_flat_strip/");

  ConversionProperties props;
  
  props.addOption("flatten comp");
  props.addOption("basePath", filename);
  props.addOption("performValidation", true); 
  props.addOption("stripPackages", pkgToStrip);
  props.addOption("abortIfUnflattenable", "none");

  SBMLConverter* converter = SBMLConverterRegistry::getInstance().
                                                    getConverterFor(props);
  
  // load document
  std::string cfile = filename + dir + file1;  
  SBMLDocument* doc = readSBMLFromFile(cfile.c_str());

  // fail if there is no model (readSBMLFromFile always returns a valid doc)
  fail_unless(doc->getModel() != NULL);

  fail_unless(doc->getErrorLog()->getNumFailsWithSeverity(LIBSBML_SEV_ERROR) 
                                                          == numErrors);

  converter->setDocument(doc);
  int result = converter->convert();

  // fail if conversion was not valid
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);

  std::string newModel = writeSBMLToStdString(doc);
  
  std::string ffile = filename + dir + file2;
  SBMLDocument* fdoc = readSBMLFromFile(ffile.c_str());
  std::string flatModel = writeSBMLToStdString(fdoc);

  fail_unless(flatModel == newModel);

  delete doc;
  delete fdoc;
  delete converter;
}


void TestPairDiffDir(std::string file1, std::string file2, 
                     std::string pkgToStrip,
                     unsigned int numErrors = 0)
{
  std::string filename(TestDataDirectory);

  ConversionProperties props;
  
  props.addOption("flatten comp");
  props.addOption("basePath", filename);
  props.addOption("performValidation", true);
  props.addOption("stripPackages", pkgToStrip);
  props.addOption("abortIfUnflattenable", "none");

  SBMLConverter* converter = SBMLConverterRegistry::getInstance().
                                                    getConverterFor(props);
  
  // load document
  std::string cfile = filename + file1;  
  SBMLDocument* doc = readSBMLFromFile(cfile.c_str());

  // fail if there is no model (readSBMLFromFile always returns a valid doc)
  fail_unless(doc->getModel() != NULL);

  fail_unless(doc->getErrorLog()->getNumFailsWithSeverity(LIBSBML_SEV_ERROR) 
                                                          == numErrors);

  converter->setDocument(doc);
  int result = converter->convert();

  // fail if conversion was not valid
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);

  std::string newModel = writeSBMLToStdString(doc);
  
  std::string ffile = filename + file2;
  SBMLDocument* fdoc = readSBMLFromFile(ffile.c_str());
  std::string flatModel = writeSBMLToStdString(fdoc);

  fail_unless(flatModel == newModel);

  delete doc;
  delete fdoc;
  delete converter;
}


void TestPairNoStrip(std::string file1, std::string file2, std::string pkgToStrip,
              unsigned int numErrors = 0)
{
  std::string filename(TestDataDirectory);
  std::string dir("test_external_flat_strip/");

  ConversionProperties props;
  
  props.addOption("flatten comp");
  props.addOption("basePath", filename);
  props.addOption("performValidation", true);
  props.addOption("stripPackages", pkgToStrip);
  props.addOption("abortIfUnflattenable", "none");
  props.addOption("stripUnflattenablePackages", false);

  SBMLConverter* converter = SBMLConverterRegistry::getInstance().
                                                    getConverterFor(props);
  
  // load document
  std::string cfile = filename + dir + file1;  
  SBMLDocument* doc = readSBMLFromFile(cfile.c_str());

  // fail if there is no model (readSBMLFromFile always returns a valid doc)
  fail_unless(doc->getModel() != NULL);

  fail_unless(doc->getErrorLog()->getNumFailsWithSeverity(LIBSBML_SEV_ERROR) 
                                                          == numErrors);

  converter->setDocument(doc);
  int result = converter->convert();

  // fail if conversion was not valid
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);

  std::string newModel = writeSBMLToStdString(doc);
  
  std::string ffile = filename + dir + file2;
  SBMLDocument* fdoc = readSBMLFromFile(ffile.c_str());
  std::string flatModel = writeSBMLToStdString(fdoc);

  fail_unless(flatModel == newModel);

  delete doc;
  delete fdoc;
  delete converter;
}


START_TEST (test_comp_flatten_strip_fbc_submodels)
{ 
  // fbc in modeldefinitions within one doc
  if (SBMLExtensionRegistry::isPackageEnabled("fbc") == true)
  {
    TestPairDiffDir("aggregate_fbc2.xml", 
                    "aggregate_fbc2_flat_fbc_removed.xml", "fbc");
  }
}
END_TEST


START_TEST (test_comp_flatten_strip_fbc_submodels_1)
{ 
  // fbc in modeldefinitions within one doc
  if (SBMLExtensionRegistry::isPackageEnabled("fbc") == true)
  {
    TestPairDiffDir("aggregate_fbc2.xml", "aggregate_fbc2_flat.xml", "");
  }
}
END_TEST


START_TEST (test_comp_flatten_strip_fbc_external)
{ 
  // fbc in main doc and external doc
  if (SBMLExtensionRegistry::isPackageEnabled("fbc") == true)
  {
    TestPair("fbc_in_external.xml", 
             "fbc_in_external_flat_fbc_removed.xml", "fbc");
  }
}
END_TEST


START_TEST (test_comp_flatten_strip_fbc_external_1)
{ 
  // fbc in main doc and external doc - dont strip
  if (SBMLExtensionRegistry::isPackageEnabled("fbc") == true)
  {
    TestPair("fbc_in_external.xml", "fbc_in_external_flat.xml", "");
  }
}
END_TEST


START_TEST (test_comp_flatten_strip_fbc_external_2)
{ 
  // fbc not in main doc but only in external doc - strip 
  if (SBMLExtensionRegistry::isPackageEnabled("fbc") == true)
  {
    TestPair("fbc_in_external_only.xml", 
             "fbc_in_external_flat_fbc_removed.xml", "fbc");
  }
}
END_TEST


START_TEST (test_comp_flatten_strip_fbc_external_3)
{ 
  // fbc not in main doc but only in external doc
  // flat doc should declare fbc
  if (SBMLExtensionRegistry::isPackageEnabled("fbc") == true)
  {
    TestPair("fbc_in_external_only.xml", 
             "fbc_in_external_flat_fbc_added.xml", "");
  }
}
END_TEST


START_TEST (test_comp_flatten_strip_fbc_external_4)
{ 
  // comp in main doc not in external; fbc in both
  if (SBMLExtensionRegistry::isPackageEnabled("fbc") == true)
  {
    TestPair("fbc_in_external_no_comp.xml", 
             "fbc_in_external_flat_fbc_removed.xml", "fbc");
  }
}
END_TEST


START_TEST (test_comp_flatten_strip_layout_submodels)
{ 
  // layout in modeldefinitions within one doc
  if (SBMLExtensionRegistry::isPackageEnabled("layout") == true)
  {
    TestPair("layout_in_submodels.xml", 
             "layout_in_submodels_flat_layout_removed.xml", "layout");
  }
}
END_TEST


START_TEST (test_comp_flatten_strip_layout_submodels_1)
{ 
  // layout in modeldefinitions within one doc
  if (SBMLExtensionRegistry::isPackageEnabled("layout") == true)
  {
    TestPairDiffDir("aggregate_layout.xml", 
                    "aggregate_layout_flat_layout_removed.xml", "layout");
  }
}
END_TEST


START_TEST (test_comp_flatten_strip_layout_external)
{ 
 // layout in main doc and external doc
  if (SBMLExtensionRegistry::isPackageEnabled("layout") == true)
  {
    TestPair("layout_in_external.xml", 
             "layout_in_external_flat_layout_removed.xml", "layout");
  }
}
END_TEST


START_TEST (test_comp_flatten_strip_layout_external_1)
{ 
 // layout in main doc and external doc
  if (SBMLExtensionRegistry::isPackageEnabled("layout") == true)
  {
    TestPair("layout_in_external.xml", "layout_in_external_flat.xml", "");
  }
}
END_TEST


START_TEST (test_comp_flatten_strip_layout_external_2)
{ 
  // layout not in main doc but only in external doc
  if (SBMLExtensionRegistry::isPackageEnabled("layout") == true)
  {
    TestPair("layout_in_external_only.xml", 
             "layout_in_external_flat_layout_removed.xml", "layout");
  }
}
END_TEST


START_TEST (test_comp_flatten_strip_layout_external_3)
{ 
  // layout not in main doc but only in external doc
  // flat doc should declare layout
  if (SBMLExtensionRegistry::isPackageEnabled("layout") == true)
  {
    TestPair("layout_in_external_only.xml", 
             "layout_in_external_flat_layout_added.xml", "");
  }
}
END_TEST


START_TEST (test_comp_flatten_strip_layout_external_4)
{ 
  // comp in main doc not in external; layout in both
  if (SBMLExtensionRegistry::isPackageEnabled("layout") == true)
  {
    TestPair("layout_in_external_no_comp.xml", 
             "layout_in_external_flat_layout_removed.xml", "layout");
  }
}
END_TEST


START_TEST (test_comp_flatten_strip_unreq_unknown_submodels)
{ 
  // unknown unrequired package in modeldefinitions within one doc
  TestPairDiffDir("unknown9.xml", "unknown9_flat.xml", "");
}
END_TEST


START_TEST (test_comp_flatten_strip_unreq_unknown_external)
{ 
  // unknown unrequired in main doc and external doc
  TestPair("unreq_unknown_in_external.xml", "unreq_unknown_in_external_flat_unknown_removed.xml", "extra");
}
END_TEST


START_TEST (test_comp_flatten_strip_unreq_unknown_external_1)
{ 
  // unknown unrequired in main doc and external doc
  TestPair("unreq_unknown_in_external.xml", "unreq_unknown_in_external_flat_unknown_removed.xml", "");
}
END_TEST


START_TEST (test_comp_flatten_strip_unreq_unknown_external_1b)
{ 
  // unknown unrequired in main doc and external doc
  TestPairNoStrip("unreq_unknown_in_external.xml", "unreq_unknown_in_external_flat_unknown_remains.xml", "");
}
END_TEST


START_TEST (test_comp_flatten_strip_unreq_unknown_external_2)
{ 
  // unknown unrequired not in main doc but only in external doc
  TestPair("unreq_unknown_in_external_only.xml", "unreq_unknown_in_external_flat_unknown_removed.xml", "extra");
}
END_TEST


START_TEST (test_comp_flatten_strip_unreq_unknown_external_3)
{ 
  // unknown unrequired not in main doc but only in external doc
  TestPair("unreq_unknown_in_external_only.xml", "unreq_unknown_in_external_flat_unknown_removed.xml", "");
}
END_TEST


START_TEST (test_comp_flatten_strip_unreq_unknown_external_3_nostrip)
{ 
  // unknown unrequired not in main doc but only in external doc
  TestPairNoStrip("unreq_unknown_in_external_only.xml", "unreq_unknown_in_external_flat_unknown_remains.xml", "");
}
END_TEST


START_TEST (test_comp_flatten_strip_unreq_unknown_external_3_paramchild)
{ 
  // unknown unrequired not in main doc but only in external doc
  TestPairNoStrip("unreq_unknown_in_external_only_paramchild.xml", "unreq_unknown_in_external_flat_unknown_remains_paramchild.xml", "");
}
END_TEST


START_TEST (test_comp_flatten_strip_unreq_unknown_external_3_paramchild_no_comp)
{ 
  // unknown unrequired not in main doc but only in external doc
  TestPairNoStrip("unreq_unknown_in_external_only_paramchild_no_comp.xml", "unreq_unknown_in_external_flat_unknown_remains_paramchild.xml", "");
}
END_TEST


START_TEST (test_comp_flatten_strip_unreq_unknown_external_4)
{ 
  // comp in main doc not in external; unknown unrequired in both
  TestPair("unreq_unknown_in_external_no_comp.xml", "unreq_unknown_in_external_flat_unknown_removed.xml", "extra");
}
END_TEST


START_TEST (test_comp_flatten_strip_req_unknown_submodels)
{ 
  // unknown required package in modeldefinitions within one doc
  TestPairDiffDir("unknown10.xml", "unknown10_flat.xml", "", 1);
}
END_TEST


START_TEST (test_comp_flatten_strip_req_unknown_external)
{ 
  // unknown required in main doc and external doc
  TestPair("req_unknown_in_external.xml", "req_unknown_in_external_flat_unknown_removed.xml", "extra", 1);
}
END_TEST


START_TEST (test_comp_flatten_strip_req_unknown_external_1)
{ 
  // unknown required in main doc and external doc
  TestPair("req_unknown_in_external.xml", "req_unknown_in_external_flat_unknown_removed.xml", "", 1);
}
END_TEST


START_TEST (test_comp_flatten_strip_req_unknown_external_1b)
{ 
  // unknown required in main doc and external doc
  TestPairNoStrip("req_unknown_in_external.xml", "req_unknown_in_external_flat_unknown_remains.xml", "", 1);
}
END_TEST


START_TEST (test_comp_flatten_strip_req_unknown_external_2)
{ 
  // unknown required not in main doc but only in external doc
  TestPair("req_unknown_in_external_only.xml", "req_unknown_in_external_flat_unknown_removed.xml", "extra");
}
END_TEST


START_TEST (test_comp_flatten_strip_req_unknown_external_3)
{ 
  // unknown required not in main doc but only in external doc
  TestPair("req_unknown_in_external_only.xml", "req_unknown_in_external_flat_unknown_removed.xml", "");
}
END_TEST


START_TEST (test_comp_flatten_strip_req_unknown_external_3_nostrip)
{ 
  // unknown required not in main doc but only in external doc
  TestPairNoStrip("req_unknown_in_external_only.xml", "req_unknown_in_external_flat_unknown_remains.xml", "");
}
END_TEST


START_TEST (test_comp_flatten_strip_req_unknown_external_3_paramchild)
{ 
  // unknown required not in main doc but only in external doc
  TestPairNoStrip("req_unknown_in_external_only_paramchild.xml", "req_unknown_in_external_flat_unknown_remains_paramchild.xml", "");
}
END_TEST


START_TEST (test_comp_flatten_strip_req_unknown_external_3_paramchild_no_comp)
{ 
  // unknown required not in main doc but only in external doc
  TestPairNoStrip("req_unknown_in_external_only_paramchild_no_comp.xml", "req_unknown_in_external_flat_unknown_remains_paramchild.xml", "");
}
END_TEST


START_TEST (test_comp_flatten_strip_req_unknown_external_external)
{ 
  // unknown required not in main doc but only in external doc
  TestPairNoStrip("req_unknown_in_external_external_only.xml", "req_unknown_in_external_flat_unknown_remains.xml", "");
}
END_TEST


START_TEST (test_comp_flatten_strip_req_unknown_external_4)
{ 
  // comp in main doc not in external; unknown required in both
  TestPair("req_unknown_in_external_no_comp.xml", "req_unknown_in_external_flat_unknown_removed.xml", "extra", 1);
}
END_TEST


START_TEST (test_comp_flatten_strip_fbc_layout_external)
{ 
 // fbc and layout in main doc and external doc
  if (SBMLExtensionRegistry::isPackageEnabled("layout") == true
    && SBMLExtensionRegistry::isPackageEnabled("fbc") == true)
  {
    TestPair("fbc_layout_in_external.xml", 
             "fbc_layout_in_external_flat_both_removed.xml", "layout, fbc");
  }
}
END_TEST


START_TEST (test_comp_flatten_strip_fbc_layout_external_1)
{ 
 // fbc and layout in main doc and external doc
  if (SBMLExtensionRegistry::isPackageEnabled("layout") == true
    && SBMLExtensionRegistry::isPackageEnabled("fbc") == true)
  {
    TestPair("fbc_layout_in_external.xml", 
             "fbc_layout_in_external_flat_both_removed.xml", "fbc, layout");
  }
}
END_TEST


START_TEST (test_comp_flatten_strip_fbc_layout_external_2)
{ 
 // fbc and layout in main doc and external doc
  if (SBMLExtensionRegistry::isPackageEnabled("layout") == true
    && SBMLExtensionRegistry::isPackageEnabled("fbc") == true)
  {
    TestPair("fbc_layout_in_external.xml", 
             "fbc_layout_in_external_flat_fbc_removed.xml", "fbc");
  }
}
END_TEST


START_TEST (test_comp_flatten_strip_fbc_layout_external_3)
{ 
 // fbc and layout in main doc and external doc
  if (SBMLExtensionRegistry::isPackageEnabled("layout") == true
    && SBMLExtensionRegistry::isPackageEnabled("fbc") == true)
  {
    TestPair("fbc_layout_in_external.xml", 
             "fbc_layout_in_external_flat_layout_removed.xml", "layout");
  }
}
END_TEST


START_TEST (test_comp_flatten_strip_fbc_layout_external_4)
{ 
 // fbc and layout in main doc and external doc
  if (SBMLExtensionRegistry::isPackageEnabled("layout") == true
    && SBMLExtensionRegistry::isPackageEnabled("fbc") == true)
  {
    TestPair("fbc_layout_in_external.xml", 
             "fbc_layout_in_external_flat.xml", "");
  }
}
END_TEST


Suite *
create_suite_TestFlatteningConverterStripPackage (void)
{ 
  TCase *tcase = tcase_create("SBMLCompFlatteningConverterStripPackage");
  Suite *suite = suite_create("SBMLCompFlatteningConverterStripPackage");
  
  // fbc in modeldefinitions within one doc
  tcase_add_test(tcase, test_comp_flatten_strip_fbc_submodels);
  tcase_add_test(tcase, test_comp_flatten_strip_fbc_submodels_1);
  
  // fbc in main doc and external doc
  tcase_add_test(tcase, test_comp_flatten_strip_fbc_external);
  tcase_add_test(tcase, test_comp_flatten_strip_fbc_external_1);
  
  // fbc not in main doc but only in external doc - strip fbc
  tcase_add_test(tcase, test_comp_flatten_strip_fbc_external_2);

  // fbc not in main doc but only in external doc
  // flat doc should declare fbc
  tcase_add_test(tcase, test_comp_flatten_strip_fbc_external_3);

  // comp in main doc not in external; fbc in both
  tcase_add_test(tcase, test_comp_flatten_strip_fbc_external_4);

  // layout in modeldefinitions within one doc
  tcase_add_test(tcase, test_comp_flatten_strip_layout_submodels);
  tcase_add_test(tcase, test_comp_flatten_strip_layout_submodels_1);
 
  // layout in main doc and external doc
  tcase_add_test(tcase, test_comp_flatten_strip_layout_external);
  tcase_add_test(tcase, test_comp_flatten_strip_layout_external_1);
  
  // layout not in main doc but only in external doc
  tcase_add_test(tcase, test_comp_flatten_strip_layout_external_2);

  // layout not in main doc but only in external doc
  // flat doc should declare layout
  tcase_add_test(tcase, test_comp_flatten_strip_layout_external_3);

  // comp in main doc not in external; layout in both
  tcase_add_test(tcase, test_comp_flatten_strip_layout_external_4);

  // unknown unrequired package in modeldefinitions within one doc
  tcase_add_test(tcase, test_comp_flatten_strip_unreq_unknown_submodels);

  // unknown unrequired in main doc and external doc
  tcase_add_test(tcase, test_comp_flatten_strip_unreq_unknown_external);
  tcase_add_test(tcase, test_comp_flatten_strip_unreq_unknown_external_1);
  tcase_add_test(tcase, test_comp_flatten_strip_unreq_unknown_external_1b);
  
  // unknown unrequired not in main doc but only in external doc
  tcase_add_test(tcase, test_comp_flatten_strip_unreq_unknown_external_2);

  // unknown unrequired not in main doc but only in external doc
  tcase_add_test(tcase, test_comp_flatten_strip_unreq_unknown_external_3);
  tcase_add_test(tcase, test_comp_flatten_strip_unreq_unknown_external_3_nostrip);
  tcase_add_test(tcase, test_comp_flatten_strip_unreq_unknown_external_3_paramchild);
  tcase_add_test(tcase, test_comp_flatten_strip_unreq_unknown_external_3_paramchild_no_comp);

  // comp in main doc not in external; unknown unrequired in both
  tcase_add_test(tcase, test_comp_flatten_strip_unreq_unknown_external_4);

  // unknown required package in modeldefinitions within one doc
  tcase_add_test(tcase, test_comp_flatten_strip_req_unknown_submodels);
  
  // unknown required in main doc and external doc
  tcase_add_test(tcase, test_comp_flatten_strip_req_unknown_external);
  tcase_add_test(tcase, test_comp_flatten_strip_req_unknown_external_1);
  tcase_add_test(tcase, test_comp_flatten_strip_req_unknown_external_1b);
 
  // unknown required not in main doc but only in external doc
  tcase_add_test(tcase, test_comp_flatten_strip_req_unknown_external_2);

  // unknown required not in main doc but only in external doc
  tcase_add_test(tcase, test_comp_flatten_strip_req_unknown_external_3);
  tcase_add_test(tcase, test_comp_flatten_strip_req_unknown_external_3_nostrip);
  tcase_add_test(tcase, test_comp_flatten_strip_req_unknown_external_3_paramchild);
  tcase_add_test(tcase, test_comp_flatten_strip_req_unknown_external_3_paramchild_no_comp);

  // unknown required not in main doc nor in first external doc, but in referenced external doc.
  tcase_add_test(tcase, test_comp_flatten_strip_req_unknown_external_external);

  // comp in main doc not in external; unknown required in both
  tcase_add_test(tcase, test_comp_flatten_strip_req_unknown_external_4);
  
  // fbc and layout in main doc and external doc
  tcase_add_test(tcase, test_comp_flatten_strip_fbc_layout_external);
  tcase_add_test(tcase, test_comp_flatten_strip_fbc_layout_external_1);
  tcase_add_test(tcase, test_comp_flatten_strip_fbc_layout_external_2);
  tcase_add_test(tcase, test_comp_flatten_strip_fbc_layout_external_3);
  tcase_add_test(tcase, test_comp_flatten_strip_fbc_layout_external_4);
  
  
  suite_add_tcase(suite, tcase);

  return suite;
}


END_C_DECLS

