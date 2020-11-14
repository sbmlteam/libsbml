/**
 * \file    TestDistribAnnotationConverter.cpp
 * \brief   Implementation of the Tests for the distrib annotation converter
 * \author  Sarah M Keating
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

#include <sbml/packages/distrib/common/DistribExtensionTypes.h>

#include <sbml/conversion/SBMLConverterRegistry.h>
#include <sbml/SBMLReader.h>
#include <sbml/SBMLTypes.h>

#include <string>

#include <check.h>

using namespace std;

LIBSBML_CPP_NAMESPACE_USE

BEGIN_C_DECLS


extern char *TestDataDirectory;

void TestAnnotationToDistrib(string filebase)
{
  string filename(TestDataDirectory);

  ConversionProperties props;

  props.addOption("convert distrib annotations");

  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(props);

  // load document
  string cfile = filename + filebase + "_annot.xml";
  SBMLDocument* doc = readSBMLFromFile(cfile.c_str());

  // fail if there is no model (readSBMLFromFile always returns a valid document)
  fail_unless(doc->getModel() != NULL);

  converter->setDocument(doc);
  int result = converter->convert();

  // fail if conversion was not valid
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);

  string newModel = writeSBMLToStdString(doc);

  string ffile = filename + filebase + "_distrib.xml";
  SBMLDocument* fdoc = readSBMLFromFile(ffile.c_str());
  string flatModel = writeSBMLToStdString(fdoc);

  fail_unless(flatModel == newModel);

  delete doc;
  delete fdoc;
  delete converter;
}


void TestDistribToAnnotation(string filebase)
{
  string filename(TestDataDirectory);

  ConversionProperties props;

  props.addOption("convert distrib to annotations");

  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(props);

  // load document
  string cfile = filename + filebase + "_distrib.xml";
  SBMLDocument* doc = readSBMLFromFile(cfile.c_str());

  // fail if there is no model (readSBMLFromFile always returns a valid document)
  fail_unless(doc->getModel() != NULL);

  converter->setDocument(doc);
  int result = converter->convert();

  // fail if conversion was not valid
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);

  string newModel = writeSBMLToStdString(doc);

  string ffile = filename + filebase + "_annot.xml";
  SBMLDocument* fdoc = readSBMLFromFile(ffile.c_str());
  string flatModel = writeSBMLToStdString(fdoc);

  fail_unless(flatModel == newModel);

  delete doc;
  delete fdoc;
  delete converter;
}


void TestDistribToAnnotationWithMeans(string filebase)
{
  string filename(TestDataDirectory);

  ConversionProperties props;

  props.addOption("convert distrib to annotations");
  props.addOption("writeMeans", true);

  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(props);

  // load document
  string cfile = filename + filebase + "_distrib.xml";
  SBMLDocument* doc = readSBMLFromFile(cfile.c_str());

  // fail if there is no model (readSBMLFromFile always returns a valid document)
  fail_unless(doc->getModel() != NULL);

  converter->setDocument(doc);
  int result = converter->convert();

  // fail if conversion was not valid
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);

  string newModel = writeSBMLToStdString(doc);

  string ffile = filename + filebase + "_annot_means.xml";
  SBMLDocument* fdoc = readSBMLFromFile(ffile.c_str());
  string flatModel = writeSBMLToStdString(fdoc);

  fail_unless(flatModel == newModel);

  delete doc;
  delete fdoc;
  delete converter;
}


START_TEST(test_distrib_annotation_converter_normal_l3v2)
{
  TestAnnotationToDistrib("normal_l3v2");
  TestDistribToAnnotation("normal_l3v2");
  TestDistribToAnnotationWithMeans("normal_l3v2");
}
END_TEST


START_TEST(test_distrib_annotation_converter_normal_l3v1)
{
  TestAnnotationToDistrib("normal_l3v1");
  TestDistribToAnnotation("normal_l3v1");
}
END_TEST


START_TEST(test_distrib_annotation_converter_normal_l2v4)
{
  TestAnnotationToDistrib("normal_l2v4");
}
END_TEST


START_TEST(test_distrib_annotation_converter_normal_l3v1b)
{
  TestAnnotationToDistrib("normal_l3v1b");
  TestDistribToAnnotation("normal_l3v1b");
}
END_TEST


START_TEST(test_distrib_annotation_converter_normal_event_assignment)
{
  TestAnnotationToDistrib("normal_event_assignment");
  TestDistribToAnnotation("normal_event_assignment");
}
END_TEST


START_TEST(test_distrib_annotation_converter_normal_delay)
{
  TestAnnotationToDistrib("normal_delay");
  TestDistribToAnnotation("normal_delay");
}
END_TEST


START_TEST(test_distrib_annotation_converter_normal_priority)
{
  TestAnnotationToDistrib("normal_priority");
  TestDistribToAnnotation("normal_priority");
}
END_TEST


START_TEST(test_distrib_annotation_converter_normal_submodel)
{
  TestAnnotationToDistrib("normal_submodel");
  TestDistribToAnnotation("normal_submodel");
  TestDistribToAnnotationWithMeans("normal_submodel");
}
END_TEST


START_TEST(test_distrib_annotation_converter_normal_submodel2)
{
  TestAnnotationToDistrib("normal_submodel2");
}
END_TEST


START_TEST(test_distrib_annotation_converter_truncated_normal)
{
  TestAnnotationToDistrib("truncated_normal");
}
END_TEST


START_TEST(test_distrib_annotation_converter_uniform)
{
  TestAnnotationToDistrib("uniform");
  TestDistribToAnnotation("uniform");
  TestDistribToAnnotationWithMeans("uniform");
}
END_TEST


START_TEST(test_distrib_annotation_converter_uniformb)
{
  string filename(TestDataDirectory);

  ConversionProperties props;

  props.addOption("convert distrib annotations");

  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(props);

  // load document
  string cfile = filename + "uniform_b_annot.xml";
  SBMLDocument* doc = readSBMLFromFile(cfile.c_str());

  // fail if there is no model (readSBMLFromFile always returns a valid document)
  fail_unless(doc->getModel() != NULL);

  converter->setDocument(doc);
  int result = converter->convert();

  // fail if conversion was not valid
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);

  string newModel = writeSBMLToStdString(doc);

  string ffile = filename + "uniform_distrib.xml";
  SBMLDocument* fdoc = readSBMLFromFile(ffile.c_str());
  string flatModel = writeSBMLToStdString(fdoc);

  fail_unless(flatModel == newModel);

  delete doc;
  delete fdoc;
  delete converter;
}
END_TEST


START_TEST(test_distrib_annotation_converter_exponential)
{
  TestAnnotationToDistrib("exponential");
  TestDistribToAnnotation("exponential");
  TestDistribToAnnotationWithMeans("exponential");
}
END_TEST


START_TEST(test_distrib_annotation_converter_truncated_exponential)
{
  TestAnnotationToDistrib("truncated_exponential");
}
END_TEST


START_TEST(test_distrib_annotation_converter_gamma)
{
  TestAnnotationToDistrib("gamma");
  TestDistribToAnnotation("gamma");
  TestDistribToAnnotationWithMeans("gamma");
}
END_TEST


START_TEST(test_distrib_annotation_converter_truncated_gamma)
{
  TestAnnotationToDistrib("truncated_gamma");
}
END_TEST


START_TEST(test_distrib_annotation_converter_poisson)
{
  TestAnnotationToDistrib("poisson");
  TestDistribToAnnotation("poisson");
  TestDistribToAnnotationWithMeans("poisson");
}
END_TEST


START_TEST(test_distrib_annotation_converter_truncated_poisson)
{
  TestAnnotationToDistrib("truncated_poisson");
}
END_TEST


START_TEST(test_distrib_annotation_converter_lognormal)
{
  TestAnnotationToDistrib("lognormal");
  TestDistribToAnnotation("lognormal");
  TestDistribToAnnotationWithMeans("lognormal");
}
END_TEST


START_TEST(test_distrib_annotation_converter_chisquare)
{
  TestAnnotationToDistrib("chisquare");
  TestDistribToAnnotation("chisquare");
  TestDistribToAnnotationWithMeans("chisquare");
}
END_TEST


START_TEST(test_distrib_annotation_converter_laplace)
{
  TestAnnotationToDistrib("laplace");
  TestDistribToAnnotation("laplace");
  TestDistribToAnnotationWithMeans("laplace");
}
END_TEST


START_TEST(test_distrib_annotation_converter_cauchy)
{
  TestAnnotationToDistrib("cauchy");
  TestDistribToAnnotation("cauchy");
  TestDistribToAnnotationWithMeans("cauchy");
}
END_TEST


START_TEST(test_distrib_annotation_converter_rayleigh)
{
  TestAnnotationToDistrib("rayleigh");
  TestDistribToAnnotation("rayleigh");
  TestDistribToAnnotationWithMeans("rayleigh");
}
END_TEST


START_TEST(test_distrib_annotation_converter_binomial)
{
  TestAnnotationToDistrib("binomial");
  TestDistribToAnnotation("binomial");
  TestDistribToAnnotationWithMeans("binomial");
}
END_TEST


START_TEST(test_distrib_annotation_converter_bernoulli)
{
  TestAnnotationToDistrib("bernoulli");
  TestDistribToAnnotation("bernoulli");
  TestDistribToAnnotationWithMeans("bernoulli");
}
END_TEST


START_TEST(test_distrib_annotation_converter_none)
{
  TestAnnotationToDistrib("no");
  TestDistribToAnnotation("no");
}
END_TEST


START_TEST(test_distrib_annotation_converter_chisquare_wrongargs)
{
  string filename(TestDataDirectory);

  ConversionProperties props;

  props.addOption("convert distrib annotations");

  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(props);

  // load document
  string cfile = filename + "chisquare_annot_wrongargs.xml";
  SBMLDocument* doc = readSBMLFromFile(cfile.c_str());

  // fail if there is no model (readSBMLFromFile always returns a valid document)
  fail_unless(doc->getModel() != NULL);

  converter->setDocument(doc);
  int result = converter->convert();

  // fail if conversion failed
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);

  string newModel = writeSBMLToStdString(doc);

  string ffile = filename + "chisquare_distrib_wrongargs.xml"; //Not actaully changed.
  SBMLDocument* fdoc = readSBMLFromFile(ffile.c_str());
  string flatModel = writeSBMLToStdString(fdoc);

  fail_unless(flatModel == newModel);

  delete doc;
  delete fdoc;
  delete converter;
}
END_TEST


Suite *
create_suite_TestDistribAnnotationConverter (void)
{ 
  TCase *tcase = tcase_create("SBMLDistribAnnotationConverter");
  Suite *suite = suite_create("SBMLDistribAnnotationConverter");
  
  tcase_add_test(tcase, test_distrib_annotation_converter_normal_l3v2);
  tcase_add_test(tcase, test_distrib_annotation_converter_normal_l3v1);
  tcase_add_test(tcase, test_distrib_annotation_converter_normal_l2v4);
  tcase_add_test(tcase, test_distrib_annotation_converter_normal_l3v1b);
  tcase_add_test(tcase, test_distrib_annotation_converter_normal_event_assignment);
  tcase_add_test(tcase, test_distrib_annotation_converter_normal_delay);
  tcase_add_test(tcase, test_distrib_annotation_converter_normal_priority);
#ifdef USE_COMP
  tcase_add_test(tcase, test_distrib_annotation_converter_normal_submodel);
  tcase_add_test(tcase, test_distrib_annotation_converter_normal_submodel2);
#endif
  tcase_add_test(tcase, test_distrib_annotation_converter_truncated_normal);
  tcase_add_test(tcase, test_distrib_annotation_converter_uniform);
  tcase_add_test(tcase, test_distrib_annotation_converter_uniformb);
  tcase_add_test(tcase, test_distrib_annotation_converter_exponential);
  tcase_add_test(tcase, test_distrib_annotation_converter_truncated_exponential);
  tcase_add_test(tcase, test_distrib_annotation_converter_gamma);
  tcase_add_test(tcase, test_distrib_annotation_converter_truncated_gamma);
  tcase_add_test(tcase, test_distrib_annotation_converter_poisson);
  tcase_add_test(tcase, test_distrib_annotation_converter_truncated_poisson);
  tcase_add_test(tcase, test_distrib_annotation_converter_lognormal);
  tcase_add_test(tcase, test_distrib_annotation_converter_chisquare);
  tcase_add_test(tcase, test_distrib_annotation_converter_laplace);
  tcase_add_test(tcase, test_distrib_annotation_converter_cauchy);
  tcase_add_test(tcase, test_distrib_annotation_converter_rayleigh);
  tcase_add_test(tcase, test_distrib_annotation_converter_binomial);
  tcase_add_test(tcase, test_distrib_annotation_converter_bernoulli);
  tcase_add_test(tcase, test_distrib_annotation_converter_none);


  tcase_add_test(tcase, test_distrib_annotation_converter_chisquare_wrongargs);

  suite_add_tcase(suite, tcase);

  return suite;
}


END_C_DECLS

