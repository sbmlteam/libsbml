/**
 * \file    TestDistribAnnotationConverter.cpp
 * \brief   Implementation of the Tests for the distrib annotation converter
 * \author  Sarah M Keating
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
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

START_TEST(test_distrib_annotation_converter_normal_l3v2)
{
  string filename(TestDataDirectory);

  ConversionProperties props;

  props.addOption("convert distrib annotations");

  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(props);

  // load document
  string cfile = filename + "normal_l3v2_annot.xml";
  SBMLDocument* doc = readSBMLFromFile(cfile.c_str());

  // fail if there is no model (readSBMLFromFile always returns a valid document)
  fail_unless(doc->getModel() != NULL);

  converter->setDocument(doc);
  int result = converter->convert();

  // fail if conversion was not valid
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);

  string newModel = writeSBMLToStdString(doc);

  string ffile = filename + "normal_l3v2_distrib.xml";
  SBMLDocument* fdoc = readSBMLFromFile(ffile.c_str());
  string flatModel = writeSBMLToStdString(fdoc);

  fail_unless(flatModel == newModel);

  delete doc;
  delete fdoc;
  delete converter;
}
END_TEST


START_TEST(test_distrib_annotation_converter_normal_l3v1)
{
  string filename(TestDataDirectory);

  ConversionProperties props;

  props.addOption("convert distrib annotations");

  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(props);

  // load document
  string cfile = filename + "normal_l3v1_annot.xml";
  SBMLDocument* doc = readSBMLFromFile(cfile.c_str());

  // fail if there is no model (readSBMLFromFile always returns a valid document)
  fail_unless(doc->getModel() != NULL);

  converter->setDocument(doc);
  int result = converter->convert();

  // fail if conversion was not valid
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);

  string newModel = writeSBMLToStdString(doc);

  string ffile = filename + "normal_l3v1_distrib.xml";
  SBMLDocument* fdoc = readSBMLFromFile(ffile.c_str());
  string flatModel = writeSBMLToStdString(fdoc);

  fail_unless(flatModel == newModel);

  delete doc;
  delete fdoc;
  delete converter;
}
END_TEST


START_TEST(test_distrib_annotation_converter_normal_l2v4)
{
  string filename(TestDataDirectory);

  ConversionProperties props;

  props.addOption("convert distrib annotations");

  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(props);

  // load document
  string cfile = filename + "normal_l2v4_annot.xml";
  SBMLDocument* doc = readSBMLFromFile(cfile.c_str());

  // fail if there is no model (readSBMLFromFile always returns a valid document)
  fail_unless(doc->getModel() != NULL);

  converter->setDocument(doc);
  int result = converter->convert();

  // fail if conversion was not valid
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);

  string newModel = writeSBMLToStdString(doc);

  string ffile = filename + "normal_l2v4_distrib.xml";
  SBMLDocument* fdoc = readSBMLFromFile(ffile.c_str());
  string flatModel = writeSBMLToStdString(fdoc);

  fail_unless(flatModel == newModel);

  delete doc;
  delete fdoc;
  delete converter;
}
END_TEST


START_TEST(test_distrib_annotation_converter_normal_l3v1b)
{
  string filename(TestDataDirectory);

  ConversionProperties props;

  props.addOption("convert distrib annotations");

  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(props);

  // load document
  string cfile = filename + "normal_l3v1b_annot.xml";
  SBMLDocument* doc = readSBMLFromFile(cfile.c_str());

  // fail if there is no model (readSBMLFromFile always returns a valid document)
  fail_unless(doc->getModel() != NULL);

  converter->setDocument(doc);
  int result = converter->convert();

  // fail if conversion was not valid
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);

  string newModel = writeSBMLToStdString(doc);

  string ffile = filename + "normal_l3v1b_distrib.xml";
  SBMLDocument* fdoc = readSBMLFromFile(ffile.c_str());
  string flatModel = writeSBMLToStdString(fdoc);

  fail_unless(flatModel == newModel);

  delete doc;
  delete fdoc;
  delete converter;
}
END_TEST


START_TEST(test_distrib_annotation_converter_normal_event_assignment)
{
  string filename(TestDataDirectory);

  ConversionProperties props;

  props.addOption("convert distrib annotations");

  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(props);

  // load document
  string cfile = filename + "normal_event_assignment_annot.xml";
  SBMLDocument* doc = readSBMLFromFile(cfile.c_str());

  // fail if there is no model (readSBMLFromFile always returns a valid document)
  fail_unless(doc->getModel() != NULL);

  converter->setDocument(doc);
  int result = converter->convert();

  // fail if conversion was not valid
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);

  string newModel = writeSBMLToStdString(doc);

  string ffile = filename + "normal_event_assignment_distrib.xml";
  SBMLDocument* fdoc = readSBMLFromFile(ffile.c_str());
  string flatModel = writeSBMLToStdString(fdoc);

  fail_unless(flatModel == newModel);

  delete doc;
  delete fdoc;
  delete converter;
}
END_TEST


START_TEST(test_distrib_annotation_converter_normal_delay)
{
  string filename(TestDataDirectory);

  ConversionProperties props;

  props.addOption("convert distrib annotations");

  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(props);

  // load document
  string cfile = filename + "normal_delay_annot.xml";
  SBMLDocument* doc = readSBMLFromFile(cfile.c_str());

  // fail if there is no model (readSBMLFromFile always returns a valid document)
  fail_unless(doc->getModel() != NULL);

  converter->setDocument(doc);
  int result = converter->convert();

  // fail if conversion was not valid
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);

  string newModel = writeSBMLToStdString(doc);

  string ffile = filename + "normal_delay_distrib.xml";
  SBMLDocument* fdoc = readSBMLFromFile(ffile.c_str());
  string flatModel = writeSBMLToStdString(fdoc);

  fail_unless(flatModel == newModel);

  delete doc;
  delete fdoc;
  delete converter;
}
END_TEST


START_TEST(test_distrib_annotation_converter_normal_priority)
{
  string filename(TestDataDirectory);

  ConversionProperties props;

  props.addOption("convert distrib annotations");

  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(props);

  // load document
  string cfile = filename + "normal_priority_annot.xml";
  SBMLDocument* doc = readSBMLFromFile(cfile.c_str());

  // fail if there is no model (readSBMLFromFile always returns a valid document)
  fail_unless(doc->getModel() != NULL);

  converter->setDocument(doc);
  int result = converter->convert();

  // fail if conversion was not valid
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);

  string newModel = writeSBMLToStdString(doc);

  string ffile = filename + "normal_priority_distrib.xml";
  SBMLDocument* fdoc = readSBMLFromFile(ffile.c_str());
  string flatModel = writeSBMLToStdString(fdoc);

  fail_unless(flatModel == newModel);

  delete doc;
  delete fdoc;
  delete converter;
}
END_TEST


START_TEST(test_distrib_annotation_converter_normal_submodel)
{
  string filename(TestDataDirectory);

  ConversionProperties props;

  props.addOption("convert distrib annotations");

  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(props);

  // load document
  string cfile = filename + "normal_submodel_annot.xml";
  SBMLDocument* doc = readSBMLFromFile(cfile.c_str());

  // fail if there is no model (readSBMLFromFile always returns a valid document)
  fail_unless(doc->getModel() != NULL);

  converter->setDocument(doc);
  int result = converter->convert();

  // fail if conversion was not valid
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);

  string newModel = writeSBMLToStdString(doc);

  string ffile = filename + "normal_submodel_distrib.xml";
  SBMLDocument* fdoc = readSBMLFromFile(ffile.c_str());
  string flatModel = writeSBMLToStdString(fdoc);

  fail_unless(flatModel == newModel);

  delete doc;
  delete fdoc;
  delete converter;
}
END_TEST


START_TEST(test_distrib_annotation_converter_truncated_normal)
{
  string filename(TestDataDirectory);

  ConversionProperties props;

  props.addOption("convert distrib annotations");

  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(props);

  // load document
  string cfile = filename + "truncated_normal_annot.xml";
  SBMLDocument* doc = readSBMLFromFile(cfile.c_str());

  // fail if there is no model (readSBMLFromFile always returns a valid document)
  fail_unless(doc->getModel() != NULL);

  converter->setDocument(doc);
  int result = converter->convert();

  // fail if conversion was not valid
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);

  string newModel = writeSBMLToStdString(doc);

  string ffile = filename + "truncated_normal_distrib.xml";
  SBMLDocument* fdoc = readSBMLFromFile(ffile.c_str());
  string flatModel = writeSBMLToStdString(fdoc);

  fail_unless(flatModel == newModel);

  delete doc;
  delete fdoc;
  delete converter;
}
END_TEST


START_TEST(test_distrib_annotation_converter_uniform)
{
  string filename(TestDataDirectory);

  ConversionProperties props;

  props.addOption("convert distrib annotations");

  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(props);

  // load document
  string cfile = filename + "uniform_annot.xml";
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
  string filename(TestDataDirectory);

  ConversionProperties props;

  props.addOption("convert distrib annotations");

  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(props);

  // load document
  string cfile = filename + "exponential_annot.xml";
  SBMLDocument* doc = readSBMLFromFile(cfile.c_str());

  // fail if there is no model (readSBMLFromFile always returns a valid document)
  fail_unless(doc->getModel() != NULL);

  converter->setDocument(doc);
  int result = converter->convert();

  // fail if conversion was not valid
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);

  string newModel = writeSBMLToStdString(doc);

  string ffile = filename + "exponential_distrib.xml";
  SBMLDocument* fdoc = readSBMLFromFile(ffile.c_str());
  string flatModel = writeSBMLToStdString(fdoc);

  fail_unless(flatModel == newModel);

  delete doc;
  delete fdoc;
  delete converter;
}
END_TEST


START_TEST(test_distrib_annotation_converter_truncated_exponential)
{
  string filename(TestDataDirectory);

  ConversionProperties props;

  props.addOption("convert distrib annotations");

  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(props);

  // load document
  string cfile = filename + "truncated_exponential_annot.xml";
  SBMLDocument* doc = readSBMLFromFile(cfile.c_str());

  // fail if there is no model (readSBMLFromFile always returns a valid document)
  fail_unless(doc->getModel() != NULL);

  converter->setDocument(doc);
  int result = converter->convert();

  // fail if conversion was not valid
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);

  string newModel = writeSBMLToStdString(doc);

  string ffile = filename + "truncated_exponential_distrib.xml";
  SBMLDocument* fdoc = readSBMLFromFile(ffile.c_str());
  string flatModel = writeSBMLToStdString(fdoc);

  fail_unless(flatModel == newModel);

  delete doc;
  delete fdoc;
  delete converter;
}
END_TEST


START_TEST(test_distrib_annotation_converter_gamma)
{
  string filename(TestDataDirectory);

  ConversionProperties props;

  props.addOption("convert distrib annotations");

  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(props);

  // load document
  string cfile = filename + "gamma_annot.xml";
  SBMLDocument* doc = readSBMLFromFile(cfile.c_str());

  // fail if there is no model (readSBMLFromFile always returns a valid document)
  fail_unless(doc->getModel() != NULL);

  converter->setDocument(doc);
  int result = converter->convert();

  // fail if conversion was not valid
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);

  string newModel = writeSBMLToStdString(doc);

  string ffile = filename + "gamma_distrib.xml";
  SBMLDocument* fdoc = readSBMLFromFile(ffile.c_str());
  string flatModel = writeSBMLToStdString(fdoc);

  fail_unless(flatModel == newModel);

  delete doc;
  delete fdoc;
  delete converter;
}
END_TEST


START_TEST(test_distrib_annotation_converter_truncated_gamma)
{
  string filename(TestDataDirectory);

  ConversionProperties props;

  props.addOption("convert distrib annotations");

  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(props);

  // load document
  string cfile = filename + "truncated_gamma_annot.xml";
  SBMLDocument* doc = readSBMLFromFile(cfile.c_str());

  // fail if there is no model (readSBMLFromFile always returns a valid document)
  fail_unless(doc->getModel() != NULL);

  converter->setDocument(doc);
  int result = converter->convert();

  // fail if conversion was not valid
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);

  string newModel = writeSBMLToStdString(doc);

  string ffile = filename + "truncated_gamma_distrib.xml";
  SBMLDocument* fdoc = readSBMLFromFile(ffile.c_str());
  string flatModel = writeSBMLToStdString(fdoc);

  fail_unless(flatModel == newModel);

  delete doc;
  delete fdoc;
  delete converter;
}
END_TEST


START_TEST(test_distrib_annotation_converter_poisson)
{
  string filename(TestDataDirectory);

  ConversionProperties props;

  props.addOption("convert distrib annotations");

  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(props);

  // load document
  string cfile = filename + "poisson_annot.xml";
  SBMLDocument* doc = readSBMLFromFile(cfile.c_str());

  // fail if there is no model (readSBMLFromFile always returns a valid document)
  fail_unless(doc->getModel() != NULL);

  converter->setDocument(doc);
  int result = converter->convert();

  // fail if conversion was not valid
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);

  string newModel = writeSBMLToStdString(doc);

  string ffile = filename + "poisson_distrib.xml";
  SBMLDocument* fdoc = readSBMLFromFile(ffile.c_str());
  string flatModel = writeSBMLToStdString(fdoc);

  fail_unless(flatModel == newModel);

  delete doc;
  delete fdoc;
  delete converter;
}
END_TEST


START_TEST(test_distrib_annotation_converter_truncated_poisson)
{
  string filename(TestDataDirectory);

  ConversionProperties props;

  props.addOption("convert distrib annotations");

  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(props);

  // load document
  string cfile = filename + "truncated_poisson_annot.xml";
  SBMLDocument* doc = readSBMLFromFile(cfile.c_str());

  // fail if there is no model (readSBMLFromFile always returns a valid document)
  fail_unless(doc->getModel() != NULL);

  converter->setDocument(doc);
  int result = converter->convert();

  // fail if conversion was not valid
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);

  string newModel = writeSBMLToStdString(doc);

  string ffile = filename + "truncated_poisson_distrib.xml";
  SBMLDocument* fdoc = readSBMLFromFile(ffile.c_str());
  string flatModel = writeSBMLToStdString(fdoc);

  fail_unless(flatModel == newModel);

  delete doc;
  delete fdoc;
  delete converter;
}
END_TEST


START_TEST(test_distrib_annotation_converter_lognormal)
{
  string filename(TestDataDirectory);

  ConversionProperties props;

  props.addOption("convert distrib annotations");

  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(props);

  // load document
  string cfile = filename + "lognormal_annot.xml";
  SBMLDocument* doc = readSBMLFromFile(cfile.c_str());

  // fail if there is no model (readSBMLFromFile always returns a valid document)
  fail_unless(doc->getModel() != NULL);

  converter->setDocument(doc);
  int result = converter->convert();

  // fail if conversion was not valid
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);

  string newModel = writeSBMLToStdString(doc);

  string ffile = filename + "lognormal_distrib.xml";
  SBMLDocument* fdoc = readSBMLFromFile(ffile.c_str());
  string flatModel = writeSBMLToStdString(fdoc);

  fail_unless(flatModel == newModel);

  delete doc;
  delete fdoc;
  delete converter;
}
END_TEST


START_TEST(test_distrib_annotation_converter_chisquare)
{
  string filename(TestDataDirectory);

  ConversionProperties props;

  props.addOption("convert distrib annotations");

  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(props);

  // load document
  string cfile = filename + "chisquare_annot.xml";
  SBMLDocument* doc = readSBMLFromFile(cfile.c_str());

  // fail if there is no model (readSBMLFromFile always returns a valid document)
  fail_unless(doc->getModel() != NULL);

  converter->setDocument(doc);
  int result = converter->convert();

  // fail if conversion was not valid
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);

  string newModel = writeSBMLToStdString(doc);

  string ffile = filename + "chisquare_distrib.xml";
  SBMLDocument* fdoc = readSBMLFromFile(ffile.c_str());
  string flatModel = writeSBMLToStdString(fdoc);

  fail_unless(flatModel == newModel);

  delete doc;
  delete fdoc;
  delete converter;
}
END_TEST


START_TEST(test_distrib_annotation_converter_laplace)
{
  string filename(TestDataDirectory);

  ConversionProperties props;

  props.addOption("convert distrib annotations");

  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(props);

  // load document
  string cfile = filename + "laplace_annot.xml";
  SBMLDocument* doc = readSBMLFromFile(cfile.c_str());

  // fail if there is no model (readSBMLFromFile always returns a valid document)
  fail_unless(doc->getModel() != NULL);

  converter->setDocument(doc);
  int result = converter->convert();

  // fail if conversion was not valid
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);

  string newModel = writeSBMLToStdString(doc);

  string ffile = filename + "laplace_distrib.xml";
  SBMLDocument* fdoc = readSBMLFromFile(ffile.c_str());
  string flatModel = writeSBMLToStdString(fdoc);

  fail_unless(flatModel == newModel);

  delete doc;
  delete fdoc;
  delete converter;
}
END_TEST


START_TEST(test_distrib_annotation_converter_cauchy)
{
  string filename(TestDataDirectory);

  ConversionProperties props;

  props.addOption("convert distrib annotations");

  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(props);

  // load document
  string cfile = filename + "cauchy_annot.xml";
  SBMLDocument* doc = readSBMLFromFile(cfile.c_str());

  // fail if there is no model (readSBMLFromFile always returns a valid document)
  fail_unless(doc->getModel() != NULL);

  converter->setDocument(doc);
  int result = converter->convert();

  // fail if conversion was not valid
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);

    string newModel = writeSBMLToStdString(doc);

  string ffile = filename + "cauchy_distrib.xml";
  SBMLDocument* fdoc = readSBMLFromFile(ffile.c_str());
  string flatModel = writeSBMLToStdString(fdoc);

  fail_unless(flatModel == newModel);

  delete doc;
  delete fdoc;
  delete converter;
}
END_TEST


START_TEST(test_distrib_annotation_converter_rayleigh)
{
  string filename(TestDataDirectory);

  ConversionProperties props;

  props.addOption("convert distrib annotations");

  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(props);

  // load document
  string cfile = filename + "rayleigh_annot.xml";
  SBMLDocument* doc = readSBMLFromFile(cfile.c_str());

  // fail if there is no model (readSBMLFromFile always returns a valid document)
  fail_unless(doc->getModel() != NULL);

  converter->setDocument(doc);
  int result = converter->convert();

  // fail if conversion was not valid
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);

  string newModel = writeSBMLToStdString(doc);

  string ffile = filename + "rayleigh_distrib.xml";
  SBMLDocument* fdoc = readSBMLFromFile(ffile.c_str());
  string flatModel = writeSBMLToStdString(fdoc);

  fail_unless(flatModel == newModel);

  delete doc;
  delete fdoc;
  delete converter;
}
END_TEST


START_TEST(test_distrib_annotation_converter_binomial)
{
  string filename(TestDataDirectory);

  ConversionProperties props;

  props.addOption("convert distrib annotations");

  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(props);

  // load document
  string cfile = filename + "binomial_annot.xml";
  SBMLDocument* doc = readSBMLFromFile(cfile.c_str());

  // fail if there is no model (readSBMLFromFile always returns a valid document)
  fail_unless(doc->getModel() != NULL);

  converter->setDocument(doc);
  int result = converter->convert();

  // fail if conversion was not valid
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);

  string newModel = writeSBMLToStdString(doc);

  string ffile = filename + "binomial_distrib.xml";
  SBMLDocument* fdoc = readSBMLFromFile(ffile.c_str());
  string flatModel = writeSBMLToStdString(fdoc);

  fail_unless(flatModel == newModel);

  delete doc;
  delete fdoc;
  delete converter;
}
END_TEST


START_TEST(test_distrib_annotation_converter_bernoulli)
{
  string filename(TestDataDirectory);

  ConversionProperties props;

  props.addOption("convert distrib annotations");

  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(props);

  // load document
  string cfile = filename + "bernoulli_annot.xml";
  SBMLDocument* doc = readSBMLFromFile(cfile.c_str());

  // fail if there is no model (readSBMLFromFile always returns a valid document)
  fail_unless(doc->getModel() != NULL);

  converter->setDocument(doc);
  int result = converter->convert();

  // fail if conversion was not valid
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);

  string newModel = writeSBMLToStdString(doc);

  string ffile = filename + "bernoulli_distrib.xml";
  SBMLDocument* fdoc = readSBMLFromFile(ffile.c_str());
  string flatModel = writeSBMLToStdString(fdoc);

  fail_unless(flatModel == newModel);

  delete doc;
  delete fdoc;
  delete converter;
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


  tcase_add_test(tcase, test_distrib_annotation_converter_chisquare_wrongargs);

  suite_add_tcase(suite, tcase);

  return suite;
}


END_C_DECLS

