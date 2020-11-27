/**
 * \file    TestArraysFlatteningConverter.cpp
 * \brief   Implementation of the Tests for the Arrays flattening converter
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

#include <sbml/packages/arrays/common/ArraysExtensionTypes.h>

#include <sbml/conversion/SBMLConverterRegistry.h>
#include <sbml/SBMLReader.h>
#include <sbml/SBMLTypes.h>

#include <string>

#include <check.h>

using namespace std;

LIBSBML_CPP_NAMESPACE_USE

BEGIN_C_DECLS


extern char *TestDataDirectory;

START_TEST (test_arrays_get_flattening_converter)
{
  ConversionProperties props;
  props.addOption("flatten arrays");
  props.addOption("performValidation", true);
  
  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(props);
  
  // ensure we have a converter
  fail_unless(converter!= NULL);  

  // ensure that conversion without document does not work
  fail_unless(converter->convert() == LIBSBML_INVALID_OBJECT);

  ArraysPkgNamespaces ns;
  SBMLDocument doc(&ns);
  static_cast<ArraysSBMLDocumentPlugin*>(doc.getPlugin("arrays"))->setRequired(true);
  converter->setDocument(&doc);

  // ensure that conversion without model does not work
  fail_unless(converter->convert() == LIBSBML_INVALID_OBJECT);

  //Model* model =
  doc.createModel("model");

  // no parameters - so no dimensions - still wont work
  fail_unless(converter->convert() == LIBSBML_INVALID_OBJECT);
  doc.getModel()->createParameter();

  // now conversion should work
  fail_unless(converter->convert() == LIBSBML_OPERATION_SUCCESS);

  delete converter;
}
END_TEST


START_TEST(test_arrays_flattening_converter_zero_size)
{
  string filename(TestDataDirectory);

  ConversionProperties props;

  props.addOption("flatten arrays");

  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(props);

  // Create document
  ArraysPkgNamespaces *ns = new ArraysPkgNamespaces();
  SBMLDocument* doc = new SBMLDocument(ns);

  Model * m = doc->createModel();

  Compartment * c = m->createCompartment();
  c->setId("c");
  c->setSize(1);
  c->setSpatialDimensions(3.0);
  c->setConstant(true);

  Species *s = m->createSpecies();
  s->setId("s");
  s->setCompartment("c");
  s->initDefaults();

  Parameter *p = m->createParameter();
  p->setId("n");
  p->setValue(0);
  p->setConstant(true);

  ArraysSBasePlugin* plugin = static_cast<ArraysSBasePlugin*>(s->getPlugin("arrays"));
  Dimension *d = plugin->createDimension();
  d->setSize("n");
  d->setArrayDimension(0);

  converter->setDocument(doc);
  int result = converter->convert();

  // fail if conversion was not valid
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);

  string newModel = writeSBMLToStdString(doc);

  string ffile = filename + "arrays_zero_size_flat.xml";
  SBMLDocument* fdoc = readSBMLFromFile(ffile.c_str());
  string flatModel = writeSBMLToStdString(fdoc);

  fail_unless(flatModel == newModel);

  delete doc;
  delete fdoc;
  delete converter;
}
END_TEST


START_TEST(test_arrays_flattening_converter_zero_size_from_file)
{
  string filename(TestDataDirectory);

  ConversionProperties props;

  props.addOption("flatten arrays");

  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(props);

  // load document
  string cfile = filename + "arrays_zero_size.xml";
  SBMLDocument* doc = readSBMLFromFile(cfile.c_str());

  // fail if there is no model (readSBMLFromFile always returns a valid document)
  fail_unless(doc->getModel() != NULL);

  converter->setDocument(doc);
  int result = converter->convert();

  // fail if conversion was not valid
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);

  string newModel = writeSBMLToStdString(doc);

  string ffile = filename + "arrays_zero_size_flat.xml";
  SBMLDocument* fdoc = readSBMLFromFile(ffile.c_str());
  string flatModel = writeSBMLToStdString(fdoc);

  fail_unless(flatModel == newModel);

  delete doc;
  delete fdoc;
  delete converter;
}
END_TEST


START_TEST(test_arrays_flattening_converter_1x1_from_file)
{
  string filename(TestDataDirectory);

  ConversionProperties props;

  props.addOption("flatten arrays");

  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(props);

  // load document
  string cfile = filename + "arrays_1x1.xml";
  SBMLDocument* doc = readSBMLFromFile(cfile.c_str());

  // fail if there is no model (readSBMLFromFile always returns a valid document)
  fail_unless(doc->getModel() != NULL);

  converter->setDocument(doc);
  int result = converter->convert();

  // fail if conversion was not valid
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);

  string newModel = writeSBMLToStdString(doc);

  string ffile = filename + "arrays_1x1_flat.xml";
  SBMLDocument* fdoc = readSBMLFromFile(ffile.c_str());
  string flatModel = writeSBMLToStdString(fdoc);

  fail_unless(flatModel == newModel);

  delete doc;
  delete fdoc;
  delete converter;
}
END_TEST


START_TEST(test_arrays_flattening_converter_1D_size_from_file)
{
  string filename(TestDataDirectory);

  ConversionProperties props;

  props.addOption("flatten arrays");

  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(props);

  // load document
  string cfile = filename + "arrays_1D_size.xml";
  SBMLDocument* doc = readSBMLFromFile(cfile.c_str());

  // fail if there is no model (readSBMLFromFile always returns a valid document)
  fail_unless(doc->getModel() != NULL);

  converter->setDocument(doc);
  int result = converter->convert();

  // fail if conversion was not valid
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);

  string newModel = writeSBMLToStdString(doc);

  string ffile = filename + "arrays_1D_size_flat.xml";
  SBMLDocument* fdoc = readSBMLFromFile(ffile.c_str());
  string flatModel = writeSBMLToStdString(fdoc);

  fail_unless(flatModel == newModel);

  delete doc;
  delete fdoc;
  delete converter;
}
END_TEST


START_TEST(test_arrays_flattening_converter_2D_size_from_file)
{
  string filename(TestDataDirectory);

  ConversionProperties props;

  props.addOption("flatten arrays");

  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(props);

  // load document
  string cfile = filename + "arrays_2D_size.xml";
  SBMLDocument* doc = readSBMLFromFile(cfile.c_str());

  // fail if there is no model (readSBMLFromFile always returns a valid document)
  fail_unless(doc->getModel() != NULL);

  converter->setDocument(doc);
  int result = converter->convert();

  // fail if conversion was not valid
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);

  string newModel = writeSBMLToStdString(doc);

  string ffile = filename + "arrays_2D_size_flat.xml";
  SBMLDocument* fdoc = readSBMLFromFile(ffile.c_str());
  string flatModel = writeSBMLToStdString(fdoc);

  fail_unless(flatModel == newModel);

  delete doc;
  delete fdoc;
  delete converter;
}
END_TEST

START_TEST(test_arrays_flattening_converter_3D_size_from_file)
{
  string filename(TestDataDirectory);

  ConversionProperties props;

  props.addOption("flatten arrays");

  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(props);

  // load document
  string cfile = filename + "arrays_3D_size.xml";
  SBMLDocument* doc = readSBMLFromFile(cfile.c_str());

  // fail if there is no model (readSBMLFromFile always returns a valid document)
  fail_unless(doc->getModel() != NULL);

  converter->setDocument(doc);
  int result = converter->convert();

  // fail if conversion was not valid
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);

  string newModel = writeSBMLToStdString(doc);

  string ffile = filename + "arrays_3D_size_flat.xml";
  SBMLDocument* fdoc = readSBMLFromFile(ffile.c_str());
  string flatModel = writeSBMLToStdString(fdoc);

  fail_unless(flatModel == newModel);

  delete doc;
  delete fdoc;
  delete converter;
}
END_TEST


START_TEST(test_arrays_flattening_converter_1D_initialAssignment_from_file)
{
  string filename(TestDataDirectory);

  ConversionProperties props;

  props.addOption("flatten arrays");

  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(props);

  // load document
  string cfile = filename + "arrays_1D_initialAssignment.xml";
  SBMLDocument* doc = readSBMLFromFile(cfile.c_str());

  // fail if there is no model (readSBMLFromFile always returns a valid document)
  fail_unless(doc->getModel() != NULL);

  converter->setDocument(doc);
  int result = converter->convert();

  // fail if conversion was not valid
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);

  string newModel = writeSBMLToStdString(doc);

  string ffile = filename + "arrays_1D_initialAssignment_flat.xml";
  SBMLDocument* fdoc = readSBMLFromFile(ffile.c_str());
  string flatModel = writeSBMLToStdString(fdoc);

  fail_unless(flatModel == newModel);

  delete doc;
  delete fdoc;
  delete converter;
}
END_TEST


START_TEST(test_arrays_flattening_converter_1D_initialAssignment2_from_file)
{
  string filename(TestDataDirectory);

  ConversionProperties props;

  props.addOption("flatten arrays");

  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(props);

  // load document
  string cfile = filename + "arrays_1D_initialAssignment_2.xml";
  SBMLDocument* doc = readSBMLFromFile(cfile.c_str());

  // fail if there is no model (readSBMLFromFile always returns a valid document)
  fail_unless(doc->getModel() != NULL);

  converter->setDocument(doc);
  int result = converter->convert();

  // fail if conversion was not valid
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);

  string newModel = writeSBMLToStdString(doc);

  string ffile = filename + "arrays_1D_initialAssignment_2_flat.xml";
  SBMLDocument* fdoc = readSBMLFromFile(ffile.c_str());
  string flatModel = writeSBMLToStdString(fdoc);

  fail_unless(flatModel == newModel);

  delete doc;
  delete fdoc;
  delete converter;
}
END_TEST


START_TEST(test_arrays_flattening_converter_1D_reverse_from_file)
{
  string filename(TestDataDirectory);

  ConversionProperties props;

  props.addOption("flatten arrays");

  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(props);

  // load document
  string cfile = filename + "arrays_1D_reverse1.xml";
  SBMLDocument* doc = readSBMLFromFile(cfile.c_str());

  // fail if there is no model (readSBMLFromFile always returns a valid document)
  fail_unless(doc->getModel() != NULL);

  converter->setDocument(doc);
  int result = converter->convert();

  // fail if conversion was not valid
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);

  string newModel = writeSBMLToStdString(doc);

  string ffile = filename + "arrays_1D_reverse_flat.xml";
  SBMLDocument* fdoc = readSBMLFromFile(ffile.c_str());
  string flatModel = writeSBMLToStdString(fdoc);

  fail_unless(flatModel == newModel);

  delete doc;
  delete fdoc;
  delete converter;
}
END_TEST


START_TEST(test_arrays_flattening_converter_1D_reverse1_from_file)
{
  string filename(TestDataDirectory);

  ConversionProperties props;

  props.addOption("flatten arrays");

  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(props);

  // load document
  string cfile = filename + "arrays_1D_reverse.xml";
  SBMLDocument* doc = readSBMLFromFile(cfile.c_str());

  // fail if there is no model (readSBMLFromFile always returns a valid document)
  fail_unless(doc->getModel() != NULL);

  converter->setDocument(doc);
  int result = converter->convert();

  // fail if conversion was not valid
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);

  string newModel = writeSBMLToStdString(doc);

  string ffile = filename + "arrays_1D_reverse_flat.xml";
  SBMLDocument* fdoc = readSBMLFromFile(ffile.c_str());
  string flatModel = writeSBMLToStdString(fdoc);

  fail_unless(flatModel == newModel);

  delete doc;
  delete fdoc;
  delete converter;
}
END_TEST


START_TEST(test_arrays_flattening_converter_1D_reverse2_from_file)
{
  string filename(TestDataDirectory);

  ConversionProperties props;

  props.addOption("flatten arrays");

  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(props);

  // load document
  string cfile = filename + "arrays_1D_reverse_2.xml";
  SBMLDocument* doc = readSBMLFromFile(cfile.c_str());

  // fail if there is no model (readSBMLFromFile always returns a valid document)
  fail_unless(doc->getModel() != NULL);

  converter->setDocument(doc);
  int result = converter->convert();

  // fail if conversion was not valid
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);

  string newModel = writeSBMLToStdString(doc);

  string ffile = filename + "arrays_1D_reverse_2_flat.xml";
  SBMLDocument* fdoc = readSBMLFromFile(ffile.c_str());
  string flatModel = writeSBMLToStdString(fdoc);

  fail_unless(flatModel == newModel);

  delete doc;
  delete fdoc;
  delete converter;
}
END_TEST


START_TEST(test_arrays_flattening_converter_1D_species_compartment_from_file)
{
  string filename(TestDataDirectory);

  ConversionProperties props;

  props.addOption("flatten arrays");

  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(props);

  // load document
  string cfile = filename + "arrays_1D_species_compartment.xml";
  SBMLDocument* doc = readSBMLFromFile(cfile.c_str());

  // fail if there is no model (readSBMLFromFile always returns a valid document)
  fail_unless(doc->getModel() != NULL);

  converter->setDocument(doc);
  int result = converter->convert();

  // fail if conversion was not valid
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);

  string newModel = writeSBMLToStdString(doc);

  string ffile = filename + "arrays_1D_species_compartment_flat.xml";
  SBMLDocument* fdoc = readSBMLFromFile(ffile.c_str());
  string flatModel = writeSBMLToStdString(fdoc);

  fail_unless(flatModel == newModel);

  delete doc;
  delete fdoc;
  delete converter;
}
END_TEST


START_TEST(test_arrays_flattening_converter_3D_initialAssignment_from_file)
{
  string filename(TestDataDirectory);

  ConversionProperties props;

  props.addOption("flatten arrays");

  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(props);

  // load document
  string cfile = filename + "arrays_3D_initialAssignment.xml";
  SBMLDocument* doc = readSBMLFromFile(cfile.c_str());

  // fail if there is no model (readSBMLFromFile always returns a valid document)
  fail_unless(doc->getModel() != NULL);

  converter->setDocument(doc);
  int result = converter->convert();

  // fail if conversion was not valid
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);

  string newModel = writeSBMLToStdString(doc);

  string ffile = filename + "arrays_3D_initialAssignment_flat.xml";
  SBMLDocument* fdoc = readSBMLFromFile(ffile.c_str());
  string flatModel = writeSBMLToStdString(fdoc);

  fail_unless(flatModel == newModel);

  delete doc;
  delete fdoc;
  delete converter;
}
END_TEST


START_TEST(test_arrays_flattening_converter_1D_reaction_noKL)
{
  string filename(TestDataDirectory);

  ConversionProperties props;

  props.addOption("flatten arrays");

  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(props);

  // load document
  string cfile = filename + "arrays_1D_reaction_noKL.xml";
  SBMLDocument* doc = readSBMLFromFile(cfile.c_str());

  // fail if there is no model (readSBMLFromFile always returns a valid document)
  fail_unless(doc->getModel() != NULL);

  converter->setDocument(doc);
  int result = converter->convert();

  // fail if conversion was not valid
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);

  string newModel = writeSBMLToStdString(doc);

  string ffile = filename + "arrays_1D_reaction_noKL_flat.xml";
  SBMLDocument* fdoc = readSBMLFromFile(ffile.c_str());
  string flatModel = writeSBMLToStdString(fdoc);

  fail_unless(flatModel == newModel);

  delete doc;
  delete fdoc;
  delete converter;
}
END_TEST


START_TEST(test_arrays_flattening_converter_1D_reaction)
{
  string filename(TestDataDirectory);

  ConversionProperties props;

  props.addOption("flatten arrays");

  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(props);

  // load document
  string cfile = filename + "arrays_1D_reaction.xml";
  SBMLDocument* doc = readSBMLFromFile(cfile.c_str());

  // fail if there is no model (readSBMLFromFile always returns a valid document)
  fail_unless(doc->getModel() != NULL);

  converter->setDocument(doc);
  int result = converter->convert();

  // fail if conversion was not valid
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);

  string newModel = writeSBMLToStdString(doc);

  string ffile = filename + "arrays_1D_reaction_flat.xml";
  SBMLDocument* fdoc = readSBMLFromFile(ffile.c_str());
  string flatModel = writeSBMLToStdString(fdoc);

  fail_unless(flatModel == newModel);

  delete doc;
  delete fdoc;
  delete converter;
}
END_TEST


START_TEST(test_arrays_flattening_converter_1D_event)
{
  string filename(TestDataDirectory);

  ConversionProperties props;

  props.addOption("flatten arrays");

  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(props);

  // load document
  string cfile = filename + "arrays_1D_event.xml";
  SBMLDocument* doc = readSBMLFromFile(cfile.c_str());

  // fail if there is no model (readSBMLFromFile always returns a valid document)
  fail_unless(doc->getModel() != NULL);

  converter->setDocument(doc);
  int result = converter->convert();

  // fail if conversion was not valid
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);

  string newModel = writeSBMLToStdString(doc);

  string ffile = filename + "arrays_1D_event_flat.xml";
  SBMLDocument* fdoc = readSBMLFromFile(ffile.c_str());
  string flatModel = writeSBMLToStdString(fdoc);

  fail_unless(flatModel == newModel);

  delete doc;
  delete fdoc;
  delete converter;
}
END_TEST


START_TEST(test_arrays_flattening_converter_2D_species_compartment_from_file)
{
  string filename(TestDataDirectory);

  ConversionProperties props;

  props.addOption("flatten arrays");

  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(props);

  // load document
  string cfile = filename + "arrays_2D_species_compartment.xml";
  SBMLDocument* doc = readSBMLFromFile(cfile.c_str());

  // fail if there is no model (readSBMLFromFile always returns a valid document)
  fail_unless(doc->getModel() != NULL);

  converter->setDocument(doc);
  int result = converter->convert();

  // fail if conversion was not valid
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);

  string newModel = writeSBMLToStdString(doc);

  string ffile = filename + "arrays_2D_species_compartment_flat.xml";
  SBMLDocument* fdoc = readSBMLFromFile(ffile.c_str());
  string flatModel = writeSBMLToStdString(fdoc);

  fail_unless(flatModel == newModel);

  delete doc;
  delete fdoc;
  delete converter;
}
END_TEST


START_TEST(test_arrays_flattening_converter_reaction_sr_noRefAtt)
{
  string filename(TestDataDirectory);

  ConversionProperties props;

  props.addOption("flatten arrays");

  SBMLConverter* converter = SBMLConverterRegistry::getInstance().getConverterFor(props);

  // load document
  string cfile = filename + "arrays_1D_reaction_2.xml";
  SBMLDocument* doc = readSBMLFromFile(cfile.c_str());

  // fail if there is no model (readSBMLFromFile always returns a valid document)
  fail_unless(doc->getModel() != NULL);

  converter->setDocument(doc);
  int result = converter->convert();

  // fail if conversion was not valid
  fail_unless(result == LIBSBML_OPERATION_SUCCESS);

  string newModel = writeSBMLToStdString(doc);

  string ffile = filename + "arrays_1D_reaction_2_flat.xml";
  SBMLDocument* fdoc = readSBMLFromFile(ffile.c_str());
  string flatModel = writeSBMLToStdString(fdoc);

  fail_unless(flatModel == newModel);

  delete doc;
  delete fdoc;
  delete converter;
}
END_TEST


Suite *
create_suite_TestFlatteningConverter (void)
{ 
  TCase *tcase = tcase_create("SBMLArraysFlatteningConverter");
  Suite *suite = suite_create("SBMLArraysFlatteningConverter");
  
  tcase_add_test(tcase, test_arrays_get_flattening_converter);
  tcase_add_test(tcase, test_arrays_flattening_converter_zero_size);

  tcase_add_test(tcase, test_arrays_flattening_converter_zero_size_from_file);
  tcase_add_test(tcase, test_arrays_flattening_converter_1x1_from_file);
  tcase_add_test(tcase, test_arrays_flattening_converter_1D_size_from_file);
  tcase_add_test(tcase, test_arrays_flattening_converter_2D_size_from_file);
  tcase_add_test(tcase, test_arrays_flattening_converter_3D_size_from_file);
  tcase_add_test(tcase, test_arrays_flattening_converter_1D_initialAssignment_from_file);
  tcase_add_test(tcase, test_arrays_flattening_converter_1D_initialAssignment2_from_file);
  tcase_add_test(tcase, test_arrays_flattening_converter_1D_reverse_from_file);
  tcase_add_test(tcase, test_arrays_flattening_converter_1D_reverse1_from_file);
  tcase_add_test(tcase, test_arrays_flattening_converter_1D_reverse2_from_file);
  tcase_add_test(tcase, test_arrays_flattening_converter_1D_species_compartment_from_file);
  tcase_add_test(tcase, test_arrays_flattening_converter_3D_initialAssignment_from_file);
  tcase_add_test(tcase, test_arrays_flattening_converter_1D_reaction_noKL);
  tcase_add_test(tcase, test_arrays_flattening_converter_1D_reaction);
  tcase_add_test(tcase, test_arrays_flattening_converter_1D_event);
  tcase_add_test(tcase, test_arrays_flattening_converter_2D_species_compartment_from_file);
  tcase_add_test(tcase, test_arrays_flattening_converter_reaction_sr_noRefAtt);

  suite_add_tcase(suite, tcase);

  return suite;
}


END_C_DECLS

