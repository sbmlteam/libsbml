/**
 * @file    TestLevelVersionConverter.cpp
 * @brief   Tests for level version converter
 * @author  Sarah Keating
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2015 jointly by the following organizations:
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

#include <sbml/SBMLReader.h>
#include <sbml/SBMLTypes.h>

#include <sbml/conversion/SBMLLevelVersionConverter.h>
#include <sbml/conversion/ConversionProperties.h>



#include <string>
using namespace std;

#include <check.h>

LIBSBML_CPP_NAMESPACE_USE

BEGIN_C_DECLS

#include <sbml/util/util.h>


extern char *TestDataDirectory;


START_TEST (test_setup)
{
  SBMLLevelVersionConverter * converter = new SBMLLevelVersionConverter();

  fail_unless (converter->getDefaultProperties().hasOption("setLevelAndVersion") == true);
  fail_unless (converter->getDefaultProperties().hasOption("strict") == true);
  fail_unless (converter->getDefaultProperties().getTargetNamespaces() != NULL);
  
  delete converter;
}
END_TEST


START_TEST (test_setup_properties)
{
  SBMLNamespaces sbmlns(2, 5);
  ConversionProperties prop(&sbmlns);
  prop.addOption("strict", false, "should validity be preserved");
  prop.addOption("setLevelAndVersion", true, "convert the document to the given level and version");
  prop.addOption("ignorePackages", false);

  SBMLLevelVersionConverter * converter = new SBMLLevelVersionConverter();
  converter->setProperties(&prop);

  fail_unless (converter->getValidityFlag() == false);
  fail_unless (converter->getProperties()->hasOption("ignorePackages") == true);
  fail_unless (converter->getProperties()->getOption("ignorePackages")->getBoolValue() == false);
  fail_unless (converter->getTargetLevel() == 2);
  fail_unless (converter->getTargetVersion() == 5);
  
  delete converter;
}
END_TEST


START_TEST (test_setDocument)
{
  SBMLLevelVersionConverter * converter = new SBMLLevelVersionConverter();
  SBMLDocument *d = new SBMLDocument(3, 1);
  d->createModel();
  converter->setDocument(d);

  fail_unless (converter->getDocument() == d);
  
  delete converter;
  delete d;
}
END_TEST


START_TEST (test_convertL3V1ToL2V5_strict)
{
  SBMLNamespaces sbmlns(2, 5);
  ConversionProperties prop(&sbmlns);
  prop.addOption("strict", true, "should validity be preserved");
  prop.addOption("setLevelAndVersion", true, "convert the document to the given level and version");
  prop.addOption("ignorePackages", true);

  SBMLLevelVersionConverter * converter = new SBMLLevelVersionConverter();
  converter->setProperties(&prop);

  string filename(TestDataDirectory);
  filename += "l3v1-components.xml";

  SBMLDocument* d = readSBMLFromFile(filename.c_str());

  Model * m = d->getModel();
  KineticLaw *kl = m->getReaction(0)->getKineticLaw();

  // things specific to L3
  fail_unless(d->getLevel() == 3);
  fail_unless(d->getVersion() == 1);
  fail_unless(m->isSetSubstanceUnits() == true);
  fail_unless(m->getSubstanceUnits() == "mole");
  fail_unless(kl->getNumLocalParameters() == 1);

  // convert to L2V5
  converter->setDocument(d);
  fail_unless(converter->convert() == LIBSBML_OPERATION_SUCCESS);

  m = d->getModel();
  kl = m->getReaction(0)->getKineticLaw();

  fail_unless(d->getLevel() == 2);
  fail_unless(d->getVersion() == 5);
  fail_unless(m->isSetSubstanceUnits() == false);
  fail_unless(m->getSubstanceUnits() == "");
  fail_unless(kl->getNumLocalParameters() == 0);
  
  delete d;
  delete converter;
}
END_TEST


START_TEST (test_convertL2V5ToL3V1_strict)
{
  SBMLNamespaces sbmlns(3, 1);
  ConversionProperties prop(&sbmlns);
  prop.addOption("strict", true, "should validity be preserved");
  prop.addOption("setLevelAndVersion", true, "convert the document to the given level and version");
  prop.addOption("ignorePackages", true);

  SBMLLevelVersionConverter * converter = new SBMLLevelVersionConverter();
  converter->setProperties(&prop);

  string filename(TestDataDirectory);
  filename += "l2v5-components.xml";

  SBMLDocument* d = readSBMLFromFile(filename.c_str());

  Model * m = d->getModel();
  Species *s = m->getSpecies(0);

  // things specific to L2V5
  fail_unless(d->getLevel() == 2);
  fail_unless(d->getVersion() == 5);
  fail_unless(m->getNumCompartmentTypes() == 1);
  fail_unless(s->isSetSpeciesType() == true);
  fail_unless(s->getSpeciesType() == "gg");

  // convert to L3V1
  converter->setDocument(d);
  fail_unless(converter->convert() == LIBSBML_OPERATION_SUCCESS);

  m = d->getModel();
  s = m->getSpecies(0);

  fail_unless(d->getLevel() == 3);
  fail_unless(d->getVersion() == 1);
  fail_unless(m->getNumCompartmentTypes() == 0);
  fail_unless(s->isSetSpeciesType() == false);
  fail_unless(s->getSpeciesType() == "");
  
  delete d;
  delete converter;
}
END_TEST


Suite *
create_suite_TestLevelVersionConverter (void)
{ 
  Suite *suite = suite_create("LevelVersionConverter");
  TCase *tcase = tcase_create("LevelVersionConverter");


  tcase_add_test(tcase, test_setup);
  tcase_add_test(tcase, test_setDocument);
  tcase_add_test(tcase, test_setup_properties);

  tcase_add_test(tcase, test_convertL3V1ToL2V5_strict);
  tcase_add_test(tcase, test_convertL2V5ToL3V1_strict);

  suite_add_tcase(suite, tcase);

  return suite;
}


END_C_DECLS

