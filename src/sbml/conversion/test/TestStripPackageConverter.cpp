/**
 * @file    TestStripPackageConverter.cpp
 * @brief   Tests for converter that strips packages
 * @author  Sarah Keating
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

#include <sbml/conversion/SBMLStripPackageConverter.h>
#include <sbml/conversion/ConversionProperties.h>



#include <string>
using namespace std;

#include <check.h>

LIBSBML_CPP_NAMESPACE_USE

BEGIN_C_DECLS

#include <sbml/util/util.h>

extern char *TestDataDirectory;

START_TEST (test_strip_unknownreq)
{
  std::string filename(TestDataDirectory);
  std::string fileIn = filename + "package1.xml";

  SBMLDocument* d = readSBMLFromFile(fileIn.c_str());

  fail_unless(d != NULL);

  ConversionProperties props;
  props.addOption("package", "unknownreq");

  SBMLConverter* converter = new SBMLStripPackageConverter();
  converter->setProperties(&props);
  converter->setDocument(d);

  fail_unless (converter->convert() == LIBSBML_OPERATION_SUCCESS);

  std::string newModel = writeSBMLToStdString(d);
  
  
  std::string fileOut = filename + "package1-unknownreq_stripped.xml";
  SBMLDocument* fdoc = readSBMLFromFile(fileOut.c_str());
  string stripped = writeSBMLToStdString(fdoc);
  
  fail_unless(stripped == newModel);

  delete converter;
  delete fdoc;
  delete d;
}
END_TEST


START_TEST (test_strip_comp)
{
  std::string filename(TestDataDirectory);
  std::string fileIn = filename + "package1.xml";

  SBMLDocument* d = readSBMLFromFile(fileIn.c_str());

  fail_unless(d != NULL);

  ConversionProperties props;
  props.addOption("package", "comp");

  SBMLConverter* converter = new SBMLStripPackageConverter();
  converter->setProperties(&props);
  converter->setDocument(d);

  fail_unless (converter->convert() == LIBSBML_OPERATION_SUCCESS);

  std::string newModel = writeSBMLToStdString(d);
  
  
  std::string fileOut = filename + "package1-comp_stripped.xml";
  SBMLDocument* fdoc = readSBMLFromFile(fileOut.c_str());
  string stripped = writeSBMLToStdString(fdoc);
  
  fail_unless(stripped == newModel);

  delete converter;
  delete fdoc;
  delete d;
}
END_TEST

START_TEST(test_strip_unknown)
{
  const std::string model =
    "<?xml version='1.0' encoding='UTF-8'?>\n"
    "<sbml xmlns='http://www.sbml.org/sbml/level3/version1/core' level='3' version='1' "
    "xmlns:pkg1='http://www.sbml.org/sbml/level3/version1/pkg1/version1' pkg1:required='true' "
    "xmlns:pkg2='http://www.sbml.org/sbml/level3/version1/pkg2/version1' pkg2:required='true' "
    "xmlns:pkg3='http://www.sbml.org/sbml/level3/version1/pkg3/version1' pkg3:required='false' "
    ">\n\t<model/>\n"
    "</sbml>";

  SBMLDocument* doc = readSBMLFromString(model.c_str());
  fail_unless(doc->getNumUnknownPackages() == 3);
  
  ConversionProperties props;
  props.addOption("stripPackage", true);
  props.addOption("stripAllUnrecognized", true);

  fail_unless(doc->convert(props) == LIBSBML_OPERATION_SUCCESS);

  fail_unless(doc->getNumUnknownPackages() == 0);

  delete doc;
}
END_TEST

START_TEST(test_strip_unknown_1)
{
  const std::string model =
    "<?xml version='1.0' encoding='UTF-8'?>\n"
    "<sbml xmlns='http://www.sbml.org/sbml/level3/version1/core' level='3' version='1' "
    "xmlns:pkg1='http://www.sbml.org/sbml/level3/version1/pkg1/version1' pkg1:required='true' "
    "xmlns:pkg2='http://www.sbml.org/sbml/level3/version1/pkg2/version1' pkg2:required='true' "
    "xmlns:pkg3='http://www.sbml.org/sbml/level3/version1/pkg3/version1' pkg3:required='false' "
    ">\n\t<model/>\n"
    "</sbml>";

  SBMLDocument* doc = readSBMLFromString(model.c_str());
  fail_unless(doc->getNumUnknownPackages() == 3);

  ConversionProperties props;
  props.addOption("stripPackage", true);

  fail_unless(doc->convert(props) == LIBSBML_OPERATION_SUCCESS);

  fail_unless(doc->getNumUnknownPackages() == 3);

  props.addOption("package", "pkg1,pkg3");
  fail_unless(doc->convert(props) == LIBSBML_OPERATION_SUCCESS);

  fail_unless(doc->getNumUnknownPackages() == 1);

  delete doc;
}
END_TEST

Suite *
create_suite_TestStripPackageConverter (void)
{ 
  Suite *suite = suite_create("StripPackageConverter");
  TCase *tcase = tcase_create("StripPackageConverter");


  tcase_add_test(tcase, test_strip_unknownreq);
  tcase_add_test(tcase, test_strip_comp);
  tcase_add_test(tcase, test_strip_unknown);
  tcase_add_test(tcase, test_strip_unknown_1);

  suite_add_tcase(suite, tcase);

  return suite;
}


END_C_DECLS

