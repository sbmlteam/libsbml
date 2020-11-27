/**
 * \file    TestCompFlatteningConverter.cpp
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

/**
 * Wraps the string s in the appropriate XML boilerplate.
 */
#define XML_START   "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
#define SBML_START  "<sbml "
#define NS_L3v1     "xmlns=\"http://www.sbml.org/sbml/level3/version1/core\" "
#define NS_L3v2     "xmlns=\"http://www.sbml.org/sbml/level3/version2/core\" "
#define LV_L3v1     "level=\"3\" version=\"1\" comp:required=\"true\">\n"
#define LV_L3v2     "level=\"3\" version=\"2\" comp:required=\"true\">\n"
#define NS_COMP     "xmlns:comp=\"http://www.sbml.org/sbml/level3/version1/comp/version1\" "
#define SBML_END    "</sbml>\n"

#define wrapSBML_L3v1(s)  XML_START SBML_START NS_L3v1 NS_COMP LV_L3v1 s SBML_END
#define wrapSBML_L3v2(s)  XML_START SBML_START NS_COMP NS_L3v2 LV_L3v2 s SBML_END


static bool
equals(const char* expected, const char* actual)
{
  if (!strcmp(expected, actual)) return true;

  printf("\nStrings are not equal:\n");
  printf("Expected:\n[%s]\n", expected);
  printf("Actual:\n[%s]\n", actual);

  return false;
}





extern char *TestDataDirectory;

START_TEST (test_comp_upconvert)
{
  const char* expected = wrapSBML_L3v2(
    "  <model id=\"aggregate\">\n"
    "    <comp:listOfSubmodels>\n"
    "      <comp:submodel comp:id=\"submod1\" comp:modelRef=\"enzyme\"/>\n"
    "    </comp:listOfSubmodels>\n"
    "  </model>\n"
    "  <comp:listOfModelDefinitions>\n"
    "    <comp:modelDefinition id=\"enzyme\" name=\"enzyme\">\n"
    "      <listOfCompartments>\n"
    "        <compartment id=\"comp\" spatialDimensions=\"3\" size=\"1\" units=\"litre\" constant=\"true\"/>\n"
    "      </listOfCompartments>\n"
    "      <listOfSpecies>\n"
    "        <species id=\"S\" compartment=\"comp\" initialConcentration=\"0\" substanceUnits=\"mole\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\"/>\n"
    "      </listOfSpecies>\n"
    "      <listOfReactions>\n"
    "        <reaction id=\"J0\" reversible=\"true\">\n"
    "          <listOfReactants>\n"
    "            <speciesReference species=\"S\" stoichiometry=\"1\" constant=\"true\"/>\n"
    "          </listOfReactants>\n"
    "        </reaction>\n"
    "      </listOfReactions>\n"
    "    </comp:modelDefinition>\n"
    "  </comp:listOfModelDefinitions>\n");

  string filename(TestDataDirectory);
  //string filename("C:\\Development\\libsbml\\src\\sbml\\packages\\comp\\util\\test\\test-data\\");

  filename += "l3v1_fast.xml";  
  SBMLDocument* doc = readSBMLFromFile(filename.c_str());

  // fast should be set
  CompSBMLDocumentPlugin* compPlug =
    static_cast<CompSBMLDocumentPlugin*>(doc->getPlugin("comp"));
  Reaction * r = compPlug->getModelDefinition(0)->getReaction(0);
  fail_unless(r->isSetFast());


  fail_unless(doc->setLevelAndVersion(3, 2));


  string S = writeSBMLToStdString(doc);

  fail_unless(equals(expected, S.c_str()));

  // fast should not be set
  compPlug =
    static_cast<CompSBMLDocumentPlugin*>(doc->getPlugin("comp"));
  r = compPlug->getModelDefinition(0)->getReaction(0);
  fail_unless(!r->isSetFast());

  delete doc;
}
END_TEST


START_TEST(test_comp_downconvert)
{
  const char* expected = wrapSBML_L3v1(
    "  <model id=\"aggregate\">\n"
    "    <comp:listOfSubmodels>\n"
    "      <comp:submodel comp:id=\"submod1\" comp:modelRef=\"enzyme\"/>\n"
    "    </comp:listOfSubmodels>\n"
    "  </model>\n"
    "  <comp:listOfModelDefinitions>\n"
    "    <comp:modelDefinition id=\"enzyme\" name=\"enzyme\">\n"
    "      <listOfCompartments>\n"
    "        <compartment id=\"comp\" spatialDimensions=\"3\" size=\"1\" units=\"litre\" constant=\"true\"/>\n"
    "      </listOfCompartments>\n"
    "      <listOfSpecies>\n"
    "        <species id=\"S\" compartment=\"comp\" initialConcentration=\"0\" substanceUnits=\"mole\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\"/>\n"
    "      </listOfSpecies>\n"
    "      <listOfReactions>\n"
    "        <reaction id=\"J0\" reversible=\"true\" fast=\"false\">\n"
    "          <listOfReactants>\n"
    "            <speciesReference species=\"S\" stoichiometry=\"1\" constant=\"true\"/>\n"
    "          </listOfReactants>\n"
    "        </reaction>\n"
    "      </listOfReactions>\n"
    "    </comp:modelDefinition>\n"
    "  </comp:listOfModelDefinitions>\n");
  
  string filename(TestDataDirectory);
  //string filename("C:\\Development\\libsbml\\src\\sbml\\packages\\comp\\util\\test\\test-data\\");

  filename += "l3v2_fast.xml";
  SBMLDocument* doc = readSBMLFromFile(filename.c_str());

  // fast should not be set
  CompSBMLDocumentPlugin* compPlug =
    static_cast<CompSBMLDocumentPlugin*>(doc->getPlugin("comp"));
  Reaction * r = compPlug->getModelDefinition(0)->getReaction(0);
  fail_unless(!r->isSetFast());

  fail_unless(doc->setLevelAndVersion(3, 1));
  
  string S = writeSBMLToStdString(doc);

  fail_unless(equals(expected, S.c_str()));

  // fast should be set
  compPlug =
    static_cast<CompSBMLDocumentPlugin*>(doc->getPlugin("comp"));
  r = compPlug->getModelDefinition(0)->getReaction(0);
  fail_unless(r->isSetFast());

  delete doc;
}
END_TEST



Suite *
create_suite_TestLevelVersionConversion (void)
{ 
  TCase *tcase = tcase_create("SBMLCompLevelVersionConversion");
  Suite *suite = suite_create("SBMLCompLevelVersionConversion");
  
  tcase_add_test(tcase, test_comp_upconvert);
  tcase_add_test(tcase, test_comp_downconvert);

  suite_add_tcase(suite, tcase);

  return suite;
}


END_C_DECLS

