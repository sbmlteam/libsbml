/**
 * \file    TestUncertMLParsing.cpp
 * \brief   Implementation of the Tests for the UncertML parsing
 * \author  Sarah Keating
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2009-2011 jointly by the following organizations: 
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
#include <sbml/SBMLDocument.h>
#include <sbml/packages/distrib/common/DistribExtensionTypes.h>


#include <sbml/Model.h>


#include <check.h>

using namespace std;

LIBSBML_CPP_NAMESPACE_USE

CK_CPPSTART

extern char *TestDataDirectory;

static bool
equals (const char* expected, const char* actual)
{
  if ( !strcmp(expected, actual) ) return true;

  printf( "\nStrings are not equal:\n"  );
  printf( "Expected:\n[%s]\n", expected );
  printf( "Actual:\n[%s]\n"  , actual   );

  return false;
}



START_TEST (test_uncertml_read_normalDistribution)
{
  char *filename = safe_strcat(TestDataDirectory, "normalDistrib.xml");
  SBMLDocument *document = readSBMLFromFile(filename);
  free(filename);

  fail_unless ( document->getModel() != NULL );

  FunctionDefinition * fd = document->getModel()->getFunctionDefinition(0);
  DistribFunctionDefinitionPlugin * plug = 
    static_cast<DistribFunctionDefinitionPlugin*>(fd->getPlugin("distrib"));

  const DrawFromDistribution * d = plug->getDrawFromDistribution();

  const UncertMLNode * uncert = d->getUncertML();

  fail_unless (uncert->getElementName() == "NormalDistribution");
  fail_unless (uncert->getNumAttributes() == 1);
  fail_unless (uncert->getAttributes().getName(0) == "definition");
  fail_unless (uncert->getAttributes().getValue(0) == 
                    "http://www.uncertml.org/distributions/normal");
  fail_unless (uncert->getNumChildren() == 2);

  const UncertMLNode * uncert1 = uncert->getChild(0);

  fail_unless (uncert1->getElementName() == "mean");
  fail_unless (uncert1->getNumAttributes() == 0);
  fail_unless (uncert1->getNumChildren() == 1);

  const UncertMLNode * uncert2 = uncert->getChild(1);

  fail_unless (uncert2->getElementName() == "variance");
  fail_unless (uncert2->getNumAttributes() == 0);
  fail_unless (uncert2->getNumChildren() == 1);

  const UncertMLNode * child = uncert1->getChild(0);

  fail_unless (child->getElementName() == "var");
  fail_unless (child->getNumAttributes() == 1);
  fail_unless (child->getAttributes().getName(0) == "varId");
  fail_unless (child->getAttributes().getValue(0) == "mu");
  fail_unless (child->getNumChildren() == 0);

  child = uncert2->getChild(0);

  fail_unless (child->getElementName() == "var");
  fail_unless (child->getNumAttributes() == 1);
  fail_unless (child->getAttributes().getName(0) == "varId");
  fail_unless (child->getAttributes().getValue(0) == "sigma");
  fail_unless (child->getNumChildren() == 0);

  delete document;
}
END_TEST
 

START_TEST ( test_uncertml_write_normalDistribution)
{
  const char *expected = 
    "<UncertML xmlns=\"http://www.uncertml.org/3.0\">\n"
    "  <NormalDistribution definition=\"http://www.uncertml.org/distributions/normal\">\n"
    "    <mean>\n"
    "      <var varId=\"mu\"/>\n"
    "    </mean>\n"
    "    <variance>\n"
    "      <var varId=\"sigma\"/>\n"
    "    </variance>\n"
    "  </NormalDistribution>\n"
    "</UncertML>";

  char *filename = safe_strcat(TestDataDirectory, "normalDistrib.xml");
  SBMLDocument *document = readSBMLFromFile(filename);
  free(filename);

  fail_unless ( document->getModel() != NULL );

  FunctionDefinition * fd = document->getModel()->getFunctionDefinition(0);
  DistribFunctionDefinitionPlugin * plug = 
    static_cast<DistribFunctionDefinitionPlugin*>(fd->getPlugin("distrib"));

  const DrawFromDistribution * d = plug->getDrawFromDistribution();

  const UncertMLNode * uncert = d->getUncertML();

  std::string xml = uncert->toXMLString();

  fail_unless( equals(expected, xml.c_str()) );

  delete document;
}
END_TEST


START_TEST (test_uncertml_read_statistics)
{
  char *filename = safe_strcat(TestDataDirectory, "statistics.xml");
  SBMLDocument *document = readSBMLFromFile(filename);
  free(filename);
  
  fail_unless ( document->getModel() != NULL );

  Parameter * p = document->getModel()->getParameter(0);
  DistribSBasePlugin * plug = 
    static_cast<DistribSBasePlugin*>(p->getPlugin("distrib"));

  const Uncertainty * u = plug->getUncertainty();

  const UncertMLNode * uncert = u->getUncertML();

  fail_unless (uncert->getElementName() == "StatisticsCollection");
  fail_unless (uncert->getNumAttributes() == 1);
  fail_unless (uncert->getAttributes().getName(0) == "definition");
  fail_unless (uncert->getAttributes().getValue(0) == 
                    "http://www.uncertml.org/statistics");
  fail_unless (uncert->getNumChildren() == 2);

  const UncertMLNode * uncert1 = uncert->getChild(0);

  fail_unless (uncert1->getElementName() == "Mean");
  fail_unless (uncert1->getNumAttributes() == 1);
  fail_unless (uncert1->getAttributes().getName(0) == "definition");
  fail_unless (uncert1->getAttributes().getValue(0) == 
                    "http://www.uncertml.org/statistics/mean");
  fail_unless (uncert1->getNumChildren() == 1);

  const UncertMLNode * uncert2 = uncert->getChild(1);

  fail_unless (uncert2->getElementName() == "Variance");
  fail_unless (uncert2->getNumAttributes() == 1);
  fail_unless (uncert2->getAttributes().getName(0) == "definition");
  fail_unless (uncert2->getAttributes().getValue(0) == 
                    "http://www.uncertml.org/statistics/variance");
  fail_unless (uncert2->getNumChildren() == 1);

  const UncertMLNode * child = uncert1->getChild(0);

  fail_unless (child->getElementName() == "value");
  fail_unless (child->getNumAttributes() == 0);
  fail_unless (child->getNumChildren() == 1);

  const UncertMLNode * child1 = child->getChild(0);

  fail_unless (child1->getElementName() == "var");
  fail_unless (child1->getNumAttributes() == 1);
  fail_unless (child1->getAttributes().getName(0) == "varId");
  fail_unless (child1->getAttributes().getValue(0) == "V_pop");
  fail_unless (child1->getNumChildren() == 0);

  child = uncert2->getChild(0);

  fail_unless (child->getElementName() == "value");
  fail_unless (child->getNumAttributes() == 0);
  fail_unless (child->getNumChildren() == 1);

  child1 = child->getChild(0);

  fail_unless (child1->getElementName() == "var");
  fail_unless (child1->getNumAttributes() == 1);
  fail_unless (child1->getAttributes().getName(0) == "varId");
  fail_unless (child1->getAttributes().getValue(0) == "V_omega");
  fail_unless (child1->getNumChildren() == 0);

  delete document;
}
END_TEST
 

START_TEST ( test_uncertml_write_statistics)
{
  const char *expected = 
    "<UncertML xmlns=\"http://www.uncertml.org/3.0\">\n"
    "  <StatisticsCollection definition=\"http://www.uncertml.org/statistics\">\n"
    "    <Mean definition=\"http://www.uncertml.org/statistics/mean\">\n"
    "      <value>\n"
    "        <var varId=\"V_pop\"/>\n"
    "      </value>\n"
    "    </Mean>\n"
    "    <Variance definition=\"http://www.uncertml.org/statistics/variance\">\n"
    "      <value>\n"
    "        <var varId=\"V_omega\"/>\n"
    "      </value>\n"
    "    </Variance>\n"
    "  </StatisticsCollection>\n"
    "</UncertML>";

  char *filename = safe_strcat(TestDataDirectory, "statistics.xml");
  SBMLDocument *document = readSBMLFromFile(filename);
  free(filename);
  
  fail_unless ( document->getModel() != NULL );

  Parameter * p = document->getModel()->getParameter(0);
  DistribSBasePlugin * plug = 
    static_cast<DistribSBasePlugin*>(p->getPlugin("distrib"));

  const Uncertainty * u = plug->getUncertainty();

  const UncertMLNode * uncert = u->getUncertML();

  std::string xml = uncert->toXMLString();

  fail_unless( equals(expected, xml.c_str()) );

  delete document;
}
END_TEST


Suite *
create_suite_TestUncertMLNodeParsing (void)
{ 
  TCase *tcase = tcase_create("UncertMLNodeParsing");
  Suite *suite = suite_create("UncertMLNodeParsing");
  
  tcase_add_test(tcase, test_uncertml_read_normalDistribution);
  tcase_add_test(tcase, test_uncertml_write_normalDistribution);
  tcase_add_test(tcase, test_uncertml_read_statistics);
  tcase_add_test(tcase, test_uncertml_write_statistics);

  suite_add_tcase(suite, tcase);

  return suite;
}


END_C_DECLS

