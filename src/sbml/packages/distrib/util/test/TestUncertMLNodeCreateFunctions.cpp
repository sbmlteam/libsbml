/**
 * \file    TestUncertMLCreateFunctions.cpp
 * \brief   Implementation of the Tests for the UncertML helper functions
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



START_TEST ( test_uncertml_create_normalDistribution)
{
  const char *expected = 
    "<UncertML xmlns=\"http://www.uncertml.org/3.0\">\n"
    "  <NormalDistribution definition=\"http://www.uncertml.org/distributions\">\n"
    "    <mean>\n"
    "      <var varId=\"mu\"/>\n"
    "    </mean>\n"
    "    <variance>\n"
    "      <var varId=\"sigma\"/>\n"
    "    </variance>\n"
    "  </NormalDistribution>\n"
    "</UncertML>";

  UncertMLNode * uncert = UncertMLNode::createDistributionNode(
    "NormalDistribution", "mean,variance", "mu,sigma");

  std::string xml = uncert->toXMLString();

  fail_unless( equals(expected, xml.c_str()) );
  
  delete uncert;
}
END_TEST


START_TEST ( test_uncertml_create_statistics)
{
  const char *expected = 
    "<UncertML xmlns=\"http://www.uncertml.org/3.0\">\n"
    "  <StatisticsCollection definition=\"http://www.uncertml.org/statistics\">\n"
    "    <Mean definition=\"http://www.uncertml.org/statistics\">\n"
    "      <value>\n"
    "        <var varId=\"V_pop\"/>\n"
    "      </value>\n"
    "    </Mean>\n"
    "    <Variance definition=\"http://www.uncertml.org/statistics\">\n"
    "      <value>\n"
    "        <var varId=\"V_omega\"/>\n"
    "      </value>\n"
    "    </Variance>\n"
    "  </StatisticsCollection>\n"
    "</UncertML>";

  UncertMLNode * uncert = UncertMLNode::createStatisticsNode(
    "Mean,Variance", "V_pop,V_omega");


  std::string xml = uncert->toXMLString();

  fail_unless( equals(expected, xml.c_str()) );
  
  delete uncert;
}
END_TEST


Suite *
create_suite_TestUncertMLNodeCreateFunctions (void)
{ 
  TCase *tcase = tcase_create("UncertMLNodeCreateFunctions");
  Suite *suite = suite_create("UncertMLNodeCreateFunctions");
  
  tcase_add_test(tcase, test_uncertml_create_normalDistribution);
  tcase_add_test(tcase, test_uncertml_create_statistics);

  suite_add_tcase(suite, tcase);

  return suite;
}


END_C_DECLS

