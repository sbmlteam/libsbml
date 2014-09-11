/**
 * @file    TestSampledField.cpp
 * @brief   TestSampledField unit tests
 * @author  Sarah Keating
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2014 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
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

#include <limits>

#include <iostream>
#include <check.h>
#include <sbml/common/extern.h>
#include <sbml/packages/spatial/common/SpatialExtensionTypes.h>
#include <sbml/extension/SBMLExtensionRegistry.h>
#include <sbml/SBMLTypeCodes.h>
#include <string>

/** @cond doxygenIgnored */

using namespace std;
LIBSBML_CPP_NAMESPACE_USE

/** @endcond doxygenIgnored */

static bool
equals (const char* expected, const char* actual)
{
  if ( !strcmp(expected, actual) ) return true;

  printf( "\nStrings are not equal:\n"  );
  printf( "Expected:\n[%s]\n", expected );
  printf( "Actual:\n[%s]\n"  , actual   );

  return false;
}


CK_CPPSTART

static SampledField* G; 
static SpatialPkgNamespaces* GNS;
static char*    S;

void
SampledFieldTest_setup (void)
{
  GNS = new SpatialPkgNamespaces();
  G = new SampledField(GNS);
  S = NULL;
  
  if (G == NULL)
  {
    fail("Failed to create a SampledField object");
  }
}


void
SampledFieldTest_teardown (void)
{
  delete G;
  delete GNS;
  free (S);
}


START_TEST (test_SampledField_create)
{
  fail_unless(G->isSetSamples() == false);
  fail_unless(G->isSetSamplesLength() == false);
  fail_unless(G->isSetDataType() == false);
}
END_TEST


START_TEST (test_SampledField_samplesLength)
{
  fail_unless(G->isSetSamples() == false);
  fail_unless(G->isSetSamplesLength() == false);

  G->setSamplesLength(3);

  fail_unless(G->isSetSamples() == false);
  fail_unless(G->isSetSamplesLength() == true);

  fail_unless(G->getSamplesLength() == 3);

  /* here we have not get the samples so dont crash */
  int samples [1] = {0};
  G->getSamples(samples);
  fail_unless(samples[0] == 0);

  G->unsetSamplesLength();

  fail_unless(G->isSetSamples() == false);
  fail_unless(G->isSetSamplesLength() == false);

  fail_unless(G->getSamplesLength() == SBML_INT_MAX);
}
END_TEST


START_TEST (test_SampledField_samples)
{
  fail_unless(G->isSetSamples() == false);
  fail_unless(G->isSetSamplesLength() == false);

  int samples [] = {1,2};
  G->setSamples(samples, 2);

  fail_unless(G->isSetSamples() == true);
  fail_unless(G->isSetSamplesLength() == true);

  fail_unless(G->getSamplesLength() == 2);

  int samplesRet [2];
  G->getSamples(samplesRet);
  fail_unless(samplesRet[0] == 1);
  fail_unless(samplesRet[1] == 2);

  G->unsetSamples();

  fail_unless(G->isSetSamples() == false);
  fail_unless(G->isSetSamplesLength() == false);
}
END_TEST

#if (0)
START_TEST (test_SampledField_samples_mismatchLength_1)
{
  fail_unless(G->isSetSamples() == false);
  fail_unless(G->isSetSamplesLength() == false);

  int samples [] = {1,2};
  G->setSamples(samples, 3);

  fail_unless(G->isSetSamples() == true);
  fail_unless(G->isSetSamplesLength() == true);

  fail_unless(G->getSamplesLength() == 3);

  int samplesRet [] = {0, 0, 0};
  G->getSamples(samplesRet);
  fail_unless(samplesRet[0] == 1);
  fail_unless(samplesRet[1] == 2);
  fail_unless(samplesRet[2] != 0);

  G->unsetSamples();

  fail_unless(G->isSetSamples() == false);
  fail_unless(G->isSetSamplesLength() == false);
}
END_TEST


START_TEST (test_SampledField_samples_mismatchLength_2)
{
  fail_unless(G->isSetSamples() == false);
  fail_unless(G->isSetSamplesLength() == false);

  int samples [] = {1,2};
  G->setSamples(samples, 1);

  fail_unless(G->isSetSamples() == true);
  fail_unless(G->isSetSamplesLength() == true);

  fail_unless(G->getSamplesLength() == 1);

  int samplesRet [1];
  G->getSamples(samplesRet);
  fail_unless(samplesRet[0] == 1);
  // really just making sure we dont crash
  fail_unless(samplesRet[1] != 2);
  fail_unless(samplesRet[2] != 0);

  G->unsetSamples();

  fail_unless(G->isSetSamples() == false);
  fail_unless(G->isSetSamplesLength() == false);
}
END_TEST
#endif

START_TEST (test_SampledField_dataType)
{
  fail_unless(G->isSetDataType() == false);

  G->setDataType("uint8");

  fail_unless(G->isSetDataType() == true);

  fail_unless(G->getDataType() == SPATIAL_DATAKIND_UINT8);

  G->unsetDataType();

  fail_unless(G->isSetDataType() == false);
}
END_TEST


START_TEST (test_SampledField_output)
{
  const char *expected = "<sampledField dataType=\"uint8\" samplesLength=\"3\">1 2 3 </sampledField>";

  int points [] = {1,2,3};
  G->setSamples(points, 3);
  G->setDataType("uint8");

  S = G->toSBML();

  fail_unless( equals(expected, S) );
}
END_TEST


Suite *
create_suite_SampledField (void)
{
  Suite *suite = suite_create("SampledField");
  TCase *tcase = tcase_create("SampledField");

  tcase_add_checked_fixture(tcase, SampledFieldTest_setup, SampledFieldTest_teardown);
 
  tcase_add_test( tcase, test_SampledField_create         );

  tcase_add_test( tcase, test_SampledField_samplesLength   );
  tcase_add_test( tcase, test_SampledField_samples         );
  //tcase_add_test( tcase, test_SampledField_samples_mismatchLength_1  );
  //tcase_add_test( tcase, test_SampledField_samples_mismatchLength_2  );
  tcase_add_test( tcase, test_SampledField_dataType        );
  tcase_add_test( tcase, test_SampledField_output         );

  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND
