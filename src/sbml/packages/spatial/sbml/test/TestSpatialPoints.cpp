/**
 * @file    TestSpatialPoints.cpp
 * @brief   TestSpatialPoints unit tests
 * @author  Sarah Keating
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

static SpatialPoints* G; 
static SpatialPkgNamespaces* GNS;
static char*    S;

void
SpatialPointsTest_setup (void)
{
  GNS = new SpatialPkgNamespaces();
  G = new SpatialPoints(GNS);
  S = NULL;
  
  if (G == NULL)
  {
    fail("Failed to create a SpatialPoints object");
  }
}


void
SpatialPointsTest_teardown (void)
{
  delete G;
  delete GNS;
  free (S);
}


START_TEST (test_SpatialPoints_create)
{
  fail_unless(G->isSetId() == false);
  fail_unless(G->isSetArrayData() == false);
  fail_unless(G->isSetArrayDataLength() == false);
  fail_unless(G->isSetCompression() == false);
  fail_unless(G->isSetDataType() == false);
}
END_TEST


START_TEST (test_SpatialPoints_id)
{
  fail_unless(G->isSetId() == false);

  fail_unless(G->setId("1") == LIBSBML_INVALID_ATTRIBUTE_VALUE);
  fail_unless(G->isSetId() == false);

  fail_unless(G->setId("i1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(G->isSetId() == true);
  fail_unless(G->getId() == "i1");

  fail_unless(G->unsetId() == LIBSBML_OPERATION_SUCCESS);
  fail_unless(G->isSetId() == false);
}
END_TEST


START_TEST (test_SpatialPoints_compression)
{
  fail_unless(G->isSetCompression() == false);

  fail_unless(G->setCompression("deflated") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(G->isSetCompression() == true);
  fail_unless(G->getCompression() == SPATIAL_COMPRESSIONKIND_DEFLATED);

  fail_unless(G->unsetCompression() == LIBSBML_OPERATION_SUCCESS);
  fail_unless(G->isSetCompression() == false);
}
END_TEST


START_TEST (test_SpatialPoints_dataType)
{
  fail_unless(G->isSetDataType() == false);

  fail_unless(G->setDataType("int") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(G->isSetDataType() == true);
  fail_unless(G->getDataType() == SPATIAL_DATAKIND_INT);

  fail_unless(G->unsetDataType() == LIBSBML_OPERATION_SUCCESS);
  fail_unless(G->isSetDataType() == false);
}
END_TEST



START_TEST (test_SpatialPoints_output)
{
  const char *expected = 
    "<spatialPoints id=\"i\" compression=\"uncompressed\" arrayDataLength=\"3\" dataType=\"double\">1 2 3 </spatialPoints>";


  double points [] = {1,2,3};

  G->setId("i");
  G->setCompression("uncompressed");
  G->setDataType("double");
  G->setArrayDataLength(3);
  
  G->setArrayData(points, 3);  

  S = G->toSBML();

  fail_unless( equals(expected, S) );

}
END_TEST


START_TEST (test_SpatialPoints_compress)
{
  string expectedCompressed = 
    "<spatialPoints id=\"i\" compression=\"deflated\" arrayDataLength=\"44\" dataType=\"double\">120 218 51 84 48 82 48 86 48 208 51 85 48 87 48 213 51 50 64 1 70 10 186 38 122 6 6 150 200 192 2 168 12 168 88 207 28 93 41 0 4 95 13 250 </spatialPoints>";

  string expectedUncompressed = 
    "<spatialPoints id=\"i\" compression=\"uncompressed\" arrayDataLength=\"9\" dataType=\"double\">1 2 3 0.5 7 5.2000000000000002 -4.0099999999999998 5.5 7.7000000000000002 </spatialPoints>";

  double points [] = {1,2,3, 0.5, 7, 5.2, -4.01, 5.5, 7.7};
//  double points[] = { 0, 0, 0, 0, 1, 0, 0, 1, 1, -1, -1, -1 };
  G->setId("i");
  G->setCompression("uncompressed");
  G->setDataType("double");
  G->setArrayDataLength(3);

  G->setArrayData(points, 9);  

  fail_unless(G->getArrayDataLength() == 9);
  fail_unless(G->getArrayData() == "1 2 3 0.5 7 5.2000000000000002 -4.0099999999999998 5.5 7.7000000000000002 ");

  int ret = G->compress(9);

  S = G->toSBML();

#ifdef USE_ZLIB
  fail_unless(ret == LIBSBML_OPERATION_SUCCESS);
  fail_unless(expectedCompressed == S);
#else
  fail_unless(ret == LIBSBML_OPERATION_FAILED);
  fail_unless(expectedUncompressed == S);
#endif
  free(S);
  G->uncompress();
  S = G->toSBML();
  fail_unless(expectedUncompressed == S);

}
END_TEST


START_TEST(test_SpatialPoints_uncompressInternal)
{
  double points [] = {1,2,3, 0.5, 7, 5.2, -4.01, 5.5, 7.7};
  G->setId("i");
  G->setCompression("uncompressed");
  G->setDataType("double");

  G->setArrayData(points, 9);  

  G->compress(9);

  size_t len = G->getUncompressedLength();
  fail_unless(len == 9);
  double* result = NULL;
  G->getUncompressedData(result, len);

  for (size_t i = 0; i < len; i++)
  {
    fail_unless(result[i] == points[i]);
  }
  free(result);

}
END_TEST


Suite *
create_suite_SpatialPoints (void)
{
  Suite *suite = suite_create("SpatialPoints");
  TCase *tcase = tcase_create("SpatialPoints");

  tcase_add_checked_fixture(tcase, SpatialPointsTest_setup, SpatialPointsTest_teardown);
 
  tcase_add_test( tcase, test_SpatialPoints_create      );

  tcase_add_test( tcase, test_SpatialPoints_id          );
  tcase_add_test( tcase, test_SpatialPoints_compression );
  tcase_add_test( tcase, test_SpatialPoints_dataType      );
  tcase_add_test( tcase, test_SpatialPoints_output      );
  tcase_add_test( tcase, test_SpatialPoints_compress    );
  tcase_add_test( tcase, test_SpatialPoints_uncompressInternal    );

  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND
