/**
 * @file    TestParametricObject.cpp
 * @brief   TestParametricObject unit tests
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

static ParametricObject* G; 
static SpatialPkgNamespaces* GNS;
static char*    S;

void
ParametricObjectTest_setup (void)
{
  GNS = new SpatialPkgNamespaces();
  G = new ParametricObject(GNS);
  S = NULL;
  
  if (G == NULL)
  {
    fail("Failed to create a ParametricObject object");
  }
}


void
ParametricObjectTest_teardown (void)
{
  delete G;
  delete GNS;
  free (S);
}


START_TEST (test_ParametricObject_create)
{
  fail_unless(G->isSetId() == false);
  fail_unless(G->isSetPolygonType() == false);
  fail_unless(G->isSetDomainType() == false);
  fail_unless(G->isSetPointIndex() == false);
}
END_TEST


START_TEST (test_ParametricObject_id)
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


START_TEST (test_ParametricObject_polygonType)
{
  fail_unless(G->isSetPolygonType() == false);

  fail_unless(G->setPolygonType("triangle") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(G->isSetPolygonType() == true);
  fail_unless(G->getPolygonType() == SPATIAL_POLYGONKIND_TRIANGLE);

  fail_unless(G->unsetPolygonType() == LIBSBML_OPERATION_SUCCESS);
  fail_unless(G->isSetPolygonType() == false);
}
END_TEST


START_TEST (test_ParametricObject_domain)
{
  fail_unless(G->isSetDomainType() == false);

  fail_unless(G->setDomainType("i1") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(G->isSetDomainType() == true);
  fail_unless(G->getDomainType() == "i1");

  fail_unless(G->unsetDomainType() == LIBSBML_OPERATION_SUCCESS);
  fail_unless(G->isSetDomainType() == false);
}
END_TEST



START_TEST (test_ParametricObject_output)
{
  const char *expected = 
    "<parametricObject id=\"i\" polygonType=\"triangle\" domainType=\"p\" pointIndexLength=\"3\">1 2 3 </parametricObject>";


  int points [] = {1,2,3};

  G->setId("i");
  G->setPolygonType("triangle");
  G->setDomainType("p");
  G->setPointIndexLength(3);
  
  G->setPointIndex(points, 3);  

  S = G->toSBML();

  fail_unless( equals(expected, S) );

}
END_TEST


START_TEST (test_ParametricObject_compress)
{
  string expectedCompressed = 
    "<parametricObject id=\"i\" polygonType=\"triangle\" domainType=\"p\" pointIndexLength=\"19\" compression=\"deflated\">120 218 51 84 48 82 48 6 99 67 5 83 48 13 0 27 216 2 231 </parametricObject>";

  string expectedUncompressed = 
    "<parametricObject id=\"i\" polygonType=\"triangle\" domainType=\"p\" pointIndexLength=\"9\" compression=\"uncompressed\">1 2 3 2 3 1 5 2 3 </parametricObject>";

  int points [] = {1,2,3,2,3,1,5,2,3};
  //int points[] = { 0, 1, 2};
  G->setId("i");
  G->setPolygonType("triangle");
  G->setDomainType("p");
  G->setCompression("uncompressed");

  G->setPointIndex(points, 9);  

  fail_unless(G->getPointIndexLength() == 9);
  fail_unless(G->getPointIndex() == "1 2 3 2 3 1 5 2 3 ");

  int ret = G->compress(9);
  S = G->toSBML();
#ifdef USE_ZLIB
  fail_unless(ret == LIBSBML_OPERATION_SUCCESS);
  fail_unless( expectedCompressed == S );
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


START_TEST(test_ParametricObject_uncompressInternal)
{
  int points[] = { 1,2,3,2,3,1,5,2,3 };
  //int points[] = { 0, 1, 2};
  G->setId("i");
  G->setPolygonType("triangle");
  G->setDomainType("p");

  G->setPointIndex(points, 9);

  G->compress(9);

  size_t len = G->getUncompressedLength();
  fail_unless(len == 9);
  int* result = NULL;
  G->getUncompressedData(result, len);

  for (size_t i = 0; i < len; i++)
  {
    fail_unless(result[i] == points[i]);
  }
  free(result);

}
END_TEST


Suite *
create_suite_ParametricObject (void)
{
  Suite *suite = suite_create("ParametricObject");
  TCase *tcase = tcase_create("ParametricObject");

  tcase_add_checked_fixture(tcase, ParametricObjectTest_setup, ParametricObjectTest_teardown);
 
  tcase_add_test( tcase, test_ParametricObject_create      );

  tcase_add_test( tcase, test_ParametricObject_id          );
  tcase_add_test( tcase, test_ParametricObject_polygonType );
  tcase_add_test( tcase, test_ParametricObject_domain      );
  tcase_add_test( tcase, test_ParametricObject_output      );
  tcase_add_test( tcase, test_ParametricObject_compress    );
  tcase_add_test( tcase, test_ParametricObject_uncompressInternal    );

  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND
