/**
 * @file    TestPolygonObject.cpp
 * @brief   TestPolygonObject unit tests
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

static PolygonObject* G; 
static SpatialPkgNamespaces* GNS;
static char*    S;

void
PolygonObjectTest_setup (void)
{
  GNS = new SpatialPkgNamespaces();
  G = new PolygonObject(GNS);
  S = NULL;
  
  if (G == NULL)
  {
    fail("Failed to create a PolygonObject object");
  }
}


void
PolygonObjectTest_teardown (void)
{
  delete G;
  delete GNS;
  free (S);
}


START_TEST (test_PolygonObject_create)
{
  fail_unless(G->isSetPointIndex() == false);

}
END_TEST


START_TEST (test_PolygonObject_pointIndices)
{
  fail_unless(G->isSetPointIndex() == false);

  int points [] = {1,2,3};
  G->setPointIndex(points, 3);

  fail_unless(G->isSetPointIndex() == true);

  fail_unless(G->getPointIndexLength() == 3);


}
END_TEST


START_TEST (test_PolygonObject_output)
{
  const char *expected = "<polygonObject pointIndexLength=\"3\">1 2 3 </polygonObject>";

  int points [] = {1,2,3};
  G->setPointIndex(points, 3);

  S = G->toSBML();

  fail_unless( equals(expected, S) );
}
END_TEST


Suite *
create_suite_PolygonObject (void)
{
  Suite *suite = suite_create("PolygonObject");
  TCase *tcase = tcase_create("PolygonObject");

  tcase_add_checked_fixture(tcase, PolygonObjectTest_setup, PolygonObjectTest_teardown);
 
  tcase_add_test( tcase, test_PolygonObject_create         );

  tcase_add_test( tcase, test_PolygonObject_pointIndices   );
  tcase_add_test( tcase, test_PolygonObject_output         );

  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND
