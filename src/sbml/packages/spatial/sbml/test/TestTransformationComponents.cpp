/**
 * @file    TestTransformationComponents.cpp
 * @brief   TestTransformationComponents unit tests
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

static TransformationComponents* G; 
static SpatialPkgNamespaces* GNS;
static char*    S;

void
TransformationComponentsTest_setup (void)
{
  GNS = new SpatialPkgNamespaces();
  G = new TransformationComponents(GNS);
  S = NULL;
  
  if (G == NULL)
  {
    fail("Failed to create a TransformationComponents object");
  }
}


void
TransformationComponentsTest_teardown (void)
{
  delete G;
  delete GNS;
  free (S);
}


START_TEST (test_TransformationComponents_create)
{
  fail_unless(G->isSetComponents() == false);
  fail_unless(G->isSetComponentsLength() == false);
}
END_TEST


START_TEST (test_TransformationComponents_componentsLength)
{
  fail_unless(G->isSetComponents() == false);
  fail_unless(G->isSetComponentsLength() == false);

  G->setComponentsLength(3);

  fail_unless(G->isSetComponents() == false);
  fail_unless(G->isSetComponentsLength() == true);

  fail_unless(G->getComponentsLength() == 3);

  /* here we have not set the components so dont crash */
  double components [1] = {0.0};
  G->getComponents(components);
  fail_unless(util_isEqual(components[0], 0.0));

  G->unsetComponentsLength();

  fail_unless(G->isSetComponents() == false);
  fail_unless(G->isSetComponentsLength() == false);

  fail_unless(G->getComponentsLength() == SBML_INT_MAX);
}
END_TEST


START_TEST (test_TransformationComponents_components)
{
  fail_unless(G->isSetComponents() == false);
  fail_unless(G->isSetComponentsLength() == false);

  double components [] = {1.3,2.4};
  G->setComponents(components, 2);

  fail_unless(G->isSetComponents() == true);
  fail_unless(G->isSetComponentsLength() == true);

  fail_unless(G->getComponentsLength() == 2);

  double componentsRet [2];
  G->getComponents(componentsRet);
  fail_unless(util_isEqual(componentsRet[0], 1.3));
  fail_unless(util_isEqual(componentsRet[1], 2.4));

  G->unsetComponents();

  fail_unless(G->isSetComponents() == false);
  fail_unless(G->isSetComponentsLength() == false);
}
END_TEST

#if (0)
START_TEST (test_TransformationComponents_components_mismatchLength_1)
{
  fail_unless(G->isSetComponents() == false);
  fail_unless(G->isSetComponentsLength() == false);

  double components [] = {1.3,2.4};
  G->setComponents(components, 3);

  fail_unless(G->isSetComponents() == true);
  fail_unless(G->isSetComponentsLength() == true);

  fail_unless(G->getComponentsLength() == 3);

  double componentsRet [] = {0, 0, 0};
  G->getComponents(componentsRet);
  fail_unless(util_isEqual(componentsRet[0], 1.3));
  fail_unless(util_isEqual(componentsRet[1], 2.4));
  fail_unless(componentsRet[2] != 0);

  G->unsetComponents();

  fail_unless(G->isSetComponents() == false);
  fail_unless(G->isSetComponentsLength() == false);
}
END_TEST


START_TEST (test_TransformationComponents_components_mismatchLength_2)
{
  fail_unless(G->isSetComponents() == false);
  fail_unless(G->isSetComponentsLength() == false);

  double components [] = {1.3,2.4};
  G->setComponents(components, 1);

  fail_unless(G->isSetComponents() == true);
  fail_unless(G->isSetComponentsLength() == true);

  fail_unless(G->getComponentsLength() == 1);

  double componentsRet [1];
  G->getComponents(componentsRet);
  fail_unless(util_isEqual(componentsRet[0], 1.3));
  // really just making sure we dont crash
  fail_unless(util_isEqual(componentsRet[1], 2.4) == 0);
  fail_unless(componentsRet[2] != 0);

  G->unsetComponents();

  fail_unless(G->isSetComponents() == false);
  fail_unless(G->isSetComponentsLength() == false);
}
END_TEST
#endif

START_TEST (test_TransformationComponents_output)
{
  const char *expected = "<transformationComponents componentsLength=\"3\">1.3 2.5 3.7 </transformationComponents>";

  double points [] = {1.3,2.5,3.7};
  G->setComponents(points, 3);

  S = G->toSBML();

  fail_unless( equals(expected, S) );
}
END_TEST


Suite *
create_suite_TransformationComponents (void)
{
  Suite *suite = suite_create("TransformationComponents");
  TCase *tcase = tcase_create("TransformationComponents");

  tcase_add_checked_fixture(tcase, TransformationComponentsTest_setup, TransformationComponentsTest_teardown);
 
  tcase_add_test( tcase, test_TransformationComponents_create         );

  tcase_add_test( tcase, test_TransformationComponents_componentsLength   );
  tcase_add_test( tcase, test_TransformationComponents_components         );
  //tcase_add_test( tcase, test_TransformationComponents_components_mismatchLength_1  );
  //tcase_add_test( tcase, test_TransformationComponents_components_mismatchLength_2  );
  tcase_add_test( tcase, test_TransformationComponents_output         );

  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND
