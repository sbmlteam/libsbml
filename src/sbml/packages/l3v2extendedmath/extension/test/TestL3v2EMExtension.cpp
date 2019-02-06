/**
 * @file    TestL3v2EMExtension.cpp
 * @brief   TestL3v2EMExtension unit tests
 * @author  Sarah M Keating
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
#include <sbml/packages/l3v2extendedmath/common/L3v2extendedmathExtensionTypes.h>
#include <sbml/extension/SBMLExtensionRegistry.h>
#include <sbml/SBMLTypeCodes.h>
#include <string>

/** @cond doxygenIgnored */

using namespace std;
LIBSBML_CPP_NAMESPACE_USE

/** @endcond doxygenIgnored */


CK_CPPSTART

static L3v2extendedmathExtension* G; 
static L3v2extendedmathPkgNamespaces* GNS;
static string L3V2EM_XMLNS_L3V1V1;
static string CORE_XMLNS_L2V4;
static string L3V2EM_PACKAGE_NAME;

void
L3v2EMExtensionTest_setup (void)
{
  try
  {
    G = new L3v2extendedmathExtension();
    GNS = new L3v2extendedmathPkgNamespaces();
    L3V2EM_PACKAGE_NAME = G->getName();
    L3V2EM_XMLNS_L3V1V1 = GNS->getURI();
    CORE_XMLNS_L2V4 = SBMLNamespaces::getSBMLNamespaceURI(2, 4);
  }
  catch(...)
  {
    fail("Failed to create a L3v2extendedmathExtension object");
  }
}


void
L3v2EMExtensionTest_teardown (void)
{
  delete G;
  delete GNS;
}


START_TEST (test_L3v2EMExtension_getName)
{
  fail_unless(G->getName() == "l3v2extendedmath");
  fail_unless(G->getName() == L3V2EM_PACKAGE_NAME);
}
END_TEST


START_TEST (test_L3v2EMExtension_getURI)
{
  fail_unless(G->getURI(3,1,1) == L3V2EM_XMLNS_L3V1V1);
  fail_unless(G->getURI(2,1,1) == "");
  fail_unless(G->getURI(4,1,1) == "");
}
END_TEST


START_TEST (test_L3v2EMExtension_getLevelVersion)
{
  fail_unless(G->getLevel(L3V2EM_XMLNS_L3V1V1) == 3);
  fail_unless(G->getLevel(CORE_XMLNS_L2V4)             == 0);
  fail_unless(G->getLevel("")                          == 0);

  fail_unless(G->getVersion(L3V2EM_XMLNS_L3V1V1) == 1);
  fail_unless(G->getVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(G->getVersion("")                          == 0);

  fail_unless(G->getPackageVersion(L3V2EM_XMLNS_L3V1V1) == 1);
  fail_unless(G->getPackageVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(G->getPackageVersion("")                          == 0);
}
END_TEST


START_TEST (test_L3v2EMExtension_getSBMLExtensionNamespaces)
{
  L3v2extendedmathPkgNamespaces *l3v2extendedmathns;
  l3v2extendedmathns = static_cast<L3v2extendedmathPkgNamespaces*>(G->getSBMLExtensionNamespaces(L3V2EM_XMLNS_L3V1V1));

  fail_unless(l3v2extendedmathns->getLevel()          == 3);
  fail_unless(l3v2extendedmathns->getVersion()        == 1);
  fail_unless(l3v2extendedmathns->getPackageVersion() == 1);

  delete l3v2extendedmathns;
  l3v2extendedmathns = static_cast<L3v2extendedmathPkgNamespaces*>(G->getSBMLExtensionNamespaces(""));

  fail_unless(l3v2extendedmathns == NULL);
}
END_TEST


START_TEST(test_L3v2EMExtension_copy)
{
  L3v2extendedmathExtension *g2 = new L3v2extendedmathExtension(*G);

  fail_unless(g2->getName() == "l3v2extendedmath");
  fail_unless(g2->getName() == L3V2EM_PACKAGE_NAME);

  fail_unless(g2->getURI(3,1,1) == L3V2EM_XMLNS_L3V1V1);
  fail_unless(g2->getURI(2,1,1) == "");
  fail_unless(g2->getURI(4,1,1) == "");

  fail_unless(g2->getLevel(L3V2EM_XMLNS_L3V1V1) == 3);
  fail_unless(g2->getLevel(CORE_XMLNS_L2V4)             == 0);
  fail_unless(g2->getLevel("")                          == 0);

  fail_unless(g2->getVersion(L3V2EM_XMLNS_L3V1V1) == 1);
  fail_unless(g2->getVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(g2->getVersion("")                          == 0);

  fail_unless(g2->getPackageVersion(L3V2EM_XMLNS_L3V1V1) == 1);
  fail_unless(g2->getPackageVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(g2->getPackageVersion("")                          == 0);

  delete g2;
}
END_TEST


START_TEST(test_L3v2EMExtension_assignment)
{
  L3v2extendedmathExtension* g2 = new L3v2extendedmathExtension();

  (*g2) = (*G);

  fail_unless(g2->getName() == "l3v2extendedmath");
  fail_unless(g2->getName() == L3V2EM_PACKAGE_NAME);

  fail_unless(g2->getURI(3,1,1) == L3V2EM_XMLNS_L3V1V1);
  fail_unless(g2->getURI(2,1,1) == "");
  fail_unless(g2->getURI(4,1,1) == "");

  fail_unless(g2->getLevel(L3V2EM_XMLNS_L3V1V1) == 3);
  fail_unless(g2->getLevel(CORE_XMLNS_L2V4)             == 0);
  fail_unless(g2->getLevel("")                          == 0);

  fail_unless(g2->getVersion(L3V2EM_XMLNS_L3V1V1) == 1);
  fail_unless(g2->getVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(g2->getVersion("")                          == 0);

  fail_unless(g2->getPackageVersion(L3V2EM_XMLNS_L3V1V1) == 1);
  fail_unless(g2->getPackageVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(g2->getPackageVersion("")                          == 0);

  delete g2;
}
END_TEST


START_TEST(test_L3v2EMExtension_clone)
{
  L3v2extendedmathExtension* g2 = G->clone();

  fail_unless(g2->getName() == "l3v2extendedmath");
  fail_unless(g2->getName() == L3V2EM_PACKAGE_NAME);

  fail_unless(g2->getURI(3,1,1) == L3V2EM_XMLNS_L3V1V1);
  fail_unless(g2->getURI(2,1,1) == "");
  fail_unless(g2->getURI(4,1,1) == "");

  fail_unless(g2->getLevel(L3V2EM_XMLNS_L3V1V1) == 3);
  fail_unless(g2->getLevel(CORE_XMLNS_L2V4)             == 0);
  fail_unless(g2->getLevel("")                          == 0);

  fail_unless(g2->getVersion(L3V2EM_XMLNS_L3V1V1) == 1);
  fail_unless(g2->getVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(g2->getVersion("")                          == 0);

  fail_unless(g2->getPackageVersion(L3V2EM_XMLNS_L3V1V1) == 1);
  fail_unless(g2->getPackageVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(g2->getPackageVersion("")                          == 0);

  delete g2;
}
END_TEST


START_TEST(test_L3v2EMExtension_registry)
{
  const SBMLExtension* sbext = SBMLExtensionRegistry::getInstance().getExtension("l3v2extendedmath");

  fail_unless(sbext != NULL);

  fail_unless(sbext->getName() == "l3v2extendedmath");
  fail_unless(sbext->getName() == L3V2EM_PACKAGE_NAME);

  fail_unless(sbext->getURI(3,1,1) == L3V2EM_XMLNS_L3V1V1);
  fail_unless(sbext->getURI(2,1,1) == "");
  fail_unless(sbext->getURI(4,1,1) == "");

  fail_unless(sbext->getLevel(L3V2EM_XMLNS_L3V1V1) == 3);
  fail_unless(sbext->getLevel(CORE_XMLNS_L2V4)             == 0);
  fail_unless(sbext->getLevel("")                          == 0);

  fail_unless(sbext->getVersion(L3V2EM_XMLNS_L3V1V1) == 1);
  fail_unless(sbext->getVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(sbext->getVersion("")                          == 0);

  fail_unless(sbext->getPackageVersion(L3V2EM_XMLNS_L3V1V1) == 1);
  fail_unless(sbext->getPackageVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(sbext->getPackageVersion("")                          == 0);

  delete sbext;
}
END_TEST


Suite *
create_suite_L3v2EMExtension (void)
{
  Suite *suite = suite_create("L3v2EMExtension");
  TCase *tcase = tcase_create("L3v2EMExtension");

  tcase_add_checked_fixture(tcase, L3v2EMExtensionTest_setup, L3v2EMExtensionTest_teardown);
 
  tcase_add_test( tcase, test_L3v2EMExtension_getName         );
  tcase_add_test( tcase, test_L3v2EMExtension_getURI          );
  tcase_add_test( tcase, test_L3v2EMExtension_getLevelVersion );
  tcase_add_test( tcase, test_L3v2EMExtension_getSBMLExtensionNamespaces);
  tcase_add_test( tcase, test_L3v2EMExtension_copy            );
  tcase_add_test( tcase, test_L3v2EMExtension_assignment      );
  tcase_add_test( tcase, test_L3v2EMExtension_clone           );
  tcase_add_test( tcase, test_L3v2EMExtension_registry        );

  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND
