/**
 * @file    TestDistribExtension.cpp
 * @brief   TestDistribExtension unit tests
 * @author  Sarah Keating
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
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
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 * 3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 * Pasadena, CA, USA
 *
 * Copyright (C) 2002-2005 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. Japan Science and Technology Agency, Japan
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation. A copy of the license agreement is provided in the
 * file named "LICENSE.txt" included with this software distribution and also
 * available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 */

#include <limits>

#include <iostream>
#include <check.h>
#include <sbml/conversion/ConversionProperties.h>
#include <sbml/packages/distrib/common/DistribExtensionTypes.h>
#include <sbml/extension/SBMLExtensionRegistry.h>
#include <sbml/SBMLTypeCodes.h>
#include <sbml/SBMLReader.h>
#include <string>

/** @cond doxygenIgnored */

using namespace std;
LIBSBML_CPP_NAMESPACE_USE

/** @endcond doxygenIgnored */


CK_CPPSTART
extern char *TestDataDirectory;

static DistribExtension* G; 
static DistribPkgNamespaces* GNS;
static string DISTRIB_XMLNS_L3V1V1;
static string CORE_XMLNS_L2V4;
static string DISTRIB_PACKAGE_NAME;

void
DistribExtensionTest_setup (void)
{
  try
  {
    G = new DistribExtension();
    GNS = new DistribPkgNamespaces();
    DISTRIB_PACKAGE_NAME = G->getName();
    DISTRIB_XMLNS_L3V1V1 = GNS->getURI();
    DISTRIB_XMLNS_L3V1V1 = G->getURI(3,1,1);
    CORE_XMLNS_L2V4 = SBMLNamespaces::getSBMLNamespaceURI(2, 4);
  }
  catch(...)
  {
    fail("Failed to create a DistribExtension object");
  }
}


void
DistribExtensionTest_teardown (void)
{
  delete G;
  delete GNS;
}


START_TEST (test_DistribExtension_getName)
{
  fail_unless(G->getName() == "distrib");
  fail_unless(G->getName() == DISTRIB_PACKAGE_NAME);
}
END_TEST


START_TEST (test_DistribExtension_getURI)
{
  fail_unless(G->getURI(3,1,1) == DISTRIB_XMLNS_L3V1V1);
  fail_unless(G->getURI(3, 2, 1) == DISTRIB_XMLNS_L3V1V1);
  fail_unless(G->getURI(2,1,1) == "");
  fail_unless(G->getURI(4,1,1) == "");
}
END_TEST


START_TEST (test_DistribExtension_getLevelVersion)
{
  fail_unless(G->getLevel(DISTRIB_XMLNS_L3V1V1) == 3);
  fail_unless(G->getLevel(CORE_XMLNS_L2V4)             == 0);
  fail_unless(G->getLevel("")                          == 0);

  fail_unless(G->getVersion(DISTRIB_XMLNS_L3V1V1) == 1);
  fail_unless(G->getVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(G->getVersion("")                          == 0);

  fail_unless(G->getPackageVersion(DISTRIB_XMLNS_L3V1V1) == 1);
  fail_unless(G->getPackageVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(G->getPackageVersion("")                          == 0);
}
END_TEST


START_TEST (test_DistribExtension_getSBMLExtensionNamespaces)
{
  DistribPkgNamespaces *Distribns;
  Distribns = static_cast<DistribPkgNamespaces*>(G->getSBMLExtensionNamespaces(DISTRIB_XMLNS_L3V1V1));

  fail_unless(Distribns->getLevel()          == 3);
  fail_unless(Distribns->getVersion()        == 1);
  fail_unless(Distribns->getPackageVersion() == 1);

  delete Distribns;
  Distribns = static_cast<DistribPkgNamespaces*>(G->getSBMLExtensionNamespaces(""));

  fail_unless(Distribns == NULL);
}
END_TEST


START_TEST(test_DistribExtension_copy)
{
  DistribExtension *g2 = new DistribExtension(*G);

  fail_unless(g2->getName() == "distrib");
  fail_unless(g2->getName() == DISTRIB_PACKAGE_NAME);

  fail_unless(g2->getURI(3,1,1) == DISTRIB_XMLNS_L3V1V1);
  fail_unless(g2->getURI(3, 2, 1) == DISTRIB_XMLNS_L3V1V1);
  fail_unless(g2->getURI(2,1,1) == "");
  fail_unless(g2->getURI(4,1,1) == "");

  fail_unless(g2->getLevel(DISTRIB_XMLNS_L3V1V1) == 3);
  fail_unless(g2->getLevel(CORE_XMLNS_L2V4)             == 0);
  fail_unless(g2->getLevel("")                          == 0);

  fail_unless(g2->getVersion(DISTRIB_XMLNS_L3V1V1) == 1);
  fail_unless(g2->getVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(g2->getVersion("")                          == 0);

  fail_unless(g2->getPackageVersion(DISTRIB_XMLNS_L3V1V1) == 1);
  fail_unless(g2->getPackageVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(g2->getPackageVersion("")                          == 0);

  delete g2;
}
END_TEST


START_TEST(test_DistribExtension_assignment)
{
  DistribExtension* g2 = new DistribExtension();

  (*g2) = (*G);

  fail_unless(g2->getName() == "distrib");
  fail_unless(g2->getName() == DISTRIB_PACKAGE_NAME);

  fail_unless(g2->getURI(3, 1, 1) == DISTRIB_XMLNS_L3V1V1);
  fail_unless(g2->getURI(3, 2, 1) == DISTRIB_XMLNS_L3V1V1);
  fail_unless(g2->getURI(2, 1, 1) == "");
  fail_unless(g2->getURI(4, 1, 1) == "");

  fail_unless(g2->getLevel(DISTRIB_XMLNS_L3V1V1) == 3);
  fail_unless(g2->getLevel(CORE_XMLNS_L2V4) == 0);
  fail_unless(g2->getLevel("") == 0);

  fail_unless(g2->getVersion(DISTRIB_XMLNS_L3V1V1) == 1);
  fail_unless(g2->getVersion(CORE_XMLNS_L2V4) == 0);
  fail_unless(g2->getVersion("") == 0);

  fail_unless(g2->getPackageVersion(DISTRIB_XMLNS_L3V1V1) == 1);
  fail_unless(g2->getPackageVersion(CORE_XMLNS_L2V4) == 0);
  fail_unless(g2->getPackageVersion("") == 0);

  delete g2;
}
END_TEST


START_TEST(test_DistribExtension_clone)
{
  DistribExtension* g2 = G->clone();

  fail_unless(g2->getName() == "distrib");
  fail_unless(g2->getName() == DISTRIB_PACKAGE_NAME);

  fail_unless(g2->getURI(3, 1, 1) == DISTRIB_XMLNS_L3V1V1);
  fail_unless(g2->getURI(3, 2, 1) == DISTRIB_XMLNS_L3V1V1);
  fail_unless(g2->getURI(2, 1, 1) == "");
  fail_unless(g2->getURI(4, 1, 1) == "");

  fail_unless(g2->getLevel(DISTRIB_XMLNS_L3V1V1) == 3);
  fail_unless(g2->getLevel(CORE_XMLNS_L2V4) == 0);
  fail_unless(g2->getLevel("") == 0);

  fail_unless(g2->getVersion(DISTRIB_XMLNS_L3V1V1) == 1);
  fail_unless(g2->getVersion(CORE_XMLNS_L2V4) == 0);
  fail_unless(g2->getVersion("") == 0);

  fail_unless(g2->getPackageVersion(DISTRIB_XMLNS_L3V1V1) == 1);
  fail_unless(g2->getPackageVersion(CORE_XMLNS_L2V4) == 0);
  fail_unless(g2->getPackageVersion("") == 0);

  delete g2;
}
END_TEST


START_TEST(test_DistribExtension_registry)
{
  const SBMLExtension* sbext = SBMLExtensionRegistry::getInstance().getExtension("distrib");

  fail_unless(sbext != NULL);

  fail_unless(sbext->getName() == "distrib");
  fail_unless(sbext->getName() == DISTRIB_PACKAGE_NAME);

  fail_unless(sbext->getURI(3,1,1) == DISTRIB_XMLNS_L3V1V1);
  fail_unless(sbext->getURI(3, 2, 1) == DISTRIB_XMLNS_L3V1V1);
  fail_unless(sbext->getURI(2,1,1) == "");
  fail_unless(sbext->getURI(4,1,1) == "");

  fail_unless(sbext->getLevel(DISTRIB_XMLNS_L3V1V1) == 3);
  fail_unless(sbext->getLevel(CORE_XMLNS_L2V4)             == 0);
  fail_unless(sbext->getLevel("")                          == 0);

  fail_unless(sbext->getVersion(DISTRIB_XMLNS_L3V1V1) == 1);
  fail_unless(sbext->getVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(sbext->getVersion("")                          == 0);

  fail_unless(sbext->getPackageVersion(DISTRIB_XMLNS_L3V1V1) == 1);
  fail_unless(sbext->getPackageVersion(CORE_XMLNS_L2V4)             == 0);
  fail_unless(sbext->getPackageVersion("")                          == 0);

  delete sbext;
}
END_TEST


START_TEST(test_DistribExtension_typecode)
{
  const SBMLExtension* sbext = SBMLExtensionRegistry::getInstance().getExtension("distrib");

  fail_unless(sbext != NULL);
  fail_unless(strcmp(sbext->getStringFromTypeCode(SBML_DISTRIB_UNCERTPARAMETER), "UncertParameter") == 0);
  fail_unless(strcmp(sbext->getStringFromTypeCode(SBML_DISTRIB_UNCERTAINTY), "Uncertainty") == 0);
  fail_unless(strcmp(sbext->getStringFromTypeCode(SBML_DISTRIB_UNCERTSTATISTICSPAN), "UncertSpan") == 0);
  fail_unless(strcmp(sbext->getStringFromTypeCode(SBML_DISTRIB_DISTRIBBASE), "DistribBase") == 0); //39  /*!<DistribUncertStatisticSpan */
  fail_unless(strcmp(sbext->getStringFromTypeCode(SBML_DISTRIB_DISTRIBBASE +1), "(Unknown SBML Distrib Type)") == 0);

  delete sbext;
}
END_TEST

START_TEST(test_DistribExtension_SBMLtypecode)
{	
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_DISTRIB_UNCERTPARAMETER, "distrib"), "UncertParameter") == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_DISTRIB_UNCERTAINTY, "distrib"), "Uncertainty") == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_DISTRIB_UNCERTSTATISTICSPAN, "distrib"), "UncertSpan") == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_DISTRIB_DISTRIBBASE, "distrib"), "DistribBase") == 0);
  fail_unless(strcmp(SBMLTypeCode_toString(SBML_DISTRIB_DISTRIBBASE + 1, "distrib"), "(Unknown SBML Distrib Type)") == 0);
}
END_TEST

START_TEST(test_DistribExtension_enablePackage)
{	
  SBMLDocument doc(3, 1);
  doc.createModel();
  DistribExtension de;
  string uri = de.getURI(doc.getLevel(), doc.getVersion(), 1);
  doc.enablePackage(uri, "distrib", true);
  //No fail_unless:  we're just making sure the above doesn't infinitely recurse.
}
END_TEST

Suite *
create_suite_DistribExtension (void)
{
  Suite *suite = suite_create("DistribExtension");
  TCase *tcase = tcase_create("DistribExtension");

  tcase_add_checked_fixture(tcase, DistribExtensionTest_setup, DistribExtensionTest_teardown);
 
  tcase_add_test( tcase, test_DistribExtension_getName         );
  tcase_add_test( tcase, test_DistribExtension_getURI          );
  tcase_add_test( tcase, test_DistribExtension_getLevelVersion );
  tcase_add_test( tcase, test_DistribExtension_getSBMLExtensionNamespaces);
  tcase_add_test( tcase, test_DistribExtension_copy            );
  tcase_add_test( tcase, test_DistribExtension_assignment      );
  tcase_add_test( tcase, test_DistribExtension_clone           );
  tcase_add_test( tcase, test_DistribExtension_registry        );
  tcase_add_test( tcase, test_DistribExtension_typecode        );
  tcase_add_test( tcase, test_DistribExtension_SBMLtypecode    );
  tcase_add_test( tcase, test_DistribExtension_enablePackage    );

  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND
