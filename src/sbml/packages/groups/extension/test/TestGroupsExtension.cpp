/**
 * @file    TestGroupsExtension.cpp
 * @brief   TestGroupsExtension unit tests
 * @author  Akiya Jouraku
 *
 * $Id: $
 * $HeadURL: $
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
#include <sbml/packages/groups/common/GroupsExtensionTypes.h>
#include <sbml/extension/SBMLExtensionRegistry.h>
#include <sbml/SBMLTypeCodes.h>
#include <string>

/** @cond doxygenIgnored */

using namespace std;
LIBSBML_CPP_NAMESPACE_USE

/** @endcond doxygenIgnored */


CK_CPPSTART

static GroupsExtension* G; 
static GroupsPkgNamespaces* GNS;
static string GROUPS_XMLNS_L3V1V1;
static string CORE_XMLNS_L2V4;
static string GROUPS_PACKAGE_NAME;

void
GroupsExtensionTest_setup (void)
{
  try
  {
    G = new GroupsExtension();
    GNS = new GroupsPkgNamespaces();
    GROUPS_PACKAGE_NAME = G->getName();
    GROUPS_XMLNS_L3V1V1 = GNS->getURI();
    CORE_XMLNS_L2V4 = SBMLNamespaces::getSBMLNamespaceURI(2, 4);
  }
  catch(...)
  {
    fail("Failed to create a GroupsExtension object");
  }
}


void
GroupsExtensionTest_teardown (void)
{
  delete G;
  delete GNS;
}


START_TEST (test_GroupsExtension_getName)
{
  fail_unless(G->getName() == "groups");
  fail_unless(G->getName() == GROUPS_PACKAGE_NAME);
}
END_TEST


START_TEST (test_GroupsExtension_getURI)
{
  fail_unless(G->getURI(3,1,1) == GROUPS_XMLNS_L3V1V1);
  fail_unless(G->getURI(2,1,1) == "");
  fail_unless(G->getURI(4,1,1) == "");
}
END_TEST


START_TEST (test_GroupsExtension_getLevelVersion)
{
  fail_unless(G->getLevel(GROUPS_XMLNS_L3V1V1) == 3);
  fail_unless(G->getLevel(CORE_XMLNS_L2V4)     == 0);
  fail_unless(G->getLevel("")                  == 0);

  fail_unless(G->getVersion(GROUPS_XMLNS_L3V1V1) == 1);
  fail_unless(G->getVersion(CORE_XMLNS_L2V4)     == 0);
  fail_unless(G->getVersion("")                  == 0);

  fail_unless(G->getPackageVersion(GROUPS_XMLNS_L3V1V1) == 1);
  fail_unless(G->getPackageVersion(CORE_XMLNS_L2V4)     == 0);
  fail_unless(G->getPackageVersion("")                  == 0);
}
END_TEST


START_TEST (test_GroupsExtension_getSBMLExtensionNamespaces)
{
  GroupsPkgNamespaces *groupsns;
  groupsns = static_cast<GroupsPkgNamespaces*>(G->getSBMLExtensionNamespaces(GROUPS_XMLNS_L3V1V1));

  fail_unless(groupsns->getLevel()          == 3);
  fail_unless(groupsns->getVersion()        == 1);
  fail_unless(groupsns->getPackageVersion() == 1);

  delete groupsns;

  groupsns = static_cast<GroupsPkgNamespaces*>(G->getSBMLExtensionNamespaces(""));

  fail_unless(groupsns == NULL);
}
END_TEST


START_TEST(test_GroupsExtension_copy)
{
  GroupsExtension *g2 = new GroupsExtension(*G);

  fail_unless(g2->getName() == "groups");
  fail_unless(g2->getName() == GROUPS_PACKAGE_NAME);

  fail_unless(g2->getURI(3,1,1) == GROUPS_XMLNS_L3V1V1);
  fail_unless(g2->getURI(2,1,1) == "");
  fail_unless(g2->getURI(4,1,1) == "");

  fail_unless(g2->getLevel(GROUPS_XMLNS_L3V1V1) == 3);
  fail_unless(g2->getLevel(CORE_XMLNS_L2V4)     == 0);
  fail_unless(g2->getLevel("")                  == 0);

  fail_unless(g2->getVersion(GROUPS_XMLNS_L3V1V1) == 1);
  fail_unless(g2->getVersion(CORE_XMLNS_L2V4)     == 0);
  fail_unless(g2->getVersion("")                  == 0);

  fail_unless(g2->getPackageVersion(GROUPS_XMLNS_L3V1V1) == 1);
  fail_unless(g2->getPackageVersion(CORE_XMLNS_L2V4)     == 0);
  fail_unless(g2->getPackageVersion("")                  == 0);

  delete g2;
}
END_TEST


START_TEST(test_GroupsExtension_assignment)
{
  GroupsExtension* g2 = new GroupsExtension();

  (*g2) = (*G);

  fail_unless(g2->getName() == "groups");
  fail_unless(g2->getName() == GROUPS_PACKAGE_NAME);

  fail_unless(g2->getURI(3,1,1) == GROUPS_XMLNS_L3V1V1);
  fail_unless(g2->getURI(2,1,1) == "");
  fail_unless(g2->getURI(4,1,1) == "");

  fail_unless(g2->getLevel(GROUPS_XMLNS_L3V1V1) == 3);
  fail_unless(g2->getLevel(CORE_XMLNS_L2V4)     == 0);
  fail_unless(g2->getLevel("")                  == 0);

  fail_unless(g2->getVersion(GROUPS_XMLNS_L3V1V1) == 1);
  fail_unless(g2->getVersion(CORE_XMLNS_L2V4)     == 0);
  fail_unless(g2->getVersion("")                  == 0);

  fail_unless(g2->getPackageVersion(GROUPS_XMLNS_L3V1V1) == 1);
  fail_unless(g2->getPackageVersion(CORE_XMLNS_L2V4)     == 0);
  fail_unless(g2->getPackageVersion("")                  == 0);

  delete g2;
}
END_TEST


START_TEST(test_GroupsExtension_clone)
{
  GroupsExtension* g2 = G->clone();

  fail_unless(g2->getName() == "groups");
  fail_unless(g2->getName() == GROUPS_PACKAGE_NAME);

  fail_unless(g2->getURI(3,1,1) == GROUPS_XMLNS_L3V1V1);
  fail_unless(g2->getURI(2,1,1) == "");
  fail_unless(g2->getURI(4,1,1) == "");

  fail_unless(g2->getLevel(GROUPS_XMLNS_L3V1V1) == 3);
  fail_unless(g2->getLevel(CORE_XMLNS_L2V4)     == 0);
  fail_unless(g2->getLevel("")                  == 0);

  fail_unless(g2->getVersion(GROUPS_XMLNS_L3V1V1) == 1);
  fail_unless(g2->getVersion(CORE_XMLNS_L2V4)     == 0);
  fail_unless(g2->getVersion("")                  == 0);

  fail_unless(g2->getPackageVersion(GROUPS_XMLNS_L3V1V1) == 1);
  fail_unless(g2->getPackageVersion(CORE_XMLNS_L2V4)     == 0);
  fail_unless(g2->getPackageVersion("")                  == 0);

  delete g2;
}
END_TEST


START_TEST(test_GroupsExtension_registry)
{
  const SBMLExtension* sbext = SBMLExtensionRegistry::getInstance().getExtension("groups");

  fail_unless(sbext != NULL);

  fail_unless(sbext->getName() == "groups");
  fail_unless(sbext->getName() == GROUPS_PACKAGE_NAME);

  fail_unless(sbext->getURI(3,1,1) == GROUPS_XMLNS_L3V1V1);
  fail_unless(sbext->getURI(2,1,1) == "");
  fail_unless(sbext->getURI(4,1,1) == "");

  fail_unless(sbext->getLevel(GROUPS_XMLNS_L3V1V1) == 3);
  fail_unless(sbext->getLevel(CORE_XMLNS_L2V4)     == 0);
  fail_unless(sbext->getLevel("")                  == 0);

  fail_unless(sbext->getVersion(GROUPS_XMLNS_L3V1V1) == 1);
  fail_unless(sbext->getVersion(CORE_XMLNS_L2V4)     == 0);
  fail_unless(sbext->getVersion("")                  == 0);

  fail_unless(sbext->getPackageVersion(GROUPS_XMLNS_L3V1V1) == 1);
  fail_unless(sbext->getPackageVersion(CORE_XMLNS_L2V4)     == 0);
  fail_unless(sbext->getPackageVersion("")                  == 0);

  delete sbext;
}
END_TEST


START_TEST(test_GroupsExtension_typecode)
{
  const SBMLExtension* sbext = SBMLExtensionRegistry::getInstance().
                                                      getExtension("groups");

  fail_unless(sbext != NULL);
  fail_unless(strcmp(sbext->getStringFromTypeCode(SBML_GROUPS_GROUP), 
                                                              "Group")== 0);
  fail_unless(strcmp(sbext->getStringFromTypeCode(SBML_GROUPS_MEMBER), 
                                                            "Member") == 0);
  fail_unless(strcmp(sbext->getStringFromTypeCode(SBML_GROUPS_GROUP+1), 
                                        "(Unknown SBML Groups Type)") == 0);
  fail_unless(strcmp(sbext->getStringFromTypeCode(SBML_GROUPS_MEMBER-1), 
                                        "(Unknown SBML Groups Type)") == 0);

  delete sbext;
}
END_TEST

START_TEST(test_GroupsExtension_SBMLtypecode)
{	
	fail_unless(strcmp(SBMLTypeCode_toString(SBML_GROUPS_GROUP     ,"groups"), 
                                                              "Group") == 0);
	fail_unless(strcmp(SBMLTypeCode_toString(SBML_GROUPS_MEMBER    ,"groups"), 
                                                             "Member") == 0);
	fail_unless(strcmp(SBMLTypeCode_toString(SBML_GROUPS_GROUP + 1, "groups"), 
                                         "(Unknown SBML Groups Type)") == 0);
	fail_unless(strcmp(SBMLTypeCode_toString(SBML_GROUPS_MEMBER - 1, "groups"), 
                                         "(Unknown SBML Groups Type)") == 0);
}
END_TEST

Suite *
create_suite_GroupsExtension (void)
{
  Suite *suite = suite_create("GroupsExtension");
  TCase *tcase = tcase_create("GroupsExtension");

  tcase_add_checked_fixture(tcase, GroupsExtensionTest_setup, GroupsExtensionTest_teardown);
 
  tcase_add_test( tcase, test_GroupsExtension_getName         );
  tcase_add_test( tcase, test_GroupsExtension_getURI          );
  tcase_add_test( tcase, test_GroupsExtension_getLevelVersion );
  tcase_add_test( tcase, test_GroupsExtension_getSBMLExtensionNamespaces);
  tcase_add_test( tcase, test_GroupsExtension_copy            );
  tcase_add_test( tcase, test_GroupsExtension_assignment      );
  tcase_add_test( tcase, test_GroupsExtension_clone           );
  tcase_add_test( tcase, test_GroupsExtension_registry        );
  tcase_add_test( tcase, test_GroupsExtension_typecode        );
  tcase_add_test( tcase, test_GroupsExtension_SBMLtypecode    );

  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND
