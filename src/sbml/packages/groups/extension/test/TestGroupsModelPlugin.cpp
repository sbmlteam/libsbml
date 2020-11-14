/**
 * @file    TestGroupsModelPlugin.cpp
 * @brief   TestGroupsModelPlugin unit tests
 * @author  Lucian Smith
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
#include <sbml/SBMLTypes.h>
#include <string>

/** @cond doxygenIgnored */

using namespace std;
LIBSBML_CPP_NAMESPACE_USE

/** @endcond doxygenIgnored */


CK_CPPSTART

extern char *TestDataDirectory;

START_TEST (test_GroupsModelPlugin_copyNesting1)
{
  //GroupsPkgNamespaces gpn;
  //SBMLDocument doc(&gpn);
  char *filename = safe_strcat(TestDataDirectory, "groups-nested1.xml");
  SBMLDocument *document = readSBMLFromFile(filename);
  Model* model = document->getModel();
  fail_unless(model != NULL);
  GroupsModelPlugin* mplugin = static_cast<GroupsModelPlugin*>(model->getPlugin("groups"));
  fail_unless(mplugin != NULL);

  fail_unless(mplugin->getNumGroups() == 4);
  Group* group1 = mplugin->getGroup(0);
  Group* group2 = mplugin->getGroup(1);
  Group* group3 = mplugin->getGroup(2);
  Group* group4 = mplugin->getGroup(3);
  fail_unless(!group1->getListOfMembers()->isSetSBOTerm());
  fail_unless(!group2->getListOfMembers()->isSetSBOTerm());
  fail_unless(!group3->getListOfMembers()->isSetSBOTerm());
  fail_unless( group4->getListOfMembers()->isSetSBOTerm());
  mplugin->copyInformationToNestedLists();
  fail_unless(!group1->getListOfMembers()->isSetSBOTerm());
  fail_unless( group2->getListOfMembers()->isSetSBOTerm());
  fail_unless( group3->getListOfMembers()->isSetSBOTerm());
  fail_unless( group4->getListOfMembers()->isSetSBOTerm());

  group4->getListOfMembers()->setNotes("These are some notes.", true);
  mplugin->copyInformationToNestedLists();
  fail_unless(!group1->getListOfMembers()->isSetNotes());
  fail_unless( group2->getListOfMembers()->isSetNotes());
  fail_unless( group3->getListOfMembers()->isSetNotes());

  group4->getListOfMembers()->setAnnotation("This is a malformed annotation, but who cares.");
  mplugin->copyInformationToNestedLists();
  fail_unless(!group1->getListOfMembers()->isSetAnnotation());
  fail_unless( group2->getListOfMembers()->isSetAnnotation());
  fail_unless( group3->getListOfMembers()->isSetAnnotation());

  delete document;
  safe_free((void*)(filename));
}
END_TEST


START_TEST (test_GroupsModelPlugin_copyNesting2)
{
  //GroupsPkgNamespaces gpn;
  //SBMLDocument doc(&gpn);
  char *filename = safe_strcat(TestDataDirectory, "groups-nested1.xml");
  SBMLDocument *document = readSBMLFromFile(filename);
  Model* model = document->getModel();
  fail_unless(model != NULL);
  GroupsModelPlugin* mplugin = static_cast<GroupsModelPlugin*>(model->getPlugin("groups"));
  fail_unless(mplugin != NULL);

  fail_unless(mplugin->getNumGroups() == 4);
  Group* group1 = mplugin->getGroup(0);
  Group* group2 = mplugin->getGroup(1);
  Group* group3 = mplugin->getGroup(2);
  Group* group4 = mplugin->getGroup(3);
  group3->getListOfMembers()->setSBOTerm(5);
  mplugin->copyInformationToNestedLists();
  fail_unless(!group1->getListOfMembers()->isSetSBOTerm());
  fail_unless( group2->getListOfMembers()->getSBOTerm() == 5);
  fail_unless( group3->getListOfMembers()->getSBOTerm() == 5);
  fail_unless( group4->getListOfMembers()->getSBOTerm() == 252);

  group4->getListOfMembers()->setNotes("These are some notes, v1.", true);
  group3->getListOfMembers()->setNotes("These are some notes, v2.", true);
  mplugin->copyInformationToNestedLists();
  fail_unless(!group1->getListOfMembers()->isSetNotes());
  fail_unless( group2->getListOfMembers()->getNotesString().find("These are some notes, v2.") != string::npos);
  fail_unless( group3->getListOfMembers()->getNotesString().find("These are some notes, v2.") != string::npos);
  fail_unless( group4->getListOfMembers()->getNotesString().find("These are some notes, v1.") != string::npos);

  group4->getListOfMembers()->setAnnotation("This is a malformed annotation, v1.");
  group3->getListOfMembers()->setAnnotation("This is a malformed annotation, v2.");
  mplugin->copyInformationToNestedLists();
  fail_unless(!group1->getListOfMembers()->isSetAnnotation());
  fail_unless( group2->getListOfMembers()->getAnnotationString().find("This is a malformed annotation, v2.") != string::npos);
  fail_unless( group3->getListOfMembers()->getAnnotationString().find("This is a malformed annotation, v2.") != string::npos);
  fail_unless( group4->getListOfMembers()->getAnnotationString().find("This is a malformed annotation, v1.") != string::npos);

  delete document;
  safe_free((void*)(filename));
}
END_TEST


START_TEST (test_GroupsModelPlugin_copyNesting3)
{
  //GroupsPkgNamespaces gpn;
  //SBMLDocument doc(&gpn);
  char *filename = safe_strcat(TestDataDirectory, "groups-nested1.xml");
  SBMLDocument *document = readSBMLFromFile(filename);
  Model* model = document->getModel();
  fail_unless(model != NULL);
  GroupsModelPlugin* mplugin = static_cast<GroupsModelPlugin*>(model->getPlugin("groups"));
  fail_unless(mplugin != NULL);

  fail_unless(mplugin->getNumGroups() == 4);
  Group* group1 = mplugin->getGroup(0);
  Group* group2 = mplugin->getGroup(1);
  Group* group3 = mplugin->getGroup(2);
  Group* group4 = mplugin->getGroup(3);
  group2->getListOfMembers()->setSBOTerm(5);
  mplugin->copyInformationToNestedLists();
  fail_unless(!group1->getListOfMembers()->isSetSBOTerm());
  fail_unless( group2->getListOfMembers()->getSBOTerm() == 5);
  fail_unless( group3->getListOfMembers()->getSBOTerm() == 252);
  fail_unless( group4->getListOfMembers()->getSBOTerm() == 252);

  group4->getListOfMembers()->setNotes("These are some notes, v1.", true);
  group2->getListOfMembers()->setNotes("These are some notes, v2.", true);
  mplugin->copyInformationToNestedLists();
  fail_unless(!group1->getListOfMembers()->isSetNotes());
  fail_unless( group2->getListOfMembers()->getNotesString().find("These are some notes, v2.") != string::npos);
  fail_unless( group3->getListOfMembers()->getNotesString().find("These are some notes, v1.") != string::npos);
  fail_unless( group4->getListOfMembers()->getNotesString().find("These are some notes, v1.") != string::npos);

  group4->getListOfMembers()->setAnnotation("This is a malformed annotation, v1.");
  group2->getListOfMembers()->setAnnotation("This is a malformed annotation, v2.");
  mplugin->copyInformationToNestedLists();
  fail_unless(!group1->getListOfMembers()->isSetAnnotation());
  fail_unless( group2->getListOfMembers()->getAnnotationString().find("This is a malformed annotation, v2.") != string::npos);
  fail_unless( group3->getListOfMembers()->getAnnotationString().find("This is a malformed annotation, v1.") != string::npos);
  fail_unless( group4->getListOfMembers()->getAnnotationString().find("This is a malformed annotation, v1.") != string::npos);

  delete document;
  safe_free((void*)(filename));
}
END_TEST


START_TEST (test_GroupsModelPlugin_copyNesting1_v2)
{
  //GroupsPkgNamespaces gpn;
  //SBMLDocument doc(&gpn);
  char *filename = safe_strcat(TestDataDirectory, "groups-nested2.xml");
  SBMLDocument *document = readSBMLFromFile(filename);
  Model* model = document->getModel();
  fail_unless(model != NULL);
  GroupsModelPlugin* mplugin = static_cast<GroupsModelPlugin*>(model->getPlugin("groups"));
  fail_unless(mplugin != NULL);

  fail_unless(mplugin->getNumGroups() == 4);
  Group* group1 = mplugin->getGroup(0);
  Group* group2 = mplugin->getGroup(1);
  Group* group3 = mplugin->getGroup(2);
  Group* group4 = mplugin->getGroup(3);
  fail_unless(!group1->getListOfMembers()->isSetSBOTerm());
  fail_unless(!group2->getListOfMembers()->isSetSBOTerm());
  fail_unless(!group3->getListOfMembers()->isSetSBOTerm());
  fail_unless( group4->getListOfMembers()->isSetSBOTerm());
  mplugin->copyInformationToNestedLists();
  fail_unless(!group1->getListOfMembers()->isSetSBOTerm());
  fail_unless( group2->getListOfMembers()->isSetSBOTerm());
  fail_unless( group3->getListOfMembers()->isSetSBOTerm());
  fail_unless( group4->getListOfMembers()->isSetSBOTerm());

  group4->getListOfMembers()->setNotes("These are some notes.", true);
  mplugin->copyInformationToNestedLists();
  fail_unless(!group1->getListOfMembers()->isSetNotes());
  fail_unless( group2->getListOfMembers()->isSetNotes());
  fail_unless( group3->getListOfMembers()->isSetNotes());

  group4->getListOfMembers()->setAnnotation("This is a malformed annotation, but who cares.");
  mplugin->copyInformationToNestedLists();
  fail_unless(!group1->getListOfMembers()->isSetAnnotation());
  fail_unless( group2->getListOfMembers()->isSetAnnotation());
  fail_unless( group3->getListOfMembers()->isSetAnnotation());

  delete document;
  safe_free((void*)(filename));
}
END_TEST


START_TEST (test_GroupsModelPlugin_copyNesting2_v2)
{
  //GroupsPkgNamespaces gpn;
  //SBMLDocument doc(&gpn);
  char *filename = safe_strcat(TestDataDirectory, "groups-nested2.xml");
  SBMLDocument *document = readSBMLFromFile(filename);
  Model* model = document->getModel();
  fail_unless(model != NULL);
  GroupsModelPlugin* mplugin = static_cast<GroupsModelPlugin*>(model->getPlugin("groups"));
  fail_unless(mplugin != NULL);

  fail_unless(mplugin->getNumGroups() == 4);
  Group* group1 = mplugin->getGroup(0);
  Group* group2 = mplugin->getGroup(1);
  Group* group3 = mplugin->getGroup(2);
  Group* group4 = mplugin->getGroup(3);
  group3->getListOfMembers()->setSBOTerm(5);
  mplugin->copyInformationToNestedLists();
  fail_unless(!group1->getListOfMembers()->isSetSBOTerm());
  fail_unless( group2->getListOfMembers()->getSBOTerm() == 5);
  fail_unless( group3->getListOfMembers()->getSBOTerm() == 5);
  fail_unless( group4->getListOfMembers()->getSBOTerm() == 252);

  group4->getListOfMembers()->setNotes("These are some notes, v1.", true);
  group3->getListOfMembers()->setNotes("These are some notes, v2.", true);
  mplugin->copyInformationToNestedLists();
  fail_unless(!group1->getListOfMembers()->isSetNotes());
  fail_unless( group2->getListOfMembers()->getNotesString().find("These are some notes, v2.") != string::npos);
  fail_unless( group3->getListOfMembers()->getNotesString().find("These are some notes, v2.") != string::npos);
  fail_unless( group4->getListOfMembers()->getNotesString().find("These are some notes, v1.") != string::npos);

  group4->getListOfMembers()->setAnnotation("This is a malformed annotation, v1.");
  group3->getListOfMembers()->setAnnotation("This is a malformed annotation, v2.");
  mplugin->copyInformationToNestedLists();
  fail_unless(!group1->getListOfMembers()->isSetAnnotation());
  fail_unless( group2->getListOfMembers()->getAnnotationString().find("This is a malformed annotation, v2.") != string::npos);
  fail_unless( group3->getListOfMembers()->getAnnotationString().find("This is a malformed annotation, v2.") != string::npos);
  fail_unless( group4->getListOfMembers()->getAnnotationString().find("This is a malformed annotation, v1.") != string::npos);

  delete document;
  safe_free((void*)(filename));
}
END_TEST


START_TEST (test_GroupsModelPlugin_copyNesting3_v2)
{
  //GroupsPkgNamespaces gpn;
  //SBMLDocument doc(&gpn);
  char *filename = safe_strcat(TestDataDirectory, "groups-nested2.xml");
  SBMLDocument *document = readSBMLFromFile(filename);
  Model* model = document->getModel();
  fail_unless(model != NULL);
  GroupsModelPlugin* mplugin = static_cast<GroupsModelPlugin*>(model->getPlugin("groups"));
  fail_unless(mplugin != NULL);

  fail_unless(mplugin->getNumGroups() == 4);
  Group* group1 = mplugin->getGroup(0);
  Group* group2 = mplugin->getGroup(1);
  Group* group3 = mplugin->getGroup(2);
  Group* group4 = mplugin->getGroup(3);
  group2->getListOfMembers()->setSBOTerm(5);
  mplugin->copyInformationToNestedLists();
  fail_unless(!group1->getListOfMembers()->isSetSBOTerm());
  fail_unless( group2->getListOfMembers()->getSBOTerm() == 5);
  fail_unless( group3->getListOfMembers()->getSBOTerm() == 252);
  fail_unless( group4->getListOfMembers()->getSBOTerm() == 252);

  group4->getListOfMembers()->setNotes("These are some notes, v1.", true);
  group2->getListOfMembers()->setNotes("These are some notes, v2.", true);
  mplugin->copyInformationToNestedLists();
  fail_unless(!group1->getListOfMembers()->isSetNotes());
  fail_unless( group2->getListOfMembers()->getNotesString().find("These are some notes, v2.") != string::npos);
  fail_unless( group3->getListOfMembers()->getNotesString().find("These are some notes, v1.") != string::npos);
  fail_unless( group4->getListOfMembers()->getNotesString().find("These are some notes, v1.") != string::npos);

  group4->getListOfMembers()->setAnnotation("This is a malformed annotation, v1.");
  group2->getListOfMembers()->setAnnotation("This is a malformed annotation, v2.");
  mplugin->copyInformationToNestedLists();
  fail_unless(!group1->getListOfMembers()->isSetAnnotation());
  fail_unless( group2->getListOfMembers()->getAnnotationString().find("This is a malformed annotation, v2.") != string::npos);
  fail_unless( group3->getListOfMembers()->getAnnotationString().find("This is a malformed annotation, v1.") != string::npos);
  fail_unless( group4->getListOfMembers()->getAnnotationString().find("This is a malformed annotation, v1.") != string::npos);

  delete document;
  safe_free((void*)(filename));
}
END_TEST


Suite *
create_suite_GroupsModelPlugin (void)
{
  Suite *suite = suite_create("GroupsModelPlugin");
  TCase *tcase = tcase_create("GroupsModelPlugin");

  tcase_add_test( tcase, test_GroupsModelPlugin_copyNesting1_v2);
  tcase_add_test( tcase, test_GroupsModelPlugin_copyNesting2_v2);
  tcase_add_test( tcase, test_GroupsModelPlugin_copyNesting3_v2);

  tcase_add_test( tcase, test_GroupsModelPlugin_copyNesting1);
  tcase_add_test( tcase, test_GroupsModelPlugin_copyNesting2);
  tcase_add_test( tcase, test_GroupsModelPlugin_copyNesting3);

  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND
