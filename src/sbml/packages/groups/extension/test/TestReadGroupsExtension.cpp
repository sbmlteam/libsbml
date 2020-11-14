/**
 * @file    TestReadGroupsExtension.cpp
 * @brief   Unit tests of writing GroupsExtension 
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
#include <sbml/SBMLTypes.h>
#include <sbml/common/extern.h>
#include <sbml/extension/SBMLExtensionRegistry.h>
#include <sbml/packages/groups/common/GroupsExtensionTypes.h>
#include <string>

/** @cond doxygenIgnored */

using namespace std;
LIBSBML_CPP_NAMESPACE_USE

/** @endcond doxygenIgnored */


CK_CPPSTART

extern char *TestDataDirectory;

START_TEST (test_GroupsExtension_read_L3V1V1)
{
  char *filename = safe_strcat(TestDataDirectory, "groups-example1.xml");
  SBMLDocument *document = readSBMLFromFile(filename);
  fail_unless(document->getPackageName() == "core");

  Model *model = document->getModel();

  fail_unless(model != NULL);
  fail_unless(model->getPackageName() == "core");
  fail_unless(document->getNumErrors() == 0);

  // get the Group

  GroupsModelPlugin* mplugin = static_cast<GroupsModelPlugin*>(model->getPlugin("groups"));
  fail_unless(mplugin != NULL);

  fail_unless(mplugin->getNumGroups() == 1);
  fail_unless(mplugin->getListOfGroups()->getPackageName() == "groups");

  Group* group = mplugin->getGroup(0);
  fail_unless(group->getId()          == "ATP");
  fail_unless(group->getSBOTermID()   == "SBO:0000252");
  fail_unless(group->getKind()        == GROUP_KIND_CLASSIFICATION);
  fail_unless(group->getNumMembers()  == 2);
  fail_unless(group->getPackageName() == "groups");

  fail_unless(group->getListOfMembers()->getPackageName() == "groups");

  Member* member = group->getMember(0);
  fail_unless(member->getIdRef()      == "ATPc");
  fail_unless(member->getPackageName() == "groups");

  member = group->getMember(1);
  fail_unless(member->getIdRef()      == "ATPm");
  fail_unless(member->getPackageName() == "groups");

  delete document; 
  safe_free(filename);
}
END_TEST


START_TEST (test_GroupsExtension_read_L3V1V1_defaultNS)
{
  char *filename = safe_strcat(TestDataDirectory, "groups-example1-defaultNS.xml");
  SBMLDocument *document = readSBMLFromFile(filename);
  
  fail_unless(document->getPackageName() == "core");

  Model *model = document->getModel();

  document->printErrors();

  fail_unless(model != NULL);
  fail_unless(document->getNumErrors() == 0);

  // get the Group

  GroupsModelPlugin* mplugin = static_cast<GroupsModelPlugin*>(model->getPlugin("groups"));
  fail_unless(mplugin != NULL);

  fail_unless(mplugin->getNumGroups() == 1);
  fail_unless(mplugin->getListOfGroups()->getPackageName() == "groups");

  Group* group = mplugin->getGroup(0);
  fail_unless(group->getId()          == "ATP");
  fail_unless(group->getSBOTermID()   == "SBO:0000252");
  fail_unless(group->getKind()        == GROUP_KIND_CLASSIFICATION);
  fail_unless(group->isSetKind()      == true);
  fail_unless(!strcmp(GroupKind_toString(group->getKind()), "classification"));
  fail_unless(group->getNumMembers()  == 2);
  fail_unless(group->getPackageName() == "groups");

  fail_unless(group->getListOfMembers()->getPackageName() == "groups");

  Member* member = group->getMember(0);
  fail_unless(member->getIdRef()      == "ATPc");
  fail_unless(member->getPackageName() == "groups");

  member = group->getMember(1);
  fail_unless(member->getIdRef()      == "ATPm");
  fail_unless(member->getPackageName() == "groups");

  delete document;  
  safe_free(filename);
}
END_TEST


START_TEST (test_GroupsExtension_read_L3V1V1_unknown_elements)
{
  const char* s1 =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<sbml xmlns=\"http://www.sbml.org/sbml/level3/version1/core\" xmlns:groups=\"http://www.sbml.org/sbml/level3/version1/groups/version1\" level=\"3\" version=\"1\" groups:required=\"false\">\n"
    "  <model>\n"
    "    <listOfCompartments>\n"
    "      <compartment id=\"cytosol\" constant=\"true\"/>\n"
    "      <compartment id=\"mitochon\" constant=\"true\"/>\n"
    "    </listOfCompartments>\n"
    "    <listOfSpecies>\n"
    "      <species id=\"ATPc\" compartment=\"cytosol\" initialConcentration=\"1\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\"/>\n"
    "      <species id=\"ATPm\" compartment=\"mitochon\" initialConcentration=\"2\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\"/>\n"
    "    </listOfSpecies>\n"
    "    <groups:listOfGroups>\n"
    "      <groups:group sboTerm=\"SBO:0000252\" groups:kind=\"partonomy\" groups:id=\"ATP\" groups:unknown=\"unknown\" >\n"
    "        <groups:listOfMembers>\n"
    "          <groups:member groups:idRef=\"ATPc\" groups:unknown=\"unknown\"/>\n"
    "          <groups:member groups:idRef=\"ATPm\"/>\n"
    "        </groups:listOfMembers>\n"
    "        <groups:unknown>\n"
    "        </groups:unknown>\n"
    "      </groups:group>\n"
    "    </groups:listOfGroups>\n"
    "  </model>\n"
    "</sbml>\n"
    ;

  SBMLDocument *document = readSBMLFromString(s1);
  Model *model = document->getModel();
 
  fail_unless(model != NULL);
  fail_unless(document->getNumErrors() == 3);

  delete document;
}
END_TEST


Suite *
create_suite_ReadGroupsExtension (void)
{
  Suite *suite = suite_create("ReadGroupsExtension");
  TCase *tcase = tcase_create("ReadGroupsExtension");

  tcase_add_test( tcase, test_GroupsExtension_read_L3V1V1);
  tcase_add_test( tcase, test_GroupsExtension_read_L3V1V1_defaultNS);
  tcase_add_test( tcase, test_GroupsExtension_read_L3V1V1_unknown_elements);
  
  
  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND
