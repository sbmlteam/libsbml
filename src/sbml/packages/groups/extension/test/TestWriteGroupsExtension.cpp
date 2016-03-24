/**
 * @file    TestWriteGroupsExtension.cpp
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
#include <sbml/extension/SBMLExtensionRegistry.h>
#include <sbml/packages/groups/extension/GroupsModelPlugin.h>
#include <sbml/packages/groups/extension/GroupsExtension.h>
#include <string>

/** @cond doxygenIgnored */

using namespace std;
LIBSBML_CPP_NAMESPACE_USE

/** @endcond doxygenIgnored */


CK_CPPSTART

static string GROUPS_XMLNS_L3V1V1;
static GroupsExtension* G; 
static GroupsPkgNamespaces* GNS;

void
WriteGroupsExtensionTest_setup (void)
{
	try
	{
		G = new GroupsExtension();
		GNS = new GroupsPkgNamespaces();
		GROUPS_XMLNS_L3V1V1 = GNS->getURI();
	}
	catch(...)
	{
		fail("Failed to create a GroupsExtension object");
	}
}


void
WriteGroupsExtensionTest_teardown (void)
{
	delete G;
	delete GNS;
}

START_TEST (test_GroupsExtension_create_and_write_L3V1V1)
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
    "      <groups:group sboTerm=\"SBO:0000252\" groups:id=\"ATP\" groups:kind=\"collection\">\n"
    "        <groups:listOfMembers>\n"
    "          <groups:member groups:idRef=\"ATPc\"/>\n"
    "          <groups:member groups:idRef=\"ATPm\"/>\n"
    "        </groups:listOfMembers>\n"
    "      </groups:group>\n"
    "    </groups:listOfGroups>\n"
    "  </model>\n"
    "</sbml>\n"
    ;

  GroupsPkgNamespaces *sbmlns = new GroupsPkgNamespaces(3,1,1);

  // create the document

  SBMLDocument document(sbmlns);

  // mark groups as not required

  document.setPackageRequired("groups", false);

  // create the Model

  Model* model=document.createModel();

  // create the Compartment

  Compartment* compartment = model->createCompartment();
  compartment->setId("cytosol");
  compartment->setConstant(true);

  compartment=model->createCompartment();
  compartment->setId("mitochon");
  compartment->setConstant(true);

  // create the Species

  Species* species = model->createSpecies();
  species->setId("ATPc");
  species->setCompartment("cytosol");
  species->setHasOnlySubstanceUnits(false);
  species->setBoundaryCondition(false);
  species->setConstant(false);
  species->setInitialConcentration(1);

  species = model->createSpecies();
  species->setId("ATPm");
  species->setCompartment("mitochon");
  species->setHasOnlySubstanceUnits(false);
  species->setBoundaryCondition(false);
  species->setConstant(false);
  
  species->setInitialConcentration(2);

  // create the Group

  GroupsModelPlugin* mplugin = static_cast<GroupsModelPlugin*>(model->getPlugin("groups"));

  fail_unless(mplugin != NULL);

  Group* group = mplugin->createGroup();
  fail_unless(group->setId("ATP")              == LIBSBML_OPERATION_SUCCESS);
  fail_unless(group->setSBOTerm("SBO:0000252") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(group->setKind(GroupKind_fromString("collection"))
                                               == LIBSBML_OPERATION_SUCCESS);

  Member* member = group->createMember();
  fail_unless(member->setIdRef("ATPc") == LIBSBML_OPERATION_SUCCESS);

  member = group->createMember();
  fail_unless(member->setIdRef("ATPm") == LIBSBML_OPERATION_SUCCESS);

  std::string s2 = writeSBMLToStdString(&document);

  fail_unless(s1 == s2); 

  // check clone()

  SBMLDocument document2 = document;
  s2 = writeSBMLToStdString(&document2);
  fail_unless(s1==s2);

  // check operator=

  Model m = *(document.getModel());
  document2.setModel(&m);
  s2 = writeSBMLToStdString(&document2);
  fail_unless(s1==s2);
 
  delete sbmlns;
}
END_TEST


START_TEST (test_GroupsExtension_create_add_and_write_L3V1V1)
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
    "      <groups:group sboTerm=\"SBO:0000252\" groups:id=\"ATP\" groups:kind=\"collection\">\n"
    "        <groups:listOfMembers>\n"
    "          <groups:member groups:idRef=\"ATPc\"/>\n"
    "          <groups:member groups:idRef=\"ATPm\"/>\n"
    "        </groups:listOfMembers>\n"
    "      </groups:group>\n"
    "    </groups:listOfGroups>\n"
    "  </model>\n"
    "</sbml>\n"
    ;

  GroupsPkgNamespaces *sbmlns = new GroupsPkgNamespaces(3,1,1);

  // create the document

  SBMLDocument document = SBMLDocument(sbmlns);

  // mark groups as not required

  document.setPackageRequired("groups", false);
  
  // create the Model

  Model model = Model(sbmlns);

  // create the Compartment

  Compartment compartment1 = Compartment(sbmlns);
  compartment1.setId("cytosol");
  compartment1.setConstant(true);
  model.addCompartment(&compartment1);

  Compartment compartment2 = Compartment(sbmlns);
  compartment2.setId("mitochon");
  compartment2.setConstant(true);
  model.addCompartment(&compartment2);

  // create the Species

  Species species1 = Species(sbmlns);
  species1.setId("ATPc");
  species1.setCompartment("cytosol");
  species1.setInitialConcentration(1);
  species1.setConstant(false);
  species1.setBoundaryCondition(false);
  species1.setHasOnlySubstanceUnits(false);
  model.addSpecies(&species1);

  Species species2 = Species(sbmlns);
  species2.setId("ATPm");
  species2.setCompartment("mitochon");
  species2.setInitialConcentration(2);
  species2.setConstant(false);
  species2.setBoundaryCondition(false);
  species2.setHasOnlySubstanceUnits(false);
  model.addSpecies(&species2);

  // create the Group

  GroupsModelPlugin *mplugin = static_cast<GroupsModelPlugin*>(model.getPlugin("groups"));

  fail_unless(mplugin != NULL);

  Group group = Group(sbmlns);
  fail_unless(group.setId("ATP")              == LIBSBML_OPERATION_SUCCESS);
  fail_unless(group.setSBOTerm("SBO:0000252") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(group.setKind(GROUP_KIND_COLLECTION)
                                               == LIBSBML_OPERATION_SUCCESS);

  Member member1 = Member(sbmlns);
  fail_unless(member1.setIdRef("ATPc") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(group.addMember(&member1) == LIBSBML_OPERATION_SUCCESS);

  Member member2 = Member(sbmlns);
  fail_unless(member2.setIdRef("ATPm") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(group.addMember(&member2) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(mplugin->addGroup(&group) == LIBSBML_OPERATION_SUCCESS);
  fail_unless(mplugin->getGroup(0) != NULL);

  // set the model to the document

  document.setModel(&model);  

  std::string s2 = writeSBMLToStdString(&document);

  fail_unless(s1 == s2); 
  
  delete sbmlns;
}
END_TEST


START_TEST (test_GroupsExtension_read_enable_via_model_and_write_L3V1V1)
{
  const char* s1 =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<sbml xmlns=\"http://www.sbml.org/sbml/level3/version1/core\" level=\"3\" version=\"1\">\n"
    "  <model>\n"
    "    <listOfCompartments>\n"
    "      <compartment id=\"cytosol\" constant=\"true\"/>\n"
    "      <compartment id=\"mitochon\" constant=\"true\"/>\n"
    "    </listOfCompartments>\n"
    "    <listOfSpecies>\n"
    "      <species id=\"ATPc\" compartment=\"cytosol\" initialConcentration=\"1\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\"/>\n"
    "      <species id=\"ATPm\" compartment=\"mitochon\" initialConcentration=\"2\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\"/>\n"
    "    </listOfSpecies>\n"
    "  </model>\n"
    "</sbml>\n"
    ;

  const char* s1a =
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
    "      <groups:group sboTerm=\"SBO:0000252\" groups:id=\"ATP\" groups:kind=\"collection\">\n"
    "        <groups:listOfMembers>\n"
    "          <groups:member groups:idRef=\"ATPc\"/>\n"
    "          <groups:member groups:idRef=\"ATPm\"/>\n"
    "        </groups:listOfMembers>\n"
    "      </groups:group>\n"
    "    </groups:listOfGroups>\n"
    "  </model>\n"
    "</sbml>\n"
    ;

  SBMLDocument *document = readSBMLFromString(s1);

  fail_unless(document->getNumPlugins()             == 0);

  Model *model = document->getModel();

  fail_unless(model != NULL);
  fail_unless(model->getNumPlugins() == 0);

  //
  // enable the groups package by invoking enablePackage function with Model object
  //
  fail_unless(model->enablePackage(GROUPS_XMLNS_L3V1V1, "groups", true) == LIBSBML_OPERATION_SUCCESS);

  // mark groups as not required

  document->setPackageRequired("groups", false);

  fail_unless(document->getNumPlugins() == 1);
  fail_unless(model->getNumPlugins()    == 1);

  // create a group object 

  GroupsModelPlugin* mplugin = static_cast<GroupsModelPlugin*>(model->getPlugin("groups"));

  fail_unless(mplugin != NULL);

  Group* group = mplugin->createGroup();
  fail_unless(group->setId("ATP")              == LIBSBML_OPERATION_SUCCESS);
  fail_unless(group->setSBOTerm("SBO:0000252") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(group->setKind(GroupKind_fromString("collection"))
                                               == LIBSBML_OPERATION_SUCCESS);

  Member* member = group->createMember();
  fail_unless(member->setIdRef("ATPc") == LIBSBML_OPERATION_SUCCESS);

  member = group->createMember();
  fail_unless(member->setIdRef("ATPm") == LIBSBML_OPERATION_SUCCESS);

  std::string s2 = writeSBMLToStdString(document);

  fail_unless(s1a == s2); 
  
  delete document;  
}
END_TEST


START_TEST (test_GroupsExtension_read_disable_via_model_and_write_L3V1V1)
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
    "      <groups:group sboTerm=\"SBO:0000252\" groups:id=\"ATP\" groups:kind=\"collection\">\n"
    "        <groups:listOfMembers>\n"
    "          <groups:member groups:idRef=\"ATPc\"/>\n"
    "          <groups:member groups:idRef=\"ATPm\"/>\n"
    "        </groups:listOfMembers>\n"
    "      </groups:group>\n"
    "    </groups:listOfGroups>\n"
    "  </model>\n"
    "</sbml>\n"
    ;

  const char* s1d =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<sbml xmlns=\"http://www.sbml.org/sbml/level3/version1/core\" level=\"3\" version=\"1\">\n"
    "  <model>\n"
    "    <listOfCompartments>\n"
    "      <compartment id=\"cytosol\" constant=\"true\"/>\n"
    "      <compartment id=\"mitochon\" constant=\"true\"/>\n"
    "    </listOfCompartments>\n"
    "    <listOfSpecies>\n"
    "      <species id=\"ATPc\" compartment=\"cytosol\" initialConcentration=\"1\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\"/>\n"
    "      <species id=\"ATPm\" compartment=\"mitochon\" initialConcentration=\"2\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\"/>\n"
    "    </listOfSpecies>\n"
    "  </model>\n"
    "</sbml>\n"
    ;

  SBMLDocument *document = readSBMLFromString(s1);

  fail_unless(document->getNumPlugins() == 1);

  Model *model = document->getModel();

  fail_unless(model != NULL);
  fail_unless(model->getNumPlugins() == 1);

  //
  // disable the groups package by invoking enablePackage function with Model object
  //
  fail_unless(model->enablePackage(GROUPS_XMLNS_L3V1V1, "groups", false) == LIBSBML_OPERATION_SUCCESS);

  fail_unless(document->getNumPlugins() == 0);
  fail_unless(model->getNumPlugins()    == 0);

  char *s2 = writeSBMLToString(document);

  fail_unless(strcmp(s1d,s2) == 0); 
  
  free(s2);
  delete document;  
}
END_TEST


START_TEST (test_GroupsExtension_read_enable_via_sbmldocument_and_write_L3V1V1)
{
  const char* s1 =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<sbml xmlns=\"http://www.sbml.org/sbml/level3/version1/core\" level=\"3\" version=\"1\">\n"
    "  <model>\n"
    "    <listOfCompartments>\n"
    "      <compartment id=\"cytosol\" constant=\"true\"/>\n"
    "      <compartment id=\"mitochon\" constant=\"true\"/>\n"
    "    </listOfCompartments>\n"
    "    <listOfSpecies>\n"
    "      <species id=\"ATPc\" compartment=\"cytosol\" initialConcentration=\"1\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\"/>\n"
    "      <species id=\"ATPm\" compartment=\"mitochon\" initialConcentration=\"2\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\"/>\n"
    "    </listOfSpecies>\n"
    "  </model>\n"
    "</sbml>\n"
    ;

  const char* s1a =
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
    "      <groups:group sboTerm=\"SBO:0000252\" groups:id=\"ATP\" groups:kind=\"collection\">\n"
    "        <groups:listOfMembers>\n"
    "          <groups:member groups:idRef=\"ATPc\"/>\n"
    "          <groups:member groups:idRef=\"ATPm\"/>\n"
    "        </groups:listOfMembers>\n"
    "      </groups:group>\n"
    "    </groups:listOfGroups>\n"
    "  </model>\n"
    "</sbml>\n"
    ;

  SBMLDocument *document = readSBMLFromString(s1);

  fail_unless(document->getNumPlugins()             == 0);

  //
  // enable the groups package by invoking enablePackage function with SBMLDocument object
  //
  fail_unless(document->enablePackage(GROUPS_XMLNS_L3V1V1, "groups", true) == LIBSBML_OPERATION_SUCCESS);

  // mark groups as not required

  document->setPackageRequired("groups", false);

  fail_unless(document->getNumPlugins()             == 1);

  Model *model = document->getModel();

  fail_unless(model != NULL);
  fail_unless(model->getNumPlugins() == 1);

  // create a group object 

  GroupsModelPlugin* mplugin = static_cast<GroupsModelPlugin*>(model->getPlugin("groups"));

  fail_unless(mplugin != NULL);

  Group* group = mplugin->createGroup();
  fail_unless(group->setId("ATP")              == LIBSBML_OPERATION_SUCCESS);
  fail_unless(group->setSBOTerm("SBO:0000252") == LIBSBML_OPERATION_SUCCESS);
  fail_unless(group->setKind(GroupKind_fromString("collection"))
                                               == LIBSBML_OPERATION_SUCCESS);

  Member* member = group->createMember();
  fail_unless(member->setIdRef("ATPc") == LIBSBML_OPERATION_SUCCESS);

  member = group->createMember();
  fail_unless(member->setIdRef("ATPm") == LIBSBML_OPERATION_SUCCESS);

  char *s2 = writeSBMLToString(document);

  fail_unless(strcmp(s1a,s2) == 0); 
  
  free(s2);
  delete document;  
}
END_TEST


START_TEST (test_GroupsExtension_read_disable_via_sbmldocument_and_write_L3V1V1)
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
    "      <groups:group sboTerm=\"SBO:0000252\" groups:id=\"ATP\" groups:kind=\"collection\">\n"
    "        <groups:listOfMembers>\n"
    "          <groups:member groups:idRef=\"ATPc\"/>\n"
    "          <groups:member groups:idRef=\"ATPm\"/>\n"
    "        </groups:listOfMembers>\n"
    "      </groups:group>\n"
    "    </groups:listOfGroups>\n"
    "  </model>\n"
    "</sbml>\n"
    ;

  const char* s1d =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<sbml xmlns=\"http://www.sbml.org/sbml/level3/version1/core\" level=\"3\" version=\"1\">\n"
    "  <model>\n"
    "    <listOfCompartments>\n"
    "      <compartment id=\"cytosol\" constant=\"true\"/>\n"
    "      <compartment id=\"mitochon\" constant=\"true\"/>\n"
    "    </listOfCompartments>\n"
    "    <listOfSpecies>\n"
    "      <species id=\"ATPc\" compartment=\"cytosol\" initialConcentration=\"1\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\"/>\n"
    "      <species id=\"ATPm\" compartment=\"mitochon\" initialConcentration=\"2\" hasOnlySubstanceUnits=\"false\" boundaryCondition=\"false\" constant=\"false\"/>\n"
    "    </listOfSpecies>\n"
    "  </model>\n"
    "</sbml>\n"
    ;

  SBMLDocument *document = readSBMLFromString(s1);

  fail_unless(document->getNumPlugins() == 1);

  //
  // disable the groups package by invoking enablePackage function with Model object
  //
  fail_unless(document->enablePackage(GROUPS_XMLNS_L3V1V1, "groups", false) == LIBSBML_OPERATION_SUCCESS);

  fail_unless(document->getNumPlugins() == 0);

  Model *model = document->getModel(); 

  fail_unless(model != NULL);
  fail_unless(model->getNumPlugins() == 0);

  char *s2 = writeSBMLToString(document);

  fail_unless(strcmp(s1d,s2) == 0); 
  
  free(s2);
  delete document;  
}
END_TEST



Suite *
create_suite_WriteGroupsExtension (void)
{
  Suite *suite = suite_create("WriteGroupsExtension");
  TCase *tcase = tcase_create("WriteGroupsExtension");

  tcase_add_checked_fixture(tcase, WriteGroupsExtensionTest_setup, WriteGroupsExtensionTest_teardown);	
	
  tcase_add_test( tcase, test_GroupsExtension_create_and_write_L3V1V1);
  tcase_add_test( tcase, test_GroupsExtension_create_add_and_write_L3V1V1);
  tcase_add_test( tcase, test_GroupsExtension_read_enable_via_model_and_write_L3V1V1);
  tcase_add_test( tcase, test_GroupsExtension_read_disable_via_model_and_write_L3V1V1);
  tcase_add_test( tcase, test_GroupsExtension_read_enable_via_sbmldocument_and_write_L3V1V1);
  tcase_add_test( tcase, test_GroupsExtension_read_disable_via_sbmldocument_and_write_L3V1V1);
  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND
