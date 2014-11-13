/**
 * \file    TestUncertMLNodeAttributes.cpp
 * \brief   Implementation of the Tests for the UncertML attributes/elements
 * \author  Sarah Keating
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

#include <sbml/common/common.h>

#include <sbml/SBMLReader.h>
#include <sbml/SBMLDocument.h>
#include <sbml/packages/distrib/util/UncertMLNode.h>

#include <check.h>

using namespace std;

LIBSBML_CPP_NAMESPACE_USE

CK_CPPSTART

static UncertMLNode *node;


void
UncertMLTest_setup (void)
{
  node = new UncertMLNode();

  if (node == NULL)
  {
    fail("Failed to create an UncertMLNode object.");
  }
}


void
UncertMLTest_teardown (void)
{
  delete node;
}


START_TEST (test_uncertml_elementName)
{
  fail_unless ( node->isSetElementName() == false );

  int i = node->setElementName("NormalDistribution");

  fail_unless ( i == LIBSBML_OPERATION_SUCCESS );
  fail_unless ( node->isSetElementName() == true );

  std::string name = node->getElementName();

  fail_unless ( name == "NormalDistribution" );

  i = node->unsetElementName();

  fail_unless ( i == LIBSBML_OPERATION_SUCCESS );
  fail_unless ( node->isSetElementName() == false );
  fail_unless ( node->getElementName() == "" );
}
END_TEST
  

START_TEST (test_uncertml_attributes)
{
  fail_unless ( node->getNumAttributes() == 0 );

  XMLAttributes * attr = new XMLAttributes();
  attr->add("definition", "http://");

  int i = node->setAttributes(*(attr));

  delete attr;

  fail_unless ( i == LIBSBML_OPERATION_SUCCESS );
  fail_unless ( node->getNumAttributes() == 1 );

  const XMLAttributes retrieved = node->getAttributes();

  fail_unless ( retrieved.isEmpty() == false );
  fail_unless ( retrieved.getLength() == 1 );
  fail_unless ( retrieved.getName(0) == "definition" );
  fail_unless ( retrieved.getValue(0) == "http://" );

  i = node->unsetAttributes();

  fail_unless ( i == LIBSBML_OPERATION_SUCCESS );
  fail_unless ( node->getNumAttributes() == 0 );
  fail_unless ( node->getAttributes().isEmpty() == true );
}
END_TEST
  

START_TEST (test_uncertml_children)
{
  fail_unless ( node->getNumChildren() == 0 );
  
  node->setElementName("parent");

  UncertMLNode * child = new UncertMLNode();
  child->setElementName("child");

  int i = node->addChild(child);

  fail_unless ( i == LIBSBML_OPERATION_SUCCESS );
  fail_unless ( node->getNumChildren() == 1 );

  const UncertMLNode * retrieved = node->getChild(0);

  fail_unless ( retrieved != NULL );
  fail_unless ( retrieved->getElementName() == "child" );  
}
END_TEST


Suite *
create_suite_TestUncertMLNodeAttributes (void)
{ 
  TCase *tcase = tcase_create("UncertMLNodeAttributes");
  Suite *suite = suite_create("UncertMLNodeAttributes");
  
  tcase_add_checked_fixture(tcase, UncertMLTest_setup, UncertMLTest_teardown);

  tcase_add_test(tcase, test_uncertml_elementName);
  tcase_add_test(tcase, test_uncertml_attributes);
  tcase_add_test(tcase, test_uncertml_children);

  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND

