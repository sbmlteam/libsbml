/**
 * \file    TestReadSBML.cpp
 * \brief   Read SBML unit tests
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2002 California Institute of Technology and Japan Science and
 * Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is
 * provided in the file named "LICENSE.txt" included with this software
 * distribution.  It is also available online at
 * http://sbml.org/software/libsbml/license.html
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 */
#include "common/common.h"
#include "XMLTriple.h"
#include "XMLNamespaces.h"


#include <check.h>
using namespace std;

CK_CPPSTART
START_TEST ( test_NS_copyConstructor )
{
  XMLNamespaces * ns = new XMLNamespaces();
  ns->add("http://test1.org/", "test1");
  
  fail_unless( ns->getLength() == 1 );
  fail_unless( ns->isEmpty() == 0 );
  fail_unless(ns->getPrefix(0) == "test1");
  fail_unless(ns->getURI("test1") == "http://test1.org/");

  XMLNamespaces * ns2 = new XMLNamespaces(*ns);

  fail_unless( ns2->getLength() == 1 );
  fail_unless( ns2->isEmpty() == 0 );
  fail_unless(ns2->getPrefix(0) == "test1");
  fail_unless(ns2->getURI("test1") == "http://test1.org/");

  delete ns2;
  delete ns;
}
END_TEST

START_TEST ( test_NS_assignmentOperator )
{
  XMLNamespaces * ns = new XMLNamespaces();
  ns->add("http://test1.org/", "test1");
  
  fail_unless( ns->getLength() == 1 );
  fail_unless( ns->isEmpty() == 0 );
  fail_unless(ns->getPrefix(0) == "test1");
  fail_unless(ns->getURI("test1") == "http://test1.org/");

  XMLNamespaces * ns2 = new XMLNamespaces();
  (*ns2) = *ns;

  fail_unless( ns2->getLength() == 1 );
  fail_unless( ns2->isEmpty() == 0 );
  fail_unless(ns2->getPrefix(0) == "test1");
  fail_unless(ns2->getURI("test1") == "http://test1.org/");

  delete ns2;
  delete ns;
}
END_TEST


START_TEST ( test_NS_clone )
{
  XMLNamespaces * ns = new XMLNamespaces();
  ns->add("http://test1.org/", "test1");
  
  fail_unless( ns->getLength() == 1 );
  fail_unless( ns->isEmpty() == 0 );
  fail_unless(ns->getPrefix(0) == "test1");
  fail_unless(ns->getURI("test1") == "http://test1.org/");

  XMLNamespaces * ns2 = static_cast<XMLNamespaces*>(ns->clone());

  fail_unless( ns2->getLength() == 1 );
  fail_unless( ns2->isEmpty() == 0 );
  fail_unless(ns2->getPrefix(0) == "test1");
  fail_unless(ns2->getURI("test1") == "http://test1.org/");

  delete ns2;
  delete ns;
}
END_TEST


START_TEST ( test_Triple_copyConstructor )
{
  XMLTriple *t = new XMLTriple("sarah", "http://foo.org/", "bar");

  fail_unless (t->getName() == "sarah");
  fail_unless (t->getURI() == "http://foo.org/");
  fail_unless (t->getPrefix() == "bar");

  XMLTriple *t2 = new XMLTriple(*t);

  fail_unless (t2->getName() == "sarah");
  fail_unless (t2->getURI() == "http://foo.org/");
  fail_unless (t2->getPrefix() == "bar");

  delete t;
  delete t2;
}
END_TEST


START_TEST ( test_Triple_assignmentOperator )
{
  XMLTriple *t = new XMLTriple("sarah", "http://foo.org/", "bar");

  fail_unless (t->getName() == "sarah");
  fail_unless (t->getURI() == "http://foo.org/");
  fail_unless (t->getPrefix() == "bar");

  XMLTriple *t2 = new XMLTriple;
  (*t2) = *t;

  fail_unless (t2->getName() == "sarah");
  fail_unless (t2->getURI() == "http://foo.org/");
  fail_unless (t2->getPrefix() == "bar");

  delete t;
  delete t2;
}
END_TEST

START_TEST ( test_Triple_clone )
{
  XMLTriple *t = new XMLTriple("sarah", "http://foo.org/", "bar");

  fail_unless (t->getName() == "sarah");
  fail_unless (t->getURI() == "http://foo.org/");
  fail_unless (t->getPrefix() == "bar");

  XMLTriple * t2 = static_cast<XMLTriple*>(t->clone());

  fail_unless (t2->getName() == "sarah");
  fail_unless (t2->getURI() == "http://foo.org/");
  fail_unless (t2->getPrefix() == "bar");

  delete t;
  delete t2;
}
END_TEST

Suite *
create_suite_CopyAndClone (void)
{
  Suite *suite = suite_create("CopyAndClone");
  TCase *tcase = tcase_create("CopyAndClone");

  tcase_add_test( tcase, test_NS_copyConstructor );
  tcase_add_test( tcase, test_NS_assignmentOperator );
  tcase_add_test( tcase, test_NS_clone );
  tcase_add_test( tcase, test_Triple_copyConstructor );
  tcase_add_test( tcase, test_Triple_assignmentOperator );
  tcase_add_test( tcase, test_Triple_clone );
  //tcase_add_test( tcase, test_ModelHistory_copyConstructor );
  //tcase_add_test( tcase, test_ModelHistory_assignmentOperator );
  //tcase_add_test( tcase, test_ModelHistory_clone );
  suite_add_tcase(suite, tcase);

  return suite;
}
CK_CPPEND
