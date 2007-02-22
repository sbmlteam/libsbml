/**
 * \file    TestXMLNamespaces.c
 * \brief   XMLNamespaces unit tests
 * \author  Michael Hucka <mhucka@caltech.edu>
 *
 * $Id$
 * $Source$
 */
/* Copyright 2007 California Institute of Technology.
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
#include "XMLNamespaces.h"

#include <check.h>


static XMLNamespaces_t *NS;

void
XMLNamespacesTest_setup (void)
{
  NS = XMLNamespaces_create();

  if (NS == NULL)
  {
    fail("XMLNamespacesTest_setup() failed to create a XMLNamespaces object.");
  }
}

void
XMLNamespacesTest_teardown (void)
{
  XMLNamespaces_free(NS);
}


START_TEST (test_XMLNamespaces_baseline)
{
  fail_unless( XMLNamespaces_getLength(NS) == 0 );
  fail_unless( XMLNamespaces_isEmpty(NS) == 1 );
}
END_TEST


START_TEST (test_XMLNamespaces_add)
{
  fail_unless( XMLNamespaces_getLength(NS) == 0 );
  fail_unless( XMLNamespaces_isEmpty(NS) == 1 );

  XMLNamespaces_add(NS, "http://test1.org/", "test1");
  fail_unless( XMLNamespaces_getLength(NS) == 1 );
  fail_unless( XMLNamespaces_isEmpty(NS) == 0 );

  XMLNamespaces_add(NS, "http://test2.org/", "test2");
  fail_unless( XMLNamespaces_getLength(NS) == 2 );
  fail_unless( XMLNamespaces_isEmpty(NS) == 0 );

  fail_if( XMLNamespaces_getIndex(NS, "http://test1.org/") == -1);
}
END_TEST


START_TEST (test_XMLNamespaces_get)
{
  XMLNamespaces_add(NS, "http://test1.org/", "test1");    /* index 0 */
  XMLNamespaces_add(NS, "http://test2.org/", "test2");    /* index 1 */
  XMLNamespaces_add(NS, "http://test3.org/", "test3");    /* index 1 */
  XMLNamespaces_add(NS, "http://test4.org/", "test4");    /* index 1 */
  XMLNamespaces_add(NS, "http://test5.org/", "test5");    /* index 1 */
  XMLNamespaces_add(NS, "http://test6.org/", "test6");    /* index 1 */
  XMLNamespaces_add(NS, "http://test7.org/", "test7");    /* index 1 */
  XMLNamespaces_add(NS, "http://test8.org/", "test8");    /* index 1 */
  XMLNamespaces_add(NS, "http://test9.org/", "test9");    /* index 1 */

  fail_unless( XMLNamespaces_getLength(NS) == 9 );

  fail_unless( XMLNamespaces_getIndex(NS, "http://test1.org/") == 0 );
  fail_unless( strcmp(XMLNamespaces_getPrefix(NS, 1), "test2") == 0 );
  fail_unless( strcmp(XMLNamespaces_getPrefixByURI(NS, "http://test1.org/"),
		      "test1") == 0 );
  fail_unless( strcmp(XMLNamespaces_getURI(NS, 1), "http://test2.org/") == 0 );
  fail_unless( strcmp(XMLNamespaces_getURIByPrefix(NS, "test2"),
		      "http://test2.org/") == 0 );
}
END_TEST


Suite *
create_suite_XMLNamespaces (void)
{
  Suite *suite = suite_create("XMLNamespaces");
  TCase *tcase = tcase_create("XMLNamespaces");

  tcase_add_checked_fixture( tcase,
                             XMLNamespacesTest_setup,
                             XMLNamespacesTest_teardown );

  tcase_add_test( tcase, test_XMLNamespaces_baseline  );
  tcase_add_test( tcase, test_XMLNamespaces_add       );
  tcase_add_test( tcase, test_XMLNamespaces_get       );
  suite_add_tcase(suite, tcase);

  return suite;
}

