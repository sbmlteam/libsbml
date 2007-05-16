/**
 * \file    TestXMLTriple.c
 * \brief   XMLTriple unit tests
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
#include "XMLTriple.h"

#include <check.h>


START_TEST (test_XMLTriple_create)
{
  XMLTriple_t *t = XMLTriple_create();
  fail_unless(t != NULL);
  fail_unless(XMLTriple_isEmpty(t) != 0);
  XMLTriple_free(t);

  t = XMLTriple_createWith("attr", "uri", "prefix");
  fail_unless( strcmp(XMLTriple_getName(t), "attr") == 0 );
  fail_unless( strcmp(XMLTriple_getURI(t), "uri") == 0 );
  fail_unless( strcmp(XMLTriple_getPrefix(t), "prefix") == 0 );
  fail_unless(XMLTriple_isEmpty(t) == 0);
  XMLTriple_free(t);
}
END_TEST


Suite *
create_suite_XMLTriple (void)
{
  Suite *suite = suite_create("XMLTriple");
  TCase *tcase = tcase_create("XMLTriple");

  tcase_add_test( tcase, test_XMLTriple_create  );
  suite_add_tcase(suite, tcase);

  return suite;
}

