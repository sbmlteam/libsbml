/**
 * \file    TestXMLError.c
 * \brief   XMLError unit tests
 * \author  Sarah Keating
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
#include "XMLError.h"

#include <check.h>


START_TEST (test_XMLError_create)
{
  XMLError_t *error = XMLError_create();
  fail_unless(error != NULL);
  XMLError_free(error);

  error = XMLError_createWithIdAndMessage(23, "My message");
  fail_unless( strcmp(XMLError_getMessage(error), "My message") == 0 );
  fail_unless( XMLError_getId(error) == 23 );
  XMLError_free(error);
}
END_TEST


Suite *
create_suite_XMLError (void)
{
  Suite *suite = suite_create("XMLError");
  TCase *tcase = tcase_create("XMLError");

  tcase_add_test( tcase, test_XMLError_create  );
  suite_add_tcase(suite, tcase);

  return suite;
}

