/**
 * \file    TestXMLErrorLog.c
 * \brief   XMLErrorLog unit tests
 * \author  Sarah Keating
 *
 * $Id$
 * $HeadURL$
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

#include <sbml/common/common.h>
#include <sbml/xml/XMLError.h>
#include <sbml/xml/XMLErrorLog.h>

#include <check.h>

#if __cplusplus
CK_CPPSTART
#endif

START_TEST (test_XMLErrorLog_create)
{
  XMLErrorLog_t *log = XMLErrorLog_create();
  
  fail_unless(log != NULL);
  fail_unless(XMLErrorLog_getNumErrors(log) == 0);

  XMLErrorLog_free(log);

}
END_TEST

START_TEST (test_XMLErrorLog_add)
{
  XMLErrorLog_t *log = XMLErrorLog_create();
  XMLError_t* error = XMLError_create();

  XMLErrorLog_add( log, error );

  fail_unless( log != NULL );
  fail_unless( XMLErrorLog_getNumErrors(log) == 1 );

  fail_unless( XMLErrorLog_getError(log, 0) != NULL );
  fail_unless( XMLErrorLog_getError(log, 2) == NULL );

  XMLErrorLog_free(log);
}
END_TEST


START_TEST (test_XMLErrorLog_clear)
{
  XMLErrorLog_t *log = XMLErrorLog_create();
  XMLError_t* error = XMLError_create();

  XMLErrorLog_add( log, error );

  XMLErrorLog_clearLog(log);

  fail_unless( log != NULL );
  fail_unless( XMLErrorLog_getNumErrors(log) == 0 );

  XMLErrorLog_free(log);
}
END_TEST

START_TEST (test_XMLErrorLog_accessWithNULL)
{

  XMLErrorLog_add(NULL, NULL);
  XMLErrorLog_clearLog(NULL);
  XMLErrorLog_free(NULL);  

  fail_unless ( XMLErrorLog_getError(NULL, 0) == NULL );
  fail_unless ( XMLErrorLog_getNumErrors(NULL) == 0 );
}
END_TEST

Suite *
create_suite_XMLErrorLog (void)
{
  Suite *suite = suite_create("XMLErrorLog");
  TCase *tcase = tcase_create("XMLErrorLog");

  tcase_add_test( tcase, test_XMLErrorLog_create           );
  tcase_add_test( tcase, test_XMLErrorLog_add              );
  tcase_add_test( tcase, test_XMLErrorLog_clear            );
  tcase_add_test( tcase, test_XMLErrorLog_accessWithNULL   );
  
  suite_add_tcase(suite, tcase);

  return suite;
}

#if __cplusplus
CK_CPPEND
#endif

