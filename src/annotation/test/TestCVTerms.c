/**
 * \file    TestCVTerms.cpp
 * \brief   CVTerms unit tests
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
#include "CVTerm.h"

#include <check.h>


START_TEST (test_CVTerm_create)
{
  CVTerm_t *term = CVTerm_createWithQualifierType(MODEL_QUALIFIER);

  fail_unless(term != NULL);
  fail_unless(CVTerm_getQualifierType(term) == MODEL_QUALIFIER);
  CVTerm_free(term);

}
END_TEST

//START_TEST (test_CVTerm_createFromNode)
//{
//  CVTerm_t *term = CVTerm_createWithQualifierType(MODEL_QUALIFIER);
//
//  fail_unless(term != NULL);
//  fail_unless(CVTerm_getQualifierType(term) == MODEL_QUALIFIER);
//  CVTerm_free(term);
//
//}
//END_TEST



Suite *
create_suite_CVTerms (void)
{
  Suite *suite = suite_create("CVTerms");
  TCase *tcase = tcase_create("CVTerms");

  tcase_add_test( tcase, test_CVTerm_create  );
  suite_add_tcase(suite, tcase);

  return suite;
}


END_C_DECLS
