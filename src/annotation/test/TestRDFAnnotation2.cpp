/**
 * \file    TestRDFAnnotation.cpp
 * \brief   fomula units data unit tests
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


#include "sbml/common/common.h"
#include "sbml/common/extern.h"

#include "sbml/SBMLReader.h"
#include "sbml/SBMLTypes.h"

#include "sbml/SBMLDocument.h"
#include "sbml/Model.h"
#include "sbml/SBMLTypeCodes.h"

#include "RDFAnnotation.h"
#include "ModelHistory.h"

#include <check.h>

static char*         S;

ostringstream*   OSS2;
XMLOutputStream* XOS2;
static Model *m2;
static SBMLDocument* d2;

extern char *TestDataDirectory;

/* 
 * tests the results from rdf annotations
 */
CK_CPPSTART
static 

void
RDFAnnotation2_setup (void)
{
  d2 = new SBMLDocument();
  S = 0;
  OSS2 = new ostringstream;
  XOS2 = new XMLOutputStream(*OSS2);
 
  char *filename = safe_strcat(TestDataDirectory, "annotation2.xml");

  d2 = readSBML(filename);
  m2 = d2->getModel();


}


void
RDFAnnotation2_teardown (void)
{
//  delete d;
  free(S);

  delete OSS2;
  delete XOS2;
}

static bool
equals1 (const char* expected, const char* actual)
{
  if ( !strcmp(expected, actual) ) return true;

  printf( "\nStrings are not equal:\n"  );
  printf( "Expected:\n[%s]\n", expected );
  printf( "Actual:\n[%s]\n"  , actual   );

  return false;
}


static bool
equals (const char* expected)
{
  return equals1(expected, OSS2->str().c_str());
}

START_TEST (test_RDFAnnotation2_getModelHistory)
{
  ModelHistory * history = m2->getModelHistory();

  fail_unless(history != NULL);

  ModelCreator * mc = (ModelCreator * )(history->getCreator()->get(0));

  fail_unless(!strcmp(ModelCreator_getFamilyName(mc), "Hucka"));
  fail_unless(!strcmp(ModelCreator_getGivenName(mc), "Mike"));
  fail_unless(!strcmp(ModelCreator_getEmail(mc), "mhucka@caltech.edu"));
  fail_unless(!strcmp(ModelCreator_getOrganisation(mc), "BNMC"));

  //ModelCreator * mc1 = (ModelCreator * )(history->getCreator()->get(1));

  //fail_unless(!strcmp(ModelCreator_getFamilyName(mc1), "Keating"));
  //fail_unless(!strcmp(ModelCreator_getGivenName(mc1), "Sarah"));
  //fail_unless(!strcmp(ModelCreator_getEmail(mc1), "skeating@caltech.edu"));
  //fail_unless(!strcmp(ModelCreator_getOrganisation(mc1), "UH"));

  Date * date = history->getCreatedDate();
  fail_unless(Date_getYear(date) == 2005);
  fail_unless(Date_getMonth(date) == 2);
  fail_unless(Date_getDay(date) == 2);
  fail_unless(Date_getHour(date) == 14);
  fail_unless(Date_getMinute(date) == 56);
  fail_unless(Date_getSecond(date) == 11);
  fail_unless(Date_getSignOffset(date) == 0);
  fail_unless(Date_getHoursOffset(date) == 0);
  fail_unless(Date_getMinutesOffset(date) == 0);
  fail_unless(!strcmp(Date_getDateAsString(date), "2005-02-02T14:56:11Z"));

  date = history->getModifiedDate();
  fail_unless(Date_getYear(date) == 2006);
  fail_unless(Date_getMonth(date) == 5);
  fail_unless(Date_getDay(date) == 30);
  fail_unless(Date_getHour(date) == 10);
  fail_unless(Date_getMinute(date) == 46);
  fail_unless(Date_getSecond(date) == 2);
  fail_unless(Date_getSignOffset(date) == 0);
  fail_unless(Date_getHoursOffset(date) == 0);
  fail_unless(Date_getMinutesOffset(date) == 0);
  fail_unless(!strcmp(Date_getDateAsString(date), "2006-05-30T10:46:02Z"));

  delete history;
}
END_TEST



Suite *
create_suite_RDFAnnotation2 (void)
{
  Suite *suite = suite_create("RDFAnnotation2");
  TCase *tcase = tcase_create("RDFAnnotation2");

  tcase_add_checked_fixture(tcase,
                            RDFAnnotation2_setup,
                            RDFAnnotation2_teardown);

  tcase_add_test(tcase, test_RDFAnnotation2_getModelHistory );
  suite_add_tcase(suite, tcase);

  return suite;
}


CK_CPPEND
