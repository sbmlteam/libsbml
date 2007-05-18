/**
 * \file    TestModelHistory.cpp
 * \brief   ModelHistory unit tests
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
#include "sbml/common/extern.h"
#include "util/List.h"
#include "ModelHistory.h"
#include "../../xml/XMLNode.h"


#include <check.h>



START_TEST (test_Date_create)
{
  Date_t * date = Date_createFromValues(2005, 12, 30, 12, 15, 45, 1, 2, 0);

  fail_unless(date != NULL);
  fail_unless(Date_getYear(date) == 2005);
  fail_unless(Date_getMonth(date) == 12);
  fail_unless(Date_getDay(date) == 30);
  fail_unless(Date_getHour(date) == 12);
  fail_unless(Date_getMinute(date) == 15);
  fail_unless(Date_getSecond(date) == 45);
  fail_unless(Date_getSignOffset(date) == 1);
  fail_unless(Date_getHoursOffset(date) == 2);
  fail_unless(Date_getMinutesOffset(date) == 0);

  Date_free(date);
}
END_TEST

START_TEST (test_Date_createFromString)
{
  const char * dd = "2012-12-02T14:56:11Z";

  Date_t * date = Date_createFromString(dd);

  fail_unless(date != NULL);
  fail_unless(!strcmp(Date_getDateAsString(date), "2012-12-02T14:56:11Z"));
  fail_unless(Date_getYear(date) == 2012);
  fail_unless(Date_getMonth(date) == 12);
  fail_unless(Date_getDay(date) == 2);
  fail_unless(Date_getHour(date) == 14);
  fail_unless(Date_getMinute(date) == 56);
  fail_unless(Date_getSecond(date) == 11);
  fail_unless(Date_getSignOffset(date) == 0);
  fail_unless(Date_getHoursOffset(date) == 0);
  fail_unless(Date_getMinutesOffset(date) == 0);

  Date_free(date);
}
END_TEST

START_TEST (test_Date_setters)
{
  Date_t * date = Date_createFromValues(2005, 12, 30, 12, 15, 45, 1, 2, 0);
  fail_unless(date != NULL);

  Date_setYear(date, 2012);
  Date_setMonth(date, 3);
  Date_setDay(date, 28);
  Date_setHour(date, 23);
  Date_setMinute(date, 4);
  Date_setSecond(date, 32);
  Date_setSignOffset(date, 1);
  Date_setHoursOffset(date, 2);
  Date_setMinutesOffset(date, 32);

  fail_unless(Date_getYear(date) == 2012);
  fail_unless(Date_getMonth(date) == 3);
  fail_unless(Date_getDay(date) == 28);
  fail_unless(Date_getHour(date) == 23);
  fail_unless(Date_getMinute(date) == 4);
  fail_unless(Date_getSecond(date) == 32);
  fail_unless(Date_getSignOffset(date) == 1);
  fail_unless(Date_getHoursOffset(date) == 2);
  fail_unless(Date_getMinutesOffset(date) == 32);
  fail_unless(!strcmp(Date_getDateAsString(date), "2012-03-28T23:04:32+02:32"));

  Date_free(date);
}
END_TEST

START_TEST (test_Date_getDateAsString)
{
  char * dd = "2005-02-02T14:56:11Z";

  Date_t * date = Date_createFromString(dd);

  fail_unless(date != NULL);
  fail_unless(Date_getYear(date) == 2005);
  fail_unless(Date_getMonth(date) == 2);
  fail_unless(Date_getDay(date) == 2);
  fail_unless(Date_getHour(date) == 14);
  fail_unless(Date_getMinute(date) == 56);
  fail_unless(Date_getSecond(date) == 11);
  fail_unless(Date_getSignOffset(date) == 0);
  fail_unless(Date_getHoursOffset(date) == 0);
  fail_unless(Date_getMinutesOffset(date) == 0);

  Date_setYear(date, 2012);
  Date_setMonth(date, 3);
  Date_setDay(date, 28);
  Date_setHour(date, 23);
  Date_setMinute(date, 4);
  Date_setSecond(date, 32);
  Date_setSignOffset(date, 1);
  Date_setHoursOffset(date, 2);
  Date_setMinutesOffset(date, 32);

  fail_unless(!strcmp(Date_getDateAsString(date), "2012-03-28T23:04:32+02:32"));

  Date_free(date);
}
END_TEST

START_TEST(test_ModelCreator_create)
{
  ModelCreator_t * mc = ModelCreator_create();

  fail_unless(mc != NULL);

  ModelCreator_free(mc);

}
END_TEST


START_TEST(test_ModelCreator_setters)
{
  ModelCreator_t * mc = ModelCreator_create();

  fail_unless(mc != NULL);

  fail_unless(ModelCreator_isSetFamilyName(mc) == 0);
  fail_unless(ModelCreator_isSetGivenName(mc) == 0);
  fail_unless(ModelCreator_isSetEmail(mc) == 0);
  fail_unless(ModelCreator_isSetOrganisation(mc) == 0);

  ModelCreator_setFamilyName(mc, "Keating");
  ModelCreator_setGivenName(mc, "Sarah");
  ModelCreator_setEmail(mc, "sbml-team@caltech.edu");
  ModelCreator_setOrganisation(mc, "UH");

  fail_unless(!strcmp(ModelCreator_getFamilyName(mc), "Keating"));
  fail_unless(!strcmp(ModelCreator_getGivenName(mc), "Sarah"));
  fail_unless(!strcmp(ModelCreator_getEmail(mc), "sbml-team@caltech.edu"));
  fail_unless(!strcmp(ModelCreator_getOrganisation(mc), "UH"));

  fail_unless(ModelCreator_isSetFamilyName(mc) == 1);
  fail_unless(ModelCreator_isSetGivenName(mc) == 1);
  fail_unless(ModelCreator_isSetEmail(mc) == 1);
  fail_unless(ModelCreator_isSetOrganisation(mc) == 1);

  ModelCreator_unsetFamilyName(mc);
  ModelCreator_unsetGivenName(mc);
  ModelCreator_unsetEmail(mc);
  ModelCreator_unsetOrganisation(mc);

  fail_unless(!strcmp(ModelCreator_getFamilyName(mc), ""));
  fail_unless(!strcmp(ModelCreator_getGivenName(mc), ""));
  fail_unless(!strcmp(ModelCreator_getEmail(mc), ""));
  fail_unless(!strcmp(ModelCreator_getOrganisation(mc), ""));

  fail_unless(ModelCreator_isSetFamilyName(mc) == 0);
  fail_unless(ModelCreator_isSetGivenName(mc) == 0);
  fail_unless(ModelCreator_isSetEmail(mc) == 0);
  fail_unless(ModelCreator_isSetOrganisation(mc) == 0);
  ModelCreator_free(mc);

}
END_TEST

START_TEST (test_ModelHistory_create)
{
  ModelHistory_t * history = ModelHistory_create();

  fail_unless(history != NULL);
  fail_unless(ModelHistory_getListCreators(history) != NULL);
  fail_unless(ModelHistory_getCreatedDate(history) == NULL);
  fail_unless(ModelHistory_getModifiedDate(history) == NULL);

  ModelHistory_free(history);
}
END_TEST

START_TEST (test_ModelHistory_addCreator)
{
  ModelCreator_t * newMC;
  ModelHistory_t * history = ModelHistory_create();

  fail_unless(ModelHistory_getNumCreators(history) == 0);

  fail_unless(history != NULL);

  ModelCreator_t * mc = ModelCreator_create();
  fail_unless(mc != NULL);

  ModelCreator_setFamilyName(mc, "Keating");
  ModelCreator_setGivenName(mc, "Sarah");
  ModelCreator_setEmail(mc, "sbml-team@caltech.edu");
  ModelCreator_setOrganisation(mc, "UH");

  ModelHistory_addCreator(history, mc);

  fail_unless(ModelHistory_getNumCreators(history) == 1);
  ModelCreator_free(mc);

  newMC = List_get(ModelHistory_getListCreators(history), 0);
  fail_unless(newMC != NULL);

  fail_unless(!strcmp(ModelCreator_getFamilyName(newMC), "Keating"));
  fail_unless(!strcmp(ModelCreator_getGivenName(newMC), "Sarah"));
  fail_unless(!strcmp(ModelCreator_getEmail(newMC), "sbml-team@caltech.edu"));
  fail_unless(!strcmp(ModelCreator_getOrganisation(newMC), "UH"));

  ModelCreator_free(newMC);

  ModelHistory_free(history);
}
END_TEST

START_TEST (test_ModelHistory_setCreatedDate)
{
  ModelHistory_t * history = ModelHistory_create();

  fail_unless(history != NULL);

  fail_unless(ModelHistory_isSetCreatedDate(history) == 0);

  Date_t * date = Date_createFromValues(2005, 12, 30, 12, 15, 45, 1, 2, 0);
  
  ModelHistory_setCreatedDate(history, date);
  fail_unless(ModelHistory_isSetCreatedDate(history) == 1);

  Date_free(date);

  Date_t * newdate = ModelHistory_getCreatedDate(history);
  fail_unless(Date_getYear(newdate) == 2005);
  fail_unless(Date_getMonth(newdate) == 12);
  fail_unless(Date_getDay(newdate) == 30);
  fail_unless(Date_getHour(newdate) == 12);
  fail_unless(Date_getMinute(newdate) == 15);
  fail_unless(Date_getSecond(newdate) == 45);
  fail_unless(Date_getSignOffset(newdate) == 1);
  fail_unless(Date_getHoursOffset(newdate) == 2);
  fail_unless(Date_getMinutesOffset(newdate) == 0);

  ModelHistory_free(history);

}
END_TEST


START_TEST (test_ModelHistory_setModifiedDate)
{
  ModelHistory_t * history = ModelHistory_create();

  fail_unless(history != NULL);
  fail_unless(ModelHistory_isSetModifiedDate(history) == 0);

  Date_t * date = Date_createFromValues(2005, 12, 30, 12, 15, 45, 1, 2, 0);
  
  ModelHistory_setModifiedDate(history, date);
//  Date_free(date);
  fail_unless(ModelHistory_isSetModifiedDate(history) == 1);

  Date_t * newdate = ModelHistory_getModifiedDate(history);
  fail_unless(Date_getYear(newdate) == 2005);
  fail_unless(Date_getMonth(newdate) == 12);
  fail_unless(Date_getDay(newdate) == 30);
  fail_unless(Date_getHour(newdate) == 12);
  fail_unless(Date_getMinute(newdate) == 15);
  fail_unless(Date_getSecond(newdate) == 45);
  fail_unless(Date_getSignOffset(newdate) == 1);
  fail_unless(Date_getHoursOffset(newdate) == 2);
  fail_unless(Date_getMinutesOffset(newdate) == 0);

  ModelHistory_free(history);
}
END_TEST


Suite *
create_suite_ModelHistory (void)
{
  Suite *suite = suite_create("ModelHistory");
  TCase *tcase = tcase_create("ModelHistory");


  tcase_add_test( tcase, test_Date_create  );
  tcase_add_test( tcase, test_Date_createFromString  );
  tcase_add_test( tcase, test_Date_setters  );
  tcase_add_test( tcase, test_Date_getDateAsString  );
  tcase_add_test( tcase, test_ModelCreator_create  );
  tcase_add_test( tcase, test_ModelCreator_setters  );
  tcase_add_test( tcase, test_ModelHistory_create  );
  tcase_add_test( tcase, test_ModelHistory_addCreator  );
  tcase_add_test( tcase, test_ModelHistory_setCreatedDate  );
  tcase_add_test( tcase, test_ModelHistory_setModifiedDate  );
  suite_add_tcase(suite, tcase);

  return suite;
}


END_C_DECLS
