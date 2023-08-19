/**
 * \file    TestErrorLog.c
 * \brief   ErrorLog unit tests
 * \author  Ben Bornstein
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
 *
 * Copyright (C) 2019 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations: 
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

#include <sbml/SBase.h>
#include <sbml/SBMLErrorLog.h>
#include <sbml/xml/XMLError.h>
#include <sbml/xml/XMLNamespaces.h>
#include <sbml/SBMLDocument.h>

#include <check.h>



#include <sbml/common/extern.h>

LIBSBML_CPP_NAMESPACE_USE


BEGIN_C_DECLS

START_TEST(test_SBMLErrorLog_removeAll)
{
  SBMLErrorLog_t *log = SBMLErrorLog_create();
  XMLError_t *e = XMLError_createWithIdAndMessage(1234, "1");

  SBMLErrorLog_add(log, (const SBMLError_t*)(e));
  SBMLErrorLog_add(log, (const SBMLError_t*)(e));
  SBMLErrorLog_add(log, (const SBMLError_t*)(e));
  SBMLErrorLog_add(log, (const SBMLError_t*)(e));
  
  fail_unless(SBMLErrorLog_contains(log, 1234) == true);
  fail_unless(SBMLErrorLog_getNumErrors(log) == 4);
  
  SBMLErrorLog_remove(log, 1234);
  fail_unless(SBMLErrorLog_contains(log, 1234) == true);
  fail_unless(SBMLErrorLog_getNumErrors(log) == 3);

  SBMLErrorLog_removeAll(log, 1234);
  fail_unless(SBMLErrorLog_contains(log, 1234) == false);
  fail_unless(SBMLErrorLog_getNumErrors(log) == 0);

  XMLError_free(e);
  SBMLErrorLog_free(log);

}
END_TEST


START_TEST(test_SBMLErrorLog_add_clear)
{
  SBMLErrorLog_t *log = SBMLErrorLog_create();
  XMLError_t *e = XMLError_createWithIdAndMessage(1234, "1");
  XMLError_t *e1 = XMLError_createWithIdAndMessage(2345, "1");

  SBMLErrorLog_add(log, (const SBMLError_t*)(e));
  SBMLErrorLog_add(log, (const SBMLError_t*)(e1));
  SBMLErrorLog_add(log, (const SBMLError_t*)(e));
  SBMLErrorLog_add(log, (const SBMLError_t*)(e));

  fail_unless(SBMLErrorLog_contains(log, 1234) == true);
  fail_unless(SBMLErrorLog_contains(log, 2345) == true);
  fail_unless(SBMLErrorLog_getNumErrors(log) == 4);

  SBMLErrorLog_remove(log, 2345);
  fail_unless(SBMLErrorLog_contains(log, 1234) == true);
  fail_unless(SBMLErrorLog_contains(log, 2345) == false);
  fail_unless(SBMLErrorLog_getNumErrors(log) == 3);

  SBMLErrorLog_clearLog(log);
  fail_unless(SBMLErrorLog_getNumErrors(log) == 0);

  XMLError_free(e);
  XMLError_free(e1);
  SBMLErrorLog_free(log);
}
END_TEST


START_TEST(test_SBMLErrorLog_get)
{
  SBMLErrorLog_t *log = SBMLErrorLog_create();
  XMLError_t *e = XMLError_createWithIdAndMessage(1234, "1");
  XMLError_t *e1 = XMLError_createWithIdAndMessage(2345, "1");

  SBMLErrorLog_add(log, (const SBMLError_t*)(e));
  SBMLErrorLog_add(log, (const SBMLError_t*)(e1));

  const SBMLError_t* r = SBMLErrorLog_getError(log, 1);

  fail_unless(r != NULL);
  fail_unless(XMLError_getSeverity(r) == 1);
  fail_unless(XMLError_getErrorId(r) == 2345);

  const SBMLError_t* r1 = SBMLErrorLog_getErrorWithSeverity(log, 0, 1);

  fail_unless(r1 != NULL);
  fail_unless(XMLError_getSeverity(r1) == 1);
  fail_unless(XMLError_getErrorId(r1) == 1234);

  fail_unless(SBMLErrorLog_getNumFailsWithSeverity(log, 1) == 2);

  XMLError_free(e);
  XMLError_free(e1);
  //XMLError_free((SBMLError_t*)(r));
  //XMLError_free((SBMLError_t*)(r1));
  SBMLErrorLog_free(log);
}
END_TEST



Suite *
create_suite_SBMLErrorLog (void)
{
  Suite *suite = suite_create("ErrorLog");
  TCase *tcase = tcase_create("ErrorLog");

  tcase_add_test(tcase, test_SBMLErrorLog_removeAll);
  tcase_add_test(tcase, test_SBMLErrorLog_add_clear);
  tcase_add_test(tcase, test_SBMLErrorLog_get);

  suite_add_tcase(suite, tcase);

  return suite;
}

END_C_DECLS

