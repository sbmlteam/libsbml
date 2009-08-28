/**
 * \file    TestModelCreator_newSetters.cpp
 * \brief   ModelCreator unit tests
 * \author  Sarah Keating
 *
 * $Id: TestModelCreator_newSetters.c 7758 2008-07-26 12:41:26Z ajouraku $
 * $HeadURL: https://sbml.svn.sourceforge.net/svnroot/sbml/trunk/libsbml/src/annotation/test/TestModelCreator_newSetters.c $
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
#include <sbml/common/extern.h>
#include <sbml/util/List.h>
#include <sbml/annotation/ModelHistory.h>
#include <sbml/xml/XMLNode.h>


#include <check.h>

START_TEST (test_ModelCreator_setFamilyName)
{
  ModelCreator_t * mc = ModelCreator_create();
  fail_unless(mc != NULL);

  int i = ModelCreator_setFamilyName(mc, "Keating");

  fail_unless( i == LIBSBML_OPERATION_SUCCESS );
  fail_unless(ModelCreator_isSetFamilyName(mc) == 1);
  fail_unless(!strcmp(ModelCreator_getFamilyName(mc), "Keating"));

  i = ModelCreator_setFamilyName(mc, "");

  fail_unless( i == LIBSBML_OPERATION_SUCCESS );
  fail_unless(ModelCreator_isSetFamilyName(mc) == 0);

  i = ModelCreator_setFamilyName(mc, "Keating");

  fail_unless(ModelCreator_isSetFamilyName(mc) == 1);

  i = ModelCreator_unsetFamilyName(mc);

  fail_unless( i == LIBSBML_OPERATION_SUCCESS );
  fail_unless(ModelCreator_isSetFamilyName(mc) == 0);

  ModelCreator_free(mc);
}
END_TEST


START_TEST (test_ModelCreator_setGivenName)
{
  ModelCreator_t * mc = ModelCreator_create();
  fail_unless(mc != NULL);

  int i = ModelCreator_setGivenName(mc, "Sarah");

  fail_unless( i == LIBSBML_OPERATION_SUCCESS );
  fail_unless(ModelCreator_isSetGivenName(mc) == 1);
  fail_unless(!strcmp(ModelCreator_getGivenName(mc), "Sarah"));

  i = ModelCreator_setGivenName(mc, "");

  fail_unless( i == LIBSBML_OPERATION_SUCCESS );
  fail_unless(ModelCreator_isSetGivenName(mc) == 0);

  i = ModelCreator_setGivenName(mc, "Sarah");

  fail_unless(ModelCreator_isSetGivenName(mc) == 1);

  i = ModelCreator_unsetGivenName(mc);

  fail_unless( i == LIBSBML_OPERATION_SUCCESS );
  fail_unless(ModelCreator_isSetGivenName(mc) == 0);

  ModelCreator_free(mc);
}
END_TEST


START_TEST (test_ModelCreator_setEmail)
{
  ModelCreator_t * mc = ModelCreator_create();
  fail_unless(mc != NULL);

  int i = ModelCreator_setEmail(mc, "Keating");

  fail_unless( i == LIBSBML_OPERATION_SUCCESS );
  fail_unless(ModelCreator_isSetEmail(mc) == 1);
  fail_unless(!strcmp(ModelCreator_getEmail(mc), "Keating"));

  i = ModelCreator_setEmail(mc, "");

  fail_unless( i == LIBSBML_OPERATION_SUCCESS );
  fail_unless(ModelCreator_isSetEmail(mc) == 0);

  i = ModelCreator_setEmail(mc, "Keating");

  fail_unless(ModelCreator_isSetEmail(mc) == 1);

  i = ModelCreator_unsetEmail(mc);

  fail_unless( i == LIBSBML_OPERATION_SUCCESS );
  fail_unless(ModelCreator_isSetEmail(mc) == 0);

  ModelCreator_free(mc);
}
END_TEST


START_TEST (test_ModelCreator_setOrganization)
{
  ModelCreator_t * mc = ModelCreator_create();
  fail_unless(mc != NULL);

  int i = ModelCreator_setOrganization(mc, "Caltech");

  fail_unless( i == LIBSBML_OPERATION_SUCCESS );
  fail_unless(ModelCreator_isSetOrganization(mc) == 1);
  fail_unless(!strcmp(ModelCreator_getOrganization(mc), "Caltech"));

  i = ModelCreator_setOrganization(mc, "");

  fail_unless( i == LIBSBML_OPERATION_SUCCESS );
  fail_unless(ModelCreator_isSetOrganization(mc) == 0);

  i = ModelCreator_setOrganization(mc, "Caltech");

  fail_unless(ModelCreator_isSetOrganization(mc) == 1);

  i = ModelCreator_unsetOrganization(mc);

  fail_unless( i == LIBSBML_OPERATION_SUCCESS );
  fail_unless(ModelCreator_isSetOrganization(mc) == 0);

  ModelCreator_free(mc);
}
END_TEST


Suite *
create_suite_ModelCreator_newSetters (void)
{
  Suite *suite = suite_create("ModelCreator_newSetters");
  TCase *tcase = tcase_create("ModelCreator_newSetters");


  tcase_add_test( tcase, test_ModelCreator_setFamilyName  );
  tcase_add_test( tcase, test_ModelCreator_setGivenName  );
  tcase_add_test( tcase, test_ModelCreator_setEmail  );
  tcase_add_test( tcase, test_ModelCreator_setOrganization  );

  suite_add_tcase(suite, tcase);

  return suite;
}


END_C_DECLS
