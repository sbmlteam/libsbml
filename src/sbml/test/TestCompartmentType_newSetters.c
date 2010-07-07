/**
 * \file    TestCompartmentType_newSetters.c
 * \brief   CompartmentType unit tests for new set function API
 * \author  Sarah Keating
 *
 * $Id$
 * $HeadURL$
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2007 California Institute of Technology.
 * Copyright 2002-2005 California Institute of Technology and
 *                     Japan Science and Technology Corporation.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *----------------------------------------------------------------------- -->*/


#include <sbml/common/common.h>

#include <sbml/SBase.h>
#include <sbml/CompartmentType.h>
#include <sbml/xml/XMLNamespaces.h>
#include <sbml/SBMLDocument.h>

#include <check.h>


static CompartmentType_t *C;


void
CompartmentTypeTest1_setup (void)
{
  C = CompartmentType_create(2, 2);

  if (C == NULL)
  {
    fail("CompartmentType_create() returned a NULL pointer.");
  }
}


void
CompartmentTypeTest1_teardown (void)
{
  CompartmentType_free(C);
}


START_TEST (test_CompartmentType_setId2)
{
  int i = CompartmentType_setId(C, "1cell");

  fail_unless( i == LIBSBML_INVALID_ATTRIBUTE_VALUE );
  fail_unless( !CompartmentType_isSetId(C) );
}
END_TEST


START_TEST (test_CompartmentType_setId3)
{
  int i = CompartmentType_setId(C, "cell");

  fail_unless( i == LIBSBML_OPERATION_SUCCESS );
  fail_unless( CompartmentType_isSetId(C) );
  fail_unless( !strcmp(CompartmentType_getId(C), "cell" ));
}
END_TEST


START_TEST (test_CompartmentType_setId4)
{
  int i = CompartmentType_setId(C, NULL);

  fail_unless( i == LIBSBML_OPERATION_SUCCESS );
  fail_unless( !CompartmentType_isSetId(C) );
}
END_TEST


START_TEST (test_CompartmentType_setName1)
{
  int i = CompartmentType_setName(C, "cell");

  fail_unless( i == LIBSBML_OPERATION_SUCCESS );
  fail_unless( CompartmentType_isSetName(C) );

  i = CompartmentType_unsetName(C);

  fail_unless( i == LIBSBML_OPERATION_SUCCESS );
  fail_unless( !CompartmentType_isSetName(C) );
}
END_TEST


START_TEST (test_CompartmentType_setName2)
{
  int i = CompartmentType_setName(C, "1cell");

  fail_unless( i == LIBSBML_OPERATION_SUCCESS );
  fail_unless( CompartmentType_isSetName(C) );

  i = CompartmentType_unsetName(C);

  fail_unless( i == LIBSBML_OPERATION_SUCCESS );
  fail_unless( !CompartmentType_isSetName(C) );
}
END_TEST


START_TEST (test_CompartmentType_setName3)
{
  int i = CompartmentType_setName(C, NULL);

  fail_unless( i == LIBSBML_OPERATION_SUCCESS );
  fail_unless( !CompartmentType_isSetName(C) );
}
END_TEST


Suite *
create_suite_CompartmentType_newSetters (void)
{
  Suite *suite = suite_create("CompartmentType_newSetters");
  TCase *tcase = tcase_create("CompartmentType_newSetters");


  tcase_add_checked_fixture( tcase,
                             CompartmentTypeTest1_setup,
                             CompartmentTypeTest1_teardown );

  tcase_add_test( tcase, test_CompartmentType_setId2       );
  tcase_add_test( tcase, test_CompartmentType_setId3       );
  tcase_add_test( tcase, test_CompartmentType_setId4       );
  tcase_add_test( tcase, test_CompartmentType_setName1       );
  tcase_add_test( tcase, test_CompartmentType_setName2       );
  tcase_add_test( tcase, test_CompartmentType_setName3       );


  suite_add_tcase(suite, tcase);

  return suite;
}
