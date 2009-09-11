/**
 * \file    TestL3Model.c
 * \brief   L3 Model unit tests
 * \author  Sarah Keating
 *
 * $Id:  $
 * $HeadURL: $
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
#include <sbml/Model.h>
#include <sbml/xml/XMLNamespaces.h>
#include <sbml/SBMLDocument.h>

#include <check.h>


static Model_t *M;


void
L3ModelTest_setup (void)
{
  M = Model_create(3, 1);

  if (M == NULL)
  {
    fail("Model_create(3, 1) returned a NULL pointer.");
  }
}


void
L3ModelTest_teardown (void)
{
  Model_free(M);
}


START_TEST (test_L3_Model_create)
{
  fail_unless( SBase_getTypeCode  ((SBase_t *) M) == SBML_MODEL );
  fail_unless( SBase_getMetaId    ((SBase_t *) M) == NULL );
  fail_unless( SBase_getNotes     ((SBase_t *) M) == NULL );
  fail_unless( SBase_getAnnotation((SBase_t *) M) == NULL );

  fail_unless( Model_getId     (M) == NULL );
  fail_unless( Model_getName   (M) == NULL );
  fail_unless( Model_getSubstanceUnits(M) == NULL );
  fail_unless( Model_getTimeUnits(M) == NULL );
  fail_unless( Model_getVolumeUnits(M) == NULL );
  fail_unless( Model_getAreaUnits(M) == NULL );
  fail_unless( Model_getLengthUnits(M) == NULL );
  fail_unless( Model_getConversionFactor(M) == NULL );

  fail_unless( !Model_isSetId     (M) );
  fail_unless( !Model_isSetName   (M) );
  fail_unless( !Model_isSetSubstanceUnits(M) );
  fail_unless( !Model_isSetTimeUnits(M) );
  fail_unless( !Model_isSetVolumeUnits(M) );
  fail_unless( !Model_isSetAreaUnits(M) );
  fail_unless( !Model_isSetLengthUnits(M) );
  fail_unless( !Model_isSetConversionFactor(M) );
}
END_TEST


START_TEST (test_L3_Model_free_NULL)
{
  Model_free(NULL);
}
END_TEST


START_TEST (test_L3_Model_id)
{
  char *id = "mitochondria";


  fail_unless( !Model_isSetId(M) );
  
  Model_setId(M, id);

  fail_unless( !strcmp(Model_getId(M), id) );
  fail_unless( Model_isSetId(M) );

  if (Model_getId(M) == id)
  {
    fail("Model_setId(...) did not make a copy of string.");
  }
 
  Model_unsetId(M);
  
  fail_unless( !Model_isSetId(M) );

  if (Model_getId(M) != NULL)
  {
    fail("Model_unsetId(M) did not clear string.");
  }
}
END_TEST


START_TEST (test_L3_Model_name)
{
  char *name = "My_Favorite_Factory";


  fail_unless( !Model_isSetName(M) );

  Model_setName(M, name);

  fail_unless( !strcmp(Model_getName(M), name) );
  fail_unless( Model_isSetName(M) );

  if (Model_getName(M) == name)
  {
    fail("Model_setName(...) did not make a copy of string.");
  }

  Model_unsetName(M);
  
  fail_unless( !Model_isSetName(M) );

  if (Model_getName(M) != NULL)
  {
    fail("Model_unsetName(M) did not clear string.");
  }
}
END_TEST


START_TEST (test_L3_Model_substanceUnits)
{
  char *units = "mole";


  fail_unless( !Model_isSetSubstanceUnits(M) );

  Model_setSubstanceUnits(M, units);

  fail_unless( !strcmp(Model_getSubstanceUnits(M), units) );
  fail_unless( Model_isSetSubstanceUnits(M) );

  if (Model_getSubstanceUnits(M) == units)
  {
    fail("Model_setSubstanceUnits(...) did not make a copy of string.");
  }

  Model_unsetSubstanceUnits(M);
  
  fail_unless( !Model_isSetSubstanceUnits(M) );

  if (Model_getSubstanceUnits(M) != NULL)
  {
    fail("Model_unsetSubstanceUnits(M) did not clear string.");
  }
}
END_TEST


START_TEST (test_L3_Model_timeUnits)
{
  char *units = "mole";


  fail_unless( !Model_isSetTimeUnits(M) );

  Model_setTimeUnits(M, units);

  fail_unless( !strcmp(Model_getTimeUnits(M), units) );
  fail_unless( Model_isSetTimeUnits(M) );

  if (Model_getTimeUnits(M) == units)
  {
    fail("Model_setTimeUnits(...) did not make a copy of string.");
  }

  Model_unsetTimeUnits(M);
  
  fail_unless( !Model_isSetTimeUnits(M) );

  if (Model_getTimeUnits(M) != NULL)
  {
    fail("Model_unsetTimeUnits(M) did not clear string.");
  }
}
END_TEST


START_TEST (test_L3_Model_volumeUnits)
{
  char *units = "mole";


  fail_unless( !Model_isSetVolumeUnits(M) );

  Model_setVolumeUnits(M, units);

  fail_unless( !strcmp(Model_getVolumeUnits(M), units) );
  fail_unless( Model_isSetVolumeUnits(M) );

  if (Model_getVolumeUnits(M) == units)
  {
    fail("Model_setVolumeUnits(...) did not make a copy of string.");
  }

  Model_unsetVolumeUnits(M);
  
  fail_unless( !Model_isSetVolumeUnits(M) );

  if (Model_getVolumeUnits(M) != NULL)
  {
    fail("Model_unsetVolumeUnits(M) did not clear string.");
  }
}
END_TEST


START_TEST (test_L3_Model_areaUnits)
{
  char *units = "mole";


  fail_unless( !Model_isSetAreaUnits(M) );

  Model_setAreaUnits(M, units);

  fail_unless( !strcmp(Model_getAreaUnits(M), units) );
  fail_unless( Model_isSetAreaUnits(M) );

  if (Model_getAreaUnits(M) == units)
  {
    fail("Model_setAreaUnits(...) did not make a copy of string.");
  }

  Model_unsetAreaUnits(M);
  
  fail_unless( !Model_isSetAreaUnits(M) );

  if (Model_getAreaUnits(M) != NULL)
  {
    fail("Model_unsetAreaUnits(M) did not clear string.");
  }
}
END_TEST


START_TEST (test_L3_Model_lengthUnits)
{
  char *units = "mole";


  fail_unless( !Model_isSetLengthUnits(M) );

  Model_setLengthUnits(M, units);

  fail_unless( !strcmp(Model_getLengthUnits(M), units) );
  fail_unless( Model_isSetLengthUnits(M) );

  if (Model_getLengthUnits(M) == units)
  {
    fail("Model_setLengthUnits(...) did not make a copy of string.");
  }

  Model_unsetLengthUnits(M);
  
  fail_unless( !Model_isSetLengthUnits(M) );

  if (Model_getLengthUnits(M) != NULL)
  {
    fail("Model_unsetLengthUnits(M) did not clear string.");
  }
}
END_TEST


START_TEST (test_L3_Model_conversionFactor)
{
  char *units = "mole";


  fail_unless( !Model_isSetConversionFactor(M) );

  Model_setConversionFactor(M, units);

  fail_unless( !strcmp(Model_getConversionFactor(M), units) );
  fail_unless( Model_isSetConversionFactor(M) );

  if (Model_getConversionFactor(M) == units)
  {
    fail("Model_setConversionFactor(...) did not make a copy of string.");
  }

  Model_unsetConversionFactor(M);
  
  fail_unless( !Model_isSetConversionFactor(M) );

  if (Model_getConversionFactor(M) != NULL)
  {
    fail("Model_unsetConversionFactor(M) did not clear string.");
  }
}
END_TEST


START_TEST (test_L3_Model_createWithNS )
{
  XMLNamespaces_t *xmlns = XMLNamespaces_create();
  XMLNamespaces_add(xmlns, "http://www.sbml.org", "testsbml");
  SBMLNamespaces_t *sbmlns = SBMLNamespaces_create(3,1);
  SBMLNamespaces_addNamespaces(sbmlns,xmlns);

  Model_t *m = 
    Model_createWithNS (sbmlns);


  fail_unless( SBase_getTypeCode  ((SBase_t *) m) == SBML_MODEL );
  fail_unless( SBase_getMetaId    ((SBase_t *) m) == NULL );
  fail_unless( SBase_getNotes     ((SBase_t *) m) == NULL );
  fail_unless( SBase_getAnnotation((SBase_t *) m) == NULL );

  fail_unless( SBase_getLevel       ((SBase_t *) m) == 3 );
  fail_unless( SBase_getVersion     ((SBase_t *) m) == 1 );

  fail_unless( Model_getNamespaces     (m) != NULL );
  fail_unless( XMLNamespaces_getLength(Model_getNamespaces(m)) == 2 );


  fail_unless( Model_getId     (m) == NULL );
  fail_unless( Model_getName   (m) == NULL );
  fail_unless( Model_getSubstanceUnits(m) == NULL );
  fail_unless( Model_getTimeUnits(m) == NULL );
  fail_unless( Model_getVolumeUnits(m) == NULL );
  fail_unless( Model_getAreaUnits(m) == NULL );
  fail_unless( Model_getLengthUnits(m) == NULL );
  fail_unless( Model_getConversionFactor(m) == NULL );

  fail_unless( !Model_isSetId     (m) );
  fail_unless( !Model_isSetName   (m) );
  fail_unless( !Model_isSetSubstanceUnits(m) );
  fail_unless( !Model_isSetTimeUnits(m) );
  fail_unless( !Model_isSetVolumeUnits(m) );
  fail_unless( !Model_isSetAreaUnits(m) );
  fail_unless( !Model_isSetLengthUnits(m) );
  fail_unless( !Model_isSetConversionFactor(m) );

  Model_free(m);
}
END_TEST


Suite *
create_suite_L3_Model (void)
{
  Suite *suite = suite_create("L3_Model");
  TCase *tcase = tcase_create("L3_Model");


  tcase_add_checked_fixture( tcase,
                             L3ModelTest_setup,
                             L3ModelTest_teardown );

  tcase_add_test( tcase, test_L3_Model_create              );
  tcase_add_test( tcase, test_L3_Model_free_NULL           );
  tcase_add_test( tcase, test_L3_Model_id               );
  tcase_add_test( tcase, test_L3_Model_name             );
  tcase_add_test( tcase, test_L3_Model_substanceUnits   );
  tcase_add_test( tcase, test_L3_Model_timeUnits   );
  tcase_add_test( tcase, test_L3_Model_volumeUnits   );
  tcase_add_test( tcase, test_L3_Model_areaUnits   );
  tcase_add_test( tcase, test_L3_Model_lengthUnits   );
  tcase_add_test( tcase, test_L3_Model_conversionFactor   );
  tcase_add_test( tcase, test_L3_Model_createWithNS         );

  suite_add_tcase(suite, tcase);

  return suite;
}
