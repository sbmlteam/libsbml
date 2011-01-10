/**
 * \file    TestCVTerms_newSetters.cpp
 * \brief   CVTerms unit tests
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
#include <sbml/annotation/CVTerm.h>
#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLTriple.h>

#include <check.h>

#if __cplusplus
CK_CPPSTART
#endif


START_TEST (test_CVTerm_setModelQualifierType)
{
  CVTerm_t *term = CVTerm_createWithQualifierType(MODEL_QUALIFIER);

  fail_unless(term != NULL);
  fail_unless(CVTerm_getQualifierType(term) == MODEL_QUALIFIER);
  fail_unless(CVTerm_getModelQualifierType(term) == BQM_UNKNOWN);
  fail_unless(CVTerm_getBiologicalQualifierType(term) == BQB_UNKNOWN); 
  
  int i = CVTerm_setModelQualifierType(term, BQM_IS);

  fail_unless(i == LIBSBML_OPERATION_SUCCESS);
  fail_unless(CVTerm_getQualifierType(term) == MODEL_QUALIFIER);
  fail_unless(CVTerm_getModelQualifierType(term) == BQM_IS);
  fail_unless(CVTerm_getBiologicalQualifierType(term) == BQB_UNKNOWN); 

  i = CVTerm_setQualifierType(term, BIOLOGICAL_QUALIFIER);
  
  fail_unless( i == LIBSBML_OPERATION_SUCCESS);
  fail_unless(CVTerm_getQualifierType(term) == BIOLOGICAL_QUALIFIER);
  fail_unless(CVTerm_getModelQualifierType(term) == BQM_UNKNOWN);
  fail_unless(CVTerm_getBiologicalQualifierType(term) == BQB_UNKNOWN); 

  i = CVTerm_setModelQualifierType(term, BQM_IS);

  fail_unless ( i == LIBSBML_INVALID_ATTRIBUTE_VALUE);
  fail_unless(CVTerm_getQualifierType(term) == BIOLOGICAL_QUALIFIER);
  fail_unless(CVTerm_getBiologicalQualifierType(term) == BQB_UNKNOWN); 
  fail_unless(CVTerm_getModelQualifierType(term) == BQM_UNKNOWN);
  
  CVTerm_free(term);
}
END_TEST


START_TEST (test_CVTerm_setBiolQualifierType)
{
  CVTerm_t *term = CVTerm_createWithQualifierType(BIOLOGICAL_QUALIFIER);

  fail_unless(term != NULL);
  fail_unless(CVTerm_getQualifierType(term) == BIOLOGICAL_QUALIFIER);
  fail_unless(CVTerm_getModelQualifierType(term) == BQM_UNKNOWN);
  fail_unless(CVTerm_getBiologicalQualifierType(term) == BQB_UNKNOWN); 
  
  int i = CVTerm_setBiologicalQualifierType(term, BQB_IS);

  fail_unless(i == LIBSBML_OPERATION_SUCCESS);
  fail_unless(CVTerm_getQualifierType(term) == BIOLOGICAL_QUALIFIER);
  fail_unless(CVTerm_getBiologicalQualifierType(term) == BQB_IS);
  fail_unless(CVTerm_getModelQualifierType(term) == BQM_UNKNOWN);

  i = CVTerm_setQualifierType(term, MODEL_QUALIFIER);
  
  fail_unless( i == LIBSBML_OPERATION_SUCCESS);
  fail_unless(CVTerm_getQualifierType(term) == MODEL_QUALIFIER);
  fail_unless(CVTerm_getModelQualifierType(term) == BQM_UNKNOWN);
  fail_unless(CVTerm_getBiologicalQualifierType(term) == BQB_UNKNOWN); 

  i = CVTerm_setBiologicalQualifierType(term, BQB_IS);

  fail_unless ( i == LIBSBML_INVALID_ATTRIBUTE_VALUE);
  fail_unless(CVTerm_getQualifierType(term) == MODEL_QUALIFIER);
  fail_unless(CVTerm_getModelQualifierType(term) == BQM_UNKNOWN);
  fail_unless(CVTerm_getBiologicalQualifierType(term) == BQB_UNKNOWN); 
  
  CVTerm_free(term);
}
END_TEST


START_TEST (test_CVTerm_addResource)
{
  CVTerm_t *term = CVTerm_createWithQualifierType(MODEL_QUALIFIER);
  const char * resource = "GO6666";
  XMLAttributes_t *xa;

  fail_unless(term != NULL);
  fail_unless(CVTerm_getQualifierType(term) == MODEL_QUALIFIER);
  
  int i = CVTerm_addResource(term, "");

  fail_unless ( i == LIBSBML_OPERATION_FAILED);

  xa = CVTerm_getResources(term);

  fail_unless(XMLAttributes_getLength(xa) == 0);
  
  i = CVTerm_addResource(term, resource);

  fail_unless ( i == LIBSBML_OPERATION_SUCCESS);

  xa = CVTerm_getResources(term);

  fail_unless(XMLAttributes_getLength(xa) == 1);
  fail_unless(!strcmp(XMLAttributes_getName(xa, 0), "rdf:resource"));
  fail_unless(!strcmp(XMLAttributes_getValue(xa, 0), "GO6666"));

  CVTerm_free(term);
}
END_TEST


START_TEST (test_CVTerm_removeResource)
{
  CVTerm_t *term = CVTerm_createWithQualifierType(MODEL_QUALIFIER);
  const char * resource = "GO6666";
  XMLAttributes_t *xa;

  fail_unless(term != NULL);
  fail_unless(CVTerm_getQualifierType(term) == MODEL_QUALIFIER);
  
  CVTerm_addResource(term, resource);
  xa = CVTerm_getResources(term);

  fail_unless(XMLAttributes_getLength(xa) == 1);
  
  int i = CVTerm_removeResource(term, "CCC");

  fail_unless ( i == LIBSBML_INVALID_ATTRIBUTE_VALUE);

  xa = CVTerm_getResources(term);

  fail_unless(XMLAttributes_getLength(xa) == 1);
  
  i = CVTerm_removeResource(term, resource);

  fail_unless ( i == LIBSBML_OPERATION_SUCCESS);

  xa = CVTerm_getResources(term);

  fail_unless(XMLAttributes_getLength(xa) == 0);

  CVTerm_free(term);
}
END_TEST


Suite *
create_suite_CVTerms_newSetters (void)
{
  Suite *suite = suite_create("CVTerms_newSetters");
  TCase *tcase = tcase_create("CVTerms_newSetters");

  tcase_add_test( tcase, test_CVTerm_setModelQualifierType  );
  tcase_add_test( tcase, test_CVTerm_setBiolQualifierType  );
  tcase_add_test( tcase, test_CVTerm_addResource  );
  tcase_add_test( tcase, test_CVTerm_removeResource  );

  suite_add_tcase(suite, tcase);

  return suite;
}


#if __cplusplus
CK_CPPEND
#endif
