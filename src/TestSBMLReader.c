/**
 * Filename    : TestSBMLReader.c
 * Description : SBMLReader unit tests
 * Author(s)   : SBW Development Group <sysbio-team@caltech.edu>
 * Organization: Caltech ERATO Kitano Systems Biology Project
 * Created     : 2003-03-22
 * Revision    : $Id$
 * Source      : $Source$
 *
 * Copyright 2003 California Institute of Technology and
 * Japan Science and Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 2.1 of the License, or
 * any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY, WITHOUT EVEN THE IMPLIED WARRANTY OF
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  The software and
 * documentation provided hereunder is on an "as is" basis, and the
 * California Institute of Technology and Japan Science and Technology
 * Corporation have no obligations to provide maintenance, support,
 * updates, enhancements or modifications.  In no event shall the
 * California Institute of Technology or the Japan Science and Technology
 * Corporation be liable to any party for direct, indirect, special,
 * incidental or consequential damages, including lost profits, arising
 * out of the use of this software and its documentation, even if the
 * California Institute of Technology and/or Japan Science and Technology
 * Corporation have been advised of the possibility of such damage.  See
 * the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 *
 * The original code contained here was initially developed by:
 *
 *     Ben Bornstein
 *     The Systems Biology Workbench Development Group
 *     ERATO Kitano Systems Biology Project
 *     Control and Dynamical Systems, MC 107-81
 *     California Institute of Technology
 *     Pasadena, CA, 91125, USA
 *
 *     http://www.cds.caltech.edu/erato
 *     mailto:sysbio-team@caltech.edu
 *
 * Contributor(s):
 */


#include "sbml/common.h"
#include "sbml/SBMLReader.h"


extern char *TestDataDirectory;


START_TEST (test_SBMLReader_create)
{
  SBMLReader_t *sr = SBMLReader_create();


  fail_unless(sr->schemaValidationLevel == XML_SCHEMA_VALIDATION_NONE, NULL);
  fail_unless(sr->schemaFilenameL1v1    == NULL, NULL);
  fail_unless(sr->schemaFilenameL1v2    == NULL, NULL);
  fail_unless(sr->schemaFilenameL2v1    == NULL, NULL);

  SBMLReader_free(sr);
}
END_TEST


START_TEST (test_SBMLReader_free_NULL)
{
  SBMLReader_free(NULL);
}
END_TEST


START_TEST (test_SBMLReader_setSchemaFilename)
{
  SBMLReader_t *sr       = SBMLReader_create();
  char         *filename = "sbml.xsd";


  SBMLReader_setSchemaFilenameL1v1(sr, filename);

  fail_unless( !strcmp(sr->schemaFilenameL1v1, filename), NULL );

  if (sr->schemaFilenameL1v1 == filename)
  {
    fail("SBMLReader_setSchemaFilename(...) did not make a copy of string.");
  }

  SBMLReader_setSchemaFilenameL1v1(sr, NULL);

  if (sr->schemaFilenameL1v1 != NULL)
  {
    fail("SBMLReader_setSchemaFilename(...) did not clear string.");
  }

  SBMLReader_free(sr);
}
END_TEST


START_TEST (test_SBMLReader_readSBML_schema_basic)
{
  SBMLDocument_t *d;
  SBMLReader_t   *sr = SBMLReader_create();

  char *schema   = "sbml-l1v1.xsd";
  char *filename = safe_strcat(TestDataDirectory, "l1v1-branch.xml");


  sr->schemaValidationLevel = XML_SCHEMA_VALIDATION_BASIC;
  SBMLReader_setSchemaFilenameL1v1(sr, schema);

  d = SBMLReader_readSBML(sr, filename);

  fail_unless( SBMLDocument_getNumWarnings(d) == 0, NULL );
  fail_unless( SBMLDocument_getNumErrors(d)   == 0, NULL );
  fail_unless( SBMLDocument_getNumFatals(d)   == 0, NULL );

  SBMLDocument_free(d);
  SBMLReader_free(sr);
  safe_free(filename);
}
END_TEST


START_TEST (test_SBMLReader_readSBML_schema_basic_L1v2)
{
  SBMLDocument_t *d;
  SBMLReader_t   *sr = SBMLReader_create();

  char *schema   = "sbml-l1v2.xsd";
  char *filename = safe_strcat(TestDataDirectory, "l1v2-branch.xml");


  sr->schemaValidationLevel = XML_SCHEMA_VALIDATION_BASIC;
  SBMLReader_setSchemaFilenameL1v2(sr, schema);

  d = SBMLReader_readSBML(sr, filename);

  fail_unless( SBMLDocument_getNumWarnings(d) == 0, NULL );
  fail_unless( SBMLDocument_getNumErrors(d)   == 0, NULL );
  fail_unless( SBMLDocument_getNumFatals(d)   == 0, NULL );

  SBMLDocument_free(d);
  SBMLReader_free(sr);
  safe_free(filename);
}
END_TEST


START_TEST (test_SBMLReader_readSBML_schema_basic_with_error)
{
  SBMLDocument_t *d;
  SBMLReader_t   *sr = SBMLReader_create();

  char *schema   = "sbml-l1v1.xsd";
  char *filename = safe_strcat( TestDataDirectory,
                               "l1v1-branch-schema-error.xml" );


  sr->schemaValidationLevel = XML_SCHEMA_VALIDATION_BASIC;
  SBMLReader_setSchemaFilenameL1v1(sr, schema);

  d = SBMLReader_readSBML(sr, filename);

  fail_unless( SBMLDocument_getNumWarnings(d) == 0, NULL );
  fail_unless( SBMLDocument_getNumErrors(d)   == 1, NULL );
  fail_unless( SBMLDocument_getNumFatals(d)   == 0, NULL );

  SBMLDocument_free(d);
  SBMLReader_free(sr);
  safe_free(filename);
}
END_TEST


START_TEST (test_SBMLReader_readSBML_schema_full)
{
  SBMLDocument_t *d;
  SBMLReader_t   *sr = SBMLReader_create();

  char *schema   = "sbml-l1v1.xsd";
  char *filename = safe_strcat(TestDataDirectory, "l1v1-branch.xml");


  sr->schemaValidationLevel = XML_SCHEMA_VALIDATION_FULL;
  SBMLReader_setSchemaFilenameL1v1(sr, schema);

  d = SBMLReader_readSBML(sr, filename);

  fail_unless( SBMLDocument_getNumWarnings(d) == 0, NULL );
  fail_unless( SBMLDocument_getNumErrors(d)   == 0, NULL );
  fail_unless( SBMLDocument_getNumFatals(d)   == 0, NULL );

  SBMLDocument_free(d);
  SBMLReader_free(sr);
  safe_free(filename);
}
END_TEST


Suite *
create_suite_SBMLReader (void) 
{ 
  Suite *suite = suite_create("SBMLReader");
  TCase *tcase = tcase_create("SBMLReader");
 

  tcase_add_test(tcase, test_SBMLReader_create                           );
  tcase_add_test(tcase, test_SBMLReader_setSchemaFilename                );
  tcase_add_test(tcase, test_SBMLReader_free_NULL                        );
  tcase_add_test(tcase, test_SBMLReader_readSBML_schema_basic            );
  tcase_add_test(tcase, test_SBMLReader_readSBML_schema_basic_L1v2       );
  tcase_add_test(tcase, test_SBMLReader_readSBML_schema_basic_with_error );
  tcase_add_test(tcase, test_SBMLReader_readSBML_schema_full             );
  suite_add_tcase(suite, tcase);

  return suite;
}
