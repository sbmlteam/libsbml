/**
 * \file    TestSBMLReader.cpp
 * \brief   SBMLReader unit tests
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2003 California Institute of Technology and
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
 *     The Systems Biology Markup Language Development Group
 *     ERATO Kitano Symbiotic Systems Project
 *     Control and Dynamical Systems, MC 107-81
 *     California Institute of Technology
 *     Pasadena, CA, 91125, USA
 *
 *     http://www.cds.caltech.edu/erato
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s):
 *  Stephan Hoops
 */


#include <iostream>

#include "SBMLDocument.h"
#include "SBMLReader.h"

#include <check.h>


BEGIN_C_DECLS


static SBMLDocument* D;


static void
debugPrint (const char* filename)
{
  using namespace std;


  cout << endl << endl << filename << ":" << endl;

  if (D)
  {
    D->printErrors  (cout);
  }
  else
  {
    cout << "D is NULL!" << endl;
  }

  cout << endl << endl;
}


void
SBMLReaderTest_setup ()
{
  D = 0;
}


void
SBMLReaderTest_teardown ()
{
  delete D;
}


START_TEST (test_SBMLReader_create)
{
  SBMLReader reader;
  /* this function no longer exists
  fail_unless(reader.getSchemaValidationLevel() == XML_SCHEMA_VALIDATION_NONE);
  */
}
END_TEST


START_TEST (test_SBMLReader_readSBML_schema_basic)
{
  const char* filename = "test-data/l1v1-branch.xml";
 /* SBMLReader  reader(XML_SCHEMA_VALIDATION_BASIC); */
  SBMLReader  reader();


  D = reader.readSBML(filename);
  // debugPrint(filename);

  fail_unless( D != 0 );
  fail_unless( D->getNumErrors()   == 0 );
}
END_TEST


START_TEST (test_SBMLReader_readSBML_schema_basic_L1v2)
{
  const char* filename = "test-data/l2v1-branch.xml";
 /* SBMLReader  reader(XML_SCHEMA_VALIDATION_BASIC); */
  SBMLReader  reader();


  D = reader.readSBML(filename);
  // debugPrint(filename);

  fail_unless( D != 0 );
  fail_unless( D->getNumErrors()   == 0 );
}
END_TEST


START_TEST (test_SBMLReader_readSBML_schema_basic_with_error)
{
  const char* filename = "test-data/l1v1-branch-schema-error.xml";
 /* SBMLReader  reader(XML_SCHEMA_VALIDATION_BASIC); */
  SBMLReader  reader();


  D = reader.readSBML(filename);
  // debugPrint(filename);

  fail_unless( D != 0 );
  fail_unless( D->getNumErrors()   == 1 );
}
END_TEST


START_TEST (test_SBMLReader_readSBML_schema_full)
{
  const char* filename = "test-data/l1v1-branch.xml";
 /* SBMLReader  reader(XML_SCHEMA_VALIDATION_BASIC); */
  SBMLReader  reader();


  D = reader.readSBML(filename);
  // debugPrint(filename);

  fail_unless( D != 0 );
  fail_unless( D->getNumErrors()   == 0 );
}
END_TEST


START_TEST (test_readSBML_NULL)
{
  fail_unless( readSBML(NULL)           == NULL );
  fail_unless( readSBMLFromString(NULL) == NULL );
}
END_TEST


START_TEST (test_readSBML_file_not_found)
{
  const char* filename = "test-data/nonexistent.xml";


  D = readSBML(filename);
  // debugPrint(filename);

  fail_unless( D != 0 );
  fail_unless( D->getNumErrors()   == 1 );

  /*
  fail_unless( D->getFatal(0)->getId() == SBML_READ_ERROR_FILE_NOT_FOUND );
  */
}
END_TEST
/* these all test functions that are not in libSBML in this way

START_TEST (test_readSBML_not_xml)
{
  const char* filename = "test-data/not-xml.txt";


  D = readSBML(filename);
  // debugPrint(filename);

  fail_unless( D != 0 );
  fail_unless( D->getNumWarnings() == 0 );
  fail_unless( D->getNumErrors()   == 0 );
  fail_unless( D->getNumFatals()   == 1 );

  fail_unless( D->getFatal(0)->getId() == SBML_READ_ERROR_NOT_XML  );
}
END_TEST


START_TEST (test_readSBML_no_encoding)
{
  const char* filename = "test-data/no-encoding.xml";


  D = readSBML(filename);
  // debugPrint(filename);

  fail_unless( D != 0 );
  fail_unless( D->getNumWarnings() == 0 );
  fail_unless( D->getNumErrors()   == 1 );
  fail_unless( D->getNumFatals()   == 0 );

  fail_unless( D->getError(0)->getId() == SBML_READ_ERROR_NO_ENCODING );
}
END_TEST


START_TEST (test_readSBML_not_utf8)
{
  const char* filename = "test-data/not-utf8.xml";


  D = readSBML(filename);
  // debugPrint(filename);

  fail_unless( D != 0 );
  fail_unless( D->getNumWarnings() == 0 );
  fail_unless( D->getNumErrors()   == 1 );
  fail_unless( D->getNumFatals()   == 0 );

  fail_unless( D->getError(0)->getId() == SBML_READ_ERROR_NOT_UTF_8 );
}
END_TEST


START_TEST (test_readSBML_unknown_encoding)
{
  const char* filename = "test-data/unknown-encoding.xml";


  D = readSBML(filename);
  // debugPrint(filename);

  fail_unless( D != 0 );
  fail_unless( D->getNumWarnings() == 0 );
  fail_unless( D->getNumErrors()   == 0 );
  fail_unless( D->getNumFatals()   == 1 );

  fail_unless( D->getFatal(0)->getId() == SBML_READ_ERROR_UNKNOWN_ENCODING );
}
END_TEST


START_TEST (test_readSBML_not_sbml)
{
  const char* filename = "test-data/not-sbml.xml";


  D = readSBML(filename);
  // debugPrint(filename);

  fail_unless( D != 0 );
  fail_unless( D->getNumWarnings() == 0 );
  fail_unless( D->getNumErrors()   == 0 );
  fail_unless( D->getNumFatals()   == 1 );

  fail_unless( D->getFatal(0)->getId() == SBML_READ_ERROR_NOT_SBML );
}
END_TEST


START_TEST (test_readSBML_unknown_sbml)
{
  const char* filename = "test-data/unknown-sbml.xml";


  D = readSBML(filename);
  // debugPrint(filename);

  fail_unless( D != 0 );
  fail_unless( D->getNumWarnings() == 0 );
  fail_unless( D->getNumErrors()   == 0 );
  fail_unless( D->getNumFatals()   == 1 );

  fail_unless( D->getFatal(0)->getId() == SBML_READ_ERROR_UNKNOWN_SBML );
}
END_TEST



START_TEST (test_readSBMLFromString_empty)
{
  D = readSBMLFromString("");
  // debugPrint("(empty string)");

  fail_unless( D != 0 );
  fail_unless( D->getNumWarnings() == 0 );
  fail_unless( D->getNumErrors()   == 0 );
  fail_unless( D->getNumFatals()   == 1 );

  fail_unless( D->getFatal(0)->getId() == SBML_READ_ERROR_NOT_XML  );
}
END_TEST
*/

Suite *
create_suite_SBMLReader (void) 
{ 
  Suite *suite = suite_create("SBMLReader");
  TCase *tcase = tcase_create("SBMLReader");


  tcase_add_test(tcase, test_SBMLReader_create       );
  tcase_add_test(tcase, test_readSBML_NULL           );
  tcase_add_test(tcase, test_readSBML_file_not_found );
  tcase_add_test(tcase, test_readSBML_not_sbml       );


#ifndef USE_EXPAT

  // No XML Schema validation possible with Expat.
  tcase_add_test(tcase, test_SBMLReader_readSBML_schema_basic            );
  tcase_add_test(tcase, test_SBMLReader_readSBML_schema_basic_L1v2       );
  tcase_add_test(tcase, test_SBMLReader_readSBML_schema_basic_with_error );
  tcase_add_test(tcase, test_SBMLReader_readSBML_schema_full             );

  /* libSBML Expat cannot detect these specific error conditions (yet)
  tcase_add_test(tcase, test_readSBML_not_xml          );
  tcase_add_test(tcase, test_readSBML_no_encoding      );
  tcase_add_test(tcase, test_readSBML_not_utf8         );
  tcase_add_test(tcase, test_readSBML_unknown_encoding );
  tcase_add_test(tcase, test_readSBML_unknown_sbml     );
  tcase_add_test(tcase, test_readSBMLFromString_empty  );*/

#endif  // !USE_EXPAT


  suite_add_tcase(suite, tcase);

  return suite;
}


END_C_DECLS
