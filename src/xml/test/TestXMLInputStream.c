/**
 * \file    TestXMLInputStream.c
 * \brief   XMLInputStream unit tests
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

#include <sbml/common/common.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLToken.h>
#include <sbml/xml/XMLErrorLog.h>

#include <check.h>
/**
 * Wraps the string s in the appropriate XML boilerplate.
 */
#define XML_START   "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
#define SBML_START  "<sbml "
#define NS_L1       "xmlns=\"http://www.sbml.org/sbml/level1\" "
#define NS_L2v1     "xmlns=\"http://www.sbml.org/sbml/level2\" "
#define NS_L2v2     "xmlns=\"http://www.sbml.org/sbml/level2/version2\" "
#define LV_L1v1     "level=\"1\" version=\"1\">\n"
#define LV_L1v2     "level=\"1\" version=\"2\">\n"
#define LV_L2v1     "level=\"2\" version=\"1\">\n"
#define LV_L2v2     "level=\"2\" version=\"2\">\n"
#define SBML_END    "</sbml>\n"

#define wrapXML(s)        XML_START s
#define wrapSBML_L1v1(s)  XML_START SBML_START NS_L1   LV_L1v1 s SBML_END
#define wrapSBML_L1v2(s)  XML_START SBML_START NS_L1   LV_L1v2 s SBML_END
#define wrapSBML_L2v1(s)  XML_START SBML_START NS_L2v1 LV_L2v1 s SBML_END
#define wrapSBML_L2v2(s)  XML_START SBML_START NS_L2v2 LV_L2v2 s SBML_END


START_TEST (test_XMLInputStream_create)
{
  const char* text = wrapSBML_L2v1("  <model id=\"Branch\"/>\n");

  XMLInputStream_t * stream = XMLInputStream_create(text, 0, "");

  fail_unless(stream != NULL);
  fail_unless(XMLInputStream_isEOF(stream) == 0);
  fail_unless(XMLInputStream_isGood(stream) == 1);
  fail_unless(XMLInputStream_isError(stream) == 0);

  XMLInputStream_next(stream);
  fail_unless(strcmp(XMLInputStream_getEncoding(stream), "UTF-8") == 0);

  XMLInputStream_free(stream);

}
END_TEST


START_TEST (test_XMLInputStream_next_peek)
{
  const char* text = 
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<sbml "
    "xmlns=\"http://www.sbml.org/sbml/level2\" "
    "level=\"2\" version=\"1\">\n"
    "  <model id=\"Branch\"/>\n"
    "</sbml>";

  XMLInputStream_t *stream = XMLInputStream_create(text, 0, "");;
  const XMLToken_t  *next = XMLInputStream_peek(stream);
  
  fail_unless(stream != NULL);
  
  fail_unless(strcmp(XMLToken_getName(next), "sbml") == 0);
  
  XMLToken_t * next1 = XMLInputStream_next(stream);
  
  fail_unless(strcmp(XMLToken_getName(next1), "sbml") == 0);
 
  XMLInputStream_free(stream);

}
END_TEST


START_TEST (test_XMLInputStream_skip)
{
  const char* text = 
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<sbml "
    "xmlns=\"http://www.sbml.org/sbml/level2\" "
    "level=\"2\" version=\"1\">\n"
    "<listOfFunctionDefinitions>\n"
    "<notes>My Functions</notes>\n"
    "<functionDefinition/>\n"
    "</listOfFunctionDefinitions>\n"
    "<listOfUnitDefinitions>\n"
    "<notes>My Units</notes>\n"
    "<unitDefinition/>\n"
    "</listOfUnitDefinitions>\n"
    "</sbml>";

        
  XMLInputStream_t *stream = XMLInputStream_create(text, 0, "");;

  fail_unless(stream != NULL);
  
  XMLToken_t * next = XMLInputStream_next(stream);
  XMLInputStream_skipText (stream);

  /* skip past listOfFunctionDefinitions */
  XMLInputStream_skipPastEnd(stream, XMLInputStream_next(stream)); 
  XMLInputStream_skipText (stream);

  next = XMLInputStream_next(stream);

  fail_unless(strcmp(XMLToken_getName(next), "listOfUnitDefinitions") == 0);
 
  XMLInputStream_free(stream);

}
END_TEST


START_TEST (test_XMLInputStream_setErrorLog)
{
  const char* text = 
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<sbml "
    "xmlns=\"http://www.sbml.org/sbml/level2\" "
    "level=\"2\" version=\"1\">\n"
    "<listOfFunctionDefinitions>\n"
    "<notes>My Functions</notes>\n"
    "<functionDefinition/>\n"
    "</listOfFunctionDefinitions>\n"
    "<listOfUnitDefinitions>\n"
    "<notes>My Units</notes>\n"
    "<unitDefinition/>\n"
    "</listOfUnitDefinitions>\n"
    "</sbml>";

        
  XMLInputStream_t *stream = XMLInputStream_create(text, 0, "");;

  fail_unless(stream != NULL);

  XMLErrorLog_t *log = XMLErrorLog_create();

  XMLInputStream_setErrorLog(stream, log);

  fail_unless(XMLInputStream_getErrorLog(stream) == log);
  
}
END_TEST 


Suite *
create_suite_XMLInputStream (void)
{
  Suite *suite = suite_create("XMLInputStream");
  TCase *tcase = tcase_create("XMLInputStream");

  tcase_add_test( tcase, test_XMLInputStream_create  );
  tcase_add_test( tcase, test_XMLInputStream_next_peek  );
  tcase_add_test( tcase, test_XMLInputStream_skip  );
  tcase_add_test( tcase, test_XMLInputStream_setErrorLog  );
  suite_add_tcase(suite, tcase);

  return suite;
}

