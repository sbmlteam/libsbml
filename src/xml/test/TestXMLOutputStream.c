/**
 * \file    TestXMLOutputStream.c
 * \brief   XMLOutputStream unit tests
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
#include <sbml/xml/XMLToken.h>
#include <sbml/xml/XMLTriple.h>
#include <sbml/xml/XMLOutputStream.h>
#include <sbml/xml/XMLErrorLog.h>

#include <check.h>


START_TEST (test_XMLOutputStream_createStdout)
{
  XMLOutputStream_t * stream = XMLOutputStream_createAsStdout("UTF-8", 0);
  fail_unless(stream != NULL);

  XMLOutputStream_free(stream);

}
END_TEST

START_TEST (test_XMLOutputStream_createFile)
{
  XMLOutputStream_t * stream = XMLOutputStream_createFile("out.xml","UTF-8", 0);
  fail_unless(stream != NULL);

  XMLOutputStream_free(stream);

}
END_TEST

START_TEST (test_XMLOutputStream_createString)
{
  const char * expected = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
  
  XMLOutputStream_t * stream = XMLOutputStream_createAsString("UTF-8", 1);
  fail_unless(stream != NULL);

  const char * string = XMLOutputStream_getString(stream);

  fail_unless(!strcmp(string, expected));

  XMLOutputStream_free(stream);

}
END_TEST

START_TEST (test_XMLOutputStream_startEnd)
{
  XMLOutputStream_t *stream = XMLOutputStream_createAsString("", 0);
  
  fail_unless(stream != NULL);

  XMLOutputStream_startEndElement(stream, "id");

  const char * string = XMLOutputStream_getString(stream);

  fail_unless(!strcmp(string, "<id/>"));

  XMLOutputStream_free(stream);
}
END_TEST
  
START_TEST (test_XMLOutputStream_Elements)
{
  double d = 2.4; 
  long l = 123456789;
  unsigned int ui = 5;
  int i = -3;
  XMLOutputStream_t *stream = XMLOutputStream_createAsString("", 0);
  XMLOutputStream_startElement(stream, "fred");
  XMLOutputStream_writeAttributeChars(stream, "chars", "two");
  XMLOutputStream_writeAttributeBool(stream, "bool", 1);
  XMLOutputStream_writeAttributeDouble(stream, "double", d);
  XMLOutputStream_writeAttributeLong(stream, "long", l);
  XMLOutputStream_writeAttributeUInt(stream, "uint", ui);
  XMLOutputStream_writeAttributeInt(stream, "int", i);
  XMLOutputStream_endElement(stream, "fred");

  const char * expected = "<fred chars=\"two\" bool=\"true\" double=\"2.4\" long=\"123456789\" uint=\"5\" int=\"-3\"/>";
  const char * s = XMLOutputStream_getString(stream);

  fail_unless(!strcmp(s,expected));

  XMLOutputStream_free(stream);

}
END_TEST


Suite *
create_suite_XMLOutputStream (void)
{
  Suite *suite = suite_create("XMLOutputStream");
  TCase *tcase = tcase_create("XMLOutputStream");

  tcase_add_test( tcase, test_XMLOutputStream_createStdout  );
  tcase_add_test( tcase, test_XMLOutputStream_createFile  );
  tcase_add_test( tcase, test_XMLOutputStream_createString  );
  tcase_add_test( tcase, test_XMLOutputStream_startEnd  );
  tcase_add_test( tcase, test_XMLOutputStream_Elements  );
  suite_add_tcase(suite, tcase);

  return suite;
}

