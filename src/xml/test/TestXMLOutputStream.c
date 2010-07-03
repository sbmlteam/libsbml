/**
 * \file    TestXMLOutputStream.c
 * \brief   XMLOutputStream unit tests
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

  const char * str = XMLOutputStream_getString(stream);

  fail_unless(!strcmp(str, expected));

  XMLOutputStream_free(stream);

}
END_TEST

START_TEST (test_XMLOutputStream_createStdoutWithProgramInfo)
{
  XMLOutputStream_t * stream = 
    XMLOutputStream_createAsStdoutWithProgramInfo("UTF-8", 0, "foo", "bar");
  fail_unless(stream != NULL);

  XMLOutputStream_free(stream);

}
END_TEST

START_TEST (test_XMLOutputStream_createFileWithProgramInfo)
{
  XMLOutputStream_t * stream = 
    XMLOutputStream_createFileWithProgramInfo("out.xml","UTF-8", 0,
                                                 "foo", "bar");
  fail_unless(stream != NULL);

  XMLOutputStream_free(stream);

}
END_TEST

START_TEST (test_XMLOutputStream_createStringWithProgramInfo)
{
  const char * expected = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
  
  XMLOutputStream_t * stream = 
    XMLOutputStream_createAsStringWithProgramInfo("UTF-8", 1, "", "");
  fail_unless(stream != NULL);

  const char * str = XMLOutputStream_getString(stream);

  fail_unless(!strcmp(str, expected));

  XMLOutputStream_free(stream);

}
END_TEST

START_TEST (test_XMLOutputStream_startEnd)
{
  XMLOutputStream_t *stream = XMLOutputStream_createAsString("", 0);
  
  fail_unless(stream != NULL);

  XMLOutputStream_startEndElement(stream, "id");

  const char * str = XMLOutputStream_getString(stream);

  fail_unless(!strcmp(str, "<id/>"));

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

START_TEST (test_XMLOutputStream_CharacterReference)
{
  XMLOutputStream_t *stream = XMLOutputStream_createAsString("", 0);
  XMLOutputStream_startElement(stream, "testcr");
  XMLOutputStream_writeAttributeChars(stream, "chars",    "one"     );
  XMLOutputStream_writeAttributeChars(stream, "amp",      "&"       );
  XMLOutputStream_writeAttributeChars(stream, "deccr",    "&#0168;"  );
  XMLOutputStream_writeAttributeChars(stream, "hexcr",    "&#x00a8;");
  XMLOutputStream_writeAttributeChars(stream, "lhexcr",   "&#x00A8;");
  XMLOutputStream_writeAttributeChars(stream, "nodeccr1", "&#01688"  );
  XMLOutputStream_writeAttributeChars(stream, "nodeccr2", "&#;"     );
  XMLOutputStream_writeAttributeChars(stream, "nodeccr3", "&#00a8;" );
  XMLOutputStream_writeAttributeChars(stream, "nodeccr4", "&#00A8;" );
  XMLOutputStream_writeAttributeChars(stream, "nohexcr1", "&#x;"    );
  XMLOutputStream_writeAttributeChars(stream, "nohexcr2", "&#xABCD" );
  XMLOutputStream_endElement(stream, "testcr");

  const char * expected = "<testcr chars=\"one\" amp=\"&amp;\" deccr=\"&#0168;\" hexcr=\"&#x00a8;\" lhexcr=\"&#x00A8;\" nodeccr1=\"&amp;#01688\" nodeccr2=\"&amp;#;\" nodeccr3=\"&amp;#00a8;\" nodeccr4=\"&amp;#00A8;\" nohexcr1=\"&amp;#x;\" nohexcr2=\"&amp;#xABCD\"/>";
  const char * s = XMLOutputStream_getString(stream);

  fail_unless(!strcmp(s,expected));

  XMLOutputStream_free(stream);

}
END_TEST

START_TEST (test_XMLOutputStream_PredefinedEntity)
{
  XMLOutputStream_t *stream = XMLOutputStream_createAsString("", 0);
  XMLOutputStream_startElement(stream, "testpde");
  XMLOutputStream_writeAttributeChars(stream, "amp",     "&"     );
  XMLOutputStream_writeAttributeChars(stream, "apos",    "'"     );
  XMLOutputStream_writeAttributeChars(stream, "gt",      ">"     );
  XMLOutputStream_writeAttributeChars(stream, "lt",      "<"     );
  XMLOutputStream_writeAttributeChars(stream, "quot",    "\""    );
  XMLOutputStream_writeAttributeChars(stream, "pdeamp",  "&amp;" );
  XMLOutputStream_writeAttributeChars(stream, "pdeapos", "&apos;");
  XMLOutputStream_writeAttributeChars(stream, "pdegt",   "&gt;"  );
  XMLOutputStream_writeAttributeChars(stream, "pdelt",   "&lt;"  );
  XMLOutputStream_writeAttributeChars(stream, "pdequot", "&quot;");

  XMLOutputStream_endElement(stream, "testpde");

  const char * expected = "<testpde amp=\"&amp;\" apos=\"&apos;\" gt=\"&gt;\" lt=\"&lt;\" quot=\"&quot;\" pdeamp=\"&amp;\" pdeapos=\"&apos;\" pdegt=\"&gt;\" pdelt=\"&lt;\" pdequot=\"&quot;\"/>";
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
  tcase_add_test( tcase, test_XMLOutputStream_createStdoutWithProgramInfo  );
  tcase_add_test( tcase, test_XMLOutputStream_createFileWithProgramInfo    );
  tcase_add_test( tcase, test_XMLOutputStream_createStringWithProgramInfo  );
  tcase_add_test( tcase, test_XMLOutputStream_startEnd  );
  tcase_add_test( tcase, test_XMLOutputStream_Elements  );
  tcase_add_test( tcase, test_XMLOutputStream_CharacterReference );
  tcase_add_test( tcase, test_XMLOutputStream_PredefinedEntity );
  suite_add_tcase(suite, tcase);

  return suite;
}

