/**
 * \file    TestXMLToken.c
 * \brief   XMLToken unit tests
 * \author  Michael Hucka <mhucka@caltech.edu>
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

#include "common/common.h"
#include "XMLToken.h"
#include "XMLTriple.h"

#include <check.h>


START_TEST (test_XMLToken_create)
{
  XMLToken_t *token;
  XMLTriple_t *triple;
  XMLAttributes_t *attr;

  token = XMLToken_create();
  fail_unless(token != NULL);
  XMLToken_free(token);

  triple = XMLTriple_createWith("attr", "uri", "prefix");

  token = XMLToken_createWithTriple(triple);
  fail_unless(token != NULL);
  fail_unless(strcmp(XMLToken_getName(token), "attr") == 0);
  fail_unless(strcmp(XMLToken_getPrefix(token), "prefix") == 0);
  fail_unless(strcmp(XMLToken_getURI(token), "uri") == 0);
  XMLToken_free(token);

  attr = XMLAttributes_create();
  fail_unless(attr != NULL);
  XMLAttributes_add(attr, "attr2", "value");
  token = XMLToken_createWithTripleAttr(triple, attr);  
  fail_unless(token != NULL);
  const XMLAttributes_t *returnattr = XMLToken_getAttributes(token);
  fail_unless(strcmp(XMLAttributes_getName(returnattr, 0), "attr2") == 0);
  XMLToken_free(token);
  XMLTriple_free(triple);
  XMLAttributes_free(attr);
}
END_TEST


START_TEST (test_XMLToken_fields)
{
  /* Tokens created with just a triple are flagged as end elements. */

  XMLTriple_t *triple;
  XMLToken_t *token;

  triple = XMLTriple_createWith("attr", "uri", "prefix");
  token = XMLToken_createWithTriple(triple);
  fail_unless(XMLToken_isElement(token) == 1);
  fail_unless(XMLToken_isEnd(token) == 1);
  fail_unless(XMLToken_isStart(token) == 0);
  fail_unless(XMLToken_isText(token) == 0);
  fail_unless(XMLToken_isEOF(token) == 0);

  fail_unless(strcmp(XMLToken_getName(token), "attr") == 0);
  fail_unless(strcmp(XMLToken_getURI(token), "uri") == 0);
  fail_unless(strcmp(XMLToken_getPrefix(token), "prefix") == 0);
  XMLToken_free(token);
  XMLTriple_free(triple);
}
END_TEST

START_TEST (test_XMLToken_chars)
{
  XMLToken_t *token;

  token = XMLToken_createWithText("This is text");
  fail_unless(XMLToken_isElement(token) == 0);
  fail_unless(XMLToken_isEnd(token) == 0);
  fail_unless(XMLToken_isStart(token) == 0);
  fail_unless(XMLToken_isText(token) == 1);
  fail_unless(XMLToken_isEOF(token) == 0);

  fail_unless(strcmp(XMLToken_getCharacters(token), "This is text") == 0);

  XMLToken_free(token);
}
END_TEST


Suite *
create_suite_XMLToken (void)
{
  Suite *suite = suite_create("XMLToken");
  TCase *tcase = tcase_create("XMLToken");

  tcase_add_test( tcase, test_XMLToken_create  );
  tcase_add_test( tcase, test_XMLToken_fields  );
  tcase_add_test( tcase, test_XMLToken_chars  );
  suite_add_tcase(suite, tcase);

  return suite;
}

