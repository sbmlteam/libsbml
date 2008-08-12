/**
 * \file    TestXMLToken.c
 * \brief   XMLToken unit tests
 * \author  Michael Hucka <mhucka@caltech.edu>
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


START_TEST (test_XMLToken_namespace_add)
{
  XMLToken_t *token = XMLToken_create();

  fail_unless( XMLToken_getNamespacesLength(token) == 0 );
  fail_unless( XMLToken_isNamespacesEmpty(token)  == 1 );

  XMLToken_addNamespace(token, "http://test1.org/", "test1");
  fail_unless( XMLToken_getNamespacesLength(token) == 1 );
  fail_unless( XMLToken_isNamespacesEmpty(token)  == 0 );

  XMLToken_addNamespace(token, "http://test2.org/", "test2");
  fail_unless( XMLToken_getNamespacesLength(token) == 2 );
  fail_unless( XMLToken_isNamespacesEmpty(token)  == 0 );

  XMLToken_addNamespace(token, "http://test1.org/", "test1a");
  fail_unless( XMLToken_getNamespacesLength(token) == 3 );
  fail_unless( XMLToken_isNamespacesEmpty(token)  == 0 );

  XMLToken_addNamespace(token, "http://test1.org/", "test1a");
  fail_unless( XMLToken_getNamespacesLength(token) == 3 );
  fail_unless( XMLToken_isNamespacesEmpty(token)  == 0 );

  fail_if( XMLToken_getNamespaceIndex(token, "http://test1.org/") == -1);

  XMLToken_free(token);
}
END_TEST


START_TEST (test_XMLToken_namespace_get)
{
  XMLToken_t *token = XMLToken_create();

  XMLToken_addNamespace(token, "http://test1.org/", "test1");    /* index 0 */
  XMLToken_addNamespace(token, "http://test2.org/", "test2");    /* index 1 */
  XMLToken_addNamespace(token, "http://test3.org/", "test3");    /* index 2 */
  XMLToken_addNamespace(token, "http://test4.org/", "test4");    /* index 3 */
  XMLToken_addNamespace(token, "http://test5.org/", "test5");    /* index 4 */
  XMLToken_addNamespace(token, "http://test6.org/", "test6");    /* index 5 */
  XMLToken_addNamespace(token, "http://test7.org/", "test7");    /* index 6 */
  XMLToken_addNamespace(token, "http://test8.org/", "test8");    /* index 7 */
  XMLToken_addNamespace(token, "http://test9.org/", "test9");    /* index 8 */

  fail_unless( XMLToken_getNamespacesLength(token) == 9 );

  fail_unless( XMLToken_getNamespaceIndex(token, "http://test1.org/") == 0 );
  fail_unless( strcmp(XMLToken_getNamespacePrefix(token, 1), "test2") == 0 );
  fail_unless( strcmp(XMLToken_getNamespacePrefixByURI(token, "http://test1.org/"),
		      "test1") == 0 );
  fail_unless( strcmp(XMLToken_getNamespaceURI(token, 1), "http://test2.org/") == 0 );
  fail_unless( strcmp(XMLToken_getNamespaceURIByPrefix(token, "test2"),
		      "http://test2.org/") == 0 );

  fail_unless( XMLToken_getNamespaceIndex(token, "http://test1.org/") ==  0 );
  fail_unless( XMLToken_getNamespaceIndex(token, "http://test2.org/") ==  1 );
  fail_unless( XMLToken_getNamespaceIndex(token, "http://test5.org/") ==  4 );
  fail_unless( XMLToken_getNamespaceIndex(token, "http://test9.org/") ==  8 );
  fail_unless( XMLToken_getNamespaceIndex(token, "http://testX.org/") == -1 );

  fail_unless( XMLToken_hasNamespaceURI(token, "http://test1.org/") !=  0 );
  fail_unless( XMLToken_hasNamespaceURI(token, "http://test2.org/") !=  0 );
  fail_unless( XMLToken_hasNamespaceURI(token, "http://test5.org/") !=  0 );
  fail_unless( XMLToken_hasNamespaceURI(token, "http://test9.org/") !=  0 );
  fail_unless( XMLToken_hasNamespaceURI(token, "http://testX.org/") ==  0 );

  fail_unless( XMLToken_getNamespaceIndexByPrefix(token, "test1") ==  0 );
  fail_unless( XMLToken_getNamespaceIndexByPrefix(token, "test5") ==  4 );
  fail_unless( XMLToken_getNamespaceIndexByPrefix(token, "test9") ==  8 );
  fail_unless( XMLToken_getNamespaceIndexByPrefix(token, "testX") == -1 );

  fail_unless( XMLToken_hasNamespacePrefix(token, "test1") !=  0 );
  fail_unless( XMLToken_hasNamespacePrefix(token, "test5") !=  0 );
  fail_unless( XMLToken_hasNamespacePrefix(token, "test9") !=  0 );
  fail_unless( XMLToken_hasNamespacePrefix(token, "testX") ==  0 );

  fail_unless( XMLToken_hasNamespaceNS(token, "http://test1.org/", "test1") !=  0 );
  fail_unless( XMLToken_hasNamespaceNS(token, "http://test5.org/", "test5") !=  0 );
  fail_unless( XMLToken_hasNamespaceNS(token, "http://test9.org/", "test9") !=  0 );
  fail_unless( XMLToken_hasNamespaceNS(token, "http://testX.org/", "testX") ==  0 );

  XMLToken_free(token);
}
END_TEST


START_TEST (test_XMLToken_namespace_remove)
{
  XMLToken_t *token = XMLToken_create();

  XMLToken_addNamespace(token, "http://test1.org/", "test1"); 
  XMLToken_addNamespace(token, "http://test2.org/", "test2");
  XMLToken_addNamespace(token, "http://test3.org/", "test3"); 
  XMLToken_addNamespace(token, "http://test4.org/", "test4");
  XMLToken_addNamespace(token, "http://test5.org/", "test5");

  fail_unless( XMLToken_getNamespacesLength(token) == 5 );
  XMLToken_removeNamespace(token, 4);
  fail_unless( XMLToken_getNamespacesLength(token) == 4 );
  XMLToken_removeNamespace(token, 3);
  fail_unless( XMLToken_getNamespacesLength(token) == 3 );
  XMLToken_removeNamespace(token, 2);
  fail_unless( XMLToken_getNamespacesLength(token) == 2 );
  XMLToken_removeNamespace(token, 1);
  fail_unless( XMLToken_getNamespacesLength(token) == 1 );
  XMLToken_removeNamespace(token, 0);
  fail_unless( XMLToken_getNamespacesLength(token) == 0 );


  XMLToken_addNamespace(token, "http://test1.org/", "test1");
  XMLToken_addNamespace(token, "http://test2.org/", "test2");
  XMLToken_addNamespace(token, "http://test3.org/", "test3");
  XMLToken_addNamespace(token, "http://test4.org/", "test4");
  XMLToken_addNamespace(token, "http://test5.org/", "test5");

  fail_unless( XMLToken_getNamespacesLength(token) == 5 );
  XMLToken_removeNamespace(token, 0);
  fail_unless( XMLToken_getNamespacesLength(token) == 4 );
  XMLToken_removeNamespace(token, 0);
  fail_unless( XMLToken_getNamespacesLength(token) == 3 );
  XMLToken_removeNamespace(token, 0);
  fail_unless( XMLToken_getNamespacesLength(token) == 2 );
  XMLToken_removeNamespace(token, 0);
  fail_unless( XMLToken_getNamespacesLength(token) == 1 );
  XMLToken_removeNamespace(token, 0);
  fail_unless( XMLToken_getNamespacesLength(token) == 0 );

  XMLToken_free(token);
}
END_TEST


START_TEST (test_XMLToken_namespace_remove_by_prefix)
{
  XMLToken_t *token = XMLToken_create();

  XMLToken_addNamespace(token, "http://test1.org/", "test1"); 
  XMLToken_addNamespace(token, "http://test2.org/", "test2");
  XMLToken_addNamespace(token, "http://test3.org/", "test3"); 
  XMLToken_addNamespace(token, "http://test4.org/", "test4");
  XMLToken_addNamespace(token, "http://test5.org/", "test5");

  fail_unless( XMLToken_getNamespacesLength(token) == 5 );
  XMLToken_removeNamespaceByPrefix(token, "test1");
  fail_unless( XMLToken_getNamespacesLength(token) == 4 );
  XMLToken_removeNamespaceByPrefix(token, "test2");
  fail_unless( XMLToken_getNamespacesLength(token) == 3 );
  XMLToken_removeNamespaceByPrefix(token, "test3");
  fail_unless( XMLToken_getNamespacesLength(token) == 2 );
  XMLToken_removeNamespaceByPrefix(token, "test4");
  fail_unless( XMLToken_getNamespacesLength(token) == 1 );
  XMLToken_removeNamespaceByPrefix(token, "test5");
  fail_unless( XMLToken_getNamespacesLength(token) == 0 );

  XMLToken_addNamespace(token, "http://test1.org/", "test1");
  XMLToken_addNamespace(token, "http://test2.org/", "test2");
  XMLToken_addNamespace(token, "http://test3.org/", "test3");
  XMLToken_addNamespace(token, "http://test4.org/", "test4");
  XMLToken_addNamespace(token, "http://test5.org/", "test5");

  fail_unless( XMLToken_getNamespacesLength(token) == 5 );
  XMLToken_removeNamespaceByPrefix(token, "test5");
  fail_unless( XMLToken_getNamespacesLength(token) == 4 );
  XMLToken_removeNamespaceByPrefix(token, "test4");
  fail_unless( XMLToken_getNamespacesLength(token) == 3 );
  XMLToken_removeNamespaceByPrefix(token, "test3");
  fail_unless( XMLToken_getNamespacesLength(token) == 2 );
  XMLToken_removeNamespaceByPrefix(token, "test2");
  fail_unless( XMLToken_getNamespacesLength(token) == 1 );
  XMLToken_removeNamespaceByPrefix(token, "test1");
  fail_unless( XMLToken_getNamespacesLength(token) == 0 );

  XMLToken_addNamespace(token, "http://test1.org/", "test1"); 
  XMLToken_addNamespace(token, "http://test2.org/", "test2"); 
  XMLToken_addNamespace(token, "http://test3.org/", "test3");
  XMLToken_addNamespace(token, "http://test4.org/", "test4");
  XMLToken_addNamespace(token, "http://test5.org/", "test5");

  fail_unless( XMLToken_getNamespacesLength(token) == 5 );
  XMLToken_removeNamespaceByPrefix(token, "test3");
  fail_unless( XMLToken_getNamespacesLength(token) == 4 );
  XMLToken_removeNamespaceByPrefix(token, "test1");
  fail_unless( XMLToken_getNamespacesLength(token) == 3 );
  XMLToken_removeNamespaceByPrefix(token, "test4");
  fail_unless( XMLToken_getNamespacesLength(token) == 2 );
  XMLToken_removeNamespaceByPrefix(token, "test5");
  fail_unless( XMLToken_getNamespacesLength(token) == 1 );
  XMLToken_removeNamespaceByPrefix(token, "test2");
  fail_unless( XMLToken_getNamespacesLength(token) == 0 );

  XMLToken_free(token);
}
END_TEST


START_TEST (test_XMLToken_namespace_set_clear )
{
  XMLToken_t *token   = XMLToken_create();
  XMLNamespaces_t* ns = XMLNamespaces_create();

  fail_unless( XMLToken_getNamespacesLength(token) == 0 );
  fail_unless( XMLToken_isNamespacesEmpty(token)   == 1 );  

  XMLNamespaces_add(ns, "http://test1.org/", "test1"); 
  XMLNamespaces_add(ns, "http://test2.org/", "test2");
  XMLNamespaces_add(ns, "http://test3.org/", "test3"); 
  XMLNamespaces_add(ns, "http://test4.org/", "test4");
  XMLNamespaces_add(ns, "http://test5.org/", "test5");

  XMLToken_setNamespaces(token, ns);

  fail_unless(XMLToken_getNamespacesLength(token) == 5 );
  fail_unless(XMLToken_isNamespacesEmpty(token)   == 0 );  
  fail_unless(strcmp(XMLToken_getNamespacePrefix(token, 0), "test1") == 0 );
  fail_unless(strcmp(XMLToken_getNamespacePrefix(token, 1), "test2") == 0 );
  fail_unless(strcmp(XMLToken_getNamespacePrefix(token, 2), "test3") == 0 );
  fail_unless(strcmp(XMLToken_getNamespacePrefix(token, 3), "test4") == 0 );
  fail_unless(strcmp(XMLToken_getNamespacePrefix(token, 4), "test5") == 0 );
  fail_unless(strcmp(XMLToken_getNamespaceURI(token, 0), "http://test1.org/") == 0 );
  fail_unless(strcmp(XMLToken_getNamespaceURI(token, 1), "http://test2.org/") == 0 );
  fail_unless(strcmp(XMLToken_getNamespaceURI(token, 2), "http://test3.org/") == 0 );
  fail_unless(strcmp(XMLToken_getNamespaceURI(token, 3), "http://test4.org/") == 0 );
  fail_unless(strcmp(XMLToken_getNamespaceURI(token, 4), "http://test5.org/") == 0 );

  XMLToken_clearNamespaces(token);
  fail_unless( XMLToken_getNamespacesLength(token) == 0 );

  XMLNamespaces_free(ns);
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
  tcase_add_test( tcase, test_XMLToken_namespace_add );
  tcase_add_test( tcase, test_XMLToken_namespace_get );
  tcase_add_test( tcase, test_XMLToken_namespace_remove );
  tcase_add_test( tcase, test_XMLToken_namespace_remove_by_prefix );
  tcase_add_test( tcase, test_XMLToken_namespace_set_clear );

  suite_add_tcase(suite, tcase);

  return suite;
}

