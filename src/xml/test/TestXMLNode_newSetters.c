/**
 * \file    TestXMLNode_newSetters.c
 * \brief   XMLNode unit tests
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
#include <sbml/xml/XMLTriple.h>
#include <sbml/xml/XMLToken.h>
#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLNamespaces.h>

#include <check.h>

#if __cplusplus
BEGIN_C_DECL
#endif

START_TEST (test_XMLNode_addChild1)
{
  XMLNode_t *node = XMLNode_create();
  XMLNode_t *node2 = XMLNode_create();

  int i = XMLNode_addChild(node, node2);

  fail_unless( i == LIBSBML_OPERATION_SUCCESS);
  fail_unless(XMLNode_getNumChildren(node) == 1);

  XMLNode_free(node);
  XMLNode_free(node2);
}
END_TEST


START_TEST (test_XMLNode_addChild2)
{
  XMLTriple_t*     triple = XMLTriple_createWith("test","","");
  XMLAttributes_t* attr   = XMLAttributes_create();
  XMLNode_t *node = XMLNode_createStartElement(triple, attr);
  XMLNode_t *node2 = XMLNode_create();

  int i = XMLNode_addChild(node, node2);

  fail_unless( i == LIBSBML_OPERATION_SUCCESS);
  fail_unless(XMLNode_getNumChildren(node) == 1);

  XMLTriple_free(triple);
  XMLAttributes_free(attr);
  XMLNode_free(node);
  XMLNode_free(node2);
}
END_TEST


START_TEST (test_XMLNode_addChild3)
{
  XMLTriple_t*     triple = XMLTriple_createWith("test","","");
  XMLNode_t *node = XMLNode_createEndElement(triple);
  XMLNode_t *node2 = XMLNode_create();

  int i = XMLNode_addChild(node, node2);

  fail_unless( i == LIBSBML_INVALID_XML_OPERATION);
  fail_unless(XMLNode_getNumChildren(node) == 0);

  XMLTriple_free(triple);
  XMLNode_free(node);
  XMLNode_free(node2);
}
END_TEST


START_TEST (test_XMLNode_removeChildren)
{
  XMLNode_t *node = XMLNode_create();
  XMLNode_t *node2 = XMLNode_create();
  XMLNode_t *node3 = XMLNode_create();

  XMLNode_addChild(node, node2);
  XMLNode_addChild(node, node3);

  fail_unless(XMLNode_getNumChildren(node) == 2);

  int i = XMLNode_removeChildren(node);
  fail_unless( i == LIBSBML_OPERATION_SUCCESS);
  fail_unless(XMLNode_getNumChildren(node) == 0);

  XMLNode_free(node);
  XMLNode_free(node2);
  XMLNode_free(node3);
}
END_TEST


START_TEST(test_XMLNode_removeAttributes)
{
  /*-- setup --*/

  XMLTriple_t*     triple = XMLTriple_createWith("test","","");
  XMLAttributes_t* attr   = XMLAttributes_create();
  XMLNode_t*      node  = XMLNode_createStartElement(triple, attr);

  XMLTriple_t* xt2    = XMLTriple_createWith("name3", "http://name3.org/", "p3");
  XMLTriple_t* xt1    = XMLTriple_createWith("name5", "http://name5.org/", "p5");
  int i = XMLNode_addAttr(node, "name1", "val1");
  
  fail_unless(i == LIBSBML_OPERATION_SUCCESS);
  fail_unless (XMLAttributes_getLength(XMLNode_getAttributes(node)) == 1);

  i = XMLNode_addAttrWithNS(node, "name2", "val2", "http://name1.org/", "p1");
  fail_unless(i == LIBSBML_OPERATION_SUCCESS);
  fail_unless (XMLAttributes_getLength(XMLNode_getAttributes(node)) == 2);

  i = XMLNode_addAttrWithTriple(node, xt2, "val2");
  
  fail_unless(i == LIBSBML_OPERATION_SUCCESS);
  fail_unless (XMLAttributes_getLength(XMLNode_getAttributes(node)) == 3);

  i = XMLNode_addAttr(node, "name4", "val4");

  fail_unless(i == LIBSBML_OPERATION_SUCCESS);
  fail_unless (XMLAttributes_getLength(XMLNode_getAttributes(node)) == 4);

  i = XMLNode_removeAttr(node, 7);

  fail_unless ( i == LIBSBML_INDEX_EXCEEDS_SIZE );

  i = XMLNode_removeAttrByName(node, "name7");

  fail_unless ( i == LIBSBML_INDEX_EXCEEDS_SIZE );

  i = XMLNode_removeAttrByNS(node, "name7", "namespaces7");

  fail_unless ( i == LIBSBML_INDEX_EXCEEDS_SIZE );

  i = XMLNode_removeAttrByTriple(node, xt1);

  fail_unless ( i == LIBSBML_INDEX_EXCEEDS_SIZE );
  fail_unless (XMLAttributes_getLength(XMLNode_getAttributes(node)) == 4);

  i = XMLNode_removeAttr(node, 3);

  fail_unless ( i == LIBSBML_OPERATION_SUCCESS );
  fail_unless (XMLAttributes_getLength(XMLNode_getAttributes(node)) == 3);

  i = XMLNode_removeAttrByName(node, "name1");

  fail_unless ( i == LIBSBML_OPERATION_SUCCESS );
  fail_unless (XMLAttributes_getLength(XMLNode_getAttributes(node)) == 2);

  i = XMLNode_removeAttrByNS(node, "name2", "http://name1.org/");

  fail_unless ( i == LIBSBML_OPERATION_SUCCESS );
  fail_unless (XMLAttributes_getLength(XMLNode_getAttributes(node)) == 1);

  i = XMLNode_removeAttrByTriple(node, xt2);

  fail_unless ( i == LIBSBML_OPERATION_SUCCESS );
  fail_unless (XMLAttributes_getLength(XMLNode_getAttributes(node)) == 0);

  /*-- teardown --*/

  XMLTriple_free(xt1);
  XMLTriple_free(xt2);
  XMLTriple_free(triple);
  XMLAttributes_free(attr);
  XMLNode_free(node);
}
END_TEST


START_TEST(test_XMLNode_clearAttributes)
{
  /*-- setup --*/

  XMLTriple_t*     triple = XMLTriple_createWith("test","","");
  XMLAttributes_t* attr   = XMLAttributes_create();
  XMLNode_t*      node  = XMLNode_createStartElement(triple, attr);

  XMLTriple_t* xt2    = XMLTriple_createWith("name3", "http://name3.org/", "p3");
  XMLTriple_t* xt1    = XMLTriple_createWith("name5", "http://name5.org/", "p5");
  int i = XMLNode_addAttr(node, "name1", "val1");
  
  fail_unless(i == LIBSBML_OPERATION_SUCCESS);
  fail_unless (XMLAttributes_getLength(XMLNode_getAttributes(node)) == 1);

  i = XMLNode_addAttrWithNS(node, "name2", "val2", "http://name1.org/", "p1");
  fail_unless(i == LIBSBML_OPERATION_SUCCESS);
  fail_unless (XMLAttributes_getLength(XMLNode_getAttributes(node)) == 2);

  i = XMLNode_addAttrWithTriple(node, xt2, "val2");
  
  fail_unless(i == LIBSBML_OPERATION_SUCCESS);
  fail_unless (XMLAttributes_getLength(XMLNode_getAttributes(node)) == 3);

  i = XMLNode_addAttr(node, "name4", "val4");

  fail_unless(i == LIBSBML_OPERATION_SUCCESS);
  fail_unless (XMLAttributes_getLength(XMLNode_getAttributes(node)) == 4);

  i = XMLNode_clearAttributes(node);

  fail_unless ( i == LIBSBML_OPERATION_SUCCESS );
  fail_unless (XMLAttributes_getLength(XMLNode_getAttributes(node)) == 0);

  /*-- teardown --*/

  XMLTriple_free(xt1);
  XMLTriple_free(xt2);
  XMLTriple_free(triple);
  XMLAttributes_free(attr);
  XMLNode_free(node);
}
END_TEST


START_TEST(test_XMLNode_removeNamespaces)
{
  /*-- setup --*/

  XMLTriple_t*     triple = XMLTriple_createWith("test","","");
  XMLAttributes_t* attr   = XMLAttributes_create();
  XMLNode_t*       node   = XMLNode_createStartElement(triple, attr);
  const XMLNamespaces_t* nms;

  int i = XMLNode_addNamespace(node, "http://test1.org/", "test1");
  
  fail_unless(i == LIBSBML_OPERATION_SUCCESS);
  nms = XMLNode_getNamespaces(node);
  fail_unless (XMLNamespaces_getLength(nms) == 1);

  i = XMLNode_addNamespace(node, "http://test2.org/", "test2");
  
  fail_unless(i == LIBSBML_OPERATION_SUCCESS);
  nms = XMLNode_getNamespaces(node);
  fail_unless (XMLNamespaces_getLength(nms) == 2);

  i = XMLNode_removeNamespace(node, 7);

  fail_unless ( i == LIBSBML_INDEX_EXCEEDS_SIZE );
  nms = XMLNode_getNamespaces(node);
  fail_unless (XMLNamespaces_getLength(nms) == 2);

  i = XMLNode_removeNamespaceByPrefix(node, "name7");

  fail_unless ( i == LIBSBML_INDEX_EXCEEDS_SIZE );
  nms = XMLNode_getNamespaces(node);
  fail_unless (XMLNamespaces_getLength(nms) == 2);

  i = XMLNode_removeNamespace(node, 0);

  fail_unless ( i == LIBSBML_OPERATION_SUCCESS );
  nms = XMLNode_getNamespaces(node);
  fail_unless (XMLNamespaces_getLength(nms) == 1);

  i = XMLNode_removeNamespaceByPrefix(node, "test2");

  fail_unless ( i == LIBSBML_OPERATION_SUCCESS );
  nms = XMLNode_getNamespaces(node);
  fail_unless (XMLNamespaces_getLength(nms) == 0);

  XMLTriple_free(triple);
  XMLAttributes_free(attr);
  XMLNode_free(node);
}
END_TEST


START_TEST(test_XMLNode_clearNamespaces)
{
  /*-- setup --*/

  XMLTriple_t*     triple = XMLTriple_createWith("test","","");
  XMLAttributes_t* attr   = XMLAttributes_create();
  XMLNode_t*       node   = XMLNode_createStartElement(triple, attr);
  const XMLNamespaces_t* nms;

  int i = XMLNode_addNamespace(node, "http://test1.org/", "test1");
  
  fail_unless(i == LIBSBML_OPERATION_SUCCESS);
  nms = XMLNode_getNamespaces(node);
  fail_unless (XMLNamespaces_getLength(nms) == 1);

  i = XMLNode_addNamespace(node, "http://test2.org/", "test2");
  
  fail_unless(i == LIBSBML_OPERATION_SUCCESS);
  nms = XMLNode_getNamespaces(node);
  fail_unless (XMLNamespaces_getLength(nms) == 2);

  i = XMLNode_clearNamespaces(node);

  fail_unless ( i == LIBSBML_OPERATION_SUCCESS );
  nms = XMLNode_getNamespaces(node);
  fail_unless (XMLNamespaces_getLength(nms) == 0);

  XMLTriple_free(triple);
  XMLAttributes_free(attr);
  XMLNode_free(node);
}
END_TEST


Suite *
create_suite_XMLNode_newSetters (void)
{
  Suite *suite = suite_create("XMLNode_newSetters");
  TCase *tcase = tcase_create("XMLNode_newSetters");

  tcase_add_test( tcase, test_XMLNode_addChild1  );
  tcase_add_test( tcase, test_XMLNode_addChild2  );
  tcase_add_test( tcase, test_XMLNode_addChild3  );
  tcase_add_test( tcase, test_XMLNode_removeChildren  );
  tcase_add_test( tcase, test_XMLNode_removeAttributes  );
  tcase_add_test( tcase, test_XMLNode_clearAttributes  );
  tcase_add_test( tcase, test_XMLNode_removeNamespaces  );
  tcase_add_test( tcase, test_XMLNode_clearNamespaces  );

  suite_add_tcase(suite, tcase);

  return suite;
}

#if __cplusplus
END_C_DECL
#endif

