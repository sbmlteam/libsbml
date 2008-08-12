/**
 * \file    TestXMLNode.c
 * \brief   XMLNode unit tests
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
#include <sbml/xml/XMLTriple.h>
#include <sbml/xml/XMLToken.h>
#include <sbml/xml/XMLNode.h>

#include <check.h>


START_TEST (test_XMLNode_create)
{
  XMLNode_t *node = XMLNode_create();

  fail_unless(node != NULL);
  fail_unless(XMLNode_getNumChildren(node) == 0);
  XMLNode_free(node);

  node = XMLNode_create();
  fail_unless(node != NULL);

  XMLNode_t *node2 = XMLNode_create();
  fail_unless(node2 != NULL);

  XMLNode_addChild(node, node2);
  fail_unless(XMLNode_getNumChildren(node) == 1);

  XMLNode_t *node3 = XMLNode_create();
  fail_unless(node3 != NULL);

  XMLNode_addChild(node, node3);
  fail_unless(XMLNode_getNumChildren(node) == 2);

  XMLNode_free(node);
  XMLNode_free(node2);
  XMLNode_free(node3);
}
END_TEST


START_TEST (test_XMLNode_createFromToken)
{
  XMLToken_t *token;
  XMLTriple_t *triple;
  XMLNode_t *node;

  triple = XMLTriple_createWith("attr", "uri", "prefix");
  token = XMLToken_createWithTriple(triple);
  node = XMLNode_createFromToken(token);

  fail_unless(node != NULL);
  fail_unless(XMLNode_getNumChildren(node) == 0);

  fail_unless(strcmp(XMLNode_getName(node), "attr") == 0);
  fail_unless(strcmp(XMLNode_getPrefix(node), "prefix") == 0);
  fail_unless(strcmp(XMLNode_getURI(node), "uri") == 0);
  fail_unless (XMLNode_getChild(node, 1) != NULL);

  XMLToken_free(token);
  XMLTriple_free(triple);
  XMLNode_free(node);



}
END_TEST


START_TEST (test_XMLNode_createElement)
{
  XMLTriple_t     *triple;
  XMLAttributes_t *attr;
  XMLNamespaces_t *ns;
  XMLNode_t *snode, *enode, *tnode;

  const XMLAttributes_t* cattr;
  const char* name   = "test";
  const char* uri    = "http://test.org/";
  const char* prefix = "p";
  const char* text   = "text node";

  /* start element with namespace */

  triple = XMLTriple_createWith(name, uri, prefix);
  ns     = XMLNamespaces_create();
  attr   = XMLAttributes_create();
  XMLNamespaces_add(ns, uri, prefix);
  XMLAttributes_addWithNamespace(attr,"id", "value", uri, prefix);
  snode = XMLNode_createStartElementNS(triple, attr, ns);

  fail_unless(snode != NULL);
  fail_unless(XMLNode_getNumChildren(snode) == 0);
  fail_unless(strcmp(XMLNode_getName  (snode), name  ) == 0);
  fail_unless(strcmp(XMLNode_getPrefix(snode), prefix) == 0);
  fail_unless(strcmp(XMLNode_getURI   (snode), uri   ) == 0);
  fail_unless(XMLNode_isElement(snode) == 1);
  fail_unless(XMLNode_isStart  (snode) == 1);
  fail_unless(XMLNode_isEnd    (snode) == 0);
  fail_unless(XMLNode_isText   (snode) == 0);

  XMLNode_setEnd(snode);
  fail_unless ( XMLNode_isEnd(snode) == 1);
  XMLNode_unsetEnd(snode);
  fail_unless ( XMLNode_isEnd(snode) == 0);

  cattr = XMLNode_getAttributes(snode);
  fail_unless(cattr != NULL);
  fail_unless(strcmp(XMLAttributes_getName  (cattr, 0), "id"   ) == 0);
  fail_unless(strcmp(XMLAttributes_getValue (cattr, 0), "value") == 0);
  fail_unless(strcmp(XMLAttributes_getPrefix(cattr, 0),  prefix) == 0);
  fail_unless(strcmp(XMLAttributes_getURI   (cattr, 0),  uri   ) == 0);

  XMLTriple_free(triple);
  XMLAttributes_free(attr);
  XMLNamespaces_free(ns);
  XMLNode_free(snode);

  /* start element */

  attr   = XMLAttributes_create();
  XMLAttributes_add(attr,"id", "value");
  triple = XMLTriple_createWith(name, "", "");
  snode  = XMLNode_createStartElement(triple, attr);

  fail_unless(snode != NULL);
  fail_unless(XMLNode_getNumChildren(snode) == 0);
  fail_unless(strcmp(XMLNode_getName  (snode), "test") == 0);
  fail_unless(XMLNode_getPrefix(snode) == NULL );
  fail_unless(XMLNode_getURI   (snode) == NULL );
  fail_unless(XMLNode_isElement(snode) == 1);
  fail_unless(XMLNode_isStart  (snode) == 1);
  fail_unless(XMLNode_isEnd    (snode) == 0);
  fail_unless(XMLNode_isText   (snode) == 0);

  cattr = XMLNode_getAttributes(snode);
  fail_unless(cattr != NULL);
  fail_unless(strcmp(XMLAttributes_getName  (cattr, 0), "id"   ) == 0);
  fail_unless(strcmp(XMLAttributes_getValue (cattr, 0), "value") == 0);
  fail_unless(XMLAttributes_getPrefix(cattr, 0) == NULL);
  fail_unless(XMLAttributes_getURI   (cattr, 0) == NULL);

  /* end element */

  enode = XMLNode_createEndElement(triple);
  fail_unless(enode != NULL);
  fail_unless(XMLNode_getNumChildren(enode) == 0);
  fail_unless(strcmp(XMLNode_getName(enode), "test") == 0);
  fail_unless(XMLNode_getPrefix(enode) == NULL );
  fail_unless(XMLNode_getURI   (enode) == NULL );
  fail_unless(XMLNode_isElement(enode) == 1);
  fail_unless(XMLNode_isStart  (enode) == 0);
  fail_unless(XMLNode_isEnd    (enode) == 1);
  fail_unless(XMLNode_isText   (enode) == 0);

  /* text node */

  tnode = XMLNode_createTextNode(text);
  fail_unless(tnode != NULL);
  fail_unless(strcmp(XMLNode_getCharacters(tnode), text) == 0);
  fail_unless(XMLNode_getNumChildren(tnode) == 0);
  fail_unless(XMLNode_getName  (tnode) == NULL);
  fail_unless(XMLNode_getPrefix(tnode) == NULL );
  fail_unless(XMLNode_getURI   (tnode) == NULL );
  fail_unless(XMLNode_isElement(tnode) == 0);
  fail_unless(XMLNode_isStart  (tnode) == 0);
  fail_unless(XMLNode_isEnd    (tnode) == 0);
  fail_unless(XMLNode_isText   (tnode) == 1);

  XMLTriple_free(triple);
  XMLAttributes_free(attr);
  XMLNode_free(snode);
  XMLNode_free(enode);
  XMLNode_free(tnode);

}
END_TEST


START_TEST (test_XMLNode_getters)
{
  XMLToken_t *token;
  XMLNode_t *node;
  XMLTriple_t *triple;
  XMLAttributes_t *attr;
  XMLNamespaces_t *NS;

  NS = XMLNamespaces_create();
  XMLNamespaces_add(NS, "http://test1.org/", "test1");

  token = XMLToken_createWithText("This is a test");
  node = XMLNode_createFromToken(token);

  fail_unless(node != NULL);
  fail_unless(XMLNode_getNumChildren(node) == 0);

  fail_unless(strcmp(XMLNode_getCharacters(node), "This is a test") == 0);
  fail_unless (XMLNode_getChild(node, 1) != NULL);

  attr = XMLAttributes_create();
  fail_unless(attr != NULL);
  XMLAttributes_add(attr, "attr2", "value");
  
  triple = XMLTriple_createWith("attr", "uri", "prefix");
  token = XMLToken_createWithTripleAttr(triple, attr);  

  fail_unless(token != NULL);
  node = XMLNode_createFromToken(token);

  fail_unless(strcmp(XMLNode_getName(node), "attr") == 0);
  fail_unless(strcmp(XMLNode_getURI(node), "uri") == 0);
  fail_unless(strcmp(XMLNode_getPrefix(node), "prefix") == 0);

  const XMLAttributes_t *returnattr = XMLNode_getAttributes(node);
  fail_unless(strcmp(XMLAttributes_getName(returnattr, 0), "attr2") == 0);
  fail_unless(strcmp(XMLAttributes_getValue(returnattr, 0), "value") == 0);

  token = XMLToken_createWithTripleAttrNS(triple, attr, NS); 
  node = XMLNode_createFromToken(token);

  const XMLNamespaces_t *returnNS = XMLNode_getNamespaces(node);
  fail_unless( XMLNamespaces_getLength(returnNS) == 1 );
  fail_unless( XMLNamespaces_isEmpty(returnNS) == 0 );
  
  XMLTriple_free(triple);
  XMLToken_free(token);
  XMLNode_free(node);

}
END_TEST


START_TEST (test_XMLNode_convert)
{
  const char* xmlstr = "<annotation>\n"
                       "  <test xmlns=\"http://test.org/\" id=\"test\">test</test>\n"
                       "</annotation>";
  XMLNode_t       *node;
  const XMLNode_t *child, *gchild;
  const XMLAttributes_t *attr;
  const XMLNamespaces_t *ns;

  node   = XMLNode_convertStringToXMLNode(xmlstr, NULL);
  child  = XMLNode_getChild(node,0);
  gchild = XMLNode_getChild(child,0);
  attr   = XMLNode_getAttributes(child);
  ns     = XMLNode_getNamespaces(child);

  fail_unless(strcmp(XMLNode_getName(node), "annotation") == 0);
  fail_unless(strcmp(XMLNode_getName(child),"test" ) == 0);
  fail_unless(strcmp(XMLNode_getCharacters(gchild),"test" ) == 0);
  fail_unless(strcmp(XMLAttributes_getName (attr,0), "id"   ) == 0);
  fail_unless(strcmp(XMLAttributes_getValue(attr,0), "test" ) == 0);
  fail_unless(strcmp(XMLNamespaces_getURI(ns,0), "http://test.org/" ) == 0 );
  fail_unless(XMLNamespaces_getPrefix(ns,0) == NULL );

  char* toxmlstring = XMLNode_toXMLString(node);
  fail_unless( strcmp(toxmlstring, xmlstr) == 0);

  XMLNode_free(node);
  safe_free(toxmlstring);

}
END_TEST


START_TEST (test_XMLNode_insert)
{
  /* setup */

  XMLAttributes_t* attr = XMLAttributes_create();

  XMLTriple_t *trp_p  = XMLTriple_createWith("parent","","");
  XMLTriple_t *trp_c1 = XMLTriple_createWith("child1","","");
  XMLTriple_t *trp_c2 = XMLTriple_createWith("child2","","");
  XMLTriple_t *trp_c3 = XMLTriple_createWith("child3","","");
  XMLTriple_t *trp_c4 = XMLTriple_createWith("child4","","");
  XMLTriple_t *trp_c5 = XMLTriple_createWith("child5","","");

  XMLNode_t *p  = XMLNode_createStartElement(trp_p,  attr);
  XMLNode_t *c1 = XMLNode_createStartElement(trp_c1, attr);
  XMLNode_t *c2 = XMLNode_createStartElement(trp_c2, attr);
  XMLNode_t *c3 = XMLNode_createStartElement(trp_c3, attr);
  XMLNode_t *c4 = XMLNode_createStartElement(trp_c4, attr);
  XMLNode_t *c5 = XMLNode_createStartElement(trp_c5, attr);

  /* test of insert */

  XMLNode_addChild(p, c2);
  XMLNode_addChild(p, c4);
  XMLNode_insertChild(p, 0, c1);
  XMLNode_insertChild(p, 2, c3);
  XMLNode_insertChild(p, 4, c5);
  fail_unless(XMLNode_getNumChildren(p) == 5);
  fail_unless(strcmp(XMLNode_getName(XMLNode_getChild(p,0)), "child1") == 0);
  fail_unless(strcmp(XMLNode_getName(XMLNode_getChild(p,1)), "child2") == 0);
  fail_unless(strcmp(XMLNode_getName(XMLNode_getChild(p,2)), "child3") == 0);
  fail_unless(strcmp(XMLNode_getName(XMLNode_getChild(p,3)), "child4") == 0);
  fail_unless(strcmp(XMLNode_getName(XMLNode_getChild(p,4)), "child5") == 0);

  XMLNode_removeChildren(p);

  XMLNode_insertChild(p, 0, c1);
  XMLNode_insertChild(p, 0, c2);
  XMLNode_insertChild(p, 0, c3);
  XMLNode_insertChild(p, 0, c4);
  XMLNode_insertChild(p, 0, c5);
  fail_unless(XMLNode_getNumChildren(p) == 5);
  fail_unless(strcmp(XMLNode_getName(XMLNode_getChild(p,0)), "child5") == 0);
  fail_unless(strcmp(XMLNode_getName(XMLNode_getChild(p,1)), "child4") == 0);
  fail_unless(strcmp(XMLNode_getName(XMLNode_getChild(p,2)), "child3") == 0);
  fail_unless(strcmp(XMLNode_getName(XMLNode_getChild(p,3)), "child2") == 0);
  fail_unless(strcmp(XMLNode_getName(XMLNode_getChild(p,4)), "child1") == 0);

  XMLNode_removeChildren(p);

  /* test of insert by an index which is out of range */

  XMLNode_insertChild(p, 1, c1);
  XMLNode_insertChild(p, 2, c2);
  XMLNode_insertChild(p, 3, c3);
  XMLNode_insertChild(p, 4, c4);
  XMLNode_insertChild(p, 5, c5);
  fail_unless(XMLNode_getNumChildren(p) == 5);
  fail_unless(strcmp(XMLNode_getName(XMLNode_getChild(p,0)), "child1") == 0);
  fail_unless(strcmp(XMLNode_getName(XMLNode_getChild(p,1)), "child2") == 0);
  fail_unless(strcmp(XMLNode_getName(XMLNode_getChild(p,2)), "child3") == 0);
  fail_unless(strcmp(XMLNode_getName(XMLNode_getChild(p,3)), "child4") == 0);
  fail_unless(strcmp(XMLNode_getName(XMLNode_getChild(p,4)), "child5") == 0);

  XMLNode_removeChildren(p);

  /* test for the return value of insert */

  XMLNode_t* tmp;

  tmp = XMLNode_insertChild(p, 0, c1);
  fail_unless(strcmp(XMLNode_getName(tmp),"child1") == 0);
  tmp = XMLNode_insertChild(p, 0, c2);
  fail_unless(strcmp(XMLNode_getName(tmp),"child2") == 0);
  tmp = XMLNode_insertChild(p, 0, c3);
  fail_unless(strcmp(XMLNode_getName(tmp),"child3") == 0);
  tmp = XMLNode_insertChild(p, 0, c4);
  fail_unless(strcmp(XMLNode_getName(tmp),"child4") == 0);
  tmp = XMLNode_insertChild(p, 0, c5);
  fail_unless(strcmp(XMLNode_getName(tmp),"child5") == 0);

  XMLNode_removeChildren(p);

  tmp = XMLNode_insertChild(p, 1, c1);
  fail_unless(strcmp(XMLNode_getName(tmp),"child1") == 0);
  tmp = XMLNode_insertChild(p, 2, c2);
  fail_unless(strcmp(XMLNode_getName(tmp),"child2") == 0);
  tmp = XMLNode_insertChild(p, 3, c3);
  fail_unless(strcmp(XMLNode_getName(tmp),"child3") == 0);
  tmp = XMLNode_insertChild(p, 4, c4);
  fail_unless(strcmp(XMLNode_getName(tmp),"child4") == 0);
  tmp = XMLNode_insertChild(p, 5, c5);
  fail_unless(strcmp(XMLNode_getName(tmp),"child5") == 0);

  /* teardown*/

  XMLNode_free(p);
  XMLNode_free(c1);
  XMLNode_free(c2);
  XMLNode_free(c3);
  XMLNode_free(c4);
  XMLNode_free(c5);
  XMLAttributes_free(attr);
  XMLTriple_free(trp_p);
  XMLTriple_free(trp_c1);
  XMLTriple_free(trp_c2);
  XMLTriple_free(trp_c3);
  XMLTriple_free(trp_c4);
  XMLTriple_free(trp_c5);

}
END_TEST


START_TEST (test_XMLNode_remove)
{
  /* setup */

  XMLAttributes_t* attr = XMLAttributes_create();

  XMLTriple_t *trp_p  = XMLTriple_createWith("parent","","");
  XMLTriple_t *trp_c1 = XMLTriple_createWith("child1","","");
  XMLTriple_t *trp_c2 = XMLTriple_createWith("child2","","");
  XMLTriple_t *trp_c3 = XMLTriple_createWith("child3","","");
  XMLTriple_t *trp_c4 = XMLTriple_createWith("child4","","");
  XMLTriple_t *trp_c5 = XMLTriple_createWith("child5","","");

  XMLNode_t *p  = XMLNode_createStartElement(trp_p,  attr);
  XMLNode_t *c1 = XMLNode_createStartElement(trp_c1, attr);
  XMLNode_t *c2 = XMLNode_createStartElement(trp_c2, attr);
  XMLNode_t *c3 = XMLNode_createStartElement(trp_c3, attr);
  XMLNode_t *c4 = XMLNode_createStartElement(trp_c4, attr);
  XMLNode_t *c5 = XMLNode_createStartElement(trp_c5, attr);

  /* test of remove */

  XMLNode_t* r;

  XMLNode_addChild(p, c1);
  XMLNode_addChild(p, c2);
  XMLNode_addChild(p, c3);
  XMLNode_addChild(p, c4);
  XMLNode_addChild(p, c5);

  r = XMLNode_removeChild(p, 5);
  fail_unless( r == NULL );

  r = XMLNode_removeChild(p, 1);
  fail_unless(XMLNode_getNumChildren(p) == 4);
  fail_unless(strcmp(XMLNode_getName(r),"child2") == 0);
  XMLNode_free(r);

  r = XMLNode_removeChild(p, 3);
  fail_unless(XMLNode_getNumChildren(p) == 3);
  fail_unless(strcmp(XMLNode_getName(r),"child5") == 0);
  XMLNode_free(r);

  r = XMLNode_removeChild(p, 0);
  fail_unless(XMLNode_getNumChildren(p) == 2);
  fail_unless(strcmp(XMLNode_getName(r),"child1") == 0);
  XMLNode_free(r);

  r = XMLNode_removeChild(p, 1);
  fail_unless(XMLNode_getNumChildren(p) == 1);
  fail_unless(strcmp(XMLNode_getName(r),"child4") == 0);
  XMLNode_free(r);

  r = XMLNode_removeChild(p, 0);
  fail_unless(XMLNode_getNumChildren(p) == 0);
  fail_unless(strcmp(XMLNode_getName(r),"child3") == 0);
  XMLNode_free(r);

  /* test of sequential remove (in reverse order) */

  XMLNode_addChild(p, c1);
  XMLNode_addChild(p, c2);
  XMLNode_addChild(p, c3);
  XMLNode_addChild(p, c4);
  XMLNode_addChild(p, c5);

  r = XMLNode_removeChild(p, 4);
  fail_unless(XMLNode_getNumChildren(p) == 4);
  fail_unless(strcmp(XMLNode_getName(r),"child5") == 0);
  XMLNode_free(r);

  r = XMLNode_removeChild(p, 3);
  fail_unless(XMLNode_getNumChildren(p) == 3);
  fail_unless(strcmp(XMLNode_getName(r),"child4") == 0);
  XMLNode_free(r);

  r = XMLNode_removeChild(p, 2);
  fail_unless(XMLNode_getNumChildren(p) == 2);
  fail_unless(strcmp(XMLNode_getName(r),"child3") == 0);
  XMLNode_free(r);

  r = XMLNode_removeChild(p, 1);
  fail_unless(XMLNode_getNumChildren(p) == 1);
  fail_unless(strcmp(XMLNode_getName(r),"child2") == 0);
  XMLNode_free(r);

  r = XMLNode_removeChild(p, 0);
  fail_unless(XMLNode_getNumChildren(p) == 0);
  fail_unless(strcmp(XMLNode_getName(r),"child1") == 0);
  XMLNode_free(r);

  /* test of sequential remove*/

  XMLNode_addChild(p, c1);
  XMLNode_addChild(p, c2);
  XMLNode_addChild(p, c3);
  XMLNode_addChild(p, c4);
  XMLNode_addChild(p, c5);


  r = XMLNode_removeChild(p, 0);
  fail_unless(XMLNode_getNumChildren(p) == 4);
  fail_unless(strcmp(XMLNode_getName(r),"child1") == 0);
  XMLNode_free(r);

  r = XMLNode_removeChild(p, 0);
  fail_unless(XMLNode_getNumChildren(p) == 3);
  fail_unless(strcmp(XMLNode_getName(r),"child2") == 0);
  XMLNode_free(r);

  r = XMLNode_removeChild(p, 0);
  fail_unless(XMLNode_getNumChildren(p) == 2);
  fail_unless(strcmp(XMLNode_getName(r),"child3") == 0);
  XMLNode_free(r);

  r = XMLNode_removeChild(p, 0);
  fail_unless(XMLNode_getNumChildren(p) == 1);
  fail_unless(strcmp(XMLNode_getName(r),"child4") == 0);
  XMLNode_free(r);

  r = XMLNode_removeChild(p, 0);
  fail_unless(XMLNode_getNumChildren(p) == 0);
  fail_unless(strcmp(XMLNode_getName(r),"child5") == 0);
  XMLNode_free(r);


  /* test of sequential remove and insert */

  XMLNode_addChild(p, c1);
  XMLNode_addChild(p, c2);
  XMLNode_addChild(p, c3);
  XMLNode_addChild(p, c4);
  XMLNode_addChild(p, c5);

  r = XMLNode_removeChild(p, 0);
  fail_unless(strcmp(XMLNode_getName(r),"child1") == 0);
  XMLNode_insertChild(p, 0, r);
  fail_unless(XMLNode_getNumChildren(p) == 5);
  fail_unless(strcmp(XMLNode_getName(XMLNode_getChild(p,0)),"child1") == 0);
  XMLNode_free(r);

  r = XMLNode_removeChild(p, 1);
  fail_unless(strcmp(XMLNode_getName(r),"child2") == 0);
  XMLNode_insertChild(p, 1, r);
  fail_unless(XMLNode_getNumChildren(p) == 5);
  fail_unless(strcmp(XMLNode_getName(XMLNode_getChild(p,1)),"child2") == 0);
  XMLNode_free(r);

  r = XMLNode_removeChild(p, 2);
  fail_unless(strcmp(XMLNode_getName(r),"child3") == 0);
  XMLNode_insertChild(p, 2, r);
  fail_unless(XMLNode_getNumChildren(p) == 5);
  fail_unless(strcmp(XMLNode_getName(XMLNode_getChild(p,2)),"child3") == 0);
  XMLNode_free(r);

  r = XMLNode_removeChild(p, 3);
  fail_unless(strcmp(XMLNode_getName(r),"child4") == 0);
  XMLNode_insertChild(p, 3, r);
  fail_unless(XMLNode_getNumChildren(p) == 5);
  fail_unless(strcmp(XMLNode_getName(XMLNode_getChild(p,3)),"child4") == 0);
  XMLNode_free(r);

  r = XMLNode_removeChild(p, 4);
  fail_unless(strcmp(XMLNode_getName(r),"child5") == 0);
  XMLNode_insertChild(p, 4, r);
  fail_unless(XMLNode_getNumChildren(p) == 5);
  fail_unless(strcmp(XMLNode_getName(XMLNode_getChild(p,4)),"child5") == 0);
  XMLNode_free(r);

  /* teardown*/

  XMLNode_free(p);
  XMLNode_free(c1);
  XMLNode_free(c2);
  XMLNode_free(c3);
  XMLNode_free(c4);
  XMLNode_free(c5);
  XMLAttributes_free(attr);
  XMLTriple_free(trp_p);
  XMLTriple_free(trp_c1);
  XMLTriple_free(trp_c2);
  XMLTriple_free(trp_c3);
  XMLTriple_free(trp_c4);
  XMLTriple_free(trp_c5);

}
END_TEST


Suite *
create_suite_XMLNode (void)
{
  Suite *suite = suite_create("XMLNode");
  TCase *tcase = tcase_create("XMLNode");

  tcase_add_test( tcase, test_XMLNode_create  );
  tcase_add_test( tcase, test_XMLNode_createFromToken  );
  tcase_add_test( tcase, test_XMLNode_createElement  );
  tcase_add_test( tcase, test_XMLNode_getters  );
  tcase_add_test( tcase, test_XMLNode_convert  );
  tcase_add_test( tcase, test_XMLNode_insert  );
  tcase_add_test( tcase, test_XMLNode_remove  );
  suite_add_tcase(suite, tcase);

  return suite;
}

