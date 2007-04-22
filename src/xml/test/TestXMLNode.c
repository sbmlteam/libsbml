/**
 * \file    TestXMLNode.c
 * \brief   XMLNode unit tests
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
#include "XMLTriple.h"
#include "XMLToken.h"
#include "XMLNode.h"

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

  /*fail_unless (XMLToken_getName((XMLToken_t*)(node)) == "attr");*/
  fail_unless (XMLNode_getChild(node, 1) != NULL);

  XMLToken_free(token);
  XMLTriple_free(triple);
  XMLNode_free(node);



}
END_TEST

Suite *
create_suite_XMLNode (void)
{
  Suite *suite = suite_create("XMLNode");
  TCase *tcase = tcase_create("XMLNode");

  tcase_add_test( tcase, test_XMLNode_create  );
  tcase_add_test( tcase, test_XMLNode_createFromToken  );
  suite_add_tcase(suite, tcase);

  return suite;
}

