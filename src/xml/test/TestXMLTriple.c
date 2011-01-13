/**
 * \file    TestXMLTriple.c
 * \brief   XMLTriple unit tests
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

#include <check.h>

#if __cplusplus
CK_CPPSTART
#endif

START_TEST (test_XMLTriple_create)
{
  XMLTriple_t *t = XMLTriple_create();
  fail_unless(t != NULL);
  fail_unless(XMLTriple_isEmpty(t) != 0);
  XMLTriple_free(t);

  t = XMLTriple_createWith("attr", "uri", "prefix");
  fail_unless( strcmp(XMLTriple_getName(t), "attr") == 0 );
  fail_unless( strcmp(XMLTriple_getURI(t), "uri") == 0 );
  fail_unless( strcmp(XMLTriple_getPrefix(t), "prefix") == 0 );
  fail_unless( strcmp(XMLTriple_getPrefixedName(t), "prefix:attr") == 0 );
  fail_unless(XMLTriple_isEmpty(t) == 0);
  XMLTriple_free(t);

  t = XMLTriple_createWith("attr", "uri", "");
  fail_unless( strcmp(XMLTriple_getName(t), "attr") == 0 );
  fail_unless( strcmp(XMLTriple_getURI(t), "uri") == 0 );
  fail_unless( XMLTriple_getPrefix(t) == NULL );
  fail_unless( strcmp(XMLTriple_getPrefixedName(t), "attr") == 0 );
  fail_unless(XMLTriple_isEmpty(t) == 0);
  XMLTriple_free(t);

}
END_TEST


START_TEST (test_XMLTriple_comparison)
{
  XMLTriple_t *t1 = XMLTriple_createWith("attr", "uri", "prefix");
  XMLTriple_t *t2 = XMLTriple_createWith("attr", "uri", "prefix");
  XMLTriple_t *t3 = XMLTriple_createWith("diff", "diff", "diff");

  fail_unless( XMLTriple_equalTo(t1, t2) != 0 );
  fail_unless( XMLTriple_equalTo(t1, t3) == 0 );
  fail_unless( XMLTriple_equalTo(t2, t3) == 0 );
  fail_unless( XMLTriple_notEqualTo(t1, t2) == 0 );
  fail_unless( XMLTriple_notEqualTo(t1, t3) != 0 );
  fail_unless( XMLTriple_notEqualTo(t2, t3) != 0 );

  XMLTriple_free(t1);
  XMLTriple_free(t2);
  XMLTriple_free(t3);
}
END_TEST

START_TEST (test_XMLTriple_accessWithNULL)
{
  fail_unless( XMLTriple_clone(NULL) == NULL);
  fail_unless( XMLTriple_createWith(NULL, NULL, NULL) == NULL);
  fail_unless( XMLTriple_equalTo(NULL, NULL) == 1);
  fail_unless( XMLTriple_equalTo(NULL, XMLTriple_create()) == 0);
  
  XMLTriple_free(NULL);
  
  fail_unless( XMLTriple_getName(NULL) == NULL);
  fail_unless( XMLTriple_getPrefix(NULL) == NULL);
  fail_unless( XMLTriple_getPrefixedName(NULL) == NULL);
  fail_unless( XMLTriple_getURI(NULL) == NULL);
  fail_unless( XMLTriple_isEmpty(NULL) == 1);
  fail_unless( XMLTriple_notEqualTo(NULL, XMLTriple_create()) == 1);
  fail_unless( XMLTriple_notEqualTo(NULL, NULL) == 0);
}
END_TEST

Suite *
create_suite_XMLTriple (void)
{
  Suite *suite = suite_create("XMLTriple");
  TCase *tcase = tcase_create("XMLTriple");

  tcase_add_test( tcase, test_XMLTriple_create         );
  tcase_add_test( tcase, test_XMLTriple_comparison     );
  tcase_add_test( tcase, test_XMLTriple_accessWithNULL );
  
  suite_add_tcase(suite, tcase);

  return suite;
}


#if __cplusplus
CK_CPPEND
#endif
