/**
 * \file    TestSBase.cpp
 * \brief   SBase unit tests
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2002 California Institute of Technology and
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
 */


#include <sbml/common/common.h>
#include <sbml/common/extern.h>

#include <sbml/SBase.h>
#include <sbml/Model.h>
#include <sbml/annotation/CVTerm.h>

#include <check.h>


static SBase *S;


BEGIN_C_DECLS


void
SBaseTest_setup (void)
{
  S = new(std::nothrow) Model;

  if (S == NULL)
  {
    fail("'new(std::nothrow) SBase;' returned a NULL pointer.");
  }

}


void
SBaseTest_teardown (void)
{
  delete S;
}


START_TEST (test_SBase_setMetaId)
{
  char *metaid = "x12345";


  SBase_setMetaId(S, metaid);

  fail_unless( !strcmp(SBase_getMetaId(S), metaid), NULL );
  fail_unless( SBase_isSetMetaId(S)      , NULL );

  if (SBase_getMetaId(S) == metaid)
  {
    fail("SBase_setMetaId(...) did not make a copy of string.");
  }

  /* Reflexive case (pathological) */
  SBase_setMetaId(S, SBase_getMetaId(S));
  fail_unless( !strcmp(SBase_getMetaId(S), metaid), NULL );

  SBase_setMetaId(S, NULL);
  fail_unless( !SBase_isSetMetaId(S), NULL );

  if (SBase_getMetaId(S) != NULL)
  {
    fail("SBase_setMetaId(S, NULL) did not clear string.");
  }
}
END_TEST


START_TEST (test_SBase_setNotes)
{
  XMLToken_t *token;
  XMLNode_t *node;

  token = XMLToken_createWithText("This is a test note");
  node = XMLNode_createFromToken(token);


  SBase_setNotes(S, node);

  fail_unless(SBase_isSetNotes(S) == 1);

  if (SBase_getNotes(S) == node)
  {
    fail("SBase_setNotes(...) did not make a copy of node.");
  }
  XMLNode_t *t1 = SBase_getNotes(S);

  fail_unless(XMLNode_getNumChildren(t1) == 0);
  fail_unless(!strcmp(XMLNode_getCharacters(t1), "This is a test note"));


  /* Reflexive case (pathological)  */
  SBase_setNotes(S, SBase_getNotes(S));
  t1 = SBase_getNotes(S);
  fail_unless(XMLNode_getNumChildren(t1) == 0);
  const char * chars = XMLNode_getCharacters(t1);
  fail_unless(!strcmp(chars, "This is a test note"));

  SBase_setNotes(S, NULL);
  fail_unless(SBase_isSetNotes(S) == 0 );

  if (SBase_getNotes(S) != NULL)
  {
    fail("SBase_setNotes(S, NULL) did not clear string.");
  }

  SBase_setNotes(S, node);

  fail_unless(SBase_isSetNotes(S) == 1);

  XMLNode_free(node);
}
END_TEST


START_TEST (test_SBase_setAnnotation)
{
  XMLToken_t *token;
  XMLNode_t *node;

  token = XMLToken_createWithText("This is a test note");
  node = XMLNode_createFromToken(token);


  SBase_setAnnotation(S, node);

  fail_unless(SBase_isSetAnnotation(S) == 1);

  XMLNode_t *t1 = SBase_getAnnotation(S);

  fail_unless(XMLNode_getNumChildren(t1) == 0);
  fail_unless(!strcmp(XMLNode_getCharacters(t1), "This is a test note"));

  if (SBase_getAnnotation(S) == node)
  {
    fail("SBase_setAnnotation(...) did not make a copy of node.");
  }

  /* Reflexive case (pathological) */
  SBase_setAnnotation(S, SBase_getAnnotation(S));
  fail_unless(!strcmp(XMLNode_getCharacters(SBase_getAnnotation(S)), "This is a test note"));

  SBase_setAnnotation(S, NULL);
  fail_unless(SBase_isSetAnnotation(S) == 0 );

  if (SBase_getAnnotation(S) != NULL)
  {
    fail("SBase_setAnnotation(S, NULL) did not clear string.");
  }

  SBase_setAnnotation(S, node);

  fail_unless(SBase_isSetAnnotation(S) == 1);

  SBase_unsetAnnotation(S);

  fail_unless(SBase_isSetAnnotation(S) == 0);
}
END_TEST

START_TEST(test_SBase_CVTerms)
{
  CVTerm_t * cv = CVTerm_createWithQualifierType(BIOLOGICAL_QUALIFIER);
  
  fail_unless(SBase_getNumCVTerms(S) == 0);
  fail_unless(SBase_getCVTerms(S) == NULL);

  SBase_addCVTerm(S, cv);
  fail_unless(SBase_getNumCVTerms(S) == 1);
  fail_unless(SBase_getCVTerms(S) != NULL);

  fail_unless(SBase_getCVTerm(S, 0) != cv);

  CVTerm_free(cv);


}
END_TEST

Suite *
create_suite_SBase (void)
{
  Suite *suite = suite_create("SBase");
  TCase *tcase = tcase_create("SBase");


  tcase_add_checked_fixture(tcase, SBaseTest_setup, SBaseTest_teardown);

  tcase_add_test(tcase, test_SBase_setMetaId     );
  tcase_add_test(tcase, test_SBase_setNotes      );
  tcase_add_test(tcase, test_SBase_setAnnotation );
  tcase_add_test(tcase, test_SBase_CVTerms );

  suite_add_tcase(suite, tcase);

  return suite;
}


END_C_DECLS
