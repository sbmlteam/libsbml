/**
 * \file    TestSAX2AttributesMock.cpp
 * \brief   SAX2AttributesMock unit tests
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
 *   Stefan Hoops
 */


#include <iostream>
#include <check.h>

#include "common.h"
#include "extern.h"

#include "SAX2AttributesMock.h"

#ifdef USE_EXPAT
#  include "ExpatXMLString.h"
#else
   using namespace xercesc;
#endif  // USE_EXPAT


BEGIN_C_DECLS


static SAX2AttributesMock *attrs;


void
SAX2AttributesMockTest_setup (void)
{
  attrs = new SAX2AttributesMock(2);

  attrs->add( "level"  , "1" );
  attrs->add( "version", "2" );
}


void
SAX2AttributesMockTest_teardown (void)
{
  delete attrs;
}


START_TEST (test_getLength)
{
  fail_unless(attrs->getLength() == 2, NULL);
}
END_TEST


START_TEST (test_add_too_many)
{
  attrs->add("add_should_ignore", "true");

  fail_unless(attrs->getLength() == 2, NULL);
}
END_TEST


START_TEST (test_getValue)
{
  char* value0;
  char* value1;

  value0 = XMLString::transcode( attrs->getValue( unsigned(0) ) );
  value1 = XMLString::transcode( attrs->getValue( unsigned(1) ) );

  fail_unless( !strcmp(value0, "1"), NULL );
  fail_unless( !strcmp(value1, "2"), NULL );

  fail_unless(attrs->getValue(20) == 0, NULL);

  delete [] value0;
  delete [] value1;
}
END_TEST


START_TEST (test_getLocalName)
{
  char* name0;
  char* name1;

  name0 = XMLString::transcode( attrs->getLocalName( unsigned(0) ) );
  name1 = XMLString::transcode( attrs->getLocalName( unsigned(1) ) );

  fail_unless( !strcmp(name0, "level")  , NULL );
  fail_unless( !strcmp(name1, "version"), NULL );

  fail_unless(attrs->getLocalName(2) == 0, NULL);

  delete [] name0;
  delete [] name1;
}
END_TEST


Suite *
create_suite_SAX2AttributesMock (void)
{
  Suite *suite = suite_create("SAX2AttributesMock");
  TCase *tcase = tcase_create("SAX2AttributesMock");


  tcase_add_checked_fixture(tcase,
                            SAX2AttributesMockTest_setup,
                            SAX2AttributesMockTest_teardown);
 
  tcase_add_test( tcase, test_getLength    );
  tcase_add_test( tcase, test_getValue     );
  tcase_add_test( tcase, test_getLocalName );
  tcase_add_test( tcase, test_add_too_many );

  suite_add_tcase(suite, tcase);

  return suite;
}


END_C_DECLS
