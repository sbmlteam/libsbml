/**
 * Filename    : TestXMLStringFormatter.cpp
 * Description : XMLStringFormatter unit tests
 * Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2002-10-25
 * Revision    : $Id$
 * Source      : $Source$
 *
 * Copyright 2002 California Institute of Technology and
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
 *     mailto:sysbio-team@caltech.edu
 *
 * Contributor(s):
 */


#include "sbml/common.h"
#include "sbml/SAX2AttributesMock.hpp"
#include "sbml/XMLStringFormatter.hpp"


#ifdef __cplusplus
extern "C" {
#endif


static XMLStringFormatter *XSFormatter;


void
TestXMLStringFormatter_setup (void)
{
  char *outEncoding = safe_strdup("LATIN1");


  XSFormatter = new XMLStringFormatter(outEncoding);

  safe_free(outEncoding);
}


void
TestXMLStringFormatter_teardown (void)
{
  delete XSFormatter;
}


START_TEST (test_XMLStringFormatter_startElement)
{
  SAX2AttributesMock attrs(1);

  XMLCh*      localname  = 0;
  const char* result     = 0;
  const char* expected   = "<body xmlns=\"http://www.w3.org/1999/xhtml\">";


  localname = XMLString::transcode("body");
  attrs.add("xmlns", "http://www.w3.org/1999/xhtml");

  XSFormatter->startElement(localname, attrs);
  result = XSFormatter->getString();

  fail_unless( !strcmp(result, expected), NULL );
}
END_TEST


START_TEST (test_XMLStringFormatter_bug_no_chCloseAngle)
{
  const char* result = 0;


  XSFormatter->endElement( XMLString::transcode("p") );
  result = XSFormatter->getString();

  fail_unless( !strcmp( result, "</p>"), NULL );
}
END_TEST


Suite *
create_suite_XMLStringFormatter (void)
{
  Suite *suite = suite_create("XMLStringFormatter");
  TCase *tcase = tcase_create("XMLStringFormatter");


  tcase_add_checked_fixture(tcase,
                            TestXMLStringFormatter_setup,
                            TestXMLStringFormatter_teardown);
 
  tcase_add_test( tcase, test_XMLStringFormatter_startElement        );
  tcase_add_test( tcase, test_XMLStringFormatter_bug_no_chCloseAngle );

  suite_add_tcase(suite, tcase);

  return suite;
}


#ifdef __cplusplus
}
#endif
