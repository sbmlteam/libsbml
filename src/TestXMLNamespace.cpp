/**
 * Filename    : TestXMLNamespace.cpp
 * Description : XMLNamespace unit tests
 * Author(s)   : SBML Development Group <sbml-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2004-09-15
 * Revision    : $Id$
 * Source      : $Source$
 *
 * Copyright 2004 California Institute of Technology and
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


#include <iostream>
#include <check.h>

#include "sbml/common.h"
#include "sbml/XMLNamespace.hpp"


BEGIN_C_DECLS


START_TEST (test_XMLNamespace_startsWithXMLNS)
{
  fail_unless( XMLNamespace::startsWithXMLNS("xmlns:jd") == true, NULL );
  fail_unless( XMLNamespace::startsWithXMLNS("XMLNS:vc") == true, NULL );
  fail_unless( XMLNamespace::startsWithXMLNS("XmLnS:ec") == true, NULL );
}
END_TEST


START_TEST (test_XMLNamespace_create)
{
  XMLNamespace ns("gepasi", "http://www.sbml.org/ns/gepasi");


  fail_unless( ns.getPrefix() == "gepasi", NULL );
  fail_unless( ns.getURI()    == "http://www.sbml.org/ns/gepasi", NULL );
}
END_TEST


START_TEST (test_XMLNamespace_create_remove_xmlns)
{
  XMLNamespace ns("xmlns:jsim", "http://www.sbml.org/ns/jsim");


  fail_unless( ns.getPrefix() == "jsim", NULL );
  fail_unless( ns.getURI()    == "http://www.sbml.org/ns/jsim", NULL );
}
END_TEST


Suite *
create_suite_XMLNamespace (void)
{
  Suite *suite = suite_create("XMLNamespace");
  TCase *tcase = tcase_create("XMLNamespace");

 
  tcase_add_test( tcase, test_XMLNamespace_startsWithXMLNS     );
  tcase_add_test( tcase, test_XMLNamespace_create              );
  tcase_add_test( tcase, test_XMLNamespace_create_remove_xmlns );

  suite_add_tcase(suite, tcase);

  return suite;
}


END_C_DECLS
