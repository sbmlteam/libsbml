/**
 * Filename    : TestXMLNamespaceList.cpp
 * Description : XMLNamespaceList unit tests
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
#include "sbml/XMLNamespaceList.hpp"


BEGIN_C_DECLS


START_TEST (test_XMLNamespaceList_empty)
{
  XMLNamespaceList nl;
  fail_unless( nl.getLength() == 0, NULL );
}
END_TEST


START_TEST (test_XMLNamespaceList_add_get)
{
  XMLNamespaceList nl;


  nl.add( "jd", "http://www.sbml.org/ns/jdesigner" );

  fail_unless( nl.getLength() == 1, NULL );

  fail_unless( nl.getPrefix(0)                                  == "jd", NULL );
  fail_unless( nl.getPrefix("http://www.sbml.org/ns/jdesigner") == "jd", NULL );

  fail_unless( nl.getURI(0)    == "http://www.sbml.org/ns/jdesigner", NULL );
  fail_unless( nl.getURI("jd") == "http://www.sbml.org/ns/jdesigner", NULL );


  nl.add( "ls", "http://www.sbml.org/ns/libsbml" );

  fail_unless( nl.getLength() == 2, NULL );

  fail_unless( nl.getPrefix(1)                                == "ls", NULL );
  fail_unless( nl.getPrefix("http://www.sbml.org/ns/libsbml") == "ls", NULL );

  fail_unless( nl.getURI(1)    == "http://www.sbml.org/ns/libsbml", NULL );
  fail_unless( nl.getURI("ls") == "http://www.sbml.org/ns/libsbml", NULL );


  nl.add( "ms", "http://www.sbml.org/ns/mathsbml" );

  fail_unless( nl.getLength() == 3, NULL );

  fail_unless( nl.getPrefix(2)                                 == "ms", NULL );
  fail_unless( nl.getPrefix("http://www.sbml.org/ns/mathsbml") == "ms", NULL );

  fail_unless( nl.getURI(2)    == "http://www.sbml.org/ns/mathsbml", NULL );
  fail_unless( nl.getURI("ms") == "http://www.sbml.org/ns/mathsbml", NULL );
}
END_TEST


START_TEST (test_XMLNamespaceList_not_found)
{
  XMLNamespaceList nl;


  fail_unless( nl.getPrefix(0) .empty(), NULL );
  fail_unless( nl.getPrefix("http://www.sbml.org/ns/ecell").empty(), NULL );
  fail_unless( nl.getURI(0)    .empty(), NULL );
  fail_unless( nl.getURI("ec") .empty(), NULL );
}
END_TEST


START_TEST (test_XMLNamespaceList_remove_xmlns)
{
  XMLNamespaceList nl;


  nl.add( "xmlns:vcell", "http://www.sbml.org/ns/vcell" );

  fail_unless( nl.getPrefix(0)                              == "vcell", NULL );
  fail_unless( nl.getPrefix("http://www.sbml.org/ns/vcell") == "vcell", NULL );
  fail_unless( nl.getURI(0)       == "http://www.sbml.org/ns/vcell", NULL );
  fail_unless( nl.getURI("vcell") == "http://www.sbml.org/ns/vcell", NULL );
}
END_TEST


Suite *
create_suite_XMLNamespaceList (void)
{
  Suite *suite = suite_create("XMLNamespaceList");
  TCase *tcase = tcase_create("XMLNamespaceList");

 
  tcase_add_test( tcase, test_XMLNamespaceList_empty        );
  tcase_add_test( tcase, test_XMLNamespaceList_add_get      );
  tcase_add_test( tcase, test_XMLNamespaceList_not_found    );
  tcase_add_test( tcase, test_XMLNamespaceList_remove_xmlns );

  suite_add_tcase(suite, tcase);

  return suite;
}


END_C_DECLS
