/**
 * Filename    : TestSBMLFormatter.cpp
 * Description : SBMLFormatter unit tests
 * Author(s)   : SBW Development Group <sysbio-team@caltech.edu>
 * Organization: Caltech ERATO Kitano Systems Biology Project
 * Created     : 2003-03-07
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
 *     The Systems Biology Workbench Development Group
 *     ERATO Kitano Systems Biology Project
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
#include "sbml/SBMLFormatter.hpp"


BEGIN_C_DECLS


/**
 * Wraps the string s in the appropriate XML boilerplate.
 */
#define XML_HEADER   "<?xml version=\"1.0\" encoding=\"LATIN1\"?>\n"
#define wrapXML(s)   XML_HEADER s


MemBufFormatTarget *target;
SBMLFormatter      *formatter;


void
TestSBMLFormatter_setup (void)
{
  target    = new MemBufFormatTarget();
  formatter = new SBMLFormatter("LATIN1", target);
}


void
TestSBMLFormatter_teardown (void)
{
  delete formatter;
}


START_TEST (test_SBMLFormatter_SBMLDocument)
{
  SBMLDocument_t *d = SBMLDocument_createWith(1, 2);
  const char     *s = wrapXML("<sbml level=\"1\" version=\"2\">\n</sbml>\n");


  *formatter << d;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);

  SBMLDocument_free(d);
}
END_TEST


START_TEST (test_SBMLFormatter_Model)
{
  Model_t    *m = Model_createWith("Branch");
  const char *s = wrapXML("<model name=\"Branch\">\n</model>\n");


  *formatter << m;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);

  Model_free(m);
}
END_TEST


START_TEST (test_SBMLFormatter_Unit)
{
  Unit_t *u = Unit_createWith(UNIT_KIND_KILOGRAM, 1, -3);

  const char *s = wrapXML
  (
    "<unit kind=\"kilogram\" exponent=\"1\" scale=\"-3\"/>\n"
  );


  *formatter << u;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);

  Unit_free(u);
}
END_TEST


START_TEST (test_SBMLFormatter_UnitDefinition)
{
  UnitDefinition_t *ud = UnitDefinition_createWith("mmls");

  const char *s = wrapXML
  (
    "<unitDefinition name=\"mmls\">\n</unitDefinition>\n"
  );


  *formatter << ud;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);

  UnitDefinition_free(ud);
}
END_TEST


START_TEST (test_SBMLFormatter_Compartment)
{
  Compartment_t *c = Compartment_createWith("A", 2.1, NULL, "B");

  const char *s = wrapXML
  (
    "<compartment name=\"A\" volume=\"2.1\" outside=\"B\"/>\n"
  );


  *formatter << c;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL);

  Compartment_free(c);
}
END_TEST


START_TEST (test_SBMLFormatter_Species)
{
  Species_t *sp = Species_createWith("Ca2", "cell", 0.7, "mole", 0, 2);

  const char *s = wrapXML
  (
    "<species name=\"Ca2\" compartment=\"cell\" initialAmount=\"0.7\""
    " units=\"mole\" boundaryCondition=\"false\" charge=\"2\"/>\n"
  );

  *formatter << sp;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL );

  Species_free(sp);
}
END_TEST


START_TEST (test_SBMLFormatter_Parameter)
{
  Parameter_t *p = Parameter_createWith("Km1", 2.3, "second");

  const char *s = wrapXML
  (
    "<parameter name=\"Km1\" value=\"2.3\" units=\"second\"/>\n"
  );


  *formatter << p;

  fail_unless( !strcmp((char *) target->getRawBuffer(), s), NULL );

  Parameter_free(p);
}
END_TEST


Suite *
create_suite_SBMLFormatter (void)
{
  Suite *suite = suite_create("SBMLFormatter");
  TCase *tcase = tcase_create("SBMLFormatter");


  tcase_add_checked_fixture(tcase,
                            TestSBMLFormatter_setup,
                            TestSBMLFormatter_teardown);
 
  tcase_add_test( tcase, test_SBMLFormatter_SBMLDocument   );
  tcase_add_test( tcase, test_SBMLFormatter_Model          );
  tcase_add_test( tcase, test_SBMLFormatter_Unit           );
  tcase_add_test( tcase, test_SBMLFormatter_UnitDefinition );
  tcase_add_test( tcase, test_SBMLFormatter_Compartment    );
  tcase_add_test( tcase, test_SBMLFormatter_Species        );
  tcase_add_test( tcase, test_SBMLFormatter_Parameter      );

  suite_add_tcase(suite, tcase);

  return suite;
}


END_C_DECLS
