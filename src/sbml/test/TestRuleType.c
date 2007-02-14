/**
 * \file    TestRuleType.h
 * \brief   RuleType enumeration unit tests
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


#include "common/common.h"
#include "RuleType.h"

#include <check.h>


START_TEST (test_RuleType_forName)
{
  fail_unless( RuleType_forName("scalar") == RULE_TYPE_SCALAR, NULL );
  fail_unless( RuleType_forName("rate")   == RULE_TYPE_RATE  , NULL );

  fail_unless( RuleType_forName("ScAlAr") == RULE_TYPE_SCALAR, NULL );
  fail_unless( RuleType_forName("rAtE")   == RULE_TYPE_RATE  , NULL );

  fail_unless( RuleType_forName(NULL)     == RULE_TYPE_INVALID, NULL );
  fail_unless( RuleType_forName("")       == RULE_TYPE_INVALID, NULL );
  fail_unless( RuleType_forName("foobar") == RULE_TYPE_INVALID, NULL );
}
END_TEST


START_TEST (test_RuleType_toString)
{
  const char* s;


  s = RuleType_toString(RULE_TYPE_SCALAR);
  fail_unless(!strcmp(s, "scalar"), NULL);

  s = RuleType_toString(RULE_TYPE_RATE);
  fail_unless(!strcmp(s, "rate"), NULL);

  s = RuleType_toString(RULE_TYPE_INVALID);
  fail_unless(!strcmp(s, "(Invalid RuleType)"), NULL );

  s = RuleType_toString(-1);
  fail_unless(!strcmp(s, "(Invalid RuleType)"), NULL );

  s = RuleType_toString(RULE_TYPE_INVALID + 1);
  fail_unless(!strcmp(s, "(Invalid RuleType)"), NULL );
}
END_TEST


Suite *
create_suite_RuleType (void) 
{ 
  Suite *suite = suite_create("RuleType");
  TCase *tcase = tcase_create("RuleType");


  tcase_add_test( tcase, test_RuleType_forName  );
  tcase_add_test( tcase, test_RuleType_toString );

  suite_add_tcase(suite, tcase);

  return suite;
}
