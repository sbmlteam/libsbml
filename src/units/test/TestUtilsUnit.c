/**
 * \file    TestUnitFormulaFormatter.c
 * \brief   UnitFormulaFormatter unit tests (no pun intended)
 * \author  Sarah Keating and Ralph Gauges
 *
 * $Id$
 * $Source$
 */
/* Copyright 2003 California Institute of Technology and
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


#include <check.h>
#include "common/common.h"

#include "sbml/Unit.h"
#include "sbml/UnitDefinition.h"
#include "math/ASTNode.h"

#include "../Utils_Unit.h"

START_TEST(test_unit_remove_scale)
{
    Unit_t * u = Unit_createWith(UNIT_KIND_LITRE, 1, -3);
    
 //   Unit_removeScale(u);
    fail_unless(Unit_getMultiplier(u) == 1);
    fail_unless(Unit_getScale(u) == -3);

    Unit_free(u);
}
END_TEST

Suite *
create_suite_UtilsUnit (void) 
{ 
  Suite *suite = suite_create("UtilsUnit");
  TCase *tcase = tcase_create("UtilsUnit");
 

  tcase_add_test( tcase, test_unit_remove_scale     );

  suite_add_tcase(suite, tcase);

  return suite;
}
