/**
 * \file    TestConstraint.c
 * \brief   SBML Constraint unit tests
 * \author  Sarah Keating
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


#include "common/common.h"
#include "math/FormulaParser.h"

#include "SBase.h"
#include "Constraint.h"

#include <check.h>


static Constraint_t *C;


void
ConstraintTest_setup (void)
{
  C = Constraint_create();

  if (C == NULL)
  {
    fail("Constraint_create() returned a NULL pointer.");
  }
}


void
ConstraintTest_teardown (void)
{
  Constraint_free(C);
}


START_TEST (test_Constraint_create)
{
  fail_unless( SBase_getTypeCode  ((SBase_t *) C) == SBML_CONSTRAINT );
  fail_unless( SBase_getMetaId    ((SBase_t *) C) == NULL );
/*  fail_unless( SBase_getNotes     ((SBase_t *) C) == NULL );
  fail_unless( SBase_getAnnotation((SBase_t *) C) == NULL );
*/
  fail_unless( !Constraint_isSetMessage(C) );
  fail_unless( !Constraint_isSetMath    (C) );
}
END_TEST


START_TEST (test_Constraint_free_NULL)
{
  Constraint_free(NULL);
}
END_TEST


START_TEST (test_Constraint_setMath)
{
  ASTNode_t *math = SBML_parseFormula("2 * k");

  Constraint_setMath(C, math);

  fail_unless( Constraint_getMath(C) != math );
  fail_unless( Constraint_isSetMath(C) );

  /* Reflexive case (pathological) */
  Constraint_setMath(C, (ASTNode_t *) Constraint_getMath(C));

  fail_unless( Constraint_getMath(C) != math );

  Constraint_setMath(C, NULL);
  fail_unless( !Constraint_isSetMath(C) );

  if (Constraint_getMath(C) != NULL)
  {
    fail("Constraint_setMath(C, NULL) did not clear ASTNode.");
  }
}
END_TEST


Suite *
create_suite_Constraint (void)
{
  Suite *suite = suite_create("Constraint");
  TCase *tcase = tcase_create("Constraint");


  tcase_add_checked_fixture( tcase,
                             ConstraintTest_setup,
                             ConstraintTest_teardown );

  tcase_add_test( tcase, test_Constraint_create      );
  tcase_add_test( tcase, test_Constraint_free_NULL   );
  tcase_add_test( tcase, test_Constraint_setMath     );

  suite_add_tcase(suite, tcase);

  return suite;
}
