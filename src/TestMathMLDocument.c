/**
 * Filename    : TestMathMLDocument.c
 * Description : MathMLDocument unit tests
 * Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2003-05-06
 * Revision    : $Id$
 * Source      : $Source$
 *
 * Copyright 2003 California Institute of Technology and
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


#include <check.h>

#include "sbml/common.h"
#include "sbml/FormulaParser.h"

#include "sbml/MathMLDocument.h"


START_TEST (test_MathMLDocument_create)
{
  MathMLDocument_t *d = MathMLDocument_create();


  fail_unless( d->math == NULL, NULL );

  MathMLDocument_free(d);
}
END_TEST


START_TEST (test_MathMLDocument_free_NULL)
{
  MathMLDocument_free(NULL);
}
END_TEST


START_TEST (test_MathMLDocument_setMath)
{
  MathMLDocument_t *d    = MathMLDocument_create();
  ASTNode_t        *math = SBML_parseFormula("1 + 1 + 2 + 3 + 5 + 8");


  MathMLDocument_setMath(d, math);

  fail_unless( d->math == math, NULL );
  fail_unless( MathMLDocument_isSetMath(d), NULL );

  /* Reflexive case (pathological) */
  MathMLDocument_setMath(d, d->math);
  fail_unless( d->math == math, NULL );

  MathMLDocument_setMath(d, NULL);
  fail_unless( !MathMLDocument_isSetMath(d), NULL );

  if (d->math != NULL)
  {
    fail( "MathMLDocument_setMath(d, NULL) did not clear ASTNode." );
  }

  MathMLDocument_free(d);
}
END_TEST


Suite *
create_suite_MathMLDocument (void) 
{ 
  Suite *suite = suite_create("MathMLDocument");
  TCase *tcase = tcase_create("MathMLDocument");
 

  tcase_add_test( tcase, test_MathMLDocument_create    );
  tcase_add_test( tcase, test_MathMLDocument_free_NULL );
  tcase_add_test( tcase, test_MathMLDocument_setMath   );

  suite_add_tcase(suite, tcase);

  return suite;
}
