/**
 * \file    TestRunner.c
 * \brief   Runs all unit tests in the math module
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


#include <check.h>
#include <stdlib.h>

#include "common/extern.h"
#include "util/memory.h"


/**
 * Test suite creation function prototypes.
 *
 * These functions are needed only for calls in main() below.  Therefore a
 * separate header file is not necessary and only adds a maintenance burden
 * to keep the two files synchronized.
 */
BEGIN_C_DECLS

Suite *create_suite_ASTNode          (void);
Suite *create_suite_FormulaFormatter (void);
Suite *create_suite_FormulaParser    (void);
Suite *create_suite_FormulaTokenizer (void);
Suite *create_suite_MathMLDocument   (void);
Suite *create_suite_MathMLFormatter  (void);
Suite *create_suite_MathMLHandler    (void);
Suite *create_suite_MathMLWriter     (void);

END_C_DECLS


int
main (void) 
{ 
  int num_failed;


  SRunner *runner = srunner_create( create_suite_ASTNode() );


  srunner_add_suite( runner, create_suite_FormulaFormatter () );
  srunner_add_suite( runner, create_suite_FormulaParser    () );
  srunner_add_suite( runner, create_suite_FormulaTokenizer () );
  srunner_add_suite( runner, create_suite_MathMLDocument   () );
  srunner_add_suite( runner, create_suite_MathMLFormatter  () );
  srunner_add_suite( runner, create_suite_MathMLHandler    () );
  srunner_add_suite( runner, create_suite_MathMLWriter     () );


#ifdef TRACE_MEMORY
  srunner_set_fork_status(runner, CK_NOFORK);
#endif

  srunner_run_all(runner, CK_NORMAL);
  num_failed = srunner_ntests_failed(runner);

#ifdef TRACE_MEMORY

  if (MemTrace_getNumLeaks() > 0)
  {
    MemTrace_printLeaks(stdout);
  }

  MemTrace_printStatistics(stdout);

#endif

  srunner_free(runner);

  return num_failed;
}
