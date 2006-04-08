/**
 * \file    TestRunner.c
 * \brief   Runs all unit tests in the math module
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2003 California Institute of Technology and Japan Science and
 * Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is
 * provided in the file named "LICENSE.txt" included with this software
 * distribution.  It is also available online at
 * http://sbml.org/software/libsbml/license.html
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 */


#include <check.h>
#include <stdlib.h>

#include "common/extern.h"


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
Suite *create_suite_ReadMathML       (void);
Suite *create_suite_WriteMathML      (void);

END_C_DECLS


int
main (void) 
{ 
  int num_failed;


  SRunner *runner = srunner_create( create_suite_ASTNode() );

  srunner_add_suite( runner, create_suite_FormulaFormatter () );
  srunner_add_suite( runner, create_suite_FormulaParser    () );
  srunner_add_suite( runner, create_suite_FormulaTokenizer () );
  srunner_add_suite( runner, create_suite_ReadMathML       () );
  srunner_add_suite( runner, create_suite_WriteMathML      () );

  /* srunner_set_fork_status(runner, CK_NOFORK); */

  srunner_run_all(runner, CK_NORMAL);
  num_failed = srunner_ntests_failed(runner);

  srunner_free(runner);

  return num_failed;
}
