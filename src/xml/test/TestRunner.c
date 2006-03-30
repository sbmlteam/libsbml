/**
 * \file    TestRunner.c
 * \brief   Runs all unit tests in the xml module
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */


#include <check.h>


Suite *create_suite_XMLAttributes (void);


int
main (void) 
{ 
  SRunner *runner = srunner_create( create_suite_XMLAttributes() );

  /* srunner_add_suite( runner, create_suite_name () ); */

  srunner_run_all(runner, CK_NORMAL);

  return (srunner_ntests_failed(runner) == 0) ? 0 : 1;
}
