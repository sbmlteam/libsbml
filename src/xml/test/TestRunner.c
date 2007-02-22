/**
 * \file    TestRunner.c
 * \brief   Runs all unit tests in the xml module
 * \author  Ben Bornstein and Michael Hucka
 *
 * $Id$
 * $Source$
 */


#include <check.h>


Suite *create_suite_XMLAttributes (void);
Suite *create_suite_XMLNamespaces (void);


int
main (void) 
{ 
  int num_failed = 0;
  SRunner *runner = srunner_create(create_suite_XMLAttributes());

  srunner_add_suite(runner, create_suite_XMLNamespaces());

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
