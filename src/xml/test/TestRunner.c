/**
 * \file    TestRunner.c
 * \brief   Runs all unit tests in the xml module
 * \author  Ben Bornstein and Michael Hucka
 *
 * $Id$
 * $Source$
 */

#include <string.h>
#include <check.h>



Suite *create_suite_XMLAttributes (void);
Suite *create_suite_XMLNamespaces (void);
Suite *create_suite_XMLNode (void);
Suite *create_suite_XMLTriple (void);
Suite *create_suite_XMLToken (void);
Suite *create_suite_CopyAndClone (void);
Suite *create_suite_XMLError (void);
Suite *create_suite_XMLError_C (void);
Suite *create_suite_XMLErrorLog (void);

int
main (int argc, char* argv[]) 
{ 
  int num_failed = 0;
  SRunner *runner = srunner_create(create_suite_XMLAttributes());
  srunner_add_suite(runner, create_suite_CopyAndClone());

  srunner_add_suite(runner, create_suite_XMLNamespaces());
  srunner_add_suite(runner, create_suite_XMLTriple());
  srunner_add_suite(runner, create_suite_XMLToken());
  srunner_add_suite(runner, create_suite_XMLNode());
  srunner_add_suite(runner, create_suite_XMLError());
  srunner_add_suite(runner, create_suite_XMLError_C());
  srunner_add_suite(runner, create_suite_XMLErrorLog());

  if (argc > 1 && !strcmp(argv[1], "-nofork"))
  {
    srunner_set_fork_status( runner, CK_NOFORK );
  }
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
