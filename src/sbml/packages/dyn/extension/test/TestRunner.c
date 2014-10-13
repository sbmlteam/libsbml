/**
 * @file    TestRunner.c
 * @brief   Runs all unit tests in the extension module in the req package
 * @author  Akiya Jouraku
 *
 * $Id: $
 * $HeadURL: $
 */

#include <check.h>
#include <stdlib.h>
#include <string.h>


#include <sbml/common/extern.h>
#include <sbml/util/memory.h>

#if defined(__cplusplus)
LIBSBML_CPP_NAMESPACE_USE
CK_CPPSTART
#endif

Suite *create_suite_DynExtension (void);
Suite *create_suite_WriteDynExtension (void);
Suite *create_suite_ReadDynExtension (void);


/**
 * Global.
 *
 * Declared extern in TestAnnotation suite.
 */
char *TestDataDirectory;

/**
 * Sets TestDataDirectory for the the TestAnnotation suite.
 *
 * For Automake's distcheck target to work properly, TestDataDirectory must
 * begin with the value of the environment variable SRCDIR.
 */
void
setTestDataDirectory (void)
{
  char *srcdir = getenv("srcdir");
  size_t  length  = (srcdir == NULL) ? 0 : strlen(srcdir);


  /**
   * strlen("/test-data/") = 11 + 1 (for NULL) = 12
   */
  TestDataDirectory = (char *) safe_calloc( length + 12, sizeof(char) );

  if (srcdir != NULL)
  {
    strcpy(TestDataDirectory, srcdir);
    strcat(TestDataDirectory, "/");
  }

  strcat(TestDataDirectory, "test-data/");
}

int
main (int argc, char* argv[]) 
{ 
  int num_failed = 0;
  setTestDataDirectory();

  SRunner *runner = srunner_create(create_suite_DynExtension());
  srunner_add_suite(runner, create_suite_WriteDynExtension());
  srunner_add_suite(runner, create_suite_ReadDynExtension());

  if (argc > 1 && !strcmp(argv[1], "-nofork"))
  {
    srunner_set_fork_status( runner, CK_NOFORK );
  }

  srunner_run_all(runner, CK_NORMAL);
  num_failed = srunner_ntests_failed(runner);

  srunner_free(runner);

  safe_free(TestDataDirectory);

  return num_failed;
}

#if defined(__cplusplus)
CK_CPPEND
#endif

