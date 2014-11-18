/**
 * @file    TestRunner.c
 * @brief   Runs all unit tests in the extension module in the group package
 * @author  Akiya Jouraku
 *
 * $Id: $
 * $HeadURL: $
 */

#include <string.h>
#include <check.h>
#include <stdlib.h>
#include <sbml/util/memory.h>
#if defined(__cplusplus)
LIBSBML_CPP_NAMESPACE_USE
CK_CPPSTART
#endif

  
/**
 * Global.
 *
 * Declared extern in TestReadFromFileN suites.
 */
char *TestDataDirectory;

Suite *create_suite_SpatialExtension (void);
Suite *create_suite_WriteSpatialExtension (void);
Suite *create_suite_ReadSpatialExtension (void);



/**
 * Sets TestDataDirectory for the the TestReadFromFileN suites.
 *
 * For Automake's distcheck target to work properly, TestDataDirectory must
 * begin with the value of the environment variable SRCDIR.
 */
void
setTestDataDirectory (void)
{
  char *srcdir = getenv("srcdir");
  int  length  = (srcdir == NULL) ? 0 : strlen(srcdir);


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
  

  SRunner *runner = srunner_create(create_suite_SpatialExtension());
  srunner_add_suite(runner, create_suite_WriteSpatialExtension());
  srunner_add_suite(runner, create_suite_ReadSpatialExtension());

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
