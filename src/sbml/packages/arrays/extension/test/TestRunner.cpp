/**
 * @file    TestRunner.c
 * @brief   Runs all unit tests in the extension module in the Arrays package
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

#ifdef LIBSBML_USE_VLD
  #include <vld.h>
#endif

#if defined(__cplusplus)
LIBSBML_CPP_NAMESPACE_USE
CK_CPPSTART
#endif

Suite *create_suite_ArraysExtension (void);
Suite *create_suite_WriteArraysExtension (void);
Suite *create_suite_ReadArraysExtension (void);
Suite *create_suite_ReadMathML(void);
Suite *create_suite_WriteMathMLFromAST(void);
Suite *create_suite_ArrayInfixParsing(void);
Suite *create_suite_ArrayInfixWriting(void);
Suite *create_suite_ArrayInfixToMathML(void);


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
  int  length  = (srcdir == NULL) ? 0 : (int)strlen(srcdir);


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

  //SRunner *runner = srunner_create(create_suite_WriteMathMLFromAST());

  SRunner *runner = srunner_create(create_suite_ArraysExtension());
  srunner_add_suite(runner, create_suite_WriteArraysExtension());
  srunner_add_suite(runner, create_suite_ReadArraysExtension());
  srunner_add_suite(runner, create_suite_ReadMathML());
  srunner_add_suite(runner, create_suite_WriteMathMLFromAST());
  srunner_add_suite(runner, create_suite_ArrayInfixParsing());
  srunner_add_suite(runner, create_suite_ArrayInfixWriting());
  srunner_add_suite(runner, create_suite_ArrayInfixToMathML());


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

