/**
 * \file    TestRunner.c
 * \brief   Runs all unit tests in the math module
 * \author  Ben Bornstein
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
 *
 * Copyright (C) 2019 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *  
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 *     Pasadena, CA, USA 
 *  
 * Copyright (C) 2002-2005 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. Japan Science and Technology Agency, Japan
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ---------------------------------------------------------------------- -->*/

#include <check.h>
#include <stdlib.h>
#include <string.h>

#include <sbml/util/memory.h>
#include <sbml/common/extern.h>

#ifdef LIBSBML_USE_VLD
  #include <vld.h>
#endif

/**
 * Test suite creation function prototypes.
 *
 * These functions are needed only for calls in main() below.  Therefore a
 * separate header file is not necessary and only adds a maintenance burden
 * to keep the two files synchronized.
 */
#if defined(__cplusplus)
LIBSBML_CPP_NAMESPACE_USE
CK_CPPSTART
#endif

Suite *create_suite_ASTNode          (void);
Suite *create_suite_FormulaFormatter (void);
Suite *create_suite_FormulaParser    (void);
Suite *create_suite_L3FormulaFormatter(void);
Suite *create_suite_L3FormulaParser  (void);
Suite *create_suite_L3FormulaParserC (void);
Suite *create_suite_FormulaTokenizer (void);
Suite *create_suite_ReadMathML       (void);
Suite *create_suite_WriteMathML      (void);
Suite *create_suite_WriteMathMLFromAST    (void);

Suite *create_suite_TestReadFromFile1     (void);
Suite *create_suite_TestReadFromFile2     (void);

Suite *create_suite_TestValidASTNode      (void);

Suite *create_suite_TestChildFunctions    (void);
Suite *create_suite_TestGetValue          (void);
Suite *create_suite_TestReadFromFileL3V2(void);
Suite *create_suite_TestInferRnFunctions (void);
Suite *create_suite_TestDerivativeFunctions(void);


/**
 * Global.
 *
 * Declared extern in TestReadFromFileN suites.
 */
char *TestDataDirectory;


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
main (void) 
{ 
  int num_failed;

  setTestDataDirectory();

  //SRunner *runner = srunner_create(create_suite_TestChildFunctions() );

  SRunner *runner = srunner_create( create_suite_ASTNode() );

  srunner_add_suite( runner, create_suite_FormulaFormatter     () );
  srunner_add_suite( runner, create_suite_FormulaParser        () );
  srunner_add_suite( runner, create_suite_L3FormulaFormatter   () );
  srunner_add_suite( runner, create_suite_L3FormulaParser      () );
  srunner_add_suite( runner, create_suite_L3FormulaParserC     () );
  srunner_add_suite( runner, create_suite_FormulaTokenizer     () );
  srunner_add_suite( runner, create_suite_ReadMathML           () );
  srunner_add_suite( runner, create_suite_WriteMathML          () );
  srunner_add_suite( runner, create_suite_WriteMathMLFromAST   () );

  srunner_add_suite( runner, create_suite_TestReadFromFile1() );
  srunner_add_suite( runner, create_suite_TestReadFromFile2() );
 
  srunner_add_suite( runner, create_suite_TestValidASTNode() );

  srunner_add_suite( runner, create_suite_TestChildFunctions() );
  srunner_add_suite( runner, create_suite_TestGetValue() );

  srunner_add_suite(runner, create_suite_TestReadFromFileL3V2());
  srunner_add_suite(runner, create_suite_TestInferRnFunctions());
  srunner_add_suite(runner, create_suite_TestDerivativeFunctions());

  /* srunner_set_fork_status(runner, CK_NOFORK); */

  srunner_run_all(runner, CK_NORMAL);
  num_failed = srunner_ntests_failed(runner);

  srunner_free(runner);

  safe_free(TestDataDirectory);

  return num_failed;
}

#if defined(__cplusplus)
CK_CPPEND
#endif


