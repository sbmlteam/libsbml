/**
 * \file   TestConsistencyValidator.cpp
 * \brief  Runs the ConsistencyValidator on each SBML file in test-data/
 * \author Sarah Keating
 * \author Ben Bornstein
 * \author Michael Hucka
 *
 * $Id$
 * $Source$
 */
/* Copyright 2006-2007 California Institute of Technology.
 * Copyright 2005      California Institute of Technology and
 *                     Japan Science and Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution.  
 * It is also available online at http://sbml.org/software/libsbml/license.html
 */

#include <iostream>
#include <set>

#include <algorithm>

#include "TestFile.h"
#include "TestValidator.h"

#include <sbml/validator/ConsistencyValidator.h>
#include <sbml/validator/MathMLConsistencyValidator.h>
#include <sbml/validator/IdentifierConsistencyValidator.h>
#include <sbml/validator/SBOConsistencyValidator.h>
#include <sbml/validator/UnitConsistencyValidator.h>
#include <sbml/validator/L1CompatibilityValidator.h>
#include <sbml/validator/L2v1CompatibilityValidator.h>
#include <sbml/validator/L2v2CompatibilityValidator.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */


/**
 * @return true if the Validator behaved as expected when validating
 * TestFile, false otherwise.
 */
bool
runMainTest (const TestFile& file)
{
  ConsistencyValidator validator;
  TestValidator        tester(validator);


  validator.init();

  return tester.test(file);
}

/**
 * @return true if the Validator behaved as expected when validating
 * TestFile, false otherwise.
 */
bool
runIdTest (const TestFile& file)
{
  IdentifierConsistencyValidator validator;
  TestValidator        tester(validator);


  validator.init();

  return tester.test(file);
}

/**
 * @return true if the Validator behaved as expected when validating
 * TestFile, false otherwise.
 */
bool
runMathMLTest (const TestFile& file)
{
  MathMLConsistencyValidator validator;
  TestValidator        tester(validator);


  validator.init();

  return tester.test(file);
}

/**
 * @return true if the Validator behaved as expected when validating
 * TestFile, false otherwise.
 */
bool
runSBOTest (const TestFile& file)
{
  SBOConsistencyValidator validator;
  TestValidator        tester(validator);


  validator.init();

  return tester.test(file);
}

/**
 * @return true if the Validator behaved as expected when validating
 * TestFile, false otherwise.
 */
bool
runUnitTest (const TestFile& file)
{
  UnitConsistencyValidator validator;
  TestValidator        tester(validator);


  validator.init();

  return tester.test(file);
}

/**
 * @return true if the Validator behaved as expected when validating
 * TestFile, false otherwise.
 */
bool
runL1Test (const TestFile& file)
{
  L1CompatibilityValidator validator;
  TestValidator            tester(validator);


  validator.init();

  return tester.test(file);
}

/**
 * @return true if the Validator behaved as expected when validating
 * TestFile, false otherwise.
 */
bool
runL2v1Test (const TestFile& file)
{
  L2v1CompatibilityValidator validator;
  TestValidator            tester(validator);


  validator.init();

  return tester.test(file);
}

/**
 * @return true if the Validator behaved as expected when validating
 * TestFile, false otherwise.
 */
bool
runL2v2Test (const TestFile& file)
{
  L2v2CompatibilityValidator validator;
  TestValidator            tester(validator);


  validator.init();

  return tester.test(file);
}

/**
 * @return true if the Validator behaved as expected when validating
 * TestFile, false otherwise.
 */
bool
runTestBadXML (const TestFile& file)
{
  ConsistencyValidator validator;
  TestValidator        tester(validator);


  validator.init();

  return tester.test(file);
}

/**
 * Run a given set of tests and print the results.
 */
unsigned int
runTests ( const string& msg,
	   const string& directory,
	   unsigned int  begin,
	   unsigned int  end,
	   bool (*tester)(const TestFile& file), 
     unsigned int library)
{
  cout.precision(0);
  cout.width(3);

  cout << msg << "." << endl;

  set<TestFile> files    = TestFile::getFilesIn(directory, begin, end, library);
  unsigned int  passes   = count_if(files.begin(), files.end(), tester);
  unsigned int  failures = files.size() - passes;
  double        percent  = (static_cast<double>(passes) / files.size()) * 100;

  cout << static_cast<int>(percent) << "%: Checks: " << files.size();
  cout << ", Failures: " << failures << endl;

  return failures;
}

/**
 * Runs the libSBML ConsistencyValidator on all consistency TestFiles in
 * the test-data/ directory.
 * Runs the libSBML L1CompatibilityValidator on all TestFiles in the
 * test-data-l2-l1-conversion/ directory.
 */
int
main (int argc, char* argv[])
{
  unsigned int library = 0;
#ifdef USE_EXPAT
  library = 1;
#endif
#ifdef USE_LIBXML
  library = 2;
#endif
  unsigned int failed = 0;

  failed += runTests( "Testing Bad XML Constraints (0000 - 10000)",
		      "test-data", 0, 9999, runTestBadXML, library);

  failed += runTests( "Testing General XML Consistency Constraints (10000 - 10199)",
		      "test-data", 10000, 10199, runMainTest, library);

  failed += runTests( "Testing General MathML Consistency Constraints (10200 - 10299)",
		      "test-data", 10200, 10299, runMathMLTest, library);

  failed += runTests( "Testing Id Consistency Constraints (10300 - 10399)",
		      "test-data", 10300, 10399, runIdTest, library);

  failed += runTests( "Testing General Annotation Consistency Constraints (10400 - 10499)",
		      "test-data", 10400, 10499, runMainTest, library);

  failed += runTests( "Testing Unit Consistency Constraints (10500 - 10699)",
		      "test-data", 10500, 10699, runUnitTest, library);

  failed += runTests( "Testing SBO Consistency Constraints (10700 - 10799)",
		      "test-data", 10700, 10799, runSBOTest, library);

  failed += runTests( "Testing General Annotation Consistency Constraints (10800 - 19999)",
		      "test-data", 10800, 19999, runMainTest, library);

  failed += runTests( "Testing Model Consistency Constraints (20000 - 29999)",
		      "test-data", 20000, 29999, runMainTest, library);

  failed += runTests( "Testing L1 Compatibility Constraints (90000 - 91999)",
		      "test-data-l2-l1-conversion", 90000, 91999, runL1Test, library);

  failed += runTests( "Testing L2v1 Compatibility Constraints (92000 - 92999)",
		      "test-data-l2-l1-conversion", 92000, 92999, runL2v1Test, library);

  failed += runTests("Testing L2v2 Compatibility Constraints (93000 - 93999)",
		     "test-data-l2-l1-conversion", 93000, 93999, runL2v2Test, library);

  return failed;
}

