/*
 * \file   TestL3v2MathConsistencyValidator.cpp
 * \brief  Runs the ConsistencyValidator on each SBML file in test-data/
 * \author Sarah Keating
 * \author Ben Bornstein
 * \author Michael Hucka
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
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
 * Copyright 2011-2012 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ---------------------------------------------------------------------- -->*/

#include <iostream>
#include <set>

#include <algorithm>

#include "TestFile.h"
#include "TestValidator.h"

#include <sbml/packages/l3v2extendedmath/validator/L3v2extendedmathMathMLConsistencyValidator.h>
#include <sbml/packages/l3v2extendedmath/validator/L3v2extendedmathUnitConsistencyValidator.h>

#include <sbml/SBMLConstructorException.h>

#ifdef LIBSBML_USE_VLD
  #include <vld.h>
#endif
/** @cond doxygenIgnored */

using namespace std;
LIBSBML_CPP_NAMESPACE_USE

/** @endcond */


/*
 * @return true if the Validator behaved as expected when validating
 * TestFile, false otherwise.
 */
bool
runMainTest (const TestFile& file)
{
  return false;
}
bool
runMathMLTest (const TestFile& file)
{
  L3v2extendedmathMathMLConsistencyValidator validator;
  TestValidator        tester(validator);


  validator.init();

  return tester.test(file);
}

bool
runUnitTest (const TestFile& file)
{
  L3v2extendedmathUnitConsistencyValidator validator;
  TestValidator        tester(validator);


  validator.init();

  bool result = true;

  try
  {
    result = tester.test(file);
  }
  catch (SBMLConstructorException &e)
  {
    cout << e.getSBMLErrMsg() << endl;
    result = false;
  }

  return result;
}
/*
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
  unsigned int  passes   = static_cast<unsigned int>( count_if(files.begin(), files.end(), tester) );
  unsigned int  failures = static_cast<unsigned int>( files.size() ) - passes;
  double        percent  = (static_cast<double>(passes) / files.size()) * 100;

  cout << static_cast<int>(percent) << "%: Checks: " << files.size();
  cout << ", Failures: " << failures << endl;

  return failures;
}

/*
 * Runs the libSBML ConsistencyValidator on all consistency TestFiles in
 * the test-data/ directory.
 * Runs the libSBML L1CompatibilityValidator on all TestFiles in the
 * test-data-conversion/ directory.
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

  string prefix(".");

  if (argc == 2)
  {
    prefix = argv[1];
  }
  else {
    char *srcdir = getenv("srcdir");
    if (srcdir != NULL) {
      prefix = srcdir;
    }
  }

  // allow the test runner to be invoked with the directory containing
  // the test-data and test-data conversion directories
  string testDataDir = prefix + "/" + "test-data";
  string testThisDataDir;

  cout << endl;
  cout << "Validator testrunner" << endl;
  cout << "====================" << endl;
  cout << "using test data from           : " << testDataDir << endl;
  cout << endl;


  testThisDataDir = testDataDir + "/" + "mathml-constraints";
  failed += runTests("Testing MathML Consistency Constraints (10200 - 10299)",
    testThisDataDir, 0, 0, runMathMLTest, library);


  testThisDataDir = testDataDir + "/" + "unit-constraints";
  failed += runTests( "Testing Unit Consistency Constraints (10500 - 10599)",
          testThisDataDir, 0, 0, runUnitTest, library);

  return failed;
}

