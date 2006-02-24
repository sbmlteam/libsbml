/**
 * \file   TestValidator.cpp
 * \brief  Validator unit tests
 * \author Ben Bornstein
 * 
 * $Id$
 * $Source$
 */
/* Copyright 2005 California Institute of Technology and
 * Japan Science and Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 2.1 of the License, or
 * any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY, WITHOUT EVEN THE IMPLIED WARRANTY OF
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  The software and
 * documentation provided hereunder is on an "as is" basis, and the
 * California Institute of Technology and Japan Science and Technology
 * Corporation have no obligations to provide maintenance, support,
 * updates, enhancements or modifications.  In no event shall the
 * California Institute of Technology or the Japan Science and Technology
 * Corporation be liable to any party for direct, indirect, special,
 * incidental or consequential damages, including lost profits, arising
 * out of the use of this software and its documentation, even if the
 * California Institute of Technology and/or Japan Science and Technology
 * Corporation have been advised of the possibility of such damage.  See
 * the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 *
 * The original code contained here was initially developed by:
 *
 *     Ben Bornstein
 *     The Systems Biology Markup Language Development Group
 *     ERATO Kitano Symbiotic Systems Project
 *     Control and Dynamical Systems, MC 107-81
 *     California Institute of Technology
 *     Pasadena, CA, 91125, USA
 *
 *     http://www.sbml.org
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s):
 */


#ifndef TestValidator_h
#define TestValidator_h


#ifdef __cplusplus


#include <vector>


class TestFile;
class Validator;


class TestValidator
{
public:

   TestValidator (Validator& v);
  ~TestValidator ();

  /**
   * @return true if the Validator behaved as expected when validating
   * TestFile, false otherwise.
   */
  bool test (const TestFile& file);


private:

  /**
   * Calls readSBML() with TestFile.  The readSBML() function reports basic
   * XML Validity and XML Schema errors.
   *
   * @return true if no errors were encountered, false otherwise.
   */
  bool testReadSBML (const TestFile& file);

  /**
   * @return true if the all Validator messages for the given Constraint id
   * test cases should be printed, false otherwise.
   */
  bool isVerbose (unsigned int id);

  /**
   * Reads the environment variable 'LIBSBML_TEST_VALIDATOR_VERBOSE' and
   * sets the internal verbose state accordingly.
   *
   * LIBSBML_TEST_VALIDATOR_VERBOSE may either be set to 'all' (case
   * sensitive) or a space separated list of constraint ids.
   */
  void readVerboseFromEnvironment ();


  Validator& mValidator;

  bool                      mVerboseAll;
  std::vector<unsigned int> mVerboseConstraintIds;
};


#endif  /* __cplusplus */
#endif  /* TestValidator_h */
