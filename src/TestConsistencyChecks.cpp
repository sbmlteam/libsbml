/**
 * Filename    : TestConsistencyChecks.cpp
 * Description : Unit tests for ValidationRules.c
 * Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2003-03-07
 * Revision    : $Id$
 * Source      : $Source$
 *
 *
 * Run genConsistencyChecks.py to generate ConsistencyChecks.i.  The latter
 * is #included by this file.  It contains the code for each unit test.
 * Each unit tests consists of a minimal model to exercise one part of
 * ValidationRules.c.
 *
 * Copyright 2002 California Institute of Technology and
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
 *     http://www.cds.caltech.edu/erato
 *     mailto:sysbio-team@caltech.edu
 *
 * Contributor(s):
 *   Stephan Hoops
 *   Ben Kovitz
 */



#include <iostream>
#include <strstream>
#include <check.h>

#include <ctype.h>
#include <sys/types.h>
#include <dirent.h>
#include <string>
#include <fstream>
#include <vector>
#include <set>
#include <ctype.h>

#include <sbml/common.h>
#include <sbml/SBMLDocument.h>
#include <sbml/SBMLReader.h>


using namespace std;


class Errors : public multiset<string>
{
public:
   Errors(SBMLDocument_t* d) {
      unsigned int numErrors = SBMLDocument_getNumErrors(d);
      for (int i = 0; i < numErrors; i++) {
         ParseMessage_t* pm = SBMLDocument_getError(d, i);
         string got = ParseMessage_getMessage(pm);
         insert(got);
      }
   }

   Errors() { }
};


class Expectation : public vector<string>
{
   Errors got;

public:
   Expectation(const string& modelString) {
      string line;
      istrstream ifs(modelString.c_str());

      while (getline(ifs, line)) {
         string expect = parseExpect(line);
         if (expect.size() != 0) {
            push_back(expect);
         }
      }
   }

   bool matches(Errors& errors) {
      got = errors;

      if (size() == 0 && errors.size() == 0)
         return true;

      if (expectNoErrors() && errors.size() == 0)
         return true;

      if (size() != errors.size())
         return false;

      for (iterator it = begin(); it != end(); it++) {
         if (errors.find(*it) == errors.end()) {
            return false;
         }
      }

      return true;
   }

   string whatDidYouExpect() {
      string result = "expected";

      if (expectNoErrors()) {
         result += " no errors";
      } else {
         result += ":\n";
         for (iterator it = begin(); it != end(); it++) {
            result += "   " + *it + '\n';
         }
      }

      return result;
   }

   string whatDidYouGot() {
      string result = "got";

      if (got.size() == 0) {
         result += " no errors";
      } else {
         result += ":\n";
         for (Errors::iterator it = got.begin(); it != got.end(); it++) {
            result += "   " + *it + '\n';
         }
      }

      return result;
   }

   bool expectNoErrors() {
      return size() == 1 && at(0) == "ok";
   }

private:
   static string parseExpect(string line)
   {
      int start = line.find("EXPECT:");
      if (start != string::npos) {
         start += 7; // length of EXPECT:
         while (isspace(line[start])) {
            start++;
         }
         return line.substr(start, line.size() - start);
      } else {
         string nothing;
         return nothing;
      }
   }
};


bool
passesConsistencyTest(const string& modelName, const string& modelString)
{
   bool result = true;
   Expectation expect(modelString);

   SBMLDocument_t* d = readSBMLFromString(modelString.c_str());
   SBMLDocument_validate(d);

   Errors errors(d);

   if (!expect.matches(errors)) {
      printf(
         "\n\n%s failed:\n%s\n%s\n",
         modelName.c_str(),
         expect.whatDidYouExpect().c_str(),
         expect.whatDidYouGot().c_str()
      );
      result = false;
   }

   SBMLDocument_free(d);

   return result;
}


BEGIN_C_DECLS


void
ConsistencyTest_setup (void)
{
}


void
ConsistencyTest_teardown (void)
{
}


#include "test-data/consistency-checks/ConsistencyChecks.txt"


END_C_DECLS
