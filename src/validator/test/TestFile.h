/**
 * \file   TestFile.h
 * \brief  Enscapsulates an XML file in the test-data/ directory
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


#ifndef TestFile_h
#define TestFile_h


#ifdef __cplusplus


#include <set>
#include <string>


/**
 * TestFiles (e.g. in the test-data/ directory) have the following naming
 * convention:
 *
 *   cccc-pass-00-nn.xml, or
 *   cccc-fail-ff-nn.xml
 *
 * Where:
 *
 *   cccc  is the four digit constraint id the file is designed to test
 *
 *   pass  indicates the file must pass validation without error
 *
 *   fail  indicates the file must fail validation with extactly ff errors
 *         all with constraint id cccc.
 *
 *   nn    is the sequence id (to allow multiple test files per constraint).
 */
class TestFile
{
public:

  const std::string& getFilename  () const { return mFilename;  }
  const std::string& getDirectory () const { return mDirectory; }
  
  std::string getFullname () const;

  unsigned int  getConstraintId     () const;
  unsigned int  getNumFailures      () const;
  unsigned int  getSequenceId       () const;
  unsigned int  getAdditionalFailId () const;

  /**
   * @return the set of TestFiles in the given directory.
   *
   * You may optionally limit to the TestFiles returned to only those with
   * ConstraintIds in the range [begin, end] (if begin == end == 0, all
   * TestFiles in the given directory will be returned).
   */
  static std::set<TestFile> getFilesIn ( const std::string& directory,
                                         unsigned int begin = 0,
                                         unsigned int end   = 0, 
                                         unsigned int library = 0);

  /**
   * Sort (and test equality) by filename.
   */
  bool operator < (const TestFile& rhs) const
  {
    return mFilename < rhs.mFilename;
  }


private:

  /**
   * Creates a new TestFile based on filename.
   */
  TestFile (const std::string& directory, const std::string& filename) :
    mDirectory(directory), mFilename(filename) { }

  /**
   * @return true if filename adheres to the TestFile naming convention,
   * false otherwise.
   */
  static bool isValid (const std::string& filename);


  std::string mDirectory;
  std::string mFilename;
};


#endif  /* __cplusplus */
#endif  /* TestFile_h  */
