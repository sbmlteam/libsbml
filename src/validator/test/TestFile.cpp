/**
 * \file   TestFile.cpp
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


#include <cstdlib>

#if defined(WIN32) && !defined(CYGWIN)
#  include "tps/dirent.h"
#else
#  include <sys/types.h>
#  include <dirent.h>
#endif  /* WIN32 */

#include <iostream>
#include "TestFile.h"


using namespace std;


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
 *
 *
 * Offsets within mFilename:
 *
 *           1       1
 * 0123456789012345678
 * cccc-pass-00-nn.xmL
 * cccc-fail-ff-nn.xml
 */


std::string
TestFile::getFullname () const
{
  return mDirectory + "/" + mFilename;
}


unsigned int
TestFile::getConstraintId () const
{
  return atol( mFilename.substr(0, 4).c_str() );
}


unsigned int
TestFile::getSequenceId () const
{
  return atol( mFilename.substr(13, 2).c_str() );
}


unsigned int
TestFile::getNumFailures () const
{
  return atol( mFilename.substr(10, 2).c_str() );
}


/**
 * @return true if filename adheres to the TestFile naming convention,
 * false otherwise.
 */
bool
TestFile::isValid (const string& filename)
{
  return filename.length() == 19 && filename.substr(15, 4) == ".xml";
}


/**
 * @return the set of TestFiles in the given directory.
 *
 * You may optionally limit to the TestFiles returned to only those with
 * ConstraintIds in the range [begin, end] (if begin == end == 0, all
 * TestFiles in the given directory will be returned).
 */
set<TestFile>
TestFile::getFilesIn ( const string& directory,
                       unsigned int  begin,
                       unsigned int  end )
{
  DIR*           dir;
  struct dirent* entry;
  set<TestFile>  result;


  dir = opendir( directory.c_str() );

  if (dir == NULL)
  {
    cerr << "Could not obtain a list of files in directory "
         << "[" << directory << "]." << endl;
    return result;
  }

  for (entry = readdir(dir); entry != NULL; entry = readdir(dir))
  {
    string filename(entry->d_name);

    if ( TestFile::isValid(filename) )
    {
      TestFile     file(directory, filename);
      unsigned int id = file.getConstraintId();

      if ((begin == 0 && end == 0) || (id >= begin && id <= end))
      {
        result.insert( TestFile(directory, filename) );
      }
    }
  }

  closedir(dir);

  return result;
}
