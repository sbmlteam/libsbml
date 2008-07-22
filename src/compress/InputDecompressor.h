/**
 *@cond doxygen-libsbml-internal
 **
 *
 * @file    InputDecompressor.h
 * @brief   utility class for input decompression
 * @author  Akiya Jouraku
 *
 * $Id: $
 * $HeadURL: $
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2008 California Institute of Technology.
 * Copyright 2002-2005 California Institute of Technology and
 *                     Japan Science and Technology Corporation.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *----------------------------------------------------------------------- -->*/

#ifndef InputDecompressor_h
#define InputDecompressor_h

#include <iostream>
#include <sbml/common/extern.h>
#include <sbml/compress/CompressCommon.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */


class LIBSBML_EXTERN InputDecompressor
{
public:

 /**
  * Opens the given gzip file as a gzifstream (subclass of std::ifstream class) object
  * for read access and returned the stream object.
  *
  * @param filename a string, the gzip file name to be read.
  *
  * @note ZlibNotLinked will be thrown if zlib is not linked with libSBML at compile time.
  *
  * @return a istream* object bound to the given gzip file or NULL if the initialization
  * for the object failed.
  */
  static istream* openGzipIStream (const std::string& filename);


 /**
  * Opens the given bzip2 file as a bzifstream (subclass of std::ifstream class) object
  * for read access and returned the stream object.
  *
  * @param filename a string, the bzip2 file name to be read.
  *
  * @note Bzip2NotLinked will be thrown if zlib is not linked with libSBML at compile time.  
  *
  * @return a istream* object bound to the given bzip2 file or NULL if the initialization
  * for the object failed.
  */
  static istream* openBzip2IStream (const std::string& filename);


 /**
  * Opens the given zip file as a zipifstream (subclass of std::ifstream class) object
  * for read access and returned the stream object.
  *
  * @param filename a string, the zip file name to be read.
  *
  * @return a istream* object bound to the given zip file or NULL if the initialization
  * for the object failed.
  *
  * @note The first file in the given zip archive file will be opened if the zip archive
  * contains two or more files.
  */
  static istream* openZipIStream (const std::string& filename);


 /**
  * Opens the given gzip file and returned the string in the file.
  *
  * @param filename a string, the gzip file name to be read.
  *
  * @note ZlibNotLinked will be thrown if zlib is not linked with libSBML at compile time.
  *
  * @return a string, the string in the given file, or empty string if 
  * failed to open the file.
  */
  static char* getStringFromGzip (const std::string& filename);


 /**
  * Opens the given bzip2 file and returned the string in the file.
  *
  * @param filename a string, the bzip2 file name to be read.
  *
  * @note Bzip2NotLinked will be thrown if zlib is not linked with libSBML at compile time.
  *
  * @return a string, the string in the given file, or empty string if failed to open
  * the file.
  */
  static char* getStringFromBzip2 (const std::string& filename);


 /**
  * Opens the given zip file and returned the string in the file.
  *
  * @param filename a string, the zip file name to be read.
  *
  * @return a string, the string in the given file, or empty string if failed to open
  * the file.
  *
  * @note ZlibNotLinked will be thrown if zlib is not linked with libSBML at compile time.
  * The first file in the given zip archive file will be opened if the zip archive
  * contains two or more files.
  */
  static char* getStringFromZip (const std::string& filename);

};

#endif // InputDecompressor_h

/** @endcond doxygen-libsbml-internal */
