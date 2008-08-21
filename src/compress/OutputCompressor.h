/**
 *@endcond doxygen-libsbml-internal 
 **
 *
 * @file    OutputCompressor.h
 * @brief   utility class for output compression
 * @author  Akiya Jouraku
 *
 * $Id$
 * $HeadURL$
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

#ifndef OutputCompressor_h
#define OutputCompressor_h

#include <iostream>
#include <sbml/common/extern.h>
#include <sbml/compress/CompressCommon.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */


class LIBSBML_EXTERN OutputCompressor
{
public:

 /**
  * Opens the given gzip file as a gzofstream (subclass of std::ofstream class) object
  * for write access and returned the stream object.
  *
  * @param filename a string, the gzip file name to be written.
  *
  * @note ZlibNotLinked will be thrown if zlib is not linked with libSBML at compile time.
  *
  * @return a ostream* object bound to the given gzip file or NULL if the initialization
  * for the object failed.
  */
  static ostream* openGzipOStream(const std::string& filename);


 /**
  * Opens the given bzip2 file as a bzofstream (subclass of std::ofstream class) object
  * for write access and returned the stream object.
  *
  * @param filename a string, the bzip2 file name to be written.
  *
  * @note Bzip2NotLinked will be thrown if zlib is not linked with libSBML at compile time.
  *
  * @return a ostream* object bound to the given bzip2 file or NULL if the initialization
  * for the object failed.
  */
  static ostream* openBzip2OStream(const std::string& filename);


 /**
  * Opens the given zip file as a zipofstream (subclass of std::ofstream class) object
  * for write access and returned the stream object.
  *
  * @param filename a string, the zip archive file name to be written.
  * @param filenameinzip a string, the file name to be archived in the above zip archive file.
  * ('filenameinzip' will be extracted when the 'filename' is unzipped)
  *
  * @note ZlibNotLinked will be thrown if zlib is not linked with libSBML at compile time.
  *
  * @return a ostream* object bound to the given zip file or NULL if the initialization
  * for the object failed.
  */
  static ostream* openZipOStream(const std::string& filename, const std::string& filenameinzip);

};

#endif // OutputCompressor_h

/** @endcond doxygen-libsbml-internal */
