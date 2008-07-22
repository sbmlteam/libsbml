/**
 *@cond doxygen-libsbml-internal
 **
 *
 * @file    OutputCompressor.cpp
 * @brief   utility class for output compression
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

#include <ostream>
#include <fstream>
#include <iostream>
#include <new>

#include <sbml/compress/OutputCompressor.h>

#ifdef USE_ZLIB
#include <sbml/compress/zfstream.h>
#include <sbml/compress/zipfstream.h>
#endif //USE_ZLIB

#ifdef USE_BZ2
#include <sbml/compress/bzfstream.h>
#endif //USE_BZ2

using namespace std;


/**
 * Opens the given gzip file as a gzofstream (subclass of std::ofstream class) object
 * for write access and returned the stream object.
 *
 * @return a ostream* object bound to the given gzip file or NULL if the initialization
 * for the object failed.
 */
std::ostream* 
OutputCompressor::openGzipOStream(const std::string& filename)
{
#ifdef USE_ZLIB
  return new(std::nothrow) gzofstream(filename.c_str(), ios_base::out | ios_base::binary);
#else
  throw ZlibNotLinked();
  return NULL; // never reached
#endif
}


/**
 * Opens the given bzip2 file as a bzofstream (subclass of std::ofstream class) object
 * for write access and returned the stream object.
 *
 * @return a ostream* object bound to the given bzip2 file or NULL if the initialization
 * for the object failed.
 */
std::ostream* 
OutputCompressor::openBzip2OStream(const std::string& filename)
{
#ifdef USE_BZ2
  return new(std::nothrow) bzofstream(filename.c_str(), ios_base::out | ios_base::binary);
#else
  throw Bzip2NotLinked();
  return NULL; // never reached
#endif
}


/**
 * Opens the given zip file as a zipofstream (subclass of std::ofstream class) object
 * for write access and returned the stream object.
 *
 * @return a ostream* object bound to the given zip file or NULL if the initialization
 * for the object failed.
 */
std::ostream* 
OutputCompressor::openZipOStream(const std::string& filename, const std::string& filenameinzip)
{
#ifdef USE_ZLIB
  return new(std::nothrow) zipofstream(filename.c_str(), filenameinzip.c_str(), 
                                       ios_base::out | ios_base::binary);
#else
  throw ZlibNotLinked();
  return NULL; // never reached
#endif
}

/** @endcond doxygen-libsbml-internal */
