/**
 *@cond doxygen-libsbml-internal
 **
 *
 * @file    CompressCommon.cpp
 * @brief   common classes/functions for compression/decompression I/O
 * @author  Akiya Jouraku
 *
 * $Id$
 * $HeadURL$
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2009 California Institute of Technology.
 * Copyright 2002-2005 California Institute of Technology and
 *                     Japan Science and Technology Corporation.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *----------------------------------------------------------------------- -->*/

#include <sbml/compress/CompressCommon.h>

using namespace std;

/**
 * Predicate returning @c true or @c false depending on whether
 * libSBML is linked with zlib at compile time.
 *
 * @return @c true if zlib is linked, @c false otherwise.
 */
LIBSBML_EXTERN
bool hasZlib() 
{
#ifdef USE_ZLIB
  return true;
#else
  return false;
#endif // USE_ZLIB
}

/**
 * Predicate returning @c true or @c false depending on whether
 * libSBML is linked with bzip2 at compile time.
 *
 * @return @c true if bzip2 is linked, @c false otherwise.
 */
LIBSBML_EXTERN
bool hasBzip2() 
{
#ifdef USE_BZ2
  return true;
#else
  return false;
#endif // USE_BZ2
}

/** @endcond doxygen-libsbml-internal */
