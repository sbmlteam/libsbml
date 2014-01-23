/**
 * @file    libsbml-version.cpp
 * @brief   Define libSBML version numbers for access from client software.
 * @author  Akiya Jouraku
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2009-2013 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EBML-EBI), Hinxton, UK
 *  
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 *     Pasadena, CA, USA 
 *  
 * Copyright (C) 2002-2005 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. Japan Science and Technology Agency, Japan
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution and
 * also available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 */

#include "libsbml-version.h"
#include <string.h>

#ifdef USE_EXPAT
#include <expat.h>
#endif 

#ifdef USE_XERCES
#include <xercesc/util/XercesDefs.hpp>
#include <xercesc/util/XercesVersion.hpp>
#endif

#ifdef USE_LIBXML
#include <libxml/xmlversion.h>
#endif

#ifdef USE_ZLIB
#include <zlib.h>
#endif

#ifdef USE_BZ2
#include <bzlib.h>
#endif

LIBSBML_CPP_NAMESPACE_BEGIN

/**
 * Returns the libSBML version as an integer: version 1.2.3 becomes 10203.
 *
 * @return the libSBML version as an integer: version 1.2.3 becomes 10203.
 */
LIBSBML_EXTERN
int 
getLibSBMLVersion () 
{ 
  return LIBSBML_VERSION; 
}


/**
 * Returns the libSBML version as a string of the form "1.2.3".
 *
 * @return the libSBML version as a string of the form "1.2.3".
 */
LIBSBML_EXTERN
const char* 
getLibSBMLDottedVersion () 
{ 
  return LIBSBML_DOTTED_VERSION;
}


/**
 * Returns the libSBML version as a string: version 1.2.3 becomes "10203".
 *
 * @return the libSBML version as a string: version 1.2.3 becomes "10203".
 */
LIBSBML_EXTERN
const char* 
getLibSBMLVersionString () 
{ 
  return LIBSBML_VERSION_STRING;
}

/**
 * Returns an indication whether libSBML has been compiled with
 * against a specific library. 
 *
 * @param option the library to test against, this can be one of
 *        "expat", "libxml", "xerces-c", "bzip2", "zip"
 * 
 * @return 0 in case the libSBML has not been compiled against 
 *         that library and non-zero otherwise (for libraries 
 *         that define an integer version number that number will 
 *         be returned).
 *
 * @see getLibSBMLDependencyVersionOf()
 */
LIBSBML_EXTERN
int 
isLibSBMLCompiledWith(const char* option)
{
  if (option == NULL) return 0;

  if (strcmp(option, "expat") == 0)
  {
#ifdef USE_EXPAT
    return 1;
#else
    return 0;
#endif
  }

  if (strcmp(option, "libxml") == 0 ||
      strcmp(option, "xml2") == 0 ||
      strcmp(option, "libxml2") == 0)
  {
#ifdef USE_LIBXML
    return LIBXML_VERSION;
#else
    return 0;
#endif
  }

  if (strcmp(option, "xerces-c") == 0 ||
    strcmp(option, "xercesc") == 0)
  {
#ifdef USE_XERCES
#ifdef _XERCES_VERSION
    return _XERCES_VERSION;
#else
    return 1;
#endif
#else
    return 0;
#endif
  }

  if (strcmp(option, "zlib") == 0 ||
    strcmp(option, "zip") == 0)
  {
#ifdef USE_ZLIB
    return ZLIB_VERNUM;
#else
    return 0;
#endif
  }

  if (strcmp(option, "bzip") == 0 ||
    strcmp(option, "bzip2") == 0 ||
    strcmp(option, "bz2") == 0)
  {
#ifdef USE_BZ2
    return 1;
#else
    return 0;
#endif
  }

  return 0;
}

/**
 * Returns the version string for the dependency library used. 
 *
 * @param option the library for which the version
 *        should be retrieved, this can be one of
 *        "expat", "libxml", "xerces-c", "bzip2", "zip"
 * 
 * @return NULL in case libSBML has not been compiled against 
 *         that library and a version string otherwise.
 *
 * @see isLibSBMLCompiledWith()
 */
LIBSBML_EXTERN
const char* 
getLibSBMLDependencyVersionOf(const char* option)
{
  if (option == NULL) return NULL;
  
  if (strcmp(option, "expat") == 0)
  {
#ifdef USE_EXPAT
    return static_cast<const char*>(XML_ExpatVersion());
#else
    return NULL;
#endif
  }

  if (strcmp(option, "libxml") == 0 ||
    strcmp(option, "libxml2") == 0)
  {
#ifdef USE_LIBXML
    return LIBXML_DOTTED_VERSION;
#else
    return NULL;
#endif
  }

  if (strcmp(option, "xerces-c") == 0 ||
    strcmp(option, "xercesc") == 0)
  {
#ifdef USE_XERCES
    return XERCES_FULLVERSIONDOT;
#else
    return NULL;
#endif
  }

  if (strcmp(option, "zlib") == 0 ||
    strcmp(option, "zip") == 0)
  {
#ifdef USE_ZLIB
    return ZLIB_VERSION;
#else
    return NULL;
#endif
  }

  if (strcmp(option, "bzip") == 0 ||
    strcmp(option, "bzip2") == 0 ||
    strcmp(option, "bz2") == 0)
  {
#ifdef USE_BZ2
    return BZ2_bzlibVersion();
#else
    return NULL;
#endif
  }

  return NULL;
}



LIBSBML_CPP_NAMESPACE_END


