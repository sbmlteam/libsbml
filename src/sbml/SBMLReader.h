/**
 * @file    SBMLReader.h
 * @brief   Reads an SBML Document into memory
 * @author  Ben Bornstein
 *
 * $Id$
 * $Source$
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2007 California Institute of Technology.
 * Copyright 2002-2005 California Institute of Technology and
 *                     Japan Science and Technology Corporation.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *----------------------------------------------------------------------- -->*/

#ifndef SBMLReader_h
#define SBMLReader_h


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/util/util.h>


#ifdef __cplusplus


#include <string>


class SBMLDocument;


class LIBSBML_EXTERN SBMLReader
{
public:

  /**
   * Creates a new SBMLReader and returns it. 
   */
  SBMLReader ();

  /**
   * Destorys this SBMLReader.
   */
  virtual ~SBMLReader ();


  /**
   * Reads an SBML document from the given file.  If filename does not
   * exist or is not an SBML file, a fatal error will be logged.
   * Errors can be identified by their unique ids, e.g.:
   *
   * <code>
   *   SBMLDocument* d = reader.readSBML(filename);
   *
   *   if (d->getNumErrors() > 0)\n
   *   {\n
   *     if (d->getError(0)->getId() == SBML_READ_ERROR_FILE_NOT_FOUND)\n
   *     if (d->getError(0)->getId() == SBML_READ_ERROR_NOT_SBML)\n
   *   }\n
   * </code>
   *
   * @return a pointer to the SBMLDocument read.
   */
  SBMLDocument* readSBML (const std::string& filename);

  /**
   * Reads an SBML document from the given XML string.
   *
   * If the string does not begin with XML declaration:
   *
   *   <?xml version='1.0' encoding='UTF-8'?>
   *
   * it will be prepended.
   *
   * This method will log a fatal error if the XML string is not SBML.  See
   * the method documentation for readSBML(filename) for example error
   * checking code.
   *
   * @return a pointer to the SBMLDocument read.
   */
  SBMLDocument* readSBMLFromString (const std::string& xml);



protected:

  /**
   * Used by readSBML() and readSBMLFromString().
   */
  SBMLDocument* readInternal (const char* content, bool isFile = true);


};


#endif /* __cplusplus */


BEGIN_C_DECLS


#ifndef SWIG


/**
 * Creates a new SBMLReader and returns it.  By default XML Schema
 * validation is off.
 */
LIBSBML_EXTERN
SBMLReader_t *
SBMLReader_create (void);

/**
 * Frees the given SBMLReader.
 */
LIBSBML_EXTERN
void
SBMLReader_free (SBMLReader_t *sr);


/**
 * Reads an SBML document from the given file.  If filename does not exist
 * or is not an SBML file, an error will be logged.  Errors can be
 * identified by their unique ids, e.g.:
 *
 * <code>
 *   SBMLReader_t   *sr;\n
 *   SBMLDocument_t *d;
 *
 *   sr = SBMLReader_create();
 *
 *   d = SBMLReader_readSBML(reader, filename);
 *
 *   if (SBMLDocument_getNumErrors(d) > 0)\n
 *   {\n
 *     if (XMLError_getId(SBMLDocument_getError(d, 0))
 *                                           == SBML_READ_ERROR_FILE_NOT_FOUND)\n
 *     if (XMLError_getId(SBMLDocument_getError(d, 0))
 *                                           == SBML_READ_ERROR_NOT_SBML)\n
 *   }\n
 * </code>
 *
 * @return a pointer to the SBMLDocument read.
 */
LIBSBML_EXTERN
SBMLDocument_t *
SBMLReader_readSBML (SBMLReader_t *sr, const char *filename);

/**
 * Reads an SBML document from the given XML string.
 *
 * If the string does not begin with XML declaration:
 *
 *   <?xml version='1.0' encoding='UTF-8'?>
 *
 * it will be prepended.
 *
 * This method will log a fatal error if the XML string is not SBML.  See
 * the method documentation for readSBML(filename) for example error
 * checking code.
 *
 * @return a pointer to the SBMLDocument read.
 */
LIBSBML_EXTERN
SBMLDocument_t *
SBMLReader_readSBMLFromString (SBMLReader_t *sr, const char *xml);


#endif  /* !SWIG */


/**
 * Reads an SBML document from the given file.  If filename does not exist
 * or is not an SBML file, an error will be logged.  Errors can be
 * identified by their unique ids, e.g.:
 *
 * <code>
 *   SBMLReader_t   *sr;\n
 *   SBMLDocument_t *d;
 *
 *   sr = SBMLReader_create();
 *
 *   d = SBMLReader_readSBML(reader, filename);
 *
 *   if (SBMLDocument_getNumErrors(d) > 0)\n
 *   {\n
 *     if (XMLError_getId(SBMLDocument_getError(d, 0))
 *                                           == SBML_READ_ERROR_FILE_NOT_FOUND)\n
 *     if (XMLError_getId(SBMLDocument_getError(d, 0))
 *                                           == SBML_READ_ERROR_NOT_SBML)\n
 *   }\n
 * </code>
 *
 * @return a pointer to the SBMLDocument read.
 */
LIBSBML_EXTERN
SBMLDocument_t *
readSBML (const char *filename);

/**
 * Reads an SBML document from the given XML string.
 *
 * If the string does not begin with XML declaration:
 *
 *   <?xml version='1.0' encoding='UTF-8'?>
 *
 * it will be prepended.
 *
 * This method will log a fatal error if the XML string is not SBML.  See
 * the method documentation for readSBML(filename) for example error
 * checking code.
 *
 * @return a pointer to the SBMLDocument read.
 */
LIBSBML_EXTERN
SBMLDocument_t *
readSBMLFromString (const char *xml);


END_C_DECLS


#endif  /* SBMLReader_h */
