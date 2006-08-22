/**
 * \file    SBMLReader.h
 * \brief   Reads an SBML Document into memory
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2002 California Institute of Technology and Japan Science and
 * Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is
 * provided in the file named "LICENSE.txt" included with this software
 * distribution.  It is also available online at
 * http://sbml.org/software/libsbml/license.html
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 */


#ifndef SBMLReader_h
#define SBMLReader_h


#include <sbml/common/extern.h>
#include <sbml/common/sbmlfwd.h>
#include <sbml/util/util.h>


/**
 * Warnings, errors and fatal errors are logged to an SBMLDocument while
 * reading.  Three types of fatal errors in particular will be logged
 * immediately.  They have the following ParseMessage ids:
 */
typedef enum
{
    SBML_READ_ERROR_NONE
  , SBML_READ_ERROR_OUT_OF_MEMORY
  , SBML_READ_ERROR_FILE_NOT_FOUND
  , SBML_READ_ERROR_NOT_XML
  , SBML_READ_ERROR_NO_ENCODING
  , SBML_READ_ERROR_NOT_UTF_8
  , SBML_READ_ERROR_UNKNOWN_ENCODING
  , SBML_READ_ERROR_NOT_SBML
  , SBML_READ_ERROR_UNKNOWN_SBML
  , SBML_READ_ERROR_UNKNOWN
} SBMLReadError_t;


#ifdef __cplusplus


#include <string>


class SBMLDocument;


class LIBSBML_EXTERN SBMLReader
{
public:

  /**
   * Creates a new SBMLReader and returns it.  By default XML Schema
   * validation is off.
   */
  SBMLReader (bool doSchemaValidation = false);

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
   *   if (d->getNumFatals() > 0)
   *   {
   *     if (d->getFatal(0)->getId() == SBML_READ_ERROR_FILE_NOT_FOUND)
   *     if (d->getFatal(0)->getId() == SBML_READ_ERROR_NOT_SBML)
   *   }
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


  /**
   * @return true if this SBMLReader will perform XML Schema validation,
   * false otherwise.
   */
  bool doSchemaValidation () const;

  /**
   * Indicates whether or not this SBMLReader should perform XML Schema
   * validation.
   */
  void setSchemaValidation (bool doSchemaValidation);


protected:

  /**
   * Used by readSBML() and readSBMLFromString().
   */
  SBMLDocument* readInternal (const char* content, bool isFile = true);


  bool mDoSchemaValidation;
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
 * or is not an SBML file, a fatal error will be logged.  Errors can be
 * identified by their unique ids, e.g.:
 *
 * <code>
 *   SBMLReader     *sr;
 *   SBMLDocument_t *d;
 *
 *   sr = SBMLReader_create();
 *   SBMLReader_setSchemaValidationLevel(sr, XML_SCHEMA_VALIDATION_BASIC);
 *
 *   d = SBMLReader_readSBML(reader, filename);
 *
 *   if (SBMLDocument_getNumFatals(d) > 0)
 *   {
 *     ParseMessage_t *pm = SBMLDocument_getFatal(d, 0);
 *     if (ParseMessage_getId(pm) == SBML_READ_ERROR_FILE_NOT_FOUND)
 *     if (ParseMessage_getId(pm) == SBML_READ_ERROR_NOT_SBML)
 *   }
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


/**
 * @return true if this SBMLReader will perform XML Schema validation,
 * false otherwise.
 */
LIBSBML_EXTERN
int
SBMLReader_doSchemaValidation (const SBMLReader_t *sr);

/**
 * Indicates whether or not this SBMLReader should perform XML Schema
 * validation.
 */
LIBSBML_EXTERN
void
SBMLReader_setSchemaValidation (SBMLReader_t *sr, int doSchemaValidation);


#endif  /* !SWIG */


/**
 * Reads an SBML document from the given file.  If filename does not exist
 * or is not an SBML file, a fatal error will be logged.  Errors can be
 * identified by their unique ids, e.g.:
 *
 * <code>
 *   SBMLDocument_t *d = SBMLReader_readSBML(reader, filename);
 *
 *   if (SBMLDocument_getNumFatals(d) > 0)
 *   {
 *     ParseMessage_t *pm = SBMLDocument_getFatal(d, 0);
 *     if (ParseMessage_getId(pm) == SBML_READ_ERROR_FILE_NOT_FOUND)
 *     if (ParseMessage_getId(pm) == SBML_READ_ERROR_NOT_SBML)
 *   }
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
