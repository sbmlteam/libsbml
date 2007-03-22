/**
 * \file    SBMLReader.cpp
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


#include <sbml/xml/XMLInputStream.h>

#include <sbml/SBMLDocument.h>
#include <sbml/SBMLReader.h>


using namespace std;


/**
 * Creates a new SBMLReader and returns it.  By default XML Schema
 * validation is off.
 */
SBMLReader::SBMLReader (bool doSchemaValidation) :
  mDoSchemaValidation(doSchemaValidation)
{
}


/**
 * Destorys this SBMLReader.
 */
SBMLReader::~SBMLReader ()
{
}


/**
 * Reads an SBML document from the given file.  If filename does not exist
 * or is not an SBML file, a fatal error will be logged.  Errors can be
 * identified by their unique ids, e.g.:
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
SBMLDocument*
SBMLReader::readSBML (const string& filename)
{
  return readInternal(filename.c_str(), true);
}


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
SBMLDocument*
SBMLReader::readSBMLFromString (const string& xml)
{
  return readInternal(xml.c_str(), false);
}


/**
 * @return true if this SBMLReader will perform XML Schema validation,
 * false otherwise.
 */
bool
SBMLReader::doSchemaValidation () const
{
  return mDoSchemaValidation;
}


/**
 * Indicates whether or not this SBMLReader should perform XML Schema
 * validation.
 */
void
SBMLReader::setSchemaValidation (bool doSchemaValidation)
{
  mDoSchemaValidation = doSchemaValidation;
}


/**
 * Used by readSBML() and readSBMLFromString().
 */
SBMLDocument*
SBMLReader::readInternal (const char* content, bool isFile)
{
  SBMLDocument* d = new SBMLDocument();

  if (isFile && content && (util_file_exists(content) == false))
  {
    d->getErrorLog()->logError(00001, 0);
  }
  else
  {
  
    XMLInputStream stream(content, isFile);

    if (stream.getEncoding() == "")
    {
      d->getErrorLog()->logError(00002, 0);
    }
    else if (stream.getEncoding() != "UTF-8")
    {
      d->getErrorLog()->logError(10101, 0);
    }

    stream.setErrorLog( d->getErrorLog() );
    d->read(stream);

    if (d->getModel() == 0)
    {
      d->getErrorLog()->logError(20201, 0);
    }

  }
  return d;
}


/**
 * Creates a new SBMLReader and returns it.  By default XML Schema
 * validation is off.
 */
LIBSBML_EXTERN
SBMLReader_t *
SBMLReader_create ()
{
  return new (nothrow) SBMLReader;
}


/**
 * Frees the given SBMLReader.
 */
LIBSBML_EXTERN
void
SBMLReader_free (SBMLReader_t *sr)
{
  delete sr;
}


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
SBMLReader_readSBML (SBMLReader_t *sr, const char *filename)
{
  return (filename != NULL) ? sr->readSBML(filename) : sr->readSBML("");
}


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
SBMLReader_readSBMLFromString (SBMLReader_t *sr, const char *xml)
{
  return (xml != NULL) ? sr->readSBMLFromString(xml) :
                         sr->readSBMLFromString("");
}


/**
 * @return true if this SBMLReader will perform XML Schema validation,
 * false otherwise.
 */
LIBSBML_EXTERN
int
SBMLReader_doSchemaValidation (const SBMLReader_t *sr)
{
  return static_cast<int>( sr->doSchemaValidation() );
}


/**
 * Indicates whether or not this SBMLReader should perform XML Schema
 * validation.
 */
LIBSBML_EXTERN
void
SBMLReader_setSchemaValidation (SBMLReader_t *sr, int doSchemaValidation)
{
  sr->setSchemaValidation( static_cast<bool>(doSchemaValidation) );
}


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
readSBML (const char *filename)
{
  SBMLReader sr;
  return sr.readSBML(filename);
}


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
readSBMLFromString (const char *xml)
{
  SBMLReader sr;
  return sr.readSBMLFromString(xml);
}
