/**
 * \file    SBMLReader.cpp
 * \brief   Reads an SBML Document into memory
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2002 California Institute of Technology and
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
 *     http://www.cds.caltech.edu/erato
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s):
 *   Stefan Hoops
 */


#include <ios>
#include <fstream>
#include <sstream>

#include "common/common.h"

// This needs to be before the Xerces-C++ includes for Visual C++ 6.0 
#include "xml/ParseMessage.h"

#ifdef USE_EXPAT
typedef void SAX2XMLReader;
#else
#  include <xercesc/util/PlatformUtils.hpp>
#endif  // USE_EXPAT

#include "SBMLDocument.h"
#include "SBMLHandler.h"
#include "SBMLParser.h"
#include "SBMLReader.h"


#ifndef USE_EXPAT
using namespace xercesc;
#endif // !USE_EXPAT

using namespace std;


/**
 * Maps SBMLReadError_t code to a message
 */
static
const char* SBMLReadErrorMessages[] =
{
    "No error"
  , "Out of memory"
  , "File not found"
  , "Content is not XML"
  , "No encoding specified."
  , "Content is not UTF-8 encoded"
  , "Unknown encoding"
  , "Content is not SBML"
  , "Unknown SBML Level/Version combination"
  , "Generic Internal Error"
};


/**
 * @return a message for the given SBMLReadError_t code.
 */
static const char*
getMessage (SBMLReadError_t code)
{
  if (code < SBML_READ_ERROR_NONE || code > SBML_READ_ERROR_UNKNOWN)
  {
    code = SBML_READ_ERROR_UNKNOWN;
  }

  return SBMLReadErrorMessages[code];
}


/**
 * Creates a new SBMLReader and returns it.
 *
 * By default schema validation is off (XML_SCHEMA_VALIDATION_NONE).
 */
LIBSBML_EXTERN
SBMLReader::SBMLReader (XMLSchemaValidation_t level) : mValidationLevel(level)
{
}


/**
 * Destorys this SBMLReader.
 */
LIBSBML_EXTERN
SBMLReader::~SBMLReader ()
{
}


/**
 * @return the schema validation level used by this SBMLReader.
 */
LIBSBML_EXTERN
XMLSchemaValidation_t
SBMLReader::getSchemaValidationLevel() const
{
  return mValidationLevel;
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
LIBSBML_EXTERN
SBMLDocument*
SBMLReader::readSBML (const std::string& filename)
{
  return read_internal(filename.c_str(), NULL);
}


/**
 * Reads an SBML document from the given XML string.
 *
 * The XML string must be complete and legal XML document.  Among other
 * things, it must start with an XML processing instruction.  For e.g.,:
 *
 *   <?xml version='1.0' encoding='UTF-8'?>
 *
 * This method will log a fatal error if the XML string is not SBML.  See
 * the method documentation for readSBML(filename) for example error
 * checking code.
 *
 * @return a pointer to the SBMLDocument read.
 */
LIBSBML_EXTERN
SBMLDocument*
SBMLReader::readSBMLFromString (const std::string& xml)
{
  return read_internal(NULL, xml.c_str());
}


/**
 * Sets the schema validation level used by this SBMLReader.
 *
 * The levels are:
 *
 *   XML_SCHEMA_VALIDATION_NONE (0) turns schema validation off.
 *
 *   XML_SCHEMA_VALIDATION_BASIC (1) validates an XML instance document
 *   against an XML Schema.  Those who wish to perform schema checking on
 *   SBML documents should use this option.
 *
 *   XML_SCHEMA_VALIDATION_FULL (2) validates both the instance document
 *   itself *and* the XML Schema document.  The XML Schema document is
 *   checked for violation of particle unique attribution constraints and
 *   particle derivation restrictions, which is both time-consuming and
 *   memory intensive.
 */
LIBSBML_EXTERN
void
SBMLReader::setSchemaValidationLevel (XMLSchemaValidation_t level)
{
  mValidationLevel = level;
}


/**
 * Logs the given fatal error to the SBMLDocument's list of fatal errors.
 * If message is not given, a message will be provided based on the error
 * code.
 */
void
SBMLReader::logFatal (SBMLDocument* d, SBMLReadError_t code, const char* msg)
{
  if (!msg) msg = getMessage(code);
  d->fatal.add( new ParseMessage(code, msg) );
}


/**
 * Used by readSBML() and readSBMLFromString()  Pass in either:
 *
 *   - a filename to read and a NULL XML string, or
 *   - a NULL filename and an XML string
 */
SBMLDocument*
SBMLReader::read_internal (const char* filename, const char* xml)
{
  SBMLDocument* d = new SBMLDocument();

  if (filename && (util_file_exists(filename) == false))
  {
    logFatal(d, SBML_READ_ERROR_FILE_NOT_FOUND);
  }
  else
  {
    #ifdef USE_EXPAT
      read_expat (d, filename, xml);
    #else
      read_xerces(d, filename, xml);
    #endif  // USE_EXPAT
  }

  return d;
}


void
SBMLReader::read_expat (SBMLDocument* d, const char* filename, const char* xml)
{
#ifdef USE_EXPAT

  const int BUFFER_SIZE = 0xfffe;
  char*     pBuffer     = 0;


  SBMLHandler handler(d);
  handler.enableElementHandler();

  try
  {
    if (xml)
    {
      if (!handler.parse(xml, -1, true))
        throw handler.getErrorString();
    }
    else if (filename)
    {
      std::ifstream is(filename);
      is.exceptions(ios_base::badbit | ios_base::failbit);

      pBuffer   = new char[BUFFER_SIZE + 1];
      bool done = false;

      while (!done)
      {
        is.get(pBuffer, BUFFER_SIZE, 0);
        done = is.eof();
            
        if (!handler.parse(pBuffer, -1, done))
          throw handler.getErrorString();
      }
    }
  }
  catch (const char* msg)
  {
    logFatal(d, SBML_READ_ERROR_UNKNOWN, msg);
  }
  catch (...)
  {
    logFatal(d, SBML_READ_ERROR_UNKNOWN);
  }

  if (handler.sawSBML() == false)
  {
    logFatal(d, SBML_READ_ERROR_NOT_SBML);
  }

  delete [] pBuffer;
#endif  // USE_EXPAT
}


void
SBMLReader::read_xerces (SBMLDocument* d, const char* filename, const char* xml)
{
#ifndef USE_EXPAT
  try
  {
    XML_PLATFORM_UTILS_INIT();

    const char* content = filename;
    bool        isFile  = true;

    if (xml)
    {
      content = xml;
      isFile  = false;
    }

    SBMLParser  parser(d, content, isFile);

    parser.setSchemaValidation(mValidationLevel);
    parser.parse();
  }
  catch (std::bad_alloc& e)
  {
    logFatal(d, SBML_READ_ERROR_OUT_OF_MEMORY);
  }
  catch (NotXMLException& e)
  {
    logFatal(d, SBML_READ_ERROR_NOT_XML);
  }
  catch (UnknownEncodingException& e)
  {
    logFatal(d, SBML_READ_ERROR_UNKNOWN_ENCODING);
  }
  catch (NotSBMLException& e)
  {
    logFatal(d, SBML_READ_ERROR_NOT_SBML);
  }
  catch (UnknownSBMLException& e)
  {
    logFatal(d, SBML_READ_ERROR_UNKNOWN_SBML);
  }
  catch (...)
  {
    logFatal(d, SBML_READ_ERROR_UNKNOWN);
  }
#endif  // !USE_EXPAT
}




/**
 * Creates a new SBMLReader and returns a pointer to it.
 *
 * By default schema validation is off (XML_SCHEMA_VALIDATION_NONE).
 */
LIBSBML_EXTERN
SBMLReader_t *
SBMLReader_create (void)
{
  return new (std::nothrow) SBMLReader;
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
 * @return the schema validation level used by this SBMLReader.
 */
LIBSBML_EXTERN
XMLSchemaValidation_t
SBMLReader_getSchemaValidationLevel(const SBMLReader_t *sr)
{
  return sr->getSchemaValidationLevel();
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
  return (filename != NULL) ? sr->readSBML(filename) : NULL;
}


/**
 * Reads an SBML document from the given XML string.
 *
 * The XML string must be complete and legal XML document.  Among other
 * things, it must start with an XML processing instruction.  For e.g.,:
 *
 *   <?xml version='1.0' encoding='UTF-8'?>
 *
 * This method will log a fatal error if the XML string is not SBML.  See
 * the function documentation for SBMLReader_readSBML(filename) for example
 * error checking code.
 *
 * @return a pointer to the SBMLDocument read.
 */
LIBSBML_EXTERN
SBMLDocument_t *
SBMLReader_readSBMLFromString (SBMLReader_t *sr, const char *xml)
{
  return (xml != NULL) ? sr->readSBMLFromString(xml) : NULL;
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
  return SBMLReader_readSBML(&sr, filename);
}


/**
 * Reads an SBML document from the given XML string.
 *
 * The XML string must be complete and legal XML document.  Among other
 * things, it must start with an XML processing instruction.  For e.g.,:
 *
 *   <?xml version='1.0' encoding='UTF-8'?>
 *
 * This method will log a fatal error if the XML string is not SBML.  See
 * the function documentation for readSBML(filename) for example error
 * checking code.
 *
 * @return a pointer to the SBMLDocument read.
 */
LIBSBML_EXTERN
SBMLDocument_t *
readSBMLFromString (const char *xml)
{
  SBMLReader sr;
  return SBMLReader_readSBMLFromString(&sr, xml);
}


/**
 * Sets the schema validation level used by this SBMLReader.
 *
 * The levels are:
 *
 *   XML_SCHEMA_VALIDATION_NONE (0) turns schema validation off.
 *
 *   XML_SCHEMA_VALIDATION_BASIC (1) validates an XML instance document
 *   against an XML Schema.  Those who wish to perform schema checking on
 *   SBML documents should use this option.
 *
 *   XML_SCHEMA_VALIDATION_FULL (2) validates both the instance document
 *   itself *and* the XML Schema document.  The XML Schema document is
 *   checked for violation of particle unique attribution constraints and
 *   particle derivation restrictions, which is both time-consuming and
 *   memory intensive.
 */
LIBSBML_EXTERN
void
SBMLReader_setSchemaValidationLevel ( SBMLReader_t *sr,
                                      XMLSchemaValidation_t level )
{
  sr->setSchemaValidationLevel(level);
}
