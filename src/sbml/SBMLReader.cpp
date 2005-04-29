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

// This needs to be before the xerces includes for Visual C++ 6.0 
#include "xml/ParseMessage.h"

#ifdef USE_EXPAT
typedef void SAX2XMLReader;
#else
#  include <xercesc/framework/MemBufInputSource.hpp>
#  include <xercesc/sax2/DefaultHandler.hpp>
#  include <xercesc/sax2/SAX2XMLReader.hpp>
#  include <xercesc/sax2/XMLReaderFactory.hpp>
#  include <xercesc/util/PlatformUtils.hpp>
#  include <xercesc/util/XMLString.hpp>
   using namespace xercesc;
#endif  // USE_EXPAT

#include "SBMLDocument.h"
#include "SBMLHandler.h"

#include "SBMLReader.h"


using namespace std;


#ifndef USE_EXPAT
/**
 * Creates a new ParseMessage from the given XMLException and returns a
 * pointer to it.
 */
ParseMessage_t*
ParseMessage_createFrom (const XMLException& e)
{
  char*           msg = XMLString::transcode( e.getMessage() );
  ParseMessage_t* pm;


  pm = ParseMessage_createWith(0, msg, e.getSrcLine(), 0);

  XMLString::release(&msg);

  return pm;
}
#endif  // !USE_EXPAT


/**
 * Creates a new XMLReader and returns a pointer to it.
 *
 * The given DefaultHandler is set as both the primary content and error
 * handler for the XMLReader.  The reader is also configured with some
 * basic settings for reading SBML files.
 */
SAX2XMLReader*
XMLReader_create (DefaultHandler* h)
{
  SAX2XMLReader* xr;


#ifdef USE_EXPAT
  xr = NULL;
#else
  xr = XMLReaderFactory::createXMLReader();


  xr->setFeature( XMLUni::fgSAX2CoreNameSpaces       , true );
  xr->setFeature( XMLUni::fgSAX2CoreNameSpacePrefixes, true );

  xr->setContentHandler(h);
  xr->setErrorHandler  (h);
#endif  // USE_EXPAT

  return xr;
}


#ifndef USE_EXPAT
/**
 * Reads only the <sbml> element from either filename or MemBufInputSource.
 * The SBML level and version help determine the appropriate XML namespace
 * and schema file to use when validating SBML documents.
 */
void
XMLReader_readSBMLElement ( SAX2XMLReader*     reader,
                            const char*        filename,
                            MemBufInputSource* input )
{
  XMLPScanToken token;
  bool          success = false;


  if (input != NULL)
  {
    success = reader->parseFirst(*input, token);
  }
  else
  {
    success = reader->parseFirst(filename, token);
  }

  if (success)
  {
    reader->parseNext(token);
  }
}


/**
 * Sets the given XMLReader reader to validate the document it reads
 * against an XML Schema.
 */
void
XMLReader_setSchema (SAX2XMLReader* xr, SBMLReader* sr)
{
  bool doFull = (sr->getSchemaValidationLevel() == XML_SCHEMA_VALIDATION_FULL);


  xr->setFeature( XMLUni::fgSAX2CoreValidation      , true   );
  xr->setFeature( XMLUni::fgXercesSchema            , true   );
  xr->setFeature( XMLUni::fgXercesSchemaFullChecking, doFull );
}


/**
 * Sets the ExternalSchemaLocation property of the given XMLReader reader.
 */
void
XMLReader_setSchemaLocation (SAX2XMLReader* xr, const char* location)
{
  XMLCh* s = XMLString::transcode(location);
      

  xr->setProperty(XMLUni::fgXercesSchemaExternalSchemaLocation, s);

  XMLString::release(&s);
}
#endif  // !USE_EXPAT


/**
 * Creates a new SBMLReader and returns it.
 *
 * By default schema validation is off (XML_SCHEMA_VALIDATION_NONE) and
 * schemaFilenames are empty.
 */
LIBSBML_EXTERN
SBMLReader::SBMLReader (XMLSchemaValidation_t level) :
  schemaValidationLevel(level)
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
 * @return the schema filename used by this SBMLReader to validate SBML
 * Level 1 version 1 documents.
 */
LIBSBML_EXTERN
const std::string&
SBMLReader::getSchemaFilenameL1v1 () const
{
  return schemaFilenameL1v1;
}


/**
 * @return the schema filename used by this SBMLReader to validate SBML
 * Level 1 version 2 documents.
 */
LIBSBML_EXTERN
const std::string&
SBMLReader::getSchemaFilenameL1v2 () const
{
  return schemaFilenameL1v2;
}


/**
 * @return the schema filename used by this SBMLReader to validate SBML Level
 * 2 version 1 documents.
 */
LIBSBML_EXTERN
const std::string&
SBMLReader::getSchemaFilenameL2v1 () const
{
  return schemaFilenameL2v1;
}


/**
 * @return the SchemaLocation to use when reading the given SBMLDocument.
 * 
 * This function assumes the SBMLDocument level and version correspond to
 * the document that will be read.
 */
char*
SBMLReader::getSchemaLocation (SBMLDocument* d) const
{
  unsigned int level   = d->getLevel();
  unsigned int version = d->getVersion();

  const char* xmlns    = NULL;
  const char* filename = NULL;
  char*       location = NULL;


  if (level == 1)
  {
    xmlns    = "http://www.sbml.org/sbml/level1 ";
    filename = (version == 1) ? schemaFilenameL1v1.c_str() :
                                schemaFilenameL1v2.c_str();
  }
  else if (level == 2)
  {
    xmlns    = "http://www.sbml.org/sbml/level2 ";
    filename = schemaFilenameL2v1.c_str();
  }

  //
  // Set location
  //
  if (xmlns && filename)
  {
    location = safe_strcat(xmlns, filename);
  }

  return location;
}


/**
 * @return the schema validation level used by this SBMLReader.
 */
LIBSBML_EXTERN
XMLSchemaValidation_t
SBMLReader::getSchemaValidationLevel() const
{
  return schemaValidationLevel;
}


/**
 * Used by readSBML() and readSBMLFromString()  Pass in either:
 *
 *   - a filename to read and a NULL XML string, or
 *   - a NULL filename and an XML string
 */
SBMLDocument*
SBMLReader::readSBML_internal (const char* filename, const char* xml)
{
  if (filename && !util_file_exists(filename)) return 0;


#ifdef USE_EXPAT

  const int BUFFER_SIZE = 0xfffe;
  char*     pBuffer     = 0;

  SBMLDocument* d = new SBMLDocument();  
  SBMLHandler   handler(d);


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
    d->fatal.add( new ParseMessage(0, msg) );
    d->setModel(0);
  }
  catch (ios_base::failure& e)
  {
    ostringstream msg;
    msg << "Error while reading from file '" << filename << "'.";

    d->fatal.add( new ParseMessage(0, msg.str()) );
    d->setModel(0);
  }
  catch (...)
  {
    d->fatal.add( new ParseMessage(0, "Unexpected Parse Error") );
    d->setModel(0);
  }

  delete [] pBuffer;

#else

  SBMLDocument* d = new SBMLDocument;


  try
  {
    XML_PLATFORM_UTILS_INIT();
  }
  catch (const XMLException& e)
  {
    d->fatal.add( ParseMessage_createFrom(e) );
    return d;
  }


  DefaultHandler*    handler = new SBMLHandler(d);
  SAX2XMLReader*     reader  = XMLReader_create(handler);
  MemBufInputSource* input   = NULL;


  if (xml != NULL)
  {
    input = new MemBufInputSource( (const XMLByte*) xml,
                                   strlen(xml),
                                   "FromString",
                                   false );
  }

  //
  // XML Schema Validation (based on the state of the SBMLReader)
  //
  if (schemaValidationLevel != XML_SCHEMA_VALIDATION_NONE)
  {
    //
    // Reads only the <sbml> element from either filename or
    // MemBufInputSource.
    //
    // The reader must be reset (destroyed and re-recreated) after calling
    // this function.
    //
    XMLReader_readSBMLElement(reader, filename, input);

    delete reader;
    reader = XMLReader_create(handler);

    XMLReader_setSchema(reader, this);

    //
    // Set the schema location
    //
    char* location = getSchemaLocation(d);

    if (location != NULL)
    {
      XMLReader_setSchemaLocation(reader, location);
      safe_free(location);
    }
  }


  //
  // Read SBML document
  //
  try
  {
    if (input != NULL)
    {
      reader->parse(*input);
    }
    else
    {
      reader->parse(filename);
    }
  }
  catch (const XMLException& e)
  {
    d->fatal.add( ParseMessage_createFrom(e) );
  }
  catch (...)
  {
    d->fatal.add( new ParseMessage(100, "Unexpected Exception", 0, 0) );
  }

  delete input;
  delete reader;
  delete handler;

#endif  // USE_EXPAT

  return d;
}


/**
 * Reads an SBML document from the given file.
 *
 * @return a pointer to the SBMLDocument read or NULL if the given file
 * does not exist.
 */
LIBSBML_EXTERN
SBMLDocument*
SBMLReader::readSBML (const std::string& filename)
{
  return readSBML_internal(filename.c_str(), NULL);
}


/**
 * Reads an SBML document from the given XML string.
 *
 * The XML string must be complete and legal XML document.  Among other
 * things, it must start with an XML processing instruction.  For e.g.,:
 *
 *   <?xml version='1.0' encoding='UTF-8'?>
 *
 * @return a pointer to the SBMLDocument read.
 */
LIBSBML_EXTERN
SBMLDocument*
SBMLReader::readSBMLFromString (const std::string& xml)
{
  return readSBML_internal(NULL, xml.c_str());
}


/**
 * Sets the schema filename used by this SBMLReader to validate SBML Level
 * 1 version 1 documents.
 *
 * The filename should be either i) an absolute path or ii) relative to the
 * directory contain the SBML file(s) to be read.
 */
LIBSBML_EXTERN
void
SBMLReader::setSchemaFilenameL1v1 (const std::string& filename)
{
  schemaFilenameL1v1 = filename;
}


/**
 * Sets the schema filename used by this SBMLReader to validate SBML Level
 * 1 version 2 documents.
 *
 * The filename should be either i) an absolute path or ii) relative to the
 * directory contain the SBML file(s) to be read.
 */
LIBSBML_EXTERN
void
SBMLReader::setSchemaFilenameL1v2 (const std::string& filename)
{
  schemaFilenameL1v2 = filename;
}


/**
 * Sets the schema filename used by this SBMLReader to validate SBML Level
 * 2 version 1 documents.
 *
 * The filename should be either i) an absolute path or ii) relative to the
 * directory contain the SBML file(s) to be read.
 */
LIBSBML_EXTERN
void
SBMLReader::setSchemaFilenameL2v1 (const std::string& filename)
{
  schemaFilenameL2v1 = filename;
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
  schemaValidationLevel = level;
}




/**
 * Creates a new SBMLReader and returns a pointer to it.
 *
 * By default schema validation is off (XML_SCHEMA_VALIDATION_NONE) and
 * schemaFilename is NULL.
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
 * @return the schema filename used by this SBMLReader to validate SBML
 * Level 1 version 1 documents.
 */
LIBSBML_EXTERN
const char *
SBMLReader_getSchemaFilenameL1v1 (const SBMLReader_t *sr)
{
  const std::string& s = sr->getSchemaFilenameL1v1();
  return s.empty() ? NULL : s.c_str();
}


/**
 * @return the schema filename used by this SBMLReader to validate SBML
 * Level 1 version 2 documents.
 */
LIBSBML_EXTERN
const char *
SBMLReader_getSchemaFilenameL1v2 (const SBMLReader_t *sr)
{
  const std::string& s = sr->getSchemaFilenameL1v2();
  return s.empty() ? NULL : s.c_str();
}


/**
 * @return the schema filename used by this SBMLReader to validate SBML Level
 * 2 version 1 documents.
 */
LIBSBML_EXTERN
const char *
SBMLReader_getSchemaFilenameL2v1 (const SBMLReader_t *sr)
{
  const std::string& s = sr->getSchemaFilenameL2v1();
  return s.empty() ? NULL : s.c_str();
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
 * Reads an SBML document from the given file.
 *
 * @return a pointer to the SBMLDocument read or NULL if filename is NULL
 * or does not exist.
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
 * @return a pointer to the SBMLDocument read or NULL if the given XML
 * string is NULL.
 */
LIBSBML_EXTERN
SBMLDocument_t *
SBMLReader_readSBMLFromString (SBMLReader_t *sr, const char *xml)
{
  return (xml != NULL) ? sr->readSBMLFromString(xml) : NULL;
}


/**
 * Reads an SBML document from the given file.  This convenience function
 * is functionally equivalent to:
 *
 *   SBMLReader_readSBML(SBMLReader_create(), filename);
 *
 * @return a pointer to the SBMLDocument read or NULL if the given file
 * does not exist.
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
 * This convenience function is functionally equivalent to:
 *
 *   SBMLReader_readSBMLFromString(SBMLReader_create(), filename);
 *
 * @return a pointer to the SBMLDocument read or NULL if the given XML
 * string is NULL.
 */
LIBSBML_EXTERN
SBMLDocument_t *
readSBMLFromString (const char *xml)
{
  SBMLReader sr;
  return SBMLReader_readSBMLFromString(&sr, xml);
}


/**
 * Sets the schema filename used by this SBMLReader to validate SBML Level
 * 1 version 1 documents.
 *
 * The filename should be either i) an absolute path or ii) relative to the
 * directory contain the SBML file(s) to be read.
 */
LIBSBML_EXTERN
void
SBMLReader_setSchemaFilenameL1v1 (SBMLReader_t *sr, const char *filename)
{
  sr->setSchemaFilenameL1v1(filename ? filename : "");
}


/**
 * Sets the schema filename used by this SBMLReader to validate SBML Level
 * 1 version 2 documents.
 *
 * The filename should be either i) an absolute path or ii) relative to the
 * directory contain the SBML file(s) to be read.
 */
LIBSBML_EXTERN
void
SBMLReader_setSchemaFilenameL1v2 (SBMLReader_t *sr, const char *filename)
{
  sr->setSchemaFilenameL1v2(filename ? filename : "");
}


/**
 * Sets the schema filename used by this SBMLReader to validate SBML Level
 * 2 version 1 documents.
 *
 * The filename should be either i) an absolute path or ii) relative to the
 * directory contain the SBML file(s) to be read.
 */
LIBSBML_EXTERN
void
SBMLReader_setSchemaFilenameL2v1 (SBMLReader_t *sr, const char *filename)
{
  sr->setSchemaFilenameL2v1(filename ? filename : "");
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
