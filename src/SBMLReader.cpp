/**
 * Filename    : SBMLReader.cpp
 * Description : Reads an SBML Document into memory
 * Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2002-10-15
 * Revision    : $Id$
 * Source      : $Source$
 *
 * Copyright 2002 California Institute of Technology and
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
 *     mailto:sysbio-team@caltech.edu
 *
 * Contributor(s):
 */


#include <iostream>

#include <xercesc/framework/MemBufInputSource.hpp>
#include <xercesc/sax2/DefaultHandler.hpp>
#include <xercesc/sax2/SAX2XMLReader.hpp>
#include <xercesc/sax2/XMLReaderFactory.hpp>
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/util/XMLString.hpp>

#include "sbml/common.h"
#include "sbml/SBMLHandler.hpp"

#include "sbml/List.h"
#include "sbml/SBMLDocument.h"
#include "sbml/SBMLReader.h"


/**
 * Creates a new ParseMessage from the given XMLException and returns a
 * pointer to it.
 */
ParseMessage_t*
ParseMessage_createFrom (const XMLException& e)
{
  char*           msg = XMLString::transcode( e.getMessage() );
  ParseMessage_t* pm;


  pm = ParseMessage_createWith(msg, e.getSrcLine(), 0);

  XMLString::release(&msg);

  return pm;
}




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
  SAX2XMLReader* xr = XMLReaderFactory::createXMLReader();


  xr->setFeature( XMLUni::fgSAX2CoreNameSpaces       , true );
  xr->setFeature( XMLUni::fgSAX2CoreNameSpacePrefixes, true );

  xr->setContentHandler(h);
  xr->setErrorHandler  (h);

  return xr;
}


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
  bool          success;


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
XMLReader_setSchema (SAX2XMLReader* xr, SBMLReader_t* sr)
{
  bool doFull = (sr->schemaValidationLevel == XML_SCHEMA_VALIDATION_FULL);


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
  SBMLReader_t *sr = (SBMLReader_t *) safe_malloc( sizeof(SBMLReader_t) );


  sr->schemaValidationLevel = XML_SCHEMA_VALIDATION_NONE;
  sr->schemaFilenameL1v1    = NULL;
  sr->schemaFilenameL1v2    = NULL;
  sr->schemaFilenameL2v1    = NULL;

  return sr;
}


/**
 * Frees the given SBMLReader.
 */
LIBSBML_EXTERN
void
SBMLReader_free (SBMLReader_t *sr)
{
  if (sr == NULL) return;


  safe_free(sr->schemaFilenameL1v1);
  safe_free(sr->schemaFilenameL1v2);
  safe_free(sr->schemaFilenameL2v1);
  safe_free(sr);
}


/**
 * @return the SchemaLocation to use when reading the given SBMLDocument.
 * 
 * This function assumes the SBMLDocument level and version correspond to
 * the document that will be read.  The caller owns the returned string and
 * is responsible for freeing it
 */
char *
SBMLReader_getSchemaLocation (SBMLReader_t* sr, SBMLDocument_t* d)
{
  unsigned int level   = SBMLDocument_getLevel(d);
  unsigned int version = SBMLDocument_getVersion(d);

  char* xmlns    = NULL;
  char* filename = NULL;
  char* location = NULL;


  if (level == 1)
  {
    xmlns    = "http://www.sbml.org/sbml/level1 ";
    filename = (version == 1) ? sr->schemaFilenameL1v1 : sr->schemaFilenameL1v2;
  }
  else if (d->level == 2)
  {
    xmlns    = "http://www.sbml.org/sbml/level2 ";
    filename = sr->schemaFilenameL2v1;
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
 * Used by SBMLReader_readSBML(SBMLReader_t *sr, const char *filename) and
 * SBMLReader_readSBMLFromString(...).
 */
SBMLDocument_t*
SBMLReader_readSBML_internal ( SBMLReader_t* sr,
                               const char*   filename,
                               const char*   xml )
{
  SBMLDocument_t* d = SBMLDocument_create();


  try
  {
    XMLPlatformUtils::Initialize();
  }
  catch (const XMLException& e)
  {
    List_add(d->fatal, ParseMessage_createFrom(e));
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
  if (sr->schemaValidationLevel != XML_SCHEMA_VALIDATION_NONE)
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

    XMLReader_setSchema(reader, sr);

    //
    // Set the schema location
    //
    char* location = SBMLReader_getSchemaLocation(sr, d);

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
    List_add(d->fatal, ParseMessage_createFrom(e));
  }
  catch (...)
  {
    List_add(d->fatal, ParseMessage_createWith("Unexcepted Exception", 0, 0));
  }

  if (input != NULL)
  {
    delete input;
  }

  delete reader;
  delete handler;

  return d;
}


/**
 * Reads the SBML document from the given file and returns a pointer to it.
 */
LIBSBML_EXTERN
SBMLDocument_t *
SBMLReader_readSBML (SBMLReader_t *sr, const char *filename)
{
  return SBMLReader_readSBML_internal(sr, filename, NULL);
}


/**
 * Reads the SBML document from the given XML string and returns a pointer
 * to it.
 *
 * The XML string must be complete and legal XML document.  Among other
 * things, it must start with an XML processing instruction.  For e.g.,:
 *
 *   <?xml version='1.0' encoding='UTF-8'?>
 */
LIBSBML_EXTERN
SBMLDocument_t *
SBMLReader_readSBMLFromString (SBMLReader_t *sr, const char *xml)
{
  return SBMLReader_readSBML_internal(sr, NULL, xml);
}


/**
 * Reads the SBML document from the given file and returns a pointer to it.
 * This convenience function is functionally equivalent to:
 *
 *   SBMLReader_readSBML(SBMLReader_create(), filename);
 */
LIBSBML_EXTERN
SBMLDocument_t *
readSBML (const char *filename)
{
  SBMLReader_t sr = {XML_SCHEMA_VALIDATION_NONE, NULL, NULL, NULL};


  return SBMLReader_readSBML(&sr, filename);
}


/**
 * Reads the SBML document from the given XML string and returns a pointer
 * to it.  This convenience function is functionally equivalent to:
 *
 *   SBMLReader_readSBMLFromString(SBMLReader_create(), filename);
 */
LIBSBML_EXTERN
SBMLDocument_t *
readSBMLFromString (const char *xml)
{
  SBMLReader_t sr = {XML_SCHEMA_VALIDATION_NONE, NULL, NULL, NULL};


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
  if (sr->schemaFilenameL1v1 == filename) return;


  if (sr->schemaFilenameL1v1 != NULL)
  {
    safe_free(sr->schemaFilenameL1v1);
  }


  sr->schemaFilenameL1v1 = (filename == NULL) ? NULL : safe_strdup(filename);
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
  if (sr->schemaFilenameL1v2 == filename) return;


  if (sr->schemaFilenameL1v2 != NULL)
  {
    safe_free(sr->schemaFilenameL1v2);
  }


  sr->schemaFilenameL1v2 = (filename == NULL) ? NULL : safe_strdup(filename);
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
  if (sr->schemaFilenameL2v1 == filename) return;


  if (sr->schemaFilenameL2v1 != NULL)
  {
    safe_free(sr->schemaFilenameL2v1);
  }


  sr->schemaFilenameL2v1 = (filename == NULL) ? NULL : safe_strdup(filename);
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
  sr->schemaValidationLevel = level;
}
