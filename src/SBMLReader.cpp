/**
 * Filename    : SBMLReader.cpp
 * Description : Reads an SBML Document into memory
 * Author(s)   : SBW Development Group <sysbio-team@caltech.edu>
 * Organization: Caltech ERATO Kitano Systems Biology Project
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
 *     The Systems Biology Workbench Development Group
 *     ERATO Kitano Systems Biology Project
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

#include "sbml/List.h"

#include <xercesc/framework/MemBufInputSource.hpp>
#include <xercesc/sax2/DefaultHandler.hpp>
#include <xercesc/sax2/SAX2XMLReader.hpp>
#include <xercesc/sax2/XMLReaderFactory.hpp>
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/util/XMLString.hpp>

#include "sbml/SBMLHandler.hpp"
#include "sbml/SBMLDocument.h"
#include "sbml/SBMLReader.h"


/**
 * These functions are "private", i.e. they are used only by other
 * functions in this file.
 */
ParseMessage_t* ParseMessage_createFrom (const XMLException& e);

SBMLDocument_t*
SBMLReader_readSBML_internal ( SBMLReader_t* sr,
                               const char*   filename,
                               const char*   xml );


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
  sr->schemaFilename        = NULL;

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


  safe_free(sr->schemaFilename);
  safe_free(sr);
}


/**
 * Sets the schema filename used by this SBMLReader.
 *
 * The filename should be either i) an absolute path or ii) relative to the
 * directory contain the SBML file(s) to be read.
 */
LIBSBML_EXTERN
void
SBMLReader_setSchemaFilename (SBMLReader_t *sr, const char *filename)
{
  if (sr->schemaFilename != NULL)
  {
    safe_free(sr->schemaFilename);
  }


  sr->schemaFilename = (filename == NULL) ? NULL : safe_strdup(filename);
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
  SBMLReader_t sr = {XML_SCHEMA_VALIDATION_NONE, NULL};


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
  SBMLReader_t sr = {XML_SCHEMA_VALIDATION_NONE, NULL};


  return SBMLReader_readSBMLFromString(&sr, xml);
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
  SBMLDocument_t *d = SBMLDocument_create();


  try
  {
    XMLPlatformUtils::Initialize();
  }
  catch (const XMLException& e)
  {
    List_add(d->fatal, ParseMessage_createFrom(e));
    return d;
  }


  SAX2XMLReader*     reader  = XMLReaderFactory::createXMLReader();
  DefaultHandler*    handler = new SBMLHandler(d);
  MemBufInputSource* input   = NULL;


  if (xml != NULL)
  {
    input = new MemBufInputSource( (const XMLByte*) xml,
                                   strlen(xml),
                                   "FromString",
                                   false );
  }

  reader->setFeature( XMLUni::fgSAX2CoreNameSpaces       , true );
  reader->setFeature( XMLUni::fgSAX2CoreNameSpacePrefixes, true );

  reader->setContentHandler(handler);
  reader->setErrorHandler(handler);

  //
  // XML Schema Validation (based on the state of the SBMLReader)
  //
  if (sr->schemaValidationLevel != XML_SCHEMA_VALIDATION_NONE)
  {
    reader->setFeature( XMLUni::fgSAX2CoreValidation, true );
    reader->setFeature( XMLUni::fgXercesSchema      , true );

    reader->setFeature(XMLUni::fgXercesSchemaFullChecking,
                       sr->schemaValidationLevel == XML_SCHEMA_VALIDATION_FULL);

    if (sr->schemaFilename != NULL)
    {
      char   xmlns[]  = "http://www.sbml.org/sbml/level2 ";
      char*  location = safe_strcat(xmlns, sr->schemaFilename);
      XMLCh* value    = XMLString::transcode(location);
      
      reader->setProperty(XMLUni::fgXercesSchemaExternalSchemaLocation, value);

      delete [] value;
      safe_free(location);
    }
  }


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
 * Creates a new ParseMessage from the given XMLException and returns a
 * pointer to it.
 */
ParseMessage_t*
ParseMessage_createFrom (const XMLException& e)
{
  char*           msg = XMLString::transcode( e.getMessage() );
  ParseMessage_t* pm;


  pm = ParseMessage_createWith(msg, e.getSrcLine(), 0);

  delete [] msg;

  return pm;
}
