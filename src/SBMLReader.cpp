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

#include "sbml/common.h"
#include "sbml/List.h"

#include <xercesc/framework/MemBufInputSource.hpp>
#include <xercesc/sax2/DefaultHandler.hpp>
#include <xercesc/sax2/SAX2XMLReader.hpp>
#include <xercesc/sax2/XMLReaderFactory.hpp>
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/util/XMLString.hpp>

#include "sbml/SAX2SBMLHandler.hpp"
#include "sbml/SBMLDocument.h"
#include "sbml/SBMLReader.h"


/**
 * Creates a new ParseMessage with the given message and returns a pointer
 * to it.
 */
ParseMessage_t*
createParseMessage (const char* msg)
{
  ParseMessage_t* pm;


  pm = (ParseMessage_t *) safe_calloc(1, sizeof(ParseMessage_t));

  pm->message = safe_strdup(msg);
  pm->line    = -1;
  pm->column  = -1;

  return pm;
}


/**
 * Creates a new ParseMessage from the given XMLException and returns a
 * pointer to it.
 */
ParseMessage_t*
createParseMessage (const XMLException& e)
{
  char*           msg = XMLString::transcode( e.getMessage() );
  ParseMessage_t* pm  = createParseMessage(msg);


  pm->line = (unsigned int) e.getSrcLine();

  delete [] msg;

  return pm;
}


/**
 * Creates a new ParseMessage from the given SAXException and returns a
 * pointer to it.
 */
ParseMessage_t*
createParseMessage (const SAXParseException& e)
{
  char*           msg = XMLString::transcode( e.getMessage() );
  ParseMessage_t* pm  = createParseMessage(msg);


  pm->line   = (unsigned int) e.getLineNumber();
  pm->column = (unsigned int) e.getColumnNumber();

  delete [] msg;

  return pm;
}


/**
 * Used by readSBML() and readSBMLFromString().
 */
SBMLDocument_t*
readSBML_internal (const char *filename, const char* xml)
{
  SBMLDocument_t *d = SBMLDocument_create();


  try
  {
    XMLPlatformUtils::Initialize();
  }
  catch (const XMLException& e)
  {
    List_add(d->fatal, createParseMessage(e));
    return d;
  }


  SAX2XMLReader*     reader  = XMLReaderFactory::createXMLReader();
  DefaultHandler*    handler = new SAX2SBMLHandler(d);
  MemBufInputSource* input   = NULL;


  if (xml != NULL)
  {
    input = new MemBufInputSource( (const XMLByte*) xml,
                                   strlen(xml),
                                   "FromString",
                                   false );
  }

  reader->setFeature(XMLUni::fgSAX2CoreNameSpaces,        true);
  reader->setFeature(XMLUni::fgSAX2CoreNameSpacePrefixes, true);

  reader->setContentHandler(handler);
  reader->setErrorHandler(handler);

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
    List_add(d->fatal, createParseMessage(e));
  }
  catch (const SAXParseException& e)
  {
    List_add(d->fatal, createParseMessage(e));
  }
  catch (...)
  {
    List_add(d->fatal, createParseMessage("Unexcepted Exception"));
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
SBMLDocument_t *
readSBML (const char *filename)
{
  return readSBML_internal(filename, NULL);
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
SBMLDocument_t *
readSBMLFromString (const char *xml)
{
  return readSBML_internal(NULL, xml);
}
