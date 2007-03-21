/**
 * \file    ExpatParser.cpp
 * \brief   Adapts the Expat XML parser to the XMLParser interface
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2006 California Institute of Technology and Japan Science and
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


#include <iostream>

#include "XMLFileBuffer.h"
#include "XMLMemoryBuffer.h"
#include "XMLErrorLog.h"

#include "ExpatHandler.h"
#include "ExpatParser.h"


using namespace std;


static const int BUFFER_SIZE = 8192;


/**
 * Creates a new ExpatParser.  The parser will notify the given XMLHandler
 * of parse events and errors.
 */
ExpatParser::ExpatParser (XMLHandler& handler) :
   mParser ( XML_ParserCreateNS(0, ' ') )
 , mHandler( mParser, handler )
 , mBuffer ( 0 )
 , mSource ( 0 )
{
  if (mParser) mBuffer = XML_GetBuffer(mParser, BUFFER_SIZE);
}


/**
 * Destroys this ExpatParser.
 */
ExpatParser::~ExpatParser ()
{
  XML_ParserFree(mParser);
  delete mSource;
}


/**
 * @return true if the parser encountered an error, false otherwise.
 */
bool
ExpatParser::error () const
{
  bool error = (mParser == 0 || mBuffer == 0);

  if (mSource) error = error || mSource->error();
  return error;
}


/**
 * @return the current column position of the parser.
 */
unsigned int
ExpatParser::getColumn () const
{
  return mHandler.getColumn();
}


/**
 * @return the current line position of the parser.
 */
unsigned int
ExpatParser::getLine () const
{
  return mHandler.getLine();
}


/**
 * Parses XML content in one fell swoop.
 *
 * If isFile is true (default), content is treated as a filename from
 * which to read the XML content.  Otherwise, content is treated as a
 * null-terminated buffer containing XML data and is read directly.
 *
 * @return true if the parse was successful, false otherwise.
 */
bool
ExpatParser::parse (const char* content, bool isFile)
{
  bool result = parseFirst(content, isFile);

  if (result)
  {
    while( parseNext() );
    result = (error() == false);
  }

  parseReset();

  return result;
}


/**
 * Begins a progressive parse of XML content.  This parses the first
 * chunk of the XML content and returns.  Successive chunks are parsed by
 * calling parseNext().
 *
 * A chunk differs slightly depending on the underlying XML parser.  For
 * Xerces and libXML chunks correspond to XML elements.  For Expat, a
 * chunk is the size of its internal buffer.
 *
 * If isFile is true (default), content is treated as a filename from which
 * to read the XML content.  Otherwise, content is treated as a buffer
 * containing XML data and is read directly.
 *
 * @return true if the first step of the progressive parse was
 * successful, false otherwise.
 */
bool
ExpatParser::parseFirst (const char* content, bool isFile)
{
  if ( error() ) return false;


  if (isFile)
  {
    mSource = new XMLFileBuffer(content);

    if (mSource->error())
    {
      cerr << "error: Could not open filename '" << content << "' for reading."
           << endl;
    }
  }
  else
  {
    mSource = new XMLMemoryBuffer(content, strlen(content));
  }

  if ( !error() )
  {
    mHandler.startDocument();
  }

  return parseNext();
}


/**
 * Parses the next chunk of XML content.
 *
 * @return true if the next step of the progressive parse was successful,
 * false otherwise or when at EOF.
 */
bool
ExpatParser::parseNext ()
{
  if ( error() ) return false;

  mBuffer = XML_GetBuffer(mParser, BUFFER_SIZE);

  int bytes = mSource->copyTo(mBuffer, BUFFER_SIZE);
  int done  = (bytes == 0);

  if ( mSource->error() )
  {
    fprintf(stderr, "error: Could not read from source buffer.\n");
    return false;
  }
  if ( XML_ParseBuffer(mParser, bytes, done) == XML_STATUS_ERROR )
  {
    if (mErrorLog)
    {
      /* rather than report the error and continue we catch errors
       * on the second read and add them to the error log
       
      fprintf( stderr, "XML parse error at line %i:\n%s\n",
               XML_GetCurrentLineNumber(mParser),
               XML_ErrorString(XML_GetErrorCode(mParser)) );
      */
      getErrorLog()->add( XMLError(XML_GetErrorCode(mParser), 
        XML_ErrorString(XML_GetErrorCode(mParser)), 
        XMLError::Error, "", XML_GetCurrentLineNumber(mParser), 
        XML_GetCurrentColumnNumber(mParser)));
    }
    return false;
  }

  if ( !error() && done )
  {
    mHandler.endDocument();
  }

  return !done;
}


/**
 * Resets the progressive parser.  Call between the last call to
 * parseNext() and the next call to parseFirst().
 */
void
ExpatParser::parseReset ()
{
  delete mSource;
  mSource = 0;
}
