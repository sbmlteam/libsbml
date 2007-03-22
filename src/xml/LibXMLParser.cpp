/**
 * \file    LibXMLParser.cpp
 * \brief   Adapts the LibXML XML parser to the XMLParser interface
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

#include <sbml/xml/XMLFileBuffer.h>
#include <sbml/xml/XMLMemoryBuffer.h>

#include <sbml/xml/LibXMLHandler.h>
#include <sbml/xml/LibXMLParser.h>


using namespace std;


static const int BUFFER_SIZE = 8192;




/**
 * Creates a new LibXMLParser.  The parser will notify the given XMLHandler
 * of parse events and errors.
 */
LibXMLParser::LibXMLParser (XMLHandler& handler) :
   mParser ( 0                     )
 , mHandler( handler               )
 , mBuffer ( new char[BUFFER_SIZE] )
 , mSource ( 0 )
{
  xmlSAXHandler* sax  = LibXMLHandler::getInternalHandler();
  void*          data = static_cast<void*>(&mHandler);
  mParser             = xmlCreatePushParserCtxt(sax, data, 0, 0, 0);

  mHandler.setContext(mParser);
}


/**
 * Destroys this LibXMLParser.
 */
LibXMLParser::~LibXMLParser ()
{
  xmlFreeParserCtxt(mParser);
  delete [] mBuffer;
  delete    mSource;
}


/**
 * @return true if the parser encountered an error, false otherwise.
 */
bool
LibXMLParser::error () const
{
  bool error = (mParser == 0 || mBuffer == 0);

  if (mSource) error = error || mSource->error();
  return error;
}


/**
 * @return the current column position of the parser.
 */
unsigned int
LibXMLParser::getColumn () const
{
  return mHandler.getColumn();
}


/**
 * @return the current line position of the parser.
 */
unsigned int
LibXMLParser::getLine () const
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
LibXMLParser::parse (const char* content, bool isFile)
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
 * Xerces and libXML chunks correspond to XML elements.  For Expat, a chunk
 * is the size of its internal buffer.
 *
 * If isFile is true (default), content is treated as a filename from which
 * to read the XML content.  Otherwise, content is treated as a buffer
 * containing XML data and is read directly.
 *
 * @return true if the first step of the progressive parse was
 * successful, false otherwise.
 */
bool
LibXMLParser::parseFirst (const char* content, bool isFile)
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

  return parseNext();
}


/**
 * Parses the next chunk of XML content.
 *
 * @return true if the next step of the progressive parse was successful,
 * false otherwise or when at EOF.
 */
bool
LibXMLParser::parseNext ()
{
  if ( error() ) return false;

  int bytes = mSource->copyTo(mBuffer, BUFFER_SIZE);
  int done  = (bytes == 0);


  if ( mSource->error() )
  {
    cerr << "error: Could not read from source buffer." << endl;
    return false;
  }

  if ( xmlParseChunk(mParser, mBuffer, bytes, done) )
  {
    cerr << "libXML parse error." << endl;
    return false;
  }

  return !done;
}


/**
 * Resets the progressive parser.  Call between the last call to
 * parseNext() and the next call to parseFirst().
 */
void
LibXMLParser::parseReset ()
{
  xmlCtxtResetPush(mParser, 0, 0, 0, 0);

  delete mSource;
  mSource = 0;
}
