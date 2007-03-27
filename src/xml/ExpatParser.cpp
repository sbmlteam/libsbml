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

#include <sbml/xml/XMLFileBuffer.h>
#include <sbml/xml/XMLMemoryBuffer.h>
#include <sbml/xml/XMLErrorLog.h>

#include <sbml/xml/ExpatHandler.h>
#include <sbml/xml/ExpatParser.h>


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

  return true;
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
    cerr << "error: Could not read from source buffer." << endl;
    return false;
  }

  // Attempt to parse the content, checking for the Expat return status.

  if ( XML_ParseBuffer(mParser, bytes, done) == XML_STATUS_ERROR )
  {
    reportError(XML_GetErrorCode(mParser),
		XML_GetCurrentLineNumber(mParser),
		XML_GetCurrentColumnNumber(mParser));
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


/*
 * Expat's error messages are conveniently defined as a consecutive
 * sequence starting from 0.  This makes a translation table easy to
 * create.  The indexes into this table are the Expat codes, and the
 * values are our own error codes.
 */
static enum XMLParser::errorCodes expatErrorTable[] = {
  XMLParser::NoError,                       // XML_ERROR_NONE
  XMLParser::ErrorOutOfMemory,              // XML_ERROR_NO_MEMORY
  XMLParser::ErrorNotWellFormed,            // XML_ERROR_SYNTAX
  XMLParser::ErrorNotWellFormed,            // XML_ERROR_NO_ELEMENTS
  XMLParser::ErrorNotWellFormed,            // XML_ERROR_INVALID_TOKEN
  XMLParser::ErrorUnclosedToken,            // XML_ERROR_UNCLOSED_TOKEN
  XMLParser::ErrorInvalidChar,              // XML_ERROR_PARTIAL_CHAR
  XMLParser::ErrorTagMismatch,              // XML_ERROR_TAG_MISMATCH
  XMLParser::ErrorDupAttribute,             // XML_ERROR_DUPLICATE_ATTRIBUTE
  XMLParser::ErrorBadDOCTYPE,               // XML_ERROR_JUNK_AFTER_DOC_ELEMENT
  XMLParser::UnknownError,                  // XML_ERROR_PARAM_ENTITY_REF
  XMLParser::ErrorUndefinedEntity,          // XML_ERROR_UNDEFINED_ENTITY
  XMLParser::ErrorUndefinedEntity,          // XML_ERROR_RECURSIVE_ENTITY_REF
  XMLParser::ErrorUndefinedEntity,          // XML_ERROR_ASYNC_ENTITY
  XMLParser::ErrorInvalidChar,              // XML_ERROR_BAD_CHAR_REF
  XMLParser::ErrorInvalidChar,              // XML_ERROR_BINARY_ENTITY_REF
  XMLParser::ErrorUndefinedEntity,          // XML_ERROR_ATTRIBUTE_EXTERNAL_ENTITY_REF
  XMLParser::ErrorBadProcessingInstruction, // XML_ERROR_MISPLACED_XML_PI
  XMLParser::ErrorBadXMLDecl,               // XML_ERROR_UNKNOWN_ENCODING
  XMLParser::ErrorBadXMLDecl,               // XML_ERROR_INCORRECT_ENCODING
  XMLParser::ErrorNotWellFormed,            // XML_ERROR_UNCLOSED_CDATA_SECTION
  XMLParser::ErrorInvalidConstruct,         // XML_ERROR_EXTERNAL_ENTITY_HANDLING
  XMLParser::ErrorBadXMLDecl,               // XML_ERROR_NOT_STANDALONE
  XMLParser::UnknownError,                  // XML_ERROR_UNEXPECTED_STATE
  XMLParser::UnknownError,                  // XML_ERROR_ENTITY_DECLARED_IN_PE
  XMLParser::ErrorInvalidConstruct,         // XML_ERROR_FEATURE_REQUIRES_XML_DTD
  XMLParser::ErrorInvalidConstruct,         // XML_ERROR_CANT_CHANGE_FEATURE_ONCE_PARSING
  XMLParser::ErrorBadPrefixDefinition,      // XML_ERROR_UNBOUND_PREFIX
  XMLParser::ErrorBadPrefixValue,           // XML_ERROR_UNDECLARING_PREFIX
  XMLParser::ErrorBadProcessingInstruction, // XML_ERROR_INCOMPLETE_PE
  XMLParser::ErrorBadXMLDecl,               // XML_ERROR_XML_DECL
  XMLParser::UnknownError,                  // XML_ERROR_TEXT_DECL
  XMLParser::UnknownError,                  // XML_ERROR_PUBLICID
  XMLParser::UnknownError,                  // XML_ERROR_SUSPENDED
  XMLParser::UnknownError,                  // XML_ERROR_NOT_SUSPENDED
  XMLParser::UnknownError,                  // XML_ERROR_ABORTED
  XMLParser::UnknownError,                  // XML_ERROR_FINISHED
  XMLParser::UnknownError                   // XML_ERROR_SUSPEND_PE
};


void
ExpatParser::reportError (const int expatCode,
			  const unsigned int lineNumber,
			  const unsigned int columnNumber)
{
  int numTableEntries = sizeof(expatErrorTable)/sizeof(expatErrorTable[0]);

  if (expatCode > 0 && expatCode < numTableEntries)
  {
    enum XMLParser::errorCodes code = expatErrorTable[expatCode];

    if (mErrorLog)
    {
      getErrorLog()->add(XMLError(code,
				  XMLParser::getErrorMessage(code),
				  XMLError::Error,
				  "", 
				  lineNumber,
				  columnNumber));
    }
    else
    {
      // We have no error log, but we can't gloss over this error.  Use the
      // measure of last resort.

      cerr << "XML parsing error at line and column numbers " 
	   << lineNumber << ":" << columnNumber << ":\n"
	   << XMLParser::getErrorMessage(code)
	   << endl;
    }
  }
  else
  {
    // The given code doesn't correspond to any known Expat error code.
    // This must mean something is wrong with our code.

    if (mErrorLog)
    {
      getErrorLog()->add(
          XMLError(XMLParser::UnknownError,
		   XMLParser::getErrorMessage(XMLParser::UnknownError),
		   XMLError::Error,
		   "",
		   lineNumber,
		   columnNumber));
    }
    else
    {
      cerr << "Internal error while parsing XML at line and column numbers "
	   << lineNumber << ":" << columnNumber << ":\n"
	   << XMLParser::getErrorMessage(XMLParser::UnknownError)
	   << endl;
    }
  }
}
