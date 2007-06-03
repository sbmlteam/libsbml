/**
 * @file    ExpatParser.cpp
 * @brief   Adapts the Expat XML parser to the XMLParser interface
 * @author  Ben Bornstein <ben.bornstein@jpl.nasa.gov>
 * @author  Michael Hucka <mhucka@caltech.edu>
 *
 * $Id$
 * $Source$
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2007 California Institute of Technology.
 * Copyright 2002-2005 California Institute of Technology and
 *                     Japan Science and Technology Corporation.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution and
 * also available online as http://sbml.org/software/libsbml/license.html
 *----------------------------------------------------------------------- -->*/

#include <iostream>

#include <sbml/xml/XMLFileBuffer.h>
#include <sbml/xml/XMLMemoryBuffer.h>
#include <sbml/xml/XMLErrorLog.h>

#include <sbml/xml/ExpatHandler.h>
#include <sbml/xml/ExpatParser.h>

/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */


/** @cond doxygen-libsbml-internal */

static const int BUFFER_SIZE = 8192;

/*
 * Expat's error messages are conveniently defined as a consecutive
 * sequence starting from 0.  This makes a translation table easy to
 * create.  The indexes into this table are the Expat codes, and the
 * values are our own error codes.
 */
static enum XMLError::Code expatErrorTable[] = {
  XMLError::UnknownError,             // XML_ERROR_NONE
  XMLError::OutOfMemory,              // XML_ERROR_NO_MEMORY
  XMLError::NotWellFormed,            // XML_ERROR_SYNTAX
  XMLError::NotWellFormed,            // XML_ERROR_NO_ELEMENTS
  XMLError::NotWellFormed,            // XML_ERROR_INVALID_TOKEN
  XMLError::UnclosedToken,            // XML_ERROR_UNCLOSED_TOKEN
  XMLError::InvalidChar,              // XML_ERROR_PARTIAL_CHAR
  XMLError::TagMismatch,              // XML_ERROR_TAG_MISMATCH
  XMLError::DuplicateAttribute,       // XML_ERROR_DUPLICATE_ATTRIBUTE
  XMLError::BadDOCTYPE,               // XML_ERROR_JUNK_AFTER_DOC_ELEMENT
  XMLError::UnknownError,             // XML_ERROR_PARAM_ENTITY_REF
  XMLError::UndefinedEntity,          // XML_ERROR_UNDEFINED_ENTITY
  XMLError::UndefinedEntity,          // XML_ERROR_RECURSIVE_ENTITY_REF
  XMLError::UndefinedEntity,          // XML_ERROR_ASYNC_ENTITY
  XMLError::InvalidChar,              // XML_ERROR_BAD_CHAR_REF
  XMLError::InvalidChar,              // XML_ERROR_BINARY_ENTITY_REF
  XMLError::UndefinedEntity,          // XML_ERROR_ATTRIBUTE_EXTERNAL_ENTITY_REF
  XMLError::BadXMLDeclLocation,       // XML_ERROR_MISPLACED_XML_PI
  XMLError::BadXMLDecl,               // XML_ERROR_UNKNOWN_ENCODING
  XMLError::BadXMLDecl,               // XML_ERROR_INCORRECT_ENCODING
  XMLError::NotWellFormed,            // XML_ERROR_UNCLOSED_CDATA_SECTION
  XMLError::InvalidConstruct,         // XML_ERROR_EXTERNAL_ENTITY_HANDLING
  XMLError::BadXMLDecl,               // XML_ERROR_NOT_STANDALONE
  XMLError::UnknownError,             // XML_ERROR_UNEXPECTED_STATE
  XMLError::UnknownError,             // XML_ERROR_ENTITY_DECLARED_IN_PE
  XMLError::InvalidConstruct,         // XML_ERROR_FEATURE_REQUIRES_XML_DTD
  XMLError::InvalidConstruct,         // XML_ERROR_CANT_CHANGE_FEATURE_ONCE_PARSING
  XMLError::BadPrefix,                // XML_ERROR_UNBOUND_PREFIX
  XMLError::BadPrefix,                // XML_ERROR_UNDECLARING_PREFIX
  XMLError::NotWellFormed,            // XML_ERROR_INCOMPLETE_PE
  XMLError::BadXMLDecl,               // XML_ERROR_XML_DECL
  XMLError::UnknownError,             // XML_ERROR_TEXT_DECL
  XMLError::UnknownError,             // XML_ERROR_PUBLICID
  XMLError::UnknownError,             // XML_ERROR_SUSPENDED
  XMLError::UnknownError,             // XML_ERROR_NOT_SUSPENDED
  XMLError::UnknownError,             // XML_ERROR_ABORTED
  XMLError::UnknownError,             // XML_ERROR_FINISHED
  XMLError::UnknownError              // XML_ERROR_SUSPEND_PE
};


const enum XMLError::Code
translateError(const int expatCode)
{
  int numTableEntries = sizeof(expatErrorTable)/sizeof(expatErrorTable[0]);

  if (expatCode > 0 && expatCode < numTableEntries)
    return expatErrorTable[expatCode];
  else
    return XMLError::UnknownError;
}


/*
 * Note that the given error code is an XMLError code, not a code
 * number returned by the underlying parser.  Codes returned by the
 * parser must be translated first.
 *
 * @see translateError().
 */
void
ExpatParser::reportError (const XMLError::Code code,
			  const string& extraMsg,
			  const unsigned int line,
			  const unsigned int column)
{
  if (mErrorLog)
    mErrorLog->add(XMLError( code, extraMsg, line, column) );
  else
  {
    // We have no error log, but we shouldn't gloss over this error.  Use
    // the measure of last resort.

    cerr << XMLError::getStandardMessage(code)
	 << " at line and column numbers " << line << ":" << column << ":\n"
	 << extraMsg << endl;
  }
}


/**
 * Creates a new ExpatParser given an XMLHandler object.
 *
 * The parser will notify the given XMLHandler of parse events and errors.
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
 * Returns true or false depending on whether the last operation
 * caused the underlying parser to generate an error.  Errors may
 * result from out-of-memory conditions as well.  This is called
 * by methods such as @c parse() and @c parseNext().
 * 
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
 * @return the column position of the current parser's location
 * in the XML input stream.
 */
unsigned int
ExpatParser::getColumn () const
{
  return mHandler.getColumn();
}


/**
 * @return the line position of the current parser's location
 * in the XML input stream.
 */
unsigned int
ExpatParser::getLine () const
{
  return mHandler.getLine();
}


/**
 * Parses XML content in one fell swoop.
 *
 * If \p isFile whoa is true (default), \p content is treated as a filename from
 * which to read the XML content.  Otherwise, \p content is treated as a
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
      reportError(XMLError::FileUnreadable, content, getLine(), getColumn());
      return false;
    }
  }
  else
  {
    mSource = new XMLMemoryBuffer(content, strlen(content));

    if (mSource == 0) reportError( XMLError::OutOfMemory, "", 0, 0 );
  }

  if ( !mSource->error() )
  {
    mHandler.startDocument();
    return true;
  }
  else
  {
    return false;
  }
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

  if ( mBuffer == 0 )
  {
    // See if Expat logged an error.  There are only two things that
    // XML_GetErrorCode will report: parser state errors and "out of memory".
    // So we check for the first and default to the out-of-memory case.

    switch ( XML_GetErrorCode(mParser) )
    {
    case XML_ERROR_SUSPENDED:
    case XML_ERROR_FINISHED:
      reportError(XMLError::InternalParserError);
      break;

    default:
      reportError(XMLError::OutOfMemory);
      break;
    }

    return false;
  }

  int bytes = mSource->copyTo(mBuffer, BUFFER_SIZE);
  int done  = (bytes == 0);

  // Attempt to parse the content, checking for the Expat return status.

  if ( XML_ParseBuffer(mParser, bytes, done) == XML_STATUS_ERROR )
  {
    reportError(translateError(XML_GetErrorCode(mParser)), "",
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


/** @endcond doxygen-libsbml-internal */
