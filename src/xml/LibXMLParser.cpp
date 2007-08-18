/**
 * @file    LibXMLParser.cpp
 * @brief   Adapts the LibXML XML parser to the XMLParser interface
 * @author  Ben Bornstein
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

#include <libxml/xmlerror.h>

#include <sbml/xml/XMLFileBuffer.h>
#include <sbml/xml/XMLMemoryBuffer.h>

#include <sbml/xml/LibXMLHandler.h>
#include <sbml/xml/LibXMLParser.h>


/** @cond doxygen-ignored */

using namespace std;

/** @endcond doxygen-ignored */


/** @cond doxygen-libsbml-internal */

static const int BUFFER_SIZE = 8192;

/*
 * Table mapping libXML error codes to ours.  The error code numbers are not
 * contiguous, hence the table has to map pairs of numbers rather than
 * simply being an array of codes.  The table is an array of vectors of
 * items [libxml code, our code], where `our code' is an error code
 * taken from the enumeration XMLParser::errorCodes.
 *
 * see /usr/include/libxml2/libxml/xmlerror.h
 * http://stuff.mit.edu/afs/sipb/project/php/share/gtk-doc/html/libxml2/libxml2-parser.html
 */ 
static struct libxmlErrors {
  const int           libxmlCode;
  enum XMLError::Code ourCode;
} libxmlErrorTable[] = {
  { XML_ERR_INTERNAL_ERROR, 	       XMLError::NotWellFormed},
  { XML_ERR_NO_MEMORY,		       XMLError::OutOfMemory},
  { XML_ERR_DOCUMENT_START,	       XMLError::NotWellFormed},
  { XML_ERR_DOCUMENT_EMPTY,	       XMLError::EmptyXML},
  { XML_ERR_DOCUMENT_END,	       XMLError::NotWellFormed},
  { XML_ERR_INVALID_HEX_CHARREF,       XMLError::InvalidConstruct},
  { XML_ERR_INVALID_DEC_CHARREF,       XMLError::InvalidConstruct},
  { XML_ERR_INVALID_CHARREF,	       XMLError::InvalidConstruct},
  { XML_ERR_INVALID_CHAR,	       XMLError::InvalidChar},
  { XML_ERR_CHARREF_AT_EOF,	       XMLError::NotWellFormed},
  { XML_ERR_CHARREF_IN_PROLOG,	       XMLError::NotWellFormed},
  { XML_ERR_CHARREF_IN_EPILOG,	       XMLError::NotWellFormed},
  { XML_ERR_CHARREF_IN_DTD,	       XMLError::NotWellFormed},
  { XML_ERR_UNDECLARED_ENTITY,         XMLError::UndefinedEntity},
  { XML_WAR_UNDECLARED_ENTITY,         XMLError::UndefinedEntity},
  { XML_ERR_UNKNOWN_ENCODING,	       XMLError::BadXMLDecl},
  { XML_ERR_UNSUPPORTED_ENCODING,      XMLError::BadXMLDecl},
  { XML_ERR_STRING_NOT_STARTED,        XMLError::NotWellFormed},
  { XML_ERR_STRING_NOT_CLOSED,	       XMLError::NotWellFormed},
  { XML_ERR_NS_DECL_ERROR,	       XMLError::NotWellFormed},
  { XML_ERR_LT_IN_ATTRIBUTE,	       XMLError::BadAttribute},
  { XML_ERR_ATTRIBUTE_NOT_STARTED,     XMLError::InternalParserError},
  { XML_ERR_ATTRIBUTE_NOT_FINISHED,    XMLError::NotWellFormed},
  { XML_ERR_ATTRIBUTE_WITHOUT_VALUE,   XMLError::MissingAttributeValue},
  { XML_ERR_ATTRIBUTE_REDEFINED,       XMLError::DuplicateAttribute},
  { XML_ERR_LITERAL_NOT_STARTED,       XMLError::InternalParserError},
  { XML_ERR_LITERAL_NOT_FINISHED,      XMLError::InternalParserError},
  { XML_ERR_COMMENT_NOT_FINISHED,      XMLError::BadXMLComment},
  { XML_ERR_PI_NOT_STARTED,	       XMLError::BadProcessingInstruction},
  { XML_ERR_PI_NOT_FINISHED,	       XMLError::BadProcessingInstruction},
  { XML_ERR_ATTLIST_NOT_STARTED,       XMLError::NotWellFormed},
  { XML_ERR_ATTLIST_NOT_FINISHED,      XMLError::NotWellFormed},
  { XML_ERR_MIXED_NOT_STARTED,	       XMLError::NotWellFormed},
  { XML_ERR_MIXED_NOT_FINISHED,	       XMLError::NotWellFormed},
  { XML_ERR_ELEMCONTENT_NOT_STARTED,   XMLError::NotWellFormed},
  { XML_ERR_ELEMCONTENT_NOT_FINISHED,  XMLError::NotWellFormed},
  { XML_ERR_XMLDECL_NOT_STARTED,       XMLError::BadXMLDecl},
  { XML_ERR_XMLDECL_NOT_FINISHED,      XMLError::BadXMLDecl},
  { XML_ERR_CONDSEC_NOT_STARTED,       XMLError::NotWellFormed},
  { XML_ERR_CONDSEC_NOT_FINISHED,      XMLError::NotWellFormed},
  { XML_ERR_EXT_SUBSET_NOT_FINISHED,   XMLError::NotWellFormed},
  { XML_ERR_DOCTYPE_NOT_FINISHED,      XMLError::BadDOCTYPE},
  { XML_ERR_MISPLACED_CDATA_END,       XMLError::NotWellFormed},
  { XML_ERR_CDATA_NOT_FINISHED,	       XMLError::NotWellFormed},
  { XML_ERR_RESERVED_XML_NAME,	       XMLError::BadXMLDeclLocation},
  { XML_ERR_SPACE_REQUIRED,	       XMLError::NotWellFormed},
  { XML_ERR_SEPARATOR_REQUIRED,	       XMLError::NotWellFormed},
  { XML_ERR_NMTOKEN_REQUIRED,	       XMLError::NotWellFormed},
  { XML_ERR_NAME_REQUIRED,	       XMLError::NotWellFormed},
  { XML_ERR_PCDATA_REQUIRED,	       XMLError::NotWellFormed},
  { XML_ERR_URI_REQUIRED,	       XMLError::NotWellFormed},
  { XML_ERR_PUBID_REQUIRED,	       XMLError::NotWellFormed},
  { XML_ERR_LT_REQUIRED,	       XMLError::NotWellFormed},
  { XML_ERR_GT_REQUIRED,	       XMLError::NotWellFormed},
  { XML_ERR_LTSLASH_REQUIRED,	       XMLError::NotWellFormed},
  { XML_ERR_EQUAL_REQUIRED,	       XMLError::NotWellFormed},
  { XML_ERR_TAG_NAME_MISMATCH,	       XMLError::TagMismatch},
  { XML_ERR_TAG_NOT_FINISHED,	       XMLError::NotWellFormed},
  { XML_ERR_STANDALONE_VALUE,	       XMLError::NotWellFormed},
  { XML_ERR_ENCODING_NAME,	       XMLError::BadXMLDecl},
  { XML_ERR_HYPHEN_IN_COMMENT,	       XMLError::BadXMLComment},
  { XML_ERR_INVALID_ENCODING,	       XMLError::BadXMLDecl},
  { XML_ERR_CONDSEC_INVALID,	       XMLError::NotWellFormed},
  { XML_ERR_VALUE_REQUIRED,	       XMLError::NotWellFormed},
  { XML_ERR_NOT_WELL_BALANCED,	       XMLError::NotWellFormed},
  { XML_ERR_EXTRA_CONTENT,	       XMLError::NotWellFormed},
  { XML_ERR_INVALID_URI,	       XMLError::NotWellFormed},
  { XML_ERR_URI_FRAGMENT,	       XMLError::NotWellFormed},
  { XML_WAR_CATALOG_PI,		       XMLError::NotWellFormed},
  { XML_ERR_NO_DTD,		       XMLError::NotWellFormed},
  { XML_ERR_CONDSEC_INVALID_KEYWORD,   XMLError::NotWellFormed},
  { XML_ERR_VERSION_MISSING,	       XMLError::BadXMLDecl},
  { XML_WAR_UNKNOWN_VERSION,	       XMLError::BadXMLDecl},
  { XML_WAR_LANG_VALUE,		       XMLError::NotWellFormed},
  { XML_WAR_NS_URI,		       XMLError::BadPrefix},
  { XML_WAR_NS_URI_RELATIVE,	       XMLError::BadPrefix},
  { XML_ERR_MISSING_ENCODING,	       XMLError::MissingXMLEncoding},
#if LIBXML_VERSION >= 20627
  { XML_WAR_SPACE_VALUE,	       XMLError::NotWellFormed},
  { XML_ERR_NOT_STANDALONE,	       XMLError::NotWellFormed},
  { XML_ERR_NOTATION_PROCESSING,       XMLError::NotWellFormed},
  { XML_WAR_NS_COLUMN,		       XMLError::NotWellFormed},
#endif
  { XML_NS_ERR_XML_NAMESPACE,	       XMLError::BadPrefixValue},
  { XML_NS_ERR_UNDEFINED_NAMESPACE,    XMLError::BadPrefix},
  { XML_NS_ERR_QNAME,		       XMLError::BadPrefix},
  { XML_NS_ERR_ATTRIBUTE_REDEFINED,    XMLError::NotWellFormed},
#if LIBXML_VERSION >= 20627
  { XML_NS_ERR_EMPTY,		       XMLError::NotWellFormed},
#endif
  // The next one should always be last.  It's used only as a marker.
  { 205,			       XMLError::UnknownError},
};


const enum XMLError::Code
translateError(const int libxmlCode)
{
  unsigned int tableSize = sizeof(libxmlErrorTable)/sizeof(libxmlErrorTable[0]);


  if (libxmlCode > 0 && libxmlCode < 205)
  {
    // Iterate through the table, searching for a match for the given code.
    // Yes, this is inefficient, but if we're already in an exception,
    // who cares how efficient the error look-up is?

    for (unsigned int i = 0; i < tableSize; i++)
      if (libxmlErrorTable[i].libxmlCode == libxmlCode)
	return libxmlErrorTable[i].ourCode;

    return XMLError::UnrecognizedParserCode;
  }

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
LibXMLParser::reportError (const XMLError::Code code,
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

    xmlErrorPtr libxmlError = xmlGetLastError();

    cerr << XMLError::getStandardMessage(code) << " at line and column numbers ";

    if (line != 0 || column != 0)
      cerr << line << ":" << column;
    else
      cerr << libxmlError->line << ":" << libxmlError->int2;

    cerr << ":\n" << extraMsg << endl;
  }
}


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

  if ( isFile )
  {
    mSource = new XMLFileBuffer(content);

    if ( mSource->error() )
    {
      reportError(XMLError::FileUnreadable, content, 0, 0);
      return false;
    }
  }
  else
  {
    mSource = new XMLMemoryBuffer(content, strlen(content));
  }

  if ( mSource == 0 )
  {
    reportError(XMLError::OutOfMemory, "", 0, 0);
    return false;
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
LibXMLParser::parseNext ()
{
  if ( error() ) return false;

  int bytes = mSource->copyTo(mBuffer, BUFFER_SIZE);
  int done  = (bytes == 0);

  if ( mSource->error() )
  {
    reportError(XMLError::InternalParserError,
		"error: Could not read from source buffer.");
    return false;
  }

  if ( xmlParseChunk(mParser, mBuffer, bytes, done) )
  {
    xmlErrorPtr libxmlError = xmlGetLastError();

    // I tried reporting the message from libXML that's available in
    // libxmlError->message, but the thing is bogus: it will say things
    // like "such and such error model line 0" which is wrong and
    // confusing.  So even though we lose some details by dropping the
    // libXML message, I think it's less confusing for the user.

    reportError(translateError(libxmlError->code), "",
		libxmlError->line, libxmlError->int2);
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
LibXMLParser::parseReset ()
{
  xmlCtxtResetPush(mParser, 0, 0, 0, 0);

  delete mSource;
  mSource = 0;
}


/** @endcond doxygen-libsbml-internal */
