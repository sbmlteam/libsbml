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
    cerr << "error: Could not read from source buffer." << endl;
    return false;
  }

  if ( xmlParseChunk(mParser, mBuffer, bytes, done) )
  {
    reportError(((*mParser).lastError).code ,
      ((*mParser).lastError).line,((*mParser).lastError).int2);
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

/*
 * Table mapping libXML error codes to ours.  The error code numbers are not
 * contiguous, hence the table has to map pairs of numbers rather than
 * simply being an array of codes.  The table is an array of vectors of
 * items [libxml code, our code], where `our code' is an error code
 * taken from the enumeration XMLParser::errorCodes.
 *
 * See  /include/xercesc/framework/XMLErrorCodes.hpp
 * and /src/xerces-c-src_2_7_0/src/xercesc/NLS/EN_US/XMLErrList_EN_US.Xml
 */ 
static struct libxmlErrors {
  const int                  libxmlCode;
  enum XMLParser::errorCodes ourCode;
} libxmlErrorTable[] = {
/* XML_ERR_INTERNAL_ERROR */            { 1, XMLParser::ErrorNotWellFormed},
/* XML_ERR_NO_MEMORY */                 { 2, XMLParser::ErrorNotWellFormed},
/* XML_ERR_DOCUMENT_START */            { 3, XMLParser::ErrorNotWellFormed},
/* XML_ERR_DOCUMENT_EMPTY */            { 4, XMLParser::ErrorNotWellFormed},
/* XML_ERR_DOCUMENT_END */              { 5, XMLParser::ErrorNotWellFormed},
/* XML_ERR_INVALID_HEX_CHARREF */       { 6, XMLParser::ErrorNotWellFormed},
/* XML_ERR_INVALID_DEC_CHARREF */       { 7, XMLParser::ErrorNotWellFormed},
/* XML_ERR_INVALID_CHARREF */           { 8, XMLParser::ErrorNotWellFormed},
/* XML_ERR_INVALID_CHAR */              { 9, XMLParser::ErrorNotWellFormed},
/* XML_ERR_CHARREF_AT_EOF */            { 10, XMLParser::ErrorNotWellFormed},
/* XML_ERR_CHARREF_IN_PROLOG */         { 11, XMLParser::ErrorNotWellFormed},
/* XML_ERR_CHARREF_IN_EPILOG */         { 12, XMLParser::ErrorNotWellFormed},
/* XML_ERR_CHARREF_IN_DTD */            { 13, XMLParser::ErrorNotWellFormed},
/* XML_ERR_ENTITYREF_AT_EOF */          { 14, XMLParser::ErrorNotWellFormed},
/* XML_ERR_ENTITYREF_IN_PROLOG */       { 15, XMLParser::ErrorNotWellFormed},
/* XML_ERR_ENTITYREF_IN_EPILOG */       { 16, XMLParser::ErrorNotWellFormed},
/* XML_ERR_ENTITYREF_IN_DTD */          { 17, XMLParser::ErrorNotWellFormed},
/* XML_ERR_PEREF_AT_EOF */              { 18, XMLParser::ErrorNotWellFormed},
/* XML_ERR_PEREF_IN_PROLOG */           { 19, XMLParser::ErrorNotWellFormed},
/* XML_ERR_PEREF_IN_EPILOG */           { 20, XMLParser::ErrorNotWellFormed},
/* XML_ERR_PEREF_IN_INT_SUBSET */       { 21, XMLParser::ErrorNotWellFormed},
/* XML_ERR_ENTITYREF_NO_NAME */         { 22, XMLParser::ErrorNotWellFormed},
/* XML_ERR_ENTITYREF_SEMICOL_MISSING */ { 23, XMLParser::ErrorNotWellFormed},
/* XML_ERR_PEREF_NO_NAME */             { 24, XMLParser::ErrorNotWellFormed},
/* XML_ERR_PEREF_SEMICOL_MISSING */     { 25, XMLParser::ErrorNotWellFormed},
/* XML_ERR_UNDECLARED_ENTITY */         { 26, XMLParser::ErrorBadProcessingInstruction},
/* XML_WAR_UNDECLARED_ENTITY */         { 27, XMLParser::ErrorNotWellFormed},
/* XML_ERR_UNPARSED_ENTITY */           { 28, XMLParser::ErrorNotWellFormed},
/* XML_ERR_ENTITY_IS_EXTERNAL */        { 29, XMLParser::ErrorNotWellFormed},
/* XML_ERR_ENTITY_IS_PARAMETER */       { 30, XMLParser::ErrorNotWellFormed},
/* XML_ERR_UNKNOWN_ENCODING */          { 31, XMLParser::ErrorNotWellFormed},
/* XML_ERR_UNSUPPORTED_ENCODING */      { 32, XMLParser::ErrorNotWellFormed},
/* XML_ERR_STRING_NOT_STARTED */        { 33, XMLParser::ErrorNotWellFormed},
/* XML_ERR_STRING_NOT_CLOSED */         { 34, XMLParser::ErrorNotWellFormed},
/* XML_ERR_NS_DECL_ERROR */             { 35, XMLParser::ErrorNotWellFormed},
/* XML_ERR_ENTITY_NOT_STARTED */        { 36, XMLParser::ErrorNotWellFormed},
/* XML_ERR_ENTITY_NOT_FINISHED */       { 37, XMLParser::ErrorNotWellFormed},
/* XML_ERR_LT_IN_ATTRIBUTE */           { 38, XMLParser::ErrorNotWellFormed},
/* XML_ERR_ATTRIBUTE_NOT_STARTED */     { 39, XMLParser::ErrorNotWellFormed},
/* XML_ERR_ATTRIBUTE_NOT_FINISHED */    { 40, XMLParser::ErrorNotWellFormed},
/* XML_ERR_ATTRIBUTE_WITHOUT_VALUE */   { 41, XMLParser::ErrorNotWellFormed},
/* XML_ERR_ATTRIBUTE_REDEFINED */       { 42, XMLParser::ErrorDupAttribute},
/* XML_ERR_LITERAL_NOT_STARTED */       { 43, XMLParser::ErrorNotWellFormed},
/* XML_ERR_LITERAL_NOT_FINISHED */      { 44, XMLParser::ErrorNotWellFormed},
/* XML_ERR_COMMENT_NOT_FINISHED */      { 45, XMLParser::ErrorNotWellFormed},
/* XML_ERR_PI_NOT_STARTED */            { 46, XMLParser::ErrorNotWellFormed},
/* XML_ERR_PI_NOT_FINISHED */           { 47, XMLParser::ErrorNotWellFormed},
/* XML_ERR_NOTATION_NOT_STARTED */      { 48, XMLParser::ErrorNotWellFormed},
/* XML_ERR_NOTATION_NOT_FINISHED */     { 49, XMLParser::ErrorNotWellFormed},
/* XML_ERR_ATTLIST_NOT_STARTED */       { 50, XMLParser::ErrorNotWellFormed},
/* XML_ERR_ATTLIST_NOT_FINISHED */      { 51, XMLParser::ErrorNotWellFormed},
/* XML_ERR_MIXED_NOT_STARTED */         { 52, XMLParser::ErrorNotWellFormed},
/* XML_ERR_MIXED_NOT_FINISHED */        { 53, XMLParser::ErrorNotWellFormed},
/* XML_ERR_ELEMCONTENT_NOT_STARTED */   { 54, XMLParser::ErrorNotWellFormed},
/* XML_ERR_ELEMCONTENT_NOT_FINISHED */  { 55, XMLParser::ErrorNotWellFormed},
/* XML_ERR_XMLDECL_NOT_STARTED */       { 56, XMLParser::ErrorNotWellFormed},
/* XML_ERR_XMLDECL_NOT_FINISHED */      { 57, XMLParser::ErrorNotWellFormed},
/* XML_ERR_CONDSEC_NOT_STARTED */       { 58, XMLParser::ErrorNotWellFormed},
/* XML_ERR_CONDSEC_NOT_FINISHED */      { 59, XMLParser::ErrorNotWellFormed},
/* XML_ERR_EXT_SUBSET_NOT_FINISHED */   { 60, XMLParser::ErrorNotWellFormed},
/* XML_ERR_DOCTYPE_NOT_FINISHED */      { 61, XMLParser::ErrorNotWellFormed},
/* XML_ERR_MISPLACED_CDATA_END */       { 62, XMLParser::ErrorNotWellFormed},
/* XML_ERR_CDATA_NOT_FINISHED */        { 63, XMLParser::ErrorNotWellFormed},
/* XML_ERR_RESERVED_XML_NAME */         { 64, XMLParser::ErrorBadProcessingInstruction},
/* XML_ERR_SPACE_REQUIRED */            { 65, XMLParser::ErrorNotWellFormed},
/* XML_ERR_SEPARATOR_REQUIRED */        { 66, XMLParser::ErrorNotWellFormed},
/* XML_ERR_NMTOKEN_REQUIRED */          { 67, XMLParser::ErrorNotWellFormed},
/* XML_ERR_NAME_REQUIRED */             { 68, XMLParser::ErrorNotWellFormed},
/* XML_ERR_PCDATA_REQUIRED */           { 69, XMLParser::ErrorNotWellFormed},
/* XML_ERR_URI_REQUIRED */              { 70, XMLParser::ErrorNotWellFormed},
/* XML_ERR_PUBID_REQUIRED */            { 71, XMLParser::ErrorNotWellFormed},
/* XML_ERR_LT_REQUIRED */               { 72, XMLParser::ErrorNotWellFormed},
/* XML_ERR_GT_REQUIRED */               { 73, XMLParser::ErrorNotWellFormed},
/* XML_ERR_LTSLASH_REQUIRED */          { 74, XMLParser::ErrorNotWellFormed},
/* XML_ERR_EQUAL_REQUIRED */            { 75, XMLParser::ErrorNotWellFormed},
/* XML_ERR_TAG_NAME_MISMATCH */         { 76, XMLParser::ErrorBadPrefixDefinition},
/* XML_ERR_TAG_NOT_FINISHED */          { 77, XMLParser::ErrorNotWellFormed},
/* XML_ERR_STANDALONE_VALUE */          { 78, XMLParser::ErrorNotWellFormed},
/* XML_ERR_ENCODING_NAME */             { 79, XMLParser::ErrorNotWellFormed},
/* XML_ERR_HYPHEN_IN_COMMENT */         { 80, XMLParser::ErrorNotWellFormed},
/* XML_ERR_INVALID_ENCODING */          { 81, XMLParser::ErrorNotWellFormed},
/* XML_ERR_EXT_ENTITY_STANDALONE */     { 82, XMLParser::ErrorNotWellFormed},
/* XML_ERR_CONDSEC_INVALID */           { 83, XMLParser::ErrorNotWellFormed},
/* XML_ERR_VALUE_REQUIRED */            { 84, XMLParser::ErrorNotWellFormed},
/* XML_ERR_NOT_WELL_BALANCED */         { 85, XMLParser::ErrorNotWellFormed},
/* XML_ERR_EXTRA_CONTENT */             { 86, XMLParser::ErrorNotWellFormed},
/* XML_ERR_ENTITY_CHAR_ERROR */         { 87, XMLParser::ErrorNotWellFormed},
/* XML_ERR_ENTITY_PE_INTERNAL */        { 88, XMLParser::ErrorNotWellFormed},
/* XML_ERR_ENTITY_LOOP */               { 89, XMLParser::ErrorNotWellFormed},
/* XML_ERR_ENTITY_BOUNDARY */           { 90, XMLParser::ErrorNotWellFormed},
/* XML_ERR_INVALID_URI */               { 91, XMLParser::ErrorNotWellFormed},
/* XML_ERR_URI_FRAGMENT */              { 92, XMLParser::ErrorNotWellFormed},
/* XML_WAR_CATALOG_PI */                { 93, XMLParser::ErrorNotWellFormed},
/* XML_ERR_NO_DTD */                    { 94, XMLParser::ErrorNotWellFormed},
/* XML_ERR_CONDSEC_INVALID_KEYWORD */   { 95, XMLParser::ErrorNotWellFormed},
/* XML_ERR_VERSION_MISSING */           { 96, XMLParser::ErrorNotWellFormed},
/* XML_WAR_UNKNOWN_VERSION */           { 97, XMLParser::ErrorNotWellFormed},
/* XML_WAR_LANG_VALUE */                { 98, XMLParser::ErrorNotWellFormed},
/* XML_WAR_NS_URI */                    { 99, XMLParser::ErrorNotWellFormed},
/* XML_WAR_NS_URI_RELATIVE */           { 100, XMLParser::ErrorNotWellFormed},
/* XML_ERR_MISSING_ENCODING */          { 101, XMLParser::ErrorNotWellFormed},
/* XML_WAR_SPACE_VALUE */               { 102, XMLParser::ErrorNotWellFormed},
/* XML_ERR_NOT_STANDALONE */            { 103, XMLParser::ErrorNotWellFormed},
/* XML_ERR_ENTITY_PROCESSING */         { 104, XMLParser::ErrorNotWellFormed},
/* XML_ERR_NOTATION_PROCESSING */       { 105, XMLParser::ErrorNotWellFormed},
/* XML_WAR_NS_COLUMN */                 { 106, XMLParser::ErrorNotWellFormed},
/* XML_WAR_ENTITY_REDEFINED */          { 107, XMLParser::ErrorNotWellFormed},
/* XML_NS_ERR_XML_NAMESPACE */          { 200, XMLParser::ErrorNotWellFormed},
/* XML_NS_ERR_UNDEFINED_NAMESPACE */    { 201, XMLParser::ErrorBadPrefixDefinition},
/* XML_NS_ERR_QNAME */                  { 202, XMLParser::ErrorNotWellFormed},
/* XML_NS_ERR_ATTRIBUTE_REDEFINED */    { 203, XMLParser::ErrorNotWellFormed},
/* XML_NS_ERR_EMPTY */                  { 204, XMLParser::ErrorNotWellFormed},
  // The next one should always be last.  It's used only as a marker.
                                        { 205, XMLParser::UnknownError},
};

void
LibXMLParser::reportError (const int code,
			   const unsigned int lineNumber,
			   const unsigned int columnNumber)
{
  unsigned int tableSize = sizeof(libxmlErrorTable)/sizeof(libxmlErrorTable[0]);
  xmlErrorPtr libxmlError = xmlGetLastError();


  if (code > 0 && code < 205)
  {
    // Iterate through the table, searching for a match for the given code.
    // Yes, this is inefficient, but if we're already in an exception,
    // who cares how efficient the error look-up is?

    for (unsigned int i = 0; i < tableSize; i++)
    {
      if (libxmlErrorTable[i].libxmlCode == code)
      {
        if (mErrorLog)
        {
	        getErrorLog()->add( 
	            XMLError(libxmlErrorTable[i].ourCode,
		            XMLParser::getErrorMessage(libxmlErrorTable[i].ourCode),
		            XMLError::Error,
				      "",
				      libxmlError->line,
				      libxmlError->int2));
        }
        else
        {
          // We have no error log, but we can't gloss over this error.  Use the
          // measure of last resort.

          cerr << "XML parsing error at line and column numbers " 
	      << libxmlError->line << ":"
	      << libxmlError->int2 << ":\n"
	      << XMLParser::getErrorMessage(libxmlErrorTable[i].ourCode) << endl;
        }
    }
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

/*
 * Notes about libxml2 data structures.
 *
 * struct _xmlError {
 *     int	domain	: What part of the library raised this error
 *     int	code	: The error code, e.g. an xmlParserError
 *     char *	message	: human-readable informative error message
 *     xmlErrorLevel	level	: how consequent is the error
 *     char *	file	: the filename
 *     int	line	: the line number if available
 *     char *	str1	: extra string information
 *     char *	str2	: extra string information
 *     char *	str3	: extra string information
 *     int	int1	: extra number information
 *     int	int2	: column number of the error or 0 if N/A (todo: rename this field when
 *     void *	ctxt	: the parser context if available
 *     void *	node	: the node in the tree
 * } xmlError;
 * 
 *  XML_ERR_INTERNAL_ERROR = 1 
 *  XML_ERR_NO_MEMORY = 2 
 *  XML_ERR_DOCUMENT_START = 3 
 *  XML_ERR_DOCUMENT_EMPTY = 4 
 *  XML_ERR_DOCUMENT_END = 5 
 *  XML_ERR_INVALID_HEX_CHARREF = 6 
 *  XML_ERR_INVALID_DEC_CHARREF = 7 
 *  XML_ERR_INVALID_CHARREF = 8 
 *  XML_ERR_INVALID_CHAR = 9 
 *  XML_ERR_CHARREF_AT_EOF = 10 
 *  XML_ERR_CHARREF_IN_PROLOG = 11 
 *  XML_ERR_CHARREF_IN_EPILOG = 12 
 *  XML_ERR_CHARREF_IN_DTD = 13 
 *  XML_ERR_ENTITYREF_AT_EOF = 14 
 *  XML_ERR_ENTITYREF_IN_PROLOG = 15 
 *  XML_ERR_ENTITYREF_IN_EPILOG = 16 
 *  XML_ERR_ENTITYREF_IN_DTD = 17 
 *  XML_ERR_PEREF_AT_EOF = 18 
 *  XML_ERR_PEREF_IN_PROLOG = 19 
 *  XML_ERR_PEREF_IN_EPILOG = 20 
 *  XML_ERR_PEREF_IN_INT_SUBSET = 21 
 *  XML_ERR_ENTITYREF_NO_NAME = 22 
 *  XML_ERR_ENTITYREF_SEMICOL_MISSING = 23 
 *  XML_ERR_PEREF_NO_NAME = 24 
 *  XML_ERR_PEREF_SEMICOL_MISSING = 25 
 *  XML_ERR_UNDECLARED_ENTITY = 26 
 *  XML_WAR_UNDECLARED_ENTITY = 27 
 *  XML_ERR_UNPARSED_ENTITY = 28 
 *  XML_ERR_ENTITY_IS_EXTERNAL = 29 
 *  XML_ERR_ENTITY_IS_PARAMETER = 30 
 *  XML_ERR_UNKNOWN_ENCODING = 31 
 *  XML_ERR_UNSUPPORTED_ENCODING = 32 
 *  XML_ERR_STRING_NOT_STARTED = 33 
 *  XML_ERR_STRING_NOT_CLOSED = 34 
 *  XML_ERR_NS_DECL_ERROR = 35 
 *  XML_ERR_ENTITY_NOT_STARTED = 36 
 *  XML_ERR_ENTITY_NOT_FINISHED = 37 
 *  XML_ERR_LT_IN_ATTRIBUTE = 38 
 *  XML_ERR_ATTRIBUTE_NOT_STARTED = 39 
 *  XML_ERR_ATTRIBUTE_NOT_FINISHED = 40 
 *  XML_ERR_ATTRIBUTE_WITHOUT_VALUE = 41 
 *  XML_ERR_ATTRIBUTE_REDEFINED = 42 
 *  XML_ERR_LITERAL_NOT_STARTED = 43 
 *  XML_ERR_LITERAL_NOT_FINISHED = 44 
 *  XML_ERR_COMMENT_NOT_FINISHED = 45 
 *  XML_ERR_PI_NOT_STARTED = 46 
 *  XML_ERR_PI_NOT_FINISHED = 47 
 *  XML_ERR_NOTATION_NOT_STARTED = 48 
 *  XML_ERR_NOTATION_NOT_FINISHED = 49 
 *  XML_ERR_ATTLIST_NOT_STARTED = 50 
 *  XML_ERR_ATTLIST_NOT_FINISHED = 51 
 *  XML_ERR_MIXED_NOT_STARTED = 52 
 *  XML_ERR_MIXED_NOT_FINISHED = 53 
 *  XML_ERR_ELEMCONTENT_NOT_STARTED = 54 
 *  XML_ERR_ELEMCONTENT_NOT_FINISHED = 55 
 *  XML_ERR_XMLDECL_NOT_STARTED = 56 
 *  XML_ERR_XMLDECL_NOT_FINISHED = 57 
 *  XML_ERR_CONDSEC_NOT_STARTED = 58 
 *  XML_ERR_CONDSEC_NOT_FINISHED = 59 
 *  XML_ERR_EXT_SUBSET_NOT_FINISHED = 60 
 *  XML_ERR_DOCTYPE_NOT_FINISHED = 61 
 *  XML_ERR_MISPLACED_CDATA_END = 62 
 *  XML_ERR_CDATA_NOT_FINISHED = 63 
 *  XML_ERR_RESERVED_XML_NAME = 64 
 *  XML_ERR_SPACE_REQUIRED = 65 
 *  XML_ERR_SEPARATOR_REQUIRED = 66 
 *  XML_ERR_NMTOKEN_REQUIRED = 67 
 *  XML_ERR_NAME_REQUIRED = 68 
 *  XML_ERR_PCDATA_REQUIRED = 69 
 *  XML_ERR_URI_REQUIRED = 70 
 *  XML_ERR_PUBID_REQUIRED = 71 
 *  XML_ERR_LT_REQUIRED = 72 
 *  XML_ERR_GT_REQUIRED = 73 
 *  XML_ERR_LTSLASH_REQUIRED = 74 
 *  XML_ERR_EQUAL_REQUIRED = 75 
 *  XML_ERR_TAG_NAME_MISMATCH = 76 
 *  XML_ERR_TAG_NOT_FINISHED = 77 
 *  XML_ERR_STANDALONE_VALUE = 78 
 *  XML_ERR_ENCODING_NAME = 79 
 *  XML_ERR_HYPHEN_IN_COMMENT = 80 
 *  XML_ERR_INVALID_ENCODING = 81 
 *  XML_ERR_EXT_ENTITY_STANDALONE = 82 
 *  XML_ERR_CONDSEC_INVALID = 83 
 *  XML_ERR_VALUE_REQUIRED = 84 
 *  XML_ERR_NOT_WELL_BALANCED = 85 
 *  XML_ERR_EXTRA_CONTENT = 86 
 *  XML_ERR_ENTITY_CHAR_ERROR = 87 
 *  XML_ERR_ENTITY_PE_INTERNAL = 88 
 *  XML_ERR_ENTITY_LOOP = 89 
 *  XML_ERR_ENTITY_BOUNDARY = 90 
 *  XML_ERR_INVALID_URI = 91 
 *  XML_ERR_URI_FRAGMENT = 92 
 *  XML_WAR_CATALOG_PI = 93 
 *  XML_ERR_NO_DTD = 94 
 *  XML_ERR_CONDSEC_INVALID_KEYWORD = 95 
 *  XML_ERR_VERSION_MISSING = 96 
 *  XML_WAR_UNKNOWN_VERSION = 97 
 *  XML_WAR_LANG_VALUE = 98 
 *  XML_WAR_NS_URI = 99 
 *  XML_WAR_NS_URI_RELATIVE = 100 
 *  XML_ERR_MISSING_ENCODING = 101 
 *  XML_WAR_SPACE_VALUE = 102 
 *  XML_ERR_NOT_STANDALONE = 103 
 *  XML_ERR_ENTITY_PROCESSING = 104 
 *  XML_ERR_NOTATION_PROCESSING = 105 
 *  XML_WAR_NS_COLUMN = 106 
 *  XML_WAR_ENTITY_REDEFINED = 107 
 *  XML_NS_ERR_XML_NAMESPACE = 200
 *  XML_NS_ERR_UNDEFINED_NAMESPACE = 201 
 *  XML_NS_ERR_QNAME = 202 
 *  XML_NS_ERR_ATTRIBUTE_REDEFINED = 203 
 *  XML_NS_ERR_EMPTY = 204 
 *
 *  * http://stuff.mit.edu/afs/sipb/project/php/share/gtk-doc/html/libxml2/libxml2-parser.html
 */


