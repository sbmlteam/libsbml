/**
 * @file    XercesParser.cpp
 * @brief   Adapts the Xerces-C++ XML parser to the XMLParser interface
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

#include <cstring>
#include <iostream>

#include <xercesc/framework/LocalFileInputSource.hpp>
#include <xercesc/framework/MemBufInputSource.hpp>
#include <xercesc/parsers/SAX2XMLReaderImpl.hpp>
#include <xercesc/sax2/XMLReaderFactory.hpp>
#include <xercesc/util/PlatformUtils.hpp>

#include <sbml/xml/XMLHandler.h>
#include <sbml/xml/XMLErrorLog.h>

#include <sbml/xml/XercesTranscode.h>
#include <sbml/xml/XercesParser.h>


/** @cond doxygen-ignored */

using namespace std;
using namespace xercesc;

/** @endcond doxygen-ignored */


/** @cond doxygen-libsbml-internal */

/**
 * Subclass of Xerces' `SAXParseException' that can store the error code.
 */
class OurSAXParseException : public SAXParseException
{
public:
  /**
    * Create a new instance of OurSAXParseException.
    * See also the `SAXParseException' in Xerces 2.7.0.
    *
    * @param message The error or warning message.
    * @param publicId The public identifer of the entity that generated
    *                 the error or warning.
    * @param systemId The system identifer of the entity that generated
    *                 the error or warning.
    * @param lineNumber The line number of the end of the text that
    *                   caused the error or warning.
    * @param columnNumber The column number of the end of the text that
    *                     caused the error or warning.
    * @param manager    Pointer to the memory manager to be used to
    *                   allocate objects.
    * @param errorCode  Integer error code of what caused this exception.
    *
    * @see Parser#setLocale
    */
    OurSAXParseException
    (
     const int               theErrorCode
     , const XMLCh* const    theMessage
     , const XMLCh* const    thePublicId
     , const XMLCh* const    theSystemId
     , const XMLSSize_t      theLineNumber
     , const XMLSSize_t      theColumnNumber 
    ) : SAXParseException(theMessage, thePublicId, theSystemId,
			  theLineNumber, theColumnNumber)
      
  {
    lastXercesError = theErrorCode;
  };

  /**
   * Last error code returned by the Xerces parser when it threw a
   * `SAXParseException'.
   */
  int lastXercesError;
};


/**
 * XercesReader is a specialization of the default SAX2XMLReader that
 * captures and redirects XML declarations and some special errors.
 */
class XercesReader : public SAX2XMLReaderImpl
{
public:

  XercesReader (XMLHandler& handler) : mHandler(handler) { }
  virtual ~XercesReader () { }

  /**
   * This method is used to report the XML decl scanned by the parser.
   * Refer to the XML specification to see the meaning of parameters.
   */
  virtual void XMLDecl (  const XMLCh* const version
                        , const XMLCh* const encoding
                        , const XMLCh* const standalone
                        , const XMLCh* const autoEncoding
		       )
  {
    mHandler.XML( XercesTranscode(version), XercesTranscode(encoding) );
  }

  /**
   * The default error definition for SAX2XMLReaderImpl throws away the
   * error code and doesn't report it, even if you install a custom
   * error handler.  (SAXParseException doesn't even have a field for
   * the error number.  WTF?)  We want that error code number.  There 
   * doesn't seem to be a way to get it except by overriding the 
   * definition of the `error' method.
   */
  virtual void error (  const unsigned int                errCode
		      , const XMLCh* const                msgDomain
		      , const XMLErrorReporter::ErrTypes  errType
		      , const XMLCh* const                errorText
		      , const XMLCh* const                systemId
		      , const XMLCh* const                publicId
		      , const XMLSSize_t                  lineNum
		      , const XMLSSize_t                  colNum
		     )
  {
    OurSAXParseException toThrow =
      OurSAXParseException(errCode, errorText, publicId, systemId, 
			   lineNum, colNum);
    throw toThrow;
  }

  XMLHandler&  mHandler;
};


/**
 * Creates a new XercesParser.  The parser will notify the given XMLHandler
 * of parse events and errors.
 */
XercesParser::XercesParser (XMLHandler& handler) :
   mReader         ( 0       )
 , mSource         ( 0       )
 , mHandler        ( handler )
{
  try
  {
    XMLPlatformUtils::Initialize();

    mReader = new XercesReader(handler); // XMLReaderFactory::createXMLReader();

    mReader->setContentHandler(&mHandler);
    mReader->setErrorHandler(&mHandler);

    mReader->setFeature( XMLUni::fgSAX2CoreNameSpaces       , true );
    mReader->setFeature( XMLUni::fgSAX2CoreNameSpacePrefixes, true );
  }
  catch (...)
  {
  }
}


/**
 * Destroys this XercesParser.
 */
XercesParser::~XercesParser ()
{
  delete mReader;
  delete mSource;
}


/**
 * Creates a Xerces-C++ InputSource appropriate to the given XML content.
 */
InputSource*
XercesParser::createSource (const char* content, bool isFile)
{
  InputSource* source = 0;


  if (isFile)
  {
    XMLCh* filename = XMLString::transcode(content);
    source          = new LocalFileInputSource(filename);
    XMLString::release(&filename);
  }
  else
  {
    const unsigned int size  = strlen(content);
    const XMLByte*     bytes = reinterpret_cast<const XMLByte*>(content);
    source = new MemBufInputSource(bytes, size, "FromString", false); 
  }

  return source;
}


/**
 * @return true if the parser encountered an error, false otherwise.
 */
bool
XercesParser::error () const
{
  return (mReader == 0);
}


/**
 * @return the current column position of the parser.
 */
unsigned int
XercesParser::getColumn () const
{
  return mHandler.getColumn();
}


/**
 * @return the current line position of the parser.
 */
unsigned int
XercesParser::getLine () const
{
  return mHandler.getLine();
}


/**
 * @return true if the parse was successful, false otherwise;
 */
bool
XercesParser::parse (const char* content, bool isFile, bool isProgressive)
{
  if ( error() ) return false;

  bool result = true;

  try
  {
    mSource = createSource(content, isFile);

    if (mSource)
    {
      if (isProgressive)
      {
        mReader->parseFirst(*mSource, mToken);
      }
      else
      {
        mReader->parse(*mSource);
      }
    }
    else
    {
      result = false;
    }
  }
  catch (...)
  {
    result = false;
  }

  return result;
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
XercesParser::parse (const char* content, bool isFile)
{
  return parse(content, isFile, false);
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
 * If isFile is true (default), content is treated as a filename from
 * which to read the XML content.  Otherwise, content is treated as a
 * null-terminated buffer containing XML data and is read directly.
 *
 * @return true if the first step of the progressive parse was
 * successful, false otherwise.
 */
bool
XercesParser::parseFirst (const char* content, bool isFile)
{
  return parse(content, isFile, true);
}


/**
 * Parses the next chunk of XML content.
 *
 * @return true if the next step of the progressive parse was successful,
 * false otherwise or when at EOF.
 */
bool
XercesParser::parseNext ()
{
  bool result = true;
  int nError  = 0;

  if ( error() ) return false;

  try
  {
    mReader->parseNext(mToken);
  }
  catch (const OurSAXParseException& e)
  {
    reportError(e.lastXercesError, e.getLineNumber(), e.getColumnNumber());
    result = false;
  }
  catch (const XMLException& e)
  {
    // This shouldn't happen.  If it does, we should figure out how to
    // do the same trick as with OurSAXParseException so that we can
    // report canonical error numbers for this case too.

    char* msg = XMLString::transcode( e.getMessage() );
    getErrorLog()->add( XMLError(nError, msg, 
        XMLError::Error, "", e.getSrcLine(), 1));
    result = false;
  }
  catch (const SAXParseException&)
  {
    char* msg = "Unknown exception";
    getErrorLog()->add( XMLError(nError, msg, 
        XMLError::Error, "", 1, 1));
    result = false;
  }
  catch (...)
  {
    char* msg = "Unknown exception";
    getErrorLog()->add( XMLError(nError, msg, 
        XMLError::Error, "", 1, 1));
    result = false;
  }
  return result;
}


/**
 * Resets the progressive parser.  Call between the last call to
 * parseNext() and the next call to parseFirst().
 */
void
XercesParser::parseReset ()
{
  if (mReader)
  {
    mReader->parseReset(mToken);
  }

  delete mSource;
  mSource = 0;
}

/*
 * Table mapping Xerces error codes to ours.  The error code numbers are not
 * contiguous, hence the table has to map pairs of numbers rather than
 * simply being an array of codes.  The table is an array of vectors of
 * items [xerces code, our code], where `our code' is an error code
 * taken from the enumeration XMLParser::errorCodes.
 *
 * See  /include/xercesc/framework/XMLErrorCodes.hpp
 * and /src/xerces-c-src_2_7_0/src/xercesc/NLS/EN_US/XMLErrList_EN_US.Xml
 */ 
static struct xercesError {
  const int                  xercesCode;
  enum XMLParser::errorCodes ourCode;
} xercesErrorTable[] = {
  { XMLErrs::ExpectedEndOfTagX,       XMLParser::ErrorTagMismatch},
  { XMLErrs::ExpectedWhitespace,      XMLParser::ErrorNotWellFormed},
  { XMLErrs::ExpectedEqSign,          XMLParser::ErrorNotWellFormed},
  { XMLErrs::ExpectedComment,         XMLParser::ErrorNotWellFormed},
  { XMLErrs::ExpectedCommentOrPI,     XMLParser::ErrorNotWellFormed},
  { XMLErrs::ExpectedAttrValue,       XMLParser::ErrorNotWellFormed},
  { XMLErrs::AttrAlreadyUsedInSTag,   XMLParser::ErrorDupAttribute},
  { XMLErrs::NoPIStartsWithXML,       XMLParser::ErrorBadProcessingInstruction},
  { XMLErrs::EntityNotFound,          XMLParser::ErrorUndefinedEntity},
  { XMLErrs::UnknownPrefix,           XMLParser::ErrorBadPrefixDefinition},
  { XMLErrs::NoUseOfxmlnsAsPrefix,    XMLParser::ErrorBadPrefixDefinition},
  { XMLErrs::PrefixXMLNotMatchXMLURI, XMLParser::ErrorBadPrefixDefinition},
  { XMLErrs::NoEmptyStrNamespace,     XMLParser::ErrorBadPrefixValue},
  // The next one should always be last.  It's used only as a marker.
  { XMLErrs::F_HighBounds,            XMLParser::UnknownError},
};


void
XercesParser::reportError (const int code,
			   const unsigned int lineNumber,
			   const unsigned int columnNumber)
{
  unsigned int tableSize = sizeof(xercesErrorTable)/sizeof(xercesErrorTable[0]);


  if (code > 0 && code < XMLErrs::F_HighBounds)
  {
    // Iterate through the table, searching for a match for the given code.
    // Yes, this is inefficient, but if we're already in an exception,
    // who cares how efficient the error look-up is?

    for (unsigned int i = 0; i < tableSize; i++)
    {
      if (xercesErrorTable[i].xercesCode == code)
      {
	if (mErrorLog)
	{
	  getErrorLog()->add( 
	      XMLError(xercesErrorTable[i].ourCode,
		       XMLParser::getErrorMessage(xercesErrorTable[i].ourCode),
		       XMLError::Error,
		       "", 
		       lineNumber,
		       columnNumber));
	}
	else
	{
	  // We have no error log, but we can't gloss over this error.  Use
	  // the measure of last resort.

	  cerr << "XML parsing error at line and column numbers " 
	       << lineNumber << ":" << columnNumber << ":\n"
	       << XMLParser::getErrorMessage(xercesErrorTable[i].ourCode)
	       << endl;

	}
      }
    };
  }
  else
  {
    // The given code doesn't correspond to any known Xerces error code.
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
};

/** @endcond doxygen-libsbml-internal */


//     /* error numbers are from the expat error enumeration
//      * xerces does not enumerate its saxexceptions
//      */
//     if (strncmp(msg.c_str(), "Expected end of tag", 19) == 0)
//     {
//       nError = 7;
//     }
//     else if (strncmp(msg.c_str(), "Expected whitespace", 19) == 0)
//     {
//       msg = "Not well formed";
//       nError = 4;
//     }
//     else if (strncmp(msg.c_str(), "Expected equal sign", 19) == 0)
//     {
//       nError = 4;
//     }
//     else if (strncmp(msg.c_str(), "Expected comment or", 19) == 0)
//     {
//       nError = 4;
//     }
//     else if (strncmp(msg.c_str(), "Expected an attribute value", 27) == 0)
//     {
//       msg += " - Not well formed";
//       nError = 4;
//     }
//     else if (strncmp(msg.c_str(), "The attribute '", 15) == 0)
//     {
//       nError = 8;
//     }
//     else if (strncmp(msg.c_str(), "No processing instruction starts with", 37) == 0)
//     {
//       nError = 17;
//     }
//     else if (strncmp(msg.c_str(), "Entity '", 8) == 0)
//     {
//       nError = 11;
//     }
//     else if (strncmp(msg.c_str(), "The prefix '", 12) == 0)
//     {
//       nError = 27;
//     }
//     else if (strncmp(msg.c_str(), "The value of the attribute '", 28) == 0)
//     {
//       nError = 28;
//     }

//     getErrorLog()->add( XMLError(nError, msg, 
//       XMLError::Error, "", e.getLineNumber(), e.getColumnNumber()));
    
//     result = false;
