/**
 * \file    XercesParser.cpp
 * \brief   Adapts the Xerces-C++ XML parser to the XMLParser interface
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


#include <cstring>

#include <xercesc/framework/LocalFileInputSource.hpp>
#include <xercesc/framework/MemBufInputSource.hpp>
#include <xercesc/parsers/SAX2XMLReaderImpl.hpp>
#include <xercesc/sax2/XMLReaderFactory.hpp>
#include <xercesc/util/PlatformUtils.hpp>

#include <sbml/xml/XMLHandler.h>
#include <sbml/xml/XMLErrorLog.h>

#include <sbml/xml/XercesTranscode.h>
#include <sbml/xml/XercesParser.h>


using namespace std;
using namespace xercesc;


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
                        , const XMLCh* const autoEncoding )
  {
    mHandler.XML( XercesTranscode(version), XercesTranscode(encoding) );
  }

  XMLHandler&  mHandler;
};


/**
 * Creates a new XercesParser.  The parser will notify the given XMLHandler
 * of parse events and errors.
 */
XercesParser::XercesParser (XMLHandler& handler) :
   mReader ( 0       )
 , mSource ( 0       )
 , mHandler( handler )
{
  try
  {
    XMLPlatformUtils::Initialize();

    mReader = new XercesReader(handler); // XMLReaderFactory::createXMLReader();

    mReader->setContentHandler( &mHandler );
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
  int nError = 0;
  if ( error() ) return false;

  try
  {
    mReader->parseNext(mToken);
  }
  catch (const SAXParseException& e)
  {
    std::string  msg = XMLString::transcode( e.getMessage() );

    /* error numbers are from the expat error enumeration
     * xerces does not enumerate its saxexceptions
     */
    if (strncmp(msg.c_str(), "Expected end of tag", 19) == 0)
    {
      nError = 7;
    }
    else if (strncmp(msg.c_str(), "Expected whitespace", 19) == 0)
    {
      msg = "Not well formed";
      nError = 4;
    }
    else if (strncmp(msg.c_str(), "Expected equal sign", 19) == 0)
    {
      nError = 4;
    }
    else if (strncmp(msg.c_str(), "Expected comment or", 19) == 0)
    {
      nError = 4;
    }
    else if (strncmp(msg.c_str(), "Expected an attribute value", 27) == 0)
    {
      msg += " - Not well formed";
      nError = 4;
    }
    else if (strncmp(msg.c_str(), "The attribute '", 15) == 0)
    {
      nError = 8;
    }
    else if (strncmp(msg.c_str(), "No processing instruction starts with", 37) == 0)
    {
      nError = 17;
    }
    else if (strncmp(msg.c_str(), "Entity '", 8) == 0)
    {
      nError = 11;
    }
    else if (strncmp(msg.c_str(), "The prefix '", 12) == 0)
    {
      nError = 27;
    }
    else if (strncmp(msg.c_str(), "The value of the attribute '", 28) == 0)
    {
      nError = 28;
    }

    getErrorLog()->add( XMLError(nError, msg, 
      XMLError::Error, "", e.getLineNumber(), e.getColumnNumber()));
    
    result = false;

  }
  catch (const XMLException& e)
  {
    char*           msg = XMLString::transcode( e.getMessage() );
    result = false;
    getErrorLog()->add( XMLError(nError, msg, 
        XMLError::Error, "", e.getSrcLine(), 1));
  }
  catch (...)
  {
    char * msg = "Unknown exception";
    result = false;
    getErrorLog()->add( XMLError(nError, msg, 
        XMLError::Error, "", 1, 1));
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
