/**
 * \file    XMLParser.h
 * \brief   XMLParser interface and factory
 * \author  Ben Bornstein
 * \author  Sarah Keating
 * \author  Michael Hucka
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


#ifndef XMLParser_h
#define XMLParser_h

#ifdef __cplusplus

#include <string>
#include <sbml/xml/XMLExtern.h>


class XMLErrorLog;
class XMLHandler;


class LIBLAX_EXTERN XMLParser
{
public:

  /**
   * Creates a new XMLParser.  The parser will notify the given XMLHandler
   * of parse events and errors.
   *
   * The library parameter indicates the underlying XML library to use if
   * the XML compatibility layer has been linked against multiple XML
   * libraries.  It may be one of: "expat" (default), "libxml", or
   * "xerces".
   *
   * If the XML compatibility layer has been linked against only a single
   * XML library, the library parameter is ignored.
   */
  static XMLParser* create (  XMLHandler&       handler
                            , const std::string library = "" );

  /**
   * Destroys this XMLParser.
   */
  virtual ~XMLParser ();


  /**
   * @return the current column position of the parser.
   */
  virtual unsigned int getColumn () const = 0;

  /**
   * @return the current line position of the parser.
   */
  virtual unsigned int getLine () const = 0;

  /**
   * @return an XMLErrorLog which can be used to log XML parse errors and
   * other validation errors (and messages).
   */
  XMLErrorLog* getErrorLog ();

  /**
   * Parses XML content in one fell swoop.
   *
   * If isFile is true (default), content is treated as a filename from
   * which to read the XML content.  Otherwise, content is treated as a
   * null-terminated buffer containing XML data and is read directly.
   *
   * @return true if the parse was successful, false otherwise.
   */
  virtual bool parse (const char* content, bool isFile = true) = 0;

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
  virtual bool parseFirst (const char* content, bool isFile = true) = 0;

  /**
   * Parses the next chunk of XML content.
   *
   * @return true if the next step of the progressive parse was successful,
   * false otherwise or when at EOF.
   */
  virtual bool parseNext () = 0;

  /**
   * Resets the progressive parser.  Call between the last call to
   * parseNext() and the next call to parseFirst().
   */
  virtual void parseReset () = 0;

  /**
   * Canonical error codes returned for low-level XML parser errors.
   * This is an abstraction of errors from the multiple parsers (Xerces,
   * Expat, libxml2) supported by libSBML.
   */
  enum errorCodes
  {
      NoError                       = 0
    , ErrorNoXMLDecl                = 1
    , ErrorBadXMLDecl               = 2
    , ErrorInvalidChar              = 3
    , ErrorNotWellFormed            = 4
    , ErrorUnclosedToken            = 5
    , ErrorInvalidConstruct         = 6
    , ErrorTagMismatch              = 7
    , ErrorDupAttribute             = 8
    , ErrorBadDOCTYPE               = 9
    , ErrorUndefinedEntity          = 11
    , ErrorBadProcessingInstruction = 17
    , ErrorBadPrefixDefinition      = 27
    , ErrorBadPrefixValue           = 28
    , ErrorOutOfMemory              = 9998
    // Make sure this is the last and highest-numbered item:
    , UnknownError                  = 9999
  };

  /**
   * Log or otherwise report the error from the parser indicated by the
   * given integer code.
   */
  virtual void reportError (  const int code
			    , const unsigned int lineNumber
			    , const unsigned int columnNumber) = 0;

  /**
   * Sets the XMLErrorLog this parser will use to log errors.
   */
  void setErrorLog (XMLErrorLog* log);


protected:

  /**
   * Creates a new XMLParser.  The parser will notify the given XMLHandler
   * of parse events and errors.
   *
   * Only subclasses may call this constructor directly.  Everyone else
   * should use XMLParser::create().
   */
  XMLParser ();

  /**
   * Returns a string message given an error code taken from the
   * 'errorCodes' enumeration.
   */
  const char *getErrorMessage (enum errorCodes code);


  XMLErrorLog* mErrorLog;
  bool         mOwnLog;
};

#endif  /* __cplusplus */

#endif  /* XMLParser_h */
