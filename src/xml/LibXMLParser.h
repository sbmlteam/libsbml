/**
 * @file    LibXMLParser.h
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

#ifndef LibXMLParser_h
#define LibXMLParser_h

#ifdef __cplusplus

#include <string>

#include <sbml/xml/XMLParser.h>
#include <sbml/xml/LibXMLHandler.h>
#include <sbml/xml/XMLErrorLog.h>


/** @cond doxygen-libsbml-internal */

class XMLBuffer;
class XMLHandler;
class XMLErrorLog;


class LibXMLParser : public XMLParser
{
public:

  /**
   * Creates a new LibXMLParser.  The parser will notify the given XMLHandler
   * of parse events and errors.
   */
  LibXMLParser (XMLHandler& handler);


  /**
   * Destroys this LibXMLParser.
   */
  virtual ~LibXMLParser ();


  /**
   * @return the current column position of the parser.
   */
  virtual unsigned int getColumn () const;


  /**
   * @return the current line position of the parser.
   */
  virtual unsigned int getLine () const;


  /**
   * Parses XML content in one fell swoop.
   *
   * If isFile is true (default), content is treated as a filename from
   * which to read the XML content.  Otherwise, content is treated as a
   * null-terminated buffer containing XML data and is read directly.
   *
   * @return true if the parse was successful, false otherwise.
   */
  virtual bool parse (const char* content, bool isFile);


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
   * buffer containing XML data and is read directly.
   *
   * @return true if the first step of the progressive parse was
   * successful, false otherwise.
   */
  virtual bool parseFirst (const char* content, bool isFile);


  /**
   * Parses the next chunk of XML content.
   *
   * @return true if the next step of the progressive parse was successful,
   * false otherwise or when at EOF.
   */
  virtual bool parseNext ();


  /**
   * Resets the progressive parser.  Call between the last call to
   * parseNext() and the next call to parseFirst().
   */
  virtual void parseReset ();


protected:

  /**
   * @return true if the parser encountered an error, false otherwise.
   */
  bool error () const;


  xmlParserCtxt*  mParser;
  LibXMLHandler   mHandler;
  char*           mBuffer;
  XMLBuffer*      mSource;


private:

  /**
   * Log or otherwise report the given error.
   *
   * @docnote The native C++ implementation of this method defines a
   * default argument value.  In the documentation generated for different
   * libSBML language bindings, you may or may not see corresponding
   * arguments in the method declarations.  For example, in Java, a default
   * argument is handled by declaring two separate methods, with one of
   * them having the argument and the other one lacking the argument.
   * However, the libSBML documentation will be @em identical for both
   * methods.  Consequently, if you are reading this and do not see an
   * argument even though one is described, please look for descriptions of
   * other variants of this method near where this one appears in the
   * documentation.
   */
  void reportError (  const XMLErrorCode_t code
		    , const std::string& extraMsg     = ""
		    , const unsigned int lineNumber   = 0
		    , const unsigned int columnNumber = 0 );

};

/** @endcond doxygen-libsbml-internal */

#endif  /* __cplusplus */
#endif  /* LibXMLParser_h */
