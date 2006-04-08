/**
 * \file    XercesParser.h
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


#ifndef XercesParser_h
#define XercesParser_h


#include <string>
#include <xercesc/sax2/SAX2XMLReader.hpp>

#include <sbml/xml/XMLParser.h>
#include <sbml/xml/XercesHandler.h>


class xercesc::SAX2XMLReader;
class XMLHandler;


class XercesParser : public XMLParser
{
public:

  /**
   * Creates a new XercesParser.  The parser will notify the given XMLHandler
   * of parse events and errors.
   */
  XercesParser (XMLHandler& handler);

  /**
   * Destroys this XercesParser.
   */
  virtual ~XercesParser ();


  /**
   * @return the current column position of the parser.
   */
  virtual unsigned int getColumn () const;

  /**
   * @return the current line position of the parser.
   */
  virtual unsigned int getLine () const;

  /**
   * Parses XML content.
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
   * null-terminated buffer containing XML data and is read directly.
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

  /**
   * @return true if the parse was successful, false otherwise;
   */
  bool parse (const char* content, bool isFile, bool isProgressive);

  /**
   * Creates a Xerces-C++ InputSource appropriate to the given XML content.
   */
  static xercesc::InputSource* createSource (const char* content, bool isFile);


  xercesc::SAX2XMLReader*  mReader;
  xercesc::InputSource*    mSource;
  xercesc::XMLPScanToken   mToken;
  XercesHandler            mHandler;

};


#endif  /* XercesParser_h */
