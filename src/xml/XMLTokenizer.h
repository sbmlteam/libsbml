/**
 * \file    XMLTokenizer.h
 * \brief   Uses an XMLHandler to deliver an XML stream as a series of tokens
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

#ifndef XMLTokenizer_h
#define XMLTokenizer_h


#include <deque>

#include "XMLExtern.h"
#include "XMLHandler.h"


class LIBLAX_EXTERN XMLToken;


class XMLTokenizer : public XMLHandler
{
public:

  /**
   * Creates a new XMLTokenizer.
   */
  XMLTokenizer ();

  /**
   * Destroys this XMLTokenizer.
   */
  ~XMLTokenizer ();


  /**
   * @return the encoding of the underlying XML document.
   */
  const std::string& getEncoding ();

  /**
   * @return true if this XMLTokenizer has at least one XMLToken ready to
   * deliver, false otherwise.
   *
   * Note that hasNext() == false does not imply isEOF() == true.  The
   * XMLTokenizer may simply be waiting for the XMLParser to parse more of
   * the document.
   */
  bool hasNext () const;

  /**
   * @return true if the end of the XML file (document) has been reached
   * and there are no more tokens to consume, false otherwise.
   */
  bool isEOF () const;

  /**
   * Consume the next XMLToken and return it.
   *
   * @return the next XMLToken.
   */
  XMLToken next ();

  /**
   * Returns the next XMLToken without consuming it.  A subsequent call to
   * either peek() or next() will return the same token.
   *
   * @return the next XMLToken.
   */
  const XMLToken& peek ();

  /**
   * Receive notification of the XML declaration, i.e.
   * <?xml version="1.0" encoding="UTF-8"?>
   */
  virtual void XML (const std::string& version, const std::string& encoding);

  /**
   * Receive notification of the start of an element.
   */
  virtual void startElement (const XMLToken& element);

  /**
   * Receive notification of the end of the document.
   */
  virtual void endDocument ();

  /**
   * Receive notification of the end of an element.
   */
  virtual void endElement (const XMLToken& element);

  /**
   * Receive notification of character data inside an element.
   */
  virtual void characters (const XMLToken& data);


 protected:

  bool mInChars;
  bool mInStart;
  bool mEOFSeen;

  std::string mEncoding;

  XMLToken             mCurrent;
  std::deque<XMLToken> mTokens;
};


#endif  /* XMLTokenizer_h */
