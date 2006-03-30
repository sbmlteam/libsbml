/**
 * \file    XMLToken.h
 * \brief   A unit of XML syntax, either an XML element or text.
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


#ifndef XMLToken_h
#define XMLToken_h


#include <string>

#include "XMLExtern.h"
#include "XMLAttributes.h"
#include "XMLNamespaces.h"
#include "XMLOutputStream.h"
#include "XMLTriple.h"


class XMLOutputStream;


class LIBLAX_EXTERN XMLToken
{
public:

  /**
   * Creates a new empty XMLToken.
   */
  XMLToken ();

  /**
   * Creates a start element XMLToken with the given set of attributes and
   * namespace declarations.
   */
  XMLToken (  const XMLTriple&      triple
            , const XMLAttributes&  attributes
            , const XMLNamespaces&  namespaces
            , const unsigned int    line   = 0
            , const unsigned int    column = 0 );

  /**
   * Creates a start element XMLToken with the given set of attributes.
   */
  XMLToken (  const XMLTriple&      triple
            , const XMLAttributes&  attributes
            , const unsigned int    line   = 0
            , const unsigned int    column = 0 );


  /**
   * Creates an end element XMLToken.
   */
  XMLToken (  const XMLTriple&    triple
            , const unsigned int  line   = 0
            , const unsigned int  column = 0 );

  /**
   * Creates a text XMLToken.
   */
  XMLToken (  const std::string&  chars
            , const unsigned int  line   = 0
            , const unsigned int  column = 0 );

  /**
   * Destroys this XMLToken.
   */
  virtual ~XMLToken ();


  /**
   * Appends characters to this XML text content.
   */
  void append (const std::string& chars);

  /**
   * @return the XMLAttributes of this XML element.
   */
  const XMLAttributes& getAttributes () const;

  /**
   * @return the characters of this XML text.
   */
  const std::string& getCharacters () const;

  /**
   * @return the column at which this XMLToken occurred.
   */
  unsigned int getColumn () const;

  /**
   * @return the line at which this XMLToken occurred.
   */
  unsigned int getLine () const;

  /**
   * @return the XML namespace declarations for this XML element.
   */
  const XMLNamespaces& getNamespaces () const;

  /**
   * @return the (unqualified) name of this XML element.
   */
  const std::string& getName () const;

  /**
   * @return the namespace prefix of this XML element.  If no prefix
   * exists, an empty string will be return.
   */
  const std::string& getPrefix () const;

  /**
   * @return the namespace URI of this XML element.
   */
  const std::string& getURI () const;


  /**
   * @return true if this XMLToken is an XML element.
   */
  bool isElement () const;

  /**
   * @return true if this XMLToken is an XML end element, false
   * otherwise.
   */
  bool isEnd () const;

  /**
   * @return true if this XMLToken is an XML end element for the given XML
   * start element, false otherwise.
   */
  bool isEndFor (const XMLToken& element) const;

  /**
   * @return true if this XMLToken is an end of file (input) marker, false
   * otherwise.
   */
  bool isEOF () const;

  /**
   * @return true if this XMLToken is an XML start element, false
   * otherwise.
   */
  bool isStart () const;

  /**
   * @return true if this XMLToken is text, false otherwise.
   */
  bool isText () const;

  /**
   * Declares this XML start element is also an end element.
   */
  void setEnd ();

  /**
   * Declares this XMLToken is an end-of-file (input) marker.
   */
  void setEOF ();

  /**
   * Writes this XMLToken to stream.
   */
  void write (XMLOutputStream& stream) const;

  /**
   * Inserts this XMLToken into stream.
   */
  friend
  XMLOutputStream& operator<< (XMLOutputStream& stream, const XMLToken& token);


protected:

  XMLTriple     mTriple;
  XMLAttributes mAttributes;
  XMLNamespaces mNamespaces;

  std::string mChars;

  bool mIsStart;
  bool mIsEnd;
  bool mIsText;

  unsigned int mLine;
  unsigned int mColumn;
};


#endif  /* XMLToken_h */
