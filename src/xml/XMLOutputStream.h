/**
 * \file    XMLOutputStream.h
 * \brief   XMLOutputStream
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


#ifndef XMLOutputStream_h
#define XMLOutputStream_h


#include <iostream>
#include <string>

#include <sbml/xml/XMLExtern.h>


class XMLTriple;


class LIBLAX_EXTERN XMLOutputStream
{
public:

  /**
   * Creates a new XMLOutputStream that wraps stream.
   */
  XMLOutputStream (  std::ostream&       stream
                   , const std::string&  encoding     = "UTF-8"
                   , bool                writeXMLDecl = true );


  /**
   * Writes the given XML end element name to this XMLOutputStream.
   */
  void endElement (const std::string& name);

  /**
   * Writes the given XML end element 'prefix:name' to this
   * XMLOutputStream.
   */
  void endElement (const XMLTriple& triple);

  /**
   * Turns automatic indentation on or off for this XMLOutputStream.
   */
  void setAutoIndent (bool indent);

  /**
   * Writes the given XML start element name to this XMLOutputStream.
   */
  void startElement (const std::string& name);

  /**
   * Writes the given XML start element 'prefix:name' to this
   * XMLOutputStream.
   */
  void startElement (const XMLTriple& triple);

  /**
   * Writes the given XML start and end element name to this XMLOutputStream.
   */
  void startEndElement (const std::string& name);

  /**
   * Writes the given XML start and end element 'prefix:name' to this
   * XMLOutputStream.
   */
  void startEndElement (const XMLTriple& triple);


  /**
   * Writes the given attribute, name="value" to this XMLOutputStream.
   */
  void writeAttribute (const std::string& name, const std::string& value);

  /**
   * Writes the given attribute, prefix:name="value" to this
   * XMLOutputStream.
   */
  void writeAttribute (const XMLTriple& triple, const std::string& value);

  /**
   * Writes the given attribute, name="true" or name="false" to this
   * XMLOutputStream.
   */
  void writeAttribute (const std::string& name, const bool& value);

  /**
   * Writes the given attribute, prefix:name="true" or prefix:name="false"
   * to this XMLOutputStream.
   */
  void writeAttribute (const XMLTriple& triple, const bool& value);

  /**
   * Writes the given attribute, name="value" to this XMLOutputStream.
   */
  void writeAttribute (const std::string& name, const double& value);

  /**
   * Writes the given attribute, prefix:name="value" to this
   * XMLOutputStream.
   */
  void writeAttribute (const XMLTriple& triple, const double& value);

  /**
   * Writes the given attribute, name="value" to this XMLOutputStream.
   */
  void writeAttribute (const std::string& name, const long& value);

  /**
   * Writes the given attribute, prefix:name="value" to this
   * XMLOutputStream.
   */
  void writeAttribute (const XMLTriple& triple, const long& value);

  /**
   * Writes the given attribute, name="value" to this XMLOutputStream.
   */
  void writeAttribute (const std::string& name, const int& value);

  /**
   * Writes the given attribute, prefix:name="value" to this
   * XMLOutputStream.
   */
  void writeAttribute (const XMLTriple& triple, const int& value);

  /**
   * Writes the given attribute, name="value" to this XMLOutputStream.
   */
  void writeAttribute (const std::string& name, const unsigned int& value);

  /**
   * Writes the given attribute, prefix:name="value" to this
   * XMLOutputStream.
   */
  void writeAttribute (const XMLTriple& triple, const unsigned int& value);


  /**
   * Writes the XML declaration:
   * <?xml version="1.0" encoding="..."?>
   */
  void writeXMLDecl ();


  /**
   * Outputs the given characters to the underlying stream.
   */
  XMLOutputStream& operator<< (const std::string& chars);

  /**
   * Outputs the given double to the underlying stream.
   */
  XMLOutputStream& operator<< (const double& value);

  /**
   * Outputs the given long to the underlying stream.
   */
  XMLOutputStream& operator<< (const long& value);

  /**
   * Outputs a single character to the underlying stream.
   */
  XMLOutputStream& operator<< (const char& c)
  {
    switch (c)
    {
      case '&' : mStream << "&amp;" ; break;
      case '\'': mStream << "&apos;"; break;
      case '<' : mStream << "&lt;"  ; break;
      case '>' : mStream << "&gt;"  ; break;
      case '"' : mStream << "&quot;"; break;
      default  : mStream << c;        break;
    }

    return *this;
  }


  /**
   * Decreases the indentation level for this XMLOutputStream.
   */
  void downIndent ();

  /**
   * Increases the indentation level for this XMLOutputStream.
   */
  void upIndent ();


protected:

  /**
   * Unitialized XMLOutputStreams may only be created by subclasses.
   */
  XMLOutputStream ();

  /**
   * Outputs the given characters to the underlying stream.
   */
  void writeChars (const std::string& name);

  /**
   * Outputs indentation whitespace.
   */
  void writeIndent (bool isEnd = false);

  /**
   * Outputs name.
   */
  void writeName (const std::string& name);

  /**
   * Outputs prefix:name.
   */
  void writeName (const XMLTriple& triple);

  /**
   * Outputs value in quotes.
   */
  void writeValue (const std::string& value);

  /**
   * Outputs "true" or "false" in quotes.
   */
  void writeValue (const bool& value);

  /**
   * Outputs the double value in quotes, or "INF", "-INF", or "NaN".
   */
  void writeValue (const double& value);

  /**
   * Outputs the long value in quotes.
   */
  void writeValue (const long& value);

  /**
   * Outputs the int value in quotes.
   */
  void writeValue (const int& value);

  /**
   * Outputs the int value in quotes.
   */
  void writeValue (const unsigned int& value);


  bool mInStart;
  bool mDoIndent;

  unsigned int mIndent;

  std::ostream& mStream;
  std::string   mEncoding;
};


#endif  /* XMLOutputStream_h */
