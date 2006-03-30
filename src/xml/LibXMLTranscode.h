/**
 * \file    LibXMLTranscode.h
 * \brief   Transcodes a LibXML xmlChar string to UTF-8.
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


#ifndef LibXMLTranscode_h
#define LibXMLTranscode_h


#include <string>
#include <libxml/parser.h>


/**
 * Transcodes a LibXML xmlChar* string to UTF-8.  This class offers
 * implicit conversion to a C++ string.
 */
class LibXMLTranscode
{
public:

  LibXMLTranscode (const xmlChar* s, int len = -1) :
    mBuffer(reinterpret_cast<const char*>(s)), mLen(len) { }

  ~LibXMLTranscode () { }

  operator std::string ()
  {
    if (mBuffer == 0)
      return "";
    else
      return (mLen == -1) ? std::string(mBuffer) : std::string(mBuffer, mLen);
  }


private:

  const char* mBuffer;
  int         mLen;

  LibXMLTranscode  ();
  LibXMLTranscode  (const LibXMLTranscode&);
  LibXMLTranscode& operator= (const LibXMLTranscode&);

};


#endif  /* LibXMLTranscode_h */
