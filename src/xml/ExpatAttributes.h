/**
 * \file    ExpatAttributes.hpp
 * \breif   Attributes wrapper class
 * \author  Stefan Hoops <shoops@vt.edu>
 *
 * $Id$
 * $Source$
 */
/* Copyright (c) 2003 Stefan Hoops
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 * 
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 *  Contributor(s):
 *    Ben Bornstein
 */


#ifndef ExpatAttributes_h
#define ExpatAttributes_h


#ifdef __cplusplus


#include <string>
#include <map>

#include <expat.h>

#include "common.h"
#include "ExpatUnicodeChars.h"


class Attributes
{
public:

  /**
   * Creates a new Xerces-C++ Attributes that to wrap the given "raw" Expat
   * attributes.  The Expat attribute names are assumed to be in namespace
   * triplet form separated by sepChar.
   */
  Attributes (const XML_Char** pAttrs, XML_Char sepChar = chSpace);

  /**
   * Destroys this Attribute set.
   */
  virtual ~Attributes ();


  /**
   * @return the number of attributes in this list.
   */
  virtual unsigned int getLength () const;

  /**
   * @return the namespace URI of an attribute in this list (by position),
   * or NULL if index is out of range.
   */
  virtual const XML_Char* getURI (const unsigned int index) const;

  /**
   * @return the local name of an attribute in this list (by position),
   * or NULL if index is out of range.
   */
  virtual const XML_Char* getLocalName (const unsigned int index) const;

  /**
   * @return the namespace prefix qualified name of an attribute in this
   * list (by position), or NULL if the index is out of range.
   */
  virtual const XML_Char* getQName (const unsigned int index) const;

  /**
   * Lookup the index of an attribute by XML 1.0 qualified name.
   *
   * @return the index of the attribute, or -1 if it does not appear in the
   * list.
   */
  virtual int getIndex (const XML_Char* const qname) const;

  /**
   * Lookup an Attributes value by XML 1.0 qualified name.
   *
   * @return The attribute value as a string or NULL if the attribute is
   * not in the list.
   */
  virtual const XML_Char* getValue (const XML_Char* qname) const;

  /**
   * @return the value of an attribute in the list (by position), or NULL
   * if index is out of range.
   */
  virtual const XML_Char* getValue (const unsigned int index) const;


protected:

  Attributes();


private:

  XML_Char**   mpAttributes;
  XML_Char     mSepChar;
  unsigned int mSize;

  /* Points to the respective parts of each attribute name. */
  XML_Char** mURIs;
  XML_Char** mLocalNames;
  XML_Char** mPrefixes;

  typedef std::map<unsigned int, std::string> IndexNameMap;
  mutable IndexNameMap mQNames;
};


#endif  /* __cplusplus */
#endif  /* ExpatAttributes_h */
