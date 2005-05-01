/**
 * \file    ExpatAttributes.cpp
 * \brief   Attributes wrapper class
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


#include <string>
#include <map>

#include <cstring>

#include "XMLUtil.h"
#include "ExpatAttributes.h"


using namespace std;


Attributes::Attributes () :
    mpAttributes( 0 )
  , mSepChar    ( chSpace )
  , mSize       ( 0 )
  , mURIs       ( 0 )
  , mLocalNames ( 0 )
  , mPrefixes   ( 0 )
{
}


/**
 * Creates a new Xerces-C++ Attributes that to wrap the given "raw" Expat
 * attributes.  The Expat attribute names are assumed to be in namespace
 * triplet form separated by sepChar.
 */
Attributes::Attributes (const XML_Char** papszAttrs, XML_Char sepChar) :
    mpAttributes( const_cast<XML_Char**>(papszAttrs) )
  , mSepChar    ( sepChar )
  , mSize       ( 0 )
  , mURIs       ( 0 )
  , mLocalNames ( 0 )
  , mPrefixes   ( 0 )
{
  if (mpAttributes)
  {
    while (mpAttributes[2 * mSize]) mSize++;

    if (mSize)
    {
      mURIs       = new XML_Char*[mSize];
      mLocalNames = new XML_Char*[mSize];
      mPrefixes   = new XML_Char*[mSize];

      //
      // Split each attribute name into (up to) three parts and store the
      // start of each part in mURIs[n], mLocalNames[n], and mPrefixes[n]
      // repsectively.
      //
      for (unsigned int n = 0; n < mSize; ++n)
      {
        XMLUtil::splitNamespaceTriplets(   mpAttributes[2 * n]
                                         , & mURIs      [n]
                                         , & mLocalNames[n]
                                         , & mPrefixes  [n]
                                         , mSepChar );
      }
    }
  }
}


/**
 * Destroys this Attribute set.
 */
Attributes::~Attributes ()
{
  for (unsigned int n = 0; n < mSize; ++n)
  {
    XMLUtil::unsplitNamespaceTriplets(   & mURIs      [n]
                                       , & mLocalNames[n]
                                       , & mPrefixes  [n]
                                       , mSepChar );
  }

  delete [] mURIs;
  delete [] mLocalNames;
  delete [] mPrefixes;
}


/**
 * @return the number of attributes in this list.
 */
unsigned int
Attributes::getLength() const
{
  return mSize;
}


/**
 * @return the namespace URI of an attribute in this list (by position),
 * or NULL if index is out of range.
 */
const XML_Char*
Attributes::getURI (const unsigned int index) const
{
  return (index < 0 || index >= mSize) ? 0 : mURIs[index];
}


/**
 * @return the local name of an attribute in this list (by position),
 * or NULL if index is out of range.
 */
const XML_Char*
Attributes::getLocalName (const unsigned int index) const
{
  return (index < 0 || index >= mSize) ? 0 : mLocalNames[index];
}


/**
 * @return the namespace prefix qualified name of an attribute in this list
 * (by position), or NULL if the index is out of range.
 *
 * QName lookups are rare, and are not provided by Expat, so this
 * implementation creates them on demand and then caches them.
 */
const XML_Char*
Attributes::getQName (const unsigned int index) const
{
  if (index < 0 || index >= mSize) return 0;


  IndexNameMap::iterator pos = mQNames.find(index);

  if (pos == mQNames.end())
  {
    string qname;

    if (mPrefixes[index])
    {
      qname.append(mPrefixes[index]);
      qname.append(":");
    }

    qname.append(mLocalNames[index]);

    pos = mQNames.insert( make_pair(index, qname) ).first;
  }

  return pos->second.c_str();
}


/**
 * Lookup the index of an attribute by name.
 *
 * @return the index of the attribute, or -1 if it does not appear in the
 * list.
 */
int
Attributes::getIndex (const XML_Char* name) const
{
  for (int index = 0; index < mSize; ++index)
  {
    if ( !strcmp(mLocalNames[index], name) ) return index;
  }
  
  return -1;
}


/**
 * Lookup an Attributes value by name.
 *
 * @return The attribute value as a string or NULL if the attribute is
 * not in the list.
 */
const XML_Char*
Attributes::getValue (const XML_Char* const name) const
{
  return getValue( getIndex(name) );
}


/**
 * @return the value of an attribute in the list (by position), or NULL
 * if index is out of range.
 */
const XML_Char*
Attributes::getValue (const unsigned int index) const
{
  return (index < 0 || index >= mSize) ? 0 : mpAttributes[2 * index + 1];
}
