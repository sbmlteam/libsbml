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
 *    Ben Bornstein - Minor reformatting, modifications and integration
 *                    into the libsbml source tree.
 */


#include <cstring>
#include "ExpatAttributes.h"


Attributes::Attributes() :
    mpAttributes(0)
  , mSize(0)
{
}


Attributes::Attributes (const XML_Char** papszAttrs) :
    mpAttributes(papszAttrs)
  , mSize(0)
{
  while (mpAttributes[2 * mSize]) mSize++;
}


Attributes::~Attributes() {}


unsigned int
Attributes::getLength() const
{
  return mSize;
}


unsigned int
Attributes::getIndex (const XML_Char* name) const
{
  unsigned int index;


  for (index = 0; index < mSize; index++)
  {
    if ( !strcmp(mpAttributes[2 * index], name) )
    {
      return index;
    }
  }
  
  return InvalidIndex;
}


const XML_Char*
Attributes::getValue (const XML_Char* const name) const
{
  return getValue( getIndex(name) );
}


const XML_Char*
Attributes::getValue (const unsigned int index) const
{
  return (index < mSize) ? mpAttributes[2 * index + 1] : "";
}


const XML_Char*
Attributes::getQName (const unsigned int index) const
{
  return mpAttributes[2 * index];
}
