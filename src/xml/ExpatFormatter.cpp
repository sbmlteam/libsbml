/**
 * \file    ExpatFormatter.cpp
 * \brief   XMLFormatter class for output (source)
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
 *
 */


#include <iostream>
#include <expat.h>

#include "ExpatFormatter.h"


XMLFormatter::XMLFormatter():
  mpTarget(NULL),
  mEscapeFlag(XMLFormatter::NoEscapes),
  mpF(&XMLFormatter::writeUTF8),
  mEncoding("UTF-8"),
  mByteCounter(0)
{
  memset(mByteBuffer, 0x0, sizeof(mByteBuffer));
}

  
XMLFormatter::XMLFormatter (const XML_Char    * encoding,
                            XMLFormatTarget   * target,
                            const EscapeFlags & escapeFlag,
                            const UnRepFlags  & unRepFlag):
  mpTarget(target),
  mEncoding(encoding),
  mByteCounter(0)
{
  operator<<(escapeFlag);
  memset(mByteBuffer, 0x0, sizeof(mByteBuffer));
}

  
XMLFormatter::~XMLFormatter() {}


XMLFormatter&
XMLFormatter::operator << (const EscapeFlags & escapeFlag)
{
  mEscapeFlag = escapeFlag;

  switch (mEscapeFlag)
  {
    case StdEscapes:
      mpF = &XMLFormatter::stdEscape;
      break;

    case AttrEscapes:
      mpF = &XMLFormatter::attrEscape;
      break;

    case CharEscapes:
      mpF = &XMLFormatter::charEscape;
      break;

    default:
      mpF = &XMLFormatter::writeUTF8;
      break;
  }

  return *this;
}


XMLFormatter&
XMLFormatter::operator << (const XML_Char & chr)
{
  (this->*mpF)(chr);
  return *this;
}


XMLFormatter&
XMLFormatter::operator << (const XML_Char * chars)
{
  unsigned int i, imax;

  for (i = 0, imax = strlen(chars); i < imax; i++) (this->*mpF)(chars[i]);

  return *this;
}

void XMLFormatter::formatBuf(const XML_Char* const & chars,
                             const unsigned int & len,
                             const EscapeFlags & escapeFlags,
                             const UnRepFlags & unRepFlags)
{
  *this << escapeFlags;
  
  unsigned int i;
  const XML_Char *tmp = chars;
  
  for (i = 0; i < len; i++, tmp++) *this << *tmp;
}


const XML_Char*
XMLFormatter::getEncodingName() const 
{
  return mEncoding.c_str();
}


void
XMLFormatter::stdEscape (const XML_Char& c)
{
  switch (c)
  {
    case '&':
      if (mByteCounter) flushBuffer();
      *mpTarget << "&amp;";
      break;
      
    case '\'':
      if (mByteCounter) flushBuffer();
      *mpTarget << "&apos;";
      break;
      
    case '<':
      if (mByteCounter) flushBuffer();
      *mpTarget << "&lt;";
      break;
      
    case '>':
      if (mByteCounter) flushBuffer();
      *mpTarget << "&gt;";
      break;
      
    case '\"':
      if (mByteCounter) flushBuffer();
      *mpTarget << "&quot;";
      break;
      
    default:
      writeUTF8(c);
      break;
  }
}


void
XMLFormatter::attrEscape (const XML_Char& c)
{
  switch (c)
  {
    case '&':
      if (mByteCounter) flushBuffer();
      *mpTarget << "&amp;";
      break;
      
    case '<':
      if (mByteCounter) flushBuffer();
      *mpTarget << "&lt;";
      break;
      
    case '\"':
      if (mByteCounter) flushBuffer();
      *mpTarget << "&quot;";
      break;
      
    default:
      writeUTF8(c);
      break;
  }
}  


void
XMLFormatter::charEscape (const XML_Char& c)
{
  switch (c)
  {
    case '&':
      if (mByteCounter) flushBuffer();
      *mpTarget << "&amp;";
      break;
      
    case '<':
      if (mByteCounter) flushBuffer();
      *mpTarget << "&lt;";
      break;
      
    default:
      writeUTF8(c);
      break;
  }
}  

void
XMLFormatter::writeUTF8 (const XML_Char& c)
{
  /* Based on RFC 2279. */
  /* This will work for 0x00 - 0xff */

  if (mByteCounter && (c & 0xc0) == 0x80)
  {
    mByteBuffer[mByteCounter--] = c;
    if (!mByteCounter) flushBuffer(true);

    return;
  }
  
  if (mByteCounter) flushBuffer();

  if ((unsigned char) c < 0x80)
  {
    *mpTarget << c;
  }
  else if ((c & 0xe0) == 0xc0) 
  {
    mByteBuffer[0] = c;
    mByteCounter = 1;
  }
  else if ((c & 0xf0) == 0xe0)
  {
    mByteBuffer[0] = c;
    mByteCounter = 2;    
  }
  else
  {
    *mpTarget << (char) (0xc0 + ((c >> 6) & 0x03));
    *mpTarget << (char) (0x80 + (c & 0x3f));
  }
}

void
XMLFormatter::flushBuffer(const bool & isMultiByte)
{
  XML_Char * tmp = mByteBuffer;

  for (tmp = mByteBuffer; *tmp; tmp++) 
  {
    if (isMultiByte || (unsigned char) *tmp < 0x80)
    {
      *mpTarget << *tmp;
    }
    else
    {
      *mpTarget << (char) (0xc0 + ((*tmp >> 6) & 0x03));
      *mpTarget << (char) (0x80 + (*tmp & 0x3f));
    }

    *tmp = 0x0;
  }

  mByteCounter = 0;
}




// ----------------------------------------------------------------------
// XMLFormatTarget
// ----------------------------------------------------------------------

XMLFormatTarget::XMLFormatTarget()
{
}




// ----------------------------------------------------------------------
// LocalFileFormatTarget
// ----------------------------------------------------------------------


LocalFileFormatTarget::LocalFileFormatTarget ():
  XMLFormatTarget()
{
}


LocalFileFormatTarget::LocalFileFormatTarget (const char * filename):
    XMLFormatTarget()
  , stream(filename)
{
}


XMLFormatTarget&
LocalFileFormatTarget::operator<< (const char& ch)
{
  stream << ch;
  return *this;
}


XMLFormatTarget&
LocalFileFormatTarget::operator<< (const char* str)
{
  stream << str;
  return *this;
}




// ----------------------------------------------------------------------
// MemBufFormatTarget
// ----------------------------------------------------------------------

MemBufFormatTarget::MemBufFormatTarget():
    XMLFormatTarget()
  , mSize(1024)
  , mpBuffer(new char[mSize])
{
}


MemBufFormatTarget::~MemBufFormatTarget()
{
  delete [] mpBuffer;
}


const char * MemBufFormatTarget::getRawBuffer() const
{
  unsigned int size = stream.str().length() + 1;


  if (mSize < size)
  {
    delete [] mpBuffer;

    mSize    = size;
    mpBuffer = new char[mSize];
  }
  
  strcpy(mpBuffer, stream.str().c_str());

  return mpBuffer;
}


unsigned int
MemBufFormatTarget::getLen() const
{
  return stream.str().length();
}


void
MemBufFormatTarget::reset()
{
  stream.str("");
}


XMLFormatTarget&
MemBufFormatTarget::operator<< (const char& ch)
{
  stream << ch;
  return *this;
}


XMLFormatTarget&
MemBufFormatTarget::operator<< (const char* str)
{
  stream << str;
  return *this;
}
