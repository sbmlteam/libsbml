/*
 * Filename    : ExpatFormatter.cpp
 * Description : XMLFormatter class for output (source)
 * Author(s)   : Stefan Hoops <shoops@vt.edu>
 * Organization: Virginia Bioinformatics Institute
 * Created     : 2003-08-29
 * Revision    : $Id$
 * Source      : $Source$
 * 
 * Copyright (c) 2003 Stefan Hoops
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

#include "ExpatFormatter.hpp"


XMLFormatter::XMLFormatter():
  mpTarget(NULL),
  mEscapeFlag(XMLFormatter::NoEscapes),
  mpF(&XMLFormatter::writeUTF8),
  mEncoding("UTF-8")
{
}

  
XMLFormatter::XMLFormatter (const XML_Char    * encoding,
                            XMLFormatTarget   * target,
                            const EscapeFlags & escapeFlag,
                            const UnRepFlags  & unRepFlag):
  mpTarget(target),
  mEncoding(encoding)
{
  operator<<(escapeFlag);
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
      *(std::ostream *)mpTarget << "&amp;";
      break;
      
    case '\'':
      *(std::ostream *)mpTarget << "&apos;";
      break;
      
    case '<':
      *(std::ostream *)mpTarget << "&lt;";
      break;
      
    case '>':
      *(std::ostream *)mpTarget << "&gt;";
      break;
      
    case '\"':
      *(std::ostream *)mpTarget << "&quot;";
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
      *(std::ostream *)mpTarget << "&amp;";
      break;
      
    case '<':
      *(std::ostream *)mpTarget << "&lt;";
      break;
      
    case '\"':
      *(std::ostream *)mpTarget << "&quot;";
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
      *(std::ostream *)mpTarget << "&amp;";
      break;
      
    case '<':
      *(std::ostream *)mpTarget << "&lt;";
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

  if ((unsigned char) c < 0x80)
  {
    *(std::ostream *)mpTarget << c;
  }
  else
  {
    *(std::ostream *)mpTarget << (char) (0xc0 + ((c >> 6) & 0x03));
    *(std::ostream *)mpTarget << (char) (0x80 + (c & 0x3f));
  }
}


XMLFormatTarget::XMLFormatTarget() {}


LocalFileFormatTarget::LocalFileFormatTarget():
  XMLFormatTarget(),
  std::ofstream()
{}


LocalFileFormatTarget::LocalFileFormatTarget(const char * filename):
  XMLFormatTarget(),
  std::ofstream(filename)
{}


MemBufFormatTarget::MemBufFormatTarget():
  XMLFormatTarget(),
  std::ostringstream(),
  mSize(1024),
  mpBuffer(new char[mSize])
{}


MemBufFormatTarget::~MemBufFormatTarget() {delete [] mpBuffer;}


const char * MemBufFormatTarget::getRawBuffer() const
{
  unsigned int size = str().length() + 1;
  if (mSize < size)
    {
      delete [] mpBuffer;
      const_cast<MemBufFormatTarget *>(this)->mSize = size;
      const_cast<MemBufFormatTarget *>(this)->mpBuffer = new char[mSize];
    }
  
  strcpy(mpBuffer, str().c_str());
  return mpBuffer;
}


unsigned int
MemBufFormatTarget::getLen() const
{
  return str().length();
}


void
MemBufFormatTarget::reset()
{
  str("");
}
