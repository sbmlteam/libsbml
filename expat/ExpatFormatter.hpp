/*
 * Filename    : ExpatFormatter.hpp
 * Description : XMLFormatter class for output (header)
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


#ifndef ExpatFormatter_hpp
#define ExpatFormatter_hpp


#include <fstream>
#include <sstream>
#include <expat.h>


class XMLFormatTarget
{
public:
  virtual XMLFormatTarget& operator<< (const char& ch)  = 0;
  virtual XMLFormatTarget& operator<< (const char* str) = 0;

protected:
  XMLFormatTarget();
};


class LocalFileFormatTarget : public XMLFormatTarget
{
public:
  LocalFileFormatTarget(const char * filename);

  virtual XMLFormatTarget& operator<< (const char& ch);
  virtual XMLFormatTarget& operator<< (const char* str);


protected:
  LocalFileFormatTarget();

private:
  std::ofstream stream;
};


class MemBufFormatTarget : public XMLFormatTarget
{
public:
  MemBufFormatTarget ();
  ~MemBufFormatTarget();

  const char*  getRawBuffer() const;
  unsigned int getLen      () const;
  void         reset       ();

  virtual XMLFormatTarget& operator<< (const char& ch);
  virtual XMLFormatTarget& operator<< (const char* str);

private:
  mutable unsigned int mSize;
  mutable char*        mpBuffer;
  std::ostringstream   stream;
};


class XMLFormatter
{
public:

  enum EscapeFlags 
  {
    NoEscapes,
    StdEscapes,
    AttrEscapes,
    CharEscapes,
    EscapeFlags_Count,
    DefaultEscape = 999
  };

  enum UnRepFlags 
  {
    UnRep_Fail,
    UnRep_CharRef,
    UnRep_Replace,
    DefaultUnRep = 999
  };


private:

  XMLFormatTarget* mpTarget;
  EscapeFlags      mEscapeFlag;
  std::string      mEncoding;

  void (XMLFormatter::*mpF)(const XML_Char & chr);


protected:

  XMLFormatter();

  
public:

  XMLFormatter(const XML_Char * encoding,
               XMLFormatTarget * target,
               const EscapeFlags & escapeFlags = NoEscapes,
               const UnRepFlags & unRepFlags = UnRep_Fail);
  
  ~XMLFormatter();
  XMLFormatter & operator << (const EscapeFlags & escapeFlag);
  XMLFormatter & operator << (const XML_Char & chr);
  XMLFormatter & operator << (const XML_Char * chars);
  void formatBuf(const XML_Char* const & chars,
                 const unsigned int & len,
                 const EscapeFlags & escapeFlags= DefaultEscape,
                 const UnRepFlags & unRepFlags = DefaultUnRep);
  const XML_Char * getEncodingName() const;


private: 

  void stdEscape(const XML_Char & chr);
  void attrEscape(const XML_Char & chr);
  void charEscape(const XML_Char & chr);

  void writeUTF8(const XML_Char & chr);
};


#endif  // ExpatFormatter_hpp
