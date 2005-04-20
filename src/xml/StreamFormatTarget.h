/*
 * \file    StreamFormatTarget.h
 * \brief   XMLFormatter class for stream output (source)
 * \author  Ben Bornstein
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
 *    Stefan Hoops
 */


#ifndef StreamFormatTarget_h
#define StreamFormatTarget_h


#ifdef __cplusplus


#include <iosfwd>
#include "common.h"


#ifdef USE_EXPAT
#  include <expat.h>
#  include "ExpatFormatter.h"
#else
#  include <xercesc/framework/XMLFormatter.hpp>
#  include <xercesc/util/PlatformUtils.hpp>
   using xercesc::XMLFormatter;
   using xercesc::XMLFormatTarget;
#endif


class StreamFormatTarget : public XMLFormatTarget
{
public:

  StreamFormatTarget (std::ostream& o);
  ~StreamFormatTarget();

#ifdef USE_EXPAT
  virtual XMLFormatTarget& operator<< (const char& ch);
  virtual XMLFormatTarget& operator<< (const char* str);
#else
  virtual void writeChars(const XMLByte* const toWrite,
                          const unsigned int   count,
                          XMLFormatter*  const formatter);
 
  virtual void flush();
#endif  /* USE_EXPAT */


private:

  StreamFormatTarget ();
  std::ostream& mO;
};


#endif  /* __cplusplus */
#endif  /* StreamFormatTarget_h */
