/*
 * Filename    : StreamFormatTarget.hpp
 * Description : XMLFormatter class for stream output (source)
 * Author(s)   : Stefan Hoops <shoops@vt.edu>
 * Organization: Virginia Bioinformatics Institute
 * Created     : 2004-12-02
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
#ifndef StreamFormatTarget_hpp
#define StreamFormatTarget_hpp


#include <iostream>

#include "sbml/common.h"

#ifdef USE_EXPAT
#include <expat.h>
#include <ExpatFormatter.hpp>
#else
#include <xercesc/framework/XMLFormatter.hpp>
#endif


class StreamFormatTarget : public XMLFormatTarget
{
private:
  StreamFormatTarget ();

public:
  StreamFormatTarget (std::ostream & o);
  ~StreamFormatTarget();
#ifdef USE_EXPAT
  virtual XMLFormatTarget& operator<< (const char& ch);
  virtual XMLFormatTarget& operator<< (const char* str);
#else
  virtual void writeChars(const XMLByte* const toWrite,
                          const unsigned int   count,
                          XMLFormatter* const  formatter);
 
  virtual void flush();
#endif

private:
  std::ostream  & mO;
};

#endif // StreamFormatTarget_hpp