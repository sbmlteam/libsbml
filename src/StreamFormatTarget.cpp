/*
 * Filename    : StreamFormatTarget.cpp
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

// ----------------------------------------------------------------------
// StreamFormatTarget
// ----------------------------------------------------------------------

#include "sbml/StreamFormatTarget.hpp"

#ifndef USE_EXPAT
XERCES_CPP_NAMESPACE_BEGIN
#endif

StreamFormatTarget::StreamFormatTarget ():
XMLFormatTarget(),
mO(std::cout)
{}

StreamFormatTarget::StreamFormatTarget (std::ostream & o):
XMLFormatTarget(),
mO(o)
{}
  
StreamFormatTarget::~StreamFormatTarget() {}

#ifdef USE_EXPAT
XMLFormatTarget&
StreamFormatTarget::operator<< (const char& ch)
{
  mO << ch;
  return *this;
}

XMLFormatTarget&
StreamFormatTarget::operator<< (const char* str)
{
  mO << str;
  return *this;
}

#else

void StreamFormatTarget::flush()
{
  mO.flush();
}

void StreamFormatTarget::writeChars(const XMLByte* const  toWrite
                                  , const unsigned int    count
                                  , XMLFormatter* const   formatter)
{
        // Surprisingly, Solaris was the only platform on which
        // required the char* cast to print out the string correctly.
        // Without the cast, it was printing the pointer value in hex.
        // Quite annoying, considering every other platform printed
        // the string with the explicit cast to char* below.
    mO.write((char *) toWrite, (int) count);
    mO.flush();

}
#endif

#ifndef USE_EXPAT
XERCES_CPP_NAMESPACE_END
#endif // !USE_EXPAT

