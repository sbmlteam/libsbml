/*
 * Filename    : ExpatAttributes.hpp
 * Description : Attributes wrapper class
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
 *    Ben Bornstein - Minor reformatting, modifications and integration
 *                    into the libsbml source tree.
 */


#ifndef ExpatAttributes_hpp
#define ExpatAttributes_hpp


#include <expat.h>


class Attributes
{
public:
   Attributes(const XML_Char** pAttrs);
  ~Attributes();

  virtual unsigned int getLength () const;
  virtual unsigned int getIndex  (const XML_Char* const name) const;

  virtual const XML_Char* getValue(const XML_Char*    name)  const;
  virtual const XML_Char* getValue(const unsigned int index) const;
  virtual const XML_Char* getQName(const unsigned int index) const;

  //
  // For the statement:
  //
  //   static const unsigned int InvalidIndex = (unsigned int) -1;
  //
  // MSVC++ 6.0 complains: "error C2258: illegal pure syntax, must be '=
  // 0'", but g++ has no problem with it?!  Fine.  For now, just #define.
  //
#define InvalidIndex ((unsigned int) -1)


protected:
  Attributes();


private:
  const XML_Char** mpAttributes;
  unsigned int     mSize;
};


#endif  // ExpatAttributes_hpp
