/*
 * Filename    : ExpatXMLString.h
 * Description : XMLString class
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


#ifndef ExpatXMLString_hpp
#define ExpatXMLString_hpp


#include <expat.h>


class XMLString
{
public:
  static XML_Char * replicate(const XML_Char * str);
  static void trim(XML_Char * str);
  static unsigned int stringLen(const XML_Char * str);
  static int compareString(const XML_Char * str1, const XML_Char * str2);
  static int compareIString(const XML_Char * str1, const XML_Char * str2);
  static XML_Char * transcode(const XML_Char * str, const int & length = -1);
  static bool isAllWhiteSpace(const XML_Char * str, const int & length = -1);
  static void release(XML_Char ** str);
};


#endif // ExpatXMLString_hpp
