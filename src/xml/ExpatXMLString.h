/**
 * \file    ExpatXMLString.h
 * \brief   XMLString class
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


#ifndef ExpatXMLString_h
#define ExpatXMLString_h


#ifdef __cplusplus


#include <expat.h>


class XMLString
{
public:
  static XML_Char * replicate(const XML_Char * str);
  static void trim(XML_Char * str);
  static unsigned int stringLen(const XML_Char * str);
  static int indexOf(const XML_Char * str1, const XML_Char ch);
  static int compareString(const XML_Char * str1, const XML_Char * str2);
  static int compareIString(const XML_Char * str1, const XML_Char * str2);

  /**
   * Copies source, including the terminating null character, to the
   * location specified by target.
   *
   * No overflow checking is performed when strings are copied or
   * appended. The behavior of copyString is undefined if the source and
   * destination strings overlap.
   *
   * @param  target  destination string
   * @param  source  null-terminated source string
   */
  static void copyString (XML_Char* target, const XML_Char* source);

  /**
   * Copies source, upto a fixed number of characters, to the location
   * specified by target.
   *
   * No overflow checking is performed when strings are copied or
   * appended. The behavior of copyNString is undefined if the source and
   * destination strings overlap.
   *
   * @param  target  destination string
   * @param  source  null-terminated source string
   */
  static void copyNString (XML_Char*       target,
                           const XML_Char* source,
                           unsigned int    maxChars);

  static XML_Char * transcode(const XML_Char * str, const int & length = -1);
  static bool isAllWhiteSpace(const XML_Char * str, const int & length = -1);
  static void release(XML_Char ** str);
};


#endif /* __cplusplus */
#endif /* ExpatXMLString_h */
