/**
 * \file    ExpatXMLString.cpp
 * \brief   XMLString class
 * \author  Stefan Hoops <shoops@vt.edu>
 *
 * $Id$
 * Source$
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
#include <cstdlib>

#include "ExpatXMLString.h"


XML_Char* 
XMLString::replicate (const XML_Char* str)
{
  return (str) ? strdup(str) : NULL;
}


void
XMLString::trim (XML_Char* str)
{
  if (!str) return;

  /* Whitespace are: 0x20 | 0x9 | 0xD | 0xA */

  /* strip leading white spaces */
  XML_Char * tmp1 = str;
  while (*tmp1 &&
         (*tmp1 == 0x20 || *tmp1 == 0x9 || *tmp1 == 0xD || *tmp1 == 0xA))
         tmp1++;

  /* strip trailing white spaces */
  XML_Char * tmp2 = str + strlen(str) - 1;
  while (tmp2 >= tmp1 &&
         (*tmp2 == 0x20 || *tmp2 == 0x9 || *tmp2 == 0xD || *tmp2 == 0xA))
    tmp2--;

  /* calculate length of trimmed string */
  unsigned int len = tmp2 - tmp1 + 1;

  /* copy trimmed string */
  unsigned int i;
  for (i = 0, tmp2 = str; i < len; i++, tmp1++, tmp2++) *tmp2 = *tmp1;
  
  /* terminate */
  *tmp2 = '\0';

  return;
}


unsigned int
XMLString::stringLen (const XML_Char* str)
{
  return str ? strlen(str) : 0;
}


int
XMLString::compareString (const XML_Char* str1, const XML_Char* str2)
{
  return strcmp(str1, str2);
}


int
XMLString::indexOf (const XML_Char * str1, const XML_Char ch)
{
  char* pos = strchr(str1, ch);
  return pos ? pos - str1 : -1;
}


int
XMLString::compareIString (const XML_Char* str1, const XML_Char* str2)
{
#if (defined(WIN32) && !defined(__CYGWIN__))
  return _stricmp(str1, str2);
#else
  return strcasecmp(str1, str2);
#endif
}


/**
 * Copies source, including the terminating null character, to the location
 * specified by target.
 *
 * No overflow checking is performed when strings are copied or
 * appended. The behavior of copyString is undefined if the source and
 * destination strings overlap.
 *
 * @param  target  destination string
 * @param  source  null-terminated source string
 */
void
XMLString::copyString (XML_Char* target, const XML_Char* source)
{
  strcpy(target, source);
}


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
void
XMLString::copyNString (XML_Char*       target,
                        const XML_Char* source,
                        unsigned int    maxChars)
{
  strncpy(target, source, maxChars);
}


XML_Char*
XMLString::transcode (const XML_Char* str, const int& length)
{
  /* We are currently doing nothing here */
  if (str)
  {
    if (length < 0)
    {
      return strdup(str);
    }
    else
    {
      char* tmp   = (char*) malloc( (length + 1) * sizeof(char) );
      memcpy(tmp, str, length);
      tmp[length] = 0;
      return tmp;
    }
  }
  else
  {
    return NULL;
  }
}


bool
XMLString::isAllWhiteSpace (const XML_Char* str, const int& length)
{
  /* Whitespace are: 0x20 | 0x9 | 0xD | 0xA */
  int Length = length;
  if (Length < 0) Length = strlen(str);

  const XML_Char *tmp = str;
  int i = 0;
  while (i < Length && 
         (*tmp == 0x20 || *tmp == 0x9 || *tmp == 0xD || *tmp == 0xA))
    {
       i++;
       tmp++;
    }

  if (i != Length) return false;
  else return true;
}


void
XMLString::release (XML_Char** str)
{
  if (*str)
  {
    free(*str);
    *str = NULL;
  }
}
