/**
 * \file    local.cpp
 * \brief   Java-specific SWIG support code for wrapping libSBML API
 * \author  Ben Bornstein
 *
 * $Id$
 * $HeadURL$
 */
/* Copyright 2004 California Institute of Technology and
 * Japan Science and Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 2.1 of the License, or
 * any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY, WITHOUT EVEN THE IMPLIED WARRANTY OF
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  The software and
 * documentation provided hereunder is on an "as is" basis, and the
 * California Institute of Technology and Japan Science and Technology
 * Corporation have no obligations to provide maintenance, support,
 * updates, enhancements or modifications.  In no event shall the
 * California Institute of Technology or the Japan Science and Technology
 * Corporation be liable to any party for direct, indirect, special,
 * incidental or consequential damages, including lost profits, arising
 * out of the use of this software and its documentation, even if the
 * California Institute of Technology and/or Japan Science and Technology
 * Corporation have been advised of the possibility of such damage.  See
 * the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 *
 * The original code contained here was initially developed by:
 *
 *     Ben Bornstein and Ben Kovitz
 *     
 *     The SBML Team
 *     Control and Dynamical Systems, MC 107-81
 *     California Institute of Technology
 *     Pasadena, CA, 91125, USA
 *
 *     http://sbml.org
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s):
 */


/**
 * This file must exist because ../swig/libsbml.i %includes it.
 */

//--------------------------------------------------------------------------------
// (Currently (2008-07-25), this file is used only for Windows)
//
// Utility functions for converting encodings between Unicode (wide char), 
// UTF8 and ANSI CP (multibyte char)
//
// 1) char*    convertUnicodeToUTF8(const wchar_t* src_wstr) 
// 2) char*    convertUnicodeToACP(const wchar_t* src_wstr) 
// 3) wchar_t* convertUTF8ToUnicode(const char* src_str) 
// 4) char*    convertACPToUTF8(const char* src_str) 
// 5) char*    convertUTF8ToACP(const char* src_str) 
//
//--------------------------------------------------------------------------------

#ifdef WIN32

#include <windows.h>
#include <winnls.h>

//
// convert Unicode -> UTF8 (for Windows)
//
char* convertUnicodeToUTF8(const wchar_t* src_wstr) 
{
  int    length;
  int    clength;
  char*  sbuf;

  //
  // Unicode -> UTF8
  //

  //
  // Check wbuf length		
  //
  length = WideCharToMultiByte(CP_UTF8, 0, (LPCWSTR)src_wstr, -1, NULL, 0, NULL, NULL);

  if(length == 0){
    return NULL;
  }

  sbuf = new char[length+1];

  //
  // Convert
  //
  clength = WideCharToMultiByte(CP_UTF8, 0, (LPCWSTR)src_wstr, -1, sbuf, length, NULL, NULL);
  sbuf[clength] = 0;

  if(clength == 0){
    delete [] sbuf;
    return NULL;
  }

  return sbuf;
}


//
// convert Unicode -> ANSI CP (for Windows)
//
char* convertUnicodeToACP(const wchar_t* src_wstr) 
{
  int    length;
  int    clength;
  char*  sbuf;

  //
  // Unicode -> ANSI CP
  //

  //
  // Check wbuf length		
  //
  length = WideCharToMultiByte(CP_ACP, 0, (LPCWSTR)src_wstr, -1, NULL, 0, NULL, NULL);

  if(length == 0){
    return NULL;
  }

  sbuf = new char[length+1];

  //
  // Convert
  //
  clength = WideCharToMultiByte(CP_ACP, 0, (LPCWSTR)src_wstr, -1, sbuf, length, NULL, NULL);
  sbuf[clength] = 0;

  if(clength == 0){
    delete [] sbuf;
    return NULL;
  }

  return sbuf;
}


//
// convert UTF8 -> Unicode (for Windows)
//
wchar_t* convertUTF8ToUnicode(const char* src_str) 
{
  int      length;
  int      c_length;
  wchar_t* wbuf;

  //
  // UTF8 -> Unicode
  //

  // Check src_str length
  length = MultiByteToWideChar(CP_UTF8, 0, (LPCSTR)src_str, -1, NULL, 0);
  if(length == 0){
    return NULL;
  }

  wbuf = new wchar_t[length+1];

  // Convert
  c_length = MultiByteToWideChar(CP_UTF8, 0, (LPCSTR)src_str, -1, wbuf, length);
  wbuf[c_length] = 0;
  if(c_length == 0) {
    delete [] wbuf;
    return NULL;
  }

  return wbuf;

}


//
// convert ANSI CP -> UTF8  for Windows
//
char* convertACPToUTF8(const char* src_str) 
{
  int      length;
  int      c_length;
  wchar_t* wbuf;
  char*    ubuf;

  //
  // ANSI CP -> Unicode
  //
  
  // Check src_str length
  length = MultiByteToWideChar(CP_ACP, 0, (LPCSTR)src_str, -1, NULL, 0);
  if(length == 0){
  	return NULL;
  }

  wbuf = new wchar_t[length+1];

  // Convert
  c_length = MultiByteToWideChar(CP_ACP, 0,(LPCSTR)src_str,-1,wbuf,length);
  wbuf[c_length] = 0;

  if(c_length == 0) {
    delete [] wbuf;
    return NULL;
  }

  //
  // Unicode -> UTF8
  //

  // Check wbuf length		
  length = WideCharToMultiByte(CP_UTF8, 0, (LPCWSTR)wbuf,-1,NULL,0,NULL,NULL);

  if(length == 0){
    delete [] wbuf;
    return NULL;
  }

  ubuf = new char[length+1];

  // Convert
  c_length = WideCharToMultiByte(CP_UTF8, 0, (LPCWSTR)wbuf,-1,ubuf,length,NULL,NULL);
  ubuf[c_length] = 0;

  if(c_length == 0){
    delete [] wbuf;
    delete [] ubuf;
    return NULL;
  }

  delete [] wbuf;
  return ubuf;

}


//
// convert UTF8 -> ANSI CP  for Windows
//
char* convertUTF8ToACP(const char* src_str) 
{
  int      length;
  int      c_length;
  wchar_t* wbuf;
  char*    ubuf;

  //
  // UTF8 -> Unicode
  //

  // Check src_str length
  length = MultiByteToWideChar(CP_UTF8, 0, (LPCSTR)src_str, -1, NULL, 0);
  if(length == 0){
    return NULL;
  }

  wbuf = new wchar_t[length+1];

  // Convert
  c_length = MultiByteToWideChar(CP_UTF8, 0,(LPCSTR)src_str,-1,wbuf,length);
  wbuf[c_length] = 0;
  if(c_length == 0) {
    delete [] wbuf;
    return NULL;
  }

  //
  // Unicode -> ANSI CP
  //

  // Check wbuf length		
  length = WideCharToMultiByte(CP_ACP, 0, (LPCWSTR)wbuf,-1,NULL,0,NULL,NULL);

  if(length == 0){
    delete [] wbuf;
    return NULL;
  }

  ubuf = new char[length+1];

  // Convert
  c_length = WideCharToMultiByte(CP_ACP, 0, (LPCWSTR)wbuf,-1,ubuf,length,NULL,NULL);
  ubuf[c_length] = 0;

  if(c_length == 0){
    delete [] wbuf;
    delete [] ubuf;
    return NULL;
  }

  delete [] wbuf;
  return ubuf;

}

#endif // WIN32
