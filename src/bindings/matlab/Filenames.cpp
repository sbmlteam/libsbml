/**
 * \file    TranslateSBML.cpp
 * \brief   MATLAB code for translating SBML document into MATLAB structure
 * \author  Sarah Keating
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2016 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *  
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 *     Pasadena, CA, USA 
 *  
 * Copyright (C) 2002-2005 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. Japan Science and Technology Agency, Japan
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ---------------------------------------------------------------------- -->*/

#include <stdio.h>
#include <string.h>

#include <mex.h>

#ifndef USE_OCTAVE
#include <matrix.h>
#endif

////////////////////////////////////////////////////////////////////////////
//
// ensure reading of unicode filenames

#if defined(WIN32) && !defined(CYGWIN) && !defined(USE_OCTAVE)
#define FILE_CHAR wchar_t*
#define FILE_FOPEN(file) _wfopen(file, L"r")
#define USE_FILE_WCHAR 1
#else 
#define FILE_CHAR char*
#define FILE_FOPEN(file) fopen(file, "r")
#endif

#ifndef uint16_t
#define uint16_t unsigned short
#endif


FILE_CHAR readUnicodeString(const mxArray *prhs, mwSize length)
{
#ifdef USE_OCTAVE
  char* ansii = (char *) mxCalloc(length, sizeof(char));
  mxGetString(prhs, ansii, length);
  return ansii;
#else   
  wchar_t* utf16 = (wchar_t *) mxCalloc(length, sizeof(wchar_t));
  char* utf8 = NULL;
  uint16_T *ch = (uint16_T *) mxGetData(prhs);
  wchar_t* p = utf16;
  mwSize i;
  for ( i = 0; i < length-1; ++i)
    *p++ = *ch++;
  *p = 0;

#if USE_FILE_WCHAR
  return utf16;
#else

  utf8 = (char*)mxCalloc(length*2, sizeof(char));

  wcstombs(utf8, utf16, length*2);

  /*mxFree(utf16);*/

  if (utf8 != NULL && strlen(utf8) == 0 && length > 0)
  {
    reportError("readUnicodeString", 
      "This string uses characters that cannot be "
      "expressed in UTF8, please rename the file.");
  }

  return utf8;
#endif /* USE_FILE_WCHAR */ 

#endif /* USE_OCTAVE*/ 

}


FILE_CHAR readUnicodeStringFromArrays(mxArray *mxFilename[2])

{
  mwSize nBuflen = (mxGetM(mxFilename[0])*mxGetN(mxFilename[0])+1);
  FILE_CHAR pacTempString1 = readUnicodeString(mxFilename[0],nBuflen);

  mwSize nBufferLen = (mxGetM(mxFilename[1])*mxGetN(mxFilename[1])+1);
  FILE_CHAR  pacTempString2 = readUnicodeString(mxFilename[1],nBufferLen);
  
#if USE_FILE_WCHAR
  FILE_CHAR  pacFilename = (wchar_t *) mxCalloc(nBufferLen+nBuflen, sizeof(wchar_t));
  wcscpy(pacFilename, pacTempString2);
  wcscat(pacFilename, pacTempString1);
#else
  FILE_CHAR  pacFilename = (char *) mxCalloc(nBufferLen+nBuflen, sizeof(char));
  strcpy(pacFilename, pacTempString2);
  strcat(pacFilename, pacTempString1);
#endif
  
  /*mxFree(pacTempString1);*/
  /*mxFree(pacTempString2);*/
  return pacFilename;
}

#if USE_FILE_WCHAR

int endsWith(const wchar_t* fileName, const char* ext)
{
  size_t len = wcslen(fileName), i;
  size_t targetLen = strlen(ext);
  wchar_t* temp1 =  (wchar_t*)malloc((targetLen + 1) * sizeof(wchar_t));
  char* temp2 =  (char*)malloc((targetLen+1)*sizeof(char));
  int result = 0;
  
  memset(temp1, 0, targetLen*sizeof(wchar_t));
  memset(temp2, 0, targetLen*sizeof(char));

  for (i = 0; i < targetLen; ++i)
  {
    temp1[i] = fileName[len - targetLen + i];
  }
  
  wcstombs(temp2,temp1, targetLen);
  result = strcmp_insensitive(temp2, ext);

  /*mxFree(temp1);*/
  /*mxFree(temp2);*/
  free(temp1);
  free(temp2);
  return result;
}

#endif

FILE_CHAR
browseForFilename()
{
  FILE_CHAR filename = NULL;
  mxArray * mxFilename[2], * mxExt[1];
  mxExt[0] = mxCreateString(".xml");
  int nStatus = mexCallMATLAB(2, mxFilename, 1, mxExt, "uigetfile");

  if (nStatus != 0)
  {
    reportError("TranslateSBML:GUIInput:filename", 
      "Failed to read filename");
  }

  /* get the filename returned */
  filename = readUnicodeStringFromArrays(mxFilename);

  mxDestroyArray(mxExt[0]);
  mxDestroyArray(mxFilename[1]);
  mxDestroyArray(mxFilename[0]);

  return filename;
}

