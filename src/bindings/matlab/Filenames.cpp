////////////////////////////////////////////////////////////////////////////
//
// Filenames.cpp

////////////////////////////////////////////////////////////////////////////
//
// ensure reading of unicode filenames

#include "Filenames.h"
#include "CommonFunctions.h"
#include "InputOutput.h"

#include <mex.h>

#ifndef USE_OCTAVE
#include <matrix.h>
#endif


FILE_CHAR readUnicodeString(const mxArray *prhs, mwSize length, GV& gv)
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
      "expressed in UTF8, please rename the file.", gv);
  }

  return utf8;
#endif /* USE_FILE_WCHAR */ 

#endif /* USE_OCTAVE*/ 

}


FILE_CHAR readUnicodeStringFromArrays(mxArray *mxFilename[2], GV& gv)

{
  mwSize nBuflen = (mxGetM(mxFilename[0])*mxGetN(mxFilename[0])+1);
  FILE_CHAR pacTempString1 = readUnicodeString(mxFilename[0],nBuflen, gv);

  mwSize nBufferLen = (mxGetM(mxFilename[1])*mxGetN(mxFilename[1])+1);
  FILE_CHAR  pacTempString2 = readUnicodeString(mxFilename[1],nBufferLen, gv);
  
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
browseForFilename(GV& gv)
{
  FILE_CHAR filename = NULL;
  mxArray * mxFilename[2], * mxExt[1];
  mxExt[0] = mxCreateString(".xml");
  int nStatus = mexCallMATLAB(2, mxFilename, 1, mxExt, "uigetfile");

  if (nStatus != 0)
  {
    reportError("TranslateSBML:GUIInput:filename", 
      "Failed to read filename", gv);
  }

  /* get the filename returned */
  filename = readUnicodeStringFromArrays(mxFilename, gv);

  mxDestroyArray(mxExt[0]);
  mxDestroyArray(mxFilename[1]);
  mxDestroyArray(mxFilename[0]);

  return filename;
}

FILE_CHAR validateFilenameForOutput(int nrhs, const mxArray *prhs[], GV& gv)
{
  FILE_CHAR filename = NULL;
  if (nrhs >= 2)
  {
    if (mxIsChar(prhs[1]) != 1)
    {
      reportError("OutputSBML:inputArguments:invalidFilename", 
        "Second input must be a filename\n"
        "USAGE: OutputSBML(SBMLModel, (filename), (exclusiveFlag))", gv);
    }

    size_t nBuflen = (mxGetM(prhs[1])*mxGetN(prhs[1])+1);
    filename = readUnicodeString(prhs[1], (mwSize)nBuflen, gv);
  }
  else
  {
    filename = browseForFilename(gv);
  }

     /* 
    * check that the extension has been used  
    */
#if USE_FILE_WCHAR
    if (wcsstr(filename, L".xml") == NULL)
    {
      wcscat(filename, L".xml");
    }
#else
    /* check that the extension has been used  */
    if (strstr(filename, ".xml") == NULL)
    {
      strcat(filename, ".xml");
    }
#endif


  return filename;
}

void
checkFileExists(FILE_CHAR filename, GV& gv)
{
    FILE *fp;
    fp = FILE_FOPEN(filename);
    if(fp == NULL)
    {
      char * msgTxt = NULL;
#if USE_FILE_WCHAR
      msgTxt = (char *) safe_calloc(wcslen(filename)+35, sizeof(char));
      sprintf(msgTxt, "File %ws does not exist on this path", filename);
#else
      msgTxt = (char *) safe_calloc(strlen(filename)+35, sizeof(char));
      sprintf(msgTxt, "File %s does not exist on this path", filename);
#endif
      reportError("TranslateSBML:inputArguments:filename", msgTxt, gv);
      safe_free(msgTxt);
    }
    else
    {
      fclose(fp);
    }
}
FILE_CHAR
getGivenFilename(const mxArray* prhs[], GV& gv)
{
  FILE_CHAR filename = NULL;
  size_t nBufferLen  = mxGetNumberOfElements (prhs[0]) + 1;
  filename = readUnicodeString(prhs[0], nBufferLen, gv);
  if (filename == NULL)
  {
    reportError("TranslateSBML:inputArguments:filename", 
      "Failed to read filename", gv);
  }
  checkFileExists(filename, gv);
  return filename;
}
FILE_CHAR
getFilename(int nrhs, const mxArray* prhs[], unsigned int& validateFlag, 
            unsigned int& verboseFlag, GV &gv)
{
  FILE_CHAR filename = NULL;

  double *pr = 0;
  switch (nrhs)
  {
  case 4:
    // arg 3
    pr = mxGetPr(prhs[3]);

    if (*pr == 0)
    {
      gv.fbcUsingId = false;
    }
    else
    {
      gv.fbcUsingId = true;
    }
    pr++;
    if (*pr == 0)
    {
      gv.fbcAddGeneProducts = false;
    }
    else
    {
      gv.fbcAddGeneProducts = true;
    }
    // arg 2
    verboseFlag = (int)mxGetScalar(prhs[2]);
    // arg 1
    validateFlag = (int)mxGetScalar(prhs[1]);
    // arg 0
    filename = getGivenFilename(prhs, gv);
    break;
  case 3: 
    // arg 2
    verboseFlag = (int)mxGetScalar(prhs[2]);
    // arg 1
    validateFlag = (int)mxGetScalar(prhs[1]);
    // arg 0
    filename = getGivenFilename(prhs, gv);
    break;
  case 2:
    // arg 1
    validateFlag = (int)mxGetScalar(prhs[1]);
    // arg 0
    filename = getGivenFilename(prhs, gv);
    break;
  case 1:
    // arg 0
    filename = getGivenFilename(prhs, gv);
    break;
  case 0:
    filename = browseForFilename(gv);
    if (answerYesToQuestion("Do you want to validate the model? Enter y/n "))
    {
      validateFlag = 1;
    }
    gv.fbcUsingId = false;
    gv.fbcAddGeneProducts = true;
    break;
  default:
    break;
  }
  return filename;
}

