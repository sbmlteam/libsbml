#ifndef COMMON_MATLAB_CODE_INCLUDED
#define COMMON_MATLAB_CODE_INCLUDED

#include "Types.cpp"
 
//
// report error/free memory and exit when error encountered

void 
FreeMem(GV& gv)
{
  /* destroy arrays created */
  mxDestroyArray(gv.modelArray);
  gv.modelArray = NULL;
}

void
reportError(const std::string&id, const std::string& message, GV& gv)
{
  if (gv.freeMemory)
  {
    FreeMem(gv);
  }

  mexErrMsgIdAndTxt(id.c_str(), message.c_str());
}


mxArray *
CreateIntScalar (int nValue)
{
  mxArray * pArray;
  int * panData;

  pArray = mxCreateNumericMatrix(1,1,mxINT32_CLASS, mxREAL);
  panData = (int *)mxGetData(pArray);
  panData[0] = nValue;

  return pArray;
}


FieldType_t
getFieldType(const char* type)
{
  if (type != NULL)
  {
    const FieldType_t lo = TYPE_BOOL;
    const FieldType_t hi = (const FieldType_t)(TYPE_UNKNOWN - 1);

    return (FieldType_t)util_bsearchStringsI(FIELDTYPE_STRINGS, type, lo, hi);
  }
  else
    return TYPE_UNKNOWN;
}

// only used by OutputSBML
bool getRequiredStatus(const std::string& prefix, GV& gv)
{
  bool required = false;

  if (gv.reqdPkgPrefixes.contains(prefix))
  {
    required = true;
  }

  return required;
}

void populatePackageLists(GV& gv)
{
  //reqdPkgPrefixes.append("comp");
  //reqdPkgPrefixes.append("spatial");

  gv.unreqdPkgPrefixes.append("fbc");
  gv.unreqdPkgPrefixes.append("qual");
  gv.unreqdPkgPrefixes.append("groups");
}

// only used by OutputSBML
bool
isUnknownType(std::string tc)
{
  // TO DO 
  if (tc == "(Unknown SBML Type)")
    return true;
  else if (tc == "(Unknown SBML Fbc Type)")
    return true;
  else if (tc == "(Unknown SBML Groups Type)")
    return true;
  else if (tc == "(Unknown SBML Qual Type)")
    return true;
  else
    return false;
}

///////////////////////////////////////////////////////////////////////////////
//
// class to store model details


////////////////////////////////////////////////////////////////////////////
//
// Filenames.cpp

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
  filename = readUnicodeStringFromArrays(mxFilename);

  mxDestroyArray(mxExt[0]);
  mxDestroyArray(mxFilename[1]);
  mxDestroyArray(mxFilename[0]);

  return filename;
}

////////////////////////////////////////////////////////////////////////////
//
// Arguments.cpp

/* determine whether we are in octave or matlab */
unsigned int
determinePlatform(GV& gv)
{
  unsigned int usingOctave = 0;
  mxArray * mxOctave[1];

  mexCallMATLAB(1, mxOctave, 0, NULL, "isoctave");

  size_t nBuflen = (mxGetM(mxOctave[0])*mxGetN(mxOctave[0])+1);
  char * pacTempString1 = (char *)(safe_calloc(nBuflen, sizeof(char)));
  int nStatus = mxGetString(mxOctave[0], pacTempString1, (mwSize)(nBuflen));

  if (nStatus != 0)
  {
    reportError("OutputSBML:platformDetection", 
      "Could not determine platform", gv);
  }

  safe_free(pacTempString1);
  mxDestroyArray(mxOctave[0]);

  return usingOctave;
}

bool
answerYesToQuestion(const std::string& question)
{
  bool answer = false;
  mxArray *mxPrompt[2], *mxReply[1];
  char *pacReply;
  mxPrompt[0]= mxCreateString(question.c_str());
  mxPrompt[1]= mxCreateString("s");
  mexCallMATLAB(1, mxReply, 2, mxPrompt, "input");
  mxDestroyArray(mxPrompt[0]);
  mxDestroyArray(mxPrompt[1]);

  size_t nBufferLen = (mxGetM(mxReply[0])*mxGetN(mxReply[0])+1);
  pacReply = (char *) (safe_calloc(nBufferLen, sizeof(char)));
  mxGetString(mxReply[0], pacReply, (mwSize)(nBufferLen));
  mxDestroyArray(mxReply[0]);

  if (strcmp_insensitive(pacReply, "y") == 0)
  {
    answer = true;
  }
  safe_free(pacReply);

  return answer;
}

///////////////////////////////////////////////////////////////////////////////
//
// functions used to check arguments for OutputSBML

void
validateNumberOfInputsForOutput(int nrhs, const mxArray *prhs[], 
  unsigned int usingOctave, unsigned int& outputVersion, int nlhs, GV& gv)
{
  if (nlhs > 0 && nrhs == 0)
  {
    outputVersion = 1;
  }
  else
  {
    if (nrhs < 1)
    {
      reportError("OutputSBML:inputArgs",
        "Must supply at least the model as an input argument\n"
        "USAGE: OutputSBML(SBMLModel, (filename), (exclusiveFlag), (applyUserValidation), (fbcGeneProductOptions))", gv);
    }
    if (usingOctave == 1 && nrhs < 2)
    {
      reportError("OutputSBML:Octave:needFilename",
        "Octave requires the filename to be specified\n"
        "USAGE: OutputSBML(SBMLModel, (filename), (exclusiveFlag), (applyUserValidation), (fbcGeneProductOptions))", gv);
    }
    if (nrhs > 5)
    {
      reportError("OutputSBML:inputArguments", "Too many input arguments\n"
        "USAGE: OutputSBML(SBMLModel, (filename), (exclusiveFlag), (applyUserValidation), (fbcGeneProductOptions))", gv);
    }

    if (nrhs > 1 && ((mxIsChar(prhs[1]) != 1) || (mxGetM(prhs[1]) != 1)))
    {
      reportError("OutputSBML:inputArguments:invalidFilename",
        "Second argument must be a filename\n"
        "USAGE: OutputSBML(SBMLModel, (filename), (exclusiveFlag), (applyUserValidation), (fbcGeneProductOptions))", gv);
    }
    if (nrhs > 2 && !mxIsNumeric(prhs[2]))
    {
      reportError("OutputSBML:inputArguments:exclusiveFlag",
        "exclusiveFlag is an optional argument but must be a number\n"
        "USAGE: OutputSBML(SBMLModel, (filename), (exclusiveFlag), (applyUserValidation), (fbcGeneProductOptions))", gv);
    }

    if (nrhs > 3 && !mxIsNumeric(prhs[3]))
    {
      reportError("OutputSBML:inputArguments:applyUserValidation",
        "applyUserValidation is an optional argument but must be a number\n"
        "USAGE: OutputSBML(SBMLModel, (filename), (exclusiveFlag), (applyUserValidation), (fbcGeneProductOptions))", gv);
    }

    if (nrhs > 4 && (!mxIsNumeric(prhs[4]) || (mxGetM(prhs[4]) != 1) || (mxGetN(prhs[4]) != 2)))
    {
      reportError("OutputSBML:inputArguments:fbcGeneProductOptions",
        "fbcGeneProductOptions is an optional argument but must be an array with two numbers\n"
        "USAGE: OutputSBML(SBMLModel, (filename), (exclusiveFlag), (applyUserValidation), (fbcGeneProductOptions))", gv);
    }

  }

}

void
validateNumberOfOutputsForOutput(int nlhs, GV& gv)
{
  if (nlhs > 0)
  {
    reportError("OutputSBML:outputArguments", "Too many output arguments\n"
      "USAGE: OutputSBML(SBMLModel, (filename), (exclusiveFlag))", gv);
  }
}

void
populateModelArray(int nrhs, const mxArray *prhs[], GV& gv)
{
  gv.modelArray = mxDuplicateArray(prhs[0]);

  /**
  * note second argument may be the filename
  *
  * we now have the option of a third argument that indicates that we
  * want the structure to ONLY contain expected fields or not
  *
  * and a fourth argument that tells us whether to apply user
  * specific validation
  *
  * and a fifth argument saying whether to use ids/lebels in fbc
  */
  if (nrhs > 4)
  {
    double *pr2 = mxGetPr(prhs[2]);
    if (*pr2 == 0)
    {
        gv.onlyExpectedFields = false;
    }
    else
    {
        gv.onlyExpectedFields = true;
    }
    double *pr3 = mxGetPr(prhs[3]);
    if (*pr3 == 0)
    {
        gv.applyUserValidation = false;
    }
    else
    {
        gv.applyUserValidation = true;
    }
    double *pr = mxGetPr(prhs[4]);
  
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
  }
  else if (nrhs > 3)
  {
    double *pr2 = mxGetPr(prhs[2]);
    if (*pr2 == 0)
    {
        gv.onlyExpectedFields = false;
    }
    else
    {
        gv.onlyExpectedFields = true;
    }
    double *pr3 = mxGetPr(prhs[3]);
    if (*pr3 == 0)
    {
        gv.applyUserValidation = false;
    }
    else
    {
        gv.applyUserValidation = true;
    }
    gv.fbcUsingId = false;
    gv.fbcAddGeneProducts = true;
  }  
  else if ( nrhs > 2)
  {
    double *pr2 = mxGetPr(prhs[2]);
    if (*pr2 == 0)
    {
        gv.onlyExpectedFields = false;
    }
    else
    {
        gv.onlyExpectedFields = true;
    }
    gv.applyUserValidation = false;
    gv.fbcUsingId = false;
    gv.fbcAddGeneProducts = true;
  }
  else
  {
      gv.onlyExpectedFields = true;
      gv.applyUserValidation = false;
      gv.fbcUsingId = false;
      gv.fbcAddGeneProducts = true;
  }
  
  // we have made memory - need to free it is we exit prematurely
  gv.freeMemory = true;
}

void 
validateModel(GV& gv)
{
  mxArray * mxCheckStructure[2];
  mxArray * mxModel[3];
  mxModel[0] = gv.modelArray;
  if (gv.onlyExpectedFields)
  {
    mxModel[1] = mxCreateDoubleScalar(1);
  }
  else
  {
  
    mxModel[1] = mxCreateDoubleScalar(0);
  }
  if (gv.applyUserValidation)
  {
    mxModel[2] = mxCreateDoubleScalar(1);
  }
  else
  {

    mxModel[2] = mxCreateDoubleScalar(0);
  }
  int nStatus = mexCallMATLAB(2, mxCheckStructure, 3, mxModel, "isSBML_Model");

  int value = (int)(mxGetScalar(mxCheckStructure[0]));
  if ((nStatus != 0) || (value != 1))
  {
    /* there are errors - use the pacTempString1 char * to list these to the user */
    size_t nBuflen = (mxGetM(mxCheckStructure[1])*mxGetN(mxCheckStructure[1])+1);
    char * pacTempString1 = (char *)safe_calloc(nBuflen, sizeof(char));
    nStatus = mxGetString(mxCheckStructure[1], pacTempString1, (mwSize)(nBuflen));
    std::ostringstream errMsg;
    if (nStatus == 0)
    {
      errMsg << "\nFirst input must be a valid MATLAB_SBML Structure\n\n" <<
        "Errors reported: " << pacTempString1 << "\nUSAGE: OutputSBML(SBMLModel"
        << ", (filename), (exclusiveFlag), (applyUserValidation))";
      reportError("OutputSBML:inputArguments:invalidModelSupplied", errMsg.str(), gv);
    }
    else
    {
      errMsg << "\nFirst input must be a valid MATLAB_SBML Structure\n\n" <<
        "\nUSAGE: OutputSBML(SBMLModel, (filename), (exclusiveFlag), (applyUserValidation))";
      reportError("OutputSBML:inputArguments:invalidStructureSupplied", errMsg.str(), gv);
    } 
    safe_free(pacTempString1);
  }

  mxDestroyArray(mxCheckStructure[0]);
  mxDestroyArray(mxCheckStructure[1]);
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
    filename = readUnicodeString(prhs[1], (mwSize)nBuflen);
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

//////////////

// functions for TranslatSBML
void
validateNumberOfInputsForTranslate(int nrhs, const mxArray *prhs[], 
                                   unsigned int usingOctave, GV& gv)
{
  if (nrhs > 4)
  {
    reportError("TranslateSBML:inputArguments", "Too many input arguments\n"
      "USAGE: [model, (errors), (version)] = "
      "TranslateSBML((filename), (validateFlag), (verboseFlag), (fbcGeneProductOptions))", gv);
  }

  if (nrhs > 0 && ((mxIsChar(prhs[0]) != 1) || (mxGetM(prhs[0]) != 1)))
  {
    reportError("TranslateSBML:inputArguments:invalidFilename", 
      "First argument must be a filename\n"
      "USAGE: [model, (errors), (version)] = "
      "TranslateSBML((filename), (validateFlag), (verboseFlag), (fbcGeneProductOptions))", gv);
  }
  if (nrhs > 1 && !mxIsNumeric(prhs[1]))
  {
    reportError("TranslateSBML:inputArguments:validateFlag", 
      "validateFlag is an optional argument but must be a number\n"
      "USAGE: [model, (errors), (version)] = "
      "TranslateSBML((filename), (validateFlag), (verboseFlag), (fbcGeneProductOptions))", gv);
  }

  if (nrhs > 2 && !mxIsNumeric(prhs[2]))
  {
    reportError("TranslateSBML:inputArguments:verboseFlag", 
      "verboseFlag is an optional argument but must be a number\n"
      "USAGE: [model, (errors), (version)] = "
      "TranslateSBML((filename), (validateFlag), (verboseFlag), (fbcGeneProductOptions))", gv);
  }

  if (nrhs > 3 && (!mxIsNumeric(prhs[3]) || (mxGetM(prhs[3]) != 1) || (mxGetN(prhs[3]) != 2)))
  {
    reportError("TranslateSBML:inputArguments:fbcGeneProductOptions", 
      "fbcGeneProductOptions is an optional argument but must be an array with two numbers\n"
      "USAGE: [model, (errors), (version)] = "
      "TranslateSBML((filename), (validateFlag), (verboseFlag), (fbcGeneProductOptions))", gv);
  }

  if (usingOctave && nrhs == 0)
  {
    reportError("TranslateSBML:Octave:needFilename", 
      "Octave requires the filename to be specified\n"
        "USAGE: [model, (errors), (version)] = "
        "TranslateSBML(filename, (validateFlag), (verboseFlag))", gv);
  }
}

void
validateNumberOfOutputsForTranslate(int nlhs, mxArray *plhs[], 
                                    unsigned int& outputErrors,
                                    unsigned int& outputVersion, GV & gv)
{
  switch (nlhs)
  {
  case 3:
    outputErrors = 1;
    outputVersion = 1;
    break;
  case 2:
    outputErrors = 1;
    break;
  case 1:
  case 0:
    break;
  default:
    reportError("TranslateSBML:outputArguments", "Too many output arguments\n"
      "USAGE: [model, (errors), (version)] = "
      "TranslateSBML((filename), (validateFlag), (verboseFlag))", gv);
    break;
  }
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
  filename = readUnicodeString(prhs[0], nBufferLen);

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

///////////////////////////////////////////////////////////////////////////

// functions called by main functions 
FILE_CHAR
validateInputOutputForOutput(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], 
                    unsigned int usingOctave, unsigned int& outputVersion, GV& gv)
{
  FILE_CHAR filename = NULL;
  validateNumberOfInputsForOutput(nrhs, prhs, usingOctave, outputVersion, nlhs, gv);
  if (outputVersion == 0)
  {
    validateNumberOfOutputsForOutput(nlhs, gv);

    populateModelArray(nrhs, prhs, gv);
    validateModel(gv);
    filename = validateFilenameForOutput(nrhs, prhs, gv);
  }
  return filename;
}

FILE_CHAR
validateInputOutputForTranslate(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], 
                    unsigned int usingOctave, unsigned int& outputErrors,
                    unsigned int& outputVersion, unsigned int& validateFlag,
                    unsigned int& verboseFlag, GV& gv)
{
  FILE_CHAR filename = NULL;
  validateNumberOfInputsForTranslate(nrhs, prhs, usingOctave, gv);
  validateNumberOfOutputsForTranslate(nlhs, plhs, outputErrors, outputVersion, gv);
  filename = getFilename(nrhs, prhs, validateFlag, verboseFlag, gv);

  return filename;
}


void
OutputVersionInformation(mxArray *plhs[], int pos, GV& gv)
{
  const char *version_struct[] =
  {
    "libSBML_version",
    "libSBML_version_string",
    "XML_parser",
    "XML_parser_version",
    "isFBCEnabled",
    "packagesEnabled"
  };

  const char *xml_parsers[] =
  {
    "libxml2" ,
    "expat" ,
    "xerces",
    "not found"
  };

  mwSize dims[2] = {1, 1};

  const char * parser = xml_parsers[0];
  unsigned int i = 0;
  populatePackageLists(gv);

  plhs[pos] = mxCreateStructArray(2, dims, 6, version_struct);

  mxSetField(plhs[pos], 0, "libSBML_version", CreateIntScalar(getLibSBMLVersion()));
  mxSetField(plhs[pos], 0, "libSBML_version_string", mxCreateString(getLibSBMLDottedVersion()));

  while (isLibSBMLCompiledWith(parser) == 0 && i < 3)
  {
    ++i;
    parser = xml_parsers[i];
  }

  mxSetField(plhs[pos], 0, "XML_parser", mxCreateString(parser));
  mxSetField(plhs[pos], 0, "XML_parser_version", mxCreateString(getLibSBMLDependencyVersionOf(parser)));

#ifdef USE_FBC
  mxSetField(plhs[pos], 0, "isFBCEnabled", mxCreateString("enabled"));

#else
  mxSetField(plhs[pos], 0, "isFBCEnabled", mxCreateString("disabled"));

#endif
  std::ostringstream oss;
  bool first = true;
  for (unsigned int i = 0; i < SBMLExtensionRegistry::getNumRegisteredPackages(); ++i)
  {
    std::string name = SBMLExtensionRegistry::getRegisteredPackageName(i);
    if (gv.reqdPkgPrefixes.contains(name) || gv.unreqdPkgPrefixes.contains(name))
    {
      if (!first) 
      {
        oss << ";";
      }
      oss << name;
      first = false;
    }
  }

  std::string msg = oss.str();
  mxSetField(plhs[pos], 0, "packagesEnabled", mxCreateString(msg.c_str()));
}


#endif // COMMON_MATLAB_CODE_INCLUDED