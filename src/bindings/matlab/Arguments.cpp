/**
 * \file    TranslateSBML.cpp
 * \brief   MATLAB code for translating SBML document into MATLAB structure
 * \author  Sarah Keating
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2013-2017 jointly by the following organizations:
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

/* determine whether we are in octave or matlab */
unsigned int
determinePlatform()
{
  unsigned int usingOctave = 0;
  mxArray * mxOctave;

  mexCallMATLAB(1, &mxOctave, 0, NULL, "isoctave");

  size_t nBuflen = (mxGetM(mxOctave)*mxGetN(mxOctave)+1);
  char * pacTempString1 = (char *)(safe_calloc(nBuflen, sizeof(char)));
  int nStatus = mxGetString(mxOctave, pacTempString1, (mwSize)(nBuflen));

  if (nStatus != 0)
  {
    reportError("OutputSBML:platformDetection", 
      "Could not determine platform", false);
  }

  safe_free(pacTempString1);
  mxDestroyArray(mxOctave);

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
  pacReply = (char *) mxCalloc(nBufferLen, sizeof(char));
  mxGetString(mxReply[0], pacReply, (mwSize)(nBufferLen));
  mxDestroyArray(mxReply[0]);

  if (strcmp_insensitive(pacReply, "y") == 0)
  {
    answer = true;
  }

  return answer;
}

///////////////////////////////////////////////////////////////////////////////
//
// functions used to check arguments for OutputSBML

void
validateNumberOfInputsForOutput(int nrhs, const mxArray *prhs[], unsigned int usingOctave)
{
  if (nrhs < 1)
  {
    reportError("OutputSBML:inputArgs", 
      "Must supply at least the model as an input argument\n"
      "USAGE: OutputSBML(SBMLModel, (filename), (exclusiveFlag))", false);
  }
  if (usingOctave == 1 && nrhs < 2)
  {
    reportError("OutputSBML:Octave:needFilename", 
      "Octave requires the filename to be specified\n"
      "USAGE: OutputSBML(SBMLModel, filename, (exclusiveFlag))", false);
  }
  if (nrhs > 4)
  {
    reportError("OutputSBML:inputArguments", "Too many input arguments\n"
      "USAGE: OutputSBML(SBMLModel, (filename), (exclusiveFlag))", false);
  }
}

void
validateNumberOfOutputsForOutput(int nlhs)
{
  if (nlhs > 0)
  {
    reportError("OutputSBML:outputArguments", "Too many output arguments\n"
      "USAGE: OutputSBML(SBMLModel, (filename), (exclusiveFlag))", false);
  }
}

void
populateModelArray(int nrhs, const mxArray *prhs[])
{
  mxModel[0] = mxDuplicateArray(prhs[0]);
  mexMakeArrayPersistent(mxModel[0]);
  mexAtExit(FreeMem);

  /**
  * we now have the option of a third argument that indicates that we
  * want the structure to ONLY contain expected fields or not
  */
  if (nrhs > 2)
  {
    mxModel[1] = (mxArray *)prhs[2];
  }  
  else
  {
    mxModel[1] = mxCreateDoubleScalar(1);
  }
}

void 
validateModel()
{
  mxArray * mxCheckStructure[2];
  int nStatus = mexCallMATLAB(2, mxCheckStructure, 2, mxModel, "isSBML_Model");

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
        << ", (filename), (exclusiveFlag))";
      reportError("OutputSBML:inputArguments:invalidModelSupplied", errMsg.str(), false);
    }
    else
    {
      errMsg << "\nFirst input must be a valid MATLAB_SBML Structure\n\n" <<
        "\nUSAGE: OutputSBML(SBMLModel, (filename), (exclusiveFlag))";
      reportError("OutputSBML:inputArguments:invalidStructureSupplied", errMsg.str(), false);
    } 
    safe_free(pacTempString1);
  }

  mxDestroyArray(mxCheckStructure[0]);
  mxDestroyArray(mxCheckStructure[1]);
}

FILE_CHAR validateFilenameForOutput(int nrhs, const mxArray *prhs[])
{
  FILE_CHAR filename = NULL;
  if (nrhs >= 2)
  {
    if (mxIsChar(prhs[1]) != 1)
    {
      reportError("OutputSBML:inputArguments:invalidFilename", 
        "Second input must be a filename\n"
        "USAGE: OutputSBML(SBMLModel, (filename), (exclusiveFlag))");
    }

    int nBuflen = (mxGetM(prhs[1])*mxGetN(prhs[1])+1);
    filename = readUnicodeString(prhs[1], (mwSize)nBuflen);
  }
  else
  {
    filename = browseForFilename();
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
                                   unsigned int usingOctave)
{
  if (nrhs > 3)
  {
    reportError("TranslateSBML:inputArguments", "Too many input arguments\n"
      "USAGE: [model, (errors), (version)] = "
      "TranslateSBML((filename), (validateFlag), (verboseFlag))");
  }

  if (nrhs > 0 && ((mxIsChar(prhs[0]) != 1) || (mxGetM(prhs[0]) != 1)))
  {
    reportError("TranslateSBML:inputArguments:invalidFilename", 
      "First argument must be a filename\n"
      "USAGE: [model, (errors), (version)] = "
      "TranslateSBML((filename), (validateFlag), (verboseFlag))");
  }
  if (nrhs > 1 && !mxIsNumeric(prhs[1]))
  {
    reportError("TranslateSBML:inputArguments:validateFlag", 
      "validateFlag is an optional argument but must be a number\n"
      "USAGE: [model, (errors), (version)] = "
      "TranslateSBML((filename), (validateFlag), (verboseFlag))");
  }

  if (nrhs > 2 && !mxIsNumeric(prhs[2]))
  {
    reportError("TranslateSBML:inputArguments:verboseFlag", 
      "verboseFlag is an optional argument but must be a number\n"
      "USAGE: [model, (errors), (version)] = "
      "TranslateSBML((filename), (validateFlag), (verboseFlag))");
  }

  if (usingOctave && nrhs == 0)
  {
    reportError("TranslateSBML:Octave:needFilename", 
      "Octave requires the filename to be specified\n"
        "USAGE: [model, (errors), (version)] = "
        "TranslateSBML(filename, (validateFlag), (verboseFlag))");
  }
}

void
validateNumberOfOutputsForTranslate(int nlhs, mxArray *plhs[], 
                                    unsigned int& outputErrors,
                                    unsigned int& outputVersion)
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
      "TranslateSBML((filename), (validateFlag), (verboseFlag))");
    break;
  }
}

void
checkFileExists(FILE_CHAR filename)
{
    FILE *fp;
    fp = FILE_FOPEN(filename);
    if(fp == NULL)
    {
      char * msgTxt = NULL;
#if USE_FILE_WCHAR
      msgTxt = (char *) mxCalloc(wcslen(filename)+35, sizeof(char));
#else
      msgTxt = (char *) mxCalloc(strlen(filename)+35, sizeof(char));
#endif
      sprintf(msgTxt, "File %s does not exist on this path", filename);
      reportError("TranslateSBML:inputArguments:filename", msgTxt);
    }
    else
    {
      fclose(fp);
    }

}

FILE_CHAR
getGivenFilename(const mxArray* prhs[])
{
  FILE_CHAR filename = NULL;
  size_t nBufferLen  = mxGetNumberOfElements (prhs[0]) + 1;
  filename = readUnicodeString(prhs[0], nBufferLen);

  if (filename == NULL)
  {
    reportError("TranslateSBML:inputArguments:filename", 
      "Failed to read filename");
  }

  checkFileExists(filename);
  return filename;
}

FILE_CHAR
getFilename(int nrhs, const mxArray* prhs[], unsigned int& validateFlag, 
            unsigned int& verboseFlag)
{
  FILE_CHAR filename = NULL;

  if (nrhs > 0)
  {
    filename = getGivenFilename(prhs);

    /* if a second argument has been given this is the flag indicating
    * whether to validate the Model or not
    */
    if (nrhs > 1)
    {
      validateFlag = (int) mxGetScalar(prhs[1]);   
    }

    /* if a third argument has been given this is the flag indicating
    * whether to print out errors or not
    */
    if (nrhs > 2)
    {
      verboseFlag = (int) mxGetScalar(prhs[2]);   
    }

  }
  else
  {
    filename = browseForFilename();
    if (answerYesToQuestion("Do you want to validate the model? Enter y/n "))
    {
      validateFlag = 1;
    }
  }

  return filename;
}

///////////////////////////////////////////////////////////////////////////

// functions called by main functions 
FILE_CHAR
validateInputOutputForOutput(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], 
                    unsigned int usingOctave)
{
  validateNumberOfInputsForOutput(nrhs, prhs, usingOctave);
  validateNumberOfOutputsForOutput(nlhs);
  populateModelArray(nrhs, prhs);
  validateModel();
  FILE_CHAR filename = validateFilenameForOutput(nrhs, prhs);
  return filename;
}

FILE_CHAR
validateInputOutputForTranslate(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], 
                    unsigned int usingOctave, unsigned int& outputErrors,
                    unsigned int& outputVersion, unsigned int& validateFlag,
                    unsigned int& verboseFlag)
{
  FILE_CHAR filename = NULL;
  validateNumberOfInputsForTranslate(nrhs, prhs, usingOctave);
  validateNumberOfOutputsForTranslate(nlhs, plhs, outputErrors, outputVersion);
  filename = getFilename(nrhs, prhs, validateFlag, verboseFlag);

  return filename;
}

