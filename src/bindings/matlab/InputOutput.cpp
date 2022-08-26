
#include "InputOutput.h"
#include "CommonFunctions.h"

#include <sbml/extension/SBMLExtensionRegistry.h>

 

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
  else
  {
    if (strcmp(pacTempString1, "1") == 0)
    {
      usingOctave = 1;
    }
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


