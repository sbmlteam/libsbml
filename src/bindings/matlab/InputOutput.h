#ifndef INPUT_OUTPUT_INCLUDED
#define INPUT_OUTPUT_INCLUDED

#include <mex.h>

#ifndef USE_OCTAVE
#include <matrix.h>
#endif

#include "Filenames.h"


unsigned int determinePlatform(GV& gv);

bool answerYesToQuestion(const std::string& question);

void validateNumberOfInputsForOutput(int nrhs, const mxArray *prhs[], 
  unsigned int usingOctave, unsigned int& outputVersion, int nlhs, GV& gv);

void validateNumberOfOutputsForOutput(int nlhs, GV& gv);

void populateModelArray(int nrhs, const mxArray *prhs[], GV& gv);

void validateModel(GV& gv);

void validateNumberOfInputsForTranslate(int nrhs, const mxArray *prhs[], 
                                   unsigned int usingOctave, GV& gv);

void validateNumberOfOutputsForTranslate(int nlhs, mxArray *plhs[], 
                                    unsigned int& outputErrors,
                                    unsigned int& outputVersion, GV & gv);

FILE_CHAR validateInputOutputForOutput(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], 
                    unsigned int usingOctave, unsigned int& outputVersion, GV& gv);

FILE_CHAR validateInputOutputForTranslate(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], 
                    unsigned int usingOctave, unsigned int& outputErrors,
                    unsigned int& outputVersion, unsigned int& validateFlag,
                    unsigned int& verboseFlag, GV& gv);

void OutputVersionInformation(mxArray *plhs[], int pos, GV& gv);

#endif // INPUT_OUTPUT_INCLUDED