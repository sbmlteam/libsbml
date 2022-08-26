////////////////////////////////////////////////////////////////////////////
//
// Filenames.h

////////////////////////////////////////////////////////////////////////////
#ifndef FILENAMES_INCLUDED
#define FILENAMES_INCLUDED

#include "Variables.h"

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

FILE_CHAR readUnicodeString(const mxArray *prhs, mwSize length, GV& gv);

FILE_CHAR readUnicodeStringFromArrays(mxArray *mxFilename[2], GV& gv);

#if USE_FILE_WCHAR
    int endsWith(const wchar_t* fileName, const char* ext);
#endif

FILE_CHAR browseForFilename(GV& gv);

FILE_CHAR validateFilenameForOutput(int nrhs, const mxArray *prhs[], GV& gv);

void checkFileExists(FILE_CHAR filename, GV& gv);

FILE_CHAR getGivenFilename(const mxArray* prhs[], GV& gv);

FILE_CHAR getFilename(int nrhs, const mxArray* prhs[], unsigned int& validateFlag, 
                        unsigned int& verboseFlag, GV &gv);


#endif //FILENAMES_INCLUDED
            