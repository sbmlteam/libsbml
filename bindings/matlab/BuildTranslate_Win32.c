/**
 * Filename    : BuildTranslate_Win32.c
 * Description : MATLAB code for building TranslateSBML.dll in windows environment
 * Author(s)   : SBML Development Group <sbml-team@caltech.edu>
 * Organization: University of Hertfordshire STRC
 * Created     : 2003-09-15
 * Revision    : $Id$
 * Source      : $Source$
 *
 * Copyright 2003 California Institute of Technology, the Japan Science
 * and Technology Corporation, and the University of Hertfordshire
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
 * California Institute of Technology, the Japan Science and Technology
 * Corporation, and the University of Hertfordshire have no obligations to
 * provide maintenance, support, updates, enhancements or modifications.  In
 * no event shall the California Institute of Technology, the Japan Science
 * and Technology Corporation or the University of Hertfordshire be liable
 * to any party for direct, indirect, special, incidental or consequential
 * damages, including lost profits, arising out of the use of this software
 * and its documentation, even if the California Institute of Technology
 * and/or Japan Science and Technology Corporation and/or University of
 * Hertfordshire have been advised of the possibility of such damage.  See
 * the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 *
 * The original code contained here was initially developed by:
 *
 *     Sarah Keating
 *     Science and Technology Research Centre
 *     University of Hertfordshire
 *     Hatfield, AL10 9AB
 *     United Kingdom
 *
 *     http://www.sbml.org
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s): 
 */
#include <mex.h>
#include <matrix.h>

#include <string.h>
/**
 * NAME:    mexFunction
 *
 * PARAMETERS:  int     nlhs     -  number of output arguments  
 *              mxArray *plhs[]  -  output arguments
 *              int     nrhs     -  number of input arguments
 *              mxArray *prhs[]  -  input arguments
 *
 * RETURNS:    
 *
 * FUNCTION:  MATLAB standard dll export function
 *            any returns are made through the mxArray * prhs
 */

void
mexFunction (int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{

    mxArray * mxWin32Files[1];
    mxArray * mxInput[2];
    mxArray * mxExt[3];
    int nStatus, nBuflen, nDirFound;
    char *pacFilename, *pacWin32Dir, *pacTempString, *pacInclude;
    int i, j, count;
    
    /* check input/outputs are correct */
    if (nlhs != 0 || nrhs != 0)
    {
        mexErrMsgTxt("No input or output arguments required.");
    }
    
    /*  assign the inputs to function uigetdir
        call this function which returns the path to the win32 files
    */
 
    mxInput[0] = mxCreateString("");
    mxInput[1] = mxCreateString("Locate the libsbml win32 files");
    
    nDirFound = 0;

    while (nDirFound == 0) 
    {
        nStatus = mexCallMATLAB(1, mxWin32Files, 2, mxInput, "uigetdir");
    
        if (nStatus != 0)
        {
            mexErrMsgTxt("Failed to read filename");
        }
        
        /* get the directory name returned */
        nBuflen = (mxGetM(mxWin32Files[0])*mxGetN(mxWin32Files[0])+1);
        pacWin32Dir = mxCalloc(nBuflen, sizeof(char));
        nStatus = mxGetString(mxWin32Files[0], pacWin32Dir, nBuflen);
        pacTempString = mxCalloc(10, sizeof(char));

        /* check that the last 5 characters are 'win32' */
        for (i = nBuflen-6, j = 0; i < nBuflen; i++, j++)
        {
            pacTempString[j] = pacWin32Dir[i];
        }
       
         if (stricmp(pacTempString, "win32") ==0)
         {
            nDirFound = 1;
         }
  } 

  /* allocate the names for the include path and linked library */ 
  pacFilename = mxCalloc(nBuflen+20, sizeof(char));
  pacInclude = mxCalloc(nBuflen+20, sizeof(char));

  sprintf(pacFilename, "%s\\bin\\libsbml.lib", pacWin32Dir);
  sprintf(pacInclude, "-I%s\\include\\sbml", pacWin32Dir);
    
   
  /* call mex function */
  mxExt[0] = mxCreateString("TranslateSBML.c");
  mxExt[1] = mxCreateString(pacInclude);
  mxExt[2] = mxCreateString(pacFilename);
  nStatus = mexCallMATLAB(0, NULL, 3, mxExt, "mex");

  if (nStatus != 0)
  {
      mexErrMsgTxt("Failed to build file");
  }

  plhs[0] = mxCreateString("Compile successful");
}