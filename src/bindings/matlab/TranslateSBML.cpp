/**
 * \file    TranslateSBML.cpp
 * \brief   MATLAB code for translating SBML document into MATLAB structure
 * \author  Sarah Keating
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
 *
 * Copyright (C) 2019 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
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
#include <algorithm>

#include <sbml/SBMLReader.h>
#include <sbml/SBMLTypes.h>
#include <sbml/xml/XMLNode.h>
#include <sbml/math/ASTNode.h>
#include <sbml/extension/SBasePlugin.h>
#include <sbml/extension/SBMLExtensionRegistry.h>

LIBSBML_CPP_NAMESPACE_USE

#ifdef USE_FBC
#include <sbml/packages/fbc/common/FbcExtensionTypes.h>
#endif


#include "ModelDetails.h"
#include "StructureFields.h"
#include "InputOutput.h"
#include "Filenames.h"
#include "CommonFunctions.h" 

////////////////////////////////////////////////////////////////////////////
//
// TranslateSBML.cpp
SBMLDocument*
readSBMLDocument(FILE_CHAR filename)
{
  SBMLDocument* doc = NULL;
#if USE_FILE_WCHAR
  if (endsWith(filename, ".xml") == 0)
  {
    StringBuffer_t *sb = NULL;
    unsigned long count = 0;
    char buffer[1024];

    FILE* fp = FILE_FOPEN(filename);

    sb = StringBuffer_create(1);

    while ((count = (unsigned long)fread(&buffer, sizeof(char), 1024, fp)) > 0)
    {
      StringBuffer_appendWithLength(sb,buffer, (unsigned long)count); 
      memset(&buffer, 0, 1024*sizeof(char));
    }	
    StringBuffer_appendChar(sb, 0);

    fclose(fp);
    doc = readSBMLFromString(StringBuffer_getBuffer(sb));
    StringBuffer_free(sb);
  }
  else
  {
    size_t len = wcslen(filename);
    char* file = (char*) mxCalloc(len+1, sizeof(char));
    wcstombs(file, filename, len);
    doc = readSBML(file);
    mxFree(file);
  }
#else
  doc = readSBML(filename); 
#endif

  return doc;
}

void
OutputErrorInformation(mxArray *plhs[], SBMLDocument* doc)
{
  const char *error_struct[] =
  {
    "line",
    "errorId",
    "severity",
    "message"
  };

  mwSize errordims[2];

  unsigned int totalerrors = doc->getNumErrors();
  errordims[0] = 1;
  errordims[1] = totalerrors;
  plhs[1] = mxCreateStructArray(2, errordims, 4, error_struct);
  for (unsigned int i = 0; i < totalerrors; ++i)
  {
    const XMLError* e = (const XMLError*)(doc->getError(i));
    mxSetField(plhs[1], i, "line", CreateIntScalar(e->getLine()));
    mxSetField(plhs[1], i, "errorId", CreateIntScalar(e->getErrorId()));
    mxSetField(plhs[1], i, "severity", mxCreateString(e->getSeverityAsString().c_str()));
    mxSetField(plhs[1], i, "message", mxCreateString(e->getMessage().c_str()));
  }
}

void
displayErrors(SBMLDocument* doc, unsigned int warnings, unsigned int errors, 
              unsigned int verboseFlag, unsigned int& listWarningsFlag)
{
  std::ostringstream numErrs;
  numErrs << "The model contains " << errors << " errors";
  if (warnings > 0)
  {
    numErrs << " and " << warnings << " warnings";
  }
  numErrs << "." << std::endl;

  displayLine(numErrs.str());

  if (verboseFlag == 1 && warnings > 0)
  {
    if (!answerYesToQuestion("Do you want to exclude the warnings from the list? Enter y/n ") )
    {
      listWarningsFlag = 1;
    }
  }

  if (verboseFlag == 1)
  {
    numErrs.str("");
    numErrs.clear();
    numErrs << "************************************************************"
      << std::endl << "Line ErrorId Severity Message" << std::endl;

    displayLine(numErrs.str());
  
    for (unsigned int i = 0; i < doc->getNumErrors(); ++i)
    {
      const XMLError* e = (const XMLError_t *) doc->getError(i);

      if (listWarningsFlag == 1 || e->getSeverity() > 1)
      {
        numErrs.str("");
        numErrs.clear();
        numErrs << e->getLine() << ": (" << e->getErrorId() << ")  "
          << e->getSeverityAsString() << " " << e->getMessage() << std::endl;

        displayLine(numErrs.str());
      }
    }

  }
}

unsigned int 
validateDocument(SBMLDocument* doc, unsigned int validateFlag, unsigned int verboseFlag,
                 unsigned int& errors, unsigned int& warnings)
{
  /* check for errors at read */
  unsigned int totalerrors = doc->getNumErrors();

  if (validateFlag > 0)
  {
    if (verboseFlag > 0 && totalerrors > 0)
    {
      if (!answerYesToQuestion("There are errors found during reading. Do you want to continue validation? Enter y/n "))
      {
        totalerrors += doc->checkConsistency();
      }
    }
    else
    {
      totalerrors += doc->checkConsistency();
    }
  }

  /* divide the totalerrors into errors 
  * and warnings
  */
  for (unsigned int i = 0; i < totalerrors; ++i)
  {
    const XMLError * e = (const XMLError *) doc->getError(i);
    if (e->getSeverity() < 2)
    {
      warnings = warnings + 1;
    }
  }
  errors = totalerrors - warnings;

  return totalerrors;
}


///////////////////////////////////////////////////////////////////////////////
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
  GV gv;
  // we have not made persistent memory
  gv.freeMemory = false;

  // default values
  gv.fbcUsingId = false;
  gv.fbcAddGeneProducts = true;

  std::ostringstream numErrs;
  /* determine whether we are in octave or matlab */
  unsigned int usingOctave = determinePlatform(gv);

  /* flags for determining what to output and whether to validate */
  unsigned int outputErrors = 0;
  unsigned int outputVersion = 0;
  unsigned int validateFlag = 0;
  unsigned int verboseFlag = 1;
  unsigned int listWarningsFlag = 0;
  bool readModel = true;

  FILE_CHAR pacFilename = validateInputOutputForTranslate(nlhs, plhs, nrhs, prhs, usingOctave, outputErrors,
    outputVersion, validateFlag, verboseFlag, gv);

  SBMLDocument* sbmlDocument = readSBMLDocument(pacFilename);

  if (sbmlDocument->getModel() == NULL)
  {
   /* at this point - if there have been fatal errors 
    * dont try anything else
    */
    readModel = false;
  }
  else
  {
    ///* check for errors at read */
    unsigned int errors = 0, warnings = 0;
    unsigned int totalerrors = validateDocument(sbmlDocument, validateFlag, verboseFlag, errors, warnings);

   ///*if errors occur report these - promt user as to whether to import the Model*/
    if (totalerrors != 0)
    {
      displayErrors(sbmlDocument, warnings, errors, verboseFlag, listWarningsFlag);     

      if (!(errors == 0 && listWarningsFlag == 0))
      {
        if (validateFlag == 0)
        {
          numErrs.str("");
          numErrs.clear();
          numErrs << "Error encountered during read." << std::endl;
          displayLine(numErrs.str());
        }
        else
        {
          if (verboseFlag == 1)
          {
            if (!answerYesToQuestion("Do you want to load the model anyway? Enter y/n "))
            {
              readModel = false;
            }
          }
        }
      }
    }
  }
  // output required structures
  if (outputVersion == 1)
  {
    OutputVersionInformation(plhs, 2, gv);
  }

  if (outputErrors == 1)
  {
    OutputErrorInformation(plhs, sbmlDocument);
  }
  
  if (readModel) 
  {
    Model * sbmlModel = sbmlDocument->getModel();
    gv.details = new ModelDetails(sbmlDocument, gv);
    populatePackageLists(gv);

    std::string tc = "model";
    const std::string func = "TranslateSBML";
    StructureFields *sf = new StructureFields(tc, gv);
    sf->createStructure(func, sbmlDocument);

//    plhs[0] = sf->getStructure();
    mxArray* mxArgs[3];
    mxArgs[0] = mxDuplicateArray(sf->getStructure());
    mxArgs[1] = CreateIntScalar(sbmlDocument->getLevel());
    mxArgs[2] = CreateIntScalar(sbmlDocument->getVersion());
    mexCallMATLAB(1, &plhs[0], 3, mxArgs, "addLevelVersion");
    mxDestroyArray(mxArgs[0]);
    mxDestroyArray(mxArgs[1]);
    mxDestroyArray(mxArgs[2]);
    delete gv.details;
    delete sf;
  }
  else
  {
    /* we havent read in a Model */
    numErrs.str("");
    numErrs.clear();
    numErrs << "No model returned." << std::endl;
    displayLine(numErrs.str());

    plhs[0] = mxCreateStructArray(0, 0, 0, NULL);
  }

  // need to free the document!
  delete sbmlDocument;
}

