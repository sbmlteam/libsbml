/**
 * @file    OutputSBML.cpp
 * @brief   MATLAB code for translating SBML-MATLAB structure into a SBML document.
 * @author  Sarah Keating
 *
 * <!--------------------------------------------------------------------------
 * This file is part of SBMLToolbox.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of SBMLToolbox.
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
 * in the file named "LICENSE.txt" included with this software distribution.
 * and also available online as http://sbml.org/software/sbmltoolbox/license.html
 * ---------------------------------------------------------------------- -->*/

#include <mex.h>
#ifndef USE_OCTAVE
#include <matrix.h>
#endif
#include <string.h>
#include <algorithm>

#include <sbml/SBMLReader.h>
#include <sbml/SBMLTypes.h>
#include <sbml/xml/XMLNode.h>
#include <sbml/math/ASTNode.h>

#include "StructureFields.cpp"
#include "Filenames.cpp"
#include "Arguments.cpp"

LIBSBML_CPP_NAMESPACE_USE

#ifdef USE_FBC
#include <sbml/packages/fbc/common/FbcExtensionTypes.h>
#endif

///////////////////////////////////////////////////////////////////////////////

// global variables
mxArray * mxModel[2];
ModelDetails * details;

IdList reqdPkgPrefixes;
IdList unreqdPkgPrefixes;



SBMLDocument *
createSBMLDocument()
{
  SBMLDocument * sbmlDocument;
  sbmlDocument = new SBMLDocument(details->getNamespaces());

  PkgMap pm = details->getPackages();
  for (PkgIter it = pm.begin(); it != pm.end(); ++it)
  {
    const std::string& prefix = it->first;
    sbmlDocument->setPackageRequired(prefix, getRequiredStatus(prefix));
  }
  return sbmlDocument;

}

//////////////////////////////////////////////////////////////////////////

// here deal with things that do not fit with 'normal' 
// hopefully this is the only place that manual coding will be necessary
#ifdef USE_FBC

mxArray*
getAssociation(unsigned int i)
{
  mxArray* mxAssociation = NULL;
  mxArray* mxRn = NULL;
  mxArray* mxGPA = NULL;
  mxRn = mxGetField(mxModel[0], 0, "reaction");
  if (mxRn != NULL && mxIsStruct(mxRn))
  {
    mxGPA = mxGetField(mxRn, i, "fbc_geneProductAssociation");
    if (mxGPA != NULL)
    {
      mxAssociation = mxGetField(mxGPA, 0, "fbc_association");
    }
  }
  return mxAssociation;
}

void 
addGPAAttributes(FbcAssociation* pAssociation, mxArray* mxAssociation)
{
  IdList* atts = new IdList();
  atts->append("notes");
  atts->append("annotation");
  atts->append("metaid");
  atts->append("id");
  atts->append("name");

  for (unsigned int i = 0; i < atts->size(); i++)
  {
    std::string name = atts->at(i);
    std::string value = StructureFields::readString(mxAssociation, name, 0);
    if (!value.empty()) 
    {
      if (name == "notes")
      {
        pAssociation->setNotes(value);
      }
      else if (name == "annotation")
      {
        pAssociation->setAnnotation(value);
      }
      else
      {
        pAssociation->setAttribute(name, value);
      }
    }
  }
  int sbo = StructureFields::readInt(mxAssociation, "sboTerm", 0);
  if (sbo != -1)
  {
    pAssociation->setSBOTerm(sbo);
  }
}

void
addGeneProductAssociations(SBMLDocument* sbmlDocument)
{
  FbcModelPlugin* mplug = static_cast<FbcModelPlugin*>(sbmlDocument->getModel()->getPlugin("fbc"));
  for (unsigned int i = 0; i < sbmlDocument->getModel()->getNumReactions(); i++)
  {
    Reaction * rn = sbmlDocument->getModel()->getReaction(i);
    FbcReactionPlugin* plug = static_cast<FbcReactionPlugin*>(rn->getPlugin("fbc"));
    if (plug->isSetGeneProductAssociation())
    {
      mxArray* mxAssoc = NULL;
      mxAssoc = getAssociation(i);
      if (mxAssoc != NULL)
      {
        std::string association = StructureFields::readString(mxAssoc, "fbc_association", 0);
        if (!association.empty())
        {
          FbcAssociation* pAssociation = FbcAssociation::parseFbcInfixAssociation(association, mplug);
          plug->getGeneProductAssociation()->setAssociation(pAssociation);
          addGPAAttributes(plug->getGeneProductAssociation()->getAssociation(), mxAssoc);
        }
      }
    }
  }
}

void
dealWithAnomalies(SBMLDocument* sbmlDocument)
{
  bool fbcPresent = details->isPackagePresent("fbc");

  if (fbcPresent)
  {
    // the fbc active objective on the listOfObjectives
    std::string obj = StructureFields::readString(mxModel[0], "fbc_activeObjective", 0);
    if (!obj.empty())
    {
      FbcModelPlugin* plug = static_cast<FbcModelPlugin*>(sbmlDocument->getModel()->getPlugin("fbc"));
      plug->setActiveObjectiveId(obj);
    }

    // the gene product associations in fbc v2
    unsigned int vers = StructureFields::readUint(mxModel[0], "fbc_version", 0);
    if (vers == 2)
    {
      addGeneProductAssociations(sbmlDocument);
    }
  }

}
#endif
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
 *            any returns are made through the mxArray * plhs
 */
void
mexFunction (int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
  FILE_CHAR pacFilename = NULL;
  SBMLDocument *sbmlDocument;
  Model *sbmlModel;

  /* determine whether we are in octave or matlab */
  unsigned int usingOctave = determinePlatform();

  /***************************************************************************
  * validate inputs and outputs
  ****************************************************************************/
  
  pacFilename = validateInputOutputForOutput(nlhs, plhs, nrhs, prhs, usingOctave);


  /**************************************************************************
  * get the details of the model
  ***************************************************************************/
  populatePackageLists();
  details = new ModelDetails();

  ///* look for fbc */

  //if (XMLNamespaces_hasPrefix(SBMLNamespaces_getNamespaces(ns), "fbc") == 1)
  //{
  //  fbcPresent = 1;
  //}

  ///* the fbc namespace may not be set 
  //* but thet fbc package may still be used
  //*/
  //if (fbcPresent == 0)
  //{
  //  nStatus = mxGetFieldNumber(mxModel[0], "fbc_version");
  //  if (nStatus > 0)
  //  {
  //    fbcPresent = 1;
  //    xmlns = XMLNamespaces_create();
  //    XMLNamespaces_add(xmlns, 
  //      "http://www.sbml.org/sbml/level3/version1/fbc/version1", "fbc");
  //    SBMLNamespaces_addNamespaces(ns, xmlns);
  //  }
  //}

  sbmlDocument =   createSBMLDocument();
  //if (fbcPresent == 1)
  //{
  //  SBMLDocument_setPkgRequired(sbmlDocument, "fbc", 0);
  //  mxFBCVersion = mxGetField(mxModel[0], 0, "fbc_version");
  //  nFBCVersion = (unsigned int) mxGetScalar(mxFBCVersion);
  //  fbcVersion = nFBCVersion;
  //}

  /* create a model within the document */
  sbmlModel = sbmlDocument->createModel();

  StructureFields *sf = new StructureFields(sbmlModel, mxModel[0]);

  std::string id = std::string("OutputSBML:GetModel:") + sf->getTypeCode();
  sf->addAttributes(id);

#ifdef USE_FBC
  dealWithAnomalies(sbmlDocument);
#endif
  /**********************************************************************
  * output the resulting model to specified file
  **************************************************************************/

  /* write the SBML document to the filename specified */
  unsigned int nStatus = 0;
#if USE_FILE_WCHAR
  {
    char* sbml = writeSBMLToString(sbmlDocument);
    size_t len = strlen(sbml);
    FILE* fp = _wfopen(pacFilename, L"w");
    fwrite(sbml, sizeof(char), len, fp);
    fclose(fp);
    free(sbml);
    nStatus = 1;
  }
#else
  nStatus = writeSBML(sbmlDocument, pacFilename);
#endif

  if (nStatus != 1)
  {
    reportError("OutputSBML:writeFile", "Failed to write file");
  }
  else
  {
    mexPrintf("Document written\n");
  }

  delete sbmlDocument;

//  FreeMem();
}
