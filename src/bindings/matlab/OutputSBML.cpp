/**
 * @file    OutputSBML.cpp
 * @brief   MATLAB code for translating SBML-MATLAB structure into a SBML document.
 * @author  Sarah Keating
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

#include "InputOutput.h"
#include "ModelDetails.h"
#include "StructureFields.h"
#include "Variables.h"
#include "Filenames.h"
#include "CommonFunctions.h"

////////////////////////////////////////////////////////////////////////////
//
// OutputSBML.cpp

SBMLDocument
createSBMLDocument(GV& gv)
{
  SBMLDocument sbmlDocument = SBMLDocument(gv.details->getNamespaces());

  PkgMap pm = gv.details->getPackages();
  for (PkgIter it = pm.begin(); it != pm.end(); ++it)
  {
    const std::string& prefix = it->first;
    sbmlDocument.setPackageRequired(prefix, getRequiredStatus(prefix, gv));
  }
  return sbmlDocument;

}

//////////////////////////////////////////////////////////////////////////

// here deal with things that do not fit with 'normal' 
// hopefully this is the only place that manual coding will be necessary
#ifdef USE_FBC

mxArray*
getAssociation(unsigned int i, GV& gv)
{
  mxArray* mxAssociation = NULL;
  mxArray* mxRn = NULL;
  mxArray* mxGPA = NULL;
  mxRn = mxGetField(gv.modelArray, 0, "reaction");
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
addGPAAttributes(FbcAssociation* pAssociation, mxArray* mxAssociation, GV& gv)
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
    std::string value = StructureFields::readString(mxAssociation, name, 0, gv);
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
  int sbo = StructureFields::readInt(mxAssociation, "sboTerm", 0, gv);
  if (sbo != -1)
  {
    pAssociation->setSBOTerm(sbo);
  }
}

void
addGeneProductAssociations(SBMLDocument* sbmlDocument, GV& gv)
{
  FbcModelPlugin* mplug = static_cast<FbcModelPlugin*>(sbmlDocument->getModel()->getPlugin("fbc"));
  for (unsigned int i = 0; i < sbmlDocument->getModel()->getNumReactions(); i++)
  {
    Reaction * rn = sbmlDocument->getModel()->getReaction(i);
    FbcReactionPlugin* plug = static_cast<FbcReactionPlugin*>(rn->getPlugin("fbc"));
    if (plug->isSetGeneProductAssociation())
    {
      mxArray* mxAssoc = NULL;
      mxAssoc = getAssociation(i, gv);
      if (mxAssoc != NULL)
      {
        std::string association = StructureFields::readString(mxAssoc, "fbc_association", 0, gv);
        if (!association.empty())
        {
          FbcAssociation* pAssociation = FbcAssociation::parseFbcInfixAssociation(association, mplug,
            gv.fbcUsingId, gv.fbcAddGeneProducts);
          plug->getGeneProductAssociation()->setAssociation(pAssociation);
          addGPAAttributes(plug->getGeneProductAssociation()->getAssociation(), mxAssoc, gv);
        }
      }
    }
  }
}

void
dealWithAnomalies(SBMLDocument* sbmlDocument, GV& gv)
{
  bool fbcPresent = gv.details->isPackagePresent("fbc");

  if (fbcPresent)
  {
    // the fbc active objective on the listOfObjectives
    std::string obj = StructureFields::readString(gv.modelArray, "fbc_activeObjective", 0, gv);
    if (!obj.empty())
    {
      FbcModelPlugin* plug = static_cast<FbcModelPlugin*>(sbmlDocument->getModel()->getPlugin("fbc"));
      plug->setActiveObjectiveId(obj);
    }

    // the gene product associations in fbc v2
    unsigned int vers = StructureFields::readUint(gv.modelArray, "fbc_version", 0, gv);
    if (vers > 1)
    {
      addGeneProductAssociations(sbmlDocument, gv);
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
  GV gv;
  // we have not made persistent memory
  gv.freeMemory = false;
  FILE_CHAR pacFilename = NULL;
  SBMLDocument sbmlDocument;
  Model *sbmlModel;
  unsigned int outputVersion = 0;

  /* determine whether we are in octave or matlab */
  unsigned int usingOctave = determinePlatform(gv);

  /***************************************************************************
  * validate inputs and outputs
  ****************************************************************************/
  
  pacFilename = validateInputOutputForOutput(nlhs, plhs, nrhs, prhs, 
                                             usingOctave, outputVersion, gv);

  // output required structures
  if (outputVersion == 1)
  {
    OutputVersionInformation(plhs, 0, gv);
  }
  else
  {
    /**************************************************************************
    * get the details of the model
    ***************************************************************************/
    populatePackageLists(gv);
    gv.details = new ModelDetails(gv);

    sbmlDocument = createSBMLDocument(gv);

    /* create a model within the document */
    sbmlModel = sbmlDocument.createModel();

    StructureFields *sf = new StructureFields(sbmlModel, gv.modelArray, gv);

    std::string id = std::string("OutputSBML:GetModel:") + sf->getTypeCode();
    sf->addAttributes(id);

#ifdef USE_FBC
    dealWithAnomalies(&sbmlDocument, gv);
#endif
    /**********************************************************************
    * output the resulting model to specified file
    **************************************************************************/

    /* write the SBML document to the filename specified */
    unsigned int nStatus = 0;
#if USE_FILE_WCHAR
    {
      char* sbml = writeSBMLToString(&sbmlDocument);
      if (sbml != NULL)
      {
        size_t len = strlen(sbml);
        FILE* fp = _wfopen(pacFilename, L"w");
        if (fp != NULL)
        {
          fwrite(sbml, sizeof(char), len, fp);
          fclose(fp);
          nStatus = 1;
        }

        free(sbml);
      }
    }
#else
    nStatus = writeSBML(&sbmlDocument, pacFilename);
#endif

    if (nStatus != 1)
    {
      std::stringstream str; 
      str << "Failed to write file" << std::endl << std::endl;
      str << sbmlDocument.getErrorLog()->toString(); 
      reportError("OutputSBML:writeFile", str.str(), gv);
    }
    else
    {
      mexPrintf("Document written\n");
    }


    delete gv.details;
    delete sf;
  }
}
