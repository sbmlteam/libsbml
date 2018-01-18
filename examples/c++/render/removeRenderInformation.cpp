/**
 * @file    removeRenderInformation.cpp
 * @brief   removes render information from the given SBML file
 * @author  Frank Bergmann
 *
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */

#include <stdlib.h>
#include <sbml/SBMLTypes.h>
#include <sbml/extension/SBMLDocumentPlugin.h>

#include <sbml/packages/layout/common/LayoutExtensionTypes.h>
#include <sbml/packages/render/common/RenderExtensionTypes.h>

#include <iostream>

using namespace std;
LIBSBML_CPP_NAMESPACE_USE


void deleteRenderInformationFromLayout(LayoutModelPlugin *lPlugin)
{
  if (lPlugin == NULL) return;
  
  RenderListOfLayoutsPlugin *lolPlugin = (RenderListOfLayoutsPlugin*)lPlugin->getListOfLayouts()->getPlugin("render");
  if (lolPlugin != NULL)
  {
    lolPlugin->getListOfGlobalRenderInformation()->clear();
  }

  for (int i = 0; i < lPlugin->getNumLayouts(); i++)
  {
    Layout* current = lPlugin->getLayout(i);
    if (current == NULL) continue;

    RenderLayoutPlugin *rPlugins = (RenderLayoutPlugin*) current->getPlugin("render");
    if (rPlugins == NULL) continue;

    rPlugins->getListOfLocalRenderInformation()->clear();
  }
}

int main(int argc,char** argv){
  
  if (argc != 3)
  {
    cerr << "usage: removeRenderInformation <input file> <output file>" << endl;
    cerr << "       removes the render information object from the input file." << endl;
    return 1;
  }
  
   string inputFile = argv[1];
  string outputFile = argv[2];
  
  SBMLDocument* doc = readSBMLFromFile(inputFile.c_str());
  unsigned int numErrors = doc->getErrorLog()->getNumFailsWithSeverity(LIBSBML_SEV_ERROR);
  
  if (numErrors > 0)
  {
    cerr << "Encountered errors while reading the file. " << endl;
    cerr << "Please correct the following errors and try again." << endl;
	  doc->printErrors();
    return 2;
  }
  
  SBMLDocumentPlugin* plugin = (SBMLDocumentPlugin*) doc->getPlugin("render");
  if (plugin == NULL)
  {
    // if this is a level 2 model, it could be that simply a render annotation is in place 
    if (doc->getLevel() < 3)
    {
      LayoutModelPlugin* lPlugin = (LayoutModelPlugin*)doc->getModel()->getPlugin("layout");
      deleteRenderInformationFromLayout(lPlugin);      
    }
    else
    {
      cout << "Warning: the document did not use the render information in the first place. " << endl;
    }
  }
  else
  {
    // simply disable the package, this will cause it to no longer being written out
    doc->disablePackage(plugin->getURI(), plugin->getPrefix());
  }

  string sbml = writeSBMLToString(doc);

  writeSBMLToFile(doc, outputFile.c_str());

  return 0;
}

