/**
 * @file    printRenderInformation.cpp
 * @brief   prints an overview of the render information in the given SBML file
 * @author  Frank Bergmann
 *
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */

#include <stdlib.h>
#include <sbml/SBMLTypes.h>
#include <sbml/packages/layout/common/LayoutExtensionTypes.h>
#include <sbml/packages/render/common/RenderExtensionTypes.h>

#include <iostream>
#include <string>
#include <strstream>

using namespace std;
LIBSBML_CPP_NAMESPACE_USE

string toString(RelAbsVector& vec)
{
  stringstream str;
  str << vec.getAbsoluteValue() << " + " << vec.getRelativeValue() << "%";
  return str.str();
}

int main(int argc,char** argv){
if (argc != 3)
  {
    cerr << "usage: printRenderInformation <input file> " << endl;
    cerr << "       prints a summary of the render information object." << endl;
    return 1;
  }
  
  string inputFile = argv[1];
  
  SBMLDocument* doc = readSBMLFromFile(inputFile.c_str());
  unsigned int numErrors = doc->getNumErrors();
  
  if (numErrors > 0)
  {
    cerr << "Encountered errors while reading the file. " << endl;
    cerr << "Please correct the following errors and try again." << endl;
	  doc->printErrors();
    return 2;
  }
  
  Model* model = doc->getModel();
  
  LayoutModelPlugin *plugin = (LayoutModelPlugin*) model->getPlugin ("layout");
  
  if (plugin == NULL || plugin->getNumLayouts() == 0)
  {
    cerr << "The loaded model contains no layout information, please add these first." << endl;
	return 3;
  }
  
  RenderListOfLayoutsPlugin *lolPlugin = (RenderListOfLayoutsPlugin*) plugin->getListOfLayouts()->getPlugin("render");
  if (lolPlugin != NULL && lolPlugin->getNumGlobalRenderInformationObjects() > 0)
  {
    cout << "The loaded model contains global Render information: " << endl;	

    for (unsigned int i = 0; i < lolPlugin->getNumGlobalRenderInformationObjects(); i++)
    {
      GlobalRenderInformation *info = lolPlugin->getRenderInformation(i);

      cout << "Id: " << info->getId() << endl;
      cout << "Name: " << info->getName() << endl;
      cout << "Program Name: " << info->getProgramName() << endl;
      cout << "Program Version: " << info->getProgramVersion() << endl;
      cout << "Background color: " << info->getBackgroundColor() << endl;
      
      cout << "Color Definitions:" << endl;
      for (unsigned int j = 0; j < info->getNumColorDefinitions(); j++) {
        ColorDefinition* color = info->getColorDefinition(j);
        cout << "\tcolor: " << j << 
                " id: " << color->getId() << 
                " color: " << color->createValueString() << endl;
      }

      cout << "GradientDefinitions: " << endl;
      for (unsigned int j = 0; j < info->getNumGradientDefinitions(); j++){
        GradientBase* base = info->getGradientDefinition(j);
        LinearGradient *linear = dynamic_cast<LinearGradient*>(base);
        RadialGradient *radial = dynamic_cast<RadialGradient*>(base);

        if (linear != NULL)
        {
          cout << "\tLinear Gradient: " << linear->getId() 
            << " start: " << toString(linear->getXPoint1()) << ", " << toString(linear->getYPoint1())
            << " end: " << toString(linear->getXPoint2()) << ", " << toString(linear->getYPoint2()) 
            << endl;
        }
        else if (radial != NULL)
        {
          cout << "\tRadial Gradient: " << radial->getId() 
            << " center: " << toString(radial->getCenterX()) << ", " << toString(radial->getCenterY())
            << " focal: " << toString(radial->getFocalPointX()) << ", " << toString(radial->getFocalPointY())
            << endl;
        }

        for (unsigned int k = 0; k < base->getNumGradientStops(); k++)
        {
          GradientStop *stop = base->getGradientStop(k);
          cout << "\t\tstop " << k << " id: " << stop->getId() << " stop-color: " << stop->getStopColor() << endl;
        }
    
      } // gradient definitions
    
      // similarly for the remaining elements 
      cout << "\tNumber of Line Endings: " << info->getNumLineEndings() << endl;

      // and finally the styles

      for (unsigned int j = 0; j < info->getNumStyles(); j++)
      {
        GlobalStyle* style = info->getStyle(j);

        cout << "\tstyle " << j << " id: " << style->getId() << " applies to: "<< endl; 
        cout << "\t\troles:" << style->createRoleString() << 
                " types: " << style->createTypeString() << endl;

      }

    } // for: global render information

  } // numGlobalRenderInformation > 0

  // add render information to the first layout
  Layout *layout = plugin->getLayout(0);

  RenderLayoutPlugin *rPlugin = (RenderLayoutPlugin*) layout->getPlugin("render");
  if (rPlugin != NULL && rPlugin->getNumLocalRenderInformationObjects() > 0)
  {
    cout << "The loaded model contains local Render information. " << endl;
	// here we would do the same as above for the local render information ...
  }

  return 0;
}

