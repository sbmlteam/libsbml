/**
 * @file    addRenderInformation.cpp
 * @brief   Adds render information to the given SBML file
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

using namespace std;
LIBSBML_CPP_NAMESPACE_USE

void addRenderInformation(RenderLayoutPlugin *rPlugin)
{
  
  if (rPlugin == NULL)
  {
    cerr << "could not add render information!" << endl;
    exit(4);
  }
 
  LocalRenderInformation *rInfo = rPlugin->createLocalRenderInformation();
  rInfo->setId("info");
  rInfo->setName("Example Render Information");
  rInfo->setProgramName("RenderInformation Examples");
  rInfo->setProgramVersion("1.0");
 
  // add some colors
  ColorDefinition *color = rInfo->createColorDefinition();
  color->setId("black");
  color->setColorValue("#000000");

  color = rInfo->createColorDefinition();
  color->setId("silver");
  color->setColorValue("#c0c0c0");

  color = rInfo->createColorDefinition();
  color->setId("white");
  color->setColorValue("#FFFFFF");

  // add a linear gradient from black to white to silver
  LinearGradient *gradient = rInfo->createLinearGradientDefinition();
  gradient->setId("simpleGradient");
  gradient->setPoint1(RelAbsVector(), RelAbsVector());
  gradient->setPoint2(RelAbsVector(0, 100), RelAbsVector(0, 100));

  GradientStop *stop = gradient->createGradientStop();
  stop->setOffset(RelAbsVector());
  stop->setStopColor("white");

  stop = gradient->createGradientStop();
  stop->setOffset(RelAbsVector(0, 100));
  stop->setStopColor("silver");

  // add a species style that represents them as ellipses with the gradient above
  Style* style = rInfo->createStyle("ellipseStyle");
  style->getGroup()->setFillColor("simpleGradient");
  style->getGroup()->setStroke("black");
  style->getGroup()->setStrokeWidth(2.0);
  style->addType("SPECIESGLYPH");

  Ellipse* ellipse = style->getGroup()->createEllipse();
  ellipse->setCenter2D(RelAbsVector(0, 50), RelAbsVector(0, 50));
  ellipse->setRadii(RelAbsVector(0, 50), RelAbsVector(0, 50));

}

int main(int argc,char** argv){

  if (argc != 3)
  {
    cerr << "usage: addRenderInformation <input file> <output file>" << endl;
    cerr << "       Adds a render information object to the input file." << endl;
    return 1;
  }
  
  string inputFile = argv[1];
  string outputFile = argv[2];
  
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
    cout << "The loaded model contains global Render information. " << endl;	
  } 

  // add render information to the first layout
  Layout *layout = plugin->getLayout(0);

  RenderLayoutPlugin *rPlugin = (RenderLayoutPlugin*) layout->getPlugin("render");
  if (rPlugin != NULL && rPlugin->getNumLocalRenderInformationObjects() > 0)
  {
    cout << "The loaded model contains local Render information. " << endl;
  }
  else 
  {
    string uri = (doc->getLevel() == 2 ? RenderExtension::getXmlnsL2() :   
      RenderExtension::getXmlnsL3V1V1());

    // enable render package
    doc->enablePackage(uri, "render", true);
    doc->setPackageRequired("render", false);

    rPlugin = (RenderLayoutPlugin*) layout->getPlugin("render");

    addRenderInformation(rPlugin);

    writeSBMLToFile(doc, outputFile.c_str());

  }


  return 0;
}

