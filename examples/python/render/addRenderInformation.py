# 
# @file    addRenderInformation.py
# @brief   Adds render information to the given SBML file
# @author  Frank Bergmann
# 
# This file is part of libSBML.  Please visit http:#sbml.org for more
# information about SBML, and the latest version of libSBML.
# 

import sys
import os.path
from libsbml import *

def addRenderInformation(rPlugin):
    if (rPlugin == None):
        print("could not add render information!");
        sys.exit(4);

    rInfo = rPlugin.createLocalRenderInformation();
    rInfo.setId("info");
    rInfo.setName("Example Render Information");
    rInfo.setProgramName("RenderInformation Examples");
    rInfo.setProgramVersion("1.0");

    # add some colors
    color = rInfo.createColorDefinition();
    color.setId("black");
    color.setColorValue("#000000");

    color = rInfo.createColorDefinition();
    color.setId("silver");
    color.setColorValue("#c0c0c0");

    color = rInfo.createColorDefinition();
    color.setId("white");
    color.setColorValue("#FFFFFF");

    # add a linear gradient from black to white to silver
    gradient = rInfo.createLinearGradientDefinition();
    gradient.setId("simpleGradient");
    gradient.setPoint1(RelAbsVector(), RelAbsVector());
    gradient.setPoint2(RelAbsVector(0, 100), RelAbsVector(0, 100));

    stop = gradient.createGradientStop();
    stop.setOffset(RelAbsVector());
    stop.setStopColor("white");

    stop = gradient.createGradientStop();
    stop.setOffset(RelAbsVector(0, 100));
    stop.setStopColor("silver");

    # add a species style that represents them as ellipses with the gradient above
    style = rInfo.createStyle("ellipseStyle");
    style.getGroup().setFillColor("simpleGradient");
    style.getGroup().setStroke("black");
    style.getGroup().setStrokeWidth(2.0);
    style.addType("SPECIESGLYPH");

    ellipse = style.getGroup().createEllipse();
    ellipse.setCenter2D(RelAbsVector(0, 50), RelAbsVector(0, 50));
    ellipse.setRadii(RelAbsVector(0, 50), RelAbsVector(0, 50));

	
def main (args):
  """
    Usage:  addRenderInformation <input file> <output file>
            Adds a render information object to the input file.
  """
  
  if (len(args) != 3):
        print(main.__doc__)
        return 1


  inputFile = args[1];
  outputFile = args[2];

  doc = readSBMLFromFile(inputFile);
  numErrors = doc.getNumErrors();

  if (numErrors > 0):
      print("Encountered errors while reading the file. ");
      print("Please correct the following errors and try again.");
      doc.printErrors();
      return 2;

  model = doc.getModel();

  plugin = model.getPlugin("layout");

  if (plugin == None or plugin.getNumLayouts() == 0):
      print("The loaded model contains no layout information, please add these first.");
      return 3;

  lolPlugin = plugin.getListOfLayouts().getPlugin("render");
  if (lolPlugin != None and lolPlugin.getNumGlobalRenderInformationObjects() > 0):
      print("The loaded model contains global Render information. ");

  # add render information to the first layout
  layout = plugin.getLayout(0);

  rPlugin = layout.getPlugin("render");
  if (rPlugin != None and rPlugin.getNumLocalRenderInformationObjects() > 0):
      print("The loaded model contains local Render information. ");
  else:
      uri = RenderExtension.getXmlnsL2() if doc.getLevel() == 2 else RenderExtension.getXmlnsL3V1V1();

      # enable render package
      doc.enablePackage(uri, "render", True);
      doc.setPackageRequired("render", False);

      rPlugin = layout.getPlugin("render");

      addRenderInformation(rPlugin);

      writeSBMLToFile(doc, outputFile);

  return 0;

if __name__ == '__main__':
  main(sys.argv)  
