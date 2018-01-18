# 
# @file    printRenderInformation.py
# @brief   prints an overview of the render information in the given SBML file
# @author  Frank Bergmann
# 
# This file is part of libSBML.  Please visit http:#sbml.org for more
# information about SBML, and the latest version of libSBML.
# 
import sys
import os.path
from libsbml import *


def toString(vec):
    return str(vec.getAbsoluteValue()) + " + " + str(vec.getRelativeValue()) + "%"

def main (args):
  """
    Usage:  printRenderInformation <input file>
            prints a summary of the render information object.
  """
  
  if (len(args) != 2):
        print(main.__doc__)
        return 1


  inputFile = args[1];

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
      print("The loaded model contains global Render information: ");

      for  i in range ( 0, lolPlugin.getNumGlobalRenderInformationObjects()):
          info = lolPlugin.getRenderInformation(i);

          print("Id: " + info.getId());
          print("Name: " + info.getName());
          print("Program Name: " + info.getProgramName());
          print("Program Version: " + info.getProgramVersion());
          print("Background color: " + info.getBackgroundColor());

          print("Color Definitions:");
          for  j in range ( 0, info.getNumColorDefinitions()):
              color = info.getColorDefinition(j);			  
              print("\tcolor: " + str(j) +
                                " id: " + color.getId() +
                                " color: " + color.createValueString());

          print("GradientDefinitions: ");
          for j in range (0, info.getNumGradientDefinitions()):
              grad = info.getGradientDefinition(j);

              if (grad.getElementName() == "linearGradient"):
                  print("\tLinear Gradient: " + grad.getId()
                                    + " start: " + toString(grad.getXPoint1()) + ", " +
                                    toString(grad.getYPoint1())
                                    + " end: " + toString(grad.getXPoint2()) + ", " +
                                    toString(grad.getYPoint2())
                      );
              else:
                  print("\tRadial Gradient: " + grad.getId()
                                    + " center: " + toString(grad.getCenterX()) + ", " +
                                    toString(grad.getCenterY())
                                    + " focal: " + toString(grad.getFocalPointX()) + ", " +
                                    toString(grad.getFocalPointY())
                      );

              for k in range (0, grad.getNumGradientStops()):
                  stop = grad.getGradientStop(k);
                  print("\t\tstop " + str(k) + " id: " + stop.getId() + " stop-color: " +
                                    stop.getStopColor());

          # similarly for the remaining elements 
          print("\tNumber of Line Endings: " + str(info.getNumLineEndings()));

          # and finally the styles

          for j in range (0, info.getNumStyles()):
              style = info.getStyle(j);

              print("\tstyle " + str(j) + " id: " + style.getId() + " applies to: ");
              print("\t\troles:" + style.createRoleString() +
                                " types: " + style.createTypeString());

  # add render information to the first layout
  layout = plugin.getLayout(0);

  rPlugin = layout.getPlugin("render");
  if (rPlugin != None and rPlugin.getNumLocalRenderInformationObjects() > 0):
      print("The loaded model contains local Render information. ");
      # here we would do the same as above for the local render information ...

  return 0;

if __name__ == '__main__':
  main(sys.argv)  
