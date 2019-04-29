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
import libsbml


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

  doc = libsbml.readSBMLFromFile(inputFile)

  print("Using libSBML: {0} supporting packages for:".format(libsbml.getLibSBMLDottedVersion()))
  for i in range (libsbml.SBMLExtensionRegistry.getNumRegisteredPackages()):
    print("\t" + libsbml.SBMLExtensionRegistry.getRegisteredPackageName(i))

  print("\nThe document is: level {0}, version {1}".format(doc.getLevel(), doc.getVersion()))
  for i in range(doc.getNumPlugins()):
      print("  doc uses package: {0}".format(doc.getPlugin(i).getElementNamespace()))
    
  print("\n")

  numErrors = doc.getNumErrors(libsbml.LIBSBML_SEV_ERROR)

  if numErrors > 0:
      print("Encountered errors while reading the file. ")
      print("Please correct the following errors and try again.")
      doc.printErrors()
      return 2

  model = doc.getModel()

  plugin = model.getPlugin("layout")

  if plugin == None or plugin.getNumLayouts() == 0:
      print("The loaded model contains no layout information, please add these first.")
      return 3

  lolPlugin = plugin.getListOfLayouts().getPlugin("render")
  if (lolPlugin != None and lolPlugin.getNumGlobalRenderInformationObjects() > 0):
      print("The loaded model contains global Render information: ")

      for  i in range ( 0, lolPlugin.getNumGlobalRenderInformationObjects()):
          info = lolPlugin.getRenderInformation(i)

          print_render_info(info)

  # add render information to the first layout
  layout = plugin.getLayout(0)

  rPlugin = layout.getPlugin("render")
  if (rPlugin != None and rPlugin.getNumLocalRenderInformationObjects() > 0):
      print("The loaded model contains local Render information. ")
      # here we would do the same as above for the local render information ...

  return 0

def print_render_info(info):
    if info.isSetId(): 
        print("  Id: " + info.getId())
    if info.isSetName(): 
        print("  Name: " + info.getName())
    if info.isSetProgramName():
        print("  Program Name: " + info.getProgramName())
    if info.isSetProgramVersion():
        print("  Program Version: " + info.getProgramVersion())
    if info.isSetBackgroundColor():
        print("  Background color: " + info.getBackgroundColor())

    print("\nColor Definitions:")
    for  j in range ( 0, info.getNumColorDefinitions()):
        color = info.getColorDefinition(j)			  
        print("\tcolor: " + str(j) +
                          " id: " + color.getId() +
                          " color: " + color.createValueString())

    print("\nGradientDefinitions: ")
    for j in range (0, info.getNumGradientDefinitions()):
        grad = info.getGradientDefinition(j)

        print_gradient_definition(grad)

    # similarly for the remaining elements 
    print("\nNumber of Line Endings: " + str(info.getNumLineEndings()))

    # and finally the styles
    print("\nStyles: ")
    for j in range (0, info.getNumStyles()):
        style = info.getStyle(j)
        print_style(style, j)

def print_style(style, j):
    # type: (libsbml.Style, int) -> None
    print("\tstyle " + str(j) + " id: " + style.getId() + " applies to: ")
    print("\t\troles:" + style.createRoleString() +
                      " types: " + style.createTypeString())

    if not style.isSetGroup(): 
        return
    
    group = style.getGroup()
    if group.isSetStroke():
        print("\t\tstroke: {0}".format(group.getStroke()))

    if group.isSetFill():
        print("\t\tfill: {0}".format(group.getFill()))

    for element in group.getListOfElements(): 
        if isinstance(element, libsbml.GraphicalPrimitive2D):
            print ("\t\tsub element {0} stroke {1}, fill {2}".format(element.getElementName(), element.getStroke(), element.getFill()))
        elif isinstance(element, libsbml.GraphicalPrimitive1D): 
            print ("\t\tsub element {0} stroke {1}".format(element.getElementName(), element.getStroke()))

def print_gradient_definition(grad):
    if (grad.getElementName() == "linearGradient"):
        print("\tLinear Gradient: " + grad.getId()
                          + " start: " + toString(grad.getXPoint1()) + ", " +
                          toString(grad.getYPoint1())
                          + " end: " + toString(grad.getXPoint2()) + ", " +
                          toString(grad.getYPoint2())
            )
    else:
        print("\tRadial Gradient: " + grad.getId()
                          + " center: " + toString(grad.getCenterX()) + ", " +
                          toString(grad.getCenterY())
                          + " focal: " + toString(grad.getFocalPointX()) + ", " +
                          toString(grad.getFocalPointY())
            )

    for k in range (0, grad.getNumGradientStops()):
        stop = grad.getGradientStop(k)
        print("\t\tstop " + str(k) + " id: " + stop.getId() + " stop-color: " +
                          stop.getStopColor())

if __name__ == '__main__':
  main(sys.argv)  
