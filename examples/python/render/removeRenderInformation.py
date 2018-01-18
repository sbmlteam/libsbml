# 
# @file    removeRenderInformation.py
# @brief   removes render information from the given SBML file
# @author  Frank Bergmann
# 
# This file is part of libSBML.  Please visit http:#sbml.org for more
# information about SBML, and the latest version of libSBML.
# 
import sys
import os.path
from libsbml import *



def main (args):
  """
    Usage:  removeRenderInformation <input file> <output file>
            removes the render information object from the input file.
  """
  
  if (len(args) != 3):
        print(main.__doc__)
        return 1

  inputFile = args[1];
  outputFile = args[2];

  doc = readSBMLFromFile(inputFile)
  numErrors = doc.getNumErrors()

  if (numErrors > 0):
      print( "Encountered errors while reading the file. " );
      print( "Please correct the following errors and try again." );
      doc.printErrors();
      return 2;

  plugin = doc.getPlugin("render");
  if (plugin == None):
      print( "Warning: the document did not use the render information in the first place. " );
  else:
      # simply disable the package, this will cause it to no longer being written out
      doc.disablePackage(plugin.getURI(), plugin.getPrefix());

  writeSBMLToFile(doc, outputFile);

  return 0;


if __name__ == '__main__':
  main(sys.argv)  
