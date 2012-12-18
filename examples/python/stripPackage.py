#!/usr/bin/env python
##
## @file    stripPackage.py
## @brief   Strips the given package from the given SBML file.
## @author  Frank T. Bergmann
## 
##
## This file is part of libSBML.  Please visit http://sbml.org for more
## information about SBML, and the latest version of libSBML.

import sys
import os.path
import libsbml

def main (args):
  """usage: stripPackage.py input-filename package-to-strip output-filename
  """
  if len(args) != 4:
    print(main.__doc__)
    sys.exit(1)

  infile  = args[1]
  package = args[2]
  outfile = args[3]

  if not os.path.exists(infile):
    print("[Error] %s : No such file." % (infile))
    sys.exit(1)

  reader  = libsbml.SBMLReader()
  writer  = libsbml.SBMLWriter()
  sbmldoc = reader.readSBML(infile)

  if sbmldoc.getNumErrors() > 0:
    if sbmldoc.getError(0).getErrorId() == libsbml.XMLFileUnreadable:
      # Handle case of unreadable file here.
      sbmldoc.printErrors()
    elif sbmldoc.getError(0).getErrorId() == libsbml.XMLFileOperationError:
      # Handle case of other file error here.
      sbmldoc.printErrors()
    else:
      # Handle other error cases here.
      sbmldoc.printErrors()

    sys.exit(1)
    
  props = libsbml.ConversionProperties()
  props.addOption("stripPackage", True, "Strip SBML Level 3 package constructs from the model")
  props.addOption("package", package, "Name of the SBML Level 3 package to be stripped")
  if (sbmldoc.convert(props) != libsbml.LIBSBML_OPERATION_SUCCESS):
	print("[Error] Conversion failed...")
	sys.exit(1)
  
  writer.writeSBML(sbmldoc, outfile)
  print("[OK] stripped package '%s' from %s to %s" % (package, infile, outfile))

if __name__ == '__main__':
  main(sys.argv)  
