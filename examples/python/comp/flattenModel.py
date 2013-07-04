#!/usr/bin/env python
##
## @file    flattenModel.py
## @brief   Flattens the comp code from the given SBML file.
## @author  Frank T. Bergmann
##
##
## This file is part of libSBML.  Please visit http://sbml.org for more
## information about SBML, and the latest version of libSBML.

import sys
import os.path
import libsbml

def main (args):
  """usage: flattenModel.py [-p] input-filename output-filename
      -p : list unused ports
  """
  if len(args) != 4 and len(args) != 3 :
    print(main.__doc__)
    sys.exit(1)

  leavePorts = False

  if len(args) == 3:
    infile  = args[1]
    outfile = args[2]
  elif len(args) == 4:
    if args[1] != "-p":
      print(main.__doc__)
      sys.exit(1)
    else:
      leavePorts = True
      infile  = args[2]
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

  # Create the converter options
  props = libsbml.ConversionProperties()
  props.addOption("flatten comp", True, "flatten comp")
  props.addOption("leavePorts", leavePorts, "unused ports should be listed in the flattened model")

  # do conversion
  result = sbmldoc.convert(props)
  if (result != libsbml.LIBSBML_OPERATION_SUCCESS):
    sbmldoc.printErrors()
    print("[Error] Conversion failed... ("+ str(result) + ")")
    sys.exit(1)

  writer.writeSBML(sbmldoc, outfile)
  print("Flat model written to %s" % (outfile))

if __name__ == '__main__':
  main(sys.argv)
