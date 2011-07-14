#!/usr/bin/env python
##
## @file    echoSBML.py
## @brief   Echos an SBML model.
## @author  Akiya Jouraku (translated from libSBML C++ examples)
## @author  Ben Bornstein
## @author  Michael Hucka
## 
##
## This file is part of libSBML.  Please visit http://sbml.org for more
## information about SBML, and the latest version of libSBML.

import sys
import os.path
import libsbml

def main (args):
  """usage: echoSBML.py input-filename output-filename
  """
  if len(args) != 3:
    print(main.__doc__)
    sys.exit(1)

  infile  = args[1]
  outfile = args[2]

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
    
  writer.writeSBML(sbmldoc, outfile)
  print("[OK] Echoed %s to %s" % (infile, outfile))

if __name__ == '__main__':
  main(sys.argv)  
