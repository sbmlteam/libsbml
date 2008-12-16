#!/usr/bin/env python
##
## @file    echoSBML.py
## @brief   Echos an SBML model.
## @author  Akiya Jouraku (translated from libSBML C++ examples)
## @author  Ben Bornstein
## @author  Michael Hucka
## 
## $Id$
## $HeadURL$
##
## This file is part of libSBML.  Please visit http://sbml.org for more
## information about SBML, and the latest version of libSBML.
##

import sys
import os.path
import libsbml

def main (args):
  """usage: echoSBML.py input-filename output-filename
  """
  if len(args) != 3:
    print main.__doc__
    sys.exit(1)

  infile  = args[1]
  outfile = args[2]

  if not os.path.exists(infile):
    print "[Error] %s : No such file." % (infile)
    sys.exit(1)

  reader  = libsbml.SBMLReader()
  writer  = libsbml.SBMLWriter()
  sbmldoc = reader.readSBML(infile)

  if sbmldoc.getNumErrors() > 0:
    sbmldoc.printErrors()
    print "[Error] Cannot read %s" % (infile)    
    sys.exit(1)
    
  writer.writeSBML(sbmldoc, outfile)
  print "[OK] Echoed %s to %s" % (infile, outfile)

if __name__ == '__main__':
  main(sys.argv)  
