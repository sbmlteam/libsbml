#!/usr/bin/env python
# =============================================================================
# @file   writeErrorTable.py
# @brief  Write documentation for SBML error codes
# @author Sarah Keating
# @author Michael Hucka
# =============================================================================
#
# This Python program interrogates the currently-installed libSBML library to
# generate two output files in the current diretory:
#
#  a) The complete file docs/src/common-text/sbmlerror-table.html.  Once
#     created, the file sbmlerror-table.html can be copied to its normal
#     location in docs/src/common-text/
#  
#  b) A file named "enum.txt" containing a portion of the contents of
#     src/sbml/SBMLError.h.  Once created, the contents of enum.txt must be
#     inserted manually into SBMLError.h as the body of the typedef enum
#     SBMLErrorCode_t.
#
# The purpose of doing this is to allow the text of the short error messages
# to be maintained in one place, SBMLErrorTable.h, and then this used to
# generate the documentation for the errors.  The documentation appears in
# two forms: the sbmlerror-table.html file, and the Doxygen-compatible
# comments placed next to each error code number in the enum SBMLErrorCode_t.
#
# Here is how to use this program:
#
#  1) Make any desired updates to the text of the diagnostic messages in
#     src/sbml/SBMLErrorTable.h.
# 
#  2) Rebuild and install libSBML on your computer.
# 
#  3) Configure your PYTHONPATH environment variable to encompass this newly-
#     installed copy of libSBML.  Double-check that your Python executable
#     is in fact picking up the copy of libSBML you think it is.
# 
#  4) Run the command
#
#       python writeErrorTable.py
#
#  5) Replace sbmlerror-table.html and edit SBMLError.h, as mentioned above.
# 
#<!-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.
#
# Copyright (C) 2009-2013 jointly by the following organizations: 
#     1. California Institute of Technology, Pasadena, CA, USA
#     2. EMBL European Bioinformatics Institute (EBML-EBI), Hinxton, UK
#  
# Copyright (C) 2006-2008 by the California Institute of Technology,
#     Pasadena, CA, USA 
#  
# Copyright (C) 2002-2005 jointly by the following organizations: 
#     1. California Institute of Technology, Pasadena, CA, USA
#     2. Japan Science and Technology Agency, Japan
# 
# This library is free software; you can redistribute it and/or modify it
# under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation.  A copy of the license agreement is provided
# in the file named "LICENSE.txt" included with this software distribution
# and also available online as http://sbml.org/software/libsbml/license.html
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -->

import re, os, sys

from types import *
from imp import *
from string import *
from libsbml import *


# -----------------------------------------------------------------------------
# Globals.
# -----------------------------------------------------------------------------

# List of error codes that we ignore for purposes of documentation.

listOfErrorsToIgnore = [20905, 21112, 90000, 99502, 99503, 99504, 99999]


# -----------------------------------------------------------------------------
# Helper functions.
# -----------------------------------------------------------------------------

def getModule():
  m = find_module("libsbml")
  return load_module('_libsbml', None, m[1], m[2])


def errorSymbol(module, errorNum):
  # This is an inefficient way of getting the string corresponding to a
  # given error number, but we don't care because this application isn't
  # performance-critical.  The algorithm is also fragile, but if we're
  # careful about the range of values attempted (i.e., numbers from 10000
  # to 100000), it should be okay.
  #
  symbols = dir(module)
  attributes = [None]*len(symbols) 
  for i in range(0, len(symbols)):
    attributes[i] = getattr(module, symbols[i])
  for i in range(0, len(symbols)):
    if isinstance(attributes[i], int) == True and attributes[i] == errorNum:
      return symbols[i]


def writeTableHeader(stream):
  stream.write('<center>\n\
<table width="95%" cellspacing="1" cellpadding="2" border="0" class="text-table small-font alt-row-colors">\n\
 <tr style="background: lightgray" class="normal-font">\n\
     <th valign="bottom"><strong>Enumerator</strong></th>\n\
     <th valign="bottom"><strong>Meaning</strong></th>\n\
     <th align="center" width="15">L 1 V 1</th>\n\
     <th align="center" width="15">L 1 V 2</th>\n\
     <th align="center" width="15">L 2 V 1</th>\n\
     <th align="center" width="15">L 2 V 2</th>\n\
     <th align="center" width="15">L 2 V 3</th>\n\
     <th align="center" width="15">L 2 V 4</th>\n\
     <th align="center" width="15">L 3 V 1</th>\n\
 </tr>\n\
')


def writeTableFooter(stream):
  stream.write('<table>\n\
</center>\n\
')


def toHTML(text):
  text = text.replace('<', '&lt;')
  text = text.replace('>', '&gt;')
  text = text.replace('&lt;', '<code>&lt;')
  text = text.replace('&gt;', '&gt;</code>')
  return text


def severityText(severity):
  if (severity == 3):
    return "<td class=\"s-fatal\">F</td>"
  elif (severity == 2):
    return "<td class=\"s-error\">E</td>"
  elif (severity == 1):
    return "<td class=\"s-warning\">W</td>"
  else:
    return "<td class=\"s-na\">N</td>"


def writeSeverities(stream, errorNum):
  e = SBMLError(errorNum, 1, 1)
  stream.write(severityText(e.getSeverity()))
  e = SBMLError(errorNum, 1, 2)
  stream.write(severityText(e.getSeverity()))
  e = SBMLError(errorNum, 2, 1)
  stream.write(severityText(e.getSeverity()))
  e = SBMLError(errorNum, 2, 2)
  stream.write(severityText(e.getSeverity()))
  e = SBMLError(errorNum, 2, 3)
  stream.write(severityText(e.getSeverity()))
  e = SBMLError(errorNum, 2, 4)
  stream.write(severityText(e.getSeverity()))
  e = SBMLError(errorNum, 3, 1)
  stream.write(severityText(e.getSeverity()))


def writeTableBody(stream):
  global listOfErrorsToIgnore

  module = getModule()
  for errNum in list(set(range(10000, 100000)) - set(listOfErrorsToIgnore)):
    e = SBMLError(errNum, 1, 1)
    if e.isValid():
      sys.stdout.write(".")             # Echo to terminal to show progress
      sys.stdout.flush()
      stream.write("<tr>")
      stream.write("<td><code>" + errorSymbol(module, errNum) + "</code></td>")
      stream.write("<td>" + toHTML(e.getShortMessage()) + "</td>")
      writeSeverities(stream, errNum)
      stream.write("</tr>\n")
  sys.stdout.write("\n")
    

def writeEnumBody(stream):
  module = getModule()
  for errNum in range(10000, 100000):
    e = SBMLError(errNum, 1, 1)
    if e.isValid():
      sys.stdout.write(".")             # Echo to terminal to show progress
      sys.stdout.flush()
      if errNum == 10000:
        stream.write("  ")
      else:
        stream.write(", ")
      stream.write("%s = %s" %(ljust(errorSymbol(module, errNum), 37), str(errNum)))
      msg = e.getShortMessage()
      if msg != '':
        stream.write(" /*!< " + toHTML(msg) + ". */")
      stream.write("\n")

  sys.stdout.write("\n")



# -----------------------------------------------------------------------------
# Main.
# -----------------------------------------------------------------------------

def main (args):
  """Usage:
            python writeErrorTable.py
  """
  # Start with some sanity-checking.

  if len(sys.argv) != 1:
    print(main.__doc__)
    sys.exit(1)

  # Write sbmlerror-table.html.

  print "Writing sbmlerror-table.html"
  stream = open("sbmlerror-table.html", 'w')
  writeTableHeader(stream)
  writeTableBody(stream)
  writeTableFooter(stream)
  stream.close()

  # Write the guts of the SBMLError.h enumeration.

  print "Writing enum.txt"
  stream = open("enum.txt", 'w')
  writeEnumBody(stream)
  stream.close()


if __name__ == '__main__':
  main(sys.argv)
