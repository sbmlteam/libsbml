#!/usr/bin/env python
#
# @file   writeErrorTable.py
# @brief  Write the error table used in documenting SBMLError
# @author Sarah Keating
# @author Michael Hucka
# 
#<!---------------------------------------------------------------------------
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.
#
# Copyright (C) 2009-2011 jointly by the following organizations: 
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
#----------------------------------------------------------------------- -->*/

import re, os, sys, string
from libsbml import *

def WriteSev(error, line):
  sev = error.getSeverity()
  if (sev == 3):
    line = line + "<td class=\"s-fatal\">F</td>"
  elif (sev == 2):
    line = line + "<td class=\"s-error\">E</td>"
  elif (sev == 1):
    line = line + "<td class=\"s-warning\">W</td>"
  else:
    line = line + "<td class=\"s-na\">N</td>"
  return line

def fixupHTML(text):
  text = text.replace('<', '&lt;')
  text = text.replace('>', '&gt;')
  return text

if len(sys.argv) != 1:
  print 'Usage: writeErrorTable.py'
else:
  errfile = open("list.txt")
  output = open("out.txt", "w")
  for decr in errfile.readlines():
    num = int (decr[40:45])
    print num
    blank = decr.find(" ")
    name = decr[0:blank]
    error = SBMLError(num,1,1)
    line = " * <tr><td><code>"
    line = line + name
    line = line + "</code></td><td>"
    line = line + fixupHTML(error.getShortMessage()) + "</td>"
    line = WriteSev(error, line)
    error = SBMLError(num,1,2)
    line = WriteSev(error, line)
    error = SBMLError(num,2,1)
    line = WriteSev(error, line)
    error = SBMLError(num,2,2)
    line = WriteSev(error, line)
    error = SBMLError(num,2,3)
    line = WriteSev(error, line)
    error = SBMLError(num,2,4)
    line = WriteSev(error, line)
    error = SBMLError(num,3,1)
    line = WriteSev(error, line)
    line = line +"</tr>\n"
    output.write(line)
