#!/usr/bin/env python
#
# @file   writeErrors.py
# @brief  Write the error enumeration used in documenting SBMLError
# @author Sarah Keating
# @author Michael Hucka
#
# $Id$
# $HeadURL$
#
# <!--------------------------------------------------------------------------
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
# ---------------------------------------------------------------------- -->*/

import re, os, sys, string
from libsbml import *

def fixupHTML(text):
  text = text.replace(' Level ', ' Level&nbsp;')
  text = text.replace(' Version ', ' Version&nbsp;')
  text = text.replace(" <", " <code>&lt;")
  text = text.replace(">", "&gt;</code>")
  text = text.replace("<code&gt;</code>&lt;", "<code>&lt;")
  text = text.replace(" '", " <code>")
  text = text.replace("' ", "</code> ")
  return text

if len(sys.argv) != 1:
  print 'Usage: writeErrors.py'
else:
  errfile = open("list.txt")
  output = open("out2.txt", "w")
  for decr in errfile.readlines():
    num = int (decr[40:45])
    error = SBMLError(num, 3,1)
    line = " , "
    line = line + decr[0:45]
    line = line + " /*!< " + fixupHTML(error.getShortMessage()) + " */\n"	
    output.write(line)
