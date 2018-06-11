#!/usr/bin/env python
##
## @file    print_groups.py
## @brief   Example printing the groups contained in an sbml file
## @author  Frank T. Bergmann
##
## <!--------------------------------------------------------------------------
## This sample program is distributed under a different license than the rest
## of libSBML.  This program uses the open-source MIT license, as follows:
##
## Copyright (c) 2013-2018 by the California Institute of Technology
## (California, USA), the European Bioinformatics Institute (EMBL-EBI, UK)
## and the University of Heidelberg (Germany), with support from the National
## Institutes of Health (USA) under grant R01GM070923.  All rights reserved.
##
## Permission is hereby granted, free of charge, to any person obtaining a
## copy of this software and associated documentation files (the "Software"),
## to deal in the Software without restriction, including without limitation
## the rights to use, copy, modify, merge, publish, distribute, sublicense,
## and/or sell copies of the Software, and to permit persons to whom the
## Software is furnished to do so, subject to the following conditions:
##
## The above copyright notice and this permission notice shall be included in
## all copies or substantial portions of the Software.
##
## THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
## IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
## FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
## THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
## LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
## FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
## DEALINGS IN THE SOFTWARE.
##
## Neither the name of the California Institute of Technology (Caltech), nor
## of the European Bioinformatics Institute (EMBL-EBI), nor of the University
## of Heidelberg, nor the names of any contributors, may be used to endorse
## or promote products derived from this software without specific prior
## written permission.
## ------------------------------------------------------------------------ -->

import sys
import os.path
from libsbml import *

def print_member(member):
  print ("  > Member")
  if member.isSetIdRef():
    print ("   idRef     : {0}".format(member.getIdRef()))
  if member.isSetMetaIdRef():
    print ("   metaIdRef : {0}".format(member.getMetaIdRef()))

def print_group(group):
  if group is None: 
    return
  
  print ("# Group")
  if group.isSetId():
    print ("  id         : {0}".format(group.getId()))
  if group.isSetName():
    print ("  name       : {0}".format(group.getName()))
  if group.isSetSBOTerm():
    print ("  SBO term   : {0}".format(group.getSBOTermID()))
  if group.isSetKind():
    print ("  kind       : {0}".format(group.getKindAsString()))
    
  members = group.getListOfMembers()
  for member in members:
    print_member(member)
  

def main (args):
  """Usage: print_groups filename
  """

  if len(args) != 2:
    print(main.__doc__)
    sys.exit(2)

  filename = args[1]
  document = readSBML(filename)

  errors = document.getNumErrors(LIBSBML_SEV_ERROR)

  print("filename: {0}\n".format(filename))

  if errors > 0:
      document.printErrors()
      return errors

  # Model  
  m = document.getModel()
  
  plug = m.getPlugin('groups')
  if plug is None: 
    print("This file does not have groups enabled");
    return
    
  
  list = plug.getListOfGroups()
  print("number of groups: {0}\n".format(list.size()))
  for group in list: 
    print_group(group)
    
  
    
  


if __name__ == '__main__':
  main(sys.argv)  
