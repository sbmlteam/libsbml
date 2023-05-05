#!/usr/bin/env python3
##
## @file    setNamesFromIds.py
## @brief   Utility program, renaming all Names to match their ids. 
##
## @author  Frank T. Bergmann
## 
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
## 
## 

import sys
import os.path
import time 
import libsbml

# This class implements an identifier transformer, that means it can be used
# to rename all sbase elements. 
class SetNamesFromId(libsbml.IdentifierTransformer):
  def __init__(self):
    # call the constructor of the base class
    libsbml.IdentifierTransformer.__init__(self)

  # The function actually doing the transforming. This function is called 
  # once for each SBase element in the model. 
  def transform(self, element):
    # return in case we don't have a valid element
    if element is None or element.getTypeCode() == libsbml.SBML_LOCAL_PARAMETER:
        return libsbml.LIBSBML_OPERATION_SUCCESS

    # or if there is nothing to do
    if element.isSetId() == False or element.getId() == element.getName():
        return libsbml.LIBSBML_OPERATION_SUCCESS

   	# set it
    element.setName(element.getId())

    return libsbml.LIBSBML_OPERATION_SUCCESS

def main (args):
  """Usage: setNamesFromIds filename output
  """
  if len(args) != 3:
    print(main.__doc__)
    sys.exit(1)
  
  filename = args[1]
  output = args[2]

  # read the document
  start = time.time() * 1000
  document = libsbml.readSBMLFromFile(filename)
  stop = time.time() * 1000

  print ("")
  print ("            filename: {0}".format( filename))
  print ("      read time (ms): {0}".format( stop - start))

  # stop in case of serious errors
  errors = document.getNumErrors(libsbml.LIBSBML_SEV_ERROR)
  if errors > 0:
      print ("            error(s): {0}".format(errors))
      document.printErrors()
      sys.exit (errors)

  # get a list of all elements, as we will need to know all identifiers
  allElements = document.getListOfAllElements()

  # create the transformer
  trans = SetNamesFromId()

  # rename the identifiers (using the elements we already gathered before)
  start = time.time() * 1000
  document.getModel().renameIDs(allElements, trans)
  stop = time.time() * 1000
  print ("    rename time (ms): {0}".format(stop - start))

  # write to file
  start = time.time() * 1000
  libsbml.writeSBMLToFile(document, output)
  stop = time.time() * 1000
  print ("     write time (ms): {0}".format(stop - start))
  print ("")

  # if we got here all went well ... 
  
if __name__ == '__main__':
  main(sys.argv)  
