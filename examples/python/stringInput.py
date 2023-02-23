#!/usr/bin/env python3
## 
## @file    stringInput.py
## @brief   illustrates how libSBML deals with string and unicode input
## @author  Brett Olivier
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

import sys
import os.path
import libsbml

def main (args):
  """Usage: stringInput filename
  """

  if len(args) != 2:
    print(main.__doc__)
    sys.exit(2)

  filename = args[1]
  document = libsbml.readSBMLFromFile(filename)

  errors = document.getNumErrors()

  print("filename: " + filename + "\n")

  if errors > 0:
      document.printErrors()
      return errors

  # Model
  
  m = document.getModel()

  # testing ascii input, this should work for Python 2 and 3
  
  ascii_name = str('new_ascii_name')
  m.setName(ascii_name)
  
  print(type(m.getName()))
  
  # testing unicode input, this should also work for Python 3 and
  # Python 2 if libSBML has been compiled with Swig version > 3.0.8
  
  unicode_name = u'new_unicode_name'
  m.setName(unicode_name)
  
  print(type(m.getName()))
  
  return errors


if __name__ == '__main__':
  main(sys.argv)  
