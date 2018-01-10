#!/usr/bin/env python
##
## @file    convertLayout.py
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
from libsbml import *

def convertDocToL2(doc, outputFile):
  layoutNsUri = "http://projects.eml.org/bcb/sbml/level2"
  layoutNs = LayoutPkgNamespaces(2, 4)
  renderNsUri = "http://projects.eml.org/bcb/sbml/render/level2"
  renderNs = RenderPkgNamespaces(2, 4)

  prop = ConversionProperties(SBMLNamespaces(2,4))
  prop.addOption('strict', False)
  prop.addOption('setLevelAndVersion', True)
  prop.addOption('ignorePackages', True)
  doc.convert(prop)


  docPlugin = doc.getPlugin("layout")
  if docPlugin is not None:
    docPlugin.setElementNamespace(layoutNsUri)

  doc.getSBMLNamespaces().removePackageNamespace(3, 1, "layout", 1)
  doc.getSBMLNamespaces().addPackageNamespace("layout", 1)

  rdocPlugin = doc.getPlugin("render")
  if rdocPlugin is not None:
    rdocPlugin.setElementNamespace(renderNsUri)

  doc.getSBMLNamespaces().removePackageNamespace(3, 1, "render", 1)
  doc.getSBMLNamespaces().addPackageNamespace("render", 1)

  writeSBMLToFile(doc, outputFile)

def convertDocToL3(doc, outputFile):
  layoutNsUri = "http://www.sbml.org/sbml/level3/version1/layout/version1"
  layoutNs = LayoutPkgNamespaces(3, 1)
  renderNsUri = "http://www.sbml.org/sbml/level3/version1/render/version1"
  renderNs = RenderPkgNamespaces(3, 1)

  prop = ConversionProperties(SBMLNamespaces(3,1))
  prop.addOption('strict', False)
  prop.addOption('setLevelAndVersion', True)
  prop.addOption('ignorePackages', True)
  doc.convert(prop)


  docPlugin = doc.getPlugin("layout")
  if docPlugin is not None:
    docPlugin.setElementNamespace(layoutNsUri)

  doc.getSBMLNamespaces().removePackageNamespace(3, 1, "layout", 1)
  doc.getSBMLNamespaces().addPackageNamespace("layout", 1)

  rdocPlugin = doc.getPlugin("render")
  if rdocPlugin is not None:
    rdocPlugin.setElementNamespace(renderNsUri)

  doc.getSBMLNamespaces().removePackageNamespace(3, 1, "render", 1)
  doc.getSBMLNamespaces().addPackageNamespace("render", 1)

  writeSBMLToFile(doc, outputFile)

def convertFileToL2(inputFile, outputFile):
  doc  = readSBMLFromFile(inputFile)
  convertDocToL2(doc, outputFile)

def convertFileToL3(inputFile, outputFile):
  doc  = readSBMLFromFile(inputFile)
  convertDocToL3(doc, outputFile)

def convertFile(inputFile, outputFile):
  doc  = readSBMLFromFile(inputFile)
  if doc.getLevel() == 3:
    convertDocToL2(doc, outputFile)
  else:
    convertDocToL3(doc, outputFile)
  
  
if __name__ == '__main__':
  if len(sys.argv) != 3:
     print("""

     usage: convertLayout <input file> <output file>

     """)
     sys.exit(1)

  convertFile(sys.argv[1], sys.argv[2])
