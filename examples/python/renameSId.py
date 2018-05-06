# 
# @file    renameSId.py
# @brief   Utility program, renaming a specific SId 
#          while updating all references to it.
# @author  Frank T. Bergmann
# 
# <!--------------------------------------------------------------------------
# This sample program is distributed under a different license than the rest
# of libSBML.  This program uses the open-source MIT license, as follows:
# 
# Copyright (c) 2013-2018 by the California Institute of Technology
# (California, USA), the European Bioinformatics Institute (EMBL-EBI, UK)
# and the University of Heidelberg (Germany), with support from the National
# Institutes of Health (USA) under grant R01GM070923.  All rights reserved.
# 
# Permission is hereby granted, free of charge, to any person obtaining a
# copy of this software and associated documentation files (the "Software"),
# to deal in the Software without restriction, including without limitation
# the rights to use, copy, modify, merge, publish, distribute, sublicense,
# and/or sell copies of the Software, and to permit persons to whom the
# Software is furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
# THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
# FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
# DEALINGS IN THE SOFTWARE.
# 
# Neither the name of the California Institute of Technology (Caltech), nor
# of the European Bioinformatics Institute (EMBL-EBI), nor of the University
# of Heidelberg, nor the names of any contributors, may be used to endorse
# or promote products derived from this software without specific prior
# written permission.
# ------------------------------------------------------------------------ -->
# 

import libsbml 
import sys



def renameSId (filename, oldSId, newSId, output_file): 
    if oldSId == newSId:
        print("The Ids are identical, renaming stopped.")
        return

    if not libsbml.SyntaxChecker.isValidInternalSId(newSId):
        print("The new SId '{0}' does not represent a valid SId.".format(newSId))
        return

    document = libsbml.readSBMLFromFile(filename)
    
    errors = document.getNumErrors(libsbml.LIBSBML_SEV_ERROR)
    
    if errors > 0:
        document.printErrors()
        return
    
    # find elements for old id
    element = document.getElementBySId(oldSId)
    if element == None:
        print("Found no element with SId '{0}'".format(oldSId))
        return
    
    # found element -> renaming
    element.setId(newSId)

    # update all references to this element
    allElements = document.getListOfAllElements()
    for i in range(allElements.getSize()):
        allElements.get(i).renameSIdRefs(oldSId, newSId)
    
    
    # write to file
    libsbml.writeSBMLToFile(document, output_file)
    


if __name__ == "__main__":
    if len(sys.argv) != 5:
        print("Usage: renameSId filename oldSId newSId output")
        sys.exit(1)
    renameSId(sys.argv[1], sys.argv[2], sys.argv[3], sys.argv[4])

