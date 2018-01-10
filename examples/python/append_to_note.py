#!/usr/bin/env python
##
## \file    append_to_note.py
## \brief   adds to the notes of an element in a document
##
##          This file takes three or four arguments;
##
##           input-file  - the input SBML file
##           id          - the SBML Id of the element to which notes should be appended
##           string      - the string to be added to the body of the existing note
##           output-file - an optional output file
##
## \author  Frank T. Bergmann
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

import libsbml
import sys

def getBody(notes):
    if notes is None:
        return None
    if notes.isStart() and notes.getName() == "body":
        return notes
    for i in range(notes.getNumChildren()):
        current = getBody(notes.getChild(i))
        if current is not None:
            return current
    return None

def addToNotes(doc, id, stringToAdd):
    element = doc.getElementBySId(id)
    if element is None:
        print ("Couldn't find element '{0}'".format(id))
        exit(2)
    notes = element.getNotes()
    if notes is None:
        print("Element '{0}' has no notes".format(id))
        exit(3)
    body = getBody(notes)
    if body is None:
        print ("Element '{0}' has no body".format(id))
        exit(4)
    print ("Body before change: ")
    print (body.toXMLString())
    body.addChild(libsbml.XMLNode_convertStringToXMLNode(stringToAdd))
    print ("\nBody after change: ")
    print (body.toXMLString())


if __name__ == "__main__":
    if len(sys.argv) < 4:
        print ("usage append_to_note  input-file id note-to-add [output-file]")
        exit(1)

    doc = libsbml.readSBMLFromFile(sys.argv[1])

    if doc.getNumErrors(libsbml.LIBSBML_SEV_ERROR) > 0:
        print("Errors occurred loading the file")
        doc.printErrors()
        exit(1)

    addToNotes(doc, sys.argv[2], sys.argv[3])

    if len(sys.argv) > 4:
        libsbml.writeSBMLToFile(doc, sys.argv[4])
