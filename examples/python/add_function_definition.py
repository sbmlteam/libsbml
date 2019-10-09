#!/bin/env python
## @file    add_function_definition.py
## @brief   Example creating and using a function definition
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


import libsbml
import sys
import os

def create_sbml_file(file_name):
    """ 
        Simple function that creates a new SBML model 'model1' with a non constant parameter x
    """
    doc = libsbml.SBMLDocument()
    model = doc.createModel()
    model.setId('model1')

    x = model.createParameter()
    x.setId('x')
    x.setValue(0)
    x.setConstant(False)

    libsbml.writeSBMLToFile(doc, file_name)


def add_function_defintion(model, id, function):
    #type: (libsbml.Model, str, str)
    """
        this function adds a new function definition to the given sbml model
    """

    if model.getFunctionDefinition(id):
        raise ValueError('the model already has a function definition with id %s' % id)
    fun = model.createFunctionDefinition()
    fun.setId(id)

    math = libsbml.parseL3Formula(function)
    if not math: 
        raise ValueError('the formula could not be parsed')
    fun.setMath(math)

def add_assignment(model, parameter_id, formula):
    #type: (libsbml.Model, str, str)
    """
        this function adds an assignment rule to the given parameter
    """
    if not model.getParameter(parameter_id):
        raise ValueError('the model has no parameter %s' % parameter_id)

    rule = model.createAssignmentRule()
    rule.setVariable(parameter_id)
    
    math = libsbml.parseL3Formula(formula)
    if not math: 
        raise ValueError('the formula could not be parsed')
    
    rule.setMath(math)


if __name__ == "__main__":

    # create a test model if none exists
    if not os.path.exists('test.xml'):
        create_sbml_file('test.xml')

    # read in the test model 
    doc = libsbml.readSBMLFromFile('test.xml')

    # bail if it contained errors
    if doc.getNumErrors(libsbml.LIBSBML_SEV_ERROR) > 0: 
        doc.printErrors()
        sys.exit(1)

    model = doc.getModel()

    # add a function definiton 'f(x) = sin(x)' to the model
    add_function_defintion(model, 'f', 'lambda(x, sin(x))')

    # add an assignment to model: x = sin(time)
    add_assignment(model, 'x', 'f(time)')

    # validate the model and bail if it contains errors now
    doc.checkConsistency()
    if doc.getNumErrors(libsbml.LIBSBML_SEV_ERROR) > 0: 
        doc.printErrors()
        sys.exit(2)

    # write model to the file
    libsbml.writeSBMLToFile(doc, 'test.xml')
