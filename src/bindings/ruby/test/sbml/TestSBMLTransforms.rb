#
# @file    TestSBMLTransforms.rb
# @brief   SBMLTransforms unit tests
#
# @author  Akiya Jouraku (Ruby conversion)
# @author  Sarah Keating 
#
# $Id$
# $HeadURL$
#
# This test file was converted from src/sbml/test/TestSBMLTransforms.cpp
# with the help of conversion sciprt (ctest_converter.pl).
#
#<!---------------------------------------------------------------------------
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.
#
# Copyright 2005-2009 California Institute of Technology.
# Copyright 2002-2005 California Institute of Technology and
#                     Japan Science and Technology Corporation.
# 
# This library is free software; you can redistribute it and/or modify it
# under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation.  A copy of the license agreement is provided
# in the file named "LICENSE.txt" included with this software distribution
# and also available online as http://sbml.org/software/libsbml/license.html
#--------------------------------------------------------------------------->*/
require 'test/unit'
require 'libSBML'

class TestSBMLTransforms < Test::Unit::TestCase

  def test_SBMLTransforms_replaceFD
    reader = LibSBML::SBMLReader.new()
    filename = "../../sbml/test/test-data/"
    filename += "multiple-functions.xml"
    d = reader.readSBML(filename)
    if (d == nil)
    end
    m = d.getModel()
    assert( m.getNumFunctionDefinitions() == 2 )
    ast = m.getReaction(2).getKineticLaw().getMath()
    assert ((  "f(S1, p) * compartmentOne / t" == LibSBML::formulaToString(ast) ))
    fd = m.getFunctionDefinition(0)
    LibSBML::SBMLTransforms.replaceFD(ast,fd)
    assert ((  "S1 * p * compartmentOne / t" == LibSBML::formulaToString(ast) ))
    ast = m.getReaction(1).getKineticLaw().getMath()
    assert ((  "f(f(S1, p), compartmentOne) / t" == LibSBML::formulaToString(ast) ))
    LibSBML::SBMLTransforms.replaceFD(ast,fd)
    assert ((  "S1 * p * compartmentOne / t" == LibSBML::formulaToString(ast) ))
    ast = m.getReaction(0).getKineticLaw().getMath()
    assert ((  "g(f(S1, p), compartmentOne) / t" == LibSBML::formulaToString(ast) ))
    LibSBML::SBMLTransforms.replaceFD(ast,fd)
    assert ((  "g(S1 * p, compartmentOne) / t" == LibSBML::formulaToString(ast) ))
    fd = m.getFunctionDefinition(1)
    LibSBML::SBMLTransforms.replaceFD(ast,fd)
    assert ((  "f(S1 * p, compartmentOne) / t" == LibSBML::formulaToString(ast) ))
    ast = m.getReaction(0).getKineticLaw().getMath()
    lofd = m.getListOfFunctionDefinitions()
    LibSBML::SBMLTransforms.replaceFD(ast,lofd)
    assert ((  "S1 * p * compartmentOne / t" == LibSBML::formulaToString(ast) ))
    d.expandFunctionDefinitions()
    assert( d.getModel().getNumFunctionDefinitions() == 0 )
    ast = d.getModel().getReaction(0).getKineticLaw().getMath()
    assert ((  "S1 * p * compartmentOne / t" == LibSBML::formulaToString(ast) ))
    ast = d.getModel().getReaction(1).getKineticLaw().getMath()
    assert ((  "S1 * p * compartmentOne / t" == LibSBML::formulaToString(ast) ))
    ast = d.getModel().getReaction(2).getKineticLaw().getMath()
    assert ((  "S1 * p * compartmentOne / t" == LibSBML::formulaToString(ast) ))
  end

end
