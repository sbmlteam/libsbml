#
# @file    TestSBMLConvertStrict.rb
# @brief   SBMLConvert unit tests for strict conversion
#
# @author  Akiya Jouraku (Ruby conversion)
# @author  Sarah Keating 
#
# $Id$
# $HeadURL$
#
# This test file was converted from src/sbml/test/TestSBMLConvertStrict.c
# with the help of conversion sciprt (ctest_converter.pl).
#
#<!---------------------------------------------------------------------------
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.
#
# Copyright 2005-2010 California Institute of Technology.
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

class TestSBMLConvertStrict < Test::Unit::TestCase

  def test_SBMLConvertStrict_convertL1ParamRule
    d = LibSBML::SBMLDocument.new(1,2)
    m = d.createModel()
    c = m.createCompartment()
    c.setId( "c")
    p = m.createParameter()
    p.setId( "p")
    p1 = m.createParameter()
    p1.setId( "p1")
    math = LibSBML::parseFormula("p")
    ar = m.createAssignmentRule()
    ar.setVariable( "p1")
    ar.setMath(math)
    ar.setUnits( "mole")
    success = d.setLevelAndVersion(2,1,true)
    assert( success == true )
    assert( d.getLevel() == 2 )
    assert( d.getVersion() == 1 )
    r1 = d.getModel().getRule(0)
    assert( r1.getUnits() == "" )
    d = nil
  end

  def test_SBMLConvertStrict_convertNonStrictSBO
    d = LibSBML::SBMLDocument.new(2,4)
    m = d.createModel()
    c = m.createCompartment()
    c.setId( "c")
    c.setConstant(false)
    (c).setSBOTerm(64)
    success = d.setLevelAndVersion(2,3,true)
    assert( success == false )
    assert( d.getLevel() == 2 )
    assert( d.getVersion() == 4 )
    success = d.setLevelAndVersion(2,2,true)
    assert( success == false )
    assert( d.getLevel() == 2 )
    assert( d.getVersion() == 4 )
    success = d.setLevelAndVersion(2,1,true)
    assert( success == true )
    assert( d.getLevel() == 2 )
    assert( d.getVersion() == 1 )
    c1 = d.getModel().getCompartment(0)
    assert( (c1).getSBOTerm() == -1 )
    success = d.setLevelAndVersion(1,2,true)
    assert( success == true )
    assert( d.getLevel() == 1 )
    assert( d.getVersion() == 2 )
    c2 = d.getModel().getCompartment(0)
    assert( (c2).getSBOTerm() == -1 )
    d = nil
  end

  def test_SBMLConvertStrict_convertNonStrictUnits
    d = LibSBML::SBMLDocument.new(2,4)
    m = d.createModel()
    c = m.createCompartment()
    c.setId( "c")
    c.setConstant(false)
    p = m.createParameter()
    p.setId( "p")
    p.setUnits( "mole")
    math = LibSBML::parseFormula("p")
    ar = m.createAssignmentRule()
    ar.setVariable( "c")
    ar.setMath(math)
    success = d.setLevelAndVersion(2,1,true)
    assert( success == false )
    assert( d.getLevel() == 2 )
    assert( d.getVersion() == 4 )
    success = d.setLevelAndVersion(2,2,true)
    assert( success == false )
    assert( d.getLevel() == 2 )
    assert( d.getVersion() == 4 )
    success = d.setLevelAndVersion(2,3,true)
    assert( success == false )
    assert( d.getLevel() == 2 )
    assert( d.getVersion() == 4 )
    success = d.setLevelAndVersion(1,2,true)
    assert( success == false )
    assert( d.getLevel() == 2 )
    assert( d.getVersion() == 4 )
    d = nil
  end

  def test_SBMLConvertStrict_convertSBO
    d = LibSBML::SBMLDocument.new(2,4)
    m = d.createModel()
    c = m.createCompartment()
    c.setId( "c")
    (c).setSBOTerm(240)
    success = d.setLevelAndVersion(2,3,true)
    assert( success == true )
    assert( d.getLevel() == 2 )
    assert( d.getVersion() == 3 )
    success = d.setLevelAndVersion(2,2,true)
    assert( success == true )
    assert( d.getLevel() == 2 )
    assert( d.getVersion() == 2 )
    c1 = d.getModel().getCompartment(0)
    assert( (c1).getSBOTerm() == -1 )
    d = nil
  end

  def test_SBMLConvertStrict_convertToL1
    d = LibSBML::SBMLDocument.new(2,4)
    m = d.createModel()
    (m).setMetaId( "_m")
    c = m.createCompartment()
    c.setId( "c")
    (c).setSBOTerm(240)
    s = m.createSpecies()
    s.setId( "s")
    s.setCompartment( "c")
    s.setHasOnlySubstanceUnits(true)
    success = d.setLevelAndVersion(1,2,true)
    assert( success == true )
    assert( d.getLevel() == 1 )
    assert( d.getVersion() == 2 )
    m1 = d.getModel()
    assert( (m1).getMetaId() == "" )
    c1 = m1.getCompartment(0)
    assert( (c1).getSBOTerm() == -1 )
    s1 = m1.getSpecies(0)
    assert( s1.getHasOnlySubstanceUnits() == false )
    d = nil
  end

end
