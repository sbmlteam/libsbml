#
# @file    TestSBMLConvert.rb
# @brief   SBMLConvert unit tests
# @author  Akiya Jouraku (Ruby conversion)
# @author  Ben Bornstein 
#
# $Id$
# $Source$
#
# This test file was converted from src/sbml/test/TestSBMLConvert.c
# wiht the help of conversion sciprt (ctest_converter.pl).
#
#<!---------------------------------------------------------------------------
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.
#
# Copyright 2005-2008 California Institute of Technology.
# Copyright 2002-2005 California Institute of Technology and
#                     Japan Science and Technology Corporation.
# 
# This library is free software; you can redistribute it and/or modify it
# under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation.  A copy of the license agreement is provided
# in the file named "LICENSE.txt" included with this software distribution
# and also available online as http://sbml.org/software/libsbml/license.html
#--------------------------------------------------------------------------->*/
#
require 'test/unit'
require 'libSBML'

class TestSBMLConvert < Test::Unit::TestCase

  def test_SBMLConvert_addModifiersToReaction
    d = LibSBML::SBMLDocument.new(1,2)
    m = d.createModel
    kl = LibSBML::KineticLaw.new("k1*S1*S2*S3*S4*S5")
    r = LibSBML::Reaction.new("R", "",kl,1)
    m.addSpecies(LibSBML::Species.new("S1", ""))
    m.addSpecies(LibSBML::Species.new("S2", ""))
    m.addSpecies(LibSBML::Species.new("S3", ""))
    m.addSpecies(LibSBML::Species.new("S4", ""))
    m.addSpecies(LibSBML::Species.new("S5", ""))
    r.addReactant(LibSBML::SpeciesReference.new("S1",1,1))
    r.addReactant(LibSBML::SpeciesReference.new("S2",1,1))
    r.addProduct(LibSBML::SpeciesReference.new("S5",1,1))
    m.addReaction(r)
    assert( r.getNumModifiers == 0 )
    d.setLevelAndVersion(2,1)
    assert( d.getLevel == 2 )
    assert( d.getVersion == 1 )
    assert( m.getReaction(0).getNumModifiers == 2 )
    ssr1 = m.getReaction(0).getModifier(0)
    ssr2 = m.getReaction(0).getModifier(1)
    assert ((  "S3" == ssr1.getSpecies ))
    assert ((  "S4" == ssr2.getSpecies ))
  end

  def test_SBMLConvert_convertToL1_SBMLDocument
    d = LibSBML::SBMLDocument.new(2,1)
    d.setLevelAndVersion(1,2)
    assert( d.getLevel == 1 )
    assert( d.getVersion == 2 )
  end

  def test_SBMLConvert_convertToL1_Species_Amount
    d = LibSBML::SBMLDocument.new(2,1)
    m = d.createModel
    sid = "C"
    c = LibSBML::Compartment.new
    s = LibSBML::Species.new
    c.setId(sid)
    m.addCompartment(c)
    s.setCompartment(sid)
    s.setInitialAmount(2.34)
    m.addSpecies(s)
    d.setLevelAndVersion(1,2)
    assert( s.getInitialAmount == 2.34 )
  end

  def test_SBMLConvert_convertToL1_Species_Concentration
    d = LibSBML::SBMLDocument.new(2,1)
    m = d.createModel
    sid = "C"
    c = LibSBML::Compartment.new
    s = LibSBML::Species.new
    c.setId(sid)
    c.setSize(1.2)
    m.addCompartment(c)
    s.setCompartment(sid)
    s.setInitialConcentration(2.34)
    m.addSpecies(s)
    d.setLevelAndVersion(1,2)
    #ifndef CYGWIN
    assert( m.getSpecies(0).getInitialAmount == 2.808 )
    #endif
    s1 = m.getSpecies(0)
    assert( s1 != nil )
    assert ((  "C" == s1.getCompartment ))
    assert( m.getCompartment( "C").getSize == 1.2 )
    assert( s1.getInitialConcentration == 2.34 )
    assert( s1.isSetInitialConcentration == true )
  end

  def test_SBMLConvert_convertToL2_SBMLDocument
    d = LibSBML::SBMLDocument.new(1,2)
    d.setLevelAndVersion(2,1)
    assert( d.getLevel == 2 )
    assert( d.getVersion == 1 )
    d.setLevelAndVersion(2,2)
    assert( d.getLevel == 2 )
    assert( d.getVersion == 2 )
    d.setLevelAndVersion(2,3)
    assert( d.getLevel == 2 )
    assert( d.getVersion == 3 )
  end

end
