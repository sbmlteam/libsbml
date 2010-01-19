#
# @file    TestSBMLConvert.rb
# @brief   SBMLConvert unit tests
#
# @author  Akiya Jouraku (Ruby conversion)
# @author  Ben Bornstein 
#
# $Id$
# $HeadURL$
#
# This test file was converted from src/sbml/test/TestSBMLConvert.c
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

class TestSBMLConvert < Test::Unit::TestCase

  def test_SBMLConvert_addModifiersToReaction
    d = LibSBML::SBMLDocument.new(1,2)
    m = d.createModel()
    r = m.createReaction()
    kl = r.createKineticLaw()
    kl.setFormula( "k1*S1*S2*S3*S4*S5")
    s1 = m.createSpecies()
    s1.setId( "S1" )
    s2 = m.createSpecies()
    s2.setId( "S2")
    s3 = m.createSpecies()
    s3.setId( "S3")
    s4 = m.createSpecies()
    s4.setId( "S4")
    s5 = m.createSpecies()
    s5.setId( "S5")
    sr1 = r.createReactant()
    sr2 = r.createReactant()
    sr3 = r.createProduct()
    sr1.setSpecies( "S1")
    sr2.setSpecies( "S2")
    sr3.setSpecies( "S5")
    assert( r.getNumModifiers() == 0 )
    d.setLevelAndVersion(2,1,false)
    assert( d.getLevel() == 2 )
    assert( d.getVersion() == 1 )
    assert( m.getReaction(0).getNumModifiers() == 2 )
    ssr1 = m.getReaction(0).getModifier(0)
    ssr2 = m.getReaction(0).getModifier(1)
    assert ((  "S3" == ssr1.getSpecies() ))
    assert ((  "S4" == ssr2.getSpecies() ))
    d = nil
  end

  def test_SBMLConvert_convertFromL3
    d = LibSBML::SBMLDocument.new(3,1)
    m = d.createModel()
    sid =  "C";
    c = m.createCompartment()
    c.setId(sid)
    c.setSize(1.2)
    c.setUnits( "volume")
    assert( d.setLevelAndVersion(1,1,false) == false )
    assert( d.setLevelAndVersion(1,2,false) == false )
    assert( d.setLevelAndVersion(2,1,false) == false )
    assert( d.setLevelAndVersion(2,2,false) == false )
    assert( d.setLevelAndVersion(2,3,false) == false )
    assert( d.setLevelAndVersion(2,4,false) == false )
    assert( d.setLevelAndVersion(3,1,false) == true )
  end

  def test_SBMLConvert_convertToL1_SBMLDocument
    d = LibSBML::SBMLDocument.new(2,1)
    d.setLevelAndVersion(1,2,false)
    assert( d.getLevel() == 1 )
    assert( d.getVersion() == 2 )
    d = nil
  end

  def test_SBMLConvert_convertToL1_Species_Amount
    d = LibSBML::SBMLDocument.new(2,1)
    m = d.createModel()
    sid =  "C";
    c = LibSBML::Compartment.new(2,4)
    s = LibSBML::Species.new(2,4)
    c.setId(sid)
    m.addCompartment(c)
    s.setCompartment(sid)
    s.setInitialAmount(2.34)
    m.addSpecies(s)
    d.setLevelAndVersion(1,2,false)
    assert( s.getInitialAmount() == 2.34 )
    d = nil
  end

  def test_SBMLConvert_convertToL1_Species_Concentration
    d = LibSBML::SBMLDocument.new(2,1)
    m = d.createModel()
    sid =  "C";
    c = LibSBML::Compartment.new(2,1)
    s = LibSBML::Species.new(2,1)
    c.setId(sid)
    c.setSize(1.2)
    m.addCompartment(c)
    s.setId( "s"  )
    s.setCompartment(sid)
    s.setInitialConcentration(2.34)
    m.addSpecies(s)
    d.setLevelAndVersion(1,2,false)
    s1 = m.getSpecies(0)
    assert( s1 != nil )
    assert ((  "C" == s1.getCompartment() ))
    assert( m.getCompartment( "C").getSize() == 1.2 )
    assert( s1.getInitialConcentration() == 2.34 )
    assert( s1.isSetInitialConcentration() == true )
    d = nil
  end

  def test_SBMLConvert_convertToL2_SBMLDocument
    d = LibSBML::SBMLDocument.new(1,2)
    d.setLevelAndVersion(2,1,false)
    assert( d.getLevel() == 2 )
    assert( d.getVersion() == 1 )
    d.setLevelAndVersion(2,2,false)
    assert( d.getLevel() == 2 )
    assert( d.getVersion() == 2 )
    d.setLevelAndVersion(2,3,false)
    assert( d.getLevel() == 2 )
    assert( d.getVersion() == 3 )
    d = nil
  end

  def test_SBMLConvert_convertToL2v4_DuplicateAnnotations_doc
    d = LibSBML::SBMLDocument.new(2,1)
    d.createModel()
    annotation =  "<rdf/>\n<rdf/>";
    i = (d).setAnnotation(annotation)
    assert( d.getLevel() == 2 )
    assert( d.getVersion() == 1 )
    assert( (d).getAnnotation().getNumChildren() == 2 )
    d.setLevelAndVersion(2,4,false)
    assert( d.getLevel() == 2 )
    assert( d.getVersion() == 4 )
    assert( (d).getAnnotation().getNumChildren() == 1 )
    d = nil
  end

  def test_SBMLConvert_convertToL2v4_DuplicateAnnotations_model
    d = LibSBML::SBMLDocument.new(2,1)
    m = d.createModel()
    annotation =  "<rdf/>\n<rdf/>";
    i = (m).setAnnotation(annotation)
    assert( d.getLevel() == 2 )
    assert( d.getVersion() == 1 )
    assert( (m).getAnnotation().getNumChildren() == 2 )
    d.setLevelAndVersion(2,4,false)
    assert( d.getLevel() == 2 )
    assert( d.getVersion() == 4 )
    m = d.getModel()
    assert( (m).getAnnotation().getNumChildren() == 1 )
    d = nil
  end

  def test_SBMLConvert_convertToL3_defaultUnits
    d = LibSBML::SBMLDocument.new(1,2)
    m = d.createModel()
    sid =  "C";
    c = m.createCompartment()
    c.setId(sid)
    c.setSize(1.2)
    c.setUnits( "volume")
    assert( m.getNumUnitDefinitions() == 0 )
    d.setLevelAndVersion(3,1,false)
    assert( m.getNumUnitDefinitions() == 2 )
    ud = m.getUnitDefinition(0)
    assert( ud != nil )
    assert ((  "volume" == ud.getId() ))
    assert( ud.getNumUnits() == 1 )
    u = ud.getUnit(0)
    assert( u.getKind() == LibSBML::UNIT_KIND_LITRE )
    assert( u.getExponent() == 1 )
    assert( u.getMultiplier() == 1 )
    assert( u.getScale() == 0 )
    ud = m.getUnitDefinition(1)
    assert( ud != nil )
    assert ((  "time" == ud.getId() ))
    assert( ud.getNumUnits() == 1 )
    u = ud.getUnit(0)
    assert( u.getKind() == LibSBML::UNIT_KIND_SECOND )
    assert( u.getExponent() == 1 )
    assert( u.getMultiplier() == 1 )
    assert( u.getScale() == 0 )
    d = nil
  end

  def test_SBMLConvert_invalidLevelVersion
    d = LibSBML::SBMLDocument.new(2,1)
    m = d.createModel()
    sid =  "C";
    c = m.createCompartment()
    c.setId(sid)
    c.setSize(1.2)
    c.setUnits( "volume")
    assert( d.setLevelAndVersion(1,3,false) == false )
    assert( d.setLevelAndVersion(2,5,false) == false )
    assert( d.setLevelAndVersion(3,2,false) == false )
    assert( d.setLevelAndVersion(4,1,false) == false )
  end

end
