#
# This file was converted from libsbml/src/sbml/test/TestSBMLConvert.c
# with the help of test_c2ruby.pl (manual handling required).
#
require 'test/unit'
require 'libSBML'

class TestSBMLConvert < Test::Unit::TestCase

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
    assert_equal 2.808, m.getSpecies(0).getInitialAmount
    #endif
    s1 = m.getSpecies(0)
    assert_not_equal nil, s1
    assert_equal  "C",s1.getCompartment
    assert_equal 1.2, m.getCompartment( "C").getSize
    assert_equal 2.34, s1.getInitialConcentration
    assert_equal true, s1.isSetInitialConcentration
  end

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
    assert_equal 0, r.getNumModifiers
    d.setLevelAndVersion(2,1)
    assert_equal 2, d.getLevel
    assert_equal 1, d.getVersion
    assert_equal 2, m.getReaction(0).getNumModifiers
    ssr1 = m.getReaction(0).getModifier(0)
    ssr2 = m.getReaction(0).getModifier(1)
    assert_equal  "S3",ssr1.getSpecies
    assert_equal  "S4",ssr2.getSpecies
  end

  def test_SBMLConvert_convertToL2_SBMLDocument
    d = LibSBML::SBMLDocument.new(1,2)
    d.setLevelAndVersion(2,1)
    assert_equal 2, d.getLevel
    assert_equal 1, d.getVersion
    d.setLevelAndVersion(2,2)
    assert_equal 2, d.getLevel
    assert_equal 2, d.getVersion
    d.setLevelAndVersion(2,3)
    assert_equal 2, d.getLevel
    assert_equal 3, d.getVersion
  end

  def test_SBMLConvert_convertToL1_SBMLDocument
    d = LibSBML::SBMLDocument.new(2,1)
    d.setLevelAndVersion(1,2)
    assert_equal 1, d.getLevel
    assert_equal 2, d.getVersion
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
    assert_equal 2.34, s.getInitialAmount
  end

end
