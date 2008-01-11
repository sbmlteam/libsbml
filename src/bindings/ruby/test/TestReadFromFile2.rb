#
# This file was converted from libsbml/src/sbml/test/TestReadFromFile2.c
# with the help of test_c2ruby.pl (manual handling required).
#
require 'test/unit'
require 'libSBML'

class TestReadFromFile2 < Test::Unit::TestCase

  def test_read_l1v1_units
    filename = "../../sbml/test/test-data/l1v1-units.xml"
    d = LibSBML::readSBML(filename)
      assert_equal 1, d.getLevel
      assert_equal 1, d.getVersion
      m = d.getModel
      assert_equal 2, m.getNumUnitDefinitions
      ud = m.getUnitDefinition(0)
      assert_equal  "substance",ud.getName
      ud = m.getUnitDefinition(1)
      assert_equal  "mls",ud.getName
      ud = m.getUnitDefinition(0)
      assert_equal 1, ud.getNumUnits
      u = ud.getUnit(0)
      assert_equal LibSBML::UNIT_KIND_MOLE, u.getKind
      assert_equal 1, u.getExponent
      assert_equal -3, u.getScale
      ud = m.getUnitDefinition(1)
      assert_equal 3, ud.getNumUnits
      u = ud.getUnit(0)
      assert_equal LibSBML::UNIT_KIND_MOLE, u.getKind
      assert_equal 1, u.getExponent
      assert_equal -3, u.getScale
      u = ud.getUnit(1)
      assert_equal LibSBML::UNIT_KIND_LITER, u.getKind
      assert_equal -1, u.getExponent
      assert_equal 0, u.getScale
      u = ud.getUnit(2)
      assert_equal LibSBML::UNIT_KIND_SECOND, u.getKind
      assert_equal -1, u.getExponent
      assert_equal 0, u.getScale
      assert_equal 1, m.getNumCompartments
      c = m.getCompartment(0)
      assert_equal  "cell",c.getName
      assert_equal 4, m.getNumSpecies
      s = m.getSpecies(0)
      assert_equal  "x0"   ,s.getName
      assert_equal  "cell" ,s.getCompartment
      assert_equal 1, s.getInitialAmount
      assert_equal false, s.getBoundaryCondition
      s = m.getSpecies(1)
      assert_equal  "x1"   ,s.getName
      assert_equal  "cell" ,s.getCompartment
      assert_equal 1, s.getInitialAmount
      assert_equal false, s.getBoundaryCondition
      s = m.getSpecies(2)
      assert_equal  "s1"   ,s.getName
      assert_equal  "cell" ,s.getCompartment
      assert_equal 1, s.getInitialAmount
      assert_equal false, s.getBoundaryCondition
      s = m.getSpecies(3)
      assert_equal  "s2"   ,s.getName
      assert_equal  "cell" ,s.getCompartment
      assert_equal 1, s.getInitialAmount
      assert_equal false, s.getBoundaryCondition
      assert_equal 2, m.getNumParameters
      p = m.getParameter(0)
      assert_equal  "vm"  ,p.getName
      assert_equal  "mls" ,p.getUnits
      assert_equal 2, p.getValue
      p = m.getParameter(1)
      assert_equal  "km"  ,p.getName
      assert_equal 2, p.getValue
      assert_equal 3, m.getNumReactions
      r = m.getReaction(0)
      assert_equal  "v1",r.getName
      assert_not_equal false, r.getReversible
      assert_equal false, r.getFast
      r = m.getReaction(1)
      assert_equal  "v2",r.getName
      assert_not_equal false, r.getReversible
      assert_equal false, r.getFast
      r = m.getReaction(2)
      assert_equal  "v3",r.getName
      assert_not_equal false, r.getReversible
      assert_equal false, r.getFast
      r = m.getReaction(0)
      assert_equal 1, r.getNumReactants
      assert_equal 1, r.getNumProducts
      sr = r.getReactant(0)
      assert_equal  "x0",sr.getSpecies
      assert_equal 1, sr.getStoichiometry
      assert_equal 1, sr.getDenominator
      sr = r.getProduct(0)
      assert_equal  "s1",sr.getSpecies
      assert_equal 1, sr.getStoichiometry
      assert_equal 1, sr.getDenominator
      kl = r.getKineticLaw
      assert_equal  "cell * (vm * s1)/(km + s1)",kl.getFormula
      r = m.getReaction(1)
      assert_equal 1, r.getNumReactants
      assert_equal 1, r.getNumProducts
      sr = r.getReactant(0)
      assert_equal  "s1",sr.getSpecies
      assert_equal 1, sr.getStoichiometry
      assert_equal 1, sr.getDenominator
      sr = r.getProduct(0)
      assert_equal  "s2",sr.getSpecies
      assert_equal 1, sr.getStoichiometry
      assert_equal 1, sr.getDenominator
      kl = r.getKineticLaw
      assert_equal  "cell * (vm * s2)/(km + s2)",kl.getFormula
      r = m.getReaction(2)
      assert_equal 1, r.getNumReactants
      assert_equal 1, r.getNumProducts
      sr = r.getReactant(0)
      assert_equal  "s2",sr.getSpecies
      assert_equal 1, sr.getStoichiometry
      assert_equal 1, sr.getDenominator
      sr = r.getProduct(0)
      assert_equal  "x1",sr.getSpecies
      assert_equal 1, sr.getStoichiometry
      assert_equal 1, sr.getDenominator
      kl = r.getKineticLaw
      assert_equal  "cell * (vm * s1)/(km + s1)",kl.getFormula
  end

  def test_create_l1v1_units
    d = LibSBML::SBMLDocument.new
    m = LibSBML::Model.new
    d.setModel(m)
    ud = m.createUnitDefinition
    ud.setName( "substance")
    u = m.createUnit
    u.setKind(LibSBML::UNIT_KIND_MOLE)
    u.setScale(-3)
    ud = m.createUnitDefinition
    ud.setName( "mls")
    u = m.createUnit
    u.setKind(LibSBML::UNIT_KIND_MOLE)
    u.setScale(-3)
    u = m.createUnit
    u.setKind(LibSBML::UNIT_KIND_LITER)
    u.setExponent(-1)
    u = m.createUnit
    u.setKind(LibSBML::UNIT_KIND_SECOND)
    u.setExponent(-1)
    c = m.createCompartment
    c.setName( "cell")
    s = m.createSpecies
    s.setName( "x0")
    s.setCompartment( "cell")
    s.setInitialAmount(1)
    s = m.createSpecies
    s.setName( "x1")
    s.setCompartment( "cell")
    s.setInitialAmount(1)
    s = m.createSpecies
    s.setName( "s1")
    s.setCompartment( "cell")
    s.setInitialAmount(1)
    s = m.createSpecies
    s.setName( "s2")
    s.setCompartment( "cell")
    s.setInitialAmount(1)
    p = m.createParameter
    p.setName( "vm")
    p.setUnits( "mls")
    p.setValue(2)
    p = m.createParameter
    p.setName( "km")
    p.setValue(2)
    r = m.createReaction
    r.setName( "v1")
    sr = m.createReactant
    sr.setSpecies( "x0")
    sr = m.createProduct
    sr.setSpecies( "s1")
    kl = m.createKineticLaw
    kl.setFormula( "(vm * s1)/(km + s1)")
    r = m.createReaction
    r.setName( "v2")
    sr = m.createReactant
    sr.setSpecies( "s1")
    sr = m.createProduct
    sr.setSpecies( "s2")
    kl = m.createKineticLaw
    kl.setFormula( "(vm * s2)/(km + s2)")
    r = m.createReaction
    r.setName( "v3")
    sr = m.createReactant
    sr.setSpecies( "s2")
    sr = m.createProduct
    sr.setSpecies( "x1")
    kl = m.createKineticLaw
    kl.setFormula( "(vm * s1)/(km + s1)")
  end

end
