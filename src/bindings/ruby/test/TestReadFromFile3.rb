#
# This file was converted from libsbml/src/sbml/test/TestReadFromFile3.c
# with the help of test_c2ruby.pl (manual handling required).
#
require 'test/unit'
require 'libSBML'

class TestReadFromFile3 < Test::Unit::TestCase

  def test_read_l1v1_rules
    filename = "../../sbml/test/test-data/l1v1-rules.xml"
    d = LibSBML::readSBML(filename)
      assert_equal 1, d.getLevel
      assert_equal 1, d.getVersion
      m = d.getModel
      assert_equal 1, m.getNumCompartments
      c = m.getCompartment(0)
      assert_equal  "cell",c.getName
      assert_equal 1, c.getVolume
      assert_equal 6, m.getNumSpecies
      s = m.getSpecies(0)
      assert_equal  "s1"   ,s.getName
      assert_equal  "cell" ,s.getCompartment
      assert_equal 4, s.getInitialAmount
      assert_equal false, s.getBoundaryCondition
      s = m.getSpecies(1)
      assert_equal  "s2"   ,s.getName
      assert_equal  "cell" ,s.getCompartment
      assert_equal 2, s.getInitialAmount
      assert_equal false, s.getBoundaryCondition
      s = m.getSpecies(2)
      assert_equal  "x0"   ,s.getName
      assert_equal  "cell" ,s.getCompartment
      assert_equal 1, s.getInitialAmount
      assert_equal false, s.getBoundaryCondition
      s = m.getSpecies(3)
      assert_equal  "x1"   ,s.getName
      assert_equal  "cell" ,s.getCompartment
      assert_equal 0, s.getInitialAmount
      assert_equal false, s.getBoundaryCondition
      s = m.getSpecies(4)
      assert_equal  "x2"   ,s.getName
      assert_equal  "cell" ,s.getCompartment
      assert_equal 1, s.getInitialAmount
      assert_equal false, s.getBoundaryCondition
      s = m.getSpecies(5)
      assert_equal  "x3"   ,s.getName
      assert_equal  "cell" ,s.getCompartment
      assert_equal 0, s.getInitialAmount
      assert_equal false, s.getBoundaryCondition
      assert_equal 7, m.getNumParameters
      p = m.getParameter(0)
      assert_equal  "k1",p.getName
      assert_equal 1.2, p.getValue
      p = m.getParameter(1)
      assert_equal  "k2",p.getName
      assert_equal 1000, p.getValue
      p = m.getParameter(2)
      assert_equal  "k3",p.getName
      assert_equal 3000, p.getValue
      p = m.getParameter(3)
      assert_equal  "k4",p.getName
      assert_equal 4.5, p.getValue
      assert_equal 4, m.getNumRules
      pr = m.getRule(0)
      assert_equal  "t",pr.getVariable
      assert_equal  "s1 + s2",pr.getFormula
      pr = m.getRule(1)
      assert_equal  "k",pr.getVariable
      assert_equal  "k3/k2",pr.getFormula
      scr = m.getRule(2)
      assert_equal  "x2",scr.getVariable
      assert_equal  "k * (s1+s2)/(1 + k)",scr.getFormula
      scr = m.getRule(3)
      assert_equal  "x3",scr.getVariable
      assert_equal  "p*(t - s2)",scr.getFormula
      assert_equal 2, m.getNumReactions
      r = m.getReaction(0)
      assert_equal  "j1",r.getName
      assert_not_equal false, r.getReversible
      assert_equal false, r.getFast
      r = m.getReaction(1)
      assert_equal  "j3",r.getName
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
      assert_equal  "k1 * x0",kl.getFormula
      r = m.getReaction(1)
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
      assert_equal  "k4 * s2",kl.getFormula
  end

end
