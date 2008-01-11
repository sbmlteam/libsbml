#
# This file was converted from libsbml/src/sbml/test/TestReadFromFile1.c
# with the help of test_c2ruby.pl (manual handling required).
#
require 'test/unit'
require 'libSBML'

class TestReadFromFile1 < Test::Unit::TestCase

  def test_read_l1v1_branch
    filename = "../../sbml/test/test-data/l1v1-branch.xml"
    d = LibSBML::readSBML(filename)
      assert_equal 1, d.getLevel
      assert_equal 1, d.getVersion
      m = d.getModel
      assert_equal  "Branch",m.getName
      assert_equal 1, m.getNumCompartments
      c = m.getCompartment(0)
      assert_equal  "compartmentOne",c.getName
      assert_equal 1, c.getVolume
      assert_equal 4, m.getNumSpecies
      s = m.getSpecies(0)
      assert_equal  "S1"             ,s.getName
      assert_equal  "compartmentOne" ,s.getCompartment
      assert_equal 0, s.getInitialAmount
      assert_equal false, s.getBoundaryCondition
      s = m.getSpecies(1)
      assert_equal  "X0"             ,s.getName
      assert_equal  "compartmentOne" ,s.getCompartment
      assert_equal 0, s.getInitialAmount
      assert_equal true, s.getBoundaryCondition
      s = m.getSpecies(2)
      assert_equal  "X1"             ,s.getName
      assert_equal  "compartmentOne" ,s.getCompartment
      assert_equal 0, s.getInitialAmount
      assert_equal true, s.getBoundaryCondition
      s = m.getSpecies(3)
      assert_equal  "X2"             ,s.getName
      assert_equal  "compartmentOne" ,s.getCompartment
      assert_equal 0, s.getInitialAmount
      assert_equal true, s.getBoundaryCondition
      assert_equal 3, m.getNumReactions
      r = m.getReaction(0)
      assert_equal  "reaction_1",r.getName
      assert_equal false, r.getReversible
      assert_equal false, r.getFast
      r = m.getReaction(1)
      assert_equal  "reaction_2",r.getName
      assert_equal false, r.getReversible
      assert_equal false, r.getFast
      r = m.getReaction(2)
      assert_equal  "reaction_3",r.getName
      assert_equal false, r.getReversible
      assert_equal false, r.getFast
      r = m.getReaction(0)
      assert_equal 1, r.getNumReactants
      assert_equal 1, r.getNumProducts
      sr = r.getReactant(0)
      assert_equal  "X0",sr.getSpecies
      assert_equal 1, sr.getStoichiometry
      assert_equal 1, sr.getDenominator
      sr = r.getProduct(0)
      assert_equal  "S1",sr.getSpecies
      assert_equal 1, sr.getStoichiometry
      assert_equal 1, sr.getDenominator
      kl = r.getKineticLaw
      assert_equal  "k1 * X0",kl.getFormula
      assert_equal 1, kl.getNumParameters
      p = kl.getParameter(0)
      assert_equal  "k1",p.getName
      assert_equal 0, p.getValue
      r = m.getReaction(1)
      assert_equal 1, r.getNumReactants
      assert_equal 1, r.getNumProducts
      sr = r.getReactant(0)
      assert_equal  "S1",sr.getSpecies
      assert_equal 1, sr.getStoichiometry
      assert_equal 1, sr.getDenominator
      sr = r.getProduct(0)
      assert_equal  "X1",sr.getSpecies
      assert_equal 1, sr.getStoichiometry
      assert_equal 1, sr.getDenominator
      kl = r.getKineticLaw
      assert_equal  "k2 * S1",kl.getFormula
      assert_equal 1, kl.getNumParameters
      p = kl.getParameter(0)
      assert_equal  "k2",p.getName
      assert_equal 0, p.getValue
      r = m.getReaction(2)
      assert_equal 1, r.getNumReactants
      assert_equal 1, r.getNumProducts
      sr = r.getReactant(0)
      assert_equal  "S1",sr.getSpecies
      assert_equal 1, sr.getStoichiometry
      assert_equal 1, sr.getDenominator
      sr = r.getProduct(0)
      assert_equal  "X2",sr.getSpecies
      assert_equal 1, sr.getStoichiometry
      assert_equal 1, sr.getDenominator
      kl = r.getKineticLaw
      assert_equal  "k3 * S1",kl.getFormula
      assert_equal 1, kl.getNumParameters
      p = kl.getParameter(0)
      assert_equal  "k3",p.getName
      assert_equal 0, p.getValue
  end

end
