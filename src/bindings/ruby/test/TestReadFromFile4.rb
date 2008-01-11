#
# This file was converted from libsbml/src/sbml/test/TestReadFromFile4.c
# with the help of test_c2ruby.pl (manual handling required).
#
require 'test/unit'
require 'libSBML'

class TestReadFromFile4 < Test::Unit::TestCase

  def test_read_l1v1_minimal
    filename = "../../sbml/test/test-data/l1v1-minimal.xml"
    d = LibSBML::readSBML(filename)
      assert_equal 1, d.getLevel
      assert_equal 1, d.getVersion
      m = d.getModel
      assert_equal 1, m.getNumCompartments
      c = m.getCompartment(0)
      assert_equal  "x",c.getName
      assert_equal 1, m.getNumSpecies
      s = m.getSpecies(0)
      assert_equal  "y" ,s.getName
      assert_equal  "x" ,s.getCompartment
      assert_equal 1, s.getInitialAmount
      assert_equal false, s.getBoundaryCondition
      assert_equal 1, m.getNumReactions
      r = m.getReaction(0)
      assert_equal  "r",r.getName
      assert_not_equal false, r.getReversible
      assert_equal false, r.getFast
      assert_equal 1, r.getNumReactants
      assert_equal 1, r.getNumProducts
      sr = r.getReactant(0)
      assert_equal  "y",sr.getSpecies
      assert_equal 1, sr.getStoichiometry
      assert_equal 1, sr.getDenominator
      sr = r.getProduct(0)
      assert_equal  "y",sr.getSpecies
      assert_equal 1, sr.getStoichiometry
      assert_equal 1, sr.getDenominator
  end

end
