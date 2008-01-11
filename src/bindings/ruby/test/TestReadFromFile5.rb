#
# This file was converted from libsbml/src/sbml/test/TestReadFromFile5.cpp
# with the help of test_c2ruby.pl (manual handling required).
#
require 'test/unit'
require 'libSBML'

class TestReadFromFile5 < Test::Unit::TestCase

  def test_read_l2v1_assignment
    filename = "../../sbml/test/test-data/l2v1-assignment.xml"
    reader = LibSBML::SBMLReader.new
    d = reader.readSBML(filename)
    assert_equal 2, d.getLevel()
    assert_equal 1, d.getVersion()
    m = d.getModel()
    assert_not_equal nil, m
    assert_equal 1, m.getNumCompartments()
    c = m.getCompartment(0)
    assert_not_equal nil, c
    assert_equal "cell", c.getId()
    assert_equal 5, m.getNumSpecies()
    s = m.getSpecies(0)
    assert_not_equal nil, s
    assert_equal "X0", s.getId()
    assert_equal "cell", s.getCompartment()
    assert_equal 1.0, s.getInitialConcentration()
    s = m.getSpecies(1)
    assert_not_equal nil, s
    assert_equal "X1", s.getId()
    assert_equal "cell", s.getCompartment()
    assert_equal 0.0, s.getInitialConcentration()
    s = m.getSpecies(2)
    assert_not_equal nil, s
    assert_equal "T", s.getId()
    assert_equal "cell", s.getCompartment()
    assert_equal 0.0, s.getInitialConcentration()
    s = m.getSpecies(3)
    assert_not_equal nil, s
    assert_equal "S1", s.getId()
    assert_equal "cell", s.getCompartment()
    assert_equal 0.0, s.getInitialConcentration()
    s = m.getSpecies(4)
    assert_not_equal nil, s
    assert_equal "S2", s.getId()
    assert_equal "cell", s.getCompartment()
    assert_equal 0.0, s.getInitialConcentration()
    assert_equal 1, m.getNumParameters()
    p = m.getParameter(0)
    assert_not_equal nil, p
    assert_equal "Keq", p.getId()
    assert_equal 2.5, p.getValue()
    assert_equal 2, m.getNumRules()
    ar =  m.getRule(0)
    assert_not_equal nil, ar
    assert_equal "S1", ar.getVariable()
    assert_equal "T / (1 + Keq)", ar.getFormula()
    ar =  m.getRule(1) 
    assert_not_equal nil, ar
    assert_equal "S2", ar.getVariable()
    assert_equal "Keq * S1", ar.getFormula()
    assert_equal 2, m.getNumReactions()
    r = m.getReaction(0)
    assert_not_equal nil, r
    assert_equal "in", r.getId()
    assert_equal 1, r.getNumReactants()
    assert_equal 1, r.getNumProducts()
    sr = r.getReactant(0)
    assert_not_equal nil, sr
    assert_equal "X0", sr.getSpecies()
    sr = r.getProduct(0)
    assert_not_equal nil, sr
    assert_equal "T", sr.getSpecies()
    kl = r.getKineticLaw()
    assert_not_equal nil, kl
    assert_equal "k1 * X0", kl.getFormula()
    assert_equal 1, kl.getNumParameters()
    p = kl.getParameter(0)
    assert_not_equal nil, p
    assert_equal "k1", p.getId()
    assert_equal 0.1, p.getValue()
    r = m.getReaction(1)
    assert_not_equal nil, r
    assert_equal "out", r.getId()
    assert_equal 1, r.getNumReactants()
    assert_equal 1, r.getNumProducts()
    sr = r.getReactant(0)
    assert_not_equal nil, sr
    assert_equal "T", sr.getSpecies()
    sr = r.getProduct(0)
    assert_not_equal nil, sr
    assert_equal "X1", sr.getSpecies()
    kl = r.getKineticLaw()
    assert_not_equal nil, kl
    assert_equal "k2 * T", kl.getFormula()
    assert_equal 1, kl.getNumParameters()
    p = kl.getParameter(0)
    assert_not_equal nil, p
    assert_equal "k2", p.getId()
    assert_equal 0.15, p.getValue()
  end

end
