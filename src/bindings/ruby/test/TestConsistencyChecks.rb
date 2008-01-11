#
# This file was converted from libsbml/src/sbml/test/TestConsistencyChecks.cpp
# with the help of test_c2ruby.pl (manual handling required).
#
require 'test/unit'
require 'libSBML'

class TestConsistencyChecks < Test::Unit::TestCase

  def test_consistency_checks
    filename  = "../../sbml/test/test-data/inconsistent.xml"
    reader = LibSBML::SBMLReader.new()
    d = reader.readSBML(filename)
    errors = d.checkConsistency()
    assert_equal 1, errors
    assert_equal 10301, d.getError(0).getErrorId()
    d.getErrorLog().clearLog()
    d.setConsistencyChecks(LibSBML::LIBSBML_CAT_IDENTIFIER_CONSISTENCY,false)
    errors = d.checkConsistency()
    assert_equal 1, errors
    assert_equal 20612, d.getError(0).getErrorId()
    d.getErrorLog().clearLog()
    d.setConsistencyChecks(LibSBML::LIBSBML_CAT_GENERAL_CONSISTENCY,false)
    errors = d.checkConsistency()
    assert_equal 1, errors
    assert_equal 10701, d.getError(0).getErrorId()
    d.getErrorLog().clearLog()
    d.setConsistencyChecks(LibSBML::LIBSBML_CAT_SBO_CONSISTENCY,false)
    errors = d.checkConsistency()
    assert_equal 1, errors
    assert_equal 10214, d.getError(0).getErrorId()
    d.getErrorLog().clearLog()
    d.setConsistencyChecks(LibSBML::LIBSBML_CAT_MATHML_CONSISTENCY,false)
    errors = d.checkConsistency()
    assert_equal 2, errors
    assert_equal 10523, d.getError(0).getErrorId()
    assert_equal 99505, d.getError(1).getErrorId()
    d.getErrorLog().clearLog()
    d.setConsistencyChecks(LibSBML::LIBSBML_CAT_UNITS_CONSISTENCY,false)
    errors = d.checkConsistency()
    assert_equal 1, errors
    assert_equal 80701, d.getError(0).getErrorId()
  end

end
