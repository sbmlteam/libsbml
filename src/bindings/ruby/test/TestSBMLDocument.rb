#
# This file was converted from libsbml/src/sbml/test/TestSBMLDocument.c
# with the help of test_c2ruby.pl (manual handling required).
#
require 'test/unit'
require 'libSBML'

class TestSBMLDocument < Test::Unit::TestCase

  def test_SBMLDocument_free_NULL
  end

  def test_SBMLDocument_setModel
    d = LibSBML::SBMLDocument.new
    m1 = LibSBML::Model.new
    m2 = LibSBML::Model.new
    assert_equal nil, d.getModel
    d.setModel(m1)
    assert_equal m1, d.getModel
    d.setModel(d.getModel)
    assert_equal m1, d.getModel
    d.setModel(m2)
    assert_equal m2, d.getModel
  end

  def test_SBMLDocument_setLevelAndVersion
    d = LibSBML::SBMLDocument.new
    d.setLevelAndVersion(2,2)
    m1 = LibSBML::Model.new
    d.setModel(m1)
    assert_equal true, d.setLevelAndVersion(2,3)
    assert_equal true, d.setLevelAndVersion(2,1)
    assert_equal true, d.setLevelAndVersion(1,2)
    assert_equal false, d.setLevelAndVersion(1,1)
  end

  def test_SBMLDocument_createWith
    d = LibSBML::SBMLDocument.new(1,2)
    assert_equal LibSBML::SBML_DOCUMENT, d.getTypeCode
    assert_equal nil, d.getNotes
    assert_equal nil, d.getAnnotation
    assert_equal 1, d.getLevel
    assert_equal 2, d.getVersion
    assert_equal 0, d.getNumErrors
  end

  def test_SBMLDocument_setLevelAndVersion_Warning
    d = LibSBML::SBMLDocument.new
    d.setLevelAndVersion(2,2)
    m1 = LibSBML::Model.new
    m1.setSBOTerm(2)
    d.setModel(m1)
    assert_equal true, d.setLevelAndVersion(2,3)
    assert_equal true, d.setLevelAndVersion(2,1)
    assert_equal true, d.setLevelAndVersion(1,2)
    assert_equal false, d.setLevelAndVersion(1,1)
  end

  def test_SBMLDocument_create
    d = LibSBML::SBMLDocument.new
    assert_equal LibSBML::SBML_DOCUMENT, d.getTypeCode
    assert_equal nil, d.getNotes
    assert_equal nil, d.getAnnotation
    assert_equal 2, d.getLevel
    assert_equal 3, d.getVersion
    assert_equal 0, d.getNumErrors
  end

  def test_SBMLDocument_setLevelAndVersion_Error
    d = LibSBML::SBMLDocument.new
    d.setLevelAndVersion(2,1)
    m1 = LibSBML::Model.new
    u = LibSBML::Unit.new
    u.setKind(LibSBML::UnitKind_forName("mole"))
    u.setOffset(3.2)
    ud = LibSBML::UnitDefinition.new
    ud.addUnit(u)
    m1.addUnitDefinition(ud)
    d.setModel(m1)
    assert_equal false, d.setLevelAndVersion(2,2)
    assert_equal false, d.setLevelAndVersion(2,3)
    assert_equal false, d.setLevelAndVersion(1,2)
    assert_equal false, d.setLevelAndVersion(1,1)
  end

end
