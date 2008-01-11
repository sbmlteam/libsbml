#
# This file was converted from libsbml/src/sbml/test/TestParameter.c
# with the help of test_c2ruby.pl (manual handling required).
#
require 'test/unit'
require 'libSBML'

class TestParameter < Test::Unit::TestCase

  def test_Parameter_setUnits
    units = "second"
    @@p.setUnits(units)
    assert_equal units,@@p.getUnits
    assert_equal true, @@p.isSetUnits
      @@p.setUnits(@@p.getUnits)
      assert_equal units,@@p.getUnits
      @@p.setUnits("")
      assert_equal false, @@p.isSetUnits
  end

  def test_Parameter_setName
    name = "Forward Michaelis-Menten Constant"
    @@p.setName(name)
    assert_equal name,@@p.getName
    assert_equal true, @@p.isSetName
      @@p.setName(@@p.getName)
      assert_equal name,@@p.getName
      @@p.setName("")
      assert_equal false, @@p.isSetName
  end

  def test_Parameter_create
    assert_equal LibSBML::SBML_PARAMETER, @@p.getTypeCode
    assert_equal "", @@p.getMetaId
    assert_equal nil, @@p.getNotes
    assert_equal nil, @@p.getAnnotation
    assert_equal "", @@p.getId
    assert_equal "", @@p.getName
    assert_equal "", @@p.getUnits
    assert_equal true, @@p.getConstant
    assert_equal false, @@p.isSetId
    assert_equal false, @@p.isSetName
    assert_equal false, @@p.isSetValue
    assert_equal false, @@p.isSetUnits
  end

  def setup
    @@p = LibSBML::Parameter.new
      
  end

  def test_Parameter_free_NULL
  end

  def test_Parameter_setId
    id = "Km1"
    @@p.setId(id)
    assert_equal id,@@p.getId
    assert_equal true, @@p.isSetId
      @@p.setId(@@p.getId)
      assert_equal id,@@p.getId
      @@p.setId("")
      assert_equal false, @@p.isSetId
  end

  def test_Parameter_createWith
    p = LibSBML::Parameter.new("delay",6.2, "second")
    assert_equal LibSBML::SBML_PARAMETER, p.getTypeCode
    assert_equal "", p.getMetaId
    assert_equal nil, p.getNotes
    assert_equal nil, p.getAnnotation
    assert_equal  "delay" ,p.getId
    assert_equal  "second",p.getUnits
    assert_equal "", p.getName
    assert_equal 6.2, p.getValue
    assert_equal true, p.getConstant
    assert_equal true, p.isSetId
    assert_equal false, p.isSetName
    assert_equal true, p.isSetValue
    assert_equal true, p.isSetUnits
  end

end
