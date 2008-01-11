#
# This file was converted from libsbml/src/sbml/test/TestSpeciesType.c
# with the help of test_c2ruby.pl (manual handling required).
#
require 'test/unit'
require 'libSBML'

class TestSpeciesType < Test::Unit::TestCase

  def test_SpeciesType_create
    assert_equal LibSBML::SBML_SPECIES_TYPE, @@ct.getTypeCode
    assert_equal "", @@ct.getMetaId
    assert_equal nil, @@ct.getNotes
    assert_equal nil, @@ct.getAnnotation
    assert_equal "", @@ct.getId
    assert_equal "", @@ct.getName
    assert_equal false, @@ct.isSetId
    assert_equal false, @@ct.isSetName
  end

  def test_SpeciesType_unsetName
    @@ct.setName( "name")
    assert_equal  "name"     ,@@ct.getName
    assert_equal true, @@ct.isSetName
    @@ct.unsetName
    assert_equal false, @@ct.isSetName
  end

  def test_SpeciesType_setId
    id = "mitochondria"
    @@ct.setId(id)
    assert_equal id,@@ct.getId
    assert_equal true, @@ct.isSetId
      @@ct.setId(@@ct.getId)
      assert_equal id,@@ct.getId
      @@ct.setId("")
      assert_equal false, @@ct.isSetId
  end

  def test_SpeciesType_free_NULL
  end

  def test_SpeciesType_setName
    name = "My Favorite Factory"
    @@ct.setName(name)
    assert_equal name,@@ct.getName
    assert_equal true, @@ct.isSetName
      @@ct.setName(@@ct.getName)
      assert_equal name,@@ct.getName
      @@ct.setName("")
      assert_equal false, @@ct.isSetName
  end

  def test_SpeciesType_createWith
    c = LibSBML::SpeciesType.new("A", "")
    assert_equal LibSBML::SBML_SPECIES_TYPE, c.getTypeCode
    assert_equal "", c.getMetaId
    assert_equal nil, c.getNotes
    assert_equal nil, c.getAnnotation
    assert_equal "", c.getName
    assert_equal  "A"     ,c.getId
    assert_equal true, c.isSetId
    assert_equal false, c.isSetName
  end

  def setup
    @@ct = LibSBML::SpeciesType.new
      
  end

end
