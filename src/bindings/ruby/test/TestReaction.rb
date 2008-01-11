#
# This file was converted from libsbml/src/sbml/test/TestReaction.c
# with the help of test_c2ruby.pl (manual handling required).
#
require 'test/unit'
require 'libSBML'

class TestReaction < Test::Unit::TestCase

  def test_Reaction_addReactant
    sr = LibSBML::SpeciesReference.new
    @@r.addReactant(sr)
    assert_equal 1, @@r.getNumReactants
    assert_equal 0, @@r.getNumProducts
    assert_equal 0, @@r.getNumModifiers
  end

  def test_Reaction_getReactantById
    sr1 = LibSBML::SpeciesReference.new("R1",1,1)
    sr2 = LibSBML::SpeciesReference.new("R2",1,1)
    @@r.addReactant(sr1)
    @@r.addReactant(sr2)
    assert_equal 2, @@r.getNumReactants
    assert_equal 0, @@r.getNumProducts
    assert_equal 0, @@r.getNumModifiers
    assert_equal sr1, @@r.getReactant( "R1")
    assert_equal sr2, @@r.getReactant( "R2")
    assert_equal nil, @@r.getReactant( "R3")
  end

  def test_Reaction_createWith
    kl = LibSBML::KineticLaw.new
    r = LibSBML::Reaction.new("r1", "",kl,0)
    r.setFast(1);
    assert_equal LibSBML::SBML_REACTION, r.getTypeCode
    assert_equal "", r.getMetaId
    assert_equal nil, r.getNotes
    assert_equal nil, r.getAnnotation
    assert_equal "", r.getName
    assert_equal  "r1",r.getId
    assert_equal false, r.getReversible
    assert_equal true, r.getFast
    assert_equal true, r.isSetId
    assert_equal false, r.isSetName
    assert_equal true, r.isSetKineticLaw
    assert_equal 0, r.getNumReactants
    assert_equal 0, r.getNumProducts
    assert_equal 0, r.getNumModifiers
  end

  def test_Reaction_free_NULL
  end

  def test_Reaction_addProduct
    sr = LibSBML::SpeciesReference.new
    @@r.addProduct(sr)
    assert_equal 0, @@r.getNumReactants
    assert_equal 1, @@r.getNumProducts
    assert_equal 0, @@r.getNumModifiers
  end

  def test_Reaction_addModifier
    @@r.addModifier(LibSBML::ModifierSpeciesReference.new())
    assert_equal 0, @@r.getNumReactants
    assert_equal 0, @@r.getNumProducts
    assert_equal 1, @@r.getNumModifiers
  end

  def test_Reaction_getReactant
    sr1 = LibSBML::SpeciesReference.new
    sr2 = LibSBML::SpeciesReference.new
    sr1.setSpecies( "R1")
    sr2.setSpecies( "R2")
    @@r.addReactant(sr1)
    @@r.addReactant(sr2)
    assert_equal 2, @@r.getNumReactants
    assert_equal 0, @@r.getNumProducts
    assert_equal 0, @@r.getNumModifiers
    sr1 = @@r.getReactant(0)
    sr2 = @@r.getReactant(1)
    assert_equal  "R1",sr1.getSpecies
    assert_equal  "R2",sr2.getSpecies
  end

  def setup
    @@r = LibSBML::Reaction.new
      
  end

  def test_Reaction_getModifier
    msr1 = LibSBML::ModifierSpeciesReference.new()
    msr2 = LibSBML::ModifierSpeciesReference.new()
    msr1.setSpecies( "M1")
    msr2.setSpecies( "M2")
    @@r.addModifier(msr1)
    @@r.addModifier(msr2)
    assert_equal 0, @@r.getNumReactants
    assert_equal 0, @@r.getNumProducts
    assert_equal 2, @@r.getNumModifiers
    msr1 = @@r.getModifier(0)
    msr2 = @@r.getModifier(1)
    assert_equal  "M1",msr1.getSpecies
    assert_equal  "M2",msr2.getSpecies
  end

  def test_Reaction_getProduct
    sr1 = LibSBML::SpeciesReference.new
    sr2 = LibSBML::SpeciesReference.new
    sr1.setSpecies( "P1")
    sr2.setSpecies( "P2")
    @@r.addProduct(sr1)
    @@r.addProduct(sr2)
    assert_equal 0, @@r.getNumReactants
    assert_equal 2, @@r.getNumProducts
    assert_equal 0, @@r.getNumModifiers
    sr1 = @@r.getProduct(0)
    sr2 = @@r.getProduct(1)
    assert_equal  "P1",sr1.getSpecies
    assert_equal  "P2",sr2.getSpecies
  end

  def test_Reaction_create
    assert_equal LibSBML::SBML_REACTION, @@r.getTypeCode
    assert_equal "", @@r.getMetaId
    assert_equal nil, @@r.getNotes
    assert_equal nil, @@r.getAnnotation
    assert_equal "", @@r.getId
    assert_equal "", @@r.getName
    assert_equal nil, @@r.getKineticLaw
    assert_not_equal false, @@r.getReversible
    assert_equal false, @@r.getFast
    assert_equal false, @@r.isSetId
    assert_equal false, @@r.isSetName
    assert_equal false, @@r.isSetKineticLaw
    assert_equal 0, @@r.getNumReactants
    assert_equal 0, @@r.getNumProducts
    assert_equal 0, @@r.getNumModifiers
  end

  def test_Reaction_setName
    name = "MapK Cascade"
    @@r.setName(name)
    assert_equal name,@@r.getName
    assert_equal true, @@r.isSetName
      @@r.setName(@@r.getName)
      assert_equal name,@@r.getName
      @@r.setName("")
      assert_equal false, @@r.isSetName
  end

  def test_Reaction_getModifierById
    msr1 = LibSBML::ModifierSpeciesReference.new()
    msr2 = LibSBML::ModifierSpeciesReference.new()
    msr1.setSpecies( "M1")
    msr2.setSpecies( "M2")
    @@r.addModifier(msr1)
    @@r.addModifier(msr2)
    assert_equal 0, @@r.getNumReactants
    assert_equal 0, @@r.getNumProducts
    assert_equal 2, @@r.getNumModifiers
    assert_equal msr1, @@r.getModifier( "M1")
    assert_equal msr2, @@r.getModifier( "M2")
    assert_equal nil, @@r.getModifier( "M3")
  end

  def test_Reaction_getProductById
    sr1 = LibSBML::SpeciesReference.new("P1",1,1)
    sr2 = LibSBML::SpeciesReference.new("P2",1,1)
    @@r.addProduct(sr1)
    @@r.addProduct(sr2)
    assert_equal 0, @@r.getNumReactants
    assert_equal 2, @@r.getNumProducts
    assert_equal 0, @@r.getNumModifiers
    assert_equal sr1, @@r.getProduct( "P1")
    assert_equal sr2, @@r.getProduct( "P2")
    assert_equal nil, @@r.getProduct( "P3")
  end

  def test_Reaction_setId
    id = "J1"
    @@r.setId(id)
    assert_equal id,@@r.getId
    assert_equal true, @@r.isSetId
      @@r.setId(@@r.getId)
      assert_equal id,@@r.getId
      @@r.setId("")
      assert_equal false, @@r.isSetId
  end

end
