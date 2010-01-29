#
# @file    TestReaction_newSetters.rb
# @brief   Reaction unit tests for new set function API
#
# @author  Akiya Jouraku (Ruby conversion)
# @author  Sarah Keating 
#
# $Id$
# $HeadURL$
#
# This test file was converted from src/sbml/test/TestReaction_newSetters.c
# with the help of conversion sciprt (ctest_converter.pl).
#
#<!---------------------------------------------------------------------------
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.
#
# Copyright 2005-2010 California Institute of Technology.
# Copyright 2002-2005 California Institute of Technology and
#                     Japan Science and Technology Corporation.
# 
# This library is free software; you can redistribute it and/or modify it
# under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation.  A copy of the license agreement is provided
# in the file named "LICENSE.txt" included with this software distribution
# and also available online as http://sbml.org/software/libsbml/license.html
#--------------------------------------------------------------------------->*/
require 'test/unit'
require 'libSBML'

class TestReaction_newSetters < Test::Unit::TestCase

  def setup
    @@r = LibSBML::Reaction.new(1,2)
    if (@@r == nil)
    end
  end

  def teardown
    @@r = nil
  end

  def test_Reaction_addModifier1
    m = LibSBML::Reaction.new(2,2)
    p = LibSBML::ModifierSpeciesReference.new(2,2)
    p1 = LibSBML::ModifierSpeciesReference.new(2,2)
    p1.setSpecies( "k")
    p1.setId( "k1")
    i = m.addModifier(p)
    assert( i == LibSBML::LIBSBML_INVALID_OBJECT )
    p.setSpecies( "k")
    p.setId( "k1")
    i = m.addModifier(p)
    assert( i == LibSBML::LIBSBML_OPERATION_SUCCESS )
    assert( m.getNumModifiers() == 1 )
    i = m.addModifier(p1)
    assert( i == LibSBML::LIBSBML_DUPLICATE_OBJECT_ID )
    assert( m.getNumModifiers() == 1 )
    p = nil
    p1 = nil
    m = nil
  end

  def test_Reaction_addModifier2
    m = LibSBML::Reaction.new(2,2)
    p = LibSBML::ModifierSpeciesReference.new(2,1)
    p.setSpecies( "k")
    i = m.addModifier(p)
    assert( i == LibSBML::LIBSBML_VERSION_MISMATCH )
    assert( m.getNumModifiers() == 0 )
    p = nil
    m = nil
  end

  def test_Reaction_addModifier3
    m = LibSBML::Reaction.new(2,2)
    p = nil
    i = m.addModifier(p)
    assert( i == LibSBML::LIBSBML_OPERATION_FAILED )
    assert( m.getNumModifiers() == 0 )
    m = nil
  end

  def test_Reaction_addProduct1
    m = LibSBML::Reaction.new(2,2)
    p = LibSBML::SpeciesReference.new(2,2)
    p1 = LibSBML::SpeciesReference.new(2,2)
    p1.setSpecies( "k")
    p1.setId( "k1")
    i = m.addProduct(p)
    assert( i == LibSBML::LIBSBML_INVALID_OBJECT )
    p.setSpecies( "k")
    p.setId( "k1")
    i = m.addProduct(p)
    assert( i == LibSBML::LIBSBML_OPERATION_SUCCESS )
    assert( m.getNumProducts() == 1 )
    i = m.addProduct(p1)
    assert( i == LibSBML::LIBSBML_DUPLICATE_OBJECT_ID )
    assert( m.getNumProducts() == 1 )
    p = nil
    p1 = nil
    m = nil
  end

  def test_Reaction_addProduct2
    m = LibSBML::Reaction.new(2,2)
    p = LibSBML::SpeciesReference.new(2,1)
    p.setSpecies( "k")
    i = m.addProduct(p)
    assert( i == LibSBML::LIBSBML_VERSION_MISMATCH )
    assert( m.getNumProducts() == 0 )
    p = nil
    m = nil
  end

  def test_Reaction_addProduct3
    m = LibSBML::Reaction.new(2,2)
    p = LibSBML::SpeciesReference.new(1,2)
    p.setSpecies( "k")
    i = m.addProduct(p)
    assert( i == LibSBML::LIBSBML_LEVEL_MISMATCH )
    assert( m.getNumProducts() == 0 )
    p = nil
    m = nil
  end

  def test_Reaction_addProduct4
    m = LibSBML::Reaction.new(2,2)
    p = nil
    i = m.addProduct(p)
    assert( i == LibSBML::LIBSBML_OPERATION_FAILED )
    assert( m.getNumProducts() == 0 )
    m = nil
  end

  def test_Reaction_addReactant1
    m = LibSBML::Reaction.new(2,2)
    p = LibSBML::SpeciesReference.new(2,2)
    p1 = LibSBML::SpeciesReference.new(2,2)
    p1.setSpecies( "k")
    p1.setId( "k1")
    i = m.addReactant(p)
    assert( i == LibSBML::LIBSBML_INVALID_OBJECT )
    p.setSpecies( "k")
    p.setId( "k1")
    i = m.addReactant(p)
    assert( i == LibSBML::LIBSBML_OPERATION_SUCCESS )
    assert( m.getNumReactants() == 1 )
    i = m.addReactant(p1)
    assert( i == LibSBML::LIBSBML_DUPLICATE_OBJECT_ID )
    assert( m.getNumReactants() == 1 )
    p1 = nil
    p = nil
    m = nil
  end

  def test_Reaction_addReactant2
    m = LibSBML::Reaction.new(2,2)
    p = LibSBML::SpeciesReference.new(2,1)
    p.setSpecies( "k")
    i = m.addReactant(p)
    assert( i == LibSBML::LIBSBML_VERSION_MISMATCH )
    assert( m.getNumReactants() == 0 )
    p = nil
    m = nil
  end

  def test_Reaction_addReactant3
    m = LibSBML::Reaction.new(2,2)
    p = LibSBML::SpeciesReference.new(1,2)
    p.setSpecies( "k")
    i = m.addReactant(p)
    assert( i == LibSBML::LIBSBML_LEVEL_MISMATCH )
    assert( m.getNumReactants() == 0 )
    p = nil
    m = nil
  end

  def test_Reaction_addReactant4
    m = LibSBML::Reaction.new(2,2)
    p = nil
    i = m.addReactant(p)
    assert( i == LibSBML::LIBSBML_OPERATION_FAILED )
    assert( m.getNumReactants() == 0 )
    m = nil
  end

  def test_Reaction_createKineticLaw
    r = LibSBML::Reaction.new(2,2)
    kl = r.createKineticLaw()
    assert( r.isSetKineticLaw() == true )
    assert( (kl).getLevel() == 2 )
    assert( (kl).getVersion() == 2 )
    r = nil
  end

  def test_Reaction_createModifier
    m = LibSBML::Reaction.new(2,2)
    p = m.createModifier()
    assert( m.getNumModifiers() == 1 )
    assert( (p).getLevel() == 2 )
    assert( (p).getVersion() == 2 )
    m = nil
  end

  def test_Reaction_createProduct
    m = LibSBML::Reaction.new(2,2)
    p = m.createProduct()
    assert( m.getNumProducts() == 1 )
    assert( (p).getLevel() == 2 )
    assert( (p).getVersion() == 2 )
    m = nil
  end

  def test_Reaction_createReactant
    m = LibSBML::Reaction.new(2,2)
    p = m.createReactant()
    assert( m.getNumReactants() == 1 )
    assert( (p).getLevel() == 2 )
    assert( (p).getVersion() == 2 )
    m = nil
  end

  def test_Reaction_setFast1
    i = @@r.setFast(true)
    assert( i == LibSBML::LIBSBML_OPERATION_SUCCESS )
    assert( @@r.getFast() == true )
    assert_equal true, @@r.isSetFast()
    i = @@r.setFast(false)
    assert( i == LibSBML::LIBSBML_OPERATION_SUCCESS )
    assert( @@r.getFast() == false )
    assert_equal true, @@r.isSetFast()
    i = @@r.unsetFast()
    assert( i == LibSBML::LIBSBML_OPERATION_SUCCESS )
    assert( @@r.getFast() == false )
    assert_equal false, @@r.isSetFast()
  end

  def test_Reaction_setFast2
    r1 = LibSBML::Reaction.new(2,4)
    i = r1.unsetFast()
    assert( i == LibSBML::LIBSBML_OPERATION_SUCCESS )
    assert( r1.getFast() == false )
    assert_equal false, r1.isSetFast()
  end

  def test_Reaction_setId1
    i = @@r.setId( "1cell")
    assert( i == LibSBML::LIBSBML_INVALID_ATTRIBUTE_VALUE )
    assert_equal false, @@r.isSetId()
  end

  def test_Reaction_setId2
    i = @@r.setId( "cell")
    assert( i == LibSBML::LIBSBML_OPERATION_SUCCESS )
    assert_equal true, @@r.isSetId()
    assert ((  "cell"  == @@r.getId() ))
    i = @@r.setId("")
    assert( i == LibSBML::LIBSBML_OPERATION_SUCCESS )
    assert_equal false, @@r.isSetId()
  end

  def test_Reaction_setKineticLaw1
    kl = LibSBML::KineticLaw.new(2,1)
    i = @@r.setKineticLaw(kl)
    assert( i == LibSBML::LIBSBML_LEVEL_MISMATCH )
    assert_equal false, @@r.isSetKineticLaw()
    kl = nil
  end

  def test_Reaction_setKineticLaw2
    kl = LibSBML::KineticLaw.new(1,1)
    i = @@r.setKineticLaw(kl)
    assert( i == LibSBML::LIBSBML_VERSION_MISMATCH )
    assert_equal false, @@r.isSetKineticLaw()
    kl = nil
  end

  def test_Reaction_setKineticLaw3
    kl = LibSBML::KineticLaw.new(1,2)
    i = @@r.setKineticLaw(kl)
    assert( i == LibSBML::LIBSBML_OPERATION_SUCCESS )
    assert_equal true, @@r.isSetKineticLaw()
    kl = nil
  end

  def test_Reaction_setKineticLaw4
    i = @@r.setKineticLaw(nil)
    assert( i == LibSBML::LIBSBML_OPERATION_SUCCESS )
    assert_equal false, @@r.isSetKineticLaw()
    i = @@r.unsetKineticLaw()
    assert( i == LibSBML::LIBSBML_OPERATION_SUCCESS )
    assert_equal false, @@r.isSetKineticLaw()
  end

  def test_Reaction_setName1
    i = @@r.setName( "cell")
    assert( i == LibSBML::LIBSBML_OPERATION_SUCCESS )
    assert_equal true, @@r.isSetName()
    i = @@r.unsetName()
    assert( i == LibSBML::LIBSBML_OPERATION_SUCCESS )
    assert_equal false, @@r.isSetName()
  end

  def test_Reaction_setName2
    p = LibSBML::Reaction.new(2,2)
    i = p.setName( "1cell")
    assert( i == LibSBML::LIBSBML_OPERATION_SUCCESS )
    assert_equal true, p.isSetName()
    i = p.unsetName()
    assert( i == LibSBML::LIBSBML_OPERATION_SUCCESS )
    assert_equal false, p.isSetName()
    p = nil
  end

  def test_Reaction_setName3
    p = LibSBML::Reaction.new(2,2)
    i = p.setName("")
    assert( i == LibSBML::LIBSBML_OPERATION_SUCCESS )
    assert_equal false, p.isSetName()
    p = nil
  end

  def test_Reaction_setReversible1
    i = @@r.setReversible(true)
    assert( i == LibSBML::LIBSBML_OPERATION_SUCCESS )
    assert( @@r.getReversible() == true )
    i = @@r.setReversible(false)
    assert( i == LibSBML::LIBSBML_OPERATION_SUCCESS )
    assert( @@r.getReversible() == false )
  end

end
