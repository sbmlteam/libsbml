#
# @file    TestReaction.rb
# @brief   SBML Reaction unit tests
# @author  Akiya Jouraku (Ruby conversion)
# @author  Ben Bornstein 
#
# $Id$
# $Source$
#
# This test file was converted from src/sbml/test/TestReaction.c
# wiht the help of conversion sciprt (ctest_converter.pl).
#
#<!---------------------------------------------------------------------------
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.
#
# Copyright 2005-2008 California Institute of Technology.
# Copyright 2002-2005 California Institute of Technology and
#                     Japan Science and Technology Corporation.
# 
# This library is free software; you can redistribute it and/or modify it
# under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation.  A copy of the license agreement is provided
# in the file named "LICENSE.txt" included with this software distribution
# and also available online as http://sbml.org/software/libsbml/license.html
#--------------------------------------------------------------------------->*/
#
require 'test/unit'
require 'libSBML'

class TestReaction < Test::Unit::TestCase

  def setup
    @@r = LibSBML::Reaction.new
    if (@@r == nil)
    end
  end

  def test_Reaction_addModifier
    @@r.addModifier(LibSBML::ModifierSpeciesReference.new())
    assert( @@r.getNumReactants == 0 )
    assert( @@r.getNumProducts == 0 )
    assert( @@r.getNumModifiers == 1 )
  end

  def test_Reaction_addProduct
    sr = LibSBML::SpeciesReference.new
    @@r.addProduct(sr)
    assert( @@r.getNumReactants == 0 )
    assert( @@r.getNumProducts == 1 )
    assert( @@r.getNumModifiers == 0 )
  end

  def test_Reaction_addReactant
    sr = LibSBML::SpeciesReference.new
    @@r.addReactant(sr)
    assert( @@r.getNumReactants == 1 )
    assert( @@r.getNumProducts == 0 )
    assert( @@r.getNumModifiers == 0 )
  end

  def test_Reaction_create
    assert( @@r.getTypeCode == LibSBML::SBML_REACTION )
    assert( @@r.getMetaId == "" )
    assert( @@r.getNotes == nil )
    assert( @@r.getAnnotation == nil )
    assert( @@r.getId == "" )
    assert( @@r.getName == "" )
    assert( @@r.getKineticLaw == nil )
    assert( @@r.getReversible != false )
    assert( @@r.getFast == false )
    assert_equal false, @@r.isSetId
    assert_equal false, @@r.isSetName
    assert_equal false, @@r.isSetKineticLaw
    assert( @@r.getNumReactants == 0 )
    assert( @@r.getNumProducts == 0 )
    assert( @@r.getNumModifiers == 0 )
  end

  def test_Reaction_createWith
    kl = LibSBML::KineticLaw.new
    r = LibSBML::Reaction.new("r1", "",kl,0)
    r.setFast(1)
    assert( r.getTypeCode == LibSBML::SBML_REACTION )
    assert( r.getMetaId == "" )
    assert( r.getNotes == nil )
    assert( r.getAnnotation == nil )
    assert( r.getName == "" )
    assert ((  "r1" == r.getId ))
    assert( r.getReversible == false )
    assert( r.getFast == true )
    assert_equal true, r.isSetId
    assert_equal false, r.isSetName
    assert_equal true, r.isSetKineticLaw
    assert( r.getNumReactants == 0 )
    assert( r.getNumProducts == 0 )
    assert( r.getNumModifiers == 0 )
  end

  def test_Reaction_free_NULL
  end

  def test_Reaction_getModifier
    msr1 = LibSBML::ModifierSpeciesReference.new()
    msr2 = LibSBML::ModifierSpeciesReference.new()
    msr1.setSpecies( "M1")
    msr2.setSpecies( "M2")
    @@r.addModifier(msr1)
    @@r.addModifier(msr2)
    assert( @@r.getNumReactants == 0 )
    assert( @@r.getNumProducts == 0 )
    assert( @@r.getNumModifiers == 2 )
    msr1 = @@r.getModifier(0)
    msr2 = @@r.getModifier(1)
    assert ((  "M1" == msr1.getSpecies ))
    assert ((  "M2" == msr2.getSpecies ))
  end

  def test_Reaction_getModifierById
    msr1 = LibSBML::ModifierSpeciesReference.new()
    msr2 = LibSBML::ModifierSpeciesReference.new()
    msr1.setSpecies( "M1")
    msr2.setSpecies( "M2")
    @@r.addModifier(msr1)
    @@r.addModifier(msr2)
    assert( @@r.getNumReactants == 0 )
    assert( @@r.getNumProducts == 0 )
    assert( @@r.getNumModifiers == 2 )
    assert( @@r.getModifier( "M1") != msr1 )
    assert( @@r.getModifier( "M2") != msr2 )
    assert( @@r.getModifier( "M3") == nil )
  end

  def test_Reaction_getProduct
    sr1 = LibSBML::SpeciesReference.new
    sr2 = LibSBML::SpeciesReference.new
    sr1.setSpecies( "P1")
    sr2.setSpecies( "P2")
    @@r.addProduct(sr1)
    @@r.addProduct(sr2)
    assert( @@r.getNumReactants == 0 )
    assert( @@r.getNumProducts == 2 )
    assert( @@r.getNumModifiers == 0 )
    sr1 = @@r.getProduct(0)
    sr2 = @@r.getProduct(1)
    assert ((  "P1" == sr1.getSpecies ))
    assert ((  "P2" == sr2.getSpecies ))
  end

  def test_Reaction_getProductById
    sr1 = LibSBML::SpeciesReference.new("P1",1,1)
    sr2 = LibSBML::SpeciesReference.new("P2",1,1)
    @@r.addProduct(sr1)
    @@r.addProduct(sr2)
    assert( @@r.getNumReactants == 0 )
    assert( @@r.getNumProducts == 2 )
    assert( @@r.getNumModifiers == 0 )
    assert( @@r.getProduct( "P1") != sr1 )
    assert( @@r.getProduct( "P2") != sr2 )
    assert( @@r.getProduct( "P3") == nil )
  end

  def test_Reaction_getReactant
    sr1 = LibSBML::SpeciesReference.new
    sr2 = LibSBML::SpeciesReference.new
    sr1.setSpecies( "R1")
    sr2.setSpecies( "R2")
    @@r.addReactant(sr1)
    @@r.addReactant(sr2)
    assert( @@r.getNumReactants == 2 )
    assert( @@r.getNumProducts == 0 )
    assert( @@r.getNumModifiers == 0 )
    sr1 = @@r.getReactant(0)
    sr2 = @@r.getReactant(1)
    assert ((  "R1" == sr1.getSpecies ))
    assert ((  "R2" == sr2.getSpecies ))
  end

  def test_Reaction_getReactantById
    sr1 = LibSBML::SpeciesReference.new("R1",1,1)
    sr2 = LibSBML::SpeciesReference.new("R2",1,1)
    @@r.addReactant(sr1)
    @@r.addReactant(sr2)
    assert( @@r.getNumReactants == 2 )
    assert( @@r.getNumProducts == 0 )
    assert( @@r.getNumModifiers == 0 )
    assert( @@r.getReactant( "R1") != sr1 )
    assert( @@r.getReactant( "R2") != sr2 )
    assert( @@r.getReactant( "R3") == nil )
  end

  def test_Reaction_setId
    id = "J1"
    @@r.setId(id)
    assert (( id == @@r.getId ))
    assert_equal true, @@r.isSetId
    if (@@r.getId == id)
    end
    @@r.setId(@@r.getId)
    assert (( id == @@r.getId ))
    @@r.setId("")
    assert_equal false, @@r.isSetId
    if (@@r.getId != nil)
    end
  end

  def test_Reaction_setName
    name = "MapK Cascade"
    @@r.setName(name)
    assert (( name == @@r.getName ))
    assert_equal true, @@r.isSetName
    if (@@r.getName == name)
    end
    @@r.setName(@@r.getName)
    assert (( name == @@r.getName ))
    @@r.setName("")
    assert_equal false, @@r.isSetName
    if (@@r.getName != nil)
    end
  end

end
