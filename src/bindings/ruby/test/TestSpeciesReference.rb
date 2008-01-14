#
# @file    TestSpeciesReference.rb
# @brief   SpeciesReference unit tests
# @author  Akiya Jouraku (Ruby conversion)
# @author  Ben Bornstein 
#
# $Id$
# $Source$
#
# This test file was converted from src/sbml/test/TestSpeciesReference.c
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

class TestSpeciesReference < Test::Unit::TestCase

  def setup
    @@sr = LibSBML::SpeciesReference.new
    if (@@sr == nil)
    end
  end

  def test_SpeciesReference_create
    assert( @@sr.getTypeCode == LibSBML::SBML_SPECIES_REFERENCE )
    assert( @@sr.getMetaId == "" )
    assert( @@sr.getNotes == nil )
    assert( @@sr.getAnnotation == nil )
    assert( @@sr.getSpecies == "" )
    assert( @@sr.getStoichiometry == 1 )
    assert( @@sr.getStoichiometryMath == nil )
    assert( @@sr.getDenominator == 1 )
    assert_equal false, @@sr.isSetSpecies
    assert_equal false, @@sr.isSetStoichiometryMath
  end

  def test_SpeciesReference_createModifier
    sr = LibSBML::ModifierSpeciesReference.new()
    assert( sr.getTypeCode == LibSBML::SBML_MODIFIER_SPECIES_REFERENCE )
    assert( sr.getMetaId == "" )
    assert( sr.getNotes == nil )
    assert( sr.getAnnotation == nil )
    assert_equal true, sr.isModifier
  end

  def test_SpeciesReference_createWith
    sr = LibSBML::SpeciesReference.new("s3",4,2)
    assert( sr.getTypeCode == LibSBML::SBML_SPECIES_REFERENCE )
    assert( sr.getMetaId == "" )
    assert( sr.getNotes == nil )
    assert( sr.getAnnotation == nil )
    assert ((  "s3" == sr.getSpecies ))
    assert( sr.getStoichiometry == 4 )
    assert( sr.getDenominator == 2 )
    assert_equal true, sr.isSetSpecies
  end

  def test_SpeciesReference_free_NULL
  end

  def test_SpeciesReference_setId
    species = "X0"
    @@sr.setId(species)
    assert (( species == @@sr.getId ))
    assert_equal true, @@sr.isSetId
    if (@@sr.getId == species)
    end
    @@sr.setId(@@sr.getId)
    assert (( species == @@sr.getId ))
    @@sr.setId("")
    assert_equal false, @@sr.isSetId
    if (@@sr.getId != nil)
    end
  end

  def test_SpeciesReference_setSpecies
    species = "X0"
    @@sr.setSpecies(species)
    assert (( species == @@sr.getSpecies ))
    assert_equal true, @@sr.isSetSpecies
    if (@@sr.getSpecies == species)
    end
    @@sr.setSpecies(@@sr.getSpecies)
    assert (( species == @@sr.getSpecies ))
    @@sr.setSpecies("")
    assert_equal false, @@sr.isSetSpecies
    if (@@sr.getSpecies != nil)
    end
  end

  def test_SpeciesReference_setStoichiometryMath
    math = LibSBML::parseFormula("k3 / k2")
    stoich = LibSBML::StoichiometryMath.new(math)
    @@sr.setStoichiometryMath(stoich)
    math1 = @@sr.getStoichiometryMath
    assert( math1 != nil )
    formula = LibSBML::formulaToString(math1.getMath)
    assert( formula != nil )
    assert ((  "k3 / k2" == formula ))
    assert_equal true, @@sr.isSetStoichiometryMath
  end

end
