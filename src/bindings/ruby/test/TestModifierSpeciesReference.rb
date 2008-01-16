#
# @file    TestModifierSpeciesReference.rb
# @brief   ModifierSpeciesReference unit tests
# @author  Akiya Jouraku (Ruby conversion)
# @author  Ben Bornstein 
#
# $Id$
# $Source$
#
# This test file was converted from src/sbml/test/TestModifierSpeciesReference.c
# with the help of conversion sciprt (ctest_converter.pl).
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

class TestModifierSpeciesReference < Test::Unit::TestCase

  def setup
    @@msr = LibSBML::ModifierSpeciesReference.new()
    if (@@msr == nil)
    end
  end

  def teardown
    @@msr = nil
  end

  def test_ModifierSpeciesReference_create
    assert( @@msr.getTypeCode == LibSBML::SBML_MODIFIER_SPECIES_REFERENCE )
    assert( @@msr.getMetaId == "" )
    assert( @@msr.getNotes == nil )
    assert( @@msr.getAnnotation == nil )
    assert( @@msr.getSpecies == "" )
    assert_equal false, @@msr.isSetSpecies
    assert_equal true, @@msr.isModifier
  end

  def test_ModifierSpeciesReference_free_NULL
    
  end

  def test_ModifierSpeciesReference_setSpecies
    species = "s1"
    @@msr.setSpecies(species)
    s = @@msr.getSpecies
    assert (( species == s ))
    assert_equal true, @@msr.isSetSpecies
    if (@@msr.getSpecies == species)
    end
    s = @@msr.getSpecies
    @@msr.setSpecies(s)
    s = @@msr.getSpecies
    assert (( species == s ))
    @@msr.setSpecies("")
    assert_equal false, @@msr.isSetSpecies
    if (@@msr.getSpecies != nil)
    end
  end

end
