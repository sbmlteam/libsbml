#
# @file    TestSpeciesConcentrationRule.rb
# @brief   SpeciesConcentrationRule unit tests
# @author  Akiya Jouraku (Ruby conversion)
# @author  Ben Bornstein 
#
# $Id$
# $Source$
#
# This test file was converted from src/sbml/test/TestSpeciesConcentrationRule.c
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

class TestSpeciesConcentrationRule < Test::Unit::TestCase

  def setup
    @@scr = LibSBML::AssignmentRule.new()
    @@scr.setL1TypeCode(LibSBML::SBML_SPECIES_CONCENTRATION_RULE)
    if (@@scr == nil)
    end
  end

  def teardown
    @@scr = nil
  end

  def test_SpeciesConcentrationRule_create
    assert( @@scr.getTypeCode == LibSBML::SBML_ASSIGNMENT_RULE )
    assert( @@scr.getL1TypeCode == LibSBML::SBML_SPECIES_CONCENTRATION_RULE )
    assert( @@scr.getNotes == nil )
    assert( @@scr.getAnnotation == nil )
    assert( @@scr.getFormula == "" )
    assert( @@scr.getType == LibSBML::RULE_TYPE_SCALAR )
    assert( @@scr.getVariable == "" )
    assert_equal false, @@scr.isSetVariable
  end

  def test_SpeciesConcentrationRule_createWith
    scr = LibSBML::RateRule.new("c", "v + 1")
    scr.setL1TypeCode(LibSBML::SBML_SPECIES_CONCENTRATION_RULE)
    assert( scr.getTypeCode == LibSBML::SBML_RATE_RULE )
    assert( scr.getL1TypeCode == LibSBML::SBML_SPECIES_CONCENTRATION_RULE )
    assert( scr.getNotes == nil )
    assert( scr.getAnnotation == nil )
    assert ((  "v + 1" == scr.getFormula ))
    assert ((  "c" == scr.getVariable ))
    assert( scr.getType == LibSBML::RULE_TYPE_RATE )
    assert_equal true, scr.isSetVariable
    scr = nil
  end

  def test_SpeciesConcentrationRule_free_NULL
    
  end

  def test_SpeciesConcentrationRule_setSpecies
    species = "s2"
    @@scr.setVariable(species)
    assert (( species == @@scr.getVariable ))
    assert_equal true, @@scr.isSetVariable
    if (@@scr.getVariable == species)
    end
    s = @@scr.getVariable
    @@scr.setVariable(s)
    assert (( species == @@scr.getVariable ))
    @@scr.setVariable("")
    assert_equal false, @@scr.isSetVariable
    if (@@scr.getVariable != nil)
    end
  end

end
