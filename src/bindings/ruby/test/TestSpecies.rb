#
# @file    TestSpecies.rb
# @brief   Species unit tests
# @author  Akiya Jouraku (Ruby conversion)
# @author  Ben Bornstein 
#
# $Id$
# $Source$
#
# This test file was converted from src/sbml/test/TestSpecies.c
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

class TestSpecies < Test::Unit::TestCase

  def setup
    @@s = LibSBML::Species.new
    if (@@s == nil)
    end
  end

  def test_Species_create
    assert( @@s.getTypeCode == LibSBML::SBML_SPECIES )
    assert( @@s.getMetaId == "" )
    assert( @@s.getNotes == nil )
    assert( @@s.getAnnotation == nil )
    assert( @@s.getId == "" )
    assert( @@s.getName == "" )
    assert( @@s.getCompartment == "" )
    assert( @@s.getInitialAmount == 0.0 )
    assert( @@s.getInitialConcentration == 0.0 )
    assert( @@s.getSubstanceUnits == "" )
    assert( @@s.getSpatialSizeUnits == "" )
    assert( @@s.getHasOnlySubstanceUnits == false )
    assert( @@s.getBoundaryCondition == false )
    assert( @@s.getCharge == 0 )
    assert( @@s.getConstant == false )
    assert_equal false, @@s.isSetId
    assert_equal false, @@s.isSetName
    assert_equal false, @@s.isSetCompartment
    assert_equal false, @@s.isSetInitialAmount
    assert_equal false, @@s.isSetInitialConcentration
    assert_equal false, @@s.isSetSubstanceUnits
    assert_equal false, @@s.isSetSpatialSizeUnits
    assert_equal false, @@s.isSetUnits
    assert_equal false, @@s.isSetCharge
  end

  def test_Species_createWith
    s = LibSBML::Species.new("Ca", "Calcium")
    assert( s.getTypeCode == LibSBML::SBML_SPECIES )
    assert( s.getMetaId == "" )
    assert( s.getNotes == nil )
    assert( s.getAnnotation == nil )
    assert ((  "Calcium"   == s.getName ))
    assert( s.getSpatialSizeUnits == "" )
    assert( s.getHasOnlySubstanceUnits == false )
    assert( s.getConstant == false )
    assert ((  "Ca"   == s.getId ))
    assert_equal true, s.isSetId
    assert_equal true, s.isSetName
    assert_equal false, s.isSetCompartment
    assert_equal false, s.isSetSubstanceUnits
    assert_equal false, s.isSetSpatialSizeUnits
    assert_equal false, s.isSetUnits
    assert_equal false, s.isSetInitialAmount
    assert_equal false, s.isSetInitialConcentration
    assert_equal false, s.isSetCharge
  end

  def test_Species_free_NULL
  end

  def test_Species_setCompartment
    compartment = "cell"
    @@s.setCompartment(compartment)
    assert (( compartment == @@s.getCompartment ))
    assert_equal true, @@s.isSetCompartment
    if (@@s.getCompartment == compartment)
    end
    @@s.setCompartment(@@s.getCompartment)
    assert (( compartment == @@s.getCompartment ))
    @@s.setCompartment("")
    assert_equal false, @@s.isSetCompartment
    if (@@s.getCompartment != nil)
    end
  end

  def test_Species_setId
    id = "Glucose"
    @@s.setId(id)
    assert (( id == @@s.getId ))
    assert_equal true, @@s.isSetId
    if (@@s.getId == id)
    end
    @@s.setId(@@s.getId)
    assert (( id == @@s.getId ))
    @@s.setId("")
    assert_equal false, @@s.isSetId
    if (@@s.getId != nil)
    end
  end

  def test_Species_setInitialAmount
    assert_equal false, @@s.isSetInitialAmount
    assert_equal false, @@s.isSetInitialConcentration
    @@s.setInitialAmount(1.2)
    assert_equal true, @@s.isSetInitialAmount
    assert_equal false, @@s.isSetInitialConcentration
    assert( @@s.getInitialAmount == 1.2 )
  end

  def test_Species_setInitialConcentration
    assert_equal false, @@s.isSetInitialAmount
    assert_equal false, @@s.isSetInitialConcentration
    @@s.setInitialConcentration(3.4)
    assert_equal false, @@s.isSetInitialAmount
    assert_equal true, @@s.isSetInitialConcentration
    assert( @@s.getInitialConcentration == 3.4 )
  end

  def test_Species_setName
    name = "So Sweet"
    @@s.setName(name)
    assert (( name == @@s.getName ))
    assert_equal true, @@s.isSetName
    if (@@s.getName == name)
    end
    @@s.setName(@@s.getName)
    assert (( name == @@s.getName ))
    @@s.setName("")
    assert_equal false, @@s.isSetName
    if (@@s.getName != nil)
    end
  end

  def test_Species_setSpatialSizeUnits
    units = "volume"
    @@s.setSpatialSizeUnits(units)
    assert (( units == @@s.getSpatialSizeUnits ))
    assert_equal true, @@s.isSetSpatialSizeUnits
    if (@@s.getSpatialSizeUnits == units)
    end
    @@s.setSpatialSizeUnits(@@s.getSpatialSizeUnits)
    assert (( units == @@s.getSpatialSizeUnits ))
    @@s.setSpatialSizeUnits("")
    assert_equal false, @@s.isSetSpatialSizeUnits
    if (@@s.getSpatialSizeUnits != nil)
    end
  end

  def test_Species_setSubstanceUnits
    units = "item"
    @@s.setSubstanceUnits(units)
    assert (( units == @@s.getSubstanceUnits ))
    assert_equal true, @@s.isSetSubstanceUnits
    if (@@s.getSubstanceUnits == units)
    end
    @@s.setSubstanceUnits(@@s.getSubstanceUnits)
    assert (( units == @@s.getSubstanceUnits ))
    @@s.setSubstanceUnits("")
    assert_equal false, @@s.isSetSubstanceUnits
    if (@@s.getSubstanceUnits != nil)
    end
  end

  def test_Species_setUnits
    units = "mole"
    @@s.setUnits(units)
    assert (( units == @@s.getUnits ))
    assert_equal true, @@s.isSetUnits
    if (@@s.getSubstanceUnits == units)
    end
    @@s.setUnits(@@s.getSubstanceUnits)
    assert (( units == @@s.getUnits ))
    @@s.setUnits("")
    assert_equal false, @@s.isSetUnits
    if (@@s.getSubstanceUnits != nil)
    end
  end

end
