#
# @file    TestParameter.rb
# @brief   Parameter unit tests
# @author  Akiya Jouraku (Ruby conversion)
# @author  Ben Bornstein 
#
# $Id$
# $Source$
#
# This test file was converted from src/sbml/test/TestParameter.c
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

class TestParameter < Test::Unit::TestCase

  def setup
    @@p = LibSBML::Parameter.new
    if (@@p == nil)
    end
  end

  def teardown
    @@p = nil
  end

  def test_Parameter_create
    assert( @@p.getTypeCode == LibSBML::SBML_PARAMETER )
    assert( @@p.getMetaId == "" )
    assert( @@p.getNotes == nil )
    assert( @@p.getAnnotation == nil )
    assert( @@p.getId == "" )
    assert( @@p.getName == "" )
    assert( @@p.getUnits == "" )
    assert( @@p.getConstant == true )
    assert_equal false, @@p.isSetId
    assert_equal false, @@p.isSetName
    assert_equal false, @@p.isSetValue
    assert_equal false, @@p.isSetUnits
  end

  def test_Parameter_createWith
    p = LibSBML::Parameter.new("delay",6.2, "second")
    assert( p.getTypeCode == LibSBML::SBML_PARAMETER )
    assert( p.getMetaId == "" )
    assert( p.getNotes == nil )
    assert( p.getAnnotation == nil )
    assert ((  "delay"  == p.getId ))
    assert ((  "second" == p.getUnits ))
    assert( p.getName == "" )
    assert( p.getValue == 6.2 )
    assert( p.getConstant == true )
    assert_equal true, p.isSetId
    assert_equal false, p.isSetName
    assert_equal true, p.isSetValue
    assert_equal true, p.isSetUnits
    p = nil
  end

  def test_Parameter_free_NULL
    
  end

  def test_Parameter_setId
    id = "Km1"
    @@p.setId(id)
    assert (( id == @@p.getId ))
    assert_equal true, @@p.isSetId
    if (@@p.getId == id)
    end
    @@p.setId(@@p.getId)
    assert (( id == @@p.getId ))
    @@p.setId("")
    assert_equal false, @@p.isSetId
    if (@@p.getId != nil)
    end
  end

  def test_Parameter_setName
    name = "Forward Michaelis-Menten Constant"
    @@p.setName(name)
    assert (( name == @@p.getName ))
    assert_equal true, @@p.isSetName
    if (@@p.getName == name)
    end
    @@p.setName(@@p.getName)
    assert (( name == @@p.getName ))
    @@p.setName("")
    assert_equal false, @@p.isSetName
    if (@@p.getName != nil)
    end
  end

  def test_Parameter_setUnits
    units = "second"
    @@p.setUnits(units)
    assert (( units == @@p.getUnits ))
    assert_equal true, @@p.isSetUnits
    if (@@p.getUnits == units)
    end
    @@p.setUnits(@@p.getUnits)
    assert (( units == @@p.getUnits ))
    @@p.setUnits("")
    assert_equal false, @@p.isSetUnits
    if (@@p.getUnits != nil)
    end
  end

end
