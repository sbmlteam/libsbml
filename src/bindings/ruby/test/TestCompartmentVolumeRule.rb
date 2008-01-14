#
# @file    TestCompartmentVolumeRule.rb
# @brief   CompartmentVolumeRule unit tests
# @author  Akiya Jouraku (Ruby conversion)
# @author  Ben Bornstein 
#
# $Id$
# $Source$
#
# This test file was converted from src/sbml/test/TestCompartmentVolumeRule.c
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

class TestCompartmentVolumeRule < Test::Unit::TestCase

  def setup
    @@cvr = LibSBML::AssignmentRule.new()
    @@cvr.setL1TypeCode(LibSBML::SBML_COMPARTMENT_VOLUME_RULE)
    if (@@cvr == nil)
    end
  end

  def test_CompartmentVolumeRule_create
    assert( @@cvr.getTypeCode == LibSBML::SBML_ASSIGNMENT_RULE )
    assert( @@cvr.getL1TypeCode == LibSBML::SBML_COMPARTMENT_VOLUME_RULE )
    assert( @@cvr.getNotes == nil )
    assert( @@cvr.getAnnotation == nil )
    assert( @@cvr.getFormula == "" )
    assert( @@cvr.getType == LibSBML::RULE_TYPE_SCALAR )
    assert( @@cvr.getVariable == "" )
    assert_equal false, @@cvr.isSetVariable
  end

  def test_CompartmentVolumeRule_createWith
    cvr = LibSBML::RateRule.new("c", "v + 1")
    cvr.setL1TypeCode(LibSBML::SBML_COMPARTMENT_VOLUME_RULE)
    assert( cvr.getTypeCode == LibSBML::SBML_RATE_RULE )
    assert( cvr.getL1TypeCode == LibSBML::SBML_COMPARTMENT_VOLUME_RULE )
    assert( cvr.getNotes == nil )
    assert( cvr.getAnnotation == nil )
    assert ((  "v + 1" == cvr.getFormula ))
    assert ((  "c" == cvr.getVariable ))
    assert( cvr.getType == LibSBML::RULE_TYPE_RATE )
    assert_equal true, cvr.isSetVariable
  end

  def test_CompartmentVolumeRule_free_NULL
  end

  def test_CompartmentVolumeRule_setCompartment
    compartment = "cell"
    @@cvr.setVariable(compartment)
    assert (( compartment == @@cvr.getVariable ))
    assert_equal true, @@cvr.isSetVariable
    if (@@cvr.getVariable == compartment)
    end
    c = @@cvr.getVariable
    @@cvr.setVariable(c)
    assert (( compartment == @@cvr.getVariable ))
    @@cvr.setVariable("")
    assert_equal false, @@cvr.isSetVariable
    if (@@cvr.getVariable != nil)
    end
  end

end
