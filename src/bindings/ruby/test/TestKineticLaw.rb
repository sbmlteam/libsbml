#
# @file    TestKineticLaw.rb
# @brief   SBML KineticLaw unit tests
# @author  Akiya Jouraku (Ruby conversion)
# @author  Ben Bornstein 
#
# $Id$
# $Source$
#
# This test file was converted from src/sbml/test/TestKineticLaw.c
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

class TestKineticLaw < Test::Unit::TestCase

  def setup
    @@kl = LibSBML::KineticLaw.new
    if (@@kl == nil)
    end
  end

  def teardown
    @@kl = nil
  end

  def test_KineticLaw_addParameter
    p = LibSBML::Parameter.new
    @@kl.addParameter(p)
    assert( @@kl.getNumParameters == 1 )
    p = nil
  end

  def test_KineticLaw_create
    assert( @@kl.getTypeCode == LibSBML::SBML_KINETIC_LAW )
    assert( @@kl.getMetaId == "" )
    assert( @@kl.getNotes == nil )
    assert( @@kl.getAnnotation == nil )
    assert( @@kl.getFormula == "" )
    assert( @@kl.getMath == nil )
    assert( @@kl.getTimeUnits == "" )
    assert( @@kl.getSubstanceUnits == "" )
    assert_equal false, @@kl.isSetFormula
    assert_equal false, @@kl.isSetMath
    assert_equal false, @@kl.isSetTimeUnits
    assert_equal false, @@kl.isSetSubstanceUnits
    assert( @@kl.getNumParameters == 0 )
  end

  def test_KineticLaw_createWith
    kl = LibSBML::KineticLaw.new("k1 * X0")
    assert( kl.getTypeCode == LibSBML::SBML_KINETIC_LAW )
    assert( kl.getMetaId == "" )
    assert( kl.getNotes == nil )
    assert( kl.getAnnotation == nil )
    math = kl.getMath
    assert( math != nil )
    formula = LibSBML::formulaToString(math)
    assert( formula != nil )
    assert ((  "k1 * X0" == formula ))
    assert (( formula == kl.getFormula ))
    assert_equal true, kl.isSetMath
    assert_equal true, kl.isSetFormula
    assert( kl.getNumParameters == 0 )
    kl = nil
  end

  def test_KineticLaw_createWithMath
    math1 = LibSBML::parseFormula("k3 / k2")
    kl = LibSBML::KineticLaw.new(math1)
    assert( kl.getTypeCode == LibSBML::SBML_KINETIC_LAW )
    assert( kl.getMetaId == "" )
    assert( kl.getNotes == nil )
    assert( kl.getAnnotation == nil )
    math = kl.getMath
    assert( math != nil )
    formula = LibSBML::formulaToString(math)
    assert( formula != nil )
    assert ((  "k3 / k2" == formula ))
    assert (( formula == kl.getFormula ))
    assert_equal true, kl.isSetMath
    assert_equal true, kl.isSetFormula
    assert_equal false, kl.isSetTimeUnits
    assert_equal false, kl.isSetSubstanceUnits
    assert( kl.getNumParameters == 0 )
    kl = nil
  end

  def test_KineticLaw_free_NULL
    
  end

  def test_KineticLaw_getParameter
    k1 = LibSBML::Parameter.new
    k2 = LibSBML::Parameter.new
    k1.setName( "k1")
    k2.setName( "k2")
    k1.setValue(3.14)
    k2.setValue(2.72)
    @@kl.addParameter(k1)
    @@kl.addParameter(k2)
    k1 = nil
    k2 = nil
    assert( @@kl.getNumParameters == 2 )
    k1 = @@kl.getParameter(0)
    k2 = @@kl.getParameter(1)
    assert ((  "k1" == k1.getName ))
    assert ((  "k2" == k2.getName ))
    assert( k1.getValue == 3.14 )
    assert( k2.getValue == 2.72 )
  end

  def test_KineticLaw_getParameterById
    k1 = LibSBML::Parameter.new
    k2 = LibSBML::Parameter.new
    k1.setId( "k1")
    k2.setId( "k2")
    k1.setValue(3.14)
    k2.setValue(2.72)
    @@kl.addParameter(k1)
    @@kl.addParameter(k2)
    k1 = nil
    k2 = nil
    assert( @@kl.getNumParameters == 2 )
    k1 = @@kl.getParameter( "k1")
    k2 = @@kl.getParameter( "k2")
    assert ((  "k1" == k1.getId ))
    assert ((  "k2" == k2.getId ))
    assert( k1.getValue == 3.14 )
    assert( k2.getValue == 2.72 )
  end

  def test_KineticLaw_setFormula
    formula = "k1*X0"
    @@kl.setFormula(formula)
    assert (( formula == @@kl.getFormula ))
    assert_equal true, @@kl.isSetFormula
    if (@@kl.getFormula == formula)
    end
    @@kl.setFormula(@@kl.getFormula)
    assert (( formula == @@kl.getFormula ))
    @@kl.setFormula("")
    assert_equal false, @@kl.isSetFormula
    if (@@kl.getFormula != nil)
    end
  end

  def test_KineticLaw_setFormulaFromMath
    math = LibSBML::parseFormula("k1 * X0")
    assert_equal false, @@kl.isSetMath
    assert_equal false, @@kl.isSetFormula
    @@kl.setMath(math)
    assert_equal true, @@kl.isSetMath
    assert_equal true, @@kl.isSetFormula
    assert ((  "k1 * X0" == @@kl.getFormula ))
    math = nil
  end

  def test_KineticLaw_setMath
    math = LibSBML::parseFormula("k3 / k2")
    @@kl.setMath(math)
    math1 = @@kl.getMath
    assert( math1 != nil )
    formula = LibSBML::formulaToString(math1)
    assert( formula != nil )
    assert ((  "k3 / k2" == formula ))
    assert( @@kl.getMath != math )
    assert_equal true, @@kl.isSetMath
    @@kl.setMath(@@kl.getMath)
    math1 = @@kl.getMath
    assert( math1 != nil )
    formula = LibSBML::formulaToString(math1)
    assert( formula != nil )
    assert ((  "k3 / k2" == formula ))
    assert( @@kl.getMath != math )
    @@kl.setMath(nil)
    assert_equal false, @@kl.isSetMath
    if (@@kl.getMath != nil)
    end
    math = nil
  end

  def test_KineticLaw_setMathFromFormula
    formula = "k3 / k2"
    assert_equal false, @@kl.isSetMath
    assert_equal false, @@kl.isSetFormula
    @@kl.setFormula(formula)
    assert_equal true, @@kl.isSetMath
    assert_equal true, @@kl.isSetFormula
    formula = LibSBML::formulaToString(@@kl.getMath)
    assert ((  "k3 / k2" == formula ))
  end

end
