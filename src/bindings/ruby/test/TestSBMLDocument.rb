#
# @file    TestSBMLDocument.rb
# @brief   SBMLDocument unit tests
# @author  Akiya Jouraku (Ruby conversion)
# @author  Ben Bornstein 
#
# $Id$
# $Source$
#
# This test file was converted from src/sbml/test/TestSBMLDocument.c
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

class TestSBMLDocument < Test::Unit::TestCase

  def test_SBMLDocument_create
    d = LibSBML::SBMLDocument.new
    assert( d.getTypeCode == LibSBML::SBML_DOCUMENT )
    assert( d.getNotes == nil )
    assert( d.getAnnotation == nil )
    assert( d.getLevel == 2 )
    assert( d.getVersion == 3 )
    assert( d.getNumErrors == 0 )
    d = nil
  end

  def test_SBMLDocument_createWith
    d = LibSBML::SBMLDocument.new(1,2)
    assert( d.getTypeCode == LibSBML::SBML_DOCUMENT )
    assert( d.getNotes == nil )
    assert( d.getAnnotation == nil )
    assert( d.getLevel == 1 )
    assert( d.getVersion == 2 )
    assert( d.getNumErrors == 0 )
    d = nil
  end

  def test_SBMLDocument_free_NULL
    
  end

  def test_SBMLDocument_setLevelAndVersion
    d = LibSBML::SBMLDocument.new
    d.setLevelAndVersion(2,2)
    m1 = LibSBML::Model.new
    d.setModel(m1)
    assert( d.setLevelAndVersion(2,3) == true )
    assert( d.setLevelAndVersion(2,1) == true )
    assert( d.setLevelAndVersion(1,2) == true )
    assert( d.setLevelAndVersion(1,1) == false )
    d = nil
  end

  def test_SBMLDocument_setLevelAndVersion_Error
    d = LibSBML::SBMLDocument.new
    d.setLevelAndVersion(2,1)
    m1 = LibSBML::Model.new
    u = LibSBML::Unit.new
    u.setKind(LibSBML::UnitKind_forName("mole"))
    u.setOffset(3.2)
    ud = LibSBML::UnitDefinition.new
    ud.addUnit(u)
    m1.addUnitDefinition(ud)
    d.setModel(m1)
    assert( d.setLevelAndVersion(2,2) == false )
    assert( d.setLevelAndVersion(2,3) == false )
    assert( d.setLevelAndVersion(1,2) == false )
    assert( d.setLevelAndVersion(1,1) == false )
    d = nil
  end

  def test_SBMLDocument_setLevelAndVersion_Warning
    d = LibSBML::SBMLDocument.new
    d.setLevelAndVersion(2,2)
    m1 = LibSBML::Model.new
    (m1).setSBOTerm(2)
    d.setModel(m1)
    assert( d.setLevelAndVersion(2,3) == true )
    assert( d.setLevelAndVersion(2,1) == true )
    assert( d.setLevelAndVersion(1,2) == true )
    assert( d.setLevelAndVersion(1,1) == false )
    d = nil
  end

  def test_SBMLDocument_setModel
    d = LibSBML::SBMLDocument.new
    m1 = LibSBML::Model.new
    m2 = LibSBML::Model.new
    assert( d.getModel == nil )
    d.setModel(m1)
    assert( d.getModel != m1 )
    d.setModel(d.getModel)
    assert( d.getModel != m1 )
    d.setModel(m2)
    assert( d.getModel != m2 )
    d = nil
  end

end
