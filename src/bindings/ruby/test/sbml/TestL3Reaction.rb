#
# @file    TestL3Reaction.rb
# @brief   L3 Reaction unit tests
#
# @author  Akiya Jouraku (Ruby conversion)
# @author  Sarah Keating 
#
# $Id$
# $HeadURL$
#
# This test file was converted from src/sbml/test/TestL3Reaction.c
# with the help of conversion sciprt (ctest_converter.pl).
#
#<!---------------------------------------------------------------------------
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.
#
# Copyright 2005-2009 California Institute of Technology.
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

class TestL3Reaction < Test::Unit::TestCase

  def setup
    @@r = LibSBML::Reaction.new(3,1)
    if (@@r == nil)
    end
  end

  def teardown
    @@r = nil
  end

  def test_L3_Reaction_compartment
    compartment =  "cell";
    assert_equal false, @@r.isSetCompartment()
    @@r.setCompartment(compartment)
    assert (( compartment == @@r.getCompartment() ))
    assert_equal true, @@r.isSetCompartment()
    if (@@r.getCompartment() == compartment)
    end
    @@r.unsetCompartment()
    assert_equal false, @@r.isSetCompartment()
    if (@@r.getCompartment() != nil)
    end
  end

  def test_L3_Reaction_create
    assert( @@r.getTypeCode() == LibSBML::SBML_REACTION )
    assert( @@r.getMetaId() == "" )
    assert( @@r.getNotes() == nil )
    assert( @@r.getAnnotation() == nil )
    assert( @@r.getId() == "" )
    assert( @@r.getName() == "" )
    assert( @@r.getCompartment() == "" )
    assert( @@r.getFast() == false )
    assert( @@r.getReversible() == true )
    assert_equal false, @@r.isSetId()
    assert_equal false, @@r.isSetName()
    assert_equal false, @@r.isSetCompartment()
    assert_equal false, @@r.isSetFast()
    assert_equal false, @@r.isSetReversible()
  end

  def test_L3_Reaction_createWithNS
    xmlns = LibSBML::XMLNamespaces.new()
    xmlns.add( "http://www.sbml.org", "testsbml")
    sbmlns = LibSBML::SBMLNamespaces.new(3,1)
    sbmlns.addNamespaces(xmlns)
    r = LibSBML::Reaction.new(sbmlns)
    assert( r.getTypeCode() == LibSBML::SBML_REACTION )
    assert( r.getMetaId() == "" )
    assert( r.getNotes() == nil )
    assert( r.getAnnotation() == nil )
    assert( r.getLevel() == 3 )
    assert( r.getVersion() == 1 )
    assert( r.getNamespaces() != nil )
    assert( r.getNamespaces().getLength() == 2 )
    assert( r.getId() == "" )
    assert( r.getName() == "" )
    assert( r.getCompartment() == "" )
    assert( r.getFast() == false )
    assert( r.getReversible() == true )
    assert_equal false, r.isSetId()
    assert_equal false, r.isSetName()
    assert_equal false, r.isSetCompartment()
    assert_equal false, r.isSetFast()
    assert_equal false, r.isSetReversible()
    r = nil
  end

  def test_L3_Reaction_fast
    assert( @@r.isSetFast() == false )
    @@r.setFast(true)
    assert( @@r.getFast() == true )
    assert( @@r.isSetFast() == true )
    @@r.setFast(false)
    assert( @@r.getFast() == false )
    assert( @@r.isSetFast() == true )
  end

  def test_L3_Reaction_free_NULL
  end

  def test_L3_Reaction_hasRequiredAttributes
    r = LibSBML::Reaction.new(3,1)
    assert_equal false, r.hasRequiredAttributes()
    r.setId( "id")
    assert_equal false, r.hasRequiredAttributes()
    r.setFast(false)
    assert_equal false, r.hasRequiredAttributes()
    r.setReversible(false)
    assert_equal true, r.hasRequiredAttributes()
    r = nil
  end

  def test_L3_Reaction_id
    id =  "mitochondria";
    assert_equal false, @@r.isSetId()
    @@r.setId(id)
    assert (( id == @@r.getId() ))
    assert_equal true, @@r.isSetId()
    if (@@r.getId() == id)
    end
  end

  def test_L3_Reaction_name
    name =  "My_Favorite_Factory";
    assert_equal false, @@r.isSetName()
    @@r.setName(name)
    assert (( name == @@r.getName() ))
    assert_equal true, @@r.isSetName()
    if (@@r.getName() == name)
    end
    @@r.unsetName()
    assert_equal false, @@r.isSetName()
    if (@@r.getName() != nil)
    end
  end

  def test_L3_Reaction_reversible
    assert( @@r.isSetReversible() == false )
    @@r.setReversible(true)
    assert( @@r.getReversible() == true )
    assert( @@r.isSetReversible() == true )
    @@r.setReversible(false)
    assert( @@r.getReversible() == false )
    assert( @@r.isSetReversible() == true )
  end

end
