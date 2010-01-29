#
# @file    TestCVTerms.rb
# @brief   CVTerms unit tests
#
# @author  Akiya Jouraku (Ruby conversion)
# @author  Sarah Keating 
#
# $Id$
# $HeadURL$
#
# This test file was converted from src/sbml/test/TestCVTerms.c
# with the help of conversion sciprt (ctest_converter.pl).
#
#<!---------------------------------------------------------------------------
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.
#
# Copyright 2005-2010 California Institute of Technology.
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

class TestCVTerms < Test::Unit::TestCase

  def test_CVTerm_addResource
    term = LibSBML::CVTerm.new(LibSBML::MODEL_QUALIFIER)
    resource =  "GO6666";
    assert( term != nil )
    assert( term.getQualifierType() == LibSBML::MODEL_QUALIFIER )
    term.addResource(resource)
    xa = term.getResources()
    assert( xa.getLength() == 1 )
    assert ((  "rdf:resource" == xa.getName(0) ))
    assert ((  "GO6666" == xa.getValue(0) ))
    term = nil
  end

  def test_CVTerm_create
    term = LibSBML::CVTerm.new(LibSBML::MODEL_QUALIFIER)
    assert( term != nil )
    assert( term.getQualifierType() == LibSBML::MODEL_QUALIFIER )
    term = nil
  end

  def test_CVTerm_createFromNode
    qual_triple = LibSBML::XMLTriple.new("is", "", "bqbiol")
    bag_triple = LibSBML::XMLTriple.new()
    li_triple = LibSBML::XMLTriple.new()
    att = LibSBML::XMLAttributes.new()
    att.add( "", "This is my resource")
    att1 = LibSBML::XMLAttributes.new()
    li_token = LibSBML::XMLToken.new(li_triple,att)
    bag_token = LibSBML::XMLToken.new(bag_triple,att1)
    qual_token = LibSBML::XMLToken.new(qual_triple,att1)
    li = LibSBML::XMLNode.new(li_token)
    bag = LibSBML::XMLNode.new(bag_token)
    node = LibSBML::XMLNode.new(qual_token)
    bag.addChild(li)
    node.addChild(bag)
    term = LibSBML::CVTerm.new(node)
    assert( term != nil )
    assert( term.getQualifierType() == LibSBML::BIOLOGICAL_QUALIFIER )
    assert( term.getBiologicalQualifierType() == LibSBML::BQB_IS )
    xa = term.getResources()
    assert( xa.getLength() == 1 )
    assert ((  "rdf:resource" == xa.getName(0) ))
    assert ((  "This is my resource" == xa.getValue(0) ))
    qual_triple = nil
    bag_triple = nil
    li_triple = nil
    li_token = nil
    bag_token = nil
    qual_token = nil
    att = nil
    att1 = nil
    term = nil
    node = nil
    bag = nil
    li = nil
  end

  def test_CVTerm_getResources
    term = LibSBML::CVTerm.new(LibSBML::MODEL_QUALIFIER)
    resource =  "GO6666";
    resource1 =  "OtherURI";
    term.addResource(resource)
    term.addResource(resource1)
    number = term.getNumResources()
    assert( number == 2 )
    assert ((  "GO6666" == term.getResourceURI(0) ))
    assert ((  "OtherURI" == term.getResourceURI(1) ))
    term = nil
  end

  def test_CVTerm_set_get
    term = LibSBML::CVTerm.new(LibSBML::MODEL_QUALIFIER)
    assert( term != nil )
    assert( term.getQualifierType() == LibSBML::MODEL_QUALIFIER )
    term.setModelQualifierType(LibSBML::BQM_IS)
    assert( term != nil )
    assert( term.getQualifierType() == LibSBML::MODEL_QUALIFIER )
    assert( term.getModelQualifierType() == LibSBML::BQM_IS )
    term.setQualifierType(LibSBML::BIOLOGICAL_QUALIFIER)
    term.setBiologicalQualifierType(LibSBML::BQB_IS)
    assert( term.getQualifierType() == LibSBML::BIOLOGICAL_QUALIFIER )
    assert( term.getBiologicalQualifierType() == LibSBML::BQB_IS )
    term = nil
  end

end
