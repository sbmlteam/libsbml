#
# @file    TestReadFromFile5.rb
# @brief   Reads test-data/l2v1-assignment.xml into memory and tests it.
# @author  Akiya Jouraku (Ruby conversion)
# @author  Ben Bornstein 
#
# $Id$
# $Source$
#
# This test file was converted from src/sbml/test/TestReadFromFile5.cpp
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

class TestReadFromFile5 < Test::Unit::TestCase

  def test_read_l2v1_assignment
    reader = LibSBML::SBMLReader.new()
    filename = "../../sbml/test/test-data/l2v1-assignment.xml"
    d = reader.readSBML(filename)
    if (d == nil)
    end
    assert( d.getLevel() == 2 )
    assert( d.getVersion() == 1 )
    m = d.getModel()
    assert( m != nil )
    assert( m.getNumCompartments() == 1 )
    c = m.getCompartment(0)
    assert( c != nil )
    assert( c.getId() == "cell" )
    assert( m.getNumSpecies() == 5 )
    s = m.getSpecies(0)
    assert( s != nil )
    assert( s.getId() == "X0" )
    assert( s.getCompartment() == "cell" )
    assert( s.getInitialConcentration() == 1.0 )
    s = m.getSpecies(1)
    assert( s != nil )
    assert( s.getId() == "X1" )
    assert( s.getCompartment() == "cell" )
    assert( s.getInitialConcentration() == 0.0 )
    s = m.getSpecies(2)
    assert( s != nil )
    assert( s.getId() == "T" )
    assert( s.getCompartment() == "cell" )
    assert( s.getInitialConcentration() == 0.0 )
    s = m.getSpecies(3)
    assert( s != nil )
    assert( s.getId() == "S1" )
    assert( s.getCompartment() == "cell" )
    assert( s.getInitialConcentration() == 0.0 )
    s = m.getSpecies(4)
    assert( s != nil )
    assert( s.getId() == "S2" )
    assert( s.getCompartment() == "cell" )
    assert( s.getInitialConcentration() == 0.0 )
    assert( m.getNumParameters() == 1 )
    p = m.getParameter(0)
    assert( p != nil )
    assert( p.getId() == "Keq" )
    assert( p.getValue() == 2.5 )
    assert( m.getNumRules() == 2 )
    ar = m.getRule(0)
    assert( ar != nil )
    assert( ar.getVariable() == "S1" )
    assert( ar.getFormula() == "T / (1 + Keq)" )
    ar = m.getRule(1)
    assert( ar != nil )
    assert( ar.getVariable() == "S2" )
    assert( ar.getFormula() == "Keq * S1" )
    assert( m.getNumReactions() == 2 )
    r = m.getReaction(0)
    assert( r != nil )
    assert( r.getId() == "in" )
    assert( r.getNumReactants() == 1 )
    assert( r.getNumProducts() == 1 )
    sr = r.getReactant(0)
    assert( sr != nil )
    assert( sr.getSpecies() == "X0" )
    sr = r.getProduct(0)
    assert( sr != nil )
    assert( sr.getSpecies() == "T" )
    kl = r.getKineticLaw()
    assert( kl != nil )
    assert( kl.getFormula() == "k1 * X0" )
    assert( kl.getNumParameters() == 1 )
    p = kl.getParameter(0)
    assert( p != nil )
    assert( p.getId() == "k1" )
    assert( p.getValue() == 0.1 )
    r = m.getReaction(1)
    assert( r != nil )
    assert( r.getId() == "out" )
    assert( r.getNumReactants() == 1 )
    assert( r.getNumProducts() == 1 )
    sr = r.getReactant(0)
    assert( sr != nil )
    assert( sr.getSpecies() == "T" )
    sr = r.getProduct(0)
    assert( sr != nil )
    assert( sr.getSpecies() == "X1" )
    kl = r.getKineticLaw()
    assert( kl != nil )
    assert( kl.getFormula() == "k2 * T" )
    assert( kl.getNumParameters() == 1 )
    p = kl.getParameter(0)
    assert( p != nil )
    assert( p.getId() == "k2" )
    assert( p.getValue() == 0.15 )
  end

end
