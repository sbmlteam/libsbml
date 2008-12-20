#
# @file    TestListOf.rb
# @brief   ListOf unit tests
#
# @author  Akiya Jouraku (Ruby conversion)
# @author  Ben Bornstein 
#
# $Id:$
# $HeadURL:$
#
# This test file was converted from src/sbml/test/TestListOf.c
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
require 'test/unit'
require 'libSBML'

class TestListOf < Test::Unit::TestCase

  def test_ListOf_clear
    lo = LibSBML::ListOf.new()
    sp = LibSBML::Species.new()
    lo.append(sp)
    lo.append(sp)
    lo.append(sp)
    lo.append(sp)
    lo.append(sp)
    assert( lo.size() == 5 )
    lo.clear(1)
    assert( lo.size() == 0 )
    lo.append(sp)
    lo.append(sp)
    lo.append(sp)
    lo.append(sp)
    lo.appendAndOwn(sp)
    assert( lo.size() == 5 )
    lo.get(0)
    lo.get(1)
    lo.get(2)
    lo.get(3)
    lo.get(4)
    lo.clear(0)
    assert( lo.size() == 0 )
    lo = nil
  end

  def test_ListOf_create
    lo = LibSBML::ListOf.new()
    assert( lo.getTypeCode() == LibSBML::SBML_LIST_OF )
    assert( lo.getNotes() == nil )
    assert( lo.getAnnotation() == nil )
    assert( lo.getMetaId() == "" )
    assert( lo.size() == 0 )
    lo = nil
  end

  def test_ListOf_free_NULL
  end

  def test_ListOf_remove
    lo = LibSBML::ListOf.new()
    sp = LibSBML::Species.new()
    assert( lo.size() == 0 )
    lo.append(sp)
    lo.append(sp)
    lo.append(sp)
    lo.append(sp)
    lo.append(sp)
    assert( lo.size() == 5 )
    lo.remove(0)
    lo.remove(0)
    lo.remove(0)
    lo.remove(0)
    lo.remove(0)
    assert( lo.size() == 0 )
    lo.append(sp)
    lo.append(sp)
    lo.append(sp)
    lo.append(sp)
    lo.appendAndOwn(sp)
    assert( lo.size() == 5 )
    lo = nil
  end

end
