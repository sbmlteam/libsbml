#
# @file    TestDate_newSetters.rb
# @brief   Date unit tests
#
# @author  Akiya Jouraku (Ruby conversion)
# @author  Sarah Keating 
#
# $Id$
# $HeadURL$
#
# This test file was converted from src/sbml/test/TestDate_newSetters.c
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

class TestDate_newSetters < Test::Unit::TestCase

  def test_Date_setDateAsString
    date = LibSBML::Date.new(2007,10,23,14,15,16,1,3,0)
    assert( date != nil )
    i = date.setDateAsString( "20081-12-30T12:15:45+02:00")
    assert( i == LibSBML::LIBSBML_INVALID_ATTRIBUTE_VALUE )
    assert (("2007-10-23T14:15:16+03:00" == date.getDateAsString() ))
    i = date.setDateAsString( "200-12-30T12:15:45+02:00")
    assert( i == LibSBML::LIBSBML_INVALID_ATTRIBUTE_VALUE )
    assert (("2007-10-23T14:15:16+03:00" == date.getDateAsString() ))
    i = date.setDateAsString("")
    assert( i == LibSBML::LIBSBML_OPERATION_SUCCESS )
    assert (("2000-01-01T00:00:00Z" == date.getDateAsString() ))
    i = date.setDateAsString( "2008-12-30T12:15:45+02:00")
    assert( i == LibSBML::LIBSBML_OPERATION_SUCCESS )
    assert( date.getYear() == 2008 )
    assert( date.getMonth() == 12 )
    assert( date.getDay() == 30 )
    assert( date.getHour() == 12 )
    assert( date.getMinute() == 15 )
    assert( date.getSecond() == 45 )
    assert( date.getSignOffset() == 1 )
    assert( date.getHoursOffset() == 2 )
    assert( date.getMinutesOffset() == 0 )
    date = nil
  end

  def test_Date_setDay
    date = LibSBML::Date.new(2005,2,12,12,15,45,1,2,0)
    assert( date != nil )
    i = date.setDay(29)
    assert( i == LibSBML::LIBSBML_INVALID_ATTRIBUTE_VALUE )
    assert( date.getDay() == 1 )
    i = date.setDay(31)
    assert( i == LibSBML::LIBSBML_INVALID_ATTRIBUTE_VALUE )
    assert( date.getDay() == 1 )
    i = date.setDay(15)
    assert( i == LibSBML::LIBSBML_OPERATION_SUCCESS )
    assert( date.getDay() == 15 )
    assert (("2005-02-15T12:15:45+02:00" == date.getDateAsString() ))
    date = nil
  end

  def test_Date_setHour
    date = LibSBML::Date.new(2005,12,30,12,15,45,1,2,0)
    assert( date != nil )
    i = date.setHour(434)
    assert( i == LibSBML::LIBSBML_INVALID_ATTRIBUTE_VALUE )
    assert( date.getHour() == 0 )
    i = date.setHour(12121)
    assert( i == LibSBML::LIBSBML_INVALID_ATTRIBUTE_VALUE )
    assert( date.getHour() == 0 )
    i = date.setHour(9)
    assert( i == LibSBML::LIBSBML_OPERATION_SUCCESS )
    assert( date.getHour() == 9 )
    assert (("2005-12-30T09:15:45+02:00" == date.getDateAsString() ))
    date = nil
  end

  def test_Date_setHoursOffset
    date = LibSBML::Date.new(2005,12,30,12,15,45,1,2,0)
    assert( date != nil )
    i = date.setHoursOffset(434)
    assert( i == LibSBML::LIBSBML_INVALID_ATTRIBUTE_VALUE )
    assert( date.getHoursOffset() == 0 )
    i = date.setHoursOffset(13)
    assert( i == LibSBML::LIBSBML_INVALID_ATTRIBUTE_VALUE )
    assert( date.getHoursOffset() == 0 )
    i = date.setHoursOffset(11)
    assert( i == LibSBML::LIBSBML_OPERATION_SUCCESS )
    assert( date.getHoursOffset() == 11 )
    assert (("2005-12-30T12:15:45+11:00" == date.getDateAsString() ))
    date = nil
  end

  def test_Date_setMinute
    date = LibSBML::Date.new(2005,12,30,12,15,45,1,2,0)
    assert( date != nil )
    i = date.setMinute(434)
    assert( i == LibSBML::LIBSBML_INVALID_ATTRIBUTE_VALUE )
    assert( date.getMinute() == 0 )
    i = date.setMinute(12121)
    assert( i == LibSBML::LIBSBML_INVALID_ATTRIBUTE_VALUE )
    assert( date.getMinute() == 0 )
    i = date.setMinute(32)
    assert( i == LibSBML::LIBSBML_OPERATION_SUCCESS )
    assert( date.getMinute() == 32 )
    assert (("2005-12-30T12:32:45+02:00" == date.getDateAsString() ))
    date = nil
  end

  def test_Date_setMinutesOffset
    date = LibSBML::Date.new(2005,12,30,12,15,45,1,2,0)
    assert( date != nil )
    i = date.setMinutesOffset(434)
    assert( i == LibSBML::LIBSBML_INVALID_ATTRIBUTE_VALUE )
    assert( date.getMinutesOffset() == 0 )
    i = date.setMinutesOffset(60)
    assert( i == LibSBML::LIBSBML_INVALID_ATTRIBUTE_VALUE )
    assert( date.getMinutesOffset() == 0 )
    i = date.setMinutesOffset(45)
    assert( i == LibSBML::LIBSBML_OPERATION_SUCCESS )
    assert( date.getMinutesOffset() == 45 )
    assert (("2005-12-30T12:15:45+02:45" == date.getDateAsString() ))
    date = nil
  end

  def test_Date_setMonth
    date = LibSBML::Date.new(2005,12,30,12,15,45,1,2,0)
    assert( date != nil )
    i = date.setMonth(434)
    assert( i == LibSBML::LIBSBML_INVALID_ATTRIBUTE_VALUE )
    assert( date.getMonth() == 1 )
    i = date.setMonth(12121)
    assert( i == LibSBML::LIBSBML_INVALID_ATTRIBUTE_VALUE )
    assert( date.getMonth() == 1 )
    i = date.setMonth(11)
    assert( i == LibSBML::LIBSBML_OPERATION_SUCCESS )
    assert( date.getMonth() == 11 )
    assert (("2005-11-30T12:15:45+02:00" == date.getDateAsString() ))
    date = nil
  end

  def test_Date_setOffsetSign
    date = LibSBML::Date.new(2005,12,30,12,15,45,1,2,0)
    assert( date != nil )
    i = date.setSignOffset(2)
    assert( i == LibSBML::LIBSBML_INVALID_ATTRIBUTE_VALUE )
    assert( date.getSignOffset() == 0 )
    i = date.setSignOffset(4)
    assert( i == LibSBML::LIBSBML_INVALID_ATTRIBUTE_VALUE )
    assert( date.getSignOffset() == 0 )
    i = date.setSignOffset(0)
    assert( i == LibSBML::LIBSBML_OPERATION_SUCCESS )
    assert( date.getSignOffset() == 0 )
    assert (("2005-12-30T12:15:45-02:00" == date.getDateAsString() ))
    date = nil
  end

  def test_Date_setSecond
    date = LibSBML::Date.new(2005,12,30,12,15,45,1,2,0)
    assert( date != nil )
    i = date.setSecond(434)
    assert( i == LibSBML::LIBSBML_INVALID_ATTRIBUTE_VALUE )
    assert( date.getSecond() == 0 )
    i = date.setSecond(12121)
    assert( i == LibSBML::LIBSBML_INVALID_ATTRIBUTE_VALUE )
    assert( date.getSecond() == 0 )
    i = date.setSecond(32)
    assert( i == LibSBML::LIBSBML_OPERATION_SUCCESS )
    assert( date.getSecond() == 32 )
    assert (("2005-12-30T12:15:32+02:00" == date.getDateAsString() ))
    date = nil
  end

  def test_Date_setYear
    date = LibSBML::Date.new(2005,12,30,12,15,45,1,2,0)
    assert( date != nil )
    i = date.setYear(434)
    assert( i == LibSBML::LIBSBML_INVALID_ATTRIBUTE_VALUE )
    assert( date.getYear() == 2000 )
    i = date.setYear(12121)
    assert( i == LibSBML::LIBSBML_INVALID_ATTRIBUTE_VALUE )
    assert( date.getYear() == 2000 )
    i = date.setYear(2008)
    assert( i == LibSBML::LIBSBML_OPERATION_SUCCESS )
    assert( date.getYear() == 2008 )
    assert (("2008-12-30T12:15:45+02:00" == date.getDateAsString() ))
    date = nil
  end

end
