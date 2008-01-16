#
# @file    TestSBase.rb
# @brief   SBase unit tests
# @author  Akiya Jouraku (Ruby conversion)
# @author  Ben Bornstein 
#
# $Id$
# $Source$
#
# This test file was converted from src/sbml/test/TestSBase.cpp
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

class TestSBase < Test::Unit::TestCase

  def setup
    @@s = LibSBML::Model.new()
    if (@@s == nil)
    end
  end

  def teardown
    @@s = nil
  end

  def test_SBase_CVTerms
    cv = LibSBML::CVTerm.new(LibSBML::BIOLOGICAL_QUALIFIER)
    assert( @@s.getNumCVTerms == 0 )
    @@s.addCVTerm(cv)
    assert( @@s.getNumCVTerms == 1 )
    assert( @@s.getCVTerm(0) != cv )
    cv = nil
  end

  def test_SBase_addCVTerms
    cv = LibSBML::CVTerm.new(LibSBML::BIOLOGICAL_QUALIFIER)
    cv.setBiologicalQualifierType(LibSBML::BQB_ENCODES)
    cv.addResource( "foo")
    @@s.addCVTerm(cv)
    assert( @@s.getNumCVTerms == 1 )
    res = @@s.getCVTerm(0).getResources
    assert ((  "foo" == res.getValue(0) ))
    cv1 = LibSBML::CVTerm.new(LibSBML::BIOLOGICAL_QUALIFIER)
    cv1.setBiologicalQualifierType(LibSBML::BQB_IS)
    cv1.addResource( "bar")
    @@s.addCVTerm(cv1)
    assert( @@s.getNumCVTerms == 2 )
    cv2 = LibSBML::CVTerm.new(LibSBML::BIOLOGICAL_QUALIFIER)
    cv2.setBiologicalQualifierType(LibSBML::BQB_IS)
    cv2.addResource( "bar1")
    @@s.addCVTerm(cv2)
    assert( @@s.getNumCVTerms == 2 )
    res = @@s.getCVTerm(1).getResources
    assert( res.getLength == 2 )
    assert ((  "bar" == res.getValue(0) ))
    assert ((  "bar1" == res.getValue(1) ))
    cv4 = LibSBML::CVTerm.new(LibSBML::BIOLOGICAL_QUALIFIER)
    cv4.setBiologicalQualifierType(LibSBML::BQB_IS)
    cv4.addResource( "bar1")
    @@s.addCVTerm(cv4)
    assert( @@s.getNumCVTerms == 2 )
    res = @@s.getCVTerm(1).getResources
    assert( res.getLength == 2 )
    assert ((  "bar" == res.getValue(0) ))
    assert ((  "bar1" == res.getValue(1) ))
    cv5 = LibSBML::CVTerm.new(LibSBML::BIOLOGICAL_QUALIFIER)
    cv5.setBiologicalQualifierType(LibSBML::BQB_HAS_PART)
    cv5.addResource( "bar1")
    @@s.addCVTerm(cv5)
    assert( @@s.getNumCVTerms == 2 )
    res = @@s.getCVTerm(1).getResources
    assert( res.getLength == 2 )
    assert ((  "bar" == res.getValue(0) ))
    assert ((  "bar1" == res.getValue(1) ))
    cv = nil
    cv2 = nil
    cv1 = nil
    cv4 = nil
  end

  def test_SBase_getQualifiersFromResources
    cv = LibSBML::CVTerm.new(LibSBML::BIOLOGICAL_QUALIFIER)
    cv.setBiologicalQualifierType(LibSBML::BQB_ENCODES)
    cv.addResource( "foo")
    @@s.addCVTerm(cv)
    assert( @@s.getResourceBiologicalQualifier( "foo") == LibSBML::BQB_ENCODES )
    cv1 = LibSBML::CVTerm.new(LibSBML::MODEL_QUALIFIER)
    cv1.setModelQualifierType(LibSBML::BQM_IS)
    cv1.addResource( "bar")
    @@s.addCVTerm(cv1)
    assert( @@s.getResourceModelQualifier( "bar") == LibSBML::BQM_IS )
    cv = nil
    cv1 = nil
  end

  def test_SBase_setAnnotation
    token = LibSBML::XMLToken.new("This is a test note")
    node = LibSBML::XMLNode.new(token)
    @@s.setAnnotation(node)
    assert( @@s.isSetAnnotation == true )
    t1 = @@s.getAnnotation
    assert( t1.getNumChildren == 1 )
    assert ((  "This is a test note" == t1.getChild(0).getCharacters ))
    if (@@s.getAnnotation == node)
    end
    @@s.setAnnotation(@@s.getAnnotation)
    assert ((  "This is a test note" == @@s.getAnnotation.getChild(0).getCharacters ))
    @@s.setAnnotation(nil)
    assert( @@s.isSetAnnotation == false )
    if (@@s.getAnnotation != nil)
    end
    @@s.setAnnotation(node)
    assert( @@s.isSetAnnotation == true )
    @@s.unsetAnnotation
    assert( @@s.isSetAnnotation == false )
  end

  def test_SBase_setAnnotationString
    annotation = "This is a test note"
    taggedannotation = "<annotation>This is a test note</annotation>"
    @@s.setAnnotation(annotation)
    assert( @@s.isSetAnnotation == true )
    if (( taggedannotation != @@s.getAnnotationString ))
    end
    t1 = @@s.getAnnotation
    assert( t1.getNumChildren == 1 )
    t2 = t1.getChild(0)
    assert ((  "This is a test note" == t2.getChild(0).getCharacters ))
    @@s.setAnnotation(@@s.getAnnotationString)
    t1 = @@s.getAnnotation
    assert( t1.getNumChildren == 1 )
    chars = @@s.getAnnotationString
    assert (( taggedannotation == chars ))
    @@s.setAnnotation( "")
    assert( @@s.isSetAnnotation == false )
    if (@@s.getAnnotationString != nil)
    end
    @@s.setAnnotation(taggedannotation)
    assert( @@s.isSetAnnotation == true )
    if (( taggedannotation != @@s.getAnnotationString ))
    end
    t1 = @@s.getAnnotation
    assert( t1.getNumChildren == 1 )
    t2 = t1.getChild(0)
    assert ((  "This is a test note" == t2.getCharacters ))
  end

  def test_SBase_setMetaId
    metaid = "x12345"
    @@s.setMetaId(metaid)
    assert (( metaid == @@s.getMetaId ))
    assert_equal true, @@s.isSetMetaId
    if (@@s.getMetaId == metaid)
    end
    @@s.setMetaId(@@s.getMetaId)
    assert (( metaid == @@s.getMetaId ))
    @@s.setMetaId("")
    assert_equal false, @@s.isSetMetaId
    if (@@s.getMetaId != nil)
    end
  end

  def test_SBase_setNotes
    token = LibSBML::XMLToken.new("This is a test note")
    node = LibSBML::XMLNode.new(token)
    @@s.setNotes(node)
    assert( @@s.isSetNotes == true )
    if (@@s.getNotes == node)
    end
    t1 = @@s.getNotes
    assert( t1.getNumChildren == 1 )
    assert ((  "This is a test note" == t1.getChild(0).getCharacters ))
    @@s.setNotes(@@s.getNotes)
    t1 = @@s.getNotes
    assert( t1.getNumChildren == 1 )
    chars = t1.getChild(0).getCharacters
    assert ((  "This is a test note" == chars ))
    @@s.setNotes(nil)
    assert( @@s.isSetNotes == false )
    if (@@s.getNotes != nil)
    end
    @@s.setNotes(node)
    assert( @@s.isSetNotes == true )
    node = nil
  end

  def test_SBase_setNotesString
    notes = "This is a test note"
    taggednotes = "<notes>This is a test note</notes>"
    @@s.setNotes(notes)
    assert( @@s.isSetNotes == true )
    if (( taggednotes != @@s.getNotesString ))
    end
    t1 = @@s.getNotes
    assert( t1.getNumChildren == 1 )
    t2 = t1.getChild(0)
    assert ((  "This is a test note" == t2.getChild(0).getCharacters ))
    @@s.setNotes(@@s.getNotesString)
    t1 = @@s.getNotes
    assert( t1.getNumChildren == 1 )
    chars = @@s.getNotesString
    assert (( taggednotes == chars ))
    @@s.setNotes( "")
    assert( @@s.isSetNotes == false )
    if (@@s.getNotesString != nil)
    end
    @@s.setNotes(taggednotes)
    assert( @@s.isSetNotes == true )
    if (( taggednotes != @@s.getNotesString ))
    end
    t1 = @@s.getNotes
    assert( t1.getNumChildren == 1 )
    t2 = t1.getChild(0)
    assert ((  "This is a test note" == t2.getCharacters ))
  end

  def test_SBase_unsetCVTerms
    cv = LibSBML::CVTerm.new(LibSBML::BIOLOGICAL_QUALIFIER)
    cv.setBiologicalQualifierType(LibSBML::BQB_ENCODES)
    cv.addResource( "foo")
    @@s.addCVTerm(cv)
    cv1 = LibSBML::CVTerm.new(LibSBML::BIOLOGICAL_QUALIFIER)
    cv1.setBiologicalQualifierType(LibSBML::BQB_IS)
    cv1.addResource( "bar")
    @@s.addCVTerm(cv1)
    cv2 = LibSBML::CVTerm.new(LibSBML::BIOLOGICAL_QUALIFIER)
    cv2.setBiologicalQualifierType(LibSBML::BQB_IS)
    cv2.addResource( "bar1")
    @@s.addCVTerm(cv2)
    cv4 = LibSBML::CVTerm.new(LibSBML::BIOLOGICAL_QUALIFIER)
    cv4.setBiologicalQualifierType(LibSBML::BQB_IS)
    cv4.addResource( "bar1")
    @@s.addCVTerm(cv4)
    assert( @@s.getNumCVTerms == 2 )
    @@s.unsetCVTerms
    assert( @@s.getNumCVTerms == 0 )
    cv = nil
    cv2 = nil
    cv1 = nil
    cv4 = nil
  end

end
