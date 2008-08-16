/// 
///  @file    TestSBase.cs
///  @brief   SBase unit tests
///  @author  Frank Bergmann (Csharp conversion)
///  @author  Akiya Jouraku (Csharp conversion)
///  @author  Ben Bornstein 
/// 
///  $Id:$
///  $HeadURL:$
/// 
///  This test file was converted from src/sbml/test/TestSBase.cpp
///  with the help of conversion sciprt (ctest_converter.pl).
/// 
/// <!---------------------------------------------------------------------------
///  This file is part of libSBML.  Please visit http://sbml.org for more
///  information about SBML, and the latest version of libSBML.
/// 
///  Copyright 2005-2008 California Institute of Technology.
///  Copyright 2002-2005 California Institute of Technology and
///                      Japan Science and Technology Corporation.
///  
///  This library is free software; you can redistribute it and/or modify it
///  under the terms of the GNU Lesser General Public License as published by
///  the Free Software Foundation.  A copy of the license agreement is provided
///  in the file named "LICENSE.txt" included with this software distribution
///  and also available online as http://sbml.org/software/libsbml/license.html
/// --------------------------------------------------------------------------->*/


namespace LibSBMLCSTest {

  using libsbml;

  using  System.IO;

  public class TestSBase {
    public class AssertionError : System.Exception 
    {
      public AssertionError() : base()
      {
        
      }
    }


    static void assertTrue(bool condition)
    {
      if (condition == true)
      {
        return;
      }
      throw new AssertionError();
    }

    static void assertEquals(object a, object b)
    {
      if ( (a == null) && (b == null) )
      {
        return;
      }
      else if (a.Equals(b))
      {
        return;
      }
  
      throw new AssertionError();
    }

    static void assertNotEquals(object a, object b)
    {
      if ( (a == null) && (b == null) )
      {
        throw new AssertionError();
      }
      else if (a.Equals(b))
      {
        throw new AssertionError();
      }
    }

    static void assertEquals(bool a, bool b)
    {
      if ( a == b )
      {
        return;
      }
      throw new AssertionError();
    }

    static void assertNotEquals(bool a, bool b)
    {
      if ( a != b )
      {
        return;
      }
      throw new AssertionError();
    }

    static void assertEquals(int a, int b)
    {
      if ( a == b )
      {
        return;
      }
      throw new AssertionError();
    }

    static void assertNotEquals(int a, int b)
    {
      if ( a != b )
      {
        return;
      }
      throw new AssertionError();
    }

    private SBase S;

    public void setUp()
    {
      S = new Model();
      if (S == null);
      {
      }
    }

    public void tearDown()
    {
      S = null;
    }

    public void test_SBase_CVTerms()
    {
      CVTerm cv = new  CVTerm(libsbml.BIOLOGICAL_QUALIFIER);
      assertTrue( S.getNumCVTerms() == 0 );
      S.addCVTerm(cv);
      assertTrue( S.getNumCVTerms() == 1 );
      assertTrue( S.getCVTerm(0) != cv );
      cv = null;
    }

    public void test_SBase_addCVTerms()
    {
      CVTerm cv = new  CVTerm(libsbml.BIOLOGICAL_QUALIFIER);
      cv.setBiologicalQualifierType(libsbml.BQB_ENCODES);
      cv.addResource( "foo");
      S.addCVTerm(cv);
      assertTrue( S.getNumCVTerms() == 1 );
      XMLAttributes res = S.getCVTerm(0).getResources();
      assertTrue((  "foo" == res.getValue(0) ));
      CVTerm cv1 = new  CVTerm(libsbml.BIOLOGICAL_QUALIFIER);
      cv1.setBiologicalQualifierType(libsbml.BQB_IS);
      cv1.addResource( "bar");
      S.addCVTerm(cv1);
      assertTrue( S.getNumCVTerms() == 2 );
      CVTerm cv2 = new  CVTerm(libsbml.BIOLOGICAL_QUALIFIER);
      cv2.setBiologicalQualifierType(libsbml.BQB_IS);
      cv2.addResource( "bar1");
      S.addCVTerm(cv2);
      assertTrue( S.getNumCVTerms() == 2 );
      res = S.getCVTerm(1).getResources();
      assertTrue( res.getLength() == 2 );
      assertTrue((  "bar" == res.getValue(0) ));
      assertTrue((  "bar1" == res.getValue(1) ));
      CVTerm cv4 = new  CVTerm(libsbml.BIOLOGICAL_QUALIFIER);
      cv4.setBiologicalQualifierType(libsbml.BQB_IS);
      cv4.addResource( "bar1");
      S.addCVTerm(cv4);
      assertTrue( S.getNumCVTerms() == 2 );
      res = S.getCVTerm(1).getResources();
      assertTrue( res.getLength() == 2 );
      assertTrue((  "bar" == res.getValue(0) ));
      assertTrue((  "bar1" == res.getValue(1) ));
      CVTerm cv5 = new  CVTerm(libsbml.BIOLOGICAL_QUALIFIER);
      cv5.setBiologicalQualifierType(libsbml.BQB_HAS_PART);
      cv5.addResource( "bar1");
      S.addCVTerm(cv5);
      assertTrue( S.getNumCVTerms() == 2 );
      res = S.getCVTerm(1).getResources();
      assertTrue( res.getLength() == 2 );
      assertTrue((  "bar" == res.getValue(0) ));
      assertTrue((  "bar1" == res.getValue(1) ));
      cv = null;
      cv2 = null;
      cv1 = null;
      cv4 = null;
    }

    public void test_SBase_getQualifiersFromResources()
    {
      CVTerm cv = new  CVTerm(libsbml.BIOLOGICAL_QUALIFIER);
      cv.setBiologicalQualifierType(libsbml.BQB_ENCODES);
      cv.addResource( "foo");
      S.addCVTerm(cv);
      assertTrue( S.getResourceBiologicalQualifier( "foo") == libsbml.BQB_ENCODES );
      CVTerm cv1 = new  CVTerm(libsbml.MODEL_QUALIFIER);
      cv1.setModelQualifierType(libsbml.BQM_IS);
      cv1.addResource( "bar");
      S.addCVTerm(cv1);
      assertTrue( S.getResourceModelQualifier( "bar") == libsbml.BQM_IS );
      cv = null;
      cv1 = null;
    }

    public void test_SBase_setAnnotation()
    {
      XMLToken token;
      XMLNode node;
      token = new  XMLToken("This is a test note");
      node = new  XMLNode(token);
      S.setAnnotation(node);
      assertTrue( S.isSetAnnotation() == true );
      XMLNode t1 = S.getAnnotation();
      assertTrue( t1.getNumChildren() == 1 );
      assertTrue((  "This is a test note" == t1.getChild(0).getCharacters() ));
      if (S.getAnnotation() == node);
      {
      }
      S.setAnnotation(S.getAnnotation());
      assertTrue((  "This is a test note" == S.getAnnotation().getChild(0).getCharacters() ));
      S.setAnnotation("");
      assertTrue( S.isSetAnnotation() == false );
      if (S.getAnnotation() != null);
      {
      }
      S.setAnnotation(node);
      assertTrue( S.isSetAnnotation() == true );
      S.unsetAnnotation();
      assertTrue( S.isSetAnnotation() == false );
    }

    public void test_SBase_setAnnotationString()
    {
      string annotation = "This is a test note";
      string taggedannotation = "<annotation>This is a test note</annotation>";
      S.setAnnotation(annotation);
      assertTrue( S.isSetAnnotation() == true );
      if (( taggedannotation != S.getAnnotationString() ));
      {
      }
      XMLNode t1 = S.getAnnotation();
      assertTrue( t1.getNumChildren() == 1 );
      XMLNode t2 = t1.getChild(0);
      assertTrue((  "This is a test note" == t2.getChild(0).getCharacters() ));
      S.setAnnotation(S.getAnnotationString());
      t1 = S.getAnnotation();
      assertTrue( t1.getNumChildren() == 1 );
      string chars = S.getAnnotationString();
      assertTrue(( taggedannotation == chars ));
      S.setAnnotation( "");
      assertTrue( S.isSetAnnotation() == false );
      if (S.getAnnotationString() != null);
      {
      }
      S.setAnnotation(taggedannotation);
      assertTrue( S.isSetAnnotation() == true );
      if (( taggedannotation != S.getAnnotationString() ));
      {
      }
      t1 = S.getAnnotation();
      assertTrue( t1.getNumChildren() == 1 );
      t2 = t1.getChild(0);
      assertTrue((  "This is a test note" == t2.getCharacters() ));
    }

    public void test_SBase_setMetaId()
    {
      string metaid = "x12345";
      S.setMetaId(metaid);
      assertTrue(( metaid == S.getMetaId() ));
      assertEquals( true, S.isSetMetaId() );
      if (S.getMetaId() == metaid);
      {
      }
      S.setMetaId(S.getMetaId());
      assertTrue(( metaid == S.getMetaId() ));
      S.setMetaId("");
      assertEquals( false, S.isSetMetaId() );
      if (S.getMetaId() != null);
      {
      }
    }

    public void test_SBase_setNotes()
    {
      XMLToken token;
      XMLNode node;
      token = new  XMLToken("This is a test note");
      node = new  XMLNode(token);
      S.setNotes(node);
      assertTrue( S.isSetNotes() == true );
      if (S.getNotes() == node);
      {
      }
      XMLNode t1 = S.getNotes();
      assertTrue( t1.getNumChildren() == 1 );
      assertTrue((  "This is a test note" == t1.getChild(0).getCharacters() ));
      S.setNotes(S.getNotes());
      t1 = S.getNotes();
      assertTrue( t1.getNumChildren() == 1 );
      string chars = t1.getChild(0).getCharacters();
      assertTrue((  "This is a test note" == chars ));
      S.setNotes("");
      assertTrue( S.isSetNotes() == false );
      if (S.getNotes() != null);
      {
      }
      S.setNotes(node);
      assertTrue( S.isSetNotes() == true );
      node = null;
    }

    public void test_SBase_setNotesString()
    {
      string notes = "This is a test note";
      string taggednotes = "<notes>This is a test note</notes>";
      S.setNotes(notes);
      assertTrue( S.isSetNotes() == true );
      if (( taggednotes != S.getNotesString() ));
      {
      }
      XMLNode t1 = S.getNotes();
      assertTrue( t1.getNumChildren() == 1 );
      XMLNode t2 = t1.getChild(0);
      assertTrue((  "This is a test note" == t2.getChild(0).getCharacters() ));
      S.setNotes(S.getNotesString());
      t1 = S.getNotes();
      assertTrue( t1.getNumChildren() == 1 );
      string chars = S.getNotesString();
      assertTrue(( taggednotes == chars ));
      S.setNotes( "");
      assertTrue( S.isSetNotes() == false );
      if (S.getNotesString() != null);
      {
      }
      S.setNotes(taggednotes);
      assertTrue( S.isSetNotes() == true );
      if (( taggednotes != S.getNotesString() ));
      {
      }
      t1 = S.getNotes();
      assertTrue( t1.getNumChildren() == 1 );
      t2 = t1.getChild(0);
      assertTrue((  "This is a test note" == t2.getCharacters() ));
    }

    public void test_SBase_unsetCVTerms()
    {
      CVTerm cv = new  CVTerm(libsbml.BIOLOGICAL_QUALIFIER);
      cv.setBiologicalQualifierType(libsbml.BQB_ENCODES);
      cv.addResource( "foo");
      S.addCVTerm(cv);
      CVTerm cv1 = new  CVTerm(libsbml.BIOLOGICAL_QUALIFIER);
      cv1.setBiologicalQualifierType(libsbml.BQB_IS);
      cv1.addResource( "bar");
      S.addCVTerm(cv1);
      CVTerm cv2 = new  CVTerm(libsbml.BIOLOGICAL_QUALIFIER);
      cv2.setBiologicalQualifierType(libsbml.BQB_IS);
      cv2.addResource( "bar1");
      S.addCVTerm(cv2);
      CVTerm cv4 = new  CVTerm(libsbml.BIOLOGICAL_QUALIFIER);
      cv4.setBiologicalQualifierType(libsbml.BQB_IS);
      cv4.addResource( "bar1");
      S.addCVTerm(cv4);
      assertTrue( S.getNumCVTerms() == 2 );
      S.unsetCVTerms();
      assertTrue( S.getNumCVTerms() == 0 );
      cv = null;
      cv2 = null;
      cv1 = null;
      cv4 = null;
    }

  }
}
