/// 
///  @file    TestSBase.cs
///  @brief   SBase unit tests
///  @author  Frank Bergmann (Csharp conversion)
///  @author  Akiya Jouraku (Csharp conversion)
///  @author  Ben Bornstein 
/// 
///  $Id$
///  $HeadURL$
/// 
///  This test file was converted from src/sbml/test/TestSBase.cpp
///  with the help of conversion sciprt (ctest_converter.pl).
/// 
/// <!---------------------------------------------------------------------------
///  This file is part of libSBML.  Please visit http://sbml.org for more
///  information about SBML, and the latest version of libSBML.
/// 
///  Copyright 2005-2009 California Institute of Technology.
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
    }

    public void test_SBase_CVTerms()
    {
      CVTerm cv = new  CVTerm(libsbml.BIOLOGICAL_QUALIFIER);
      assertTrue( S.getNumCVTerms() == 0 );
      S.setMetaId( "sbase1");
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
      S.setMetaId( "sbase1");
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

    public void test_SBase_appendNotes()
    {
      XMLToken token;
      XMLNode node;
      XMLToken token1;
      XMLNode node1;
      XMLNode node2;
      XMLTriple triple = new  XMLTriple("p", "", "");
      XMLAttributes att = new  XMLAttributes();
      XMLNamespaces ns = new  XMLNamespaces();
      ns.add( "http://www.w3.org/1999/xhtml", "");
      XMLToken token4 = new  XMLToken("This is my text");
      XMLNode node4 = new XMLNode(token4);
      XMLToken token5 = new  XMLToken("This is additional text");
      XMLNode node5 = new XMLNode(token5);
      token = new  XMLToken(triple,att,ns);
      node = new XMLNode(token);
      node.addChild(node4);
      S.setNotes(node);
      assertTrue( S.isSetNotes() == true );
      token1 = new  XMLToken(triple,att,ns);
      node1 = new XMLNode(token1);
      node1.addChild(node5);
      S.appendNotes(node1);
      assertTrue( S.isSetNotes() == true );
      node2 = S.getNotes();
      assertTrue( node2.getNumChildren() == 2 );
      assertTrue((  "p" == node2.getChild(0).getName() ));
      assertTrue( node2.getChild(0).getNumChildren() == 1 );
      assertTrue((  "p" == node2.getChild(1).getName() ));
      assertTrue( node2.getChild(1).getNumChildren() == 1 );
      string chars1 = node2.getChild(0).getChild(0).getCharacters();
      string chars2 = node2.getChild(1).getChild(0).getCharacters();
      assertTrue((  "This is my text" == chars1 ));
      assertTrue((  "This is additional text" == chars2 ));
      node = null;
      node1 = null;
    }

    public void test_SBase_appendNotes1()
    {
      XMLAttributes att = new  XMLAttributes();
      XMLNamespaces ns = new  XMLNamespaces();
      ns.add( "http://www.w3.org/1999/xhtml", "");
      XMLTriple html_triple = new  XMLTriple("html", "", "");
      XMLTriple head_triple = new  XMLTriple("head", "", "");
      XMLTriple body_triple = new  XMLTriple("body", "", "");
      XMLTriple p_triple = new  XMLTriple("p", "", "");
      XMLToken html_token = new  XMLToken(html_triple,att,ns);
      XMLToken head_token = new  XMLToken(head_triple,att);
      XMLToken body_token = new  XMLToken(body_triple,att);
      XMLToken p_token = new  XMLToken(p_triple,att);
      XMLToken text_token = new  XMLToken("This is my text");
      XMLNode html_node = new XMLNode(html_token);
      XMLNode head_node = new XMLNode(head_token);
      XMLNode body_node = new XMLNode(body_token);
      XMLNode p_node = new XMLNode(p_token);
      XMLNode text_node = new XMLNode(text_token);
      XMLToken text_token1 = new  XMLToken("This is more text");
      XMLNode html_node1 = new XMLNode(html_token);
      XMLNode head_node1 = new XMLNode(head_token);
      XMLNode body_node1 = new XMLNode(body_token);
      XMLNode p_node1 = new XMLNode(p_token);
      XMLNode text_node1 = new XMLNode(text_token1);
      XMLNode notes;
      XMLNode child,child1;
      p_node.addChild(text_node);
      body_node.addChild(p_node);
      html_node.addChild(head_node);
      html_node.addChild(body_node);
      p_node1.addChild(text_node1);
      body_node1.addChild(p_node1);
      html_node1.addChild(head_node1);
      html_node1.addChild(body_node1);
      S.setNotes(html_node);
      S.appendNotes(html_node1);
      notes = S.getNotes();
      assertTrue((  "notes" == notes.getName() ));
      assertTrue( notes.getNumChildren() == 1 );
      child = notes.getChild(0);
      assertTrue((  "html" == child.getName() ));
      assertTrue( child.getNumChildren() == 2 );
      child = child.getChild(1);
      assertTrue((  "body" == child.getName() ));
      assertTrue( child.getNumChildren() == 2 );
      child1 = child.getChild(0);
      assertTrue((  "p" == child1.getName() ));
      assertTrue( child1.getNumChildren() == 1 );
      child1 = child1.getChild(0);
      assertTrue((  "This is my text" == child1.getCharacters() ));
      assertTrue( child1.getNumChildren() == 0 );
      child1 = child.getChild(1);
      assertTrue((  "p" == child1.getName() ));
      assertTrue( child1.getNumChildren() == 1 );
      child1 = child1.getChild(0);
      assertTrue((  "This is more text" == child1.getCharacters() ));
      assertTrue( child1.getNumChildren() == 0 );
      att = null;
      ns = null;
      html_triple = null;
      head_triple = null;
      body_triple = null;
      p_triple = null;
      html_token = null;
      head_token = null;
      body_token = null;
      p_token = null;
      text_token = null;
      text_token1 = null;
      html_node = null;
      head_node = null;
      body_node = null;
      p_node = null;
      text_node = null;
      html_node1 = null;
      head_node1 = null;
      body_node1 = null;
      p_node1 = null;
      text_node1 = null;
    }

    public void test_SBase_appendNotes2()
    {
      XMLAttributes att = new  XMLAttributes();
      XMLNamespaces ns = new  XMLNamespaces();
      ns.add( "http://www.w3.org/1999/xhtml", "");
      XMLTriple html_triple = new  XMLTriple("html", "", "");
      XMLTriple head_triple = new  XMLTriple("head", "", "");
      XMLTriple body_triple = new  XMLTriple("body", "", "");
      XMLTriple p_triple = new  XMLTriple("p", "", "");
      XMLToken html_token = new  XMLToken(html_triple,att,ns);
      XMLToken head_token = new  XMLToken(head_triple,att);
      XMLToken body_token = new  XMLToken(body_triple,att);
      XMLToken p_token = new  XMLToken(p_triple,att);
      XMLToken text_token = new  XMLToken("This is my text");
      XMLNode html_node = new XMLNode(html_token);
      XMLNode head_node = new XMLNode(head_token);
      XMLNode body_node = new XMLNode(body_token);
      XMLNode p_node = new XMLNode(p_token);
      XMLNode text_node = new XMLNode(text_token);
      XMLToken body_token1 = new  XMLToken(body_triple,att,ns);
      XMLToken text_token1 = new  XMLToken("This is more text");
      XMLNode body_node1 = new XMLNode(body_token1);
      XMLNode p_node1 = new XMLNode(p_token);
      XMLNode text_node1 = new XMLNode(text_token1);
      XMLNode notes;
      XMLNode child,child1;
      p_node.addChild(text_node);
      body_node.addChild(p_node);
      html_node.addChild(head_node);
      html_node.addChild(body_node);
      p_node1.addChild(text_node1);
      body_node1.addChild(p_node1);
      S.setNotes(html_node);
      S.appendNotes(body_node1);
      notes = S.getNotes();
      assertTrue((  "notes" == notes.getName() ));
      assertTrue( notes.getNumChildren() == 1 );
      child = notes.getChild(0);
      assertTrue((  "html" == child.getName() ));
      assertTrue( child.getNumChildren() == 2 );
      child = child.getChild(1);
      assertTrue((  "body" == child.getName() ));
      assertTrue( child.getNumChildren() == 2 );
      child1 = child.getChild(0);
      assertTrue((  "p" == child1.getName() ));
      assertTrue( child1.getNumChildren() == 1 );
      child1 = child1.getChild(0);
      assertTrue((  "This is my text" == child1.getCharacters() ));
      assertTrue( child1.getNumChildren() == 0 );
      child1 = child.getChild(1);
      assertTrue((  "p" == child1.getName() ));
      assertTrue( child1.getNumChildren() == 1 );
      child1 = child1.getChild(0);
      assertTrue((  "This is more text" == child1.getCharacters() ));
      assertTrue( child1.getNumChildren() == 0 );
      att = null;
      ns = null;
      html_triple = null;
      head_triple = null;
      body_triple = null;
      p_triple = null;
      html_token = null;
      head_token = null;
      body_token = null;
      p_token = null;
      text_token = null;
      text_token1 = null;
      body_token1 = null;
      html_node = null;
      head_node = null;
      body_node = null;
      p_node = null;
      text_node = null;
      body_node1 = null;
      p_node1 = null;
      text_node1 = null;
    }

    public void test_SBase_appendNotes3()
    {
      XMLAttributes att = new  XMLAttributes();
      XMLNamespaces ns = new  XMLNamespaces();
      ns.add( "http://www.w3.org/1999/xhtml", "");
      XMLTriple html_triple = new  XMLTriple("html", "", "");
      XMLTriple head_triple = new  XMLTriple("head", "", "");
      XMLTriple body_triple = new  XMLTriple("body", "", "");
      XMLTriple p_triple = new  XMLTriple("p", "", "");
      XMLToken html_token = new  XMLToken(html_triple,att,ns);
      XMLToken head_token = new  XMLToken(head_triple,att);
      XMLToken body_token = new  XMLToken(body_triple,att);
      XMLToken p_token = new  XMLToken(p_triple,att);
      XMLToken text_token = new  XMLToken("This is my text");
      XMLNode html_node = new XMLNode(html_token);
      XMLNode head_node = new XMLNode(head_token);
      XMLNode body_node = new XMLNode(body_token);
      XMLNode p_node = new XMLNode(p_token);
      XMLNode text_node = new XMLNode(text_token);
      XMLToken p_token1 = new  XMLToken(p_triple,att,ns);
      XMLToken text_token1 = new  XMLToken("This is more text");
      XMLNode p_node1 = new XMLNode(p_token1);
      XMLNode text_node1 = new XMLNode(text_token1);
      XMLNode notes;
      XMLNode child,child1;
      p_node.addChild(text_node);
      body_node.addChild(p_node);
      html_node.addChild(head_node);
      html_node.addChild(body_node);
      p_node1.addChild(text_node1);
      S.setNotes(html_node);
      S.appendNotes(p_node1);
      notes = S.getNotes();
      assertTrue((  "notes" == notes.getName() ));
      assertTrue( notes.getNumChildren() == 1 );
      child = notes.getChild(0);
      assertTrue((  "html" == child.getName() ));
      assertTrue( child.getNumChildren() == 2 );
      child = child.getChild(1);
      assertTrue((  "body" == child.getName() ));
      assertTrue( child.getNumChildren() == 2 );
      child1 = child.getChild(0);
      assertTrue((  "p" == child1.getName() ));
      assertTrue( child1.getNumChildren() == 1 );
      child1 = child1.getChild(0);
      assertTrue((  "This is my text" == child1.getCharacters() ));
      assertTrue( child1.getNumChildren() == 0 );
      child1 = child.getChild(1);
      assertTrue((  "p" == child1.getName() ));
      assertTrue( child1.getNumChildren() == 1 );
      child1 = child1.getChild(0);
      assertTrue((  "This is more text" == child1.getCharacters() ));
      assertTrue( child1.getNumChildren() == 0 );
      att = null;
      ns = null;
      html_triple = null;
      head_triple = null;
      body_triple = null;
      p_triple = null;
      html_token = null;
      head_token = null;
      body_token = null;
      p_token = null;
      text_token = null;
      text_token1 = null;
      p_token1 = null;
      html_node = null;
      head_node = null;
      body_node = null;
      p_node = null;
      text_node = null;
      p_node1 = null;
      text_node1 = null;
    }

    public void test_SBase_appendNotes4()
    {
      XMLAttributes att = new  XMLAttributes();
      XMLNamespaces ns = new  XMLNamespaces();
      ns.add( "http://www.w3.org/1999/xhtml", "");
      XMLTriple html_triple = new  XMLTriple("html", "", "");
      XMLTriple head_triple = new  XMLTriple("head", "", "");
      XMLTriple body_triple = new  XMLTriple("body", "", "");
      XMLTriple p_triple = new  XMLTriple("p", "", "");
      XMLToken html_token = new  XMLToken(html_triple,att,ns);
      XMLToken head_token = new  XMLToken(head_triple,att);
      XMLToken body_token = new  XMLToken(body_triple,att);
      XMLToken p_token = new  XMLToken(p_triple,att);
      XMLToken body_token1 = new  XMLToken(body_triple,att,ns);
      XMLToken text_token = new  XMLToken("This is my text");
      XMLNode body_node = new XMLNode(body_token1);
      XMLNode p_node = new XMLNode(p_token);
      XMLNode text_node = new XMLNode(text_token);
      XMLToken text_token1 = new  XMLToken("This is more text");
      XMLNode html_node1 = new XMLNode(html_token);
      XMLNode head_node1 = new XMLNode(head_token);
      XMLNode body_node1 = new XMLNode(body_token);
      XMLNode p_node1 = new XMLNode(p_token);
      XMLNode text_node1 = new XMLNode(text_token1);
      XMLNode notes;
      XMLNode child,child1;
      p_node.addChild(text_node);
      body_node.addChild(p_node);
      p_node1.addChild(text_node1);
      body_node1.addChild(p_node1);
      html_node1.addChild(head_node1);
      html_node1.addChild(body_node1);
      S.setNotes(body_node);
      S.appendNotes(html_node1);
      notes = S.getNotes();
      assertTrue((  "notes" == notes.getName() ));
      assertTrue( notes.getNumChildren() == 1 );
      child = notes.getChild(0);
      assertTrue((  "html" == child.getName() ));
      assertTrue( child.getNumChildren() == 2 );
      child = child.getChild(1);
      assertTrue((  "body" == child.getName() ));
      assertTrue( child.getNumChildren() == 2 );
      child1 = child.getChild(0);
      assertTrue((  "p" == child1.getName() ));
      assertTrue( child1.getNumChildren() == 1 );
      child1 = child1.getChild(0);
      assertTrue((  "This is my text" == child1.getCharacters() ));
      assertTrue( child1.getNumChildren() == 0 );
      child1 = child.getChild(1);
      assertTrue((  "p" == child1.getName() ));
      assertTrue( child1.getNumChildren() == 1 );
      child1 = child1.getChild(0);
      assertTrue((  "This is more text" == child1.getCharacters() ));
      assertTrue( child1.getNumChildren() == 0 );
      att = null;
      ns = null;
      html_triple = null;
      head_triple = null;
      body_triple = null;
      p_triple = null;
      body_token = null;
      p_token = null;
      text_token = null;
      text_token1 = null;
      body_token1 = null;
      body_node = null;
      p_node = null;
      text_node = null;
      html_node1 = null;
      head_node1 = null;
      body_node1 = null;
      p_node1 = null;
      text_node1 = null;
    }

    public void test_SBase_appendNotes5()
    {
      XMLAttributes att = new  XMLAttributes();
      XMLNamespaces ns = new  XMLNamespaces();
      ns.add( "http://www.w3.org/1999/xhtml", "");
      XMLTriple html_triple = new  XMLTriple("html", "", "");
      XMLTriple head_triple = new  XMLTriple("head", "", "");
      XMLTriple body_triple = new  XMLTriple("body", "", "");
      XMLTriple p_triple = new  XMLTriple("p", "", "");
      XMLToken html_token = new  XMLToken(html_triple,att,ns);
      XMLToken head_token = new  XMLToken(head_triple,att);
      XMLToken body_token = new  XMLToken(body_triple,att);
      XMLToken p_token = new  XMLToken(p_triple,att);
      XMLToken p_token1 = new  XMLToken(p_triple,att,ns);
      XMLToken text_token = new  XMLToken("This is my text");
      XMLNode p_node = new XMLNode(p_token1);
      XMLNode text_node = new XMLNode(text_token);
      XMLToken text_token1 = new  XMLToken("This is more text");
      XMLNode html_node1 = new XMLNode(html_token);
      XMLNode head_node1 = new XMLNode(head_token);
      XMLNode body_node1 = new XMLNode(body_token);
      XMLNode p_node1 = new XMLNode(p_token);
      XMLNode text_node1 = new XMLNode(text_token1);
      XMLNode notes;
      XMLNode child,child1;
      p_node.addChild(text_node);
      p_node1.addChild(text_node1);
      body_node1.addChild(p_node1);
      html_node1.addChild(head_node1);
      html_node1.addChild(body_node1);
      S.setNotes(p_node);
      S.appendNotes(html_node1);
      notes = S.getNotes();
      assertTrue((  "notes" == notes.getName() ));
      assertTrue( notes.getNumChildren() == 1 );
      child = notes.getChild(0);
      assertTrue((  "html" == child.getName() ));
      assertTrue( child.getNumChildren() == 2 );
      child = child.getChild(1);
      assertTrue((  "body" == child.getName() ));
      assertTrue( child.getNumChildren() == 2 );
      child1 = child.getChild(0);
      assertTrue((  "p" == child1.getName() ));
      assertTrue( child1.getNumChildren() == 1 );
      child1 = child1.getChild(0);
      assertTrue((  "This is my text" == child1.getCharacters() ));
      assertTrue( child1.getNumChildren() == 0 );
      child1 = child.getChild(1);
      assertTrue((  "p" == child1.getName() ));
      assertTrue( child1.getNumChildren() == 1 );
      child1 = child1.getChild(0);
      assertTrue((  "This is more text" == child1.getCharacters() ));
      assertTrue( child1.getNumChildren() == 0 );
      att = null;
      ns = null;
      html_triple = null;
      head_triple = null;
      body_triple = null;
      p_triple = null;
      body_token = null;
      p_token = null;
      p_token1 = null;
      text_token = null;
      text_token1 = null;
      p_node = null;
      text_node = null;
      html_node1 = null;
      head_node1 = null;
      body_node1 = null;
      p_node1 = null;
      text_node1 = null;
    }

    public void test_SBase_appendNotes6()
    {
      XMLAttributes att = new  XMLAttributes();
      XMLNamespaces ns = new  XMLNamespaces();
      ns.add( "http://www.w3.org/1999/xhtml", "");
      XMLTriple body_triple = new  XMLTriple("body", "", "");
      XMLTriple p_triple = new  XMLTriple("p", "", "");
      XMLToken body_token = new  XMLToken(body_triple,att,ns);
      XMLToken p_token = new  XMLToken(p_triple,att);
      XMLToken text_token = new  XMLToken("This is my text");
      XMLNode body_node = new XMLNode(body_token);
      XMLNode p_node = new XMLNode(p_token);
      XMLNode text_node = new XMLNode(text_token);
      XMLToken text_token1 = new  XMLToken("This is more text");
      XMLNode body_node1 = new XMLNode(body_token);
      XMLNode p_node1 = new XMLNode(p_token);
      XMLNode text_node1 = new XMLNode(text_token1);
      XMLNode notes;
      XMLNode child,child1;
      p_node.addChild(text_node);
      body_node.addChild(p_node);
      p_node1.addChild(text_node1);
      body_node1.addChild(p_node1);
      S.setNotes(body_node);
      S.appendNotes(body_node1);
      notes = S.getNotes();
      assertTrue((  "notes" == notes.getName() ));
      assertTrue( notes.getNumChildren() == 1 );
      child = notes.getChild(0);
      assertTrue((  "body" == child.getName() ));
      assertTrue( child.getNumChildren() == 2 );
      child1 = child.getChild(0);
      assertTrue((  "p" == child1.getName() ));
      assertTrue( child1.getNumChildren() == 1 );
      child1 = child1.getChild(0);
      assertTrue((  "This is my text" == child1.getCharacters() ));
      assertTrue( child1.getNumChildren() == 0 );
      child1 = child.getChild(1);
      assertTrue((  "p" == child1.getName() ));
      assertTrue( child1.getNumChildren() == 1 );
      child1 = child1.getChild(0);
      assertTrue((  "This is more text" == child1.getCharacters() ));
      assertTrue( child1.getNumChildren() == 0 );
      att = null;
      ns = null;
      body_triple = null;
      p_triple = null;
      body_token = null;
      p_token = null;
      text_token = null;
      text_token1 = null;
      body_node = null;
      p_node = null;
      text_node = null;
      body_node1 = null;
      p_node1 = null;
      text_node1 = null;
    }

    public void test_SBase_appendNotes7()
    {
      XMLAttributes att = new  XMLAttributes();
      XMLNamespaces ns = new  XMLNamespaces();
      ns.add( "http://www.w3.org/1999/xhtml", "");
      XMLTriple body_triple = new  XMLTriple("body", "", "");
      XMLTriple p_triple = new  XMLTriple("p", "", "");
      XMLToken body_token = new  XMLToken(body_triple,att,ns);
      XMLToken p_token1 = new  XMLToken(p_triple,att,ns);
      XMLToken text_token = new  XMLToken("This is my text");
      XMLToken p_token = new  XMLToken(p_triple,att);
      XMLNode p_node = new XMLNode(p_token1);
      XMLNode text_node = new XMLNode(text_token);
      XMLToken text_token1 = new  XMLToken("This is more text");
      XMLNode body_node1 = new XMLNode(body_token);
      XMLNode p_node1 = new XMLNode(p_token);
      XMLNode text_node1 = new XMLNode(text_token1);
      XMLNode notes;
      XMLNode child,child1;
      p_node.addChild(text_node);
      p_node1.addChild(text_node1);
      body_node1.addChild(p_node1);
      S.setNotes(p_node);
      S.appendNotes(body_node1);
      notes = S.getNotes();
      assertTrue((  "notes" == notes.getName() ));
      assertTrue( notes.getNumChildren() == 1 );
      child = notes.getChild(0);
      assertTrue((  "body" == child.getName() ));
      assertTrue( child.getNumChildren() == 2 );
      child1 = child.getChild(0);
      assertTrue((  "p" == child1.getName() ));
      assertTrue( child1.getNumChildren() == 1 );
      child1 = child1.getChild(0);
      assertTrue((  "This is my text" == child1.getCharacters() ));
      assertTrue( child1.getNumChildren() == 0 );
      child1 = child.getChild(1);
      assertTrue((  "p" == child1.getName() ));
      assertTrue( child1.getNumChildren() == 1 );
      child1 = child1.getChild(0);
      assertTrue((  "This is more text" == child1.getCharacters() ));
      assertTrue( child1.getNumChildren() == 0 );
      att = null;
      ns = null;
      body_triple = null;
      p_triple = null;
      body_token = null;
      p_token = null;
      p_token1 = null;
      text_token = null;
      text_token1 = null;
      p_node = null;
      text_node = null;
      body_node1 = null;
      p_node1 = null;
      text_node1 = null;
    }

    public void test_SBase_appendNotes8()
    {
      XMLAttributes att = new  XMLAttributes();
      XMLNamespaces ns = new  XMLNamespaces();
      ns.add( "http://www.w3.org/1999/xhtml", "");
      XMLTriple body_triple = new  XMLTriple("body", "", "");
      XMLTriple p_triple = new  XMLTriple("p", "", "");
      XMLToken body_token = new  XMLToken(body_triple,att,ns);
      XMLToken p_token = new  XMLToken(p_triple,att);
      XMLToken text_token = new  XMLToken("This is my text");
      XMLNode body_node = new XMLNode(body_token);
      XMLNode p_node = new XMLNode(p_token);
      XMLNode text_node = new XMLNode(text_token);
      XMLToken p_token1 = new  XMLToken(p_triple,att,ns);
      XMLToken text_token1 = new  XMLToken("This is more text");
      XMLNode p_node1 = new XMLNode(p_token1);
      XMLNode text_node1 = new XMLNode(text_token1);
      XMLNode notes;
      XMLNode child,child1;
      p_node.addChild(text_node);
      body_node.addChild(p_node);
      p_node1.addChild(text_node1);
      S.setNotes(body_node);
      S.appendNotes(p_node1);
      notes = S.getNotes();
      assertTrue((  "notes" == notes.getName() ));
      assertTrue( notes.getNumChildren() == 1 );
      child = notes.getChild(0);
      assertTrue((  "body" == child.getName() ));
      assertTrue( child.getNumChildren() == 2 );
      child1 = child.getChild(0);
      assertTrue((  "p" == child1.getName() ));
      assertTrue( child1.getNumChildren() == 1 );
      child1 = child1.getChild(0);
      assertTrue((  "This is my text" == child1.getCharacters() ));
      assertTrue( child1.getNumChildren() == 0 );
      child1 = child.getChild(1);
      assertTrue((  "p" == child1.getName() ));
      assertTrue( child1.getNumChildren() == 1 );
      child1 = child1.getChild(0);
      assertTrue((  "This is more text" == child1.getCharacters() ));
      assertTrue( child1.getNumChildren() == 0 );
      att = null;
      ns = null;
      body_triple = null;
      p_triple = null;
      body_token = null;
      p_token = null;
      text_token = null;
      text_token1 = null;
      p_token1 = null;
      body_node = null;
      p_node = null;
      text_node = null;
      p_node1 = null;
      text_node1 = null;
    }

    public void test_SBase_appendNotesString()
    {
      string notes =  "<p xmlns=\"http://www.w3.org/1999/xhtml\">This is a test note </p>";;
      string taggednewnotes = "<notes>\n" + "  <p xmlns=\"http://www.w3.org/1999/xhtml\">This is a test note </p>\n" + 
    "  <p xmlns=\"http://www.w3.org/1999/xhtml\">This is more test notes </p>\n" + 
    "</notes>";
      string taggednewnotes2 = "<notes>\n" + "  <p xmlns=\"http://www.w3.org/1999/xhtml\">This is a test note </p>\n" + 
    "  <p xmlns=\"http://www.w3.org/1999/xhtml\">This is more test notes 1</p>\n" + 
    "  <p xmlns=\"http://www.w3.org/1999/xhtml\">This is more test notes 2</p>\n" + 
    "</notes>";
      string newnotes =  "<p xmlns=\"http://www.w3.org/1999/xhtml\">This is more test notes </p>";
      string newnotes2 = "<p xmlns=\"http://www.w3.org/1999/xhtml\">This is more test notes 1</p>" +
			"<p xmlns=\"http://www.w3.org/1999/xhtml\">This is more test notes 2</p>";
      string newnotes3 = "<notes>\n" + "  <p xmlns=\"http://www.w3.org/1999/xhtml\">This is more test notes </p>\n" + "</notes>";
      string newnotes4 = "<notes>\n" + "  <p xmlns=\"http://www.w3.org/1999/xhtml\">This is more test notes 1</p>\n" + 
    "  <p xmlns=\"http://www.w3.org/1999/xhtml\">This is more test notes 2</p>\n" + 
    "</notes>";
      S.setNotes(notes);
      assertTrue( S.isSetNotes() == true );
      S.appendNotes(newnotes);
      string notes1 = S.getNotesString();
      assertTrue( S.isSetNotes() == true );
      assertTrue(( notes1 == taggednewnotes ));
      S.setNotes(notes);
      S.appendNotes(newnotes2);
      string notes2 = S.getNotesString();
      assertTrue( S.isSetNotes() == true );
      assertTrue(( notes2 == taggednewnotes2 ));
      S.setNotes(notes);
      S.appendNotes(newnotes3);
      string notes3 = S.getNotesString();
      assertTrue( S.isSetNotes() == true );
      assertTrue(( notes3 == taggednewnotes ));
      S.setNotes(notes);
      S.appendNotes(newnotes4);
      string notes4 = S.getNotesString();
      assertTrue( S.isSetNotes() == true );
      assertTrue(( notes4 == taggednewnotes2 ));
    }

    public void test_SBase_appendNotesString1()
    {
      string notes = "<html xmlns=\"http://www.w3.org/1999/xhtml\">\n" + "  <head>\n" + 
    "    <title/>\n" + 
    "  </head>\n" + 
    "  <body>\n" + 
    "    <p>This is a test note </p>\n" + 
    "  </body>\n" + 
    "</html>";
      string taggednewnotes = "<notes>\n" + 
    "  <html xmlns=\"http://www.w3.org/1999/xhtml\">\n" + 
    "    <head>\n" + 
    "      <title/>\n" + 
    "    </head>\n" + 
    "    <body>\n" + 
    "      <p>This is a test note </p>\n" + 
    "      <p>This is more test notes </p>\n" + 
    "    </body>\n" + 
    "  </html>\n" + 
    "</notes>";
      string addnotes = "<html xmlns=\"http://www.w3.org/1999/xhtml\">\n" + "  <head>\n" + 
    "    <title/>\n" + 
    "  </head>\n" + 
    "  <body>\n" + 
    "    <p>This is more test notes </p>\n" + 
    "  </body>\n" + 
    "</html>";
      string addnotes2 = "<notes>\n" + 
    "  <html xmlns=\"http://www.w3.org/1999/xhtml\">\n" + 
    "    <head>\n" + 
    "      <title/>\n" + 
    "    </head>\n" + 
    "    <body>\n" + 
    "      <p>This is more test notes </p>\n" + 
    "    </body>\n" + 
    "  </html>\n" + 
    "</notes>";
      S.setNotes(notes);
      S.appendNotes(addnotes);
      string notes1 = S.getNotesString();
      assertTrue( S.isSetNotes() == true );
      assertTrue(( notes1 == taggednewnotes ));
      S.setNotes(notes);
      S.appendNotes(addnotes2);
      string notes2 = S.getNotesString();
      assertTrue( S.isSetNotes() == true );
      assertTrue(( notes2 == taggednewnotes ));
    }

    public void test_SBase_appendNotesString2()
    {
      string notes = "<html xmlns=\"http://www.w3.org/1999/xhtml\">\n" + "  <head>\n" + 
    "    <title/>\n" + 
    "  </head>\n" + 
    "  <body>\n" + 
    "    <p>This is a test note </p>\n" + 
    "  </body>\n" + 
    "</html>";
      string taggednewnotes = "<notes>\n" + 
    "  <html xmlns=\"http://www.w3.org/1999/xhtml\">\n" + 
    "    <head>\n" + 
    "      <title/>\n" + 
    "    </head>\n" + 
    "    <body>\n" + 
    "      <p>This is a test note </p>\n" + 
    "      <p>This is more test notes </p>\n" + 
    "    </body>\n" + 
    "  </html>\n" + 
    "</notes>";
      string addnotes = "<body xmlns=\"http://www.w3.org/1999/xhtml\">\n" + "  <p>This is more test notes </p>\n" + "</body>\n";
      string addnotes2 = "<notes>\n" + 
    "  <body xmlns=\"http://www.w3.org/1999/xhtml\">\n" + 
    "    <p>This is more test notes </p>\n" + 
    "  </body>\n" + 
    "</notes>";
      S.setNotes(notes);
      S.appendNotes(addnotes);
      string notes1 = S.getNotesString();
      assertTrue( S.isSetNotes() == true );
      assertTrue(( notes1 == taggednewnotes ));
      S.setNotes(notes);
      S.appendNotes(addnotes2);
      string notes2 = S.getNotesString();
      assertTrue( S.isSetNotes() == true );
      assertTrue(( notes2 == taggednewnotes ));
    }

    public void test_SBase_appendNotesString3()
    {
      string notes = "<html xmlns=\"http://www.w3.org/1999/xhtml\">\n" + "  <head>\n" + 
    "    <title/>\n" + 
    "  </head>\n" + 
    "  <body>\n" + 
    "    <p>This is a test note </p>\n" + 
    "  </body>\n" + 
    "</html>";
      string taggednewnotes = "<notes>\n" + 
    "  <html xmlns=\"http://www.w3.org/1999/xhtml\">\n" + 
    "    <head>\n" + 
    "      <title/>\n" + 
    "    </head>\n" + 
    "    <body>\n" + 
    "      <p>This is a test note </p>\n" + 
    "      <p xmlns=\"http://www.w3.org/1999/xhtml\">This is more test notes </p>\n" + 
    "    </body>\n" + 
    "  </html>\n" + 
    "</notes>";
      string taggednewnotes2 = "<notes>\n" + 
    "  <html xmlns=\"http://www.w3.org/1999/xhtml\">\n" + 
    "    <head>\n" + 
    "      <title/>\n" + 
    "    </head>\n" + 
    "    <body>\n" + 
    "      <p>This is a test note </p>\n" + 
    "      <p xmlns=\"http://www.w3.org/1999/xhtml\">This is more test notes 1</p>\n" + 
    "      <p xmlns=\"http://www.w3.org/1999/xhtml\">This is more test notes 2</p>\n" + 
    "    </body>\n" + 
    "  </html>\n" + 
    "</notes>";
      string addnotes =  "<p xmlns=\"http://www.w3.org/1999/xhtml\">This is more test notes </p>\n";
      string addnotes2 = "<p xmlns=\"http://www.w3.org/1999/xhtml\">This is more test notes 1</p>\n" +
			"<p xmlns=\"http://www.w3.org/1999/xhtml\">This is more test notes 2</p>";
      string addnotes3 = "<notes>\n" +
			"  <p xmlns=\"http://www.w3.org/1999/xhtml\">This is more test notes </p>\n" + 
    "</notes>";
      string addnotes4 = "<notes>\n" + "  <p xmlns=\"http://www.w3.org/1999/xhtml\">This is more test notes 1</p>\n" + 
    "  <p xmlns=\"http://www.w3.org/1999/xhtml\">This is more test notes 2</p>\n" + 
    "</notes>";
      S.setNotes(notes);
      S.appendNotes(addnotes);
      string notes1 = S.getNotesString();
      assertTrue( S.isSetNotes() == true );
      assertTrue(( notes1 == taggednewnotes ));
      S.setNotes(notes);
      S.appendNotes(addnotes2);
      string notes2 = S.getNotesString();
      assertTrue( S.isSetNotes() == true );
      assertTrue(( notes2 == taggednewnotes2 ));
      S.setNotes(notes);
      S.appendNotes(addnotes3);
      string notes3 = S.getNotesString();
      assertTrue( S.isSetNotes() == true );
      assertTrue(( notes3 == taggednewnotes ));
      S.setNotes(notes);
      S.appendNotes(addnotes4);
      string notes4 = S.getNotesString();
      assertTrue( S.isSetNotes() == true );
      assertTrue(( notes4 == taggednewnotes2 ));
    }

    public void test_SBase_appendNotesString4()
    {
      string notes = "<body xmlns=\"http://www.w3.org/1999/xhtml\">\n" + "  <p>This is a test note </p>\n" + "</body>";;
      string taggednewnotes = "<notes>\n" + 
    "  <html xmlns=\"http://www.w3.org/1999/xhtml\">\n" + 
    "    <head>\n" + 
    "      <title/>\n" + 
    "    </head>\n" + 
    "    <body>\n" + 
    "      <p>This is a test note </p>\n" + 
    "      <p>This is more test notes </p>\n" + 
    "    </body>\n" + 
    "  </html>\n" + 
    "</notes>";
      string addnotes = "<html xmlns=\"http://www.w3.org/1999/xhtml\">\n" + "  <head>\n" + 
    "    <title/>\n" + 
    "  </head>\n" + 
    "  <body>\n" + 
    "    <p>This is more test notes </p>\n" + 
    "  </body>\n" + 
    "</html>";
      string addnotes2 = "<notes>\n" + 
    "  <html xmlns=\"http://www.w3.org/1999/xhtml\">\n" + 
    "    <head>\n" + 
    "      <title/>\n" + 
    "    </head>\n" + 
    "    <body>\n" + 
    "      <p>This is more test notes </p>\n" + 
    "    </body>\n" + 
    "  </html>\n" + 
    "</notes>";
      S.setNotes(notes);
      S.appendNotes(addnotes);
      string notes1 = S.getNotesString();
      assertTrue( S.isSetNotes() == true );
      assertTrue(( notes1 == taggednewnotes ));
      S.setNotes(notes);
      S.appendNotes(addnotes2);
      string notes2 = S.getNotesString();
      assertTrue( S.isSetNotes() == true );
      assertTrue(( notes2 == taggednewnotes ));
    }

    public void test_SBase_appendNotesString5()
    {
      string notes =  "<p xmlns=\"http://www.w3.org/1999/xhtml\">This is a test note </p>";;
      string taggednewnotes = "<notes>\n" + 
    "  <html xmlns=\"http://www.w3.org/1999/xhtml\">\n" + 
    "    <head>\n" + 
    "      <title/>\n" + 
    "    </head>\n" + 
    "    <body>\n" + 
    "      <p xmlns=\"http://www.w3.org/1999/xhtml\">This is a test note </p>\n" + 
    "      <p>This is more test notes </p>\n" + 
    "    </body>\n" + 
    "  </html>\n" + 
    "</notes>";
      string addnotes = "<html xmlns=\"http://www.w3.org/1999/xhtml\">\n" + "  <head>\n" + 
    "    <title/>\n" + 
    "  </head>\n" + 
    "  <body>\n" + 
    "    <p>This is more test notes </p>\n" + 
    "  </body>\n" + 
    "</html>";
      string addnotes2 = "<notes>\n" + 
    "  <html xmlns=\"http://www.w3.org/1999/xhtml\">\n" + 
    "    <head>\n" + 
    "      <title/>\n" + 
    "    </head>\n" + 
    "    <body>\n" + 
    "      <p>This is more test notes </p>\n" + 
    "    </body>\n" + 
    "  </html>\n" + 
    "</notes>";
      S.setNotes(notes);
      S.appendNotes(addnotes);
      string notes1 = S.getNotesString();
      assertTrue( S.isSetNotes() == true );
      assertTrue(( notes1 == taggednewnotes ));
      S.setNotes(notes);
      S.appendNotes(addnotes2);
      string notes2 = S.getNotesString();
      assertTrue( S.isSetNotes() == true );
      assertTrue(( notes2 == taggednewnotes ));
    }

    public void test_SBase_appendNotesString6()
    {
      string notes = "<body xmlns=\"http://www.w3.org/1999/xhtml\">\n" + "  <p>This is a test note </p>\n" + "</body>";;
      string taggednewnotes = "<notes>\n" + 
    "  <body xmlns=\"http://www.w3.org/1999/xhtml\">\n" + 
    "    <p>This is a test note </p>\n" + 
    "    <p>This is more test notes </p>\n" + 
    "  </body>\n" + 
    "</notes>";
      string addnotes = "<body xmlns=\"http://www.w3.org/1999/xhtml\">\n" + "  <p>This is more test notes </p>\n" + "</body>";
      string addnotes2 = "<notes>\n" + 
    "  <body xmlns=\"http://www.w3.org/1999/xhtml\">\n" + 
    "    <p>This is more test notes </p>\n" + 
    "  </body>\n" + 
    "</notes>";
      S.setNotes(notes);
      S.appendNotes(addnotes);
      string notes1 = S.getNotesString();
      assertTrue( S.isSetNotes() == true );
      assertTrue(( notes1 == taggednewnotes ));
      S.setNotes(notes);
      S.appendNotes(addnotes2);
      string notes2 = S.getNotesString();
      assertTrue( S.isSetNotes() == true );
      assertTrue(( notes2 == taggednewnotes ));
    }

    public void test_SBase_appendNotesString7()
    {
      string notes =  "<p xmlns=\"http://www.w3.org/1999/xhtml\">This is a test note </p>";;
      string taggednewnotes = "<notes>\n" + 
    "  <body xmlns=\"http://www.w3.org/1999/xhtml\">\n" + 
    "    <p xmlns=\"http://www.w3.org/1999/xhtml\">This is a test note </p>\n" + 
    "    <p>This is more test notes </p>\n" + 
    "  </body>\n" + 
    "</notes>";
      string addnotes = "<body xmlns=\"http://www.w3.org/1999/xhtml\">\n" + "  <p>This is more test notes </p>\n" + "</body>";;
      string addnotes2 = "<notes>\n" + 
    "  <body xmlns=\"http://www.w3.org/1999/xhtml\">\n" + 
    "    <p>This is more test notes </p>\n" + 
    "  </body>\n" + 
    "</notes>";
      S.setNotes(notes);
      S.appendNotes(addnotes);
      string notes1 = S.getNotesString();
      assertTrue( S.isSetNotes() == true );
      assertTrue(( notes1 == taggednewnotes ));
      S.setNotes(notes);
      S.appendNotes(addnotes2);
      string notes2 = S.getNotesString();
      assertTrue( S.isSetNotes() == true );
      assertTrue(( notes2 == taggednewnotes ));
    }

    public void test_SBase_appendNotesString8()
    {
      string notes = "<body xmlns=\"http://www.w3.org/1999/xhtml\">\n" + "  <p>This is a test note </p>\n" + "</body>";;
      string taggednewnotes = "<notes>\n" + 
    "  <body xmlns=\"http://www.w3.org/1999/xhtml\">\n" + 
    "    <p>This is a test note </p>\n" + 
    "    <p xmlns=\"http://www.w3.org/1999/xhtml\">This is more test notes </p>\n" + 
    "  </body>\n" + 
    "</notes>";
      string taggednewnotes2 = "<notes>\n" + 
    "  <body xmlns=\"http://www.w3.org/1999/xhtml\">\n" + 
    "    <p>This is a test note </p>\n" + 
    "    <p xmlns=\"http://www.w3.org/1999/xhtml\">This is more test notes 1</p>\n" + 
    "    <p xmlns=\"http://www.w3.org/1999/xhtml\">This is more test notes 2</p>\n" + 
    "  </body>\n" + 
    "</notes>";
      string addnotes =  "<p xmlns=\"http://www.w3.org/1999/xhtml\">This is more test notes </p>";
      string addnotes2 = "<p xmlns=\"http://www.w3.org/1999/xhtml\">This is more test notes 1</p>\n" +
			"<p xmlns=\"http://www.w3.org/1999/xhtml\">This is more test notes 2</p>";
      string addnotes3 = "<notes>\n" + 
    "  <p xmlns=\"http://www.w3.org/1999/xhtml\">This is more test notes </p>\n" + 
    "</notes>";
      string addnotes4 = "<notes>\n" + 
    "  <p xmlns=\"http://www.w3.org/1999/xhtml\">This is more test notes 1</p>\n" + 
    "  <p xmlns=\"http://www.w3.org/1999/xhtml\">This is more test notes 2</p>\n" + 
    "</notes>";
      S.setNotes(notes);
      S.appendNotes(addnotes);
      string notes1 = S.getNotesString();
      assertTrue( S.isSetNotes() == true );
      assertTrue(( notes1 == taggednewnotes ));
      S.setNotes(notes);
      S.appendNotes(addnotes2);
      string notes2 = S.getNotesString();
      assertTrue( S.isSetNotes() == true );
      assertTrue(( notes2 == taggednewnotes2 ));
      S.setNotes(notes);
      S.appendNotes(addnotes3);
      string notes3 = S.getNotesString();
      assertTrue( S.isSetNotes() == true );
      assertTrue(( notes3 == taggednewnotes ));
      S.setNotes(notes);
      S.appendNotes(addnotes4);
      string notes4 = S.getNotesString();
      assertTrue( S.isSetNotes() == true );
      assertTrue(( notes4 == taggednewnotes2 ));
    }

    public void test_SBase_getQualifiersFromResources()
    {
      CVTerm cv = new  CVTerm(libsbml.BIOLOGICAL_QUALIFIER);
      cv.setBiologicalQualifierType(libsbml.BQB_ENCODES);
      cv.addResource( "foo");
      S.setMetaId( "sbase1");
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
      node = new XMLNode(token);
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
      S.setAnnotation((XMLNode)null);
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
      string annotation =  "This is a test note";;
      string taggedannotation =  "<annotation>This is a test note</annotation>";;
      S.setAnnotation(annotation);
      assertTrue( S.isSetAnnotation() == true );
      if (( taggedannotation != S.getAnnotationString() ));
      {
      }
      XMLNode t1 = S.getAnnotation();
      assertTrue( t1.getNumChildren() == 1 );
      assertTrue((  "This is a test note" == t1.getChild(0).getCharacters() ));
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
      XMLNode t2 = t1.getChild(0);
      assertTrue((  "This is a test note" == t2.getCharacters() ));
    }

    public void test_SBase_setMetaId()
    {
      string metaid =  "x12345";;
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
      node = new XMLNode(token);
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
      S.setNotes((XMLNode)null);
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
      string notes =  "This is a test note";;
      string taggednotes =  "<notes>This is a test note</notes>";;
      S.setNotes(notes);
      assertTrue( S.isSetNotes() == true );
      if (( taggednotes != S.getNotesString() ));
      {
      }
      XMLNode t1 = S.getNotes();
      assertTrue( t1.getNumChildren() == 1 );
      assertTrue((  "This is a test note" == t1.getChild(0).getCharacters() ));
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
      XMLNode t2 = t1.getChild(0);
      assertTrue((  "This is a test note" == t2.getCharacters() ));
    }

    public void test_SBase_unsetCVTerms()
    {
      CVTerm cv = new  CVTerm(libsbml.BIOLOGICAL_QUALIFIER);
      cv.setBiologicalQualifierType(libsbml.BQB_ENCODES);
      cv.addResource( "foo");
      S.setMetaId( "sbase1");
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
