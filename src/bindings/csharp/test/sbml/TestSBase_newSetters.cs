/// 
///  @file    TestSBase_newSetters.cs
///  @brief   SBase unit tests for new set API
///  @author  Frank Bergmann (Csharp conversion)
///  @author  Akiya Jouraku (Csharp conversion)
///  @author  Sarah Keating 
/// 
///  $Id$
///  $HeadURL$
/// 
///  This test file was converted from src/sbml/test/TestSBase_newSetters.cpp
///  with the help of conversion sciprt (ctest_converter.pl).
/// 
/// <!---------------------------------------------------------------------------
///  This file is part of libSBML.  Please visit http://sbml.org for more
///  information about SBML, and the latest version of libSBML.
/// 
///  Copyright 2005-2010 California Institute of Technology.
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

  public class TestSBase_newSetters {

    static int INT_MAX = 2147483647;

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
      else if ( (a == null) || (b == null) )
      {
        throw new AssertionError();
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
      else if ( (a == null) || (b == null) )
      {
        return;
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
      S = new Model(2,4);
      if (S == null);
      {
      }
    }

    public void tearDown()
    {
    }

    public void test_SBase_addCVTerms()
    {
      CVTerm cv = new  CVTerm(libsbml.BIOLOGICAL_QUALIFIER);
      cv.setBiologicalQualifierType(libsbml.BQB_ENCODES);
      cv.addResource( "foo");
      long i = S.addCVTerm(cv);
      assertTrue( i == libsbml.LIBSBML_UNEXPECTED_ATTRIBUTE );
      assertTrue( S.getNumCVTerms() == 0 );
      S.setMetaId( "_id");
      i = S.addCVTerm(cv);
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertTrue( S.getNumCVTerms() == 1 );
      assertTrue( S.getCVTerms() != null );
      i = S.addCVTerm(null);
      assertTrue( i == libsbml.LIBSBML_OPERATION_FAILED );
      assertTrue( S.getNumCVTerms() == 1 );
      assertTrue( S.getCVTerms() != null );
      CVTerm cv2 = new  CVTerm(libsbml.MODEL_QUALIFIER);
      i = S.addCVTerm(cv2);
      assertTrue( i == libsbml.LIBSBML_INVALID_OBJECT );
      assertTrue( S.getNumCVTerms() == 1 );
      assertTrue( S.getCVTerms() != null );
      cv = null;
      cv2 = null;
    }

    public void test_SBase_appendAnnotation()
    {
      XMLToken token;
      XMLNode node;
      XMLToken token1;
      XMLNode node1;
      token = new  XMLToken("This is a test note");
      node = new XMLNode(token);
      token1 = new  XMLToken("This is additional");
      node1 = new XMLNode(token1);
      long i = S.setAnnotation(node);
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      i = S.appendAnnotation(node1);
      XMLNode t1 = S.getAnnotation();
      assertTrue( t1.getNumChildren() == 2 );
      assertTrue((     "This is a test note" == t1.getChild(0).getCharacters() ));
      assertTrue((     "This is additional" == t1.getChild(1).getCharacters() ));
    }

    public void test_SBase_appendAnnotationString()
    {
      XMLToken token;
      XMLNode node;
      token = new  XMLToken("This is a test note");
      node = new XMLNode(token);
      long i = S.setAnnotation(node);
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      i = S.appendAnnotation( "This is additional");
      XMLNode t1 = S.getAnnotation();
      assertTrue( t1.getNumChildren() == 2 );
      assertTrue((     "This is a test note" == t1.getChild(0).getCharacters() ));
      XMLNode c1 = t1.getChild(1);
      assertTrue( c1.getNumChildren() == 0 );
      assertTrue((  "This is additional" == c1.getCharacters() ));
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
      long i = S.setNotes(node);
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertTrue( S.isSetNotes() == true );
      token1 = new  XMLToken(triple,att,ns);
      node1 = new XMLNode(token1);
      node1.addChild(node5);
      i = S.appendNotes(node1);
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
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
      XMLTriple title_triple = new  XMLTriple("title", "", "");
      XMLTriple body_triple = new  XMLTriple("body", "", "");
      XMLTriple p_triple = new  XMLTriple("p", "", "");
      XMLToken html_token = new  XMLToken(html_triple,att,ns);
      XMLToken head_token = new  XMLToken(head_triple,att);
      XMLToken title_token = new  XMLToken(title_triple,att);
      XMLToken body_token = new  XMLToken(body_triple,att);
      XMLToken p_token = new  XMLToken(p_triple,att);
      XMLToken text_token = new  XMLToken("This is my text");
      XMLNode html_node = new XMLNode(html_token);
      XMLNode head_node = new XMLNode(head_token);
      XMLNode title_node = new XMLNode(title_token);
      XMLNode body_node = new XMLNode(body_token);
      XMLNode p_node = new XMLNode(p_token);
      XMLNode text_node = new XMLNode(text_token);
      XMLToken text_token1 = new  XMLToken("This is more text");
      XMLNode html_node1 = new XMLNode(html_token);
      XMLNode head_node1 = new XMLNode(head_token);
      XMLNode title_node1 = new XMLNode(title_token);
      XMLNode body_node1 = new XMLNode(body_token);
      XMLNode p_node1 = new XMLNode(p_token);
      XMLNode text_node1 = new XMLNode(text_token1);
      XMLNode notes;
      XMLNode child,child1;
      p_node.addChild(text_node);
      body_node.addChild(p_node);
      head_node.addChild(title_node);
      html_node.addChild(head_node);
      html_node.addChild(body_node);
      p_node1.addChild(text_node1);
      body_node1.addChild(p_node1);
      head_node1.addChild(title_node1);
      html_node1.addChild(head_node1);
      html_node1.addChild(body_node1);
      long i = S.setNotes(html_node);
      i = S.appendNotes(html_node1);
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
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
      XMLTriple title_triple = new  XMLTriple("title", "", "");
      XMLTriple body_triple = new  XMLTriple("body", "", "");
      XMLTriple p_triple = new  XMLTriple("p", "", "");
      XMLToken html_token = new  XMLToken(html_triple,att,ns);
      XMLToken head_token = new  XMLToken(head_triple,att);
      XMLToken title_token = new  XMLToken(title_triple,att);
      XMLToken body_token = new  XMLToken(body_triple,att);
      XMLToken p_token = new  XMLToken(p_triple,att);
      XMLToken text_token = new  XMLToken("This is my text");
      XMLNode html_node = new XMLNode(html_token);
      XMLNode head_node = new XMLNode(head_token);
      XMLNode title_node = new XMLNode(title_token);
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
      head_node.addChild(title_node);
      html_node.addChild(head_node);
      html_node.addChild(body_node);
      p_node1.addChild(text_node1);
      body_node1.addChild(p_node1);
      long i = S.setNotes(html_node);
      i = S.appendNotes(body_node1);
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
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
      XMLTriple title_triple = new  XMLTriple("title", "", "");
      XMLTriple body_triple = new  XMLTriple("body", "", "");
      XMLTriple p_triple = new  XMLTriple("p", "", "");
      XMLToken html_token = new  XMLToken(html_triple,att,ns);
      XMLToken head_token = new  XMLToken(head_triple,att);
      XMLToken title_token = new  XMLToken(title_triple,att);
      XMLToken body_token = new  XMLToken(body_triple,att);
      XMLToken p_token = new  XMLToken(p_triple,att);
      XMLToken text_token = new  XMLToken("This is my text");
      XMLNode html_node = new XMLNode(html_token);
      XMLNode head_node = new XMLNode(head_token);
      XMLNode title_node = new XMLNode(title_token);
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
      head_node.addChild(title_node);
      html_node.addChild(head_node);
      html_node.addChild(body_node);
      p_node1.addChild(text_node1);
      long i = S.setNotes(html_node);
      i = S.appendNotes(p_node1);
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
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
      XMLTriple title_triple = new  XMLTriple("title", "", "");
      XMLTriple body_triple = new  XMLTriple("body", "", "");
      XMLTriple p_triple = new  XMLTriple("p", "", "");
      XMLToken html_token = new  XMLToken(html_triple,att,ns);
      XMLToken head_token = new  XMLToken(head_triple,att);
      XMLToken title_token = new  XMLToken(title_triple,att);
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
      XMLNode title_node1 = new XMLNode(title_token);
      XMLNode body_node1 = new XMLNode(body_token);
      XMLNode p_node1 = new XMLNode(p_token);
      XMLNode text_node1 = new XMLNode(text_token1);
      XMLNode notes;
      XMLNode child,child1;
      p_node.addChild(text_node);
      body_node.addChild(p_node);
      p_node1.addChild(text_node1);
      body_node1.addChild(p_node1);
      head_node1.addChild(title_node1);
      html_node1.addChild(head_node1);
      html_node1.addChild(body_node1);
      long i = S.setNotes(body_node);
      i = S.appendNotes(html_node1);
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
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
      XMLTriple title_triple = new  XMLTriple("title", "", "");
      XMLTriple body_triple = new  XMLTriple("body", "", "");
      XMLTriple p_triple = new  XMLTriple("p", "", "");
      XMLToken html_token = new  XMLToken(html_triple,att,ns);
      XMLToken head_token = new  XMLToken(head_triple,att);
      XMLToken title_token = new  XMLToken(title_triple,att);
      XMLToken body_token = new  XMLToken(body_triple,att);
      XMLToken p_token = new  XMLToken(p_triple,att);
      XMLToken p_token1 = new  XMLToken(p_triple,att,ns);
      XMLToken text_token = new  XMLToken("This is my text");
      XMLNode p_node = new XMLNode(p_token1);
      XMLNode text_node = new XMLNode(text_token);
      XMLToken text_token1 = new  XMLToken("This is more text");
      XMLNode html_node1 = new XMLNode(html_token);
      XMLNode head_node1 = new XMLNode(head_token);
      XMLNode title_node1 = new XMLNode(title_token);
      XMLNode body_node1 = new XMLNode(body_token);
      XMLNode p_node1 = new XMLNode(p_token);
      XMLNode text_node1 = new XMLNode(text_token1);
      XMLNode notes;
      XMLNode child,child1;
      p_node.addChild(text_node);
      p_node1.addChild(text_node1);
      body_node1.addChild(p_node1);
      head_node1.addChild(title_node1);
      html_node1.addChild(head_node1);
      html_node1.addChild(body_node1);
      long i = S.setNotes(p_node);
      i = S.appendNotes(html_node1);
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
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
      long i = S.setNotes(body_node);
      i = S.appendNotes(body_node1);
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
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
      long i = S.setNotes(p_node);
      i = S.appendNotes(body_node1);
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
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
      long i = S.setNotes(body_node);
      i = S.appendNotes(p_node1);
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
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
      string taggednotes = "<notes>\n" + "  <p xmlns=\"http://www.w3.org/1999/xhtml\">This is a test note </p>\n" + "</notes>";;
      string taggednewnotes = "<notes>\n" + "  <p xmlns=\"http://www.w3.org/1999/xhtml\">This is a test note </p>\n" + 
    "  <p xmlns=\"http://www.w3.org/1999/xhtml\">This is more test notes </p>\n" + 
    "</notes>";
      string badnotes =  "<notes>This is a test note</notes>";;
      string newnotes =  "<p xmlns=\"http://www.w3.org/1999/xhtml\">This is more test notes </p>";;
      long i = S.setNotes(notes);
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertTrue( S.isSetNotes() == true );
      i = S.appendNotes(badnotes);
      string notes1 = S.getNotesString();
      assertTrue( i == libsbml.LIBSBML_INVALID_OBJECT );
      assertTrue( S.isSetNotes() == true );
      assertTrue(( notes1 == taggednotes ));
      i = S.appendNotes(newnotes);
      notes1 = S.getNotesString();
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertTrue( S.isSetNotes() == true );
      assertTrue(( notes1 == taggednewnotes ));
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
      long i = S.setNotes(notes);
      i = S.appendNotes(addnotes);
      string notes1 = S.getNotesString();
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertTrue( S.isSetNotes() == true );
      assertTrue(( notes1 == taggednewnotes ));
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
      string addnotes = "<body xmlns=\"http://www.w3.org/1999/xhtml\">\n" + "  <p>This is more test notes </p>\n" + "</body>\n";;
      long i = S.setNotes(notes);
      i = S.appendNotes(addnotes);
      string notes1 = S.getNotesString();
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertTrue( S.isSetNotes() == true );
      assertTrue(( notes1 == taggednewnotes ));
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
      string addnotes =  "<p xmlns=\"http://www.w3.org/1999/xhtml\">This is more test notes </p>";;
      long i = S.setNotes(notes);
      i = S.appendNotes(addnotes);
      string notes1 = S.getNotesString();
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertTrue( S.isSetNotes() == true );
      assertTrue(( notes1 == taggednewnotes ));
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
      long i = S.setNotes(notes);
      i = S.appendNotes(addnotes);
      string notes1 = S.getNotesString();
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertTrue( S.isSetNotes() == true );
      assertTrue(( notes1 == taggednewnotes ));
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
      long i = S.setNotes(notes);
      i = S.appendNotes(addnotes);
      string notes1 = S.getNotesString();
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertTrue( S.isSetNotes() == true );
      assertTrue(( notes1 == taggednewnotes ));
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
      string addnotes = "<body xmlns=\"http://www.w3.org/1999/xhtml\">\n" + "  <p>This is more test notes </p>\n" + "</body>";;
      long i = S.setNotes(notes);
      i = S.appendNotes(addnotes);
      string notes1 = S.getNotesString();
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertTrue( S.isSetNotes() == true );
      assertTrue(( notes1 == taggednewnotes ));
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
      long i = S.setNotes(notes);
      i = S.appendNotes(addnotes);
      string notes1 = S.getNotesString();
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertTrue( S.isSetNotes() == true );
      assertTrue(( notes1 == taggednewnotes ));
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
      string addnotes =  "<p xmlns=\"http://www.w3.org/1999/xhtml\">This is more test notes </p>";;
      long i = S.setNotes(notes);
      i = S.appendNotes(addnotes);
      string notes1 = S.getNotesString();
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertTrue( S.isSetNotes() == true );
      assertTrue(( notes1 == taggednewnotes ));
    }

    public void test_SBase_setAnnotation()
    {
      XMLToken token;
      XMLNode node;
      token = new  XMLToken("This is a test note");
      node = new XMLNode(token);
      long i = S.setAnnotation(node);
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertTrue( S.isSetAnnotation() == true );
      i = S.unsetAnnotation();
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      S.unsetAnnotation();
      assertTrue( S.isSetAnnotation() == false );
      i = S.setAnnotation(node);
      i = S.setAnnotation((XMLNode)null);
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertTrue( S.isSetAnnotation() == false );
    }

    public void test_SBase_setAnnotationString()
    {
      string annotation =  "This is a test note";;
      string taggedannotation =  "<annotation>This is a test note</annotation>";;
      long i = S.setAnnotation(annotation);
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertTrue( S.isSetAnnotation() == true );
      i = S.setAnnotation( "");
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertTrue( S.isSetAnnotation() == false );
      i = S.setAnnotation(taggedannotation);
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertTrue( S.isSetAnnotation() == true );
      i = S.unsetAnnotation();
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertTrue( S.isSetAnnotation() == false );
    }

    public void test_SBase_setMetaId1()
    {
      SBase c = new Compartment(1,2);
      long i = c.setMetaId( "cell");
      assertTrue( i == libsbml.LIBSBML_UNEXPECTED_ATTRIBUTE );
      assertEquals( false, c.isSetMetaId() );
      c = null;
    }

    public void test_SBase_setMetaId2()
    {
      long i = S.setMetaId( "1cell");
      assertTrue( i == libsbml.LIBSBML_INVALID_ATTRIBUTE_VALUE );
      assertEquals( false, S.isSetMetaId() );
      i = S.unsetMetaId();
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertEquals( false, S.isSetMetaId() );
    }

    public void test_SBase_setMetaId3()
    {
      long i = S.setMetaId( "cell");
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertEquals( true, S.isSetMetaId() );
      assertTrue((  "cell"  == S.getMetaId() ));
      i = S.unsetMetaId();
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertEquals( false, S.isSetMetaId() );
    }

    public void test_SBase_setMetaId4()
    {
      long i = S.setMetaId( "cell");
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertEquals( true, S.isSetMetaId() );
      assertTrue((  "cell"  == S.getMetaId() ));
      i = S.setMetaId("");
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertEquals( false, S.isSetMetaId() );
    }

    public void test_SBase_setNamespaces()
    {
      XMLNamespaces ns = new XMLNamespaces();
      ns.add("url", "name");
      long i = S.setNamespaces(ns);
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertTrue( (S).getNamespaces().getLength() == 1 );
      i = S.setNamespaces(null);
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertTrue( (S).getNamespaces() == null );
    }

    public void test_SBase_setNotes()
    {
      XMLToken token;
      XMLNode node;
      XMLTriple triple = new  XMLTriple("p", "", "");
      XMLAttributes att = new  XMLAttributes();
      XMLNamespaces ns = new  XMLNamespaces();
      ns.add( "http://www.w3.org/1999/xhtml", "");
      XMLToken tt = new  XMLToken("This is my text");
      XMLNode n1 = new XMLNode(tt);
      token = new  XMLToken(triple,att,ns);
      node = new XMLNode(token);
      node.addChild(n1);
      long i = S.setNotes(node);
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertTrue( S.isSetNotes() == true );
      i = S.unsetNotes();
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertTrue( ! (S.isSetNotes() == true) );
      token = new  XMLToken("This is a test note");
      node = new XMLNode(token);
      i = S.setNotes(node);
      assertTrue( i == libsbml.LIBSBML_INVALID_OBJECT );
      assertTrue( ! (S.isSetNotes() == true) );
      token = new  XMLToken(triple,att,ns);
      node = new XMLNode(token);
      node.addChild(n1);
      i = S.setNotes(node);
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertTrue( S.isSetNotes() == true );
      i = S.setNotes((XMLNode)null);
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertTrue( ! (S.isSetNotes() == true) );
      node = null;
    }

    public void test_SBase_setNotes1()
    {
      XMLAttributes att = new  XMLAttributes();
      XMLNamespaces ns = new  XMLNamespaces();
      ns.add( "http://www.w3.org/1999/xhtml", "");
      XMLTriple html_triple = new  XMLTriple("html", "", "");
      XMLTriple head_triple = new  XMLTriple("head", "", "");
      XMLTriple title_triple = new  XMLTriple("title", "", "");
      XMLTriple body_triple = new  XMLTriple("body", "", "");
      XMLTriple p_triple = new  XMLTriple("p", "", "");
      XMLToken html_token = new  XMLToken(html_triple,att,ns);
      XMLToken head_token = new  XMLToken(head_triple,att);
      XMLToken title_token = new  XMLToken(title_triple,att);
      XMLToken body_token = new  XMLToken(body_triple,att);
      XMLToken p_token = new  XMLToken(p_triple,att);
      XMLToken text_token = new  XMLToken("This is my text");
      XMLNode html_node = new XMLNode(html_token);
      XMLNode head_node = new XMLNode(head_token);
      XMLNode title_node = new XMLNode(title_token);
      XMLNode body_node = new XMLNode(body_token);
      XMLNode p_node = new XMLNode(p_token);
      XMLNode text_node = new XMLNode(text_token);
      XMLNode notes;
      XMLNode child;
      p_node.addChild(text_node);
      body_node.addChild(p_node);
      head_node.addChild(title_node);
      html_node.addChild(head_node);
      html_node.addChild(body_node);
      long i = S.setNotes(html_node);
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      notes = S.getNotes();
      assertTrue((  "notes" == notes.getName() ));
      assertTrue( notes.getNumChildren() == 1 );
      child = notes.getChild(0);
      assertTrue((  "html" == child.getName() ));
      assertTrue( child.getNumChildren() == 2 );
      child = child.getChild(1);
      assertTrue((  "body" == child.getName() ));
      assertTrue( child.getNumChildren() == 1 );
      child = child.getChild(0);
      assertTrue((  "p" == child.getName() ));
      assertTrue( child.getNumChildren() == 1 );
      child = child.getChild(0);
      assertTrue((  "This is my text" == child.getCharacters() ));
      assertTrue( child.getNumChildren() == 0 );
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
      html_node = null;
      head_node = null;
      body_node = null;
      p_node = null;
      text_node = null;
    }

    public void test_SBase_setNotes2()
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
      XMLNode notes;
      XMLNode child;
      p_node.addChild(text_node);
      body_node.addChild(p_node);
      long i = S.setNotes(body_node);
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      notes = S.getNotes();
      assertTrue((  "notes" == notes.getName() ));
      assertTrue( notes.getNumChildren() == 1 );
      child = notes.getChild(0);
      assertTrue((  "body" == child.getName() ));
      assertTrue( child.getNumChildren() == 1 );
      child = child.getChild(0);
      assertTrue((  "p" == child.getName() ));
      assertTrue( child.getNumChildren() == 1 );
      child = child.getChild(0);
      assertTrue((  "This is my text" == child.getCharacters() ));
      assertTrue( child.getNumChildren() == 0 );
      att = null;
      ns = null;
      body_triple = null;
      p_triple = null;
      body_token = null;
      p_token = null;
      text_token = null;
      body_node = null;
      p_node = null;
      text_node = null;
    }

    public void test_SBase_setNotes3()
    {
      XMLAttributes att = new  XMLAttributes();
      XMLNamespaces ns = new  XMLNamespaces();
      ns.add( "http://www.w3.org/1999/xhtml", "");
      XMLTriple p_triple = new  XMLTriple("p", "", "");
      XMLToken p_token = new  XMLToken(p_triple,att,ns);
      XMLToken text_token = new  XMLToken("This is my text");
      XMLNode p_node = new XMLNode(p_token);
      XMLNode text_node = new XMLNode(text_token);
      XMLNode notes;
      XMLNode child;
      p_node.addChild(text_node);
      long i = S.setNotes(p_node);
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      notes = S.getNotes();
      assertTrue((  "notes" == notes.getName() ));
      assertTrue( notes.getNumChildren() == 1 );
      child = notes.getChild(0);
      assertTrue((  "p" == child.getName() ));
      assertTrue( child.getNumChildren() == 1 );
      child = child.getChild(0);
      assertTrue((  "This is my text" == child.getCharacters() ));
      assertTrue( child.getNumChildren() == 0 );
      att = null;
      ns = null;
      p_triple = null;
      p_token = null;
      text_token = null;
      p_node = null;
      text_node = null;
    }

    public void test_SBase_setNotesString()
    {
      string notes =  "<p xmlns=\"http://www.w3.org/1999/xhtml\">This is a test note </p>";;
      string taggednotes =  "<notes><p xmlns=\"http://www.w3.org/1999/xhtml\">This is a test note </p></notes>";;
      string badnotes =  "<notes>This is a test note</notes>";;
      long i = S.setNotes(notes);
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertTrue( S.isSetNotes() == true );
      i = S.unsetNotes();
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertTrue( ! (S.isSetNotes() == true) );
      i = S.setNotes(taggednotes);
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertTrue( S.isSetNotes() == true );
      i = S.setNotes((XMLNode)null);
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertTrue( ! (S.isSetNotes() == true) );
      i = S.setNotes(badnotes);
      assertTrue( i == libsbml.LIBSBML_INVALID_OBJECT );
      assertTrue( ! (S.isSetNotes() == true) );
    }

    public void test_SBase_setSBOTerm1()
    {
      SBase c = new Compartment(1,2);
      long i = c.setSBOTerm(2);
      assertTrue( i == libsbml.LIBSBML_UNEXPECTED_ATTRIBUTE );
      assertEquals( false, c.isSetSBOTerm() );
      c = null;
    }

    public void test_SBase_setSBOTerm2()
    {
      long i = S.setSBOTerm(5);
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertEquals( true, S.isSetSBOTerm() );
      assertTrue( S.getSBOTerm() == 5 );
      assertTrue( (  "SBO:0000005" != S.getSBOTermID() ) == false );
      i = S.unsetSBOTerm();
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertEquals( false, S.isSetSBOTerm() );
      i = S.setSBOTerm(0);
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertEquals( true, S.isSetSBOTerm() );
      assertTrue( S.getSBOTerm() == 0 );
      assertTrue( (  "SBO:0000000" != S.getSBOTermID() ) == false );
      i = S.setSBOTerm(9999999);
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertEquals( true, S.isSetSBOTerm() );
      assertTrue( S.getSBOTerm() == 9999999 );
      assertTrue( (  "SBO:9999999" != S.getSBOTermID() ) == false );
      i = S.setSBOTerm( "SBO:0000005");
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertEquals( true, S.isSetSBOTerm() );
      assertTrue( S.getSBOTerm() == 5 );
      assertTrue( (  "SBO:0000005" != S.getSBOTermID() ) == false );
      i = S.unsetSBOTerm();
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertEquals( false, S.isSetSBOTerm() );
      i = S.setSBOTerm( "SBO:0000000");
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertEquals( true, S.isSetSBOTerm() );
      assertTrue( S.getSBOTerm() == 0 );
      assertTrue( (  "SBO:0000000" != S.getSBOTermID() ) == false );
      i = S.setSBOTerm( "SBO:9999999");
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertEquals( true, S.isSetSBOTerm() );
      assertTrue( S.getSBOTerm() == 9999999 );
      assertTrue( (  "SBO:9999999" != S.getSBOTermID() ) == false );
      i = S.setSBOTerm(INT_MAX);
      assertTrue( i == libsbml.LIBSBML_INVALID_ATTRIBUTE_VALUE );
      assertEquals( false, S.isSetSBOTerm() );
      assertTrue( S.getSBOTermID() == "" );
      i = S.setSBOTerm(-1);
      assertTrue( i == libsbml.LIBSBML_INVALID_ATTRIBUTE_VALUE );
      assertEquals( false, S.isSetSBOTerm() );
      assertTrue( S.getSBOTermID() == "" );
      i = S.setSBOTerm(10000000);
      assertTrue( i == libsbml.LIBSBML_INVALID_ATTRIBUTE_VALUE );
      assertEquals( false, S.isSetSBOTerm() );
      assertTrue( S.getSBOTermID() == "" );
    }

    public void test_SBase_unsetCVTerms()
    {
      CVTerm cv = new  CVTerm(libsbml.BIOLOGICAL_QUALIFIER);
      cv.setBiologicalQualifierType(libsbml.BQB_ENCODES);
      cv.addResource( "foo");
      S.setMetaId( "_id");
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
      long i = S.unsetCVTerms();
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertTrue( S.getNumCVTerms() == 0 );
      assertTrue( S.getCVTerms() == null );
      cv = null;
      cv2 = null;
      cv1 = null;
      cv4 = null;
    }

  }
}
