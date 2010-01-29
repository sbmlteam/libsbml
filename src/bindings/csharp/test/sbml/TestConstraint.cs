/// 
///  @file    TestConstraint.cs
///  @brief   SBML Constraint unit tests
///  @author  Frank Bergmann (Csharp conversion)
///  @author  Akiya Jouraku (Csharp conversion)
///  @author  Sarah Keating 
/// 
///  $Id$
///  $HeadURL$
/// 
///  This test file was converted from src/sbml/test/TestConstraint.c
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

  public class TestConstraint {
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

    private Constraint C;

    public void setUp()
    {
      C = new  Constraint(2,4);
      if (C == null);
      {
      }
    }

    public void tearDown()
    {
      C = null;
    }

    public void test_Constraint_create()
    {
      assertTrue( C.getTypeCode() == libsbml.SBML_CONSTRAINT );
      assertTrue( C.getMetaId() == "" );
      assertTrue( C.getNotes() == null );
      assertTrue( C.getAnnotation() == null );
      assertEquals( false, C.isSetMessage() );
      assertEquals( false, C.isSetMath() );
    }

    public void test_Constraint_createWithNS()
    {
      XMLNamespaces xmlns = new  XMLNamespaces();
      xmlns.add( "http://www.sbml.org", "testsbml");
      SBMLNamespaces sbmlns = new  SBMLNamespaces(2,2);
      sbmlns.addNamespaces(xmlns);
      Constraint object1 = new  Constraint(sbmlns);
      assertTrue( object1.getTypeCode() == libsbml.SBML_CONSTRAINT );
      assertTrue( object1.getMetaId() == "" );
      assertTrue( object1.getNotes() == null );
      assertTrue( object1.getAnnotation() == null );
      assertTrue( object1.getLevel() == 2 );
      assertTrue( object1.getVersion() == 2 );
      assertTrue( object1.getNamespaces() != null );
      assertTrue( object1.getNamespaces().getLength() == 2 );
      object1 = null;
    }

    public void test_Constraint_free_NULL()
    {
    }

    public void test_Constraint_setMath()
    {
      ASTNode math = libsbml.parseFormula("2 * k");
      C.setMath(math);
      assertTrue( C.getMath() != math );
      assertEquals( true, C.isSetMath() );
      C.setMath(C.getMath());
      assertTrue( C.getMath() != math );
      C.setMath(null);
      assertEquals( false, C.isSetMath() );
      if (C.getMath() != null);
      {
      }
      math = null;
    }

    public void test_Constraint_setMessage()
    {
      XMLNode text = XMLNode.convertStringToXMLNode(" Some text ",null);
      XMLTriple triple = new  XMLTriple("p", "http://www.w3.org/1999/xhtml", "");
      XMLAttributes att = new  XMLAttributes();
      XMLNamespaces xmlns = new  XMLNamespaces();
      xmlns.add( "http://www.w3.org/1999/xhtml", "");
      XMLNode p = new XMLNode(triple,att,xmlns);
      p.addChild(text);
      XMLTriple triple1 = new  XMLTriple("message", "", "");
      XMLAttributes att1 = new  XMLAttributes();
      XMLNode node = new XMLNode(triple1,att1);
      node.addChild(p);
      C.setMessage(node);
      assertTrue( C.getMessage() != node );
      assertTrue( C.isSetMessage() == true );
      C.setMessage(C.getMessage());
      assertTrue( C.getMessage() != node );
      assertTrue( C.getMessageString() != null );
      C.unsetMessage();
      assertEquals( false, C.isSetMessage() );
      if (C.getMessage() != null);
      {
      }
      node = null;
    }

  }
}
