/// 
///  @file    TestEventAssignment.cs
///  @brief   SBML EventAssignment unit tests
///  @author  Frank Bergmann (Csharp conversion)
///  @author  Akiya Jouraku (Csharp conversion)
///  @author  Ben Bornstein 
/// 
///  $Id:$
///  $HeadURL:$
/// 
///  This test file was converted from src/sbml/test/TestEventAssignment.c
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

  public class TestEventAssignment {
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

    private EventAssignment EA;

    public void setUp()
    {
      EA = new  EventAssignment();
      if (EA == null);
      {
      }
    }

    public void tearDown()
    {
      EA = null;
    }

    public void test_EventAssignment_create()
    {
      assertTrue( EA.getTypeCode() == libsbml.SBML_EVENT_ASSIGNMENT );
      assertTrue( EA.getMetaId() == "" );
      assertTrue( EA.getNotes() == null );
      assertTrue( EA.getAnnotation() == null );
      assertTrue( EA.getVariable() == "" );
      assertTrue( EA.getMath() == null );
    }

    public void test_EventAssignment_createWith()
    {
      ASTNode math = libsbml.parseFormula("0");
      EventAssignment ea = new  EventAssignment("k",math);
      assertTrue( ea.getTypeCode() == libsbml.SBML_EVENT_ASSIGNMENT );
      assertTrue( ea.getMetaId() == "" );
      assertTrue( ea.getNotes() == null );
      assertTrue( ea.getAnnotation() == null );
      assertTrue( ea.getMath() != math );
      assertEquals( true, ea.isSetMath() );
      assertTrue((  "k" == ea.getVariable() ));
      assertEquals( true, ea.isSetVariable() );
      math = null;
      ea = null;
    }

    public void test_EventAssignment_createWithLevelVersionAndNamespace()
    {
      XMLNamespaces xmlns = new  XMLNamespaces();
      xmlns.add( "http://www.sbml.org", "sbml");
      EventAssignment object1 = new  EventAssignment(2,1,xmlns);
      assertTrue( object1.getTypeCode() == libsbml.SBML_EVENT_ASSIGNMENT );
      assertTrue( object1.getMetaId() == "" );
      assertTrue( object1.getNotes() == null );
      assertTrue( object1.getAnnotation() == null );
      assertTrue( object1.getLevel() == 2 );
      assertTrue( object1.getVersion() == 1 );
      assertTrue( object1.getNamespaces() != null );
      assertTrue( object1.getNamespaces().getLength() == 1 );
      object1 = null;
    }

    public void test_EventAssignment_free_NULL()
    {
    }

    public void test_EventAssignment_setMath()
    {
      ASTNode math = libsbml.parseFormula("2 * k");
      string formula;
      ASTNode math1;
      EA.setMath(math);
      math1 = EA.getMath();
      assertTrue( math1 != null );
      formula = libsbml.formulaToString(math1);
      assertTrue( formula != null );
      assertTrue((  "2 * k" == formula ));
      assertTrue( EA.getMath() != math );
      assertEquals( true, EA.isSetMath() );
      EA.setMath(EA.getMath());
      math1 = EA.getMath();
      assertTrue( math1 != null );
      formula = libsbml.formulaToString(math1);
      assertTrue( formula != null );
      assertTrue((  "2 * k" == formula ));
      assertTrue( EA.getMath() != math );
      EA.setMath(null);
      assertEquals( false, EA.isSetMath() );
      if (EA.getMath() != null);
      {
      }
      math = null;
    }

    public void test_EventAssignment_setVariable()
    {
      string variable =  "k2";;
      EA.setVariable(variable);
      assertTrue(( variable == EA.getVariable() ));
      assertEquals( true, EA.isSetVariable() );
      if (EA.getVariable() == variable);
      {
      }
      EA.setVariable(EA.getVariable());
      assertTrue(( variable == EA.getVariable() ));
      EA.setVariable("");
      assertEquals( false, EA.isSetVariable() );
      if (EA.getVariable() != null);
      {
      }
    }

  }
}
