/// 
///  @file    TestInitialAssignment.cs
///  @brief   SBML InitialAssignment unit tests
///  @author  Frank Bergmann (Csharp conversion)
///  @author  Akiya Jouraku (Csharp conversion)
///  @author  Sarah Keating 
/// 
///  $Id:$
///  $HeadURL:$
/// 
///  This test file was converted from src/sbml/test/TestInitialAssignment.c
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

  public class TestInitialAssignment {
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

    private InitialAssignment IA;

    public void setUp()
    {
      IA = new  InitialAssignment();
      if (IA == null);
      {
      }
    }

    public void tearDown()
    {
      IA = null;
    }

    public void test_InitialAssignment_create()
    {
      assertTrue( IA.getTypeCode() == libsbml.SBML_INITIAL_ASSIGNMENT );
      assertTrue( IA.getMetaId()== "" == true );
      assertTrue( IA.getNotes() == null );
      assertTrue( IA.getAnnotation() == null );
      assertTrue( IA.getSymbol()== "" == true );
      assertTrue( IA.getMath() == null );
    }

    public void test_InitialAssignment_createWith()
    {
      InitialAssignment ia = new  InitialAssignment("k");
      assertTrue( ia.getTypeCode() == libsbml.SBML_INITIAL_ASSIGNMENT );
      assertTrue( ia.getMetaId()== "" == true );
      assertTrue( ia.getNotes() == null );
      assertTrue( ia.getAnnotation() == null );
      assertEquals( false, ia.isSetMath() );
      assertTrue((  "k" == ia.getSymbol() ));
      assertEquals( true, ia.isSetSymbol() );
      ia = null;
    }

    public void test_InitialAssignment_free_NULL()
    {
    }

    public void test_InitialAssignment_setMath()
    {
      ASTNode math = libsbml.parseFormula("2 * k");
      string formula;
      ASTNode math1;
      IA.setMath(math);
      math1 = IA.getMath();
      assertTrue( math1 != null );
      formula = libsbml.formulaToString(math1);
      assertTrue( formula != null );
      assertTrue((  "2 * k" == formula ));
      assertTrue( IA.getMath() != math );
      assertEquals( true, IA.isSetMath() );
      IA.setMath(IA.getMath());
      math1 = IA.getMath();
      assertTrue( math1 != null );
      formula = libsbml.formulaToString(math1);
      assertTrue( formula != null );
      assertTrue((  "2 * k" == formula ));
      assertTrue( IA.getMath() != math );
      IA.setMath(null);
      assertEquals( false, IA.isSetMath() );
      if (IA.getMath() != null);
      {
      }
      math = null;
    }

    public void test_InitialAssignment_setSymbol()
    {
      string Symbol = "k2";
      IA.setSymbol(Symbol);
      assertTrue(( Symbol == IA.getSymbol() ));
      assertEquals( true, IA.isSetSymbol() );
      if (IA.getSymbol() == Symbol);
      {
      }
      IA.setSymbol(IA.getSymbol());
      assertTrue(( Symbol == IA.getSymbol() ));
      IA.setSymbol("");
      assertEquals( false, IA.isSetSymbol() );
      if (IA.getSymbol() != null);
      {
      }
    }

  }
}
