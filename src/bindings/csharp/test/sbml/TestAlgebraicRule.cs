/// 
///  @file    TestAlgebraicRule.cs
///  @brief   AlgebraicRule unit tests
///  @author  Frank Bergmann (Csharp conversion)
///  @author  Akiya Jouraku (Csharp conversion)
///  @author  Ben Bornstein 
/// 
///  $Id:$
///  $HeadURL:$
/// 
///  This test file was converted from src/sbml/test/TestAlgebraicRule.c
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

  public class TestAlgebraicRule {
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

    private Rule AR;

    public void setUp()
    {
      AR = new  AlgebraicRule();
      if (AR == null);
      {
      }
    }

    public void tearDown()
    {
      AR = null;
    }

    public void test_AlgebraicRule_create()
    {
      assertTrue( AR.getTypeCode() == libsbml.SBML_ALGEBRAIC_RULE );
      assertTrue( AR.getMetaId() == "" );
      assertTrue( AR.getNotes() == null );
      assertTrue( AR.getAnnotation() == null );
      assertTrue( AR.getFormula() == "" );
      assertTrue( AR.getMath() == null );
    }

    public void test_AlgebraicRule_createWithFormula()
    {
      ASTNode math;
      string formula;
      Rule ar = new  AlgebraicRule("1 + 1");
      assertTrue( ar.getTypeCode() == libsbml.SBML_ALGEBRAIC_RULE );
      assertTrue( ar.getMetaId() == "" );
      math = ar.getMath();
      assertTrue( math != null );
      formula = libsbml.formulaToString(math);
      assertTrue( formula != null );
      assertTrue((  "1 + 1" == formula ));
      assertTrue(( formula == ar.getFormula() ));
      ar = null;
    }

    public void test_AlgebraicRule_createWithLevelVersionAndNamespace()
    {
      XMLNamespaces xmlns = new  XMLNamespaces();
      xmlns.add( "http://www.sbml.org", "sbml");
      Rule r = new  AlgebraicRule(2,3,xmlns);
      assertTrue( r.getTypeCode() == libsbml.SBML_ALGEBRAIC_RULE );
      assertTrue( r.getMetaId() == "" );
      assertTrue( r.getNotes() == null );
      assertTrue( r.getAnnotation() == null );
      assertTrue( r.getLevel() == 2 );
      assertTrue( r.getVersion() == 3 );
      assertTrue( r.getNamespaces() != null );
      assertTrue( r.getNamespaces().getLength() == 1 );
      r = null;
    }

    public void test_AlgebraicRule_createWithMath()
    {
      ASTNode math = libsbml.parseFormula("1 + 1");
      Rule ar = new  AlgebraicRule(math);
      assertTrue( ar.getTypeCode() == libsbml.SBML_ALGEBRAIC_RULE );
      assertTrue( ar.getMetaId() == "" );
      assertTrue((  "1 + 1" == ar.getFormula() ));
      assertTrue( ar.getMath() != math );
      ar = null;
    }

    public void test_AlgebraicRule_free_NULL()
    {
    }

  }
}
