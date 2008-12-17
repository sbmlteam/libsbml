/// 
///  @file    TestKineticLaw.cs
///  @brief   SBML KineticLaw unit tests
///  @author  Frank Bergmann (Csharp conversion)
///  @author  Akiya Jouraku (Csharp conversion)
///  @author  Ben Bornstein 
/// 
///  $Id$
///  $URL$
/// 
///  This test file was converted from src/sbml/test/TestKineticLaw.c
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

  public class TestKineticLaw {
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

    private KineticLaw KL;

    public void setUp()
    {
      KL = new  KineticLaw();
      if (KL == null);
      {
      }
    }

    public void tearDown()
    {
      KL = null;
    }

    public void test_KineticLaw_addParameter()
    {
      Parameter p = new  Parameter();
      KL.addParameter(p);
      assertTrue( KL.getNumParameters() == 1 );
      p = null;
    }

    public void test_KineticLaw_create()
    {
      assertTrue( KL.getTypeCode() == libsbml.SBML_KINETIC_LAW );
      assertTrue( KL.getMetaId()== "" == true );
      assertTrue( KL.getNotes() == null );
      assertTrue( KL.getAnnotation() == null );
      assertTrue( KL.getFormula()== "" == true );
      assertTrue( KL.getMath() == null );
      assertTrue( KL.getTimeUnits()== "" == true );
      assertTrue( KL.getSubstanceUnits()== "" == true );
      assertEquals( false, KL.isSetFormula() );
      assertEquals( false, KL.isSetMath() );
      assertEquals( false, KL.isSetTimeUnits() );
      assertEquals( false, KL.isSetSubstanceUnits() );
      assertTrue( KL.getNumParameters() == 0 );
    }

    public void test_KineticLaw_createWith()
    {
      ASTNode math;
      string formula;
      KineticLaw kl = new  KineticLaw("k1 * X0");
      assertTrue( kl.getTypeCode() == libsbml.SBML_KINETIC_LAW );
      assertTrue( kl.getMetaId()== "" == true );
      assertTrue( kl.getNotes() == null );
      assertTrue( kl.getAnnotation() == null );
      math = kl.getMath();
      assertTrue( math != null );
      formula = libsbml.formulaToString(math);
      assertTrue( formula != null );
      assertTrue((  "k1 * X0" == formula ));
      assertTrue(( formula == kl.getFormula() ));
      assertEquals( true, kl.isSetMath() );
      assertEquals( true, kl.isSetFormula() );
      assertTrue( kl.getNumParameters() == 0 );
      kl = null;
    }

    public void test_KineticLaw_createWithMath()
    {
      ASTNode math1 = libsbml.parseFormula("k3 / k2");
      ASTNode math;
      string formula;
      KineticLaw kl = new  KineticLaw(math1);
      assertTrue( kl.getTypeCode() == libsbml.SBML_KINETIC_LAW );
      assertTrue( kl.getMetaId()== "" == true );
      assertTrue( kl.getNotes() == null );
      assertTrue( kl.getAnnotation() == null );
      math = kl.getMath();
      assertTrue( math != null );
      formula = libsbml.formulaToString(math);
      assertTrue( formula != null );
      assertTrue((  "k3 / k2" == formula ));
      assertTrue(( formula == kl.getFormula() ));
      assertEquals( true, kl.isSetMath() );
      assertEquals( true, kl.isSetFormula() );
      assertEquals( false, kl.isSetTimeUnits() );
      assertEquals( false, kl.isSetSubstanceUnits() );
      assertTrue( kl.getNumParameters() == 0 );
      kl = null;
    }

    public void test_KineticLaw_free_NULL()
    {
    }

    public void test_KineticLaw_getParameter()
    {
      Parameter k1 = new  Parameter();
      Parameter k2 = new  Parameter();
      k1.setName( "k1");
      k2.setName( "k2");
      k1.setValue(3.14);
      k2.setValue(2.72);
      KL.addParameter(k1);
      KL.addParameter(k2);
      k1 = null;
      k2 = null;
      assertTrue( KL.getNumParameters() == 2 );
      k1 = KL.getParameter(0);
      k2 = KL.getParameter(1);
      assertTrue((  "k1" == k1.getName() ));
      assertTrue((  "k2" == k2.getName() ));
      assertTrue( k1.getValue() == 3.14 );
      assertTrue( k2.getValue() == 2.72 );
    }

    public void test_KineticLaw_getParameterById()
    {
      Parameter k1 = new  Parameter();
      Parameter k2 = new  Parameter();
      k1.setId( "k1");
      k2.setId( "k2");
      k1.setValue(3.14);
      k2.setValue(2.72);
      KL.addParameter(k1);
      KL.addParameter(k2);
      k1 = null;
      k2 = null;
      assertTrue( KL.getNumParameters() == 2 );
      k1 = KL.getParameter( "k1");
      k2 = KL.getParameter( "k2");
      assertTrue((  "k1" == k1.getId() ));
      assertTrue((  "k2" == k2.getId() ));
      assertTrue( k1.getValue() == 3.14 );
      assertTrue( k2.getValue() == 2.72 );
    }

    public void test_KineticLaw_setBadFormula()
    {
      string formula = "k1 X0";
      KL.setFormula(formula);
      assertEquals( true, KL.isSetFormula() );
      assertEquals( false, KL.isSetMath() );
    }

    public void test_KineticLaw_setFormula()
    {
      string formula = "k1*X0";
      KL.setFormula(formula);
      assertTrue(( formula == KL.getFormula() ));
      assertEquals( true, KL.isSetFormula() );
      if (KL.getFormula() == formula);
      {
      }
      KL.setFormula(KL.getFormula());
      assertTrue(( formula == KL.getFormula() ));
      KL.setFormula("");
      assertEquals( false, KL.isSetFormula() );
      if (KL.getFormula() != null);
      {
      }
    }

    public void test_KineticLaw_setFormulaFromMath()
    {
      ASTNode math = libsbml.parseFormula("k1 * X0");
      assertEquals( false, KL.isSetMath() );
      assertEquals( false, KL.isSetFormula() );
      KL.setMath(math);
      assertEquals( true, KL.isSetMath() );
      assertEquals( true, KL.isSetFormula() );
      assertTrue((  "k1 * X0" == KL.getFormula() ));
      math = null;
    }

    public void test_KineticLaw_setMath()
    {
      ASTNode math = libsbml.parseFormula("k3 / k2");
      string formula;
      ASTNode math1;
      KL.setMath(math);
      math1 = KL.getMath();
      assertTrue( math1 != null );
      formula = libsbml.formulaToString(math1);
      assertTrue( formula != null );
      assertTrue((  "k3 / k2" == formula ));
      assertTrue( KL.getMath() != math );
      assertEquals( true, KL.isSetMath() );
      KL.setMath(KL.getMath());
      math1 = KL.getMath();
      assertTrue( math1 != null );
      formula = libsbml.formulaToString(math1);
      assertTrue( formula != null );
      assertTrue((  "k3 / k2" == formula ));
      assertTrue( KL.getMath() != math );
      KL.setMath(null);
      assertEquals( false, KL.isSetMath() );
      if (KL.getMath() != null);
      {
      }
      math = null;
    }

    public void test_KineticLaw_setMathFromFormula()
    {
      string formula = "k3 / k2";
      assertEquals( false, KL.isSetMath() );
      assertEquals( false, KL.isSetFormula() );
      KL.setFormula(formula);
      assertEquals( true, KL.isSetMath() );
      assertEquals( true, KL.isSetFormula() );
      formula = libsbml.formulaToString(KL.getMath());
      assertTrue((  "k3 / k2" == formula ));
    }

  }
}
