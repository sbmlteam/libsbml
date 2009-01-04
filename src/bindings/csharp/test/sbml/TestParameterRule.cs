/// 
///  @file    TestParameterRule.cs
///  @brief   ParameterRule unit tests
///  @author  Frank Bergmann (Csharp conversion)
///  @author  Akiya Jouraku (Csharp conversion)
///  @author  Ben Bornstein 
/// 
///  $Id:$
///  $HeadURL:$
/// 
///  This test file was converted from src/sbml/test/TestParameterRule.c
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

  public class TestParameterRule {
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

    private Rule PR;

    public void setUp()
    {
      PR = new  AssignmentRule();
      PR.setL1TypeCode(libsbml.SBML_PARAMETER_RULE);
      if (PR == null);
      {
      }
    }

    public void tearDown()
    {
      PR = null;
    }

    public void test_ParameterRule_create()
    {
      assertTrue( PR.getTypeCode() == libsbml.SBML_ASSIGNMENT_RULE );
      assertTrue( PR.getL1TypeCode() == libsbml.SBML_PARAMETER_RULE );
      assertTrue( PR.getNotes() == null );
      assertTrue( PR.getAnnotation() == null );
      assertTrue( PR.getFormula() == "" );
      assertTrue( PR.getUnits() == "" );
      assertTrue( PR.getVariable() == "" );
      assertTrue( PR.getType() == libsbml.RULE_TYPE_SCALAR );
      assertEquals( false, PR.isSetVariable() );
      assertEquals( false, PR.isSetUnits() );
    }

    public void test_ParameterRule_createWith()
    {
      Rule pr;
      pr = new  RateRule("c", "v + 1");
      pr.setL1TypeCode(libsbml.SBML_PARAMETER_RULE);
      assertTrue( pr.getTypeCode() == libsbml.SBML_RATE_RULE );
      assertTrue( pr.getL1TypeCode() == libsbml.SBML_PARAMETER_RULE );
      assertTrue( pr.getNotes() == null );
      assertTrue( pr.getAnnotation() == null );
      assertTrue( pr.getUnits() == "" );
      assertTrue((  "v + 1" == pr.getFormula() ));
      assertTrue((  "c" == pr.getVariable() ));
      assertTrue( pr.getType() == libsbml.RULE_TYPE_RATE );
      assertEquals( true, pr.isSetVariable() );
      assertEquals( false, pr.isSetUnits() );
      pr = null;
    }

    public void test_ParameterRule_free_NULL()
    {
    }

    public void test_ParameterRule_setName()
    {
      string name =  "cell";;
      string c;
      PR.setVariable(name);
      assertTrue(( name == PR.getVariable() ));
      assertEquals( true, PR.isSetVariable() );
      if (PR.getVariable() == name);
      {
      }
      c = PR.getVariable();
      PR.setVariable(c);
      assertTrue(( name == PR.getVariable() ));
      PR.setVariable("");
      assertEquals( false, PR.isSetVariable() );
      if (PR.getVariable() != null);
      {
      }
    }

    public void test_ParameterRule_setUnits()
    {
      string units =  "cell";;
      PR.setUnits(units);
      assertTrue(( units == PR.getUnits() ));
      assertEquals( true, PR.isSetUnits() );
      if (PR.getUnits() == units);
      {
      }
      PR.setUnits(PR.getUnits());
      assertTrue(( units == PR.getUnits() ));
      PR.setUnits("");
      assertEquals( false, PR.isSetUnits() );
      if (PR.getUnits() != null);
      {
      }
    }

  }
}
