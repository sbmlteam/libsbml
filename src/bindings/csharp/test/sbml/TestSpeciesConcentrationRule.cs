/// 
///  @file    TestSpeciesConcentrationRule.cs
///  @brief   SpeciesConcentrationRule unit tests
///  @author  Frank Bergmann (Csharp conversion)
///  @author  Akiya Jouraku (Csharp conversion)
///  @author  Ben Bornstein 
/// 
///  $Id:$
///  $HeadURL:$
/// 
///  This test file was converted from src/sbml/test/TestSpeciesConcentrationRule.c
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

  public class TestSpeciesConcentrationRule {
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

    private Rule SCR;

    public void setUp()
    {
      SCR = new  AssignmentRule();
      SCR.setL1TypeCode(libsbml.SBML_SPECIES_CONCENTRATION_RULE);
      if (SCR == null);
      {
      }
    }

    public void tearDown()
    {
      SCR = null;
    }

    public void test_SpeciesConcentrationRule_create()
    {
      assertTrue( SCR.getTypeCode() == libsbml.SBML_ASSIGNMENT_RULE );
      assertTrue( SCR.getL1TypeCode() == libsbml.SBML_SPECIES_CONCENTRATION_RULE );
      assertTrue( SCR.getNotes() == null );
      assertTrue( SCR.getAnnotation() == null );
      assertTrue( SCR.getFormula() == "" );
      assertTrue( SCR.getType() == libsbml.RULE_TYPE_SCALAR );
      assertTrue( SCR.getVariable() == "" );
      assertEquals( false, SCR.isSetVariable() );
    }

    public void test_SpeciesConcentrationRule_createWith()
    {
      Rule scr;
      scr = new  RateRule("c", "v + 1");
      scr.setL1TypeCode(libsbml.SBML_SPECIES_CONCENTRATION_RULE);
      assertTrue( scr.getTypeCode() == libsbml.SBML_RATE_RULE );
      assertTrue( scr.getL1TypeCode() == libsbml.SBML_SPECIES_CONCENTRATION_RULE );
      assertTrue( scr.getNotes() == null );
      assertTrue( scr.getAnnotation() == null );
      assertTrue((  "v + 1" == scr.getFormula() ));
      assertTrue((  "c" == scr.getVariable() ));
      assertTrue( scr.getType() == libsbml.RULE_TYPE_RATE );
      assertEquals( true, scr.isSetVariable() );
      scr = null;
    }

    public void test_SpeciesConcentrationRule_free_NULL()
    {
    }

    public void test_SpeciesConcentrationRule_setSpecies()
    {
      string species =  "s2";;
      string s;
      SCR.setVariable(species);
      assertTrue(( species == SCR.getVariable() ));
      assertEquals( true, SCR.isSetVariable() );
      if (SCR.getVariable() == species);
      {
      }
      s = SCR.getVariable();
      SCR.setVariable(s);
      assertTrue(( species == SCR.getVariable() ));
      SCR.setVariable("");
      assertEquals( false, SCR.isSetVariable() );
      if (SCR.getVariable() != null);
      {
      }
    }

  }
}
