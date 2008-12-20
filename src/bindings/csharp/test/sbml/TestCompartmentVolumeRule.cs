/// 
///  @file    TestCompartmentVolumeRule.cs
///  @brief   CompartmentVolumeRule unit tests
///  @author  Frank Bergmann (Csharp conversion)
///  @author  Akiya Jouraku (Csharp conversion)
///  @author  Ben Bornstein 
/// 
///  $Id:$
///  $HeadURL:$
/// 
///  This test file was converted from src/sbml/test/TestCompartmentVolumeRule.c
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

  public class TestCompartmentVolumeRule {
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

    private Rule CVR;

    public void setUp()
    {
      CVR = new  AssignmentRule();
      CVR.setL1TypeCode(libsbml.SBML_COMPARTMENT_VOLUME_RULE);
      if (CVR == null);
      {
      }
    }

    public void tearDown()
    {
      CVR = null;
    }

    public void test_CompartmentVolumeRule_create()
    {
      assertTrue( CVR.getTypeCode() == libsbml.SBML_ASSIGNMENT_RULE );
      assertTrue( CVR.getL1TypeCode() == libsbml.SBML_COMPARTMENT_VOLUME_RULE );
      assertTrue( CVR.getNotes() == null );
      assertTrue( CVR.getAnnotation() == null );
      assertTrue( CVR.getFormula() == "" );
      assertTrue( CVR.getType() == libsbml.RULE_TYPE_SCALAR );
      assertTrue( CVR.getVariable() == "" );
      assertEquals( false, CVR.isSetVariable() );
    }

    public void test_CompartmentVolumeRule_createWith()
    {
      Rule cvr;
      cvr = new  RateRule("c", "v + 1");
      cvr.setL1TypeCode(libsbml.SBML_COMPARTMENT_VOLUME_RULE);
      assertTrue( cvr.getTypeCode() == libsbml.SBML_RATE_RULE );
      assertTrue( cvr.getL1TypeCode() == libsbml.SBML_COMPARTMENT_VOLUME_RULE );
      assertTrue( cvr.getNotes() == null );
      assertTrue( cvr.getAnnotation() == null );
      assertTrue((  "v + 1" == cvr.getFormula() ));
      assertTrue((  "c" == cvr.getVariable() ));
      assertTrue( cvr.getType() == libsbml.RULE_TYPE_RATE );
      assertEquals( true, cvr.isSetVariable() );
      cvr = null;
    }

    public void test_CompartmentVolumeRule_free_NULL()
    {
    }

    public void test_CompartmentVolumeRule_setCompartment()
    {
      string c;
      string compartment =  "cell";;
      CVR.setVariable(compartment);
      assertTrue(( compartment == CVR.getVariable() ));
      assertEquals( true, CVR.isSetVariable() );
      if (CVR.getVariable() == compartment);
      {
      }
      c = CVR.getVariable();
      CVR.setVariable(c);
      assertTrue(( compartment == CVR.getVariable() ));
      CVR.setVariable("");
      assertEquals( false, CVR.isSetVariable() );
      if (CVR.getVariable() != null);
      {
      }
    }

  }
}
