/// 
///  @file    TestParameter.cs
///  @brief   Parameter unit tests
///  @author  Frank Bergmann (Csharp conversion)
///  @author  Akiya Jouraku (Csharp conversion)
///  @author  Ben Bornstein 
/// 
///  $Id:$
///  $HeadURL:$
/// 
///  This test file was converted from src/sbml/test/TestParameter.c
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

  public class TestParameter {
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

    private Parameter P;

    public void setUp()
    {
      P = new  Parameter();
      if (P == null);
      {
      }
    }

    public void tearDown()
    {
      P = null;
    }

    public void test_Parameter_create()
    {
      assertTrue( P.getTypeCode() == libsbml.SBML_PARAMETER );
      assertTrue( P.getMetaId() == "" );
      assertTrue( P.getNotes() == null );
      assertTrue( P.getAnnotation() == null );
      assertTrue( P.getId() == "" );
      assertTrue( P.getName() == "" );
      assertTrue( P.getUnits() == "" );
      assertTrue( P.getConstant() == true );
      assertEquals( false, P.isSetId() );
      assertEquals( false, P.isSetName() );
      assertEquals( false, P.isSetValue() );
      assertEquals( false, P.isSetUnits() );
    }

    public void test_Parameter_createWith()
    {
      Parameter p = new  Parameter("delay",6.2, "second");
      assertTrue( p.getTypeCode() == libsbml.SBML_PARAMETER );
      assertTrue( p.getMetaId() == "" );
      assertTrue( p.getNotes() == null );
      assertTrue( p.getAnnotation() == null );
      assertTrue((  "delay"  == p.getId() ));
      assertTrue((  "second" == p.getUnits() ));
      assertTrue( p.getName() == "" );
      assertTrue( p.getValue() == 6.2 );
      assertTrue( p.getConstant() == true );
      assertEquals( true, p.isSetId() );
      assertEquals( false, p.isSetName() );
      assertEquals( true, p.isSetValue() );
      assertEquals( true, p.isSetUnits() );
      p = null;
    }

    public void test_Parameter_createWithLevelVersionAndNamespace()
    {
      XMLNamespaces xmlns = new  XMLNamespaces();
      xmlns.add( "http://www.sbml.org", "sbml");
      Parameter object1 = new  Parameter(2,1,xmlns);
      assertTrue( object1.getTypeCode() == libsbml.SBML_PARAMETER );
      assertTrue( object1.getMetaId() == "" );
      assertTrue( object1.getNotes() == null );
      assertTrue( object1.getAnnotation() == null );
      assertTrue( object1.getLevel() == 2 );
      assertTrue( object1.getVersion() == 1 );
      assertTrue( object1.getNamespaces() != null );
      assertTrue( object1.getNamespaces().getLength() == 1 );
      object1 = null;
    }

    public void test_Parameter_free_NULL()
    {
    }

    public void test_Parameter_setId()
    {
      string id =  "Km1";;
      P.setId(id);
      assertTrue(( id == P.getId() ));
      assertEquals( true, P.isSetId() );
      if (P.getId() == id);
      {
      }
      P.setId(P.getId());
      assertTrue(( id == P.getId() ));
      P.setId("");
      assertEquals( false, P.isSetId() );
      if (P.getId() != null);
      {
      }
    }

    public void test_Parameter_setName()
    {
      string name =  "Forward Michaelis-Menten Constant";;
      P.setName(name);
      assertTrue(( name == P.getName() ));
      assertEquals( true, P.isSetName() );
      if (P.getName() == name);
      {
      }
      P.setName(P.getName());
      assertTrue(( name == P.getName() ));
      P.setName("");
      assertEquals( false, P.isSetName() );
      if (P.getName() != null);
      {
      }
    }

    public void test_Parameter_setUnits()
    {
      string units =  "second";;
      P.setUnits(units);
      assertTrue(( units == P.getUnits() ));
      assertEquals( true, P.isSetUnits() );
      if (P.getUnits() == units);
      {
      }
      P.setUnits(P.getUnits());
      assertTrue(( units == P.getUnits() ));
      P.setUnits("");
      assertEquals( false, P.isSetUnits() );
      if (P.getUnits() != null);
      {
      }
    }

  }
}
