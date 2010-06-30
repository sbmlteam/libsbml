/// 
///  @file    TestCompartment_newSetters.cs
///  @brief   Compartment unit tests for new set function API
///  @author  Frank Bergmann (Csharp conversion)
///  @author  Akiya Jouraku (Csharp conversion)
///  @author  Sarah Keating 
/// 
///  $Id$
///  $HeadURL$
/// 
///  This test file was converted from src/sbml/test/TestCompartment_newSetters.c
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

  public class TestCompartment_newSetters {
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

    private Compartment C;

    public void setUp()
    {
      C = new  Compartment(1,2);
      if (C == null);
      {
      }
    }

    public void tearDown()
    {
      C = null;
    }

    public void test_Compartment_setCompartmentType1()
    {
      long i = C.setCompartmentType( "cell");
      assertTrue( i == libsbml.LIBSBML_UNEXPECTED_ATTRIBUTE );
      assertEquals( false, C.isSetCompartmentType() );
      i = C.unsetCompartmentType();
      assertTrue( i == libsbml.LIBSBML_UNEXPECTED_ATTRIBUTE );
      assertEquals( false, C.isSetCompartmentType() );
    }

    public void test_Compartment_setCompartmentType2()
    {
      Compartment c = new  Compartment(2,2);
      long i = c.setCompartmentType( "1cell");
      assertTrue( i == libsbml.LIBSBML_INVALID_ATTRIBUTE_VALUE );
      assertEquals( false, c.isSetCompartmentType() );
      i = c.unsetCompartmentType();
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertEquals( false, c.isSetCompartmentType() );
      c = null;
    }

    public void test_Compartment_setCompartmentType3()
    {
      Compartment c = new  Compartment(2,2);
      long i = c.setCompartmentType( "cell");
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertEquals( true, c.isSetCompartmentType() );
      assertTrue((  "cell"  == c.getCompartmentType() ));
      i = c.unsetCompartmentType();
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertEquals( false, c.isSetCompartmentType() );
      c = null;
    }

    public void test_Compartment_setCompartmentType4()
    {
      Compartment c = new  Compartment(2,2);
      long i = c.setCompartmentType("");
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertEquals( false, c.isSetCompartmentType() );
      c = null;
    }

    public void test_Compartment_setConstant1()
    {
      long i = C.setConstant(false);
      assertTrue( i == libsbml.LIBSBML_UNEXPECTED_ATTRIBUTE );
      assertTrue( C.getConstant() == false );
    }

    public void test_Compartment_setConstant2()
    {
      Compartment c = new  Compartment(2,2);
      long i = c.setConstant(false);
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertTrue( c.getConstant() == false );
      c = null;
    }

    public void test_Compartment_setId2()
    {
      Compartment c = new  Compartment(2,2);
      long i = c.setId( "1cell");
      assertTrue( i == libsbml.LIBSBML_INVALID_ATTRIBUTE_VALUE );
      assertEquals( false, c.isSetId() );
      c = null;
    }

    public void test_Compartment_setId3()
    {
      Compartment c = new  Compartment(2,2);
      long i = c.setId( "cell");
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertEquals( true, c.isSetId() );
      assertTrue((  "cell"  == c.getId() ));
      c = null;
    }

    public void test_Compartment_setId4()
    {
      Compartment c = new  Compartment(2,2);
      long i = c.setId("");
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertEquals( false, c.isSetId() );
      c = null;
    }

    public void test_Compartment_setName1()
    {
      long i = C.setName( "cell");
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertEquals( true, C.isSetName() );
      i = C.unsetName();
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertEquals( false, C.isSetName() );
    }

    public void test_Compartment_setName2()
    {
      Compartment c = new  Compartment(1,2);
      long i = c.setName( "1cell");
      assertTrue( i == libsbml.LIBSBML_INVALID_ATTRIBUTE_VALUE );
      assertEquals( false, c.isSetName() );
      i = c.unsetName();
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertEquals( false, c.isSetName() );
      c = null;
    }

    public void test_Compartment_setName3()
    {
      long i = C.setName("");
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertEquals( false, C.isSetName() );
    }

    public void test_Compartment_setOutside1()
    {
      long i = C.setOutside( "1cell");
      assertTrue( i == libsbml.LIBSBML_INVALID_ATTRIBUTE_VALUE );
      assertEquals( false, C.isSetOutside() );
      i = C.unsetOutside();
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertEquals( false, C.isSetOutside() );
    }

    public void test_Compartment_setOutside2()
    {
      long i = C.setOutside( "litre");
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertEquals( true, C.isSetOutside() );
      i = C.unsetOutside();
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertEquals( false, C.isSetOutside() );
    }

    public void test_Compartment_setOutside3()
    {
      long i = C.setOutside("");
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertEquals( false, C.isSetOutside() );
    }

    public void test_Compartment_setSize1()
    {
      long i = C.setSize(2.0);
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertTrue( C.getSize() == 2.0 );
      i = C.unsetSize();
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
    }

    public void test_Compartment_setSize2()
    {
      Compartment c = new  Compartment(2,2);
      long i = c.setSize(4);
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertTrue( c.getSize() == 4 );
      assertEquals( true, c.isSetSize() );
      i = c.unsetSize();
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertEquals( false, c.isSetSize() );
      c = null;
    }

    public void test_Compartment_setSpatialDimensions1()
    {
      long i = C.setSpatialDimensions(2);
      assertTrue( i == libsbml.LIBSBML_UNEXPECTED_ATTRIBUTE );
      assertTrue( C.getSpatialDimensions() == 3 );
    }

    public void test_Compartment_setSpatialDimensions2()
    {
      Compartment c = new  Compartment(2,2);
      long i = c.setSpatialDimensions(4);
      assertTrue( i == libsbml.LIBSBML_INVALID_ATTRIBUTE_VALUE );
      assertTrue( c.getSpatialDimensions() == 3 );
      c = null;
    }

    public void test_Compartment_setSpatialDimensions3()
    {
      Compartment c = new  Compartment(2,2);
      long i = c.setSpatialDimensions(2);
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertTrue( c.getSpatialDimensions() == 2 );
      c = null;
    }

    public void test_Compartment_setSpatialDimensions4()
    {
      Compartment c = new  Compartment(2,2);
      long i = c.setSpatialDimensions(2.0);
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertTrue( c.getSpatialDimensions() == 2 );
      c = null;
    }

    public void test_Compartment_setSpatialDimensions5()
    {
      Compartment c = new  Compartment(2,2);
      long i = c.setSpatialDimensions(2.2);
      assertTrue( i == libsbml.LIBSBML_INVALID_ATTRIBUTE_VALUE );
      assertTrue( c.getSpatialDimensions() == 3 );
      c = null;
    }

    public void test_Compartment_setUnits1()
    {
      long i = C.setUnits( "1cell");
      assertTrue( i == libsbml.LIBSBML_INVALID_ATTRIBUTE_VALUE );
      assertEquals( false, C.isSetUnits() );
      i = C.unsetUnits();
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertEquals( false, C.isSetUnits() );
    }

    public void test_Compartment_setUnits2()
    {
      long i = C.setUnits( "litre");
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertEquals( true, C.isSetUnits() );
      i = C.unsetUnits();
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertEquals( false, C.isSetUnits() );
    }

    public void test_Compartment_setUnits3()
    {
      long i = C.setUnits("");
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertEquals( false, C.isSetUnits() );
    }

    public void test_Compartment_setVolume1()
    {
      long i = C.setVolume(2.0);
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertTrue( C.getVolume() == 2.0 );
      assertEquals( true, C.isSetVolume() );
      i = C.unsetVolume();
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertTrue( C.getVolume() == 1.0 );
      assertEquals( true, C.isSetVolume() );
    }

    public void test_Compartment_setVolume2()
    {
      Compartment c = new  Compartment(2,2);
      long i = c.setVolume(4);
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertTrue( c.getVolume() == 4.0 );
      assertEquals( true, c.isSetVolume() );
      i = c.unsetVolume();
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertEquals( false, c.isSetVolume() );
      c = null;
    }

  }
}
