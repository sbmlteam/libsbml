/// 
///  @file    TestSpecies.cs
///  @brief   Species unit tests
///  @author  Frank Bergmann (Csharp conversion)
///  @author  Akiya Jouraku (Csharp conversion)
///  @author  Ben Bornstein 
/// 
///  $Id$
///  $URL$
/// 
///  This test file was converted from src/sbml/test/TestSpecies.c
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

  public class TestSpecies {
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

    private Species S;

    public void setUp()
    {
      S = new  Species();
      if (S == null);
      {
      }
    }

    public void tearDown()
    {
      S = null;
    }

    public void test_Species_create()
    {
      assertTrue( S.getTypeCode() == libsbml.SBML_SPECIES );
      assertTrue( S.getMetaId()== "" == true );
      assertTrue( S.getNotes() == null );
      assertTrue( S.getAnnotation() == null );
      assertTrue( S.getId()== "" == true );
      assertTrue( S.getName()== "" == true );
      assertTrue( S.getCompartment()== "" == true );
      assertTrue( S.getInitialAmount() == 0.0 );
      assertTrue( S.getInitialConcentration() == 0.0 );
      assertTrue( S.getSubstanceUnits()== "" == true );
      assertTrue( S.getSpatialSizeUnits()== "" == true );
      assertTrue( S.getHasOnlySubstanceUnits() == false );
      assertTrue( S.getBoundaryCondition() == false );
      assertTrue( S.getCharge() == 0 );
      assertTrue( S.getConstant() == false );
      assertEquals( false, S.isSetId() );
      assertEquals( false, S.isSetName() );
      assertEquals( false, S.isSetCompartment() );
      assertEquals( false, S.isSetInitialAmount() );
      assertEquals( false, S.isSetInitialConcentration() );
      assertEquals( false, S.isSetSubstanceUnits() );
      assertEquals( false, S.isSetSpatialSizeUnits() );
      assertEquals( false, S.isSetUnits() );
      assertEquals( false, S.isSetCharge() );
    }

    public void test_Species_createWith()
    {
      Species s = new  Species("Ca", "Calcium");
      assertTrue( s.getTypeCode() == libsbml.SBML_SPECIES );
      assertTrue( s.getMetaId()== "" == true );
      assertTrue( s.getNotes() == null );
      assertTrue( s.getAnnotation() == null );
      assertTrue((  "Calcium"   == s.getName() ));
      assertTrue( s.getSpatialSizeUnits()== "" == true );
      assertTrue( s.getHasOnlySubstanceUnits() == false );
      assertTrue( s.getConstant() == false );
      assertTrue((  "Ca"   == s.getId() ));
      assertEquals( true, s.isSetId() );
      assertEquals( true, s.isSetName() );
      assertEquals( false, s.isSetCompartment() );
      assertEquals( false, s.isSetSubstanceUnits() );
      assertEquals( false, s.isSetSpatialSizeUnits() );
      assertEquals( false, s.isSetUnits() );
      assertEquals( false, s.isSetInitialAmount() );
      assertEquals( false, s.isSetInitialConcentration() );
      assertEquals( false, s.isSetCharge() );
      s = null;
    }

    public void test_Species_free_NULL()
    {
    }

    public void test_Species_setCompartment()
    {
      string compartment = "cell";
      S.setCompartment(compartment);
      assertTrue(( compartment == S.getCompartment() ));
      assertEquals( true, S.isSetCompartment() );
      if (S.getCompartment() == compartment);
      {
      }
      S.setCompartment(S.getCompartment());
      assertTrue(( compartment == S.getCompartment() ));
      S.setCompartment("");
      assertEquals( false, S.isSetCompartment() );
      if (S.getCompartment() != null);
      {
      }
    }

    public void test_Species_setId()
    {
      string id = "Glucose";
      S.setId(id);
      assertTrue(( id == S.getId() ));
      assertEquals( true, S.isSetId() );
      if (S.getId() == id);
      {
      }
      S.setId(S.getId());
      assertTrue(( id == S.getId() ));
      S.setId("");
      assertEquals( false, S.isSetId() );
      if (S.getId() != null);
      {
      }
    }

    public void test_Species_setInitialAmount()
    {
      assertEquals( false, S.isSetInitialAmount() );
      assertEquals( false, S.isSetInitialConcentration() );
      S.setInitialAmount(1.2);
      assertEquals( true, S.isSetInitialAmount() );
      assertEquals( false, S.isSetInitialConcentration() );
      assertTrue( S.getInitialAmount() == 1.2 );
    }

    public void test_Species_setInitialConcentration()
    {
      assertEquals( false, S.isSetInitialAmount() );
      assertEquals( false, S.isSetInitialConcentration() );
      S.setInitialConcentration(3.4);
      assertEquals( false, S.isSetInitialAmount() );
      assertEquals( true, S.isSetInitialConcentration() );
      assertTrue( S.getInitialConcentration() == 3.4 );
    }

    public void test_Species_setName()
    {
      string name = "So Sweet";
      S.setName(name);
      assertTrue(( name == S.getName() ));
      assertEquals( true, S.isSetName() );
      if (S.getName() == name);
      {
      }
      S.setName(S.getName());
      assertTrue(( name == S.getName() ));
      S.setName("");
      assertEquals( false, S.isSetName() );
      if (S.getName() != null);
      {
      }
    }

    public void test_Species_setSpatialSizeUnits()
    {
      string units = "volume";
      S.setSpatialSizeUnits(units);
      assertTrue(( units == S.getSpatialSizeUnits() ));
      assertEquals( true, S.isSetSpatialSizeUnits() );
      if (S.getSpatialSizeUnits() == units);
      {
      }
      S.setSpatialSizeUnits(S.getSpatialSizeUnits());
      assertTrue(( units == S.getSpatialSizeUnits() ));
      S.setSpatialSizeUnits("");
      assertEquals( false, S.isSetSpatialSizeUnits() );
      if (S.getSpatialSizeUnits() != null);
      {
      }
    }

    public void test_Species_setSubstanceUnits()
    {
      string units = "item";
      S.setSubstanceUnits(units);
      assertTrue(( units == S.getSubstanceUnits() ));
      assertEquals( true, S.isSetSubstanceUnits() );
      if (S.getSubstanceUnits() == units);
      {
      }
      S.setSubstanceUnits(S.getSubstanceUnits());
      assertTrue(( units == S.getSubstanceUnits() ));
      S.setSubstanceUnits("");
      assertEquals( false, S.isSetSubstanceUnits() );
      if (S.getSubstanceUnits() != null);
      {
      }
    }

    public void test_Species_setUnits()
    {
      string units = "mole";
      S.setUnits(units);
      assertTrue(( units == S.getUnits() ));
      assertEquals( true, S.isSetUnits() );
      if (S.getSubstanceUnits() == units);
      {
      }
      S.setUnits(S.getSubstanceUnits());
      assertTrue(( units == S.getUnits() ));
      S.setUnits("");
      assertEquals( false, S.isSetUnits() );
      if (S.getSubstanceUnits() != null);
      {
      }
    }

  }
}
