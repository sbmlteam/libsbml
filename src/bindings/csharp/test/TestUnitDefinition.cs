/// 
///  @file    TestUnitDefinition.cs
///  @brief   SBML UnitDefinition unit tests
///  @author  Frank Bergmann (Csharp conversion)
///  @author  Akiya Jouraku (Csharp conversion)
///  @author  Ben Bornstein 
/// 
///  $Id:$
///  $HeadURL:$
/// 
///  This test file was converted from src/sbml/test/TestUnitDefinition.c
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

  public class TestUnitDefinition {
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

    private UnitDefinition UD;

    public void setUp()
    {
      UD = new  UnitDefinition();
      if (UD == null);
      {
      }
    }

    public void tearDown()
    {
      UD = null;
    }

    public void test_UnitDefinition_addUnit()
    {
      Unit u = new  Unit();
      UD.addUnit(u);
      assertTrue( UD.getNumUnits() == 1 );
      u = null;
    }

    public void test_UnitDefinition_create()
    {
      assertTrue( UD.getTypeCode() == libsbml.SBML_UNIT_DEFINITION );
      assertTrue( UD.getMetaId()== "" == true );
      assertTrue( UD.getNotes() == null );
      assertTrue( UD.getAnnotation() == null );
      assertTrue( UD.getId()== "" == true );
      assertTrue( UD.getName()== "" == true );
      assertEquals( false, UD.isSetId() );
      assertEquals( false, UD.isSetName() );
      assertTrue( UD.getNumUnits() == 0 );
    }

    public void test_UnitDefinition_createWith()
    {
      UnitDefinition ud = new  UnitDefinition("mmls", "");
      assertTrue( ud.getTypeCode() == libsbml.SBML_UNIT_DEFINITION );
      assertTrue( ud.getMetaId()== "" == true );
      assertTrue( ud.getNotes() == null );
      assertTrue( ud.getAnnotation() == null );
      assertTrue( ud.getName()== "" == true );
      assertTrue((  "mmls" == ud.getId() ));
      assertEquals( true, ud.isSetId() );
      assertTrue( ud.getNumUnits() == 0 );
      ud = null;
    }

    public void test_UnitDefinition_createWithName()
    {
      UnitDefinition ud = new  UnitDefinition("", "mmol liter^-1 sec^-1");
      assertTrue( ud.getTypeCode() == libsbml.SBML_UNIT_DEFINITION );
      assertTrue( ud.getMetaId()== "" == true );
      assertTrue( ud.getNotes() == null );
      assertTrue( ud.getAnnotation() == null );
      assertTrue( ud.getId()== "" == true );
      assertTrue((  "mmol liter^-1 sec^-1" == ud.getName() ));
      assertEquals( true, ud.isSetName() );
      assertTrue( ud.getNumUnits() == 0 );
      ud = null;
    }

    public void test_UnitDefinition_free_NULL()
    {
    }

    public void test_UnitDefinition_getUnit()
    {
      Unit mole = new  Unit();
      Unit litre = new  Unit();
      Unit second = new  Unit();
      mole.setKind(libsbml.UnitKind_forName("mole"));
      litre.setKind(libsbml.UnitKind_forName("litre"));
      second.setKind(libsbml.UnitKind_forName("second"));
      mole.setScale(-3);
      litre.setExponent(-1);
      second.setExponent(-1);
      UD.addUnit(mole);
      UD.addUnit(litre);
      UD.addUnit(second);
      mole = null;
      litre = null;
      second = null;
      assertTrue( UD.getNumUnits() == 3 );
      mole = UD.getUnit(0);
      litre = UD.getUnit(1);
      second = UD.getUnit(2);
      assertTrue( mole.getKind() == libsbml.UNIT_KIND_MOLE );
      assertTrue( litre.getKind() == libsbml.UNIT_KIND_LITRE );
      assertTrue( second.getKind() == libsbml.UNIT_KIND_SECOND );
      assertTrue( mole.getScale() == -3 );
      assertTrue( litre.getExponent() == -1 );
      assertTrue( second.getExponent() == -1 );
    }

    public void test_UnitDefinition_isVariantOfArea()
    {
      Unit dim = new  Unit();
      dim.setKind(libsbml.UnitKind_forName("dimensionless"));
      Unit u = UD.createUnit();
      assertEquals( false, UD.isVariantOfArea() );
      u.setKind(libsbml.UNIT_KIND_METRE);
      u.setExponent(2);
      assertEquals( true, UD.isVariantOfArea() );
      u.setScale(-1);
      assertEquals( true, UD.isVariantOfArea() );
      u.setMultiplier(2);
      assertEquals( true, UD.isVariantOfArea() );
      u.setOffset(3);
      assertEquals( true, UD.isVariantOfArea() );
      u.setExponent(3);
      assertEquals( false, UD.isVariantOfArea() );
      u.setExponent(2);
      UD.addUnit(dim);
      assertEquals( true, UD.isVariantOfArea() );
    }

    public void test_UnitDefinition_isVariantOfLength()
    {
      Unit dim = new  Unit();
      dim.setKind(libsbml.UnitKind_forName("dimensionless"));
      Unit u = UD.createUnit();
      assertEquals( false, UD.isVariantOfLength() );
      u.setKind(libsbml.UNIT_KIND_METRE);
      u.setExponent(1);
      assertEquals( true, UD.isVariantOfLength() );
      u.setScale(-1);
      assertEquals( true, UD.isVariantOfLength() );
      u.setMultiplier(2);
      assertEquals( true, UD.isVariantOfLength() );
      u.setOffset(3);
      assertEquals( true, UD.isVariantOfLength() );
      u.setExponent(2);
      assertEquals( false, UD.isVariantOfLength() );
      u.setExponent(1);
      UD.addUnit(dim);
      assertEquals( true, UD.isVariantOfLength() );
    }

    public void test_UnitDefinition_isVariantOfSubstance_1()
    {
      Unit dim = new  Unit();
      dim.setKind(libsbml.UnitKind_forName("dimensionless"));
      Unit u = UD.createUnit();
      assertEquals( false, UD.isVariantOfSubstance() );
      u.setKind(libsbml.UNIT_KIND_MOLE);
      u.setExponent(1);
      assertEquals( true, UD.isVariantOfSubstance() );
      u.setScale(-1);
      assertEquals( true, UD.isVariantOfSubstance() );
      u.setMultiplier(2);
      assertEquals( true, UD.isVariantOfSubstance() );
      u.setOffset(3);
      assertEquals( true, UD.isVariantOfSubstance() );
      u.setExponent(-3);
      assertEquals( false, UD.isVariantOfSubstance() );
      u.setExponent(1);
      UD.addUnit(dim);
      assertEquals( true, UD.isVariantOfSubstance() );
    }

    public void test_UnitDefinition_isVariantOfSubstance_2()
    {
      Unit dim = new  Unit();
      dim.setKind(libsbml.UnitKind_forName("dimensionless"));
      Unit u = UD.createUnit();
      assertEquals( false, UD.isVariantOfSubstance() );
      u.setKind(libsbml.UNIT_KIND_ITEM);
      u.setExponent(1);
      assertEquals( true, UD.isVariantOfSubstance() );
      u.setScale(-1);
      assertEquals( true, UD.isVariantOfSubstance() );
      u.setMultiplier(2);
      assertEquals( true, UD.isVariantOfSubstance() );
      u.setOffset(3);
      assertEquals( true, UD.isVariantOfSubstance() );
      u.setExponent(-2);
      assertEquals( false, UD.isVariantOfSubstance() );
      u.setExponent(1);
      UD.addUnit(dim);
      assertEquals( true, UD.isVariantOfSubstance() );
    }

    public void test_UnitDefinition_isVariantOfTime()
    {
      Unit dim = new  Unit();
      dim.setKind(libsbml.UnitKind_forName("dimensionless"));
      Unit u = UD.createUnit();
      assertEquals( false, UD.isVariantOfTime() );
      u.setKind(libsbml.UNIT_KIND_SECOND);
      u.setExponent(1);
      assertEquals( true, UD.isVariantOfTime() );
      u.setScale(-10);
      assertEquals( true, UD.isVariantOfTime() );
      u.setMultiplier(10);
      assertEquals( true, UD.isVariantOfTime() );
      u.setOffset(30);
      assertEquals( true, UD.isVariantOfTime() );
      u.setExponent(2);
      assertEquals( false, UD.isVariantOfTime() );
      u.setExponent(1);
      UD.addUnit(dim);
      assertEquals( true, UD.isVariantOfTime() );
    }

    public void test_UnitDefinition_isVariantOfVolume_1()
    {
      Unit dim = new  Unit();
      dim.setKind(libsbml.UnitKind_forName("dimensionless"));
      Unit u = UD.createUnit();
      assertEquals( false, UD.isVariantOfVolume() );
      u.setKind(libsbml.UNIT_KIND_LITRE);
      u.setExponent(1);
      assertEquals( true, UD.isVariantOfVolume() );
      u.setScale(100);
      assertEquals( true, UD.isVariantOfVolume() );
      u.setMultiplier(5);
      assertEquals( true, UD.isVariantOfVolume() );
      u.setOffset(-5);
      assertEquals( true, UD.isVariantOfVolume() );
      u.setExponent(-1);
      assertEquals( false, UD.isVariantOfVolume() );
      u.setExponent(1);
      UD.addUnit(dim);
      assertEquals( true, UD.isVariantOfVolume() );
    }

    public void test_UnitDefinition_isVariantOfVolume_2()
    {
      Unit dim = new  Unit();
      dim.setKind(libsbml.UnitKind_forName("dimensionless"));
      Unit u = UD.createUnit();
      assertEquals( false, UD.isVariantOfVolume() );
      u.setKind(libsbml.UNIT_KIND_METRE);
      u.setExponent(3);
      assertEquals( true, UD.isVariantOfVolume() );
      u.setScale(100);
      assertEquals( true, UD.isVariantOfVolume() );
      u.setMultiplier(5);
      assertEquals( true, UD.isVariantOfVolume() );
      u.setOffset(-5);
      assertEquals( true, UD.isVariantOfVolume() );
      u.setExponent(2);
      assertEquals( false, UD.isVariantOfVolume() );
      u.setExponent(3);
      UD.addUnit(dim);
      assertEquals( true, UD.isVariantOfVolume() );
    }

    public void test_UnitDefinition_setId()
    {
      string id = "mmls";
      UD.setId(id);
      assertTrue(( id == UD.getId() ));
      assertEquals( true, UD.isSetId() );
      if (UD.getId() == id);
      {
      }
      UD.setId(UD.getId());
      assertTrue(( id == UD.getId() ));
      UD.setId("");
      assertEquals( false, UD.isSetId() );
      if (UD.getId() != null);
      {
      }
    }

    public void test_UnitDefinition_setName()
    {
      string name = "mmol liter^-1 sec^-1";
      UD.setName(name);
      assertTrue(( name == UD.getName() ));
      assertEquals( true, UD.isSetName() );
      if (UD.getName() == name);
      {
      }
      UD.setName(UD.getName());
      assertTrue(( name == UD.getName() ));
      UD.setName("");
      assertEquals( false, UD.isSetName() );
      if (UD.getName() != null);
      {
      }
    }

  }
}
