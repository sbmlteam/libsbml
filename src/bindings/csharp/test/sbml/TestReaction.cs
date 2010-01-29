/// 
///  @file    TestReaction.cs
///  @brief   SBML Reaction unit tests
///  @author  Frank Bergmann (Csharp conversion)
///  @author  Akiya Jouraku (Csharp conversion)
///  @author  Ben Bornstein 
/// 
///  $Id$
///  $HeadURL$
/// 
///  This test file was converted from src/sbml/test/TestReaction.c
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

  public class TestReaction {
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

    private Reaction R;

    public void setUp()
    {
      R = new  Reaction(2,4);
      if (R == null);
      {
      }
    }

    public void tearDown()
    {
      R = null;
    }

    public void test_Reaction_addModifier()
    {
      ModifierSpeciesReference msr = new  ModifierSpeciesReference(2,4);
      msr.setSpecies( "s");
      R.addModifier(msr);
      assertTrue( R.getNumReactants() == 0 );
      assertTrue( R.getNumProducts() == 0 );
      assertTrue( R.getNumModifiers() == 1 );
    }

    public void test_Reaction_addProduct()
    {
      SpeciesReference sr = new  SpeciesReference(2,4);
      sr.setSpecies( "s");
      R.addProduct(sr);
      assertTrue( R.getNumReactants() == 0 );
      assertTrue( R.getNumProducts() == 1 );
      assertTrue( R.getNumModifiers() == 0 );
      sr = null;
    }

    public void test_Reaction_addReactant()
    {
      SpeciesReference sr = new  SpeciesReference(2,4);
      sr.setSpecies( "s");
      R.addReactant(sr);
      assertTrue( R.getNumReactants() == 1 );
      assertTrue( R.getNumProducts() == 0 );
      assertTrue( R.getNumModifiers() == 0 );
      sr = null;
    }

    public void test_Reaction_create()
    {
      assertTrue( R.getTypeCode() == libsbml.SBML_REACTION );
      assertTrue( R.getMetaId() == "" );
      assertTrue( R.getNotes() == null );
      assertTrue( R.getAnnotation() == null );
      assertTrue( R.getId() == "" );
      assertTrue( R.getName() == "" );
      assertEquals(R.getKineticLaw(),null);
      assertTrue( R.getReversible() != false );
      assertTrue( R.getFast() == false );
      assertEquals( false, R.isSetId() );
      assertEquals( false, R.isSetName() );
      assertEquals( false, R.isSetKineticLaw() );
      assertTrue( R.getNumReactants() == 0 );
      assertTrue( R.getNumProducts() == 0 );
      assertTrue( R.getNumModifiers() == 0 );
    }

    public void test_Reaction_createWithNS()
    {
      XMLNamespaces xmlns = new  XMLNamespaces();
      xmlns.add( "http://www.sbml.org", "testsbml");
      SBMLNamespaces sbmlns = new  SBMLNamespaces(2,1);
      sbmlns.addNamespaces(xmlns);
      Reaction object1 = new  Reaction(sbmlns);
      assertTrue( object1.getTypeCode() == libsbml.SBML_REACTION );
      assertTrue( object1.getMetaId() == "" );
      assertTrue( object1.getNotes() == null );
      assertTrue( object1.getAnnotation() == null );
      assertTrue( object1.getLevel() == 2 );
      assertTrue( object1.getVersion() == 1 );
      assertTrue( object1.getNamespaces() != null );
      assertTrue( object1.getNamespaces().getLength() == 2 );
      object1 = null;
    }

    public void test_Reaction_free_NULL()
    {
    }

    public void test_Reaction_getModifier()
    {
      ModifierSpeciesReference msr1 = new  ModifierSpeciesReference(2,4);
      ModifierSpeciesReference msr2 = new  ModifierSpeciesReference(2,4);
      msr1.setSpecies( "M1");
      msr2.setSpecies( "M2");
      R.addModifier(msr1);
      R.addModifier(msr2);
      msr1 = null;
      msr2 = null;
      assertTrue( R.getNumReactants() == 0 );
      assertTrue( R.getNumProducts() == 0 );
      assertTrue( R.getNumModifiers() == 2 );
      msr1 = R.getModifier(0);
      msr2 = R.getModifier(1);
      assertTrue((  "M1" == msr1.getSpecies() ));
      assertTrue((  "M2" == msr2.getSpecies() ));
    }

    public void test_Reaction_getModifierById()
    {
      ModifierSpeciesReference msr1 = new  ModifierSpeciesReference(2,4);
      ModifierSpeciesReference msr2 = new  ModifierSpeciesReference(2,4);
      msr1.setSpecies( "M1");
      msr2.setSpecies( "M2");
      R.addModifier(msr1);
      R.addModifier(msr2);
      assertTrue( R.getNumReactants() == 0 );
      assertTrue( R.getNumProducts() == 0 );
      assertTrue( R.getNumModifiers() == 2 );
      assertNotEquals(R.getModifier( "M1"),msr1);
      assertNotEquals(R.getModifier( "M2"),msr2);
      assertEquals(R.getModifier( "M3"),null);
      msr1 = null;
      msr2 = null;
    }

    public void test_Reaction_getProduct()
    {
      SpeciesReference sr1 = new  SpeciesReference(2,4);
      SpeciesReference sr2 = new  SpeciesReference(2,4);
      sr1.setSpecies( "P1");
      sr2.setSpecies( "P2");
      R.addProduct(sr1);
      R.addProduct(sr2);
      sr1 = null;
      sr2 = null;
      assertTrue( R.getNumReactants() == 0 );
      assertTrue( R.getNumProducts() == 2 );
      assertTrue( R.getNumModifiers() == 0 );
      sr1 = R.getProduct(0);
      sr2 = R.getProduct(1);
      assertTrue((  "P1" == sr1.getSpecies() ));
      assertTrue((  "P2" == sr2.getSpecies() ));
    }

    public void test_Reaction_getProductById()
    {
      SpeciesReference sr1 = new  SpeciesReference(2,4);
      sr1.setSpecies( "P1");
      SpeciesReference sr2 = new  SpeciesReference(2,4);
      sr2.setSpecies( "P1");
      R.addProduct(sr1);
      R.addProduct(sr2);
      assertTrue( R.getNumReactants() == 0 );
      assertTrue( R.getNumProducts() == 2 );
      assertTrue( R.getNumModifiers() == 0 );
      assertNotEquals(R.getProduct( "P1"),sr1);
      assertNotEquals(R.getProduct( "P2"),sr2);
      assertEquals(R.getProduct( "P3"),null);
      sr1 = null;
      sr2 = null;
    }

    public void test_Reaction_getReactant()
    {
      SpeciesReference sr1 = new  SpeciesReference(2,4);
      SpeciesReference sr2 = new  SpeciesReference(2,4);
      sr1.setSpecies( "R1");
      sr2.setSpecies( "R2");
      R.addReactant(sr1);
      R.addReactant(sr2);
      sr1 = null;
      sr2 = null;
      assertTrue( R.getNumReactants() == 2 );
      assertTrue( R.getNumProducts() == 0 );
      assertTrue( R.getNumModifiers() == 0 );
      sr1 = R.getReactant(0);
      sr2 = R.getReactant(1);
      assertTrue((  "R1" == sr1.getSpecies() ));
      assertTrue((  "R2" == sr2.getSpecies() ));
    }

    public void test_Reaction_getReactantById()
    {
      SpeciesReference sr1 = new  SpeciesReference(2,4);
      sr1.setSpecies( "R1");
      SpeciesReference sr2 = new  SpeciesReference(2,4);
      sr2.setSpecies( "R2");
      R.addReactant(sr1);
      R.addReactant(sr2);
      assertTrue( R.getNumReactants() == 2 );
      assertTrue( R.getNumProducts() == 0 );
      assertTrue( R.getNumModifiers() == 0 );
      assertNotEquals(R.getReactant( "R1"),sr1);
      assertNotEquals(R.getReactant( "R2"),sr2);
      assertEquals(R.getReactant( "R3"),null);
      sr1 = null;
      sr2 = null;
    }

    public void test_Reaction_removeModifier()
    {
      ModifierSpeciesReference o1,o2,o3;
      o1 = R.createModifier();
      o2 = R.createModifier();
      o3 = R.createModifier();
      o3.setSpecies("test");
      assertTrue( R.removeModifier(0) == o1 );
      assertTrue( R.getNumModifiers() == 2 );
      assertTrue( R.removeModifier(0) == o2 );
      assertTrue( R.getNumModifiers() == 1 );
      assertTrue( R.removeModifier("test") == o3 );
      assertTrue( R.getNumModifiers() == 0 );
      o1 = null;
      o2 = null;
      o3 = null;
    }

    public void test_Reaction_removeProduct()
    {
      SpeciesReference o1,o2,o3;
      o1 = R.createProduct();
      o2 = R.createProduct();
      o3 = R.createProduct();
      o3.setSpecies("test");
      assertTrue( R.removeProduct(0) == o1 );
      assertTrue( R.getNumProducts() == 2 );
      assertTrue( R.removeProduct(0) == o2 );
      assertTrue( R.getNumProducts() == 1 );
      assertTrue( R.removeProduct("test") == o3 );
      assertTrue( R.getNumProducts() == 0 );
      o1 = null;
      o2 = null;
      o3 = null;
    }

    public void test_Reaction_removeReactant()
    {
      SpeciesReference o1,o2,o3;
      o1 = R.createReactant();
      o2 = R.createReactant();
      o3 = R.createReactant();
      o3.setSpecies("test");
      assertTrue( R.removeReactant(0) == o1 );
      assertTrue( R.getNumReactants() == 2 );
      assertTrue( R.removeReactant(0) == o2 );
      assertTrue( R.getNumReactants() == 1 );
      assertTrue( R.removeReactant("test") == o3 );
      assertTrue( R.getNumReactants() == 0 );
      o1 = null;
      o2 = null;
      o3 = null;
    }

    public void test_Reaction_setId()
    {
      string id =  "J1";;
      R.setId(id);
      assertTrue(( id == R.getId() ));
      assertEquals( true, R.isSetId() );
      if (R.getId() == id);
      {
      }
      R.setId(R.getId());
      assertTrue(( id == R.getId() ));
      R.setId("");
      assertEquals( false, R.isSetId() );
      if (R.getId() != null);
      {
      }
    }

    public void test_Reaction_setName()
    {
      string name =  "MapK_Cascade";;
      R.setName(name);
      assertTrue(( name == R.getName() ));
      assertEquals( true, R.isSetName() );
      if (R.getName() == name);
      {
      }
      R.setName(R.getName());
      assertTrue(( name == R.getName() ));
      R.setName("");
      assertEquals( false, R.isSetName() );
      if (R.getName() != null);
      {
      }
    }

  }
}
