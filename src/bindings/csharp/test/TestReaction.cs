/// 
///  @file    TestReaction.cs
///  @brief   SBML Reaction unit tests
///  @author  Frank Bergmann (Csharp conversion)
///  @author  Akiya Jouraku (Csharp conversion)
///  @author  Ben Bornstein 
/// 
///  $Id$
///  $URL$
/// 
///  This test file was converted from src/sbml/test/TestReaction.c
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

    private Reaction R;

    public void setUp()
    {
      R = new  Reaction();
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
      R.addModifier(new ModifierSpeciesReference());
      assertTrue( R.getNumReactants() == 0 );
      assertTrue( R.getNumProducts() == 0 );
      assertTrue( R.getNumModifiers() == 1 );
    }

    public void test_Reaction_addProduct()
    {
      SpeciesReference sr = new  SpeciesReference();
      R.addProduct(sr);
      assertTrue( R.getNumReactants() == 0 );
      assertTrue( R.getNumProducts() == 1 );
      assertTrue( R.getNumModifiers() == 0 );
      sr = null;
    }

    public void test_Reaction_addReactant()
    {
      SpeciesReference sr = new  SpeciesReference();
      R.addReactant(sr);
      assertTrue( R.getNumReactants() == 1 );
      assertTrue( R.getNumProducts() == 0 );
      assertTrue( R.getNumModifiers() == 0 );
      sr = null;
    }

    public void test_Reaction_create()
    {
      assertTrue( R.getTypeCode() == libsbml.SBML_REACTION );
      assertTrue( R.getMetaId()== "" == true );
      assertTrue( R.getNotes() == null );
      assertTrue( R.getAnnotation() == null );
      assertTrue( R.getId()== "" == true );
      assertTrue( R.getName()== "" == true );
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

    public void test_Reaction_createWith()
    {
      KineticLaw kl = new  KineticLaw();
      Reaction r = new  Reaction("r1", "",kl,false);
      r.setFast(true);
      assertTrue( r.getTypeCode() == libsbml.SBML_REACTION );
      assertTrue( r.getMetaId()== "" == true );
      assertTrue( r.getNotes() == null );
      assertTrue( r.getAnnotation() == null );
      assertTrue( r.getName()== "" == true );
      assertTrue((  "r1" == r.getId() ));
      assertTrue( r.getReversible() == false );
      assertTrue( r.getFast() == true );
      assertEquals( true, r.isSetId() );
      assertEquals( false, r.isSetName() );
      assertEquals( true, r.isSetKineticLaw() );
      assertTrue( r.getNumReactants() == 0 );
      assertTrue( r.getNumProducts() == 0 );
      assertTrue( r.getNumModifiers() == 0 );
      kl = null;
      r = null;
    }

    public void test_Reaction_free_NULL()
    {
    }

    public void test_Reaction_getModifier()
    {
      ModifierSpeciesReference msr1 = new ModifierSpeciesReference();
      ModifierSpeciesReference msr2 = new ModifierSpeciesReference();
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
      ModifierSpeciesReference msr1 = new ModifierSpeciesReference();
      ModifierSpeciesReference msr2 = new ModifierSpeciesReference();
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
      SpeciesReference sr1 = new  SpeciesReference();
      SpeciesReference sr2 = new  SpeciesReference();
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
      SpeciesReference sr1 = new  SpeciesReference("P1",1,1);
      SpeciesReference sr2 = new  SpeciesReference("P2",1,1);
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
      SpeciesReference sr1 = new  SpeciesReference();
      SpeciesReference sr2 = new  SpeciesReference();
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
      SpeciesReference sr1 = new  SpeciesReference("R1",1,1);
      SpeciesReference sr2 = new  SpeciesReference("R2",1,1);
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

    public void test_Reaction_setId()
    {
      string id = "J1";
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
      string name = "MapK Cascade";
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
