/// 
///  @file    TestSpeciesReference_newSetters.cs
///  @brief   SpeciesReference unit tests for new set function API
///  @author  Frank Bergmann (Csharp conversion)
///  @author  Akiya Jouraku (Csharp conversion)
///  @author  Sarah Keating 
/// 
///  $Id$
///  $HeadURL$
/// 
///  This test file was converted from src/sbml/test/TestSpeciesReference_newSetters.c
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

  public class TestSpeciesReference_newSetters {
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

    private SpeciesReference sr;

    public void setUp()
    {
      sr = new  SpeciesReference(2,4);
      if (sr == null);
      {
      }
    }

    public void tearDown()
    {
      sr = null;
    }

    public void test_SpeciesReference_setDenominator1()
    {
      long i = sr.setDenominator(2);
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertTrue( sr.getDenominator() == 2 );
    }

    public void test_SpeciesReference_setDenominator2()
    {
      SpeciesReference c = new  SpeciesReference(2,2);
      long i = c.setDenominator(4);
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertTrue( c.getDenominator() == 4 );
      c = null;
    }

    public void test_SpeciesReference_setId1()
    {
      long i = sr.setId( "cell");
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertEquals( true, sr.isSetId() );
      assertTrue((  "cell"  == sr.getId() ));
    }

    public void test_SpeciesReference_setId2()
    {
      long i = sr.setId( "1cell");
      assertTrue( i == libsbml.LIBSBML_INVALID_ATTRIBUTE_VALUE );
      assertEquals( false, sr.isSetId() );
    }

    public void test_SpeciesReference_setId3()
    {
      SpeciesReference c = new  SpeciesReference(2,1);
      long i = c.setId( "cell");
      c = null;
    }

    public void test_SpeciesReference_setId4()
    {
      long i = sr.setId( "cell");
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertEquals( true, sr.isSetId() );
      assertTrue((  "cell"  == sr.getId() ));
      i = sr.setId("");
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertEquals( false, sr.isSetId() );
    }

    public void test_SpeciesReference_setName1()
    {
      long i = sr.setName( "cell");
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertEquals( true, sr.isSetName() );
      i = sr.unsetName();
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertEquals( false, sr.isSetName() );
    }

    public void test_SpeciesReference_setName2()
    {
      long i = sr.setName( "1cell");
      assertTrue( i == libsbml.LIBSBML_INVALID_ATTRIBUTE_VALUE );
      assertEquals( false, sr.isSetName() );
      i = sr.unsetName();
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertEquals( false, sr.isSetName() );
    }

    public void test_SpeciesReference_setName3()
    {
      SpeciesReference c = new  SpeciesReference(2,1);
      long i = c.setName( "cell");
      assertTrue( i == libsbml.LIBSBML_UNEXPECTED_ATTRIBUTE );
      assertEquals( false, c.isSetName() );
      c = null;
    }

    public void test_SpeciesReference_setName4()
    {
      long i = sr.setName( "cell");
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertEquals( true, sr.isSetName() );
      i = sr.setName("");
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertEquals( false, sr.isSetName() );
    }

    public void test_SpeciesReference_setSpecies1()
    {
      long i = sr.setSpecies( "mm");
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertEquals( true, sr.isSetSpecies() );
    }

    public void test_SpeciesReference_setSpecies2()
    {
      SpeciesReference c = new  SpeciesReference(2,2);
      long i = c.setSpecies( "1cell");
      assertTrue( i == libsbml.LIBSBML_INVALID_ATTRIBUTE_VALUE );
      assertEquals( false, c.isSetSpecies() );
      c = null;
    }

    public void test_SpeciesReference_setSpecies3()
    {
      SpeciesReference c = new  SpeciesReference(2,2);
      long i = c.setSpecies( "mole");
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertTrue((  "mole" == c.getSpecies() ));
      assertEquals( true, c.isSetSpecies() );
      c = null;
    }

    public void test_SpeciesReference_setSpecies4()
    {
      long i = sr.setSpecies( "mm");
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertEquals( true, sr.isSetSpecies() );
      i = sr.setSpecies("");
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertEquals( false, sr.isSetSpecies() );
    }

    public void test_SpeciesReference_setStoichiometry1()
    {
      long i = sr.setStoichiometry(2.0);
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertTrue( sr.getStoichiometry() == 2.0 );
    }

    public void test_SpeciesReference_setStoichiometry2()
    {
      SpeciesReference c = new  SpeciesReference(2,2);
      long i = c.setStoichiometry(4);
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertTrue( c.getStoichiometry() == 4.0 );
      c = null;
    }

    public void test_SpeciesReference_setStoichiometryMath1()
    {
      StoichiometryMath sm = new  StoichiometryMath(2,4);
      ASTNode math = new  ASTNode(libsbml.AST_TIMES);
      ASTNode a = new  ASTNode();
      ASTNode b = new  ASTNode();
      a.setName( "a");
      b.setName( "b");
      math.addChild(a);
      math.addChild(b);
      sm.setMath(math);
      long i = sr.setStoichiometryMath(sm);
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertEquals( true, sr.isSetStoichiometryMath() );
      assertTrue( sr.getStoichiometry() == 1 );
      i = sr.unsetStoichiometryMath();
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertEquals( false, sr.isSetStoichiometryMath() );
      sm = null;
    }

    public void test_SpeciesReference_setStoichiometryMath2()
    {
      StoichiometryMath sm = new  StoichiometryMath(2,4);
      ASTNode math = new  ASTNode(libsbml.AST_TIMES);
      ASTNode a = new  ASTNode();
      a.setName( "a");
      math.addChild(a);
      sm.setMath(math);
      long i = sr.setStoichiometryMath(sm);
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertEquals( true, sr.isSetStoichiometryMath() );
      sm = null;
    }

    public void test_SpeciesReference_setStoichiometryMath3()
    {
      long i = sr.setStoichiometryMath(null);
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertEquals( false, sr.isSetStoichiometryMath() );
      i = sr.unsetStoichiometryMath();
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertEquals( false, sr.isSetStoichiometryMath() );
    }

    public void test_SpeciesReference_setStoichiometryMath4()
    {
      StoichiometryMath sm = new  StoichiometryMath(2,4);
      ASTNode math = null;
      sm.setMath(math);
      long i = sr.setStoichiometryMath(sm);
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertEquals( true, sr.isSetStoichiometryMath() );
      assertTrue( sr.getStoichiometry() == 1 );
      i = sr.unsetStoichiometryMath();
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertEquals( false, sr.isSetStoichiometryMath() );
      sm = null;
    }

    public void test_SpeciesReference_setStoichiometryMath5()
    {
      SpeciesReference sr1 = new  SpeciesReference(1,2);
      StoichiometryMath sm = new  StoichiometryMath(2,4);
      ASTNode math = new  ASTNode(libsbml.AST_TIMES);
      ASTNode a = new  ASTNode();
      ASTNode b = new  ASTNode();
      a.setName( "a");
      b.setName( "b");
      math.addChild(a);
      math.addChild(b);
      sm.setMath(math);
      long i = sr1.setStoichiometryMath(sm);
      assertTrue( i == libsbml.LIBSBML_UNEXPECTED_ATTRIBUTE );
      assertEquals( false, sr1.isSetStoichiometryMath() );
      sm = null;
      sr1 = null;
    }

    public void test_SpeciesReference_setStoichiometryMath6()
    {
      StoichiometryMath sm = new  StoichiometryMath(2,1);
      long i = sr.setStoichiometryMath(sm);
      assertTrue( i == libsbml.LIBSBML_VERSION_MISMATCH );
      assertEquals( false, sr.isSetStoichiometryMath() );
      sm = null;
    }

  }
}
