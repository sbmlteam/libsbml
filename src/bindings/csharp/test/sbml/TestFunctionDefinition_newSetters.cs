/// 
///  @file    TestFunctionDefinition_newSetters.cs
///  @brief   FunctionDefinition unit tests for new set function API
///  @author  Frank Bergmann (Csharp conversion)
///  @author  Akiya Jouraku (Csharp conversion)
///  @author  Sarah Keating 
/// 
///  $Id$
///  $HeadURL$
/// 
///  This test file was converted from src/sbml/test/TestFunctionDefinition_newSetters.c
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

  public class TestFunctionDefinition_newSetters {
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

    private FunctionDefinition E;

    public void setUp()
    {
      E = new  FunctionDefinition(2,4);
      if (E == null);
      {
      }
    }

    public void tearDown()
    {
      E = null;
    }

    public void test_FunctionDefinition_setId1()
    {
      string id =  "1e1";;
      long i = E.setId(id);
      assertTrue( i == libsbml.LIBSBML_INVALID_ATTRIBUTE_VALUE );
      assertEquals( false, E.isSetId() );
    }

    public void test_FunctionDefinition_setId2()
    {
      string id =  "e1";;
      long i = E.setId(id);
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertTrue(( id == E.getId() ));
      assertEquals( true, E.isSetId() );
      i = E.setId("");
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertEquals( false, E.isSetId() );
    }

    public void test_FunctionDefinition_setMath1()
    {
      ASTNode math = libsbml.parseFormula("2 * k");
      long i = E.setMath(math);
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertTrue( E.getMath() != math );
      assertEquals( true, E.isSetMath() );
      i = E.setMath(null);
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertTrue( E.getMath() == null );
      assertEquals( false, E.isSetMath() );
      math = null;
    }

    public void test_FunctionDefinition_setMath2()
    {
      ASTNode math = new  ASTNode(libsbml.AST_TIMES);
      long i = E.setMath(math);
      assertTrue( i == libsbml.LIBSBML_INVALID_OBJECT );
      assertEquals( false, E.isSetMath() );
      math = null;
    }

    public void test_FunctionDefinition_setName1()
    {
      string name =  "3Set_k2";;
      long i = E.setName(name);
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertEquals( true, E.isSetName() );
    }

    public void test_FunctionDefinition_setName2()
    {
      string name =  "Set k2";;
      long i = E.setName(name);
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertTrue(( name == E.getName() ));
      assertEquals( true, E.isSetName() );
      i = E.unsetName();
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertEquals( false, E.isSetName() );
    }

    public void test_FunctionDefinition_setName3()
    {
      long i = E.setName("");
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertEquals( false, E.isSetName() );
    }

  }
}
