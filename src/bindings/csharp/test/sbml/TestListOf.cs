/// 
///  @file    TestListOf.cs
///  @brief   ListOf unit tests
///  @author  Frank Bergmann (Csharp conversion)
///  @author  Akiya Jouraku (Csharp conversion)
///  @author  Ben Bornstein 
/// 
///  $Id$
///  $HeadURL$
/// 
///  This test file was converted from src/sbml/test/TestListOf.c
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

  public class TestListOf {
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


    public void test_ListOf_clear()
    {
      ListOf lo = new  ListOf();
      SBase sp = new  Species(2,4);
      lo.append(sp);
      lo.append(sp);
      lo.append(sp);
      lo.append(sp);
      lo.append(sp);
      assertTrue( lo.size() == 5 );
      lo.clear(true);
      assertTrue( lo.size() == 0 );
      lo.append(sp);
      lo.append(sp);
      lo.append(sp);
      lo.append(sp);
      lo.appendAndOwn(sp);
      assertTrue( lo.size() == 5 );
      lo.get(0);
      lo.get(1);
      lo.get(2);
      lo.get(3);
      lo.get(4);
      lo.clear(false);
      assertTrue( lo.size() == 0 );
      lo = null;
    }

    public void test_ListOf_create()
    {
      ListOf lo = new  ListOf();
      assertTrue( lo.getTypeCode() == libsbml.SBML_LIST_OF );
      assertTrue( lo.getNotes() == null );
      assertTrue( lo.getAnnotation() == null );
      assertTrue( lo.getMetaId() == "" );
      assertTrue( lo.size() == 0 );
      lo = null;
    }

    public void test_ListOf_free_NULL()
    {
    }

    public void test_ListOf_remove()
    {
      ListOf lo = new  ListOf();
      SBase sp = new  Species(2,4);
      assertTrue( lo.size() == 0 );
      lo.append(sp);
      lo.append(sp);
      lo.append(sp);
      lo.append(sp);
      lo.append(sp);
      assertTrue( lo.size() == 5 );
      lo.remove(0);
      lo.remove(0);
      lo.remove(0);
      lo.remove(0);
      lo.remove(0);
      assertTrue( lo.size() == 0 );
      lo.append(sp);
      lo.append(sp);
      lo.append(sp);
      lo.append(sp);
      lo.appendAndOwn(sp);
      assertTrue( lo.size() == 5 );
      lo = null;
    }

  }
}
