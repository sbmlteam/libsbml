/// 
///  @file    TestSBMLNamespaces.cs
///  @brief   SBMLNamespaces unit tests
///  @author  Frank Bergmann (Csharp conversion)
///  @author  Akiya Jouraku (Csharp conversion)
///  @author  Sarah Keating 
/// 
///  $Id$
///  $HeadURL$
/// 
///  This test file was converted from src/sbml/test/TestSBMLNamespaces.cpp
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

  public class TestSBMLNamespaces {
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


    public void test_SBMLNamespaces_L1V1()
    {
      SBMLNamespaces sbml = new SBMLNamespaces(1,1);
      assertTrue( sbml.getLevel() == 1 );
      assertTrue( sbml.getVersion() == 1 );
      XMLNamespaces ns = sbml.getNamespaces();
      assertTrue( ns.getLength() == 1 );
      assertTrue( ns.getURI(0) ==  "http://www.sbml.org/sbml/level1" );
      assertTrue( ns.getPrefix(0) ==  "" );
      sbml = null;
    }

    public void test_SBMLNamespaces_L1V2()
    {
      SBMLNamespaces sbml = new SBMLNamespaces(1,2);
      assertTrue( sbml.getLevel() == 1 );
      assertTrue( sbml.getVersion() == 2 );
      XMLNamespaces ns = sbml.getNamespaces();
      assertTrue( ns.getLength() == 1 );
      assertTrue( ns.getURI(0) ==  "http://www.sbml.org/sbml/level1" );
      assertTrue( ns.getPrefix(0) ==  "" );
      sbml = null;
    }

    public void test_SBMLNamespaces_L2V1()
    {
      SBMLNamespaces sbml = new SBMLNamespaces(2,1);
      assertTrue( sbml.getLevel() == 2 );
      assertTrue( sbml.getVersion() == 1 );
      XMLNamespaces ns = sbml.getNamespaces();
      assertTrue( ns.getLength() == 1 );
      assertTrue( ns.getURI(0) ==  "http://www.sbml.org/sbml/level2" );
      assertTrue( ns.getPrefix(0) ==  "" );
      sbml = null;
    }

    public void test_SBMLNamespaces_L2V2()
    {
      SBMLNamespaces sbml = new SBMLNamespaces(2,2);
      assertTrue( sbml.getLevel() == 2 );
      assertTrue( sbml.getVersion() == 2 );
      XMLNamespaces ns = sbml.getNamespaces();
      assertTrue( ns.getLength() == 1 );
      assertTrue( ns.getURI(0) ==  "http://www.sbml.org/sbml/level2/version2" );
      assertTrue( ns.getPrefix(0) ==  "" );
      sbml = null;
    }

    public void test_SBMLNamespaces_L2V3()
    {
      SBMLNamespaces sbml = new SBMLNamespaces(2,3);
      assertTrue( sbml.getLevel() == 2 );
      assertTrue( sbml.getVersion() == 3 );
      XMLNamespaces ns = sbml.getNamespaces();
      assertTrue( ns.getLength() == 1 );
      assertTrue( ns.getURI(0) ==  "http://www.sbml.org/sbml/level2/version3" );
      assertTrue( ns.getPrefix(0) ==  "" );
      sbml = null;
    }

    public void test_SBMLNamespaces_L2V4()
    {
      SBMLNamespaces sbml = new SBMLNamespaces(2,4);
      assertTrue( sbml.getLevel() == 2 );
      assertTrue( sbml.getVersion() == 4 );
      XMLNamespaces ns = sbml.getNamespaces();
      assertTrue( ns.getLength() == 1 );
      assertTrue( ns.getURI(0) ==  "http://www.sbml.org/sbml/level2/version4" );
      assertTrue( ns.getPrefix(0) ==  "" );
      sbml = null;
    }

    public void test_SBMLNamespaces_getURI()
    {
      assertTrue( SBMLNamespaces.getSBMLNamespaceURI(1,1) ==                              "http://www.sbml.org/sbml/level1" );
      assertTrue( SBMLNamespaces.getSBMLNamespaceURI(1,2) ==                              "http://www.sbml.org/sbml/level1" );
      assertTrue( SBMLNamespaces.getSBMLNamespaceURI(2,1) ==                              "http://www.sbml.org/sbml/level2" );
      assertTrue( SBMLNamespaces.getSBMLNamespaceURI(2,2) ==                              "http://www.sbml.org/sbml/level2/version2" );
      assertTrue( SBMLNamespaces.getSBMLNamespaceURI(2,3) ==                              "http://www.sbml.org/sbml/level2/version3" );
      assertTrue( SBMLNamespaces.getSBMLNamespaceURI(2,4) ==                              "http://www.sbml.org/sbml/level2/version4" );
    }

  }
}
