/// 
///  @file    TestModelHistory_newSetters.cs
///  @brief   ModelHistory unit tests
///  @author  Frank Bergmann (Csharp conversion)
///  @author  Akiya Jouraku (Csharp conversion)
///  @author  Sarah Keating 
/// 
///  $Id$
///  $HeadURL$
/// 
///  This test file was converted from src/sbml/test/TestModelHistory_newSetters.c
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

  public class TestModelHistory_newSetters {
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


    public void test_ModelHistory_addCreator1()
    {
      ModelHistory mh = new  ModelHistory();
      ModelCreator mc = new  ModelCreator();
      mc.setFamilyName( "Keating");
      mc.setGivenName( "Sarah");
      long i = mh.addCreator(mc);
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertTrue( mh.getNumCreators() == 1 );
      mc = null;
      mh = null;
    }

    public void test_ModelHistory_addCreator2()
    {
      ModelHistory mh = new  ModelHistory();
      ModelCreator mc = new  ModelCreator();
      mc.setGivenName( "Sarah");
      long i = mh.addCreator(mc);
      assertTrue( i == libsbml.LIBSBML_INVALID_OBJECT );
      assertTrue( mh.getNumCreators() == 0 );
      mc = null;
      mh = null;
    }

    public void test_ModelHistory_addCreator3()
    {
      ModelHistory mh = new  ModelHistory();
      ModelCreator mc = null;
      long i = mh.addCreator(mc);
      assertTrue( i == libsbml.LIBSBML_OPERATION_FAILED );
      assertTrue( mh.getNumCreators() == 0 );
      mh = null;
    }

    public void test_ModelHistory_setCreatedDate1()
    {
      ModelHistory mh = new  ModelHistory();
      assertTrue( mh != null );
      Date date = new  Date("2005-12-30T12:15:32+02:00");
      long i = mh.setCreatedDate(date);
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertTrue( mh.isSetCreatedDate() == true );
      assertTrue( date != mh.getCreatedDate() );
      string dateChar = mh.getCreatedDate().getDateAsString();
      assertTrue((  "2005-12-30T12:15:32+02:00" == dateChar ));
      i = mh.setCreatedDate(null);
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertTrue( mh.isSetCreatedDate() == false );
      date = null;
      mh = null;
    }

    public void test_ModelHistory_setCreatedDate2()
    {
      ModelHistory mh = new  ModelHistory();
      assertTrue( mh != null );
      Date date = new  Date("Jan 12");
      long i = mh.setCreatedDate(date);
      assertTrue( i == libsbml.LIBSBML_INVALID_OBJECT );
      assertTrue( mh.isSetCreatedDate() == false );
      date = null;
      mh = null;
    }

    public void test_ModelHistory_setModifiedDate1()
    {
      ModelHistory mh = new  ModelHistory();
      assertTrue( mh != null );
      Date date = new  Date("2005-12-30T12:15:32+02:00");
      long i = mh.setModifiedDate(date);
      assertTrue( i == libsbml.LIBSBML_OPERATION_SUCCESS );
      assertTrue( mh.isSetModifiedDate() == true );
      assertTrue( date != mh.getModifiedDate() );
      string dateChar = mh.getModifiedDate().getDateAsString();
      assertTrue((  "2005-12-30T12:15:32+02:00" == dateChar ));
      i = mh.setModifiedDate(null);
      assertTrue( i == libsbml.LIBSBML_OPERATION_FAILED );
      assertTrue( mh.isSetModifiedDate() == true );
      date = null;
      mh = null;
    }

    public void test_ModelHistory_setModifiedDate2()
    {
      ModelHistory mh = new  ModelHistory();
      assertTrue( mh != null );
      Date date = new  Date(200,13,76,56,89,90,0,0,0);
      long i = mh.setModifiedDate(date);
      assertTrue( i == libsbml.LIBSBML_INVALID_OBJECT );
      assertTrue( mh.isSetModifiedDate() == false );
      date = null;
      mh = null;
    }

  }
}
