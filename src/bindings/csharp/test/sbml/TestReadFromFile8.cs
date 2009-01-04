/// 
///  @file    TestReadFromFile8.cs
///  @brief   Reads test-data/l2v4-new.xml into memory and tests it.
///  @author  Frank Bergmann (Csharp conversion)
///  @author  Akiya Jouraku (Csharp conversion)
///  @author  Sarah Keating 
/// 
///  $Id:$
///  $HeadURL:$
/// 
///  This test file was converted from src/sbml/test/TestReadFromFile8.cpp
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

  public class TestReadFromFile8 {
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


    public void test_read_l2v4_new()
    {
      SBMLReader reader = new SBMLReader();
      SBMLDocument d = new SBMLDocument();
      Model m = new Model();
      Compartment c = new Compartment();
      Event e = new Event();
      Trigger trigger = new Trigger();
      EventAssignment ea = new EventAssignment();
      ASTNode ast = new ASTNode();
      string filename =  "../../sbml/test/test-data/";
      filename += "l2v4-new.xml";
      d = reader.readSBML(filename);
      if (d == null);
      {
      }
      assertTrue( d.getLevel() == 2 );
      assertTrue( d.getVersion() == 4 );
      m = d.getModel();
      assertTrue( m != null );
      assertTrue( m.getId() ==  "l2v4_all" );
      assertTrue( m.getNumCompartments() == 1 );
      c = m.getCompartment(0);
      assertTrue( c != null );
      assertTrue( c.getId() ==  "a" );
      assertTrue( c.getSize() == 1 );
      assertEquals( false, c.getConstant() );
      assertTrue( m.getNumEvents() == 1 );
      e = m.getEvent(0);
      assertTrue( e != null );
      assertEquals( true, e.getUseValuesFromTriggerTime() );
      assertEquals( true, e.isSetTrigger() );
      trigger = e.getTrigger();
      assertTrue( trigger != null );
      ast = trigger.getMath();
      assertTrue((  "lt(x, 3)" == libsbml.formulaToString(ast) ));
      assertTrue( e.getNumEventAssignments() == 1 );
      ea = e.getEventAssignment(0);
      assertTrue( ea != null );
      assertTrue( ea.getVariable() ==  "a" );
      ast = ea.getMath();
      assertTrue((  "x * p3" == libsbml.formulaToString(ast) ));
      d = null;
    }

  }
}
