/// 
///  @file    TestEvent.cs
///  @brief   SBML Event unit tests
///  @author  Frank Bergmann (Csharp conversion)
///  @author  Akiya Jouraku (Csharp conversion)
///  @author  Ben Bornstein 
/// 
///  $Id:$
///  $HeadURL:$
/// 
///  This test file was converted from src/sbml/test/TestEvent.c
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

  public class TestEvent {
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

    private Event E;

    public void setUp()
    {
      E = new  Event();
      if (E == null);
      {
      }
    }

    public void tearDown()
    {
      E = null;
    }

    public void test_Event_create()
    {
      assertTrue( E.getTypeCode() == libsbml.SBML_EVENT );
      assertTrue( E.getMetaId() == "" );
      assertTrue( E.getNotes() == null );
      assertTrue( E.getAnnotation() == null );
      assertTrue( E.getId() == "" );
      assertTrue( E.getName() == "" );
      assertEquals(E.getTrigger(),null);
      assertEquals(E.getDelay(),null);
      assertTrue( E.getTimeUnits() == "" );
      assertTrue( E.getNumEventAssignments() == 0 );
    }

    public void test_Event_createWith()
    {
      Event e = new  Event("e1", "");
      assertTrue( e.getTypeCode() == libsbml.SBML_EVENT );
      assertTrue( e.getMetaId() == "" );
      assertTrue( e.getNotes() == null );
      assertTrue( e.getAnnotation() == null );
      assertTrue( e.getName() == "" );
      assertEquals(e.getDelay(),null);
      assertTrue( e.getTimeUnits() == "" );
      assertTrue( e.getNumEventAssignments() == 0 );
      assertEquals( false, e.isSetTrigger() );
      assertTrue((  "e1" == e.getId() ));
      assertEquals( true, e.isSetId() );
      e = null;
    }

    public void test_Event_createWithLevelVersionAndNamespace()
    {
      XMLNamespaces xmlns = new  XMLNamespaces();
      xmlns.add( "http://www.sbml.org", "sbml");
      Event object1 = new  Event(2,4,xmlns);
      assertTrue( object1.getTypeCode() == libsbml.SBML_EVENT );
      assertTrue( object1.getMetaId() == "" );
      assertTrue( object1.getNotes() == null );
      assertTrue( object1.getAnnotation() == null );
      assertTrue( object1.getLevel() == 2 );
      assertTrue( object1.getVersion() == 4 );
      assertTrue( object1.getNamespaces() != null );
      assertTrue( object1.getNamespaces().getLength() == 1 );
      object1 = null;
    }

    public void test_Event_free_NULL()
    {
    }

    public void test_Event_full()
    {
      ASTNode math1 = libsbml.parseFormula("0");
      Trigger trigger = new  Trigger(math1);
      ASTNode math = libsbml.parseFormula("0");
      Event e = new  Event("e1", "");
      EventAssignment ea = new  EventAssignment("k",math);
      e.setTrigger(trigger);
      e.setName( "Set k2 to zero when P1 <= t");
      e.addEventAssignment(ea);
      assertTrue( e.getNumEventAssignments() == 1 );
      assertNotEquals(e.getEventAssignment(0),ea);
      math = null;
      e = null;
    }

    public void test_Event_setDelay()
    {
      ASTNode math1 = libsbml.parseFormula("0");
      Delay Delay = new  Delay(math1);
      E.setDelay(Delay);
      assertNotEquals(E.getDelay(),null);
      assertEquals( true, E.isSetDelay() );
      if (E.getDelay() == Delay);
      {
      }
      E.setDelay(E.getDelay());
      assertNotEquals(E.getDelay(),Delay);
      E.setDelay(null);
      assertEquals( false, E.isSetDelay() );
      if (E.getDelay() != null);
      {
      }
    }

    public void test_Event_setId()
    {
      string id =  "e1";;
      E.setId(id);
      assertTrue(( id == E.getId() ));
      assertEquals( true, E.isSetId() );
      if (E.getId() == id);
      {
      }
      E.setId(E.getId());
      assertTrue(( id == E.getId() ));
      E.setId("");
      assertEquals( false, E.isSetId() );
      if (E.getId() != null);
      {
      }
    }

    public void test_Event_setName()
    {
      string name =  "Set k2 to zero when P1 <= t";;
      E.setName(name);
      assertTrue(( name == E.getName() ));
      assertEquals( true, E.isSetName() );
      if (E.getName() == name);
      {
      }
      E.setName(E.getName());
      assertTrue(( name == E.getName() ));
      E.setName("");
      assertEquals( false, E.isSetName() );
      if (E.getName() != null);
      {
      }
    }

    public void test_Event_setTimeUnits()
    {
      string units =  "second";;
      E.setTimeUnits(units);
      assertTrue(( units == E.getTimeUnits() ));
      assertEquals( true, E.isSetTimeUnits() );
      if (E.getTimeUnits() == units);
      {
      }
      E.setTimeUnits(E.getTimeUnits());
      assertTrue(( units == E.getTimeUnits() ));
      E.setTimeUnits("");
      assertEquals( false, E.isSetTimeUnits() );
      if (E.getTimeUnits() != null);
      {
      }
    }

    public void test_Event_setTrigger()
    {
      ASTNode math1 = libsbml.parseFormula("0");
      Trigger trigger = new  Trigger(math1);
      E.setTrigger(trigger);
      assertNotEquals(E.getTrigger(),null);
      assertEquals( true, E.isSetTrigger() );
      if (E.getTrigger() == trigger);
      {
      }
      E.setTrigger(E.getTrigger());
      assertNotEquals(E.getTrigger(),trigger);
      E.setTrigger(null);
      assertEquals( false, E.isSetTrigger() );
      if (E.getTrigger() != null);
      {
      }
    }

    public void test_Event_setUseValuesFromTriggerTime()
    {
      E.setUseValuesFromTriggerTime(false);
      assertTrue( E.getUseValuesFromTriggerTime() == false );
      E.setUseValuesFromTriggerTime(true);
      assertTrue( E.getUseValuesFromTriggerTime() == true );
    }

  }
}
