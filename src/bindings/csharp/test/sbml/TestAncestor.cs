/// 
///  @file    TestAncestor.cs
///  @brief   SBML ancestor objects unit tests
///  @author  Frank Bergmann (Csharp conversion)
///  @author  Akiya Jouraku (Csharp conversion)
///  @author  Sarah Keating 
/// 
///  $Id$
///  $HeadURL$
/// 
///  This test file was converted from src/sbml/test/TestAncestor.cpp
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

  public class TestAncestor {
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


    public void test_AlgebraicRule_ancestor_create()
    {
      Model m = new Model(2,4);
      AlgebraicRule r = m.createAlgebraicRule();
      ListOf lo = m.getListOfRules();
      assertTrue( r.getAncestorOfType(libsbml.SBML_MODEL) == m );
      assertTrue( r.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( r.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( r.getAncestorOfType(libsbml.SBML_EVENT) == null );
      Rule obj = m.getRule(0);
      assertTrue( obj.getAncestorOfType(libsbml.SBML_MODEL) == m );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_EVENT) == null );
    }

    public void test_AssignmentRule_ancestor_create()
    {
      Model m = new Model(2,4);
      AssignmentRule r = m.createAssignmentRule();
      ListOf lo = m.getListOfRules();
      assertTrue( r.getAncestorOfType(libsbml.SBML_MODEL) == m );
      assertTrue( r.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( r.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( r.getAncestorOfType(libsbml.SBML_EVENT) == null );
      Rule obj = m.getRule(0);
      assertTrue( obj.getAncestorOfType(libsbml.SBML_MODEL) == m );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_EVENT) == null );
    }

    public void test_CompartmentType_ancestor_add()
    {
      CompartmentType ct = new CompartmentType(2,4);
      Model m = new Model(2,4);
      ct.setId("ct");
      m.addCompartmentType(ct);
      ct = null;
      ListOf lo = m.getListOfCompartmentTypes();
      CompartmentType obj = m.getCompartmentType(0);
      assertTrue( obj.getAncestorOfType(libsbml.SBML_MODEL) == m );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_EVENT) == null );
    }

    public void test_CompartmentType_ancestor_create()
    {
      Model m = new Model(2,4);
      CompartmentType ct = m.createCompartmentType();
      ListOf lo = m.getListOfCompartmentTypes();
      assertTrue( ct.getAncestorOfType(libsbml.SBML_MODEL) == m );
      assertTrue( ct.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( ct.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( ct.getAncestorOfType(libsbml.SBML_EVENT) == null );
      CompartmentType obj = m.getCompartmentType(0);
      assertTrue( obj.getAncestorOfType(libsbml.SBML_MODEL) == m );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_EVENT) == null );
    }

    public void test_Compartment_ancestor_add()
    {
      Compartment c = new Compartment(2,4);
      c.setId("C");
      Model m = new Model(2,4);
      m.addCompartment(c);
      c = null;
      ListOf lo = m.getListOfCompartments();
      Compartment obj = m.getCompartment(0);
      assertTrue( obj.getAncestorOfType(libsbml.SBML_MODEL) == m );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_EVENT) == null );
    }

    public void test_Compartment_ancestor_create()
    {
      Model m = new Model(2,4);
      Compartment c = m.createCompartment();
      ListOf lo = m.getListOfCompartments();
      assertTrue( c.getAncestorOfType(libsbml.SBML_MODEL) == m );
      assertTrue( c.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( c.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( c.getAncestorOfType(libsbml.SBML_EVENT) == null );
      Compartment obj = m.getCompartment(0);
      assertTrue( obj.getAncestorOfType(libsbml.SBML_MODEL) == m );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_EVENT) == null );
    }

    public void test_Constraint_ancestor_add()
    {
      Constraint ct = new Constraint(2,4);
      Model m = new Model(2,4);
      ct.setMath(libsbml.parseFormula("k+k"));
      m.addConstraint(ct);
      ct = null;
      ListOf lo = m.getListOfConstraints();
      Constraint obj = m.getConstraint(0);
      assertTrue( obj.getAncestorOfType(libsbml.SBML_MODEL) == m );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_EVENT) == null );
    }

    public void test_Constraint_ancestor_create()
    {
      Model m = new Model(2,4);
      Constraint ct = m.createConstraint();
      ListOf lo = m.getListOfConstraints();
      assertTrue( ct.getAncestorOfType(libsbml.SBML_MODEL) == m );
      assertTrue( ct.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( ct.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( ct.getAncestorOfType(libsbml.SBML_EVENT) == null );
      Constraint obj = m.getConstraint(0);
      assertTrue( obj.getAncestorOfType(libsbml.SBML_MODEL) == m );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_EVENT) == null );
    }

    public void test_Delay_ancestor_add()
    {
      Delay d = new Delay(2,4);
      Event e = new Event(2,4);
      e.setDelay(d);
      d = null;
      Delay obj = e.getDelay();
      assertTrue( obj.getAncestorOfType(libsbml.SBML_EVENT) == e );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_MODEL) == null );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      e = null;
    }

    public void test_Delay_ancestor_create()
    {
      Event e = new Event(2,4);
      Delay ea = e.createDelay();
      assertTrue( ea.getAncestorOfType(libsbml.SBML_EVENT) == e );
      assertTrue( ea.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( ea.getAncestorOfType(libsbml.SBML_COMPARTMENT) == null );
      Delay obj = e.getDelay();
      assertTrue( obj.getAncestorOfType(libsbml.SBML_EVENT) == e );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_COMPARTMENT) == null );
    }

    public void test_Delay_ancestor_create_model()
    {
      Model m = new Model(2,4);
      Event e = m.createEvent();
      Delay ea = m.createDelay();
      assertTrue( ea.getAncestorOfType(libsbml.SBML_EVENT) == e );
      assertTrue( ea.getAncestorOfType(libsbml.SBML_MODEL) == m );
      assertTrue( ea.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( ea.getAncestorOfType(libsbml.SBML_COMPARTMENT) == null );
      Delay obj = e.getDelay();
      assertTrue( obj.getAncestorOfType(libsbml.SBML_EVENT) == e );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_MODEL) == m );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_COMPARTMENT) == null );
    }

    public void test_EventAssignment_ancestor_add()
    {
      Event e = new Event(2,4);
      EventAssignment ea = new EventAssignment(2,4);
      ea.setVariable("c");
      ea.setMath(libsbml.parseFormula("K+L"));
      e.addEventAssignment(ea);
      ea = null;
      ListOf lo = e.getListOfEventAssignments();
      EventAssignment obj = e.getEventAssignment(0);
      assertTrue( obj.getAncestorOfType(libsbml.SBML_EVENT) == e );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_COMPARTMENT) == null );
    }

    public void test_EventAssignment_ancestor_create()
    {
      Event e = new Event(2,4);
      EventAssignment ea = e.createEventAssignment();
      ListOf lo = e.getListOfEventAssignments();
      assertTrue( ea.getAncestorOfType(libsbml.SBML_EVENT) == e );
      assertTrue( ea.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( ea.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( ea.getAncestorOfType(libsbml.SBML_COMPARTMENT) == null );
      EventAssignment obj = e.getEventAssignment(0);
      assertTrue( obj.getAncestorOfType(libsbml.SBML_EVENT) == e );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_COMPARTMENT) == null );
    }

    public void test_EventAssignment_ancestor_create_model()
    {
      Model m = new Model(2,4);
      Event e = m.createEvent();
      EventAssignment ea = m.createEventAssignment();
      ListOf lo = e.getListOfEventAssignments();
      assertTrue( ea.getAncestorOfType(libsbml.SBML_EVENT) == e );
      assertTrue( ea.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( ea.getAncestorOfType(libsbml.SBML_MODEL) == m );
      assertTrue( ea.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( ea.getAncestorOfType(libsbml.SBML_COMPARTMENT) == null );
      EventAssignment obj = e.getEventAssignment(0);
      assertTrue( obj.getAncestorOfType(libsbml.SBML_EVENT) == e );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_MODEL) == m );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_COMPARTMENT) == null );
    }

    public void test_Event_ancestor_add()
    {
      Event e = new Event(2,4);
      Model m = new Model(2,4);
      Trigger t = new Trigger(2,4);
      e.setTrigger(t);
      e.createEventAssignment();
      m.addEvent(e);
      e = null;
      ListOf lo = m.getListOfEvents();
      Event obj = m.getEvent(0);
      assertTrue( obj.getAncestorOfType(libsbml.SBML_MODEL) == m );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_COMPARTMENT) == null );
    }

    public void test_Event_ancestor_create()
    {
      Model m = new Model(2,4);
      Event e = m.createEvent();
      ListOf lo = m.getListOfEvents();
      assertTrue( e.getAncestorOfType(libsbml.SBML_MODEL) == m );
      assertTrue( e.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( e.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( e.getAncestorOfType(libsbml.SBML_PARAMETER) == null );
      Event obj = m.getEvent(0);
      assertTrue( obj.getAncestorOfType(libsbml.SBML_MODEL) == m );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_EVENT) == null );
    }

    public void test_FunctionDefinition_ancestor_add()
    {
      FunctionDefinition fd = new FunctionDefinition(2,4);
      Model m = new Model(2,4);
      fd.setId("fd");
      fd.setMath(libsbml.parseFormula("l"));
      m.addFunctionDefinition(fd);
      fd = null;
      ListOf lo = m.getListOfFunctionDefinitions();
      FunctionDefinition obj = m.getFunctionDefinition(0);
      assertTrue( obj.getAncestorOfType(libsbml.SBML_MODEL) == m );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_EVENT) == null );
    }

    public void test_FunctionDefinition_ancestor_create()
    {
      Model m = new Model(2,4);
      FunctionDefinition fd = m.createFunctionDefinition();
      ListOf lo = m.getListOfFunctionDefinitions();
      assertTrue( fd.getAncestorOfType(libsbml.SBML_MODEL) == m );
      assertTrue( fd.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( fd.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( fd.getAncestorOfType(libsbml.SBML_EVENT) == null );
      FunctionDefinition obj = m.getFunctionDefinition(0);
      assertTrue( obj.getAncestorOfType(libsbml.SBML_MODEL) == m );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_EVENT) == null );
    }

    public void test_InitialAssignment_ancestor_add()
    {
      InitialAssignment ia = new InitialAssignment(2,4);
      Model m = new Model(2,4);
      ia.setSymbol("c");
      ia.setMath(libsbml.parseFormula("9"));
      m.addInitialAssignment(ia);
      ia = null;
      ListOf lo = m.getListOfInitialAssignments();
      InitialAssignment obj = m.getInitialAssignment(0);
      assertTrue( obj.getAncestorOfType(libsbml.SBML_MODEL) == m );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_EVENT) == null );
    }

    public void test_InitialAssignment_ancestor_create()
    {
      Model m = new Model(2,4);
      InitialAssignment ia = m.createInitialAssignment();
      ListOf lo = m.getListOfInitialAssignments();
      assertTrue( ia.getAncestorOfType(libsbml.SBML_MODEL) == m );
      assertTrue( ia.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( ia.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( ia.getAncestorOfType(libsbml.SBML_EVENT) == null );
      InitialAssignment obj = m.getInitialAssignment(0);
      assertTrue( obj.getAncestorOfType(libsbml.SBML_MODEL) == m );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_EVENT) == null );
    }

    public void test_KineticLaw_Parameter_ancestor_add()
    {
      KineticLaw kl = new KineticLaw(2,4);
      Parameter p = new Parameter(2,4);
      p.setId("jake");
      kl.addParameter(p);
      p = null;
      ListOfParameters lop = kl.getListOfParameters();
      Parameter obj = kl.getParameter(0);
      assertTrue( obj.getAncestorOfType(libsbml.SBML_KINETIC_LAW) == kl );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lop );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_COMPARTMENT) == null );
      kl = null;
    }

    public void test_KineticLaw_Parameter_ancestor_create()
    {
      KineticLaw kl = new KineticLaw(2,4);
      Parameter p = kl.createParameter();
      assertTrue( kl.getNumParameters() == 1 );
      ListOfParameters lop = kl.getListOfParameters();
      assertTrue( p.getAncestorOfType(libsbml.SBML_KINETIC_LAW) == kl );
      assertTrue( p.getAncestorOfType(libsbml.SBML_LIST_OF) == lop );
      assertTrue( p.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( p.getAncestorOfType(libsbml.SBML_COMPARTMENT) == null );
      Parameter obj = kl.getParameter(0);
      assertTrue( obj.getAncestorOfType(libsbml.SBML_KINETIC_LAW) == kl );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lop );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_COMPARTMENT) == null );
      kl = null;
    }

    public void test_KineticLaw_Parameter_ancestor_create_model()
    {
      Model m = new Model(2,4);
      Reaction r = m.createReaction();
      KineticLaw kl = m.createKineticLaw();
      Parameter p = m.createKineticLawParameter();
      assertTrue( kl.getNumParameters() == 1 );
      ListOfParameters lop = kl.getListOfParameters();
      assertTrue( p.getAncestorOfType(libsbml.SBML_KINETIC_LAW) == kl );
      assertTrue( p.getAncestorOfType(libsbml.SBML_LIST_OF) == lop );
      assertTrue( p.getAncestorOfType(libsbml.SBML_REACTION) == r );
      assertTrue( p.getAncestorOfType(libsbml.SBML_MODEL) == m );
      assertTrue( p.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( p.getAncestorOfType(libsbml.SBML_COMPARTMENT) == null );
      Parameter obj = kl.getParameter(0);
      assertTrue( obj.getAncestorOfType(libsbml.SBML_KINETIC_LAW) == kl );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lop );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_REACTION) == r );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_MODEL) == m );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_COMPARTMENT) == null );
      kl = null;
    }

    public void test_KineticLaw_ancestor_add()
    {
      KineticLaw kl = new KineticLaw(2,4);
      Reaction r = new Reaction(2,4);
      r.setKineticLaw(kl);
      KineticLaw obj = r.getKineticLaw();
      assertTrue( obj.getAncestorOfType(libsbml.SBML_REACTION) == r );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_MODEL) == null );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      r = null;
    }

    public void test_KineticLaw_ancestor_create()
    {
      Reaction r = new Reaction(2,4);
      KineticLaw kl = r.createKineticLaw();
      assertTrue( kl.getAncestorOfType(libsbml.SBML_REACTION) == r );
      assertTrue( kl.getAncestorOfType(libsbml.SBML_DELAY) == null );
      assertTrue( kl.getAncestorOfType(libsbml.SBML_MODEL) == null );
      assertTrue( kl.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      KineticLaw obj = r.getKineticLaw();
      assertTrue( obj.getAncestorOfType(libsbml.SBML_REACTION) == r );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_MODEL) == null );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_DELAY) == null );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      r = null;
    }

    public void test_KineticLaw_ancestor_create_model()
    {
      Model m = new Model(2,4);
      Reaction r = m.createReaction();
      KineticLaw kl = r.createKineticLaw();
      assertTrue( kl.getAncestorOfType(libsbml.SBML_REACTION) == r );
      assertTrue( kl.getAncestorOfType(libsbml.SBML_MODEL) == m );
      assertTrue( kl.getAncestorOfType(libsbml.SBML_DELAY) == null );
      assertTrue( kl.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      KineticLaw obj = r.getKineticLaw();
      assertTrue( obj.getAncestorOfType(libsbml.SBML_REACTION) == r );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_MODEL) == m );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_DELAY) == null );
      r = null;
    }

    public void test_Model_ancestor_add()
    {
      SBMLDocument d = new SBMLDocument(2,4);
      Model m = new Model(2,4);
      d.setModel(m);
      assertTrue( d == d.getModel().getAncestorOfType(libsbml.SBML_DOCUMENT) );
      d = null;
    }

    public void test_Model_ancestor_create()
    {
      SBMLDocument d = new SBMLDocument();
      Model m = d.createModel();
      assertTrue( m.getAncestorOfType(libsbml.SBML_DOCUMENT) == d );
      Model obj = d.getModel();
      assertTrue( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == d );
      d = null;
    }

    public void test_Parameter_ancestor_add()
    {
      Parameter ia = new Parameter(2,4);
      Model m = new Model(2,4);
      ia.setId("p");
      m.addParameter(ia);
      ia = null;
      ListOf lo = m.getListOfParameters();
      Parameter obj = m.getParameter(0);
      assertTrue( obj.getAncestorOfType(libsbml.SBML_MODEL) == m );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_EVENT) == null );
    }

    public void test_Parameter_ancestor_create()
    {
      Model m = new Model(2,4);
      Parameter p = m.createParameter();
      ListOf lo = m.getListOfParameters();
      assertTrue( p.getAncestorOfType(libsbml.SBML_MODEL) == m );
      assertTrue( p.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( p.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( p.getAncestorOfType(libsbml.SBML_EVENT) == null );
      Parameter obj = m.getParameter(0);
      assertTrue( obj.getAncestorOfType(libsbml.SBML_MODEL) == m );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_EVENT) == null );
    }

    public void test_RateRule_ancestor_create()
    {
      Model m = new Model(2,4);
      RateRule r = m.createRateRule();
      ListOf lo = m.getListOfRules();
      assertTrue( r.getAncestorOfType(libsbml.SBML_MODEL) == m );
      assertTrue( r.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( r.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( r.getAncestorOfType(libsbml.SBML_EVENT) == null );
      Rule obj = m.getRule(0);
      assertTrue( obj.getAncestorOfType(libsbml.SBML_MODEL) == m );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_EVENT) == null );
    }

    public void test_Reaction_ancestor_add()
    {
      Reaction ia = new Reaction(2,4);
      Model m = new Model(2,4);
      ia.setId("k");
      m.addReaction(ia);
      ia = null;
      ListOf lo = m.getListOfReactions();
      Reaction obj = m.getReaction(0);
      assertTrue( obj.getAncestorOfType(libsbml.SBML_MODEL) == m );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_EVENT) == null );
    }

    public void test_Reaction_ancestor_create()
    {
      Model m = new Model(2,4);
      Reaction r = m.createReaction();
      ListOf lo = m.getListOfReactions();
      assertTrue( r.getAncestorOfType(libsbml.SBML_MODEL) == m );
      assertTrue( r.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( r.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( r.getAncestorOfType(libsbml.SBML_EVENT) == null );
      Reaction obj = m.getReaction(0);
      assertTrue( obj.getAncestorOfType(libsbml.SBML_MODEL) == m );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_EVENT) == null );
    }

    public void test_Rule_ancestor_add()
    {
      Rule ia = new RateRule(2,4);
      ia.setVariable("a");
      ia.setMath(libsbml.parseFormula("9"));
      Model m = new Model(2,4);
      m.addRule(ia);
      ia = null;
      ListOf lo = m.getListOfRules();
      Rule obj = m.getRule(0);
      assertTrue( obj.getAncestorOfType(libsbml.SBML_MODEL) == m );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_EVENT) == null );
    }

    public void test_SpeciesReference_Modifier_ancestor_add()
    {
      ModifierSpeciesReference sr = new ModifierSpeciesReference(2,4);
      sr.setSpecies("s");
      Reaction r = new Reaction(2,4);
      r.addModifier(sr);
      sr = null;
      ListOf lo = r.getListOfModifiers();
      ModifierSpeciesReference obj = r.getModifier(0);
      assertTrue( obj.getAncestorOfType(libsbml.SBML_REACTION) == r );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_COMPARTMENT) == null );
    }

    public void test_SpeciesReference_Modifier_ancestor_create()
    {
      Reaction r = new Reaction(2,4);
      ModifierSpeciesReference sr = r.createModifier();
      ListOf lo = r.getListOfModifiers();
      assertTrue( sr.getAncestorOfType(libsbml.SBML_REACTION) == r );
      assertTrue( sr.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( sr.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( sr.getAncestorOfType(libsbml.SBML_COMPARTMENT) == null );
      ModifierSpeciesReference obj = r.getModifier(0);
      assertTrue( obj.getAncestorOfType(libsbml.SBML_REACTION) == r );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_COMPARTMENT) == null );
    }

    public void test_SpeciesReference_Modifier_ancestor_create_model()
    {
      Model m = new Model(2,4);
      Reaction r = m.createReaction();
      ModifierSpeciesReference sr = m.createModifier();
      ListOf lo = r.getListOfModifiers();
      assertTrue( sr.getAncestorOfType(libsbml.SBML_REACTION) == r );
      assertTrue( sr.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( sr.getAncestorOfType(libsbml.SBML_MODEL) == m );
      assertTrue( sr.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( sr.getAncestorOfType(libsbml.SBML_COMPARTMENT) == null );
      ModifierSpeciesReference obj = r.getModifier(0);
      assertTrue( obj.getAncestorOfType(libsbml.SBML_REACTION) == r );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_MODEL) == m );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_COMPARTMENT) == null );
    }

    public void test_SpeciesReference_Product_ancestor_add()
    {
      SpeciesReference sr = new SpeciesReference(2,4);
      Reaction r = new Reaction(2,4);
      sr.setSpecies("p");
      r.addProduct(sr);
      sr = null;
      ListOf lo = r.getListOfProducts();
      SpeciesReference obj = r.getProduct(0);
      assertTrue( obj.getAncestorOfType(libsbml.SBML_REACTION) == r );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_COMPARTMENT) == null );
    }

    public void test_SpeciesReference_Product_ancestor_create()
    {
      Reaction r = new Reaction(2,4);
      SpeciesReference sr = r.createProduct();
      ListOf lo = r.getListOfProducts();
      assertTrue( sr.getAncestorOfType(libsbml.SBML_REACTION) == r );
      assertTrue( sr.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( sr.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( sr.getAncestorOfType(libsbml.SBML_COMPARTMENT) == null );
      SpeciesReference obj = r.getProduct(0);
      assertTrue( obj.getAncestorOfType(libsbml.SBML_REACTION) == r );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_COMPARTMENT) == null );
    }

    public void test_SpeciesReference_Product_ancestor_create_model()
    {
      Model m = new Model(2,4);
      Reaction r = m.createReaction();
      SpeciesReference sr = m.createProduct();
      ListOf lo = r.getListOfProducts();
      assertTrue( sr.getAncestorOfType(libsbml.SBML_REACTION) == r );
      assertTrue( sr.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( sr.getAncestorOfType(libsbml.SBML_MODEL) == m );
      assertTrue( sr.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( sr.getAncestorOfType(libsbml.SBML_COMPARTMENT) == null );
      SpeciesReference obj = r.getProduct(0);
      assertTrue( obj.getAncestorOfType(libsbml.SBML_REACTION) == r );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_MODEL) == m );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_COMPARTMENT) == null );
    }

    public void test_SpeciesReference_Reactant_ancestor_add()
    {
      SpeciesReference sr = new SpeciesReference(2,4);
      Reaction r = new Reaction(2,4);
      sr.setSpecies("s");
      r.addReactant(sr);
      sr = null;
      ListOf lo = r.getListOfReactants();
      SpeciesReference obj = r.getReactant(0);
      assertTrue( obj.getAncestorOfType(libsbml.SBML_REACTION) == r );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_COMPARTMENT) == null );
    }

    public void test_SpeciesReference_Reactant_ancestor_create()
    {
      Reaction r = new Reaction(2,4);
      SpeciesReference sr = r.createReactant();
      ListOf lo = r.getListOfReactants();
      assertTrue( sr.getAncestorOfType(libsbml.SBML_REACTION) == r );
      assertTrue( sr.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( sr.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( sr.getAncestorOfType(libsbml.SBML_COMPARTMENT) == null );
      SpeciesReference obj = r.getReactant(0);
      assertTrue( obj.getAncestorOfType(libsbml.SBML_REACTION) == r );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_COMPARTMENT) == null );
    }

    public void test_SpeciesReference_Reactant_ancestor_create_model()
    {
      Model m = new Model(2,4);
      Reaction r = m.createReaction();
      SpeciesReference sr = m.createReactant();
      ListOf lo = r.getListOfReactants();
      assertTrue( sr.getAncestorOfType(libsbml.SBML_REACTION) == r );
      assertTrue( sr.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( sr.getAncestorOfType(libsbml.SBML_MODEL) == m );
      assertTrue( sr.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( sr.getAncestorOfType(libsbml.SBML_COMPARTMENT) == null );
      SpeciesReference obj = r.getReactant(0);
      assertTrue( obj.getAncestorOfType(libsbml.SBML_REACTION) == r );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_MODEL) == m );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_COMPARTMENT) == null );
    }

    public void test_SpeciesType_ancestor_add()
    {
      SpeciesType ia = new SpeciesType(2,4);
      Model m = new Model(2,4);
      ia.setId("s");
      m.addSpeciesType(ia);
      ia = null;
      ListOf lo = m.getListOfSpeciesTypes();
      SpeciesType obj = m.getSpeciesType(0);
      assertTrue( obj.getAncestorOfType(libsbml.SBML_MODEL) == m );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_EVENT) == null );
    }

    public void test_SpeciesType_ancestor_create()
    {
      Model m = new Model(2,4);
      SpeciesType st = m.createSpeciesType();
      ListOf lo = m.getListOfSpeciesTypes();
      assertTrue( st.getAncestorOfType(libsbml.SBML_MODEL) == m );
      assertTrue( st.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( st.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( st.getAncestorOfType(libsbml.SBML_EVENT) == null );
      SpeciesType obj = m.getSpeciesType(0);
      assertTrue( obj.getAncestorOfType(libsbml.SBML_MODEL) == m );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_EVENT) == null );
    }

    public void test_Species_ancestor_add()
    {
      Species ia = new Species(2,4);
      Model m = new Model(2,4);
      ia.setId("s");
      ia.setCompartment("c");
      m.addSpecies(ia);
      ia = null;
      ListOf lo = m.getListOfSpecies();
      Species obj = m.getSpecies(0);
      assertTrue( obj.getAncestorOfType(libsbml.SBML_MODEL) == m );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_EVENT) == null );
    }

    public void test_Species_ancestor_create()
    {
      Model m = new Model(2,4);
      Species s = m.createSpecies();
      ListOf lo = m.getListOfSpecies();
      assertTrue( s.getAncestorOfType(libsbml.SBML_MODEL) == m );
      assertTrue( s.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( s.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( s.getAncestorOfType(libsbml.SBML_EVENT) == null );
      Species obj = m.getSpecies(0);
      assertTrue( obj.getAncestorOfType(libsbml.SBML_MODEL) == m );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_EVENT) == null );
    }

    public void test_StoichiometryMath_ancestor_add()
    {
      StoichiometryMath m = new StoichiometryMath(2,4);
      SpeciesReference sr = new SpeciesReference(2,4);
      sr.setStoichiometryMath(m);
      m = null;
      StoichiometryMath obj = sr.getStoichiometryMath();
      assertTrue( obj.getAncestorOfType(libsbml.SBML_SPECIES_REFERENCE) == sr );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_MODEL) == null );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      sr = null;
    }

    public void test_StoichiometryMath_ancestor_create()
    {
      SpeciesReference sr = new SpeciesReference(2,4);
      StoichiometryMath sm = sr.createStoichiometryMath();
      assertTrue( sm.getAncestorOfType(libsbml.SBML_SPECIES_REFERENCE) == sr );
      assertTrue( sm.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( sm.getAncestorOfType(libsbml.SBML_COMPARTMENT) == null );
      StoichiometryMath obj = sr.getStoichiometryMath();
      assertTrue( obj.getAncestorOfType(libsbml.SBML_SPECIES_REFERENCE) == sr );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_COMPARTMENT) == null );
    }

    public void test_Trigger_ancestor_add()
    {
      Trigger d = new Trigger(2,4);
      Event e = new Event(2,4);
      e.setTrigger(d);
      d = null;
      Trigger obj = e.getTrigger();
      assertTrue( obj.getAncestorOfType(libsbml.SBML_EVENT) == e );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_MODEL) == null );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      e = null;
    }

    public void test_Trigger_ancestor_create()
    {
      Event e = new Event(2,4);
      Trigger ea = e.createTrigger();
      assertTrue( ea.getAncestorOfType(libsbml.SBML_EVENT) == e );
      assertTrue( ea.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( ea.getAncestorOfType(libsbml.SBML_COMPARTMENT) == null );
      Trigger obj = e.getTrigger();
      assertTrue( obj.getAncestorOfType(libsbml.SBML_EVENT) == e );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_COMPARTMENT) == null );
    }

    public void test_Trigger_ancestor_create_model()
    {
      Model m = new Model(2,4);
      Event e = m.createEvent();
      Trigger ea = m.createTrigger();
      assertTrue( ea.getAncestorOfType(libsbml.SBML_EVENT) == e );
      assertTrue( ea.getAncestorOfType(libsbml.SBML_MODEL) == m );
      assertTrue( ea.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( ea.getAncestorOfType(libsbml.SBML_COMPARTMENT) == null );
      Trigger obj = e.getTrigger();
      assertTrue( obj.getAncestorOfType(libsbml.SBML_EVENT) == e );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_MODEL) == m );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_COMPARTMENT) == null );
    }

    public void test_UnitDefinition_ancestor_add()
    {
      UnitDefinition ia = new UnitDefinition(2,4);
      Model m = new Model(2,4);
      ia.setId("u");
      ia.createUnit();
      m.addUnitDefinition(ia);
      ia = null;
      ListOf lo = m.getListOfUnitDefinitions();
      UnitDefinition obj = m.getUnitDefinition(0);
      assertTrue( obj.getAncestorOfType(libsbml.SBML_MODEL) == m );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_EVENT) == null );
    }

    public void test_UnitDefinition_ancestor_create()
    {
      Model m = new Model(2,4);
      UnitDefinition ud = m.createUnitDefinition();
      ListOf lo = m.getListOfUnitDefinitions();
      assertTrue( ud.getAncestorOfType(libsbml.SBML_MODEL) == m );
      assertTrue( ud.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( ud.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( ud.getAncestorOfType(libsbml.SBML_EVENT) == null );
      UnitDefinition obj = m.getUnitDefinition(0);
      assertTrue( obj.getAncestorOfType(libsbml.SBML_MODEL) == m );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_EVENT) == null );
    }

    public void test_Unit_ancestor_add()
    {
      UnitDefinition ud = new UnitDefinition(2,4);
      Unit u = new Unit(2,4);
      u.setKind(libsbml.UNIT_KIND_MOLE);
      ud.addUnit(u);
      u = null;
      assertTrue( ud.getNumUnits() == 1 );
      ListOf lo = ud.getListOfUnits();
      Unit obj = ud.getUnit(0);
      assertTrue( obj.getAncestorOfType(libsbml.SBML_UNIT_DEFINITION) == ud );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_COMPARTMENT) == null );
      ud = null;
    }

    public void test_Unit_ancestor_create()
    {
      UnitDefinition ud = new UnitDefinition(2,4);
      Unit u = ud.createUnit();
      assertTrue( ud.getNumUnits() == 1 );
      ListOf lo = ud.getListOfUnits();
      assertTrue( u.getAncestorOfType(libsbml.SBML_UNIT_DEFINITION) == ud );
      assertTrue( u.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( u.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( u.getAncestorOfType(libsbml.SBML_COMPARTMENT) == null );
      Unit obj = ud.getUnit(0);
      assertTrue( obj.getAncestorOfType(libsbml.SBML_UNIT_DEFINITION) == ud );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_COMPARTMENT) == null );
      ud = null;
    }

    public void test_Unit_ancestor_create_model()
    {
      Model m = new Model(2,4);
      UnitDefinition ud = m.createUnitDefinition();
      Unit u = m.createUnit();
      assertTrue( ud.getNumUnits() == 1 );
      ListOf lo = ud.getListOfUnits();
      assertTrue( u.getAncestorOfType(libsbml.SBML_UNIT_DEFINITION) == ud );
      assertTrue( u.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( u.getAncestorOfType(libsbml.SBML_MODEL) == m );
      assertTrue( u.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( u.getAncestorOfType(libsbml.SBML_COMPARTMENT) == null );
      Unit obj = ud.getUnit(0);
      assertTrue( obj.getAncestorOfType(libsbml.SBML_UNIT_DEFINITION) == ud );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_LIST_OF) == lo );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_DOCUMENT) == null );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_MODEL) == m );
      assertTrue( obj.getAncestorOfType(libsbml.SBML_COMPARTMENT) == null );
      ud = null;
    }

  }
}
