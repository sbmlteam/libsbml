/*
 *
 * @file    TestSBMLConstructorException.cs
 * @brief   SBMLConstructorException unit tests
 *
 * @author  Akiya Jouraku
 *
 * $Id$
 * $HeadURL$
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2009 California Institute of Technology.
 * Copyright 2002-2005 California Institute of Technology and
 *                     Japan Science and Technology Corporation.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 *--------------------------------------------------------------------------->*/

namespace LibSBMLCSTest {

  using libsbml;

  using  System.IO;

  public class TestSBMLConstructorException {
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
  
    private string ErrMsg = "Level/version/namespaces combination is invalid";
    private SBMLNamespaces SN11 = new SBMLNamespaces(1,1);
    private SBMLNamespaces SN12 = new SBMLNamespaces(1,2);
    private SBMLNamespaces SN21 = new SBMLNamespaces(2,1);
    private SBMLNamespaces SN22 = new SBMLNamespaces(2,2);
    private SBMLNamespaces SN23 = new SBMLNamespaces(2,3);
    private SBMLNamespaces SN24 = new SBMLNamespaces(2,4);
  
    public void test_CompartmentType_constructor()
    {
      SBase s; 
  
      try
      {
        s = new CompartmentType(2,2);
        s = new CompartmentType(2,3);
        s = new CompartmentType(2,4);
        s = new CompartmentType(SN22);
        s = new CompartmentType(SN23);
        s = new CompartmentType(SN24);
      }
      catch (SBMLConstructorException e)
      {
         s = null;
      }
      assertTrue(s != null);
  
      string msg = "";
  
      try
      {
        s = new CompartmentType(1,1);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);
      
      msg = "";
      try
      {
        s = new CompartmentType(1,2);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);
  
      msg = "";
      try
      {
        s = new CompartmentType(2,1);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);

      msg = "";
      try
      {
        s = new CompartmentType(SN11);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);

      msg = "";
      try
      {
        s = new CompartmentType(SN12);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);

      msg = "";
      try
      {
        s = new CompartmentType(SN21);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);
  
      msg = "";
      try
      {
        s = new CompartmentType(99,99);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
    }
  
    public void test_Compartment_constructor()
    {
      SBase s; 
  
      try
      {
        s = new Compartment(1,1);
        s = new Compartment(1,2);
        s = new Compartment(2,1);
        s = new Compartment(2,2);
        s = new Compartment(2,3);
        s = new Compartment(2,4);
        s = new Compartment(SN11);
        s = new Compartment(SN12);
        s = new Compartment(SN21);
        s = new Compartment(SN22);
        s = new Compartment(SN23);
        s = new Compartment(SN24);
      }
      catch (SBMLConstructorException e)
      {
         s = null;
      }
      assertTrue(s != null);
  
      string msg = "";
      
      try
      {
        s = new Compartment(99,99);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);
    }
  
    public void test_Constraint_constructor()
    {
      SBase s; 
  
      try
      {
        s = new Constraint(2,2);
        s = new Constraint(2,3);
        s = new Constraint(2,4);
        s = new Constraint(SN22);
        s = new Constraint(SN23);
        s = new Constraint(SN24);
      }
      catch (SBMLConstructorException e)
      {
         s = null;
      }
      assertTrue(s != null);
  
      string msg = "";
  
      try
      {
        s = new Constraint(1,1);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);
      
      msg = "";
      try
      {
        s = new Constraint(1,2);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);
  
      msg = "";
      try
      {
        s = new Constraint(2,1);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);

      msg = "";
      try
      {
        s = new Constraint(SN11);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);

      msg = "";
      try
      {
        s = new Constraint(SN12);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);

      msg = "";
      try
      {
        s = new Constraint(SN21);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);
  
      msg = "";
      try
      {
        s = new Constraint(99,99);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);
  
    }
  
    public void test_InitialAssignment_constructor()
    {
      SBase s; 
  
      try
      {
        s = new InitialAssignment(2,2);
        s = new InitialAssignment(2,3);
        s = new InitialAssignment(2,4);
        s = new InitialAssignment(SN22);
        s = new InitialAssignment(SN23);
        s = new InitialAssignment(SN24);
      }
      catch (SBMLConstructorException e)
      {
         s = null;
      }
      assertTrue(s != null);
  
      string msg = "";
  
      try
      {
        s = new InitialAssignment(1,1);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);
      
      msg = "";
      try
      {
        s = new InitialAssignment(1,2);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);
  
      msg = "";
      try
      {
        s = new InitialAssignment(2,1);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);

      msg = "";
      try
      {
        s = new InitialAssignment(SN11);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);

      msg = "";
      try
      {
        s = new InitialAssignment(SN12);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);

      msg = "";
      try
      {
        s = new InitialAssignment(SN21);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);
  
      msg = "";
      try
      {
        s = new InitialAssignment(99,99);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);
  
    }
  
    public void test_Species_constructor()
    {
      SBase s; 
  
      try
      {
        s = new Species(1,1);
        s = new Species(1,2);
        s = new Species(2,1);
        s = new Species(2,2);
        s = new Species(2,3);
        s = new Species(2,4);
        s = new Species(SN11);
        s = new Species(SN12);
        s = new Species(SN21);
        s = new Species(SN22);
        s = new Species(SN23);
        s = new Species(SN24);
      }
      catch (SBMLConstructorException e)
      {
         s = null;
      }
      assertTrue(s != null);
  
      string msg = "";
      
      try
      {
        s = new Species(99,99);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);
    }
  
    public void test_SpeciesType_constructor()
    {
      SBase s; 
  
      try
      {
        s = new SpeciesType(2,2);
        s = new SpeciesType(2,3);
        s = new SpeciesType(2,4);
        s = new SpeciesType(SN22);
        s = new SpeciesType(SN23);
        s = new SpeciesType(SN24);
      }
      catch (SBMLConstructorException e)
      {
         s = null;
      }
      assertTrue(s != null);
  
      string msg = "";
  
      try
      {
        s = new SpeciesType(1,1);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);
      
      msg = "";
      try
      {
        s = new SpeciesType(1,2);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);
  
      msg = "";
      try
      {
        s = new SpeciesType(2,1);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);

      msg = "";
      try
      {
        s = new SpeciesType(SN11);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);

      msg = "";
      try
      {
        s = new SpeciesType(SN12);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);

      msg = "";
      try
      {
        s = new SpeciesType(SN21);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);
  
      msg = "";
      try
      {
        s = new SpeciesType(99,99);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);
  
    }
  
    public void test_Delay_constructor()
    {
      SBase s; 
  
      try
      {
        s = new Delay(2,1);
        s = new Delay(2,2);
        s = new Delay(2,3);
        s = new Delay(2,4);
        s = new Delay(SN21);
        s = new Delay(SN22);
        s = new Delay(SN23);
        s = new Delay(SN24);
      }
      catch (SBMLConstructorException e)
      {
         s = null;
      }
      assertTrue(s != null);
  
      string msg = "";
  
      try
      {
        s = new Delay(1,1);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);
      
      msg = "";
      try
      {
        s = new Delay(1,2);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);

      msg = "";
      try
      {
        s = new Delay(SN11);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);

      msg = "";
      try
      {
        s = new Delay(SN12);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);

      try
      {
        s = new Delay(99,99);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);
    }
  
    public void test_Trigger_constructor()
    {
      SBase s; 
  
      try
      {
        s = new Trigger(2,1);
        s = new Trigger(2,2);
        s = new Trigger(2,3);
        s = new Trigger(2,4);
        s = new Trigger(SN21);
        s = new Trigger(SN22);
        s = new Trigger(SN23);
        s = new Trigger(SN24);
      }
      catch (SBMLConstructorException e)
      {
         s = null;
      }
      assertTrue(s != null);
  
      string msg = "";
  
      try
      {
        s = new Trigger(1,1);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);
      
      msg = "";
      try
      {
        s = new Trigger(1,2);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);

      msg = "";
      try
      {
        s = new Trigger(SN11);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);

      msg = "";
      try
      {
        s = new Trigger(SN12);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);

      try
      {
        s = new Trigger(99,99);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);
    }
  
    public void test_Event_constructor()
    {
      SBase s; 
  
      try
      {
        s = new Event(2,1);
        s = new Event(2,2);
        s = new Event(2,3);
        s = new Event(2,4);
        s = new Event(SN21);
        s = new Event(SN22);
        s = new Event(SN23);
        s = new Event(SN24);
      }
      catch (SBMLConstructorException e)
      {
         s = null;
      }
      assertTrue(s != null);
  
      string msg = "";
  
      try
      {
        s = new Event(1,1);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);
      
      msg = "";
      try
      {
        s = new Event(1,2);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);

      msg = "";
      try
      {
        s = new Event(SN11);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);

      msg = "";
      try
      {
        s = new Event(SN12);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);

      try
      {
        s = new Event(99,99);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);
    }
  
    public void test_EventAssignment_constructor()
    {
      SBase s; 
  
      try
      {
        s = new EventAssignment(2,1);
        s = new EventAssignment(2,2);
        s = new EventAssignment(2,3);
        s = new EventAssignment(2,4);
        s = new EventAssignment(SN21);
        s = new EventAssignment(SN22);
        s = new EventAssignment(SN23);
        s = new EventAssignment(SN24);
      }
      catch (SBMLConstructorException e)
      {
         s = null;
      }
      assertTrue(s != null);
  
      string msg = "";
  
      try
      {
        s = new EventAssignment(1,1);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);
      
      msg = "";
      try
      {
        s = new EventAssignment(1,2);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);

      msg = "";
      try
      {
        s = new EventAssignment(SN11);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);

      msg = "";
      try
      {
        s = new EventAssignment(SN12);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);

      try
      {
        s = new EventAssignment(99,99);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);
    }
  
    public void test_ModifierSpeciesReference_constructor()
    {
      SBase s; 
  
      try
      {
        s = new ModifierSpeciesReference(2,1);
        s = new ModifierSpeciesReference(2,2);
        s = new ModifierSpeciesReference(2,3);
        s = new ModifierSpeciesReference(2,4);
        s = new ModifierSpeciesReference(SN21);
        s = new ModifierSpeciesReference(SN22);
        s = new ModifierSpeciesReference(SN23);
        s = new ModifierSpeciesReference(SN24);
      }
      catch (SBMLConstructorException e)
      {
         s = null;
      }
      assertTrue(s != null);
  
      string msg = "";
  
      try
      {
        s = new ModifierSpeciesReference(1,1);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);
      
      msg = "";
      try
      {
        s = new ModifierSpeciesReference(1,2);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);

      msg = "";
      try
      {
        s = new ModifierSpeciesReference(SN11);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);

      msg = "";
      try
      {
        s = new ModifierSpeciesReference(SN12);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);

      try
      {
        s = new ModifierSpeciesReference(99,99);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);
    }
  
    public void test_StoichiometryMath_constructor()
    {
      SBase s; 
  
      try
      {
        s = new StoichiometryMath(2,1);
        s = new StoichiometryMath(2,2);
        s = new StoichiometryMath(2,3);
        s = new StoichiometryMath(2,4);
        s = new StoichiometryMath(SN21);
        s = new StoichiometryMath(SN22);
        s = new StoichiometryMath(SN23);
        s = new StoichiometryMath(SN24);
      }
      catch (SBMLConstructorException e)
      {
         s = null;
      }
      assertTrue(s != null);
  
      string msg = "";
  
      try
      {
        s = new StoichiometryMath(1,1);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);
      
      msg = "";
      try
      {
        s = new StoichiometryMath(1,2);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);

      msg = "";
      try
      {
        s = new StoichiometryMath(SN11);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);

      msg = "";
      try
      {
        s = new StoichiometryMath(SN12);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);

      try
      {
        s = new StoichiometryMath(99,99);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);
    }
  
  
    public void test_SpeciesReference_constructor()
    {
      SBase s; 
  
      try
      {
        s = new SpeciesReference(1,1);
        s = new SpeciesReference(1,2);
        s = new SpeciesReference(2,1);
        s = new SpeciesReference(2,2);
        s = new SpeciesReference(2,3);
        s = new SpeciesReference(2,4);
        s = new SpeciesReference(SN11);
        s = new SpeciesReference(SN12);
        s = new SpeciesReference(SN21);
        s = new SpeciesReference(SN22);
        s = new SpeciesReference(SN23);
        s = new SpeciesReference(SN24);
      }
      catch (SBMLConstructorException e)
      {
         s = null;
      }
      assertTrue(s != null);
  
      string msg = "";
      
      try
      {
        s = new SpeciesReference(99,99);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);
    }
  
    public void test_FunctionDefinition_constructor()
    {
      SBase s; 
  
      try
      {
        s = new FunctionDefinition(2,1);
        s = new FunctionDefinition(2,2);
        s = new FunctionDefinition(2,3);
        s = new FunctionDefinition(2,4);
        s = new FunctionDefinition(SN21);
        s = new FunctionDefinition(SN22);
        s = new FunctionDefinition(SN23);
        s = new FunctionDefinition(SN24);
      }
      catch (SBMLConstructorException e)
      {
         s = null;
      }
      assertTrue(s != null);
  
      string msg = "";
  
      try
      {
        s = new FunctionDefinition(1,1);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);
      
      msg = "";
      try
      {
        s = new FunctionDefinition(1,2);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);

      msg = "";
      try
      {
        s = new FunctionDefinition(SN11);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);

      msg = "";
      try
      {
        s = new FunctionDefinition(SN12);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);

      try
      {
        s = new FunctionDefinition(99,99);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);
    }
  
    public void test_KineticLaw_constructor()
    {
      SBase s; 
  
      try
      {
        s = new KineticLaw(1,1);
        s = new KineticLaw(1,2);
        s = new KineticLaw(2,1);
        s = new KineticLaw(2,2);
        s = new KineticLaw(2,3);
        s = new KineticLaw(2,4);
        s = new KineticLaw(SN11);
        s = new KineticLaw(SN12);
        s = new KineticLaw(SN21);
        s = new KineticLaw(SN22);
        s = new KineticLaw(SN23);
        s = new KineticLaw(SN24);
      }
      catch (SBMLConstructorException e)
      {
         s = null;
      }
      assertTrue(s != null);
  
      string msg = "";
      
      try
      {
        s = new KineticLaw(99,99);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);
    }
  
  
    public void test_Model_constructor()
    {
      SBase s; 
  
      try
      {
        s = new Model(1,1);
        s = new Model(1,2);
        s = new Model(2,1);
        s = new Model(2,2);
        s = new Model(2,3);
        s = new Model(2,4);
        s = new Model(SN11);
        s = new Model(SN12);
        s = new Model(SN21);
        s = new Model(SN22);
        s = new Model(SN23);
        s = new Model(SN24);
      }
      catch (SBMLConstructorException e)
      {
         s = null;
      }
      assertTrue(s != null);
  
      string msg = "";
      
      try
      {
        s = new Model(99,99);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);
    }
  
    public void test_Parameter_constructor()
    {
      SBase s; 
  
      try
      {
        s = new Parameter(1,1);
        s = new Parameter(1,2);
        s = new Parameter(2,1);
        s = new Parameter(2,2);
        s = new Parameter(2,3);
        s = new Parameter(2,4);
        s = new Parameter(SN11);
        s = new Parameter(SN12);
        s = new Parameter(SN21);
        s = new Parameter(SN22);
        s = new Parameter(SN23);
        s = new Parameter(SN24);
      }
      catch (SBMLConstructorException e)
      {
         s = null;
      }
      assertTrue(s != null);
  
      string msg = "";
      
      try
      {
        s = new Parameter(99,99);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);
    }
  
    public void test_Reaction_constructor()
    {
      SBase s; 
  
      try
      {
        s = new Reaction(1,1);
        s = new Reaction(1,2);
        s = new Reaction(2,1);
        s = new Reaction(2,2);
        s = new Reaction(2,3);
        s = new Reaction(2,4);
        s = new Reaction(SN11);
        s = new Reaction(SN12);
        s = new Reaction(SN21);
        s = new Reaction(SN22);
        s = new Reaction(SN23);
        s = new Reaction(SN24);
      }
      catch (SBMLConstructorException e)
      {
         s = null;
      }
      assertTrue(s != null);
  
      string msg = "";
      
      try
      {
        s = new Reaction(99,99);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);
    }
  
    public void test_Unit_constructor()
    {
      SBase s; 
  
      try
      {
        s = new Unit(1,1);
        s = new Unit(1,2);
        s = new Unit(2,1);
        s = new Unit(2,2);
        s = new Unit(2,3);
        s = new Unit(2,4);
        s = new Unit(SN11);
        s = new Unit(SN12);
        s = new Unit(SN21);
        s = new Unit(SN22);
        s = new Unit(SN23);
        s = new Unit(SN24);
      }
      catch (SBMLConstructorException e)
      {
         s = null;
      }
      assertTrue(s != null);
  
      string msg = "";
      
      try
      {
        s = new Unit(99,99);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);
    }
  
    public void test_UnitDefinition_constructor()
    {
      SBase s; 
  
      try
      {
        s = new UnitDefinition(1,1);
        s = new UnitDefinition(1,2);
        s = new UnitDefinition(2,1);
        s = new UnitDefinition(2,2);
        s = new UnitDefinition(2,3);
        s = new UnitDefinition(2,4);
        s = new UnitDefinition(SN11);
        s = new UnitDefinition(SN12);
        s = new UnitDefinition(SN21);
        s = new UnitDefinition(SN22);
        s = new UnitDefinition(SN23);
        s = new UnitDefinition(SN24);
      }
      catch (SBMLConstructorException e)
      {
         s = null;
      }
      assertTrue(s != null);
  
      string msg = "";
      
      try
      {
        s = new UnitDefinition(99,99);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);
    }
  
    public void test_AssignmentRule_constructor()
    {
      SBase s; 
  
      try
      {
        s = new AssignmentRule(1,1);
        s = new AssignmentRule(1,2);
        s = new AssignmentRule(2,1);
        s = new AssignmentRule(2,2);
        s = new AssignmentRule(2,3);
        s = new AssignmentRule(2,4);
        s = new AssignmentRule(SN11);
        s = new AssignmentRule(SN12);
        s = new AssignmentRule(SN21);
        s = new AssignmentRule(SN22);
        s = new AssignmentRule(SN23);
        s = new AssignmentRule(SN24);
      }
      catch (SBMLConstructorException e)
      {
         s = null;
      }
      assertTrue(s != null);
  
      string msg = "";
  
      try
      {
        s = new AssignmentRule(99,99);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);
    }
  
  
    public void test_AlgebraicRule_constructor()
    {
      SBase s; 
  
      try
      {
        s = new AlgebraicRule(1,1);
        s = new AlgebraicRule(1,2);
        s = new AlgebraicRule(2,1);
        s = new AlgebraicRule(2,2);
        s = new AlgebraicRule(2,3);
        s = new AlgebraicRule(2,4);
        s = new AlgebraicRule(SN11);
        s = new AlgebraicRule(SN12);
        s = new AlgebraicRule(SN21);
        s = new AlgebraicRule(SN22);
        s = new AlgebraicRule(SN23);
        s = new AlgebraicRule(SN24);
      }
      catch (SBMLConstructorException e)
      {
         s = null;
      }
      assertTrue(s != null);
  
      string msg = "";
  
      try
      {
        s = new AlgebraicRule(99,99);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);
    }
  
    public void test_RateRule_constructor()
    {
      SBase s; 
  
      try
      {
        s = new RateRule(1,1);
        s = new RateRule(1,2);
        s = new RateRule(2,1);
        s = new RateRule(2,2);
        s = new RateRule(2,3);
        s = new RateRule(2,4);
        s = new RateRule(SN11);
        s = new RateRule(SN12);
        s = new RateRule(SN21);
        s = new RateRule(SN22);
        s = new RateRule(SN23);
        s = new RateRule(SN24);
      }
      catch (SBMLConstructorException e)
      {
         s = null;
      }
      assertTrue(s != null);
  
      string msg = "";
  
      try
      {
        s = new RateRule(99,99);
      }
      catch (SBMLConstructorException e)
      {
         msg = e.Message;
      }
      assertTrue(msg == ErrMsg);
    }
  
  }
}            
