/// 
///  @file    TestReadFromFile5.cs
///  @brief   Reads test-data/l2v1-assignment.xml into memory and tests it.
///  @author  Frank Bergmann (Csharp conversion)
///  @author  Akiya Jouraku (Csharp conversion)
///  @author  Ben Bornstein 
/// 
///  $Id$
///  $URL$
/// 
///  This test file was converted from src/sbml/test/TestReadFromFile5.cpp
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

  public class TestReadFromFile5 {
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


    public void test_read_l2v1_assignment()
    {
      SBMLReader reader = new SBMLReader();
      SBMLDocument d;
      Model m;
      Compartment c;
      Species s;
      Parameter p;
      AssignmentRule ar;
      Reaction r;
      SpeciesReference sr;
      KineticLaw kl;
      UnitDefinition ud;
      Reaction r1;
      string filename = "../../sbml/test/test-data/";
      filename += "l2v1-assignment.xml";
      d = reader.readSBML(filename);
      if (d == null);
      {
      }
      assertTrue( d.getLevel() == 2 );
      assertTrue( d.getVersion() == 1 );
      m = d.getModel();
      assertTrue( m != null );
      assertTrue( m.getNumCompartments() == 1 );
      c = m.getCompartment(0);
      assertTrue( c != null );
      assertTrue( c.getId()== "cell" == true );
      ud = c.getDerivedUnitDefinition();
      assertTrue( ud.getNumUnits() == 1 );
      assertTrue( ud.getUnit(0).getKind() == libsbml.UNIT_KIND_LITRE );
      assertTrue( m.getNumSpecies() == 5 );
      s = m.getSpecies(0);
      assertTrue( s != null );
      assertTrue( s.getId()== "X0" == true );
      assertTrue( s.getCompartment()== "cell" == true );
      assertTrue( s.getInitialConcentration() == 1.0 );
      s = m.getSpecies(1);
      assertTrue( s != null );
      assertTrue( s.getId()== "X1" == true );
      assertTrue( s.getCompartment()== "cell" == true );
      assertTrue( s.getInitialConcentration() == 0.0 );
      s = m.getSpecies(2);
      assertTrue( s != null );
      assertTrue( s.getId()== "T" == true );
      assertTrue( s.getCompartment()== "cell" == true );
      assertTrue( s.getInitialConcentration() == 0.0 );
      s = m.getSpecies(3);
      assertTrue( s != null );
      assertTrue( s.getId()== "S1" == true );
      assertTrue( s.getCompartment()== "cell" == true );
      assertTrue( s.getInitialConcentration() == 0.0 );
      s = m.getSpecies(4);
      assertTrue( s != null );
      assertTrue( s.getId()== "S2" == true );
      assertTrue( s.getCompartment()== "cell" == true );
      assertTrue( s.getInitialConcentration() == 0.0 );
      assertTrue( m.getNumParameters() == 1 );
      p = m.getParameter(0);
      assertTrue( p != null );
      assertTrue( p.getId()== "Keq" == true );
      assertTrue( p.getValue() == 2.5 );
      ud = p.getDerivedUnitDefinition();
      assertTrue( ud.getNumUnits() == 0 );
      assertTrue( m.getNumRules() == 2 );
      ar = ((AssignmentRule)  m.getRule(0) );
      assertTrue( ar != null );
      assertTrue( ar.getVariable()== "S1" == true );
      assertTrue( ar.getFormula()== "T / (1 + Keq)" == true );
      ud = ar.getDerivedUnitDefinition();
      assertTrue( ud.getNumUnits() == 2 );
      assertTrue( ud.getUnit(0).getKind() == libsbml.UNIT_KIND_MOLE );
      assertTrue( ud.getUnit(0).getExponent() == 1 );
      assertTrue( ud.getUnit(1).getKind() == libsbml.UNIT_KIND_LITRE );
      assertTrue( ud.getUnit(1).getExponent() == -1 );
      assertTrue( ar.containsUndeclaredUnits() == true );
      ar = ((AssignmentRule)  m.getRule(1) );
      assertTrue( ar != null );
      assertTrue( ar.getVariable()== "S2" == true );
      assertTrue( ar.getFormula()== "Keq * S1" == true );
      assertTrue( m.getNumReactions() == 2 );
      r = m.getReaction(0);
      assertTrue( r != null );
      assertTrue( r.getId()== "in" == true );
      assertTrue( r.getNumReactants() == 1 );
      assertTrue( r.getNumProducts() == 1 );
      sr = r.getReactant(0);
      assertTrue( sr != null );
      assertTrue( sr.getSpecies()== "X0" == true );
      sr = r.getProduct(0);
      assertTrue( sr != null );
      assertTrue( sr.getSpecies()== "T" == true );
      kl = r.getKineticLaw();
      assertTrue( kl != null );
      assertTrue( kl.getFormula()== "k1 * X0" == true );
      assertTrue( kl.getNumParameters() == 1 );
      r1 = ((Reaction) kl.getParentSBMLObject());
      assertTrue( r1 != null );
      assertTrue( r1.getId()== "in" == true );
      assertTrue( r1.getNumReactants() == 1 );
      assertTrue( r1.getNumProducts() == 1 );
      p = kl.getParameter(0);
      assertTrue( p != null );
      assertTrue( p.getId()== "k1" == true );
      assertTrue( p.getValue() == 0.1 );
      kl = ((KineticLaw) p.getParentSBMLObject().getParentSBMLObject());
      assertTrue( kl != null );
      assertTrue( kl.getFormula()== "k1 * X0" == true );
      assertTrue( kl.getNumParameters() == 1 );
      r = m.getReaction(1);
      assertTrue( r != null );
      assertTrue( r.getId()== "out" == true );
      assertTrue( r.getNumReactants() == 1 );
      assertTrue( r.getNumProducts() == 1 );
      sr = r.getReactant(0);
      assertTrue( sr != null );
      assertTrue( sr.getSpecies()== "T" == true );
      sr = r.getProduct(0);
      assertTrue( sr != null );
      assertTrue( sr.getSpecies()== "X1" == true );
      kl = r.getKineticLaw();
      assertTrue( kl != null );
      assertTrue( kl.getFormula()== "k2 * T" == true );
      assertTrue( kl.getNumParameters() == 1 );
      p = kl.getParameter(0);
      assertTrue( p != null );
      assertTrue( p.getId()== "k2" == true );
      assertTrue( p.getValue() == 0.15 );
      d = null;
    }

  }
}
