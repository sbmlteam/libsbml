/// 
///  @file    TestReadFromFile6.cs
///  @brief   Reads test-data/l2v2-newComponents.xml into memory and tests it.
///  @author  Frank Bergmann (Csharp conversion)
///  @author  Akiya Jouraku (Csharp conversion)
///  @author  Sarah Keating 
/// 
///  $Id$
///  $HeadURL$
/// 
///  This test file was converted from src/sbml/test/TestReadFromFile6.cpp
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

  public class TestReadFromFile6 {
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


    public void test_read_l2v2_newComponents()
    {
      SBMLReader reader = new SBMLReader();
      SBMLDocument d;
      Model m;
      Compartment c;
      CompartmentType ct;
      Species s;
      Parameter p;
      Reaction r;
      SpeciesReference sr;
      KineticLaw kl;
      Constraint con;
      InitialAssignment ia;
      SpeciesType st;
      ListOfCompartmentTypes loct;
      CompartmentType ct1;
      ListOfConstraints locon;
      Constraint con1;
      ListOfInitialAssignments loia;
      InitialAssignment ia1;
      ListOfReactions lor;
      Reaction r1;
      ListOfSpeciesReferences losr;
      SpeciesReference sr1;
      ASTNode ast;
      string filename =  "../../sbml/test/test-data/";
      filename += "l2v2-newComponents.xml";
      d = reader.readSBML(filename);
      if (d == null);
      {
      }
      assertTrue( d.getLevel() == 2 );
      assertTrue( d.getVersion() == 2 );
      m = d.getModel();
      assertTrue( m != null );
      assertTrue( m.getId() ==  "l2v2_newComponents" );
      assertTrue( m.getSBOTerm() == 4 );
      assertTrue( m.getSBOTermID() ==  "SBO:0000004" );
      assertTrue( m.getNumCompartments() == 2 );
      c = m.getCompartment(0);
      assertTrue( c != null );
      assertTrue( c.getId() ==  "cell" );
      assertEquals(c.getCompartmentType(), "mitochondria");
      assertTrue( c.getOutside() ==  "m" );
      assertTrue( c.getSBOTerm() == -1 );
      assertTrue( c.getSBOTermID() ==  "" );
      c = m.getCompartment(1);
      assertTrue( c != null );
      assertTrue( c.getId() ==  "m" );
      assertEquals(c.getCompartmentType(), "mitochondria");
      assertTrue( m.getNumCompartmentTypes() == 1 );
      ct = m.getCompartmentType(0);
      assertTrue( ct != null );
      assertTrue( ct.getId() ==  "mitochondria" );
      loct = m.getListOfCompartmentTypes();
      ct1 = loct.get(0);
      assertTrue( ct1 == ct );
      ct1 = loct.get("mitochondria");
      assertTrue( ct1 == ct );
      assertTrue( m.getNumSpeciesTypes() == 1 );
      st = m.getSpeciesType(0);
      assertTrue( st != null );
      assertTrue( st.getId() ==  "Glucose" );
      assertTrue( m.getNumSpecies() == 2 );
      s = m.getSpecies(0);
      assertTrue( s != null );
      assertTrue( s.getId() ==  "X0" );
      assertEquals(s.getSpeciesType(), "Glucose");
      assertTrue( s.getCompartment() ==  "cell" );
      assertEquals( false, s.isSetInitialAmount() );
      assertEquals( false, s.isSetInitialConcentration() );
      s = m.getSpecies(1);
      assertTrue( s != null );
      assertTrue( s.getId() ==  "X1" );
      assertEquals( false, s.isSetSpeciesType() );
      assertTrue( s.getCompartment() ==  "cell" );
      assertTrue( s.getInitialConcentration() == 0.013 );
      assertEquals( false, s.isSetInitialAmount() );
      assertEquals( true, s.isSetInitialConcentration() );
      assertTrue( m.getNumParameters() == 1 );
      p = m.getParameter(0);
      assertTrue( p != null );
      assertTrue( p.getId() ==  "y" );
      assertTrue( p.getValue() == 2 );
      assertTrue( p.getUnits() ==  "dimensionless" );
      assertTrue( p.getId() ==  "y" );
      assertTrue( p.getSBOTerm() == 2 );
      assertTrue( p.getSBOTermID() ==  "SBO:0000002" );
      assertTrue( m.getNumConstraints() == 1 );
      con = m.getConstraint(0);
      assertTrue( con != null );
      assertTrue( con.getSBOTerm() == 64 );
      assertTrue( con.getSBOTermID() ==  "SBO:0000064" );
      ast = con.getMath();
      assertTrue((  "lt(1, cell)" == libsbml.formulaToString(ast) ));
      locon = m.getListOfConstraints();
      con1 = locon.get(0);
      assertTrue( con1 == con );
      assertTrue( m.getNumInitialAssignments() == 1 );
      ia = m.getInitialAssignment(0);
      assertTrue( ia != null );
      assertTrue( ia.getSBOTerm() == 64 );
      assertTrue( ia.getSBOTermID() ==  "SBO:0000064" );
      assertTrue( ia.getSymbol() ==  "X0" );
      ast = ia.getMath();
      assertTrue((  "y * X1" == libsbml.formulaToString(ast) ));
      loia = m.getListOfInitialAssignments();
      ia1 = loia.get(0);
      assertTrue( ia1 == ia );
      ia1 = loia.get("X0");
      assertTrue( ia1 == ia );
      assertTrue( m.getNumReactions() == 1 );
      r = m.getReaction(0);
      assertTrue( r != null );
      assertTrue( r.getSBOTerm() == 231 );
      assertTrue( r.getSBOTermID() ==  "SBO:0000231" );
      assertTrue( r.getId() ==  "in" );
      lor = m.getListOfReactions();
      r1 = lor.get(0);
      assertTrue( r1 == r );
      r1 = lor.get("in");
      assertTrue( r1 == r );
      assertEquals( true, r.isSetKineticLaw() );
      kl = r.getKineticLaw();
      assertTrue( kl != null );
      assertTrue( kl.getSBOTerm() == 1 );
      assertTrue( kl.getSBOTermID() ==  "SBO:0000001" );
      assertEquals( true, kl.isSetMath() );
      ast = kl.getMath();
      assertTrue((  "v * X0 / t" == libsbml.formulaToString(ast) ));
      assertTrue( kl.getNumParameters() == 2 );
      p = kl.getParameter(0);
      assertTrue( p != null );
      assertTrue( p.getSBOTerm() == 2 );
      assertTrue( p.getSBOTermID() ==  "SBO:0000002" );
      assertTrue( p.getId() ==  "v" );
      assertTrue( p.getUnits() ==  "litre" );
      assertTrue( r.getNumReactants() == 1 );
      assertTrue( r.getNumProducts() == 0 );
      assertTrue( r.getNumModifiers() == 0 );
      sr = r.getReactant(0);
      assertTrue( sr != null );
      assertTrue( sr.getName() ==  "sarah" );
      assertTrue( sr.getId() ==  "me" );
      assertTrue( sr.getSpecies() ==  "X0" );
      losr = r.getListOfReactants();
      sr1 = (SpeciesReference) losr.get(0);
      assertTrue( sr1 == sr );
      sr1 = (SpeciesReference) losr.get("me");
      assertTrue( sr1 == sr );
      d = null;
    }

  }
}
