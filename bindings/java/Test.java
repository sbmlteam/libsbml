/**
 * Filename    : Test.java
 * Description : Simple test of the libSBML Java module
 * Author(s)   : SBML Development Group <sbml-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2004-04-02
 * Revision    : $Id$
 * Source      : $Source$
 *
 * Copyright 2004 California Institute of Technology and
 * Japan Science and Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 2.1 of the License, or
 * any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY, WITHOUT EVEN THE IMPLIED WARRANTY OF
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  The software and
 * documentation provided hereunder is on an "as is" basis, and the
 * California Institute of Technology and Japan Science and Technology
 * Corporation have no obligations to provide maintenance, support,
 * updates, enhancements or modifications.  In no event shall the
 * California Institute of Technology or the Japan Science and Technology
 * Corporation be liable to any party for direct, indirect, special,
 * incidental or consequential damages, including lost profits, arising
 * out of the use of this software and its documentation, even if the
 * California Institute of Technology and/or Japan Science and Technology
 * Corporation have been advised of the possibility of such damage.  See
 * the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 *
 * The original code contained here was initially developed by:
 *
 *     Ben Bornstein and Ben Kovitz
 *     The Systems Biology Markup Language Development Group
 *     ERATO Kitano Symbiotic Systems Project
 *     Control and Dynamical Systems, MC 107-81
 *     California Institute of Technology
 *     Pasadena, CA, 91125, USA
 *
 *     http://www.cds.caltech.edu/erato
 *     mailto:sysbio-team@caltech.edu
 *
 * Contributor(s):
 */


import libsbml.*;


public class Test
{
  public static void main (String args[])
  {
    testImplicitDowncastRule();
    testImplicitDowncastSBase();
    testReadFromFile1();

    System.gc();


    System.out.println("All Tests Passed.");
  }


  static void testImplicitDowncastRule()
  {
    Model m = new Model("m", "MyModel");
    


    m.addRule( new AssignmentRule("x", "1 + 1", libsbml.RULE_TYPE_SCALAR) );
    Assert( m.getNumRules() == 1 );
    Assert( m.getRule(0) instanceof AssignmentRule );
  }
    

  static void testImplicitDowncastSBase()
  {
    ListOf lo = new ListOf();

    lo.append( new Compartment("c") );
    lo.append( new Species("s")     );
    lo.append( new Parameter("p")   );

    /**
     * ListOf.get() returns an object of type SBase, so the result must be
     * downcast to an object of the appropriate type.
     */
    Assert( lo.get(0) instanceof Compartment );
    Assert( lo.get(1) instanceof Species     );
    Assert( lo.get(2) instanceof Parameter   );
  }


  /**
   * Reads the Branch model from the SBML Level 1 version 1 specification
   * and inspections ensures that every SBML object and field was read-in
   * correctly.
   *
   * This test was adapted from one of the first libSBML comprehensive
   * "unit" tests, ../../src/TestReadFromFile1.c.
   */
  static void testReadFromFile1 ()
  {
    SBMLDocument     d;
    Model            m;
    Compartment      c;
    KineticLaw       kl;
    Parameter        p;
    Reaction         r;
    Species          s;
    SpeciesReference sr;

    SBMLReader reader   = new SBMLReader();
    String     filename = "/home/bornstei/checkout/libsbml/src/test-data/l1v1-branch.xml";

    String xml =
        "<?xml version='1.0' encoding='UTF-8'?>"
      + "<sbml xmlns='http://www.sbml.org/sbml/level1' level='1' version='1'>"
      + "<model name='Branch'/>"
      + "</sbml>";

    d = reader.readSBML(filename);
    // d = reader.readSBMLFromString(xml);
    
    if (d == null)
    {
      System.err.println("readSBML('l1v1-branch.xml') returned null.");
      System.exit(0);
    }


    /**
     * <sbml level="1" version="1" ...>
     */
    Assert( d.getLevel  () == 1 );
    Assert( d.getVersion() == 1 );

    /**
     * <model name="Branch">
     */
    m = d.getModel();

    Assert( m.getName().equals("Branch") );


    /**
     * <listOfCompartments>
     *  <compartment name="compartmentOne" volume="1"/>
     * </listOfCompartments>
     */
    Assert( m.getNumCompartments() == 1 );

    c = m.getCompartment(0);
    Assert( c.getName().equals("compartmentOne") );
    Assert( c.getVolume() == 1.0 );


    /**
     * <listOfSpecies>
     *   <specie name="S1" initialAmount="0" compartment="compartmentOne"
     *           boundaryCondition="false"/>
     *   <specie name="X0" initialAmount="0" compartment="compartmentOne"
     *           boundaryCondition="true"/>
     *   <specie name="X1" initialAmount="0" compartment="compartmentOne"
     *           boundaryCondition="true"/>
     *   <specie name="X2" initialAmount="0" compartment="compartmentOne"
     *           boundaryCondition="true"/>
     * </listOfSpecies>
     */
    Assert( m.getNumSpecies() == 4 );

    s = m.getSpecies(0);
    Assert( s.getName().equals("S1") );
    Assert( s.getCompartment().equals("compartmentOne") );
    Assert( s.getInitialAmount    () == 0.0   );
    Assert( s.getBoundaryCondition() == false );

    s = m.getSpecies(1);
    Assert( s.getName().equals("X0") );
    Assert( s.getCompartment().equals("compartmentOne") );
    Assert( s.getInitialAmount    () == 0.0   );
    Assert( s.getBoundaryCondition() == true  );

    s = m.getSpecies(2);
    Assert( s.getName().equals("X1") );
    Assert( s.getCompartment().equals("compartmentOne") );
    Assert( s.getInitialAmount    () == 0.0  );
    Assert( s.getBoundaryCondition() == true );

    s = m.getSpecies(3);
    Assert( s.getName().equals("X2") );
    Assert( s.getCompartment().equals("compartmentOne") );
    Assert( s.getInitialAmount    () == 0.0  );
    Assert( s.getBoundaryCondition() == true );


    /**
     * <listOfReactions>
     *   <reaction name="reaction_1" reversible="false"> ... </reaction>
     *   <reaction name="reaction_2" reversible="false"> ... </reaction>
     *   <reaction name="reaction_3" reversible="false"> ... </reaction>
     * </listOfReactions>
     */
    Assert( m.getNumReactions() == 3 );

    r = m.getReaction(0);
    Assert( r.getName().equals("reaction_1") );
    Assert( r.getReversible() == false );
    Assert( r.getFast      () == false );

    r = m.getReaction(1);
    Assert( r.getName().equals("reaction_2") );
    Assert( r.getReversible() == false );
    Assert( r.getFast      () == false );

    r = m.getReaction(2);
    Assert( r.getName().equals("reaction_3") );
    Assert( r.getReversible() == false );
    Assert( r.getFast      () == false );

    /**
     * <reaction name="reaction_1" reversible="false">
     *   <listOfReactants>
     *     <specieReference specie="X0" stoichiometry="1"/>
     *   </listOfReactants>
     *   <listOfProducts>
     *     <specieReference specie="S1" stoichiometry="1"/>
     *   </listOfProducts>
     *   <kineticLaw formula="k1 * X0">
     *     <listOfParameters>
     *       <parameter name="k1" value="0"/>
     *     </listOfParameters>
     *   </kineticLaw>
     * </reaction>
     */
    r = m.getReaction(0);

    Assert( r.getNumReactants() == 1 );
    Assert( r.getNumProducts()  == 1 );

    sr = r.getReactant(0);
    Assert( sr.getSpecies().equals("X0") );
    Assert( sr.getStoichiometry() == 1 );
    Assert( sr.getDenominator()   == 1 );

    sr = r.getProduct(0);
    Assert( sr.getSpecies().equals("S1") );
    Assert( sr.getStoichiometry() == 1 );
    Assert( sr.getDenominator  () == 1 );

    kl = r.getKineticLaw();
    Assert( kl.getFormula().equals("k1 * X0") );
    Assert( kl.getNumParameters() == 1 );

    p = kl.getParameter(0);
    Assert( p.getName().equals("k1") );
    Assert( p.getValue() == 0 );


    /**
     * <reaction name="reaction_2" reversible="false">
     *   <listOfReactants>
     *     <specieReference specie="S1" stoichiometry="1"/>
     *   </listOfReactants>
     *   <listOfProducts>
     *     <specieReference specie="X1" stoichiometry="1"/>
     *   </listOfProducts>
     *   <kineticLaw formula="k2 * S1">
     *     <listOfParameters>
     *       <parameter name="k2" value="0"/>
     *     </listOfParameters>
     *   </kineticLaw>
     * </reaction>
     */
    r = m.getReaction(1);
    Assert( r.getNumReactants() == 1 );
    Assert( r.getNumProducts()  == 1 );

    sr = r.getReactant(0);
    Assert( sr.getSpecies().equals("S1") );
    Assert( sr.getStoichiometry() == 1 );
    Assert( sr.getDenominator  () == 1 );

    sr = r.getProduct(0);
    Assert( sr.getSpecies().equals("X1") );
    Assert( sr.getStoichiometry() == 1 );
    Assert( sr.getDenominator  () == 1 );

    kl = r.getKineticLaw();
    Assert( kl.getFormula().equals("k2 * S1") );
    Assert( kl.getNumParameters() == 1 );

    p = kl.getParameter(0);
    Assert( p.getName().equals("k2") );
    Assert( p.getValue() == 0 );


    /**
     * <reaction name="reaction_3" reversible="false">
     *   <listOfReactants>
     *     <specieReference specie="S1" stoichiometry="1"/>
     *   </listOfReactants>
     *   <listOfProducts>
     *     <specieReference specie="X2" stoichiometry="1"/>
     *   </listOfProducts>
     *   <kineticLaw formula="k3 * S1">
     *     <listOfParameters>
     *       <parameter name="k3" value="0"/>
     *     </listOfParameters>
     *   </kineticLaw>
     * </reaction>
     */
    r = m.getReaction(2);
    Assert( r.getNumReactants() == 1 );
    Assert( r.getNumProducts()  == 1 );

    sr = r.getReactant(0);
    Assert( sr.getSpecies().equals("S1") );
    Assert( sr.getStoichiometry() == 1 );
    Assert( sr.getDenominator  () == 1 );

    sr = r.getProduct(0);
    Assert( sr.getSpecies().equals("X2") );
    Assert( sr.getStoichiometry() == 1 );
    Assert( sr.getDenominator  () == 1 );

    kl = r.getKineticLaw();
    Assert( kl.getFormula().equals("k3 * S1") );
    Assert( kl.getNumParameters() == 1 );

    p = kl.getParameter(0);
    Assert( p.getName().equals("k3") );
    Assert( p.getValue() == 0 );
  }
    

  /**
   * Exits with an "Assertion failed!" message if condition is false.
   */
  static void Assert(boolean condition)
  {
    if (condition == false)
    {
      System.err.println("Assertion failed!");
      System.exit(0);
    }
  }

  
  /**
   * Loads the SWIG generated libsbml Java module when this class is
   * loaded.
   */
  static
  {
    System.loadLibrary("sbmlj");
  }
}
