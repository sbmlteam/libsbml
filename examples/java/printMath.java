/**
 * \file    printMath.java
 * \brief   Prints Rule, Reaction, and Event formulas in a given SBML Document
 * \author  Nicolas Rodriguez (translated from libSBML C++ examples)
 *
 * $Id$
 * $Source$
 */
/* Copyright 2005 California Institute of Technology and
 * Japan Science and Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2.1 of the License, or any
 * later version.
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
 * incidental or consequential damages, including lost profits, arising out
 * of the use of this software and its documentation, even if the
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
 *     Ben Bornstein
 *
 *     The SBML Team
 *     Control and Dynamical Systems, MC 107-81
 *     California Institute of Technology
 *     Pasadena, CA, 91125, USA
 *
 *     http://sbml.org
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s):
 *   Nicolas Rodriguez - Translated from C++ examples to Java
 */


import org.sbml.libsbml.ASTNode;
import org.sbml.libsbml.Event;
import org.sbml.libsbml.EventAssignment;
import org.sbml.libsbml.FunctionDefinition;
import org.sbml.libsbml.KineticLaw;
import org.sbml.libsbml.Model;
import org.sbml.libsbml.ParseMessage;
import org.sbml.libsbml.Reaction;
import org.sbml.libsbml.Rule;
import org.sbml.libsbml.SBMLDocument;
import org.sbml.libsbml.SBMLReader;
import org.sbml.libsbml.libsbml;

 

public class printMath
{
  public static void main (String[] args)
  {        
    if (args.length != 1)
    {
      println("  usage: java printMath <filename>");
      return;
    }

    long start, stop, size;
    long errors;
    int  level, version;


    SBMLDocument d;
    Model        m;
    SBMLReader   sr = new SBMLReader();

    String filename = args[0];

    start = System.currentTimeMillis();
    d     = sr.readSBML(filename);
    stop  = System.currentTimeMillis();

    errors = d.getNumWarnings() + d.getNumErrors() + d.getNumFatals();

    if (errors > 0)
    {
      libSBMLHelper.printErrors(d);
      return;
    }

    m = d.getModel();

    if (m == null)
    {
      return;
    }

    level   = (int) d.getLevel();
    version = (int) d.getVersion();

    println("File: " + filename +
            " (Level " + level + ", version " + version + ")\n");

    printMath(m);
  }

 
  static void printFunctionDefinition (int n, FunctionDefinition fd)
  {
    ASTNode math;
    String  formula;


    if (fd.isSetMath())
    {
      print("FunctionDefinition " + n + ", " + fd.getId() + "(");

      math = fd.getMath();

      /* Print function arguments. */
      if (math.getNumChildren() > 1)
      {
        print(" " + math.getLeftChild().getName());
                
        for (int i = 1; n < math.getNumChildren() - 1; ++n)
        {
          print(",  " + math.getChild(i).getName());
        }
      }

      print(") := ");

      /* Print function body. */
      if (math.getNumChildren() == 0)
      {
        println("(no body defined)");
      }
      else
      {
        math    = math.getChild(math.getNumChildren() - 1);
        formula = libsbml.formulaToString(math);
        println(formula);
      }
    }
  }


  static void printRuleMath (int n, Rule r)
  {
    String formula;


    if (r.isSetMath())
    {
      formula = libsbml.formulaToString(r.getMath());
      println("Rule " + n + ", formula: " + formula + "\n");
    }
  }
    
    
  static void printReactionMath (int n, Reaction r)
  {
    String     formula;
    KineticLaw kl;


    if (r.isSetKineticLaw())
    {
      kl = r.getKineticLaw();
            
      if ( kl.isSetMath() )
      {
        formula = libsbml.formulaToString( kl.getMath() );
        println("Reaction " + n + ", formula: " + formula);
      }
    }
  }

    
  static void printEventAssignmentMath (int n, EventAssignment ea)
  {
    if (ea.isSetMath())
    {
      String variable = ea.getVariable();
      String formula  = libsbml.formulaToString( ea.getMath() );
            
      println("  EventAssignment " + n + ", trigger: " + variable + " = " +
              formula);
    }
  }


  static void printEventMath (int n, Event e)
  {
    String formula;


    if (e.isSetDelay())
    {
      formula = libsbml.formulaToString(e.getDelay());
      println("Event " + n + " delay: " + formula);
    }

    if (e.isSetTrigger())
    {
      formula = libsbml.formulaToString(e.getTrigger());
      println("Event " + n + " trigger: " + formula);
    }

    for (int i = 0; i < e.getNumEventAssignments(); ++i)
    {
      printEventAssignmentMath(i + 1, e.getEventAssignment(i));
    }

    println("\n");
  }
    
    
  static void printMath (Model m)
  {
    for (int n = 0; n < m.getNumFunctionDefinitions(); ++n)
    {
      printFunctionDefinition(n + 1, m.getFunctionDefinition(n));
    }

    for (int n = 0; n < m.getNumRules(); ++n)
    {
      printRuleMath(n + 1, m.getRule(n));
    }

    println("");

    for (int n = 0; n < m.getNumReactions(); ++n)
    {
      printReactionMath(n + 1, m.getReaction(n));
    }

    println("");

    for (int n = 0; n < m.getNumEvents(); ++n)
    {
      printEventMath(n + 1, m.getEvent(n));
    }
  }


  static void print (String msg)
  {
    System.out.print(msg);
  }


  static void println (String msg)
  {
    System.out.println(msg);
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
