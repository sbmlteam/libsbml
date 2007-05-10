/**
 * @file    printMath.java
 * @brief   Prints Rule, Reaction, and Event formulas in a given SBML Document
 * @author  Nicolas Rodriguez (translated from libSBML C++ examples)
 * @author  Sarah Keating
 * @author  Ben Bornstein
 * @author  Michael Hucka
 *
 * $Id$
 * $Source$
 *
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */


import org.sbml.libsbml.ASTNode;
import org.sbml.libsbml.Event;
import org.sbml.libsbml.Delay;
import org.sbml.libsbml.Trigger;
import org.sbml.libsbml.EventAssignment;
import org.sbml.libsbml.FunctionDefinition;
import org.sbml.libsbml.KineticLaw;
import org.sbml.libsbml.Model;
import org.sbml.libsbml.Reaction;
import org.sbml.libsbml.Rule;
import org.sbml.libsbml.SBMLDocument;
import org.sbml.libsbml.SBMLReader;
import org.sbml.libsbml.OstreamWrapper;
import org.sbml.libsbml.libsbml;
 

public class printMath
{
  public static void main (String[] args)
  {        
    if (args.length != 1)
    {
      println("Usage: java printMath <filename>");
      System.exit(1);
    }

    String filename       = args[0];
    OstreamWrapper stderr = new OstreamWrapper(OstreamWrapper.CERR);
    SBMLReader reader     = new SBMLReader();
    SBMLDocument document;
    Model        model;
    int          level, version;

    document = reader.readSBML(filename);

    if (document.getNumErrors() > 0)
    {
      document.printErrors(stderr);
      println("Printing skipped.  Please correct the above problems first.");
      System.exit(1);
    }

    model = document.getModel();

    if (model == null)
    {
      println("There does not appear to be a model in this file");
      System.exit(0);
    }

    level   = (int) document.getLevel();
    version = (int) document.getVersion();

    println("File: " + filename +
            " (Level " + level + ", version " + version + ")");

    printMath(model);
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
      println("Rule " + n + ", formula: " + formula);
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
      formula = libsbml.formulaToString(e.getDelay().getMath());
      println("Event " + n + " delay: " + formula);
    }

    if (e.isSetTrigger())
    {
      formula = libsbml.formulaToString(e.getTrigger().getMath());
      println("Event " + n + " trigger: " + formula);
    }

    for (int i = 0; i < e.getNumEventAssignments(); ++i)
    {
      printEventAssignmentMath(i + 1, e.getEventAssignment(i));
    }

    println("\n");
  }
    
    
  static void printMath (Model model)
  {
    for (int n = 0; n < model.getNumFunctionDefinitions(); ++n)
    {
      printFunctionDefinition(n + 1, model.getFunctionDefinition(n));
      println("");
    }

    for (int n = 0; n < model.getNumRules(); ++n)
    {
      printRuleMath(n + 1, model.getRule(n));
      println("");
    }

    for (int n = 0; n < model.getNumReactions(); ++n)
    {
      printReactionMath(n + 1, model.getReaction(n));
      println("");
    }

    for (int n = 0; n < model.getNumEvents(); ++n)
    {
      printEventMath(n + 1, model.getEvent(n));
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
   * Loads the SWIG-generated libSBML Java module when this class is
   * loaded, or reports a sensible diagnostic message about why it failed.
   */
  static
  {
    String varname;

    if (System.getProperty("mrj.version") != null)
      varname = "DYLD_LIBRARY_PATH";	// We're on a Mac.
    else
      varname = "LD_LIBRARY_PATH";	// We're not on a Mac.

    try
    {
      System.loadLibrary("sbmlj");
      // For extra safety, check that the jar file is in the classpath.
      Class.forName("org.sbml.libsbml.libsbml");
    }
    catch (SecurityException e)
    {
      System.err.println("\nCould not load the libSBML library files due to a"+
			 " security exception.\n");
    }
    catch (UnsatisfiedLinkError e)
    {
      System.err.println("\nError: could not link with the libSBML library."+
			 "  It is likely\nyour " + varname +
			 " environment variable does not include\nthe"+
			 " directory containing the libsbml.dylib library"+
			 " file.\n");
      System.exit(1);
    }
    catch (ClassNotFoundException e)
    {
      System.err.println("\nError: unable to load the file libsbmlj.jar."+
			 "  It is likely\nyour " + varname +
			 " environment variable does not include\nthe "+
			 " directory containing the libsbmlj.jar file.\n");
      System.exit(1);
    }
  }
}
