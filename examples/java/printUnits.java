/**
 * @file    printUnits.java
 * @brief   Prints some unit information about the model
 * @author  Akira Funahashi (translated from libSBML C++ examples (using other Java examples provided by Nicolas Rodriguez))
 * @author  Sarah Keating
 * @author  Michael Hucka
 *
 * $Id$
 * $Source$
 *
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */


import static org.sbml.libsbml.libsbml.SBMLTypeCode_toString;
import static org.sbml.libsbml.libsbml.UnitKind_toString;

import org.sbml.libsbml.FormulaUnitsData;
import org.sbml.libsbml.Model;
import org.sbml.libsbml.ModelCreator;
import org.sbml.libsbml.ModelHistory;
import org.sbml.libsbml.SBMLWriter;
import org.sbml.libsbml.SBMLDocument;
import org.sbml.libsbml.SBMLReader;
 

public class printUnits
{
  public static void main (String[] args)
  {        
    if (args.length != 1)
    {
      println("\nUsage: java printUnits filename\n");
      System.exit(1);
    }

    String filename       = args[0];
    SBMLReader reader     = new SBMLReader();
    SBMLWriter writer     = new SBMLWriter();

    SBMLDocument d = reader.readSBML(filename);

    if (d.getNumErrors() > 0)
    {
      println("Encountered the following SBML errors:");
      d.printErrors();
      System.exit(1);
    }

    Model model = d.getModel();

    if (model == null)
    {
      println("No model present.");
      System.exit(1);
    }

    model.populateListFormulaUnitsData();
    println("Total number of formula units: " + 
            model.getNumFormulaUnitsData() + "\n");

    for (int n = 0; n < model.getNumFormulaUnitsData(); n++)
    {
      FormulaUnitsData fud = model.getFormulaUnitsData(n);
      long numUnits = fud.getUnitDefinition().getNumUnits();

      println("Formula units case #" + (n+1) + " --");
      println("  class of model entity: " + 
              SBMLTypeCode_toString(fud.getComponentTypecode()));

      println("  id of entity in model: " + fud.getUnitReferenceId());

      if (fud.getContainsUndeclaredUnits())
      {
        println(" undeclared parameters?: yes");
        println("  (can they be ignored?: " +
                 (fud.getCanIgnoreUndeclaredUnits() ? "yes)" : "no)"));
      }
      else
      {
        println(" undeclared parameters?: no");
      }

      if (numUnits > 0)
      {
        print("    units in definition: ");
        for (int p = 0; p < numUnits; p++)
        {
          int kind = fud.getUnitDefinition().getUnit(p).getKind();
          int exp = fud.getUnitDefinition().getUnit(p).getExponent();
          print(UnitKind_toString(kind) + " (exponent = " + exp + ")");
          if (p + 1 < numUnits)
          {
            print(", ");
          }
        }
      }
      print("\n\n");
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
    catch (UnsatisfiedLinkError e)
    {
      System.err.println("Error: could not link with the libSBML library."+
			 "  It is likely\nyour " + varname +
			 " environment variable does not include\nthe"+
			 " directory containing the libsbml library file.");
      System.exit(1);
    }
    catch (ClassNotFoundException e)
    {
      System.err.println("Error: unable to load the file libsbmlj.jar."+
			 "  It is likely\nyour " + varname + " environment"+
			 " variable or CLASSPATH variable\ndoes not include"+
			 " the directory containing the libsbmlj.jar file.");
      System.exit(1);
    }
    catch (SecurityException e)
    {
      System.err.println("Could not load the libSBML library files due to a"+
			 " security exception.");
    }
  }
}
