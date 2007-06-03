/**
 * @file    printSBML.java
 * @brief   Prints some information about the top-level model
 * @author  Nicolas Rodriguez (translated from libSBML C++ examples)
 * @author  Ben Bornstein
 * @author  Michael Hucka
 *
 * $Id$
 * $Source$
 *
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */


import java.io.File;

import org.sbml.libsbml.Model;
import org.sbml.libsbml.SBMLDocument;
import org.sbml.libsbml.SBMLReader;


public class printSBML
{
  public static void main (String[] args)
  {
    if (args.length != 1)
    {
      println("Usage: java printSBML filename");
      System.exit(1);
    }


    String filename       = args[0];
    SBMLReader reader     = new SBMLReader();
    SBMLDocument document;
    Model model;
    int level, version;

    document     = reader.readSBML(filename);

    if (document.getNumErrors() > 0)
    {
      document.printErrors();
      println("Printing skipped.  Please correct the above problems first.");
      System.exit(1);
    }

    model = document.getModel();

    if (model == null)
    {
      println("There does not appear to be a model in this file");
      System.exit(1);
    }

    level   = (int) document.getLevel();
    version = (int) document.getVersion();

    println("File: " + filename +
            " (Level " + level + ", version " + version + ")\n");

    if (level == 1)
    {
      println("model name: " + model.getName());
    }
    else
    {
      println("  model id: " + (model.isSetId() ? model.getId() : "(empty)"));
    }

    println("functionDefinitions: " +   model.getNumFunctionDefinitions());
    println("    unitDefinitions: " +   model.getNumUnitDefinitions()    );
    println("       compartments: " +   model.getNumCompartments()       );
    println("            species: " +   model.getNumSpecies()            );
    println("         parameters: " +   model.getNumParameters()         );
    println("          reactions: " +   model.getNumReactions()          );
    println("              rules: " +   model.getNumRules()              );
    println("             events: " +   model.getNumEvents()             );
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
