/**
 * @file    validateSBML.java
 * @brief   Validates an SBML file against the appropriate schema
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

import org.sbml.libsbml.SBMLDocument;
import org.sbml.libsbml.SBMLReader;
import org.sbml.libsbml.Model;


public class validateSBML
{
  public static void main (String[] args)
  {
    if (args.length != 1)
    {
      System.out.println("Usage: java validateSBML <filename>");
      System.exit(1);
    }

    String filename       = args[0];
    SBMLReader reader     = new SBMLReader();
    SBMLDocument document;
    long start, stop;

    start    = System.currentTimeMillis();
    document = reader.readSBML(filename);
    stop     = System.currentTimeMillis();

    if (document.getNumErrors() > 0)
    {
      print("Encountered the following errors while reading the SBML file:\n");
      document.printErrors();
      print("\nFurther consistency checking and validation aborted.\n");
      System.exit(1);
    }
    else
    {
      long errors = document.checkConsistency();
      long size   = new File(filename).length();

      println("            filename: " + filename);
      println("           file size: " + size);
      println("      read time (ms): " + (stop - start));
      println(" validation error(s): " + errors);

      if (errors > 0)
      {
	document.printErrors();
	System.exit(1);
      }
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
