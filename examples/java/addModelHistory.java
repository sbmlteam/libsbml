/**
 * @file    addModelHistory.java
 * @brief   adds Model History to a model
 * @author  Akira Funahashi (translated from libSBML C++ examples (using other Java examples provided by Nicolas Rodriguez))
 * @author  Sarah Keating
 *
 * $Id$
 * $Source$
 *
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */


import org.sbml.libsbml.Date;
import org.sbml.libsbml.ModelCreator;
import org.sbml.libsbml.ModelHistory;
import org.sbml.libsbml.SBMLWriter;
import org.sbml.libsbml.SBMLDocument;
import org.sbml.libsbml.SBMLReader;
 

public class addModelHistory
{
  public static void main (String[] args)
  {        
    if (args.length != 2)
    {
      println("\n  usage: java addModelHistory <input-filename> <output-filename>\n");
      System.exit(2);
    }

    SBMLDocument d;
    SBMLReader reader     = new SBMLReader();
    SBMLWriter writer     = new SBMLWriter();

    d = reader.readSBML(args[0]);

    long errors = d.getNumErrors();

    if (errors > 0)
    {
      println("Read Error(s):");
      d.printErrors();
      println("Correct the above and re-run.");
    }
	else
	{
      ModelHistory h = new ModelHistory();

      ModelCreator c = new ModelCreator();
      c.setFamilyName("Keating");
      c.setGivenName("Sarah");
      c.setEmail("sbml-team@caltech.edu");
      c.setOrganisation("University of Hertfordshire");

      h.addCreator(c);

      Date date = new Date("1999-11-13T06:54:32");
      Date date2 = new Date("2007-11-31T06:54:00-02:00");

      h.setCreatedDate(date);
      h.setModifiedDate(date2);

      d.getModel().setModelHistory(h);

	  writer.writeSBML(d, args[1]);
	}

    System.exit((int)errors);
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
