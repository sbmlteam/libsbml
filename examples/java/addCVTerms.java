/**
 * @file    addCVTerms.java
 * @brief   adds controlled vocabulary terms to a species in a model
 * @author  Akira Funahashi (translated from libSBML C++ examples (using other Java examples provided by Nicolas Rodriguez))
 * @author  Sarah Keating
 *
 * $Id$
 * $Source$
 *
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */


import org.sbml.libsbml.libsbmlConstants;
import org.sbml.libsbml.CVTerm;
import org.sbml.libsbml.SBMLWriter;
import org.sbml.libsbml.Species;
import org.sbml.libsbml.SBMLDocument;
import org.sbml.libsbml.SBMLReader;
 

public class addCVTerms
{
  public static void main (String[] args)
  {        
    if (args.length != 2)
    {
      println("\n  usage: java appendAnnotation <input-filename> <output-filename>");
      println("  Adds controlled vocabulary term to a species\n");
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
	  long n = d.getModel().getNumSpecies();

	  if (n <= 0)
	  { 
	    println("Model has no species.\n Cannot add CV terms\n");
	  } 
	  else
	  { 
	    Species s = d.getModel().getSpecies(0);

        CVTerm cv = new CVTerm();
        cv.setQualifierType(libsbmlConstants.BIOLOGICAL_QUALIFIER);
        cv.setBiologicalQualifierType(libsbmlConstants.BQB_IS_VERSION_OF);
        cv.addResource("http://www.geneontology.org/#GO:0005892");

        CVTerm cv2 = new CVTerm();
        cv2.setQualifierType(libsbmlConstants.BIOLOGICAL_QUALIFIER);
        cv2.setBiologicalQualifierType(libsbmlConstants.BQB_IS);
        cv2.addResource("http://www.geneontology.org/#GO:0005895");

        CVTerm cv1 = new CVTerm();
        cv1.setQualifierType(libsbmlConstants.BIOLOGICAL_QUALIFIER);
        cv1.setBiologicalQualifierType(libsbmlConstants.BQB_IS_VERSION_OF);
        cv1.addResource("http://www.ebi.ac.uk/interpro/#IPR002394");

        s.addCVTerm(cv);
        s.addCVTerm(cv2);
        s.addCVTerm(cv1);

        writer.writeSBML(d, args[1]);
	  }
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
