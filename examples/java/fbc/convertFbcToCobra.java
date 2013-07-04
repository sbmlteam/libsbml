/**
 * @file    convertFbcToCobra.java
 * @brief   Convert L3 with FBC to L2 with COBRA annotation
 * @author  Frank T. Bergmann
 * 
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */

import org.sbml.libsbml.*;

public class convertFbcToCobra
{
  public static void main (String[] args)
  {
    if (args.length != 3)
    {
      println("Usage: java convertFbcToCobra input-filename output-filename");
      System.exit(2);
    }	

	SBMLReader reader     = new SBMLReader();
	SBMLWriter writer     = new SBMLWriter();

	SBMLDocument doc = reader.readSBML(args[0]);
	if (doc.getErrorLog().getNumFailsWithSeverity(libsbml.LIBSBML_SEV_ERROR) > 0)
	{
		doc.printErrors();
	}
	else
	{
	  /* create a new conversion properties structure */
      ConversionProperties props = new ConversionProperties();
	  	  
	  /* add an option that we want to convert a model  with
	     L3 FBC to L2 with COBRA annotation */
	  props.addOption("convert fbc to cobra", true, "Convert FBC model to Cobra model");
	  
	  
	  /* perform the conversion */
	  if (doc.convert(props) != libsbml.LIBSBML_OPERATION_SUCCESS)
	  {
	  	println ("conversion failed ... ");
	  	System.exit(3); 
	  }
	  writer.writeSBML(doc, args[1]);
	}
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
    String shlibname;

    if (System.getProperty("os.name").startsWith("Mac OS"))
    {
      varname = "DYLD_LIBRARY_PATH";    // We're on a Mac.
      shlibname = "'libsbmlj.jnilib'";
    }
    else
    {
      varname = "LD_LIBRARY_PATH";      // We're not on a Mac.
      shlibname = "'libsbmlj.so' and/or 'libsbml.so'";
    }

    try
    {
      System.loadLibrary("sbmlj");
      // For extra safety, check that the jar file is in the classpath.
      Class.forName("org.sbml.libsbml.libsbml");
    }
    catch (UnsatisfiedLinkError e)
    {
      System.err.println("Error encountered while attempting to load libSBML:");
      System.err.println("Please check the value of your " + varname +
                         " environment variable and/or" +
                         " your 'java.library.path' system property" +
                         " (depending on which one you are using) to" +
                         " make sure it list the directories needed to" +
                         " find the " + shlibname + " library file and the" +
                         " libraries it depends upon (e.g., the XML parser).");
      System.exit(1);
    }
    catch (ClassNotFoundException e)
    {
      System.err.println("Error: unable to load the file 'libsbmlj.jar'." +
                         " It is likely that your -classpath command line " +
                         " setting or your CLASSPATH environment variable " +
                         " do not include the file 'libsbmlj.jar'.");
	  e.printStackTrace();
      				 
      System.exit(1);
    }
    catch (SecurityException e)
    {
      System.err.println("Error encountered while attempting to load libSBML:");
      e.printStackTrace();
      System.err.println("Could not load the libSBML library files due to a"+
                         " security exception.\n");
      System.exit(1);
    }
  }
}
