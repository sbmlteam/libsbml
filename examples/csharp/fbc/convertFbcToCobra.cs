/**
 * @file    convertFbcToCobra.cs
 * @brief   Convert L3 with FBC to L2 with COBRA annotation
 * @author  Frank T. Bergmann
 * 
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */

namespace LibSBMLCSExample
{
  using System;
  using System.IO;
  using libsbmlcs;

  public class convertFbcToCobra
  {
    public static void Main (string[] args)
    {
      if (args.Length != 2)
      {
        string myname = Path.GetFileName(Environment.GetCommandLineArgs()[0]);
        Console.WriteLine("Usage: {0} input-filename output-filename", myname);
        Environment.Exit(1);
      }

      string inputFile      = args[0];
      string outputFile     = args[1];
	  

      if ( ! File.Exists(inputFile) )
      {
        Console.WriteLine("[Error] {0} : No such file.", inputFile);
        Environment.Exit(1);        
      }

	  var now = DateTime.Now.Ticks;
      
	  SBMLReader   reader  = new SBMLReader();
      SBMLWriter   writer  = new SBMLWriter();
      SBMLDocument sbmlDoc = reader.readSBML(inputFile);

      if ( sbmlDoc.getErrorLog().getNumFailsWithSeverity(libsbml.LIBSBML_SEV_ERROR) > 0)
      {
        sbmlDoc.printErrors(); 
        Console.WriteLine("[Error] Cannot read {0}", inputFile);
        Environment.Exit(1);        
      }

	  Console.WriteLine("Read {0} in {1}", inputFile, new TimeSpan(DateTime.Now.Ticks - now).TotalMilliseconds);
	  
	  /* create a new conversion properties structure */
      ConversionProperties props = new ConversionProperties();
	  
	  /* add an option that we want to convert a model  with
	     L3 FBC to L2 with COBRA annotation */
	  props.addOption("convert fbc to cobra", true, "Convert FBC model to Cobra model");
	  
	  now = DateTime.Now.Ticks;
	  
	  /* perform the conversion */
	  int result = sbmlDoc.convert(props);
	  if (result != libsbml.LIBSBML_OPERATION_SUCCESS)
	  {
	  	Console.WriteLine ("conversion failed ... ");
	  	Environment.Exit(3);   
	  }
	  
      writer.writeSBML(sbmlDoc, outputFile);

      Console.WriteLine("[OK] converted to FBC from {0} and wrote to {1}  (in {2} msec)", inputFile, outputFile, new TimeSpan(DateTime.Now.Ticks - now).TotalMilliseconds);
    }
  }

}
