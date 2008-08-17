/**
 * @file    validateSBML.cs
 * @brief   Validates one or more SBML files
 * @author  Akiya Jouraku (translated from libSBML C++ examples)
 * @author  Ben Bornstein
 * @author  Michael Hucka
 *
 * $Id: $
 * $HeadURL: $
 *
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */


namespace LibSBMLCSExample
{
  using System;
  using System.IO;
  using libsbml;

  public class validateSBML
  {
    private static int  NumErrors         = 0;
    private static bool enableUnitCCheck  = true;

    public static void Main (string[] args)
    {
      if (args.Length < 1)
      {
        string myname = Path.GetFileName(Environment.GetCommandLineArgs()[0]);
        Console.WriteLine("Usage: {0} [-u] inputFile1 [inputFile2 ...]", myname);
        Environment.Exit(1);
      }
      else if ( args.Length == 1 && args[0] == "-u" )
      {
        string myname = Path.GetFileName(Environment.GetCommandLineArgs()[0]);
        Console.WriteLine("Usage: {0} [-u] inputFile1 [inputFile2 ...]", myname);
        Environment.Exit(1);
      }

      if ( args[0] == "-u" )
      {
        enableUnitCCheck = false;
      }

      foreach ( string inputFile in args )
      {
        if (inputFile == "-u")
        {
          continue;
        }

        Console.WriteLine("---------------------------------------------------------------------------");
        validate(inputFile);
      }

      Console.WriteLine("---------------------------------------------------------------------------");
      Console.WriteLine("Validated {0} files, {1} valid files, {2} invalid files."
                         , args.Length
                         , args.Length - NumErrors
                         , NumErrors
                       );
      if ( ! enableUnitCCheck )
      {
        Console.WriteLine("(Unit consistency checks skipped)");
      }

      if (NumErrors > 0 )
      {
        Environment.Exit(1);
      }
    }

    public static void validate(string inputFile)
    {
      if ( ! File.Exists(inputFile) )
      {
        Console.WriteLine("[Error] {0} : No such file.", inputFile);
        ++NumErrors;
        return;
      }

      SBMLDocument sbmlDoc;
      long start, stop, rtime;

      start   = System.DateTime.UtcNow.ToFileTimeUtc();
      sbmlDoc = libsbml.readSBML(inputFile);
      stop    = System.DateTime.UtcNow.ToFileTimeUtc();
      rtime   = (stop - start)/1000;

      long errors = sbmlDoc.getNumErrors();
      FileInfo fi = new FileInfo(inputFile);

      Console.WriteLine("               filename: {0}", inputFile);
      Console.WriteLine("       file size (byte): {0}", fi.Length);
      Console.WriteLine("         read time (ms): {0}", rtime    );

      bool seriousErrors = false;
  
      if (errors > 0 )
      {
        long numErrors   = 0;
        long numWarnings = 0;

        for (int i = 0; i < errors; i++)
        {
          long severity = sbmlDoc.getError(i).getSeverity();
          if (severity == libsbml.LIBSBML_SEV_ERROR || severity == libsbml.LIBSBML_SEV_FATAL )
          {
            seriousErrors = true;
            ++numErrors;
          }
          else {
            ++numWarnings;
          }
        }

        if (numErrors > 0 )
        {
          Console.WriteLine("    validation error(s): {0}", numErrors);
        }

        if (numWarnings > 0 )
        {
          Console.WriteLine("  validation warning(s): {0}", numErrors);
        }

        Console.WriteLine();
        sbmlDoc.printErrors();
      }


      // If serious errors are encountered while reading an SBML document, it
      // does not make sense to go on and do full consistency checking because
      // the model may be nonsense in the first place.

      if (seriousErrors)  
      {
        Console.WriteLine("Further consistency checking and validation aborted.\n");
        ++NumErrors;
      }
      else
      {
        long failures;

        sbmlDoc.setConsistencyChecks(libsbml.LIBSBML_CAT_UNITS_CONSISTENCY, enableUnitCCheck);
        start    = System.DateTime.UtcNow.ToFileTimeUtc();
        failures = sbmlDoc.checkConsistency();
        stop     = System.DateTime.UtcNow.ToFileTimeUtc();
        rtime    = (stop - start)/1000;
  
        Console.WriteLine("      c-check time (ms): {0}", rtime    );

        if (failures > 0)
        {
          long numErrors   = 0;
          long numWarnings = 0;

          for (int i = 0; i < failures; i++)
          {
            long severity = sbmlDoc.getError(i).getSeverity();
            if (severity == libsbml.LIBSBML_SEV_ERROR || severity == libsbml.LIBSBML_SEV_FATAL )
            {
              ++numErrors;
            }
            else {
              ++numWarnings;
            }
          }

          if (numErrors > 0 )
          {
            Console.WriteLine("   consistency error(s): {0}", numErrors);
          }

          if (numWarnings > 0 )
          {
            Console.WriteLine(" consistency warning(s): {0}", numWarnings);
          }

          Console.WriteLine();
          sbmlDoc.printErrors();
        }
        else
        {
          Console.WriteLine("                 errors: 0");
        }
      }
    }
  
  }
}
