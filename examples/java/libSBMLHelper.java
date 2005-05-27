/**
 * \file    libSBMLHelper.java
 * \brief   Provides some helper functions
 * \author  Nicolas Rodriguez
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
 *     Nicolas Rodriguez
 */


import org.sbml.libsbml.ParseMessage;
import org.sbml.libsbml.SBMLDocument;
import org.sbml.libsbml.SBMLReader;


public class libSBMLHelper
{
  /**
   * Prints any warnings, errors or fatals if present.
   *
   * @param document
   * @return The number of errors (errors + fatals) of the document
   */
  public static long printErrors (SBMLDocument document)
  {
    long errors   = document.getNumErrors();
    long fatals   = document.getNumFatals();
    long warnings = document.getNumWarnings();

    String message = new String();

    if (errors > 0 || fatals > 0 || warnings > 0)
    {
      message += "Warnings, Errors, Fatals = " +
                 warnings + ", " + errors + ", " + fatals + "\n";

      ParseMessage msg;

      if (warnings > 0)
      {
        message += "\nWARNING:" + "\n";

        for (int i = 0; i < warnings; i++)
        {
          msg      = document.getWarning(i);
          message += msg.getMessage() + "\n";
        }
      }

      if (errors > 0)
      {
        message += "\nERROR:" + "\n";

        for (int i = 0; i < errors; i++)
        {
          msg      = document.getError(i);
          message += msg.getMessage() + "\n";
        }
      }

      if (fatals > 0)
      {
        message += "\nFATAL:" + "\n";

        for (int i = 0; i < fatals; i++)
        {
          msg      = document.getFatal(i);
          message += msg.getMessage() + "\n";
        }
      }

      System.out.println(message);
    }

    return fatals + errors;
  }


  /**
   * Returns true if we can load with success the library 'sbmlj' 
   * and the class org.sbml.libsbml.libsbml.
   * <p> Means that the CLASSPATH and LD_LIBRARY_PATH (PATH or DYLIB_..)
   * seems to be set up properly.
   * 
   * @return True if we can load with success the library 'sbmlj',
   * false otherwise
   */
  public static boolean loadLibSBML ()
  {
    boolean isAvailable = false;

    try
    {
      // check that the library (.so/.dll/.dylib) is present
      System.loadLibrary("sbmlj");

      // check that the jar file is in the classpath
      Class.forName("org.sbml.libsbml.libsbml");

      // check that everything is consistent and that we can instanciate
      // libSBMl objects
      SBMLReader reader = new SBMLReader();

      isAvailable = true;
    }
    catch (SecurityException e)
    {
      System.err.println("Could not load libsbml library.");
    }
    catch (UnsatisfiedLinkError e)
    {
      System.err.println("Could not load libsbml library.");
    }
    catch (ClassNotFoundException e)
    {
      System.err.println("Could not load libsbml class file.");
    }

    return isAvailable;
  }
}
