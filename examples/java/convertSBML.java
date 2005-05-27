/**
 * \file    convertSBML.java
 * \brief   Converts SBML L1 documents (any version) to L2v1
 * \author  Nicolas Rodriguez (translated from libSBML C++ examples)
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
 *     Ben Bornstein
 *
 *     The SBML Team
 *     Control and Dynamical Systems, MC 107-81
 *     California Institute of Technology
 *     Pasadena, CA, 91125, USA
 *
 *     http://sbml.org
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s):
 *   Nicolas Rodriguez - Translated from C++ examples to Java
 */


import org.sbml.libsbml.ParseMessage;
import org.sbml.libsbml.SBMLDocument;
import org.sbml.libsbml.SBMLReader;
import org.sbml.libsbml.libsbml;
 

public class convertSBML
{
  public static void main (String[] args)
  {
    if (args.length != 2)
    {
      println("  usage: convertSBML <input-filename> <output-filename>\n" +
              "  Converts an SBML L1 file to L2 or vice versa");
      return;
    }

    
    long start, stop, size;
    long errors;
        
    SBMLDocument d;
    SBMLReader   sr = new SBMLReader();
        
    String filename = args[0];
        
    start = System.currentTimeMillis();
    d     = sr.readSBML(filename);
    stop  = System.currentTimeMillis();
        
    errors = d.getNumWarnings() + d.getNumErrors() + d.getNumFatals();
        
    if (errors > 0)
    {
      libSBMLHelper.printErrors(d);
      println("Conversion skipped.  Correct the above and re-run.");
    }
    else
    {
      d.setLevel( d.getLevel() == 2 ? 1 : 2 );
            
      errors = d.getNumWarnings() + d.getNumErrors() + d.getNumFatals();
            
      if (errors > 0)
      {
        println("Conversion Error(s):");
                
        libSBMLHelper.printErrors(d);
                
        println("Conversion skipped.  Either libSBML does not (yet) have \n" +
                "ability to convert this model or (automatic) conversion \n" +
                "is not possible.");
      }
      else
      {
        libsbml.writeSBML(d, args[1]);
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
   * Loads the SWIG generated libsbml Java module when this class is
   * loaded.
   */
  static
  {
    System.loadLibrary("sbmlj");
  }
}
