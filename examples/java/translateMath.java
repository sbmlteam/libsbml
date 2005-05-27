/**
 * \file    translateMath.java
 * \brief   Translates infix formulas into MathML and vice-versa
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


import java.io.IOException;

import org.sbml.libsbml.ASTNode;
import org.sbml.libsbml.MathMLDocument;
import org.sbml.libsbml.ParseMessage;
import org.sbml.libsbml.SBMLDocument;
import org.sbml.libsbml.SBMLReader;
import org.sbml.libsbml.libsbml;


public class translateMath
{
  public static void main (String[] args)
  {
    if (args.length != 0)
    {
      println("  usage: java translateMath\n");
      return;
    }

    println("This program translates infix formulas into MathML and" +
            "vice-versa. Ctrl-D or Ctrl-Z quits and triggers"        +
            "translation.\n");
        

    long start, stop, size;
    long errors;

    char c;
    int  i = 0;

    String mathStr = new String();

    try
    {
      // Hit CTRL-Z on PC's to send EOF, CTRL-D on Unix
      while (i != -1)
      {
        // Read a character from keyboard
        i  = System.in.read();
        // 1 byte character is returned in int.
        // So cast to char
        c = (char) i;
        mathStr += c;
      }
    }
    catch (IOException ioe)
    {
      println( "IO error:" + ioe );
    }

    String result = (mathStr.charAt(0) == '<') ?
                    translateMathML(mathStr) : translateInfix(mathStr);
  }


  /**
   * Translates the given infix formula into MathML.
   *
   * @return the MathML as a string.
   */
  static String translateInfix (String formula)
  {
    MathMLDocument d = new MathMLDocument();

    d.setMath( libsbml.parseFormula(formula) );
    return libsbml.writeMathMLToString(d);
  }


  /**
   * Translates the given MathML into an infix formula.  The MathML must
   * contain no leading whitespace, but an XML header is optional.
   *
   * @return the infix formula as a string.
   */
  static String translateMathML (String xml)
  {
    /**
     * Prepend an XML header if not already present.
     */
    if (xml.charAt(0) == '<' && xml.charAt(1) != '?')
    {
      StringBuffer sb = new StringBuffer();
            
      sb.append("<?xml version='1.0' encoding='ascii'?>\n");
      sb.append(xml);

      xml = sb.toString();
    }

    MathMLDocument d = libsbml.readMathMLFromString(xml);
    return libsbml.formulaToString( d.getMath() );
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
