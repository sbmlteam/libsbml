/**
 * \file    evaluateMath.java
 * \brief   Evaluates and outputs infix expressions
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
 *     Rainer Machne
 *     Theoretical Biochemistry Group
 *     University of Vienna
 *
 *     http://www.tbi.univie.ac.at/~raim/
 *     mailto:raim@tbi.univie.ac.at
 *
 * Contributor(s):
 *   Nicolas Rodriguez - Translated from C++ examples to Java
 */


import java.io.IOException;

import org.sbml.libsbml.ASTNode;
import org.sbml.libsbml.libsbml;
import org.sbml.libsbml.libsbmlConstants;


/**
 * This program asks the user to enter an infix formula, translates it to
 * an Abstract Syntax tree using the function:
 *
 *   ASTNode libsbml.parseFormula(String)
 *
 * evaluates the formula and returns the result.  See comments for double
 * evalAST(ASTNode n) for further information.
 */
public class evaluateMath
{
  public static void main (String[] args)
  {
    println( "This program evaluates infix formulas.\n" );
    println( "Enter triggers evaluation.\n" );
    println( "\n" );


    long start, stop, size;
    long errors;
    int  level, version;

    int  i = 0;
    char c;

    String  mathStr = new String();
    ASTNode nodes;

    try
    {
      // Hit Enter to quit the loop
      while (i != 10)
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

    nodes = libsbml.parseFormula(mathStr);

    start = System.currentTimeMillis();
    double result = evalAST(nodes);
    stop  = System.currentTimeMillis();


    println("\n" + libsbml.formulaToString(nodes) + "\n= " + result + "\n");
    println("evaluation time: " + (stop - start) + " ms\n\n");
  }


  /**
   * The function evalAST(ASTNode) evaluates the formula of an Abstract
   * Syntax Tree by simple recursion and returns the result as a double
   * value.
   *
   * If variables (ASTNodeType_t AST_NAME) occur in the formula the user is
   * asked to provide a numerical value.  When evaluating ASTs within an
   * SBML document or simulating an SBML model this node type includes
   * parameters and variables of the model.  Parameters should be retrieved
   * from the SBML file, time and variables from current values of the
   * simulation.
   *
   * Not implemented:
   *
   *   - PIECEWISE, LAMBDA, and the SBML model specific functions DELAY and
   *     TIME and user-defined functions.
   *
   *   - Complex numbers and/or checking for domains of trigonometric and
   *     root functions.
   *
   *   - Checking for precision and rounding errors.
   *
   * The Nodetypes AST_TIME, AST_DELAY and AST_PIECEWISE default to 0.  The
   * SBML DELAY function and unknown functions (SBML user-defined
   * functions) use the value of the left child (first argument to
   * function) or 0 if the node has no children.
   */
  private static double evalAST (ASTNode n)
  {
    int    i;
    double result = 0;
    int    astNodeType;

    String msg;

    int       childnum = (int) n.getNumChildren();
    ASTNode[] child    = new ASTNode[childnum];


    for(i = 0; i < childnum; i++)
    {
      child[i] = n.getChild(i);
    }

    astNodeType = n.getType();

    switch (astNodeType)
    {
      case libsbmlConstants.AST_INTEGER: 
        result = (double) n.getInteger();
        break;

      case libsbmlConstants.AST_REAL:
        result = n.getReal();
        break;

      case libsbmlConstants.AST_REAL_E:
        result = n.getReal();
        break;

      case libsbmlConstants.AST_RATIONAL:
        result = n.getReal();
        break;


      case libsbmlConstants.AST_NAME:
        msg =
          "\n--------- MESSAGE FROM EVALUATION FUNCTION ----------\n\n" +
          "Please enter a number for the variable!\n" +
          "If you do not enter a valid number (empty or characters), the \n" +
          "evaluation will proceed with a current internal value and the \n" +
          "result will make no sense.\n" +
          n.getName() + " = ";

        println(msg);
        
        String l = new String();
        double var;

        try
        {
          int input = 0;
          while (input != 10)
          {
            // Read a character from keyboard
            input  = System.in.read();
            // 1 byte character is returned in int.
            // So cast to char
            l += (char) input;
          }

          var = Double.valueOf(l).doubleValue();
          println(n.getName() + " = " + var + "\n");
          result = var;
        }
        catch (IOException e)
        {
        }

        println("\n--------- END MESSAGE ----------\n\n");
        break;


      case libsbmlConstants.AST_FUNCTION_DELAY:
        msg =
          "\n--------- MESSAGE FROM EVALUATION FUNCTION ----------\n\n"      +
          "Delays can only be evaluated during a time series simulation.\n"  +
          "The value of the first child (ie. the first argument to the"      +
          "function)\nis used for this evaluation. If the function node has" +
          "no children the\nvalue defaults to 0.\n" +
          "\n--------- END MESSAGE ----------\n\n";

        println(msg);

        if (i > 0)
        {
          result = evalAST(child[0]);
        }
        else
        {
          result = 0.0;
        }
        break;

      case libsbmlConstants.AST_NAME_TIME:
        msg =
          "\n--------- MESSAGE FROM EVALUATION FUNCTION ----------\n\n"       +
          "The time can only be evaluated during a time series simulation.\n" +
          "The value of defaults to 0\n" +
          "\n--------- END MESSAGE ----------\n\n";
        println(msg);

        result = 0.0;
        break;


      case libsbmlConstants.AST_CONSTANT_E:
        /* exp(1) is used to adjust exponentiale to machine precision */
        result = Math.exp(1);
        break;

      case libsbmlConstants.AST_CONSTANT_FALSE:
        result = 0.0;
        break;

      case libsbmlConstants.AST_CONSTANT_PI:
        /* pi = 4 * atan 1  is used to adjust Pi to machine precision */
        result = 4 * Math.atan(1.);
        break;

      case libsbmlConstants.AST_CONSTANT_TRUE:
        result = 1.0;
        break;

      case libsbmlConstants.AST_PLUS:
        result = evalAST(child[0]) + evalAST(child[1]);
        break;


      case libsbmlConstants.AST_MINUS:
        if (childnum == 1)
        {
          result = - (evalAST(child[0]));
        }
        else
        {
          result = evalAST(child[0]) - evalAST(child[1]);
        }
        break;


      case libsbmlConstants.AST_TIMES:
        result = evalAST(child[0]) * evalAST(child[1]) ;
        break;

      case libsbmlConstants.AST_DIVIDE:
        result = evalAST(child[0]) / evalAST(child[1]);
        break;

      case libsbmlConstants.AST_POWER:
        result = Math.pow(evalAST(child[0]),evalAST(child[1]));
        break;

      case libsbmlConstants.AST_LAMBDA:
        msg =
          "\n--------- MESSAGE FROM EVALUATION FUNCTION ----------\n\n" +
          "This function is not implemented yet.\n"                     +
          "The value defaults to 0.\n"                                  +
          "\n--------- END MESSAGE ----------\n\n";
        println(msg);

        result = 0.0;
        break;

      case libsbmlConstants.AST_FUNCTION:
        msg =
          "\n--------- MESSAGE FROM EVALUATION FUNCTION ----------\n\n"        +
          "This function is not known.\n"                                      +
          "Within an SBML document new functions can be defined by the user\n" +
          "or application. The value of the first child (ie. the first\n"      +
          "argument to the function) is used for this evaluation. If the\n"    +
          "function node has no children the value defaults to 0.\n"           +
          "\n--------- END MESSAGE ----------\n\n";
        println(msg);

        if (childnum > 0)
        {
          result = evalAST(child[0]);
        }
        else
        {
          result = 0.0;
        }
        break;

        case libsbmlConstants.AST_FUNCTION_ABS:
          result = Math.abs(evalAST(child[0]));
          break;

        case libsbmlConstants.AST_FUNCTION_ARCCOS:
          result = Math.acos(evalAST(child[0]));
          break;

        case libsbmlConstants.AST_FUNCTION_ARCCOSH:
          result = Math.acos(evalAST(child[0])); // TODO : fix to have acosh
          break;

        case libsbmlConstants.AST_FUNCTION_ARCCOT:
          /* arccot x =  arctan (1 / x) */
          result = Math.atan(1. / evalAST(child[0]));
          break;

        case libsbmlConstants.AST_FUNCTION_ARCCOTH:
          /* arccoth x = 1/2 * ln((x+1)/(x-1)) */
          result = ((1. / 2.) *
                    Math.log((evalAST(child[0]) + 1.) /
                             (evalAST(child[0]) - 1.) ));
          break;

        case libsbmlConstants.AST_FUNCTION_ARCCSC:
          /* arccsc(x) = Arctan(1 / sqrt((x - 1)(x + 1))) */
          result = Math.atan(1. / Math.sqrt((evalAST(child[0]) - 1.) *
                                            (evalAST(child[0]) + 1.) ));
          break;

        case libsbmlConstants.AST_FUNCTION_ARCCSCH:
          /* arccsch(x) = ln((1 + sqrt(1 + x^2)) / x) */
          result = Math.log((1. +
                             Math.pow(1 + Math.pow(evalAST(child[0]), 2), 2))
                            / evalAST(child[0]));
          break;

      case libsbmlConstants.AST_FUNCTION_ARCSEC:
        /* arcsec(x) = arctan(sqrt((x - 1)(x + 1))) */
        result = Math.atan(Math.sqrt((evalAST(child[0]) - 1.) *
                                     (evalAST(child[0]) + 1.) ));
        break;

      case libsbmlConstants.AST_FUNCTION_ARCSECH:
        /* arcsech(x) = ln((1 + sqrt(1 - x^2)) / x) */
        result = Math.log((1. + Math.pow(1 -
                                         Math.pow(evalAST(child[0]), 2), 0.5))
                          / evalAST(child[0]));
        break;

      case libsbmlConstants.AST_FUNCTION_ARCSIN:
        result = Math.asin(evalAST(child[0]));
        break;

      case libsbmlConstants.AST_FUNCTION_ARCSINH:
        result = Math.asin(evalAST(child[0])); // TODO : fix to have asinh
        break;

      case libsbmlConstants.AST_FUNCTION_ARCTAN:
        result = Math.atan(evalAST(child[0]));
        break;

      case libsbmlConstants.AST_FUNCTION_ARCTANH:
        result = Math.atan(evalAST(child[0])); // TODO : fix to have atanh
        break;

      case libsbmlConstants.AST_FUNCTION_CEILING:
        result = Math.ceil(evalAST(child[0]));
        break;

      case libsbmlConstants.AST_FUNCTION_COS:
        result = Math.cos(evalAST(child[0]));
        break;

      case libsbmlConstants.AST_FUNCTION_COSH:
        result = Math.cosh(evalAST(child[0]));
        break;

      case libsbmlConstants.AST_FUNCTION_COT:
        /* cot x = 1 / tan x */
        result = 1. / Math.tan(evalAST(child[0]));
        break;

      case libsbmlConstants.AST_FUNCTION_COTH:
        /* coth x = cosh x / sinh x */
        result = Math.cosh(evalAST(child[0])) / Math.sinh(evalAST(child[0]));
        break;

      case libsbmlConstants.AST_FUNCTION_CSC:
        /* csc x = 1 / sin x */
        result = (1. / Math.sin(evalAST(child[0])));
        break;

      case libsbmlConstants.AST_FUNCTION_CSCH:
        /* csch x = 1 / cosh x  */
        result = (1. / Math.cosh(evalAST(child[0])));
        break;

      case libsbmlConstants.AST_FUNCTION_EXP:
        result = Math.exp(evalAST(child[0]));
        break;

      case libsbmlConstants.AST_FUNCTION_FACTORIAL:
        msg =
          "\n--------- MESSAGE FROM EVALUATION FUNCTION ----------\n\n"    +
          "The factorial is only implemented for integer values. If a\n"   +
          "floating point number is passed, the floor value is used for\n" +
          "calculation!\n" +
          "\n--------- END MESSAGE ----------\n\n";
        println(msg);

        double dbl = Math.floor(evalAST(child[0]));
        for(result = 1; dbl > 1; --dbl)
        {
          result *= i;
        }
        break;

      case libsbmlConstants.AST_FUNCTION_FLOOR:
        result = Math.floor(evalAST(child[0]));
        break;

      case libsbmlConstants.AST_FUNCTION_LN:
        result = Math.log(evalAST(child[0]));
        break;

      case libsbmlConstants.AST_FUNCTION_LOG:
        result = Math.log10(evalAST(child[0]));
        break;

      case libsbmlConstants.AST_FUNCTION_PIECEWISE:
        msg =
          "\n--------- MESSAGE FROM EVALUATION FUNCTION ----------\n\n" +
          "This function is not implemented yet.\n" +
          "The value defaults to 0.\n"              +
          "\n--------- END MESSAGE ----------\n\n";
        println(msg);

        result = 0.0;
        break;

      case libsbmlConstants.AST_FUNCTION_POWER:
        result = Math.pow(evalAST(child[0]), evalAST(child[1]));
        break;

      case libsbmlConstants.AST_FUNCTION_ROOT:
        result = Math.pow(evalAST(child[1]), 1. / evalAST(child[0]));
        break;

      case libsbmlConstants.AST_FUNCTION_SEC:
        /* sec x = 1 / cos x */
        result = 1. / Math.cos(evalAST(child[0]));
        break;

      case libsbmlConstants.AST_FUNCTION_SECH:
        /* sech x = 1 / sinh x */
        result = 1. / Math.sinh(evalAST(child[0]));
        break;

      case libsbmlConstants.AST_FUNCTION_SIN:
        result = Math.sin(evalAST(child[0]));
        break;

      case libsbmlConstants.AST_FUNCTION_SINH:
        result = Math.sinh(evalAST(child[0]));
        break;

      case libsbmlConstants.AST_FUNCTION_TAN:
        result = Math.tan(evalAST(child[0]));
        break;

      case libsbmlConstants.AST_FUNCTION_TANH:
        result = Math.tanh(evalAST(child[0]));
        break;
/*
      case libsbmlConstants.AST_LOGICAL_AND:
        result = (double) (evalAST(child[0]) & evalAST(child[1]));
        break;

      case libsbmlConstants.AST_LOGICAL_NOT:
        result = (double) (!(evalAST(child[0])));
        break;

      case libsbmlConstants.AST_LOGICAL_OR:
        result = (double) ((evalAST(child[0])) || (evalAST(child[1])));
        break;

      case libsbmlConstants.AST_LOGICAL_XOR:
        result = (double) ((!(evalAST(child[0])) && (evalAST(child[1])))
                           || ((evalAST(child[0])) &&  !(evalAST(child[1]))));
        break;

      case libsbmlConstants.AST_RELATIONAL_EQ:
        result = (double) ((evalAST(child[0])) == (evalAST(child[1])));
        break;

      case libsbmlConstants.AST_RELATIONAL_GEQ:
        result = (double) ((evalAST(child[0])) >= (evalAST(child[1])));
        break;

      case libsbmlConstants.AST_RELATIONAL_GT:
        result = (double) ((evalAST(child[0])) > (evalAST(child[1])));
        break;

      case libsbmlConstants.AST_RELATIONAL_LEQ:
        result = (double) ((evalAST(child[0])) <= (evalAST(child[1])));
        break;

      case libsbmlConstants.AST_RELATIONAL_LT:
        result = (double) ((evalAST(child[0])) < (evalAST(child[1])));
        break;
*/
      default:
        result = 0;
        break;
    }

    return result;
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
