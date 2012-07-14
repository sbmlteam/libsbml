/**
 * @file    translateMath.cpp
 * @brief   Translates infix formulas into MathML and vice-versa
 * @author  Sarah Keating
 * @author  Ben Bornstein
 *
 *
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */



using System;
using System.Collections.Generic;
using System.Text;
using libsbmlcs;

public class TranslateMath
{

    public static int Main(string[] args)
    {
        string line;
        string trimmed;
        string result;
        string str;
        long len;
        StringBuilder sb = new StringBuilder(1024);


        Console.WriteLine("This program translates infix formulas into MathML and");
        Console.WriteLine("vice-versa.  Enter or return on an empty line triggers");
        Console.WriteLine("translation. Ctrl-C quits");

        while (true)
        {
            Console.WriteLine("Enter infix formula or MathML expression (Ctrl-C to quit):");
            Console.Write("> ");

            line = Console.ReadLine();
            while (line != null)
            {
                trimmed = line.Trim();
                len = trimmed.Length;
                if (len > 0)
                {
                    sb.AppendLine(trimmed);
                }
                else
                {
                    str = sb.ToString();
                    result = (str[0] == '<') ? translateMathML(str) : translateInfix(str);

                    Console.WriteLine("Result:\n\n" + result + "\n\n");
                    sb = new StringBuilder(1024);
                    break;
                }

                line = Console.ReadLine();
            }
        }
        return 0;
    }


    /**
     * Translates the given infix formula into MathML.
     *
     * @return the MathML as a string.  The caller owns the memory and is
     * responsible for freeing it.
     */
    public static string translateInfix(string formula)
    {
        ASTNode math = libsbml.parseFormula(formula);

        string result = libsbml.writeMathMLToString(math);

        return result;
    }


    /**
     * Translates the given MathML into an infix formula.  The MathML must
     * contain no leading whitespace, but an XML header is optional.
     *
     * @return the infix formula as a string.  The caller owns the memory and
     * is responsible for freeing it.
     */
    public static string translateMathML(string xml)
    {
        ASTNode math = libsbml.readMathMLFromString(xml);
        string result = libsbml.formulaToString(math);
        return result;
    }
}
