/**
 * @file    translateMath.cpp
 * @brief   Translates infix formulas into MathML and vice-versa
 * @author  Sarah Keating
 * @author  Ben Bornstein
 *
 * $Id$
 * $Source$
 *
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */


#include <iostream>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <sbml/util/util.h>
#include <sbml/util/StringBuffer.h>

#include <sbml/math/FormulaFormatter.h>
#include <sbml/math/FormulaParser.h>
#include <sbml/math/MathML.h>


#define BUFFER_SIZE 1024


using namespace std;


char *translateInfix  (const char *formula);
char *translateMathML (const char *xml);


int
main (int argc, char *argv[])
{
  char            line[BUFFER_SIZE];
  char*           trimmed;
  char*           result;
  char*           str;
  unsigned int    len;
  StringBuffer_t* sb = StringBuffer_create(1024);


  cout << endl 
       << "This program translates infix formulas into MathML and" << endl
       << "vice-versa.  Enter or return on an empty line triggers" << endl
       << "translation. Ctrl-C quits" << endl
       << endl;

  while (1)
  {
    cout << "Enter infix formula or MathML expression (Ctrl-C to quit):"
         << endl << endl;
    cout << "> " ;

    cin.getline(line, BUFFER_SIZE, '\n');

    while (line != 0)
    {
      trimmed = util_trim(line);
      len     = strlen(trimmed);

      if (len > 0)
      {
        StringBuffer_append    (sb, trimmed);
        StringBuffer_appendChar(sb, '\n');
      }
      else
      {
        str    = StringBuffer_getBuffer(sb);
        result = (str[0] == '<') ? translateMathML(str) : translateInfix(str);

        cout << "Result:" << endl << endl << result << endl << endl << endl;

        StringBuffer_reset(sb);
        break;
      }

      cin.getline(line, BUFFER_SIZE, '\n');
    }
  }

  StringBuffer_free(sb);
  return 0;
}


/**
 * Translates the given infix formula into MathML.
 *
 * @return the MathML as a string.  The caller owns the memory and is
 * responsible for freeing it.
 */
char *
translateInfix (const char *formula)
{
  char*    result;
  ASTNode* math = SBML_parseFormula(formula);

  result = writeMathMLToString(math);
  ASTNode_free(math);

  return result;
}


/**
 * Translates the given MathML into an infix formula.  The MathML must
 * contain no leading whitespace, but an XML header is optional.
 *
 * @return the infix formula as a string.  The caller owns the memory and
 * is responsible for freeing it.
 */
char *
translateMathML (const char *xml)
{
  char*           result;
  ASTNode_t*      math;

  math   = readMathMLFromString(xml);
  result = SBML_formulaToString(math);

  ASTNode_free(math);
  return result;
}
