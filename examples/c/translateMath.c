/**
 * @file    translateMath.c
 * @brief   Translates infix formulas into MathML and vice-versa
 * @author  Ben Bornstein
 * @author  Michael Hucka
 *
 * $Id$
 * $Source$
 *
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <sbml/math/FormulaFormatter.h>
#include <sbml/math/FormulaParser.h>
#include <sbml/math/MathML.h>

#include "util.h"


char *translateInfix  (const char *formula);
char *translateMathML (const char *xml);

int
main (int argc, char *argv[])
{
  char         *line;
  char         *result;
  char         *buffer  = calloc( 1, sizeof(char) );
  int           reading = 1;
  unsigned long len;


  printf( "\n" );
  printf( "This program translates infix formulas into MathML and\n" );
  printf( "vice-versa.  An 'enter' or a 'return' on an empty line\n" );
  printf( "triggers translation. Ctrl-C quits\n" );
  printf( "\n" );

  while (reading)
  {
    printf( "Enter an infix formula or MathML expression (Ctrl-C to quit):\n" );
    printf( "\n" );
    printf( "> " );

    do
    {
      line = trim_whitespace(get_line(stdin));
      len  = strlen(line);

      if (len > 0)
      {
        buffer = (char *) realloc( buffer, 1 + strlen(buffer) + len );

        strncat(buffer, line, len);
        strncat(buffer, "\n", 1);
      }
      else
      {
        result = (buffer[0] == '<') ?
          translateMathML(buffer) : translateInfix(buffer);

        printf("Result:\n\n%s\n\n\n", result);

        free(result);
        reading = 0;
      }
    }
    while (len > 0);
  }

  free(line);
  free(buffer);
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
  char      *result;
  ASTNode_t *math = SBML_parseFormula(formula);

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
  char           *result;
  ASTNode_t      *math;

  /**
   * Prepend an XML header if not already present.
   */
  if (xml[0] == '<' && xml[1] != '?')
  {
    char *header  = "<?xml version='1.0' encoding='UTF-8'?>\n";
    char *content = calloc( strlen(xml) + strlen(header) + 1, sizeof(char) );

    strncat(content, header, strlen(header));
    strncat(content, xml, strlen(xml));

    math = readMathMLFromString(content);
    free(content);
  }
  else
  {
    math = readMathMLFromString(xml);
  }

  result = SBML_formulaToString(math);

  ASTNode_free(math);
  return result;
}
