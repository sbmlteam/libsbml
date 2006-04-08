/**
 * \file    translateMath.c
 * \brief   Translates infix formulas into MathML and vice-versa
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2003 California Institute of Technology and Japan Science and
 * Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is
 * provided in the file named "LICENSE.txt" included with this software
 * distribution.  It is also available online at
 * http://sbml.org/software/libsbml/license.html
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <sbml/util/util.h>
#include <sbml/util/StringBuffer.h>

#include <sbml/math/FormulaFormatter.h>
#include <sbml/math/FormulaParser.h>
#include <sbml/math/MathML.h>


#define BUFFER_SIZE 1024


char *translateInfix  (const char *formula);
char *translateMathML (const char *xml);


int
main (int argc, char *argv[])
{
  char           line[BUFFER_SIZE];
  char           *trimmed;
  char           *result;
  char           *str;
  unsigned int   len;
  StringBuffer_t *sb = StringBuffer_create(1024);


  printf( "\n" );
  printf( "This program translates infix formulas into MathML and\n" );
  printf( "vice-versa.  Enter or return on an empty line triggers\n" );
  printf( "translation. Ctrl-C quits\n" );
  printf( "\n" );

  while (1)
  {
    printf( "Enter infix formula or MathML expression (Ctrl-C to quit):\n" );
    printf( "\n" );
    printf( "> " );

    fgets(line, BUFFER_SIZE, stdin);

    while (line != NULL)
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

        printf("Result:\n\n%s\n\n\n", result);

        free(result);
        StringBuffer_reset(sb);
        break;
      }

      free(trimmed);
      fgets(line, BUFFER_SIZE, stdin);
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
  StringBuffer_t *sb;


  /**
   * Prepend an XML header if not already present.
   */
  if (xml[0] == '<' && xml[1] != '?')
  {
    sb = StringBuffer_create(1024);

    StringBuffer_append(sb, "<?xml version='1.0' encoding='ascii'?>\n");
    StringBuffer_append(sb, xml);

    xml = StringBuffer_getBuffer(sb);

    free(sb);
  }

  math   = readMathMLFromString(xml);
  result = SBML_formulaToString(math);

  ASTNode_free(math);
  return result;
}
