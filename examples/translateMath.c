/**
 * Filename    : translateMath.c
 * Description : Translates infix formulas into MathML and vice-versa
 * Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
 * Created     : 2003-08-01
 * Revision    : $Id$
 * Source      : $Source$
 *
 * Copyright 2003 California Institute of Technology and
 * Japan Science and Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 2.1 of the License, or
 * any later version.
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
 * incidental or consequential damages, including lost profits, arising
 * out of the use of this software and its documentation, even if the
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
 *     The Systems Biology Markup Language Development Group
 *     ERATO Kitano Symbiotic Systems Project
 *     Control and Dynamical Systems, MC 107-81
 *     California Institute of Technology
 *     Pasadena, CA, 91125, USA
 *
 *     http://www.cds.caltech.edu/erato
 *     mailto:sysbio-team@caltech.edu
 *
 * Contributor(s):
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "sbml/util.h"

#include "sbml/FormulaFormatter.h"
#include "sbml/FormulaParser.h"

#include "sbml/MathMLReader.h"
#include "sbml/MathMLWriter.h"

#include "sbml/StringBuffer.h"

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
  char *result;

  MathMLDocument_t *d    = MathMLDocument_create();
  ASTNode_t        *math = SBML_parseFormula(formula);


  MathMLDocument_setMath(d, math);
  result = writeMathMLToString(d);

  MathMLDocument_free(d);
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
  char             *result;
  MathMLDocument_t *d;
  StringBuffer_t   *sb;


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

  d      = readMathMLFromString(xml);
  result = SBML_formulaToString( MathMLDocument_getMath(d) );

  MathMLDocument_free(d);
  return result;
}
