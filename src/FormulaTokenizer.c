/**
 * Filename    : FormulaTokenizer.c
 * Description : Tokenizes an SBML formula string
 * Author(s)   : SBW Development Group <sysbio-team@caltech.edu>
 * Organization: Caltech ERATO Kitano Systems Biology Project
 * Created     : 2003-05-02
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
 *     The Systems Biology Workbench Development Group
 *     ERATO Kitano Systems Biology Project
 *     Control and Dynamical Systems, MC 107-81
 *     California Institute of Technology
 *     Pasadena, CA, 91125, USA
 *
 *     http://www.cds.caltech.edu/erato
 *     mailto:sysbio-team@caltech.edu
 *
 * Contributor(s):
 */


#include <ctype.h>
#include "sbml/FormulaTokenizer.h"


/**
 * Converts the given Token (which must be of type TT_NAME) to a TT_REAL
 * NaN or Inf as appropriate.
 */
void Token_convertNaNInf (Token_t *t);


/**
 * Creates a new FormulaTokenizer for the given formula string and returns
 * a pointer to it.
 */
LIBSBML_EXTERN
FormulaTokenizer_t *
FormulaTokenizer_create (const char *formula)
{
  FormulaTokenizer_t *ft;


  ft = (FormulaTokenizer_t *) safe_malloc( sizeof(FormulaTokenizer_t) );

  ft->formula = safe_strdup(formula);
  ft->pos     = 0;

  return ft;
}


/**
 * Frees the given FormulaTokenizer.
 */
LIBSBML_EXTERN
void
FormulaTokenizer_free (FormulaTokenizer_t *ft)
{
  if (ft == NULL) return;


  safe_free(ft->formula);
  safe_free(ft);
}


/**
 * Reads a TT_NAME from the FormulaTokenizer into the given Token.  This is
 * a supporting function for FormulaTokenizer_nextToken().
 *
 * A TT_NAME is an SName:
 *
 *   letter ::=  a .. z, A .. Z
 *   digit  ::=  0 .. 9
 *   SName  ::= ( letter |  _  ) ( letter | digit |  _  )*
 *
 * This function assumes the character ft->formula[ft->pos] is either a
 * letter or an underscore.  FormulaTokenizer_nextToken() ensures this
 * precondition before calling this function.
 */
void
FormulaTokenizer_getName (FormulaTokenizer_t *ft, Token_t *t)
{
  char c;
  int  start, stop, len;


  t->type = TT_NAME;

  start = ft->pos;
  c     = ft->formula[ ++ft->pos ];

  while (isalpha(c) || isdigit(c) || c == '_')
  {
    c = ft->formula[ ++ft->pos ];
  }

  stop = ft->pos;
  len  = stop - start;

  t->value.name      = (char *) safe_malloc(len + 1);
  t->value.name[len] = '\0';
  strncpy(t->value.name, ft->formula + start, len);
}


/**
 * Reads either a TT_INTEGER or a TT_REAL from the FormulaTokenizer into
 * the given Token.  This is a supporting function for
 * FormulaTokenizer_nextToken().
 *
 * This function assumes the character ft->formula[ft->pos] is either a
 * period or a digit.  FormulaTokenizer_nextToken() ensures this
 * precondition before calling this function.
 *
 * Sign characters preceding the number are not recognized, but sign
 * characters in the exponent are recognized.
 *
 * A subtle error case:
 *
 * If the string starting with ft->formula[ft->pos] is some combination of
 * a '.', 'e|E' or '+|-' with no intervening digits, the token will be
 * marked as TT_UNKNOWN and t->value.ch set to ft->formula[ft->pos]
 * (a '.').
 */
void
FormulaTokenizer_getNumber (FormulaTokenizer_t *ft, Token_t *t)
{
  char c;
  char endchar;
  char *endptr;

  unsigned int start, stop, len;

  unsigned int exppos = 0;
  unsigned int endpos = 0;

  int seendot = 0;
  int seenexp = 0;
  int seensgn = 0;


  start = ft->pos;
  c     = ft->formula[ start ];

  /**
   * ([0-9]+\.?[0-9]*|\.[0-9]+)([eE][-+]?[0-9]+)?
   */
  while (1)
  {
    if (c == '.' && seendot == 0)
    {
      seendot = 1;
    }
    else if ((c == 'e' || c == 'E') && seenexp == 0)
    {
      seenexp = 1;
      exppos  = ft->pos;
    }
    else if ((c == '+' || c == '-') && seenexp == 1 && seensgn == 0)
    {
      seensgn = 1;
    }
    else if (c < '0' || c > '9')
    {
      endchar = c;
      endpos  = ft->pos;
      break;
    }

    c = ft->formula[ ++ft->pos ];
  }

  /**
   * Temporarily terminate ft->formula will a NULL at the character just
   * beyond the end of the number.  This prevents strtod() and strtol()
   * (below) from reading past the end of the number.
   *
   * This prevents at least one obscure bug where something like '3e 4' is
   * understood as one token 3e4 instead of two: 3e0 and 4.
   */
  ft->formula[ endpos ] = '\0';

  stop = ft->pos;
  len  = stop - start;

  /**
   * If the token is composed only of some combination of '.', 'e|E' or
   * '+|-' mark it as TT_UNKNOWN.  Otherwise, strtod() or strtol() should
   * be able to convert it, as all the syntax checking was performed above.
   */
  if (len == (seendot + seenexp + seensgn))
  {
    t->type     = TT_UNKNOWN;
    t->value.ch = ft->formula[start];
  }
  else if (seendot || seenexp)
  {
    /**
     * Temporarily "hide" the exponent part, so strtod below will convert
     * only the mantissa part.
     */
    if (seenexp)
    {
      c                     = ft->formula[ exppos ];
      ft->formula[ exppos ] = '\0';
    }

    t->type       = TT_REAL;
    t->value.real = strtod(ft->formula + start, &endptr);

    /**
     * Convert the exponent part and "unhide" it.
     */
    if (seenexp)
    {
      t->type     = TT_REAL_E;
      t->exponent = strtol(ft->formula + exppos + 1, &endptr, 10);

      ft->formula[ exppos ] = c;
    }
  }
  else
  {
    t->type          = TT_INTEGER;
    t->value.integer = strtol(ft->formula + start, &endptr, 10);
  }

  /**
   * Restore the character overwritten above.
   */
  ft->formula[ endpos ] = endchar;
}


/**
 * @return the next token in the formula string.  If no more tokens are
 * available, the token type will be TT_END.
 */
LIBSBML_EXTERN
Token_t *
FormulaTokenizer_nextToken (FormulaTokenizer_t *ft)
{
  char     c = ft->formula[ ft->pos ];
  Token_t *t = Token_create();


  /**
   * Skip whitespace
   */
  while (isspace(c))
  {
    c = ft->formula[ ++ft->pos ];
  }


  if (c == '\0')
  {
    t->type     = TT_END;
    t->value.ch = c;
  }
  else if (c == '+' || c == '-' || c == '*' || c == '/' ||
           c == '^' || c == '(' || c == ')' || c == ',' )
  {
    t->type     = (TokenType_t) c;
    t->value.ch = c;
    ft->pos++;
  }
  else if (isalpha(c) || c == '_')
  {
    FormulaTokenizer_getName(ft, t);
  }
  else if (c == '.' || isdigit(c))
  {
    FormulaTokenizer_getNumber(ft, t);
  }
  else
  {
    t->type     = TT_UNKNOWN;
    t->value.ch = c;
    ft->pos++;
  }

  if (t->type == TT_NAME)
  {
    Token_convertNaNInf(t);
  }

  return t;
}


/**
 * Creates a new Token and returns a point to it.
 */
LIBSBML_EXTERN
Token_t *
Token_create (void)
{
  Token_t *t = (Token_t *) safe_calloc(1, sizeof(Token_t));


  t->type = TT_UNKNOWN;

  return t;
}


/**
 * Converts the given Token (which must be of type TT_NAME) to a TT_REAL
 * NaN or Inf as appropriate.
 */
void
Token_convertNaNInf (Token_t *t)
{
  if ( !strcmp_insensitive(t->value.name, "NaN") )
  {
    safe_free(t->value.name);
    t->type       = TT_REAL;
    t->value.real = util_NaN();
  }
  else if ( !strcmp_insensitive(t->value.name, "Inf") )
  {
    safe_free(t->value.name);
    t->type       = TT_REAL;
    t->value.real = util_PosInf();
  }
}


/**
 * Frees the given Token
 */
LIBSBML_EXTERN
void
Token_free (Token_t *t)
{
  if (t == NULL) return;


  if (t->type == TT_NAME)
  {
    safe_free(t->value.name);
  }

  safe_free(t);
}
