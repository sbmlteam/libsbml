/**
 * Filename    : util.c
 * Description : Utility functions
 * Author(s)   : SBW Development Group <sysbio-team@caltech.edu>
 * Organization: Caltech ERATO Kitano Systems Biology Project
 * Created     : 2002-10-16
 * Revision    : $Id$
 * Source      : $Source$
 *
 * Copyright 2002 California Institute of Technology and
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


#include "sbml/common.h"
#include "sbml/List.h"
#include "sbml/util.h"


#include <ctype.h>


/**
 * Attempts to open filename for the given access mode and return a pointer
 * to it.  If the filename could not be opened, prints an error message and
 * exits.
 */
FILE *
safe_fopen (const char *filename, const char *mode)
{
  const char *format  = "%s: error: Could not open file '%s' for %s.\n";
  const char *modestr = strcmp(mode, "r") ? "writing" : "reading";
  FILE       *fp      = fopen(filename, mode);


  if (fp == (FILE *) NULL)
  {
    fprintf(stderr, format, PACKAGE, filename, modestr);
    exit(-1);
  }

  return fp;
}


/**
 * Returns a pointer to a new string which is the concatenation of the
 * strings str1 and str2.  Memory for the new string is obtained with
 * safe_malloc() and can be freed with safe_free().
 *
 * NOTE: This strcat behaves differently than standard library strcat().
 */
char *
safe_strcat (char *str1, char *str2)
{
  int  len1    = strlen(str1);
  int  len2    = strlen(str2);
  char *concat = (char *) safe_malloc( len1 + len2 + 1 );


  strncpy(concat, str1, len1 + 1);
  strncat(concat, str2, len2);

  return concat;
}


/**
 * @return a pointer to a new string which is a duplicate of the string s.
 * Memory for the string is obtained with safe_malloc() and can be freed
 * with safe_free().
 */
char *
safe_strdup (const char* s)
{
  size_t  size      = strlen(s) + 1;
  char   *duplicate = (char *) safe_malloc(size * sizeof(char));


  strncpy(duplicate, s, size);

  return duplicate;
}


/**
 * Compares two strings s1 and s2, ignoring the case of the characters.
 *
 * @return an integer less than, equal to, or greater than zero if s1 is
 * found, respectively, to be less than, to match, or be greater than s2.
 */
int
strcmp_insensitive (const char *s1, const char *s2)
{
  while ( (*s1 != '\0') && 
          (tolower( *(unsigned char *) s1) == tolower( *(unsigned char *) s2)) )
  {
    s1++;
    s2++;
  }

  return tolower( *(unsigned char *) s1) - tolower( *(unsigned char *) s2);
}
