/**
 * @file    util.c
 * @brief   Utility functions. 
 * @author  Ben Bornstein
 *
 * $Id$
 * $Source$
 *
 *<!---------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright 2005-2007 California Institute of Technology.
 * Copyright 2002-2005 California Institute of Technology and
 *                     Japan Science and Technology Corporation.
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution and
 * also available online as http://sbml.org/software/libsbml/license.html
 *------------------------------------------------------------------------- -->
 *
 * Ths file implements a small number of utility functions that may be
 * useful inside and outside of libSBML.
 */

#include <ctype.h>
#include <locale.h>
#include <sys/stat.h>
#include <sys/types.h>

#include <sbml/common/common.h>

#include <sbml/util/List.h>
#include <sbml/util/util.h>


/** @cond doxygen-libsbml-internal */


/**
 * Identical to snprintf except printing always occurs according to the
 * "C" locale.  This function does not affect the locale of the calling
 * program.
 */
int
c_locale_snprintf (char *str, size_t size, const char *format, ...)
{
  int result;
  va_list ap;

  va_start(ap, format);
  result = c_locale_vsnprintf(str, size, format, ap);
  va_end(ap);

  return result;
}


/**
 * Identical to vsnprintf except printing always occurs according to the
 * "C" locale.  This function does not affect the locale of the calling
 * program.
 */
int
c_locale_vsnprintf (char *str, size_t size, const char *format, va_list ap)
{
#ifdef _MSC_VER
#  define vsnprintf _vsnprintf
#endif

  int result;
  char *locale;


  locale = safe_strdup(setlocale(LC_ALL, NULL));
  setlocale(LC_ALL, "C");

  result = vsnprintf(str, size, format, ap);

  setlocale(LC_ALL, locale);
  free(locale);
  
  return result;
}


/**
 * Identical to strtod except conversion always occurs according to the
 * "C" locale.  This function does not affect the locale of the calling
 * program.
 */
double
c_locale_strtod (const char *nptr, char **endptr)
{
  double result;
  char *locale;


  locale = safe_strdup(setlocale(LC_ALL, NULL));
  setlocale(LC_ALL, "C");

  result = strtod(nptr, endptr);

  setlocale(LC_ALL, locale);
  free(locale);

  return result;
}


/**
 * Attempts to open filename for the given access mode and return a pointer
 * to it.  If the filename could not be opened, prints an error message and
 * exits.
 */
LIBSBML_EXTERN
FILE *
safe_fopen (const char *filename, const char *mode)
{
  const char *format  = "%s: error: Could not open file '%s' for %s.\n";
  const char *modestr = strcmp(mode, "r") ? "writing" : "reading";
  FILE       *fp      = fopen(filename, mode);


  if (fp == (FILE *) NULL)
  {
    fprintf(stderr, format, PACKAGE_NAME, filename, modestr);
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
LIBSBML_EXTERN
char *
safe_strcat (const char *str1, const char *str2)
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
LIBSBML_EXTERN
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
LIBSBML_EXTERN
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


/**
 * Easier-to-read and NULL-friendly string comparison.
 */
LIBSBML_EXTERN
unsigned int
streq (const char *s, const char *t)
{
  if (s == NULL)
    return t == NULL;
  else if (t == NULL)
    return 0;
  else
    return !strcmp(s, t);
}


/**
 * Peforms a binary search on the string table strings to find string s.
 *
 * All strings from strings[lo] to strings[hi] are searched.  The string
 * comparison function used is strcmp_insensitive().  Since the search is
 * binary, the strings table must be sorted, irrespecitve of case.
 *
 * @return the index of s in strings, if s was found, or stop + 1
 * otherwise.
 */
LIBSBML_EXTERN
int
util_bsearchStringsI (const char **strings, const char *s, int lo, int hi)
{
  int cond;
  int mid;
  int result = hi + 1;


  if (s == NULL) return result;

  while (lo <= hi)
  {
    mid  = (lo + hi) / 2;
    cond = strcmp_insensitive(s, strings[mid]);
      
    if (cond < 0)
    {
      hi = mid - 1;
    }
    else if (cond > 0)
    {
      lo = mid + 1;
    }
    else
    {
      result = mid;
      break;
    }
  }

  return result;
}


/**
 * @returns true (non-zero) if filename exists, false (zero) otherwise.
 */
LIBSBML_EXTERN
int
util_file_exists (const char *filename)
{
#ifdef _MSC_VER
#  define stat _stat
#endif

  struct stat buf;
  return stat(filename, &buf) == 0;
}


/**
 * Removes leading and trailing whitespace from the string s.
 *
 * @return a pointer to a new string which is a duplicate of the string s,
 * with leading and trailing whitespace removed or NULL is s is NULL.
 *
 * Whitespace is determined by isspace().
 */
LIBSBML_EXTERN
char *
util_trim (const char *s)
{
  const char *start = s;
  const char *end;

  char *trimmed = NULL;
  int  len;


  if (s == NULL) return NULL;

  len = strlen(s);
  end = start + len - 1;

  /**
   * Skip leading whitespace.
   *
   * When this loop terminates, start will point the first non-whitespace
   * character of the string or NULL.
   */
  while ( len > 0 && isspace(*start) )
  {
    start++;
    len--;
  }

  /**
   * Skip trailing whitespace.
   *
   * When this loop terminates, end will point the last non-whitespace
   * character of the string.
   */
  while ( len > 0 && isspace(*end) )
  {
    end--;
    len--;
  }

  /**
   * If len is zero, the string is either empty or pure whitespace.  Set
   * trimmed to an empty string.
   */
  if (len == 0)
  {
    trimmed    = (char *) safe_malloc(1);
    trimmed[0] = '\0';
  }

  /**
   * Otherwise...
   */
  else
  {
    trimmed = (char *) safe_malloc(len + 1);

    strncpy(trimmed, start, len);
    trimmed[len] = '\0';
  }

  return trimmed;
}


/**
 * Removes leading and trailing whitespace from the string s.
 *
 * @return a pointer to the first non-whitespace character of the string s
 * (which may be the terminating NULL), or NULL if s is NULL.  The first
 * trailing whitespace character will be replaced with NULL.
 *
 * Whitespace is determined by isspace().
 */
LIBSBML_EXTERN
char *
util_trim_in_place (char *s)
{
  char *end;
  int   len;


  if (s == NULL) return NULL;

  len = strlen(s);
  end = s + len - 1;

  /**
   * Skip leading whitespace.
   *
   * When this loop terminates, s will point the first non-whitespace
   * character of the string or NULL.
   */
  while ( len > 0 && isspace(*s) )
  {
    s++;
    len--;
  }

  /**
   * Skip trailing whitespace.
   *
   * When this loop terminates, end will point the last non-whitespace
   * character of the string.
   */
  while ( len > 0 && isspace(*end) )
  {
    end--;
    len--;
  }

  s[len] = '\0';

  return s;
}


/** @endcond doxygen-libsbml-internal */


/**
 * Returns a representation of @c NaN.
 * 
 * @return a (quiet) NaN.
 */
LIBSBML_EXTERN
double
util_NaN (void)
{
  double z = 0.0;

  // MSVC++ will produce a compile error if 0.0 is used instead of z.
  return 0.0 / z;
}


/**
 * Returns a representation of the IEEE-754 "Negative Infinity" value.
 * 
 * @return IEEE-754 Negative Infinity.
 */
LIBSBML_EXTERN
double
util_NegInf (void)
{
  double z = 0.0;

  // MSVC++ will produce a compile error if 0.0 is used instead of z.
  return -1.0 / z;
}


/**
 * Returns a representation of the IEEE-754 "Positive Infinity" value.
 * 
 * @return IEEE-754 Positive Infinity
 */
LIBSBML_EXTERN
double
util_PosInf (void)
{
  double z = 0.0;

  // MSVC++ will produce a compile error if 0.0 is used instead of z.
  return 1.0 / z;
}


/**
 * Returns a representation of the IEEE-754 "Negative Zero" value.
 * 
 * @return IEEE-754 Negative Zero.
 */
LIBSBML_EXTERN
double
util_NegZero (void)
{
  return -1.0 / util_PosInf();
}


/**
 * Function for testing whether a given value represents negative infinity.
 *
 * @param d the floating-point value to test
 * 
 * @return @c -1 (for false) if @p d represents negative infinity, @c 1 if
 * @p d represents positive infinity, and @c 0 otherwise.
 */
LIBSBML_EXTERN
int
util_isInf (double d)
{
#ifdef _MSC_VER
#  define finite(d) _finite(d)
#  define isnan(d)  _isnan(d)
#endif

  if ( !(finite(d) || isnan(d)) )
  {
    return (d < 0) ? -1 : 1;
  }

  return 0;
}


/**
 * Function for testing whether a given value represents negative zero.
 *
 * @param d the floating-point value to test
 * 
 * @return nonzero (for true) if @p d is an IEEE-754 negative zero, zero
 * (for false) otherwise.
 */
LIBSBML_EXTERN
int
util_isNegZero (double d)
{
  unsigned char *b = (unsigned char *) &d;


#if WORDS_BIGENDIAN
  return b[0] == 0x80;
#else
  return b[7] == 0x80;
#endif
}
