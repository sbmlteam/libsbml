/**
 * @file    util.c
 * @brief   Supporting functions for example code
 * @author  Ben Bornstein
 *
 * $Id$
 * $Source$
 *
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */

#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <sys/stat.h>

#if WIN32 && !defined(CYGWIN)
#  include <windows.h>
#else
#  include <sys/time.h>
#endif /* WIN32 && !CYGWIN */



/**
 * @return the number of milliseconds elapsed since the Epoch.
 */
unsigned long
getCurrentMillis (void)
{
  unsigned long result = 0;


#if WIN32 && !defined(CYGWIN)

  result = (unsigned long) GetTickCount();

#else

  struct timeval tv;

  if (gettimeofday(&tv, NULL) == 0)
  {
    result = (unsigned long) (tv.tv_sec * 1000) + (tv.tv_usec * .001);
  }

#endif /* WIN32 && !CYGWIN */

  return result;
}


/**
 * @return the size (in bytes) of the given filename.
 */
unsigned long
getFileSize (const char *filename)
{
  struct stat   s;
  unsigned long result = 0;


  if (stat(filename, &s) == 0)
  {
    result = s.st_size;
  }

  return result;
}


/**
 * Removes whitespace from both ends of the given string.  The string
 * is modified in-place.
 *
 * This was originally in libSBML's util/util.c, but moved here to
 * make this set of example programs more self-contained.
 */
void
trim_whitespace (char *s)
{
  char *end;
  int   len;

  if (s == NULL) return;

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
}


#define INPUT_LINE_LENGTH 1024

/**
 * The function get_line reads a line from a file (in this case "stdin" and
 * returns it as a string.  It is taken from the utilities library of the
 * VIENNA RNA PACKAGE ( http://www.tbi.univie.ac.at/~ivo/RNA/ )
 */
char*
get_line ( FILE *fp )
{
  /* reads lines of arbitrary length from fp */
  char s[INPUT_LINE_LENGTH], *line, *cp;
  
  line = NULL;
  do
  {
    if ( fgets( s, 512, fp ) == NULL ) break;
    cp = strchr( s, '\n' );
    if ( cp != NULL ) *cp = '\0';
    if ( line == NULL )
      line = calloc( 1+strlen(s), sizeof(char) );
    else
      line = (char *)realloc( line, 1+strlen( s )+strlen( line ) );
    strcat( line, s );
  } while ( cp == NULL );

  return line;
}
