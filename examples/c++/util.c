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
    result = static_cast<unsigned long>
    (
      (tv.tv_sec * 1000) + (tv.tv_usec * .001)
    );
  }

#endif /* WIN32 && !CYGWIN */

  return result;
}


/**
 * @return the size (in bytes) of the given filename.
 */
unsigned long
getFileSize (const char* filename)
{
  struct stat   s;
  unsigned long result = 0;


  if (stat(filename, &s) == 0)
  {
    result = s.st_size;
  }

  return result;
}
