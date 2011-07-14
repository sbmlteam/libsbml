/**
 * @file    util.c
 * @brief   Supporting functions for example code
 * @author  Ben Bornstein
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

#include <sbml/common/extern.h>

BEGIN_C_DECLS


/**
 * @return the number of milliseconds elapsed since the Epoch.
 */
unsigned long long
getCurrentMillis (void)
{
  unsigned long long result = 0;


#if WIN32 && !defined(CYGWIN)

  result = (unsigned long long) GetTickCount();

#else

  struct timeval tv;

  if (gettimeofday(&tv, 0) == 0)
  {
    result = (unsigned long long)
    (
     (tv.tv_sec * (time_t) 1000) + (tv.tv_usec * .001)
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

END_C_DECLS

