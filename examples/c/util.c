/**
 * \file    util.c
 * \brief   Supporting functions for example code
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2002 California Institute of Technology and Japan Science and
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
