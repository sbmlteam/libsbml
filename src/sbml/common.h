/**
 * Filename    : common.h
 * Description : Common functions and macros for C and C++
 * Author(s)   : SBW Development Group <sysbio-team@caltech.edu>
 * Organization: Caltech ERATO Kitano Systems Biology Project
 * Created     : 2003-02-10
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


#ifndef SBML_COMMON_H
#define SBML_COMMON_H 1


#if WIN32
#  define HAVE_CONFIG_H 1
#endif

#if HAVE_CONFIG_H
#  include "config.h"
#endif


#if WIN32

#if !defined(CYGWIN)

#undef  HAVE_LIBCHECK
#define snprintf _snprintf

/**
 * Avoids silly MSVC++ warning C4800: 'const int' : forcing value to bool
 * 'true' or 'false' (performance warning)
 */
#pragma warning(disable: 4800)

#endif /* !CYGWIN */

/**
 * The following ifdef block is the standard way of creating macros which
 * make exporting from a DLL simpler. All files within this DLL are
 * compiled with the LIBSBML_EXPORTS symbol defined on the command line.
 * This symbol should not be defined on any project that uses this
 * DLL. This way any other project whose source files include this file see
 * LIBSBML_EXTERN functions as being imported from a DLL, wheras this DLL
 * sees symbols defined with this macro as being exported.
 *
 * (From Andrew Finney's sbwdefs.h, with "SBW" replaced by "LIBSBML" :)
 */
#if LIBSBML_EXPORTS
#  define LIBSBML_EXTERN __declspec(dllexport)
#else
#  define LIBSBML_EXTERN __declspec(dllimport)
#endif

#else

#define LIBSBML_EXTERN

#endif  /* WIN32 */



#include <stdio.h>
#include <sys/types.h>

#if STDC_HEADERS
#  include <errno.h>
#  include <stdlib.h>
#  include <string.h>
#elif HAVE_STRINGS_H
#  include <strings.h>
#endif

#ifndef errno
  extern int errno;
#endif

#if HAVE_LIBCHECK
#  include <check.h>
#endif


#include "memory.h"
#include "util.h"


#if __cplusplus
#  define BEGIN_C_DECLS extern "C" {
#  define END_C_DECLS   }
#else
#  define BEGIN_C_DECLS
#  define END_C_DECLS
#endif


#endif  /** SBML_COMMON_H **/
