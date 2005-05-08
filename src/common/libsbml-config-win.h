/**
 * \file    libsbml-config-win.h
 * \brief   Configuration variables for Windows (e.g. MSVC++) builds
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2004 California Institute of Technology and
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
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s):
 */


/* Define to 1 if you have the <check.h> header file. */
/* #define HAVE_CHECK_H 1 */

/* Define to 1 if you have the `check' library (-lcheck). */
/* #define HAVE_LIBCHECK 1 */


/* Define to 1 if you have the <expat.h> header file. */
/* #undef HAVE_EXPAT_H */

/* Define to 1 to use the Expat XML library */
/* #undef USE_EXPAT */


/* Define to 1 if you have the <errno.h> header file. */
#define HAVE_ERRNO_H 1

/* Define to 1 if you have the <ieeefp.h> header file. */
/* #define HAVE_IEEEFP_H 1 */

/* Define to 1 if you have the ANSI C header files. */
#define STDC_HEADERS 1

/* Define to 1 if you have the <math.h> header file. */
#define HAVE_MATH_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#define HAVE_SYS_TYPES_H 1

/* Define to 1 if you have the `m' library (-lm). */
/* #define HAVE_LIBM 1 */


/* Define to 1 to enable primitive memory tracing. */
/* #define TRACE_MEMORY 1 */

/* Define to 1 to build the SBML layout extension. */
/* #define USE_LAYOUT 1 */


/* Define to 1 if your processor stores words with the most significant byte
   first (like Motorola and SPARC, unlike Intel and VAX). */
/* #define WORDS_BIGENDIAN 1 */


/* Define to the full name of this package. */
#define PACKAGE_NAME "libSBML"

/* Define to the version of this package. */
#define PACKAGE_VERSION "2.3.0"
