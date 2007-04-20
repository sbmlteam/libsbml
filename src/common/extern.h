/**
 * @file    extern.h
 * @brief   Defines common C externs: e.g. LIBSBML_EXTERN
 * @author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2003 California Institute of Technology and Japan Science and
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


#ifndef LIBSBML_EXTERN_H
#define LIBSBML_EXTERN_H


#if (WIN32 && ! defined CYGWIN && ! defined LIBSBML_STATIC )

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

/**
 * Disable MSVC++ warning C4800: 'const int' : forcing value to bool 'true'
 * or 'false' (performance warning).
 */
#pragma warning(disable: 4800)

/**
 * Disable MSVC++ warning C4291: no matching operator delete found.
 */
#pragma warning(disable: 4291)

/**
 * Disable MSVC++ warning C4251: class 'type' needs to have dll-interface
 * to be used by clients of class 'type2'.
 *
 * For an explanation of why this is safe, see:
 *   - http://www.unknownroad.com/rtfm/VisualStudio/warningC4251.html
 */
#pragma warning(disable: 4251)

#else

#define LIBSBML_EXTERN

#endif  /* WIN32 */


#if __cplusplus
#  define BEGIN_C_DECLS extern "C" {
#  define END_C_DECLS   }
#else
#  define BEGIN_C_DECLS
#  define END_C_DECLS
#endif


#endif  /** LIBSBML_EXTERN_H **/
