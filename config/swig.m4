dnl
dnl Filename    : swig.m4
dnl Description : Autoconf macro to check for SWIG
dnl Author(s)   : Michael Hucka <mhucka@caltech.edu>
dnl Created     : 2004-06-18
dnl Revision    : $Id$
dnl $HeadURL$
dnl
dnl <!-------------------------------------------------------------------------
dnl This file is part of libSBML.  Please visit http://sbml.org for more
dnl information about SBML, and the latest version of libSBML.
dnl
dnl Copyright 2005-2010 California Institute of Technology.
dnl Copyright 2002-2005 California Institute of Technology and
dnl                     Japan Science and Technology Corporation.
dnl 
dnl This library is free software; you can redistribute it and/or modify it
dnl under the terms of the GNU Lesser General Public License as published by
dnl the Free Software Foundation.  A copy of the license agreement is provided
dnl in the file named "LICENSE.txt" included with this software distribution
dnl and also available online as http://sbml.org/software/libsbml/license.html
dnl --------------------------------------------------------------------- -->*/

dnl
dnl Supports --with-swig[=PREFIX]
dnl

AC_DEFUN([CONFIG_PROG_SWIG],
[
  AC_SUBST(SWIG_CONFIG_OPT)

  AC_ARG_WITH([swig],
    AC_HELP_STRING([--with-swig=PREFIX],
	           [Regenerate SWIG wrappers [[default=if_needed]]]),
    [with_swig=$withval],
    [with_swig=no])

  if test "$with_swig" != "no"; then

    if test $with_swig != yes; then
      dnl Users seems to have supplied a prefix directory path.  See if we can
      dnl find swig somewhere in the given tree.

      dnl 1st remove trailing slashes because it can confuse tests below.

      with_swig=`echo $with_swig | sed -e 's,\(.*\)/$,\1,g'`

      AC_PATH_PROG([SWIG], [swig], [no], [$with_swig/bin])
      AC_SUBST(SWIG_CONFIG_OPT,[=$with_swig])
    else
      dnl No prefix directory path supplied for --with-swig.  Use defaults.

      AC_PATH_PROG([SWIG], [swig])
    fi

    dnl Set up replacement variables, including some that we don't currently
    dnl use but may in the future.

    AC_DEFINE([USE_SWIG], 1, [Define to 1 to use SWIG])
    AC_SUBST(USE_SWIG, 1)

    AC_SUBST(SWIGLIB)
    AC_SUBST(SWIGFLAGS)

    AC_SUBST(SWIG_CPPFLAGS)
    AC_SUBST(SWIG_LDFLAGS)
    AC_SUBST(SWIG_LIBS)

  else
    dnl --with-swig not given.  Use the simplest default.
    SWIG=swig

  fi

  dnl Record the version of SWIG we need, for later testing. 
  dnl Note that this is always set, regardless of whether --with-swig is given.

  SWIGNEEDVERSION=[$1]

  dnl Do substitutions we always do.

  AC_SUBST(SWIG)
  AC_SUBST(SWIGNEEDVERSION)    

  dnl We record the USE_XXX flag, for later testing in Makefiles.

  LIBSBML_OPTIONS="$LIBSBML_OPTIONS USE_SWIG"

])
