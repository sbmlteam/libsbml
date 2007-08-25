dnl
dnl Filename    : perl.m4
dnl Description : Autoconf macro to check for existence of Perl
dnl Author(s)   : Mike Hucka
dnl Organization: SBML Team
dnl Created     : 2005-05-01
dnl Revision    : $Id$
dnl Source      : $Source$
dnl
dnl <!-------------------------------------------------------------------------
dnl This file is part of libSBML.  Please visit http://sbml.org for more
dnl information about SBML, and the latest version of libSBML.
dnl
dnl Copyright 2005-2007 California Institute of Technology.
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
dnl Check --with-perl[=PREFIX]
dnl

AC_DEFUN([CONFIG_PROG_PERL],
[
  AC_ARG_VAR([PERL])

  AC_ARG_WITH(perl,
              AC_HELP_STRING([--with-perl=PREFIX],
                             [Generate Perl interface library [[default=no]]]),
	      [with_perl=$withval],
	      [with_perl=no])

  if test $with_perl != no; then

    dnl Find a perl executable.

    if test $with_perl != yes; then
      AC_PATH_PROG([PERL], [perl], [$with_perl/bin/perl],
                     [no-perl-found], [$with_perl/bin])
    else
      AC_PATH_PROG([PERL], [perl])
    fi

    AC_DEFINE([USE_PERL], 1, [Define to 1 to use Perl])
    AC_SUBST(USE_PERL, 1)

    AC_SUBST(PERL_CPPFLAGS)
    AC_SUBST(PERL_LDFLAGS)
    AC_SUBST(PERL_LIBS)

  fi

  dnl We record the USE_XXX flag, for later testing in Makefiles.

  LIBSBML_OPTIONS="$LIBSBML_OPTIONS USE_PERL"

])
