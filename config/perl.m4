dnl
dnl Filename    : perl.m4
dnl Description : Autoconf macro to check for existence of Perl
dnl Author(s)   : Mike Hucka
dnl Organization: SBML Team
dnl Created     : 2005-05-01
dnl Revision    : $Id$
dnl Source      : $Source$
dnl
dnl Copyright 2005 California Institute of Technology and
dnl Japan Science and Technology Corporation.
dnl
dnl This library is free software; you can redistribute it and/or modify it
dnl under the terms of the GNU Lesser General Public License as published
dnl by the Free Software Foundation; either version 2.1 of the License, or
dnl any later version.
dnl
dnl This library is distributed in the hope that it will be useful, but
dnl WITHOUT ANY WARRANTY, WITHOUT EVEN THE IMPLIED WARRANTY OF
dnl MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  The software and
dnl documentation provided hereunder is on an "as is" basis, and the
dnl California Institute of Technology and Japan Science and Technology
dnl Corporation have no obligations to provide maintenance, support,
dnl updates, enhancements or modifications.  In no event shall the
dnl California Institute of Technology or the Japan Science and Technology
dnl Corporation be liable to any party for direct, indirect, special,
dnl incidental or consequential damages, including lost profits, arising
dnl out of the use of this software and its documentation, even if the
dnl California Institute of Technology and/or Japan Science and Technology
dnl Corporation have been advised of the possibility of such damage.  See
dnl the GNU Lesser General Public License for more details.
dnl
dnl You should have received a copy of the GNU Lesser General Public License
dnl along with this library; if not, write to the Free Software Foundation,
dnl Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
dnl
dnl The original code contained here was initially developed by:
dnl
dnl     Michael Hucka
dnl     SBML Team
dnl     Control and Dynamical Systems, MC 107-81
dnl     California Institute of Technology
dnl     Pasadena, CA, 91125, USA
dnl
dnl     http://www.sbml.org
dnl     mailto:sbml-team@caltech.edu
dnl
dnl Contributor(s):
dnl

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
