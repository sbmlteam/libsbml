dnl
dnl Filename    : libcheck.m4
dnl Description : Autoconf macro to check for existence of libcheck library
dnl Author(s)   : SBML Team <sbml-team@caltech.edu>
dnl Organization: California Institute of Technology
dnl
dnl $Id$
dnl $Source$
dnl
dnl Copyright 2004 California Institute of Technology and
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
dnl Check --with-check[=PREFIX] is specified and libcheck is installed.
dnl

AC_DEFUN([CONFIG_LIB_LIBCHECK],
[
  AC_ARG_WITH([check],
    AC_HELP_STRING([--with-check=PREFIX],
                   [Build the test suite using libcheck (default=no)]),
    [with_libcheck=$withval],
    [with_libcheck=no])

  libcheck_found=no

  if test $with_libcheck != no; then

    AC_LANG_PUSH(C)

    if test $with_libcheck != yes; then
      libcheck_root="$with_libcheck"
      CONFIG_ADD_LDPATH($libcheck_root/lib)

      LIBCHECK_CPPFLAGS="-I$libcheck_root/include"
      LIBCHECK_LDFLAGS="-L$libcheck_root/lib"
      LIBCHECK_LIBS="-lcheck"
    else
      dnl On the Macs, if the user has installed libcheck via Fink and they
      dnl used the default Fink install path of /sw, the following should
      dnl catch it.  We do this so that Mac users are more likely to find
      dnl success even if they only type --with-check.

      dnl This is a case statement in case we need to do something similar
      dnl for other host types in the future.

      case $host in
      *darwin*) 
        libcheck_root="/sw"
        CONFIG_ADD_LDPATH($libcheck_root/lib)

        LIBCHECK_CPPFLAGS="-I$libcheck_root/include"
        LIBCHECK_LDFLAGS="-L$libcheck_root/lib"
        LIBCHECK_LIBS="-lcheck"
	;;
      esac    

      dnl Note that CONFIG_ADD_LDPATH is deliberately not called in cases
      dnl other than the two above.
    fi

    dnl The following is grungy but I don't know how else to make 
    dnl AC_CHECK_LIB use particular library and include paths without
    dnl permanently resetting CPPFLAGS etc.

    tmp_CPPFLAGS=$CPPFLAGS
    tmp_LDFLAGS=$LDFLAGS
    CPPFLAGS="$LIBCHECK_CPPFLAGS $CPPFLAGS"
    LDFLAGS="$LIBCHECK_LDFLAGS $LDFLAGS"

    AC_CHECK_HEADERS([check.h], [libcheck_found=yes], [libcheck_found=no])

    if test $libcheck_found = no; then
      AC_CHECK_LIB([check], [srunner_create],
        [libcheck_found=yes],
        [libcheck_found=no])
    fi

    if test $libcheck_found = no; then
      AC_MSG_ERROR([Could not find the libcheck library.])
    fi

    CPPFLAGS=$tmp_CPPFLAGS
    LDFLAGS=$tmp_LDFLAGS
    LIBS=$tmp_LIBS

    AC_LANG_POP(C)

    AC_DEFINE([USE_LIBCHECK], 1, [Define to 1 to use the check library])
    AC_SUBST(USE_LIBCHECK, 1)

    AC_SUBST(LIBCHECK_CPPFLAGS)
    AC_SUBST(LIBCHECK_LDFLAGS)
    AC_SUBST(LIBCHECK_LIBS)

  fi

  dnl We record the USE_XXX flag, for later testing in Makefiles.

  LIBSBML_OPTIONS="$LIBSBML_OPTIONS USE_LIBCHECK"

])
