dnl
dnl Filename    : expat.m4
dnl Description : Autoconf macro to check for existence of Expat library
dnl Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
dnl Organization: JST ERATO Kitano Symbiotic Systems Project
dnl Created     : 2003-11-05
dnl Revision    : $Id$
dnl Source      : $Source$
dnl
dnl Copyright 2003 California Institute of Technology and
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
dnl     Ben Bornstein
dnl     The Systems Biology Markup Language Development Group
dnl     ERATO Kitano Symbiotic Systems Project
dnl     Control and Dynamical Systems, MC 107-81
dnl     California Institute of Technology
dnl     Pasadena, CA, 91125, USA
dnl
dnl     http://www.cds.caltech.edu/erato
dnl     mailto:sysbio-team@caltech.edu
dnl
dnl Contributor(s):
dnl   Stephan Hoops
dnl   Mike Hucka <mhucka@caltech.edu> Enhancements to this file.

dnl
dnl Check --with-expat[=PREFIX] is specified and Expat 1.95.0 or greater
dnl is installed.
dnl

AC_DEFUN(CONFIG_LIB_EXPAT,
[
  AC_ARG_WITH([expat],
    AC_HELP_STRING([--with-expat=PREFIX],
                   [Use Expat XML Library [[default=no]]]),
    [with_expat=$withval],
    [with_expat=no])

  if test $with_expat != no; then

    AC_LANG_PUSH(C)

    EXPAT_CPPFLAGS=
    EXPAT_LDFLAGS=
    EXPAT_LIBS=

    if test $with_expat != yes; then
      EXPAT_CPPFLAGS="-I$with_expat/include -I\$(top_srcdir)/expat"
      EXPAT_LDFLAGS="-L$with_expat/lib"
    fi

    EXPAT_LIBS="-lexpat"

    dnl The following is grungy but I don't know how else to make 
    dnl AC_CHECK_LIB use particular library and include paths.

    tmp_CPPFLAGS=$CPPFLAGS
    tmp_LDFLAGS=$LDFLAGS
    tmp_LIBS=$LIBS
    CPPFLAGS="$CPPFLAGS $EXPAT_CPPFLAGS"
    LDFLAGS="$LDFLAGS $EXPAT_LDFLAGS"
    LIBS="$LIBS $EXPAT_LIBS"

    AC_CHECK_LIB(expat, XML_ParserCreate, [expat_found=yes], [expat_found=no])

    if test $expat_found = no; then
      AC_MSG_ERROR([Could not find the Expat XML library.])
    fi

    AC_CHECK_HEADERS(expat.h, [expat_found=yes], [expat_found=no])

    if test $expat_found = no; then
      AC_MSG_ERROR([Could not find the file expat.h.])
    fi

    CPPFLAGS=$tmp_CPPFLAGS
    LDFLAGS=$tmp_LDFLAGS
    LIBS=$tmp_LIBS

    AC_LANG_POP(C)

    AC_DEFINE([USE_EXPAT], 1, [Define to 1 to use the Expat XML library])
    AC_SUBST(USE_EXPAT, 1)

    AC_SUBST(EXPAT_CPPFLAGS)
    AC_SUBST(EXPAT_LDFLAGS)
    AC_SUBST(EXPAT_LIBS)

  fi

  if test $with_expat != no; then
    with_xerces=no
  fi

])
