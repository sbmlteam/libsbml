dnl
dnl Filename    : Makefile.am
dnl Description : Process this file with autoconf (2.57) to produce a
dnl Description : configure script.
dnl Author(s)   : SBML Development Group <sbml-team@caltech.edu>
dnl Organization: JST ERATO Kitano Symbiotic Systems Project
dnl Created     : 2003-02-13
dnl Revision    : $Id$
dnl Source      : $Source$
dnl
dnl Copyright 2002 California Institute of Technology and
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
dnl     mailto:sbml-team@caltech.edu
dnl
dnl Contributor(s):
dnl


dnl ---------------------------------------------------------------------------
dnl Autoconf and Automake initialization.
dnl
dnl Bug reports are sent to sbml-team@cds.caltech.edu
dnl ---------------------------------------------------------------------------

AC_INIT([SBML Library], [2.0.5], [sbml-team@caltech.edu], [libsbml])
AC_CONFIG_AUX_DIR(config)
AM_INIT_AUTOMAKE

AC_REVISION($Revision$)

AC_PREREQ(2.57)

AC_CONFIG_SRCDIR(src/util.c)

AC_CONFIG_HEADERS(src/sbml/config.h)


dnl ---------------------------------------------------------------------------
dnl Command-line options
dnl ---------------------------------------------------------------------------

AC_ARG_ENABLE([memory-tracing],
  AC_HELP_STRING([--enable-memory-tracing],
                 [Enable memory tracing (default: disabled)]),
  [if test "$enableval" = yes; then
    AC_DEFINE([TRACE_MEMORY], 1,
              [Define to 1 to enable primitive memory tracing.])
   fi
  ])

AC_ARG_WITH([check],
  AC_HELP_STRING([--with-check=PREFIX],
                 [Build the test suite using libcheck (default=no)]),
  [with_libcheck=$withval],
  [with_libcheck=no])

AC_ARG_WITH([xerces],
  AC_HELP_STRING([--with-xerces=PREFIX],
                 [Use Xerces XML Library [[default=yes]]]),
  [with_xerces=$withval],
  [with_xerces=yes])

AC_ARG_WITH([expat],
  AC_HELP_STRING([--with-expat=PREFIX],
                 [Use Expat XML Library [[default=no]]]),
  [with_expat=$withval],
  [with_expat=no])

if test $with_expat != no; then
  with_xerces=no
  AC_DEFINE([USE_EXPAT], 1, [Define to 1 to use the Expat XML library])
fi

dnl ---------------------------------------------------------------------------
dnl Checks for programs.
dnl ---------------------------------------------------------------------------

AC_PROG_CXX
AC_PROG_CC
AC_PROG_CPP

AC_LIBTOOL_WIN32_DLL
AC_PROG_LIBTOOL


dnl ---------------------------------------------------------------------------
dnl Check host operating system.
dnl ---------------------------------------------------------------------------

dnl AC_CANONICAL_HOST

TestDataDirectory="\$(srcdir)"

case "$host_os" in
  *linux*)
    AM_CPPFLAGS="-DLINUX"
    ;;

  *solaris*)
    AM_CPPFLAGS="-DSOLARIS"
    ;;

  *cygwin*)
    AM_CPPFLAGS="-DWIN32 -DCYGWIN -DLIBSBML_EXPORTS"
    ;;

  *darwin*)
    AM_CPPFLAGS="-DMACOSX"
    TestDataDirectory="."
    ;;
esac

AC_SUBST(AM_CPPFLAGS)
AC_SUBST(TestDataDirectory)


dnl ---------------------------------------------------------------------------
dnl Checks for libraries.
dnl
dnl Check for either the Xerces or Expat XML libraries.
dnl The default is Xerces.
dnl ---------------------------------------------------------------------------

AC_LANG(C)

if test $libcheck_enabled != no; then
  AC_CHECK_LIB(check, srunner_create)
fi

AC_CHECK_LIB(m, isnan)

AC_LIB_XERCES
AC_LIB_EXPAT

if test $with_xerces != no; then
  xml_library="Xerces-C++"
else
  xml_library="Expat"
fi

AC_LIB_LIBCHECK

dnl ---------------------------------------------------------------------------
dnl Checks for header files.
dnl ---------------------------------------------------------------------------

AC_HEADER_STDC
AC_CHECK_HEADERS(errno.h)
AC_CHECK_HEADERS(math.h)
AC_CHECK_HEADERS(ieeefp.h)


dnl ---------------------------------------------------------------------------
dnl Checks for typedefs, structures, and compiler characteristics.
dnl ---------------------------------------------------------------------------

AC_C_BIGENDIAN
AC_C_CONST
AC_TYPE_SIZE_T


dnl ---------------------------------------------------------------------------
dnl Checks for library functions.
dnl ---------------------------------------------------------------------------

dnl AC_CHECK_FUNCS(...)


dnl ---------------------------------------------------------------------------
dnl Output
dnl ---------------------------------------------------------------------------

AM_CONDITIONAL(LIBCHECK_ENABLED, test "$libcheck_enabled" != "no")
AM_CONDITIONAL(USE_EXPAT, test "$with_expat" != "no")

AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([expat/Makefile])
AC_CONFIG_FILES([src/Makefile])

AC_OUTPUT


dnl ---------------------------------------------------------------------------
dnl Inform users of settings
dnl ---------------------------------------------------------------------------

if test -f "VERSION.txt"; then
    LIBSBML_VERSION=[`cat VERSION.txt`]
fi

echo ""
echo "Build Settings for libsbml v${LIBSBML_VERSION}:"
echo "----------------------------------"
echo "  host os     = $host_os"
echo "  install dir = $prefix"
echo "  test suite  = $libcheck_enabled"
echo "  XML library = $xml_library"
echo "  CC          = $CC"
echo "  CXX         = $CXX"
echo "  CFLAGS      = $CFLAGS"
echo "  CXXFLAGS    = $CXXFLAGS"
echo "  CPPFLAGS    = $CPPFLAGS $AM_CPPFLAGS"
echo "  LDFLAGS     = $LDFLAGS"
echo "  LIBS        = $LIBS"
echo ""
