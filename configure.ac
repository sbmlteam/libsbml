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
dnl Autoconf initialization.
dnl
dnl Bug reports are sent to sbml-team@cds.caltech.edu
dnl ---------------------------------------------------------------------------

if test -f "VERSION.txt"; then
    m4_define(LIBSBML_VERSION, m4_esyscmd(cat VERSION.txt | tr -d '\n\r'))
fi

AC_INIT([libSBML], LIBSBML_VERSION, [sbml-team@caltech.edu], [libsbml])
AC_CONFIG_AUX_DIR(config)

AC_REVISION($Revision$)

AC_PREREQ(2.57)

AC_CONFIG_SRCDIR(src/util.c)

AC_CONFIG_HEADERS(src/sbml/config.h)


dnl ---------------------------------------------------------------------------
dnl Command-line options
dnl ---------------------------------------------------------------------------

dnl Other cmd-line options are defined in the individual .m4 files in config.

AC_ARG_ENABLE([memory-tracing],
  AC_HELP_STRING([--enable-memory-tracing],
                 [Enable memory tracing (default: disabled)]),
    [enable_tracing=$withval],
    [enable_tracing=no])

if test "$enable_tracing" != no; then
  AC_DEFINE([TRACE_MEMORY], 1,
            [Define to 1 to enable primitive memory tracing.])
fi

dnl ---------------------------------------------------------------------------
dnl Check host operating system.
dnl ---------------------------------------------------------------------------

AC_CANONICAL_HOST

TestDataDirectory="\$(srcdir)"
SHAREDLIBEXT="so"

case $host_os in
  *linux*)
    CPPFLAGS="-DLINUX $CPPFLAGS"
    HOST_TYPE="linux"
    ;;

  *solaris*)
    CPPFLAGS="-DSOLARIS $CPPFLAGS"
    HOST_TYPE="solaris"
    ;;

  *cygwin*)
    CPPFLAGS="-DWIN32 -DCYGWIN -DLIBSBML_EXPORTS $CPPFLAGS"
    HOST_TYPE="cygwin"
    ;;

  *darwin*)
    CPPFLAGS="-DMACOSX $CPPFLAGS"
    HOST_TYPE="darwin"
    SHAREDLIBEXT="dylib"
    TestDataDirectory="."
    ;;
esac

AC_SUBST(TestDataDirectory)
AC_SUBST(HOST_TYPE)
AC_SUBST(SHAREDLIBEXT)

dnl ---------------------------------------------------------------------------
dnl Checks for various programs and packages.
dnl ---------------------------------------------------------------------------

AC_PROG_CXX
AC_PROG_CC
AC_PROG_CPP
AC_PROG_RANLIB
AC_PROG_INSTALL
AC_PROG_MAKE_SET

AC_PATH_PROG([AUTOCONF],[autoconf],[autoconf])
AC_PATH_PROG([ACLOCAL],[aclocal],[aclocal])
AC_PATH_PROG([AR],[ar],[ar])

CONFIG_PROG_SWIG(1.3.21)
CONFIG_PROG_PYTHON
CONFIG_PROG_JAVA(1.4.0)
CONFIG_PROG_MATLAB

dnl AC_LIBTOOL_WIN32_DLL
dnl AC_PROG_LIBTOOL


dnl ---------------------------------------------------------------------------
dnl Checks for libraries.
dnl ---------------------------------------------------------------------------

AC_LANG(C)

AC_CHECK_LIB(m, isnan)

dnl Make sure to test for expat before xerces, because xerces is the default.

CONFIG_LIB_EXPAT
CONFIG_LIB_XERCES
CONFIG_LIB_LIBCHECK


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
AC_EXEEXT
AC_OBJEXT


dnl ---------------------------------------------------------------------------
dnl Miscellaneous
dnl ---------------------------------------------------------------------------

# Root directory
# Translate path for native Windows compilers for use with 'make check'

ROOT_DIR=`pwd`
case $host in
*-*-cygwin* | *-*-mingw*)
  if (cygpath --mixed $ROOT_DIR) >/dev/null 2>/dev/null; then
    ROOT_DIR=`cygpath --mixed $ROOT_DIR`
  fi
  # Extra files generated by some Windows compilers
  EXTRA_CLEAN="*.stackdump *.exp *.lib"
  ;;
esac


dnl ---------------------------------------------------------------------------
dnl Output
dnl ---------------------------------------------------------------------------

AC_CONFIG_FILES([config/makefile-common-vars.mk])
AC_CONFIG_FILES([./Makefile])
AC_CONFIG_FILES([src/Makefile])
AC_CONFIG_FILES([bindings/Makefile])
AC_CONFIG_FILES([bindings/python/Makefile])
dnl AC_CONFIG_FILES([bindings/matlab/Makefile])    # not yet ready
AC_CONFIG_FILES([bindings/java/Makefile])
AC_CONFIG_FILES([docs/Makefile])
AC_CONFIG_FILES([docs/src/Doxyfile])
AC_CONFIG_FILES([docs/src/Makefile])
AC_CONFIG_FILES([docs/formatted/Makefile])
AC_CONFIG_FILES([win32/Makefile])
AC_CONFIG_FILES([expat/Makefile])

AC_OUTPUT


dnl ---------------------------------------------------------------------------
dnl Inform users of settings
dnl ---------------------------------------------------------------------------

if test $with_expat != no; then
  xml_library="Expat"
else
  xml_library="Xerces-C++"
fi

echo ""
echo "Build Settings for ${PACKAGE_TARNAME} v${PACKAGE_VERSION}:"
echo "----------------------------------"
echo "  host type            = $HOST_TYPE"
echo "  install dir          = $prefix"
echo "  XML library          = $xml_library"
echo "  CC                   = $CC"
echo "  CXX                  = $CXX"
echo "  CFLAGS               = $CFLAGS"
echo "  CXXFLAGS             = $CXXFLAGS"
echo "  CPPFLAGS             = $CPPFLAGS"
echo "  LDFLAGS              = $LDFLAGS"
echo "  LIBS                 = $LIBS"
  
if test "$with_libcheck" != "no"; then
  echo "  use libcheck?        = $USE_LIBCHECK"
fi

if test "$with_swig" != "no"; then
  echo "  use SWIG?            = $USE_SWIG"
fi

if test "$with_python" != "no"; then
  echo "  use Python?          = $USE_PYTHON"
fi

if test "$with_java" != "no"; then
  echo "  use Java?            = $USE_JAVA"
fi

if test "$with_matlab" != "no"; then
  echo "  use Matlab?          = $USE_MATLAB"
fi

if test "$enable_tracing" != "no"; then
  echo "  enable memory trace? = 1"
fi

echo ""
