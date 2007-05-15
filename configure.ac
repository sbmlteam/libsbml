dnl
dnl Filename    : configure.ac
dnl Description : Run this file through autoconf to produce `configure'
dnl Author(s)   : SBML Team <sbml-team@caltech.edu>
dnl Created     : 2003-02-13
dnl Revision    : $Id$
dnl Source      : $Source$
dnl
dnl Copyright 2002-2004 California Institute of Technology and
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
dnl     Ben Bornstein and Michael Hucka
dnl
dnl     The SBML Team
dnl     Control and Dynamical Systems, MC 107-81
dnl     California Institute of Technology
dnl     Pasadena, CA, 91125, USA
dnl
dnl     http://sbml.org
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
   m4_define(LIBSBML_VERSION, 
       m4_esyscmd(cat VERSION.txt  | sed 's/\-.*$//' | tr -d '\n\r'))
fi

AC_INIT([libSBML], LIBSBML_VERSION, [sbml-team@caltech.edu], [libsbml])
AC_CONFIG_AUX_DIR(config)

AC_REVISION($Revision$)

AC_PREREQ(2.57)

AC_CONFIG_SRCDIR(src/Makefile.in)

AC_CONFIG_HEADERS(src/common/libsbml-config-unix.h)
AC_CONFIG_HEADERS(src/common/libsbml-package.h)


dnl ---------------------------------------------------------------------------
dnl Command-line options
dnl ---------------------------------------------------------------------------

AC_ARG_ENABLE([memory-tracing],
  AC_HELP_STRING([--enable-memory-tracing],
                 [Enable memory tracing (default: disabled)]),
    [enable_tracing=$withval],
    [enable_tracing=no])

if test "$enable_tracing" != no; then
  AC_DEFINE([TRACE_MEMORY], 1,
            [Define to 1 to enable primitive memory tracing.])
fi


AC_ARG_ENABLE([layout],
  AC_HELP_STRING([--enable-layout=yes|no],
                 [Build the SBML layout extension (default=no)]),
  [enable_layout=$withval],
  [enable_layout=no])

if test "$enable_layout" != no; then
  AC_DEFINE([USE_LAYOUT], 1, [Define to 1 to build the SBML layout extension.])
  AC_SUBST(USE_LAYOUT, 1)
fi

dnl Other cmd-line options are defined in the individual .m4 files in the
dnl config/ subdirectory.

dnl ---------------------------------------------------------------------------
dnl Check host operating system.
dnl ---------------------------------------------------------------------------

AC_CANONICAL_HOST

TestDataDirectory="\$(srcdir)"

case $host_os in
  *linux*)
    CPPFLAGS="-DLINUX $CPPFLAGS"
    HOST_TYPE="linux"
    SHAREDLIBEXT="so"
    ;;

  *solaris*)
    CPPFLAGS="-DSOLARIS $CPPFLAGS"
    HOST_TYPE="solaris"
    SHAREDLIBEXT="so"
    ;;

  *cygwin*)
    CPPFLAGS="-DWIN32 -DCYGWIN -DLIBSBML_EXPORTS $CPPFLAGS"
    HOST_TYPE="cygwin"
    SHAREDLIBEXT="dll"
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

dnl Provide dynamic library path settings.

CONFIG_LDPATH


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

CONFIG_PROG_SWIG(1.3.31)
CONFIG_PROG_PYTHON
CONFIG_PROG_PERL
CONFIG_PROG_JAVA(1.4.0)
CONFIG_PROG_MATLAB
CONFIG_PROG_LISP

CONFIG_PROG_DOXYGEN(1.4.5)

dnl AC_LIBTOOL_WIN32_DLL
dnl AC_PROG_LIBTOOL


dnl ---------------------------------------------------------------------------
dnl Checks for libraries.
dnl ---------------------------------------------------------------------------

AC_LANG(C)

AC_CHECK_LIB(m, isnan)

dnl Make sure to test xerces *last*, because xerces is the default and the
dnl config macro needs to know whether any other XML libraries have been
dnl chosen instead of it.

CONFIG_LIB_EXPAT
CONFIG_LIB_LIBXML
CONFIG_LIB_XERCES
CONFIG_LIB_LIBCHECK(0.9.2)


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

dnl In order to allow certain Makefile tests, we define a variable that
dnl stores a list of options available to configure.  This variable is
dnl added to by the configure options defined in config/ .m4 files.

AC_DEFINE(LIBSBML_OPTIONS)
AC_SUBST(LIBSBML_OPTIONS)


dnl ---------------------------------------------------------------------------
dnl Output
dnl ---------------------------------------------------------------------------

AC_CONFIG_FILES([config/makefile-common-vars.mk])
AC_CONFIG_FILES([./Makefile])

AC_CONFIG_FILES([src/Makefile])

AC_CONFIG_FILES([src/common/Makefile])

AC_CONFIG_FILES([src/bindings/Makefile])

AC_CONFIG_FILES([src/bindings/python/Makefile])
AC_CONFIG_FILES([src/bindings/python/setup.py])

AC_CONFIG_FILES([src/bindings/perl/Makefile])
AC_CONFIG_FILES([src/bindings/perl/Makefile.PL])

AC_CONFIG_FILES([src/bindings/matlab/Makefile])
AC_CONFIG_FILES([src/bindings/matlab/test/Makefile])

AC_CONFIG_FILES([src/bindings/java/Makefile])

AC_CONFIG_FILES([src/bindings/lisp/Makefile])
dnl logical pathnames for build
AC_CONFIG_FILES([src/bindings/lisp/libsbml-config.lisp])
AC_CONFIG_FILES([src/bindings/lisp/build.lisp])
dnl logical pathnames for installed version
AC_CONFIG_FILES([src/bindings/lisp/libsbml-config-run.lisp])

AC_CONFIG_FILES([src/annotation/Makefile])
AC_CONFIG_FILES([src/annotation/test/Makefile])

AC_CONFIG_FILES([src/math/Makefile])
AC_CONFIG_FILES([src/math/test/Makefile])

AC_CONFIG_FILES([src/units/Makefile])
AC_CONFIG_FILES([src/units/test/Makefile])

AC_CONFIG_FILES([src/sbml/Makefile])
AC_CONFIG_FILES([src/sbml/test/Makefile])

AC_CONFIG_FILES([src/sbml/layout/Makefile])
AC_CONFIG_FILES([src/sbml/layout/test/Makefile])

AC_CONFIG_FILES([src/util/Makefile])
AC_CONFIG_FILES([src/util/test/Makefile])

AC_CONFIG_FILES([src/validator/Makefile])
AC_CONFIG_FILES([src/validator/constraints/Makefile])
AC_CONFIG_FILES([src/validator/test/Makefile])

AC_CONFIG_FILES([src/xml/Makefile])
AC_CONFIG_FILES([src/xml/test/Makefile])

AC_CONFIG_FILES([docs/Makefile])
AC_CONFIG_FILES([docs/src/doxygen-config-c.txt])
AC_CONFIG_FILES([docs/src/doxygen-config-cpp.txt])
AC_CONFIG_FILES([docs/src/Makefile])
AC_CONFIG_FILES([docs/src/tex/libsbmlextras.sty])

AC_CONFIG_FILES([win32/Makefile])
AC_CONFIG_FILES([win32/MSVC6/Makefile])
AC_CONFIG_FILES([win32/MSVC7/Makefile])

AC_CONFIG_FILES([examples/c/Makefile])
AC_CONFIG_FILES([examples/c++/Makefile])
AC_CONFIG_FILES([examples/java/Makefile])

if test "$enable_layout" != "no" ; then
  AC_CONFIG_FILES([examples/layout/Makefile])
fi

AC_OUTPUT


dnl ---------------------------------------------------------------------------
dnl Inform users of settings
dnl ---------------------------------------------------------------------------

dnl Gather up the values of the flags into one set of variables for easier
dnl printing below.

if test $with_expat != no; then
  xml_library="Expat"
  xml_cppflags=$EXPAT_CPPFLAGS
  xml_ldflags=$EXPAT_LDFLAGS
  xml_libs=$EXPAT_LIBS
elif test $with_libxml != no; then
  xml_library="libxml2"
  xml_cppflags=$LIBXML_CPPFLAGS
  xml_ldflags=$LIBXML_LDFLAGS
  xml_libs=$LIBXML_LIBS
else
  xml_library="Xerces-C++"
  xml_cppflags=$XERCES_CPPFLAGS
  xml_ldflags=$XERCES_LDFLAGS
  xml_libs=$XERCES_LIBS
fi


dnl Simple helper function used below.

print_flag()
{
  if test -n "$2" -a "$2" != "no"; then
    i=`expr 13 - ${#1}`
    whitespace="             "
    echo "  Using $1 with path ${whitespace:0:i}= $3"
  fi
}

 
echo ""
echo "------------------------------------------------------------"
echo "${PACKAGE_NAME} version ${PACKAGE_VERSION}"
echo ""
echo "Configured on host \"`hostname`\""
echo "  host type                     = $HOST_TYPE"
echo "  host vendor                   = $host_vendor"
echo "  host operating system         = $host_os"
echo "  host CPU architecture         = $host_cpu"
echo ""
echo "Build flags:"
echo "  CC                            = $CC"
echo "  CXX                           = $CXX"
echo "  CFLAGS                        = $CFLAGS"
echo "  CXXFLAGS                      = $CXXFLAGS"
echo "  CPPFLAGS                      = $CPPFLAGS"
echo "  LDFLAGS                       = $LDFLAGS"
echo "  LIBS                          = $LIBS"
echo ""
echo "XML parser library configuration:"
echo "  XML library                   = $xml_library"
echo "  CPPFLAGS for XML              = $xml_cppflags"
echo "  LDFLAGS for XML               = $xml_ldflags"
echo "  LIBS for XML                  = $xml_libs"
echo ""
echo "Other ${PACKAGE_NAME} configuration settings:"
echo "  install dir (\$prefix)         = $prefix"

print_flag "libcheck"  	$with_libcheck	$with_check
print_flag "SWIG"   	$with_swig	$SWIG
print_flag "Java"   	$with_java	$JAVA
print_flag "Python" 	$with_python	$PYTHON
print_flag "Perl"   	$with_perl	$PERL
print_flag "MATLAB"   	$with_matlab	$MATLAB
print_flag "Lisp"   	$with_lisp	$lisp
print_flag "Asdf"   	$EXT_ASDF	$ASDF
print_flag "UFFI"   	$EXT_UFFI	$UFFI

if test "$enable_layout" != "no" ; then
  echo "  use Layout Extension        = yes  (experimental)"
fi

echo ""
echo "Other potentially important settings:"
if test "$HOST_TYPE" = "darwin"; then
  echo "  value of \$DYLD_LIBRARY_PATH   = $DYLD_LIBRARY_PATH"
else
  echo "  value of \$LD_LIBRARY_PATH     = $LD_LIBRARY_PATH"
fi
if test -n "$with_java" -a "$with_java" != "no"; then
  echo "  value of \$CLASSPATH for Java  = $CLASSPATH"  
fi

dnl Temporarily unavailable anyway, so skip this for now:
dnl echo "  enable memory tracing? = $enable_tracing"

echo "------------------------------------------------------------"
echo ""
