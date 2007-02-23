dnl
dnl Filename    : xercesc.m4
dnl Description : Autoconf macro to check for existence of Xerces-C library
dnl Author(s)   : Ben Bornstein, Mike Hucka
dnl Created     : 2003-02-14
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
dnl     Ben Bornstein and Mike Hucka
dnl
dnl     The SBML Team
dnl     Control and Dynamical Systems, MC 107-81
dnl     California Institute of Technology
dnl     Pasadena, CA, 91125, USA
dnl
dnl     http://sbml.org
dnl     mailto:sbml-team@caltech.edu
dnl

dnl
dnl Check --with-xerces[=PREFIX] is specified and Xerces-C++ is installed.
dnl

AC_DEFUN([CONFIG_LIB_XERCES],
[
  AC_ARG_WITH([xerces],
    AC_HELP_STRING([--with-xerces=PREFIX],
                   [Use the Xerces XML Library [[default=yes]]]),
    [with_xerces="$withval"],
    [with_xerces=yes])

  if test $with_expat != no -o $with_libxml != no; then
    with_xerces=no
  fi

  if test $with_xerces != no; then

    AC_MSG_CHECKING([for Apache's Xerces-C++ XML library])
    AC_MSG_RESULT([])

    AC_LANG_PUSH(C++)

    if test $with_xerces != yes; then
      xerces_root="$with_xerces"
      CONFIG_ADD_LDPATH($xerces_root/lib)

      XERCES_CPPFLAGS="-I$xerces_root/include"
      XERCES_LDFLAGS="-L$xerces_root/lib"
    fi

    XERCES_LIBS="-lxerces-c"

    dnl The following is grungy but I don't know how else to make 
    dnl AC_CHECK_LIB use particular library and include paths.

    tmp_CPPFLAGS=$CPPFLAGS
    tmp_LDFLAGS=$LDFLAGS
    tmp_LIBS=$LIBS
    CPPFLAGS="$CPPFLAGS $XERCES_CPPFLAGS"
    LDFLAGS="$LDFLAGS $XERCES_LDFLAGS"
    LIBS="$LIBS $XERCES_LIBS"

    AC_CHECK_HEADER([xercesc/util/XercesVersion.hpp],,
	AC_MSG_ERROR([unable to find Xerces-C++ header files]))

    AC_MSG_CHECKING([Xerces-C++ library version])
    AC_PREPROC_IFELSE(
      [AC_LANG_PROGRAM([#include <xercesc/util/XercesVersion.hpp>], [
#if  _XERCES_VERSION == 20600
#error cannot use Xerces version 2.6.0
#else
exit(0);
#endif
])],
      [AC_MSG_RESULT(OK)],
      [ echo "*** Xerces-C++ version 2.6.0 has serious bugs and cannot be"
	echo "*** used by libSBML.  Please use either versions 2.4 - 2.5 or"
	echo "*** version 2.7.0"
	AC_MSG_ERROR([unable to use this version of Xerces-C++ library])])

    AC_MSG_CHECKING([ability to link with Xerces-C++ library])
    AC_TRY_LINK([
#include <xercesc/util/XercesDefs.hpp>
#include <xercesc/util/PlatformUtils.hpp>
#ifndef XERCES_HAS_CPP_NAMESPACE
#define XERCES_CPP_NAMESPACE_QUALIFIER
#endif
],
      [XERCES_CPP_NAMESPACE_QUALIFIER XMLPlatformUtils::Initialize();],
      [AC_MSG_RESULT([yes])],
      [AC_MSG_ERROR([unable to link with the Xerces-C++ XML library.])])

    CPPFLAGS=$tmp_CPPFLAGS
    LDFLAGS=$tmp_LDFLAGS
    LIBS=$tmp_LIBS

    AC_LANG_POP

    AC_DEFINE([USE_XERCES], 1, [Define to 1 to use the Xerces XML library])
    AC_SUBST(USE_XERCES, 1)

    AC_SUBST(XERCES_CPPFLAGS)
    AC_SUBST(XERCES_LDFLAGS)
    AC_SUBST(XERCES_LIBS)

  fi

  dnl We record the USE_XXX flag, for later testing in Makefiles.

  LIBSBML_OPTIONS="$LIBSBML_OPTIONS USE_XERCES"

])
