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
dnl     Ben Bornstein
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
dnl   Mike Hucka <mhucka@caltech.edu> Enhancements to this file.
dnl

dnl
dnl Check --with-xerces[=PREFIX] is specified and Xerces-C++ is installed.
dnl

AC_DEFUN([CONFIG_LIB_XERCES],
[
  AC_ARG_WITH([xerces],
    AC_HELP_STRING([--with-xerces=PREFIX],
                   [Use Xerces XML Library [[default=yes]]]),
    [with_xerces=$withval],
    [with_xerces=yes])

  if test $with_expat != no; then
    with_xerces=no
  fi

  if test $with_xerces != no; then

    AC_MSG_CHECKING([for Apache's Xerces-C XML library])

    AC_LANG_PUSH(C++)

    if test $with_xerces != yes; then
      xerces_root="$with_xerces"
      RUN_LDPATH="$RUN_LDPATH:$xerces_root/lib"
    fi

    XERCES_CPPFLAGS="-I$xerces_root/include"
    XERCES_LDFLAGS="-L$xerces_root/lib"
    XERCES_LIBS="-lxerces-c"

    dnl The following is grungy but I don't know how else to make 
    dnl AC_CHECK_LIB use particular library and include paths.

    tmp_CPPFLAGS=$CPPFLAGS
    tmp_LDFLAGS=$LDFLAGS
    tmp_LIBS=$LIBS
    CPPFLAGS="$CPPFLAGS $XERCES_CPPFLAGS"
    LDFLAGS="$LDFLAGS $XERCES_LDFLAGS"
    LIBS="$LIBS $XERCES_LIBS"

    AC_TRY_LINK([
#include <xercesc/util/XercesDefs.hpp>
#include <xercesc/util/PlatformUtils.hpp>
#ifndef XERCES_HAS_CPP_NAMESPACE
#define XERCES_CPP_NAMESPACE_QUALIFIER
#endif
      ],
      [XERCES_CPP_NAMESPACE_QUALIFIER XMLPlatformUtils::Initialize();],
      [xerces_found=yes],
      [xerces_found=no])

    if test $xerces_found = yes; then
      AC_MSG_RESULT([yes])
    else
      AC_MSG_ERROR([Could not find the Xerces XML library.])
    fi

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
