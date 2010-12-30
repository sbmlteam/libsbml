dnl
dnl Filename    : xercesc.m4
dnl Description : Autoconf macro to check for existence of Xerces-C library
dnl Author(s)   : Ben Bornstein, Mike Hucka
dnl Created     : 2003-02-14
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
dnl Check --with-xerces[=PREFIX] is specified and Xerces-C++ is installed.
dnl

AC_DEFUN([CONFIG_LIB_XERCES],
[
  AC_ARG_WITH([xerces],
    AC_HELP_STRING([--with-xerces@<:@=PREFIX@:>@],
                   [use the Xerces XML Library @<:@default=no@:>@]),
    [with_xerces="$withval"],
    [with_xerces=no])

  if test $with_xerces != no; then

    AC_MSG_CHECKING([for Apache's Xerces-C++ XML library])
    AC_MSG_RESULT([])

    AC_LANG_PUSH(C++)

    if test $with_xerces != yes; then
      xerces_root="$with_xerces"
      xerces_lib_path="$xerces_root/lib${LIBSUFFIX}"
      CONFIG_ADD_LDPATH($xerces_lib_path)

      XERCES_CPPFLAGS="-I$xerces_root/include"
      XERCES_LDFLAGS="-L$xerces_lib_path"
    else
      dnl On the Macs, if the user has installed xerces via Fink and they
      dnl used the default Fink install path of /sw, the following should
      dnl catch it.  We do this so that Mac users are more likely to find
      dnl success even if they only type --with-xerces.

      dnl This is a case statement in case we need to do something similar
      dnl for other host types in the future.

      case $host in
      *darwin*) 
        if test -e "/sw"; then
          xerces_root="/sw"
          xerces_lib_path="/sw/lib"
          CONFIG_ADD_LDPATH($xerces_lib_path)
          XERCES_CPPFLAGS="-I$xerces_root/include"
          XERCES_LDFLAGS="-L$xerces_lib_path"
        fi
	;;
      esac    

      dnl Note that CONFIG_ADD_LDPATH is deliberately not called in cases
      dnl other than the two above.
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

    # Set up LD_LIBRARY_PATH/DYLD_LIBRARY_PATH for compiling the
    # test program below

    tmp_library_path=""
    case $host in
    *darwin*) 
      tmp_library_path="$DYLD_LIBRARY_PATH"
      DYLD_LIBRARY_PATH="$DYLD_LIBRARY_PATH;$xerces_lib_path"
      export DYLD_LIBRARY_PATH
      ;;
    *)
      tmp_library_path="$LD_LIBRARY_PATH"
      LD_LIBRARY_PATH="$LD_LIBRARY_PATH;$xerces_lib_path"
      export LD_LIBRARY_PATH
      ;;
    esac    

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
    case $host in
    *darwin*) 
      DYLD_LIBRARY_PATH=$tmp_library_path
      export DYLD_LIBRARY_PATH
      ;;
    *)
      LD_LIBRARY_PATH=$tmp_library_path
      export LD_LIBRARY_PATH
      ;;
    esac    

    AC_LANG_POP

    AC_DEFINE([USE_XERCES], 1, [Define to 1 to use the Xerces XML library])
    AC_SUBST(USE_XERCES, 1)

    AC_SUBST(XERCES_CPPFLAGS)
    AC_SUBST(XERCES_LDFLAGS)
    AC_SUBST(XERCES_LIBS)

    AC_SUBST(XML_PARSER, [xerces])

  fi

  dnl We record the USE_XXX flag, for later testing in Makefiles.

  LIBSBML_OPTIONS="$LIBSBML_OPTIONS USE_XERCES"

])
