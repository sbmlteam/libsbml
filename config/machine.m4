dnl @file    machine.m4
dnl @brief   Autoconf macros to check for --enable-m32, --enable-m64
dnl @author  Mike Hucka
dnl $Id$
dnl $HeadURL$
dnl
dnl ---------------------------------------------------------------------------
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
dnl ---------------------------------------------------------------------------

dnl
dnl Supports --enable-universal-binary[=ARG], --enable-m32 and --enable-m64.
dnl If --enable-universal-binary is supplied, --enable-m32/m64 is ignored.
dnl
dnl Note that because of the way dolt.m4 and libtool work, setting these
dnl will affect the ../libtool script that is constructed when running
dnl ../configure.  In particular, it will result in different libtool scripts
dnl being constructed for a platform depending on whether -m32 or -m64 are
dnl used.  
dnl

AC_DEFUN([CONFIG_BINARIES],
[
  AC_ARG_ENABLE([m32],
    AC_HELP_STRING([--enable-m32],
                   [build 32-bit binaries @<:@default=autodetect@:>@]))

  AC_ARG_ENABLE([m64],
    AC_HELP_STRING([--enable-m64],
                   [build 64-bit binaries @<:@default=autodetect@:>@]))

  AC_ARG_ENABLE(universal-binary,
              AC_HELP_STRING([--enable-universal-binary@<:@=ARG@:>@],
                             [generate universal binaries on MacOSX @<:@default=no@:>@
                              ("-arch i386 -arch ppc" used when no ARG given) ]),
	      [enable_univbinary=$enableval],
	      [enable_univbinary=no])

  bin_flag=""

  if test "x$enable_m32" = "xyes"; then
    case $host in
    *darwin*) bin_flag="-arch i386" ;;
    *)        bin_flag="-m32"       ;;
    esac
  fi

  if test "x$enable_m64" = "xyes"; then
    case $host in
    *darwin*) bin_flag="-arch x86_64" ;;
    *)        bin_flag="-m64"         ;;
    esac
  fi

  CFLAGS="${CFLAGS} ${bin_flag}"
  CXXFLAGS="${CXXFLAGS} ${bin_flag}"
  LDFLAGS="${LDFLAGS} ${bin_flag}"

  dnl
  dnl If --enable-universal-binary is given, --enable-m32/m64 are ignored.
  dnl This is because it's easier to explain to people that they should 
  dnl provide the -arch options they want explicitly, rather than trying
  dnl to figure out how --enable-m32/m64 should interact with the universal
  dnl binary setting.  Also, --enable-universal-binary is only valid on Macs.
  dnl

  if test "x$enable_univbinary" != xno; then

    dnl This option can be used only on MacOSX.

    if test -z `expr *darwin*:$host`; then
        AC_MSG_ERROR([
***************************************************************************
Very sorry, but the option --enable-universal-binary can be used only on
MacOSX systems.  The purpose of the flag is to configure the creation of
multiarchitecture binaries (also known as "fat" binaries, not to be 
confused with the FAT filesystem).  Out of the set of platforms supported 
by libSBML, only MacOSX systems commonly support fat binaries.
***************************************************************************
])
    fi

    if test "x$enable_univbinary" != xyes;
    then
      CFLAGS="${CFLAGS} $enable_univbinary"
      CXXFLAGS="${CXXFLAGS} $enable_univbinary"
      LDFLAGS="${LDFLAGS} $enable_univbinary"
    else
      dnl If we're on Snow Leopard, build 64-bit versions of the binaries in
      dnl addition to PPC versions, because Snow Leopard builds everything
      dnl else in 64-bit by default.  This makes libSBML more compatible.

      OSX_MAJOR_VER=`uname -r | cut -d'.' -f1`
      if expr ${OSX_MAJOR_VER} \>= 10 | grep -q 1; then
	default_OPTS="-arch i386 -arch x86_64 -arch ppc"
      elif expr ${OSX_MAJOR_VER} \>= 9 | grep -q 1; then
	default_OPTS="-arch i386 -arch ppc"
      fi

      CFLAGS="${CFLAGS} ${default_OPTS}"
      CXXFLAGS="${CXXFLAGS} ${default_OPTS}"
      LDFLAGS="${LDFLAGS} ${default_OPTS}"
    fi

    AC_DEFINE([USE_UNIVBINARY], 1, [Define to 1 to add options for creating universal binaries])
    AC_SUBST(USE_UNIVBINARY, 1)
  fi

  dnl Trim leading/trailing empty spaces from the strings.

  CFLAGS="`echo ${CFLAGS} | sed -e 's/^ *//' -e 's/ *$//'`"
  CXXFLAGS="`echo ${CXXFLAGS} | sed -e 's/^ *//' -e 's/ *$//'`"
  LDFLAGS="`echo ${LDFLAGS} | sed -e 's/^ *//' -e 's/ *$//'`"

  dnl We record the USE_XXX flag, for later testing in Makefiles.

  LIBSBML_OPTIONS="$LIBSBML_OPTIONS USE_UNIVBINARY"

])

