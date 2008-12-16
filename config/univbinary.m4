dnl
dnl Filename    : univbinary.m4
dnl Description : Autoconf macro to check for the options for creating universal binaries
dnl Author(s)   : SBML Team <sbml-team@caltech.edu>
dnl Organization: California Institute of Technology
dnl Created     : 2008-11-25
dnl Revision    : $Id: $
dnl $HeadURL$
dnl
dnl Portions of this code originally came from the aclocal 1.7
dnl distribution.  The main macro was named AM_PATH_PYTHON, but I (Mike
dnl Hucka) changed the names of this and various variables it defines
dnl because it is being used outside of automake.

dnl Portions of the code contained here was initially developed by:
dnl Andrew Dalke and James Henstridge.
dnl Copyright 1999, 2000, 2001, 2002  Free Software Foundation, Inc.
dnl
dnl This program is free software; you can redistribute it and/or modify
dnl it under the terms of the GNU General Public License as published by
dnl the Free Software Foundation; either version 2, or (at your option)
dnl any later version.
dnl
dnl This program is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl GNU General Public License for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with this program; if not, write to the Free Software
dnl Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
dnl 02111-1307, USA.

AC_DEFUN([CONFIG_LIB_UNIVBINARY],
[
  AC_ARG_VAR([UNIVBINARY])

  AC_ARG_ENABLE(universal-binary,
              AC_HELP_STRING([--enable-universal-binary=ARG],
                             [Generate universal binaries on MacOSX [[default=no]] 
                              ("-arch i386 -arch ppc" used when enabled without ARG) ]),
	      [enable_univbinary=$enableval],
	      [enable_univbinary=no])

  if test "x$enable_univbinary" != xno; then

    dnl this option can be used only on MacOSX.

    case $host in
    *darwin*) 
        ;;
    *)
        AC_MSG_ERROR([*** --enable-universal-binary can be used only on MacOSX. ***])
	;;
    esac


    if test "x$enable_univbinary" != xyes;
    then
      LDFLAGS="${LDFLAGS} $enable_univbinary"
      CFLAGS="${CFLAGS} $enable_univbinary"
      CXXFLAGS="${CXXFLAGS} $enable_univbinary"
    else
      default_OPTS="-arch i386 -arch ppc"
      LDFLAGS="${LDFLAGS} ${default_OPTS}"
      CFLAGS="${CFLAGS} ${default_OPTS}"
      CXXFLAGS="${CXXFLAGS} ${default_OPTS}"
    fi

    AC_DEFINE([USE_UNIVBINARY], 1, [Define to 1 to add options for creating universal binaries])
    AC_SUBST(USE_UNIVBINARY, 1)
  fi

  dnl We record the USE_XXX flag, for later testing in Makefiles.

  LIBSBML_OPTIONS="$LIBSBML_OPTIONS USE_UNIVBINARY"

])

