dnl
dnl Filename    : python.m4
dnl Description : Autoconf macro to check for Python
dnl Author(s)   : SBML Team <sbml-team@caltech.edu>
dnl Organization: California Institute of Technology
dnl Created     : 2004-06-18
dnl Revision    : $Id$
dnl Source      : $Source$
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

AC_DEFUN([CONFIG_PROG_PYTHON],
[
  AC_ARG_VAR([PYTHON])

  AC_ARG_WITH(python,
              AC_HELP_STRING([--with-python=PREFIX],
                             [Generate Python interface library [[default=no]]]),
	      [with_python=$withval],
	      [with_python=no])

  if test $with_python != no; then

    dnl Find a python executable.

    if test $with_python != yes;
    then
      AC_PATH_PROG([PYTHON], [python], [$with_python/bin/python],
                     [no-python-found], [$with_python/bin])
    else
      AC_PATH_PROG([PYTHON], [python])
    fi

    dnl check version if required
    m4_ifvaln([$1], [
        AC_MSG_CHECKING($PYTHON version >= $1)
        if test `$PYTHON -c ["import sys; print sys.version[:3] >= \"$1\" and \"OK\" or \"OLD\""]` = "OK"
        then
          AC_MSG_RESULT(ok)
        else
          AC_MSG_RESULT(no)
        fi
      fi])

    AC_MSG_CHECKING(for Python prefix)
    PYTHON_PREFIX=`($PYTHON -c "import sys; print sys.prefix") 2>/dev/null`
    AC_MSG_RESULT($PYTHON_PREFIX)

    changequote(<<, >>)
    PYTHON_VERSION=`$PYTHON -c "import sys; print sys.version[:3]"`
    changequote([, ])

    PYTHON_NAME="python${PYTHON_VERSION}"

    dnl Figure out the last bits for linking.
    dnl This comes in part from SWIG 1.3.31's configure.ac file.

    case $host in
    *darwin*) 
	dnl Got an ugly situation on MacOS X: need different args depending
	dnl on whether the system-installed or Fink-installed version of
	dnl Python is being used.  The following is a heuristic approach
	dnl that may not be correct for all situations.  The heuristic is:
	dnl if the Python executable is /usr/something, assume it's the
	dnl system-installed version and use -framework; otherwise, assume
	dnl it's either the Fink version or something else, and don't use
	dnl -framework.

	if test `expr ${PYTHON} ':' '/usr/bin/.*'` -ne 0; then
	  dnl MacOSX-installed version of Python (we hope).
   	  PYTHON_CPPFLAGS="-I${PYTHON_PREFIX}/include/${PYTHON_NAME}"
	  PYTHON_LDFLAGS="-L${PYTHON_PREFIX}/lib/${PYTHON_NAME}/lib-dynload -framework Python"
	else
	  dnl Fink-installed version of Python, or something else.
   	  PYTHON_CPPFLAGS="-I${PYTHON_PREFIX}/include/${PYTHON_NAME}"
	  PYTHON_LDFLAGS="-L${PYTHON_PREFIX}/lib/${PYTHON_NAME}/lib-dynload -bundle_loader ${PYTHON}"
	fi
        CONFIG_ADD_LDPATH(${PYTHON_PREFIX}/lib/${PYTHON_NAME}/lib-dynload)
	;;
    *cygwin* | *mingw*) 
	PYTHON_CPPFLAGS="-I${PYTHON_PREFIX}/include/${PYTHON_NAME} -DUSE_DL_IMPORT"
	PYTHON_LDFLAGS="-L${PYTHON_PREFIX}/lib/${PYTHON_NAME}/config"
        PYTHON_LIBS="-l${PYTHON_NAME}"
	CONFIG_ADD_LDPATH(${PYTHON_PREFIX}/lib/${PYTHON_NAME}/config)
	;;
    *)
	PYTHON_CPPFLAGS="-I${PYTHON_PREFIX}/include/${PYTHON_NAME}"
        PYTHON_LDFLAGS="-L${PYTHON_PREFIX}/lib/${PYTHON_NAME}/config"
        PYTHON_LIBS="-l${PYTHON_NAME}"
	CONFIG_ADD_LDPATH(${PYTHON_PREFIX}/lib/${PYTHON_NAME}/config)
	;;
    esac

    AC_DEFINE([USE_PYTHON], 1, [Define to 1 to use Python])
    AC_SUBST(USE_PYTHON, 1)

    AC_SUBST(PYTHON_CPPFLAGS)
    AC_SUBST(PYTHON_LDFLAGS)
    AC_SUBST(PYTHON_LIBS)

  fi

  dnl We record the USE_XXX flag, for later testing in Makefiles.

  LIBSBML_OPTIONS="$LIBSBML_OPTIONS USE_PYTHON"

])

