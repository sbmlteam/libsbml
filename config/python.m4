dnl
dnl Filename    : python.m4
dnl Description : Autoconf macro to check for Python
dnl Author(s)   : SBML Team <sbml-team@caltech.edu>
dnl Organization: California Institute of Technology
dnl Created     : 2004-06-18
dnl Revision    : $Id$
dnl $HeadURL$
dnl
dnl <!-------------------------------------------------------------------------
dnl This file is part of libSBML.  Please visit http://sbml.org for more
dnl information about SBML, and the latest version of libSBML.
dnl
dnl Copyright 2005-2009 California Institute of Technology.
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
dnl Provides --with-python[=PREFIX]
dnl

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
      dnl Remove needless trailing slashes because it can confuse tests later.
      with_python=`echo $with_python | sed -e 's,\(.*\)/$,\1,g'`

      AC_PATH_PROG([PYTHON], [python], [no], [$with_python/bin])
    else
      AC_PATH_PROG([PYTHON], [python])
    fi

    if test -z $PYTHON -o "$PYTHON" = "no" -o ! -f $PYTHON;
    then
      AC_MSG_ERROR([*** python missing - please install first or check config.log ***])
    fi

    dnl check version if required
    m4_ifvaln([$1], [
        AC_MSG_CHECKING($PYTHON version >= $1)
        if test `"$PYTHON" -c ["import sys; print sys.version[:3] >= \"$1\" and \"OK\" or \"OLD\""]` = "OK"
        then
          AC_MSG_RESULT(ok)
        else
          AC_MSG_RESULT(no)
          AC_MSG_ERROR([*** python version $1 or later is required ***])
        fi
    ])

    AC_MSG_CHECKING(for Python prefix)
    PYTHON_PREFIX=`("$PYTHON" -c "import sys; print sys.prefix") 2>/dev/null`
    AC_MSG_RESULT($PYTHON_PREFIX)

    changequote(<<, >>)
    PYTHON_VERSION=`"$PYTHON" -c "import sys; print sys.version[:3]"`
    changequote([, ])

    PYTHON_NAME="python${PYTHON_VERSION}"

    PYTHON_H="${PYTHON_PREFIX}/include/${PYTHON_NAME}/Python.h"
    AC_MSG_CHECKING(for Python.h)
    if test -z $PYTHON_H || ! test -f $PYTHON_H;
    then
      AC_MSG_RESULT(no)
      AC_MSG_ERROR([*** $PYTHON_H missing - please install first or check config.log ***])
    fi
    AC_MSG_RESULT(yes)

    dnl Figure out the last bits for linking.
    dnl This comes in part from SWIG 1.3.31's configure.ac file.

    case $host in
    *darwin*) 
	dnl Got an ugly situation on MacOS X: need different args depending
	dnl on whether the Python came from MacOS, Fink, or the Mac Python
	dnl from www.python.org.  The following uses a set of heuristics.
	dnl 1. If the python comes from /Library, assume it's Mac Python.
	dnl    Use the -framework flag.
	dnl 2. If it's from /System, assume it's the standard MacOS one.
	dnl    Use the -framework flag.
	dnl 3. If it's from anywhere else assume it's either the Fink
	dnl    version or something else, and don't use -framework.

	
	if test `expr "${PYTHON_PREFIX}" ':' '/Library/Frameworks/.*'` -ne 0; then
	  dnl Assume Mac Python from www.python.org/download/mac

   	  PYTHON_CPPFLAGS="-I${PYTHON_PREFIX}/include/${PYTHON_NAME}"
	  PYTHON_LDFLAGS="-L${PYTHON_PREFIX}/lib/${PYTHON_NAME}/lib-dynload -F/Library/Frameworks -framework Python"

	elif test `expr "${PYTHON_PREFIX}" ':' '/System/Library/Frameworks/.*'` -ne 0; then
	  dnl MacOSX-installed version of Python (we hope).

   	  PYTHON_CPPFLAGS="-I${PYTHON_PREFIX}/include/${PYTHON_NAME}"
	  PYTHON_LDFLAGS="-L${PYTHON_PREFIX}/lib/${PYTHON_NAME}/lib-dynload -F/System/Library/Frameworks -framework Python"

	else
	  macosx_version=`sw_vers -productVersion | cut -d"." -f1,2` 
          if test ${macosx_version} '>' 10.2; then
            dnl According to configure.in of Python source code, -undefined
            dnl dynamic_lookup should be used for 10.3 or later. Actually,
            dnl the option is needed to avoid undefined symbols error when
            dnl building a Python binding library with non-system-installed
            dnl Python on 10.3 or later.  Also, according to the man page of
            dnl ld, environment variable MACOSX_DEPLOYMENT_TARGET must be set
            dnl to 10.3 or higher to use -undefined dynamic_lookup.
            dnl Currently, the environment variables is set in
            dnl src/binding/python/Makefile.in.
  
   	    PYTHON_CPPFLAGS="-I${PYTHON_PREFIX}/include/${PYTHON_NAME}"
	    PYTHON_LDFLAGS="-L${PYTHON_PREFIX}/lib/${PYTHON_NAME}/lib-dynload -undefined dynamic_lookup"

	  else
	    dnl Fink-installed version of Python, or something else.

   	    PYTHON_CPPFLAGS="-I${PYTHON_PREFIX}/include/${PYTHON_NAME}"
	    PYTHON_LDFLAGS="-L${PYTHON_PREFIX}/lib/${PYTHON_NAME}/lib-dynload -bundle_loader ${PYTHON}"

	  fi
	fi
        CONFIG_ADD_LDPATH(${PYTHON_PREFIX}/lib/${PYTHON_NAME}/lib-dynload)

        # MacOS X note: this MUST remain .so even though we use .dylib for libsbml.
        PYTHON_EXT="so"
	;;
    *cygwin* | *mingw*) 
	PYTHON_CPPFLAGS="-I${PYTHON_PREFIX}/include/${PYTHON_NAME} -DUSE_DL_IMPORT"
	PYTHON_LDFLAGS="-L${PYTHON_PREFIX}/lib/${PYTHON_NAME}/config"
        PYTHON_LIBS="-l${PYTHON_NAME}"
	CONFIG_ADD_LDPATH(${PYTHON_PREFIX}/lib/${PYTHON_NAME}/config)
        PYTHON_EXT="dll"
	;;
    *)
	PYTHON_CPPFLAGS="-I${PYTHON_PREFIX}/include/${PYTHON_NAME}"
        PYTHON_LDFLAGS="-L${PYTHON_PREFIX}/lib/${PYTHON_NAME}/config"
        PYTHON_LIBS="-l${PYTHON_NAME}"
	CONFIG_ADD_LDPATH(${PYTHON_PREFIX}/lib/${PYTHON_NAME}/config)
        PYTHON_EXT="so"
	;;
    esac

    AC_DEFINE([USE_PYTHON], 1, [Define to 1 to use Python])
    AC_SUBST(USE_PYTHON, 1)

    AC_SUBST(PYTHON_CPPFLAGS)
    AC_SUBST(PYTHON_LDFLAGS)
    AC_SUBST(PYTHON_LIBS)
    AC_SUBST(PYTHON_EXT)

  fi

  dnl We record the USE_XXX flag, for later testing in Makefiles.

  LIBSBML_OPTIONS="$LIBSBML_OPTIONS USE_PYTHON"

])

