dnl Filename    : doxygen.m4
dnl Description : Autoconf macro to check for existence of Doxygen
dnl Author(s)   : Mike Hucka <mhucka@caltech.edu>
dnl Created     : 2007-04-16
dnl Revision    : $Id$
dnl Source      : $Source$
dnl
dnl ---------------------------------------------------------------------------
dnl This file is part of libSBML.  Please visit http://sbml.org for more
dnl information about SBML, and the latest version of libSBML.
dnl
dnl Copyright 2005-2007 California Institute of Technology.
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
dnl Supports --with-doxygen[=PREFIX]
dnl

AC_DEFUN([CONFIG_PROG_DOXYGEN],
[
  AC_ARG_WITH([doxygen],
    AC_HELP_STRING([--with-doxygen=PREFIX],
                   [Specify path to doxygen (default=as found on $path)]),
    [with_doxygen=$withval],
    [with_doxygen=no])

  if test $with_doxygen != yes && test $with_doxygen != no;
  then
    dnl Remove needless trailing slashes because it can confuse tests later.
    with_doxygen=`echo $with_doxygen | sed -e 's,\(.*\)/$,\1,g'`

    AC_PATH_PROG([DOXYGEN], [doxygen], [$with_doxygen],
                 [no-doxygen-found], [$with_doxygen])
  else
    dnl Nothing supplied -- look for doxygen on the user's path.
    AC_PATH_PROG([DOXYGEN], [doxygen])
  fi

  if test -n "$DOXYGEN";
  then
    dnl We've found a copy of doxygen.
    dnl Now let's see: do we care which version we have?

    DOXYGEN_REQUEST_VERSION=

    changequote(<<, >>)

    for a in $1 $2 $3 $4 $5 $6 $7 $8 $9 x; do
        case "$a" in
            x) break;;
            [0-9]*.[0-9]*.[0-9]*) DOXYGEN_REQUEST_VERSION="$a";;
        esac
    done

    changequote([, ])

    if test -n "$DOXYGEN_REQUEST_VERSION";
    then
      dnl Yes, apparently we care about the version.

      doxygen_version=`$DOXYGEN --version`

      changequote(<<, >>)

      doxygen_major_ver=`expr $doxygen_version : '\([0-9]*\)\.[0-9]*\.[0-9]*'`
      doxygen_minor_ver=`expr $doxygen_version : '[0-9]*\.\([0-9]*\)\.[0-9]*'`
      doxygen_micro_ver=`expr $doxygen_version : '[0-9]*\.[0-9]*\.\([0-9]*\)' '|' 0`

      doxygen_major_req=`expr $DOXYGEN_REQUEST_VERSION : '\([0-9]*\)\.[0-9]*\.[0-9]*'`
      doxygen_minor_req=`expr $DOXYGEN_REQUEST_VERSION : '[0-9]*\.\([0-9]*\)\.[0-9]*'`
      doxygen_micro_req=`expr $DOXYGEN_REQUEST_VERSION : '[0-9]*\.[0-9]*\.\([0-9]*\)'`

      changequote([, ])

      AC_MSG_CHECKING(if doxygen version >= $DOXYGEN_REQUEST_VERSION )

      if test $doxygen_major_ver -ge $doxygen_major_req &&
         test $doxygen_minor_ver -ge $doxygen_minor_req &&
         test $doxygen_micro_ver -ge $doxygen_micro_req
      then
        AC_MSG_RESULT([yes (found $doxygen_version)])
        AC_DEFINE([USE_DOXYGEN], 1, [Define to 1 to use DOXYGEN])
        AC_SUBST(USE_DOXYGEN, 1)
      else
        AC_MSG_RESULT([no (found $doxygen_version)])
      fi

      dnl The variable DOXYGEN will be defined automatically.
      dnl The following are additional flags.

    else
      dnl We don't care which version we have.

      AC_DEFINE([USE_DOXYGEN], 1, [Define to 1 to use DOXYGEN])
      AC_SUBST(USE_DOXYGEN, 1)
    fi
  fi

])
