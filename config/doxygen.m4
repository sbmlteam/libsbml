dnl Filename    : doxygen.m4
dnl Description : Autoconf macro to check for existence of Doxygen
dnl Author(s)   : Mike Hucka <mhucka@caltech.edu>
dnl Created     : 2007-04-16
dnl Revision    : $Id$
dnl $HeadURL$
dnl
dnl ---------------------------------------------------------------------------
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
dnl ---------------------------------------------------------------------------

dnl
dnl Supports --with-doxygen[=PREFIX]
dnl

dnl WARNING: make sure to invoke this *after* invoking the check for Java,
dnl because there's a test at the end of this file involving $with_java.

AC_DEFUN([CONFIG_PROG_DOXYGEN],
[
  AC_SUBST(DOXYGEN_CONFIG_OPT)

  AC_ARG_WITH([doxygen],
    AC_HELP_STRING([--with-doxygen=PREFIX],
                   [Specify path to doxygen [[default=autodetect]]]),
    [with_doxygen=$withval],
    [with_doxygen=no])

  if test "$with_doxygen" != "no"; then
    if test "$with_doxygen" != "yes"; then
      dnl Users seems to have supplied a prefix directory path.  See if we can
      dnl find doxygen somewhere in the given tree.
  
      dnl 1st remove trailing slashes because it can confuse tests below.
  
      with_doxygen=`echo $with_doxygen | sed -e 's,\(.*\)/$,\1,g'`
  
      AC_PATH_PROG([DOXYGEN], [doxygen], [no], 
                   [$with_doxygen/bin $with_doxygen/Contents/Resources $with_doxygen])
      AC_SUBST(DOXYGEN_CONFIG_OPT,[=$with_doxygen])
    else
      dnl Nothing supplied -- look for doxygen on the user's path.
      AC_PATH_PROG([DOXYGEN], [doxygen])
    fi
  fi
  
  if test -n "$DOXYGEN" -a "$DOXYGEN" != "no"; then
    dnl We've found a copy of doxygen.
    dnl Check the version if required.

    m4_ifvaln([$1], [
      AC_MSG_CHECKING($DOXYGEN version >= $1)

      changequote(<<, >>)

      rx=`echo $1 | sed -e 's/\([0-9]*\)\.\([0-9]*\)\.\([0-9]*\).*/\1/'`
      ry=`echo $1 | sed -e 's/\([0-9]*\)\.\([0-9]*\)\.\([0-9]*\).*/\2/'`
      rz=`echo $1 | sed -e 's/\([0-9]*\)\.\([0-9]*\)\.\([0-9]*\).*/\3/'`
	
      version=`"$DOXYGEN" --version | tr -d '\015'`

      dx=`echo $version | sed -e 's/\([0-9]*\)\.\([0-9]*\)\.\([0-9]*\).*/\1/'`
      dy=`echo $version | sed -e 's/\([0-9]*\)\.\([0-9]*\)\.\([0-9]*\).*/\2/'`
      dz=`echo $version | sed -e 's/\([0-9]*\)\.\([0-9]*\)\.\([0-9]*\).*/\3/'`

      changequote([, ])

      if test $dx -gt $rx \
         || (test $dx -eq $rx -a $dy -gt $ry) \
         || (test $dx -eq $rx -a $dy -eq $ry -a $dz -ge $rz); then
        AC_MSG_RESULT(yes (found $dx.$dy.$dz))
      else
        AC_MSG_RESULT(no)
        AC_MSG_ERROR([Need DOXYGEN version $1, but only found version $dx.$dy.$dz.])
      fi

    ])

    AC_DEFINE([USE_DOXYGEN], 1, [Define to 1 to use DOXYGEN])
    AC_SUBST(USE_DOXYGEN, 1)

    dnl Check the existence of a jar file for javadoc if --with-java enabled.
    dnl The jar file is classes.jar (MacOSX) or tools.jar (other OSes).

    if test "$with_java" != "no"; then
      AC_MSG_CHECKING(for javadoc)
      if ! test -e $JAVADOC_JAR; then
        AC_MSG_RESULT(no)
        AC_MSG_ERROR([*** missing $JAVADOC_JAR - please install first or check config.log ***])
      else
        AC_MSG_RESULT(yes)
      fi
    fi

  else
    DOXYGEN="no-doxygen-found"
  fi

])
