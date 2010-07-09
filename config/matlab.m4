dnl
dnl Filename    : matlab.m4
dnl Description : Autoconf macro for MATLAB configuration
dnl Author(s)   : Mike Hucka
dnl Created     : 2004-08-02
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

AC_DEFUN([CONFIG_PROG_MATLAB],
[
  AC_ARG_WITH([matlab],
    AC_HELP_STRING([--with-matlab=PREFIX],
                   [Generate MATLAB interface library [[default=no]]]),
    [with_matlab=$withval],
    [with_matlab=no])

  if test $with_matlab != no; then

    if test $with_matlab != yes;
    then
      dnl Remove needless trailing slashes because it can confuse tests later.
      with_matlab=`echo $with_matlab | sed -e 's,\(.*\)/$,\1,g'`

      AC_PATH_PROG([MEX], [mex], [no], [$with_matlab/bin])
      AC_PATH_PROG([MATLAB], [matlab], [no], [$with_matlab/bin])
    else
      AC_PATH_PROG([MEX], [mex])
      AC_PATH_PROG([MATLAB], [matlab])
    fi

    if test -z "$MATLAB" -o "$MATLAB" = "no"; then
      AC_MSG_ERROR([Could not find 'matlab' executable for MATLAB.])
    elif  test -z "$MEX" -o "$MEX" = "no"; then
      AC_MSG_ERROR([Could not find 'mex' executable for MATLAB.])
    fi

    dnl Obtain MATLAB's install path:
    MATLABROOT="`$MATLAB -e | grep MATLAB= | cut -f2 -d'='`"

    dnl Determine MATLAB's machine architecture designation, so that
    dnl we can explicitly pass it to 'mex' later on.

    AC_MSG_CHECKING([what MATLAB calls this architecture])
    MEX_ARCH="`env MATLABROOT=$MATLABROOT $MATLABROOT/bin/matlab -e | egrep '^ARCH=' | cut -f2 -d'='`"
    AC_MSG_RESULT([$MEX_ARCH])

    dnl Obtain the file extension.  Be careful to tell 'mexext' the root of
    dnl the MATLAB installation tree, or it will not report the correct
    dnl architecture on some systems (e.g., MacOS 10.5 with R2010a).

    AC_MSG_CHECKING([for the MATLAB binary file extension])
    MEXEXT="`env MATLABROOT=$MATLABROOT $MATLABROOT/bin/mexext`"
    AC_MSG_RESULT([$MEXEXT])

    dnl Checks if the matlab (32bit) is used on Snow Leopard.
    dnl Since, by default, gcc generates a 64bit (x86_64) only binary on 
    dnl Snow Leopard, a 32bit (i386) binary needs to be explicitly built by
    dnl gcc with "-arch i386" option if matlab (32bit) is used.
    dnl Thus, an error message is printed if "-arch i386" option is not 
    dnl enabled with the combination of the matlab (32bit) and Snow Leopard.

    case $host in
    *darwin*) 
      OSX_MAJOR_VER=`uname -r | cut -f1 -d'.'`
      if test ${OSX_MAJOR_VER} -ge 10 -a ${MEXEXT} = "mexmaci"; then
        AC_MSG_RESULT([Note: the MATLAB version we're finding is a 32-bit version])
        AC_MSG_CHECKING([whether -arch i386 option is enabled to use MATLAB 32-bit version])
        if echo $CXXFLAGS $CPPFLAGS | grep -q "arch i386"; then
          AC_MSG_RESULT([yes, whew!])
        else
          AC_MSG_RESULT([no])
          AC_MSG_ERROR([libSBML needs to be built explicitly to include a 32-bit (i386) version, 
                        because your copy of MATLAB is a 32-bit version.  By default, MacOS 10.6+
  	 	        (Snow Leopard) builds everything as 64-bit (x86_64) binaries.  Please add
                              --enable-universal-binary="-arch i386 -arch x86_64" 
                        to your configure options, re-run the configure step, and recompile.  If
                        you get a compilation error, please check whether you have a private 
                        version of a dependent library (e.g., expat, libxml, or xerces) that was 
                        built only as a 32-bit version, and either remove or recompile it before
  	                proceeding future.])
        fi
      fi
      ;;
    esac

    dnl Conclude.

    AC_DEFINE([USE_MATLAB], 1, [Define to 1 to use MATLAB])
    AC_SUBST(USE_MATLAB, 1)

    AC_SUBST(MATLAB_CPPFLAGS)
    AC_SUBST(MATLAB_LDFLAGS)
    AC_SUBST(MATLAB_LIBS)
    AC_SUBST(MEX_FLAGS)
    AC_SUBST(MEX_ARCH)
    AC_SUBST(MEXEXT)
    AC_SUBST(MATLABROOT)

  fi

  dnl We record the USE_XXX flag, for later testing in Makefiles.

  LIBSBML_OPTIONS="$LIBSBML_OPTIONS USE_MATLAB"

])
