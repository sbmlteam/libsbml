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

    dnl
    dnl Obtain the matlab's install path 
    dnl
    MATLAB_PREFIX=`$MATLAB -e | grep '^MATLAB' | awk -F'=' '{print $2}'`

    dnl
    dnl Obtain the mex extension by mexext command.
    dnl
    MATLAB_MEXEXT="${MATLAB_PREFIX}/bin/mexext"
    MEXEXT=`${MATLAB_MEXEXT}`

    dnl Checks if the matlab (32bit) is used on Snow Leopard.
    dnl Since, by default, gcc generates a 64bit (x86_64) only binary on 
    dnl Snow Leopard, a 32bit (i386) binary needs to be explicitly built by
    dnl gcc with "-arch i386" option if matlab (32bit) is used.
    dnl Thus, an error message is printed if "-arch i386" option is not 
    dnl enabled with the combination of the matlab (32bit) and Snow Leopard.

    OSX_SNOW_LEOPARD="no"
    OSX_MAJOR_VER=`uname -r | awk -F. '{print $1}'`
    if expr ${OSX_MAJOR_VER} \>= 10 | grep -q 1; then
      OSX_SNOW_LEOPARD="yes"
    fi

    if test ${OSX_SNOW_LEOPARD} = "yes" -a ${MEXEXT} = "mexmaci"; then
      AC_MSG_CHECKING(whether -arch i386 option is enabled for Matlab 32bit on Snow Leopard.)
      OSX_ARCH_I386="no"
      if echo $CXXFLAGS | grep -q "-arch i386"; then
        OSX_ARCH_I386="yes"
      elif echo $CPPFLAGS | grep -q "-arch i386"; then
        OSX_ARCH_I386="yes"
      fi

      if test ${OSX_ARCH_I386} = "no"; then
        AC_MSG_RESULT(no)
        AC_MSG_ERROR([libSBML needs to be built explicitly to include a 32-bit (i386) version, 
                      because your copy of MATLAB is a 32-bit version.  By default, MacOS 10.6+
	 	      (Snow Leopard) builds everything as 64-bit (x86_64) binaries.  Please add
                            --enable-universal-binary="-arch i386 -arch x86_64" 
                      to your configure options, re-run the configure step, and recompile.  If
                      you get a compilation error, please check whether you have a private 
                      version of a dependent library (e.g., expat, libxml, or xerces) that was 
                      built only as a 32-bit version, and either remove or recompile it before
	              proceeding future.])
      else
        AC_MSG_RESULT(yes)
      fi
      
    fi

    AC_DEFINE([USE_MATLAB], 1, [Define to 1 to use Matlab])
    AC_SUBST(USE_MATLAB, 1)

    AC_SUBST(MATLAB_CPPFLAGS)
    AC_SUBST(MATLAB_LDFLAGS)
    AC_SUBST(MATLAB_LIBS)
    AC_SUBST(MEX_FLAGS)
    AC_SUBST(MEXEXT)

  fi

  dnl We record the USE_XXX flag, for later testing in Makefiles.

  LIBSBML_OPTIONS="$LIBSBML_OPTIONS USE_MATLAB"

])
