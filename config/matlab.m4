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

    dnl So we think we have a path to "matlab".  You would think that we
    dnl could now invoke the script as advertised, with the -e flag, to
    dnl print the environment variables MATLAB will use.  Except there's a
    dnl problem.  All versions of that script since at least R2007b have
    dnl unconditional code in there that invokes "fvwmfix" (a utility the
    dnl MathWorks ships) if the architecture is gnlx86, regardless of the
    dnl command-line arguments given (including -e, which doesn't even need
    dnl to start MATLAB).  Unfortunately, if you don't have an X system
    dnl running at the time, fvwmfix WILL HANG.  We can't by-pass the
    dnl "matlab" script and invoke some of the others like util/arch.sh,
    dnl because *those* sub-scripts rely on the parent scripts like
    dnl "matlab" to define certain shell routines that they use.
    dnl Basically, we're screwed: if the user is trying to configure libSBML
    dnl while in an environment where X isn't accessible (e.g., on a Mac,
    dnl running ssh to a Linux box -- not an uncommon scenario), they are
    dnl almost certainly going to see our "configure" script hang.

    dnl The following tries to work around this problem for most scenarios.
    dnl First, look for "activate_matlab.sh" in the same place where "mex"
    dnl is found, and use it, because it's not prone to hanging.  If
    dnl "activate_matlab.sh" is not found there, try looking in the place
    dnl where "matlab" was found.  We know "activate_matlab.sh" doesn't
    dnl exist prior to R2008, so if we can't find it, either the user has
    dnl copies of mex and matlab living outside of the MATLAB installation
    dnl directory (unlikely, but not impossible), or else they have MATLAB
    dnl version R2007 or earlier.  At this point, we have to give up and
    dnl risk using the buggy "matlab" script, because there's no reliable
    dnl way of discerning what MATLAB thinks is its ARCH value.

    TMP_ROOT_MEX="`dirname $MEX`"
    TMP_ROOT_MATLAB="`dirname $MATLAB`"
    ACTIVATE=""
    if test -e "$TMP_ROOT_MEX/activate_matlab.sh"; then
      ACTIVATE="$TMP_ROOT_MEX/activate_matlab.sh"
    elif test -e "$TMP_ROOT_MATLAB/activate_matlab.sh"; then
      ACTIVATE="$TMP_ROOT_MATLAB/activate_matlab.sh"
    fi

    if test -n "$ACTIVATE"; then
      AC_MSG_CHECKING([for the value of MATLABROOT])
      MATLABROOT="`$ACTIVATE -v -test | grep MATLABROOT | cut -f2 -d'=' | tr -d ' '`"
      AC_MSG_RESULT([$MATLABROOT])

      AC_MSG_CHECKING([the architecture of the MATLAB installation])
      MEX_ARCH="`$ACTIVATE -v -test | grep ARCH | cut -f2 -d'=' | tr -d ' '`"
      AC_MSG_RESULT([$MEX_ARCH])
    else
      AC_MSG_RESULT([WARNING: if configure hangs at the next step, consult the libSBML documentation])

      AC_MSG_CHECKING([for the value of MATLABROOT])
      MATLABROOT="`$MATLAB -e | grep MATLAB= | cut -f2 -d'='`"
      AC_MSG_RESULT([$MATLABROOT])

      dnl Determine MATLAB's machine architecture designation, so that
      dnl we can explicitly pass it to 'mex' later on.

      AC_MSG_CHECKING([what MATLAB calls this architecture])
      MEX_ARCH="`env MATLABROOT=$MATLABROOT $MATLABROOT/bin/matlab -e | egrep '^ARCH=' | cut -f2 -d'='`"
      AC_MSG_RESULT([$MEX_ARCH])
    fi

    dnl Obtain the file extension.  Be careful to tell 'mexext' the root of
    dnl the MATLAB installation tree, or it will not report the correct
    dnl architecture on some systems (e.g., MacOS 10.5 with R2010a).  Also,
    dnl mexext will sometimes still report the wrong thing, and must be
    dnl corrected.  For example, the R2010a Linux versions we have are
    dnl 32-bit installations, but mexext will happily report a 64-bit
    dnl extension because it doesn't bother to check whether the MATLAB
    dnl installation actually *has* the 64-bit components.

    AC_MSG_CHECKING([for the MATLAB binary file extension])
    MEXEXT="`env MATLABROOT=$MATLABROOT $MATLABROOT/bin/mexext`"
    if test "$MEX_ARCH" = "glnx86" -a "$MEXEXT" = "mexa64"; then
      AC_MSG_RESULT(["mexext" thinks it's $MEXEXT, but I say it's mexglx])
      MEXEXT="mexglx"
    else
      AC_MSG_RESULT([$MEXEXT])
    fi

    dnl Checks that the MATLAB architecture agrees with the type of
    dnl binaries we're set up to create.

    case $host in
    *darwin*) 

      dnl MacOSX 10.6 (Snow Leopard) makes 64-bit binaries by default.
      dnl MacOSX 10.5 (Leopard) makes 32-bit binaries by default.
      dnl MATLAB (2009b and possibly others) comes in both variants, and tries
      dnl to guess what kind of binary it should compile; however, it does this
      dnl evidently *not* based on the MacOSX version, but rather the machine
      dnl architecture.  So some combinations result in linkage problems unless
      dnl we explicitly tell MATLAB to compile in the right mode.

      dnl Known MATLAB architecture and binary designations, for reference:
      dnl   ARCH       MEXEXT       SYSTEM
      dnl   -------    --------     --------------------
      dnl   mac        .mexmac      PPC? (unconfirmed)
      dnl   maci       .mexmaci     Intel, 32-bit binary
      dnl   maci64     .mexmaci64   Intel, 64-bit binary

      OSX_MAJOR_VER=`uname -r | cut -f1 -d'.'`
      case ${MEX_ARCH} in
      maci64)
        if test ${OSX_MAJOR_VER} -lt 10; then
          dnl We're pre-MacOSX 10.6, which implies 32-bit binaries by default.

          AC_MSG_RESULT([Note: the MATLAB version we're finding is a 64-bit version])
          AC_MSG_CHECKING([whether 64-bit libSBML binaries are being built])
          if echo $CXXFLAGS $CPPFLAGS | grep -q "arch x86_64"; then
            AC_MSG_RESULT([yes, we are 'go' for 64-bit])
          else
            AC_MSG_RESULT([no, we have to tell mex to generate 32-bit binaries])
            MEX_ARCH=maci
            MEXEXT=mexmaci
          fi
        fi
      ;;

      maci)
        if test ${OSX_MAJOR_VER} -ge 10; then
          dnl We're on MacOSX 10.6, which makes 64-bit bins unless told not to.

          AC_MSG_RESULT([Note: the MATLAB version we're finding is a 32-bit version])
          AC_MSG_CHECKING([whether 32-bit libSBML binaries are being made])
          if echo $CXXFLAGS $CPPFLAGS | grep -q "arch i386"; then
            AC_MSG_RESULT([yes, fabulous!])
          else
            AC_MSG_RESULT([no])
            AC_MSG_ERROR([
***************************************************************************
libSBML needs to be built explicitly to include a 32-bit (i386) version, 
because your copy of MATLAB is a 32-bit version.  By default, MacOS 10.6+
(Snow Leopard) builds everything as 64-bit (x86_64) binaries.  Please add
    --enable-universal-binary="-arch i386 -arch x86_64" 
to your configure options, re-run the configure step, and recompile.  If
you get a compilation error, please check whether you have a private 
version of a dependent library (e.g., expat, libxml, or xerces) that was 
built only as a 32-bit version, and either remove, recompile or replace it
it before proceeding further.
***************************************************************************
])
          fi
        fi
        ;;
      esac
      ;;

    *linux*)
      dnl If MATLAB is installed in 32-bit form on an x86_64 system, we have
      dnl to make sure we don't try to compile only 64-bit libSBML binaries,
      dnl or else MATLAB won't be able to use them.

      dnl Known MATLAB architecture and binary designations, for reference:
      dnl   ARCH       MEXEXT       SYSTEM
      dnl   -------    --------     --------------------

      if test "${MEX_ARCH}" = "glnx86" -a "${host_cpu}" = "x86_64"; then
        AC_MSG_RESULT([Note: the MATLAB version we're finding is a 32-bit version])
        AC_MSG_CHECKING([whether 32-bit libSBML binaries are being built])
        if echo $CXXFLAGS $CPPFLAGS | grep -q "m32"; then
          AC_MSG_RESULT([yes, sweet!])
        else
          AC_MSG_RESULT([no])
          AC_MSG_ERROR([
***************************************************************************
The compilation environment, as it is currently configured, will create a
64-bit libSBML, but your copy of MATLAB is a 32-bit version.  Please re-run
the libSBML 'configure' command with the option -m32 to configure libSBML
to build in 32-bit mode, then recompile libSBML and install the result.
If you get a compilation error, please check whether you have a private 
version of a dependent library (e.g., expat, libxml, or xerces) that is
only available as a 64-bit version on your system, and either remove,
recompile or replace it it before proceeding further.
***************************************************************************
])
        fi
      elif test "${MEX_ARCH}" = "glnxa64" -a "${host_cpu}" != "x86_64"; then
        AC_MSG_RESULT([Note: the MATLAB version we're finding is a 64-bit version])
        AC_MSG_CHECKING([whether 64-bit libSBML binaries are being built])
        if echo $CXXFLAGS $CPPFLAGS | grep -q "m64"; then
          AC_MSG_RESULT([yes, we are 'go' for 64-bit])
        else
          AC_MSG_RESULT([no])
          AC_MSG_ERROR([
***************************************************************************
The compilation environment, as it is currently configured, will create a
32-bit libSBML, but your copy of MATLAB is a 64-bit version.  Please re-run
the libSBML 'configure' command with the option -m64 to configure libSBML
to build in 64-bit mode, then recompile libSBML and install the result.
If you get a compilation error, please check whether you have a private 
version of a dependent library (e.g., expat, libxml, or xerces) that is
only available as a 32-bit version on your system, and either remove,
recompile or replace it it before proceeding further.
***************************************************************************
])
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
