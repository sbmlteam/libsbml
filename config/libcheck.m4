dnl
dnl Filename    : libcheck.m4
dnl Description : Autoconf macro to check for existence of libcheck library
dnl Author(s)   : Ben Bornstein
dnl Organization: Machine Learning Systems Group, JPL
dnl Created     : 2004-02-18
dnl Revision    : $Id$
dnl Source      : $Source$
dnl
dnl Copyright 2004 California Institute of Technology.  ALL RIGHTS RESERVED.
dnl U.S. Government Sponsorship acknowledged.
dnl


dnl
dnl Check --with-check[=PREFIX] is specified and libcheck is installed.
dnl

AC_DEFUN([CONFIG_LIB_LIBCHECK],
[
  AC_ARG_WITH([check],
    AC_HELP_STRING([--with-check=PREFIX],
                   [Build the test suite using libcheck (default=no)]),
    [with_libcheck=$withval],
    [with_libcheck=no])

  libcheck_found=no

  if test $with_libcheck != no; then

    AC_LANG_PUSH(C)

    if test $with_libcheck != yes; then
      libcheck_root="$with_libcheck"
      CONFIG_ADD_LDPATH($libcheck_root/lib)
    else
      dnl On the Macs, if the user has installed libcheck via Fink and they
      dnl used the default Fink install path of /sw, the following should
      dnl catch it.  We do this so that Mac users are more likely to find
      dnl success even if they only type --with-check.

      case $host in
      *darwin*) 
        libcheck_root="/sw"
        CONFIG_ADD_LDPATH($libcheck_root/lib)
	;;
      esac    

    fi

    LIBCHECK_CPPFLAGS="-I$with_libcheck/include"
    LIBCHECK_LDFLAGS="-L$with_libcheck/lib"
    LIBCHECK_LIBS="-lcheck"

    dnl The following is grungy but I don't know how else to make 
    dnl AC_CHECK_LIB use particular library and include paths without
    dnl permanently resetting CPPFLAGS etc.

    tmp_CPPFLAGS=$CPPFLAGS
    tmp_LDFLAGS=$LDFLAGS
    CPPFLAGS="$LIBCHECK_CPPFLAGS $CPPFLAGS"
    LDFLAGS="$LIBCHECK_LDFLAGS $LDFLAGS"

    AC_CHECK_HEADERS([check.h], [libcheck_found=yes], [libcheck_found=no])

    if test $libcheck_found = no; then
      AC_CHECK_LIB([check], [srunner_create],
        [libcheck_found=yes],
        [libcheck_found=no])
    fi

    if test $libcheck_found = no; then
      AC_MSG_ERROR([Could not find the libcheck library.])
    fi

    CPPFLAGS=$tmp_CPPFLAGS
    LDFLAGS=$tmp_LDFLAGS
    LIBS=$tmp_LIBS

    AC_LANG_POP(C)

    AC_DEFINE([USE_LIBCHECK], 1, [Define to 1 to use the check library])
    AC_SUBST(USE_LIBCHECK, 1)

    AC_SUBST(LIBCHECK_CPPFLAGS)
    AC_SUBST(LIBCHECK_LDFLAGS)
    AC_SUBST(LIBCHECK_LIBS)

  fi

  dnl We record the USE_XXX flag, for later testing in Makefiles.

  LIBSBML_OPTIONS="$LIBSBML_OPTIONS USE_LIBCHECK"

])
