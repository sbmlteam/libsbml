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

AC_DEFUN(AC_LIB_LIBCHECK,
[
  libcheck_found=no

  if test $with_libcheck != no; then

    LIBCHECK_CPPFLAGS=
    LIBCHECK_LDFLAGS=

    if test $with_libcheck != yes; then
      LIBCHECK_CPPFLAGS="-I$with_libcheck/include"
      LIBCHECK_LDFLAGS="-L$with_libcheck/lib"
    fi

    AC_LANG_SAVE
    AC_LANG_C

    CPPFLAGS="$LIBCHECK_CPPFLAGS $CPPFLAGS"
    LDFLAGS="$LIBCHECK_LDFLAGS $LDFLAGS"

    AC_CHECK_HEADERS([check.h], [libcheck_found=yes], [libcheck_found=no])

    if test $libcheck_found = yes; then
      AC_CHECK_LIB([check], [srunner_create],
        [libcheck_found=yes],
        [libcheck_found=no])
    fi

    AC_LANG_RESTORE

    if test $libcheck_found = yes; then
      LIBS="$LIBS -lcheck"
    else 
      AC_MSG_ERROR([Could not find the libcheck library.])
    fi
  fi
])
