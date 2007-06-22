dnl
dnl @file    expat.m4
dnl @brief   Autoconf macro to check for existence of Expat library
dnl @author  Stephan Hoops
dnl @author  Ben Bornstein
dnl @author  Mike hucka
dnl
dnl $Id$
dnl $Source$
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

dnl Invoke as CONFIG_LIB_EXPAT or CONFIG_LIB_EXPAT(MIN-VERSION).
dnl Checks if --with-expat[=PREFIX] is specified.
dnl Default minimum version is 1.95.8 because that's the minimum LibSBML needs.

AC_DEFUN([CONFIG_LIB_EXPAT],
[
  AC_ARG_WITH([expat],
    AC_HELP_STRING([--with-expat=PREFIX],
                   [Use Expat XML Library [[default=no]]]),
    [with_expat=$withval],
    [with_expat=no])

  if test $with_expat != no; then

    AC_LANG_PUSH(C)

    EXPAT_LIBS="-lexpat"

    if test $with_expat != yes; then
      expat_root="$with_expat"
      CONFIG_ADD_LDPATH($expat_root/lib)

      EXPAT_CPPFLAGS="-I$expat_root/include"
      EXPAT_LDFLAGS="-L$expat_root/lib"
    else
      dnl On the Macs, if the user has installed expat via Fink and they
      dnl used the default Fink install path of /sw, the following should
      dnl catch it.  We do this so that Mac users are more likely to find
      dnl success even if they only type --with-expat.

      dnl This is a case statement in case we need to do something similar
      dnl for other host types in the future.

      case $host in
      *darwin*) 
        if test -e "/sw"; then
          expat_root="/sw"
          CONFIG_ADD_LDPATH($expat_root/lib)
          EXPAT_CPPFLAGS="-I$expat_root/include"
          EXPAT_LDFLAGS="-L$expat_root/lib"
        fi
	;;
      esac    

      dnl Note that CONFIG_ADD_LDPATH is deliberately not called in cases
      dnl other than the two above.
    fi

    dnl The following is grungy but I don't know how else to make 
    dnl AC_CHECK_LIB use particular library and include paths without
    dnl permanently resetting CPPFLAGS etc.

    tmp_CPPFLAGS=$CPPFLAGS
    tmp_LDFLAGS=$LDFLAGS
    tmp_LIBS="$LIBS"
    CPPFLAGS="$CPPFLAGS $EXPAT_CPPFLAGS"
    LDFLAGS="$LDFLAGS $EXPAT_LDFLAGS"
    LIBS="$LIBS $EXPAT_LIBS"

    expat_found=no

    AC_CHECK_LIB(expat, XML_ParserCreate, [expat_found=yes], [expat_found=no])

    if test $expat_found = no; then
      AC_MSG_ERROR([Could not find the Expat XML library.])
    fi       

    AC_CHECK_HEADERS(expat.h, [expat_found=yes], [expat_found=no])

    if test $expat_found = no; then
      AC_MSG_ERROR([Could not find the file expat.h.])
    fi

    min_expat_version=ifelse([$1], ,1.95.8,$1)
    AC_MSG_CHECKING(for Expat version >= $min_expat_version)

    dnl The next bit of code is based on libcheck.m4, which in turn
    dnl was stolen from someone else....
    rm -f conf.expat-test
    AC_RUN_IFELSE([AC_LANG_SOURCE([
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <expat.h>

int main ()
{
  int major, minor, micro;
  char *tmp_version;
  XML_Expat_Version expat_version = XML_ExpatVersionInfo();

  system ("touch conf.expat-test");

  /* HP/UX 9 writes to sscanf strings */
  tmp_version = strdup("$min_expat_version");
  if (sscanf(tmp_version, "%d.%d.%d", &major, &minor, &micro) != 3)
    {
      printf("%s, bad version string\n", "$min_expat_version");
      return 1;
    }
    
  if ((XML_MAJOR_VERSION != expat_version.major) ||
      (XML_MINOR_VERSION != expat_version.minor) ||
      (XML_MICRO_VERSION != expat_version.micro))
    {
      printf("\n*** The Expat header file (version %d.%d.%d) does not match\n",
             XML_MAJOR_VERSION, XML_MINOR_VERSION, XML_MICRO_VERSION);
      printf("*** the Expat library found (version %d.%d.%d).\n",
             expat_version.major, expat_version.minor, expat_version.micro);
      return 1;
    }

  if ((expat_version.major > major) ||
      ((expat_version.major == major) && (expat_version.minor > minor)) ||
      ((expat_version.major == major) && (expat_version.minor == minor)
        && (expat_version.micro >= micro)))
    {
      return 0;
    }
  else
    {
      printf("\n*** An old version of Expat (%d.%d.%d) was found.\n",
             expat_version.major, expat_version.minor, expat_version.micro);
      printf("*** You need a version of Expat that's at least %d.%d.%d.\n", 
	     major, minor, micro);
      printf("***\n"); 
      printf("*** If you've already installed a sufficiently new version,\n");
      printf("*** this error probably means that the wrong copy of the\n");
      printf("*** Expat library and header file are being found.  Re-run\n");
      printf("*** configure with the --with-expat=PATH option to specify\n");
      printf("*** the prefix where the correct version is installed.\n");
    }

  return 1;
}
])], AC_MSG_RESULT(yes), no_expat=yes)

    CFLAGS="$tmp_CFLAGS"
    LDFLAGS="$tmp_LDFLAGS"
    LIBS="$tmp_LIBS"

    if test "x$no_expat" = x ; then
      ifelse([$2], , :, [$2])
    else
      if test -f conf.expat-test ; then
	:
      else
        echo "*** Could not run Expat test program, trying to find out why..."

        CFLAGS="$EXPAT_CPPFLAGS $CFLAGS"
        LDFLAGS="$EXPAT_LDFLAGS $LDFLAGS"
        LIBS="$EXPAT_LIBS $LIBS"

        AC_TRY_LINK([
#include <stdio.h>
#include <stdlib.h>
#include <expat.h>
], ,    [ echo "*** The test program compiled, but did not run.  This usually"
          echo "*** means that the run-time linker is not finding expat.  You"
          echo "*** will need to set your LD_LIBRARY_PATH environment variable,"
          echo "*** or edit /etc/ld.so.conf to point to the installed location."
          echo "*** Also, make sure you have run ldconfig if that is required"
          echo "*** on your operating system."
          echo "***"
          echo "*** If you have an old version of Expat installed, it is best"
          echo "*** to remove it, although you may also be able to get things"
          echo "*** to work by modifying you value of LD_LIBRARY_PATH."],
  
        [ echo "*** The test program failed to compile or link. See the file"
          echo "*** 'config.log' for more information about what happened." ])
        
        CFLAGS="$tmp_CFLAGS"
        LDFLAGS="$tmp_LDFLAGS"
        LIBS="$tmp_LIBS"
  
      fi

      tmp_CFLAGS=""
      tmp_LDFLAGS=""
      tmp_LIBS=""

      rm -f conf.expat-test
      ifelse([$3], , AC_MSG_ERROR([Expat not found]), [$3])
    fi

    rm -f conf.expat-test

    CPPFLAGS=$tmp_CPPFLAGS
    LDFLAGS=$tmp_LDFLAGS
    LIBS="$tmp_LIBS"

    AC_LANG_POP(C)

    AC_DEFINE([USE_EXPAT], 1, [Define to 1 to use the Expat XML library])
    AC_SUBST(USE_EXPAT, 1)

    AC_SUBST(EXPAT_CPPFLAGS)
    AC_SUBST(EXPAT_LDFLAGS)
    AC_SUBST(EXPAT_LIBS)

  fi

  if test $with_expat != no; then
    with_xerces=no
  fi

  dnl We record the USE_XXX flag, for later testing in Makefiles.

  LIBSBML_OPTIONS="$LIBSBML_OPTIONS USE_EXPAT"

])
