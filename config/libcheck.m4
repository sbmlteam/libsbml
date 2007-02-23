dnl
dnl Filename    : libcheck.m4
dnl Description : Autoconf macro to check for existence of Check library
dnl Author(s)   : SBML Team <sbml-team@caltech.edu>
dnl Organization: California Institute of Technology
dnl $Id$
dnl $Source$
dnl
dnl Portions of this file originally came from the check 0.9.5
dnl distribution.  I (Mike Hucka) made some modifications because we
dnl previously had written our own libcheck.m4 and I wanted to
dnl preserve some of the features of that one, such as the messages
dnl it printed and the extra steps it too on MacOS X.

dnl AM_PATH_CHECK([MINIMUM-VERSION, [ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND]]])
dnl Test for check, and define LIBCHECK_CPPFLAGS and LIBCHECK_LDFLAGS
dnl Minimum version can be provided.

AC_DEFUN([CONFIG_LIB_LIBCHECK],
[
  AC_ARG_WITH([check],
    AC_HELP_STRING([--with-check=PREFIX],
                   [Use the libcheck unit testing library (default=no)]),
    [with_libcheck=$withval],
    [with_libcheck=no])

  min_check_version=ifelse([$1], ,0.9.2,$1)

  AC_MSG_CHECKING(for check - version >= $min_check_version)

  libcheck_found=no

  if test $with_libcheck != no; then

    AC_LANG_PUSH(C)

    LIBCHECK_LIBS="-lcheck"

    if test $with_libcheck != yes; then
      libcheck_root="$with_libcheck"
      CONFIG_ADD_LDPATH($libcheck_root/lib)
      LIBCHECK_CPPFLAGS="-I$libcheck_root/include"
      LIBCHECK_LDFLAGS="-L$libcheck_root/lib"
    else
      dnl On the Macs, if the user has installed libcheck via Fink and they
      dnl used the default Fink install path of /sw, the following should
      dnl catch it.  We do this so that Mac users are more likely to find
      dnl success even if they only type --with-check.

      dnl This is a case statement, in case we need to do something similar
      dnl for other host types in the future.

      case $host in
      *darwin*) 
        libcheck_root="/sw"
        CONFIG_ADD_LDPATH($libcheck_root/lib)
        LIBCHECK_CPPFLAGS="-I$libcheck_root/include"
        LIBCHECK_LDFLAGS="-L$libcheck_root/lib"
	;;
      esac    

      dnl Note that CONFIG_ADD_LDPATH is deliberately not called in cases
      dnl other than the two above.
    fi

    dnl The following is grungy but I don't know how else to make 
    dnl AC_CHECK_LIB use particular library and include paths without
    dnl permanently resetting CPPFLAGS etc.

    tmp_CFLAGS="$CFLAGS"
    tmp_LDFLAGS="$LDFLAGS"
    tmp_LIBS="$LIBS"
    CFLAGS="$LIBCHECK_CPPFLAGS $CFLAGS"
    LDFLAGS="$LIBCHECK_LDFLAGS $LDFLAGS"
    LIBS="$LIBCHECK_LIBS $LIBS"

    AC_CHECK_HEADERS([check.h], [libcheck_found=yes], [libcheck_found=no])

    if test $libcheck_found = no; then
      AC_CHECK_LIB([check], [srunner_create],
        [libcheck_found=yes],
        [libcheck_found=no])
    fi

    if test $libcheck_found = no; then
      AC_MSG_ERROR([Could not find the libcheck library.])
    fi

    rm -f conf.check-test
    AC_RUN_IFELSE([AC_LANG_SOURCE([
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <check.h>

int main ()
{
  int major, minor, micro;
  char *tmp_version;

  system ("touch conf.check-test");

  /* HP/UX 9 writes to sscanf strings */
  tmp_version = strdup("$min_check_version");
  if (sscanf(tmp_version, "%d.%d.%d", &major, &minor, &micro) != 3)
    {
      printf("%s, bad version string\n", "$min_check_version");
      return 1;
    }
    
  if ((CHECK_MAJOR_VERSION != check_major_version) ||
      (CHECK_MINOR_VERSION != check_minor_version) ||
      (CHECK_MICRO_VERSION != check_micro_version))
    {
      printf("\n*** The Check header file (version %d.%d.%d) does not match\n",
	     CHECK_MAJOR_VERSION, CHECK_MINOR_VERSION, CHECK_MICRO_VERSION);
      printf("*** the Check library found (version %d.%d.%d).\n",
	     check_major_version, check_minor_version, check_micro_version);
      return 1;
    }

  if ((check_major_version > major) ||
      ((check_major_version == major) && (check_minor_version > minor)) ||
      ((check_major_version == major) && (check_minor_version == minor)
	&& (check_micro_version >= micro)))
    {
      return 0;
    }
  else
    {
      printf("\n*** An old version of Check (%d.%d.%d) was found.\n",
             check_major_version, check_minor_version, check_micro_version);
      printf("*** You need a version of Check that's at least %d.%d.%d.\n", 
	major, minor, micro);
      printf("***\n"); 
      printf("*** If you've already installed a sufficiently new version,\n");
      printf("*** this error probably means that the wrong copy of the\n");
      printf("*** Check library and header file are being found.  Re-run\n");
      printf("*** configure with the --with-check=PATH option to specify\n");
      printf("*** the prefix where the correct version is installed.\n");
    }

  return 1;
}
])], , no_check=yes)

    CFLAGS="$tmp_CFLAGS"
    LDFLAGS="$tmp_LDFLAGS"
    LIBS="$tmp_LIBS"

    if test "x$no_check" = x ; then
      ifelse([$2], , :, [$2])
    else
      if test -f conf.check-test ; then
        :
      else
        echo "*** Could not run Check test program, trying to find out why..."

	CFLAGS="$LIBCHECK_CPPFLAGS $CFLAGS"
	LDFLAGS="$LIBCHECK_LDFLAGS $LDFLAGS"
	LIBS="$LIBCHECK_LIBS $LIBS"

        AC_TRY_LINK([
#include <stdio.h>
#include <stdlib.h>
#include <check.h>
], ,  [ echo "*** The test program compiled, but did not run.  This usually"
        echo "*** means that the run-time linker is not finding libcheck.  You"
        echo "*** will need to set your LD_LIBRARY_PATH environment variable,"
        echo "*** or edit /etc/ld.so.conf to point to the installed location."
        echo "*** Also, make sure you have run ldconfig if that is required"
	echo "*** on your operating system."
	echo "***"
        echo "*** If you have an old version of Check installed, it is best"
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

      rm -f conf.check-test
      ifelse([$3], , AC_MSG_ERROR([check not found]), [$3])
    fi

    CFLAGS="$tmp_CFLAGS"
    LDFLAGS="$tmp_LDFLAGS"
    LIBS="$tmp_LIBS"

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
