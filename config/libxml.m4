dnl Filename    : libxml.m4
dnl Description : Autoconf macro to check for existence of libxml library
dnl Created     : 2007-02-22
dnl Revision    : $Id$
dnl Source      : $Source$

dnl Portions of this file were taken from the file "libxml.m4" provided
dnl with the libxml2 version 2.6.27 distribution.  Here are the headers
dnl from that file:
dnl 
dnl    Configure paths for LIBXML2
dnl    Mike Hommey 2004-06-19
dnl    use CPPFLAGS instead of CFLAGS
dnl    Toshio Kuratomi 2001-04-21
dnl    Adapted from:
dnl    Configure paths for GLIB
dnl    Owen Taylor     97-11-3

dnl Modifications include changing the configure flag to be "--with-libxml"
dnl to better distinguish the library in the libSBML configure options,
dnl also changing the name of the macro to CONFIG_LIB_LIBXML

dnl CONFIG_LIB_LIBXML([MIN-VERSION, [ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND]]])
dnl Test for XML, and define LIBXML_CPPFLAGS and LIBXML_LIBS

AC_DEFUN([CONFIG_LIB_LIBXML],
[ 
  AC_ARG_WITH([libxml],
    AC_HELP_STRING([--with-libxml=PREFIX],
                   [Use the libxml2 XML library [[default=no]]]),
            [with_libxml="$withval"], [with_libxml=yes])

  if test "$with_expat" != "no" -o "$with_xerces" != "no"; then
    with_libxml=no
  fi

  if test "$with_libxml" != "no"; then

    if test "$with_libxml" != "yes"; then
      xml_config_args="$xml_config_args --prefix=$with_libxml"
      if test -n "$XML2_CONFIG" ; then
	dnl The XML2_CONFIG environment variable is set, so use that.
        XML2_CONFIG=$with_libxml/bin/xml2-config
      else
        dnl The XML2_CONFIG environment variable is not set.  Look for
	dnl xml2-config in the path given by $with_libxml
        AC_PATH_PROG(XML2_CONFIG, xml2-config, no, [$with_libxml/bin])
      fi
    else
      dnl User did not supply a path with the flag.
      if test x${XML2_CONFIG+set} != xset ; then
        dnl User did not set XML2_CONFIG either.  Try their default path.
        AC_PATH_PROG(XML2_CONFIG, xml2-config, no)
      fi
    fi

    ac_save_CPPFLAGS="$CPPFLAGS"
    ac_save_LIBS="$LIBS"

    if test "$XML2_CONFIG" = "no"; then
      echo "*** Could not find 'xml2-config' in directory $with_libxml/bin/."
      echo "*** Please check that the PATH supplied to --with-libxml=PATH"
      echo "*** is of the form '/usr/local' and not '/usr/local/lib'; in"
      echo "*** other words, omit the 'lib' part of the name.  The 'configure'"
      echo "*** utility will append 'lib' to the given path automatically."
      AC_MSG_ERROR([could not find xml2-config in $with_libxml/bin])
    fi

    min_xml_version=ifelse([$1], ,2.0.0,[$1])
    AC_MSG_CHECKING(for libxml2 - version >= $min_xml_version)
    no_xml=""
    if test "$XML2_CONFIG" = "no" ; then
      no_xml=yes
    else
      LIBXML_CPPFLAGS=`$XML2_CONFIG $xml_config_args --cflags`
      LIBXML_LIBS=`$XML2_CONFIG $xml_config_args --libs`
      ver=`$XML2_CONFIG $xml_config_args --version`
      xml_config_major_version=`echo $ver | sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\1/'`
      xml_config_minor_version=`echo $ver | sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\2/'`
      xml_config_micro_version=`echo $ver | sed 's/\([[0-9]]*\).\([[0-9]]*\).\([[0-9]]*\)/\3/'`

      CPPFLAGS="$CPPFLAGS $LIBXML_CPPFLAGS"
      LIBS="$LIBXML_LIBS $LIBS"

      dnl Now check if the installed libxml is sufficiently new.
      dnl (Also sanity checks the results of xml2-config to some extent)

      rm -f conf.xmltest
      AC_TRY_RUN([
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <libxml/xmlversion.h>

int 
main()
{
  int xml_major_version, xml_minor_version, xml_micro_version;
  int major, minor, micro;
  char *tmp_version;

  system("touch conf.xmltest");

  /* Capture xml2-config output via autoconf/configure variables */

  /* HP/UX 9 writes to sscanf strings */
  tmp_version = (char *)strdup("$min_xml_version");
  if (sscanf(tmp_version, "%d.%d.%d", &major, &minor, &micro) != 3) {
     printf("%s, bad version string from xml2-config\n", "$min_xml_version");
     exit(1);
   }
   free(tmp_version);

   /* Capture the version information from the header files */
   tmp_version = (char *)strdup(LIBXML_DOTTED_VERSION);
   if (sscanf(tmp_version, "%d.%d.%d", &xml_major_version,
              &xml_minor_version, &xml_micro_version) != 3) {
     printf("%s, bad version string from libxml includes\n", 
            "LIBXML_DOTTED_VERSION");
     exit(1);
   }
   free(tmp_version);

  /* Compare xml2-config output to the libxml headers */

  if ((xml_major_version != $xml_config_major_version) ||
      (xml_minor_version != $xml_config_minor_version) ||
      (xml_micro_version != $xml_config_micro_version))
    {
      printf("*** libxml header files (version %d.%d.%d) do not match\n",
             xml_major_version, xml_minor_version, xml_micro_version);
      printf("*** xml2-config (version %d.%d.%d)\n",
             $xml_config_major_version, $xml_config_minor_version,
             $xml_config_micro_version);
      return 1;
    } 

  /* Compare the headers to the library to make sure we match */

  /* Less than ideal -- doesn't provide us with return value feedback, 
   * only exits if there's a serious mismatch between header and library.
   */
    LIBXML_TEST_VERSION;

    /* Test that the library is greater than our minimum version */
    if ((xml_major_version > major) ||
        ((xml_major_version == major) && (xml_minor_version > minor)) ||
        ((xml_major_version == major) && (xml_minor_version == minor) &&
        (xml_micro_version >= micro)))
      {
        return 0;
       }
     else
      {
        printf("\n*** An old version of libxml2 (%d.%d.%d) was found.\n",
               xml_major_version, xml_minor_version, xml_micro_version);
        printf("*** You need a version of libxml2 newer than %d.%d.%d.\n",
               major, minor, micro);
        printf("*** The latest version of libxml2 is always available\n");
        printf("*** from ftp://ftp.xmlsoft.org.\n");
        printf("***\n");
        printf("*** If you've already installed a sufficiently new version,\n");
        printf("*** this error probably means that the wrong copy of the\n");
        printf("*** xml2-config shell script is being found.  The easiest\n");
        printf("*** way to fix this is to remove the old version of\n");
        printf("*** libxml2, but you can also set the environment variable\n");
        printf("*** XML2_CONFIG to the path of the correct copy of\n");
        printf("*** xml2-config.  (In this case, you will have to modify\n");
        printf("*** your LD_LIBRARY_PATH or DYLD_LIBRARY_PATH environment\n");
        printf("*** variable, or edit /etc/ld.so.conf so that the correct\n");
        printf("*** copy of the library is found at run-time.\n");
    }
  return 1;
}
],, no_xml=yes,[echo $ac_n "cross compiling; assumed OK... $ac_c"])
      CPPFLAGS="$ac_save_CPPFLAGS"
      LIBS="$ac_save_LIBS"
    fi

    if test "x$no_xml" = x ; then
      AC_MSG_RESULT(yes (version $xml_config_major_version.$xml_config_minor_version.$xml_config_micro_version))
      ifelse([$2], , :, [$2])     
    else
      if test "$XML2_CONFIG" = "no" ; then
        echo "*** The xml2-config script installed by LIBXML could not be found"
        echo "*** If libxml was installed in PREFIX, make sure PREFIX/bin is in"
        echo "*** your path, or set the XML2_CONFIG environment variable to the"
        echo "*** full path to xml2-config."
      else
        if test -f conf.xmltest ; then
          AC_MSG_ERROR(installed version of libxml2 is too old!)	
        else
          echo "*** Could not run libxml test program, checking why..."
          CPPFLAGS="$CPPFLAGS $LIBXML_CPPFLAGS"
          LIBS="$LIBS $LIBXML_LIBS"
          AC_TRY_LINK([
#include <libxml/xmlversion.h>
#include <stdio.h>
],      [ LIBXML_TEST_VERSION; return 0;],
        [ echo "*** The test program compiled, but did not run.  This usually"
          echo "*** means that the run-time linker is not finding libxml2.  You"
          echo "*** will need to set your LD_LIBRARY_PATH or DYLD_LIBRARY_PATH"
          echo "*** environment variable, or edit /etc/ld.so.conf to point to"
          echo "*** the installed location.  Also, make sure you have run"
          echo "*** ldconfig if that is required on your operating system."
          echo "***"
          echo "*** If you have an old version of libxml2 installed, it is best"
          echo "*** to remove it, although you may also be able to get things"
          echo "*** to work by modifying you value of LD_LIBRARY_PATH."],

        [ echo "*** The test program failed to compile or link. See the file"
          echo "*** config.log for the exact error that occured. This"
          echo "*** usually means LIBXML was incorrectly installed or that"
          echo "*** you have moved LIBXML since it was installed. In the"
          echo "*** latter case, you may want to edit the xml2-config"
          echo "*** script located in $XML2_CONFIG" ])

        fi
      fi

      LIBXML_CPPFLAGS=""
      LIBXML_LIBS=""
      ifelse([$3], , :, [$3])
    fi

    CPPFLAGS="$ac_save_CPPFLAGS"
    LIBS="$ac_save_LIBS"

    AC_DEFINE([USE_LIBXML], 1, [Define to 1 to use the libxml2 XML library])
    AC_SUBST(USE_LIBXML, 1)

    AC_SUBST(LIBXML_CPPFLAGS)
    AC_SUBST(LIBXML_LDFLAGS)
    AC_SUBST(LIBXML_LIBS)
    rm -f conf.xmltest

    dnl One more wrinkle: libxml 2.6.16 as shipped by Apple is broken, in
    dnl that it doesn't report XML_NS_ERR_UNDEFINED_NAMESPACE when the
    dnl condition arises.  Let's note the situation and conditionalize the
    dnl code where necessary.

    if test $HOST_TYPE == "darwin" \
	 -a $XML2_CONFIG == "/usr/bin/xml2-config" \
	 -a `$XML2_CONFIG --libtool-libs` == "/usr/lib/libxml2.la" \
	 -a $xml_config_major_version -eq 2 \
	 -a $xml_config_minor_version -eq 6 \
	 -a $xml_config_micro_version -eq 16; then
      AC_SUBST(BUGGY_APPLE_LIBXML, 1)
    fi

  fi

])
