dnl
dnl Filename    : java.m4
dnl Description : Autoconf macro to check for existence of Java
dnl Author(s)   : SBML Team <sbml-team@caltech.edu>
dnl Organization: California Institute of Technology
dnl Created     : 2004-07-07
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

dnl
dnl Supports --with-java[=PREFIX]
dnl

AC_DEFUN([CONFIG_PROG_JAVA],
[
  AC_ARG_WITH([java],
    AC_HELP_STRING([--with-java=PREFIX],
                   [generate Java interface library [[default=no]]]),
    [with_java=$withval],
    [with_java=no])

  AC_ARG_WITH(java-bin-check,
    AC_HELP_STRING([--without-jre-check], [disable checking for 32 vs. 64-bit JRE]),
      	  [  if test "x$withval" = "xno"; then
  		bin_check_nonfatal=1
  	     fi
  	  ]
    )

  if test "$with_java" != "no"; then

    if test "$with_java" != "yes"; then
      dnl Users seems to have supplied a prefix directory path.  See if we can 
      dnl find Java somewhere in the given tree.

      dnl 1st remove trailing slashes because it can confuse tests below.

      with_java=`echo $with_java | sed -e 's,\(.*\)/$,\1,g'`

      AC_PATH_PROG([JAVA], [java], [no], [$with_java/bin])
      AC_PATH_PROG([JAVAC], [javac], [no], [$with_java/bin])
      AC_PATH_PROG([JAR], [jar], [no], [$with_java/bin])
    else
      dnl No prefix directory path supplied for --with-java.  Use defaults.

      AC_PATH_PROG([JAVA], [java])
      AC_PATH_PROG([JAVAC], [javac])
      AC_PATH_PROG([JAR], [jar])
    fi

    dnl Sanity checks.

    if test -z "$JAVA" -o "$JAVA" = "no"; then
      AC_MSG_ERROR([Could not find `java' executable.])
    fi    
    if test -z "$JAVAC" -o "$JAVAC" = "no"; then
      AC_MSG_ERROR([Could not find `javac' executable.])
    fi    

    dnl Check version if required.

    m4_ifvaln([$1], [
      AC_MSG_CHECKING($JAVA version >= $1)

      changequote(<<, >>)

      rx=`echo $1 | sed -e 's/\([0-9]*\)\.\([0-9]*\)\.\([0-9]*\).*/\1/'`
      ry=`echo $1 | sed -e 's/\([0-9]*\)\.\([0-9]*\)\.\([0-9]*\).*/\2/'`
      rz=`echo $1 | sed -e 's/\([0-9]*\)\.\([0-9]*\)\.\([0-9]*\).*/\3/'`

      version=`"$JAVA" -version 2>&1 | sed -e 's/\(.*\)$/\1/;q'`

      jx=`echo $version | sed -e 's/java version \"\([0-9]*\)\.\([0-9]*\)\.\([0-9]*\).*/\1/'`
      jy=`echo $version | sed -e 's/java version \"\([0-9]*\)\.\([0-9]*\)\.\([0-9]*\).*/\2/'`
      jz=`echo $version | sed -e 's/java version \"\([0-9]*\)\.\([0-9]*\)\.\([0-9]*\).*/\3/'`

      changequote([, ])

      if test $jx -gt $rx \
         || (test $jx -eq $rx -a $jy -gt $ry) \
         || (test $jx -eq $rx -a $jy -eq $ry -a $jz -ge $rz); then
        AC_MSG_RESULT(yes (found $jx.$jy.$jz))
      else
        AC_MSG_RESULT(no)
        AC_MSG_ERROR([Need Java version $1, but only found version $jx.$jy.$jz.])
      fi

      dnl Retain these for use below.

      JAVA_VER_MINOR=$jy
      JAVA_VER_SUBMINOR=$jz

    ])

    dnl Look for the path to the include files for Java.

    dnl On some systems, what we first get as $JAVA or $JAVAC is a symlink.
    dnl We need to get to the real directory first.
    dnl (However, only do this if the user didn't request a specific path.)

    if test "$with_java" = "yes"; then
      while test -h "$JAVAC"; do
        dnl
        dnl This next case case statement does 2 things:
        dnl 1) On Ubuntu and maybe some other systems, following the symlinks
        dnl    gets us to /usr/bin/ecj, which is a shell script that invokes
        dnl    the actual executable.  This stumps our code trying to find
        dnl    the jni include directory.  The weak solution here is to
        dnl    detect when /usr/lib/jvm/* appears in the path at
        dnl    some point, which is the actual home under Ubuntu (and
        dnl    hopefully others) containing files we're looking for.
        dnl
        dnl 2) $javaclink may end up being a relative path (e.g. 
        dnl    "/usr/bin/java" -> "../java/bin/java" on Solaris).  So, the
        dnl    value of javaclink must be appended to $JAVAC to construct an
        dnl    absolute path.
        dnl

        dnl "/usr/lib/jvm/java-gcj/bin/javac" might be initially set to JAVAC.
        case "$JAVAC" in
          /usr/lib/jvm/*)
            break
            ;;
          *) 
            javaclink=`readlink "$JAVAC"`
            ;;
        esac

        case "$javaclink" in
          /*) 
            JAVAC="$javaclink"
            ;;
          *) 
            dnl 'X' avoids triggering unwanted echo options.
            JAVAC=`echo "X${JAVAC}" | sed -e 's/^X//' -e 's:[[^/]]*$::'`"$javaclink"
            ;;
        esac
      done
    fi

    dnl Need to find the include files.

    case "$host" in
      *darwin*)
        framework="/System/Library/Frameworks/JavaVM.framework"
        case $JAVA_VER_MINOR in
          6) 
            if test -e "$framework/Versions/CurrentJDK/Headers"; then
              headers="$framework/Versions/CurrentJDK/Headers"
            elif test -e "$framework/Versions/1.6.0/Headers"; then
              headers="$framework/Versions/1.6.0/Headers"
            elif test -e "$framework/Versions/1.6/Headers"; then
              headers="$framework/Versions/1.6/Headers"
            fi
            ;;
          5) 
            if test -e "$framework/Versions/CurrentJDK/Headers"; then
              headers="$framework/Versions/CurrentJDK/Headers"
            elif test -e "$framework/Versions/1.5.0/Headers"; then
              headers="$framework/Versions/1.5.0/Headers"
            elif test -e "$framework/Versions/1.5/Headers"; then
              headers="$framework/Versions/1.5/Headers"
            fi
            ;;
          4)
            dnl MacOS X's installation of Java 1.4.2 is broken: the header
            dnl files are missing entirely, and there are dangling symlinks.
            dnl The following are ugly kludges to try to do the best we can.
            dnl One of the things this does deliberately is use the 1.4.1
            dnl headers directory on the Mac even for Java 1.4.2 if the 
            dnl 1.4.2 headers directory can't be found.  Yuck.
            dnl 2004-07-07 <mhucka@caltech.edu>
            if test $JAVA_VER_SUBMINOR -eq 2; then
              if test -e "$framework/Versions/1.4.2/Headers"; then
                headers="$framework/Versions/1.4.2/Headers"
                elif test -e "$framework/Versions/1.4.1/Headers"; then
                headers="$framework/Versions/1.4.1/Headers"
              fi;
            elif test $JAVA_VER_SUBMINOR -eq 1; then
              if test -e "$framework/Versions/1.4.1/Headers"; then
                headers="$framework/Versions/1.4.1/Headers"
              fi;
            fi
            ;;
        esac

        if test -e "$headers/jni.h"; then
          JAVA_CPPFLAGS="$JAVA_CPPFLAGS -I\"$headers\""
          parent=`dirname "$headers"`
          JAVADOC_JAR="$parent/Classes/classes.jar"
        else
          AC_MSG_ERROR([Cannot find Java include files.])
        fi
      ;;

      *) 
        parent=`dirname "$JAVAC"`

        if test -e "$parent/include/jni.h"; then
          headers="${parent}/include"
          JAVA_CPPFLAGS="$JAVA_CPPFLAGS -I\"${headers}\""
        else

          dnl The ${parent} directory seems to be "bin" directory (e.g. "/usr/local/bin").
          dnl So, the parent's parent directory is checked below. 

          parent=`dirname "${parent}"`

          if test -e "${parent}/include/jni.h"; then
            headers="${parent}/include"
            JAVA_CPPFLAGS="$JAVA_CPPFLAGS -I\"${headers}\""
          else
            AC_MSG_ERROR([Cannot find Java include files.])
          fi
        fi

	dnl Some platforms have additional system-specific include dirs.

        case "$host_os" in
          *cygwin*) JAVA_CPPFLAGS="$JAVA_CPPFLAGS -I\"$parent/include/win32\"";;
          freebsd*) JAVA_CPPFLAGS="$JAVA_CPPFLAGS -I\"$parent/include/freebsd\"";;
          linux*)   JAVA_CPPFLAGS="$JAVA_CPPFLAGS -I\"$parent/include/linux\"";;
          solaris*) JAVA_CPPFLAGS="$JAVA_CPPFLAGS -I\"$parent/include/solaris\"";;
        esac
	    
        JAVADOC_JAR="$parent/lib/tools.jar"    
      ;;
    esac

    dnl On MacOS X, JNI libraries loaded by Java need to be created as bundles.

    case $host in
    *darwin*) 
	JAVA_LDFLAGS="${JAVA_LDFLAGS} -bundle_loader \"${JAVA}\""
	;;
    *)
	JAVA_LDFLAGS=
	;;
    esac

    dnl On MacOS X, the JNI library needs an oddball extension.

    case $host in
    *darwin*) 
	JNIEXT="jnilib"
	JNIBASENAME="libsbmlj"
	;;
    *cygwin*)
	JNIEXT="dll"
	JNIBASENAME="sbmlj"
	;;
    *)
	JNIEXT="so"
	JNIBASENAME="libsbmlj"
	;;
    esac

    if test -z "$headers"; then
      AC_MSG_ERROR([Cannot find Java include files.])
    fi

    dnl
    dnl enable --with-swig option if SWIG-generated files of Java bindings 
    dnl (libsbml_wrap.cpp and java-files/**/*.java) need to be regenerated.
    dnl 

    java_dir="src/bindings/java"

    if test "$with_swig" = "no" -o -z "$with_swig" ; then
      AC_MSG_CHECKING(whether SWIG is required for Java bindings.)
      if test ! -e "${java_dir}/libsbml_wrap.cpp" -o ! -d "${java_dir}/java-files" ; then
        with_swig="yes"
        AC_MSG_RESULT(yes)
      else
        if test "$enable_layout" = "no"; then
          if grep -q getListOfLayouts "${java_dir}/libsbml_wrap.cpp"; then
            with_swig="yes"
            AC_MSG_RESULT(yes)
          else
            AC_MSG_RESULT(no)
          fi
        else
          if grep -q getListOfLayouts "${java_dir}/libsbml_wrap.cpp"; then
            AC_MSG_RESULT(no)
          else
            with_swig="yes"
            AC_MSG_RESULT(yes)
          fi
        fi
      fi
    fi

    dnl
    dnl Check whether the JRE matches possible 32/64-bit options given
    dnl during configuration (such as telling configure --enable-m64 but
    dnl using a copy of Java that's only 32-bit).
    dnl

    if test -z "$bin_check_nonfatal"; then
      if test ${host_cpu} = "x86_64" -o -n "`echo $CFLAGS $CXXFLAGS | grep 'm64'`"; then
        AC_MSG_CHECKING(whether JRE is a 64-bit version)
        rm -f config/printJavaDataModel.class
        $JAVAC config/printJavaDataModel.java
        if test "`(cd config; $JAVA printJavaDataModel)`" = "64"; then
           AC_MSG_RESULT(yes, good news!)
        else
           AC_MSG_RESULT(no)
           AC_MSG_ERROR([
***************************************************************************
Either this platform is a 64-bit system, or configure has been invoked with
the option --enable-m64, yet the Java run-time interpreter found at 
  $JAVA
reports being a 32-bit interpreter.  This will result in JNI binaries 
that cannot be executed with that given interpreter.  Please reconfigure
libSBML using an argument to --with-java that refers to the location of a
64-bit Java environment.  If you are certain that you want to proceed
anyway, add --without-java-bin-check to the options for configure.
***************************************************************************
])
        fi
      fi
    fi

    dnl 
    dnl Finally, set up our variables.
    dnl 

    AC_DEFINE([USE_JAVA], 1, [Define to 1 to use Java])
    AC_SUBST(USE_JAVA, 1)

    AC_SUBST(JAVA_CPPFLAGS)
    AC_SUBST(JAVA_LDFLAGS)
    AC_SUBST(JNIEXT)
    AC_SUBST(JNIBASENAME)
    AC_SUBST(JAVADOC_JAR)

  fi

  dnl We record the USE_XXX flag, for later testing in Makefiles.

  LIBSBML_OPTIONS="$LIBSBML_OPTIONS USE_JAVA"

])
