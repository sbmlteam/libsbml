dnl
dnl Filename    : java.m4
dnl Description : Autoconf macro to check for existence of Java
dnl Author(s)   : SBML Team <sbml-team@caltech.edu>
dnl Organization: California Institute of Technology
dnl Created     : 2004-07-07
dnl Revision    : $Id$
dnl Source      : $Source$
dnl
dnl <!-------------------------------------------------------------------------
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
dnl --------------------------------------------------------------------- -->*/

AC_DEFUN([CONFIG_PROG_JAVA],
[
  AC_ARG_WITH([java],
    AC_HELP_STRING([--with-java=PREFIX],
                   [Generate Java interface library [[default=no]]]),
    [with_java=$withval],
    [with_java=no])

  if test $with_java != no; then

    if test $with_java != yes;
    then
      dnl Remove needless trailing slashes because it can confuse tests later.
      with_java=`echo $with_java | sed -e 's,\(.*\)/$,\1,g'`

      dnl Users seems to have supplied a prefix directory path.  See if we can 
      dnl find Java somewhere in the given tree.

      AC_PATH_PROG([JAVA], [java], [$with_java/bin/java],
                     [no-java-found], [$with_java/bin])
      AC_PATH_PROG([JAVAC], [javac], [$with_java/bin/javac],
                     [no-javac-found], [$with_java/bin])
      AC_PATH_PROG([JAR], [jar], [$with_java/bin/jar],
                     [no-jar-found], [$with_java/bin])
    else
      AC_PATH_PROG([JAVA], [java])
      AC_PATH_PROG([JAVAC], [javac])
      AC_PATH_PROG([JAR], [jar])
    fi

    dnl Sanity checks.

    if test -z $JAVA; then
      AC_MSG_ERROR([Could not find `java' executable.])
    fi    
    if test -z $JAVAC; then
      AC_MSG_ERROR([Could not find `javac' executable.])
    fi    

    dnl Check version if required.

    m4_ifvaln([$1], [
        AC_MSG_CHECKING($JAVA version >= $1)
        changequote(<<, >>)
	rx=`echo $1 | sed -e 's/\([0-9]\)\.\([0-9]\)\.\([0-9]\).*/\1/'`
	ry=`echo $1 | sed -e 's/\([0-9]\)\.\([0-9]\)\.\([0-9]\).*/\2/'`
	rz=`echo $1 | sed -e 's/\([0-9]\)\.\([0-9]\)\.\([0-9]\).*/\3/'`
	version=`"$JAVA" -version 2>&1 | sed -e 's/\(.*\)$/\1/;q'`
	jx=`echo $version | sed -e 's/java version \"\([0-9]\)\.\([0-9]\)\.\([0-9]\).*/\1/'`
	jy=`echo $version | sed -e 's/java version \"\([0-9]\)\.\([0-9]\)\.\([0-9]\).*/\2/'`
	jz=`echo $version | sed -e 's/java version \"\([0-9]\)\.\([0-9]\)\.\([0-9]\).*/\3/'`
        changequote([, ])

	dnl The following long expression (which I couldn't figure out how
	dnl to break across multiple lines in this autoconf script --
	dnl sorry) checks that the Java major version is greater than or
	dnl equal to the requested version and either (1) the minor version
	dnl number is greater than the requested minor version number, or
	dnl (2) the minor version number is equal to the requested number
	dnl and the subminor Java version number is greater than or equal
	dnl to the requested subminor version number.

        if test '(' $jx -ge $rx -a '(' '(' $jy -gt $ry ')' -o '(' '(' $jy -eq $ry ')' -a '(' $jz -ge $rz ')' ')' ')' ')' ; then
          AC_MSG_RESULT(ok)
        else
          AC_MSG_RESULT(no)
          AC_MSG_ERROR([Need Java version $1 but only found version $version.])
        fi

	dnl Retain these for use below.

	JAVA_VER_MAJOR=$jx
	JAVA_VER_MINOR=$jy
	JAVA_VER_SUBMINOR=$jz
    ])

    dnl Look for the path to the include files for Java.
    dnl This macro will set JAVA_CPPFLAGS appropriately.

    AC_JAVA_INCLUDE_DIRS

    dnl On MacOS X, JNI libraries loaded by Java need to be created as bundles.

    case $host in
    *darwin*) 
	JAVA_LDFLAGS="${JAVA_LDFLAGS} -bundle_loader ${JAVA}"
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

    dnl There is currently nothing to add for LIBS.

    JAVA_LIBS=

    AC_DEFINE([USE_JAVA], 1, [Define to 1 to use Java])
    AC_SUBST(USE_JAVA, 1)

    AC_SUBST(JAVA_CPPFLAGS)
    AC_SUBST(JAVA_LDFLAGS)
    AC_SUBST(JAVA_LIBS)
    AC_SUBST(JNIEXT)
    AC_SUBST(JNIBASENAME)

  fi

  dnl We record the USE_XXX flag, for later testing in Makefiles.

  LIBSBML_OPTIONS="$LIBSBML_OPTIONS USE_JAVA"

])


dnl The following was originally AC_JNI_INCLUDE_DIR, 
dnl Available from the GNU Autoconf Macro Archive at:
dnl http://www.gnu.org/software/ac-archive/htmldoc/ac_jni_include_dirs.html

AC_DEFUN([AC_JAVA_INCLUDE_DIRS],[
  
  test "x$JAVAC" = x && AC_MSG_ERROR(['$JAVAC' undefined])
  
  _AC_JAVA_FOLLOW_SYMLINKS("$JAVAC")
  _jtopdir=`echo "$JAVA_FOLLOWED" | sed -e 's://*:/:g' -e 's:/[[^/]]*$::'`
  case "$host_os" in
    *darwin*)
	java_bail=no
	java_mac_prefix="/System/Library/Frameworks/JavaVM.framework"
	case $JAVA_VER_MINOR in
	  6) 
	    if test -e "$java_mac_prefix/Versions/CurrentJDK/Headers"; then
	      _jinc="$java_mac_prefix/Versions/CurrentJDK/Headers"
	    elif test -e "$java_mac_prefix/Versions/1.6.0/Headers"; then
	      _jinc="$java_mac_prefix/Versions/1.6.0/Headers"
	    elif test -e "$java_mac_prefix/Versions/1.6/Headers"; then
	      _jinc="$java_mac_prefix/Versions/1.6/Headers"
	    else
	      java_bail=yes
	    fi
	    ;;
	  5) 
	    if test -e "$java_mac_prefix/Versions/CurrentJDK/Headers"; then
	      _jinc="$java_mac_prefix/Versions/CurrentJDK/Headers"
	    elif test -e "$java_mac_prefix/Versions/1.5.0/Headers"; then
	      _jinc="$java_mac_prefix/Versions/1.5.0/Headers"
	    elif test -e "$java_mac_prefix/Versions/1.5/Headers"; then
	      _jinc="$java_mac_prefix/Versions/1.5/Headers"
	    else
	      java_bail=yes
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
	      if test -e "$java_mac_prefix/Versions/1.4.2/Headers"; then
	        _jinc="$java_mac_prefix/Versions/1.4.2/Headers"
              elif test -e "$java_mac_prefix/Versions/1.4.1/Headers"; then
	        _jinc="$java_mac_prefix/Versions/1.4.1/Headers"
              else
	        java_bail=yes
	      fi;
	    elif test $JAVA_VER_SUBMINOR -eq 1; then
	      if test -e "$java_mac_prefix/Versions/1.4.1/Headers"; then
	        _jinc="$java_mac_prefix/Versions/1.4.1/Headers"
              else
	        java_bail=yes
	      fi;
            else
	      java_bail=yes
	    fi
	    ;;
	  3) 
	    if test -e "$java_mac_prefix/Versions/1.3.1/Headers"; then
	      _jinc="$java_mac_prefix/Versions/1.3.1/Headers"
	    elif test -e "$java_mac_prefix/Versions/1.3/Headers"; then
	      _jinc="$java_mac_prefix/Versions/1.3/Headers"
	    else
	      java_bail=yes
	    fi
	    ;;
	  *)
             _jtopdir=`echo "$_jtopdir" | sed -e 's:/[[^/]]*$::'`
	     _jinc="$_jtopdir/Versions/Current/Headers"
	     ;;
	esac

	if test $java_bail = yes; then
          AC_MSG_ERROR([Cannot find Java include files.])
          AC_MSG_ERROR([MacOS X 10.3.x has known problems with its Java 1.4 installation.])
          AC_MSG_ERROR([See http://developer.apple.com/java/faq.])
	fi
	;;
    *) 
        _jinc="$_jtopdir/include"
	;;
  esac
  if test -e "$_jinc/jni.h"; then
    JAVA_CPPFLAGS="$JAVA_CPPFLAGS -I$_jinc"
  else
    _jtopdir=`echo "$_jtopdir" | sed -e 's:/[[^/]]*$::'`
    if test -e "$_jtopdir/include/jni.h"; then
      JAVA_CPPFLAGS="$JAVA_CPPFLAGS -I$_jtopdir/include"
    else
      AC_MSG_ERROR([Cannot find Java include files.])
    fi
  fi
  
  dnl Get the likely subdirectories for system specific Java includes.
  case "$host_os" in
    bsdi*)    _java_inc_subdirs="bsdos";;
    linux*)   _java_inc_subdirs="linux genunix";;
    osf*)     _java_inc_subdirs="alpha";;
    solaris*) _java_inc_subdirs="solaris";;
    *cygwin*) _java_inc_subdirs="win32";;
    *)        _java_inc_subdirs="genunix";;
  esac
  
  dnl Add any subdirectories that are present.
  for jsubdir in $_java_inc_subdirs; do
    if test -d "$_jtopdir/include/$jsubdir"; then
      JAVA_CPPFLAGS="$JAVA_CPPFLAGS -I$_jtopdir/include/$jsubdir"
    fi
  done
])

dnl _AC_JAVA_FOLLOW_SYMLINKS <path>
dnl Follows symbolic links on <path>,
dnl finally setting variable _AC_JAVA_FOLLOWED
dnl --------------------
AC_DEFUN([_AC_JAVA_FOLLOW_SYMLINKS],[
  dnl find the include directory relative to the javac executable
  _cur="$1"
  while ls -ld "$_cur" 2>/dev/null | grep " -> " >/dev/null; do
        _slink=`ls -ld "$_cur" | sed 's/.* -> //'`
        case "$_slink" in
          /*) _cur="$_slink";;
          dnl 'X' avoids triggering unwanted echo options.
          *) _cur=`echo "X$_cur" | sed -e 's/^X//' -e 's:[[^/]]*$::'`"$_slink";;
        esac
  done
  JAVA_FOLLOWED="$_cur"
])# _AC_JAVA
