dnl
dnl Filename    : java.m4
dnl Description : Autoconf macro to check for existence of Java
dnl Author(s)   : SBML Team <sbml-team@caltech.edu>
dnl Organization: California Institute of Technology
dnl Created     : 2004-07-07
dnl Revision    : $Id$
dnl Source      : $Source$
dnl
dnl Copyright 2004 California Institute of Technology and
dnl Japan Science and Technology Corporation.
dnl
dnl This library is free software; you can redistribute it and/or modify it
dnl under the terms of the GNU Lesser General Public License as published
dnl by the Free Software Foundation; either version 2.1 of the License, or
dnl any later version.
dnl
dnl This library is distributed in the hope that it will be useful, but
dnl WITHOUT ANY WARRANTY, WITHOUT EVEN THE IMPLIED WARRANTY OF
dnl MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  The software and
dnl documentation provided hereunder is on an "as is" basis, and the
dnl California Institute of Technology and Japan Science and Technology
dnl Corporation have no obligations to provide maintenance, support,
dnl updates, enhancements or modifications.  In no event shall the
dnl California Institute of Technology or the Japan Science and Technology
dnl Corporation be liable to any party for direct, indirect, special,
dnl incidental or consequential damages, including lost profits, arising
dnl out of the use of this software and its documentation, even if the
dnl California Institute of Technology and/or Japan Science and Technology
dnl Corporation have been advised of the possibility of such damage.  See
dnl the GNU Lesser General Public License for more details.
dnl
dnl You should have received a copy of the GNU Lesser General Public License
dnl along with this library; if not, write to the Free Software Foundation,
dnl Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
dnl
dnl The original code contained here was initially developed by:
dnl
dnl     Michael Hucka
dnl     SBML Team
dnl     Control and Dynamical Systems, MC 107-81
dnl     California Institute of Technology
dnl     Pasadena, CA, 91125, USA
dnl
dnl     http://www.sbml.org
dnl     mailto:sbml-team@caltech.edu
dnl
dnl Contributor(s):
dnl

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
	version=`$JAVA -version 2>&1 | head -1`
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
	;;
    *)
	JNIEXT="so"
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
	dnl MacOS X's installation of Java 1.4.2 is broken: the header
	dnl files are missing entirely, and there are dangling symlinks.
	dnl The following are ugly kludges to try to do the best we can.
	dnl One of the things this does deliberately is use the 1.4.1
	dnl headers directory on the Mac even for Java 1.4.2 if the 
	dnl 1.4.2 headers directory can't be found.  Yuck.
	dnl 2004-07-07 <mhucka@caltech.edu>

	java_bail=no
	java_mac_prefix="/System/Library/Frameworks/JavaVM.framework"
	case $JAVA_VER_MINOR in
	  4)
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
	     _jinc="$_jtopdir/Versions/Headers"
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
        AC_MSG_CHECKING(symlink for $_cur)
        _slink=`ls -ld "$_cur" | sed 's/.* -> //'`
        case "$_slink" in
          /*) _cur="$_slink";;
          dnl 'X' avoids triggering unwanted echo options.
          *) _cur=`echo "X$_cur" | sed -e 's/^X//' -e 's:[[^/]]*$::'`"$_slink";;
        esac
        AC_MSG_RESULT($_cur)
  done
  JAVA_FOLLOWED="$_cur"
])# _AC_JAVA
