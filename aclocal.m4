# generated automatically by aclocal 1.7.2 -*- Autoconf -*-

# Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2002
# Free Software Foundation, Inc.
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY, to the extent permitted by law; without
# even the implied warranty of MERCHANTABILITY or FITNESS FOR A
# PARTICULAR PURPOSE.

dnl
dnl Filename    : swig.m4
dnl Description : Autoconf macro to check for SWIG
dnl Author(s)   : SBML Team <sbml-team@caltech.edu>
dnl Organization: California Institute of Technology
dnl Created     : 2004-06-18
dnl Revision    : $Id$
dnl Source      : $Source$
dnl
dnl
dnl This code originally came from the Autoconf Macro archive
dnl (ac-archive.sf.net) on 2004-06-18.  It has been modified somewhat in
dnl order to fit with the rest of libsbml.

dnl The original code contained here was initially developed by:
dnl Andrew Collier <colliera@nu.ac.za>.
dnl Copyright (C) 2003 Andrew Collier
dnl 
dnl They are distributed in the hope that they will be useful, but WITHOUT ANY
dnl WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
dnl FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
dnl details. (You should have received a copy of the GNU General Public License
dnl along with this program; if not, write to the Free Software Foundation,
dnl Inc., 59 Temple Place -- Suite 330, Boston, MA 02111-1307, USA.)
dnl 
dnl As a special exception, the Free Software Foundation gives unlimited
dnl permission to copy, distribute and modify the configure scripts that are
dnl the output of Autoconf. You need not follow the terms of the GNU General
dnl Public License when using or distributing such scripts, even though
dnl portions of the text of Autoconf appear in them. The GNU General Public
dnl License (GPL) does govern all other use of the material that constitutes
dnl the Autoconf program.
dnl 
dnl Certain portions of the Autoconf source text are designed to be copied (in
dnl certain cases, depending on the input) into the output of Autoconf. We call
dnl these the "data" portions. The rest of the Autoconf source text consists of
dnl comments plus executable code that decides which of the data portions to
dnl output in any given case. We call these comments and executable code the
dnl "non-data" portions. Autoconf never copies any of the non-data portions
dnl into its output.
dnl 
dnl This special exception to the GPL applies to versions of Autoconf released
dnl by the Free Software Foundation. When you make and distribute a modified
dnl version of Autoconf, you may extend this special exception to the GPL to
dnl apply to your modified version as well, *unless* your modified version has
dnl the potential to copy into its output some of the text that was the
dnl non-data portion of the version that you started with. (In other words,
dnl unless your change moves or copies text from the non-data portions to the
dnl data portions.) If your modification has such potential, you must delete
dnl any notice of this special exception to the GPL from your modified version.


dnl From the original description on ac-archive.sf.net:
dnl
dnl @synopsis AC_PKG_SWIG([major.minor.micro],[c++],[raw])
dnl
dnl This macro searches for a SWIG installation on your system.  If the
dnl version argument is given (e.g. 1.3.17), AC_PKG_SWIG checks that the
dnl swig package is this version number or higher.
dnl
dnl In configure.in, use as:
dnl
dnl		AC_PKG_SWIG(1.3.17)
dnl		AC_PROG_LN_S
dnl		AC_PROG_CXX
dnl		AC_PROG_LIBTOOL
dnl
dnl Acknowledgements:
dnl
dnl o stuff from a posting by Sebastian Huber <sebastian-huber@web.de> on Swig
dnl   digest, Vol 1 #1157 (3 Jan 2003).
dnl
dnl @version $Id$
dnl @author Andrew Collier <colliera@nu.ac.za>

AC_DEFUN([CONFIG_PROG_SWIG],
[
  AC_ARG_WITH(swig,
              AC_HELP_STRING([--with-swig=PREFIX],
                             [Regenerate SWIG wrappers [[default=no]]]),
	      [with_swig=$withval],
	      [with_swig=no])

  if test $with_swig != no;
  then

    SWIG_REQUEST_VERSION=

    changequote(<<, >>)

    for a in $1 $2 $3 $4 $5 $6 $7 $8 $9 x; do
        case "$a" in
            x) break;;
            [0-9]*.[0-9]*.[0-9]*) SWIG_REQUEST_VERSION="$a";;
    	c++) SWIGFLAGS="$SWIGFLAGS -c++";;
    	raw) SWIGFLAGS="$SWIGFLAGS -c";;
        esac
    done

    changequote([, ])

    if test $with_swig != yes;
    then
      AC_PATH_PROG([SWIG], [swig], [swig], [$with_swig/bin:$PATH])
    else
      AC_PATH_PROG([SWIG], [swig])
    fi

    if test -n "$SWIG";
    then
    	SWIGLIB=`$SWIG -swiglib`

	AC_SUBST(SWIG)
	AC_SUBST(SWIGLIB)
	AC_SUBST(SWIGFLAGS)

	AC_MSG_CHECKING(swig version)

	changequote(<<, >>)
	swig_version=`$SWIG -version 2>&1 | sed 's/.* \([0-9]*\.[0-9]*\.[0-9]*\).*/\1/p; d'`
	swig_major_ver=`expr $swig_version : '\([0-9]*\)\.[0-9]*\.[0-9]*'`
	swig_minor_ver=`expr $swig_version : '[0-9]*\.\([0-9]*\)\.[0-9]*'`
	swig_micro_ver=`expr $swig_version : '[0-9]*\.[0-9]*\.\([0-9]*\)' '|' 0`
	changequote([, ])

	AC_MSG_RESULT($swig_version)

	SWIGVERNUM=`printf "%02d%02d%02d" $swig_major_ver $swig_minor_ver $swig_micro_ver`
	# SWIGVERNUM=`echo $SWIG_REQUEST_VERSION | awk '{ split($[1],a,"\."); print [a[1]*1000000+a[2]*1000+a[3]] }' 2>/dev/null`

	if test -n "$SWIG_REQUEST_VERSION";
	then
		AC_MSG_CHECKING(requested swig version ($SWIG_REQUEST_VERSION))

		changequote(<<, >>)
		swig_major_req=`expr $SWIG_REQUEST_VERSION : '\([0-9]*\)\.[0-9]*\.[0-9]*'`
		swig_minor_req=`expr $SWIG_REQUEST_VERSION : '[0-9]*\.\([0-9]*\)\.[0-9]*'`
		swig_micro_req=`expr $SWIG_REQUEST_VERSION : '[0-9]*\.[0-9]*\.\([0-9]*\)'`
		changequote([, ])

		if test $swig_major_ver -ge $swig_major_req &&
		   test $swig_minor_ver -ge $swig_minor_req &&
		   test $swig_micro_ver -ge $swig_micro_req
		then
			AC_MSG_RESULT(yes)
		else
			AC_MSG_RESULT(no)
			AC_MSG_ERROR(installed version of swig is too old!)
		fi
	fi

      AC_DEFINE([USE_SWIG], 1, [Define to 1 to use SWIG])
      AC_SUBST(USE_SWIG, 1)

      AC_SUBST(SWIG_CPPFLAGS)
      AC_SUBST(SWIG_LDFLAGS)
      AC_SUBST(SWIG_LIBS)

    fi
  fi

])

dnl
dnl Filename    : python.m4
dnl Description : Autoconf macro to check for Python
dnl Author(s)   : SBML Team <sbml-team@caltech.edu>
dnl Organization: California Institute of Technology
dnl Created     : 2004-06-18
dnl Revision    : $Id$
dnl Source      : $Source$
dnl
dnl Portions of this code originally came from the aclocal 1.7
dnl distribution.  The main macro was named AM_PATH_PYTHON, but I (Mike
dnl Hucka) changed the names of this and various variables it defines
dnl because it is being used outside of automake.

dnl Portions of the code contained here was initially developed by:
dnl Andrew Dalke and James Henstridge.
dnl Copyright 1999, 2000, 2001, 2002  Free Software Foundation, Inc.
dnl
dnl This program is free software; you can redistribute it and/or modify
dnl it under the terms of the GNU General Public License as published by
dnl the Free Software Foundation; either version 2, or (at your option)
dnl any later version.
dnl
dnl This program is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl GNU General Public License for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with this program; if not, write to the Free Software
dnl Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
dnl 02111-1307, USA.

AC_DEFUN([CONFIG_PROG_PYTHON],
[
  AC_ARG_VAR([PYTHON])

  AC_ARG_WITH(python,
              AC_HELP_STRING([--with-python=PREFIX],
                             [Generate Python interface library [[default=no]]]),
	      [with_python=$withval],
	      [with_python=no])

  if test $with_python != no; then

    dnl Find a python executable.

    if test $with_python != yes;
    then
      AC_PATH_PROG([PYTHON], [python], [python], [$with_python/bin:$PATH])
    else
      AC_PATH_PROG([PYTHON], [python])
    fi

    dnl check version if required
    m4_ifvaln([$1], [
        AC_MSG_CHECKING($PYTHON version >= $1)
        if test `$PYTHON -c ["import sys; print sys.version[:3] >= \"$1\" and \"OK\" or \"OLD\""]` = "OK"
        then
          AC_MSG_RESULT(ok)
        else
          AC_MSG_RESULT(no)
        fi
      fi])

    AC_MSG_CHECKING(for Python prefix)
    PYTHON_PREFIX=`($PYTHON -c "import sys; print sys.prefix") 2>/dev/null`
    AC_MSG_RESULT($PYTHON_PREFIX)

    changequote(<<, >>)
    PYTHON_VERSION=`$PYTHON -c "import sys; print sys.version[:3]"`
    changequote([, ])

    PYTHON_NAME="python${PYTHON_VERSION}"

    dnl Figure out the last bits for linking.
    dnl This comes in part from SWIG 1.3.31's configure.ac file.

    case $host in
    *cygwin* | *mingw*) 
	PYTHON_CPPFLAGS="-I${PYTHON_PREFIX}/include/${PYTHON_NAME} -DUSE_DL_IMPORT"
	PYTHON_LDFLAGS="-L${PYTHON_PREFIX}/lib/${PYTHON_NAME}"
        PYTHON_LIBS="-l${PYTHON_NAME}"
	;;
    *darwin*) 
	dnl Got an ugly situation on MacOS X: need different args depending
	dnl on whether the system-installed or Fink-installed version of
	dnl Python is being used.  The following is a heuristic approach
	dnl that may not be correct for all situations.  The heuristic is:
	dnl if the Python executable is /usr/something, assume it's the
	dnl system-installed version and use -framework; otherwise, assume
	dnl it's either the Fink version or something else, and don't use
	dnl -framework.

	if test `expr ${PYTHON} ':' '/usr/bin/.*'` -ne 0; then
	  dnl MacOSX-installed version of Python (we hope).
   	  PYTHON_CPPFLAGS="-I${PYTHON_PREFIX}/include/${PYTHON_NAME}"
	  PYTHON_LDFLAGS="-L${PYTHON_PREFIX}/lib/${PYTHON_NAME}/lib-dynload -framework Python"
	else
	  dnl Fink-installed version of Python, or something else.
   	  PYTHON_CPPFLAGS="-I${PYTHON_PREFIX}/include/${PYTHON_NAME}"
	  PYTHON_LDFLAGS="-L${PYTHON_PREFIX}/lib/${PYTHON_NAME}/lib-dynload -bundle_loader ${PYTHON}"
	fi
	;;
    *)
	PYTHON_CPPFLAGS="-I${PYTHON_PREFIX}/include/${PYTHON_NAME}"
        PYTHON_LDFLAGS="-L${PYTHON_PREFIX}/lib/${PYTHON_NAME}"
        PYTHON_LIBS="-l${PYTHON_NAME}"
	;;
    esac

    AC_DEFINE([USE_PYTHON], 1, [Define to 1 to use Python])
    AC_SUBST(USE_PYTHON, 1)

    AC_SUBST(PYTHON_CPPFLAGS)
    AC_SUBST(PYTHON_LDFLAGS)
    AC_SUBST(PYTHON_LIBS)

  fi
])


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

AC_DEFUN(CONFIG_PROG_JAVA,
[
  AC_ARG_WITH([java],
    AC_HELP_STRING([--with-java=PREFIX],
                   [Generate Java interface library [[default=no]]]),
    [with_java=$withval],
    [with_java=no])

  if test $with_java != no; then

    if test $with_java != yes;
    then
      dnl Users seems to have supplied a prefix directory path.  See if we can 
      dnl find Java somewhere in the given tree.

      AC_PATH_PROG([JAVA], [java], [java], [$with_java/bin:$PATH])
      AC_PATH_PROG([JAVAC], [javac], [javac], [$with_java/bin:$PATH])
      AC_PATH_PROG([JAR], [jar], [jar], [$with_java/bin:$PATH])
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

AC_DEFUN(CONFIG_PROG_MATLAB,
[
  AC_ARG_WITH([matlab],
    AC_HELP_STRING([--with-matlab=PREFIX],
                   [Generate MATLAB interface library [[default=no]]]),
    [with_matlab=$withval],
    [with_matlab=no])

  if test $with_matlab != no; then




    AC_DEFINE([USE_MATLAB], 1, [Define to 1 to use Matlab])
    AC_SUBST(USE_MATLAB, 1)

    AC_SUBST(MATLAB_CPPFLAGS)
    AC_SUBST(MATLAB_LDFLAGS)
    AC_SUBST(MATLAB_LIBS)

  fi

])

dnl
dnl Filename    : expat.m4
dnl Description : Autoconf macro to check for existence of Expat library
dnl Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
dnl Organization: JST ERATO Kitano Symbiotic Systems Project
dnl Created     : 2003-11-05
dnl Revision    : $Id$
dnl Source      : $Source$
dnl
dnl Copyright 2003 California Institute of Technology and
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
dnl     Ben Bornstein
dnl     The Systems Biology Markup Language Development Group
dnl     ERATO Kitano Symbiotic Systems Project
dnl     Control and Dynamical Systems, MC 107-81
dnl     California Institute of Technology
dnl     Pasadena, CA, 91125, USA
dnl
dnl     http://www.cds.caltech.edu/erato
dnl     mailto:sysbio-team@caltech.edu
dnl
dnl Contributor(s):
dnl   Stephan Hoops
dnl   Mike Hucka <mhucka@caltech.edu> Enhancements to this file.

dnl
dnl Check --with-expat[=PREFIX] is specified and Expat 1.95.0 or greater
dnl is installed.
dnl

AC_DEFUN(CONFIG_LIB_EXPAT,
[
  AC_ARG_WITH([expat],
    AC_HELP_STRING([--with-expat=PREFIX],
                   [Use Expat XML Library [[default=no]]]),
    [with_expat=$withval],
    [with_expat=no])

  if test $with_expat != no; then

    AC_LANG_PUSH(C)

    EXPAT_CPPFLAGS=
    EXPAT_LDFLAGS=
    EXPAT_LIBS=

    if test $with_expat != yes; then
      EXPAT_CPPFLAGS="-I$with_expat/include -I\$(top_srcdir)/expat"
      EXPAT_LDFLAGS="-L$with_expat/lib"
    fi

    EXPAT_LIBS="-lexpat"

    dnl The following is grungy but I don't know how else to make 
    dnl AC_CHECK_LIB use particular library and include paths.

    tmp_CPPFLAGS=$CPPFLAGS
    tmp_LDFLAGS=$LDFLAGS
    tmp_LIBS=$LIBS
    CPPFLAGS="$CPPFLAGS $EXPAT_CPPFLAGS"
    LDFLAGS="$LDFLAGS $EXPAT_LDFLAGS"
    LIBS="$LIBS $EXPAT_LIBS"

    AC_CHECK_LIB(expat, XML_ParserCreate, [expat_found=yes], [expat_found=no])

    if test $expat_found = no; then
      AC_MSG_ERROR([Could not find the Expat XML library.])
    fi

    AC_CHECK_HEADERS(expat.h, [expat_found=yes], [expat_found=no])

    if test $expat_found = no; then
      AC_MSG_ERROR([Could not find the file expat.h.])
    fi

    CPPFLAGS=$tmp_CPPFLAGS
    LDFLAGS=$tmp_LDFLAGS
    LIBS=$tmp_LIBS

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

])

dnl
dnl Filename    : xercesc.m4
dnl Description : Autoconf macro to check for existence of Xerces-C library
dnl Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
dnl Organization: JST ERATO Kitano Symbiotic Systems Project
dnl Created     : 2003-02-14
dnl Revision    : $Id$
dnl Source      : $Source$
dnl
dnl Copyright 2002 California Institute of Technology and
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
dnl     Ben Bornstein
dnl     The Systems Biology Markup Language Development Group
dnl     ERATO Kitano Symbiotic Systems Project
dnl     Control and Dynamical Systems, MC 107-81
dnl     California Institute of Technology
dnl     Pasadena, CA, 91125, USA
dnl
dnl     http://www.cds.caltech.edu/erato
dnl     mailto:sysbio-team@caltech.edu
dnl
dnl Contributor(s):
dnl   Mike Hucka <mhucka@caltech.edu> Enhancements to this file.
dnl

dnl
dnl Check --with-xerces[=PREFIX] is specified and Xerces-C++ is installed.
dnl

AC_DEFUN(CONFIG_LIB_XERCES,
[
  AC_ARG_WITH([xerces],
    AC_HELP_STRING([--with-xerces=PREFIX],
                   [Use Xerces XML Library [[default=yes]]]),
    [with_xerces=$withval],
    [with_xerces=yes])

  if test $with_expat != no; then
    with_xerces=no
  fi

  if test $with_xerces != no; then

    AC_MSG_CHECKING([for Apache's Xerces-C XML library])

    AC_LANG_PUSH(C)

    XERCES_CPPFLAGS=
    XERCES_LDFLAGS=
    XERCES_LIBS=

    if test $with_xerces != yes; then
      XERCES_CPPFLAGS="-I$with_xerces/include"
      XERCES_LDFLAGS="-L$with_xerces/lib"
    fi

    XERCES_LIBS="-lxerces-c"

    dnl The following is grungy but I don't know how else to make 
    dnl AC_CHECK_LIB use particular library and include paths.

    tmp_CPPFLAGS=$CPPFLAGS
    tmp_LDFLAGS=$LDFLAGS
    tmp_LIBS=$LIBS
    CPPFLAGS="$CPPFLAGS $XERCES_CPPFLAGS"
    LDFLAGS="$LDFLAGS $XERCES_LDFLAGS"
    LIBS="$LIBS $XERCES_LIBS"

    AC_TRY_LINK([
#include <xercesc/util/XercesDefs.hpp>
#include <xercesc/util/PlatformUtils.hpp>
#ifndef XERCES_HAS_CPP_NAMESPACE
#define XERCES_CPP_NAMESPACE_QUALIFIER
#endif
      ],
      [XERCES_CPP_NAMESPACE_QUALIFIER XMLPlatformUtils::Initialize();],
      [xerces_found=yes],
      [xerces_found=no])

    if test $xerces_found = no; then
      AC_MSG_ERROR([Could not find the Xerces XML library.])
    fi

    CPPFLAGS=$tmp_CPPFLAGS
    LDFLAGS=$tmp_LDFLAGS
    LIBS=$tmp_LIBS

    AC_LANG_POP

    AC_DEFINE([USE_XERCES], 1, [Define to 1 to use the Xerces XML library])
    AC_SUBST(USE_XERCES, 1)

    AC_SUBST(XERCES_CPPFLAGS)
    AC_SUBST(XERCES_LDFLAGS)
    AC_SUBST(XERCES_LIBS)

  fi

])

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

AC_DEFUN(CONFIG_LIB_LIBCHECK,
[
  AC_ARG_WITH([check],
    AC_HELP_STRING([--with-check=PREFIX],
                   [Build the test suite using libcheck (default=no)]),
    [with_libcheck=$withval],
    [with_libcheck=no])

  libcheck_found=no

  if test $with_libcheck != no; then

    LIBCHECK_CPPFLAGS=
    LIBCHECK_LDFLAGS=
    LIBCHECK_LIBS=

    if test $with_libcheck != yes; then
      LIBCHECK_CPPFLAGS="-I$with_libcheck/include"
      LIBCHECK_LDFLAGS="-L$with_libcheck/lib"
    fi

    AC_LANG_PUSH(C)

    tmp_CPPFLAGS=$CPPFLAGS
    tmp_LDFLAGS=$LDFLAGS
    tmp_LIBS=$LIBS
    CPPFLAGS="$LIBCHECK_CPPFLAGS $CPPFLAGS"
    LDFLAGS="$LIBCHECK_LDFLAGS $LDFLAGS"

    AC_CHECK_HEADERS([check.h], [libcheck_found=yes], [libcheck_found=no])

    if test $libcheck_found = yes; then
      AC_CHECK_LIB([check], [srunner_create],
        [libcheck_found=yes],
        [libcheck_found=no])
    fi

    if test $libcheck_found = yes; then
      LIBCHECK_LIBS="$LIBS -lcheck"
    else 
      AC_MSG_ERROR([Could not find the libcheck library.])
    fi

    CPPFLAGS=$tmp_CPPFLAGS
    LDFLAGS=$tmp_LDFLAGS
    LIBS=$tmp_LIBS

    AC_LANG_POP(C)

    AC_DEFINE([USE_LIBCHECK], 1, [Define to 1 to use the libcheck library])
    AC_SUBST(USE_LIBCHECK, 1)

    AC_SUBST(LIBCHECK_CPPFLAGS)
    AC_SUBST(LIBCHECK_LDFLAGS)
    AC_SUBST(LIBCHECK_LIBS)

  fi

])

