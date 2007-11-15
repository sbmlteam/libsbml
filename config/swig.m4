dnl
dnl Filename    : swig.m4
dnl Description : Autoconf macro to check for SWIG
dnl Author(s)   : Michael Hucka <mhucka@caltech.edu>
dnl Created     : 2004-06-18
dnl Revision    : $Id$
dnl Source      : $Source$
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
dnl             AC_PKG_SWIG(1.3.17)
dnl             AC_PROG_LN_S
dnl             AC_PROG_CXX
dnl             AC_PROG_LIBTOOL
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
      dnl Remove needless trailing slashes because it can confuse tests later.
      with_swig=`echo $with_swig | sed -e 's,\(.*\)/$,\1,g'`

      AC_PATH_PROG([SWIG], [swig], [$with_swig/bin/swig],
                     [no-swig-found], [$with_swig/bin])
    else
      AC_PATH_PROG([SWIG], [swig])
    fi

    if test -n "$SWIG";
    then
        SWIGLIB=`"$SWIG" -swiglib`

        AC_SUBST(SWIG)
        AC_SUBST(SWIGLIB)
        AC_SUBST(SWIGFLAGS)

        AC_MSG_CHECKING(swig version)

        changequote(<<, >>)
        swig_version=`"$SWIG" -version 2>&1 | sed 's/.* \([0-9]*\.[0-9]*\.[0-9]*\).*/\1/p; d'`
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

          if test $swig_major_ver -gt $swig_major_req \
             || (test $swig_major_ver -eq $swig_major_req && \
                 test $swig_minor_ver -gt $swig_minor_req) \
             || (test $swig_major_ver -eq $swig_major_req && \
                 test $swig_minor_ver -eq $swig_minor_req && \
                 test $swig_micro_ver -ge $swig_micro_req)
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

  dnl We record the USE_XXX flag, for later testing in Makefiles.

  LIBSBML_OPTIONS="$LIBSBML_OPTIONS USE_SWIG"

])
