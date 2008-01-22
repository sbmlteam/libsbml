dnl
dnl Filename    : ruby.m4
dnl Description : Autoconf macro to check for Ruby
dnl Author(s)   : SBML Team <sbml-team@caltech.edu>
dnl Organization: California Institute of Technology
dnl Created     : 2007-12-17
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

AC_DEFUN([CONFIG_PROG_RUBY],
[
  AC_ARG_VAR([RUBY])

  AC_ARG_WITH(ruby,
              AC_HELP_STRING([--with-ruby=PREFIX],
                             [Generate Ruby interface library [[default=no]]]),
	      [with_ruby=$withval],
	      [with_ruby=no])

  if test $with_ruby != no; then

    dnl Find a ruby executable.

    if test $with_ruby != yes;
    then
      dnl Remove needless trailing slashes because it can confuse tests later.
      with_ruby=`echo $with_ruby | sed -e 's,\(.*\)/$,\1,g'`

      AC_PATH_PROG([RUBY], [ruby], [$with_ruby/bin/ruby],
                   [no-ruby-found], [$with_ruby/bin])
    else
      AC_PATH_PROG([RUBY], [ruby])
    fi

    dnl check version if required
    m4_ifvaln([$1], [
        AC_MSG_CHECKING($RUBY version >= $1)
        if test `$RUBY -rrbconfig -e ["puts RUBY_VERSION >= \"$1\" ? \"OK\" : \"OLD\""]` = "OK"
        then
          AC_MSG_RESULT(ok)
        else
          AC_MSG_RESULT(no)
        fi
    ])

    AC_MSG_CHECKING(for ruby prefix)
    RUBY_PREFIX=`$RUBY -rrbconfig -e ["include Config; puts CONFIG['prefix']"]`
    AC_MSG_RESULT($RUBY_PREFIX)

    if test `$RUBY -rrbconfig -e ["puts RUBY_VERSION >= \"1.9.0\" ? \"OK\" : \"OLD\""]` = "OK"; 
    then
      RUBY_ARCHDIR=`$RUBY -rrbconfig -e ["include Config; print \"#{CONFIG['rubyhdrdir']} -I#{CONFIG['rubyhdrdir']}/#{CONFIG['arch']}\" "]`
    else
      RUBY_ARCHDIR=`$RUBY -rrbconfig -e ["include Config; puts CONFIG['archdir']"]`
    fi    
    RUBY_LIBDIR=`$RUBY -rrbconfig -e ["include Config; puts CONFIG['libdir']"]` 
    RUBY_NAME=`$RUBY -rrbconfig -e ["include Config; puts CONFIG['RUBY_SO_NAME']"]`

    dnl Figure out the last bits for linking.
    dnl This comes in part from SWIG 1.3.31's configure.ac file.

    case $host in
    *darwin*) 
   	RUBY_CPPFLAGS="-I${RUBY_ARCHDIR}"
	RUBY_LDFLAGS="-L${RUBY_LIBDIR} -undefined dynamic_lookup"
	;;
    *cygwin* | *mingw*) 
	RUBY_CPPFLAGS="-I${RUBY_ARCHDIR} -DUSE_DL_IMPORT"
	RUBY_LDFLAGS="-L${RUBY_LIBDIR}"
        RUBY_LIBS="-l${RUBY_NAME}"
	CONFIG_ADD_LDPATH(${RUBY_PREFIX}/lib/${RUBY_NAME}/config)
	;;
    *)
	RUBY_CPPFLAGS="-I${RUBY_ARCHDIR}"
        RUBY_LDFLAGS="-L${RUBY_LIBDIR}"
        RUBY_LIBS="-l${RUBY_NAME}"
	;;
    esac

    AC_DEFINE([USE_RUBY], 1, [Define to 1 to use Ruby])
    AC_SUBST(USE_RUBY, 1)

    AC_SUBST(RUBY_CPPFLAGS)
    AC_SUBST(RUBY_LDFLAGS)
    AC_SUBST(RUBY_LIBS)

  fi

  dnl We record the USE_XXX flag, for later testing in Makefiles.

  LIBSBML_OPTIONS="$LIBSBML_OPTIONS USE_RUBY"

])

