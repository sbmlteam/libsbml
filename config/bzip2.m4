dnl Filename    : bzip2.m4
dnl Description : Autoconf macro to check for existence of bzip2 library
dnl Author(s)   : SBML Team <sbml-team@caltech.edu>
dnl Organization: California Institute of Technology
dnl Created     : 2008-07-04
dnl Revision    : $Id:$
dnl $HeadURL$
dnl
dnl <!-------------------------------------------------------------------------
dnl This file is part of libSBML.  Please visit http://sbml.org for more
dnl information about SBML, and the latest version of libSBML.
dnl
dnl Copyright 2008 California Institute of Technology.
dnl 
dnl This library is free software; you can redistribute it and/or modify it
dnl under the terms of the GNU Lesser General Public License as published by
dnl the Free Software Foundation.  A copy of the license agreement is provided
dnl in the file named "LICENSE.txt" included with this software distribution
dnl and also available online as http://sbml.org/software/libsbml/license.html
dnl --------------------------------------------------------------------- -->*/

#
# This bzip2 check code is based on the zlib check code in configure.ac
# file of OpenSSH-5.0p1 (http://www.openssh.com/).
# The file is distributed under the following terms:
#
# ------------------------------------------------------------------------------ 
# Copyright (c) 1999-2004 Damien Miller
#
# Permission to use, copy, modify, and distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
# OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
# ------------------------------------------------------------------------------ 

AC_DEFUN([CONFIG_LIB_BZ2],
[

  AC_ARG_WITH([bzip2],
	      AC_HELP_STRING([--with-bzip2=PREFIX], 
                             [Use bzip2 for read/write a bzipped SBML [[default=yes]] ]
              ),
	      [with_bzip2="$withval"], [with_bzip2=yes]
  )

  AC_MSG_CHECKING(for bzip2 library)
  if test "x$withval" != "xno" ; then
    AC_MSG_RESULT(yes)
    if test "x$withval" != "xyes"; then
      if test -d "$withval/lib"; then
        BZ2_LDFLAGS="-L${withval}/lib"
      else
        BZ2_LDFLAGS="-L${withval}"
      fi
    fi
  
    if test -d "$withval/include"; then
      BZ2_CPPFLAGS="-I${withval}/include"
    fi
  
    BZ2_LIBS="-lbz2"
  
    saved_CPPFLAGS=$CPPFLAGS
    saved_LDFLAGS=$LDFLAGS
    saved_LIBS=$LIBS
  
    AC_CHECK_LIB(bz2, BZ2_bzopen, ,
  	       [
  		dnl Check default bzip2 install dir
  		LDFLAGS="-L/usr/lib -L/usr/local/lib ${LDFLAGS} ${BZ2_LDFLAGS}"
  		CPPFLAGS="-I/usr/include -I/usr/local/include ${CPPFLAGS} ${BZ2_CPPFLAGS}"
                  LIBS="${LIBS} ${BZ2_LIBS}"
  		AC_TRY_LINK_FUNC(BZ2_bzopen, AC_DEFINE(HAVE_BZ2),
  			[
                          AC_MSG_ERROR([*** bzip2 missing - please install bzip2 first or check config.log.
                  *** Please run the configure command with "--with-bzip2=no" option if you
                  *** want to build libSBML without support for bzip2 compressed SBML file.])
  			]
  		)
  	]
     )
  
    AC_CHECK_HEADER([bzlib.h], ,
  			[
                          AC_MSG_ERROR([*** bzlib.h missing - please install bzip2 first or check config.log.
                  *** Please run the configure command with "--with-bzip2=no" option if you
                  *** want to build libSBML without support for bzip2 compressed SBML file.])
  			]
    )
  
    CPPFLAGS=$saved_CPPFLAGS
    LDFLAGS=$saved_LDFLAGS
    LIBS=$saved_LIBS
  
    AC_DEFINE([USE_BZ2], 1, [Define to 1 to use the bzip2 library])
    AC_SUBST(USE_BZ2, 1)
    AC_SUBST(BZ2_CPPFLAGS)
    AC_SUBST(BZ2_LDFLAGS)
    AC_SUBST(BZ2_LIBS)
  
    dnl We record the USE_XXX flag, for later testing in Makefiles.
  
    LIBSBML_OPTIONS="$LIBSBML_OPTIONS USE_BZ2"
  else
    AC_MSG_RESULT(no)
  fi
])
