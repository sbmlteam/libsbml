dnl
dnl Filename    : Makefile.am
dnl Description : Process this file with autoconf (2.57) to produce a
dnl Description : configure script.
dnl Author(s)   : SBW Development Group <sysbio-team@caltech.edu>
dnl Organization: Caltech ERATO Kitano Systems Biology Project
dnl Created     : 2003-02-13
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
dnl     The Systems Biology Workbench Development Group
dnl     ERATO Kitano Systems Biology Project
dnl     Control and Dynamical Systems, MC 107-81
dnl     California Institute of Technology
dnl     Pasadena, CA, 91125, USA
dnl
dnl     http://www.cds.caltech.edu/erato
dnl     mailto:sysbio-team@caltech.edu
dnl
dnl Contributor(s):
dnl


dnl ---------------------------------------------------------------------------
dnl Autoconf and Automake initialization.
dnl
dnl Bug reports are sent to sysbio-team@cds.caltech.edu
dnl ---------------------------------------------------------------------------

AC_INIT([SBML Library], [1.1.0], [sysbio-team@caltech.edu], [libsbml])
AC_CONFIG_AUX_DIR(config)
AM_INIT_AUTOMAKE



AC_REVISION($Revision$)

AC_PREREQ(2.57)


AC_CONFIG_SRCDIR(src/SBase.c)


AC_CONFIG_HEADERS(src/sbml/config.h)


dnl ---------------------------------------------------------------------------
dnl Checks for programs.
dnl ---------------------------------------------------------------------------

AC_PROG_CXX
AC_PROG_CC
AC_PROG_CPP
AC_PROG_LIBTOOL

AC_ARG_ENABLE(memory-tracing,
  AC_HELP_STRING([--enable-memory-tracing],
                 [Turn on memory tracing (default: off)]),
    [if test "$enableval" = yes; then
      AC_DEFINE([TRACE_MEMORY], 1,
                [Define to 1 to enable primitive memory tracing.])
     fi
    ])


dnl ---------------------------------------------------------------------------
dnl Checks for libraries.
dnl ---------------------------------------------------------------------------

AC_LANG(C)
AC_CHECK_LIB(check, srunner_create)

SBML_CHECK_LIB_XERCESC


dnl ---------------------------------------------------------------------------
dnl Checks for header files.
dnl ---------------------------------------------------------------------------

AC_HEADER_STDC
AC_CHECK_HEADER(errno.h)
AC_CHECK_HEADER(strings.h)



dnl ---------------------------------------------------------------------------
dnl Checks for typedefs, structures, and compiler characteristics.
dnl ---------------------------------------------------------------------------

AC_C_CONST
AC_TYPE_SIZE_T


dnl ---------------------------------------------------------------------------
dnl Checks for library functions.
dnl ---------------------------------------------------------------------------

AC_CHECK_FUNCS(ftime)
AC_CHECK_FUNCS(strtod)
AC_CHECK_FUNCS(strtol)


dnl ---------------------------------------------------------------------------
dnl Output
dnl ---------------------------------------------------------------------------


AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([src/Makefile])

AC_OUTPUT
