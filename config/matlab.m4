dnl
dnl Filename    : matlab.m4
dnl Description : Autoconf macro for MATLAB configuration
dnl Author(s)   : Mike Hucka
dnl Created     : 2004-08-02
dnl Revision    : $Id$
dnl Source      : $Source$
dnl
dnl Copyright 2004 California Institute of Technology
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
dnl
dnl     The SBML Team
dnl     Control and Dynamical Systems, MC 107-81
dnl     California Institute of Technology
dnl     Pasadena, CA, 91125, USA
dnl
dnl     http://sbml.org
dnl     mailto:sbml-team@caltech.edu
dnl
dnl Contributor(s):
dnl

AC_DEFUN(CONFIG_PROG_MATLAB,
[
  AC_ARG_WITH([matlab],
    AC_HELP_STRING([--with-matlab=PREFIX],
                   [Generate MATLAB interface library [[default=no]]]),
    [with_matlab=$withval],
    [with_matlab=no])

  if test $with_matlab != no; then

    if test $with_matlab != yes;
    then
      AC_PATH_PROG([MEX], [mex], [mex], [$with_matlab/bin:$PATH])
      AC_PATH_PROG([MATLAB], [matlab], [matlab], [$with_matlab/bin:$PATH])
    else
      AC_PATH_PROG([MEX], [mex])
      AC_PATH_PROG([MATLAB], [matlab])
    fi

    if test -z "$MATLAB"; then
      AC_MSG_ERROR([Could not find 'matlab' executable for MATLAB.])
    elif  test -z "$MEX"; then
      AC_MSG_ERROR([Could not find 'mex' executable for MATLAB.])
    fi

    case $host in
    *darwin*) 
      MEXEXT="mexmac"
      ;;
    *cygwin*) 
      MEXEXT="dll"
      ;;
    *) 
      MEXEXT="mexglx"
      ;;
    esac

    AC_DEFINE([USE_MATLAB], 1, [Define to 1 to use Matlab])
    AC_SUBST(USE_MATLAB, 1)

    AC_SUBST(MATLAB_CPPFLAGS)
    AC_SUBST(MATLAB_LDFLAGS)
    AC_SUBST(MATLAB_LIBS)
    AC_SUBST(MEX_FLAGS)
    AC_SUBST(MEXEXT)

  fi

  dnl We record the USE_XXX flag, for later testing in Makefiles.

  LIBSBML_OPTIONS="$LIBSBML_OPTIONS USE_PYTHON"

])
