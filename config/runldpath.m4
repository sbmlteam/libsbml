dnl
dnl Filename    : runldpath.m4
dnl Description : Autoconf macro to set special variable RUN_LDPATH
dnl Author(s)   : Mike Hucka
dnl Created     : 2005-04-30
dnl Revision    : $Id$
dnl Source      : $Source$
dnl
dnl Copyright 2005 California Institute of Technology
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
dnl     Mike Hucka
dnl     The SBML Team
dnl     Control and Dynamical Systems, MC 107-81
dnl     California Institute of Technology
dnl     Pasadena, CA, 91125, USA
dnl
dnl     http://sbml.org
dnl     mailto:sbml-team@caltech.edu
dnl
dnl Contributor(s):

dnl CONFIG_LDPATH needs to be called from configure.ac to initialize the 
dnl internal variable.  It needs to be called fairly early.

AC_DEFUN([CONFIG_LDPATH],
[
  AC_DEFINE([RUN_LDPATH])

  dnl The initial value is the user's LD_LIBRARY_PATH or DYLD_LIBRARY_PATH

  case $host in
  *darwin*) 
    RUN_LDPATH="$DYLD_LIBRARY_PATH"
    ;;
  *)
    RUN_LDPATH="$LD_LIBRARY_PATH"
    ;;
  esac

  AC_SUBST(RUN_LDPATH)
])


dnl CONFIG_ADD_LDPATH(path) adds "path" to list of paths used to set
dnl LD_LIBRARY_PATH.

AC_DEFUN([CONFIG_ADD_LDPATH],
[
  RUN_LDPATH="$RUN_LDPATH:$1"
])

