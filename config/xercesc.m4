##
## Filename    : xercesc.m4
## Description : Autoconf macro to check for existence of Xerces-C library
## Description : configure script.
## Author(s)   : SBW Development Group <sysbio-team@caltech.edu>
## Organization: Caltech ERATO Kitano Systems Biology Project
## Created     : 2003-02-14
## Revision    : $Id$
## Source      : $Source$
##
## Copyright 2002 California Institute of Technology and
## Japan Science and Technology Corporation.
##
## This library is free software; you can redistribute it and/or modify it
## under the terms of the GNU Lesser General Public License as published
## by the Free Software Foundation; either version 2.1 of the License, or
## any later version.
##
## This library is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY, WITHOUT EVEN THE IMPLIED WARRANTY OF
## MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  The software and
## documentation provided hereunder is on an "as is" basis, and the
## California Institute of Technology and Japan Science and Technology
## Corporation have no obligations to provide maintenance, support,
## updates, enhancements or modifications.  In no event shall the
## California Institute of Technology or the Japan Science and Technology
## Corporation be liable to any party for direct, indirect, special,
## incidental or consequential damages, including lost profits, arising
## out of the use of this software and its documentation, even if the
## California Institute of Technology and/or Japan Science and Technology
## Corporation have been advised of the possibility of such damage.  See
## the GNU Lesser General Public License for more details.
##
## You should have received a copy of the GNU Lesser General Public License
## along with this library; if not, write to the Free Software Foundation,
## Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
##
## The original code contained here was initially developed by:
##
##     Ben Bornstein
##     The Systems Biology Workbench Development Group
##     ERATO Kitano Systems Biology Project
##     Control and Dynamical Systems, MC 107-81
##     California Institute of Technology
##     Pasadena, CA, 91125, USA
##
##     http://www.cds.caltech.edu/erato
##     mailto:sysbio-team@caltech.edu
##
## Contributor(s):
##


##
## The line LIBS="$LIBS -lxerces-c" appears in two places below.  This
## duplication is necessary!  Why? 
##
##
## LIBS must be set before AC_TRY_LINK.  But, side-effects in
## AC_CACHE_CHECK are not guaranteed.  If a cached value exists for
## sbml_cv_lib_xercesc, the body of AC_CACHE_CHECK will not be
## executed.  For the change to LIBS to take effect in both cases
## (whether tested or retrieved from cache) LIBS must be set soley on
## the basis of sbml_cv_lib_xercesc.
##

AC_DEFUN([SBML_CHECK_LIB_XERCESC],
  [AC_CACHE_CHECK([for Apache's Xerces-C XML library],
    [sbml_cv_lib_xercesc],
    [AC_LANG_SAVE
     AC_LANG_CPLUSPLUS
     sbml_save_LIBS=$LIBS
     LIBS="$LIBS -lxerces-c"
     AC_TRY_LINK([#include <xercesc/util/PlatformUtils.hpp>],
       [XMLPlatformUtils::Initialize();],
       [sbml_cv_lib_xercesc=yes],
       [sbml_cv_lib_xercesc=no])
     LIBS=$sbml_save_LIBS
     AC_LANG_RESTORE
  ])

if test x"$sbml_cv_lib_xercesc" = xyes; then
  LIBS="$LIBS -lxerces-c"
else
  AC_MSG_ERROR([cannot find libxerces-c])
fi

])
