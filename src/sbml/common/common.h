/**
 * @file    common.h
 * @brief   Common functions and macros for C and C++.
 * @author  Ben Bornstein
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2009-2011 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EBML-EBI), Hinxton, UK
 *  
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 *     Pasadena, CA, USA 
 *  
 * Copyright (C) 2002-2005 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. Japan Science and Technology Agency, Japan
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is provided
 * in the file named "LICENSE.txt" included with this software distribution and
 * also available online as http://sbml.org/software/libsbml/license.html
 * ---------------------------------------------------------------------- -->*/

#ifndef LIBSBML_COMMON_H
#define LIBSBML_COMMON_H 1


#include <sbml/common/libsbml-config.h>


#include <stdio.h>


#if STDC_HEADERS
#  include <errno.h>
#  include <float.h>
#  include <stdarg.h>
#  include <stdlib.h>
#  include <string.h>
#endif

#if HAVE_MATH_H
#  include <math.h>
#endif

#if HAVE_IEEFP_H
#  include <ieeefp.h>
#endif

#ifndef errno
  extern int errno;
#endif


/**
 * Default format used by libSBML when writing out floating-point numbers
 * into the XML produced by libSBML.  This is used by
 * StringBuffer_appendReal.
 */
#define LIBSBML_FLOAT_FORMAT "%.15g"

#define LIBSBML_DOUBLE_PRECISION 15

static const int SBML_INT_MAX = 2147483647;
static const int SBML_INT_MIN = -2147483647 - 1;


#include <sbml/common/extern.h>
#include <sbml/util/memory.h>
#include <sbml/util/util.h>


#endif  /* LIBSBML_COMMON_H */
