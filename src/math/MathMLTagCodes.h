/**
 * \file    MathMLTagCodes.c
 * \brief   Maps MathML elements to numbers for comparing and storing
 * \author  Ben Bornstein
 * 
 * $Id$
 * $Source$
 */
/* Copyright 2003 California Institute of Technology and
 * Japan Science and Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 2.1 of the License, or
 * any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY, WITHOUT EVEN THE IMPLIED WARRANTY OF
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  The software and
 * documentation provided hereunder is on an "as is" basis, and the
 * California Institute of Technology and Japan Science and Technology
 * Corporation have no obligations to provide maintenance, support,
 * updates, enhancements or modifications.  In no event shall the
 * California Institute of Technology or the Japan Science and Technology
 * Corporation be liable to any party for direct, indirect, special,
 * incidental or consequential damages, including lost profits, arising
 * out of the use of this software and its documentation, even if the
 * California Institute of Technology and/or Japan Science and Technology
 * Corporation have been advised of the possibility of such damage.  See
 * the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 *
 * The original code contained here was initially developed by:
 *
 *     Ben Bornstein
 *     The Systems Biology Markup Language Development Group
 *     ERATO Kitano Symbiotic Systems Project
 *     Control and Dynamical Systems, MC 107-81
 *     California Institute of Technology
 *     Pasadena, CA, 91125, USA
 *
 *     http://www.cds.caltech.edu/erato
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s):
 *   Stefan Hoops
 */


#ifndef MathMLTagCodes_hpp
#define MathMLTagCodes_hpp


#include "common/common.hpp"


#ifdef USE_EXPAT
#  include <expat.h>
#else
#  include <xercesc/util/XMLUniDefs.hpp>
#endif  /* USE_EXPAT */


typedef long MathMLTagCode_t;

static const MathMLTagCode_t MATHML_TAG_ABS            =  0;
static const MathMLTagCode_t MATHML_TAG_AND            =  1;
static const MathMLTagCode_t MATHML_TAG_ANNOTATION     =  2;
static const MathMLTagCode_t MATHML_TAG_ANNOTATION_XML =  3;
static const MathMLTagCode_t MATHML_TAG_APPLY          =  4;
static const MathMLTagCode_t MATHML_TAG_ARCCOS         =  5;
static const MathMLTagCode_t MATHML_TAG_ARCCOSH        =  6;
static const MathMLTagCode_t MATHML_TAG_ARCCOT         =  7;
static const MathMLTagCode_t MATHML_TAG_ARCCOTH        =  8;
static const MathMLTagCode_t MATHML_TAG_ARCCSC         =  9;
static const MathMLTagCode_t MATHML_TAG_ARCCSCH        = 10;
static const MathMLTagCode_t MATHML_TAG_ARCSEC         = 11;
static const MathMLTagCode_t MATHML_TAG_ARCSECH        = 12;
static const MathMLTagCode_t MATHML_TAG_ARCSIN         = 13;
static const MathMLTagCode_t MATHML_TAG_ARCSINH        = 14;
static const MathMLTagCode_t MATHML_TAG_ARCTAN         = 15;
static const MathMLTagCode_t MATHML_TAG_ARCTANH        = 16;
static const MathMLTagCode_t MATHML_TAG_BVAR           = 17;
static const MathMLTagCode_t MATHML_TAG_CEILING        = 18;
static const MathMLTagCode_t MATHML_TAG_CI             = 19;
static const MathMLTagCode_t MATHML_TAG_CN             = 20;
static const MathMLTagCode_t MATHML_TAG_COS            = 21;
static const MathMLTagCode_t MATHML_TAG_COSH           = 22;
static const MathMLTagCode_t MATHML_TAG_COT            = 23;
static const MathMLTagCode_t MATHML_TAG_COTH           = 24;
static const MathMLTagCode_t MATHML_TAG_CSC            = 25;
static const MathMLTagCode_t MATHML_TAG_CSCH           = 26;
static const MathMLTagCode_t MATHML_TAG_CSYMBOL        = 27;
static const MathMLTagCode_t MATHML_TAG_DEGREE         = 28;
static const MathMLTagCode_t MATHML_TAG_DIVIDE         = 29;
static const MathMLTagCode_t MATHML_TAG_EQ             = 30;
static const MathMLTagCode_t MATHML_TAG_EXP            = 31;
static const MathMLTagCode_t MATHML_TAG_EXPONENTIALE   = 32;
static const MathMLTagCode_t MATHML_TAG_FACTORIAL      = 33;
static const MathMLTagCode_t MATHML_TAG_FALSE          = 34;
static const MathMLTagCode_t MATHML_TAG_FLOOR          = 35;
static const MathMLTagCode_t MATHML_TAG_GEQ            = 36;
static const MathMLTagCode_t MATHML_TAG_GT             = 37;
static const MathMLTagCode_t MATHML_TAG_INFINITY       = 38;
static const MathMLTagCode_t MATHML_TAG_LAMBDA         = 39;
static const MathMLTagCode_t MATHML_TAG_LEQ            = 40;
static const MathMLTagCode_t MATHML_TAG_LN             = 41;
static const MathMLTagCode_t MATHML_TAG_LOG            = 42;
static const MathMLTagCode_t MATHML_TAG_LOGBASE        = 43;
static const MathMLTagCode_t MATHML_TAG_LT             = 44;
static const MathMLTagCode_t MATHML_TAG_MATH           = 45;
static const MathMLTagCode_t MATHML_TAG_MINUS          = 46;
static const MathMLTagCode_t MATHML_TAG_NEQ            = 47;
static const MathMLTagCode_t MATHML_TAG_NOT            = 48;
static const MathMLTagCode_t MATHML_TAG_NOT_A_NUMBER   = 49;
static const MathMLTagCode_t MATHML_TAG_OR             = 50;
static const MathMLTagCode_t MATHML_TAG_OTHERWISE      = 51;
static const MathMLTagCode_t MATHML_TAG_PI             = 52;
static const MathMLTagCode_t MATHML_TAG_PIECE          = 53;
static const MathMLTagCode_t MATHML_TAG_PIECEWISE      = 54;
static const MathMLTagCode_t MATHML_TAG_PLUS           = 55;
static const MathMLTagCode_t MATHML_TAG_POWER          = 56;
static const MathMLTagCode_t MATHML_TAG_ROOT           = 57;
static const MathMLTagCode_t MATHML_TAG_SEC            = 58;
static const MathMLTagCode_t MATHML_TAG_SECH           = 59;
static const MathMLTagCode_t MATHML_TAG_SEMANTICS      = 60;
static const MathMLTagCode_t MATHML_TAG_SEP            = 61;
static const MathMLTagCode_t MATHML_TAG_SIN            = 62;
static const MathMLTagCode_t MATHML_TAG_SINH           = 63;
static const MathMLTagCode_t MATHML_TAG_TAN            = 64;
static const MathMLTagCode_t MATHML_TAG_TANH           = 65;
static const MathMLTagCode_t MATHML_TAG_TIMES          = 66;
static const MathMLTagCode_t MATHML_TAG_TRUE           = 67;
static const MathMLTagCode_t MATHML_TAG_XOR            = 68;
static const MathMLTagCode_t MATHML_TAG_UNKNOWN        = 69;



/**
 * Returns the tag code for the given MathML element.
 */
MathMLTagCode_t
MathMLTagCode_forElement (const XMLCh* name);


#endif  /* MathMLTagCodes_h */
