/**
 * \file    MathMLUnicodeConstants.h
 * \brief   MathML attribute and element names in (Xerces-C) Unicode.
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


#ifndef MathMLUnicodeConstants_h
#define MathMLUnicodeConstants_h


#include "common.hpp"


#ifdef USE_EXPAT
#  include "ExpatUnicodeChars.h"
#else
#  include <xercesc/util/XMLUniDefs.hpp>
   using namespace xercesc;
#endif  /* USE_EXPAT */


/**
 * http://www.w3.org/1998/Math/MathML
 */
static const XMLCh XMLNS_MathML[] =
{
  chLatin_h, chLatin_t, chLatin_t, chLatin_p,
  chColon, chForwardSlash, chForwardSlash,
  chLatin_w, chLatin_w, chLatin_w,
  chPeriod,
  chLatin_w, chDigit_3,
  chPeriod,
  chLatin_o, chLatin_r, chLatin_g,
  chForwardSlash,
  chDigit_1, chDigit_9, chDigit_9, chDigit_8,
  chForwardSlash,
  chLatin_M, chLatin_a, chLatin_t, chLatin_h,
  chForwardSlash,
  chLatin_M, chLatin_a, chLatin_t, chLatin_h, chLatin_M, chLatin_L,
  chNull
};


/**
 * http://www.sbml.org/sbml/symbols/delay
 */
static const XMLCh CSYMBOL_DEFINITION_URL_DELAY[] =
{
  chLatin_h, chLatin_t, chLatin_t, chLatin_p,
  chColon, chForwardSlash, chForwardSlash,
  chLatin_w, chLatin_w, chLatin_w,
  chPeriod,
  chLatin_s, chLatin_b, chLatin_m, chLatin_l,
  chPeriod,
  chLatin_o, chLatin_r, chLatin_g,
  chForwardSlash,
  chLatin_s, chLatin_b, chLatin_m, chLatin_l,
  chForwardSlash,
  chLatin_s, chLatin_y, chLatin_m, chLatin_b, chLatin_o, chLatin_l, chLatin_s,
  chForwardSlash,
  chLatin_d, chLatin_e, chLatin_l, chLatin_a, chLatin_y,
  chNull
};


/**
 * http://www.sbml.org/sbml/symbols/time
 */
static const XMLCh CSYMBOL_DEFINITION_URL_TIME[] =
{
  chLatin_h, chLatin_t, chLatin_t, chLatin_p,
  chColon, chForwardSlash, chForwardSlash,
  chLatin_w, chLatin_w, chLatin_w,
  chPeriod,
  chLatin_s, chLatin_b, chLatin_m, chLatin_l,
  chPeriod,
  chLatin_o, chLatin_r, chLatin_g,
  chForwardSlash,
  chLatin_s, chLatin_b, chLatin_m, chLatin_l,
  chForwardSlash,
  chLatin_s, chLatin_y, chLatin_m, chLatin_b, chLatin_o, chLatin_l, chLatin_s,
  chForwardSlash,
  chLatin_t, chLatin_i, chLatin_m, chLatin_e,
  chNull
};


static const XMLCh ELEM_ABS[] =
{
  chLatin_a, chLatin_b, chLatin_s, chNull
};

static const XMLCh ELEM_AND[] =
{
  chLatin_a, chLatin_n, chLatin_d, chNull
};

static const XMLCh ELEM_ANNOTATION[] =
{
  chLatin_a, chLatin_n, chLatin_n, chLatin_o, chLatin_t, chLatin_a, 
  chLatin_t, chLatin_i, chLatin_o, chLatin_n, chNull
};

static const XMLCh ELEM_ANNOTATION_XML[] =
{
  chLatin_a, chLatin_n, chLatin_n, chLatin_o, chLatin_t, chLatin_a, 
  chLatin_t, chLatin_i, chLatin_o, chLatin_n, chDash,
  chLatin_x, chLatin_m, chLatin_l, chNull
};

static const XMLCh ELEM_APPLY[] =
{
  chLatin_a, chLatin_p, chLatin_p, chLatin_l, chLatin_y, chNull
};

static const XMLCh ELEM_ARCCOS[] =
{
  chLatin_a, chLatin_r, chLatin_c, chLatin_c, chLatin_o, chLatin_s, 
  chNull
};

static const XMLCh ELEM_ARCCOSH[] =
{
  chLatin_a, chLatin_r, chLatin_c, chLatin_c, chLatin_o, chLatin_s, 
  chLatin_h, chNull
};

static const XMLCh ELEM_ARCCOT[] =
{
  chLatin_a, chLatin_r, chLatin_c, chLatin_c, chLatin_o, chLatin_t, 
  chNull
};

static const XMLCh ELEM_ARCCOTH[] =
{
  chLatin_a, chLatin_r, chLatin_c, chLatin_c, chLatin_o, chLatin_t, 
  chLatin_h, chNull
};

static const XMLCh ELEM_ARCCSC[] =
{
  chLatin_a, chLatin_r, chLatin_c, chLatin_c, chLatin_s, chLatin_c, 
  chNull
};

static const XMLCh ELEM_ARCCSCH[] =
{
  chLatin_a, chLatin_r, chLatin_c, chLatin_c, chLatin_s, chLatin_c, 
  chLatin_h, chNull
};

static const XMLCh ELEM_ARCSEC[] =
{
  chLatin_a, chLatin_r, chLatin_c, chLatin_s, chLatin_e, chLatin_c, 
  chNull
};

static const XMLCh ELEM_ARCSECH[] =
{
  chLatin_a, chLatin_r, chLatin_c, chLatin_s, chLatin_e, chLatin_c, 
  chLatin_h, chNull
};

static const XMLCh ELEM_ARCSIN[] =
{
  chLatin_a, chLatin_r, chLatin_c, chLatin_s, chLatin_i, chLatin_n, 
  chNull
};

static const XMLCh ELEM_ARCSINH[] =
{
  chLatin_a, chLatin_r, chLatin_c, chLatin_s, chLatin_i, chLatin_n, 
  chLatin_h, chNull
};

static const XMLCh ELEM_ARCTAN[] =
{
  chLatin_a, chLatin_r, chLatin_c, chLatin_t, chLatin_a, chLatin_n, 
  chNull
};

static const XMLCh ELEM_ARCTANH[] =
{
  chLatin_a, chLatin_r, chLatin_c, chLatin_t, chLatin_a, chLatin_n, 
  chLatin_h, chNull
};

static const XMLCh ELEM_BVAR[] =
{
  chLatin_b, chLatin_v, chLatin_a, chLatin_r, chNull
};

static const XMLCh ELEM_CEILING[] =
{
  chLatin_c, chLatin_e, chLatin_i, chLatin_l, chLatin_i, chLatin_n, 
  chLatin_g, chNull
};

static const XMLCh ELEM_CI[] =
{
  chLatin_c, chLatin_i, chNull
};

static const XMLCh ELEM_CN[] =
{
  chLatin_c, chLatin_n, chNull
};

static const XMLCh ELEM_COS[] =
{
  chLatin_c, chLatin_o, chLatin_s, chNull
};

static const XMLCh ELEM_COSH[] =
{
  chLatin_c, chLatin_o, chLatin_s, chLatin_h, chNull
};

static const XMLCh ELEM_COT[] =
{
  chLatin_c, chLatin_o, chLatin_t, chNull
};

static const XMLCh ELEM_COTH[] =
{
  chLatin_c, chLatin_o, chLatin_t, chLatin_h, chNull
};

static const XMLCh ELEM_CSC[] =
{
  chLatin_c, chLatin_s, chLatin_c, chNull
};

static const XMLCh ELEM_CSCH[] =
{
  chLatin_c, chLatin_s, chLatin_c, chLatin_h, chNull
};

static const XMLCh ELEM_CSYMBOL[] =
{
  chLatin_c, chLatin_s, chLatin_y, chLatin_m, chLatin_b, chLatin_o,
  chLatin_l, chNull
};

static const XMLCh ELEM_DEGREE[] =
{
  chLatin_d, chLatin_e, chLatin_g, chLatin_r, chLatin_e, chLatin_e, 
  chNull
};

static const XMLCh ELEM_DIVIDE[] =
{
  chLatin_d, chLatin_i, chLatin_v, chLatin_i, chLatin_d, chLatin_e, 
  chNull
};

static const XMLCh ELEM_EQ[] =
{
  chLatin_e, chLatin_q, chNull
};

static const XMLCh ELEM_EXP[] =
{
  chLatin_e, chLatin_x, chLatin_p, chNull
};

static const XMLCh ELEM_EXPONENTIALE[] =
{
  chLatin_e, chLatin_x, chLatin_p, chLatin_o, chLatin_n, chLatin_e, 
  chLatin_n, chLatin_t, chLatin_i, chLatin_a, chLatin_l, chLatin_e, 
  chNull
};

static const XMLCh ELEM_FACTORIAL[] =
{
  chLatin_f, chLatin_a, chLatin_c, chLatin_t, chLatin_o, chLatin_r, 
  chLatin_i, chLatin_a, chLatin_l, chNull
};

static const XMLCh ELEM_FALSE[] =
{
  chLatin_f, chLatin_a, chLatin_l, chLatin_s, chLatin_e, chNull
};


static const XMLCh ELEM_FLOOR[] =
{
  chLatin_f, chLatin_l, chLatin_o, chLatin_o, chLatin_r, chNull
};

static const XMLCh ELEM_GEQ[] =
{
  chLatin_g, chLatin_e, chLatin_q, chNull
};

static const XMLCh ELEM_GT[] =
{
  chLatin_g, chLatin_t, chNull
};

static const XMLCh ELEM_INFINITY[] =
{
  chLatin_i, chLatin_n, chLatin_f, chLatin_i, chLatin_n, chLatin_i, 
  chLatin_t, chLatin_y, chNull
};

static const XMLCh ELEM_LAMBDA[] =
{
  chLatin_l, chLatin_a, chLatin_m, chLatin_b, chLatin_d, chLatin_a,
  chNull
};

static const XMLCh ELEM_LEQ[] =
{
  chLatin_l, chLatin_e, chLatin_q, chNull
};

static const XMLCh ELEM_LN[] =
{
  chLatin_l, chLatin_n, chNull
};

static const XMLCh ELEM_LOG[] =
{
  chLatin_l, chLatin_o, chLatin_g, chNull
};

static const XMLCh ELEM_LOGBASE[] =
{
  chLatin_l, chLatin_o, chLatin_g, chLatin_b, chLatin_a, chLatin_s, 
  chLatin_e, chNull
};

static const XMLCh ELEM_LT[] =
{
  chLatin_l, chLatin_t, chNull
};

static const XMLCh ELEM_MATH[] =
{
  chLatin_m, chLatin_a, chLatin_t, chLatin_h, chNull
};

static const XMLCh ELEM_MINUS[] =
{
  chLatin_m, chLatin_i, chLatin_n, chLatin_u, chLatin_s, chNull
};

static const XMLCh ELEM_NEQ[] =
{
  chLatin_n, chLatin_e, chLatin_q, chNull
};

static const XMLCh ELEM_NOT[] =
{
  chLatin_n, chLatin_o, chLatin_t, chNull
};

static const XMLCh ELEM_NOT_A_NUMBER[] =
{
  chLatin_n, chLatin_o, chLatin_t,
  chLatin_a,
  chLatin_n, chLatin_u, chLatin_m, chLatin_b, chLatin_e, chLatin_r, chNull
};

static const XMLCh ELEM_OR[] =
{
  chLatin_o, chLatin_r, chNull
};

static const XMLCh ELEM_OTHERWISE[] =
{
  chLatin_o, chLatin_t, chLatin_h, chLatin_e, chLatin_r, chLatin_w,
  chLatin_i, chLatin_s, chLatin_e, chNull
};

static const XMLCh ELEM_PI[] =
{
  chLatin_p, chLatin_i, chNull
};

static const XMLCh ELEM_PIECE[] =
{
  chLatin_p, chLatin_i, chLatin_e, chLatin_c, chLatin_e, chNull
};

static const XMLCh ELEM_PIECEWISE[] =
{
  chLatin_p, chLatin_i, chLatin_e, chLatin_c, chLatin_e, chLatin_w,
  chLatin_i, chLatin_s, chLatin_e, chNull
};

static const XMLCh ELEM_PLUS[] =
{
  chLatin_p,  chLatin_l, chLatin_u, chLatin_s, chNull
};

static const XMLCh ELEM_POWER[] =
{
  chLatin_p, chLatin_o, chLatin_w, chLatin_e, chLatin_r, chNull
};

static const XMLCh ELEM_ROOT[] =
{
  chLatin_r, chLatin_o, chLatin_o, chLatin_t, chNull
};

static const XMLCh ELEM_SEC[] =
{
  chLatin_s, chLatin_e, chLatin_c, chNull
};

static const XMLCh ELEM_SECH[] =
{
  chLatin_s, chLatin_e, chLatin_c, chLatin_h, chNull
};

static const XMLCh ELEM_SEMANTICS[] =
{
  chLatin_s, chLatin_e, chLatin_m, chLatin_a, chLatin_n, chLatin_t, 
  chLatin_i, chLatin_c, chLatin_s, chNull
};

static const XMLCh ELEM_SEP[] =
{
  chLatin_s, chLatin_e, chLatin_p, chNull
};

static const XMLCh ELEM_SIN[] =
{
  chLatin_s, chLatin_i, chLatin_n, chNull
};

static const XMLCh ELEM_SINH[] =
{
  chLatin_s, chLatin_i, chLatin_n, chLatin_h, chNull
};

static const XMLCh ELEM_TAN[] =
{
  chLatin_t, chLatin_a, chLatin_n, chNull
};

static const XMLCh ELEM_TANH[] =
{
  chLatin_t, chLatin_a, chLatin_n, chLatin_h, chNull
};

static const XMLCh ELEM_TIMES[] =
{
  chLatin_t, chLatin_i, chLatin_m, chLatin_e, chLatin_s, chNull
};

static const XMLCh ELEM_TRUE[] =
{
  chLatin_t, chLatin_r, chLatin_u, chLatin_e, chNull
};

static const XMLCh ELEM_XOR[] =
{
  chLatin_x, chLatin_o, chLatin_r, chNull
};

static const XMLCh ATTR_DEFINITION_URL[] =
{
  chLatin_d, chLatin_e, chLatin_f, chLatin_i, chLatin_n, chLatin_i,
  chLatin_t, chLatin_i, chLatin_o, chLatin_n, chLatin_U, chLatin_R,
  chLatin_L, chNull
};

static const XMLCh ATTR_ENCODING[] =
{
  chLatin_e, chLatin_n, chLatin_c, chLatin_o, chLatin_d, chLatin_i, 
  chLatin_n, chLatin_g, chNull
};

static const XMLCh ATTR_TYPE[] =
{
  chLatin_t, chLatin_y, chLatin_p, chLatin_e, chNull
};

static const XMLCh ATTR_XMLNS[] =
{
  chLatin_x, chLatin_m, chLatin_l, chLatin_n, chLatin_s, chNull
};

static const XMLCh VAL_E_NOTATION[] =
{
  chLatin_e, chDash   , chLatin_n, chLatin_o, chLatin_t, chLatin_a,
  chLatin_t, chLatin_i, chLatin_o, chLatin_n, chNull
};

static const XMLCh VAL_INTEGER[] =
{
  chLatin_i, chLatin_n, chLatin_t, chLatin_e, chLatin_g, chLatin_e,
  chLatin_r, chNull
};

static const XMLCh VAL_NOT_A_NUMBER[] =
{
  chLatin_n, chLatin_o, chLatin_t, chLatin_a, chLatin_n, chLatin_u, 
  chLatin_m, chLatin_b, chLatin_e, chLatin_r, chNull
};

static const XMLCh VAL_RATIONAL[] =
{
  chLatin_r, chLatin_a, chLatin_t, chLatin_i, chLatin_o, chLatin_n,
  chLatin_a, chLatin_l, chNull
};

static const XMLCh VAL_REAL[] =
{
  chLatin_r, chLatin_e, chLatin_a, chLatin_l, chNull
};

static const XMLCh VAL_TEXT[] =
{
  chLatin_t, chLatin_e, chLatin_x, chLatin_t, chNull
};


#endif  /* MathMLUnicodeConstants_h */
