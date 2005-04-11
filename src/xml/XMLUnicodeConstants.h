/**
 * \file    XMLUnicodeConstants.h
 * \brief   Common XML strings in (Xerces-C) Unicode.
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


#ifndef XMLUnicodeConstants_h
#define XMLUnicodeConstants_h


#include "common/common.hpp"


#ifdef USE_EXPAT
#  include "ExpatUnicodeChars.h"
#else
#  include <xercesc/util/XMLUniDefs.hpp>
   using namespace xercesc;
#endif  /* USE_EXPAT */


/**
 * <?xml version="1.0" encoding="
 */
static const XMLCh XML_DECL_1[] =
{
  chOpenAngle, chQuestion,

  chLatin_x, chLatin_m, chLatin_l, chSpace,

  chLatin_v, chLatin_e, chLatin_r, chLatin_s, chLatin_i, chLatin_o,
  chLatin_n,

  chEqual,

  chDoubleQuote, chDigit_1, chPeriod, chDigit_0, chDoubleQuote, chSpace,

  chLatin_e, chLatin_n, chLatin_c, chLatin_o, chLatin_d, chLatin_i,
  chLatin_n, chLatin_g,

  chEqual, chDoubleQuote, chNull
};


/**
 * "?>
 */
static const XMLCh XML_DECL_2[] =
{
  chDoubleQuote, chQuestion, chCloseAngle, chLF, chNull
};


/**
 * 1.0
 */
static const XMLCh XML_VERSION_1_0[] =
{
  chDigit_1, chPeriod, chDigit_0, chNull
};


/**
 * 1.1
 */
static const XMLCh XML_VERSION_1_1[] =
{
  chDigit_1, chPeriod, chDigit_1, chNull
};


/**
 * "<!-- Created by "
 */
static const XMLCh XML_COMMENT_1[] =
{
  chOpenAngle, chBang, chDash, chDash, chSpace, 
  chLatin_C, chLatin_r, chLatin_e, chLatin_a, chLatin_t, chLatin_e, chLatin_d,
  chSpace,
  chLatin_b, chLatin_y,
  chSpace, chNull
};


/**
 * " version "
 */
static const XMLCh XML_COMMENT_2[] =
{
  chSpace,
  chLatin_v, chLatin_e, chLatin_r, chLatin_s, chLatin_i, chLatin_o, chLatin_n,
  chSpace, chNull
};


/**
 * " on "
 */
static const XMLCh XML_COMMENT_3[] =
{
  chSpace, chLatin_o, chLatin_n, chSpace, chNull
};


/**
 * " with libsbml version "
 */
static const XMLCh XML_COMMENT_4[] =
{
  chSpace,
  chLatin_w, chLatin_i, chLatin_t, chLatin_h,
  chSpace,
  chLatin_l, chLatin_i, chLatin_b, chLatin_s, chLatin_b, chLatin_m, chLatin_l,
  chSpace,
  chLatin_v, chLatin_e, chLatin_r, chLatin_s, chLatin_i, chLatin_o, chLatin_n,
  chSpace, chNull
};


/**
 * " -->\n"
 */
static const XMLCh XML_COMMENT_5[] =
{
  chPeriod, chSpace, chDash, chDash, chCloseAngle, chLF, chNull
};


#endif  /* XMLUnicodeConstants_h */
