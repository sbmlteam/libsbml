/**
 * Filename    : MathMLTagCodes.cpp
 * Description : Maps MathML elements to numbers for comparing and storing
 * Author(s)   : SBW Development Group <sysbio-team@caltech.edu>
 * Organization: Caltech ERATO Kitano Systems Biology Project
 * Created     : 2003-05-06
 * Revision    : $Id$
 * Source      : $Source$
 *
 * Copyright 2003 California Institute of Technology and
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
 *     The Systems Biology Workbench Development Group
 *     ERATO Kitano Systems Biology Project
 *     Control and Dynamical Systems, MC 107-81
 *     California Institute of Technology
 *     Pasadena, CA, 91125, USA
 *
 *     http://www.cds.caltech.edu/erato
 *     mailto:sysbio-team@caltech.edu
 *
 * Contributor(s):
 */


#include <iostream>

#include "sbml/common.h"

#include <xercesc/util/XMLString.hpp>
#include <xercesc/util/XMLUniDefs.hpp>

#include "sbml/MathMLTagCodes.hpp"
#include "sbml/MathMLUnicodeConstants.hpp"


static const XMLCh* MATHML_ELEMENTS[] =
{
    ELEM_ABS
  , ELEM_AND
  , ELEM_ANNOTATION
  , ELEM_ANNOTATION_XML
  , ELEM_APPLY
  , ELEM_ARCCOS
  , ELEM_ARCCOSH
  , ELEM_ARCCOT
  , ELEM_ARCCOTH
  , ELEM_ARCCSC
  , ELEM_ARCCSCH
  , ELEM_ARCSEC
  , ELEM_ARCSECH
  , ELEM_ARCSIN
  , ELEM_ARCSINH
  , ELEM_ARCTAN
  , ELEM_ARCTANH
  , ELEM_BVAR
  , ELEM_CEILING
  , ELEM_CI
  , ELEM_CN
  , ELEM_COS
  , ELEM_COSH
  , ELEM_COT
  , ELEM_COTH
  , ELEM_CSC
  , ELEM_CSCH
  , ELEM_CSYMBOL
  , ELEM_DEGREE
  , ELEM_DIVIDE
  , ELEM_EQ
  , ELEM_EXP
  , ELEM_EXPONENTIALE
  , ELEM_FACTORIAL
  , ELEM_FALSE
  , ELEM_FLOOR
  , ELEM_GEQ
  , ELEM_GT
  , ELEM_INFINITY
  , ELEM_LAMBDA
  , ELEM_LEQ
  , ELEM_LN
  , ELEM_LOG
  , ELEM_LOGBASE
  , ELEM_LT
  , ELEM_MATH
  , ELEM_MINUS
  , ELEM_NEQ
  , ELEM_NOT
  , ELEM_NOT_A_NUMBER
  , ELEM_OR
  , ELEM_OTHERWISE
  , ELEM_PI
  , ELEM_PIECE
  , ELEM_PIECEWISE
  , ELEM_PLUS
  , ELEM_POWER
  , ELEM_ROOT
  , ELEM_SEC
  , ELEM_SECH
  , ELEM_SEMANTICS
  , ELEM_SEP
  , ELEM_SIN
  , ELEM_SINH
  , ELEM_TAN
  , ELEM_TANH
  , ELEM_TIMES
  , ELEM_TRUE
  , ELEM_XOR
};


/**
 * Returns the tag code for the given SBML element.
 */
MathMLTagCode_t
MathMLTagCode_forElement (const XMLCh* name)
{
  MathMLTagCode_t hc  = MATHML_TAG_UNKNOWN;
  MathMLTagCode_t lo  = MATHML_TAG_ABS;
  MathMLTagCode_t hi  = MATHML_TAG_XOR;
  MathMLTagCode_t mid;

  int cond;


  /* Proceed iff name is not NULL and name is not an empty string */
  if (name && *name)
  {
    /* Classic Binary Search */
    while (lo <= hi)
    {
      mid  = (lo + hi) / 2;
      cond = XMLString::compareString(name, MATHML_ELEMENTS[mid]);
      
      if (cond < 0)
      {
        hi = mid - 1;
      }
      else if (cond > 0)
      {
        lo = mid + 1;
      }
      else
      {
        hc = mid;
        break;
      }
    }
  }

  return hc;
}
