/**
 * @file    L2v1CompatibilityValidator.h
 * @brief   Checks whether an SBML model can be converted from L2v2/3 to L2v1
 * @author  Sarah Keating
 *
 * $Id$
 * $Source$
 */
/* Copyright 2005 California Institute of Technology and Japan Science and
 * Technology Corporation.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is
 * provided in the file named "LICENSE.txt" included with this software
 * distribution.  It is also available online at
 * http://sbml.org/software/libsbml/license.html
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 */


#ifndef L2v1CompatibilityValidator_h
#define L2v1CompatibilityValidator_h


#ifdef __cplusplus


#include <sbml/validator/Validator.h>


class L2v1CompatibilityValidator: public Validator
{
public:

  L2v1CompatibilityValidator () :
    Validator("http://sbml.org/validator/compatibility/L2v1") { }

  virtual ~L2v1CompatibilityValidator () { }

  /**
   * Initializes this Validator with a set of Constraints.
   */
  virtual void init ();
};


#endif  /* __cplusplus */
#endif  /* L2v1CompatibilityValidator_h */
