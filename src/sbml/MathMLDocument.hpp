/**
 * Filename    : MathMLDocument.hpp
 * Description : Top-level container for all things MathML
 * Author(s)   : SBML Development Group <sysbio-team@caltech.edu>
 * Organization: JST ERATO Kitano Symbiotic Systems Project
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
 *     The Systems Biology Markup Language Development Group
 *     ERATO Kitano Symbiotic Systems Project
 *     Control and Dynamical Systems, MC 107-81
 *     California Institute of Technology
 *     Pasadena, CA, 91125, USA
 *
 *     http://www.cds.caltech.edu/erato
 *     mailto:sysbio-team@caltech.edu
 *
 * Contributor(s):
 */


#ifndef MathMLDocument_hpp
#define MathMLDocument_hpp


#include "sbml/ASTNode.hpp"


class MathMLDocument
{
public:

  /**
   * Creates a new MathMLDocument.
   */
  LIBSBML_EXTERN
  MathMLDocument ();

  /**
   * Destroys this MathMLDocument.
   */
  LIBSBML_EXTERN
  virtual ~MathMLDocument ();


  /**
   * @return the an abstract syntax tree (AST) representation of the math
   * in this MathMLDocument.
   */
  LIBSBML_EXTERN
  const ASTNode* getMath () const;

  /**
   * @return true if the math of this MathMLDocument has been set, false
   * otherwise.
   */
  LIBSBML_EXTERN
  bool isSetMath () const;

  /**
   * Sets the math of this MathMLDocument to the given ASTNode.
   *
   * The node <b>is not copied</b> and this MathMLDocument <b>takes
   * ownership</b> of it; i.e. subsequent calls to this function or a call
   * to MathMLDocument_free() will free the ASTNode (and any child nodes).
   */
  LIBSBML_EXTERN
  void setMath (ASTNode* math);


protected:

  ASTNode* math;


  friend class SBMLHandler;
};


#endif  // MathMLDocument_hpp
