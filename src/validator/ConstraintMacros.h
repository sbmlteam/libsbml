/**
 * \file    ConstraintMacros.h
 * \brief   Defines the contstraint "language"
 * \author  Ben Bornstein
 * 
 * $Id$
 * $Source$
 */
/* Copyright 2005 California Institute of Technology and
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
 *     http://www.sbml.org
 *     mailto:sbml-team@caltech.edu
 *
 * Contributor(s):
 */


#undef START_CONSTRAINT
#undef END_CONSTRAINT
#undef EXTERN_CONSTRAINT
#undef pre
#undef inv
#undef inv_or
#undef fail


#ifndef AddingConstraintsToValidator


#define START_CONSTRAINT(Id, Typename, Varname)                      \
struct Constraint ## Id: public TConstraint<Typename>                \
{                                                                    \
  Constraint ## Id (Validator& V) : TConstraint<Typename>(Id, V) { } \
protected:                                                           \
  void check_ (const Model& m, const Typename& Varname)

#define END_CONSTRAINT };

#define EXTERN_CONSTRAINT(Id, Name)

#define fail()       mLogMsg = true; return;
#define pre(expr)    if (!(expr)) return;
#define inv(expr)    if (!(expr)) { mLogMsg = true; return; }
#define inv_or(expr) if (expr) { mLogMsg = false; return; } else mLogMsg = true;


#else


#define START_CONSTRAINT(Id, Typename, Varname) \
  addConstraint( new Constraint ## Id (*this) ); \
  if (0) { const Model m; const Typename Varname; std::string msg;

#define END_CONSTRAINT }

#define EXTERN_CONSTRAINT(Id, Name) \
  addConstraint( new Name(Id, *this) );

#define pre(expr)
#define inv(expr)
#define inv_or(expr)
#define fail()


#endif  /* !AddingConstraintsToValidator */
