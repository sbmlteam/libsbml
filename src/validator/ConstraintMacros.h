/**
 * \file    ConstraintMacros.h
 * \brief   Defines the contstraint "language"
 * \author  Ben Bornstein
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


#undef START_CONSTRAINT
#undef END_CONSTRAINT
#undef EXTERN_CONSTRAINT
#undef pre
#undef inv
#undef inv_or
#undef fail


#ifndef AddingConstraintsToValidator


#define START_CONSTRAINT(Id, Typename, Varname)                      \
struct Constraint ## Typename ## Id: public TConstraint<Typename>    \
{                                                                    \
  Constraint ## Typename ## Id (Validator& V) : TConstraint<Typename>(Id, V) { } \
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
  addConstraint( new Constraint ## Typename ## Id (*this) ); \
  if (0) { const Model m; const Typename Varname; std::string msg;

#define END_CONSTRAINT }

#define EXTERN_CONSTRAINT(Id, Name) \
  addConstraint( new Name(Id, *this) );

#define pre(expr)
#define inv(expr)
#define inv_or(expr)
#define fail()


#endif  /* !AddingConstraintsToValidator */
