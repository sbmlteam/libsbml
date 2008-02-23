/**
 * \file    libsbml.h
 * \brief   Language-independent SWIG includes for wrapping libSBML
 * \author  Ben Bornstein
 *
 * $Id$
 * $Source$
 */
/* Copyright 2004 California Institute of Technology and Japan Science and
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


#include <sbml/SBMLReader.h>
#include <sbml/SBMLWriter.h>
#include <sbml/UnitKind.h>
#include <sbml/SBMLTypeCodes.h>
#include <sbml/SBase.h>
#include <sbml/ListOf.h>
#include <sbml/Model.h>
#include <sbml/SBMLDocument.h>
#include <sbml/SBMLError.h>
#include <sbml/SBMLErrorLog.h>
#include <sbml/FunctionDefinition.h>
#include <sbml/Unit.h>
#include <sbml/UnitDefinition.h>
#include <sbml/CompartmentType.h>
#include <sbml/SpeciesType.h>
#include <sbml/Compartment.h>
#include <sbml/Species.h>
#include <sbml/Parameter.h>
#include <sbml/InitialAssignment.h>
#include <sbml/Rule.h>
#include <sbml/Constraint.h>
#include <sbml/Reaction.h>
#include <sbml/KineticLaw.h>
#include <sbml/SpeciesReference.h>
#include <sbml/Event.h>
#include <sbml/EventAssignment.h>
#include <sbml/Trigger.h>
#include <sbml/Delay.h>
#include <sbml/CompartmentType.h>
#include <sbml/Constraint.h>
#include <sbml/InitialAssignment.h>
#include <sbml/SpeciesType.h>
#include <sbml/SBO.h>
#include <sbml/StoichiometryMath.h>

#include <sbml/math/ASTNode.h>
#include <sbml/math/MathML.h>
#include <sbml/math/FormulaFormatter.h>
#include <sbml/math/FormulaParser.h>

#include <sbml/xml/XMLAttributes.h>
#include <sbml/xml/XMLNamespaces.h>
#include <sbml/xml/XMLToken.h>
#include <sbml/xml/XMLNode.h>
#include <sbml/xml/XMLTriple.h>
#include <sbml/xml/XMLInputStream.h>
#include <sbml/xml/XMLOutputStream.h>
#include <sbml/xml/XMLError.h>
#include <sbml/xml/XMLErrorLog.h>
#include <sbml/xml/XMLParser.h>
#include <sbml/xml/XMLHandler.h>
#include <sbml/xml/XMLTokenizer.h>

#include <sbml/annotation/CVTerm.h>
#include <sbml/annotation/ModelHistory.h>
#include <sbml/annotation/RDFAnnotation.h>
