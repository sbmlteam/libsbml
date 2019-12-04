/**
 * @cond doxygenLibsbmlInternal
 *
 * @file    L3v2CompatibilityConstraintsDeclared.cxx
 * @brief   Declarations of constraints
 * @author  SBML Team
 * 
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2019 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
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
 * in the file named "LICENSE.txt" included with this software distribution
 * and also available online as http://sbml.org/software/libsbml/license.html
 * ---------------------------------------------------------------------- -->*/

addConstraint(new VConstraintModel96001(*this));

addConstraint(new VConstraintModel96002(*this));

addConstraint(new VConstraintUnit96003(*this));

addConstraint(new VConstraintKineticLaw96004(*this));

addConstraint(new VConstraintKineticLaw96005(*this));

addConstraint(new VConstraintSpecies96006(*this));

addConstraint(new VConstraintEvent96007(*this));

addConstraint(new VConstraintModel96008(*this));

addConstraint(new DuplicateTopLevelAnnotation(96009, *this));

addConstraint(new VConstraintCompartment96010(*this));

addConstraint(new VConstraintReaction98010(*this));
/** @endcond */

