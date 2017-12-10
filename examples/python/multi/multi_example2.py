# 
# @file    multi_example2.cs
# @brief   multi create example
# @author  Sarah Keating
# @author  Frank Bergmann
# 
# <!--------------------------------------------------------------------------
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.
# 
# Copyright (C) 2009-2013 jointly by the following organizations:
#     1. California Institute of Technology, Pasadena, CA, USA
#     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
# 
# Copyright (C) 2006-2008 by the California Institute of Technology,
#     Pasadena, CA, USA 
# 
# Copyright (C) 2002-2005 jointly by the following organizations:
#     1. California Institute of Technology, Pasadena, CA, USA
#     2. Japan Science and Technology Agency, Japan
# 
# This library is free software; you can redistribute it and/or modify it
# under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation.  A copy of the license agreement is provided
# in the file named "LICENSE.txt" included with this software distribution
# and also available online as http://sbml.org/software/libsbml/license.html
# ------------------------------------------------------------------------ -->
# 

from libsbml import * 

sbmlns = SBMLNamespaces(3,1,"multi",1)

# create the document

document = SBMLDocument(sbmlns)

# set the required attribute to True
docPlug = document.getPlugin("multi")
docPlug.setRequired(True)

# create the Model

model=document.createModel()

# create a compartment

c = model.createCompartment()
c.setId("membrane")
c.setConstant(True)

# set the multi attribute isType via the compartmentPlugin
compPlug = c.getPlugin("multi")
compPlug.setIsType(True)

# create species
s = model.createSpecies()
s.setId("s1")
s.setCompartment("membrane")
s.setBoundaryCondition(False)
s.setHasOnlySubstanceUnits(False)
s.setConstant(False)

# create reaction
r = model.createReaction()
r.setId("r1")
r.setFast(False)
r.setReversible(False)

# createReactant
sr = r.createReactant()
sr.setId("sr1")
sr.setSpecies("s1")
sr.setConstant(False)

kl = r.createKineticLaw()

ci = ASTNode(AST_NAME)
ci.setName("s1")
astPlugin = ci.getPlugin("multi")
astPlugin.setSpeciesReference("r1")

ci1 = ASTNode(AST_NAME)
astPlugin1 = ci1.getPlugin("multi")
astPlugin1.setRepresentationType("sum")
ci1.setName("s1")

math = ASTNode(AST_TIMES)
math.addChild(ci)
math.addChild(ci1)

kl.setMath(math)

writeSBML(document,"multi_example2.xml")

