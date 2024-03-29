#!/usr/bin/env python3

#
# \file    example1-L3.py
# \brief   SBML Layout example
# \author  Ralph Gauges
# \author  Akiya Jouraku
# \author  Frank Bergmann (adapted to use libsbml-5 package version)
# 

# Copyright 2004 European Media Laboratories Research gGmbH
#
# This library is free software; you can redistribute it and/or modify it
# under the terms of the GNU Lesser General Public License as published
# by the Free Software Foundation; either version 2.1 of the License, or
# any later version.
#
# This library is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY, WITHOUT EVEN THE IMPLIED WARRANTY OF
# MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  The software and
# documentation provided hereunder is on an "as is" basis, and the
# European Media Laboratories Research gGmbH have no obligations to
# provide maintenance, support, updates, enhancements or modifications.
# In no event shall the European Media Laboratories Research gGmbH be
# liable to any party for direct, indirect, special, incidental or
# consequential damages, including lost profits, arising out of the use of
# this software and its documentation, even if the European Media
# Laboratories Research gGmbH have been advised of the possibility of such
# damage.  See the GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with this library; if not, write to the Free Software Foundation,
# Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
#
# The original code contained here was initially developed by:
#
#     Ralph Gaugess
#     Bioinformatics Group
#     European Media Laboratories Research gGmbH
#     Schloss-Wolfsbrunnenweg 31c
#     69118 Heidelberg
#     Germany
#
#     http://www.eml-research.de/english/Research/BCB/
#     mailto:ralph.gauges@eml-r.villa-bosch.de
#
# Contributor(s):
#


from libsbml import *

# Creates an SBMLNamespaces object with the given SBML level, version
# package name, package version.
# 
# (NOTE) By default, the name of package (i.e. "layout") will be used
# if the argument for the prefix is missing or empty. Thus the argument
# for the prefix can be added as follows:
# 
#    SBMLNamespaces sbmlns(3,1,"layout",1,"LAYOUT")
# 
sbmlns = SBMLNamespaces(3, 1, "layout", 1)
# create the document
document = SBMLDocument(sbmlns)
# set the "required" attribute of layout package  to "true"
document.setPkgRequired("layout", True)

# create the Model

model = document.createModel()
model.setId("TestModel")
document.setModel(model)

# create the Compartment

compartment = model.createCompartment()
compartment.setId("Compartment_1")
compartment.setConstant(True)
# create the Species

species1 = model.createSpecies()
species1.setId("Species_1")
species1.setCompartment(compartment.getId())
species1.setHasOnlySubstanceUnits(False)
species1.setBoundaryCondition(False)
species1.setConstant(False)

species2 = model.createSpecies()
species2.setId("Species_2")
species2.setCompartment(compartment.getId())
species2.setCompartment(compartment.getId())
species2.setHasOnlySubstanceUnits(False)
species2.setBoundaryCondition(False)
species2.setConstant(False)

# create the Reactions

reaction1 = model.createReaction()
reaction1.setId("Reaction_1")
reaction1.setReversible(False)
reaction1.setFast(False)

reference1 = reaction1.createReactant()
reference1.setSpecies(species1.getId())
reference1.setId("SpeciesReference_1")
reference1.setConstant(False)

reference2 = reaction1.createProduct()
reference2.setSpecies(species2.getId())
reference2.setId("SpeciesReference_2")
reference2.setConstant(False)

reaction2 = model.createReaction()
reaction2.setId("Reaction_2")
reaction2.setReversible(False)
reaction2.setFast(False)

reference3 = reaction2.createReactant()
reference3.setSpecies(species2.getId())
reference3.setId("SpeciesReference_3")
reference3.setConstant(False)

reference4 = reaction2.createProduct()
reference4.setSpecies(species1.getId())
reference4.setId("SpeciesReference_4")
reference4.setConstant(False)

# create the Layout

#
# set the LayoutPkgNamespaces for Level 3 Version1 Layout Version 1
#
layoutns = LayoutPkgNamespaces(3, 1, 1)

#
# Get a LayoutModelPlugin object plugged in the model object.
#
# The type of the returned value of SBase::getPlugin() function is SBasePlugin, and
# thus the value needs to be casted for the corresponding derived class.
#

mplugin = (model.getPlugin("layout"))

if mplugin is None:
    print(
        "[Fatal Error] Layout Extension Level " + layoutns.getLevel() + " Version " + layoutns.getVersion() + " package version " + layoutns.getPackageVersion() + " is not registered.")
    sys.exit(1)

#
# Creates a Layout object via LayoutModelPlugin object.
#
layout = mplugin.createLayout()
layout.setId("Layout_1")
layout.setDimensions(Dimensions(layoutns, 400.0, 220.0))

# create the CompartmentGlyph

compartmentGlyph = layout.createCompartmentGlyph()
compartmentGlyph.setId("CompartmentGlyph_1")
compartmentGlyph.setCompartmentId(compartment.getId())
compartmentGlyph.setBoundingBox(BoundingBox(layoutns, "bb1", 5, 5, 390, 210))

# create the SpeciesGlyphs

speciesGlyph1 = layout.createSpeciesGlyph()
speciesGlyph1.setId("SpeciesGlyph_1")
speciesGlyph1.setSpeciesId(species1.getId())
speciesGlyph1.setBoundingBox(BoundingBox(layoutns, "bb2", 80, 26, 240, 24))

textGlyph1 = layout.createTextGlyph()
textGlyph1.setId("TextGlyph_01")
textGlyph1.setBoundingBox(BoundingBox(layoutns, "bbA", 92, 26, 228, 24))
textGlyph1.setOriginOfTextId(speciesGlyph1.getId())
textGlyph1.setGraphicalObjectId(speciesGlyph1.getId())

speciesGlyph2 = layout.createSpeciesGlyph()
speciesGlyph2.setId("SpeciesGlyph_2")
speciesGlyph2.setSpeciesId(species2.getId())
speciesGlyph2.setBoundingBox(BoundingBox(layoutns, "bb3", 80, 170, 240, 24))

textGlyph2 = layout.createTextGlyph()
textGlyph2.setId("TextGlyph_02")
textGlyph2.setBoundingBox(BoundingBox(layoutns, "bbB", 92, 170, 228, 24))
textGlyph2.setOriginOfTextId(speciesGlyph2.getId())
textGlyph2.setGraphicalObjectId(speciesGlyph2.getId())

# create the ReactionGlyphs

reactionGlyph1 = layout.createReactionGlyph()
reactionGlyph1.setId("ReactionGlyph_1")
reactionGlyph1.setReactionId(reaction1.getId())

reactionCurve1 = reactionGlyph1.getCurve()
ls = reactionCurve1.createLineSegment()
ls.setStart(Point(layoutns, 165, 105))
ls.setEnd(Point(layoutns, 165, 115))

reactionGlyph2 = layout.createReactionGlyph()
reactionGlyph2.setId("ReactionGlyph_1")
reactionGlyph2.setReactionId(reaction2.getId())

reactionCurve2 = reactionGlyph2.getCurve()
ls = reactionCurve2.createLineSegment()
ls.setStart(Point(layoutns, 235, 105))
ls.setEnd(Point(layoutns, 235, 115))

# add the SpeciesReferenceGlyphs

speciesReferenceGlyph1 = reactionGlyph1.createSpeciesReferenceGlyph()
speciesReferenceGlyph1.setId("SpeciesReferenceGlyph_1")
speciesReferenceGlyph1.setSpeciesGlyphId(speciesGlyph1.getId())
speciesReferenceGlyph1.setSpeciesReferenceId(reference1.getId())
speciesReferenceGlyph1.setRole(SPECIES_ROLE_SUBSTRATE)

speciesReferenceCurve1 = speciesReferenceGlyph1.getCurve()
cb = speciesReferenceCurve1.createCubicBezier()
cb.setStart(Point(layoutns, 165, 105))
cb.setBasePoint1(Point(layoutns, 165, 90))
cb.setBasePoint2(Point(layoutns, 165, 90))
cb.setEnd(Point(layoutns, 195, 60))

speciesReferenceGlyph2 = reactionGlyph1.createSpeciesReferenceGlyph()
speciesReferenceGlyph2.setId("SpeciesReferenceGlyph_2")
speciesReferenceGlyph2.setSpeciesGlyphId(speciesGlyph2.getId())
speciesReferenceGlyph2.setSpeciesReferenceId(reference2.getId())
speciesReferenceGlyph2.setRole(SPECIES_ROLE_PRODUCT)

speciesReferenceCurve2 = speciesReferenceGlyph2.getCurve()
cb = speciesReferenceCurve2.createCubicBezier()
cb.setStart(Point(layoutns, 165, 115))
cb.setBasePoint1(Point(layoutns, 165, 130))
cb.setBasePoint2(Point(layoutns, 165, 130))
cb.setEnd(Point(layoutns, 195, 160))

speciesReferenceGlyph3 = reactionGlyph2.createSpeciesReferenceGlyph()
speciesReferenceGlyph3.setId("SpeciesReferenceGlyph_3")
speciesReferenceGlyph3.setSpeciesGlyphId(speciesGlyph2.getId())
speciesReferenceGlyph3.setSpeciesReferenceId(reference3.getId())
speciesReferenceGlyph3.setRole(SPECIES_ROLE_SUBSTRATE)

speciesReferenceCurve3 = speciesReferenceGlyph3.getCurve()
cb = speciesReferenceCurve3.createCubicBezier()
cb.setStart(Point(layoutns, 235, 115))
cb.setBasePoint1(Point(layoutns, 235, 130))
cb.setBasePoint2(Point(layoutns, 235, 130))
cb.setEnd(Point(layoutns, 205, 160))

speciesReferenceGlyph4 = reactionGlyph2.createSpeciesReferenceGlyph()
speciesReferenceGlyph4.setId("SpeciesReferenceGlyph_4")
speciesReferenceGlyph4.setSpeciesGlyphId(speciesGlyph1.getId())
speciesReferenceGlyph4.setSpeciesReferenceId(reference4.getId())
speciesReferenceGlyph4.setRole(SPECIES_ROLE_PRODUCT)

speciesReferenceCurve4 = speciesReferenceGlyph4.getCurve()
cb = speciesReferenceCurve4.createCubicBezier()
cb.setStart(Point(layoutns, 235, 105))
cb.setBasePoint1(Point(layoutns, 235, 90))
cb.setBasePoint2(Point(layoutns, 235, 90))
cb.setEnd(Point(layoutns, 205, 60))

writeSBML(document, "layout_example1_L3-python.xml")
