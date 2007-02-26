/**
 * \file    example3.cpp
 * \brief   SBML Layout example
 * \author  Ralph Gauges
 *
 * $Id$
 * $Source$
 */
/* Copyright 2004 European Media Laboratories Research gGmbH
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
 * European Media Laboratories Research gGmbH have no obligations to
 * provide maintenance, support, updates, enhancements or modifications.
 * In no event shall the European Media Laboratories Research gGmbH be
 * liable to any party for direct, indirect, special, incidental or
 * consequential damages, including lost profits, arising out of the use of
 * this software and its documentation, even if the European Media
 * Laboratories Research gGmbH have been advised of the possibility of such
 * damage.  See the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 *
 * The original code contained here was initially developed by:
 *
 *     Ralph Gaugess
 *     Bioinformatics Group
 *     European Media Laboratories Research gGmbH
 *     Schloss-Wolfsbrunnenweg 31c
 *     69118 Heidelberg
 *     Germany
 *
 *     http://www.eml-research.de/english/Research/BCB/
 *     mailto:ralph.gauges@eml-r.villa-bosch.de
 *
 * Contributor(s):
 */


#include "sbml/SBMLDocument.h"
#include "sbml/Model.h"
#include "sbml/Compartment.h"
#include "sbml/Species.h"
#include "sbml/SpeciesReference.h"
#include "sbml/Reaction.h"
#include "sbml/layout/Layout.h"
#include "sbml/layout/CompartmentGlyph.h"
#include "sbml/layout/SpeciesGlyph.h"
#include "sbml/layout/ReactionGlyph.h"
#include "sbml/layout/SpeciesReferenceGlyph.h"
#include "sbml/layout/Curve.h"
#include "sbml/layout/Dimensions.h"
#include "sbml/layout/BoundingBox.h"
#include "sbml/layout/Point.h"
#include "sbml/layout/LineSegment.h"
#include "sbml/layout/CubicBezier.h"
#include "sbml/SBMLWriter.h"

int main(int argc,char** argv){

// create the document

SBMLDocument *document=new SBMLDocument(2,1);
// create the Model

Model* model=document->createModel();
model->setId("TestModel with modifiers");
document->setModel(model);


// create the Layout

Layout* layout=model->createLayout();

layout->setId("Layout_1");
Dimensions dim(400.0,230.0);
layout->setDimensions(&dim);


// create the Compartment

Compartment* compartment=model->createCompartment();
compartment->setId("Yeast");

// create the CompartmentGlyph

CompartmentGlyph* compartmentGlyph=layout->createCompartmentGlyph();
compartmentGlyph->setId("CompartmentGlyph_1");
compartmentGlyph->setCompartmentId(compartment->getId());
BoundingBox bb("bb1",5,5,390,220);
compartmentGlyph->setBoundingBox(&bb);



// create the Species, SpeciesGlyphs and associated TextGlyphs

// Glucose
Species* species_Gluc=model->createSpecies();
species_Gluc->setId("Glucose");
species_Gluc->setCompartment(compartment->getId());

SpeciesGlyph* glyph_Gluc=layout->createSpeciesGlyph();
glyph_Gluc->setId("SpeciesGlyph_Glucose");
glyph_Gluc->setSpeciesId(species_Gluc->getId());
bb=BoundingBox("bb2",105,20,130,20);
glyph_Gluc->setBoundingBox(&bb);

TextGlyph* tGlyph=layout->createTextGlyph();
tGlyph->setId("TextGlyph_Glucose");
bb=BoundingBox("bbA",115,20,110,20);
tGlyph->setBoundingBox(&bb);
tGlyph->setOriginOfTextId(species_Gluc->getId());
tGlyph->setGraphicalObjectId(glyph_Gluc->getId());

// Glucose-6-phosphate

Species* species_G6P=model->createSpecies();
species_G6P->setId("Glucose-6-phosphate");
species_G6P->setCompartment(compartment->getId());

SpeciesGlyph* glyph_G6P=layout->createSpeciesGlyph();
glyph_G6P->setId("SpeciesGlyph_G6P");
glyph_G6P->setSpeciesId(species_G6P->getId());
bb=BoundingBox("bb5",50,190,270,20);
glyph_G6P->setBoundingBox(&bb);

tGlyph=layout->createTextGlyph();
tGlyph->setId("TextGlyph_G6P");
bb=BoundingBox("bbD",60,190,250,20);
tGlyph->setBoundingBox(&bb);
tGlyph->setOriginOfTextId(species_G6P->getId());
tGlyph->setGraphicalObjectId(glyph_G6P->getId());


// ATP
Species* species_ATP=model->createSpecies();
species_ATP->setId("ATP");
species_ATP->setCompartment(compartment->getId());

SpeciesGlyph* glyph_ATP=layout->createSpeciesGlyph();
glyph_ATP->setId("SpeciesGlyph_ATP");
glyph_ATP->setSpeciesId(species_ATP->getId());
bb=BoundingBox("bb3",270,70,80,20);
glyph_ATP->setBoundingBox(&bb);

tGlyph=layout->createTextGlyph();
tGlyph->setId("TextGlyph_ATP");
bb=BoundingBox("bbB",280,70,60,20);
tGlyph->setBoundingBox(&bb);
tGlyph->setOriginOfTextId(species_ATP->getId());
tGlyph->setGraphicalObjectId(glyph_ATP->getId());

// ADP

Species* species_ADP=model->createSpecies();
species_ADP->setId("ADP");
species_ADP->setCompartment(compartment->getId());

SpeciesGlyph* glyph_ADP=layout->createSpeciesGlyph();
glyph_ADP->setId("glyph_ADP");
glyph_ADP->setSpeciesId(species_ADP->getId());
bb=BoundingBox("bb4",270,140,80,20);
glyph_ADP->setBoundingBox(&bb);

tGlyph=layout->createTextGlyph();
tGlyph->setId("TextGlyph_ADP");
bb=BoundingBox("bbC",280,140,60,20);
tGlyph->setBoundingBox(&bb);
tGlyph->setOriginOfTextId(species_ADP->getId());
tGlyph->setGraphicalObjectId(glyph_ADP->getId());


// Phosphate

Species* species_Pi=model->createSpecies();
species_Pi->setId("Pi");
species_Pi->setCompartment(compartment->getId());

SpeciesGlyph* glyph_Pi=layout->createSpeciesGlyph();
glyph_Pi->setId("SpeciesGlyph_Pi");
glyph_Pi->setSpeciesId(species_Pi->getId());
bb=BoundingBox("bb6",50,100,60,20);
glyph_Pi->setBoundingBox(&bb);

tGlyph=layout->createTextGlyph();
tGlyph->setId("TextGlyph_PI");
bb=BoundingBox("bbE",60,100,40,20);
tGlyph->setBoundingBox(&bb);
tGlyph->setOriginOfTextId(species_Pi->getId());
tGlyph->setGraphicalObjectId(glyph_Pi->getId());


// create the Reaction

Reaction* reaction_Hexokinase=model->createReaction();
reaction_Hexokinase->setId("Hexokinase");
reaction_Hexokinase->setReversible(false);

ReactionGlyph* glyph_Hexokinase=layout->createReactionGlyph();
glyph_Hexokinase->setId("glyph_Hexokinase");
glyph_Hexokinase->setReactionId(reaction_Hexokinase->getId());

Curve* curve_Hexokinase=glyph_Hexokinase->getCurve();
LineSegment* ls=curve_Hexokinase->createLineSegment();
Point p(170,100);
ls->setStart(&p);
p=Point(170,130);
ls->setEnd(&p);



// create the species reference for glucose
SpeciesReference* reference_Gluc=new SpeciesReference();
reference_Gluc->setSpecies(species_Gluc->getId());
reference_Gluc->setId("SpeciesReference_Glucose");

reaction_Hexokinase->addReactant(reference_Gluc);

// create species reference glyph for glucose
SpeciesReferenceGlyph* speciesReferenceGlyph=glyph_Hexokinase->createSpeciesReferenceGlyph();
speciesReferenceGlyph->setId("SpeciesReferenceGlyph_Glucose");
speciesReferenceGlyph->setSpeciesGlyphId(glyph_Gluc->getId());
speciesReferenceGlyph->setSpeciesReferenceId(reference_Gluc->getId());
speciesReferenceGlyph->setRole(SPECIES_ROLE_SUBSTRATE);

ls=speciesReferenceGlyph->createLineSegment();
p=Point(170,100);
ls->setStart(&p);
p=Point(170,50);
ls->setEnd(&p);

// create species reference for ATP

SpeciesReference* reference_ATP=new SpeciesReference();
reference_ATP->setSpecies(species_ATP->getId());
reference_ATP->setId("SpeciesReference_ATP");

reaction_Hexokinase->addReactant(reference_ATP);

// create the species reference glyph for ATP

speciesReferenceGlyph=glyph_Hexokinase->createSpeciesReferenceGlyph();
speciesReferenceGlyph->setId("SpeciesReferenceGlyph_ATP");
speciesReferenceGlyph->setSpeciesGlyphId(glyph_ATP->getId());
speciesReferenceGlyph->setSpeciesReferenceId(reference_ATP->getId());
speciesReferenceGlyph->setRole(SPECIES_ROLE_SIDESUBSTRATE);

CubicBezier* cb=speciesReferenceGlyph->createCubicBezier();
p=Point(170,100);
cb->setStart(&p);
p=Point(170,80);
cb->setBasePoint1(&p);
p=Point(170,80);
cb->setBasePoint2(&p);
p=Point(260,80);
cb->setEnd(&p);

// create species reference for G6P

SpeciesReference* reference_G6P=new SpeciesReference();
reference_G6P->setSpecies(species_G6P->getId());
reference_G6P->setId("SpeciesReference_G6P");

reaction_Hexokinase->addProduct(reference_G6P);

// create species reference for G6P as product

speciesReferenceGlyph=glyph_Hexokinase->createSpeciesReferenceGlyph();
speciesReferenceGlyph->setId("SpeciesReferenceGlyph_G6P_1");
speciesReferenceGlyph->setSpeciesGlyphId(glyph_G6P->getId());
speciesReferenceGlyph->setSpeciesReferenceId(reference_G6P->getId());
speciesReferenceGlyph->setRole(SPECIES_ROLE_PRODUCT);

ls=speciesReferenceGlyph->createLineSegment();
p=Point(170,130);
ls->setStart(&p);
p=Point(170,180);
ls->setEnd(&p);

// create species reference for ADP

SpeciesReference* reference_ADP=new SpeciesReference();
reference_ADP->setSpecies(species_ADP->getId());
reference_ADP->setId("SpeciesReference_ADP");

reaction_Hexokinase->addProduct(reference_ADP);

// create the species reference glyph for ADP

speciesReferenceGlyph=glyph_Hexokinase->createSpeciesReferenceGlyph();
speciesReferenceGlyph->setId("SpeciesReferenceGlyph_ADP");
speciesReferenceGlyph->setSpeciesGlyphId(glyph_ADP->getId());
speciesReferenceGlyph->setSpeciesReferenceId(reference_ADP->getId());
speciesReferenceGlyph->setRole(SPECIES_ROLE_SIDEPRODUCT);

cb=speciesReferenceGlyph->createCubicBezier();
p=Point(170,130);
cb->setStart(&p);
p=Point(170,150);
cb->setBasePoint1(&p);
p=Point(170,150);
cb->setBasePoint2(&p);
p=Point(260,150);
cb->setEnd(&p);


// create modifier species reference for glucose-6-phosphate

ModifierSpeciesReference* reference_G6P_2=new ModifierSpeciesReference();
reference_G6P_2->setSpecies(species_G6P->getId());
reference_G6P_2->setId("ModifierSpeciesReference_G6P");

reaction_Hexokinase->addModifier(reference_G6P_2);

// create species reference glyph for G6P as a modifier

speciesReferenceGlyph=glyph_Hexokinase->createSpeciesReferenceGlyph();
speciesReferenceGlyph->setId("SpeciesReferenceGlyph_G6P_2");
speciesReferenceGlyph->setSpeciesReferenceId(reference_G6P_2->getId());
speciesReferenceGlyph->setSpeciesGlyphId(glyph_G6P->getId());
speciesReferenceGlyph->setRole(SPECIES_ROLE_INHIBITOR);

cb=speciesReferenceGlyph->createCubicBezier();
p=Point(45,200);
cb->setStart(&p);
p=Point(0,200);
cb->setBasePoint1(&p);
p=Point(0,120);
cb->setBasePoint2(&p);
p=Point(165,120);
cb->setEnd(&p);

// create modifier species reference for phosphate

ModifierSpeciesReference* reference_Pi=new ModifierSpeciesReference();
reference_Pi->setSpecies(species_Pi->getId());
reference_Pi->setId("ModifierSpeciesReference_Pi");

reaction_Hexokinase->addModifier(reference_Pi);

// create the species reference glyph for Phosphate
speciesReferenceGlyph=glyph_Hexokinase->createSpeciesReferenceGlyph();
speciesReferenceGlyph->setId("SpeciesReferenceGlyph_PI");
speciesReferenceGlyph->setSpeciesReferenceId(reference_Pi->getId());
speciesReferenceGlyph->setSpeciesGlyphId(glyph_Pi->getId());
speciesReferenceGlyph->setRole(SPECIES_ROLE_ACTIVATOR);

cb=speciesReferenceGlyph->createCubicBezier();
p=Point(115,110);
cb->setStart(&p);
p=Point(140,110);
cb->setBasePoint1(&p);
p=Point(140,110);
cb->setBasePoint2(&p);
p=Point(165,110);
cb->setEnd(&p);

// write model to file

writeSBML(document,"TestModel3-g++.xml");
delete document;

}
