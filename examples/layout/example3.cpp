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


#include "common/common.h"

#include "sbml/SBMLDocument.h"
#include "sbml/Model.h"
#include "sbml/Compartment.h"
#include "sbml/Species.h"
#include "sbml/SpeciesReference.h"
#include "sbml/ModifierSpeciesReference.h"
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

SBMLDocument *document=new SBMLDocument();
// create the Model

Model* model=new Model();
model->setId("TestModel with modifiers");
document->setModel(model);
document->setLevel(2);
document->setVersion(1);


// create the Layout

Layout* layout=&model->createLayout();

layout->setId("Layout_1");
layout->setDimensions(Dimensions(400.0,230.0));


// create the Compartment

Compartment* compartment=&model->createCompartment();
compartment->setId("Yeast");

// create the CompartmentGlyph

CompartmentGlyph* compartmentGlyph=layout->createCompartmentGlyph();
compartmentGlyph->setId("CompartmentGlyph_1");
compartmentGlyph->setCompartmentId(compartment->getId());
compartmentGlyph->setBoundingBox(BoundingBox("bb1",5,5,390,220));



// create the Species, SpeciesGlyphs and associated TextGlyphs

// Glucose
Species* species_Gluc=&model->createSpecies();
species_Gluc->setId("Glucose");
species_Gluc->setCompartment(compartment->getId());

SpeciesGlyph* glyph_Gluc=layout->createSpeciesGlyph();
glyph_Gluc->setId("SpeciesGlyph_Glucose");
glyph_Gluc->setSpeciesId(species_Gluc->getId());
glyph_Gluc->setBoundingBox(BoundingBox("bb2",105,20,130,20));

TextGlyph* tGlyph=layout->createTextGlyph();
tGlyph->setId("TextGlyph_Glucose");
tGlyph->setBoundingBox(BoundingBox("bbA",115,20,110,20));
tGlyph->setOriginOfTextId(species_Gluc->getId());
tGlyph->setGraphicalObjectId(glyph_Gluc->getId());

// Glucose-6-phosphate

Species* species_G6P=&model->createSpecies();
species_G6P->setId("Glucose-6-phosphate");
species_G6P->setCompartment(compartment->getId());

SpeciesGlyph* glyph_G6P=layout->createSpeciesGlyph();
glyph_G6P->setId("SpeciesGlyph_G6P");
glyph_G6P->setSpeciesId(species_G6P->getId());
glyph_G6P->setBoundingBox(BoundingBox("bb5",50,190,270,20));

tGlyph=layout->createTextGlyph();
tGlyph->setId("TextGlyph_G6P");
tGlyph->setBoundingBox(BoundingBox("bbD",60,190,250,20));
tGlyph->setOriginOfTextId(species_G6P->getId());
tGlyph->setGraphicalObjectId(glyph_G6P->getId());


// ATP
Species* species_ATP=&model->createSpecies();
species_ATP->setId("ATP");
species_ATP->setCompartment(compartment->getId());

SpeciesGlyph* glyph_ATP=layout->createSpeciesGlyph();
glyph_ATP->setId("SpeciesGlyph_ATP");
glyph_ATP->setSpeciesId(species_ATP->getId());
glyph_ATP->setBoundingBox(BoundingBox("bb3",270,70,80,20));

tGlyph=layout->createTextGlyph();
tGlyph->setId("TextGlyph_ATP");
tGlyph->setBoundingBox(BoundingBox("bbB",280,70,60,20));
tGlyph->setOriginOfTextId(species_ATP->getId());
tGlyph->setGraphicalObjectId(glyph_ATP->getId());

// ADP

Species* species_ADP=&model->createSpecies();
species_ADP->setId("ADP");
species_ADP->setCompartment(compartment->getId());

SpeciesGlyph* glyph_ADP=layout->createSpeciesGlyph();
glyph_ADP->setId("glyph_ADP");
glyph_ADP->setSpeciesId(species_ADP->getId());
glyph_ADP->setBoundingBox(BoundingBox("bb4",270,140,80,20));

tGlyph=layout->createTextGlyph();
tGlyph->setId("TextGlyph_ADP");
tGlyph->setBoundingBox(BoundingBox("bbC",280,140,60,20));
tGlyph->setOriginOfTextId(species_ADP->getId());
tGlyph->setGraphicalObjectId(glyph_ADP->getId());


// Phosphate

Species* species_Pi=&model->createSpecies();
species_Pi->setId("Pi");
species_Pi->setCompartment(compartment->getId());

SpeciesGlyph* glyph_Pi=layout->createSpeciesGlyph();
glyph_Pi->setId("SpeciesGlyph_Pi");
glyph_Pi->setSpeciesId(species_Pi->getId());
glyph_Pi->setBoundingBox(BoundingBox("bb6",50,100,60,20));

tGlyph=layout->createTextGlyph();
tGlyph->setId("TextGlyph_PI");
tGlyph->setBoundingBox(BoundingBox("bbE",60,100,40,20));
tGlyph->setOriginOfTextId(species_Pi->getId());
tGlyph->setGraphicalObjectId(glyph_Pi->getId());


// create the Reaction

Reaction* reaction_Hexokinase=&model->createReaction();
reaction_Hexokinase->setId("Hexokinase");
reaction_Hexokinase->setReversible(false);

ReactionGlyph* glyph_Hexokinase=layout->createReactionGlyph();
glyph_Hexokinase->setId("glyph_Hexokinase");
glyph_Hexokinase->setReactionId(reaction_Hexokinase->getId());

Curve* curve_Hexokinase=glyph_Hexokinase->getCurve();
LineSegment* ls=&curve_Hexokinase->createLineSegment();
ls->setStart(Point(170,100));
ls->setEnd(Point(170,130));



// create the species reference for glucose
SpeciesReference* reference_Gluc=new SpeciesReference();
reference_Gluc->setSpecies(species_Gluc->getId());
reference_Gluc->setId("SpeciesReference_Glucose");

reaction_Hexokinase->addReactant(*reference_Gluc);

// create species reference glyph for glucose
SpeciesReferenceGlyph* speciesReferenceGlyph=&glyph_Hexokinase->createSpeciesReferenceGlyph();
speciesReferenceGlyph->setId("SpeciesReferenceGlyph_Glucose");
speciesReferenceGlyph->setSpeciesGlyphId(glyph_Gluc->getId());
speciesReferenceGlyph->setSpeciesReferenceId(reference_Gluc->getId());
speciesReferenceGlyph->setRole(SPECIES_ROLE_SUBSTRATE);

ls=&speciesReferenceGlyph->createLineSegment();
ls->setStart(Point(170,100));
ls->setEnd(Point(170,50));

// create species reference for ATP

SpeciesReference* reference_ATP=new SpeciesReference();
reference_ATP->setSpecies(species_ATP->getId());
reference_ATP->setId("SpeciesReference_ATP");

reaction_Hexokinase->addReactant(*reference_ATP);

// create the species reference glyph for ATP

speciesReferenceGlyph=&glyph_Hexokinase->createSpeciesReferenceGlyph();
speciesReferenceGlyph->setId("SpeciesReferenceGlyph_ATP");
speciesReferenceGlyph->setSpeciesGlyphId(glyph_ATP->getId());
speciesReferenceGlyph->setSpeciesReferenceId(reference_ATP->getId());
speciesReferenceGlyph->setRole(SPECIES_ROLE_SIDESUBSTRATE);

CubicBezier* cb=&speciesReferenceGlyph->createCubicBezier();
cb->setStart(Point(170,100));
cb->setBasePoint1(Point(170,80));
cb->setBasePoint2(Point(170,80));
cb->setEnd(Point(260,80));

// create species reference for G6P

SpeciesReference* reference_G6P=new SpeciesReference();
reference_G6P->setSpecies(species_G6P->getId());
reference_G6P->setId("SpeciesReference_G6P");

reaction_Hexokinase->addProduct(*reference_G6P);

// create species reference for G6P as product

speciesReferenceGlyph=&glyph_Hexokinase->createSpeciesReferenceGlyph();
speciesReferenceGlyph->setId("SpeciesReferenceGlyph_G6P_1");
speciesReferenceGlyph->setSpeciesGlyphId(glyph_G6P->getId());
speciesReferenceGlyph->setSpeciesReferenceId(reference_G6P->getId());
speciesReferenceGlyph->setRole(SPECIES_ROLE_PRODUCT);

ls=&speciesReferenceGlyph->createLineSegment();
ls->setStart(Point(170,130));
ls->setEnd(Point(170,180));

// create species reference for ADP

SpeciesReference* reference_ADP=new SpeciesReference();
reference_ADP->setSpecies(species_ADP->getId());
reference_ADP->setId("SpeciesReference_ADP");

reaction_Hexokinase->addProduct(*reference_ADP);

// create the species reference glyph for ADP

speciesReferenceGlyph=&glyph_Hexokinase->createSpeciesReferenceGlyph();
speciesReferenceGlyph->setId("SpeciesReferenceGlyph_ADP");
speciesReferenceGlyph->setSpeciesGlyphId(glyph_ADP->getId());
speciesReferenceGlyph->setSpeciesReferenceId(reference_ADP->getId());
speciesReferenceGlyph->setRole(SPECIES_ROLE_SIDEPRODUCT);

cb=&speciesReferenceGlyph->createCubicBezier();
cb->setStart(Point(170,130));
cb->setBasePoint1(Point(170,150));
cb->setBasePoint2(Point(170,150));
cb->setEnd(Point(260,150));


// create modifier species reference for glucose-6-phosphate

ModifierSpeciesReference* reference_G6P_2=new ModifierSpeciesReference();
reference_G6P_2->setSpecies(species_G6P->getId());
reference_G6P_2->setId("ModifierSpeciesReference_G6P");

reaction_Hexokinase->addModifier(*reference_G6P_2);

// create species reference glyph for G6P as a modifier

speciesReferenceGlyph=&glyph_Hexokinase->createSpeciesReferenceGlyph();
speciesReferenceGlyph->setId("SpeciesReferenceGlyph_G6P_2");
speciesReferenceGlyph->setSpeciesReferenceId(reference_G6P_2->getId());
speciesReferenceGlyph->setSpeciesGlyphId(glyph_G6P->getId());
speciesReferenceGlyph->setRole(SPECIES_ROLE_INHIBITOR);

cb=&speciesReferenceGlyph->createCubicBezier();
cb->setStart(Point(45,200));
cb->setBasePoint1(Point(0,200));
cb->setBasePoint2(Point(0,120));
cb->setEnd(Point(165,120));

// create modifier species reference for phosphate

ModifierSpeciesReference* reference_Pi=new ModifierSpeciesReference();
reference_Pi->setSpecies(species_Pi->getId());
reference_Pi->setId("ModifierSpeciesReference_Pi");

reaction_Hexokinase->addModifier(*reference_Pi);

// create the species reference glyph for Phosphate
speciesReferenceGlyph=&glyph_Hexokinase->createSpeciesReferenceGlyph();
speciesReferenceGlyph->setId("SpeciesReferenceGlyph_PI");
speciesReferenceGlyph->setSpeciesReferenceId(reference_Pi->getId());
speciesReferenceGlyph->setSpeciesGlyphId(glyph_Pi->getId());
speciesReferenceGlyph->setRole(SPECIES_ROLE_ACTIVATOR);

cb=&speciesReferenceGlyph->createCubicBezier();
cb->setStart(Point(115,110));
cb->setBasePoint1(Point(140,110));
cb->setBasePoint2(Point(140,110));
cb->setEnd(Point(165,110));

// write model to file

writeSBML(document,"TestModel3-g++.xml");
delete document;

}
