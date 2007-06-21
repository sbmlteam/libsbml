/**
 * \file    example1.java
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


import org.sbml.libsbml.*;


public class example1
{
    /**
     * Loads the SWIG generated libsbml Java module when this class is
     * loaded.
     */
    static
    {
        System.loadLibrary("sbmlj");
    }

    public static void main (String args[])
    {

        // create the document

        SBMLDocument document=new SBMLDocument(2,1);
        // create the Model

        Model model=document.createModel("TestModel");


        // create the Compartment

        Compartment compartment=model.createCompartment();
        compartment.setId("Compartment_1");

        // create the Species

        Species species1=model.createSpecies();
        species1.setId("Species_1");
        species1.setCompartment(compartment.getId());

        Species species2=model.createSpecies();
        species2.setId("Species_2");
        species2.setCompartment(compartment.getId());

        // create the Reactions

        Reaction reaction1=model.createReaction();
        reaction1.setId("Reaction_1");

        SpeciesReference reference1=new SpeciesReference();
        reference1.setSpecies(species1.getId());
        reference1.setId("SpeciesReference_1");

        reaction1.addReactant(reference1);
        reaction1.setReversible(false);

        SpeciesReference reference2=new SpeciesReference();
        reference2.setSpecies(species2.getId());
        reference2.setId("SpeciesReference_2");

        reaction1.addProduct(reference2);

        Reaction reaction2=model.createReaction();
        reaction2.setId("Reaction_2");
        reaction2.setReversible(false);

        SpeciesReference reference3=new SpeciesReference();
        reference3.setSpecies(species2.getId());
        reference3.setId("SpeciesReference_3");

        SpeciesReference reference4=new SpeciesReference();
        reference4.setSpecies(species1.getId());
        reference4.setId("SpeciesReference_4");

        reaction2.addReactant(reference3);
        reaction2.addProduct(reference4);

        // create the Layout

        Layout layout=model.createLayout();

        layout.setId("Layout_1");
        layout.setDimensions(new Dimensions(400.0,220.0));


        // create the CompartmentGlyph

        CompartmentGlyph compartmentGlyph=layout.createCompartmentGlyph();
        compartmentGlyph.setId("CompartmentGlyph_1");
        compartmentGlyph.setCompartmentId(compartment.getId());
        compartmentGlyph.setBoundingBox(new BoundingBox("bb1",5.0,5.0,390.0,210.0));


        // create the SpeciesGlyphs

        SpeciesGlyph speciesGlyph1=layout.createSpeciesGlyph();
        speciesGlyph1.setId("SpeciesGlyph_1");
        speciesGlyph1.setSpeciesId(species1.getId());
        speciesGlyph1.setBoundingBox(new BoundingBox("bb2",80.0,26.0,240.0,24.0));

        TextGlyph textGlyph1=layout.createTextGlyph();
        textGlyph1.setId("TextGlyph_01");
        textGlyph1.setBoundingBox(new BoundingBox("bbA",92.0,26.0,228.0,24.0));
        textGlyph1.setOriginOfTextId(speciesGlyph1.getId());
        textGlyph1.setGraphicalObjectId(speciesGlyph1.getId());

        SpeciesGlyph speciesGlyph2=layout.createSpeciesGlyph();
        speciesGlyph2.setId("SpeciesGlyph_2");
        speciesGlyph2.setSpeciesId(species2.getId());
        speciesGlyph2.setBoundingBox(new BoundingBox("bb3",80.0,170.0,240.0,24.0));

        TextGlyph textGlyph2=layout.createTextGlyph();
        textGlyph2.setId("TextGlyph_02");
        textGlyph2.setBoundingBox(new BoundingBox("bbB",92.0,170.0,228.0,24.0));
        textGlyph2.setOriginOfTextId(speciesGlyph2.getId());
        textGlyph2.setGraphicalObjectId(speciesGlyph2.getId());

        // create the ReactionGlyphs

        ReactionGlyph reactionGlyph1=layout.createReactionGlyph();
        reactionGlyph1.setId("ReactionGlyph_1");
        reactionGlyph1.setReactionId(reaction1.getId());

        Curve reactionCurve1=reactionGlyph1.getCurve();
        LineSegment ls=reactionCurve1.createLineSegment();
        ls.setStart(new Point(165.0,105.0));
        ls.setEnd(new Point(165.0,115.0));

        ReactionGlyph reactionGlyph2=layout.createReactionGlyph();
        reactionGlyph2.setId("ReactionGlyph_1");
        reactionGlyph2.setReactionId(reaction2.getId());

        Curve reactionCurve2=reactionGlyph2.getCurve();
        ls=reactionCurve2.createLineSegment();
        ls.setStart(new Point(235.0,105.0));
        ls.setEnd(new Point(235.0,115.0));

        // add the SpeciesReferenceGlyphs

        SpeciesReferenceGlyph speciesReferenceGlyph1=reactionGlyph1.createSpeciesReferenceGlyph();
        speciesReferenceGlyph1.setId("SpeciesReferenceGlyph_1");
        speciesReferenceGlyph1.setSpeciesGlyphId(speciesGlyph1.getId());
        speciesReferenceGlyph1.setSpeciesReferenceId(reference1.getId());
        speciesReferenceGlyph1.setRole(libsbmlConstants.SPECIES_ROLE_SUBSTRATE);

        Curve speciesReferenceCurve1=speciesReferenceGlyph1.getCurve();
        CubicBezier cb=speciesReferenceCurve1.createCubicBezier();
        cb.setStart(new Point(165.0,105.0));
        cb.setBasePoint1(new Point(165.0,90.0));
        cb.setBasePoint2(new Point(165.0,90.0));
        cb.setEnd(new Point(195.0,60.0));

        SpeciesReferenceGlyph speciesReferenceGlyph2=reactionGlyph1.createSpeciesReferenceGlyph();
        speciesReferenceGlyph2.setId("SpeciesReferenceGlyph_2");
        speciesReferenceGlyph2.setSpeciesGlyphId(speciesGlyph2.getId());
        speciesReferenceGlyph2.setSpeciesReferenceId(reference2.getId());
        speciesReferenceGlyph2.setRole(libsbmlConstants.SPECIES_ROLE_PRODUCT);

        Curve speciesReferenceCurve2=speciesReferenceGlyph2.getCurve();
        cb=speciesReferenceCurve2.createCubicBezier();
        cb.setStart(new Point(165.0,115.0));
        cb.setBasePoint1(new Point(165.0,130.0));
        cb.setBasePoint2(new Point(165.0,130.0));
        cb.setEnd(new Point(195.0,160.0));


        SpeciesReferenceGlyph speciesReferenceGlyph3=reactionGlyph2.createSpeciesReferenceGlyph();
        speciesReferenceGlyph3.setId("SpeciesReferenceGlyph_3");
        speciesReferenceGlyph3.setSpeciesGlyphId(speciesGlyph2.getId());
        speciesReferenceGlyph3.setSpeciesReferenceId(reference3.getId());
        speciesReferenceGlyph3.setRole(libsbmlConstants.SPECIES_ROLE_SUBSTRATE);

        Curve speciesReferenceCurve3=speciesReferenceGlyph3.getCurve();
        cb=speciesReferenceCurve3.createCubicBezier();
        cb.setStart(new Point(235.0,115.0));
        cb.setBasePoint1(new Point(235.0,130.0));
        cb.setBasePoint2(new Point(235.0,130.0));
        cb.setEnd(new Point(205.0,160.0));

        SpeciesReferenceGlyph speciesReferenceGlyph4=reactionGlyph2.createSpeciesReferenceGlyph();
        speciesReferenceGlyph4.setId("SpeciesReferenceGlyph_4");
        speciesReferenceGlyph4.setSpeciesGlyphId(speciesGlyph1.getId());
        speciesReferenceGlyph4.setSpeciesReferenceId(reference4.getId());
        speciesReferenceGlyph4.setRole(libsbmlConstants.SPECIES_ROLE_PRODUCT);

        Curve speciesReferenceCurve4=speciesReferenceGlyph4.getCurve();
        cb=speciesReferenceCurve4.createCubicBezier();
        cb.setStart(new Point(235.0,105.0));
        cb.setBasePoint1(new Point(235.0,90.0));
        cb.setBasePoint2(new Point(235.0,90.0));
        cb.setEnd(new Point(205.0,60.0));


        libsbml.writeSBML(document,"TestModel1-java.xml");

        System.gc();
    }
}
