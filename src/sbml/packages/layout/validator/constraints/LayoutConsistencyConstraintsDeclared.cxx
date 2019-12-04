/**
 * @cond doxygenLibsbmlInternal
 *
 * @file    LayoutConsistencyConstraintsDeclared.cxx
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

addConstraint(new VConstraintLayoutLayoutLayoutMustHaveDimensions(*this));

addConstraint(new VConstraintGraphicalObjectLayoutGOMetaIdRefMustReferenceObject(*this));

addConstraint(new VConstraintGraphicalObjectLayoutGOMustContainBoundingBox(*this));

addConstraint(new VConstraintCompartmentGlyphLayoutCGAllowedElements(*this));

addConstraint(new VConstraintCompartmentGlyphLayoutCGMetaIdRefMustReferenceObject(*this));

addConstraint(new VConstraintCompartmentGlyphLayoutCGCompartmentMustRefComp(*this));

addConstraint(new VConstraintCompartmentGlyphLayoutCGNoDuplicateReferences(*this));

addConstraint(new VConstraintSpeciesGlyphLayoutSGAllowedElements(*this));

addConstraint(new VConstraintSpeciesGlyphLayoutSGMetaIdRefMustReferenceObject(*this));

addConstraint(new VConstraintSpeciesGlyphLayoutSGSpeciesMustRefSpecies(*this));

addConstraint(new VConstraintSpeciesGlyphLayoutSGNoDuplicateReferences(*this));

addConstraint(new VConstraintReactionGlyphLayoutRGAllowedElements(*this));

addConstraint(new VConstraintReactionGlyphLayoutRGMetaIdRefMustReferenceObject(*this));

addConstraint(new VConstraintReactionGlyphLayoutRGReactionMustRefReaction(*this));

addConstraint(new VConstraintReactionGlyphLayoutRGNoDuplicateReferences(*this));

addConstraint(new VConstraintGeneralGlyphLayoutGGAllowedElements(*this));

addConstraint(new VConstraintGeneralGlyphLayoutGGMetaIdRefMustReferenceObject(*this));

addConstraint(new VConstraintGeneralGlyphLayoutGGReferenceMustRefObject(*this));

addConstraint(new VConstraintGeneralGlyphLayoutGGNoDuplicateReferences(*this));

addConstraint(new VConstraintTextGlyphLayoutTGAllowedElements(*this));

addConstraint(new VConstraintTextGlyphLayoutTGMetaIdRefMustReferenceObject(*this));

addConstraint(new VConstraintTextGlyphLayoutTGOriginOfTextMustRefObject(*this));

addConstraint(new VConstraintTextGlyphLayoutTGNoDuplicateReferences(*this));

addConstraint(new VConstraintTextGlyphLayoutTGGraphicalObjectMustRefObject(*this));

addConstraint(new VConstraintSpeciesReferenceGlyphLayoutSRGAllowedElements(*this));

addConstraint(new VConstraintSpeciesReferenceGlyphLayoutSRGMetaIdRefMustReferenceObject(*this));

addConstraint(new VConstraintSpeciesReferenceGlyphLayoutSRGSpeciesRefMustRefObject(*this));

addConstraint(new VConstraintSpeciesReferenceGlyphLayoutSRGNoDuplicateReferences(*this));

addConstraint(new VConstraintSpeciesReferenceGlyphLayoutSRGSpeciesGlyphMustRefObject(*this));

addConstraint(new VConstraintReferenceGlyphLayoutREFGAllowedElements(*this));

addConstraint(new VConstraintReferenceGlyphLayoutREFGMetaIdRefMustReferenceObject(*this));

addConstraint(new VConstraintReferenceGlyphLayoutREFGReferenceMustRefObject(*this));

addConstraint(new VConstraintReferenceGlyphLayoutREFGNoDuplicateReferences(*this));

addConstraint(new VConstraintReferenceGlyphLayoutREFGGlyphMustRefObject(*this));

addConstraint(new VConstraintBoundingBoxLayoutBBoxAllowedElements(*this));

addConstraint(new VConstraintBoundingBoxLayoutBBoxConsistent3DDefinition(*this));

addConstraint(new VConstraintCurveLayoutCurveAllowedElements(*this));

addConstraint(new VConstraintLineSegmentLayoutLSegAllowedElements(*this));

addConstraint(new VConstraintCubicBezierLayoutCBezAllowedElements(*this));


/** @endcond */

