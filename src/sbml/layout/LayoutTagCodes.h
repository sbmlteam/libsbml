/**
 * Filename    : LayoutTagCodes.h
 * Description : SBML Layout LayoutTagCodes Header
 * Organization: European Media Laboratories Research gGmbH
 * Created     : 2004-04-26
 *
 * Copyright 2004 European Media Laboratories Research gGmbH
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
 *     Ralph Gauges
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


#ifndef LayoutTagCodes_h
#define LayoutTagCodes_h


#include "common/libsbml-config.h"
#include "xml/XMLUtil.h"


#ifdef USE_EXPAT
#  include <expat.h>
#else
#  include <xercesc/util/XMLUniDefs.hpp>
#endif


typedef long LayoutTagCode_t;

static const LayoutTagCode_t TAG_CUBIC_BEZIER_BASE_POINT_1            =  1000;
static const LayoutTagCode_t TAG_CUBIC_BEZIER_BASE_POINT_2            =  1001;
static const LayoutTagCode_t TAG_BOUNDING_BOX                         =  1002;
static const LayoutTagCode_t TAG_COMPARTMENT_GLYPH                    =  1003;
static const LayoutTagCode_t TAG_CURVE                                =  1004;
static const LayoutTagCode_t TAG_CURVE_SEGMENT                        =  1005;
static const LayoutTagCode_t TAG_DIMENSIONS                           =  1006;
static const LayoutTagCode_t TAG_LINE_SEGMENT_END                     =  1007;
static const LayoutTagCode_t TAG_GRAPHICAL_OBJECT                     =  1008;
static const LayoutTagCode_t TAG_LAYOUT                               =  1009;
static const LayoutTagCode_t TAG_LAYOUTID                             =  1010;
static const LayoutTagCode_t TAG_LIST_OF_ADDITIONAL_GRAPHICAL_OBJECTS =  1011;
static const LayoutTagCode_t TAG_LIST_OF_COMPARTMENT_GLYPHS           =  1012;
static const LayoutTagCode_t TAG_LIST_OF_CURVE_SEGMENTS               =  1013;
static const LayoutTagCode_t TAG_LIST_OF_LAYOUTS                      =  1014;
static const LayoutTagCode_t TAG_LIST_OF_REACTION_GLYPHS              =  1015;
static const LayoutTagCode_t TAG_LIST_OF_SPECIES_GLYPHS               =  1016;
static const LayoutTagCode_t TAG_LIST_OF_SPECIES_REFERENCE_GLYPHS     =  1017;
static const LayoutTagCode_t TAG_LIST_OF_TEXT_GLYPHS                  =  1018;
static const LayoutTagCode_t TAG_BOUNDING_BOX_POSITION                =  1019;
static const LayoutTagCode_t TAG_REACTION_GLYPH                       =  1020;
static const LayoutTagCode_t TAG_SPECIES_GLYPH                        =  1021;
static const LayoutTagCode_t TAG_SPECIES_REFERENCE_GLYPH              =  1022;
static const LayoutTagCode_t TAG_LINE_SEGMENT_START                   =  1023;
static const LayoutTagCode_t TAG_TEXT_GLYPH                           =  1024;


/**
 * Returns the tag code for the given SBML element.
 */
LayoutTagCode_t
LayoutTagCode_forElement (const XMLCh* name);


#endif  /* LayoutTagCodes_h */
