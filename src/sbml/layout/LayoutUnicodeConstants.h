/**
 * Filename    : LayoutUnicodeConstants.h
 * Description : SBML Layout LayoutUnicodeConstants Header
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


#ifndef LayoutUnicodeConstants_h
#define LayoutUnicodeConstants_h


#include "common/common.h"


#ifdef USE_EXPAT
#  include <expat.h>
#  include "xml/ExpatUnicodeChars.h"
#else
#  include <xercesc/util/XMLUniDefs.hpp>
   using namespace xercesc;
#endif  /* USE_EXPAT */


static const XMLCh BACKGROUND[] =
{
  chLatin_b, chLatin_a, chLatin_c, chLatin_k, chLatin_g, chLatin_r, chLatin_o,
  chLatin_u, chLatin_n, chLatin_d, chNull
};

static const XMLCh COLOR[] =
{
  chLatin_c, chLatin_o, chLatin_l, chLatin_o, chLatin_r, chNull
};

static const XMLCh COMPARTMENT_GLYPH[] =
{
  chLatin_c, chLatin_o, chLatin_m, chLatin_p, chLatin_a, chLatin_r, chLatin_t,
  chLatin_m, chLatin_e, chLatin_n, chLatin_t, chLatin_G, chLatin_l, chLatin_y,
  chLatin_p, chLatin_h, chNull
};

static const XMLCh CURVE[] =
{
  chLatin_c, chLatin_u, chLatin_r, chLatin_v, chLatin_e, chNull
};

static const XMLCh CUBIC_BEZIER_BASE_POINT_1[] =
{
  chLatin_b, chLatin_a, chLatin_s, chLatin_e, chLatin_P, chLatin_o, chLatin_i,
  chLatin_n, chLatin_t, chDigit_1, chNull
};

static const XMLCh CUBIC_BEZIER_BASE_POINT_2[] =
{
  chLatin_b, chLatin_a, chLatin_s, chLatin_e, chLatin_P, chLatin_o, chLatin_i,
  chLatin_n, chLatin_t, chDigit_2, chNull
};

static const XMLCh CURVE_SEGMENT[] =
{
  chLatin_c, chLatin_u, chLatin_r, chLatin_v, chLatin_e, chLatin_S, chLatin_e,
  chLatin_g, chLatin_m, chLatin_e, chLatin_n, chLatin_t, chNull
};

static const XMLCh LAYOUT[] =
{
  chLatin_l, chLatin_a, chLatin_y, chLatin_o, chLatin_u, chLatin_t, chNull
};

static const XMLCh LIST_OF_ADDITIONAL_GRAPHICAL_OBJECTS[] =
{
  chLatin_l, chLatin_i, chLatin_s, chLatin_t, chLatin_O, chLatin_f, chLatin_A,
  chLatin_d, chLatin_d, chLatin_i, chLatin_t, chLatin_i, chLatin_o, chLatin_n,
  chLatin_a, chLatin_l, chLatin_G, chLatin_r, chLatin_a, chLatin_p, chLatin_h,
  chLatin_i, chLatin_c, chLatin_a, chLatin_l, chLatin_O, chLatin_b, chLatin_j,
  chLatin_e, chLatin_c, chLatin_t, chLatin_s, chNull
};

static const XMLCh LIST_OF_COMPARTMENT_GLYPHS[] =
{
  chLatin_l, chLatin_i, chLatin_s, chLatin_t, chLatin_O, chLatin_f, chLatin_C,
  chLatin_o, chLatin_m, chLatin_p, chLatin_a, chLatin_r, chLatin_t, chLatin_m,
  chLatin_e, chLatin_n, chLatin_t, chLatin_G, chLatin_l, chLatin_y, chLatin_p,
  chLatin_h, chLatin_s, chNull
};

static const XMLCh LIST_OF_CURVE_SEGMENTS[] =
{
  chLatin_l, chLatin_i, chLatin_s, chLatin_t, chLatin_O, chLatin_f, chLatin_C,
  chLatin_u, chLatin_r, chLatin_v, chLatin_e, chLatin_S, chLatin_e, chLatin_g,
  chLatin_m, chLatin_e, chLatin_n, chLatin_t, chLatin_s, chNull
};

static const XMLCh LIST_OF_LAYOUTS[] =
{
  chLatin_l, chLatin_i, chLatin_s, chLatin_t, chLatin_O, chLatin_f, chLatin_L,
  chLatin_a, chLatin_y, chLatin_o, chLatin_u, chLatin_t, chLatin_s, chNull
};

static const XMLCh LIST_OF_REACTION_GLYPHS[] =
{
  chLatin_l, chLatin_i, chLatin_s, chLatin_t, chLatin_O, chLatin_f, chLatin_R,
  chLatin_e, chLatin_a, chLatin_c, chLatin_t, chLatin_i, chLatin_o, chLatin_n,
  chLatin_G, chLatin_l, chLatin_y, chLatin_p, chLatin_h, chLatin_s, chNull
};

static const XMLCh LIST_OF_SPECIES_REFERENCE_GLYPHS[] =
{
  chLatin_l, chLatin_i, chLatin_s, chLatin_t, chLatin_O, chLatin_f, chLatin_S,
  chLatin_p, chLatin_e, chLatin_c, chLatin_i, chLatin_e, chLatin_s, chLatin_R,
  chLatin_e, chLatin_f, chLatin_e, chLatin_r, chLatin_e, chLatin_n, chLatin_c,
  chLatin_e, chLatin_G, chLatin_l, chLatin_y, chLatin_p, chLatin_h, chLatin_s,
  chNull
};

static const XMLCh REACTION_GLYPH[] =
{
  chLatin_r, chLatin_e, chLatin_a, chLatin_c, chLatin_t, chLatin_i, chLatin_o,
  chLatin_n, chLatin_G, chLatin_l, chLatin_y, chLatin_p, chLatin_h, chNull
};

static const XMLCh SPECIES_REFERENCE_GLYPH[] =
{
  chLatin_s, chLatin_p, chLatin_e, chLatin_c, chLatin_i, chLatin_e, chLatin_s,
  chLatin_R, chLatin_e, chLatin_f, chLatin_e, chLatin_r, chLatin_e, chLatin_n,
  chLatin_c, chLatin_e, chLatin_G, chLatin_l, chLatin_y, chLatin_p, chLatin_h,
  chNull
};

static const XMLCh LINESEGMENT_START[] =
{
  chLatin_s, chLatin_t, chLatin_a, chLatin_r, chLatin_t, chNull
};

static const XMLCh LINESEGMENT_END[] =
{
  chLatin_e, chLatin_n, chLatin_d, chNull
};

static const XMLCh BOUNDING_BOX_POSITION[] =
{
  chLatin_p, chLatin_o, chLatin_s, chLatin_i, chLatin_t, chLatin_i, chLatin_o,
  chLatin_n, chNull
};

static const XMLCh DIMENSIONS[] =
{
  chLatin_d, chLatin_i, chLatin_m, chLatin_e, chLatin_n, chLatin_s, chLatin_i,
  chLatin_o, chLatin_n, chLatin_s, chNull
};

static const XMLCh BOUNDING_BOX[] =
{
  chLatin_b, chLatin_o, chLatin_u, chLatin_n, chLatin_d, chLatin_i, chLatin_n,
  chLatin_g, chLatin_B, chLatin_o, chLatin_x, chNull
};

static const XMLCh ATTR_WIDTH[] =
{
  chLatin_w, chLatin_i, chLatin_d, chLatin_t, chLatin_h, chNull
};

static const XMLCh ATTR_HEIGHT[] =
{
  chLatin_h, chLatin_e, chLatin_i, chLatin_g, chLatin_h, chLatin_t, chNull
};

static const XMLCh ATTR_DEPTH[] =
{
  chLatin_d, chLatin_e, chLatin_p, chLatin_t, chLatin_h, chNull
};

static const XMLCh ATTR_X[] =
{
  chLatin_x, chNull
};

static const XMLCh ATTR_Y[] =
{
  chLatin_y, chNull
};

static const XMLCh ATTR_Z[] =
{
  chLatin_z, chNull
};

static const XMLCh ATTR_COMPARTMENT_ID[] =
{
  chLatin_c, chLatin_o, chLatin_m, chLatin_p, chLatin_a, chLatin_r, chLatin_t,
  chLatin_m, chLatin_e, chLatin_n, chLatin_t, chNull
};

static const XMLCh ATTR_SPECIES_ID[] =
{
  chLatin_s, chLatin_p, chLatin_e, chLatin_c, chLatin_i, chLatin_e, chLatin_s,
  chNull
};

static const XMLCh ATTR_REACTION_ID[] =
{
  chLatin_r, chLatin_e, chLatin_a, chLatin_c, chLatin_t, chLatin_i, chLatin_o,
  chLatin_n, chNull
};

static const XMLCh ATTR_SPECIES_REFERENCE_ID[] =
{
  chLatin_s, chLatin_p, chLatin_e, chLatin_c, chLatin_i, chLatin_e, chLatin_s,
  chLatin_R, chLatin_e, chLatin_f, chLatin_e, chLatin_r, chLatin_e, chLatin_n,
  chLatin_c, chLatin_e, chNull
};

static const XMLCh ATTR_SPECIES_GLYPH[] =
{
  chLatin_s, chLatin_p, chLatin_e, chLatin_c, chLatin_i, chLatin_e, chLatin_s,
  chLatin_G, chLatin_l, chLatin_y, chLatin_p, chLatin_h, chNull
};

static const XMLCh ATTR_ROLE[] =
{
  chLatin_r, chLatin_o, chLatin_l, chLatin_e, chNull
};

static const XMLCh ATTR_ORIGIN_OF_TEXT_ID[] =
{
  chLatin_o, chLatin_r, chLatin_i, chLatin_g, chLatin_i, chLatin_n, chLatin_O,
  chLatin_f, chLatin_T, chLatin_e, chLatin_x, chLatin_t, chNull
};

static const XMLCh ATTR_TEXT[] =
{
  chLatin_t, chLatin_e, chLatin_x, chLatin_t, chNull
};

static const XMLCh ATTR_GRAPHICAL_OBJECT_ID[] =
{
  chLatin_g, chLatin_r, chLatin_a, chLatin_p, chLatin_h, chLatin_i, chLatin_c,
  chLatin_a, chLatin_l, chLatin_O, chLatin_b, chLatin_j, chLatin_e, chLatin_c,
  chLatin_t, chNull
};

static const XMLCh LIST_OF_SPECIES_GLYPHS[] =
{
  chLatin_l, chLatin_i, chLatin_s, chLatin_t, chLatin_O, chLatin_f, chLatin_S,
  chLatin_p, chLatin_e, chLatin_c, chLatin_i, chLatin_e, chLatin_s, chLatin_G,
  chLatin_l, chLatin_y, chLatin_p, chLatin_h, chLatin_s, chNull
};

static const XMLCh LIST_OF_TEXT_GLYPHS[] =
{
  chLatin_l, chLatin_i, chLatin_s, chLatin_t, chLatin_O, chLatin_f, chLatin_T,
  chLatin_e, chLatin_x, chLatin_t, chLatin_G, chLatin_l, chLatin_y, chLatin_p,
  chLatin_h, chLatin_s, chNull
};

static const XMLCh GRAPHICAL_OBJECT[] =
{
  chLatin_g, chLatin_r, chLatin_a, chLatin_p, chLatin_h, chLatin_i, chLatin_c,
  chLatin_a, chLatin_l, chLatin_O, chLatin_b, chLatin_j, chLatin_e, chLatin_c,
  chLatin_t, chNull
};

static const XMLCh TEXT_GLYPH[] =
{
  chLatin_t, chLatin_e, chLatin_x, chLatin_t, chLatin_G, chLatin_l, chLatin_y,
  chLatin_p, chLatin_h, chNull
};
    
static const XMLCh SPECIES_GLYPH[] =
{
  chLatin_s, chLatin_p, chLatin_e, chLatin_c, chLatin_i, chLatin_e, chLatin_s,
  chLatin_G, chLatin_l, chLatin_y, chLatin_p, chLatin_h, chNull
};

static const XMLCh XMLNS_LAYOUT[] =
{
  chLatin_h, chLatin_t, chLatin_t, chLatin_p, chColon,

  chForwardSlash, chForwardSlash,

  chLatin_p, chLatin_r, chLatin_o, chLatin_j, chLatin_e, chLatin_c, chLatin_t,
  chLatin_s,

  chPeriod,

  chLatin_e, chLatin_m, chLatin_l,

  chPeriod,

  chLatin_o, chLatin_r, chLatin_g,

  chForwardSlash,

  chLatin_b, chLatin_c, chLatin_b,

  chForwardSlash,

  chLatin_s, chLatin_b, chLatin_m, chLatin_l,

  chForwardSlash,

  chLatin_l, chLatin_e, chLatin_v, chLatin_e, chLatin_l, chDigit_2,

  chPeriod,
  
  chLatin_x, chLatin_m, chLatin_l,

  chNull
};

static const XMLCh ATTR_REACTION[] =
{
  chLatin_r, chLatin_e, chLatin_a, chLatin_c, chLatin_t, chLatin_i, chLatin_o,
  chLatin_n, chNull
};

static const XMLCh ATTR_SPECIES_REFERENCE[] =
{
  chLatin_s, chLatin_p, chLatin_e, chLatin_c, chLatin_i, chLatin_e, chLatin_s,
  chLatin_R, chLatin_e, chLatin_f, chLatin_e, chLatin_r, chLatin_e, chLatin_n,
  chLatin_c, chLatin_e, chNull
};


static const XMLCh ATTR_RENDERGROUP[] =
{
  chLatin_r, chLatin_e, chLatin_n, chLatin_d, chLatin_e, chLatin_r,
  chLatin_G, chLatin_r, chLatin_o, chLatin_u, chLatin_p, chNull
};

static const XMLCh ATTR_XSI_TYPE[] =
{
  chLatin_x, chLatin_s, chLatin_i, chColon,
  chLatin_t, chLatin_y, chLatin_p, chLatin_e, chNull	
};

static const XMLCh LAYOUTID[] =
{
  chLatin_l, chLatin_a, chLatin_y, chLatin_o, chLatin_u, chLatin_t,
  chLatin_I, chLatin_d, chNull
};


#endif  /* LayoutUnicodeConstants_h */
