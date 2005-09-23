/**
 * Filename    : LayoutTagCodes.cpp
 * Description : SBML Layout LayoutTagCodes
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


#include <iostream>
#include "common/common.h"


#ifdef USE_EXPAT
#  include "xml/ExpatXMLString.h"
#else
#  include <xercesc/util/XMLString.hpp>
#  include <xercesc/util/XMLUniDefs.hpp>
#endif  // USE_EXPAT


#include "sbml/SBMLTagCodes.h"
#include "sbml/SBMLUnicodeConstants.h"

#include "LayoutTagCodes.h"
#include "LayoutUnicodeConstants.h"


static const XMLCh* LAYOUT_ELEMENTS[] =
{
    CUBIC_BEZIER_BASE_POINT_1
  , CUBIC_BEZIER_BASE_POINT_2
  , BOUNDING_BOX
  , COMPARTMENT_GLYPH
  , CURVE
  , CURVE_SEGMENT
  , DIMENSIONS
  , LINESEGMENT_END
  , GRAPHICAL_OBJECT
  , LAYOUT
  , LAYOUTID
  , LIST_OF_ADDITIONAL_GRAPHICAL_OBJECTS
  , LIST_OF_COMPARTMENT_GLYPHS
  , LIST_OF_CURVE_SEGMENTS
  , LIST_OF_LAYOUTS
  , LIST_OF_REACTION_GLYPHS
  , LIST_OF_SPECIES_GLYPHS
  , LIST_OF_SPECIES_REFERENCE_GLYPHS
  , LIST_OF_TEXT_GLYPHS
  , BOUNDING_BOX_POSITION
  , REACTION_GLYPH   
  , SPECIES_GLYPH
  , SPECIES_REFERENCE_GLYPH
  , LINESEGMENT_START
  , TEXT_GLYPH
};


/**
 * Returns the tag code for the given SBML element.
 */
LayoutTagCode_t
LayoutTagCode_forElement (const XMLCh* name)
{
  LayoutTagCode_t hc  = TAG_UNKNOWN;
  LayoutTagCode_t lo  = TAG_CUBIC_BEZIER_BASE_POINT_1;
  LayoutTagCode_t hi  = TAG_TEXT_GLYPH;
  LayoutTagCode_t mid;


  int cond;
  if (name && *name)
  {
    /* Classic Binary Search */
    while (lo <= hi)
    {
      mid  = (lo + hi) / 2;
      cond = XMLString::compareString(name, LAYOUT_ELEMENTS[mid-1000]);
      if (cond < 0)
      {
        hi = mid - 1;
      }
      else if (cond > 0)
      {
        lo = mid + 1;
      }
      else
      {
        hc = mid;
        break;
      }
    }
  }
  if(hc==TAG_UNKNOWN)
  {
    if(!XMLString::compareString(name, ELEM_NOTES))
    {
        hc=TAG_NOTES;
    }
    else if(!XMLString::compareString(name, ELEM_ANNOTATIONS))
    {
        hc=TAG_ANNOTATIONS;
    }
    else if(!XMLString::compareString(name, ELEM_ANNOTATION))
    {
        hc=TAG_ANNOTATIONS;
    }
  }
  return hc;
}
