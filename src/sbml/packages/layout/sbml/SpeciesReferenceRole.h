/**
 * @file    SpeciesReferenceRole.h
 * @brief   Definition of SpeciesReferenceRole enum for SBML Layout.
 * @author  Ralph Gauges
 * 
 * <!--------------------------------------------------------------------------
 * Description : SBML Layout SpeciesReferenceRole C Header
 * Organization: European Media Laboratories Research gGmbH
 * Created     : 2004-07-15
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
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
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
 *
 *     Akiya Jouraku <jouraku@bio.keio.ac.jp>
 *     Modified this file for package extension in libSBML5
 *------------------------------------------------------------------------- -->
 */

#include <sbml/common/extern.h>


#ifndef SpeciesReferenceRole_H__
#define SpeciesReferenceRole_H__

LIBSBML_CPP_NAMESPACE_BEGIN
BEGIN_C_DECLS


/**
 * @enum  SpeciesReferenceRole_t
 * @brief SpeciesReferenceRole_t is the enumeration of possible values for the 'role' attribute of a SpeciesReferenceGlyph.
 *
 * The role attribute is of type SpeciesReferenceRole and is used to specify how the species reference should be displayed. Allowed values are 'substrate', 'product', 'sidesubstrate', 'sideproduct', 'modifier', 'activator', 'inhibitor' and 'undefined'. 
 *
 * This attribute is optional and should only be necessary if the optional speciesReference attribute is not given or if the respective information from the model needs to be overridden.
 */
LIBSBML_EXTERN
typedef enum
{
    SPECIES_ROLE_UNDEFINED /*!< 'undefined':  The role of the referenced Species is undefined. */
  , SPECIES_ROLE_SUBSTRATE /*!< 'substrate':  The referenced Species is a principle substrate of the reaction. */
  , SPECIES_ROLE_PRODUCT /*!< 'product':  The referenced Species is a principle product of the reaction. */
  , SPECIES_ROLE_SIDESUBSTRATE /*!< 'sidesubstrate':  The referenced Species is a side substrate of the reaction.  Used for simple chemicals such as ATP, NAD+, etc.*/
  , SPECIES_ROLE_SIDEPRODUCT /*!< 'sideproduct':  The referenced Species is a side product of the reaction.  Used for simple chemicals such as ATP, NAD+, etc. */
  , SPECIES_ROLE_MODIFIER /*!< 'modifier':  The referenced Species influences the reaction in some way, but is not produced or consumed by it. */
  , SPECIES_ROLE_ACTIVATOR /*!< The referenced Species acts as an activator of the reaction. */
  , SPECIES_ROLE_INHIBITOR /*!< The referenced Species acts as an inhibitor of the reaction. */
} SpeciesReferenceRole_t;


END_C_DECLS
LIBSBML_CPP_NAMESPACE_END


#endif  /* SpeciesReferenceRole_H__ */
