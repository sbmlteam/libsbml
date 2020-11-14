/**
 * @file local-downcast-packages-arrays.cpp
 * @brief Casting to most specific packages object for php
 * @author SBMLTeam
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML. Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
 *
 * Copyright (C) 2019 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 * 3. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2009-2013 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 * Pasadena, CA, USA
 *
 * Copyright (C) 2002-2005 jointly by the following organizations:
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. Japan Science and Technology Agency, Japan
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation. A copy of the license agreement is provided in the
 * file named "LICENSE.txt" included with this software distribution and also
 * available online as http://sbml.org/software/libsbml/license.html
 * ------------------------------------------------------------------------ -->
 */


#ifdef USE_ARRAYS
else if (pkgName == "arrays")
{
  switch ( sb->getTypeCode() )
  {
    case SBML_LIST_OF:
      name = sb->getElementName();
	  if (name =="listOfIndices")
      {
				return SWIGTYPE_p_ListOfIndices;
      }
      else if (name == "listOfDimensions")
      {
        return SWIGTYPE_p_ListOfDimensions;
      }

      return SWIGTYPE_p_ListOf;

    case SBML_ARRAYS_INDEX:
      return SWIGTYPE_p_Index;

    case SBML_ARRAYS_DIMENSION:
      return SWIGTYPE_p_Dimension;

    default:
      return SWIGTYPE_p_SBase;
    }
  }

  #endif // USE_ARRAYS

