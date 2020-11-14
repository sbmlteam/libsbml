/**
 * @file local-packages-arrays.i
 * @brief Casting to most specific packages object for csharp
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

/**
 * Adds DownCastBase(long cPtr, boolean owner) method for the arrays package extension
 */
%typemap(cscode) ArraysExtension
%{
	public override SBasePlugin DowncastSBasePlugin(IntPtr cPtr, bool owner)
	{
		if (cPtr.Equals(IntPtr.Zero)) return null;
		
		SBasePlugin sbp = new SBasePlugin(cPtr, false);
		SBase sb = sbp.getParentSBMLObject();

		switch( sb.getTypeCode() )
		{
			default:
				return new ArraysSBasePlugin(cPtr,owner);
		}
	}

	public override SBase DowncastSBase(IntPtr cPtr, bool owner)
	{
		if (cPtr.Equals(IntPtr.Zero)) return null;
		
		SBase sb = new SBase(cPtr, false);
		switch( sb.getTypeCode() )
		{
			case (int) libsbml.SBML_LIST_OF:
				string name = sb.getElementName();
				if(name =="listOfDimensions")
				{
					return new ListOfDimensions(cPtr, owner);
				}
				else if(name =="listOfIndices")
				{
					return new ListOfIndices(cPtr, owner);
				}
				return new ListOf(cPtr, owner);
				
			case (int) libsbml.SBML_ARRAYS_DIMENSION:
				return new Dimension(cPtr, owner);
				
			case (int) libsbml.SBML_ARRAYS_INDEX:
				return new Index(cPtr, owner);

			default:
				return new SBase(cPtr, owner);
		}
	}
%}

COVARIANT_RTYPE_CLONE(ArraysExtension)
COVARIANT_RTYPE_CLONE(Index)
COVARIANT_RTYPE_CLONE(Dimension)
COVARIANT_RTYPE_CLONE(ListOfIndices)
COVARIANT_RTYPE_CLONE(ListOfDimensions)

COVARIANT_RTYPE_LISTOF_GET_REMOVE(Dimension)
COVARIANT_RTYPE_LISTOF_GET_REMOVE(Index)

%define COVARIANT_RTYPE_ISSETID(_CNAME_)
%typemap(cstype) _CNAME_* _CNAME_::isSetId  "_CNAME_"
%csmethodmodifiers _CNAME_::isSetId  "public new"
%enddef

COVARIANT_RTYPE_ISSETID(Dimension)

SBMLCONSTRUCTOR_EXCEPTION(ArraysPkgNamespaces)
SBMLCONSTRUCTOR_EXCEPTION(Dimension)
SBMLCONSTRUCTOR_EXCEPTION(Index)
SBMLCONSTRUCTOR_EXCEPTION(ListOfDimensions)
SBMLCONSTRUCTOR_EXCEPTION(ListOfIndices)

#endif  /* USE_ARRAYS */

