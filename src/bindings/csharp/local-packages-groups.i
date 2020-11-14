/**
 * @file    local-layout.i
 * @brief   cs-specific SWIG directives for wrapping libSBML layout API this file 
 *          has been adapted from the SWIG java bindings written by 
 * 	    Ben Bornstein and Akiya Jouraku
 * @author  Frank Bergmann (fbergman@u.washington.edu)
 * @author  Akiya Jouraku
 *
 * $Id: local.i 10190 2009-09-23 16:03:35Z ajouraku $
 * $URL: https://sbml.svn.sourceforge.net/svnroot/sbml/trunk/libsbml/src/bindings/csharp/local.i $
 *
 *
 * <!--------------------------------------------------------------------------
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 *
 * Copyright (C) 2020 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *     3. University College London, London, UK
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
 * ------------------------------------------------------------------------ -->
 */

#ifdef USE_GROUPS

/**
 * Adds DownCastBase(long cPtr, boolean owner) method for the groups package extension
 */
%typemap(cscode) GroupsExtension
%{
	public override SBasePlugin DowncastSBasePlugin(IntPtr cPtr, bool owner)
	{
		if (cPtr.Equals(IntPtr.Zero)) return null;
		
		SBasePlugin sbp = new SBasePlugin(cPtr, false);
		SBase sb = sbp.getParentSBMLObject();
    
    if (sb is Model)
    {
      return new GroupsModelPlugin(cPtr, owner);
    }
    
		switch( sb.getTypeCode() )
		{
			case (int) libsbml.SBML_MODEL:
				return new GroupsModelPlugin(cPtr,owner);
			default:
				return new SBasePlugin(cPtr,owner);
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
        if (name == "listOfGroups")
        {
          return new ListOfGroups(cPtr, owner);
        }
        else if (name == "listOfMembers")
        {
          return new ListOfMembers(cPtr, owner);
        }

        return new ListOf(cPtr, owner);

      case (int) libsbml.SBML_GROUPS_GROUP:
        return new Group(cPtr, owner);

      case (int) libsbml.SBML_GROUPS_MEMBER:
        return new Member(cPtr, owner);

      default:
        return new SBase(cPtr, owner);
		}
	}
%}

COVARIANT_RTYPE_CLONE(GroupsExtension)
COVARIANT_RTYPE_CLONE(Group)
COVARIANT_RTYPE_CLONE(Member)
COVARIANT_RTYPE_CLONE(ListOfGroups)
COVARIANT_RTYPE_CLONE(ListOfMembers)

COVARIANT_RTYPE_LISTOF_GET_REMOVE(Group)
COVARIANT_RTYPE_LISTOF_GET_REMOVE(Member)

SBMLCONSTRUCTOR_EXCEPTION(GroupsPkgNamespaces)
SBMLCONSTRUCTOR_EXCEPTION(Group)
SBMLCONSTRUCTOR_EXCEPTION(Member)
SBMLCONSTRUCTOR_EXCEPTION(ListOfGroups)
SBMLCONSTRUCTOR_EXCEPTION(ListOfMembers)

#endif  /* USE_GROUPS */

