/**
 * @file:   local-packages-req.i
 * @brief:  Implementation of the req class
 * @author: SBMLTeam
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
 * Copyright (C) 2019 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. University of Heidelberg, Heidelberg, Germany
 *
 * Copyright (C) 2013-2018 jointly by the following organizations:
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EMBL-EBI), Hinxton, UK
 *     3. Unversity of Heidelberg, Heidelberg, Germany
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
#ifdef USE_REQUIREDELEMENTS
/**
 * Adds DownCastBase(long cPtr, boolean owner) method for the req package extension
 */
%typemap(javacode) ReqExtension
%{
  public SBasePlugin DowncastSBasePlugin(long cPtr, boolean owner)
  {
    if (cPtr == 0) return null;

    SBasePlugin sbp = new SBasePlugin(cPtr, false);
    SBase sb = sbp.getParentSBMLObject();

    switch( sb.getTypeCode() )
    {
      default:
        return new ReqSBasePlugin(cPtr, owner);
    }
  }

  public SBase DowncastSBase(long cPtr, boolean owner)
  {
    if (cPtr == 0) return null;

    SBase sb = new SBase(cPtr, false);
    switch( sb.getTypeCode() )
    {
      case (int) libsbml.SBML_LIST_OF:
        String name = sb.getElementName();
        if (name == "listOfChangedMaths")
        {
          return new ListOfChangedMaths(cPtr, owner);
        }

        return new ListOf(cPtr, owner);

      case (int) libsbml.SBML_REQ_CHANGED_MATH:
        return new ChangedMath(cPtr, owner);

      default:
        return new SBase(cPtr, owner);
    }
  }

%}

COVARIANT_RTYPE_CLONE(ReqExtension)
COVARIANT_RTYPE_CLONE(ChangedMath)
COVARIANT_RTYPE_CLONE(ListOfChangedMaths)

COVARIANT_RTYPE_LISTOF_GET_REMOVE(ChangedMath)

SBMLCONSTRUCTOR_EXCEPTION(ReqPkgNamespaces)
SBMLCONSTRUCTOR_EXCEPTION(ChangedMath)
SBMLCONSTRUCTOR_EXCEPTION(ListOfChangedMaths)

#endif /* USE_REQUIREDELEMENTS */

