/**
 * @file local-packages-distrib.i
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
 * 1. California Institute of Technology, Pasadena, CA, USA
 * 2. University of Heidelberg, Heidelberg, Germany
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


#ifdef USE_DISTRIB
%typemap(cscode) DistribExtension
%{
  public override SBasePlugin DowncastSBasePlugin(IntPtr cPtr, bool owner)
  {
    if (cPtr.Equals(IntPtr.Zero)) return null;

    SBasePlugin sbp = new SBasePlugin(cPtr, false);
    SBase sb = sbp.getParentSBMLObject();

    switch ( sb.getTypeCode() )
    {
      case (int) libsbml.SBML_DOCUMENT:
        return new DistribSBMLDocumentPlugin(cPtr, owner);

      default:
        return new DistribSBasePlugin(cPtr, owner);
    }
  }

  public override SBase DowncastSBase(IntPtr cPtr, bool owner)
  {
    if (cPtr.Equals(IntPtr.Zero)) return null;

    SBase sb = new SBase(cPtr, false);
    switch ( sb.getTypeCode() )
    {
      case (int) libsbml.SBML_LIST_OF:
        string name = sb.getElementName();
        if (name == "listOfUncertParameters")
        {
          return new ListOfUncertParameters(cPtr, owner);
        }
        else if (name == "listOfUncertainties")
        {
          return new ListOfUncertainties(cPtr, owner);
        }

        return new ListOf(cPtr, owner);

      case (int) libsbml.SBML_DISTRIB_UNCERTPARAMETER:
        return new UncertParameter(cPtr, owner);

      case (int) libsbml.SBML_DISTRIB_UNCERTAINTY:
        return new Uncertainty(cPtr, owner);

      case (int) libsbml.SBML_DISTRIB_UNCERTSTATISTICSPAN:
        return new UncertSpan(cPtr, owner);

      default:
        return new SBase(cPtr, owner);
    }
  }

%}

COVARIANT_RTYPE_CLONE(DistribExtension)
COVARIANT_RTYPE_CLONE(UncertParameter)
COVARIANT_RTYPE_CLONE(Uncertainty)
COVARIANT_RTYPE_CLONE(UncertSpan)

COVARIANT_RTYPE_CLONE(ListOfUncertParameters)
COVARIANT_RTYPE_CLONE(ListOfUncertainties)

COVARIANT_RTYPE_LISTOF_GET_REMOVE(UncertParameter)
COVARIANT_RTYPE_LISTOF_GET_REMOVE(Uncertainty)

SBMLCONSTRUCTOR_EXCEPTION(DistribPkgNamespaces)
SBMLCONSTRUCTOR_EXCEPTION(UncertParameter)
SBMLCONSTRUCTOR_EXCEPTION(Uncertainty)
SBMLCONSTRUCTOR_EXCEPTION(UncertSpan)

SBMLCONSTRUCTOR_EXCEPTION(ListOfUncertParameters)
SBMLCONSTRUCTOR_EXCEPTION(ListOfUncertainties)

//
// Convert UncertParameter objects into the most specific object possible.
//
%typemap("csout") UncertParameter*
{
  return (UncertParameter) libsbml.DowncastSBase($imcall, $owner);
}

#endif // USE_DISTRIB

