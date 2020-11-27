/**
 * @file:   local-packages-multi.i
 * @brief:  Implementation of the multi class
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
 * ------------------------------------------------------------------------ -->
 */
#ifdef USE_MULTI
/**
 * Adds DownCastBase(long cPtr, boolean owner) method for the multi package extension
 */
%typemap(cscode) MultiExtension
%{
  public override SBasePlugin DowncastSBasePlugin(IntPtr cPtr, bool owner)
  {
    if (cPtr.Equals(IntPtr.Zero)) return null;

    SBasePlugin sbp = new SBasePlugin(cPtr, false);
    SBase sb = sbp.getParentSBMLObject();

    if (sb.getElementName() == "listOfReactions")
      return new MultiListOfReactionsPlugin(cPtr, owner);

    if (sb is Model)
    {
      return new MultiModelPlugin(cPtr, owner);
    }
    
    switch( sb.getTypeCode() )
    {
      case (int) libsbml.SBML_DOCUMENT:
        return new MultiSBMLDocumentPlugin(cPtr, owner);

      case (int) libsbml.SBML_MODEL:
        return new MultiModelPlugin(cPtr, owner);

      case (int) libsbml.SBML_COMPARTMENT:
        return new MultiCompartmentPlugin(cPtr, owner);

      case (int) libsbml.SBML_SPECIES:
        return new MultiSpeciesPlugin(cPtr, owner);

      case (int) libsbml.SBML_MODIFIER_SPECIES_REFERENCE:
        return new MultiSimpleSpeciesReferencePlugin(cPtr, owner);

      case (int) libsbml.SBML_SPECIES_REFERENCE:
        return new MultiSpeciesReferencePlugin(cPtr, owner);

      default:
        return new SBasePlugin(cPtr, owner);
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
        if (name == "listOfPossibleSpeciesFeatureValues")
        {
          return new ListOfPossibleSpeciesFeatureValues(cPtr, owner);
        }
        else if (name == "listOfSpeciesFeatureValues")
        {
          return new ListOfSpeciesFeatureValues(cPtr, owner);
        }
        else if (name == "listOfCompartmentReferences")
        {
          return new ListOfCompartmentReferences(cPtr, owner);
        }
        else if (name == "listOfSpeciesTypeInstances")
        {
          return new ListOfSpeciesTypeInstances(cPtr, owner);
        }
        else if (name == "listOfInSpeciesTypeBonds")
        {
          return new ListOfInSpeciesTypeBonds(cPtr, owner);
        }
        else if (name == "listOfOutwardBindingSites")
        {
          return new ListOfOutwardBindingSites(cPtr, owner);
        }
        else if (name == "listOfSpeciesFeatureTypes")
        {
          return new ListOfSpeciesFeatureTypes(cPtr, owner);
        }
        else if (name == "listOfSpeciesTypeComponentIndexes")
        {
          return new ListOfSpeciesTypeComponentIndexes(cPtr, owner);
        }
        else if (name == "listOfSpeciesFeatures")
        {
          return new ListOfSpeciesFeatures(cPtr, owner);
        }
        else if (name == "listOfSpeciesTypeComponentMapInProducts")
        {
          return new ListOfSpeciesTypeComponentMapInProducts(cPtr, owner);
        }
        else if (name == "listOfMultiSpeciesTypes")
        {
          return new ListOfMultiSpeciesTypes(cPtr, owner);
        }

        return new ListOf(cPtr, owner);

      case (int) libsbml.SBML_MULTI_POSSIBLE_SPECIES_FEATURE_VALUE:
        return new PossibleSpeciesFeatureValue(cPtr, owner);

      case (int) libsbml.SBML_MULTI_SPECIES_FEATURE_VALUE:
        return new SpeciesFeatureValue(cPtr, owner);

      case (int) libsbml.SBML_MULTI_COMPARTMENT_REFERENCE:
        return new CompartmentReference(cPtr, owner);

      case (int) libsbml.SBML_MULTI_SPECIES_TYPE_INSTANCE:
        return new SpeciesTypeInstance(cPtr, owner);

      case (int) libsbml.SBML_MULTI_IN_SPECIES_TYPE_BOND:
        return new InSpeciesTypeBond(cPtr, owner);

      case (int) libsbml.SBML_MULTI_OUTWARD_BINDING_SITE:
        return new OutwardBindingSite(cPtr, owner);

      case (int) libsbml.SBML_MULTI_SPECIES_FEATURE_TYPE:
        return new SpeciesFeatureType(cPtr, owner);

      case (int) libsbml.SBML_MULTI_SPECIES_TYPE_COMPONENT_INDEX:
        return new SpeciesTypeComponentIndex(cPtr, owner);

      case (int) libsbml.SBML_MULTI_SPECIES_FEATURE:
        return new SpeciesFeature(cPtr, owner);

      case (int) libsbml.SBML_MULTI_SPECIES_TYPE_COMPONENT_MAP_IN_PRODUCT:
        return new SpeciesTypeComponentMapInProduct(cPtr, owner);

      case (int) libsbml.SBML_MULTI_SPECIES_TYPE:
        return new MultiSpeciesType(cPtr, owner);

      case (int) libsbml.SBML_MULTI_BINDING_SITE_SPECIES_TYPE:
        return new BindingSiteSpeciesType(cPtr, owner);

      case (int) libsbml.SBML_MULTI_INTRA_SPECIES_REACTION:
        return new IntraSpeciesReaction(cPtr, owner);

      default:
        return new SBase(cPtr, owner);
    }
  }

%}

COVARIANT_RTYPE_CLONE(MultiExtension)
COVARIANT_RTYPE_CLONE(PossibleSpeciesFeatureValue)
COVARIANT_RTYPE_CLONE(SpeciesFeatureValue)
COVARIANT_RTYPE_CLONE(CompartmentReference)
COVARIANT_RTYPE_CLONE(SpeciesTypeInstance)
COVARIANT_RTYPE_CLONE(InSpeciesTypeBond)
COVARIANT_RTYPE_CLONE(OutwardBindingSite)
COVARIANT_RTYPE_CLONE(SpeciesFeatureType)
COVARIANT_RTYPE_CLONE(SpeciesTypeComponentIndex)
COVARIANT_RTYPE_CLONE(SpeciesFeature)
COVARIANT_RTYPE_CLONE(SpeciesTypeComponentMapInProduct)
COVARIANT_RTYPE_CLONE(MultiSpeciesType)
COVARIANT_RTYPE_CLONE(BindingSiteSpeciesType)
COVARIANT_RTYPE_CLONE(IntraSpeciesReaction)
COVARIANT_RTYPE_CLONE(ListOfPossibleSpeciesFeatureValues)
COVARIANT_RTYPE_CLONE(ListOfSpeciesFeatureValues)
COVARIANT_RTYPE_CLONE(ListOfCompartmentReferences)
COVARIANT_RTYPE_CLONE(ListOfSpeciesTypeInstances)
COVARIANT_RTYPE_CLONE(ListOfInSpeciesTypeBonds)
COVARIANT_RTYPE_CLONE(ListOfOutwardBindingSites)
COVARIANT_RTYPE_CLONE(ListOfSpeciesFeatureTypes)
COVARIANT_RTYPE_CLONE(ListOfSpeciesTypeComponentIndexes)
COVARIANT_RTYPE_CLONE(ListOfSpeciesFeatures)
COVARIANT_RTYPE_CLONE(ListOfSpeciesTypeComponentMapInProducts)
COVARIANT_RTYPE_CLONE(ListOfMultiSpeciesTypes)

COVARIANT_RTYPE_LISTOF_GET_REMOVE(PossibleSpeciesFeatureValue)
COVARIANT_RTYPE_LISTOF_GET_REMOVE(SpeciesFeatureValue)
COVARIANT_RTYPE_LISTOF_GET_REMOVE(CompartmentReference)
COVARIANT_RTYPE_LISTOF_GET_REMOVE(SpeciesTypeInstance)
COVARIANT_RTYPE_LISTOF_GET_REMOVE(InSpeciesTypeBond)
COVARIANT_RTYPE_LISTOF_GET_REMOVE(OutwardBindingSite)
COVARIANT_RTYPE_LISTOF_GET_REMOVE(SpeciesFeatureType)
COVARIANT_RTYPE_LISTOF_GET_REMOVE(SpeciesTypeComponentIndex)
COVARIANT_RTYPE_LISTOF_GET_REMOVE(SpeciesFeature)
COVARIANT_RTYPE_LISTOF_GET_REMOVE(SpeciesTypeComponentMapInProduct)
COVARIANT_RTYPE_LISTOF_GET_REMOVE(MultiSpeciesType)

SBMLCONSTRUCTOR_EXCEPTION(MultiPkgNamespaces)
SBMLCONSTRUCTOR_EXCEPTION(PossibleSpeciesFeatureValue)
SBMLCONSTRUCTOR_EXCEPTION(SpeciesFeatureValue)
SBMLCONSTRUCTOR_EXCEPTION(CompartmentReference)
SBMLCONSTRUCTOR_EXCEPTION(SpeciesTypeInstance)
SBMLCONSTRUCTOR_EXCEPTION(InSpeciesTypeBond)
SBMLCONSTRUCTOR_EXCEPTION(OutwardBindingSite)
SBMLCONSTRUCTOR_EXCEPTION(SpeciesFeatureType)
SBMLCONSTRUCTOR_EXCEPTION(SpeciesTypeComponentIndex)
SBMLCONSTRUCTOR_EXCEPTION(SpeciesFeature)
SBMLCONSTRUCTOR_EXCEPTION(SpeciesTypeComponentMapInProduct)
SBMLCONSTRUCTOR_EXCEPTION(MultiSpeciesType)
SBMLCONSTRUCTOR_EXCEPTION(BindingSiteSpeciesType)
SBMLCONSTRUCTOR_EXCEPTION(IntraSpeciesReaction)
SBMLCONSTRUCTOR_EXCEPTION(ListOfPossibleSpeciesFeatureValues)
SBMLCONSTRUCTOR_EXCEPTION(ListOfSpeciesFeatureValues)
SBMLCONSTRUCTOR_EXCEPTION(ListOfCompartmentReferences)
SBMLCONSTRUCTOR_EXCEPTION(ListOfSpeciesTypeInstances)
SBMLCONSTRUCTOR_EXCEPTION(ListOfInSpeciesTypeBonds)
SBMLCONSTRUCTOR_EXCEPTION(ListOfOutwardBindingSites)
SBMLCONSTRUCTOR_EXCEPTION(ListOfSpeciesFeatureTypes)
SBMLCONSTRUCTOR_EXCEPTION(ListOfSpeciesTypeComponentIndexes)
SBMLCONSTRUCTOR_EXCEPTION(ListOfSpeciesFeatures)
SBMLCONSTRUCTOR_EXCEPTION(ListOfSpeciesTypeComponentMapInProducts)
SBMLCONSTRUCTOR_EXCEPTION(ListOfMultiSpeciesTypes)

//
// Convert MultiSpeciesType objects into the most specific object possible.
//
%typemap("csout") MultiSpeciesType*
{
	return (MultiSpeciesType) libsbml.DowncastSBase($imcall, $owner);
}

#endif /* USE_MULTI */

