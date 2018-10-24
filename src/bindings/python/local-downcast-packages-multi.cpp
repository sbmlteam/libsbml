#ifdef USE_MULTI
else if (pkgName == "multi")
{
  switch (sb->getTypeCode() )
  {
    case SBML_LIST_OF:
      name = sb->getElementName();
      if (name == "listOfPossibleSpeciesFeatureValues")
      {
        return SWIGTYPE_p_ListOfPossibleSpeciesFeatureValues;
      }
      else if (name == "listOfSpeciesFeatureValues")
      {
        return SWIGTYPE_p_ListOfSpeciesFeatureValues;
      }
      else if (name == "listOfCompartmentReferences")
      {
        return SWIGTYPE_p_ListOfCompartmentReferences;
      }
      else if (name == "listOfSpeciesTypeInstances")
      {
        return SWIGTYPE_p_ListOfSpeciesTypeInstances;
      }
      else if (name == "listOfInSpeciesTypeBonds")
      {
        return SWIGTYPE_p_ListOfInSpeciesTypeBonds;
      }
      else if (name == "listOfOutwardBindingSites")
      {
        return SWIGTYPE_p_ListOfOutwardBindingSites;
      }
      else if (name == "listOfSpeciesFeatureTypes")
      {
        return SWIGTYPE_p_ListOfSpeciesFeatureTypes;
      }
      else if (name == "listOfSpeciesTypeComponentIndexes")
      {
        return SWIGTYPE_p_ListOfSpeciesTypeComponentIndexes;
      }
      else if (name == "listOfSpeciesFeatures")
      {
        return SWIGTYPE_p_ListOfSpeciesFeatures;
      }
      else if (name == "listOfSpeciesTypeComponentMapInProducts")
      {
        return SWIGTYPE_p_ListOfSpeciesTypeComponentMapInProducts;
      }
      else if (name == "listOfMultiSpeciesTypes")
      {
        return SWIGTYPE_p_ListOfMultiSpeciesTypes;
      }

      return SWIGTYPE_p_ListOf;

    case SBML_MULTI_POSSIBLE_SPECIES_FEATURE_VALUE:
      return SWIGTYPE_p_PossibleSpeciesFeatureValue;

    case SBML_MULTI_SPECIES_FEATURE_VALUE:
      return SWIGTYPE_p_SpeciesFeatureValue;

    case SBML_MULTI_COMPARTMENT_REFERENCE:
      return SWIGTYPE_p_CompartmentReference;

    case SBML_MULTI_SPECIES_TYPE_INSTANCE:
      return SWIGTYPE_p_SpeciesTypeInstance;

    case SBML_MULTI_IN_SPECIES_TYPE_BOND:
      return SWIGTYPE_p_InSpeciesTypeBond;

    case SBML_MULTI_OUTWARD_BINDING_SITE:
      return SWIGTYPE_p_OutwardBindingSite;

    case SBML_MULTI_SPECIES_FEATURE_TYPE:
      return SWIGTYPE_p_SpeciesFeatureType;

    case SBML_MULTI_SPECIES_TYPE_COMPONENT_INDEX:
      return SWIGTYPE_p_SpeciesTypeComponentIndex;

    case SBML_MULTI_SPECIES_FEATURE:
      return SWIGTYPE_p_SpeciesFeature;

    case SBML_MULTI_SPECIES_TYPE_COMPONENT_MAP_IN_PRODUCT:
      return SWIGTYPE_p_SpeciesTypeComponentMapInProduct;

    case SBML_MULTI_SPECIES_TYPE:
      return SWIGTYPE_p_MultiSpeciesType;

    case SBML_MULTI_BINDING_SITE_SPECIES_TYPE:
      return SWIGTYPE_p_BindingSiteSpeciesType;

    case SBML_MULTI_INTRA_SPECIES_REACTION:
      return SWIGTYPE_p_IntraSpeciesReaction;

    default:
      return SWIGTYPE_p_SBase;
  }
}

#endif // USE_MULTI 

