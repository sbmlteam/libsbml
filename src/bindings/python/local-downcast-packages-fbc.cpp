#ifdef USE_FBC
else if (pkgName == "fbc")
{
  switch (sb->getTypeCode() )
  {
    case SBML_LIST_OF:
      name = sb->getElementName();
      if (name == "listOfFbcAssociations")
      {
        return SWIGTYPE_p_ListOfFbcAssociations;
      }
      else if(name == "listOfFluxes"){
				return SWIGTYPE_p_ListOfFluxObjectives;
			}

      else if (name == "listOfFluxObjectives" || name == "listOfFluxes")
      {
        return SWIGTYPE_p_ListOfFluxObjectives;
      }
      else if (name == "listOfObjectives")
      {
        return SWIGTYPE_p_ListOfObjectives;
      }
      else if (name == "listOfGeneProducts")
      {
        return SWIGTYPE_p_ListOfGeneProducts;
      }
      else if (name == "listOfUserDefinedConstraintComponents")
      {
        return SWIGTYPE_p_ListOfUserDefinedConstraintComponents;
      }
      else if (name == "listOfUserDefinedConstraints")
      {
        return SWIGTYPE_p_ListOfUserDefinedConstraints;
      }
      else if (name == "listOfKeyValuePairs")
      {
        return SWIGTYPE_p_ListOfKeyValuePairs;
      }

      return SWIGTYPE_p_ListOf;

    case SBML_FBC_ASSOCIATION:
      return SWIGTYPE_p_FbcAssociation;
		case SBML_FBC_V1ASSOCIATION:
			return SWIGTYPE_p_Association;
		case SBML_FBC_FLUXBOUND:
			return SWIGTYPE_p_FluxBound;

    case SBML_FBC_FLUXOBJECTIVE:
      return SWIGTYPE_p_FluxObjective;

		case SBML_FBC_GENEASSOCIATION:
			return SWIGTYPE_p_GeneAssociation;

    case SBML_FBC_GENEPRODUCTASSOCIATION:
      return SWIGTYPE_p_GeneProductAssociation;

    case SBML_FBC_OBJECTIVE:
      return SWIGTYPE_p_Objective;

    case SBML_FBC_GENEPRODUCT:
      return SWIGTYPE_p_GeneProduct;

    case SBML_FBC_GENEPRODUCTREF:
      return SWIGTYPE_p_GeneProductRef;

    case SBML_FBC_AND:
      return SWIGTYPE_p_FbcAnd;

    case SBML_FBC_OR:
      return SWIGTYPE_p_FbcOr;

    case SBML_FBC_USERDEFINEDCONSTRAINTCOMPONENT:
      return SWIGTYPE_p_UserDefinedConstraintComponent;

    case SBML_FBC_USERDEFINEDCONSTRAINT:
      return SWIGTYPE_p_UserDefinedConstraint;

    case SBML_FBC_KEYVALUEPAIR:
      return SWIGTYPE_p_KeyValuePair;
    default:
      return SWIGTYPE_p_SBase;
  }
}

#endif // USE_FBC 

