/**
 * @file   local-packages-fbc.i
 * @brief  Implementation of the fbc class
 * @author SBMLTeam
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
#ifdef USE_FBC
/**
 * Adds DownCastBase(long cPtr, boolean owner) method for the fbc package extension
 */
%typemap(javacode) FbcExtension
%{
	/**
         * @internal
         */
  public SBasePlugin DowncastSBasePlugin(long cPtr, boolean owner)
  {
    if (cPtr == 0) return null;

    SBasePlugin sbp = new SBasePlugin(cPtr, false);
    SBase sb = sbp.getParentSBMLObject();

    if (sb instanceof Model)
    {
      return new FbcModelPlugin(cPtr, owner);
    }
    
    switch( sb.getTypeCode() )
    {
      case (int) libsbml.SBML_SPECIES:
        return new FbcSpeciesPlugin(cPtr, owner);

      case (int) libsbml.SBML_MODEL:
        return new FbcModelPlugin(cPtr, owner);

      case (int) libsbml.SBML_REACTION:
        return new FbcReactionPlugin(cPtr, owner);

      case (int) libsbml.SBML_DOCUMENT:
        return new FbcSBMLDocumentPlugin(cPtr, owner);
        
      default:
        return new FbcSBasePlugin(cPtr, owner);
    }
  }

	/**
         * @internal
         */
  public SBase DowncastSBase(long cPtr, boolean owner)
  {
    if (cPtr == 0) return null;

    SBase sb = new SBase(cPtr, false);
    switch( sb.getTypeCode() )
    {
      case (int) libsbml.SBML_LIST_OF:
        String name = sb.getElementName();
        if (name.equals("listOfFbcAssociations"))
        {
          return new ListOfFbcAssociations(cPtr, owner);
        }
        else if(name.equals("listOfFluxBounds"))
			     {
					return new ListOfFluxBounds(cPtr, owner);
                 }
        else if (name.equals("listOfFluxes") || name.equals("listOfFluxObjectives"))
        {
          return new ListOfFluxObjectives(cPtr, owner);
        }
        else if(name.equals("listOfGeneAssociations"))
			     {
		            return new ListOfGeneAssociations(cPtr, owner);
                 }
        else if (name.equals("listOfObjectives"))
        {
          return new ListOfObjectives(cPtr, owner);
        }
        else if (name.equals("listOfGeneProducts"))
        {
          return new ListOfGeneProducts(cPtr, owner);
        }

        else if (name.equals("listOfUserDefinedConstraintComponents"))
        {
          return new ListOfUserDefinedConstraintComponents(cPtr, owner);
        }
        else if (name.equals("listOfUserDefinedConstraints"))
        {
          return new ListOfUserDefinedConstraints(cPtr, owner);
        }
        else if (name.equals("listOfKeyValuePairs"))
        {
          return new ListOfKeyValuePairs(cPtr, owner);
        }
        return new ListOf(cPtr, owner);

      case (int) libsbml.SBML_FBC_ASSOCIATION:
        return new FbcAssociation(cPtr, owner);

			case (int) libsbml.SBML_FBC_V1ASSOCIATION:
				return new Association(cPtr, owner);
				
			case (int) libsbml.SBML_FBC_FLUXBOUND:
				return new FluxBound(cPtr, owner);

      case (int) libsbml.SBML_FBC_FLUXOBJECTIVE:
        return new FluxObjective(cPtr, owner);

      case (int) libsbml.SBML_FBC_GENEPRODUCTASSOCIATION:
        return new GeneProductAssociation(cPtr, owner);

			case (int) libsbml.SBML_FBC_GENEASSOCIATION:
				return new GeneAssociation(cPtr, owner);
				
      case (int) libsbml.SBML_FBC_OBJECTIVE:
        return new Objective(cPtr, owner);

      case (int) libsbml.SBML_FBC_GENEPRODUCT:
        return new GeneProduct(cPtr, owner);

      case (int) libsbml.SBML_FBC_GENEPRODUCTREF:
        return new GeneProductRef(cPtr, owner);

      case (int) libsbml.SBML_FBC_AND:
        return new FbcAnd(cPtr, owner);

      case (int) libsbml.SBML_FBC_OR:
        return new FbcOr(cPtr, owner);

      case (int) libsbml.SBML_FBC_USERDEFINEDCONSTRAINTCOMPONENT:
        return new UserDefinedConstraintComponent(cPtr, owner);

      case (int) libsbml.SBML_FBC_USERDEFINEDCONSTRAINT:
        return new UserDefinedConstraint(cPtr, owner);

      case (int) libsbml.SBML_FBC_KEYVALUEPAIR:
        return new KeyValuePair(cPtr, owner);

      default:
        return new SBase(cPtr, owner);
    }
  }

%}

COVARIANT_RTYPE_CLONE(FbcExtension)
COVARIANT_RTYPE_CLONE(Association)
COVARIANT_RTYPE_CLONE(GeneAssociation)
COVARIANT_RTYPE_CLONE(Objective)
COVARIANT_RTYPE_CLONE(FluxObjective)
COVARIANT_RTYPE_CLONE(FluxBound)
COVARIANT_RTYPE_CLONE(ListOfFluxBounds)
COVARIANT_RTYPE_CLONE(ListOfFluxObjectives)
COVARIANT_RTYPE_CLONE(ListOfGeneAssociations)
COVARIANT_RTYPE_CLONE(ListOfObjectives)
COVARIANT_RTYPE_CLONE(FbcAssociation)
COVARIANT_RTYPE_CLONE(GeneProductAssociation)
COVARIANT_RTYPE_CLONE(GeneProduct)
COVARIANT_RTYPE_CLONE(GeneProductRef)
COVARIANT_RTYPE_CLONE(FbcAnd)
COVARIANT_RTYPE_CLONE(FbcOr)
COVARIANT_RTYPE_CLONE(UserDefinedConstraintComponent)
COVARIANT_RTYPE_CLONE(UserDefinedConstraint)
COVARIANT_RTYPE_CLONE(KeyValuePair)
COVARIANT_RTYPE_CLONE(ListOfFbcAssociations)
COVARIANT_RTYPE_CLONE(ListOfGeneProducts)
COVARIANT_RTYPE_CLONE(ListOfUserDefinedConstraintComponents)
COVARIANT_RTYPE_CLONE(ListOfUserDefinedConstraints)
COVARIANT_RTYPE_CLONE(ListOfKeyValuePairs)

COVARIANT_RTYPE_LISTOF_GET_REMOVE(FbcAssociation)
COVARIANT_RTYPE_LISTOF_GET_REMOVE(GeneProduct)
COVARIANT_RTYPE_LISTOF_GET_REMOVE(Association)
COVARIANT_RTYPE_LISTOF_GET_REMOVE(GeneAssociation)
COVARIANT_RTYPE_LISTOF_GET_REMOVE(Objective)
COVARIANT_RTYPE_LISTOF_GET_REMOVE(FluxObjective)
COVARIANT_RTYPE_LISTOF_GET_REMOVE(FluxBound)
COVARIANT_RTYPE_LISTOF_GET_REMOVE(UserDefinedConstraintComponent)
COVARIANT_RTYPE_LISTOF_GET_REMOVE(UserDefinedConstraint)
COVARIANT_RTYPE_LISTOF_GET_REMOVE(KeyValuePair)


SBMLCONSTRUCTOR_EXCEPTION(Association)
SBMLCONSTRUCTOR_EXCEPTION(FbcPkgNamespaces)
SBMLCONSTRUCTOR_EXCEPTION(FluxBound)
SBMLCONSTRUCTOR_EXCEPTION(FluxObjective)
SBMLCONSTRUCTOR_EXCEPTION(GeneAssociation)
SBMLCONSTRUCTOR_EXCEPTION(ListOfFluxBounds)
SBMLCONSTRUCTOR_EXCEPTION(ListOfFluxObjectives)
SBMLCONSTRUCTOR_EXCEPTION(ListOfGeneAssociations)
SBMLCONSTRUCTOR_EXCEPTION(ListOfObjectives)
SBMLCONSTRUCTOR_EXCEPTION(Objective)

SBMLCONSTRUCTOR_EXCEPTION(GeneProductAssociation)
SBMLCONSTRUCTOR_EXCEPTION(FbcAssociation)
SBMLCONSTRUCTOR_EXCEPTION(GeneProduct)
SBMLCONSTRUCTOR_EXCEPTION(GeneProductRef)
SBMLCONSTRUCTOR_EXCEPTION(FbcAnd)
SBMLCONSTRUCTOR_EXCEPTION(FbcOr)
SBMLCONSTRUCTOR_EXCEPTION(ListOfFbcAssociations)

SBMLCONSTRUCTOR_EXCEPTION(UserDefinedConstraintComponent)
SBMLCONSTRUCTOR_EXCEPTION(UserDefinedConstraint)
SBMLCONSTRUCTOR_EXCEPTION(KeyValuePair)
SBMLCONSTRUCTOR_EXCEPTION(ListOfGeneProducts)
SBMLCONSTRUCTOR_EXCEPTION(ListOfUserDefinedConstraintComponents)
SBMLCONSTRUCTOR_EXCEPTION(ListOfUserDefinedConstraints)
SBMLCONSTRUCTOR_EXCEPTION(ListOfKeyValuePairs)

//
// Convert FbcAssociation objects into the most specific object possible.
//
%typemap("javaout") FbcAssociation*
{
	return (FbcAssociation) libsbml.DowncastSBase($jnicall, $owner);
}


#endif /* USE_FBC */

